-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2012, Gabriel Dos Reis.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--     - Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     - Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in
--       the documentation and/or other materials provided with the
--       distribution.
--
--     - Neither the name of The Numerical Algorithms Group Ltd. nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


import ggreater
import sys_-macros
import daase
namespace BOOT

module g_-util where
  abstraction?: %Form -> %Boolean
  getTypeOfSyntax: %Form -> %Mode
  pairList: (%List %Form,%List %Form) -> %List %Pair(%Form,%Form)
  mkList: %List %Form -> %Form
  isSubDomain: (%Mode,%Mode) -> %Form
  isDefaultPackageName: %Symbol -> %Boolean
  makeDefaultPackageName: %String -> %Symbol
  spliceSeqArgs: %List %Code -> %Code
  mkSeq: %List %Code -> %Code
  usesVariable?: (%Code,%Symbol) -> %Boolean

--%

abstraction? x ==
  x is [op,:.] and ident? op and abstractionOperator? op

bindingForm? x ==
  x is [op,:.] and ident? op and op in '(%bind LET)

++ Return true if `form' uses symbol `var'.
usesVariable?(form,var) ==
  symbol? form => symbolEq?(form,var)
  atomic? form => false
  abstraction? form =>
    not symbolMember?(var,form.absParms) and usesVariable?(form.absBody,var)
  form.op is [.,:.] and usesVariable?(form.op,var) => true
  bindingForm? form =>
    x := 
      or/[usesVariable?(second parm,var)
           or symbolEq?(first parm,var) and leave 'bound
             for parm in form.absParms]
    x is 'bound => false
    x or usesVariable?(form.absBody,var)
  -- a variable can be used only in argument position.
  or/[usesVariable?(x,var) for x in form.args]

hasNoLeave?(expr,g) ==
  atomic? expr => true
  expr is ['%leave, =g,:.] => false
  hasNoLeave?(first expr,g) and hasNoLeave?(rest expr,g)

mkScope(tag,expr) ==
  expr is ['%leave,=tag,expr'] and hasNoLeave?(expr',tag) => expr'
  expr is ['%bind,inits,expr'] and hasNoLeave?(inits,tag) =>
    mkBind(inits,mkScope(tag,expr'))
  hasNoLeave?(expr,tag) => expr
  ['%scope,tag,expr]

mkBind(inits,expr) ==
  expr is ['%leave,tag,expr'] =>
    ['%leave,tag,mkBind(inits,expr')]
  expr is ['%bind,inits',expr'] =>
    mkBind([:inits,:inits'],expr')
  ['%bind,inits,expr]

mkSeq stmts ==
  stmts is [s] => s
  ['%seq,:stmts]

++ Given a (possibly multiple) assignment expression `u', return
++ the list of all assignment sub-expressions that must be evaluated before
++ effecting the toplevel assignment indicated by `u'.  In that case,
++ modify `u' in place to reflect the new right-hand-side.
splitAssignments! u == main(u,nil) where
   main(u,l) ==
     u is ['%LET,x,v] =>
       v is ['%LET,y,.] =>
         second(u.args) := y
         [:main(v,l),v]
       [:liftAssignments! v,:l]
     l

liftAssignments! x ==
  x is ['%call,:.] => [:lift! for args in tails x.args] where
    lift!() ==
      s := first args
      atomic? s => nil
      s is ['%LET,y,:.] =>
        args.first := y
        [:splitAssignments! s,s]
      splitAssignments! s
  splitAssignments! x

++ We have a list `l' of expressions to be executed sequentially.
++ Splice in any directly-embedded sequence of expressions.
++ NOTES: This function should not be called on any program with
++        an %exit-form in it.  In particular, it should be called
++        (if at all) before any call to simplifyVMForm.
spliceSeqArgs l ==
  l = nil => l
  s := first l
  s is ['%seq,:.] =>
    stmts := spliceSeqArgs s.args
    stmts = nil => spliceSeqArgs rest l
    lastNode(stmts).rest := spliceSeqArgs rest l
    stmts
  s is ['%scope,tag,['%seq,:ys,['%leave,=tag,z]]] and hasNoLeave?(ys,tag) =>
    stmts := spliceSeqArgs [:ys,z]
    lastNode(stmts).rest := spliceSeqArgs rest l
    stmts
  s is [op,:.] and op in '(%LET %call) and (stmts := liftAssignments! s) =>
    lastNode(stmts).rest := [s,:spliceSeqArgs rest l]
    stmts
  rest l = nil => l
  l.rest := spliceSeqArgs rest l
  l

++ Apply the function `f' on all non-atomic subforms of `x' in
++ depth-first walk.  Mutate `x' in place, replacing each sub-form
++ with the result of applying `f' to that subform.
walkWith!(x,f) ==
  atomic? x => x
  abstraction? x =>
    x.absBody := walkWith!(x.absBody,f)
    x
  for ys in tails x | not atomic? first ys repeat
    ys.first := walkWith!(first ys,f)
  apply(f,x,nil)

prefixWalk!(x,f) ==
  atomic? x => x
  x := apply(f,x,nil)
  atomic? x => x
  for xs in tails x | not atomic? first xs repeat
    xs.first := prefixWalk!(first xs,f)
  x

listify x ==
  x is [.,:.] => x
  [x]

--%

++ List of category constructors that do not have entries in the 
++ constructor database. So, they are mostly recognized by their names.
$CategoryNames ==
  '(CATEGORY _
    RecordCategory _
    Join _
    EnumerationCategory _
    SubsetCategory _
    UnionCategory _
    MappingCategory)

macro builtinCategoryName? x ==
  symbolMember?(x,$CategoryNames)

++ List of domain constructors that do not have entries in the constructor
++ database. So, they are mostly recognized by their names.
++ See also $CategoryNames.
$DomainNames ==
  '(Mapping _
    SubDomain _
    Union _
    Record _
    Enumeration _
    Cross)

macro builtinFunctorName? x ==
  symbolMember?(x,$DomainNames)

++ The collection of builtin category names and builtin domain names.
$BuiltinConstructorNames ==
  [:$CategoryNames,:$DomainNames]

++ Return true if the symbol `s' designates a builtin constructor.
macro builtinConstructor? s ==
  symbolMember?(s,$BuiltinConstructorNames)

--%  

$AbstractionOperator ==
  '(LAM ILAM XLAM LAMBDA %lambda %slam)

++ Return the character designated by the string `s'.
stringToChar: %String -> %Char
stringToChar s ==
  #s = 1 => stringChar(s,0)
  s = '"\a" => $Bell
  s = '"\n" => $Newline
  s = '"\f" => $FormFeed
  s = '"\r" => $CarriageReturn
  s = '"\b" => $Backspace
  s = '"\t" => $HorizontalTab
  s = '"\v" => $VerticalTab
  error strconc('"invalid character designator: ", s)
  
++
$interpOnly := false

--% Utility Functions of General Use

mkCacheName(name) ==
  makeSymbol strconc(symbolName name,'";AL")

mkAuxiliaryName(name) ==
  makeSymbol strconc(symbolName name,'";AUX")


homogeneousListToVector(t,l) ==
  makeSimpleArrayFromList(t,l)


++ tests if x is an identifier beginning with #
isSharpVar x ==
  ident? x and stringChar(symbolName x,0) = char "#"

++ If `x' is a formal variable, return its numeral position.
++ Otherwise return nil.
formalVarNumber x ==
  not isSharpVar x => nil
  readIntegerIfCan subsString(symbolName x,1)

isSharpVarWithNum x ==
  not isSharpVar x => nil
  p := symbolName x
  (n := #p) < 2 => nil
  ok := true
  c := 0
  for i in 1..(n-1) while ok repeat
    d := stringChar(p,i)
    ok := digit? d => c := 10*c + DIG2FIX d
  if ok then c else nil


mkBuffer v ==
  [copyVector v,:#v]

macro bufferData buf ==
  first buf

macro bufferLength buf ==
  rest buf

macro bufferRef(buf,i) ==
  vectorRef(bufferData buf,i)

resizeBuffer(buf,n) ==
  #bufferData buf >= n =>
    bufferLength(buf) := n
    buf
  v := mkVector(2 * n)
  for i in 0..(bufferLength buf - 1) repeat
    vectorRef(v,i) := vectorRef(bufferData buf,i)
  bufferData(buf) := v
  bufferLength(buf) := n
  buf

bufferToVector buf ==
  n := bufferLength buf
  v := mkVector n
  for i in 0..(n-1) repeat
    vectorRef(v,i) := vectorRef(bufferData buf,i)
  v


--% Sub-domains information handlers

++ If `dom' is a subdomain, return its immediate super-domain.  
superType: %Mode -> %Maybe %Mode
superType dom ==
  dom = "$" => superType $functorForm
  dom isnt [ctor,:args] => nil
  builtinConstructor? ctor => nil
  [super,.] :=
    db := constructorDB ctor or return nil
    dbBeingDefined? db => dbSuperDomain db or return nil
    dbConstructorKind db is 'domain or return nil
    --dbSuperDomain loadDBIfNecessary db or return nil
    getSuperDomainFromDB ctor or return nil
  sublisFormal(args,super,$AtVariables)

++ If the domain designated by the domain form `dom' is a subdomain,
++ then return its defining predicate.  Otherwise, return nil.
domainVMPredicate dom ==
  dom = "$" => domainVMPredicate $functorForm
  dom isnt [ctor,:args] => false
  [.,pred] := getSuperDomainFromDB ctor or return nil
  sublisFormal(args,pred,$AtVariables)

++ Return the root of the reflexive transitive closure of
++ the super-domain chain for the domain designated by the domain 
++ form `d'.
maximalSuperType: %Mode -> %Mode
maximalSuperType d ==
  d' := superType d => maximalSuperType d'
  d

++ Note that the functor `sub' instantiates to domains that
++ are subdomains of `super' instances restricted by the 
++ predicate `pred'.
noteSubDomainInfo: (%Symbol,%Instantiation,%Form) -> %Thing
noteSubDomainInfo(sub,super,pred) ==
  SETDATABASE(sub,"SUPERDOMAIN",[super,pred])

++ Returns non-nil if `d1' is a sub-domain of `d2'.  This is the
++ case when `d1' is transitively given by an instance of SubDomain
++    d1 == SubDomain(d2,pred)
++ The transitive closure of the predicate form is returned, where 
++ the predicate parameter is `#1'.
isSubDomain(d1,d2) ==
  d1 isnt [.,:.] or d2 isnt [.,:.] => false

  -- 1.  Easy, if by syntax constructs.
  d1 is ["SubDomain",=d2,pred] => pred

  -- 2.  Just say no, if there is no hope.
  [sup,pred] := getSuperDomainFromDB first d1 or return false

  -- 3.  We may be onto something.
  -- `sup' and `pred' are in most general form.  We cannot just
  -- test for the functors, as different arguments may instantiate
  -- to super-domains.  
  args := rest d1
  sublisFormal(args,sup,$AtVariables) = d2 => 
    sublisFormal(args,pred,$AtVariables)

  -- 4.  Otherwise, lookup in the super-domain chain.
  pred' := isSubDomain(sup,d2) => 
    ['%and,pred',sublisFormal(args,pred,$AtVariables)]

  -- 5.  Lot of smoke, no fire.
  false

--%

mkList u ==
  u => ['%list,:u]
  nil

ELEMN(x, n, d) ==
  null x => d
  n = 1 => first x
  ELEMN(rest x, n-1, d)

PPtoFile(x, fname) ==
    stream := DEFIOSTREAM([['MODE, :'OUTPUT], ['FILE, :fname]], 80, 0)
    PRETTYPRINT(x, stream)
    SHUT stream
    x

ScanOrPairVec(f, ob) ==
    $seen:     local := hashTable 'EQ
 
    CATCH('ScanOrPairVecAnswer, ScanOrInner(f, ob)) where
        ScanOrInner(f, ob) ==
            tableValue($seen, ob) => nil
            cons? ob =>
                tableValue($seen, ob) := true
                ScanOrInner(f, first ob)
                ScanOrInner(f, rest ob)
                nil
            vector? ob =>
                tableValue($seen, ob) := true
                for i in 0..#ob-1 repeat ScanOrInner(f, ob.i)
                nil
            FUNCALL(f, ob) =>
                THROW('ScanOrPairVecAnswer, true)
            nil


++ Query properties for an entity in a given environment.
get: (%Thing,%Symbol,%Env) -> %Thing
get0: (%Thing,%Symbol,%Env) -> %Thing
get1: (%Thing,%Symbol,%Env) -> %Thing
get2: (%Thing,%Symbol) -> %Thing

get(x,prop,e) ==
  $InteractiveMode => get0(x,prop,e)
  get1(x,prop,e)

get0(x,prop,e) ==
  cons? x => get0(x.op,prop,e)
  u := symbolTarget(x,first first e) => symbolTarget(prop,u)
  (tail := rest first e) and (u := fastSearchCurrentEnv(x,tail)) =>
    symbolTarget(prop,u)
  nil

get1(x,prop,e) ==
  cons? x => get1(x.op,prop,e)
  prop = "modemap" and $insideCapsuleFunctionIfTrue =>
    symbolTarget("modemap",getProplist(x,$CapsuleModemapFrame))
      or get2(x,prop)
  symbolTarget(prop,getProplist(x,e)) or get2(x,prop)

get2(x,prop) ==
  prop = "modemap" and ident? x and constructor? x =>
    (u := getConstructorModemap x) => [u]
    nil
  nil

++ Update properties of an entity in an environment.
put: (%Thing,%Symbol,%Thing,%Env) -> %Env
addBinding: (%Thing,%List %Thing,%Env) -> %Env
addBindingInteractive: (%Thing, %List %Thing, %Env) -> %Env
augProplistOf: (%Thing,%Symbol,%Thing,%Env) -> %List %Thing
augProplist: (%List %Thing,%Symbol,%Thing) -> %List %Thing
augProplistInteractive: (%List %Thing,%Symbol,%Thing) -> %List %Thing
putIntSymTab: (%Thing,%Symbol,%Form,%Env) -> %Env
addIntSymTabBinding: (%Thing,%List %Thing,%Env) -> %Env

put(x,prop,val,e) ==
  $InteractiveMode and not sameObject?(e,$CategoryFrame) =>
    putIntSymTab(x,prop,val,e)
  --e must never be $CapsuleModemapFrame
  cons? x => put(first x,prop,val,e)
  newProplist := augProplistOf(x,prop,val,e)
  prop="modemap" and $insideCapsuleFunctionIfTrue =>
    SAY ["**** modemap PUT on CapsuleModemapFrame: ",val]
    $CapsuleModemapFrame:=
      addBinding(x,augProplistOf(x,"modemap",val,$CapsuleModemapFrame),
        $CapsuleModemapFrame)
    e
  addBinding(x,newProplist,e)

putIntSymTab(x,prop,val,e) ==
  cons? x => putIntSymTab(first x,prop,val,e)
  pl0 := pl := search(x,e)
  pl :=
    null pl => [[prop,:val]]
    u := objectAssoc(prop,pl) =>
      u.rest := val
      pl
    lp := lastNode pl
    u := [[prop,:val]]
    lp.rest := u
    pl
  sameObject?(pl0,pl) => e
  addIntSymTabBinding(x,pl,e)

addIntSymTabBinding(var,proplist,e is [[curContour,:.],:.]) ==
  -- change proplist of var in e destructively
  u := objectAssoc(var,curContour) =>
    u.rest := proplist
    e
  first(e).first := [[var,:proplist],:curContour]
  e

putMacro(lhs,rhs,e) ==
  lhs isnt [.,:.] => put(lhs,"macro",rhs,e)
  parms := [gensym() for p in lhs.args]
  put(lhs.op,"macro",
    ['%mlambda,parms,applySubst(pairList(lhs.args,parms),rhs)],e)

--% Syntax manipulation

++ Build a quasiquotation form for `x'.
quasiquote x ==
  ["[||]",x]

++ Extract the quoted form, otherwise return nil
isQuasiquote m ==
  m is ["[||]",y] => y


++ returns the inferred domain for the syntactic object t.
getTypeOfSyntax t ==
  t isnt [.,:.] => 
    ident? t => '(Identifier)
    (m := getBasicMode t) and not member(m,[$EmptyMode,$NoValueMode]) =>
      ["Literal",m]
    $Syntax
  [op,:.] := t
  op = "Mapping" => '(MappingAst)
  op = 'QUOTE and #t = 2 and ident? second t => ["Literal",$Symbol]
  op = "IF" => '(IfAst)
  op = "REPEAT" => '(RepeatAst)
  op = "WHILE" => '(WhileAst)
  op = "IN" => '(InAst)
  op = "COLLECT" => '(CollectAst)
  op = "construct" => '(ConstructAst)
  op = "exit" => '(ExitAst)
  op = "return" => '(ReturnAst)
  op = "SEGMENT" => '(SegmentAst)
  op = "SEQ" => '(SequenceAst)
  op = "pretend" => '(PretendAst)
  op = "::" => '(CoerceAst)
  op = "@" => '(RestrictAst)
  op = "%LET" or op = ":=" => '(LetAst)
  op = "|" => '(SuchThatAst)
  op = ":" => '(ColonAst)
  op = ":=" => '(LetAst)
  op = "%Comma" => '(CommaAst)
  op = "case" => '(CaseAst)
  op = "has" => '(HasAst)
  op = "is" => '(IsAst)
  op = "DEF" => '(DefinitionAst)
  op in '(MDEF %Macro) => '(MacroAst)
  op = "where" => '(WhereAst)
  op in '(ATTRIBUTE %Attribute) => '(AttributeAst)
  op = "Join" => '(JoinAst)
  op = "CAPSULE" => '(CapsuleAst)
  op in '(%Import import) => '(ImportAst)
  op in '(%Signature SIGNATURE) => '(SignatureAst)
  op = "CATEGORY" => '(CategoryAst)
  op = "where" => '(WhereAst)
  op = "[||]" => '(QuasiquoteAst)
  $Syntax

--%

-- Convert an arbitrary lisp object to canonical boolean.
bool: %Thing -> %Boolean
bool x ==
    null null x

++ Return true is the form `x' is a predicate known to always
++ evaluate to true.
TruthP x ==
  x = nil or x = '%false => false
  x = true or x = '%true => true
  x is ['QUOTE,:.] => true
  false

--% Record and Union utils.

stripTags doms ==
  [if dom is [":",.,dom'] then dom' else dom for dom in doms]

isTaggedUnion u ==
  u is ['Union,:tl] and tl and first tl is [":",.,.] and true

getUnionOrRecordTags u ==
  tags := nil
  if u is ['Union, :tl] or u is ['Record, :tl] then
      for t in tl repeat
         if t is [":",tag,.] then tags := [tag, :tags]
  tags

--% Various lispy things

Identity x == x

length1? l == cons? l and not cons? rest l

length2? l == cons? l and cons? (l := rest l) and not cons? rest l

pairList(u,v) == [[x,:y] for x in u for y in v]

-- GETALIST(alist,prop) == IFCDR assoc(prop,alist)
GETALIST(alist,prop) == rest assoc(prop,alist)

PUTALIST(alist,prop,val) ==
  null alist => [[prop,:val]]
  pair := assoc(prop,alist) =>
    rest pair = val => alist
    -- else we fall over Lucid's read-only storage feature again
    pair.rest := val
    alist
  lastNode(alist).rest := [[prop,:val]]
  alist

REMALIST(alist,prop) ==
  null alist => alist
  alist is [[ =prop,:.],:r] =>
    null r => nil
    alist.first := first r
    alist.rest := rest r
    alist
  null rest alist => alist
  l := alist
  ok := true
  while ok repeat
    [.,[p,:.],:r] := l
    p = prop =>
      ok := nil
      l.rest := r
    if null (l := rest l) or null rest l then ok := nil
  alist

deleteLassoc(x,y) ==
  y is [[a,:.],:y'] =>
    sameObject?(x,a) => y'
    [first y,:deleteLassoc(x,y')]
  y

--% association list functions

deleteAssoc(x,y) ==
  y is [[a,:.],:y'] =>
   a=x => deleteAssoc(x,y')
   [first y,:deleteAssoc(x,y')]
  y

deleteAssocWOC(x,y) ==
  null y => y
  [[a,:.],:t]:= y
  x=a => t
  (fn(x,y);y) where fn(x,y is [h,:t]) ==
    t is [[a,:.],:t1] =>
      x=a => y.rest := t1
      fn(x,t)
    nil

insertWOC(x,y) ==
  null y => [x]
  (fn(x,y); y) where fn(x,y is [h,:t]) ==
    x=h => nil
    null t =>
      y.rest := [h,:t]
      y.first := x
    fn(x,t)


++ Post-compose substitution `subst' with `s +-> v'
postComposeSubst(subst,s,v) ==
  [[x,:substitute(v,s,y)] for [x,:y] in subst]

--% Miscellaneous Functions for Working with Strings

fillerSpaces(n,charPart == char " ") ==
  n <= 0 => '""
  makeString(n,charPart)

centerString(text,width,fillchar) ==
  wid := entryWidth text
  wid >= width => text
  f := DIVIDE(width - wid,2)
  fill1 := ""
  for i in 1..(f.0) repeat
    fill1 := strconc(fillchar,fill1)
  fill2:= fill1
  if f.1 ~= 0 then fill1 := strconc(fillchar,fill1)
  [fill1,text,fill2]

stringChar2Integer(str,pos) ==
  -- returns small integer represented by character in position pos
  -- in string str. Returns nil if not a digit or other error.
  if ident? str then str := symbolName str
  not (string?(str) and
    integer?(pos) and (pos >= 0) and (pos < #str)) => nil
  not digit?(d := stringChar(str,pos)) => nil
  DIG2FIX d

dropLeadingBlanks str ==
  str := object2String str
  l := # str
  nb := nil
  i := 0
  while (i < l) and nb = nil repeat
    if stringChar(str,i) ~= char " " then nb := i
    else i := i + 1
  nb = 0 => str
  nb => subString(str,nb)
  '""

concat(:l) == concatList l

concatList [x,:y] ==
  null y => x
  null x => concatList y
  concat1(x,concatList y)

concat1(x,y) ==
  null x => y
  x isnt [.,:.] => (null y => x; y isnt [.,:.] => [x,y]; [x,:y])
  null y => x
  y isnt [.,:.] => [:x,y]
  [:x,:y]

--% BOOT ravel and reshape

ravel a == a

reshape(a,b) == a

--% Some functions for algebra code

boolODDP x == ODDP x

--% Miscellaneous

freeOfSharpVars x ==
  x isnt [.,:.] => not isSharpVarWithNum x
  freeOfSharpVars first x and freeOfSharpVars rest x

listOfSharpVars x ==
  x isnt [.,:.] => (isSharpVarWithNum x => [x]; nil)
  setUnion(listOfSharpVars first x,listOfSharpVars rest x)

listOfPatternIds x ==
  isPatternVar x => [x]
  x isnt [.,:.] => nil
  x is ['QUOTE,:.] => nil
  setUnion(listOfPatternIds first x,listOfPatternIds rest x)

isPatternVar v ==
  -- a pattern variable consists of a star followed by a star or digit(s)
  ident?(v) and v in '(_*_* _*1 _*2 _*3 _*4 _*5 _*6 _*7 _*8 _*9 _*10
    _*11 _*12 _*13 _*14 _*15 _*16 _*17 _*18 _*19 _*20) and true

removeZeroOne x ==
  -- replace all occurrences of (Zero) and (One) with
  -- 0 and 1
  x = $Zero => 0
  x = $One => 1
  x isnt [.,:.] => x
  [removeZeroOne first x,:removeZeroOne rest x]

removeZeroOneDestructively t ==
  -- replace all occurrences of (Zero) and (One) with
  -- 0 and 1 destructively
  t = $Zero => 0
  t = $One => 1
  t isnt [.,:.] => t
  RPLNODE(t,removeZeroOneDestructively first t,
    removeZeroOneDestructively rest t)

flattenSexpr s ==
  null s => s
  s isnt [.,:.] => s
  [f,:r] := s
  f isnt [.,:.] => [f,:flattenSexpr r]
  [:flattenSexpr f,:flattenSexpr r]

isLowerCaseLetter c ==
  lowerCase? c

isUpperCaseLetter c ==
  upperCase? c

isLetter c ==
  alphabetic? c

--% Inplace Merge Sort for Lists
-- MBM April/88

-- listSort(pred,list) or listSort(pred,list,key)
-- the pred function is a boolean valued function defining the ordering
-- the key function extracts the key from an item for comparison by pred

listSort(pred,list,:optional) ==
   not functionp pred => error "listSort: first arg must be a function"
   not LISTP list => error "listSort: second argument must be a list"
   optional = nil => mergeSort(pred,function Identity,list,# list)
   key := first optional
   not functionp key => error "listSort: last arg must be a function"
   mergeSort(pred,key,list,# list)

-- non-destructive merge sort using NOT GGREATERP as predicate
MSORT list == listSort(function GLESSEQP, copyList list)

-- destructive merge sort using NOT GGREATERP as predicate
NMSORT list == listSort(function GLESSEQP, list)

-- non-destructive merge sort using ?ORDER as predicate
orderList l == listSort(function _?ORDER, copyList l)

-- dummy defn until clean-up
-- order     l == orderList l

mergeInPlace(f,g,p,q) ==
   -- merge the two sorted lists p and q
   if null p then return p
   if null q then return q
   if FUNCALL(f,FUNCALL(g, first p),FUNCALL(g, first q))
   then (r := t := p; p := rest p)
   else (r := t := q; q := rest q)
   while not null p and not null q repeat
      if FUNCALL(f,FUNCALL(g,first p),FUNCALL(g,first q))
      then (t.rest := p; t := p; p := rest p)
      else (t.rest := q; t := q; q := rest q)
   if null p then t.rest := q else t.rest := p
   r

mergeSort(f,g,p,n) ==
   if n=2 and FUNCALL(f,FUNCALL(g,second p),FUNCALL(g,first p)) then
      t := p
      p := rest p
      p.rest := t
      t.rest := nil
   if n < 3 then return p
   -- split the list p into p and q of equal length
   l := n quo 2
   t := p
   for i in 1..l-1 repeat t := rest t
   q := rest t
   t.rest := nil
   p := mergeSort(f,g,p,l)
   q := mergeSort(f,g,q,n-l)
   mergeInPlace(f,g,p,q)

--% Throwing with glorious highlighting (maybe)

spadThrow() ==
  if $interpOnly and $mapName then
    putHist($mapName,'localModemap, nil, $e)
  THROW($SpadReaderTag,nil)

spadThrowBrightly x ==
  sayBrightly x
  spadThrow()

sublisNQ(al,e) ==
  al isnt [.,:.] => e
  fn(al,e) where fn(al,e) ==
    e isnt [.,:.] =>
      for x in al repeat
        sameObject?(first x,e) => return (e := rest x)
      e
    sameObject?(a := first e,'QUOTE) => e
    u := fn(al,a)
    v := fn(al,rest e)
    sameObject?(a,u) and sameObject?(rest e,v) => e
    [u,:v]

opOf: %Thing -> %Thing
opOf x ==
  cons? x => x.op
  x

getProplist: (%Thing,%Env) -> %List %Thing
search: (%Thing,%Env) -> %List %Thing
searchCurrentEnv: (%Thing,%List %Thing) -> %List %Thing
searchTailEnv: (%Thing,%Env) -> %List %Thing

getProplist(x,E) ==
  cons? x => getProplist(first x,E)
  u:= search(x,E) => u
  --$InteractiveMode => nil
  --$InteractiveMode and (u:= search(x,$InteractiveFrame)) => u
  (pl:=search(x,$CategoryFrame)) =>
    pl
--  (pl:=PROPLIST x) => pl
-- Above line commented out JHD/BMT 2.Aug.90

search(x,e is [curEnv,:tailEnv]) ==
  searchCurrentEnv(x,curEnv) or searchTailEnv(x,tailEnv)

searchCurrentEnv(x,currentEnv) ==
  for contour in currentEnv repeat
    if u:= objectAssoc(x,contour) then return (signal:= u)
  KDR signal

searchTailEnv(x,e) ==
  for env in e repeat
    signal:=
      for contour in env repeat
        if (u := objectAssoc(x,contour)) and objectAssoc("FLUID",u) then
          return (signal:= u)
      if signal then return signal
  KDR signal

augProplist(proplist,prop,val) ==
  $InteractiveMode => augProplistInteractive(proplist,prop,val)
  while (proplist is [[ =prop,:.],:proplist']) repeat proplist:= proplist'
  val = (u := symbolTarget(prop,proplist)) => proplist
  null val =>
    null u => proplist
    DELLASOS(prop,proplist)
  [[prop,:val],:proplist]

augProplistOf(var,prop,val,e) ==
  proplist:= getProplist(var,e)
  semchkProplist(var,proplist,prop,val)
  augProplist(proplist,prop,val)

semchkProplist(x,proplist,prop,val) ==
  prop="isLiteral" =>
    symbolTarget("value",proplist) or symbolTarget("mode",proplist) =>
      warnLiteral x
  prop in '(mode value) =>
    symbolTarget("isLiteral",proplist) => warnLiteral x

addBinding(var,proplist,e is [[curContour,:tailContour],:tailEnv]) ==
  sameObject?(proplist,getProplist(var,e)) => e
  $InteractiveMode => addBindingInteractive(var,proplist,e)
  if curContour is [[ =var,:.],:.] then curContour:= rest curContour
                 --Previous line should save some space
  [[[lx,:curContour],:tailContour],:tailEnv] where lx:= [var,:proplist]

addBindingInteractive(var,proplist,e is [[curContour,:.],:.]) ==
  -- change proplist of var in e destructively
  u := objectAssoc(var,curContour) =>
    u.rest := proplist
    e
  first(e).first := [[var,:proplist],:curContour]
  e

augProplistInteractive(proplist,prop,val) ==
  u := objectAssoc(prop,proplist) =>
    u.rest := val
    proplist
  [[prop,:val],:proplist]

position(x,l) ==
  posn(x,l,0) where
    posn(x,l,n) ==
      null l => -1
      x=first l => n
      posn(x,rest l,n+1)

insert(x,y) ==
  member(x,y) => y
  [x,:y]

after(u,v) ==
  r:= u
  for x in u for y in v repeat r:= rest r
  r


$blank == char " "

trimString s ==
  leftTrim rightTrim s

leftTrim s ==
  k := maxIndex s
  k < 0 => s
  stringChar(s,0) = $blank =>
    for i in 0..k while stringChar(s,i) = $blank repeat (j := i)
    subString(s,j + 1)
  s

rightTrim s ==  -- assumed a non-empty string
  k := maxIndex s
  k < 0 => s
  stringChar(s,k) = $blank =>
    for i in k..0 by -1 while stringChar(s,i) = $blank repeat (j := i)
    subString(s,0,j)
  s

pp x ==
  PRETTYPRINT x
  x

pr x ==
  F_,PRINT_-ONE x
  nil

intern x ==
  string? x =>
    digit? stringChar(x,0) => string2Integer x
    makeSymbol x
  x

-- variables used by browser

$htHash      := MAKE_-HASH_-TABLE()
$glossHash   := MAKE_-HASH_-TABLE()
$lispHash    := MAKE_-HASH_-TABLE()
$sysHash     := MAKE_-HASH_-TABLE()
$htSystemCommands := '(
 (boot . development) clear display (fin . development) edit help
 frame history load quit read set show synonym system
 trace what )
$currentSysList := [opOf x for x in $htSystemCommands] --see ht-root
$outStream   := nil
$recheckingFlag    := false     --see transformAndRecheckComments
$exposeFlag        := false     --if true, messages go to $outStream
$exposeFlagHeading := false     --see htcheck.boot
$checkingXmptex? := false       --see htcheck.boot
$exposeDocHeading:= nil         --see htcheck.boot
$charPlus == char "+"
$charBlank == char " "
$charLbrace == char "{"
$charRbrace == char "}"
$charBack == char "\"
$charDash == char "-"

$charTab            == abstractChar 9
$charNewline        == abstractChar 10
$charFauxNewline    == abstractChar 25
$stringNewline      == charString abstractChar 10
$stringFauxNewline  == charString abstractChar 25

$charExclusions == [char "a", char "A"]
$charQuote == char "'"
$charSemiColon == char ";"
$charComma     == char ","
$charPeriod    == char "."
$charEscapeList:= [char "%",char "#",$charBack]
$charIdentifierEndings := [char "__", char "!", char "?"]
$charSplitList := [$charComma,$charPeriod,char "[", char "]",$charLbrace, $charRbrace, char "(", char ")", char "$", char "%"]
$charDelimiters := [$charBlank, char "(", char ")", $charBack]
$HTspadmacros := '("\spadtype" "\spadcommand" "\spadop" "\spadfun" "\spadatt" "\spadsyscom" "\spad" "\s")
$HTmacs := [
  ['"\beginmenu",$charRbrace,'"menu",$charLbrace,'"\begin"],
   ['"\endmenu",$charRbrace,'"menu",$charLbrace,'"\end"],
     ['"\beginitems",$charRbrace,'"items",$charLbrace,'"\begin"],
       ['"\enditems",$charRbrace,'"items",$charLbrace,'"\end"],
         ['"\beginscroll",$charRbrace,'"scroll",$charLbrace,'"\begin"],
           ['"\endscroll",$charRbrace,'"scroll",$charLbrace,'"\end"]]

$HTlinks := '(
  "\downlink"
  "\menulink"
  "\menudownlink"
  "\menuwindowlink"
  "\menumemolink")

$HTlisplinks := '(
  "\lispdownlink"
  "\menulispdownlink"
  "\menulispwindowlink"
  "\menulispmemolink"
  "\lispwindowlink"
  "\lispmemolink")

$beginEndList := '(
  "page"
  "items"
  "menu"
  "scroll"
  "verbatim"
  "detail")

isDefaultPackageName x ==
  s := symbolName x
  stringChar(s,maxIndex s) = char "&"

defaultPackageForm? x ==
  x is [op,:.] and ident? op and isDefaultPackageName op

makeDefaultPackageName x ==
  makeSymbol strconc(x,'"&")

normalizeName: %Symbol -> %Symbol
normalizeName x ==
  x = "T" => "T$" -- rename T in spad code to avoid clash with Lisp
  x = "^" => "**" -- always use `**' internally for exponentiation
  x

++ Return the internal exported name of a potential operator `x'.
internalName x ==
  ident? x => normalizeName x
  not integer? x or x < 0 => x
  x = 0 => 'Zero
  x = 1 => 'One
  makeSymbol toString x

++ Return the external exported name of th potential operator `x'.
externalName: %Symbol -> %Symbol
externalName x ==
  x is 'Zero => "0"
  x is 'One => "1"
  x

-- gensym utils

charDigitVal c ==
  digits := '"0123456789"
  n := -1
  for i in 0..maxIndex digits while n < 0 repeat
      if c = stringChar(digits,i) then n := i
  n < 0 => error '"Character is not a digit"
  n

gensymInt g ==
  not gensym? g => error '"Need a GENSYM"
  p := symbolName g
  n := 0
  for i in 2..maxIndex p repeat
    n := 10 * n + charDigitVal stringChar(p,i)
  n

++ Return true if var is a query variable, e.g. any identifier
++ that starts with a question mark.
queryVar? var ==
  s := symbolName var
  #s > 1 and stringChar(s,0) = char "?" and digit? stringChar(s,1)

++ Returns a newly allocated domain shell (a simple vector) of length `n'.
newShell: %Short -> SIMPLE_-ARRAY
newShell n ==
  MAKE_-ARRAY(n,initial_-element <- nil)

-- Push into the BOOT package when invoked in batch mode.
AxiomCore::$sysScope := '"BOOT"
