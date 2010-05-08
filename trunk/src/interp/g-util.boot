-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2010, Gabriel Dos Reis.
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
import macros
import sys_-utility
namespace BOOT

module g_-util where
  isAtomicForm: %Form -> %Boolean
  getTypeOfSyntax: %Form -> %Mode
  pairList: (%List,%List) -> %List
  mkList: %List -> %List
  isSubDomain: (%Mode,%Mode) -> %Form

++
$interpOnly := false

--% Utility Functions of General Use

homogeneousListToVector(t,l) ==
  makeSimpleArrayFromList(t,l)


++ tests if x is an identifier beginning with #
isSharpVar x ==
  IDENTP x and SCHAR(SYMBOL_-NAME x,0) = char "#"
 
isSharpVarWithNum x ==
  null isSharpVar x => nil
  (n := QCSIZE(p := PNAME x)) < 2 => nil
  ok := true
  c := 0
  for i in 1..(n-1) while ok repeat
    d := ELT(p,i)
    ok := DIGITP d => c := 10*c + DIG2FIX d
  if ok then c else nil

++ Returns true if `form' is either an atom or a quotation.
isAtomicForm form ==
  atom form or first form = "QUOTE"


--% Sub-domains information handlers

++ If `dom' is a subdomain, return its immediate super-domain.  
superType: %Mode -> %Maybe %Mode
superType dom ==
  dom = "$" => superType $functorForm
  dom isnt [ctor,:args] => nil
  [super,.] := getSuperDomainFromDB ctor or return nil
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
  atom d1 or atom d2 => false

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
    MKPF([pred',sublisFormal(args,pred,$AtVariables)],"AND")

  -- 5.  Lot of smoke, no fire.
  false

--%

mkList u ==
  u => ["LIST",:u]
  nil

ELEMN(x, n, d) ==
  null x => d
  n = 1 => car x
  ELEMN(cdr x, n-1, d)

PPtoFile(x, fname) ==
    stream := DEFIOSTREAM([['MODE, :'OUTPUT], ['FILE, :fname]], 80, 0)
    PRETTYPRINT(x, stream)
    SHUT stream
    x

ScanOrPairVec(f, ob) ==
    $seen:     local := MAKE_-HASHTABLE 'EQ
 
    CATCH('ScanOrPairVecAnswer, ScanOrInner(f, ob)) where
        ScanOrInner(f, ob) ==
            HGET($seen, ob) => nil
            cons? ob =>
                HPUT($seen, ob, true)
                ScanOrInner(f, QCAR ob)
                ScanOrInner(f, QCDR ob)
                nil
            VECP ob =>
                HPUT($seen, ob, true)
                for i in 0..#ob-1 repeat ScanOrInner(f, ob.i)
                nil
            FUNCALL(f, ob) =>
                THROW('ScanOrPairVecAnswer, true)
            nil


++ Query properties for an entity in a given environment.
get: (%Thing,%Symbol,%List) -> %Thing
get0: (%Thing,%Symbol,%List) -> %Thing
get1: (%Thing,%Symbol,%List) -> %Thing
get2: (%Thing,%Symbol) -> %Thing

get(x,prop,e) ==
  $InteractiveMode => get0(x,prop,e)
  get1(x,prop,e)

get0(x,prop,e) ==
  not atom x => get(QCAR x,prop,e)
  u:= QLASSQ(x,first QCAR e) => QLASSQ(prop,u)
  (tail:= rest QCAR e) and (u:= fastSearchCurrentEnv(x,tail)) =>
    QLASSQ(prop,u)
  nil

get1(x,prop,e) ==
    --this is the old get
  not atom x => get(QCAR x,prop,e)
  prop="modemap" and $insideCapsuleFunctionIfTrue=true =>
    LASSOC("modemap",getProplist(x,$CapsuleModemapFrame))
      or get2(x,prop)
  LASSOC(prop,getProplist(x,e)) or get2(x,prop)

get2(x,prop) ==
  prop="modemap" and IDENTP x and constructor? x =>
    (u := getConstructorModemapFromDB x) => [u]
    nil
  nil

++ Update properties of an entity in an environment.
put: (%Thing,%Symbol,%Thing,%Env) -> %Env
addBinding: (%Thing,%List,%Env) -> %Env
addBindingInteractive: (%Thing, %List, %Env) -> %Env
augProplistOf: (%Thing,%Symbol,%Thing,%Env) -> %List
augProplist: (%List,%Symbol,%Thing) -> %List
augProplistInteractive: (%List,%Symbol,%Thing) -> %List
putIntSymTab: (%Thing,%Symbol,%Form,%Env) -> %Env
addIntSymTabBinding: (%Thing,%List,%Env) -> %Env

put(x,prop,val,e) ==
  $InteractiveMode and not EQ(e,$CategoryFrame) =>
    putIntSymTab(x,prop,val,e)
  --e must never be $CapsuleModemapFrame
  not atom x => put(first x,prop,val,e)
  newProplist := augProplistOf(x,prop,val,e)
  prop="modemap" and $insideCapsuleFunctionIfTrue=true =>
    SAY ["**** modemap PUT on CapsuleModemapFrame: ",val]
    $CapsuleModemapFrame:=
      addBinding(x,augProplistOf(x,"modemap",val,$CapsuleModemapFrame),
        $CapsuleModemapFrame)
    e
  addBinding(x,newProplist,e)

putIntSymTab(x,prop,val,e) ==
  null atom x => putIntSymTab(first x,prop,val,e)
  pl0 := pl := search(x,e)
  pl :=
    null pl => [[prop,:val]]
    u := ASSQ(prop,pl) =>
      RPLACD(u,val)
      pl
    lp := LASTPAIR pl
    u := [[prop,:val]]
    RPLACD(lp,u)
    pl
  EQ(pl0,pl) => e
  addIntSymTabBinding(x,pl,e)

addIntSymTabBinding(var,proplist,e is [[curContour,:.],:.]) ==
  -- change proplist of var in e destructively
  u := ASSQ(var,curContour) =>
    RPLACD(u,proplist)
    e
  RPLAC(CAAR e,[[var,:proplist],:curContour])
  e

--% Syntax manipulation

++ Build a quasiquotation form for `x'.
quasiquote x ==
  ["[||]",x]

++ Extract the quoted form, otherwise return nil
isQuasiquote m ==
  m is ["[||]",y] => y


++ returns the inferred domain for the syntactic object t.
getTypeOfSyntax t ==
  atom t => 
    IDENTP t => '(Identifier)
    (m := getBasicMode t) and not member(m,[$EmptyMode,$NoValueMode]) =>
      ["Literal",m]
    $Syntax
  [op,:.] := t
  op = "Mapping" => '(MappingAst)
  op = "QUOTE" and #t = 2 and IDENTP second t => ["Literal",$Symbol]
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
  op = "%LET" => '(LetAst)
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
 
TruthP x ==
    --True if x is a predicate that's always true
  x is nil => nil
  x=true => true
  x is ['QUOTE,:.] => true
  nil

--% Record and Union utils.

stripUnionTags doms ==
  [if dom is [":",.,dom'] then dom' else dom for dom in doms]

isTaggedUnion u ==
  u is ['Union,:tl] and tl and first tl is [":",.,.] and true

getUnionOrRecordTags u ==
  tags := nil
  if u is ['Union, :tl] or u is ['Record, :tl] then
      for t in tl repeat
         if t is [":",tag,.] then tags := cons(tag, tags)
  tags

--% Various lispy things

Identity x == x

length1? l == cons? l and not cons? QCDR l

length2? l == cons? l and cons? (l := QCDR l) and not cons? QCDR l

pairList(u,v) == [[x,:y] for x in u for y in v]

-- GETALIST(alist,prop) == IFCDR assoc(prop,alist)
GETALIST(alist,prop) == rest assoc(prop,alist)

PUTALIST(alist,prop,val) ==
  null alist => [[prop,:val]]
  pair := assoc(prop,alist) =>
    rest pair = val => alist
    -- else we fall over Lucid's read-only storage feature again
    QRPLACD(pair,val)
    alist
  QRPLACD(LASTPAIR alist,[[prop,:val]])
  alist

REMALIST(alist,prop) ==
  null alist => alist
  alist is [[ =prop,:.],:r] =>
    null r => NIL
    QRPLACA(alist,first r)
    QRPLACD(alist,rest r)
    alist
  null rest alist => alist
  l := alist
  ok := true
  while ok repeat
    [.,[p,:.],:r] := l
    p = prop =>
      ok := NIL
      QRPLACD(l,r)
    if null (l := QCDR l) or null rest l then ok := NIL
  alist

deleteLassoc(x,y) ==
  y is [[a,:.],:y'] =>
    EQ(x,a) => y'
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
      x=a => RPLACD(y,t1)
      fn(x,t)
    nil

insertWOC(x,y) ==
  null y => [x]
  (fn(x,y); y) where fn(x,y is [h,:t]) ==
    x=h => nil
    null t =>
      RPLACD(y,[h,:t])
      RPLACA(y,x)
    fn(x,t)



--% Miscellaneous Functions for Working with Strings

fillerSpaces(n,:charPart) ==
  n <= 0 => '""
  MAKE_-FULL_-CVEC(n,IFCAR charPart or '" ")

centerString(text,width,fillchar) ==
  wid := entryWidth text
  wid >= width => text
  f := DIVIDE(width - wid,2)
  fill1 := ""
  for i in 1..(f.0) repeat
    fill1 := STRCONC(fillchar,fill1)
  fill2:= fill1
  if f.1 ~= 0 then fill1 := STRCONC(fillchar,fill1)
  [fill1,text,fill2]

stringPrefix?(pref,str) ==
  -- sees if the first #pref letters of str are pref
  -- replaces STRINGPREFIXP
  null (string?(pref) and string?(str)) => NIL
  (lp := QCSIZE pref) = 0 => true
  lp > QCSIZE str => NIL
  ok := true
  i := 0
  while ok and (i < lp) repeat
    not EQ(SCHAR(pref,i),SCHAR(str,i)) => ok := NIL
    i := i + 1
  ok

stringChar2Integer(str,pos) ==
  -- replaces GETSTRINGDIGIT in UT LISP
  -- returns small integer represented by character in position pos
  -- in string str. Returns NIL if not a digit or other error.
  if IDENTP str then str := PNAME str
  null (string?(str) and
    integer?(pos) and (pos >= 0) and (pos < QCSIZE(str))) => NIL
  not DIGITP(d := SCHAR(str,pos)) => NIL
  DIG2FIX d

dropLeadingBlanks str ==
  str := object2String str
  l := QCSIZE str
  nb := NIL
  i := 0
  while (i < l) and not nb repeat
    if SCHAR(str,i) ~= char " " then nb := i
    else i := i + 1
  nb = 0 => str
  nb => SUBSTRING(str,nb,NIL)
  '""

concat(:l) == concatList l

concatList [x,:y] ==
  null y => x
  null x => concatList y
  concat1(x,concatList y)

concat1(x,y) ==
  null x => y
  atom x => (null y => x; atom y => [x,y]; [x,:y])
  null y => x
  atom y => [:x,y]
  [:x,:y]

--% BOOT ravel and reshape

ravel a == a

reshape(a,b) == a

--% Some functions for algebra code

boolODDP x == ODDP x

--% Miscellaneous

freeOfSharpVars x ==
  atom x => not isSharpVarWithNum x
  freeOfSharpVars first x and freeOfSharpVars rest x

listOfSharpVars x ==
  atom x => (isSharpVarWithNum x => LIST x; nil)
  union(listOfSharpVars first x,listOfSharpVars rest x)

listOfPatternIds x ==
  isPatternVar x => [x]
  atom x => nil
  x is ['QUOTE,:.] => nil
  UNIONQ(listOfPatternIds first x,listOfPatternIds rest x)

isPatternVar v ==
  -- a pattern variable consists of a star followed by a star or digit(s)
  IDENTP(v) and v in '(_*_* _*1 _*2 _*3 _*4 _*5 _*6 _*7 _*8 _*9 _*10
    _*11 _*12 _*13 _*14 _*15 _*16 _*17 _*18 _*19 _*20) and true

removeZeroOne x ==
  -- replace all occurrences of (Zero) and (One) with
  -- 0 and 1
  x = $Zero => 0
  x = $One => 1
  atom x => x
  [removeZeroOne first x,:removeZeroOne rest x]

removeZeroOneDestructively t ==
  -- replace all occurrences of (Zero) and (One) with
  -- 0 and 1 destructively
  t = $Zero => 0
  t = $One => 1
  atom t => t
  RPLNODE(t,removeZeroOneDestructively first t,
    removeZeroOneDestructively rest t)

flattenSexpr s ==
  null s => s
  atom s => s
  [f,:r] := s
  atom f => [f,:flattenSexpr r]
  [:flattenSexpr f,:flattenSexpr r]

isLowerCaseLetter c ==
  LOWER_-CASE_-P c

isUpperCaseLetter c ==
  UPPER_-CASE_-P c

isLetter c ==
  ALPHA_-CHAR_-P c

update() ==
  runCommand
    STRCONC(textEditor(), '" ",STRINGIMAGE _/VERSION,'" ",STRINGIMAGE _/WSNAME,'" A")
  _/UPDATE()

--% Inplace Merge Sort for Lists
-- MBM April/88

-- listSort(pred,list) or listSort(pred,list,key)
-- the pred function is a boolean valued function defining the ordering
-- the key function extracts the key from an item for comparison by pred

listSort(pred,list,:optional) ==
   NOT functionp pred => error "listSort: first arg must be a function"
   NOT LISTP list => error "listSort: second argument must be a list"
   null optional => mergeSort(pred,function Identity,list,LENGTH list)
   key := first optional
   NOT functionp key => error "listSort: last arg must be a function"
   mergeSort(pred,key,list,LENGTH list)

-- non-destructive merge sort using NOT GGREATERP as predicate
MSORT list == listSort(function GLESSEQP, COPY_-LIST list)

-- destructive merge sort using NOT GGREATERP as predicate
NMSORT list == listSort(function GLESSEQP, list)

-- non-destructive merge sort using ?ORDER as predicate
orderList l == listSort(function _?ORDER, COPY_-LIST l)

-- dummy defn until clean-up
-- order     l == orderList l

mergeInPlace(f,g,p,q) ==
   -- merge the two sorted lists p and q
   if null p then return p
   if null q then return q
   if FUNCALL(f,FUNCALL(g, QCAR p),FUNCALL(g, QCAR q))
   then (r := t := p; p := QCDR p)
   else (r := t := q; q := QCDR q)
   while not null p and not null q repeat
      if FUNCALL(f,FUNCALL(g,QCAR p),FUNCALL(g,QCAR q))
      then (QRPLACD(t,p); t := p; p := QCDR p)
      else (QRPLACD(t,q); t := q; q := QCDR q)
   if null p then QRPLACD(t,q) else QRPLACD(t,p)
   r

mergeSort(f,g,p,n) ==
   if n=2 and FUNCALL(f,FUNCALL(g,QCADR p),FUNCALL(g,QCAR p)) then
      t := p
      p := QCDR p
      QRPLACD(p,t)
      QRPLACD(t,NIL)
   if QSLESSP(n,3) then return p
   -- split the list p into p and q of equal length
   l := QSQUOTIENT(n,2)
   t := p
   for i in 1..l-1 repeat t := QCDR t
   q := rest t
   QRPLACD(t,NIL)
   p := mergeSort(f,g,p,l)
   q := mergeSort(f,g,q,QSDIFFERENCE(n,l))
   mergeInPlace(f,g,p,q)

--% Throwing with glorious highlighting (maybe)

spadThrow() ==
  if $interpOnly and $mapName then
    putHist($mapName,'localModemap, nil, $e)
  THROW($SpadReaderTag,nil)

spadThrowBrightly x ==
  sayBrightly x
  spadThrow()

--% Type Formatting Without Abbreviation

formatUnabbreviatedSig sig ==
  null sig => ['"() -> ()"]
  [target,:args] := dollarPercentTran sig
  target := formatUnabbreviated target
  null args => ['"() -> ",:target]
  null rest args => [:formatUnabbreviated QCAR args,'" -> ",:target]
  args := formatUnabbreviatedTuple args
  ['"(",:args,'") -> ",:target]

formatUnabbreviatedTuple t ==
  -- t is a list of types
  null t => t
  atom t => [t]
  t0 := formatUnabbreviated QCAR t
  null rest t => t0
  [:t0,'",",:formatUnabbreviatedTuple QCDR t]

formatUnabbreviated t ==
  null t =>
    ['"()"]
  atom t =>
    [t]
  t is [p,sel,arg] and p = ":" =>
    [sel,'": ",:formatUnabbreviated arg]
  t is ['Union,:args] =>
    ['Union,'"(",:formatUnabbreviatedTuple args,'")"]
  t is ['Mapping,:args] =>
    formatUnabbreviatedSig args
  t is ['Record,:args] =>
    ['Record,'"(",:formatUnabbreviatedTuple args,'")"]
  t is [arg] =>
    t
  t is [arg,arg1] =>
    [arg,'" ",:formatUnabbreviated arg1]
  t is [arg,:args] =>
    [arg,'"(",:formatUnabbreviatedTuple args,'")"]
  t

sublisNQ(al,e) ==
  atom al => e
  fn(al,e) where fn(al,e) ==
    atom e =>
      for x in al repeat
        EQ(first x,e) => return (e := rest x)
      e
    EQ(a := first e,'QUOTE) => e
    u := fn(al,a)
    v := fn(al,rest e)
    EQ(a,u) and EQ(rest e,v) => e
    [u,:v]

opOf: %Thing -> %Thing
opOf x ==
  atom x => x
  first x


getProplist: (%Thing,%Env) -> %List
search: (%Thing,%Env) -> %List
searchCurrentEnv: (%Thing,%List) -> %List
searchTailEnv: (%Thing,%Env) -> %List

getProplist(x,E) ==
  not atom x => getProplist(first x,E)
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
    if u:= ASSQ(x,contour) then return (signal:= u)
  KDR signal

searchTailEnv(x,e) ==
  for env in e repeat
    signal:=
      for contour in env repeat
        if (u:= ASSQ(x,contour)) and ASSQ("FLUID",u) then return (signal:= u)
      if signal then return signal
  KDR signal

augProplist(proplist,prop,val) ==
  $InteractiveMode => augProplistInteractive(proplist,prop,val)
  while (proplist is [[ =prop,:.],:proplist']) repeat proplist:= proplist'
  val=(u:= LASSOC(prop,proplist)) => proplist
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
    LASSOC("value",proplist) or LASSOC("mode",proplist) => warnLiteral x
  prop in '(mode value) =>
    LASSOC("isLiteral",proplist) => warnLiteral x

addBinding(var,proplist,e is [[curContour,:tailContour],:tailEnv]) ==
  EQ(proplist,getProplist(var,e)) => e
  $InteractiveMode => addBindingInteractive(var,proplist,e)
  if curContour is [[ =var,:.],:.] then curContour:= rest curContour
                 --Previous line should save some space
  [[[lx,:curContour],:tailContour],:tailEnv] where lx:= [var,:proplist]

addBindingInteractive(var,proplist,e is [[curContour,:.],:.]) ==
  -- change proplist of var in e destructively
  u := ASSQ(var,curContour) =>
    RPLACD(u,proplist)
    e
  RPLAC(CAAR e,[[var,:proplist],:curContour])
  e

augProplistInteractive(proplist,prop,val) ==
  u := ASSQ(prop,proplist) =>
    RPLACD(u,val)
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


$blank == char ('_ )

trimString s ==
  leftTrim rightTrim s

leftTrim s ==
  k := MAXINDEX s
  k < 0 => s
  s.0 = $blank =>
    for i in 0..k while s.i = $blank repeat (j := i)
    SUBSTRING(s,j + 1,nil)
  s

rightTrim s ==  -- assumed a non-empty string
  k := MAXINDEX s
  k < 0 => s
  s.k = $blank =>
    for i in k..0 by -1 while s.i = $blank repeat (j := i)
    SUBSTRING(s,0,j)
  s

pp x ==
  PRETTYPRINT x
  x

pr x ==
  F_,PRINT_-ONE x
  nil

quickAnd(a,b) ==
  a = true => b
  b = true => a
  a = false or b = false => false
  simpBool ['AND,a,b]

quickOr(a,b) ==
  a = true or b = true => true
  b = false => a
  a = false => b
  simpCatPredicate simpBool ['OR,a,b]

intern x ==
  string? x =>
    DIGITP x.0 => string2Integer x
    INTERN x
  x

isDomain a ==
  cons? a and VECP(first a) and
    member(first a.0, $domainTypeTokens)

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
$charPlus == char '_+
$charBlank == (char '_ )
$charLbrace == char '_{
$charRbrace == char '_}
$charBack == char '_\
$charDash == char '_-

$charTab            == CODE_-CHAR(9)
$charNewline        == CODE_-CHAR(10)
$charFauxNewline    == CODE_-CHAR(25)
$stringNewline      == PNAME CODE_-CHAR(10)
$stringFauxNewline  == PNAME CODE_-CHAR(25)

$charExclusions == [char 'a, char 'A]
$charQuote == char '_'
$charSemiColon == char '_;
$charComma     == char '_,
$charPeriod    == char '_.
$checkPrenAlist := [[char '_(,:char '_)],[char '_{,:char '_}],[char '_[,:char '_]]]
$charEscapeList:= [char '_%,char '_#,$charBack]
$charIdentifierEndings := [char '__, char '_!, char '_?]
$charSplitList := [$charComma,$charPeriod,char '_[, char '_],$charLbrace, $charRbrace, char '_(, char '_), char '_$, char '_%]
$charDelimiters := [$charBlank, char '_(, char '_), $charBack]
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

isDefaultPackageName x == (s := PNAME x).(MAXINDEX s) = char '_&


-- gensym utils

charDigitVal c ==
  digits := '"0123456789"
  n := -1
  for i in 0..#digits-1 while n < 0 repeat
      if c = digits.i then n := i
  n < 0 => error '"Character is not a digit"
  n

gensymInt g ==
  not GENSYMP g => error '"Need a GENSYM"
  p := PNAME g
  n := 0
  for i in 2..#p-1 repeat n := 10 * n + charDigitVal p.i
  n

++ Returns a newly allocated domain shell (a simple vector) of length `n'.
newShell: %Short -> SIMPLE_-ARRAY
newShell n ==
  MAKE_-ARRAY(n,KEYWORD::INITIAL_-ELEMENT,nil)

++ fetchs the item in the nth entry of a domain shell.
getShellEntry: (%Shell,%Short) -> %Thing
getShellEntry(s,i) ==
  SVREF(s,i)

++ sets the nth nth entry of a domain shell to an item.
setShellEntry: (%Shell,%Short,%Thing) -> %Thing
setShellEntry(s,i,t) ==
  SETF(SVREF(s,i),t)


-- Push into the BOOT package when invoked in batch mode.
AxiomCore::$sysScope := '"BOOT"
