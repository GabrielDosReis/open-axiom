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


import g_-util
import g_-opt
namespace BOOT

module c_-util where
  clearReplacement: %Symbol -> %Thing
  replaceSimpleFunctions: %Form -> %Form
  foldExportedFunctionReferences: %List -> %List
  diagnoseUnknownType: (%Mode,%Env) -> %Form
  declareUnusedParameters: (%List,%Code) -> %List
  registerFunctionReplacement: (%Symbol,%Form) -> %Thing
  getFunctionReplacement: %Symbol -> %Form
  getSuccessEnvironment: (%Form,%Env) -> %Env
  getInverseEnvironment: (%Form,%Env) -> %Env


--% 
++ if true continue compiling after errors
$scanIfTrue := false


++ If within a loop, which kind? (list comprehension or plain old loop)
$loopKind := nil

++ If within a loop, the program point for the start of the body.
$repeatBodyLabel := nil

++ The number of occurrance of `iterate' in a (plain old) loop.
$iterateCount := nil

++ The number of occurrance of `break' in a (plain old) loop.
$breakCount := 0


+++ If non nil, holds compiled value of 'Rep' of the current domain.
$Representation := nil


$formalArgList := []

++ The formal body of the category being currently compiled.
$currentCategoryBody := nil

$compErrorMessageStack := nil

--% Optimization control

++ true if we have to proclaim function signatures in the generated Lisp.
$optProclaim := false

++ true if we have to inline simple functions before codegen.
$optReplaceSimpleFunctions := false

++ true if we have to resolve references to exported operations.
$optExportedFunctionReference := false

--%

++ Quote form, if not a basic value.
quoteMinimally form ==
  FIXP form or STRINGP form or form = nil or form = true => form
  ["QUOTE",form]

++ If using old `Rep' definition semantics, return `$' when m is `Rep'.
++ Otherwise, return `m'.
dollarIfRepHack m ==
  m = "Rep" and $useRepresentationHack => "$"
  m

++ The inverse of the above.
RepIfRepHack m ==
  m = "$" and $useRepresentationHack => "Rep"
  m

++ If using old `Rep' definition semantics, return `$' is m is `Rep'.
-- ??? Eventually this and the above should be merged and/or removed.
substituteDollarIfRepHack m ==
  $useRepresentationHack => substitute("$","Rep",m)
  m

++ Return the triple for the representation domain for the
++ current functor, if any.
getRepresentation: %Env -> %Maybe %Mode
getRepresentation e ==
  (get("Rep","value",e) or return nil).expr


++ Returns true if the form `t' is an instance of the Tuple constructor.
isTupleInstance: %Form -> %Boolean
isTupleInstance t ==
  t is ["Tuple",.]

++ Returns true if the signature `sig' describes a function that can
++ accept a homogeneous variable length argument list.
isHomoegenousVarargSignature: %Signature -> %Boolean
isHomoegenousVarargSignature sig ==
  #sig = 1 and isTupleInstance first sig

++ Returns true if the arguments list `args' match in shape the
++ parameter type list `sig'.  This means that either the number
++ of arguments is exactly the number of parameters, or that the
++ signature describes a homogeneous vararg operation.
enoughArguments: (%List,%Signature) -> %Boolean
enoughArguments(args,sig) ==
  #args = #sig or isHomoegenousVarargSignature sig

++ Returns true if the operation described by the signature `sig'
++ wants its arguments as a Tuple object.
wantArgumentsAsTuple: (%List,%Signature) -> %Boolean
wantArgumentsAsTuple(args,sig) ==
  isHomoegenousVarargSignature sig and #args ~= #sig

++ We are about to seal the (Lisp) definition of a function.
++ Augment the `body' with a declaration for those `parms'
++ that are unused.
declareUnusedParameters(parms,body) ==
  unused := [p for p in parms | not CONTAINED(p,body)]
  null unused => [body]
  [["DECLARE",["IGNORE",:unused]],body]

devaluate d ==
  not REFVECP d => d
  QSGREATERP(QVSIZE d,5) and getShellEntry(d,3) is ['Category] => 
    getShellEntry(d,0)
  QSGREATERP(QVSIZE d,0) =>
    d':=getShellEntry(d,0)
    isFunctor d' => d'
    d
  d
 
devaluateList l == [devaluate d for d in l]
 
devaluateDeeply x ==
  VECP x => devaluate x
  atom x => x
  [devaluateDeeply y for y in x]

--% Debugging Functions
 
--CONTINUE() == continue()
continue() == FIN comp($x,$m,$f)
 
LEVEL(:l) == APPLY('level,l)
level(:l) ==
  null l => same()
  l is [n] and INTEGERP n => displayComp ($level:= n)
  SAY '"Correct format: (level n) where n is the level you want to go to"
 
UP() == up()
up() == displayComp ($level:= $level-1)
 
SAME() == same()
same() == displayComp $level
 
DOWN() == down()
down() == displayComp ($level:= $level+1)
 
displaySemanticErrors() ==
  n:= #($semanticErrorStack:= REMDUP $semanticErrorStack)
  n=0 => nil
  l:= nreverse $semanticErrorStack
  $semanticErrorStack:= nil
  sayBrightly bright '"  Semantic Errors:"
  displaySemanticError(l,$OutputStream)
  sayBrightly '" "
  displayWarnings()
 
displaySemanticError(l,stream) ==
  for x in l for i in 1.. repeat
    sayBrightly(['"      [",i,'"] ",:first x],stream)
 
displayWarnings() ==
  n:= #($warningStack:= REMDUP $warningStack)
  n=0 => nil
  sayBrightly bright '"  Warnings:"
  l := nreverse $warningStack
  displayWarning(l,$OutputStream)
  $warningStack:= nil
  sayBrightly '" "
 
displayWarning(l,stream) ==
  for x in l for i in 1.. repeat
    sayBrightly(['"      [",i,'"] ",:x],stream)
 
displayComp level ==
  $bright:= " << "
  $dim:= " >> "
  if $insideCapsuleFunctionIfTrue=true then
    sayBrightly ['"error in function",:bright $op,'%l]
  --mathprint removeZeroOne mkErrorExpr level
  pp removeZeroOne mkErrorExpr level
  sayBrightly ['"****** level",:bright level,'" ******"]
  [$x,$m,$f,$exitModeStack]:= ELEM($s,level)
  SAY("$x:= ",$x)
  SAY("$m:= ",$m)
  SAY "$f:="
  F_,PRINT_-ONE $f
  nil
 
mkErrorExpr level ==
  bracket ASSOCLEFT DROP(level-#$s,$s) where
    bracket l ==
      #l<2 => l
      l is [a,b] =>
        highlight(b,a) where
          highlight(b,a) ==
            atom b =>
              substitute(var,b,a) where
                var:= INTERN STRCONC(STRINGIMAGE $bright,STRINGIMAGE b,STRINGIMAGE $dim)
            highlight1(b,a) where
              highlight1(b,a) ==
                atom a => a
                a is [ =b,:c] => [$bright,b,$dim,:c]
                [highlight1(b,first a),:highlight1(b,rest a)]
      substitute(bracket rest l,second l,first l)
 
compAndTrace [x,m,e] ==
  SAY("tracing comp, compFormWithModemap of: ",x)
  TRACE_,1(["comp","compFormWithModemap"],nil)
  T:= comp(x,m,e)
  UNTRACE_,1 "comp"
  UNTRACE_,1 "compFormWithModemap"
  T
 
errorRef s == 
  stackWarning('"%1b has no value", [s])
 
unErrorRef s == 
  unStackWarning('"'%1b has no value",[s])
 
--% ENVIRONMENT FUNCTIONS
 
consProplistOf(var,proplist,prop,val) ==
  semchkProplist(var,proplist,prop,val)
  $InteractiveMode and (u:= assoc(prop,proplist)) =>
    RPLACD(u,val)
    proplist
  [[prop,:val],:proplist]
 
warnLiteral x ==
  stackWarning('"%1b is BOTH a variable a literal",[x])
 
intersectionEnvironment(e,e') ==
  ce:= makeCommonEnvironment(e,e')
  ic:= intersectionContour(deltaContour(e,ce),deltaContour(e',ce))
  e'':= (ic => addContour(ic,ce); ce)
  --$ie:= e''   this line is for debugging purposes only
 
deltaContour([[c,:cl],:el],[[c',:cl'],:el']) ==
  not EQ(el,el') => systemError '"deltaContour" --a cop out for now
  eliminateDuplicatePropertyLists contourDifference(c,c') where
    contourDifference(c,c') == [first x for x in tails c while (x~=c')]
    eliminateDuplicatePropertyLists contour ==
      contour is [[x,:.],:contour'] =>
        LASSOC(x,contour') =>
                               --save some CONSing if possible
          [first contour,:DELLASOS(x,eliminateDuplicatePropertyLists contour')]
        [first contour,:eliminateDuplicatePropertyLists contour']
      nil
 
intersectionContour(c,c') ==
  $var: local
  computeIntersection(c,c') where
    computeIntersection(c,c') ==
      varlist:= REMDUP ASSOCLEFT c
      varlist':= REMDUP ASSOCLEFT c'
      interVars:= intersection(varlist,varlist')
      unionVars:= union(varlist,varlist')
      diffVars:= setDifference(unionVars,interVars)
      modeAssoc:= buildModeAssoc(diffVars,c,c')
      [:modeAssoc,:
        [[x,:proplist]
          for [x,:y] in c | member(x,interVars) and
            (proplist:= interProplist(y,LASSOC($var:= x,c')))]]
    interProplist(p,p') ==
                            --p is new proplist; p' is old one
      [:modeCompare(p,p'),:[pair' for pair in p | (pair':= compare(pair,p'))]]
    buildModeAssoc(varlist,c,c') ==
      [[x,:mp] for x in varlist | (mp:= modeCompare(LASSOC(x,c),LASSOC(x,c')))]
    compare(pair is [prop,:val],p') ==
      --1. if the property-value pair are identical, accept it immediately
      pair=(pair':= assoc(prop,p')) => pair
      --2. if property="value" and modes are unifiable, give intersection
      --       property="value" but value=genSomeVariable)()
      (val':= KDR pair') and prop="value" and
        (m:= unifiable(val.mode,val'.mode)) => ["value",genSomeVariable(),m,nil]
            --this tells us that an undeclared variable received
            --two different values but with identical modes
      --3. property="mode" is covered by modeCompare
      prop="mode" => nil
    modeCompare(p,p') ==
      pair:= assoc("mode",p) =>
        pair':= assoc("mode",p') =>
          m'':= unifiable(rest pair,rest pair') => LIST ["mode",:m'']
          stackSemanticError(['%b,$var,'%d,"has two modes: "],nil)
       --stackWarning ("mode for",'%b,$var,'%d,"introduced conditionally")
        LIST ["conditionalmode",:rest pair]
        --LIST pair
       --stackWarning ("mode for",'%b,$var,'%d,"introduced conditionally")
      pair':= assoc("mode",p') => LIST ["conditionalmode",:rest pair']
        --LIST pair'
    unifiable(m1,m2) ==
      m1=m2 => m1
        --we may need to add code to coerce up to tagged unions
        --but this can not be done here, but should be done by compIf
      m:=
        m1 is ["Union",:.] =>
          m2 is ["Union",:.] => ["Union",:S_+(rest m1,rest m2)]
          ["Union",:S_+(rest m1,[m2])]
        m2 is ["Union",:.] => ["Union",:S_+(rest m2,[m1])]
        ["Union",m1,m2]
      for u in getDomainsInScope $e repeat
        if u is ["Union",:u'] and (and/[member(v,u') for v in rest m]) then
          return m
        --this loop will return NIL if not satisfied
 
addContour(c,E is [cur,:tail]) ==
  [NCONC(fn(c,E),cur),:tail] where
    fn(c,e) ==
        for [x,:proplist] in c repeat
           fn1(x,proplist,getProplist(x,e)) where
              fn1(x,p,ee) ==
                for pv in p repeat fn3(x,pv,ee) where
                 fn3(x,pv,e) ==
                   [p,:v]:=pv
                   if member(x,$getPutTrace) then
                     pp([x,"has",pv])
                   if p="conditionalmode" then
                     RPLACA(pv,"mode")
                     --check for conflicts with earlier mode
                     if vv:=LASSOC("mode",e) then
                        if v ~=vv then
                          stackWarning('"The conditional modes %1p and %2p conflict",
                            [v,vv])
        LIST c
 
makeCommonEnvironment(e,e') ==
  interE makeSameLength(e,e') where  --$ie:=
    interE [e,e'] ==
      rest e=rest e' => [interLocalE makeSameLength(first e,first e'),:rest e]
      interE [rest e,rest e']
    interLocalE [le,le'] ==
      rest le=rest le' =>
        [interC makeSameLength(first le,first le'),:rest le]
      interLocalE [rest le,rest le']
    interC [c,c'] ==
      c=c' => c
      interC [rest c,rest c']
    makeSameLength(x,y) ==
      fn(x,y,#x,#y) where
        fn(x,y,nx,ny) ==
          nx>ny => fn(rest x,y,nx-1,ny)
          nx<ny => fn(x,rest y,nx,ny-1)
          [x,y]

++ Return the lexically leftmost location in an assignment for.
lhsOfAssignment x ==
  x is ["%LET",lhs,:.] => lhsOfAssignment lhs
  x

getSuccessEnvironment(a,e) ==
  a is ["is",id,m] =>
    id := lhsOfAssignment id
    IDENTP id and isDomainForm(m,$EmptyEnvironment) =>
      e:=put(id,"specialCase",m,e)
      currentProplist:= getProplist(id,e)
      [.,.,e] := T := comp(m,$EmptyMode,e) or return nil -- duplicates compIs
      newProplist:= consProplistOf(id,currentProplist,"value",[m,:rest removeEnv T])
      addBinding(id,newProplist,e)
    e
  a is ["case",x,m] and (x := lhsOfAssignment x) and IDENTP x =>
    put(x,"condition",[a,:get(x,"condition",e)],e)
  a is ["and",:args] =>
    for form in args repeat
      e := getSuccessEnvironment(form,e)
    e
  a is ["not",a'] => getInverseEnvironment(a',e)
  e

getInverseEnvironment(a,e) ==
  a is ["case",x,m] and (x := lhsOfAssignment x) and IDENTP x =>
    --the next two lines are necessary to get 3-branched Unions to work
    -- old-style unions, that is
    (get(x,"condition",e) is [["OR",:oldpred]]) and member(a,oldpred) =>
      put(x,"condition",LIST MKPF(delete(a,oldpred),"OR"),e)
    getUnionMode(x,e) is ["Union",:l] =>
      l':= delete(m,l)
      for u in l' repeat
	 if u is ['_:,=m,:.] then l':= delete(u,l')
      newpred:= MKPF([["case",x,m'] for m' in l'],"OR")
      put(x,"condition",[newpred,:get(x,"condition",e)],e)
    e
  a is ["not",a'] => getSuccessEnvironment(a',e)
  e

printEnv E ==
  for x in E for i in 1.. repeat
    for y in x for j in 1.. repeat
      SAY('"******CONTOUR ",j,'", LEVEL ",i,'":******")
      for z in y repeat
        TERPRI()
        SAY("Properties Of: ",first z)
        for u in rest z repeat
          PRIN1 first u
          printString ": "
          PRETTYPRINT tran(rest u,first u) where
            tran(val,prop) ==
              prop="value" => DROP(-1,val)
              val
 
prEnv E ==
  for x in E for i in 1.. repeat
    for y in x for j in 1.. repeat
      SAY('"******CONTOUR ",j,'", LEVEL ",i,'":******")
      for z in y | not LASSOC("modemap",rest z) repeat
        TERPRI()
        SAY("Properties Of: ",first z)
        for u in rest z repeat
          PRIN1 first u
          printString ": "
          PRETTYPRINT tran(rest u,first u) where
            tran(val,prop) ==
              prop="value" => DROP(-1,val)
              val
 
prModemaps E ==
  listOfOperatorsSeenSoFar:= nil
  for x in E for i in 1.. repeat
    for y in x for j in 1.. repeat
      for z in y | null member(first z,listOfOperatorsSeenSoFar) and
        (modemap:= LASSOC("modemap",rest z)) repeat
          listOfOperatorsSeenSoFar:= [first z,:listOfOperatorsSeenSoFar]
          TERPRI()
          PRIN1 first z
          printString ": "
          PRETTYPRINT modemap
 
prTriple T ==
   SAY '"Code:"
   pp T.0
   SAY '"Mode:"
   pp T.1
 
TrimCF() ==
  new:= nil
  old:= CAAR $CategoryFrame
  for u in old repeat
    if not ASSQ(first u,new) then
      uold:= rest u
      unew:= nil
      for v in uold repeat if not ASSQ(first v,unew) then unew:= [v,:unew]
      new:= [[first u,:nreverse unew],:new]
  $CategoryFrame:= [[nreverse new]]
  nil

--%

isKnownCategory: (%Mode,%Env) -> %Boolean
isKnownCategory(c,e) ==
  c = $Type => true
  c = $Category => true
  [ctor,:args] := c
  ctor = "Join" => true           -- don't check arguments yet.
  ctor = "SubsetCategory" => true -- ditto
  get(ctor,"isCategory",e) => true
  false

--TRACE isKnownCategory
 
++ Returns non-nil if `t' is a known type in the environement `e'.
diagnoseUnknownType(t,e) ==
  atom t =>
    t in '($ constant) => t
    t' := assoc(t,getDomainsInScope e) => t'
    (m := getmode(t,e)) and isKnownCategory(m,$CategoryFrame) => t
    STRINGP t => t
    -- ??? We should not to check for $$ at this stage.  
    -- ??? This is a bug in the compiler that needs to be fixed.
    t = "$$" => t
    stackSemanticError(['"The identifier", :bright t, 
                         '"is not known to name a type"],nil)
  [ctor,:args] := t
  ctor = "Mapping" => 
    for t' in args repeat diagnoseUnknownType(t',e)
    t
  ctor = "Record" =>
    for [[.,n,t'],:fields] in tails args repeat 
      diagnoseUnknownType(t',e)
      for [.,=n,.] in fields repeat
        stackSemanticError(['"Field", :bright n, 
          '"declared more than once."], nil)
    t
  ctor = "Union" =>
    if args is [[":",:.],:.] then
      for [[.,n,t'],:fields] in tails args repeat 
        diagnoseUnknownType(t',e)
        for [.,=n,.] in fields repeat
          stackSemanticError(['"Field", :bright n, 
            '"declared more than once."], nil)
    else
      for t' in args repeat diagnoseUnknownType(t',e)
    t
  ctor = "Enumeration" =>
    for t' in args repeat
      IDENTP t' => nil
      stackSemanticError(['"Enumerators must be symbols."], nil)
    -- Make sure we don't have repeated symbolic values
    for [sym,:syms] in tails args repeat
      MEMQ(sym,syms) => 
        stackSemanticError(['"Symbolic value ", :bright sym, 
          '"is listed twice"], nil)
    t
  ctor = "[||]" => t
  ctor in $BuiltinConstructorNames => t -- ??? check Record and Union fields
  -- ??? Ideally `e' should be a local extension of $CategoryFrame
  -- ??? so that we don't have to access it here as a global state.
  get(ctor,"isFunctor",$CategoryFrame) 
    or get(ctor,"isCategory",$CategoryFrame) => t
  -- ctor maybe a constructor, but user forgot to import.  Warn.
  getConstructorAbbreviationFromDB ctor =>
    stackWarning('"Type %1pb is not in scope.  Import it",[t])
    t
  stackSemanticError(['"Identifier", :bright ctor, 
                       '"is not known to name a constructor"],nil)

--% PREDICATES
 
 
isConstantId(name,e) ==
  IDENTP name =>
    pl:= getProplist(name,e) =>
      (LASSOC("value",pl) or LASSOC("mode",pl) => false; true)
    true
  false
 
isFalse() == nil
 
isFluid s == atom s and "$"=(PNAME s).(0)
 
isFunction(x,e) ==
  get(x,"modemap",e) or GETL(x,"SPECIAL") or x="case" or getmode(x,e) is [
    "Mapping",:.]
 
isLiteral: (%Symbol,%Env) -> %Boolean
isLiteral(x,e) == 
  get(x,"isLiteral",e) => true
  false
 

makeLiteral: (%Symbol,%Env) -> %Thing
makeLiteral(x,e) == 
  put(x,"isLiteral","true",e)
 
isSomeDomainVariable s ==
  IDENTP s and #(x:= PNAME s)>2 and x.(0)="#" and x.(1)="#"

++ Return non-nil is the domain form `x' is a `subset' of domain
++ form `y' in the environment `e'.  The relation of subdomain
++ is understood as equivalent to the fact that all values of
++ the domain designated by `x' are also values of the domain
++ designated by `y'.  Examples include declaration of domain `x'
++ as satisfying  SubsetCategory(SomeCategory, y).  Or, when
++ x is defined as SubDomain(y,pred).  In that case, the predicate
++ is returned and its parameter is `#1'.
isSubset(x,y,e) ==
  x = y => true
  -- Every domain or catgory is a subset of Type.
  y = $Type => true
  -- When using the old style definition, the current domain
  -- is considered a subset of its representation domain
  x = "$" and y = "Rep" => $useRepresentationHack
  -- Expand domain representation form
  x = "Rep" and not $useRepresentationHack =>
    isSubset(getRepresentation e,y,e)
  -- Or, if x has the Subsets property set by SubsetCategory.
  pred := LASSOC(opOf x,get(opOf y,"Subsets",e)) => pred
  -- Or, they are related by subdomain chain.
  isDomainForm(x,e) and isSubDomain(x,y)
 
isDomainInScope(domain,e) ==
  domainList:= getDomainsInScope e
  atom domain =>
    MEMQ(domain,domainList) => true
    not IDENTP domain or isSomeDomainVariable domain => true
    false
  (name:= first domain)="Category" => true
  ASSQ(name,domainList) => true
--   null rest domain or domainMember(domain,domainList) => true
--   false
  isFunctor name => false
  true --is not a functor
 
isSimple x ==
  atom x or $InteractiveMode => true
  x is [op,:argl] and
    isSideEffectFree op and (and/[isSimple y for y in argl])
 
isSideEffectFree op ==
  member(op,$SideEffectFreeFunctionList) or op is ["elt",.,op'] and
    isSideEffectFree op'
 
isAlmostSimple x ==
  --returns (<new predicate> . <list of assignments>) or nil
  $assignmentList: local --$assigmentList is only used in this function
  transform:=
    fn x where
      fn x ==
        atom x or null rest x => x
        [op,y,:l]:= x
        op="has" => x
        op="is" => x
        op="%LET" =>
          IDENTP y => (setAssignment LIST x; y)
          (setAssignment [["%LET",g:= genVariable(),:l],["%LET",y,g]]; g)
        op = "case" and IDENTP y => x
        isSideEffectFree op => [op,:mapInto(rest x, function fn)]
        $assignmentList:= "failed"
      setAssignment x ==
        $assignmentList="failed" => nil
        $assignmentList:= [:$assignmentList,:x]
  $assignmentList="failed" => nil
  wrapSEQExit [:$assignmentList,transform]
 
incExitLevel u ==
  adjExitLevel(u,1,1)
  u
 
decExitLevel u ==
  (adjExitLevel(u,1,-1); removeExit0 u) where
    removeExit0 x ==
      atom x => x
      x is ["exit",0,u] => removeExit0 u
      [removeExit0 first x,:removeExit0 rest x]
 
adjExitLevel(x,seqnum,inc) ==
  atom x => x
  x is [op,:l] and op in '(SEQ REPEAT COLLECT) =>
    for u in l repeat adjExitLevel(u,seqnum+1,inc)
  x is ["exit",n,u] =>
    (adjExitLevel(u,seqnum,inc); seqnum>n => x; rplac(second x,n+inc))
  x is [op,:l] => for u in l repeat adjExitLevel(u,seqnum,inc)
 
wrapSEQExit l ==
  null rest l => first l
  [:c,x]:= [incExitLevel u for u in l]
  ["SEQ",:c,["exit",1,x]]
 
 
--% UTILITY FUNCTIONS
 
--appendOver x == "append"/x
 
removeEnv t == [t.expr,t.mode,$EmptyEnvironment]  -- t is a triple
 
-- This function seems no longer used
--ordinsert(x,l) ==
--  null l => [x]
--  x=first l => l
--  _?ORDER(x,first l) => [x,:l]
--  [first l,:ordinsert(x,rest l)]
 
makeNonAtomic x ==
  atom x => [x]
  x
 
flatten(l,key) ==
  null l => nil
  first l is [k,:r] and k=key => [:r,:flatten(rest l,key)]
  [first l,:flatten(rest l,key)]
 
genDomainVar() ==
  $Index:= $Index+1
  INTERNL STRCONC("#D",STRINGIMAGE $Index)
 
genVariable() ==
  INTERNL STRCONC("#G",STRINGIMAGE ($genSDVar:= $genSDVar+1))
 
genSomeVariable() ==
  INTERNL STRCONC("##",STRINGIMAGE ($genSDVar:= $genSDVar+1))
 
listOfIdentifiersIn x ==
  IDENTP x => [x]
  x is [op,:l] => REMDUP ("append"/[listOfIdentifiersIn y for y in l])
  nil
 
mapInto(x,fn) == [FUNCALL(fn,y) for y in x]
 
numOfOccurencesOf(x,y) ==
  fn(x,y,0) where
    fn(x,y,n) ==
      null y => 0
      x=y => n+1
      atom y => n
      fn(x,first y,n)+fn(x,rest y,n)
 
compilerMessage(msg,args) ==
  $PrintCompilerMessageIfTrue => sayPatternMsg(msg,args)
 
printDashedLine() ==
  SAY
   '"--------------------------------------------------------------------------"
 
stackSemanticError(msg,expr) ==
  BUMPERRORCOUNT "semantic"
  if $insideCapsuleFunctionIfTrue then msg:= [$op,": ",:msg]
  if atom msg then msg:= LIST msg
  entry:= [msg,expr]
  if not member(entry,$semanticErrorStack) then $semanticErrorStack:=
    [entry,:$semanticErrorStack]
  $scanIfTrue and $insideCapsuleFunctionIfTrue=true and #$semanticErrorStack-
    $initCapsuleErrorCount>3 => THROW("compCapsuleBody",nil)
  nil
 
stackWarning(msg,args == nil) ==
  msg := buildMessage(msg, args)
  if $insideCapsuleFunctionIfTrue then msg:= [$op,": ",:msg]
  if not member(msg,$warningStack) then $warningStack:= [msg,:$warningStack]
  nil
 
unStackWarning(msg,args) ==
  msg := buildMessage(msg,args)
  if $insideCapsuleFunctionIfTrue then msg:= [$op,": ",:msg]
  $warningStack:= EFFACE(msg,$warningStack)
  nil
 
stackMessage(msg,args == nil) ==
  if args ~= nil then
    msg := buildMessage(msg,args)
  $compErrorMessageStack:= [msg,:$compErrorMessageStack]
  nil
 
stackMessageIfNone msg ==
  --used in situations such as compForm where the earliest message is wanted
  if null $compErrorMessageStack then $compErrorMessageStack:=
    [msg,:$compErrorMessageStack]
  nil
 
stackAndThrow(msg, args == nil) ==
  if args ~= nil then
    msg := buildMessage(msg,args)
  $compErrorMessageStack:= [msg,:$compErrorMessageStack]
  THROW("compOrCroak",nil)
 
printString x == PRINTEXP (STRINGP x => x; PNAME x)
 
printAny x == if atom x then printString x else PRIN1 x
 
printSignature(before,op,[target,:argSigList]) ==
  printString before
  printString op
  printString ": _("
  if argSigList then
    printAny first argSigList
    for m in rest argSigList repeat (printString ","; printAny m)
  printString "_) -> "
  printAny target
  TERPRI()
 
elapsedTime() ==
  currentTime:= TEMPUS_-FUGIT()
  elapsedSeconds:= (currentTime-$previousTime)*QUOTIENT(1.0,$timerTicksPerSecond)
  $previousTime:= currentTime
  elapsedSeconds
 
addStats([a,b],[c,d]) == [a+c,b+d]
 
printStats [byteCount,elapsedSeconds] ==
  timeString := normalizeStatAndStringify elapsedSeconds
  if byteCount = 0 then SAY('"Time: ",timeString,'" SEC.") else
    SAY('"Size: ",byteCount,'" BYTES     Time: ",timeString,'" SEC.")
  TERPRI()
  nil
 
extendsCategoryForm(domain,form,form') ==
  --is domain of category form also of category form'?
  --domain is only used for SubsetCategory resolution.
  --and ensuring that X being a Ring means that it
  --satisfies (Algebra X)
  form=form' => true
  form=$Category => nil
  form' is ["Join",:l] => and/[extendsCategoryForm(domain,form,x) for x in l]
  form' is ["CATEGORY",.,:l] =>
    and/[extendsCategoryForm(domain,form,x) for x in l]
  form' is ["SubsetCategory",cat,dom] =>
    extendsCategoryForm(domain,form,cat) and isSubset(domain,dom,$e)
  form is ["Join",:l] => or/[extendsCategoryForm(domain,x,form') for x in l]
  form is ["CATEGORY",.,:l] =>
    member(form',l) or
      stackWarning('"not known that %1 is of mode %2p",[form',form]) or true
  -- if we are compiling the category `form', then we should look at
  -- the body as provided in the current definition, not a version
  -- possibly compiled previously that may have changed.
  -- FIXME: should not we go all the way down and implement
  --        polynormic recursion?
  domain = "$" and form = $definition => 
    extendsCategoryForm(domain, $currentCategoryBody, form')
  isCategoryForm(form,$EmptyEnvironment) =>
          --Constructs the associated vector
    formVec:=(compMakeCategoryObject(form,$e)).expr
            --Must be $e to pick up locally bound domains
    form' is ["SIGNATURE",op,args,:.] =>
        assoc([op,args],formVec.(1)) or
            assoc(SUBSTQ(domain,"$",[op,args]),
                  SUBSTQ(domain,"$",formVec.(1)))
    form' is ["ATTRIBUTE",at] =>
         assoc(at,formVec.2) or
            assoc(SUBSTQ(domain,"$",at),SUBSTQ(domain,"$",formVec.2))
    form' is ["IF",:.] => true --temporary hack so comp won't fail
    -- Are we dealing with an Aldor category?  If so use the "has" function ...
    # formVec = 1 => newHasTest(form,form')
    catvlist:= formVec.4
    member(form',first catvlist) or
     member(form',SUBSTQ(domain,"$",first catvlist)) or
      (or/
        [extendsCategoryForm(domain,SUBSTQ(domain,"$",cat),form')
          for [cat,:.] in second catvlist])
  nil
 
getmode(x,e) ==
  prop:=getProplist(x,e)
  u:= LASSQ("value",prop) => u.mode
  LASSQ("mode",prop)
 
getmodeOrMapping(x,e) ==
  u:= getmode(x,e) => u
  (u:= get(x,"modemap",e)) is [[[.,:map],.],:.] => ["Mapping",:map]
  nil
 
outerProduct l ==
                --of a list of lists
  null l => LIST nil
  "append"/[[[x,:y] for y in outerProduct rest l] for x in first l]
 
sublisR(al,u) ==
  atom u => u
  y:= rassoc(t:= [sublisR(al,x) for x in u],al) => y
  true => t
 
substituteOp(op',op,x) ==
  atom x => x
  [(op=(f:= first x) => op'; f),:[substituteOp(op',op,y) for y in rest x]]
 
--substituteForFormalArguments(argl,expr) ==
--  SUBLIS([[v,:a] for a in argl for v in $FormalMapVariableList],expr)
 
 -- following is only intended for substituting in domains slots 1 and 4
 -- signatures and categories
sublisV(p,e) ==
  (atom p => e; suba(p,e)) where
    suba(p,e) ==
      STRINGP e => e
      -- no need to descend vectors unless they are categories
      isCategory e => LIST2VEC [suba(p,e.i) for i in 0..MAXINDEX e]
      atom e => (y:= ASSQ(e,p) => rest y; e)
      u:= suba(p,QCAR e)
      v:= suba(p,QCDR e)
      EQ(QCAR e,u) and EQ(QCDR e,v) => e
      [u,:v]

--% DEBUGGING PRINT ROUTINES used in breaks
 
_?MODEMAPS x == _?modemaps x
_?modemaps x ==
  env:=
    $insideCapsuleFunctionIfTrue=true => $CapsuleModemapFrame
    $f
  x="all" => displayModemaps env
  -- displayOpModemaps(x,old2NewModemaps get(x,"modemap",env))
  displayOpModemaps(x,get(x,"modemap",env))


old2NewModemaps x ==
--  [[dcSig,pred] for [dcSig,[pred,:.],:.] in x]
  x is [dcSig,[pred,:.],:.]  =>  [dcSig,pred]
  x

traceUp() ==
  atom $x => sayBrightly "$x is an atom"
  for y in rest $x repeat
    u:= comp(y,$EmptyMode,$f) =>
      sayBrightly [y,'" ==> mode",'%b,u.mode,'%d]
    sayBrightly [y,'" does not compile"]
 
_?M x == _?m x
_?m x ==
  u:= comp(x,$EmptyMode,$f) => u.mode
  nil
 
traceDown() ==
  mmList:= getFormModemaps($x,$f) =>
    for mm in mmList repeat if u:= qModemap mm then return u
  sayBrightly "no modemaps for $x"
 
qModemap mm ==
  sayBrightly ['%b,"modemap",'%d,:formatModemap mm]
  [[dc,target,:sl],[pred,:.]]:= mm
  and/[qArg(a,m) for a in rest $x for m in sl] => target
  sayBrightly ['%b,"fails",'%d,'%l]
 
qArg(a,m) ==
  yesOrNo:=
    u:= comp(a,m,$f) => "yes"
    "no"
  sayBrightly [a," --> ",m,'%b,yesOrNo,'%d]
  yesOrNo="yes"
 
_?COMP x == _?comp x
_?comp x ==
  msg:=
    u:= comp(x,$EmptyMode,$f) =>
      [MAKESTRING "compiles to mode",'%b,u.mode,'%d]
    nil
  sayBrightly msg
 
_?domains() == pp getDomainsInScope $f
_?DOMAINS() == ?domains()
 
_?mode x == displayProplist(x,[["mode",:getmode(x,$f)]])
_?MODE x == _?mode x
 
_?properties x == displayProplist(x,getProplist(x,$f))
_?PROPERTIES x == _?properties x
 
_?value x == displayProplist(x,[["value",:get(x,"value",$f)]])
_?VALUE x == _?value x
 
displayProplist(x,alist) ==
  sayBrightly ["properties of",'%b,x,'%d,":"]
  fn alist where
    fn alist ==
      alist is [[prop,:val],:l] =>
        if prop="value" then val:= [val.expr,val.mode,'"..."]
        sayBrightly ["   ",'%b,prop,'%d,": ",val]
        fn deleteAssoc(prop,l)
 
displayModemaps E ==
  listOfOperatorsSeenSoFar:= nil
  for x in E for i in 1.. repeat
    for y in x for j in 1.. repeat
      for z in y | null member(first z,listOfOperatorsSeenSoFar) and
        (modemaps:= LASSOC("modemap",rest z)) repeat
          listOfOperatorsSeenSoFar:= [first z,:listOfOperatorsSeenSoFar]
          displayOpModemaps(first z,modemaps)
 
--% General object traversal functions
 
GCOPY ob == COPY ob  -- for now
 
--%
++ format the set of candidate operations.
displayAmbiguousSignatures(op,sigs) ==
  [:showCandidate(op, sig) for sig in sigs] where
     showCandidate(op,sig) ==
       ["%l", "        ", op, '": ", 
         :bright formatUnabbreviated ["Mapping",:sig]]

++ Display diagnostic message about ambiguous operation `op', with
++ possible resolutions given by the list `sigs'.
ambiguousSignatureError(op, sigs) ==
  stackSemanticError(['"signature of lhs not unique.  Candidates are:",
    :displayAmbiguousSignatures($op,sigs)],nil)


--% Capsule Directory Management

++ Holds the list of slot number-export function pairs of 
++ the current functor.
$capsuleDirectory := nil

clearCapsuleDirectory() ==
  $capsuleDirectory := nil
  $capsuleFunctionStack := nil

++ Return the linkage name of the exported operation associated with
++ slot number `slot'.  A nil entry means that either the operation
++ is not defined, or it is conditional.
getCapsuleDirectoryEntry slot ==
  rest ASSOC(slot,$capsuleDirectory)

++ Update the current capsule directory with entry controlled by 
++ predicate `pred'.
updateCapsuleDirectory(item,pred) ==
  pred ~= true => nil
  entry :=
    item is ["$",slot,["CONS",["dispatchFunction",fun],:.],:.] => [slot,:fun]
    item is ["$",slot,["CONS","IDENTITY",
              ["FUNCALL",["dispatchFunction",fun],"$"]]] => [slot,:fun]
    nil
  entry = nil => nil
  $capsuleDirectory := [entry,:$capsuleDirectory]




--% Tree walkers

++ Walk VM COND-form mutating sub-forms with unary
++ function `fun'
mutateCONDFormWithUnaryFunction(form,fun) ==
  form isnt ["COND",:body] => form
  for clauses in tails body repeat
    -- a clause is a list of forms
    for subForms in tails first clauses repeat
      rplac(first subForms, FUNCALL(fun, first subForms))
  form

++ Walk VM LET-form mutating enclosed expression forms with
++ unary function `fun'.  Every sub-form is visited except
++ local variable declarations, though their initializers
++ are visited.
mutateLETFormWithUnaryFunction(form,fun) ==
  form isnt ["LET",inits,:body] => form
  for defs in tails inits repeat
    def := first defs
    atom def => nil -- no initializer
    rplac(second def, FUNCALL(fun, second def))
  for stmts in tails body repeat
    rplac(first stmts, FUNCALL(fun, first stmts))
  form

--% 

++ List of macros used by the middle end to represent some
++ high level control structures.
-- NOTE: It is potentially dangerous to assume every occurrence of 
-- element of $middleEndMacroList is actually a macro call
$middleEndMacroList == 
  '(COLLECT REPEAT SUCHTHATCLAUSE THETA THETA1 SPADREDUCE SPADDO)

middleEndExpand: %Form -> %Form
middleEndExpand x ==
  isAtomicForm x => x
  first x in $middleEndMacroList =>
    middleEndExpand MACROEXPAND_-1 x
  a := middleEndExpand first x
  b := middleEndExpand rest x
  EQ(a,first x) and EQ(b,rest x) => x
  [a,:b]



-- A function is simple if it looks like a super combinator, and it
-- does not use its environment argument.  They can be safely replaced
-- by more efficient (hopefully) functions.

getFunctionReplacement: %Symbol -> %Form
getFunctionReplacement name ==
  GET(name, "SPADreplace")

++ remove any replacement info possibly associated with `name'.
clearReplacement name ==
  REMPROP(name,"SPADreplace")

++ Register the inlinable form of a function.
registerFunctionReplacement(name,body) ==
  LAM_,EVALANDFILEACTQ ["PUT",MKQ name,MKQ "SPADreplace",quoteMinimally body]

eqSubstAndCopy: (%List, %List, %Form) -> %Form
eqSubstAndCopy(args,parms,body) ==
  SUBLIS(pairList(parms,args),body,KEYWORD::TEST,function EQ)

eqSubst: (%List, %List, %Form) -> %Form
eqSubst(args,parms,body) ==
  NSUBLIS(pairList(parms,args),body,KEYWORD::TEST,function EQ)


++ returns true if `form' does not really induce computations.
isAtomicForm: %Form -> %Boolean
isAtomicForm form ==
  atom form or first form = "QUOTE"

++ Walk `form' and replace simple functions as appropriate.
replaceSimpleFunctions form ==
  isAtomicForm form => form
  form is ["COND",:body] =>
    mutateCONDFormWithUnaryFunction(form,"replaceSimpleFunctions")
  form is ["LET",:.] =>
    optLET mutateLETFormWithUnaryFunction(form,"replaceSimpleFunctions")
  form is ["spadConstant","$",n] =>
    null(op := getCapsuleDirectoryEntry n) => form
    getFunctionReplacement op is ["XLAM",=nil,body] 
      and isAtomicForm body => body
    -- Conservatively preserve object identity and storage 
    -- consumption by not folding non-atomic constant forms.
    form
  -- 1. process argument first.
  for args in tails rest form repeat
    arg' := replaceSimpleFunctions(arg := first args)
    not EQ(arg',arg) =>
      rplac(first args, arg')
  -- 2. see if we know something about this function.
  [fun,:args] := form
  atom fun =>
    null (fun' := getFunctionReplacement fun) => form
    -- 2.1. the renaming case.
    atom fun' =>
      rplac(first form,fun')
      NBUTLAST form
    -- 2.2. the substitution case.
    fun' is ["XLAM",parms,body] =>
      -- Inline almost constant functions.
      null parms => body
      -- Identity function toos.
      parms is [=body] => first args
      -- conservatively approximate eager semantics
      and/[isAtomicForm first as for as in tails args] =>
        -- alpha rename before substitution.
	newparms := [GENSYM() for p in parms]
	body := eqSubstAndCopy(newparms,parms,body)
	eqSubst(args,newparms,body)
      -- get cute later.
      form
    form
  fun' := replaceSimpleFunctions fun
  not EQ(fun',fun) => rplac(first form,fun')
  form


++ We are processing a function definition with parameter list `vars'
++ and body given by `body'.  If `body' is a forwarding function call, 
++ return the target function.  Otherwise, return nil.
forwardingCall?(vars,body) ==
  vars is [:vars',.] and body is [fun,: =vars'] and IDENTP fun => fun
  nil


++ Return true if `form' has a linear usage of all variables in `vars'.
usesVariablesLinearly?(form,vars) ==
  isAtomicForm form => true
  and/[numOfOccurencesOf(var,form) < 2 for var in vars] 

++ We are processing a function definition with parameter list `vars'
++ and body given by `body'.  If `body' is a form that can be inlined,
++ then return the inline form.  Otherwise, return nil.
expandableDefinition?(vars,body) ==
  expand? :=
    -- We definitely don't want to expand a form that uses
    -- the domain of computation environment.
    vars isnt [:vars',env] or CONTAINED(env,body) => false

    -- Constants are currently implemented as niladic functions, and
    -- we want to avoid disturbing object identity, so we rule
    -- out use of side-effect full operators.  
    -- FIXME: This should be done only for constant creators.
    null vars' => semiSimpleRelativeTo?(body,$VMsideEffectFreeOperators)

    isAtomicForm body => true
    [op,:args] := body
    not IDENTP op => false
    and/[isAtomicForm x for x in args] 
      or semiSimpleRelativeTo?(body,$simpleVMoperators) =>
                usesVariablesLinearly?(body,vars')
    false
  expand? => ["XLAM",vars',body]
  nil

++ Replace all SPADCALLs to operations defined in the current
++ domain.  Conditional operations are not folded.
foldSpadcall: %Form -> %Form
foldSpadcall form ==
  isAtomicForm form => form           -- leave atomic forms alone
  form is ["DECLARE",:.] => form      -- don't walk declarations
  form is ["LET",inits,:body] =>
    mutateLETFormWithUnaryFunction(form,"foldSpadcall")
  form is ["COND",:stmts] =>
    mutateCONDFormWithUnaryFunction(form,"foldSpadcall")
  for args in tails rest form repeat
    foldSpadcall first args
  first form ~= "SPADCALL" => form
  fun := lastNode form
  fun isnt [["getShellEntry","$",slot]] => form
  null (op := getCapsuleDirectoryEntry slot) => form
  rplac(first fun, "$")
  rplac(first form, op)


++ `defs' is a list of function definitions from the current domain.
++ Walk that list and replace references to unconditional operations
++ with their corresponding linkage names.  
foldExportedFunctionReferences defs ==
  for fun in defs repeat
    fun isnt [name,lamex] => nil
    lamex isnt ["LAM",vars,body] => nil
    body := replaceSimpleFunctions foldSpadcall body
    form := expandableDefinition?(vars,body) =>
      registerFunctionReplacement(name,form)
      rplac(second fun, ["LAM",vars,["DECLARE",["IGNORE",last vars]],body])
    rplac(third lamex,body)
  defs

++ record optimizations permitted at level `level'.
setCompilerOptimizations level ==
  level = nil => nil
  INTEGERP level =>
    if level = 0 then
      -- explicit request for no optimization.
      $optProclaim := false
      $optReplaceSimpleFunctions := false
    if level > 0 then 
      $optProclaim := true
      $optReplaceSimpleFunctions := true
    if level > 1 then
      $optExportedFunctionReference := true
    if level > 2 then
      $optimizeRep := true
  coreError '"unknown optimization level request"


--% Lisp backend support.

++ Proclaim the type of the capsule function `op' with signature `sig'.
++ Note that all capsule functions take an additional argument 
++ standing for the domain of computation object.
proclaimCapsuleFunction(op,sig) ==
  LAM_,EVALANDFILEACTQ
    ["DECLAIM",["FTYPE",
       ["FUNCTION",[:[vmType first d for d in tails rest sig],"%Shell"], 
          vmType first sig],op]] where
      vmType d ==
        $subdomain and d = "$" =>
          -- We want accurate approximation for subdomains/superdomains
          -- that are specialized and known to the VM.
          (m := getVMType normalize $functorForm) = "%Thing" =>
             getVMType normalize "$"
          m
        getVMType normalize d
      normalize(d,top? == true) ==
        d = "$" => 
          not top? => "*"
          -- If the representation is explicitly stated, use it.  That way
          -- we optimize abstractions just as well as builtins.
          r := getRepresentation $e => normalize(r,top?)
          -- Cope with old-style constructor definition
          atom $functorForm => [$functorForm] 
          normalize($functorForm,top?)
        atom d => 
          top? => "%Thing"
          getmode(d,$e) => "*"
          d
        [first d, :[normalize(first args,false) for args in tails rest d]]

++ Lisp back end compiler for ILAM with `name', formal `args', and `body'.
backendCompileILAM: (%Symbol,%List, %Code) -> %Symbol
backendCompileILAM(name,args,body) ==
  args' := NLIST(#args, ["GENSYM"])
  body' := eqSubst(args',args,body)
  MAKEPROP(name,"ILAM",true)
  setDynamicBinding(name,["LAMBDA",args',:body'])
  name

$CLOSEDFNS := nil

MAKE_-CLOSEDFN_-NAME() ==
  INTERNL($FUNNAME,'"!", STRINGIMAGE # $CLOSEDFNS)

backendCompileNEWNAM: %Form -> %Void
backendCompileNEWNAM x ==
  isAtomicForm x => nil
  atom(y := first x) =>
    backendCompileNEWNAM rest x
    if y = "CLOSEDFN" then
      u := MAKE_-CLOSEDFN_-NAME()
      PUSH([u,second x], $CLOSEDFNS)
      RPLACA(x,"FUNCTION")
      RPLACA(rest x,u)
  backendCompileNEWNAM first x
  backendCompileNEWNAM rest x


++ Lisp back end compiler for SLAM forms [namd,args,:body].
++ A SLAM form is one that is `functional' in the sense that
++ its values are cached, so that equal lists of argument values
++ yield equal values.  The arguments-value pairs are stored
++ as alists.
backendCompileSLAM: (%Symbol,%List,%Code) -> %Symbol
backendCompileSLAM(name,args,body) ==
  al := INTERNL(name,'";AL")    -- name of the cache alist.
  auxfn := INTERNL(name,'";")   -- name of the worker function.
  g1 := GENSYM()                -- name for the parameter.
  g2 := GENSYM()                -- name for the cache value
  u :=                          -- body of the stub function
    null args => [nil,[auxfn]]
    null rest args => [[g1],[auxfn,g1]]
    [g1,["APPLX", ["FUNCTION",auxfn], g1]]
  arg := first u
  app := second u
  codePart1 :=                  -- look up the value if it is already there
    args ~= nil => [["SETQ", g2, ["assoc",g1,al]], ["CDR",g2]]
    [al]
  codePart2 :=                  -- otherwise, compute it.
    args ~= nil => [true,["SETQ",g2,app],["SETQ",al,[[g1,:g2],:al]],g2]
    [true,["SETQ",al,app]]
  lamex := ["LAM",arg,["PROG",[g2],
                        ["RETURN",["COND",codePart1,codePart2]]]]
  setDynamicBinding(al,nil)     -- clear the cache
  -- compile the worker function, first.
  u := [auxfn,["LAMBDA",args,:body]]
  COMP370 [u]
  -- then compile the original function.
  u := [name,lamex]
  if $PrettyPrint then PRETTYPRINT u
  COMP370 [u]
  name

++ Same as backendCompileSLAM, except that the cache is a hash
++ table.  This backend compiler is used to compile constructors.
backendCompileSPADSLAM: (%Symbol,%List,%Code) -> %Symbol
backendCompileSPADSLAM(name,args,body) ==
  al := INTERNL(name,'";AL")   -- name of the cache hash table.
  auxfn := INTERNL(name,'";")  -- name of the worker function.
  g1 := GENSYM()               -- name of the worker function parameter
  g2 := GENSYM()               -- name for the cache value.
  u := 
    null args => [nil,nil,[auxfn]]
    null rest args => [[g1],["devaluate",g1],[auxfn,g1]]
    [g1,["devaluateList",g1],["APPLY",["FUNCTION",auxfn],g1]]
  arg := first u               -- parameter list
  argtran := second u          -- devaluate argument
  app := third u               -- code to compute value
  code := 
    args = nil => ["COND",[al],[true,["SETQ",al,app]]]
    ["LET",[[g2,["assoc",argtran,al]]],
      ["COND",
        [g2,["CDR",g2]],
          [true, 
            ["PROGN",["SETQ",g2,app],
               ["SETQ",al,["cons5",["CONS",argtran, g2],al]],g2]]]]
  lamex := ["LAM",arg,code]
  SETANDFILE(al,nil)           -- define the global cache.
  -- compile the worker function first.
  u := [auxfn,["LAMBDA",args,:body]]
  if $PrettyPrint then PRETTYPRINT u
  COMP370 [u]
  -- then compiler the stub (which is the user-visible constructor).
  u := [name,lamex]
  if $PrettyPrint then PRETTYPRINT u
  COMP370 [u]
  name

backendCompile2: %Code -> %Symbol
backendCompile2 code ==
  code isnt [name,[type,args,:body],:junk] or junk ~= nil =>
    systemError ['"parenthesis error in: ", code]
  type = "SLAM" => backendCompileSLAM(name,args,body)
  LASSQ(name,$clamList) => compClam(name,args,body,$clamList)
  type = "SPADSLAM" => backendCompileSPADSLAM(name,args,body)
  type = "ILAM" => backendCompileILAM(name,args,body)
  body := [name,[type,args,:body]]
  if $PrettyPrint then PRETTYPRINT body
  if not $COMPILE then SAY '"No Compilation"
  else COMP370 [body]
  name

++ returns all fuild variables contained in `x'.  Fuild variables are
++ identifiers starting with '$', except domain variable names.
backendFluidize x ==
  IDENTP x and x ~= "$" and x ~= "$$" and
    (PNAME x).0 = char "$" and not DIGITP((PNAME x).1) => x
  isAtomicForm x => nil
  first x = "FLUID" => second x
  a := backendFluidize first x
  b := backendFluidize rest x
  a = nil => b
  [a,:b]


$FluidVars := []
$LocalVars := []
$SpecialVars := []


++ push `x' into the list of local variables.
pushLocalVariable: %Symbol -> %List
pushLocalVariable x ==
  x ~= "$" and (p := PNAME x).0 = char "$" and
    p.1 ~= char "," and not DIGITP p.1 => nil
  PUSH(x,$LocalVars)


--%
--% Middle Env to Back End Transformations.
--%

--% e ::=
--%     (%ilConst  <c> <type>)                   -- constant
--%     (%ilInert  <e> <type>)                   -- inert form
--%     (%ilCtx    <d> <type>)                   -- context
--%     (%ilVar    <n> <type>)                   -- variable
--%     (%ilLisp   <e> <type>)                   -- Lisp form
--%     (%ilFun    <e> <type>)                   -- function object
--%     (%ilMm     <e> <type>)                   -- modemap
--%     (%ilLocal  <n> <type>)                   -- local function
--%     (%ilCtor   <n> <type>)                   -- constructor
--%     (%ilTag    <e> <type>)                   -- tag of union object
--%     (%ilVal    <e> <type>)                   -- value of union object
--%     (%ilCall   <e...e> <type>)               -- a call
--%     (%ilXLAM   <e> <type>)                   -- XLAM form
--%     (%ilLAM    <e> <type>)                   -- LAMBDA form

structure ILInsn ==
  %ilConst(c,t)                             -- constant
  %ilInert(e,t)                             -- inert form
  %ilContext(e,t)                           -- context
  %ilVar(n,t)                               -- variable
  %ilCtor(n,t)                              -- constructor
  %ilLocal(op,t)                            -- local function
  %ilLisp(e,t)                              -- Lisp form
  %ilModemap(e,t)                           -- exported function modemap
  %ilUnionTag e                             -- union object tag
  %ilUnionValue(e,t)                        -- union object value
  %ilDeref(e,t)                             -- deref function pointer
  %ilCall(e,t)                              -- call
  %ilType(d,t)                              -- type instantiation request
  %ilReturn(n,T,t)                          -- `return' expression
  %ilExit(n,T,t)                            -- `exit' expression

++ Convert middle end IL forms to old back end forms.
il2OldForm x ==
  atom x => x                     -- ideally should not happen
  x is ["QUOTE",:.] => x          -- idem.
  case x of
    %ilConst(c,.) => c
    %ilInert(e,.) => e
    %ilVar(n,.) => n
    %ilCtor(n,.) => n
    %ilContext(e,.) => e
    %ilLisp(e,.) => e
    %ilModemap(e,.) => e
    %ilUnionTag(e,.) => ["CAR",il2OldForm e]
    %ilUnionValue(e,.) => ["CAR",il2OldForm e]
    %ilDeref(e,.) => ["applyFun",il2OldForm e]
    %ilCall(e,.) =>
      e is [["%ilLocal",op,:.],:.] =>
        rplac(first e,op)
        ilTransformInsns rest e
        e
      ["%Call",:ilTransformInsns e]
    otherwise => ilTransformInsns x

++ Subroutines of il2OldForm to walk sequence of IL instructions.
ilTransformInsns form ==
  for insns in tails form repeat
    rplac(first insns, il2OldForm first insns)
  form


--%

++ Replace every middle end sub-forms in `x' with Lisp code.
mutateToBackendCode: %Form -> %Void
mutateToBackendCode x ==
  isAtomicForm x => nil
  -- temporarily have TRACELET report MAKEPROPs.
  if (u := first x) = "MAKEPROP" and $TRACELETFLAG then
    RPLACA(x,"MAKEPROP-SAY")
  u in '(DCQ RELET PRELET SPADLET SETQ %LET) =>
    if u ~= "DCQ" then
      $NEWSPAD or $FUNAME in $traceletFunctions =>
        nconc(x,$FUNNAME__TAIL)
        RPLACA(x,"LETT")
      $TRACELETFLAG => RPLACA(x,"/TRACE-LET")
      u = "%LET" => RPLACA(x,"SPADLET")
    mutateToBackendCode CDDR x
    if not (u in '(SETQ RELET)) then
      IDENTP second x => pushLocalVariable second x
      second x is ["FLUID",:.] =>
        PUSH(CADADR x, $FluidVars)
        rplac(second x, CADADR x)
      MAPC(function pushLocalVariable, LISTOFATOMS second x)
  IDENTP u and GET(u,"ILAM") ~= nil =>
    RPLACA(x, eval u)
    mutateToBackendCode x
  u in '(PROG LAMBDA) =>
    newBindings := []
    for y in second x repeat
      not (y in $LocalVars) =>
        $LocalVars := [y,:$LocalVars]
        newBindings := [y,:newBindings]
    res := mutateToBackendCode CDDR x
    $LocalVars := REMOVE_-IF(function LAMBDA(y(), y in newBindings), 
                     $LocalVars)
    [u,second x,:res]
  u = "DECLARE" => nil       -- there is nothing to do convert there
  mutateToBackendCode u
  mutateToBackendCode rest x


skipDeclarations: %List -> %List
skipDeclarations form ==
  while first form is ["DECLARE",:.] repeat
    form := rest form
  form

++ return the last node containing a declaration in form, otherwise nil.
lastDeclarationNode: %List -> %List
lastDeclarationNode form ==
  while second form is ["DECLARE",:.] repeat
     form := rest form
  first form is ["DECLARE",:.] => form
  nil

declareGlobalVariables: %List -> %List
declareGlobalVariables vars ==
  ["DECLARE",["SPECIAL",:vars]]

simplifySEQ form ==
  isAtomicForm form => form
  form is ["SEQ",[op,a]] and op in '(EXIT RETURN) => simplifySEQ a
  for stmts in tails form repeat
    rplac(first stmts, simplifySEQ first stmts)
  form

++ Generate Lisp code by lowering middle end defining form `x'.
++ x has the strucrure: <name, parms, stmt1, ...>
transformToBackendCode: %Form -> %Code
transformToBackendCode x ==
  $FluidVars: fluid := nil
  $LocalVars: fluid := nil
  $SpecialVars: fluid := nil
  x := middleEndExpand x
  mutateToBackendCode CDDR x
  body := skipDeclarations CDDR x
  -- Make it explicitly a sequence of statements if it is not a one liner.
  body := 
    stmt := first body
    null rest body and 
      (atom stmt or first stmt = "SEQ" or not CONTAINED("EXIT",stmt)) =>
        body
    [simplifySEQ ["SEQ",:body]]
  $FluidVars := REMDUP nreverse $FluidVars
  $LocalVars := S_-(S_-(REMDUP nreverse $LocalVars,$FluidVars),
                  LISTOFATOMS second x)
  lvars := [:$FluidVars,:$LocalVars]
  fluids := S_+($FluidVars,$SpecialVars)
  body := 
    fluids ~= nil =>
      [["PROG",lvars,declareGlobalVariables fluids, ["RETURN",:body]]]
    lvars ~= nil or CONTAINED("RETURN",body) =>
      [["PROG",lvars,["RETURN",:body]]]
    body
  -- add reference parameters to the list of special variables.
  fluids := S_+(backendFluidize second x, $SpecialVars)
  lastdecl := lastDeclarationNode rest x
  if lastdecl = nil then
    RPLACD(rest x, body)
  else
    null fluids =>
      RPLACD(lastdecl, body)
    RPLACD(lastdecl, [declareGlobalVariables fluids,:body])
  x

backendCompile1 x ==
  fname := first x
  $FUNNAME: local := fname
  $FUNNAME__TAIL: local := [fname]
  lamex := second x
  $CLOSEDFNS: local := []
  lamex := transformToBackendCode lamex
  backendCompileNEWNAM lamex
  -- Note that category constructors are evaluated before they
  -- their compiled, so this noise is not very helpful.
  if $verbose and FBOUNDP fname then
    FORMAT(true,'"~&~%;;;     ***       ~S REDEFINED~%",fname)
  [[fname,lamex],:$CLOSEDFNS]

backendCompile l ==
  MAPCAR(function backendCompile2, MAPCAN(function backendCompile1,l))

compileFileQuietly path ==
  quietlyIfInteractive COMPILE_-FILE path

++ Subroutine of compileConstructor1.  Called to compile the body
++ of a category constructor definition.
compAndDefine l ==
  _*COMP370_-APPLY_* := "PRINT-AND-EVAL-DEFUN"
  backendCompile l

++ Subroutine of compileInteractive.
compQuietly fn ==
  _*COMP370_-APPLY_* :=
    $InteractiveMode =>
      $compileDontDefineFunctions => "COMPILE-DEFUN"
      "EVAL-DEFUN"
    "PRINT-DEFUN"
  quietlyIfInteractive backendCompile fn

compileQuietly fn ==
  _*COMP370_-APPLY_* :=
     $InteractiveMode =>
       $compileDontDefineFunctions => "COMPILE-DEFUN"
       "EVAL-DEFUN"
     "PRINT-DEFUN"
  quietlyIfInteractive COMP370 fn



--%
--% Compile Time operation lookup for the benefit of domain inlining.
--%

++ Subroutine of lookupDefiningFunction.
++ Called when the domain of computation `dc' is closed (this is the
++ case of niladic constructors) to lookup up the definition function
++ of the operation `op' with signature `sig'.
lookupFunctionInstance(op,sig,dc) ==
  dom := eval dc
  sig := MSUBST(devaluate dom,dc,sig)
  compiledLookup(op,sig,dom)

++ If `x' is a formal map variable, returns its position.
++ Otherwise return nil.
isFormal: %Symbol -> %Maybe %Short
isFormal x ==
  POSITION(x,$FormalMapVariableList,KEYWORD::TEST, function EQ)

++ Expand the form at position `slot' in the domain template `shell'
++ with argument list `args'.
expandFormTemplate(shell,args,slot) ==
  FIXP slot =>
    slot = 0 => "$"
    slot = 2 => "$$"
    expandFormTemplate(shell,args,getShellEntry(shell,slot))
  atom slot => slot
  slot is ["local",parm] and (n := isFormal parm) => 
    args.n   -- FIXME: we should probably expand with dual signature
  slot is ["NRTEVAL",val] => val
  slot is ["QUOTE",val] => 
    STRINGP val => val
    slot
  [expandFormTemplate(shell,args,i) for i in slot]

++ Compare the form at `slot' in the domain templare `shell'
++ for equality with `form'.
equalFormTemplate(shell,args,slot,form) ==
  FIXP slot =>
    slot = 0 => form = "$"
    slot = 2 => form = "$$"
    equalFormTemplate(shell,args,getShellEntry(shell,slot),form)
  slot is ["local",parm] and (n := isFormal parm) => 
    equalFormTemplate(shell,args,args.n,form)
  slot is ["NTREVAL",val] => form = val
  slot is ["QUOTE",val] => 
     STRINGP val => val = form
     slot = form
  atom slot or atom form => form = slot
  #slot ~= #form => false
  and/[equalFormTemplate(shell,args,i,x) for i in slot for x in form]

++ Subroutine of lookupDefiningFunction.
++ Return the location of function templates with signature `sig',
++ descriptor address in the range [start,end), in the domain 
++ template `shell' whose local reference vector is `funDesc'.
++ Return value:
++    nil         => function not defined by `shell'.
++    "ambiguous" => too many candidates
++    <number>    => slot number of unique matching function.
getFunctionTemplate(sig,start,end,shell,args,funDesc) ==
  nargs := #rest sig
  loc := nil                           -- candidate locations
  while loc ~= "ambiguous" and start < end repeat
    n := funDesc.start                 -- arity of current operator
    PROGN
      -- Skip if arity mismatch
      i := start
      n ~= nargs => nil
      -- We are not interested in predicates, at this point.
      -- Skip if this operator's signature does not match 
      i := i + 2
      or/[not equalFormTemplate(shell,args,funDesc.k,t) 
           for k in i.. for t in sig] => nil
      -- Grab the location of this match
      loc := 
        FIXP loc => "ambiguous"
        funDesc.(i + n + 1)
    start := start + n + 4
  loc

++ Subroutine of lookupDefiningFunction.
lookupInheritedDefiningFunction(op,sig,shell,args,slot) ==
  dom := expandFormTemplate(shell,args,slot)
  atom dom or dom is ["local",:.] => nil
  lookupDefiningFunction(op,sig,dom)

++ Return the name of the function definition that explicitly implements
++ the operation `op' with signature `sig' in the domain of 
++ computation `dc'.  Otherwise, return nil.
++ Note: Only a function defined by the domain template, or its add-chains,
++       and that is unambiguous is returned.  In particular, this
++       function defaulting packages.
lookupDefiningFunction(op,sig,dc) ==
  -- 1. Read domain information, if available.
  [ctor,:args] := dc
  -- 1.1. Niladic constructors don't need approximation.
  null args => lookupFunctionInstance(op,sig,dc)
  -- 1.2. Don't look into defaulting package
  isDefaultPackageName ctor => nil
  -- 1.2. Silently give up if the constructor is just not there
  loadLibIfNotLoaded ctor
  infovec := GET(ctor, "infovec") or return nil
  -- 1.3. We need information about the original domain template
  shell := first infovec               -- domain template
  opTable := second infovec            -- operator-code table
  opTableLength := #opTable
  forgetful := infovec.4 = "lookupIncomplete"

  -- 2. Get the address range of op's descriptor set
  [.,.,.,:funDesc] := fourth infovec
  index := getOpCode(op, opTable, opTableLength - 1)
  -- 2.1. For a forgetful functor, try the add chain
  index = nil =>
    forgetful and lookupInheritedDefiningFunction(op,sig,shell,args,5)
  -- 2.2. The operation is either defined here, or is available
  --      from category package defaults.
  limit := 
    index + 2 < opTableLength => opTable.(index + 2)
    #funDesc 

  -- 3. Locate the descriptor with matching signature
  loc := getFunctionTemplate(sig,opTable.index,limit,shell,args,funDesc)

  -- 4. Look into the add-chain if necessary
  loc = nil => lookupInheritedDefiningFunction(op,sig,shell,args,shell.5)

  -- 5. Give up if the operation is overloaded on semantics predicates.
  loc = "ambiguous" => nil

  -- 6. We have a location to a function descriptor.
  fun := shell.loc
  -- 6.1. A constant producing functions?
  fun is [.,.,[.,["dispatchFunction",fun'],.]] => fun'
  -- 6.2. An inherited function?
  fun is [idx,:.] => 
    not FIXP idx => nil          -- a UFO?
    loc := funDesc.(idx + 1)
    if loc = 0 then loc := 5
    shell.loc = nil => nil
    lookupInheritedDefiningFunction(op,sig,shell,args,shell.loc)
  -- 6.3. Whatever.
  fun
