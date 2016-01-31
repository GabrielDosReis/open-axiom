-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2016, Gabriel Dos Reis.
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
  makeWorkerName: %Symbol -> %Symbol
  clearReplacement: %Symbol -> %Thing
  replaceSimpleFunctions: %Form -> %Form
  foldExportedFunctionReferences: (%Database,%List %Code) -> %List %Code
  diagnoseUnknownType: (%Mode,%Env) -> %Form
  declareUnusedParameters: %Code -> %Code
  registerFunctionReplacement: (%Database,%Symbol,%Form) -> %Thing
  getSuccessEnvironment: (%Form,%Env) -> %Env
  getInverseEnvironment: (%Form,%Env) -> %Env
  giveVariableSomeValue: (%Symbol,%Mode,%Env) -> %Env
  registerConstructor: (%Symbol,%Env) -> %Env
  currentDB: %Env -> %Maybe %Database
  -- functor data manipulation
  dbInfovec: %Symbol -> %Maybe %FunctorData
  makeDefaultPackageParameters: %Database -> %List %Symbol
  makeDefaultPackageAbbreviation: %Database -> %Symbol
  completeDefaultPackageParameters: %List %Symbol -> %List %Symbol

--% Accessors of domain and category objects

++ Return thr i-th part of a category object
macro categoryRef(c,i) ==
  vectorRef(c,i)

++ Return the i-th part of a domain object.
macro domainRef(d,i) ==
  vectorRef(d,i)

++ Return the canonical form for a domain or category object
macro canonicalForm d ==
  vectorRef(d,0)

++ Return the constructor that instantiates to the domain
++ or category object
macro instantiationCtor d ==
  canonicalForm(d).op

++ Return the canonical forms of the arguments used to instantiate
++ a domain or a category object.
macro instantiationArgs d ==
  canonicalForm(d).args

++ Return the number of arguments used to instantiate a domain object.  
macro instantiationArity d ==
  # instantiationArgs d

++ Return the list of operations exported by a category object
macro categoryExports d ==
  categoryRef(d,1)

++ Return the attribute alist of a category object.
macro categoryAttributes d ==
  categoryRef(d,2)

++ Return a 3-list of data describing the hierarchy of category `c'.
macro categoryAssociatedTypes c ==
  categoryRef(c,4)

++ Return the list of principal ancestors of category `c'.  
macro categoryPrincipals c ==
  first categoryAssociatedTypes c

++ Return the list of [ancestor,predicate,index] data of catagory `c',
++ where `ancestor' is a fundamental ancestor, `index' its sequence number.
macro categoryAncestors c ==
  second categoryAssociatedTypes c

macro categoryLocals c ==
  third categoryAssociatedTypes c

macro categoryParameters c ==
  categoryRef(c,5)

++ Reference a 3-list
++   [lookupFunction,thisDomain,optable]
++ necessary for function lookup in a domain:
macro domainDirectory d ==
  domainRef(d,1)

++ Reference the lookup function of a domain object
macro domainLookupFunction d ==
  first domainDirectory d
  
++ Reference the operator-code table of a domain object.  
macro domainOperatorTable d ==
  third domainDirectory d

++ Reference the list of (attribute, predIndex) pairs for this domain.
macro domainAttributes d ==
  domainRef(d,2)
  
++ Return the predicate values associated with the domain object.
++ This is an integer interpreted as bit vector
macro domainPredicates d ==
  domainRef(d,3)

++ Return a 3-element dotted list of address data for a domain.
macro domainData d ==
  domainRef(d,4)

--%  
structure %CompilationData ==
  Record(subst: %Substitution,idata: %Substitution,bytes: List %Fixnum,
    shell: %Vector %Thing, items: %Buffer %Pair(%SourceEntity,%Code),
      capsule: %List %Thing, lib: %Libstream,outpath: %Pathname) with
        cdSubstitution == (.subst)
        cdImplicits == (.idata)
        cdBytes == (.bytes)
        cdShell == (.shell)
        cdItems == (.items)
        cdCapsule == (.capsule)
        cdLib == (.lib)
        cdOutput == (.outpath)

++ Make a fresh compilation data structure.
makeCompilationData() ==
  mk%CompilationData(nil,nil,nil,nil,[nil,:0],nil,nil,nil)

++ Subsitution that replaces parameters with formals.
macro dbFormalSubst db ==
  cdSubstitution dbCompilerData db

++ Return source-level parameters of this constructor.
dbParameters db ==
  dbConstructorForm(db).args

++ Return implicit parameter data associated to `db'.  This
++ information is active only during the elaboration of the
++ constructor associated with `db'.
macro dbImplicitData db ==
  cdImplicits dbCompilerData db

++ Return the list of encoding bytes for a function during elaboration.
++ Transcient data.
macro dbByteList db ==
  cdBytes dbCompilerData db

++ Return the domain shell of the category object (or the category object
++ of the domain) being elaborated.
macro dbDomainShell db ==
  cdShell dbCompilerData db

++ Return a buffer of entities referenced during elaboration
++ of current functor.
macro dbEntityBuffer db ==
  cdItems dbCompilerData db

++ List (in reverse order) of used entities during elaboration of
++ current functor.
macro dbUsedEntities db ==
  first dbEntityBuffer db

++ Number of used entities during elaboration of current functor.
macro dbEntityCount db ==
  rest dbEntityBuffer db

macro dbCapsuleIR db ==
  cdCapsule dbCompilerData db

macro dbLibstream db ==
  cdLib dbCompilerData db

macro dbCodeStream db ==
  libCodeStream dbLibstream db

macro dbInsnStream db ==
  libInsnStream dbLibstream db

macro dbOutputPath db ==
  cdOutput dbCompilerData db

++ Return the existential substitution of `db'.
dbQuerySubst db ==
  x := dbImplicitData db => first x
  nil

++ List of implicit parameters to the constructor.
dbImplicitParameters db ==
  ASSOCLEFT dbQuerySubst db

dbImplicitConstraints db ==
  x := dbImplicitData db => second x

++ Apply the formal substitution or `db'to the form `x'.
macro dbSubstituteFormals(db,x) ==
  applySubst(dbFormalSubst db,x)

++ Apply the query substitution of `db' to the form `x'.  
macro dbSubstituteQueries(db,x) ==
  applySubst(dbQuerySubst db,x)

++ Apply both query and formal variable substitutions of `db' to `x'.
dbSubstituteAllQuantified(db,x) ==
  applySubst([:dbQuerySubst db,:dbFormalSubst db],x)

++ This predicate holds if this DB is for a category constructor.  
dbForCategory? db ==
  db ~= nil and dbConstructorKind db is 'category

--%
$SetCategory ==
  '(SetCategory)
  
--%

completeDefaultPackageParameters parms ==
  dollar := first setDifference('(S A B C D E F G H I),parms)
  [dollar,:parms]

makeDefaultPackageParameters db ==
  completeDefaultPackageParameters dbConstructorForm(db).args

makeDefaultPackageAbbreviation db ==
  makeSymbol strconc(symbolName dbAbbreviation db,'"-")

dbInfovec name ==
  getConstructorKindFromDB name is "category" => nil
  loadLibIfNotLoaded(name)
  u := property(name,'infovec) => u
  nil

++ Access to the shell template  associated with an infovec.
macro ivDomainShell iv ==
  first iv

++ Access to the operation table associated with an infovec.
macro ivOptable iv ==
  second iv

++ Access the alist mapping an attribute to the predicate index
++ associated with an infovec.
macro ivAttributePredicateIndexDirectory iv ==
  third iv

++ Access to additional data in the infovec
macro ivAdditionalData iv ==
  fourth iv

++ Access to predicate bitvector as associated with an infovec.
macro ivPredicateBitvector iv ==
  first ivAdditionalData iv

++ Access to the vector of category default package functors
++ associated with an infovec.
macro ivCategoryDefaults iv ==
  second ivAdditionalData iv

++ Access to the principal ancestors of a domain shell associated
++ with an infovec.
macro ivPrincipalAncestors iv ==
  third ivAdditionalData iv

++ Return the exported operation descriptors bytecode vector
++ associated with an infovec.
ivExportBytecodes iv ==
  [.,.,.,:vec] := ivAdditionalData iv
  vec

--% 

++ Token to indicate that a function body should be ignored.
$ClearBodyToken ==
  KEYWORD::OpenAxiomClearBodyToken

++
$ConstructorCache := hashTable 'EQ

++
$instantRecord := hashTable 'EQ


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
  integer? form or string? form or form = nil or form = true => form
  quote form

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
  u := get('Rep,'value,e) => u.expr
  get('Rep,"macro",e)


++ Returns true if the form `t' is an instance of the Tuple constructor.
isTupleInstance: %Form -> %Boolean
isTupleInstance t ==
  t is ["Tuple",.]

++ Returns true if the signature `sig' describes a function that can
++ accept a homogeneous variable length argument list.
isHomoegenousVarargSignature: %Sig -> %Boolean
isHomoegenousVarargSignature sig ==
  #sig = 1 and isTupleInstance first sig

++ Returns true if the arguments list `args' match in shape the
++ parameter type list `sig'.  This means that either the number
++ of arguments is exactly the number of parameters, or that the
++ signature describes a homogeneous vararg operation.
enoughArguments: (%List %Form,%Sig) -> %Boolean
enoughArguments(args,sig) ==
  #args = #sig or isHomoegenousVarargSignature sig

++ Returns true if the operation described by the signature `sig'
++ wants its arguments as a Tuple object.
wantArgumentsAsTuple: (%List %Form,%Sig) -> %Boolean
wantArgumentsAsTuple(args,sig) ==
  isHomoegenousVarargSignature sig and #args ~= #sig

abstractionOperator? x ==
  symbol? x and symbolMember?(x,$AbstractionOperator)

++ We are about to seal the (Lisp) definition of a function.
++ Augment the body of any function definition in the form `x'
++ with declarations for unused parameters.
++ that are unused.
declareUnusedParameters x == (augment x; x) where
  augment x == 
    atomic? x => nil
    x is [op,parms,body] and abstractionOperator? op =>
      augment body
      unused := [p for p in parms | not usesVariable?(body,p)]
      null unused => [body]
      x.rest.rest := [["DECLARE",["IGNORE",:unused]],body]
    for x' in x repeat
      augment x'

devaluate d ==
  not vector? d => d
  categoryObject? d => canonicalForm d
  #d > 0 =>
    d' := canonicalForm d
    isFunctor d' => d'
    d
  d
 
devaluateList l == [devaluate d for d in l]
 
devaluateDeeply x ==
  vector? x => devaluate x
  x isnt [.,:.] => x
  [devaluateDeeply y for y in x]

--% Debugging Functions
 
--CONTINUE() == continue()
continue() == FIN comp($x,$m,$f)
 
LEVEL(:l) == apply('level,l)
level(:l) ==
  null l => same()
  l is [n] and integer? n => displayComp ($level:= n)
  SAY '"Correct format: (level n) where n is the level you want to go to"
 
UP() == up()
up() == displayComp ($level:= $level-1)
 
SAME() == same()
same() == displayComp $level
 
DOWN() == down()
down() == displayComp ($level:= $level+1)
 
displaySemanticErrors() ==
  n:= #($semanticErrorStack:= removeDuplicates $semanticErrorStack)
  n=0 => nil
  l:= reverse! $semanticErrorStack
  $semanticErrorStack:= nil
  sayBrightly bright '"  Semantic Errors:"
  displaySemanticError(l,$OutputStream)
  sayBrightly '" "
  displayWarnings()
 
displaySemanticError(l,stream) ==
  for x in l for i in 1.. repeat
    sayBrightly(['"      [",i,'"] ",:first x],stream)
 
displayWarnings() ==
  n:= #($warningStack:= removeDuplicates $warningStack)
  n=0 => nil
  sayBrightly bright '"  Warnings:"
  l := reverse! $warningStack
  displayWarning(l,$OutputStream)
  $warningStack:= nil
  sayBrightly '" "
 
displayWarning(l,stream) ==
  for x in l for i in 1.. repeat
    sayBrightly(['"      [",i,'"] ",:x],stream)
 
displayComp level ==
  $bright:= " << "
  $dim:= " >> "
  if $insideCapsuleFunctionIfTrue then
    sayBrightly ['"error in function",:bright $op,'"%l"]
  --mathprint removeZeroOne mkErrorExpr level
  pp removeZeroOne mkErrorExpr level
  sayBrightly ['"****** level",:bright level,'" ******"]
  [$x,$m,$f,$exitModeStack]:= ELEM($s,level)
  SAY("$x:= ",$x)
  sayBrightly ['"$m := ",:listify form2String $m]
  --SAY "$f:="
  --F_,PRINT_-ONE $f
  nil
 
mkErrorExpr level ==
  bracket ASSOCLEFT drop(level-#$s,$s) where
    bracket l ==
      #l<2 => l
      l is [a,b] =>
        highlight(b,a) where
          highlight(b,a) ==
            b isnt [.,:.] =>
              substitute(var,b,a) where
                var:= makeSymbol strconc(STRINGIMAGE $bright,STRINGIMAGE b,STRINGIMAGE $dim)
            highlight1(b,a) where
              highlight1(b,a) ==
                a isnt [.,:.] => a
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
    u.rest := val
    proplist
  [[prop,:val],:proplist]
 
warnLiteral x ==
  stackWarning('"%1b is BOTH a variable a literal",[x])
 
intersectionEnvironment(e,e') ==
  ce:= makeCommonEnvironment(e,e')
  ic := intersectionContour(deltaContour(e,ce),deltaContour(e',ce)) =>
    addContour(ic,ce)
  ce
 
deltaContour([[c,:cl],:el],[[c',:cl'],:el']) ==
  not sameObject?(el,el') => systemError '"deltaContour" --a cop out for now
  eliminateDuplicatePropertyLists contourDifference(c,c') where
    contourDifference(c,c') ==
      [first x for x in tails c while not sameObject?(x,c')]
    eliminateDuplicatePropertyLists contour ==
      contour is [[x,:.],:contour'] =>
        LASSOC(x,contour') =>
                               --save some CONSing if possible
          [first contour,:DELLASOS(x,eliminateDuplicatePropertyLists contour')]
        [first contour,:eliminateDuplicatePropertyLists contour']
      nil
 
intersectionContour(c,c') ==
  $var: local := nil
  computeIntersection(c,c') where
    computeIntersection(c,c') ==
      varlist:= removeDuplicates ASSOCLEFT c
      varlist':= removeDuplicates ASSOCLEFT c'
      interVars := setIntersection(varlist,varlist')
      unionVars := setUnion(varlist,varlist')
      diffVars := setDifference(unionVars,interVars)
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
      pair := symbolAssoc("mode",p) =>
        pair' := symbolAssoc("mode",p') =>
          m'':= unifiable(rest pair,rest pair') => [["mode",:m'']]
          stackSemanticError(['"%b",$var,'"%d","has two modes: "],nil)
       --stackWarning ("mode for",'"%b",$var,'"%d","introduced conditionally")
        [["conditionalmode",:rest pair]]
        --LIST pair
       --stackWarning ("mode for",'"%b",$var,'"%d","introduced conditionally")
      pair' := symbolAssoc("mode",p') => [["conditionalmode",:rest pair']]
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
        if u is ["Union",:u'] and (and/[listMember?(v,u') for v in rest m]) then
          return m
        --this loop will return nil if not satisfied
 
addContour(c,E is [cur,:tail]) ==
  [append!(fn(c,E),cur),:tail] where
    fn(c,e) ==
        for [x,:proplist] in c repeat
           fn1(x,proplist,getProplist(x,e)) where
              fn1(x,p,ee) ==
                for pv in p repeat fn3(x,pv,ee) where
                 fn3(x,pv,e) ==
                   [p,:v]:=pv
                   if p="conditionalmode" then
                     pv.first := "mode"
                     --check for conflicts with earlier mode
                     if vv := symbolTarget("mode",e) then
                        if v ~=vv then
                          stackWarning('"The conditional modes %1p and %2p conflict",
                            [v,vv])
        [c]

++ Return the common root of the environments e and e'.
++ Note: we use cell pointer comparison instead of general object
++ equality comparison because both are expected to build from
++ a commont cell node.
makeCommonEnvironment(e,e') ==
  interE makeSameLength(e,e') where
    interE [e,e'] ==
      sameObject?(rest e,rest e') =>
        [interLocalE makeSameLength(first e,first e'),:rest e]
      interE [rest e,rest e']
    interLocalE [le,le'] ==
      sameObject?(rest le,rest le') =>
        [interC makeSameLength(first le,first le'),:rest le]
      interLocalE [rest le,rest le']
    interC [c,c'] ==
      sameObject?(c,c') => c
      interC [rest c,rest c']
    makeSameLength(x,y) ==
      fn(x,y,#x,#y) where
        fn(x,y,nx,ny) ==
          nx>ny => fn(rest x,y,nx-1,ny)
          nx<ny => fn(x,rest y,nx,ny-1)
          [x,y]

++ Return the lexically leftmost location in an assignment for.
lhsOfAssignment x ==
  x is [":=",lhs,:.] => lhsOfAssignment lhs
  x

getSuccessEnvironment(a,e) ==
  a is ["is",id,m] =>
    id := lhsOfAssignment id
    ident? id and isDomainForm(m,$EmptyEnvironment) =>
      e:=put(id,"specialCase",m,e)
      currentProplist:= getProplist(id,e)
      [.,.,e] := T := comp(m,$EmptyMode,e) or return nil -- duplicates compIs
      newProplist:= consProplistOf(id,currentProplist,"value",[m,:rest removeEnv T])
      addBinding(id,newProplist,e)
    e
  a is ["case",x,m] and (x := lhsOfAssignment x) and ident? x =>
    put(x,"condition",[a,:get(x,"condition",e)],e)
  a is ["and",:args] =>
    for form in args repeat
      e := getSuccessEnvironment(form,e)
    e
  a is ["not",a'] => getInverseEnvironment(a',e)
  -- Follow temporaries in complex conditionals
  symbol? a and (T := get(a,"value",e)) => getSuccessEnvironment(T.expr,e)
  e

isUnionMode(m,e) ==
  m is ["Union",:.] => m
  v := get(RepIfRepHack m,"value",e) =>
    (v.expr is ["Union",:.] => v.expr; nil)
  nil

++ Return the UnionCategory of `m' in the environment `e', if known.
unionLike?(m,e) ==
  isUnionMode(m,e) is ['Union,:branches] => ['UnionCategory,:branches]
  -- Take a cheap approximation at domains with Union-like flavour.
  T := compForMode(m,$EmptyMode,e) or return nil
  T.expr is ['Union,:branches] => ['UnionCategory,:T.expr.args]
  T.mode is ['UnionCategory,:.] => T.mode
  T.mode is ['UnionType] => ['UnionCategory]
  T.mode isnt ['Join,:cats,['CATEGORY,.,:sigs]] => nil
  listMember?(['UnionType],cats) =>
    ['UnionCategory,
      :[b for ['SIGNATURE,"case",[=$Boolean,'$,["[||]",b]]] in sigs]]
  nil

++ If `x' designates a store with multiple views, e.g. Union, return
++ the collection of those modes.
unionProperty(x,e) ==
  x isnt [.,:.] => unionLike?(getmode(x,e),e)
  nil

getInverseEnvironment(a,e) ==
  a is ["case",x,m] and (x := lhsOfAssignment x) and ident? x =>
    --the next two lines are necessary to get 3-branched Unions to work
    -- old-style unions, that is
    (get(x,"condition",e) is [["OR",:oldpred]]) and listMember?(a,oldpred) =>
      put(x,"condition",[mkpf(remove(oldpred,a),"OR")],e)
    unionProperty(x,e) is ['UnionCategory,:l] =>
      l' := remove(l,m)
      for u in l' repeat
	 if u is ['_:,=m,:.] then l' := remove(l',u)
      newpred:= mkpf([["case",x,m'] for m' in l'],"OR")
      put(x,"condition",[newpred,:get(x,"condition",e)],e)
    e
  a is ["not",a'] => getSuccessEnvironment(a',e)
  -- Follow temporaries in complex conditionals
  symbol? a and (T := get(a,"value",e)) => getInverseEnvironment(T.expr,e)
  e

++ Give some abstract value to the variable `v' of mode `m' in `e'.
++ Return the resulting environment.
giveVariableSomeValue(x,m,e) ==
  put(x,'value,[genSomeVariable(),m,nil],e)


printEnv E ==
  for x in E for i in 1.. repeat
    for y in x for j in 1.. repeat
      SAY('"******CONTOUR ",j,'", LEVEL ",i,'":******")
      for z in y repeat
        finishLine $OutputStream
        SAY("Properties Of: ",first z)
        for u in rest z repeat
          PRIN1 first u
          printString ": "
          PRETTYPRINT tran(rest u,first u) where
            tran(val,prop) ==
              prop="value" => drop(-1,val)
              val
 
prEnv E ==
  for x in E for i in 1.. repeat
    for y in x for j in 1.. repeat
      SAY('"******CONTOUR ",j,'", LEVEL ",i,'":******")
      for z in y | null symbolTarget("modemap",rest z) repeat
        finishLine $OutputStream
        SAY("Properties Of: ",first z)
        for u in rest z repeat
          PRIN1 first u
          printString ": "
          PRETTYPRINT tran(rest u,first u) where
            tran(val,prop) ==
              prop="value" => drop(-1,val)
              val
 
prModemaps E ==
  listOfOperatorsSeenSoFar:= nil
  for x in E for i in 1.. repeat
    for y in x for j in 1.. repeat
      for z in y | not member(first z,listOfOperatorsSeenSoFar) and
        (modemap := symbolTarget("modemap",rest z)) repeat
          listOfOperatorsSeenSoFar:= [first z,:listOfOperatorsSeenSoFar]
          finishLine $OutputStream
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
    if objectAssoc(first u,new) = nil then
      uold:= rest u
      unew:= nil
      for v in uold repeat
        if objectAssoc(first v,unew) = nil then
          unew:= [v,:unew]
      new:= [[first u,:reverse! unew],:new]
  $CategoryFrame:= [[reverse! new]]
  nil

--%

++ Returns non-nil if `t' is a known type in the environement `e'.
diagnoseUnknownType(t,e) ==
  t isnt [.,:.] =>
    t in '($ constant) => t
    t' := assoc(t,getDomainsInScope e) => t'
    (m := getmode(t,e)) and isCategoryForm(m,$CategoryFrame) => t
    string? t => t
    -- ??? We should not to check for $$ at this stage.  
    -- ??? This is a bug in the compiler that needs to be fixed.
    t = "$$" => t
    stackSemanticError(['"The identifier", :bright t, 
                         '"is not known to name a type"],nil)
  [ctor,:args] := t
  ctor is "Mapping" => 
    for t' in args repeat diagnoseUnknownType(t',e)
    t
  ctor is "Record" =>
    for [[.,n,t'],:fields] in tails args repeat 
      diagnoseUnknownType(t',e)
      for [.,=n,.] in fields repeat
        stackSemanticError(['"Field", :bright n, 
          '"declared more than once."], nil)
    t
  ctor is "Union" =>
    if args is [[":",:.],:.] then
      for [[.,n,t'],:fields] in tails args repeat 
        diagnoseUnknownType(t',e)
        for [.,=n,.] in fields repeat
          stackSemanticError(['"Field", :bright n, 
            '"declared more than once."], nil)
    else
      for t' in args repeat diagnoseUnknownType(t',e)
    t
  ctor is "Enumeration" =>
    for t' in args repeat
      ident? t' => nil
      stackSemanticError(['"Enumerators must be symbols."], nil)
    -- Make sure we don't have repeated symbolic values
    for [sym,:syms] in tails args repeat
      symbolMember?(sym,syms) => 
        stackSemanticError(['"Symbolic value ", :bright sym, 
          '"is listed twice"], nil)
    t
  ctor is "[||]" => t
  builtinConstructor? ctor => t
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
  ident? name =>
    pl:= getProplist(name,e) =>
      (symbolTarget("value",pl) or symbolTarget("mode",pl) => false; true)
    true
  false
 
isFalse() == nil
 
isFluid s ==
  s isnt [.,:.] and char "$" = stringChar(PNAME s,0)
 
isFunction(x,e) ==
  get(x,"modemap",e) or GETL(x,"SPECIAL") or x="case" or getmode(x,e) is [
    "Mapping",:.]
 
isLiteral: (%Symbol,%Env) -> %Boolean
isLiteral(x,e) == 
  get(x,"isLiteral",e) => true
  false

++ Remember the name of the constructor definition being processed.
registerConstructor(x,e) ==
  put('%compilerData,'%ctor,x,e)

++ Retrieve the most recent defining constructor.
currentConstructor e ==
  get('%compilerData,'%ctor,e)

++ Retrieve the DB of the constructor definition being processed.
currentDB e ==
  ctor := currentConstructor e => constructorDB ctor
  nil

makeLiteral: (%Symbol,%Env) -> %Thing
makeLiteral(x,e) == 
  put(x,"isLiteral","true",e)
 
isSomeDomainVariable s ==
  ident? s and #(x:= symbolName s) > 2 and
    stringChar(x,0) = char "#" and stringChar(x,1) = char "#"

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
  x is '$ and y is 'Rep => $useRepresentationHack
  -- Expand domain representation form
  x is 'Rep and not $useRepresentationHack =>
    isSubset(getRepresentation e,y,e)
  y is '$ and get(y,'%dc,e) = x => true
  -- Or, if x has the Subsets property set by SubsetCategory.
  pred := LASSOC(opOf x,get(opOf y,"Subsets",e)) => pred
  -- Or, they are related by subdomain chain.
  isDomainForm(x,e) and isSubDomain(x,y)
 
isDomainInScope(domain,e) ==
  domainList:= getDomainsInScope e
  domain isnt [.,:.] =>
    symbolMember?(domain,domainList) => true
    not ident? domain or isSomeDomainVariable domain => true
    false
  (name:= first domain)="Category" => true
  objectAssoc(name,domainList) => true
--   null rest domain or domainMember(domain,domainList) => true
--   false
  isFunctor name => false
  true --is not a functor
 
isSimple x ==
  atomic? x => true
  isSideEffectFree x.op and (and/[isSimple y for y in x.args])
 
isSideEffectFree op ==
  op is ["elt",.,op'] => isSideEffectFree op'
  not ident? op => false
  listMember?(op,$SideEffectFreeFunctionList) or constructor? op
 
isAlmostSimple x ==
  --returns (<new predicate> . <list of assignments>) or nil
  $assignmentList: local := nil --$assigmentList is only used in this function
  transform:=
    fn x where
      fn x ==
        x isnt [.,:.] or null rest x => x
        [op,y,:l]:= x
        op="has" => x
        op="is" => x
        op=":=" =>
          ident? y => (setAssignment [x]; y)
          (setAssignment [[":=",g:= genVariable(),:l],[":=",y,g]]; g)
        op = "case" and ident? y => x
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
      x isnt [.,:.] => x
      x is ["exit",0,u] => removeExit0 u
      [removeExit0 first x,:removeExit0 rest x]
 
adjExitLevel(x,seqnum,inc) ==
  x isnt [.,:.] => x
  x is [op,:l] and op in '(SEQ REPEAT COLLECT) =>
    for u in l repeat adjExitLevel(u,seqnum+1,inc)
  x is ["exit",n,u] =>
    (adjExitLevel(u,seqnum,inc); seqnum>n => x; x.rest.first := n+inc)
  x is [op,:l] => for u in l repeat adjExitLevel(u,seqnum,inc)
 
wrapSEQExit l ==
  null rest l => first l
  [:c,x]:= [incExitLevel u for u in l]
  ["SEQ",:c,["exit",1,x]]
 
 
--% UTILITY FUNCTIONS
 
makeWorkerName op ==
  makeSymbol strconc(symbolName op,'";")
 
removeEnv t == [t.expr,t.mode,$EmptyEnvironment]  -- t is a triple
 
-- This function seems no longer used
--ordinsert(x,l) ==
--  null l => [x]
--  x=first l => l
--  _?ORDER(x,first l) => [x,:l]
--  [first l,:ordinsert(x,rest l)]
 
makeNonAtomic x ==
  x isnt [.,:.] => [x]
  x
 
flatten(l,key) ==
  null l => nil
  first l is [k,:r] and k=key => [:r,:flatten(rest l,key)]
  [first l,:flatten(rest l,key)]
 
genDomainVar() ==
  $Index:= $Index+1
  makeSymbol strconc('"#D",toString $Index)
 
genVariable() ==
  makeSymbol strconc('"#G",toString ($genSDVar:= $genSDVar+1))
 
genSomeVariable() ==
  makeSymbol strconc('"##",toString ($genSDVar:= $genSDVar+1))
 
listOfIdentifiersIn x ==
  ident? x => [x]
  x is [op,:l] => removeDuplicates ("append"/[listOfIdentifiersIn y for y in l])
  nil
 
mapInto(x,fn) == [apply(fn,[y]) for y in x]
 
numOfOccurencesOf(x,y) ==
  fn(x,y,0) where
    fn(x,y,n) ==
      null y => 0
      x=y => n+1
      atomic? y => n
      symbol? x and abstraction? y =>
        symbolMember?(x,y.absParms) => n
        fn(x,y.absBody,n)
      fn(x,first y,n)+fn(x,rest y,n)
 
compilerMessage(msg,args) ==
  $PrintCompilerMessageIfTrue => sayPatternMsg(msg,args)
 
printDashedLine() ==
  SAY
   '"--------------------------------------------------------------------------"
 
stackSemanticError(msg,expr) ==
  BUMPERRORCOUNT "semantic"
  if $insideCapsuleFunctionIfTrue then msg:= [$op,": ",:msg]
  if msg isnt [.,:.] then msg:= [msg]
  entry:= [msg,expr]
  if not listMember?(entry,$semanticErrorStack) then $semanticErrorStack:=
    [entry,:$semanticErrorStack]
  $scanIfTrue and $insideCapsuleFunctionIfTrue=true and #$semanticErrorStack-
    $initCapsuleErrorCount>3 => THROW("compCapsuleBody",nil)
  nil
 
stackWarning(msg,args == nil) ==
  msg := buildMessage(msg, args)
  if $insideCapsuleFunctionIfTrue then msg:= [$op,": ",:msg]
  if not listMember?(msg,$warningStack) then
    $warningStack:= [msg,:$warningStack]
  nil
 
unStackWarning(msg,args) ==
  msg := buildMessage(msg,args)
  if $insideCapsuleFunctionIfTrue then msg:= [$op,": ",:msg]
  $warningStack := remove!($warningStack,msg)
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
 
printString x == PRINC (string? x => x; PNAME x)
 
printAny x ==
  if x isnt [.,:.] then printString x else PRIN1 x
 
printSignature(before,op,[target,:argSigList]) ==
  printString before
  printString op
  printString ": _("
  if argSigList then
    printAny first argSigList
    for m in rest argSigList repeat (printString ","; printAny m)
  printString "_) -> "
  printAny target
  finishLine $OutputStream
 
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
  finishLine $OutputStream
  nil
 
++is domain of category form also of category form'?
++domain is only used for SubsetCategory resolution.
++ `db', if non-nil, is the DB for the constructor being compiled.
++Ensuring that X being a Ring means that it satisfies (Algebra X)
extendsCategoryForm(db,domain,form,form') ==
  form=form' => true
  form=$Category => nil
  form' is ["Join",:l] =>
    and/[extendsCategoryForm(db,domain,form,x) for x in l]
  form' is ["CATEGORY",.,:l] =>
    and/[extendsCategoryForm(db,domain,form,x) for x in l]
  form' is ["SubsetCategory",cat,dom] =>
    extendsCategoryForm(db,domain,form,cat) and isSubset(domain,dom,$e)
  form is ["Join",:l] =>
    or/[extendsCategoryForm(db,domain,x,form') for x in l]
  form is ["CATEGORY",.,:l] =>
    listMember?(form',l) or
      stackWarning('"not known that %1 is of mode %2p",[form',form]) or true
  -- if we are compiling the category `form', then we should look at
  -- the body as provided in the current definition, not a version
  -- possibly compiled previously that may have changed.
  -- FIXME: should not we go all the way down and implement
  --        polynormic recursion?
  domain = "$" and form = dbConstructorForm db => 
    extendsCategoryForm(db,domain, $currentCategoryBody, form')
  isCategoryForm(form,$EmptyEnvironment) =>
    -- -- If we have an existing definition for this category, use it.
    -- (db := constructorDB form.op) and loadDB db =>
    --   form' is ['SIGNATURE,op,types,:.] => assoc([op,args],dbOperations db)
    --   form' is ['ATTRIBUTE,a] => assoc(a,dbAttributes db)
    --   subst := pairList(dbConstructorForm(db).args,form.args)
    --   or/[extendsCategoryForm(db,domain,applySubst(subst,cat),form')
    --        for [cat,:.] in dbAncestors db]
    -- Otherwise constructs the associated domain shell
    formVec:=(compMakeCategoryObject(form,$e)).expr
            --Must be $e to pick up locally bound domains
    form' is ["SIGNATURE",op,args,:.] =>
        assoc([op,args],categoryExports formVec) or
            assoc(substitute(domain,"$",[op,args]),
                  substitute(domain,"$",categoryExports formVec))
    form' is ["ATTRIBUTE",at] =>
         assoc(at,categoryAttributes formVec) or
            assoc(substitute(domain,"$",at),substitute(domain,"$",categoryAttributes formVec))
    form' is ["IF",:.] => true --temporary hack so comp won't fail
    listMember?(form',categoryPrincipals formVec) or
     listMember?(form',substitute(domain,"$",categoryPrincipals formVec)) or
      (or/
        [extendsCategoryForm(db,domain,substitute(domain,"$",cat),form')
          for [cat,:.] in categoryAncestors formVec])
  nil
 
getmode(x,e) ==
  prop:=getProplist(x,e)
  u := symbolTarget("value",prop) => u.mode
  symbolTarget("mode",prop)
 
getmodeOrMapping(x,e) ==
  u:= getmode(x,e) => u
  (u:= get(x,"modemap",e)) is [[[.,:map],.],:.] => ["Mapping",:map]
  nil
 
outerProduct l ==
                --of a list of lists
  null l => [nil]
  "append"/[[[x,:y] for y in outerProduct rest l] for x in first l]
 
sublisR(al,u) ==
  u isnt [.,:.] => u
  y:= rassoc(t:= [sublisR(al,x) for x in u],al) => y
  true => t
 
substituteOp(op',op,x) ==
  x isnt [.,:.] => x
  [(op=(f:= first x) => op'; f),:[substituteOp(op',op,y) for y in rest x]]
 
--substituteForFormalArguments(argl,expr) ==
--  applySubst([[v,:a] for a in argl for v in $FormalMapVariableList],expr)
 
 -- following is only intended for substituting in domains slots 1 and 4
 -- signatures and categories
sublisV(p,e) ==
  (p isnt [.,:.] => e; suba(p,e)) where
    suba(p,e) ==
      string? e => e
      -- no need to descend vectors unless they are categories
      categoryObject? e => vector [suba(p,e.i) for i in 0..maxIndex e]
      e isnt [.,:.] => (y := objectAssoc(e,p) => rest y; e)
      u:= suba(p,first e)
      v:= suba(p,rest e)
      sameObject?(first e,u) and sameObject?(rest e,v) => e
      [u,:v]

--% DEBUGGING PRINT ROUTINES used in breaks
 
old2NewModemaps x ==
--  [[dcSig,pred] for [dcSig,[pred,:.],:.] in x]
  x is [dcSig,[pred,:.],:.]  =>  [dcSig,pred]
  x

displayProplist(x,alist) ==
  sayBrightly ["properties of",'"%b",x,'"%d",":"]
  fn alist where
    fn alist ==
      alist is [[prop,:val],:l] =>
        if prop="value" then val:= [val.expr,val.mode,'"..."]
        sayBrightly ["   ",'"%b",prop,'"%d",": ",val]
        fn deleteAssoc(prop,l)
 
displayModemaps E ==
  listOfOperatorsSeenSoFar:= nil
  for x in E for i in 1.. repeat
    for y in x for j in 1.. repeat
      for z in y | not member(first z,listOfOperatorsSeenSoFar) and
        (modemaps := symbolTarget("modemap",rest z)) repeat
          listOfOperatorsSeenSoFar:= [first z,:listOfOperatorsSeenSoFar]
          displayOpModemaps(first z,modemaps)
 
--% General object traversal functions
 
GCOPY ob == copyTree ob  -- for now
 
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

++ Return the linkage name of the exported operation associated with
++ slot number `slot'.  A nil entry means that either the operation
++ is not defined, or it is conditional.
getCapsuleDirectoryEntry slot ==
  scalarTarget(slot,$capsuleDirectory)

++ Update the current capsule directory with entry controlled by 
++ predicate `pred'.
updateCapsuleDirectory(entry,pred) ==
  pred isnt true => nil
  $capsuleDirectory := [entry,:$capsuleDirectory]




--% Tree walkers

++ Walk VM conditional forms mutating sub-forms with the unary
++ function `fun'
mutateConditionalFormWithUnaryFunction(form,fun) ==
  form isnt ['%when,:body] => form
  for clauses in tails body repeat
    -- a clause is a list of forms
    for subForms in tails first clauses repeat
      subForms.first := apply(fun,[first subForms])
  form

++ Walk VM a binding-form mutating enclosed expression forms with
++ the unary function `fun'.  Every sub-form is visited except
++ local variable declarations, though their initializers
++ are visited.
mutateBindingFormWithUnaryFunction(form,fun) ==
  form isnt [op,inits,:body] and op in '(LET %bind) => form
  for defs in tails inits repeat
    def := first defs
    def isnt [.,:.] => nil -- no initializer
    def.rest.first := apply(fun,[second def])
  for stmts in tails body repeat
    stmts.first := apply(fun,[first stmts])
  form

--% 

--middleEndExpand: %Form -> %Code
middleEndExpand x ==
  x is '%void => '""  -- NIL would have caused havoc elsewhere
  x is '%false or x is '%nil => 'NIL
  ident? x and (x' := x has %Rename) => x'
  atomic? x => x
  [op,:args] := x
  ident? op and (fun := getOpcodeExpander op) =>
    middleEndExpand apply(fun,[x])
  a := middleEndExpand op
  b := middleEndExpand args
  sameObject?(a,op) and sameObject?(b,args) => x
  [a,:b]


--% A function is simple if it looks like a super combinator, and it
--% does not use its environment argument.  They can be safely replaced
--% by more efficient (hopefully) functions.

compileTimeBindingOf u ==
  symbol? u => u
  null(name:= BPINAME u)  => keyedSystemError("S2OO0001",[u])
  name="Undef" => MOAN "optimiser found unknown function"
  name
 
getFunctionReplacement name ==
  property(compileTimeBindingOf name,'SPADreplace)

++ remove any replacement info possibly associated with `name'.
clearReplacement name ==
  property(name,"SPADreplace") := nil
  property(name,'%redex) := nil

evalAndPrintBackendStmt(lib,stmt) ==
  eval stmt
  printBackendStmt(lib,stmt)

++ Register the inlinable form of a function.
registerFunctionReplacement(db,name,body) ==
  evalAndPrintBackendStmt(dbLibstream db,
    ["PUT",MKQ name,MKQ "SPADreplace",quoteMinimally body])

++ Remember the redex form of this function
registerRedexForm(db,name,parms,body) ==
  evalAndPrintBackendStmt(dbLibstream db,
    ["PUT",quote name,quote '%redex,quote ['ILAM,parms,body]])

++ Retrieve the redex form of the function `name'.
redexForm name ==
  property(name,'%redex) 

++ Attempt to resolve the indirect reference to a constant form
++ `[spadConstant,$,n]' to a simpler expression
resolveConstantForm form ==
  fun := getCapsuleDirectoryEntry third form or return form
  -- Conservatively preserve object identity and storage 
  -- consumption by not folding non-atomic constant forms.
  getFunctionReplacement fun isnt ['XLAM,=nil,body] => form
  atomic? body or isVMConstantForm body => body
  form

mutateArgumentList(args,fun) ==
  for x in tails args repeat
    arg := first x
    atomic? arg => nil
    x.first := apply(fun,[arg])
  args

inlineDirectCall call ==
  x := redexForm call.op => doInlineCall(call.args,x.absParms,x.absBody)
  fun := getFunctionReplacement call.op or return call
  -- the renaming case
  symbol? fun =>
    call.op := fun
    NBUTLAST call
  -- the substitution case.
  fun is ["XLAM",parms,body] =>
    -- almost constant function
    parms = nil => body
    -- identity function too
    parms is [=body] => first call.args
    -- conservatively approximate eager semantics
    every?(function sideEffectFree?,call.args) =>
      -- alpha rename before substitution.
      newparms := [gensym() for p in parms]
      body := applySubst(pairList(parms,newparms),body)
      applySubst!(pairList(newparms,call.args),body)
    -- a non-side effect free argument used exactly once is OK.
    parms is [p] and numOfOccurencesOf(p,body) = 1 =>
      substitute(first call.args,p,body)
    -- get cute later.
    call
  call

resolveIndirectCall form ==
  fun := lastNode form
  fun isnt [['%tref,'$,n]] => form
  op := getCapsuleDirectoryEntry n or return form
  form.op := op
  fun.first := '$
  inlineDirectCall form
    
++ Walk `form' and replace simple functions as appropriate.
replaceSimpleFunctions form ==
  atomic? form => form
  form.op is 'DECLARE => form 
  form.op is '%when =>
    mutateConditionalFormWithUnaryFunction(form,function replaceSimpleFunctions)
  form.op in '(LET %bind) =>
    mutateBindingFormWithUnaryFunction(form,function replaceSimpleFunctions)
  form is ['spadConstant,'$,.] => resolveConstantForm form
  -- process argument first.
  mutateArgumentList(form.args,function replaceSimpleFunctions)
  form.op is 'SPADCALL => resolveIndirectCall form
  -- see if we know something about this function.
  [fun,:args] := form
  symbol? fun => inlineDirectCall form
  not cons? fun => form
  form.first := replaceSimpleFunctions fun
  form


++ We are processing a function definition with parameter list `vars'
++ and body given by `body'.  If `body' is a forwarding function call, 
++ return the target function.  Otherwise, return nil.
forwardingCall?(vars,body) ==
  vars is [:vars',.] and body is [fun,: =vars'] and ident? fun => fun
  nil


++ Return true if `form' has a linear usage of all variables in `vars'.
usesVariablesLinearly?(form,vars) ==
  atomic? form => true
  form.op is '%when =>
    and/[sideEffectFree? p and usesVariablesLinearly?(c,vars)
           for [p,c] in form.args]
  and/[numOfOccurencesOf(var,form) < 2 for var in vars] 

++ List of builtin operators we should not attempt to promote
++ to inlinable status.
$NonExpandableOperators ==
  '(%store %LET SPADCALL %bind LET)

++ We are processing a function definition with parameter list `vars'
++ and body given by `body'.  If `body' is a form that can be inlined,
++ then return the inline form.  Otherwise, return nil.
expandableDefinition?(vars,body) ==
  expand? :=
    -- We definitely don't want to expand a form that uses
    -- the domain of computation environment.
    vars isnt [:vars',env] or usesVariable?(body,env) => false

    -- Constants are currently implemented as niladic functions, and
    -- we want to avoid disturbing object identity, so we rule
    -- out use of side-effect full operators.  
    -- FIXME: This should be done only for constant creators.
    null vars' => sideEffectFree? body

    atomic? body => true
    [op,:args] := body
    not ident? op or symbolMember?(op,$NonExpandableOperators) => false
    every?(function atomic?,args)
      or semiSimpleRelativeTo?(body,$simpleVMoperators) =>
                usesVariablesLinearly?(body,vars')
    false
  expand? =>
    body is [fun,: =vars'] and symbol? fun => fun
    ['XLAM,vars',body]
  nil

++ A list of routines for diagnostic reports.  These functions, in an
++ abstract sense, have type: forall T: Type . String -> T, so they
++ can be used in T-returning functions, for any T.  
$coreDiagnosticFunctions == 
  '(error userError systemError)

almostPure? x ==
  ops := [:$coreDiagnosticFunctions,:$VMsideEffectFreeOperators]
  semiSimpleRelativeTo?(x,ops)

++ `defs' is a list of function definitions from the current domain.
++ Walk that list and replace references to unconditional operations
++ with their corresponding linkage names.  
foldExportedFunctionReferences(db,defs) ==
  for fun in defs repeat
    fun isnt [name,lamex] => nil
    getFunctionReplacement name => nil
    lamex isnt ['%lambda,vars,body] => nil
    body := replaceSimpleFunctions body
    form := expandableDefinition?(vars,body) =>
      registerFunctionReplacement(db,name,form)
      second(fun) := ["LAMBDA",vars,["DECLARE",["IGNORE",last vars]],body]
    if almostPure? body then
      registerRedexForm(db,name,vars,body)
    lamex.absBody := body
  defs

++ record optimizations permitted at level `level'.
setCompilerOptimizations level ==
  level = nil => nil
  integer? level =>
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
proclaimCapsuleFunction(db,op,sig) ==
  printBackendStmt(dbLibstream db,
    ["DECLAIM",["FTYPE",
       ["FUNCTION",[:[vmType first d for d in tails rest sig],"%Shell"], 
          vmType first sig],op]]) where
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
          $functorForm isnt [.,:.] => [$functorForm] 
          normalize($functorForm,top?)
        d isnt [.,:.] => 
          top? => "%Thing"
          getmode(d,$e) => "*"
          d
        [first d, :[normalize(first args,false) for args in tails rest d]]

++ Lisp back end compiler for %slam forms [namd,args,:body].
++ A %slam form is one that is `functional' in the sense that
++ its values are cached, so that equal lists of argument values
++ yield equal values.  The arguments-value pairs are stored
++ in a hash table.  This backend compiler is used to compile constructors.
backendCompileSPADSLAM: (%Database,%Symbol,%List %Symbol,%Code) -> %Symbol
backendCompileSPADSLAM(db,name,args,body) ==
  al := mkCacheName name       -- global name for the cache hash table.
  auxfn := makeWorkerName name -- name of the worker function.
  g2 := gensym()               -- local name for the cache value.
  u := 
    args = nil => [nil,[auxfn]]
    args is [g] => [g,[auxfn,g]]
    [gensym(),[auxfn,:args]]
  key := first u              -- key into the instantiation table cache
  app := second u             -- code to compute value
  code := 
    args = nil => ["COND",[al],[true,["SETQ",al,app]]]
    [binder,:inits] :=
      args is [.] => ["LET",[g2,["assoc",key,al]]]
      ["LET*",[key,["LIST",:args]],[g2,["assoc",key,al]]]
    [binder,inits,
      ["COND",
        [g2,["CDR",g2]],
          [true, 
            ["PROGN",["SETQ",g2,app],
               ["SETQ",al,["cons5",["CONS",key,g2],al]],g2]]]]
  -- define the global cache.
  evalAndPrintBackendStmt(dbLibstream db,['DEFPARAMETER,al,nil])
  assembleCode [auxfn,["LAMBDA",args,:body]]
  assembleCode [name,["LAMBDA",args,code]]

backendCompile2: (%Maybe %Database,%Code) -> %Symbol
backendCompile2 (db,code) ==
  code isnt [name,[type,args,:body]] =>
    systemError ['"parenthesis error in: ", code]
  type = '%slam => backendCompileSPADSLAM(db,name,args,body)
  assembleCode [name,[type,args,:body]]

++ returns all fuild variables contained in `x'.  Fuild variables are
++ identifiers starting with '$', except domain variable names.
backendFluidize x ==
  ident? x and x ~= "$" and x ~= "$$" and
    stringChar(symbolName x,0) = char "$" and
      not digit? stringChar(symbolName x,1) => x
  atomic? x => nil
  first x is "FLUID" => second x
  a := backendFluidize first x
  b := backendFluidize rest x
  a = nil => b
  [a,:b]


$FluidVars := []
$LocalVars := []
$SpecialVars := []


++ push `x' into the list of local variables.
pushLocalVariable: %Symbol -> %List %Symbol
pushLocalVariable x ==
  p := symbolName x
  x ~= "$" and stringChar(p,0) = char "$" and
    stringChar(p,1) ~= char "," and not digit? stringChar(p,1) => nil
  PUSH(x,$LocalVars)

isLispSpecialVariable x ==
  s := symbolName x
  stringChar(s,0) = char "$" and #s > 1 and
    alphabetic? stringChar(s,1) and not readOnly? x
  
noteSpecialVariable x ==
  $SpecialVars := insert(x,$SpecialVars)

--%
--% Compile Time operation lookup for the benefit of domain inlining.
--%

++ If `x' is a formal map variable, returns its position.
++ Otherwise return nil.
formal?: %Symbol -> %Maybe %Short
formal? x ==
  or/[i for i in 0.. for y in $FormalMapVariableList | symbolEq?(x,y)]

++ Expand the form at position `slot' in the domain template `shell'
++ with argument list `args'.
expandFormTemplate(shell,args,slot) ==
  integer? slot =>
    slot = 0 => "$"
    slot = 2 => "$$"
    expandFormTemplate(shell,args,vectorRef(shell,slot))
  slot isnt [.,:.] => slot
  slot is ["local",parm] and (n := formal? parm) => 
    args.n   -- FIXME: we should probably expand with dual signature
  slot is ['%eval,val] => val
  slot is ['QUOTE,val] => 
    string? val => val
    slot
  [expandFormTemplate(shell,args,i) for i in slot]

++ Compare the form at `slot' in the domain templare `shell'
++ for equality with `form'.
equalFormTemplate(shell,args,slot,form) ==
  integer? slot =>
    slot = 0 => form = "$"
    slot = 2 => form = "$$"
    equalFormTemplate(shell,args,vectorRef(shell,slot),form)
  slot is ["local",parm] and (n := formal? parm) => 
    equalFormTemplate(shell,args,args.n,form)
  slot is ['%eval,val] => form = val
  slot is ['QUOTE,val] => 
     string? val or symbol? val or integer? val => val = form
     slot = form
  slot isnt [.,:.] or form isnt [.,:.] => form = slot
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
    n := arrayRef(funDesc,start)       -- arity of current operator
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
        integer? loc => "ambiguous"
        arrayRef(funDesc,i + n + 1)
    start := start + n + 4
  loc

++ Subroutine of lookupDefiningFunction.
lookupInheritedDefiningFunction(op,sig,shell,args,slot) ==
  dom := expandFormTemplate(shell,args,slot)
  dom isnt [.,:.] or dom is ["local",:.] => nil
  lookupDefiningFunction(op,sig,dom)

++ Return the name of the function definition that explicitly implements
++ the operation `op' with signature `sig' in the domain of 
++ computation `dc'.  Otherwise, return nil.
++ Note: Only a function defined by the domain template, or its add-chains,
++       and that is unambiguous is returned.  In particular, this
++       function defaulting packages.
lookupDefiningFunction(op,sig,dc) ==
  -- 1. Read domain information, if available.  Silently give up if
  -- the constructor is just not there
  [ctor,:args] := dc
  db := constructorDB ctor or return nil -- we only deal with instantiations
  loadDBIfCan db
  dbTemplate db = nil => nil  -- incomplete functor
  -- 1.1. Niladic constructors don't need approximation.
  --      FIXME: However, there may be cylic dependencies
  --      such as AN ~> IAN ~> EXPR INT ~> AN that prevents
  --      us from full evaluation.  
  args = nil and symbolMember?(ctor,$SystemInlinableConstructorNames) =>
    compiledLookup(op,sig,dc)
  -- 1.2. Don't look into defaulting package
  isDefaultPackageName ctor => nil
  infovec := property(ctor,'infovec) or return nil
  -- 1.3. We need information about the original domain template
  shell := dbTemplate db               -- domain template
  opTable := second infovec            -- operator-code table
  opTableLength := #opTable
  forgetful := dbLookupFunction db is 'lookupIncomplete

  -- 2. Get the address range of op's descriptor set
  [.,.,.,:funDesc] := fourth infovec
  index := getOpCode(op, opTable, opTableLength - 1)
  -- 2.1. For a forgetful functor, try the add chain
  index = nil =>
    forgetful and lookupInheritedDefiningFunction(op,sig,shell,args,5)
  -- 2.2. The operation is either defined here, or is available
  --      from category package defaults.
  limit := 
    index + 2 < opTableLength => vectorRef(opTable,index + 2)
    #funDesc 

  -- 3. Locate the descriptor with matching signature
  loc := getFunctionTemplate(sig,opTable.index,limit,shell,args,funDesc)

  -- 4. Look into the add-chain if necessary
  loc = nil =>
    lookupInheritedDefiningFunction(op,sig,shell,args,domainRef(shell,5))

  -- 5. Give up if the operation is overloaded on semantics predicates.
  loc is 'ambiguous => nil

  -- 6. We have a location to a function descriptor.
  fun := domainRef(shell,loc)
  -- 6.1. A constant producing function?
  fun is ['%constant,[fun',.]] => fun'
  -- 6.2. An inherited function?
  fun is [idx,:.] => 
    not integer? idx => nil          -- a UFO?
    loc := arrayRef(funDesc,idx + 1)
    if loc = 0 then loc := 5
    domainRef(shell,loc) = nil => nil
    lookupInheritedDefiningFunction(op,sig,shell,args,shell.loc)
  -- 6.3. Whatever.
  fun

++ flag parameters needs to be made atomic, otherwise Lisp is confused.
++ We try our best to preserve
++ Note that we don't need substitution in the body because flag
++ parameters are never used in the body.
cleanParameterList! parms ==
  count := 0
  for vars in tails parms repeat
    v := first vars
    ident? v => nil
    t := nil
    until not symbolMember?(t,parms) repeat
      count := count + 1
      t := makeSymbol strconc('"T",toString count)
    vars.first := t
  parms

--%
--% Other compiler artifact support
--%

moveLibdirByCopy lib ==
  checkMkdir libDirname lib
  for src in directoryEntries libStationaryDirname lib repeat
    dst := makeFilePath(directory <- relativeDirname libDirname lib,
             name <- filePathName src, type <- filePathType src)
    copyFile(filePathString src,filePathString dst)
  removeFile libStationaryDirname lib = 0 => libDirname lib
  systemError ['"Could not remove stationary directory",
                :bright libStationaryDirname lib]
