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

import nruncomp
import g_-error
import database
import modemap

namespace BOOT

module define where
  compDefine: (%Form,%Mode,%Env) -> %Maybe %Triple 
  compSubDomain: (%Form,%Mode,%Env) -> %Maybe %Triple
  compCapsule: (%Form, %Mode, %Env) -> %Maybe %Triple
  compJoin: (%Form,%Mode,%Env) -> %Maybe %Triple 
  compAdd: (%Form, %Mode, %Env) -> %Maybe %Triple 
  compCategory: (%Form,%Mode,%Env) -> %Maybe %Triple 


--%

$newCompCompare := false

++ List of mutable domains.
$mutableDomains := nil

++ True if the current constructor being compiled instantiates
++ mutable domains or packages.  Default is `false'.
$mutableDomain := false

++ when non nil, holds the declaration number of a function in a capsule.
$suffix := nil

$doNotCompileJustPrint := false

++ stack of pending capsule function definitions.
$capsuleFunctionStack := []

$functionStats := nil
$functorStats := nil

$lisplibCategory := nil
$lisplibAncestors := nil
$lisplibAbbreviation := nil
$CheckVectorList := []
$setelt := nil
$pairlis := []
$functorTarget := nil
$condAlist := []
$uncondAlist := []
$NRTslot1PredicateList := []
$NRTattributeAlist := []
$NRTslot1Info := nil
$NRTdeltaListComp := []
$template := nil
$signature := nil
$isOpPackageName := false
$lisplibCategoriesExtended := []
$lookupFunction := nil
$byteAddress := nil
$byteVec := nil
$lisplibSlot1 := nil
$sigAlist := []
$predAlist := []
$argumentConditionList := []
$finalEnv := nil
$initCapsuleErrorCount := nil
$CapsuleModemapFrame := nil
$CapsuleDomainsInScope := nil
$signatureOfForm := nil
$addFormLhs := nil
$lisplibSuperDomain := nil
$sigList := []
$atList := []


++ True if the current functor definition refines a domain.
$subdomain := false

--%

compDefineAddSignature: (%Form,%Signature,%Env) -> %Env
DomainSubstitutionFunction: (%List,%Form) -> %Form 


--% Subdomains

++ We are defining a functor with head given by `form', as a subdomain
++ of the domain designated by the domain form `super', and predicate
++ `pred' (a VM instruction form).  Emit appropriate info into the
++ databases.
emitSubdomainInfo(form,super,pred) ==
  pred := eqSubst($AtVariables,rest form,pred)
  super := eqSubst($AtVariables,rest form,super)
  evalAndRwriteLispForm("evalOnLoad2",["noteSubDomainInfo",
     quoteForm first form,quoteForm super, quoteForm pred])


++ List of operations defined in a given capsule
++ Each item on this list is of the form
++    (op sig pred)
++ where
++   op:   name of the operation
++   sig:  signature of the operation
++   pred: scope predicate of the operation.
$capsuleFunctions := nil

++ record that the operation `op' with signature `sig' and predicate
++ `pred' is defined in the current capsule of the current domain
++ being compiled.
noteCapsuleFunctionDefinition(op,sig,pred) ==
  member([op,sig,pred],$capsuleFunctions) =>
    stackAndThrow('"redefinition of %1b: %2 %3",
      [op,formatUnabbreviated ["Mapping",:sig],formatIf pred])
  $capsuleFunctions := [[op,sig,pred],:$capsuleFunctions]

++ Clear the list of functions defined in the last domain capsule.
clearCapsuleFunctionTable() ==
  $capsuleFunctions := nil


++ List of exports (paireed with scope predicate) declared in
++ the category of the currend domain or package.
++ Note: for category packages, this list is nil.
$exports := nil

noteExport(form,pred) ==
  -- don't recheck category package exports; we just check
  -- them when defining the category.  Plus, we might actually
  -- get indirect duplicates, which is OK.
  $insideCategoryPackageIfTrue => nil
  member([form,pred],$exports) =>
    stackAndThrow('"redeclaration of %1 %2",
      [form,formatIf pred])
  $exports := [[form,pred],:$exports]

clearExportsTable() ==
  $exports := nil

makePredicate l ==
  null l => true
  MKPF(l,"and")

--% FUNCTIONS WHICH MUNCH ON == STATEMENTS

++ List of reserved identifiers for which the compiler has special
++ meanings and that shall not be redefined.
$reservedNames == '(per rep _$)

++ Check that `var' (a variable of parameter name) is not a reversed name.
checkVariableName var ==
  MEMQ(var,$reservedNames) =>
    stackAndThrow('"You cannot use reserved name %1b as variable",[var])

checkParameterNames parms ==
  for p in parms repeat
    checkVariableName p

compDefine(form,m,e) ==
  $macroIfTrue: local := false
  compDefine1(form,m,e)

++ We are about to process the body of a capsule.  Check the form of
++ `Rep' definition, and whether it is appropriate to activate the
++ implicitly generated morphisms
++     per: Rep -> %
++     rep: % -> Rep
++ as local inline functions.
checkRepresentation: (%Form,%List,%Env) -> %Env
checkRepresentation(addForm,body,env) ==
  domainRep := nil
  hasAssignRep := false        -- assume code does not assign to Rep.
  viewFuns := nil

  null body => env             -- Don't be too hard on nothing.
  
  -- Locate possible Rep definition
  for [stmt,:.] in tails body repeat
    stmt is ["%LET","Rep",val] =>
      domainRep ~= nil =>
        stackAndThrow('"You cannot assign to constant domain %1b",["Rep"])
      if addForm = val then
        stackWarning('"OpenAxiom suggests removing assignment to %1b",["Rep"])
      else if addForm ~= nil then
        stackWarning('"%1b differs from the base domain",["Rep"])
      return hasAssignRep := true
    stmt is ["MDEF",["Rep",:.],:.] =>
      stackWarning('"Consider using == definition for %1b",["Rep"])
      return hasAssignRep := true
    stmt is ["IF",.,:l] or stmt is ["SEQ",:l] or stmt is ["exit",:l] =>
      checkRepresentation(nil,l,env)
    stmt isnt ["DEF",[op,:args],sig,.,val] => nil -- skip for now.
    op in '(rep per) =>
      domainRep ~= nil =>
        stackAndThrow('"You cannot define implicitly generated %1b",[op])
      viewFuns := [op,:viewFuns]
    op ~= "Rep" => nil        -- we are only interested in Rep definition
    domainRep := val
    viewFuns ~= nil =>
      stackAndThrow('"You cannot define both %1b and %2b",["Rep",:viewFuns])
    -- A package has no "%".
    $functorKind = "package" =>
      stackAndThrow('"You cannot define %1b in a package",["Rep"])
    -- It is a mistake to define Rep in category defaults
    $insideCategoryPackageIfTrue =>
      stackAndThrow('"You cannot define %1b in category defaults",["Rep"])
    if args ~= nil then
      stackAndThrow('"%1b does take arguments",["Rep"])
    if first sig ~= nil then
      stackAndThrow('"You cannot specify type for %1b",["Rep"])
    -- Now, trick the rest of the compiler into believing that
    -- `Rep' was defined the Old Way, for lookup purpose.
    rplac(first stmt,"%LET")
    rplac(rest stmt,["Rep",domainRep])
    $useRepresentationHack := false          -- Don't confuse `Rep' and `%'.

  -- Shall we perform the dirty tricks?
  if hasAssignRep then
    $useRepresentationHack := true
  -- Domain extensions with no explicit Rep definition have the
  -- the base domain as representation (at least operationally).
  else if null domainRep and addForm ~= nil then
    if $functorKind = "domain" and addForm isnt ["%Comma",:.] then
      domainRep :=
        addForm is ["SubDomain",dom,.] => 
          $subdomain := true
          dom
        addForm
      base := compForMode(domainRep,$EmptyMode,env) or 
        stackAndThrow('"1b is not a domain",[domainRep])
      $useRepresentationHack := false
      env := put("Rep","value",removeEnv base,env)
      -- ??? Maybe we should also make Rep available as macro?
  env


compDefine1: (%Form,%Mode,%Env) -> %Maybe %Triple 
compDefine1(form,m,e) ==
  $insideExpressionIfTrue: local:= false
  --1. decompose after macro-expanding form
  ['DEF,lhs,signature,specialCases,rhs]:= form:= macroExpand(form,e)
  $insideWhereIfTrue and isMacro(form,e) and (m=$EmptyMode or m=$NoValueMode)
     => [lhs,m,put(first lhs,"macro",rhs,e)]
  checkParameterNames rest lhs
  null signature.target and not MEMQ(KAR rhs,$BuiltinConstructorNames) and
    (sig:= getSignatureFromMode(lhs,e)) =>
  -- here signature of lhs is determined by a previous declaration
      compDefine1(['DEF,lhs,[first sig,:rest signature],specialCases,rhs],m,e)
  if signature.target=$Category then $insideCategoryIfTrue:= true
 
-- RDJ (11/83): when argument and return types are all declared,
--  or arguments have types declared in the environment,
--  and there is no existing modemap for this signature, add
--  the modemap by a declaration, then strip off declarations and recurse
  e := compDefineAddSignature(lhs,signature,e)
-- 2. if signature list for arguments is not empty, replace ('DEF,..) by
--       ('where,('DEF,..),..) with an empty signature list;
--     otherwise, fill in all NILs in the signature
  or/[x ~= nil for x in rest signature] => compDefWhereClause(form,m,e)
  signature.target=$Category =>
    compDefineCategory(form,m,e,nil,$formalArgList)
  isDomainForm(rhs,e) and not $insideFunctorIfTrue =>
    if null signature.target then signature:=
      [getTargetFromRhs(lhs,rhs,giveFormalParametersValues(rest lhs,e)),:
          rest signature]
    rhs:= addEmptyCapsuleIfNecessary(signature.target,rhs)
    compDefineFunctor(['DEF,lhs,signature,specialCases,rhs],m,e,nil,
      $formalArgList)
  null $form => stackAndThrow ['"bad == form ",form]
  newPrefix:=
    $prefix => INTERN STRCONC(encodeItem $prefix,'",",encodeItem $op)
    getConstructorAbbreviationFromDB $op
  compDefineCapsuleFunction(form,m,e,newPrefix,$formalArgList)

compDefineAddSignature([op,:argl],signature,e) ==
  (sig:= hasFullSignature(argl,signature,e)) and
   not assoc(['$,:sig],LASSOC('modemap,getProplist(op,e))) =>
     declForm:=
       [":",[op,:[[":",x,m] for x in argl for m in rest sig]],first signature]
     [.,.,e]:= comp(declForm,$EmptyMode,e)
     e
  e
 
hasFullSignature(argl,[target,:ml],e) ==
  target =>
    u:= [m or get(x,"mode",e) or return 'failed for x in argl for m in ml]
    u~='failed => [target,:u]
 
addEmptyCapsuleIfNecessary: (%Form,%Form) -> %Form
addEmptyCapsuleIfNecessary(target,rhs) ==
  MEMQ(KAR rhs,$SpecialDomainNames) => rhs
  ['add,rhs,['CAPSULE]]

getTargetFromRhs: (%Form, %Form, %Env) -> %Form 
getTargetFromRhs(lhs,rhs,e) ==
  --undeclared target mode obtained from rhs expression
  rhs is ['CAPSULE,:.] =>
    stackSemanticError(['"target category of ",lhs,
      '" cannot be determined from definition"],nil)
  rhs is ['SubDomain,D,:.] => getTargetFromRhs(lhs,D,e)
  rhs is ['add,D,['CAPSULE,:.]] => getTargetFromRhs(lhs,D,e)
  rhs is ['Record,:l] => ['RecordCategory,:l]
  rhs is ['Union,:l] => ['UnionCategory,:l]
  (compOrCroak(rhs,$EmptyMode,e)).mode
 
giveFormalParametersValues(argl,e) ==
  for x in argl repeat
    e:= put(x,'value,[genSomeVariable(),get(x,'mode,e),nil],e)
  e


macroExpandInPlace: (%Form,%Env) -> %Form 
macroExpandInPlace(x,e) ==
  y:= macroExpand(x,e)
  atom x or atom y => y
  RPLACA(x,first y)
  RPLACD(x,rest y)
  x

macroExpand: (%Form,%Env) -> %Form 
macroExpand(x,e) ==   --not worked out yet
  atom x => 
    u:= get(x,"macro",e) => macroExpand(u,e)
    x
  x is ['DEF,lhs,sig,spCases,rhs] =>
    ['DEF,macroExpand(lhs,e),macroExpandList(sig,e),macroExpandList(spCases,e),
      macroExpand(rhs,e)]
  macroExpandList(x,e)
 
macroExpandList(l,e) ==
  -- macros should override niladic props
  (l is [name]) and IDENTP name and niladicConstructorFromDB name and
        (u := get(name,"macro", e)) => macroExpand(u,e)
  [macroExpand(x,e) for x in l]

--% constructor evaluation
 
mkEvalableCategoryForm c ==
  c is [op,:argl] =>
    op="Join" => ["Join",:[mkEvalableCategoryForm x for x in argl]]
    op is "DomainSubstitutionMacro" => mkEvalableCategoryForm second argl
    op is "mkCategory" => c
    MEMQ(op,$CategoryNames) =>
      ([x,m,$e]:= compOrCroak(c,$EmptyMode,$e); m=$Category => x)
    --loadIfNecessary op
    getConstructorKindFromDB op = 'category or
      get(op,"isCategory",$CategoryFrame) =>
        [op,:[MKQ x for x in argl]]
    [x,m,$e]:= compOrCroak(c,$EmptyMode,$e)
    m=$Category => x
  MKQ c
 
++ Return true if we should skip compilation of category package.
++ This situation happens either when there is no default, of we are in
++ bootstrap mode, or we are compiling only for exports.
skipCategoryPackage? capsule ==
  null capsule or $bootStrapMode or $compileExportsOnly

compDefineCategory1(df is ['DEF,form,sig,sc,body],m,e,prefix,fal) ==
  categoryCapsule :=
    body is ['add,cat,capsule] =>
      body := cat
      capsule
    nil
  [d,m,e]:= compDefineCategory2(form,sig,sc,body,m,e,prefix,fal)
  if not skipCategoryPackage? categoryCapsule then [.,.,e] :=
    $insideCategoryPackageIfTrue: local := true
    $categoryPredicateList: local :=
        makeCategoryPredicates(form,$lisplibCategory)
    T := compDefine1(mkCategoryPackage(form,cat,categoryCapsule),$EmptyMode,e)
           or return stackSemanticError(
                        ['"cannot compile defaults of",:bright opOf form],nil)
    if $compileDefaultsOnly then 
      [d,m,e] := T
  [d,m,e]

makeCategoryPredicates(form,u) ==
      $tvl: local := TAKE(#rest form,$TriangleVariableList)
      $mvl: local := TAKE(#rest form,rest $FormalMapVariableList)
      fn(u,nil) where
        fn(u,pl) ==
          u is ['Join,:.,a] => fn(a,pl)
          u is ["IF",p,:x] => fnl(x,insert(EQSUBSTLIST($mvl,$tvl,p),pl))
          u is ["has",:.] => insert(EQSUBSTLIST($mvl,$tvl,u),pl)
          u is [op,:.] and op in '(SIGNATURE ATTRIBUTE) => pl
          atom u => pl
          fnl(u,pl)
        fnl(u,pl) ==
          for x in u repeat pl := fn(x,pl)
          pl
 
mkCategoryPackage(form is [op,:argl],cat,def) ==
  packageName:= INTERN(STRCONC(PNAME op,'"&"))
  packageAbb := INTERN(STRCONC(getConstructorAbbreviationFromDB op,'"-"))
  $options:local := []
  -- This stops the next line from becoming confused
  abbreviationsSpad2Cmd ['domain,packageAbb,packageName]
  -- This is a little odd, but the parser insists on calling
  -- domains, rather than packages
  nameForDollar := first SETDIFFERENCE('(S A B C D E F G H I),argl)
  packageArgl := [nameForDollar,:argl]
  capsuleDefAlist := fn(def,nil) where fn(x,oplist) ==
    atom x => oplist
    x is ['DEF,y,:.] => [y,:oplist]
    fn(rest x,fn(first x,oplist))
  catvec := eval mkEvalableCategoryForm form
  fullCatOpList:=(JoinInner([catvec],$e)).1
  catOpList :=
    [['SIGNATURE,op1,sig] for [[op1,sig],:.] in fullCatOpList
        | assoc(op1,capsuleDefAlist)]
  null catOpList => nil
  packageCategory := ['CATEGORY,'domain,
                     :SUBLISLIS(argl,$FormalMapVariableList,catOpList)]
  nils:= [nil for x in argl]
  packageSig := [packageCategory,form,:nils]
  $categoryPredicateList := substitute(nameForDollar,'$,$categoryPredicateList)
  substitute(nameForDollar,'$,
      ['DEF,[packageName,:packageArgl],packageSig,[nil,:nils],def])
 
compDefineCategory2(form,signature,specialCases,body,m,e,
  $prefix,$formalArgList) ==
    --1. bind global variables
    $insideCategoryIfTrue: local:= true
    $definition: local := form   --used by DomainSubstitutionFunction
    $form: local := nil
    $op: local := nil
    $extraParms: local := nil
    -- Remember the body for checking the current instantiation.
    $currentCategoryBody : local := body
         --Set in DomainSubstitutionFunction, used further down
    -- 1.1  augment e to add declaration $: <form>
    [$op,:argl] := $definition
    e:= addBinding("$",[['mode,:$definition]],e)
 
    -- 2. obtain signature
    signature':=
      [first signature,
        :[getArgumentModeOrMoan(a,$definition,e) for a in argl]]
    e:= giveFormalParametersValues(argl,e)
 
    -- 3. replace arguments by $1,..., substitute into body,
    --    and introduce declarations into environment
    sargl:= TAKE(# argl, $TriangleVariableList)
    $functorForm:= $form:= [$op,:sargl]
    $formalArgList:= [:sargl,:$formalArgList]
    aList := pairList(argl,sargl)
    formalBody:= SUBLIS(aList,body)
    signature' := SUBLIS(aList,signature')
    --Begin lines for category default definitions
    $functionStats: local:= [0,0]
    $functorStats: local:= [0,0]
    $getDomainCode: local := nil
    $addForm: local:= nil
    for x in sargl for t in rest signature' repeat
      [.,.,e]:= compMakeDeclaration(x,t,e)
 
    -- 4. compile body in environment of %type declarations for arguments
    op':= $op
    -- following line causes cats with no with or Join to be fresh copies
    if opOf(formalBody)~='Join and opOf(formalBody)~='mkCategory then
           formalBody := ['Join, formalBody]
    body:= optFunctorBody (compOrCroak(formalBody,signature'.target,e)).expr
    if $extraParms then
      formals:=actuals:=nil
      for u in $extraParms repeat
        formals:=[first u,:formals]
        actuals:=[MKQ rest u,:actuals]
      body := ['sublisV,['PAIR,['QUOTE,formals],['LIST,:actuals]],body]
    if argl then body:=  -- always subst for args after extraparms
        ['sublisV,['PAIR,['QUOTE,sargl],['LIST,:
          [['devaluate,u] for u in sargl]]],body]
    body:=
      ["%Bind",[[g:= GENSYM(),body]],
         ["setShellEntry",g,0,mkConstructor $form],g]
    fun:= compile [op',["LAM",sargl,body]]
 
    -- 5. give operator a 'modemap property
    pairlis := pairList(argl,$FormalMapVariableList)
    parSignature:= SUBLIS(pairlis,signature')
    parForm:= SUBLIS(pairlis,form)
    -- If we are only interested in the defaults, there is no point
    -- in writing out compiler info and load-time stuff for 
    -- the category which is assumed to have already been translated.
    if not $compileDefaultsOnly then
      lisplibWrite('"compilerInfo",
        removeZeroOne ['SETQ,'$CategoryFrame,
         ['put,['QUOTE,op'],'
          (QUOTE isCategory),true,['addModemap,MKQ op',MKQ parForm,
            MKQ parSignature,true,MKQ fun,'$CategoryFrame]]],$libFile)
      --Equivalent to the following two lines, we hope
      if null sargl then
        evalAndRwriteLispForm('NILADIC,
              ['MAKEPROP,['QUOTE,op'],'(QUOTE NILADIC),true])
 
    -- 6. put modemaps into InteractiveModemapFrame
    $domainShell := eval [op',:MAPCAR('MKQ,sargl)]
    $lisplibCategory:= formalBody
    if $LISPLIB then
      $lisplibForm:= form
      $lisplibKind:= 'category
      modemap:= [[parForm,:parSignature],[true,op']]
      $lisplibModemap:= modemap
      $lisplibParents  :=         
        getParentsFor($op,$FormalMapVariableList,$lisplibCategory)
      $lisplibAncestors := computeAncestorsOf($form,nil)
      $lisplibAbbreviation := constructor? $op
      form':=[op',:sargl]
      augLisplibModemapsFromCategory(form',formalBody,signature')
    [fun,$Category,e]

mkConstructor: %Form -> %Form
mkConstructor form ==
  atom form => ['devaluate,form]
  null rest form => ['QUOTE,[first form]]
  ['LIST,MKQ first form,:[mkConstructor x for x in rest form]]
 
compDefineCategory(df,m,e,prefix,fal) ==
  $domainShell: local := nil -- holds the category of the object being compiled
  $lisplibCategory: local := nil
  -- since we have so many ways to say state the kind of a constructor,
  -- make sure we do have some minimal internal coherence.
  ctor := opOf second df
  kind := getConstructorKindFromDB ctor
  kind ~= "category" => throwKeyedMsg("S2IC0016",[ctor,"category",kind])
  $insideFunctorIfTrue or not $LISPLIB or $compileDefaultsOnly =>
    compDefineCategory1(df,m,e,prefix,fal)
  compDefineLisplib(df,m,e,prefix,fal,'compDefineCategory1)


%CatObjRes                   -- result of compiling a category
  <=> cons(%Shell,cons(%Mode,cons(%Env,null)))
 
compMakeCategoryObject: (%Form,%Env) -> %Maybe %CatObjRes
compMakeCategoryObject(c,$e) ==
  not isCategoryForm(c,$e) => nil
  u:= mkEvalableCategoryForm c => [eval u,$Category,$e]
  nil

predicatesFromAttributes: %List -> %List
predicatesFromAttributes attrList ==
  REMDUP [second x for x in attrList]

 
compDefineFunctor(df,m,e,prefix,fal) ==
  $domainShell: local := nil -- holds the category of the object being compiled
  $profileCompiler: local := true
  $profileAlist:    local := nil
  $mutableDomain: fluid := false
  $compileExportsOnly or not $LISPLIB => 
    compDefineFunctor1(df,m,e,prefix,fal)
  compDefineLisplib(df,m,e,prefix,fal,'compDefineFunctor1)
 
compDefineFunctor1(df is ['DEF,form,signature,nils,body],
  m,$e,$prefix,$formalArgList) ==
    --  1. bind global variables
    $addForm: local := nil
    $subdomain: local := false
    $functionStats: local:= [0,0]
    $functorStats: local:= [0,0]
    $form: local := nil
    $op: local := nil
    $signature: local := nil
    $functorTarget: local := nil
    $Representation: local := nil
         --Set in doIt, accessed in the compiler - compNoStacking
    $functorForm: local := nil
    $functorLocalParameters: local := nil
    $CheckVectorList: local := nil
    $getDomainCode: local := nil -- code for getting views
    $insideFunctorIfTrue: local:= true
    $setelt: local := "setShellEntry"
    $genSDVar: local:= 0
    originale:= $e
    [$op,:argl]:= form
    $formalArgList:= [:argl,:$formalArgList]
    $pairlis: local := pairList(argl,$FormalMapVariableList)
    $mutableDomain: local :=
      -- all defaulting packages should have caching turned off
       isCategoryPackageName $op or MEMQ($op,$mutableDomains)
                                    --true if domain has mutable state
    signature':=
      [first signature,:[getArgumentModeOrMoan(a,form,$e) for a in argl]]
    $functorForm := $form := [$op,:argl]
    if null signature'.target then signature':=
      modemap2Signature getModemap($form,$e)
    $functorTarget := target := signature'.target
    $functorKind: local :=
      $functorTarget is ["CATEGORY",key,:.] => key
      "domain"
    $e:= giveFormalParametersValues(argl,$e)
    [ds,.,$e]:= compMakeCategoryObject(target,$e) or return
       stackAndThrow('"   cannot produce category object: %1pb",[target])
    $compileExportsOnly => compDefineExports(form, ds.1, signature',$e)
    $domainShell: local := COPY_-SEQ ds
    attributeList := ds.2 --see below under "loadTimeAlist"
    $condAlist: local := nil
    $uncondAlist: local := nil
    $NRTslot1PredicateList: local := predicatesFromAttributes attributeList
    $NRTattributeAlist: local := NRTgenInitialAttributeAlist attributeList
    $NRTslot1Info: local := nil  --set in NRTmakeSlot1Info
       --this is used below to set $lisplibSlot1 global
    $NRTaddForm: local := nil   -- see compAdd
    $NRTdeltaList: local := nil --list of misc. elts used in compiled fncts
    $NRTdeltaListComp: local := nil --list of compiled forms for $NRTdeltaList
    $NRTdeltaLength: local := 0 -- =length of block of extra entries in vector
    $template: local:= nil --stored in the lisplib
    $functionLocations: local := nil --locations of defined functions in source
    -- generate slots for arguments first, then for $NRTaddForm in compAdd
    for x in argl repeat NRTgetLocalIndex x
    [.,.,$e]:= compMakeDeclaration("$",target,$e)
    if not $insideCategoryPackageIfTrue  then
      $e:= augModemapsFromCategory('_$,'_$,'_$,target,$e)
    $signature:= signature'
    parSignature:= SUBLIS($pairlis,signature')
    parForm:= SUBLIS($pairlis,form)
 
    --  (3.1) now make a list of the functor's local parameters; for
    --  domain D in argl,check its signature: if domain, its type is Join(A1,..,An);
    --  in this case, D is replaced by D1,..,Dn (gensyms) which are set
    --  to the A1,..,An view of D
    makeFunctorArgumentParameters(argl,rest signature',first signature')
    $functorLocalParameters := argl

    --  4. compile body in environment of %type declarations for arguments
    op':= $op
    rettype:= signature'.target
    -- If this functor is defined as instantiation of a functor
    -- that is a subdomain of `D', then make this functor also a subdomain
    -- of that super domain `D'.
    if body is ["add",[rhsCtor,:rhsArgs],["CAPSULE"]] 
        and constructor? rhsCtor 
         and (u := getSuperDomainFromDB rhsCtor) then 
           u := sublisFormal(rhsArgs,u,$AtVariables)
           emitSubdomainInfo($form,first u, second u)
    T:= compFunctorBody(body,rettype,$e,parForm)
    -- If only compiling certain items, then ignore the body shell.
    $compileOnlyCertainItems =>
       reportOnFunctorCompilation()
       [nil, ['Mapping, :signature'], originale]
 
    body':= T.expr
    lamOrSlam:= if $mutableDomain then 'LAM else 'SPADSLAM
    fun:= compile SUBLIS($pairlis, [op',[lamOrSlam,argl,body']])
    --The above statement stops substitutions gettting in one another's way
    operationAlist := SUBLIS($pairlis,$lisplibOperationAlist)
    if $LISPLIB then
      augmentLisplibModemapsFromFunctor(parForm,operationAlist,parSignature)
    reportOnFunctorCompilation()
 
    --  5. give operator a 'modemap property
    if $LISPLIB then
      modemap:= [[parForm,:parSignature],[true,op']]
      $lisplibModemap:= modemap
      $lisplibCategory := modemap.mmTarget
      $lisplibParents  :=         
        getParentsFor($op,$FormalMapVariableList,$lisplibCategory)
      $lisplibAncestors := computeAncestorsOf($form,nil)
      $lisplibAbbreviation := constructor? $op
    $insideFunctorIfTrue:= false
    if $LISPLIB then
      $lisplibKind:=
        $functorTarget is ["CATEGORY",key,:.] and key~="domain" => 'package
        'domain
      $lisplibForm:= form
      if null $bootStrapMode then
        $NRTslot1Info := NRTmakeSlot1Info()
        $isOpPackageName: local := isCategoryPackageName $op
        if $isOpPackageName then lisplibWrite('"slot1DataBase",
          ['updateSlot1DataBase,MKQ $NRTslot1Info],$libFile)
        $lisplibFunctionLocations := SUBLIS($pairlis,$functionLocations)
        $lisplibCategoriesExtended := SUBLIS($pairlis,$lisplibCategoriesExtended)
        -- see NRTsetVector4 for initial setting of $lisplibCategoriesExtended
        libFn := getConstructorAbbreviationFromDB op'
        $lookupFunction: local :=
            NRTgetLookupFunction($functorForm,CADAR $lisplibModemap,$NRTaddForm)
            --either lookupComplete (for forgetful guys) or lookupIncomplete
        $byteAddress :local := 0
        $byteVec :local := nil
        $NRTslot1PredicateList :=
          [simpBool x for x in $NRTslot1PredicateList]
        rwriteLispForm('loadTimeStuff,
          ['MAKEPROP,MKQ $op,''infovec,getInfovecCode()])
      $lisplibSlot1 := $NRTslot1Info
      $lisplibOperationAlist:= operationAlist
    lisplibWrite('"compilerInfo",
       removeZeroOne ['SETQ,'$CategoryFrame,
        ['put,['QUOTE,op'],'
         (QUOTE isFunctor),
          ['QUOTE,operationAlist],['addModemap,['QUOTE,op'],['
           QUOTE,parForm],['QUOTE,parSignature],true,['QUOTE,op'],
            ['put,['QUOTE,op' ],'(QUOTE mode),
             ['QUOTE,['Mapping,:parSignature]],'$CategoryFrame]]]],$libFile)
    if null argl then
      evalAndRwriteLispForm('NILADIC,
            ['MAKEPROP, ['QUOTE,op'], ['QUOTE,'NILADIC], true])
    [fun,['Mapping,:signature'],originale]

++ Subroutine of compDefineFunctor1.  Called to generate backend code
++ for a functor definition. 
compFunctorBody(body,m,e,parForm) ==
  $bootStrapMode = true =>
    [bootStrapError($functorForm, _/EDITFILE),m,e]
  clearCapsuleDirectory()        -- start collecting capsule functions.
  T:= compOrCroak(body,m,e)
  $capsuleFunctionStack := nreverse $capsuleFunctionStack
  -- ??? Don't resolve default definitions, yet.
  if $insideCategoryPackageIfTrue then
    backendCompile $capsuleFunctionStack
  else 
    backendCompile foldExportedFunctionReferences $capsuleFunctionStack
  clearCapsuleDirectory()        -- release storage.
  body is [op,:.] and op in '(add CAPSULE) => T
  $NRTaddForm :=
    body is ["SubDomain",domainForm,predicate] => domainForm
    body
  T
 
reportOnFunctorCompilation() ==
  displayMissingFunctions()
  if $semanticErrorStack then sayBrightly '" "
  displaySemanticErrors()
  if $warningStack then sayBrightly '" "
  displayWarnings()
  $functorStats:= addStats($functorStats,$functionStats)
  [byteCount,elapsedSeconds] := $functorStats
  sayBrightly ['%l,:bright '"  Cumulative Statistics for Constructor",
    $op]
  timeString := normalizeStatAndStringify elapsedSeconds
  sayBrightly ['"      Time:",:bright timeString,'"seconds"]
  sayBrightly '" "
  'done
 
displayMissingFunctions() ==
  null $CheckVectorList => nil
  loc := nil              -- list of local operation signatures
  exp := nil              -- list of exported operation signatures
  for [[op,sig,:.],:pred] in $CheckVectorList  | null pred repeat
    not member(op,$formalArgList) and getmode(op,$e) is ['Mapping,:.] =>
      loc := [[op,sig],:loc]
    exp := [[op,sig],:exp]
  if loc then
    sayBrightly ['%l,:bright '"  Missing Local Functions:"]
    for [op,sig] in loc for i in 1.. repeat
      sayBrightly ['"      [",i,'"]",:bright op,
        ": ",:formatUnabbreviatedSig sig]
  if exp then
    sayBrightly ['%l,:bright '"  Missing Exported Functions:"]
    for [op,sig] in exp for i in 1.. repeat
      sayBrightly ['"      [",i,'"]",:bright op,
        ": ",:formatUnabbreviatedSig sig]
 
--% domain view code
 
makeFunctorArgumentParameters(argl,sigl,target) ==
  $forceAdd: local:= true
  $ConditionalOperators: local := nil
  ("append"/[fn(a,augmentSig(s,findExtras(a,target)))
              for a in argl for s in sigl]) where
    findExtras(a,target) ==
      --  see if conditional information implies anything else
      --  in the signature of a
      target is ['Join,:l] => "union"/[findExtras(a,x) for x in l]
      target is ['CATEGORY,.,:l] => "union"/[findExtras1(a,x) for x in l] where
        findExtras1(a,x) ==
          x is ['AND,:l] => "union"/[findExtras1(a,y) for y in l]
          x is ['OR,:l] => "union"/[findExtras1(a,y) for y in l]
          x is ['IF,c,p,q] =>
            union(findExtrasP(a,c),
                  union(findExtras1(a,p),findExtras1(a,q))) where
              findExtrasP(a,x) ==
                x is ['AND,:l] => "union"/[findExtrasP(a,y) for y in l]
                x is ['OR,:l] => "union"/[findExtrasP(a,y) for y in l]
                x is ["has",=a,y] and y is ['SIGNATURE,:.] => [y]
                nil
        nil
    augmentSig(s,ss) ==
       -- if we find something extra, add it to the signature
      null ss => s
      for u in ss repeat
        $ConditionalOperators:=[rest u,:$ConditionalOperators]
      s is ['Join,:sl] =>
        u:=ASSQ('CATEGORY,ss) =>
          MSUBST([:u,:ss],u,s)
        ['Join,:sl,['CATEGORY,'package,:ss]]
      ['Join,s,['CATEGORY,'package,:ss]]
    fn(a,s) ==
      isCategoryForm(s,$CategoryFrame) =>
        s is ["Join",:catlist] => genDomainViewList(a,rest s)
        [genDomainView(a,a,s,"getDomainView")]
      [a]
 
genDomainOps(viewName,dom,cat) ==
  oplist:= getOperationAlist(dom,dom,cat)
  siglist:= [sig for [sig,:.] in oplist]
  oplist:= substNames(dom,viewName,dom,oplist)
  cd:=
    ["%LET",viewName,['mkOpVec,dom,['LIST,:
      [['LIST,MKQ op,['LIST,:[mkTypeForm mode for mode in sig]]]
        for [op,sig] in siglist]]]]
  $getDomainCode:= [cd,:$getDomainCode]
  for [opsig,cond,:.] in oplist for i in 0.. repeat
    if member(opsig,$ConditionalOperators) then cond:=nil
    [op,sig]:=opsig
    $e:= addModemap(op,dom,sig,cond,['ELT,viewName,i],$e)
  viewName
 
genDomainView(viewName,originalName,c,viewSelector) ==
  c is ['CATEGORY,.,:l] => genDomainOps(viewName,originalName,c)
  code:=
    c is ['SubsetCategory,c',.] => c'
    c
  $e:= augModemapsFromCategory(originalName,viewName,nil,c,$e)
  cd:= ["%LET",viewName,[viewSelector,originalName,mkTypeForm code]]
  if null member(cd,$getDomainCode) then
          $getDomainCode:= [cd,:$getDomainCode]
  viewName

genDomainViewList: (%Symbol,%List) -> %List 
genDomainViewList(id,catlist) ==
  [genDomainView(id,id,cat,"getDomainView") 
     for cat in catlist | isCategoryForm(cat,$EmptyEnvironment)]
 
mkOpVec(dom,siglist) ==
  dom:= getPrincipalView dom
  substargs:= [['$,:dom.0],:pairList($FormalMapVariableList,rest dom.0)]
  oplist:= getOperationAlistFromLisplib opOf dom.0
  --new form is (<op> <signature> <slotNumber> <condition> <kind>)
  ops:= MAKE_-VEC (#siglist)
  for (opSig:= [op,sig]) in siglist for i in 0.. repeat
    u:= ASSQ(op,oplist)
    assoc(sig,u) is [.,n,.,'ELT] => ops.i := dom.n
    noplist:= SUBLIS(substargs,u)
  -- following variation on assoc needed for GENSYMS in Mutable domains
    AssocBarGensym(substitute(dom.0,'$,sig),noplist) is [.,n,.,'ELT] =>
                   ops.i := dom.n
    ops.i := [function Undef,[dom.0,i],:opSig]
  ops
 

++ form is lhs (f a1 ... an) of definition; body is rhs;
++ signature is (t0 t1 ... tn) where t0= target type, ti=type of ai, i > 0;
++ specialCases is (NIL l1 ... ln) where li is list of special cases
++ which can be given for each ti
++ removes declarative and assignment information from form and
++ signature, placing it in list L, replacing form by ("where",form',:L),
++ signature by a list of NILs (signifying declarations are in e)
compDefWhereClause(['DEF,form,signature,specialCases,body],m,e) ==
  $sigAlist: local := nil
  $predAlist: local := nil
  -- 1. create sigList= list of all signatures which have embedded
  --    declarations moved into global variable $sigAlist
  sigList:=
    [transformType fetchType(a,x,e,form) for a in rest form for x in rest signature]
       where
        fetchType(a,x,e,form) ==
          x => x
          getmode(a,e) or userError concat(
            '"There is no mode for argument",a,'"of function",first form)
        transformType x ==
          atom x => x
          x is [":",R,Rtype] =>
            ($sigAlist:= [[R,:transformType Rtype],:$sigAlist]; x)
          x is ['Record,:.] => x --RDJ 8/83
          [first x,:[transformType y for y in rest x]]
 
  -- 2. replace each argument of the form (|| x p) by x, recording
  --    the given predicate in global variable $predAlist
  argList:=
    [removeSuchthat a for a in rest form] where
      removeSuchthat x ==
        x is ["|",y,p] => ($predAlist:= [[y,:p],:$predAlist]; y)
        x
 
  -- 3. obtain a list of parameter identifiers (x1 .. xn) ordered so that
  --       the type of xi is independent of xj if i < j
  varList:=
    orderByDependency(ASSOCLEFT argDepAlist,ASSOCRIGHT argDepAlist) where
      argDepAlist:=
        [[x,:dependencies] for [x,:y] in argSigAlist] where
          dependencies() ==
            union(listOfIdentifiersIn y,
              delete(x,listOfIdentifiersIn LASSOC(x,$predAlist)))
          argSigAlist:= [:$sigAlist,:pairList(argList,sigList)]
 
  -- 4. construct a WhereList which declares and/or defines the xi's in
  --    the order constructed in step 3
  (whereList:= [addSuchthat(x,[":",x,LASSOC(x,argSigAlist)]) for x in varList])
     where addSuchthat(x,y) == (p:= LASSOC(x,$predAlist) => ["|",y,p]; y)
 
  -- 5. compile new ('DEF,("where",form',:WhereList),:.) where
  --    all argument parameters of form' are bound/declared in WhereList
  comp(form',m,e) where
    form':=
      ["where",defform,:whereList] where
        defform:=
          ['DEF,form'',signature',specialCases,body] where
            form'':= [first form,:argList]
            signature':= [first signature,:[nil for x in rest signature]]
 
orderByDependency(vl,dl) ==
  -- vl is list of variables, dl is list of dependency-lists
  selfDependents:= [v for v in vl for d in dl | MEMQ(v,d)]
  for v in vl for d in dl | MEMQ(v,d) repeat
    (SAY(v," depends on itself"); fatalError:= true)
  fatalError => userError '"Parameter specification error"
  until (null vl) repeat
    newl:=
      [v for v in vl for d in dl | null intersection(d,vl)] or return nil
    orderedVarList:= [:newl,:orderedVarList]
    vl':= setDifference(vl,newl)
    dl':= [setDifference(d,newl) for x in vl for d in dl | member(x,vl')]
    vl:= vl'
    dl:= dl'
  REMDUP nreverse orderedVarList --ordered so ith is indep. of jth if i < j
 
compDefineCapsuleFunction(df is ['DEF,form,signature,specialCases,body],
  m,$e,$prefix,$formalArgList) ==
    [lineNumber,:specialCases] := specialCases
    e := $e
    --1. bind global variables
    $form: local := nil
    $op: local := nil
    $functionStats: local:= [0,0]
    $argumentConditionList: local := nil
    $finalEnv: local := nil
             --used by ReplaceExitEtc to get a common environment
    $initCapsuleErrorCount: local:= #$semanticErrorStack
    $insideCapsuleFunctionIfTrue: local:= true
    $CapsuleModemapFrame: local:= e
    $CapsuleDomainsInScope: local:= get("$DomainsInScope","special",e)
    $insideExpressionIfTrue: local:= true
    $returnMode:= m
    -- Change "^" to "**" in definitions.  All other places have 
    -- been changed before we get here.
    if first form = "^" then 
      sayBrightly ['"Replacing", :bright '"^", '"with",:bright '"**"]
      rplac(first form,"**")
    [$op,:argl]:= form
    $form:= [$op,:argl]
    argl:= stripOffArgumentConditions argl
    $formalArgList:= [:argl,:$formalArgList]
 
    --let target and local signatures help determine modes of arguments
    argModeList:=
      identSig:= hasSigInTargetCategory(argl,form,first signature,e) =>
        (e:= checkAndDeclare(argl,form,identSig,e); rest identSig)
      [getArgumentModeOrMoan(a,form,e) for a in argl]
    argModeList:= stripOffSubdomainConditions(argModeList,argl)
    signature':= [first signature,:argModeList]
    if null identSig then  --make $op a local function
      $e := put($op,'mode,['Mapping,:signature'],$e)
 
    --obtain target type if not given
    if null first signature' then signature':=
      identSig => identSig
      getSignature($op,rest signature',e) or return nil
    e:= giveFormalParametersValues(argl,e)
 
    $signatureOfForm:= signature' --this global is bound in compCapsuleItems
    $functionLocations := [[[$op,$signatureOfForm],:lineNumber],
      :$functionLocations]
    e:= addDomain(first signature',e)
    e:= compArgumentConditions e
 
    if $profileCompiler then
      for x in argl for t in rest signature' repeat 
        profileRecord('arguments,x,t)

    --4. introduce needed domains into extendedEnv
    for domain in signature' repeat e:= addDomain(domain,e)
 
    --6. compile body in environment with extended environment
    rettype:= resolve(signature'.target,$returnMode)
 
    localOrExported :=
      null member($op,$formalArgList) and
        getmode($op,e) is ['Mapping,:.] => 'local
      'exported
 
    --6a skip if compiling only certain items but not this one
    -- could be moved closer to the top
    formattedSig := formatUnabbreviated ['Mapping,:signature']
    $compileOnlyCertainItems and _
      not member($op, $compileOnlyCertainItems) =>
        sayBrightly ['"   skipping ", localOrExported,:bright $op]
        [nil,['Mapping,:signature'],$e]
    sayBrightly ['"   compiling ",localOrExported,
      :bright $op,'": ",:formattedSig]
 
    noteCapsuleFunctionDefinition($op,signature', makePredicate $predl)
    T := CATCH('compCapsuleBody, compOrCroak(body,rettype,e))
	 or ["",rettype,e]
    NRTassignCapsuleFunctionSlot($op,signature')
    if $newCompCompare=true then
       SAY '"The old compiler generates:"
       prTriple T
    --  A THROW to the above CATCH occurs if too many semantic errors occur
    --  see stackSemanticError
    catchTag:= MKQ GENSYM()
    fun:=
      body':= replaceExitEtc(T.expr,catchTag,"TAGGEDreturn",$returnMode)
      body':= addArgumentConditions(body',$op)
      finalBody:= ["CATCH",catchTag,body']
      compile [$op,["LAM",[:argl,'_$],finalBody]]
    $functorStats:= addStats($functorStats,$functionStats)
 
    --7. give operator a 'value property
    val:= [fun,signature',e]
    [fun,['Mapping,:signature'],$e]
 
getSignatureFromMode(form,e) ==
  getmode(opOf form,e) is ['Mapping,:signature] =>
    #form~=#signature => stackAndThrow ["Wrong number of arguments: ",form]
    EQSUBSTLIST(rest form,take(#rest form,$FormalMapVariableList),signature)

candidateSignatures(op,nmodes,slot1) ==
  [sig for [[=op,sig,:.],:.] in slot1 | #sig = nmodes]

++ We are compiling a capsule function definition with head given by `form'.
++ Determine whether the function with possibly partial signature `opsig'
++ is exported.  Return the complete signature if yes; otherwise
++ return nil, with diagnostic in ambiguity case.
hasSigInTargetCategory(argl,form,opsig,e) ==
  sigs := candidateSignatures($op,#form,$domainShell.1)
  cc := checkCallingConvention(sigs,#argl)
  mList:= [(cc.i > 0 => quasiquote x; getArgumentMode(x,e))
            for x in argl for i in 0..]
    --each element is a declared mode for the variable or nil if none exists
  potentialSigList:=
    REMDUP
      [sig for sig in sigs |
          fn(sig,opsig,mList)] where
            fn(sig,opsig,mList) ==
              (null opsig or opsig=first sig) and
                (and/[compareMode2Arg(x,m) for x in mList for m in rest sig])
  c:= #potentialSigList
  1=c => first potentialSigList
    --accept only those signatures op right length which match declared modes
  0=c => (#(sig:= getSignatureFromMode(form,e))=#form => sig; nil)
  1<c =>
    ambiguousSignatureError($op,potentialSigList)
    first potentialSigList
  nil --this branch will force all arguments to be declared
 
compareMode2Arg(x,m) == null x or modeEqual(x,m)
 
getArgumentModeOrMoan: (%Form, %Form, %Env) -> %Mode
getArgumentModeOrMoan(x,form,e) ==
  getArgumentMode(x,e) or
    stackSemanticError(["argument ",x," of ",form," is not declared"],nil)

getArgumentMode: (%Form,%Env) -> %Mode 
getArgumentMode(x,e) ==
  string? x => x
  m:= get(x,'mode,e) => m
 
checkAndDeclare(argl,form,sig,e) ==
-- arguments with declared types must agree with those in sig;
-- those that don't get declarations put into e
  for a in argl for m in rest sig repeat
    isQuasiquote m => nil	  -- we just built m from a.
    m1:= getArgumentMode(a,e) =>
      not modeEqual(m1,m) =>
        stack:= ["   ",:bright a,'"must have type ",m,
          '" not ",m1,'%l,:stack]
    e:= put(a,'mode,m,e)
  if stack then
    sayBrightly ['"   Parameters of ",:bright first form,
      '" are of wrong type:",'%l,:stack]
  e
 
getSignature(op,argModeList,$e) ==
  1=#
    (sigl:=
      REMDUP
        [sig
          for [[dc,:sig],[pred,:.]] in (mmList:= get(op,'modemap,$e)) | dc='_$
            and rest sig=argModeList and knownInfo pred]) => first sigl
  null sigl =>
    (u:= getmode(op,$e)) is ['Mapping,:sig] => sig
    SAY '"************* USER ERROR **********"
    SAY("available signatures for ",op,": ")
    if null mmList
       then SAY "    NONE"
       else for [[dc,:sig],:.] in mmList repeat printSignature("     ",op,sig)
    printSignature("NEED ",op,["?",:argModeList])
    nil
  1=#sigl => first sigl
  stackSemanticError(["duplicate signatures for ",op,": ",argModeList],nil)
 
--% ARGUMENT CONDITION CODE
 
stripOffArgumentConditions argl ==
  [f for x in argl for i in 1..] where
    f() ==
      x is ["|",arg,condition] =>
        condition:= substitute('_#1,arg,condition)
        -- in case conditions are given in terms of argument names, replace
        $argumentConditionList:= [[i,arg,condition],:$argumentConditionList]
        arg
      x
 
stripOffSubdomainConditions(margl,argl) ==
  [f for x in margl for arg in argl for i in 1..] where
    f() ==
      x is ['SubDomain,marg,condition] =>
        pair:= assoc(i,$argumentConditionList) =>
          (RPLAC(second pair,MKPF([condition,second pair],'AND)); marg)
        $argumentConditionList:= [[i,arg,condition],:$argumentConditionList]
        marg
      x
 
compArgumentConditions: %Env -> %Env
compArgumentConditions e ==
  $argumentConditionList:=
    [f for [n,a,x] in $argumentConditionList] where
      f() ==
        y:= substitute(a,'_#1,x)
        T := [.,.,e]:= compOrCroak(y,$Boolean,e)
        [n,x,T.expr]
  e

addArgumentConditions($body,$functionName) ==
  $argumentConditionList =>
               --$body is only used in this function
    fn $argumentConditionList where
      fn clist ==
        clist is [[n,untypedCondition,typedCondition],:.] =>
          ['COND,[typedCondition,fn rest clist],
            [$true,["argumentDataError",n,
              MKQ untypedCondition,MKQ $functionName]]]
        null clist => $body
        systemErrorHere ["addArgumentConditions",clist]
  $body
 
putInLocalDomainReferences (def := [opName,[lam,varl,body]]) ==
  $elt: local := "getShellEntry"
  NRTputInTail CDDADR def
  def
 
 
$savableItems := nil
 
compile u ==
  [op,lamExpr] := u
  if $suffix then
    $suffix:= $suffix+1
    op':=
      opexport:=nil
      opmodes:=
        [sel
          for [[DC,:sig],[.,sel]] in get(op,'modemap,$e) |
            DC='_$ and (opexport:=true) and
             (and/[modeEqual(x,y) for x in sig for y in $signatureOfForm])]
      isLocalFunction op =>
        if opexport then userError ['%b,op,'%d,'" is local and exported"]
        INTERN STRCONC(encodeItem $prefix,'";",encodeItem op) 
      encodeFunctionName(op,$functorForm,$signatureOfForm,";",$suffix)
     where
       isLocalFunction op ==
         null member(op,$formalArgList) and
           getmode(op,$e) is ['Mapping,:.]
    u:= [op',lamExpr]
  -- If just updating certain functions, check for previous existence.
  -- Deduce old sequence number and use it (items have been skipped).
  if $LISPLIB and $compileOnlyCertainItems then
    parts := splitEncodedFunctionName(u.0, ";")
  --  Next line JHD/SMWATT 7/17/86 to deal with inner functions
    parts='inner => $savableItems:=[u.0,:$savableItems]
    unew  := nil
    for [s,t] in $splitUpItemsAlreadyThere repeat
       if parts.0=s.0 and parts.1=s.1 and parts.2=s.2 then unew := t
    null unew =>
      sayBrightly ['"   Error: Item did not previously exist"]
      sayBrightly ['"   Item not saved: ", :bright u.0]
      sayBrightly ['"   What's there is: ", $lisplibItemsAlreadyThere]
      nil
    sayBrightly ['"   Renaming ", u.0, '" as ", unew]
    u := [unew, :rest u]
    $savableItems := [unew, :$saveableItems] -- tested by embedded RWRITE
  optimizedBody:= optimizeFunctionDef u
  stuffToCompile:=
    if null $insideCapsuleFunctionIfTrue
       then optimizedBody
       else putInLocalDomainReferences optimizedBody
  $doNotCompileJustPrint=true => (PRETTYPRINT stuffToCompile; op')
  $macroIfTrue => constructMacro stuffToCompile

  -- Let the backend know about this function's type
  if $insideCapsuleFunctionIfTrue and $optProclaim then
    proclaimCapsuleFunction(op',$signatureOfForm)

  result:= spadCompileOrSetq stuffToCompile
  functionStats:=[0,elapsedTime()]
  $functionStats:= addStats($functionStats,functionStats)
  printStats functionStats
  result

++ Subroutine of compile.  Called to generate backend code for
++ items defined directly or indirectly at capsule level.   This is
++ also used to compile functors.
spadCompileOrSetq (form is [nam,[lam,vl,body]]) ==
        --bizarre hack to take account of the existence of "known" functions
        --good for performance (LISPLLIB size, BPI size, NILSEC)
  CONTAINED("",body) => sayBrightly ['"  ",:bright nam,'" not compiled"]

  -- flag parameters needs to be made atomic, otherwise Lisp is confused.
  -- We try our best to preserve
  -- Note that we don't need substitution in the body because flag
  -- parameters are never used in the body.
  vl := [ renameParameter for v in vl] where
    renameParameter() ==
      NUMBERP v or IDENTP v or string? v => v
      GENSYM '"flag"
  clearReplacement nam   -- Make sure we have fresh info
  if $optReplaceSimpleFunctions then
    body := replaceSimpleFunctions body

  if nam' := forwardingCall?(vl,body) then
      registerFunctionReplacement(nam,nam')
      sayBrightly ['"     ",:bright nam,'"is replaced by",:bright nam']
  else if macform := expandableDefinition?(vl,body) then
           registerFunctionReplacement(nam,macform)
           sayBrightly ['"     ",:bright nam,'"is replaced by",:bright body]

  form := 
    getFunctionReplacement nam => 
      [nam,[lam,vl,["DECLARE",["IGNORE",last vl]],body]]
    [nam,[lam,vl,body]]

  $insideCapsuleFunctionIfTrue => 
    $optExportedFunctionReference =>
      $capsuleFunctionStack := [form,:$capsuleFunctionStack]
      first form
    first backendCompile LIST form
  compileConstructor form
 
compileConstructor form ==
  u:= compileConstructor1 form
  clearClams()                  --clear all CLAMmed functions
  u
 
compileConstructor1 (form:=[fn,[key,vl,:bodyl]]) ==
-- fn is the name of some category/domain/package constructor;
-- we will cache all of its values on $ConstructorCache with reference
-- counts
  $clamList: local := nil
  lambdaOrSlam :=
    getConstructorKindFromDB fn = "category" => 'SPADSLAM
    $mutableDomain => 'LAMBDA
    $clamList:=
      [[fn,"$ConstructorCache",'domainEqualList,'count],:$clamList]
    'LAMBDA
  compForm:= LIST [fn,[lambdaOrSlam,vl,:bodyl]]
  if getConstructorKindFromDB fn = "category"
      then u:= compAndDefine compForm
      else u:= backendCompile compForm
  clearConstructorCache fn      --clear cache for constructor
  first u
 
constructMacro: %Form -> %Form
constructMacro (form is [nam,[lam,vl,body]]) ==
  not (and/[atom x for x in vl]) =>
    stackSemanticError(["illegal parameters for macro: ",vl],nil)
  ["XLAM",vl':= [x for x in vl | IDENTP x],body]
 
listInitialSegment(u,v) ==
  null u => true
  null v => nil
  first u=first v and listInitialSegment(rest u,rest v)
  --returns true iff u.i=v.i for i in 1..(#u)-1
 
modemap2Signature [[.,:sig],:.] == sig

uncons: %Form -> %Form 
uncons x ==
  atom x => x
  x is ["CONS",a,b] => [a,:uncons b]
 
--% CAPSULE
 
bootStrapError(functorForm,sourceFile) ==
  ['COND, _
    ['$bootStrapMode, _
        ['VECTOR,mkTypeForm functorForm,nil,nil,nil,nil,nil]],
    [''T, ['systemError,['LIST,''%b,MKQ first functorForm,''%d,'"from", _
      ''%b,MKQ namestring sourceFile,''%d,'"needs to be compiled"]]]]

compAdd(['add,$addForm,capsule],m,e) ==
  $bootStrapMode = true =>
    if $addForm is ["%Comma",:.] then code := nil
       else [code,m,e]:= comp($addForm,m,e)
    [['COND, _
       ['$bootStrapMode, _
           code],_
       [''T, ['systemError,['LIST,''%b,MKQ first $functorForm,''%d,'"from", _
         ''%b,MKQ namestring _/EDITFILE,''%d,'"needs to be compiled"]]]],m,e]
  $addFormLhs: local:= $addForm
  if $addForm is ["SubDomain",domainForm,predicate] then
    $NRTaddForm := domainForm
    NRTgetLocalIndex domainForm
    --need to generate slot for add form since all $ go-get
    --  slots will need to access it
    [$addForm,.,e]:= compSubDomain1(domainForm,predicate,m,e)
  else
    $NRTaddForm := $addForm
    [$addForm,.,e]:=
      $addForm is ["%Comma",:.] =>
        $NRTaddForm := ["%Comma",:[NRTgetLocalIndex x for x in rest $addForm]]
        compOrCroak(compTuple2Record $addForm,$EmptyMode,e)
      compOrCroak($addForm,$EmptyMode,e)
  compCapsule(capsule,m,e)
 
compTuple2Record u == ['Record,:[[":",i,x] for i in 1.. for x in rest u]]

compCapsule(['CAPSULE,:itemList],m,e) ==
  $bootStrapMode = true =>
    [bootStrapError($functorForm, _/EDITFILE),m,e]
  $insideExpressionIfTrue: local:= false
  $useRepresentationHack := true
  clearCapsuleFunctionTable()
  e := checkRepresentation($addFormLhs,itemList,e)
  compCapsuleInner(itemList,m,addDomain('_$,e))
 
compSubDomain(["SubDomain",domainForm,predicate],m,e) ==
  $addFormLhs: local:= domainForm
  $addForm: local := nil
  $NRTaddForm := domainForm
  [$addForm,.,e]:= compSubDomain1(domainForm,predicate,m,e)
  compCapsule(['CAPSULE],m,e)
 
compSubDomain1(domainForm,predicate,m,e) ==
  [.,.,e]:=
    compMakeDeclaration("#1",domainForm,addDomain(domainForm,e))
  u:=
    compCompilerPredicate(predicate,e) or
      stackSemanticError(["predicate: ",predicate,
        " cannot be interpreted with #1: ",domainForm],nil)
  pred := lispize u.expr
  -- For now, reject predicates that directly reference domains
  CONTAINED("$",pred) => 
    stackAndThrow('"predicate %1pb is not simple enough",[predicate])
  emitSubdomainInfo($form,domainForm,pred)
  $lisplibSuperDomain := [domainForm,predicate]
  [domainForm,m,e]

compCapsuleInner(itemList,m,e) ==
  e:= addInformation(m,e)
           --puts a new 'special' property of $Information
  data:= ["PROGN",:itemList]
      --RPLACd by compCapsuleItems and Friends
  e:= compCapsuleItems(itemList,nil,e)
  localParList:= $functorLocalParameters
  if $addForm then data:= ['add,$addForm,data]
  code:=
    $insideCategoryIfTrue and not $insideCategoryPackageIfTrue => data
    processFunctor($form,$signature,data,localParList,e)
  [MKPF([:$getDomainCode,code],"PROGN"),m,e]
 
--% PROCESS FUNCTOR CODE
 
processFunctor(form,signature,data,localParList,e) ==
  form is ["CategoryDefaults"] =>
    error "CategoryDefaults is a reserved name"
  buildFunctor(form,signature,data,localParList,e)
 
compCapsuleItems(itemlist,$predl,$e) ==
  $signatureOfForm: local := nil
  $suffix: local:= 0
  for item in itemlist repeat 
    $e:= compSingleCapsuleItem(item,$predl,$e)
  $e
 
compSingleCapsuleItem(item,$predl,$e) ==
  doIt(macroExpandInPlace(item,$e),$predl)
  $e
 

++ subroutine of doIt.  Called to generate runtime noop insn.
mutateToNothing item ==
  RPLACA(item,'PROGN)
  RPLACD(item,NIL)

doIt(item,$predl) ==
  $GENNO: local:= 0
  item is ['SEQ,:l,['exit,1,x]] =>
    RPLACA(item,"PROGN")
    RPLACA(LASTNODE item,x)
    for it1 in rest item repeat $e:= compSingleCapsuleItem(it1,$predl,$e)
        --This will RPLAC as appropriate
  isDomainForm(item,$e) =>
    -- convert naked top level domains to import.
    -- Note: The apparent useless destructing of `item' below is necessary
    -- because it is subject to RPLACA/RPLACD, which would create
    -- a cycle otherwise.
    u:= ["import", [first item,:rest item]]
    stackWarning('"Use: import %1p",[[first item,:rest item]])
    RPLACA(item,first u)
    RPLACD(item,rest u)
    doIt(item,$predl)
  item is ["%LET",lhs,rhs,:.] =>
    compOrCroak(item,$EmptyMode,$e) isnt [code,.,$e] =>
      stackSemanticError(["cannot compile assigned value to",:bright lhs],nil)
    not (code is ["%LET",lhs',rhs',:.] and atom lhs') =>
      code is ["PROGN",:.] =>
         stackSemanticError(["multiple assignment ",item," not allowed"],nil)
      RPLACA(item,first code)
      RPLACD(item,rest code)
    lhs:= lhs'
    if not member(KAR rhs,$NonMentionableDomainNames) and
      not MEMQ(lhs, $functorLocalParameters) then
         $functorLocalParameters:= [:$functorLocalParameters,lhs]
    if code is ["%LET",.,rhs',:.] and isDomainForm(rhs',$e) then
      if lhs="Rep" then
        --$Representation bound by compDefineFunctor, used in compNoStacking
        $Representation := getRepresentation $e
        if $optimizeRep then
          nominateForInlining $Representation
    code is ["%LET",:.] =>
      RPLACA(item,"setShellEntry")
      rhsCode := rhs'
      RPLACD(item,['$,NRTgetLocalIndex lhs,rhsCode])
    RPLACA(item,first code)
    RPLACD(item,rest code)
  item is [":",a,t] => [.,.,$e]:= compOrCroak(item,$EmptyMode,$e)
  item is ["import",:doms] =>
     for dom in doms repeat
       sayBrightly ['"   importing ",:formatUnabbreviated dom]
     [.,.,$e] := compOrCroak(item,$EmptyMode,$e)
     mutateToNothing item
  item is ["%Inline",type] =>
    processInlineRequest(type,$e)
    mutateToNothing item
  item is ["%SignatureImport",:.] =>
    [.,.,$e] := compSignatureImport(item,$EmptyMode,$e)
    mutateToNothing item
  item is ["IF",p,x,y] => doItConditionally(item,$predl)
  item is ["where",b,:l] => compOrCroak(item,$EmptyMode,$e)
  item is ["MDEF",:.] => [.,.,$e]:= compOrCroak(item,$EmptyMode,$e)
  item is ['DEF,[op,:.],:.] =>
    body:= isMacro(item,$e) => $e:= put(op,"macro",body,$e)
    [.,.,$e]:= t:= compOrCroak(item,$EmptyMode,$e)
    RPLACA(item,"CodeDefine")
        --Note that DescendCode, in CodeDefine, is looking for this
    RPLACD(second item,[$signatureOfForm])
      --This is how the signature is updated for buildFunctor to recognise
    functionPart:= ['dispatchFunction,t.expr]
    RPLACA(CDDR item,functionPart)
    RPLACD(CDDR item,nil)
  u:= compOrCroak(item,$EmptyMode,$e) =>
    ([code,.,$e]:= u; RPLACA(item,first code); RPLACD(item,rest code))
  systemErrorHere ["doIt", item]
 
isMacro(x,e) ==
  x is ['DEF,[op,:args],signature,specialCases,body] and
    null get(op,'modemap,e) and null args and null get(op,'mode,e)
      and signature is [nil] => body

++ Compile capsule-level `item' which is a conditional expression.
++ OpenAxiom's take on prepositional logical is a constructive
++ interpretation of logical connectives, in terms of IF-expresions.
++ In particular, a negation is positively interpretated by swapping
++ branches, and- and or-expressions are decomposed into nested
++ IF-expressions.  -- gdr, 2009-06-15.
doItConditionally(item,predl) ==
  item isnt ["IF",p,x,y] => systemErrorHere ["doItConditionally",item]
  p is ["not",p'] =>
    -- swap branches and recurse for positive interpretation.
    rplac(second item,p')
    rplac(third item,y)
    rplac(fourth item,x)
    doItConditionally(item,predl)
  p is ["and",p',p''] =>
    rplac(second item,p')
    rplac(third item,["IF",p'',x,COPY y])
    doItConditionally(item,predl)
  p is ["or",p',p''] =>
    rplac(second item, p')
    rplac(fourth item, ["IF",p'',COPY x,y])
    doItConditionally(item,predl)
  doItIf(item,predl,$e)
    
 
doItIf(item is [.,p,x,y],$predl,$e) ==
  olde:= $e
  [p',.,$e]:= compCompilerPredicate(p,$e) or userError ['"not a Boolean:",p]
  oldFLP:=$functorLocalParameters
  if x~="%noBranch" then
    compSingleCapsuleItem(x,[p,:$predl],getSuccessEnvironment(p,$e))
    x':=localExtras(oldFLP)
  oldFLP:=$functorLocalParameters
  if y~="%noBranch" then
    compSingleCapsuleItem(y,[["not",p],:$predl],getInverseEnvironment(p,olde))
    y':=localExtras(oldFLP)
  RPLACA(item,"COND")
  RPLACD(item,[[p',x,:x'],['(QUOTE T),y,:y']])
 where localExtras(oldFLP) ==
   EQ(oldFLP,$functorLocalParameters) => NIL
   flp1:=$functorLocalParameters
   oldFLP':=oldFLP
   n:=0
   while oldFLP' repeat
     oldFLP':=rest oldFLP'
     flp1:=rest flp1
     n:=n+1
   -- Now we have to add code to compile all the elements
   -- of functorLocalParameters that were added during the
   -- conditional compilation
   nils:=ans:=[]
   for u in flp1 repeat -- is =u form always an atom?
     if atom u or (or/[v is [.,=u,:.] for v in $getDomainCode])
       then
         nils:=[u,:nils]
       else
         gv := GENSYM()
         ans:=[["%LET",gv,u],:ans]
         nils:=[gv,:nils]
     n:=n+1
   $functorLocalParameters:=[:oldFLP,:nreverse nils]
   nreverse ans
 
--% CATEGORY AND DOMAIN FUNCTIONS

compContained: (%Form, %Mode, %Env) -> %Maybe %Triple
compContained(["CONTAINED",a,b],m,e) ==
  [a,ma,e]:= comp(a,$EmptyMode,e) or return nil
  [b,mb,e]:= comp(b,$EmptyMode,e) or return nil
  isCategoryForm(ma,e) and isCategoryForm(mb,e) =>
    (T:= [["CONTAINED",a,b],$Boolean,e]; convert(T,m))
  nil

compJoin(["Join",:argl],m,e) ==
  catList:= [(compForMode(x,$Category,e) or return 'failed).expr for x in argl]
  catList='failed => stackSemanticError(["cannot form Join of: ",argl],nil)
  catList':=
    [extract for x in catList] where
      extract() ==
        isCategoryForm(x,e) =>
          parameters:=
            union("append"/[getParms(y,e) for y in rest x],parameters)
              where getParms(y,e) ==
                atom y =>
                  isDomainForm(y,e) => LIST y
                  nil
                y is ['LENGTH,y'] => [y,y']
                LIST y
          x
        x is ["DomainSubstitutionMacro",pl,body] =>
          (parameters:= union(pl,parameters); body)
        x is ["mkCategory",:.] => x
        atom x and getmode(x,e)=$Category => x
        stackSemanticError(["invalid argument to Join: ",x],nil)
        x
  T:= [wrapDomainSub(parameters,["Join",:catList']),$Category,e]
  convert(T,m)

compForMode: (%Form,%Mode,%Env) -> %Maybe %Triple 
compForMode(x,m,e) ==
  $compForModeIfTrue: local:= true
  comp(x,m,e)

makeCategoryForm(c,e) ==
  not isCategoryForm(c,e) => nil
  [x,m,e]:= compOrCroak(c,$EmptyMode,e)
  [x,e]

mustInstantiate: %Form -> %Thing 
mustInstantiate D ==
  D is [fn,:.] and 
    not (MEMQ(fn,$DummyFunctorNames) or GET(fn,"makeFunctionList"))

wrapDomainSub: (%List, %Form) -> %Form 
wrapDomainSub(parameters,x) ==
   ["DomainSubstitutionMacro",parameters,x]
 
mkExplicitCategoryFunction(domainOrPackage,sigList,atList) ==
  body:=
    ["mkCategory",MKQ domainOrPackage,['LIST,:reverse sigList],
      ['LIST,:reverse atList],MKQ domList,nil] where
        domList() ==
          ("union"/[fn sig for ["QUOTE",[[.,sig,:.],:.]] in sigList]) where
            fn sig == [D for D in sig | mustInstantiate D]
  parameters:=
    REMDUP
      ("append"/
        [[x for x in sig | IDENTP x and x~='_$]
          for ["QUOTE",[[.,sig,:.],:.]] in sigList])
  wrapDomainSub(parameters,body)

DomainSubstitutionFunction(parameters,body) ==
  --see definition of DomainSubstitutionMacro in SPAD LISP
  if parameters then
    (body:= Subst(parameters,body)) where
      Subst(parameters,body) ==
        atom body =>
          MEMQ(body,parameters) => MKQ body
          body
        member(body,parameters) =>
          g:=GENSYM()
          $extraParms:=PUSH([g,:body],$extraParms)
           --Used in SetVector12 to generate a substitution list
           --bound in buildFunctor
           --For categories, bound and used in compDefineCategory
          MKQ g
        first body="QUOTE" => body
        cons? $definition and
            isFunctor first body and
              first body ~= first $definition
          =>  ['QUOTE,optimize body]
        [Subst(parameters,u) for u in body]
  not (body is ["Join",:.]) => body
  atom $definition => body
  null rest $definition => body 
           --should not bother if it will only be called once
  name:= INTERN STRCONC(KAR $definition,";CAT")
  SETANDFILE(name,nil)
  body:= ["COND",[name],['(QUOTE T),['SETQ,name,body]]]
  body


++ Subroutine of compCategoryItem.
++ Compile exported signature `opsig' under predicate `pred' in
++ environment `env'.
compSignature(opsig,pred,env) ==
  [op,:sig] := opsig
  not atom op =>
    for y in op repeat 
      compSignature([y,:sig],pred,env)
  op in '(per rep) =>
    stackSemanticError(['"cannot export signature for", :bright op],nil)
    nil
  noteExport(opsig,pred)
  PUSH(MKQ [opsig,pred],$sigList)
 
compCategoryItem(x,predl,env) ==
  x is nil => nil
  --1. if x is a conditional expression, recurse; otherwise, form the predicate
  x is ["COND",[p,e]] =>
    predl':= [p,:predl]
    e is ["PROGN",:l] =>
      for y in l repeat compCategoryItem(y,predl',env)
    compCategoryItem(e,predl',env)
  x is ["IF",a,b,c] =>
    a is ["not",p] => compCategoryItem(["IF",p,c,b],predl,env)
    a is ["and",p,q] =>
      compCategoryItem(["IF",p,["IF",q,b,c],COPY c],predl,env)
    a is ["or",p,q] =>
      compCategoryItem(["IF",p,b,["IF",q,COPY b,c]],predl,env)
    predl':= [a,:predl]
    if b~="%noBranch" then
      b is ["PROGN",:l] => 
        for y in l repeat compCategoryItem(y,predl',env)
      compCategoryItem(b,predl',env)
    c="%noBranch" => nil
    predl':= [["not",a],:predl]
    c is ["PROGN",:l] => 
      for y in l repeat compCategoryItem(y,predl',env)
    compCategoryItem(c,predl',env)
  pred := (predl => MKPF(predl,"AND"); true)
 
  --2. if attribute, push it and return
  x is ["ATTRIBUTE",y] => 
    -- Attribute 'nil' carries no semantics.
    y = "nil" => nil
    noteExport(y,pred)
    PUSH(MKQ [y,pred],$atList)
 
  --3. it may be a list, with PROGN as the first, and some information as the rest
  x is ["PROGN",:l] => 
    for u in l repeat 
      compCategoryItem(u,predl,env)
 
  -- 4. otherwise, x gives a signature for a
  --    single operator name or a list of names; if a list of names,
  --    recurse
  x is ["SIGNATURE",:opsig] => compSignature(opsig,pred,env)
  systemErrorHere ["compCategoryItem",x]

compCategory: (%Form,%Mode,%Env) -> %Maybe %Triple
compCategory(x,m,e) ==
  clearExportsTable()
  (m:= resolve(m,$Category))=$Category and x is ['CATEGORY,
    domainOrPackage,:l] =>
      $sigList: local := nil
      $atList: local := nil
      for x in l repeat compCategoryItem(x,nil,e)
      rep:= mkExplicitCategoryFunction(domainOrPackage,$sigList,$atList)
    --if inside compDefineCategory, provide for category argument substitution
      [rep,m,e]
  systemErrorHere ["compCategory",x]

--%
