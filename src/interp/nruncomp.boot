-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2015, Gabriel Dos Reis.
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


import simpbool
import profile
import functor
namespace BOOT

++ The "add-chain" index for a functor instance.
$AddChainIndex == 5

++ The base index for encoding items into a functor template 
++ (e.g. domainShell).  This is also the minimum length that a
++ template could possibly have.
++ Note: This is the index right after $AddChainIndex.
++ Note: It is equal to $FirstParamSlot.
$NRTbase == $AddChainIndex + 1

++
$devaluateList := []
$functorLocalParameters := []
$insideCategoryPackageIfTrue := false

++ By default, don't generate info files
$profileCompiler := false

++
$NRTaddForm := nil

++
$NRTderivedTargetIfTrue := false

addDeltaCode db ==
--NOTES: This function is called from buildFunctor to initially
--  fill slots in dbTemplate. The dbTemplate so created is stored in the
--  NRLIB. On load, makeDomainTemplate is called on this dbTemplate to
--  create a template which becomes slot 0 of the infovec for the constructor.
--The template has 6 kinds of entries:
--  (1) formal arguments and local variables, represented by (QUOTE <entry>)
--      this conflicts by (5) but is ok since each is explicitly set by
--      instantiator code;
--  (2) domains, represented by lazy forms, e.g. (Foo 12 17 6)
--  (3) latch slots, represented SPADCALLable forms which goGet an operation
--      from a domain then cache the operation in the same slot
--  (4) functions, represented by identifiers which are names of functions
--  (5) identifiers/strings, parts of signatures (now parts of signatures
--      now must all have slot numbers, represented by (QUOTE <entry>)
--  (6) constants, like 0 and 1, represented by (CONS .. ) form
  for i in $NRTbase..
    for [item,:compItem] in reverse dbUsedEntities db repeat
      domainRef(dbTemplate db,i) := deltaTran(db,item,compItem)
  domainRef(dbTemplate db,$AddChainIndex) :=
    $NRTaddForm =>
      $NRTaddForm is ["%Comma",:y] => reverse! y
      NRTencode(db,$NRTaddForm,$addForm)
    nil

deltaTran(db,item,compItem) ==
  --NOTE: all items but signatures are wrapped with %domain forms
  item is ["%domain",lhs,:.] => NRTencode(db,lhs,compItem)
  [op,dc,:sig,kind] := item
  -- NOTE: sig is already in encoded form since it comes from dbUsedEntities;
  --       so we need only encode dc. -- gdr 2008-11-28.
  dcCode := assocIndex(db,dc) or keyedSystemError("S2NR0004",[dc])
  kindFlag:= (kind is 'CONST => 'CONST; nil)
  [sig,dcCode,op,:kindFlag]

NRTreplaceAllLocalReferences(db,form) ==
  $devaluateList :local := []
  NRTputInLocalReferences(db,form)

NRTencode(db,x,y) == encode(db,x,y,true) where encode(db,x,compForm,firstTime) ==
  --converts a domain form to a lazy domain form; everything other than 
  --the operation name should be assigned a slot
  x is "$" => x
  not firstTime and (k := assocIndex(db,x)) => k
  vector? x => systemErrorHere '"NRTencode"
  cons? x =>
    op := x.op
    op is ":" => [op,second x,encode(db,third x,third compForm,false)]
    (x' := isQuasiquote x) =>
      quasiquote encode(db,x',isQuasiquote compForm,false)
    op is "Enumeration" => x
    ident? op and (constructor? op or builtinConstructor? op) =>
      [op,:[encode(db,y,z,false) for y in x.args for z in compForm.args]]
    -- enumeration constants are like field names, they do not need
    -- to be encoded.
    ['%eval,NRTreplaceAllLocalReferences(db,optimize compForm)]
  symbolMember?(x,$formalArgList) =>
    v := $FormalMapVariableList.(symbolPosition(x,$formalArgList))
    firstTime => ["local",v]
    v
  x is "$$" => x
  compForm is [.,:.] =>
    ['%eval,NRTreplaceAllLocalReferences(db,optimize compForm)]
  quote compForm

--------------FUNCTIONS CALLED DURING CAPSULE FUNCTION COMPILATION-------------
listOfBoundVars(form,e) ==
-- Only called from the function genDeltaEntry below
  form is '$ => []
  ident? form and (u:=get(form,'value,e)) =>
    u := u.expr
    builtinConstructor? KAR u => listOfBoundVars(u,e)
    [form]
  form isnt [.,:.] => []
  first form is 'QUOTE => []
  -- We don't want to pick up the tag, only the domain
  first form = ":" => listOfBoundVars(third form,e)
  first form = "Enumeration" => []
  "union"/[listOfBoundVars(x,e) for x in rest form]


dbEntitySlot(db,x) ==
  n := or/[i for i in 1.. for [z,:.] in dbUsedEntities db | x = z] =>
    $NRTbase + dbEntityCount db - n
  nil

++ Subroutine of optDeltaEntry
++ Return true if the signature `sig' contains flag types
++ that need to be quoted.
needToQuoteFlags?(sig,env) ==
  or/[selector?(t,env) for t in sig] where
    selector?(t,e) ==
      ident? t and null get(t,"value",e)

markOperation f ==
  f = nil => f
  ident? f => ['%external,f]
  f

optDeltaEntry(op,sig,dc,kind,e) ==
  -- references to modemaps from current domain are folded in a later
  -- stage of the compilation process.
  dc is '$ => nil
  ndc :=
    dc isnt [.,:.] and (T := get(dc,'value,e)) => T.expr
    dc
  sig := MSUBST(ndc,dc,sig)
  -- Don't bother if the domain of computation is not an instantiation
  -- nor a candidate for inlining.
  ndc isnt [.,:.] or not optimizableDomain? ndc => nil
  fun := lookupDefiningFunction(op,sig,ndc)
  -- following code is to handle selectors like first, rest
  if fun = nil and needToQuoteFlags?(sig,e) then
     nsig := [quoteSelector(tt,e) for tt in sig] where
       quoteSelector(x,e) ==
         not(ident? x) => x
         get(x,'value,e) => x
         x='$ => x
         MKQ x
     fun := lookupDefiningFunction(op,nsig,ndc)
  fun = nil => nil
  fun :=
    fun is ['makeSpadConstant,:.] and
      (fun' := getFunctionReplacement second fun) =>
         return fun'
    cons? fun => first fun
    fun
  markOperation getFunctionReplacement fun

++ True if we are interested only in abstract slot, not the actual
++ slot number in the template vector.
$onlyAbstractSlot := false

genDeltaEntry(op,mm,e) ==
  if mm.mmDC is 'Rep then
    mm := substitute(getRepresentation e,'Rep,mm)
  else if (x := get('$,'%dc,e)) and x = mm.mmDC then
    mm := MSUBST('$,x,mm)
  [[dc,:sig],[.,cform:=[kind,.,nsig]]] := mm
  if $profileCompiler then profileRecord(dc,op,sig)
  kind is 'XLAM => cform
  if kind is 'Subsumed then kind := 'ELT
  $onlyAbstractSlot => [kind,'$,[op,[dc,:sig]]]
  db := currentDB e
  if dc isnt [.,:.] then
    dc = "$" => nsig := sig
    if integer? nsig then nsig := MSUBST("$",dc,substitute("$$","$",sig))
  setDifference(listOfBoundVars(dc,e),$functorLocalParameters) ~= [] =>
    ['%apply,['compiledLookupCheck,MKQ op,
         mkList consSig(db,nsig,dc),consDomainForm(db,dc,nil)]]
  odc := dc
  if cons? dc then 
    dc := substitute("$$","$",dc)
  if assocIndex(db,dc) = nil and (dc is [.,:.] or
    ident? dc and symbolMember?(dc,$functorLocalParameters)) then
      -- This modemap's domain of computation did not contributte an
      -- an operation before; give it  a slot before the modemap itself.
      entry := [["%domain",addInner(db,dc)]]
      dbUsedEntities(db) := [entry,:dbUsedEntities db]
      dbEntityCount(db) := dbEntityCount db + 1
      entry.rest := compOrCroak(odc,$EmptyMode,e).expr
  u :=
    [kind,'$,index] where index() ==
      desc := [op,dc,:[getLocalIndex(db,x) for x in nsig],kind]
      n := dbEntitySlot(db,desc) => n
      n := dbEntityCount db + $NRTbase
      dbUsedEntities(db) := [[desc,op,dc,:nsig,kind],:dbUsedEntities db]
      dbEntityCount(db) := dbEntityCount db + 1
      n
  impl := optDeltaEntry(op,nsig,dc,kind,e) => impl
  u

++ Return the slot number (within the template vector of the functor
++ being compiled) of the domain or value referenced by the form `x'.
++ Otherwise, return nil this is the first time `x' is referenced, or
++ if `x' designates neither a domain nor a value (e.g. a modemap).
assocIndex: (%Thing,%Form) -> %Maybe %Short
assocIndex(db,x) ==
  x = nil => x
  x is '$ => 0
  x = $NRTaddForm => $AddChainIndex
  dbEntitySlot(db,['%domain,x])

getLocalIndex: (%Thing,%Form) -> %Short
getLocalIndex(db,item) ==
  item is "$$" => 2
  k := assocIndex(db,item) => k
  item isnt [.,:.] and not symbolMember?(item,$formalArgList) =>  --give slots to atoms
    entry := [["%domain",addInner(db,item)],:item]
    dbUsedEntities(db) := [entry,:dbUsedEntities db]
    index := $NRTbase + dbEntityCount db      -- slot number to return
    dbEntityCount(db) := dbEntityCount db + 1
    index
  entry := [["%domain",addInner(db,item)]]
  dbUsedEntities(db) := [entry,:dbUsedEntities db]
  saveIndex := $NRTbase + dbEntityCount db
  dbEntityCount(db) := dbEntityCount db + 1
  entry.rest := 
    -- when assigning slot to flag values, we don't really want to
    -- compile them.  Rather, we want to record them as if they were atoms.
    -- we don't need to compile the flag again.
    -- ??? In fact we should not be compiling again at this phase.
    -- ??? That we do is likely a bug.
    isQuasiquote item => item  
    compOrCroak(item,$EmptyMode,$e).expr
  saveIndex

++ addInner should call following function instead of getLocalIndex
++ This would prevent putting spurious items in dbUsedEntities
innerGetLocalIndex(db,x) ==
  x isnt [.,:.] => x
  op := x.op
  ident? op and (constructor? op or builtinConstructor? op) =>
    getLocalIndex(db,x)
  op is "[||]" => getLocalIndex(db,x)
  addInner(db,x)


addInner(db,x) ==
--called by genDeltaEntry and others that affect dbUsedEntities
  do
    x isnt [.,:.] => nil
    x is [":",y,z] => [x.op,y,innerGetLocalIndex(db,z)]
    x is ['SubDomain,y,:.] => innerGetLocalIndex(db,y)
    builtinConstructor? x.op or x.op is "[||]" =>
      for y in x.args repeat
        innerGetLocalIndex(db,y)
    niladicConstructor? x.op => nil
    cosig := getDualSignature x.op =>
      for y in x.args for t in cosig.source | y isnt '$ and t repeat
        innerGetLocalIndex(db,y)
    keyedSystemError("S2NR0003",[x])
  x


consSig(db,sig,dc) == [consDomainName(db,sigpart,dc) for sigpart in sig]

consDomainName(db,x,dc) ==
  x = dc => ''$
  x is '$ => ''$
  x is "$$" => ['devaluate,'$]
  x is [op,:argl] =>
    (op is 'Record) or (op is 'Union and argl is [[":",:.],:.])  =>
       mkList [MKQ op,
         :[['%list,MKQ '_:,MKQ tag,consDomainName(db,dom,dc)]
                   for [.,tag,dom] in argl]]
    isFunctor op or op is 'Mapping or constructor? op =>
         -- call to constructor? needed if op was compiled in $bootStrapMode
        mkList [MKQ op,:[consDomainName(db,y,dc) for y in argl]]
    substitute('$,"$$",x)
  x = [] => x
  y := LASSOC(x,$devaluateList) => y
  k := assocIndex(db,x) => ['devaluate,['%tref,'$,k]]
  get(x,'value,$e) =>
    isDomainForm(x,$e) => ['devaluate,x]
    x
  MKQ x

consDomainForm(db,x,dc) ==
  x is '$ => '$
  x is [op,:argl] =>
     op = ":" and argl is [tag, value] => [op,tag,consDomainForm(db,value,dc)]
     [op,:[consDomainForm(db,y,dc) for y in argl]]
  x = [] => x
  (y := LASSOC(x,$devaluateList)) => y
  k := assocIndex(db,x) => ['%tref,'$,k]
  get(x,'value,$e) or get(x,'mode,$e) => x
  MKQ x


++ Called by buildFunctor fill dbTemplate slots with names 
++ of compiled functions
descendCodeTran(db,u,condList) ==
  null u => nil
  u is ['%list] => nil
  u is ['%store,['%tref,.,i],a] =>
    null condList and a is ['%closure,fn,:.] =>
      u.first := '%list
      u.rest := nil
      domainRef(dbTemplate db,i) :=
        fn is '%constant => a.args
        fn is ['%function,fn'] => fn'
        fn
    nil   --code for this will be generated by the instantiator
  u is ['%when,:c] =>
    for [pred,:y] in c|y repeat descendCodeTran(db,first y,[pred,:condList])
  u is ['PROGN,:c] => for x in c repeat descendCodeTran(db,x,condList)
  nil

++ Remove useless statements from the elaboration `form' of
++ a function definition.  
washFunctorBody form == main form where
  main form ==
    form' := nil
    for x in form repeat
      stmt := clean x
      stmt = nil => nil
      stmt is ["PROGN",:l] => form' := [:form',:l]
      form' := [:form',stmt]
    form'

  clean x ==
    x is ["PROGN",:stmts] =>
      stmts := [s' for s in stmts | (s' := clean s) ~= nil]
      stmts = nil => nil
      rest stmts = nil => first stmts
      ["PROGN",:stmts]
    x is ['%when,:.] =>
      for cl in x.args | cl is [.,s] repeat
        second(cl) := clean s
      x
    x is ['%list] => nil
    x

--=======================================================================
--              Instantiation Code (Stuffslots)
--=======================================================================
stuffSlot(dollar,i,item) ==
  vectorRef(dollar,i) :=
    item isnt [.,:.] => [symbolFunction item,:dollar]
    item is [n,:op] and integer? n => ['newGoGet,dollar,:item]
    item is ['%constant,[a,b]] =>
      b is '$ => ['makeSpadConstant,symbolFunction a,dollar,i]
      sayBrightlyNT '"Unexpected constant environment!!"
      pp devaluate b
      nil
    item

stuffDomainSlots dollar ==
  domname := devaluate dollar
  infovec := property(opOf domname,'infovec)
  lookupFunction := symbolFunction getLookupFun infovec
  template := infovec.0
  if vectorRef(template,$AddChainIndex) then
    stuffSlot(dollar,$AddChainIndex,vectorRef(template,$AddChainIndex))
  for i in ($NRTbase + # rest domname)..maxIndex template
    | item := vectorRef(template,i) repeat
      stuffSlot(dollar,i,item)
  domainDirectory(dollar) := [lookupFunction,dollar,infovec.1]
  domainAttributes(dollar) := infovec.2
  proto4 := infovec.3
  domainData(dollar) := 
    vector? CDDR proto4 => [COPY_-SEQ first proto4,:rest proto4]   --old style
    bitVector := domainPredicates dollar
    predvec := first proto4
    packagevec := second proto4
    auxvec := vector [fn for i in 0..maxIndex predvec] where fn() ==
      not testBitVector(bitVector,predvec.i) => nil
      packagevec.i or true
    [auxvec,:CDDR proto4]

getLookupFun infovec ==
  maxIndex infovec = 4 => infovec.4
  'lookupIncomplete

makeSpadConstant [fn,dollar,slot] ==
  val := apply(fn,[dollar])
  u := domainRef(dollar,slot)
  u.first := function IDENTITY
  u.rest := val
  val

buildFunctor(db,sig,code,$locals,$e) ==
--PARAMETERS
--  $definition: constructor form, e.g. (SquareMatrix 10 (RationalNumber))
--  sig: signature of constructor form
--  code: result of "doIt", converting body of capsule to CodeDefine forms, e.g.
--       (PROGN (%LET Rep ...)
--              (: (ListOf x y) $)
--              (CodeDefine (<op> <signature> <functionName>))
--              (%when ((HasCategory $ ...) (PROGN ...))) ..)
--  $locals: list of variables to go into slot 5, e.g. (R Rep R,1 R,2 R,3 R,4)
--           same as $functorLocalParameters
--           this list is not augmented by this function
--  $e: environment
--GLOBAL VARIABLES REFERENCED:
--  $QuickCode: compilation flag
  $definition: local := dbConstructorForm db
  [name,:args] := $definition

  if code is ['add,.,newstuff] then code := newstuff

  changeDirectoryInSlot1 db  --this extends $NRTslot1PredicateList

  --LOCAL BOUND FLUID VARIABLES:
  $GENNO: local:= 0     --bound in compDefineFunctor1, then as parameter here
  $hasCategoryAlist: local := nil  --list of GENSYMs bound to (HasCategory ..) items
  $SetFunctions: local := nil  --copy of p view with preds telling when fnct defined
  $epilogue: local := nil     --code to set slot 5, things to be done last
  $HackSlot4: local := nil  --Invention of JHD 13/July/86-set in InvestigateConditions
  $extraParms:local := nil  --Set in DomainSubstitutionFunction
  $devaluateList: local :=
    --Bound to ((#1 . dv$1)..) where &1 := devaluate #1 later
    [[arg,:b] for arg in args for b in $ModeVariableList]
  $supplementaries: local := nil
   --set in InvestigateConditions to represent any additional
   --category membership tests that may be needed(see buildFunctor for details)

  oldtime:= TEMPUS_-FUGIT()
  [catsig,:argsig] := sig
  catvecListMaker := removeDuplicates
    [comp(catsig,$EmptyMode,$e).expr,
      :[compCategories(u,$e) for [u,:.] in categoryAncestors dbDomainShell db]]
  tbl := makeTable function valueEq?
  condCats := InvestigateConditions(db,[catsig,:rest catvecListMaker],tbl,$e)
  -- a list, one %for each element of catvecListMaker
  -- indicating under what conditions this
  -- category should be present.  true => always
  dbTemplate(db) := newShell($NRTbase + dbEntityCount db)
  $SetFunctions := newShell # dbTemplate db
  -- list of names n1..nn for each view
  viewNames := ['$,:[genvar() for u in rest catvecListMaker]]
  domname := 'dv_$

  -- Do this now to create predicate vector; then DescendCode can refer
  -- to predicate vector if it can
  [$uncondAlist,:$condAlist] :=    --bound in compDefineFunctor1
      setVector4Part1(db,viewNames,catvecListMaker,condCats,$e)
  [$NRTslot1PredicateList,predBitVectorCode1,:predBitVectorCode2] :=
    makePredicateBitVector(db,[:ASSOCRIGHT $condAlist,:$NRTslot1PredicateList],$e)

  storeOperationCode := DescendCode(db,code,true,nil,$e)
  addDeltaCode db
  storeOperationCode := NRTputInLocalReferences(db,storeOperationCode)
  descendCodeTran(db,storeOperationCode,nil) --side effects storeOperationCode
  codePart2:=
    argStuffCode :=
      [['%store,['%tref,'$,i],v] for i in $NRTbase.. for v in $FormalMapVariableList
	for arg in args]
    if symbolMember?($NRTaddForm,$locals) then
       addargname := $FormalMapVariableList.(symbolPosition($NRTaddForm,$locals))
       argStuffCode := [['%store,['%tref,'$,$AddChainIndex],addargname],:argStuffCode]
    [['stuffDomainSlots,'$],:argStuffCode,
       :predBitVectorCode2,storeOperationCode]

  -- Local bindings
  bindings := [:devaluateCode,createDomainCode,
                 createViewCode,createPredVecCode] where
    devaluateCode:= [[b,["devaluate",a]] for [a,:b] in $devaluateList]
    createDomainCode:=
      [domname,['%list,MKQ name,:ASSOCRIGHT $devaluateList]]
    createViewCode:= ["$",["newShell", $NRTbase + dbEntityCount db]]
    createPredVecCode := ["pv$",predBitVectorCode1]

  --CODE: part 1
  codePart1 := [setVector0Code, slot3Code,:slamCode] where
    setVector0Code := ['%store,['%tref,"$",0],"dv$"]
    slot3Code := ['%store,['%tref,"$",3],"pv$"]
    slamCode :=
      isCategoryPackageName name => nil
      [addToSlam($definition,"$")]

  --CODE: part 3
  codePart3 := $epilogue
  ans := ["%bind",bindings,
           ['%seq,:washFunctorBody optFunctorBody
              [:codePart1,:codePart2,:codePart3],"$"]]
  $getDomainCode := nil
    --if we didn't kill this, DEFINE would insert it in the wrong place
  SAY ['"time taken in buildFunctor: ",TEMPUS_-FUGIT()-oldtime]
  ans

setVector4Part1(db,siglist,formlist,condlist,e) ==
  $uncondList: local := nil
  $condList: local := nil
  $count: local := 0
  for sig in reverse siglist for form in reverse formlist
         for cond in reverse condlist repeat
                  setVector4a(db,sig,form,cond,e)
  reducedUncondlist := removeDuplicates $uncondList
  reducedConlist :=
    [[x,:y] for [x,z] in $condList| y := SETDIFFERENCE(z,reducedUncondlist)]
  revCondlist := reverseCondlist reducedConlist
  orCondlist := [[x,:MKPF(y,'OR)] for [x,:y] in revCondlist]
  [reducedUncondlist,:orCondlist]

reverseCondlist cl ==
  alist := nil
  for [x,:y] in cl repeat
    for z in y repeat
      u := assoc(z,alist)
      null u => alist := [[z,x],:alist]
      member(x,rest u) => nil
      u.rest := [x,:rest u]
  alist

setVector4a(db,sig,form,cond,e) ==
  sig is '$ =>
     domainList :=
       [domForm for d in categoryPrincipals dbDomainShell db] where
          domForm() == optimize
            T := comp(d,$EmptyMode,e) => T.expr
            d
     $uncondList := append!(domainList,$uncondList)
     if isCategoryForm(form,e) then $uncondList := [form,:$uncondList]
     $uncondList
  evalform := evalCategoryForm(form,e)
  cond is true =>
    $uncondList := [form,:append(categoryPrincipals evalform,$uncondList)]
  $condList := [[cond,[form,:categoryPrincipals evalform]],:$condList]

makeSlot1Info db ==
-- 4 cases:
-- a:T == b add c  --- slot1 directory has #s for entries defined in c
-- a:T == b        --- slot1 has all slot #s = nil (see compFunctorBody)
-- a == b add c    --- not allowed (line 7 of getTargetFromRhs)
-- a == b          --- $NRTderivedTargetIfTrue = true; set directory to nil
  pairlis :=
    $insideCategoryPackageIfTrue =>
      [[first dbParameters db,:'_$],:dbFormalSubst db]
    dbFormalSubst db
  exports :=
    transformOperationAlist applySubst(pairlis,categoryExports dbDomainShell db)
  opList :=
    $NRTderivedTargetIfTrue => 'derived
    $insideCategoryPackageIfTrue => slot1Filter exports
    exports
  addList := applySubst(pairlis,$NRTaddForm)
  [dbConstructor db,[addList,:opList]]

slot1Filter opList ==
--include only those ops which are defined within the capsule
  [u for x in opList | u := fn x] where
    fn [op,:l] ==
      u := [entry for entry in l | integer? second entry] => [op,:u]
      nil

optimizeHas u ==
--u is a list ((pred cond)...) -- see optFunctorBody
--produces an alist: (((HasCategory a b) . gensym)...)
  u is [a,:b] =>
    a='HasCategory => LASSOC(u,$hasCategoryAlist) or
      $hasCategoryAlist := [[u,:(y:=gensym())],:$hasCategoryAlist]
      y
    a="has" => optimizeHas ['HasCategory,first b,MKQ second b]
    a is 'QUOTE => u
    [optimizeHas a,:optimizeHas b]
  u

addToSlam([name,:argnames],shell) ==
  dbInstanceCache constructorDB name = nil => return nil
  null argnames => addToConstructorCache(name,nil,shell)
  args := ['%list,:ASSOCRIGHT $devaluateList]
  addToConstructorCache(name,args,shell)

changeDirectoryInSlot1 db ==  --called by buildFunctor
  --3 cases:
  --  if called inside buildFunctor, dbEntityCount gives different locs
  --  otherwise called from compFunctorBody (all lookups are forwarded):
  --    dbUsedEntities = nil  ===> all slot numbers become nil
  $lisplibOperationAlist := [sigloc(db,entry) for entry in categoryExports dbDomainShell db] where
    sigloc(db,[opsig,pred,fnsel]) ==
      if pred isnt true then
        pred := simpBool pred
        $NRTslot1PredicateList := insert(pred,$NRTslot1PredicateList)
      fnsel is [kind,a,:.] and kind in '(ELT CONST) =>
        if $insideCategoryPackageIfTrue then
          opsig := substitute('$,first dbParameters db,opsig)
        [opsig,pred,[kind,a,vectorLocation(db,first opsig,second opsig,kind)]]
      [opsig,pred,fnsel]
  sortedOplist := listSort(function GLESSEQP,
                           copyList $lisplibOperationAlist,function second)
  $lastPred: local := false
  $newEnv: local := $e
  categoryExports(dbDomainShell db) := [fn(db,entry) for entry in sortedOplist] where
    fn(db,[[op,sig],pred,fnsel]) ==
       if $lastPred ~= pred then
            $newEnv := deepChaseInferences(pred,$e)
            $lastPred := pred
       newfnsel :=
         fnsel is ['Subsumed,op1,sig1] =>
           ['Subsumed,op1,genSlotSig(db,sig1,$newEnv)]
         fnsel
       [[op, genSlotSig(db,sig,$newEnv)] ,pred,newfnsel]

genSlotSig(db,sig,$e) ==
   [getLocalIndex(db,t) for t in sig]

deepChaseInferences(pred,$e) ==
    pred is [op,:preds] and op in '(AND and %and) =>
        for p in preds repeat $e := deepChaseInferences(p,$e)
        $e
    pred is [op,pred1,:.] and op in '(OR or %or) =>
        deepChaseInferences(pred1,$e)
    pred is 'T or pred is [op,:.] and op in '(NOT not %not) => $e
    chaseInferences(pred,$e)

vectorLocation(db,op,sig,kind) ==
  u := or/[i for i in 1.. for [.,:u] in dbUsedEntities db
        | u is [=op,'$,:xsig,=kind] and sig = xsig]
  u => dbEntityCount db - u + $NRTbase 
  nil    -- this signals that calls should be forwarded

-----------------------------SLOT1 DATABASE------------------------------------

NRTputInLocalReferences(db,bod) ==
  ident? bod and (k := assocIndex(db,dom)) => ['%tref,'$,k]
  do
    bod isnt [.,:.] => nil
    bod is ['SPADCALL,:.] =>
      NRTputInTail(db,bod.args)
      fn := last bod.args
      -- The following test allows function-returning expressions
      fn is [elt,dom,ind] and dom ~='$ and elt in '(ELT CONST) =>
        k := assocIndex(db,dom) => lastNode(bod).first := ['%tref,'_$,k]
    bod.op is '%when =>
      for cc in bod.args repeat NRTputInTail(db,cc)
    bod.op in '(QUOTE CLOSEDFN) => nil
    abstraction? bod =>
      bod.absBody := NRTputInLocalReferences(db,bod.absBody)
    NRTputInLocalReferences(db,first bod)
    NRTputInTail(db,rest bod)
  bod

NRTputInTail(db,x) ==
  for y in tails x repeat
    (u := first y) isnt [.,:.] =>
      u is '$ or LASSOC(u,$devaluateList) => nil
      k := assocIndex(db,u) =>
        u isnt [.,:.] => y.first := ['%tref,'_$,k]
        -- u atomic means that the slot will always contain a vector
        y.first := ['SPADCHECKELT,'_$,k]
      --this reference must check that slot is a vector
      nil
    NRTputInLocalReferences(db,u)
  x



