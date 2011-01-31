-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2011, Gabriel Dos Reis.
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


import nrunopt
import simpbool
import profile
import functor
namespace BOOT


++ The base index for encoding items into a functor template 
++ (e.g. domainShell).  This is also the minimum length that a
++ template could possibly have.
$NRTbase ==
  6

++
$devaluateList := []
$functorLocalParameters := []
$insideCategoryPackageIfTrue := false

++ By default, don't generate info files
$profileCompiler := false

++
$Slot1DataBase := hashTable 'EQ

++
$NRTdeltaList := []
$NRTdeltaLength := 0

++
$NRTaddForm := nil

++
$NRTderivedTargetIfTrue := false
$killOptimizeIfTrue := false

NRTaddDeltaCode() ==
--NOTES: This function is called from buildFunctor to initially
--  fill slots in $template. The $template so created is stored in the
--  NRLIB. On load, makeDomainTemplate is called on this $template to
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
  kvec := first $catvecList
  for i in $NRTbase.. for item in reverse $NRTdeltaList
    for compItem in reverse $NRTdeltaListComp
      |null (s:=kvec.i) repeat
        $template.i:= deltaTran(item,compItem)
  $template.5 :=
    $NRTaddForm =>
      $NRTaddForm is ["%Comma",:y] => nreverse y
      NRTencode($NRTaddForm,$addForm)
    nil

deltaTran(item,compItem) ==
  --NOTE: all items but signatures are wrapped with %domain forms
  item is ["%domain",lhs,:.] => NRTencode(lhs,compItem)
  [op,:modemap] := item
  [dcSig,[.,[kind,:.]]] := modemap
  [dc,:sig] := dcSig
  -- NOTE: sig is already in encoded form since it comes from $NRTdeltaList;
  --       so we need only encode dc. -- gdr 2008-11-28.
  dcCode :=
    dc = '$ => 0
    NRTassocIndex dc or keyedSystemError("S2NR0004",[dc])
  kindFlag:= (kind = 'CONST => 'CONST; nil)
  [sig,dcCode,op,:kindFlag]

NRTreplaceAllLocalReferences(form) ==
  $devaluateList :local := []
  NRTputInLocalReferences form

NRTencode(x,y) == encode(x,y,true) where encode(x,compForm,firstTime) ==
  --converts a domain form to a lazy domain form; everything other than 
  --the operation name should be assigned a slot
  not firstTime and (k:= NRTassocIndex x) => k
  vector? x => systemErrorHere '"NRTencode"
  cons? x =>
    op := first x
    op = "Record" or x is ['Union,['_:,a,b],:.] =>
      [op,:[['_:,a,encode(b,c,false)]
        for [.,a,b] in rest x for [.,=a,c] in rest compForm]]
    (x' := isQuasiquote x) =>
      quasiquote encode(x',isQuasiquote compForm,false)
    IDENTP op and (constructor? op or op in '(Union Mapping)) =>
      [op,:[encode(y,z,false) for y in rest x for z in rest compForm]]
    -- enumeration constants are like field names, they do not need
    -- to be encoded.
    op = "Enumeration" => x
    ["NRTEVAL",NRTreplaceAllLocalReferences COPY_-TREE simplifyVMForm compForm]
  MEMQ(x,$formalArgList) =>
    v := $FormalMapVariableList.(POSN1(x,$formalArgList))
    firstTime => ["local",v]
    v
  x = "$" => x
  x = "$$" => x
  ['QUOTE,x]

--------------FUNCTIONS CALLED DURING CAPSULE FUNCTION COMPILATION-------------
listOfBoundVars form ==
-- Only called from the function genDeltaEntry below
  form = '$ => []
  IDENTP form and (u:=get(form,'value,$e)) =>
    u:=u.expr
    KAR u in '(Union Record) => listOfBoundVars u
    [form]
  atom form => []
  first form = 'QUOTE => []
  -- We don't want to pick up the tag, only the domain
  first form = ":" => listOfBoundVars third form
  first form = "Enumeration" => []
  "union"/[listOfBoundVars x for x in rest form]


++ Subroutine of optDeltaEntry
++ Return true if the signature `sig' contains flag types
++ that need to be quoted.
needToQuoteFlags?(sig,env) ==
  or/[selector?(t,env) for t in sig] where
    selector?(t,e) ==
      IDENTP t and null get(t,"value",e)

optDeltaEntry(op,sig,dc,eltOrConst) ==
  $killOptimizeIfTrue = true => nil
  -- references to modemaps from current domain are folded in a later
  -- stage of the compilation process.
  dc = '$ => nil
  ndc :=
    atom dc and (dcval := get(dc,'value,$e)) => dcval.expr
    dc
  sig := MSUBST(ndc,dc,sig)
  -- Don't bothe if the domain of computation is not an instantiation,
  -- or is candidate for inlining.
  atom ndc or not optimizableDomain? ndc => nil
  fun := lookupDefiningFunction(op,sig,ndc)
  -- following code is to handle selectors like first, rest
  if fun = nil and needToQuoteFlags?(sig,$e) then
     nsig := [quoteSelector tt for tt in sig] where
       quoteSelector(x) ==
         not(IDENTP x) => x
         get(x,'value,$e) => x
         x='$ => x
         MKQ x
     fun := lookupDefiningFunction(op,nsig,ndc)
  fun = nil => nil
  if cons? fun then
    eltOrConst = "CONST" => return ['XLAM,'ignore, SPADCALL fun]
    fun := first fun
  getFunctionReplacement compileTimeBindingOf fun

genDeltaEntry(opMmPair,e) ==
--called from compApplyModemap
--$NRTdeltaLength=0.. always equals length of $NRTdeltaList
  [op,[dc,:sig],[.,cform:=[eltOrConst,.,nsig]]] := opMmPair
  if $profileCompiler = true then profileRecord(dc,op,sig)
  eltOrConst = 'XLAM => cform
  if eltOrConst = 'Subsumed then eltOrConst := 'ELT
  if atom dc then
    dc = "$" => nsig := sig
    if integer? nsig then nsig := MSUBST("$",dc,substitute("$$","$",sig))
  setDifference(listOfBoundVars dc,$functorLocalParameters) ~= [] =>
    ['applyFun,['compiledLookupCheck,MKQ op,
         mkList consSig(nsig,dc),consDomainForm(dc,nil)]]
  odc := dc
  if cons? dc then 
    dc := substitute("$$","$",dc)
  opModemapPair :=
    [op,[dc,:[NRTgetLocalIndex x for x in nsig]],["T",cform]] -- force pred to T
  if null NRTassocIndex dc and
    (member(dc,$functorLocalParameters) or cons? dc) then
    --create "%domain" entry to $NRTdeltaList
      $NRTdeltaList:= [["%domain",NRTaddInner dc],:$NRTdeltaList]
      saveNRTdeltaListComp:= $NRTdeltaListComp:=[nil,:$NRTdeltaListComp]
      $NRTdeltaLength := $NRTdeltaLength+1
      compEntry:= (compOrCroak(odc,$EmptyMode,e)).expr
      saveNRTdeltaListComp.first := compEntry
  u :=
    [eltOrConst,'$,$NRTbase+$NRTdeltaLength-index] where index() ==
      (n:= POSN1(opModemapPair,$NRTdeltaList)) => n + 1
        --n + 1 since $NRTdeltaLength is 1 too large
      $NRTdeltaList:= [opModemapPair,:$NRTdeltaList]
      $NRTdeltaListComp:=[nil,:$NRTdeltaListComp]
      $NRTdeltaLength := $NRTdeltaLength+1
      0
  impl := optDeltaEntry(op,nsig,odc,eltOrConst) => impl
  u

++ Return the slot number (within the template vector of the functor
++ being compiled) of the domain or value referenced by the form `x'.
++ Otherwise, return nil this is the first time `x' is referenced, or
++ if `x' designates neither a domain nor a value (e.g. a modemap).
NRTassocIndex: %Form -> %Maybe %Short
NRTassocIndex x ==
  null x => x
  x = $NRTaddForm => 5
  k := or/[i for i in 1.. for y in $NRTdeltaList
            | first y = "%domain" and second y = x] =>
    $NRTbase + $NRTdeltaLength - k
  nil

NRTgetLocalIndex: %Form -> %Short
NRTgetLocalIndex item ==
  k := NRTassocIndex item => k
  item = "$" => 0
  item = "$$" => 2
  atom item and not MEMQ(item,$formalArgList) =>  --give slots to atoms
    $NRTdeltaList:= [["%domain",NRTaddInner item],:$NRTdeltaList]
    $NRTdeltaListComp:=[item,:$NRTdeltaListComp]
    index := $NRTbase + $NRTdeltaLength      -- slot number to return
    $NRTdeltaLength := $NRTdeltaLength+1
    index
  -- when assigning slot to flag values, we don't really want to
  -- compile them.  Rather, we want to record them as if they were atoms.
  flag := isQuasiquote item
  $NRTdeltaList:= [["%domain", NRTaddInner item], :$NRTdeltaList]
  -- remember the item's place in the `delta list' and its slot number
  -- before the recursive call to the compiler, as that might generate
  -- more references that would extend the `delta list'.
  saveNRTdeltaListComp:= $NRTdeltaListComp:=[nil,:$NRTdeltaListComp]
  saveIndex := $NRTbase + $NRTdeltaLength
  $NRTdeltaLength := $NRTdeltaLength+1
  compEntry:= 
    -- we don't need to compile the flag again.
    -- ??? In fact we should not be compiling again at this phase.
    -- ??? That we do is likely a bug.
    flag => item  
    (compOrCroak(item,$EmptyMode,$e)).expr
  saveNRTdeltaListComp.first := compEntry
  saveIndex

NRTassignCapsuleFunctionSlot(op,sig) ==
--called from compDefineCapsuleFunction
  opSig := [op,sig]
  [.,.,implementation] := NRTisExported? opSig or return nil
    --if opSig is not exported, it is local and need not be assigned
  if $insideCategoryPackageIfTrue then
      sig := substitute('$,second($functorForm),sig)
  sig := [NRTgetLocalIndex x for x in sig]
  opModemapPair := [op,['_$,:sig],["T",implementation]]
  POSN1(opModemapPair,$NRTdeltaList) => nil   --already there
  $NRTdeltaList:= [opModemapPair,:$NRTdeltaList]
  $NRTdeltaListComp := [nil,:$NRTdeltaListComp]
  $NRTdeltaLength := $NRTdeltaLength+1


++ NRTaddInner should call following function instead of NRTgetLocalIndex
++ This would prevent putting spurious items in $NRTdeltaList
NRTinnerGetLocalIndex x ==
  atom x => x
  -- following test should skip Unions, Records, Mapping
  op := first x
  op in '(Union Record Mapping Enumeration _[_|_|_]) => NRTgetLocalIndex x
  constructor? op => NRTgetLocalIndex x
  NRTaddInner x


NRTaddInner x ==
--called by genDeltaEntry and others that affect $NRTdeltaList
  PROGN
    atom x => nil
    x is ['Record,:l] =>
      for [.,.,y] in l repeat NRTinnerGetLocalIndex y
    first x in '(Union Mapping _[_|_|_]) =>
      for y in rest x repeat
         y is [":",.,z] => NRTinnerGetLocalIndex z
         NRTinnerGetLocalIndex y
    x is ['SubDomain,y,:.] => NRTinnerGetLocalIndex y
    getConstructorSignature first x is [.,:ml] =>
      for y in rest x for m in ml | not (y = '$) repeat
        isCategoryForm(m,$CategoryFrame) => NRTinnerGetLocalIndex y
    x is ["Enumeration",:.] =>
      for y in rest x repeat NRTinnerGetLocalIndex y
    keyedSystemError("S2NR0003",[x])
  x


NRTisExported? opSig ==
  or/[u for u in $domainShell.1 | u.0 = opSig]

consOpSig(op,sig,dc) ==
  if cons? op then
    keyedSystemError("S2GE0016",['"consOpSig",'"bad operator in table"])
  mkList [MKQ op,mkList consSig(sig,dc)]

consSig(sig,dc) == [consDomainName(sigpart,dc) for sigpart in sig]

consDomainName(x,dc) ==
  x = dc => ''$
  x = '$ => ''$
  x = "$$" => ['devaluate,'$]
  x is [op,:argl] =>
    (op = 'Record) or (op = 'Union and argl is [[":",:.],:.])  =>
       mkList [MKQ op,
         :[['%listlit,MKQ '_:,MKQ tag,consDomainName(dom,dc)]
                   for [.,tag,dom] in argl]]
    isFunctor op or op = 'Mapping or constructor? op =>
         -- call to constructor? needed if op was compiled in $bootStrapMode
        mkList [MKQ op,:[consDomainName(y,dc) for y in argl]]
    substitute('$,"$$",x)
  x = [] => x
  (y := LASSOC(x,$devaluateList)) => y
  k:=NRTassocIndex x =>
    ['devaluate,['ELT,'$,k]]
  get(x,'value,$e) =>
    isDomainForm(x,$e) => ['devaluate,x]
    x
  MKQ x

consDomainForm(x,dc) ==
  x = '$ => '$
  x is [op,:argl] =>
     op = ":" and argl is [tag, value] => [op, tag, consDomainForm(value,dc)]
     [op,:[consDomainForm(y,dc) for y in argl]]
  x = [] => x
  (y := LASSOC(x,$devaluateList)) => y
  k:=NRTassocIndex x => ['ELT,'$,k]
  get(x,'value,$e) or get(x,'mode,$e) => x
  MKQ x


++ Called by buildFunctor fill $template slots with names 
++ of compiled functions
NRTdescendCodeTran(u,condList) ==
  null u => nil
  u is ['%listlit] => nil
  u is [op,.,i,a] and op in '(setShellEntry QSETREFV) =>
    null condList and a is ['CONS,fn,:.] =>
      u.first := '%listlit
      u.rest := nil
      $template.i :=
        fn = 'IDENTITY => a
        fn is ['dispatchFunction,fn'] => fn'
        fn
    nil   --code for this will be generated by the instantiator
  u is ['COND,:c] =>
    for [pred,:y] in c|y repeat NRTdescendCodeTran(first y,[pred,:condList])
  u is ['PROGN,:c] => for x in c repeat NRTdescendCodeTran(x,condList)
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
    x is ['%listlit] => nil
    x

buildFunctor($definition is [name,:args],sig,code,$locals,$e) ==
--PARAMETERS
--  $definition: constructor form, e.g. (SquareMatrix 10 (RationalNumber))
--  sig: signature of constructor form
--  code: result of "doIt", converting body of capsule to CodeDefine forms, e.g.
--       (PROGN (%LET Rep ...)
--              (: (ListOf x y) $)
--              (CodeDefine (<op> <signature> <functionName>))
--              (COND ((HasCategory $ ...) (PROGN ...))) ..)
--  $locals: list of variables to go into slot 5, e.g. (R Rep R,1 R,2 R,3 R,4)
--           same as $functorLocalParameters
--           this list is not augmented by this function
--  $e: environment
--GLOBAL VARIABLES REFERENCED:
--  $domainShell: passed in from compDefineFunctor1
--  $QuickCode: compilation flag

  if code is ['add,.,newstuff] then code := newstuff

  changeDirectoryInSlot1()  --this extends $NRTslot1PredicateList

  --LOCAL BOUND FLUID VARIABLES:
  $GENNO: local:= 0     --bound in compDefineFunctor1, then as parameter here
  $catvecList: local := nil   --list of vectors v1..vn for each view
  $hasCategoryAlist: local := nil  --list of GENSYMs bound to (HasCategory ..) items
  $catNames: local := nil      --list of names n1..nn for each view
  $catsig: local := nil        --target category (used in ProcessCond)
  $SetFunctions: local := nil  --copy of p view with preds telling when fnct defined
  $ConstantAssignments: local := nil --code for creation of constants
  $epilogue: local := nil     --code to set slot 5, things to be done last
  $HackSlot4: local := nil  --Invention of JHD 13/July/86-set in InvestigateConditions
  $extraParms:local := nil  --Set in DomainSubstitutionFunction, used in setVector12
  $devaluateList: local := nil --Bound to ((#1 . dv$1)..) where &1 := devaluate #1 later
  $devaluateList:= [[arg,:b] for arg in args for b in $ModeVariableList]
  $supplementaries: local := nil
   --set in InvestigateConditions to represent any additional
   --category membership tests that may be needed(see buildFunctor for details)

  oldtime:= TEMPUS_-FUGIT()
  [$catsig,:argsig]:= sig
  catvecListMaker:=removeDuplicates
    [(comp($catsig,$EmptyMode,$e)).expr,
      :[compCategories first u for u in second $domainShell.4]]
  condCats:= InvestigateConditions [$catsig,:rest catvecListMaker]
  -- a list, one %for each element of catvecListMaker
  -- indicating under what conditions this
  -- category should be present.  true => always
  makeCatvecCode:= first catvecListMaker
  emptyVector := VECTOR()
  domainShell := newShell($NRTbase + $NRTdeltaLength)
  for i in 0..4 repeat domainShell.i := $domainShell.i
    --we will clobber elements; copy since $domainShell may be a cached vector
  $template := newShell ($NRTbase + $NRTdeltaLength)
  $catvecList:= [domainShell,:[emptyVector for u in second domainShell.4]]
  $catNames := ['$] -- for DescendCode -- to be changed below for slot 4
  $SetFunctions:= newShell # domainShell
  $catNames:= ['$,:[genvar() for u in rest catvecListMaker]]
  domname:='dv_$

  -- Do this now to create predicate vector; then DescendCode can refer
  -- to predicate vector if it can
  [$uncondAlist,:$condAlist] :=    --bound in compDefineFunctor1
      NRTsetVector4Part1($catNames,catvecListMaker,condCats)
  [$NRTslot1PredicateList,predBitVectorCode1,:predBitVectorCode2] :=
      makePredicateBitVector [:ASSOCRIGHT $condAlist,:$NRTslot1PredicateList]

  storeOperationCode:= DescendCode(code,true,nil,first $catNames)
  NRTaddDeltaCode()
  storeOperationCode:= NRTputInLocalReferences storeOperationCode
  NRTdescendCodeTran(storeOperationCode,nil) --side effects storeOperationCode
  codePart2:=
    argStuffCode :=
      [[$setelt,'$,i,v] for i in $NRTbase.. for v in $FormalMapVariableList
	for arg in args]
    if MEMQ($NRTaddForm,$locals) then
       addargname := $FormalMapVariableList.(POSN1($NRTaddForm,$locals))
       argStuffCode := [[$setelt,'$,5,addargname],:argStuffCode]
    [['stuffDomainSlots,'$],:argStuffCode,
       :predBitVectorCode2,storeOperationCode]

  $CheckVectorList := NRTcheckVector domainShell
  -- Local bindings
  bindings := [:devaluateCode,createDomainCode,
                 createViewCode,createPredVecCode] where
    devaluateCode:= [[b,["devaluate",a]] for [a,:b] in $devaluateList]
    createDomainCode:=
      [domname,['%listlit,MKQ name,:ASSOCRIGHT $devaluateList]]
    createViewCode:= ["$",["newShell", $NRTbase + $NRTdeltaLength]]
    createPredVecCode := ["pv$",predBitVectorCode1]

  --CODE: part 1
  codePart1:= [setVector0Code, slot3Code,:slamCode] where
    setVector0Code:=[$setelt,"$",0,"dv$"]
    slot3Code := [$setelt,"$",3,"pv$"]
    slamCode:=
      isCategoryPackageName name => nil
      [NRTaddToSlam($definition,"$")]

  --CODE: part 3
  $ConstantAssignments :=
      [NRTputInLocalReferences code for code in $ConstantAssignments]
  codePart3:= [:$ConstantAssignments,:$epilogue]
  ans := ["%bind",bindings,
           :washFunctorBody optFunctorBody
              [:codePart1,:codePart2,:codePart3],"$"]
  $getDomainCode:= nil
    --if we didn't kill this, DEFINE would insert it in the wrong place
  SAY ['"time taken in buildFunctor: ",TEMPUS_-FUGIT()-oldtime]
  ans

NRTcheckVector domainShell ==
--RETURNS: an alist (((op,sig),:pred) ...) of missing functions
  alist := nil
  for i in $NRTbase..MAXINDEX domainShell repeat
--Vector elements can be one of
-- (a) T           -- item was marked
-- (b) NIL         -- ???
-- (c) categoryForm-- it was a domain view; now irrelevant
-- (d) op-signature-- store missing function info in $CheckVectorList
    v := domainShell.i
    v=true => nil  --item is marked; ignore
    v=nil => nil
    atom v => systemErrorHere '"CheckVector"
    atom first v => nil  --category form; ignore
    assoc(first v,alist) => nil
    alist := [[first v,:$SetFunctions.i],:alist]
  alist

mkDomainCatName id == INTERN strconc(id,'";CAT")

NRTsetVector4Part1(siglist,formlist,condlist) ==
  $uncondList: local := nil
  $condList: local := nil
  $count: local := 0
  for sig in reverse siglist for form in reverse formlist
         for cond in reverse condlist repeat
                  NRTsetVector4a(sig,form,cond)
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

NRTsetVector4a(sig,form,cond) ==
  sig = '$ =>
     domainList :=
       [simplifyVMForm COPY comp(d,$EmptyMode,$e).expr or d
         for d in $domainShell.4.0]
     $uncondList := append(domainList,$uncondList)
     if isCategoryForm(form,$e) then $uncondList := [form,:$uncondList]
     $uncondList
  evalform := eval mkEvalableCategoryForm form
  cond = true => $uncondList := [form,:append(evalform.4.0,$uncondList)]
  $condList := [[cond,[form,:evalform.4.0]],:$condList]

NRTmakeSlot1Info() ==
-- 4 cases:
-- a:T == b add c  --- slot1 directory has #s for entries defined in c
-- a:T == b        --- slot1 has all slot #s = NIL (see compFunctorBody)
-- a == b add c    --- not allowed (line 7 of getTargetFromRhs)
-- a == b          --- $NRTderivedTargetIfTrue = true; set directory to NIL
  pairlis :=
    $insideCategoryPackageIfTrue =>
      [:argl,dollarName] := rest $form
      [[dollarName,:'_$],:mkSlot1sublis argl]
    mkSlot1sublis rest $form
  $lisplibOpAlist := transformOperationAlist SUBLIS(pairlis,$domainShell.1)
  opList :=
    $NRTderivedTargetIfTrue => 'derived
    $insideCategoryPackageIfTrue => slot1Filter $lisplibOpAlist
    $lisplibOpAlist
  addList := SUBLIS(pairlis,$NRTaddForm)
  [first $form,[addList,:opList]]

mkSlot1sublis argl ==
  pairList(argl,$FormalMapVariableList)

slot1Filter opList ==
--include only those ops which are defined within the capsule
  [u for x in opList | u := fn x] where
    fn [op,:l] ==
      u := [entry for entry in l | integer? second entry] => [op,:u]
      nil

NRToptimizeHas u ==
--u is a list ((pred cond)...) -- see optFunctorBody
--produces an alist: (((HasCategory a b) . gensym)...)
  u is [a,:b] =>
    a='HasCategory => LASSOC(u,$hasCategoryAlist) or
      $hasCategoryAlist := [[u,:(y:=gensym())],:$hasCategoryAlist]
      y
    a="has" => NRToptimizeHas ['HasCategory,first b,MKQ second b]
    a = 'QUOTE => u
    [NRToptimizeHas a,:NRToptimizeHas b]
  u

NRTaddToSlam([name,:argnames],shell) ==
  $mutableDomain => return nil
  null argnames => addToConstructorCache(name,nil,shell)
  args:= ['%listlit,:ASSOCRIGHT $devaluateList]
  addToConstructorCache(name,args,shell)

changeDirectoryInSlot1() ==  --called by buildFunctor
  --3 cases:
  --  if called inside buildFunctor, $NRTdeltaLength gives different locs
  --  otherwise called from compFunctorBody (all lookups are forwarded):
  --    $NRTdeltaList = nil  ===> all slot numbers become nil
  $lisplibOperationAlist := [sigloc entry for entry in $domainShell.1] where
    sigloc [opsig,pred,fnsel] ==
        if pred ~= 'T then
          pred := simpBool pred
          $NRTslot1PredicateList := insert(pred,$NRTslot1PredicateList)
        fnsel is [op,a,:.] and (op = 'ELT or op = 'CONST) =>
          if $insideCategoryPackageIfTrue then
              opsig := substitute('$,second($functorForm),opsig)
          [opsig,pred,[op,a,vectorLocation(first opsig,second opsig)]]
        [opsig,pred,fnsel]
  sortedOplist := listSort(function GLESSEQP,
                           COPY_-LIST $lisplibOperationAlist,function second)
  $lastPred: local := false
  $newEnv: local := $e
  $domainShell.1 := [fn entry for entry in sortedOplist] where
    fn [[op,sig],pred,fnsel] ==
       if $lastPred ~= pred then
            $newEnv := deepChaseInferences(pred,$e)
            $lastPred := pred
       newfnsel :=
         fnsel is ['Subsumed,op1,sig1] =>
           ['Subsumed,op1,genSlotSig(sig1,$newEnv)]
         fnsel
       [[op, genSlotSig(sig,$newEnv)] ,pred,newfnsel]

genSlotSig(sig,$e) ==
   [NRTgetLocalIndex t for t in sig]

deepChaseInferences(pred,$e) ==
    pred is [op,:preds] and op in '(AND and %and) =>
        for p in preds repeat $e := deepChaseInferences(p,$e)
        $e
    pred is [op,pred1,:.] and op in '(OR or %or) =>
        deepChaseInferences(pred1,$e)
    pred is 'T or pred is [op,:.] and op in '(NOT not %not) => $e
    chaseInferences(pred,$e)

vectorLocation(op,sig) ==
  u := or/[i for i in 1.. for u in $NRTdeltaList
        | u is [=op,['$,: xsig],:.] and sig=NRTsubstDelta(xsig) ]
  u => $NRTdeltaLength - u + $NRTbase 
  nil    -- this signals that calls should be forwarded

NRTsubstDelta(initSig) ==
  sig := [replaceSlotTypes s for s in initSig] where
     replaceSlotTypes(t) ==
        atom t =>
          not integer? t => t
          t = 0 => '$
          t = 2 => '_$_$
          t = 5 => $NRTaddForm
          u:= $NRTdeltaList.($NRTdeltaLength+5-t)
          first u = "%domain" => second u
          error "bad $NRTdeltaList entry"
        first t in '(Mapping Union Record _:) =>
           [first t,:[replaceSlotTypes(x) for x in rest t]]
        t

-----------------------------SLOT1 DATABASE------------------------------------

updateSlot1DataBase [name,info] == HPUT($Slot1DataBase,name,info)

NRTputInLocalReferences bod ==
  NRTputInHead bod

NRTputInHead bod ==
  atom bod => bod
  bod is ['SPADCALL,:args,fn] =>
    NRTputInTail rest bod --NOTE: args = COPY of rest bod
    -- The following test allows function-returning expressions
    fn is [elt,dom,ind] and dom ~='$ and elt in '(getShellEntry ELT QREFELT CONST) =>
      k := NRTassocIndex dom => lastNode(bod).first := ['%vref,'_$,k]
      nil
    NRTputInHead fn
    bod
  bod is ["COND",:clauses] =>
    for cc in clauses repeat NRTputInTail cc
    bod
  bod is ["QUOTE",:.] => bod
  bod is ["CLOSEDFN",:.] => bod
  NRTputInHead first bod
  NRTputInTail rest bod
  bod

NRTputInTail x ==
  for y in tails x repeat
    atom (u := first y) =>
      u='$ or LASSOC(u,$devaluateList) => nil
      k:= NRTassocIndex u =>
        atom u => y.first := ['%vref,'_$,k]
        -- u atomic means that the slot will always contain a vector
        y.first := ['SPADCHECKELT,'_$,k]
      --this reference must check that slot is a vector
      nil
    NRTputInHead u
  x



