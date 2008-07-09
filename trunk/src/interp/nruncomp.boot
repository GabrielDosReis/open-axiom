-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2008, Gabriel Dos Reis.
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


import c_-util
import simpbool
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
$Slot1DataBase := MAKE_-HASHTABLE "ID"

++
$NRTdeltaList := []
$NRTdeltaLength := 0

++
$NRTaddForm := nil


-----------------------------NEW buildFunctor CODE-----------------------------
NRTaddDeltaCode() ==
--NOTES: This function is called from NRTbuildFunctor to initially
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
  for i in $NRTbase.. for item in REVERSE $NRTdeltaList
    for compItem in REVERSE $NRTdeltaListComp
      |null (s:=kvec.i) repeat
        $template.i:= deltaTran(item,compItem)
  $template.5 :=
    $NRTaddForm =>
      $NRTaddForm is ["%Comma",:y] => NREVERSE y
      NRTencode($NRTaddForm,$addForm)
    nil

deltaTran(item,compItem) ==
  item is ['domain,lhs,:.] => NRTencode(lhs,compItem)
  --NOTE: all items but signatures are wrapped with domain forms
  [op,:modemap] := item
  [dcSig,[.,[kind,:.]]] := modemap
  [dc,:sig] := dcSig
  sig := MSUBST('$,dc,substitute("$$",'$,sig))
  dcCode :=
    dc = '$ => 0
    dc = $NRTaddForm => 5
    NRTassocIndex dc or keyedSystemError("S2NR0004",[dc])
  formalSig:= SUBLISLIS($FormalMapVariableList,$formalArgList,sig)
  kindFlag:= (kind = 'CONST => 'CONST; nil)
  newSig := [NRTassocIndex x or x for x in formalSig]
  [newSig,dcCode,op,:kindFlag]

NRTreplaceAllLocalReferences(form) ==
  $devaluateList :local := []
  NRTputInLocalReferences form

NRTencode(x,y) == encode(x,y,true) where encode(x,compForm,firstTime) ==
  --converts a domain form to a lazy domain form; everything other than 
  --the operation name should be assigned a slot
  not firstTime and (k:= NRTassocIndex x) => k
  VECP x => systemErrorHere '"NRTencode"
  PAIRP x =>
    op := first x
    op = "Record" or x is ['Union,['_:,a,b],:.] =>
      [op,:[['_:,a,encode(b,c,false)]
        for [.,a,b] in rest x for [.,=a,c] in rest compForm]]
    (x' := isQuasiquote x) =>
      quasiquote encode(x',isQuasiquote compForm,false)
    IDENTP op and (constructor? op or MEMQ(op,'(Union Mapping Enumeration))) =>
      [op,:[encode(y,z,false) for y in rest x for z in rest compForm]]
    ["NRTEVAL",NRTreplaceAllLocalReferences COPY_-TREE lispize compForm]
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
    MEMQ(KAR u,'(Union Record)) => listOfBoundVars u
    [form]
  atom form => []
  CAR form = 'QUOTE => []
  EQ(CAR form,":") => listOfBoundVars third form
  -- We don't want to pick up the tag, only the domain
  "union"/[listOfBoundVars x for x in rest form]

optDeltaEntry(op,sig,dc,eltOrConst) ==
  $killOptimizeIfTrue = true => nil
  ndc :=
    dc = '$ => $functorForm
    atom dc and (dcval := get(dc,'value,$e)) => dcval.expr
    dc
  sig := MSUBST(ndc,dc,sig)
  not MEMQ(KAR ndc,$optimizableConstructorNames) => nil
  dcval := optCallEval ndc
  -- MSUBST guarantees to use EQUAL testing
  sig := MSUBST(devaluate dcval, ndc, sig)
  if rest ndc then
     for new in rest devaluate dcval for old in rest ndc repeat
       sig := MSUBST(new,old,sig)
     -- optCallEval sends (List X) to (LIst (Integer)) etc,
     -- so we should make the same transformation
  fn := compiledLookup(op,sig,dcval)
  if null fn then
    -- following code is to handle selectors like first, rest
     nsig := [quoteSelector tt for tt in sig] where
       quoteSelector(x) ==
         not(IDENTP x) => x
         get(x,'value,$e) => x
         x='$ => x
         MKQ x
     fn := compiledLookup(op,nsig,dcval)
     if null fn then return nil
  eltOrConst="CONST" => ['XLAM,'ignore,MKQ SPADCALL fn]
  GETL(compileTimeBindingOf first fn,'SPADreplace)

genDeltaEntry opMmPair ==
--called from compApplyModemap
--$NRTdeltaLength=0.. always equals length of $NRTdeltaList
  [.,[odc,:.],.] := opMmPair
  [op,[dc,:sig],[.,cform:=[eltOrConst,.,nsig]]] := opMmPair
  if $profileCompiler = true then profileRecord(dc,op,sig)
  eltOrConst = 'XLAM => cform
  if eltOrConst = 'Subsumed then eltOrConst := 'ELT
  if atom dc then
    dc = "$" => nsig := sig
    if NUMBERP nsig then nsig := MSUBST('$,dc,substitute("$$","$",sig))
  newimp := optDeltaEntry(op,nsig,dc,eltOrConst) => newimp
  setDifference(listOfBoundVars dc,$functorLocalParameters) ^= [] =>
    ['applyFun,['compiledLookupCheck,MKQ op,
         mkList consSig(nsig,dc),consDomainForm(dc,nil)]]
  odc := dc
  if null atom dc then dc := substitute("$$",'$,dc)
  opModemapPair :=
    [op,[dc,:[NRTgetLocalIndex x for x in nsig]],["T",cform]] -- force pred to T
  if null NRTassocIndex dc and dc ^= $NRTaddForm and
    (member(dc,$functorLocalParameters) or null atom dc) then
    --create "domain" entry to $NRTdeltaList
      $NRTdeltaList:= [['domain,NRTaddInner dc,:dc],:$NRTdeltaList]
      saveNRTdeltaListComp:= $NRTdeltaListComp:=[nil,:$NRTdeltaListComp]
      $NRTdeltaLength := $NRTdeltaLength+1
      compEntry:= (compOrCroak(odc,$EmptyMode,$e)).expr
      RPLACA(saveNRTdeltaListComp,compEntry)
  u :=
    [eltOrConst,'$,$NRTbase+$NRTdeltaLength-index] where index() ==
      (n:= POSN1(opModemapPair,$NRTdeltaList)) => n + 1
        --n + 1 since $NRTdeltaLength is 1 too large
      $NRTdeltaList:= [opModemapPair,:$NRTdeltaList]
      $NRTdeltaListComp:=[nil,:$NRTdeltaListComp]
      $NRTdeltaLength := $NRTdeltaLength+1
      0
  u

genDeltaSpecialSig x ==
  x is [":",y,z] => [":",y,NRTgetLocalIndex z]
  NRTgetLocalIndex x

++ Return the slot number (within the template vector of the functor
++ being compiled) of the domain or value referenced by the form `x'.
++ Otherwise, return nil this is the first time `x' is referenced, or
++ if `x' designates neither a domain nor a value (e.g. a modemap).
NRTassocIndex: %Form -> %Maybe %Short
NRTassocIndex x == --returns index of "domain" entry x in al
  NULL x => x
  x = $NRTaddForm => 5
  k := or/[i for i in 1.. for y in $NRTdeltaList
            | y.0 = 'domain and y.1 = x and ($found := y)] =>
    $NRTbase + $NRTdeltaLength - k
  nil

NRTgetLocalIndex: %Form -> %Short
NRTgetLocalIndex item ==
  k := NRTassocIndex item => k
  item = $NRTaddForm => 5
  item = "$" => 0
  item = "$$" => 2
  value:=
    atom item =>
      MEMQ(item,$formalArgList) => item
    nil
  atom item and null value =>  --give slots to atoms
    $NRTdeltaList:= [['domain,NRTaddInner item,:value],:$NRTdeltaList]
    $NRTdeltaListComp:=[item,:$NRTdeltaListComp]
    index := $NRTbase + $NRTdeltaLength      -- slot number to return
    $NRTdeltaLength := $NRTdeltaLength+1
    index
  -- when assigning slot to flag values, we don't really want to
  -- compile them.  Rather, we want to record them as if they were atoms.
  flag := isQuasiquote item
  $NRTdeltaList:= [['domain, NRTaddInner item,:value], :$NRTdeltaList]
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
--    item
  RPLACA(saveNRTdeltaListComp,compEntry)
  saveIndex

NRTgetAddForm domain ==
  u := HGET($Slot1DataBase,first domain) =>
    EQSUBSTLIST(rest domain,$FormalMapVariableList,first u)
  systemErrorHere '"NRTgetAddForm"

NRTassignCapsuleFunctionSlot(op,sig) ==
--called from compDefineCapsuleFunction
  opSig := [op,sig]
  [.,.,implementation] := NRTisExported? opSig or return nil
    --if opSig is not exported, it is local and need not be assigned
  if $insideCategoryPackageIfTrue then
      sig := substitute('$,CADR($functorForm),sig)
  sig := [NRTgetLocalIndex x for x in sig]
  opModemapPair := [op,['_$,:sig],["T",implementation]]
  POSN1(opModemapPair,$NRTdeltaList) => nil   --already there
  $NRTdeltaList:= [opModemapPair,:$NRTdeltaList]
  $NRTdeltaListComp := [nil,:$NRTdeltaListComp]
  $NRTdeltaLength := $NRTdeltaLength+1

NRTisExported? opSig ==
  or/[u for u in $domainShell.1 | u.0 = opSig]

consOpSig(op,sig,dc) ==
  if null atom op then
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
         :[['LIST,MKQ '_:,MKQ tag,consDomainName(dom,dc)]
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

buildFunctor($definition is [name,:args],sig,code,$locals,$e) ==
--PARAMETERS
--  $definition: constructor form, e.g. (SquareMatrix 10 (RationalNumber))
--  sig: signature of constructor form
--  code: result of "doIt", converting body of capsule to CodeDefine forms, e.g.
--       (PROGN (LET Rep ...)
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

  --pp '"=================="
  --for item in $NRTdeltaList repeat pp item

--LOCAL BOUND FLUID VARIABLES:
  $GENNO: local:= 0     --bound in compDefineFunctor1, then as parameter here
  $catvecList: local := nil   --list of vectors v1..vn for each view
  $hasCategoryAlist: local := nil  --list of GENSYMs bound to (HasCategory ..) items
  $catNames: local := nil      --list of names n1..nn for each view
  $maximalViews: local := nil  --list of maximal categories for domain (???)
  $catsig: local := nil        --target category (used in ProcessCond)
  $SetFunctions: local := nil  --copy of p view with preds telling when fnct defined
  $MissingFunctionInfo: local := nil --now useless
     --vector marking which functions are assigned
  $ConstantAssignments: local := nil --code for creation of constants
  $epilogue: local := nil     --code to set slot 5, things to be done last
  $HackSlot4: local := nil  --Invention of JHD 13/July/86-set in InvestigateConditions
  $extraParms:local := nil  --Set in DomainSubstitutionFunction, used in setVector12
  $devaluateList: local := nil --Bound to ((#1 . dv$1)..) where &1 := devaluate #1 later
  $devaluateList:= [[arg,:b] for arg in args for b in $ModeVariableList]
  $supplementaries: local := nil
   --set in InvestigateConditions to represent any additional
   --category membership tests that may be needed(see buildFunctor for details)
------------------------
  $maximalViews: local := nil
  oldtime:= TEMPUS_-FUGIT()
  [$catsig,:argsig]:= sig
  catvecListMaker:=REMDUP
    [(comp($catsig,$EmptyMode,$e)).expr,
      :[compCategories first u for u in CADR $domainShell.4]]
  condCats:= InvestigateConditions [$catsig,:rest catvecListMaker]
  -- a list, one %for each element of catvecListMaker
  -- indicating under what conditions this
  -- category should be present.  true => always
  makeCatvecCode:= first catvecListMaker
  emptyVector := VECTOR()
  domainShell := newShell ($NRTbase + $NRTdeltaLength)
  for i in 0..4 repeat domainShell.i := $domainShell.i
    --we will clobber elements; copy since $domainShell may be a cached vector
  $template :=
    $NRTvec = true => newShell ($NRTbase + $NRTdeltaLength)
    nil
  $catvecList:= [domainShell,:[emptyVector for u in CADR domainShell.4]]
  $catNames := ['$] -- for DescendCode -- to be changed below for slot 4
  $maximalViews:= nil
  $SetFunctions:= newShell SIZE domainShell
  $MissingFunctionInfo:= newShell SIZE domainShell
  $catNames:= ['$,:[GENVAR() for u in rest catvecListMaker]]
  domname:='dv_$

-->  Do this now to create predicate vector; then DescendCode can refer
-->  to predicate vector if it can
  [$uncondAlist,:$condAlist] :=    --bound in compDefineFunctor1
      NRTsetVector4Part1($catNames,catvecListMaker,condCats)
  [$NRTslot1PredicateList,predBitVectorCode1,:predBitVectorCode2] :=
      makePredicateBitVector [:ASSOCRIGHT $condAlist,:$NRTslot1PredicateList]

  storeOperationCode:= DescendCode(code,true,nil,first $catNames)
  outsideFunctionCode:= NRTaddDeltaCode()
  storeOperationCode:= NRTputInLocalReferences storeOperationCode
  if $NRTvec = true then
    NRTdescendCodeTran(storeOperationCode,nil) --side effects storeOperationCode
  codePart2:=
    $NRTvec = true =>
      argStuffCode :=
        [[$setelt,'$,i,v] for i in $NRTbase.. for v in $FormalMapVariableList
          for arg in rest $definition]
      if MEMQ($NRTaddForm,$locals) then
         addargname := $FormalMapVariableList.(POSN1($NRTaddForm,$locals))
         argStuffCode := [[$setelt,'$,5,addargname],:argStuffCode]
      [['stuffDomainSlots,'$],:argStuffCode,
         :predBitVectorCode2,storeOperationCode]
    [:outsideFunctionCode,storeOperationCode]

  $CheckVectorList := NRTcheckVector domainShell
--CODE: part 1
  codePart1:= [:devaluateCode,:domainFormCode,createDomainCode,
                createViewCode,setVector0Code, slot3Code,:slamCode] where
    devaluateCode:= [['LET,b,['devaluate,a]] for [a,:b] in $devaluateList]
    domainFormCode := [['LET,a,b] for [a,:b] in NREVERSE $NRTdomainFormList]
      --$NRTdomainFormList is unused now
    createDomainCode:=
      ['LET,domname,['LIST,MKQ CAR $definition,:ASSOCRIGHT $devaluateList]]
    createViewCode:= ['LET,'$,["newShell", $NRTbase + $NRTdeltaLength]]
    setVector0Code:=[$setelt,'$,0,'dv_$]
    slot3Code := ["setShellEntry",'$,3,['LET,'pv_$,predBitVectorCode1]]
    slamCode:=
      isCategoryPackageName opOf $definition => nil
      [NRTaddToSlam($definition,'$)]

--CODE: part 3
  $ConstantAssignments :=
      [NRTputInLocalReferences code for code in $ConstantAssignments]
  codePart3:= [:constantCode1,
                :constantCode2,:epilogue] where
    constantCode1:=
      name='Integer => $ConstantAssignments
      nil
                      -- The above line is needed to get the recursion
                      -- Integer => FontTable => NonNegativeInteger  => Integer
                      -- right.  Otherwise NNI has 'unset' for 0 and 1
--  setVector4c:= setVector4part3($catNames,$catvecList)
                      -- In particular, setVector4part3 and setVector5,
                      -- which generate calls to local domain-instantiators,
                      -- must come after operations are set in the vector.
                      -- The symptoms of getting this wrong are that
                      -- operations are not set which should be
    constantCode2:= --matches previous test on Integer
      name='Integer => nil
      $ConstantAssignments
    epilogue:= $epilogue
  ans :=
    ['PROGN,:optFunctorPROGN [:codePart1,:codePart2,:codePart3], '$]
  $getDomainCode:= nil
    --if we didn't kill this, DEFINE would insert it in the wrong place
  ans:= minimalise ans
  SAY ['"time taken in buildFunctor: ",TEMPUS_-FUGIT()-oldtime]
  ans

NRTcheckVector domainShell ==
--RETURNS: an alist (((op,sig),:pred) ...) of missing functions
  alist := nil
  for i in $NRTbase..MAXINDEX domainShell repeat
--Vector elements can be one of
-- (a) T           -- item was marked
-- (b) NIL         -- item is a domain; will be filled in by setVector4part3
-- (c) categoryForm-- it was a domain view; now irrelevant
-- (d) op-signature-- store missing function info in $CheckVectorList
    v:= domainShell.i
    v=true => nil  --item is marked; ignore
    v=nil => nil  --a domain, which setVector4part3 will fill in
    atom v => systemErrorHere '"CheckVector"
    atom first v => nil  --category form; ignore
    assoc(first v,alist) => nil
    alist:=
      [[first v,:$SetFunctions.i],:alist]
  alist

-- Obsolete once we have moved to JHD's world
NRTvectorCopy(cacheName,domName,deltaLength) == GETREFV ($NRTbase + deltaLength)

mkDomainCatName id == INTERN STRCONC(id,";CAT")

NRTsetVector4(siglist,formlist,condlist) ==
  $uncondList: local := nil
  $condList: local := nil
  $count: local := 0
  for sig in reverse siglist for form in reverse formlist
         for cond in reverse condlist repeat
                  NRTsetVector4a(sig,form,cond)

  $lisplibCategoriesExtended:= [$uncondList,:$condList]
  code := ['mapConsDB,MKQ REVERSE REMDUP $uncondList]
  if $condList then
    localVariable := GENSYM()
    code := [['LET,localVariable,code]]
    for [pred,list] in $condList repeat
      code :=
        [['COND,[pred,['LET,localVariable,
          ['mergeAppend,['mapConsDB,MKQ list],localVariable]]]],
            :code]
    code := ['PROGN,:NREVERSE [['NREVERSE,localVariable],:code]]
  g := GENSYM()
  [$setelt,'$,4,['PROG2,['LET,g,code],
    ['VECTOR,['catList2catPackageList,g],g]]]

NRTsetVector4Part1(siglist,formlist,condlist) ==
  $uncondList: local := nil
  $condList: local := nil
  $count: local := 0
  for sig in reverse siglist for form in reverse formlist
         for cond in reverse condlist repeat
                  NRTsetVector4a(sig,form,cond)
  reducedUncondlist := REMDUP $uncondList
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
      RPLACD(u,[x,:rest u])
  alist

NRTsetVector4Part2(uncondList,condList) ==
  $lisplibCategoriesExtended:= [uncondList,:condList]
  code := ['mapConsDB,MKQ REVERSE REMDUP uncondList]
  if condList then
    localVariable := GENSYM()
    code := [['LET,localVariable,code]]
    for [pred,list] in condList repeat
      code :=
        [['COND,[predicateBitRef SUBLIS($pairlis,pred),['LET,localVariable,
          ['mergeAppend,['mapConsDB,MKQ list],localVariable]]]],
            :code]
    code := ['PROGN,:NREVERSE [['NREVERSE,localVariable],:code]]
  g := GENSYM()
  [$setelt,'$,4,['PROG2,['LET,g,code],
    ['VECTOR,['catList2catPackageList,g],g]]]

mergeAppend(l1,l2) ==
  ATOM l1 => l2
  member(QCAR l1,l2) => mergeAppend(QCDR l1, l2)
  CONS(QCAR l1, mergeAppend(QCDR l1, l2))

catList2catPackageList u ==
--converts ((Set) (Module R) ...) to ((Set& $) (Module& $ R)...)
  [fn x for x in u] where
    fn [op,:argl] ==
      newOp := INTERN(STRCONC(PNAME op,"&"))
      addConsDB [newOp,"$",:argl]

NRTsetVector4a(sig,form,cond) ==
  sig = '$ =>
     domainList :=
       [optimize COPY KAR comp(d,$EmptyMode,$e) or d for d in $domainShell.4.0]
     $uncondList := APPEND(domainList,$uncondList)
     if isCategoryForm(form,$e) then $uncondList := [form,:$uncondList]
     $uncondList
  evalform := eval mkEvalableCategoryForm form
  cond = true => $uncondList := [form,:APPEND(evalform.4.0,$uncondList)]
  $condList := [[cond,[form,:evalform.4.0]],:$condList]

NRTmakeSlot1 domainShell ==
  opDirectName := INTERN STRCONC(PNAME first $definition,'";opDirect")
  fun := '(function lookupInCompactTable)
  ["setShellEntry", '$,1, ['LIST,fun,'$,opDirectName]]

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
  [[a,:b] for a in argl for b in $FormalMapVariableList]

slot1Filter opList ==
--include only those ops which are defined within the capsule
  [u for x in opList | u := fn x] where
    fn [op,:l] ==
      u := [entry for entry in l | INTEGERP CADR entry] => [op,:u]
      nil

NRToptimizeHas u ==
--u is a list ((pred cond)...) -- see optFunctorBody
--produces an alist: (((HasCategory a b) . GENSYM)...)
  u is [a,:b] =>
    a='HasCategory => LASSOC(u,$hasCategoryAlist) or
      $hasCategoryAlist := [[u,:(y:=GENSYM())],:$hasCategoryAlist]
      y
    a='has => NRToptimizeHas ['HasCategory,first b,MKQ first rest b]
    a = 'QUOTE => u
    [NRToptimizeHas a,:NRToptimizeHas b]
  u

NRTaddToSlam([name,:argnames],shell) ==
  $mutableDomain => return nil
  null argnames => addToConstructorCache(name,nil,shell)
  args:= ['LIST,:ASSOCRIGHT $devaluateList]
  addToConstructorCache(name,args,shell)

changeDirectoryInSlot1() ==  --called by NRTbuildFunctor
  --3 cases:
  --  if called inside NRTbuildFunctor, $NRTdeltaLength gives different locs
  --  otherwise called from compFunctorBody (all lookups are forwarded):
  --    $NRTdeltaList = nil  ===> all slot numbers become nil
  $lisplibOperationAlist := [sigloc entry for entry in $domainShell.1] where
    sigloc [opsig,pred,fnsel] ==
        if pred ^= 'T then
          pred := simpBool pred
          $NRTslot1PredicateList := insert(pred,$NRTslot1PredicateList)
        fnsel is [op,a,:.] and (op = 'ELT or op = 'CONST) =>
          if $insideCategoryPackageIfTrue then
              opsig := substitute('$,CADR($functorForm),opsig)
          [opsig,pred,[op,a,vectorLocation(first opsig,CADR opsig)]]
        [opsig,pred,fnsel]
  sortedOplist := listSort(function GLESSEQP,
                           COPY_-LIST $lisplibOperationAlist,function CADR)
  $lastPred :local := nil
  $newEnv :local := $e
  $domainShell.1 := [fn entry for entry in sortedOplist] where
    fn [[op,sig],pred,fnsel] ==
       if $lastPred ^= pred then
            $newEnv := deepChaseInferences(pred,$e)
            $lastPred := pred
       newfnsel :=
         fnsel is ['Subsumed,op1,sig1] =>
           ['Subsumed,op1,genSlotSig(sig1,"T",$newEnv)]
         fnsel
       [[op, genSlotSig(sig,pred,$newEnv)] ,pred,newfnsel]

genSlotSig(sig,pred,$e) ==
   [NRTgetLocalIndex t for t in sig]

deepChaseInferences(pred,$e) ==
    pred is ['AND,:preds] or pred is ['and,:preds] =>
        for p in preds repeat $e := deepChaseInferences(p,$e)
        $e
    pred is ['OR,pred1,:.] or pred is ['or,pred1,:.] =>
        deepChaseInferences(pred1,$e)
    pred is 'T or pred is ['NOT,:.] or pred is ['not,:.] => $e
    chaseInferences(pred,$e)

vectorLocation(op,sig) ==
  u := or/[i for i in 1.. for u in $NRTdeltaList
        | u is [=op,[='$,: xsig],:.] and sig=NRTsubstDelta(xsig) ]
  u => $NRTdeltaLength - u + $NRTbase 
  nil    -- this signals that calls should be forwarded

NRTsubstDelta(initSig) ==
  sig := [replaceSlotTypes s for s in initSig] where
     replaceSlotTypes(t) ==
        atom t =>
          not INTEGERP t => t
          t = 0 => '$
          t = 2 => '_$_$
          t = 5 => $NRTaddForm
          u:= $NRTdeltaList.($NRTdeltaLength+5-t)
          CAR u = 'domain => CADR u
          error "bad $NRTdeltaList entry"
        MEMQ(CAR t,'(Mapping Union Record _:)) =>
           [CAR t,:[replaceSlotTypes(x) for x in rest t]]
        t
-----------------------------SLOT1 DATABASE------------------------------------

updateSlot1DataBase [name,info] == HPUT($Slot1DataBase,name,info)

NRTputInLocalReferences bod ==
  $elt: local := "getShellEntry"
  NRTputInHead bod

NRTputInHead bod ==
  atom bod => bod
  bod is ['SPADCALL,:args,fn] =>
    NRTputInTail rest bod --NOTE: args = COPY of rest bod
    -- The following test allows function-returning expressions
    fn is [elt,dom,ind] and not (dom='$) and MEMQ(elt,'(getShellEntry ELT QREFELT CONST)) =>
      k:= NRTassocIndex dom => RPLACA(LASTNODE bod,[$elt,'_$,k])
      nil
    NRTputInHead fn
    bod
  bod is ["COND",:clauses] =>
    for cc in clauses repeat NRTputInTail cc
    bod
  bod is ["QUOTE",:.] => bod
  bod is ["CLOSEDFN",:.] => bod
  bod is ["SPADCONST",dom,ind] =>
    RPLACA(bod,$elt)
    dom = '_$ => nil
    k:= NRTassocIndex dom =>
      RPLACA(LASTNODE bod,[$elt,'_$,k])
      bod
    keyedSystemError("S2GE0016",['"NRTputInHead",
       '"unexpected SPADCONST form"])
  NRTputInHead first bod
  NRTputInTail rest bod
  bod

NRTputInTail x ==
  for y in tails x repeat
    atom (u := first y) =>
      EQ(u,'$) or LASSOC(u,$devaluateList) => nil
      k:= NRTassocIndex u =>
        atom u => RPLACA(y,[$elt,'_$,k])
        -- u atomic means that the slot will always contain a vector
        RPLACA(y,['SPADCHECKELT,'_$,k])
      --this reference must check that slot is a vector
      nil
    NRTputInHead u
  x



