-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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
--     - Neither the name of The Numerical ALgorithms Group Ltd. nor the
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


)package "BOOT"

------- from info.boot -----------

-- modemap is of the form : ((op (targ arg1 arg2 ... argn)) pred (elt $ n))

--------------------> NEW DEFINITION (see modemap.boot.pamphlet)
evalAndSub(domainName,viewName,functorForm,form,$e) ==
  $lhsOfColon: local:= domainName
  isCategory form => [substNames(domainName,viewName,functorForm,form.(1)),$e]
  --next lines necessary-- see MPOLY for which $ is actual arg. --- RDJ 3/83
  if CONTAINED("$$",form) then $e:= put("$$","mode",get("$","mode",$e),$e)
  opAlist:= getOperationAlist(domainName,functorForm,form)
  substAlist:= substNames(domainName,viewName,functorForm,opAlist)
  [substAlist,$e]

--------------------> NEW DEFINITION (see modemap.boot.pamphlet)
substNames(domainName,viewName,functorForm,opalist) ==
  functorForm := SUBSTQ("$$","$", functorForm)
  nameForDollar :=
    isCategoryPackageName functorForm => CADR functorForm
    domainName
    
       -- following calls to SUBSTQ must copy to save RPLAC's in
       -- putInLocalDomainReferences
  [[:SUBSTQ("$","$$",SUBSTQ(nameForDollar,"$",modemapform)),
       [sel, viewName,if domainName = "$" then pos else
                                         CADAR modemapform]]
     for [:modemapform,[sel,"$",pos]] in
          EQSUBSTLIST(KDR functorForm,$FormalMapVariableList, opalist)]

--------------------> NEW DEFINITION (see modemap.boot.pamphlet)
addModemap1(op,mc,sig,pred,fn,e) ==
   --mc is the "mode of computation"; fn the "implementation"
  if mc='Rep then
--     if fn is [kind,'Rep,.] and
               -- save old sig for NRUNTIME
--       (kind = 'ELT or kind = 'CONST) then fn:=[kind,'Rep,sig]
     sig:= substitute("$",'Rep,sig)
  currentProplist:= getProplist(op,e) or nil
  newModemapList:=
    mkNewModemapList(mc,sig,pred,fn,LASSOC('modemap,currentProplist),e,nil)
  newProplist:= augProplist(currentProplist,'modemap,newModemapList)
  newProplist':= augProplist(newProplist,"FLUID",true)
  unErrorRef op
        --There may have been a warning about op having no value
  addBinding(op,newProplist',e)

--------------------> NEW DEFINITION (see modemap.boot.pamphlet)
addConstructorModemaps(name,form is [functorName,:.],e) ==
  $InteractiveMode: local:= nil
  e:= putDomainsInScope(name,e) --frame
  fn := GETL(functorName,"makeFunctionList")
  [funList,e]:= FUNCALL(fn,name,form,e)
  for [op,sig,opcode] in funList repeat
    if opcode is [sel,dc,n] and sel='ELT then
          nsig := substitute("$$$",name,sig)
          nsig := substitute('$,"$$$",substitute("$$",'$,nsig))
          opcode := [sel,dc,nsig]
    e:= addModemap(op,name,sig,true,opcode,e)
  e

------- from info.boot -----------

--------------------> NEW DEFINITION (see info.boot.pamphlet)
actOnInfo(u,$e) ==
  null u => $e
  u is ["PROGN",:l] => (for v in l repeat $e:= actOnInfo(v,$e); $e)
  $e:=
    put("$Information","special",Info:= [u,:get("$Information","special",$e)],$e
      )
  u is ["COND",:l] =>
      --there is nowhere %else that this sort of thing exists
    for [ante,:conseq] in l repeat
      if member(hasToInfo ante,Info) then for v in conseq repeat
        $e:= actOnInfo(v,$e)
    $e
  u is ["ATTRIBUTE",name,att] =>
    [vval,vmode,venv]:= GetValue name
    SAY("augmenting ",name,": ",u)
    key:= if CONTAINED("$",vmode) then "domain" else name
    cat:= ["CATEGORY",key,["ATTRIBUTE",att]]
    $e:= put(name,"value",[vval,mkJoin(cat,vmode),venv],$e)
      --there is nowhere %else that this sort of thing exists
  u is ["SIGNATURE",name,operator,modemap] =>
    implem:=
      (implem:=ASSOC([name,:modemap],get(operator,'modemap,$e))) =>
          CADADR implem
      name = "$" => ['ELT,name,-1]
      ['ELT,name,substitute('$,name,modemap)]
    $e:= addModemap(operator,name,modemap,true,implem,$e)
    [vval,vmode,venv]:= GetValue name
    SAY("augmenting ",name,": ",u)
    key:= if CONTAINED("$",vmode) then "domain" else name
    cat:= ["CATEGORY",key,["SIGNATURE",operator,modemap]]
    $e:= put(name,"value",[vval,mkJoin(cat,vmode),venv],$e)
  u is ["has",name,cat] =>
    [vval,vmode,venv]:= GetValue name
    cat=vmode => $e --stating the already known
    u:= compMakeCategoryObject(cat,$e) =>
         --we are adding information about a category
      [catvec,.,$e]:= u
      [ocatvec,.,$e]:= compMakeCategoryObject(vmode,$e)
      -- member(vmode,CAR catvec.4) =>
      --    JHD 82/08/08 01:40 This does not mean that we can ignore the
      --    extension, since this may not be compatible with the view we
      --    were passed
 
      --we are adding a principal descendant of what was already known
      --    $e:= augModemapsFromCategory(name,name,nil,catvec,$e)
      --    SAY("augmenting ",name,": ",cat)
      --    put(name, "value", (vval, cat, venv), $e)
      member(cat,first ocatvec.4) or
         ASSOC(cat,CADR ocatvec.4) is [.,"T",.] => $e
        --SAY("Category extension error:
        --cat shouldn't be a join
                      --what was being asserted is an ancestor of what was known
      if name="$"
        then $e:= augModemapsFromCategory(name,name,name,cat,$e)
        else
          viewName:=genDomainViewName(name,cat)
          genDomainView(viewName,name,cat,"HasCategory")
          if not MEMQ(viewName,$functorLocalParameters) then
             $functorLocalParameters:=[:$functorLocalParameters,viewName]
      SAY("augmenting ",name,": ",cat)
      $e:= put(name,"value",[vval,mkJoin(cat,vmode),venv],$e)
    SAY("extension of ",vval," to ",cat," ignored")
    $e
  systemError '"knownInfo"

------- from nruncomp.boot -----------

--------------------> NEW DEFINITION (see nruncomp.boot.pamphlet)
genDeltaEntry opMmPair ==
--called from compApplyModemap
--$NRTdeltaLength=0.. always equals length of $NRTdeltaList
  [.,[odc,:.],.] := opMmPair
  --opModemapPair := SUBLIS($LocalDomainAlist,opMmPair)
  [op,[dc,:sig],[.,cform:=[eltOrConst,.,nsig]]] := opMmPair
  if $profileCompiler = true then profileRecord(dc,op,sig)
  eltOrConst = 'XLAM => cform
  if eltOrConst = 'Subsumed then eltOrConst := 'ELT
  if atom dc then
    dc = "$" => nsig := sig
    if NUMBERP nsig then nsig := substitute('$,dc,substitute("$$","$",sig))
    -- following hack needed to invert Rep to $ substitution
--  if odc = 'Rep and cform is [.,.,osig] then sig:=osig
  newimp := optDeltaEntry(op,nsig,dc,eltOrConst) => newimp
  setDifference(listOfBoundVars dc,$functorLocalParameters) ^= [] =>
    ['applyFun,['compiledLookupCheck,MKQ op,
         mkList consSig(nsig,dc),consDomainForm(dc,nil)]]
  odc := dc
  if null atom dc then dc := substitute("$$",'$,dc)
 --   sig := substitute('$,dc,sig)
 --   cform := substitute('$,dc,cform)
  opModemapPair :=
    [op,[dc,:[genDeltaSig x for x in nsig]],["T",cform]] -- force pred to T
  if null NRTassocIndex dc and dc ^= $NRTaddForm and
    (member(dc,$functorLocalParameters) or null atom dc) then
    --create "domain" entry to $NRTdeltaList
      $NRTdeltaList:= [['domain,NRTaddInner dc,:dc],:$NRTdeltaList]
      saveNRTdeltaListComp:= $NRTdeltaListComp:=[nil,:$NRTdeltaListComp]
      $NRTdeltaLength := $NRTdeltaLength+1
      compEntry:= (compOrCroak(odc,$EmptyMode,$e)).expr
--      dc
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

--------------------> NEW DEFINITION (see nruncomp.boot.pamphlet)
NRTencode(x,y) == encode(x,y,true) where encode(x,compForm,firstTime) ==
  --converts a domain form to a lazy domain form; everything other than 
  --the operation name should be assigned a slot
  null firstTime and (k:= NRTassocIndex x) => k
  VECP x => systemErrorHere '"NRTencode"
  PAIRP x =>
    QCAR x='Record or x is ['Union,['_:,a,b],:.] =>
      [QCAR x,:[['_:,a,encode(b,c,false)]
        for [.,a,b] in QCDR x for [.,=a,c] in CDR compForm]]
    constructor? QCAR x or MEMQ(QCAR x,'(Union Mapping)) =>
      [QCAR x,:[encode(y,z,false) for y in QCDR x for z in CDR compForm]]
    ['NRTEVAL,NRTreplaceAllLocalReferences COPY_-TREE lispize compForm]
  MEMQ(x,$formalArgList) =>
    v := $FormalMapVariableList.(POSN1(x,$formalArgList))
    firstTime => ["local",v]
    v
  x = '$ => x
  x = "$$" => x
  ['QUOTE,x]

--------------------> NEW DEFINITION (see nruncomp.boot.pamphlet)
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

--------------------> NEW DEFINITION (see nruncomp.boot.pamphlet)
NRTassignCapsuleFunctionSlot(op,sig) ==
--called from compDefineCapsuleFunction
  opSig := [op,sig]
  [.,.,implementation] := NRTisExported? opSig or return nil
    --if opSig is not exported, it is local and need not be assigned
  if $insideCategoryPackageIfTrue then
      sig := substitute('$,CADR($functorForm),sig)
  sig := [genDeltaSig x for x in sig]
  opModemapPair := [op,['_$,:sig],["T",implementation]]
  POSN1(opModemapPair,$NRTdeltaList) => nil   --already there
  $NRTdeltaList:= [opModemapPair,:$NRTdeltaList]
  $NRTdeltaListComp := [nil,:$NRTdeltaListComp]
  $NRTdeltaLength := $NRTdeltaLength+1

--------------------> NEW DEFINITION (see nruncomp.boot.pamphlet)
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

------- from compiler.boot -----------

--------------------> NEW DEFINITION (see compiler.boot.pamphlet)
getFormModemaps(form is [op,:argl],e) ==
  op is ["elt",domain,op1] =>
    [x for x in getFormModemaps([op1,:argl],e) | x is [[ =domain,:.],:.]]
  null atom op => nil
  modemapList:= get(op,"modemap",e)
  if $insideCategoryPackageIfTrue then
    modemapList := [x for x in modemapList | x is [[dom,:.],:.] and dom ^= '$]
  if op="elt"
     then modemapList:= eltModemapFilter(LAST argl,modemapList,e) or return nil
     else
      if op="setelt" then modemapList:=
        seteltModemapFilter(CADR argl,modemapList,e) or return nil
  nargs:= #argl
  finalModemapList:= [mm for (mm:= [[.,.,:sig],:.]) in modemapList | #sig=nargs]
  modemapList and null finalModemapList =>
    stackMessage ["no modemap for","%b",op,"%d","with ",nargs," arguments"]
  finalModemapList

------- from functor.boot -----------

--------------------> NEW DEFINITION (see functor.boot.pamphlet)
LookUpSigSlots(sig,siglist) ==
--+ must kill any implementations below of the form (ELT $ NIL)
  if $insideCategoryPackageIfTrue then
           sig := substitute('$,CADR($functorForm),sig)
  siglist := $lisplibOperationAlist
  REMDUP [implem for u in siglist | SigSlotsMatch(sig,first u,implem:=CADDR u)
              and KADDR implem]

