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


import '"nlib"
import '"c-util"
)package "BOOT"

++
$functionLocations := []

--% Standard Library Creation Functions
 
readLib(fn,ft) == readLib1(fn,ft,"*")
 
readLib1(fn,ft,fm) ==
  -- see if it exists first
  p := pathname [fn,ft,fm]
  readLibPathFast p
 
readLibPathFast p ==
  -- assumes 1) p is a valid pathname
  --         2) file has already been checked for existence
  RDEFIOSTREAM([['FILE,:p], '(MODE . INPUT)],false)
 
writeLib(fn,ft) == writeLib1(fn,ft,"*")
 
writeLib1(fn,ft,fm) == RDEFIOSTREAM [['FILE,fn,ft,fm],'(MODE . OUTPUT)]
 
putFileProperty(fn,ft,id,val) ==
  fnStream:= writeLib1(fn,ft,"*")
  val:= rwrite( id,val,fnStream)
  RSHUT fnStream
  val
 
lisplibWrite(prop,val,filename) ==
  -- this may someday not write NIL keys, but it will now
  if $LISPLIB then
     rwrite128(prop,val,filename)
 
rwrite128(key,value,stream) ==
  rwrite(key,value,stream)
 
evalAndRwriteLispForm(key,form) ==
  eval form
  rwriteLispForm(key,form)
 
rwriteLispForm(key,form) ==
  if $LISPLIB then
    rwrite( key,form,$libFile)
    LAM_,FILEACTQ(key,form)
 
getLisplib(name,id) ==
  -- this version does cache the returned value
  getFileProperty(name,$spadLibFT,id,true)
 
getLisplibNoCache(name,id) ==
  -- this version does not cache the returned value
  getFileProperty(name,$spadLibFT,id,false)
 
getFileProperty(fn,ft,id,cache) ==
  fn in '(DOMAIN SUBDOM MODE) => nil
  p := pathname [fn,ft,'"*"]
  cache => hasFileProperty(p,id,fn)
  hasFilePropertyNoCache(p,id,fn)
 
hasFilePropertyNoCache(p,id,abbrev) ==
  -- it is assumed that the file exists and is a proper pathname
  -- startTimingProcess 'diskread
  fnStream:= readLibPathFast p
  NULL fnStream => NIL
  -- str:= object2String id
  val:= rread(id,fnStream, nil)
  RSHUT fnStream
  -- stopTimingProcess 'diskread
  val
 
--% Uninstantiating
 
unInstantiate(clist) ==
  for c in clist repeat
    clearConstructorCache(c)
  killNestedInstantiations(clist)
 
killNestedInstantiations(deps) ==
  for key in HKEYS($ConstructorCache)
    repeat
      for [arg,count,:inst] in HGET($ConstructorCache,key) repeat
        isNestedInstantiation(inst.0,deps) =>
          HREMPROP($ConstructorCache,key,arg)
 
isNestedInstantiation(form,deps) ==
  form is [op,:argl] =>
    op in deps => true
    or/[isNestedInstantiation(x,deps) for x in argl]
  false
 
--% Loading
 
loadLibIfNotLoaded libName ==
  -- replaces old SpadCondLoad
  -- loads is library is not already loaded
  $PrintOnly = 'T => NIL
  GETL(libName,'LOADED) => NIL
  loadLib libName
 
loadLib cname ==
  startTimingProcess 'load
  fullLibName := GETDATABASE(cname,'OBJECT) or return nil
  systemdir? := isSystemDirectory(pathnameDirectory fullLibName)
  update? := $forceDatabaseUpdate or not systemdir? 
  not update? =>
     loadLibNoUpdate(cname, cname, fullLibName)
  kind := GETDATABASE(cname,'CONSTRUCTORKIND)
  if $printLoadMsgs then
    sayKeyedMsg("S2IL0002",[namestring fullLibName,kind,cname])
  LOAD(fullLibName)
  clearConstructorCache cname
  updateDatabase(cname,cname,systemdir?)
  installConstructor(cname,kind)
  u := GETDATABASE(cname, 'CONSTRUCTORMODEMAP)
  updateCategoryTable(cname,kind)
  coSig :=
      u =>
          [[.,:sig],:.] := u
          CONS(NIL,[categoryForm?(x) for x in CDR sig])
      NIL
  -- in following, add property value false or NIL to possibly clear
  -- old value
  if null CDR GETDATABASE(cname,'CONSTRUCTORFORM) then
      MAKEPROP(cname,'NILADIC,'T)
    else
      REMPROP(cname,'NILADIC)
  MAKEPROP(cname,'LOADED,fullLibName)
  if $InteractiveMode then $CategoryFrame := [[nil]]
  stopTimingProcess 'load
  'T

loadLibNoUpdate(cname, libName, fullLibName) ==
  kind := GETDATABASE(cname,'CONSTRUCTORKIND)
  if $printLoadMsgs then
    sayKeyedMsg("S2IL0002",[namestring fullLibName,kind,cname])
  if CATCH('VERSIONCHECK,LOAD(fullLibName)) = -1
    then 
      PRINC('"   wrong library version...recompile ")
      PRINC(fullLibName)
      TERPRI()
      TOPLEVEL()
    else
     clearConstructorCache cname
     installConstructor(cname,kind)
     MAKEPROP(cname,'LOADED,fullLibName)
     if $InteractiveMode then $CategoryFrame := [[nil]]
     stopTimingProcess 'load
  'T
 
loadIfNecessary u == loadLibIfNecessary(u,true)
 
loadIfNecessaryAndExists u == loadLibIfNecessary(u,nil)
 
loadLibIfNecessary(u,mustExist) ==
  u = '$EmptyMode => u
  null atom u => loadLibIfNecessary(first u,mustExist)
  value:=
    functionp(u) or macrop(u) => u
    GETL(u,'LOADED) => u
    loadLib u => u
  null $InteractiveMode and ((null (y:= getProplist(u,$CategoryFrame)))
    or (null LASSOC('isFunctor,y)) and (null LASSOC('isCategory,y))) =>
      y:= GETDATABASE(u,'CONSTRUCTORKIND) =>
         y = 'category =>
            updateCategoryFrameForCategory u
         updateCategoryFrameForConstructor u
      throwKeyedMsg("S2IL0005",[u])
  value
 
convertOpAlist2compilerInfo(opalist) ==
   "append"/[[formatSig(op,sig) for sig in siglist]
                for [op,:siglist] in opalist] where
      formatSig(op, [typelist, slot,:stuff]) ==
          pred := if stuff then first stuff else 'T
          impl := if CDR stuff then CADR stuff else 'ELT -- handles 'CONST
          [[op, typelist], pred, [impl, '$, slot]]
   
updateCategoryFrameForConstructor(constructor) ==
   opAlist := GETDATABASE(constructor, 'OPERATIONALIST)
   [[dc,:sig],[pred,impl]] := GETDATABASE(constructor, 'CONSTRUCTORMODEMAP) 
   $CategoryFrame := put(constructor,'isFunctor,
       convertOpAlist2compilerInfo(opAlist),
       addModemap(constructor, dc, sig, pred, impl,
           put(constructor, 'mode, ['Mapping,:sig], $CategoryFrame)))

updateCategoryFrameForCategory(category) ==
   [[dc,:sig],[pred,impl]] := GETDATABASE(category, 'CONSTRUCTORMODEMAP) 
   $CategoryFrame :=
     put(category, 'isCategory, 'T,
         addModemap(category, dc, sig, pred, impl, $CategoryFrame))

loadFunctor u ==
  null atom u => loadFunctor first u
  loadLibIfNotLoaded u
  u
 
makeConstructorsAutoLoad() ==
  for cnam in allConstructors() repeat
    REMPROP(cnam,'LOADED)
--    fn:=GETDATABASE(cnam,'ABBREVIATION)
    if GETDATABASE(cnam,'NILADIC)
     then PUT(cnam,'NILADIC,'T)
     else REMPROP(cnam,'NILADIC)
    systemDependentMkAutoload(cnam,cnam)
 
systemDependentMkAutoload(fn,cnam) ==
    FBOUNDP(cnam) => "next"
    asharpName := GETDATABASE(cnam, 'ASHARP?) =>
         kind := GETDATABASE(cnam, 'CONSTRUCTORKIND)
         cosig := GETDATABASE(cnam, 'COSIG)
         file := GETDATABASE(cnam, 'OBJECT)
         SET_-LIB_-FILE_-GETTER(file, cnam)
         kind = 'category =>
              ASHARPMKAUTOLOADCATEGORY(file, cnam, asharpName, cosig)
         ASHARPMKAUTOLOADFUNCTOR(file, cnam, asharpName, cosig)
    SETF(SYMBOL_-FUNCTION cnam,mkAutoLoad(fn, cnam))
 
autoLoad(abb,cname) ==
  if not GETL(cname,'LOADED) then loadLib cname
  SYMBOL_-FUNCTION cname
 
setAutoLoadProperty(name) ==
--  abb := constructor? name
  REMPROP(name,'LOADED)
  SETF(SYMBOL_-FUNCTION name,mkAutoLoad(name, name))
 
--% Compilation
 
compileConstructorLib(l,op,editFlag,traceFlag) ==
  --this file corresponds to /C,1
  MEMQ('_?,l) => return editFile '(_/C TELL _*)
  optionList:= _/OPTIONS l
  funList:= TRUNCLIST(l,optionList) or [_/FN]
  options:= [[UPCASE CAR x,:CDR x] for x in optionList]
  infile:=  _/MKINFILENAM _/GETOPTION(options,'FROM_=)
  outfile:= _/MKINFILENAM _/GETOPTION(options,'TO_=)
  res:= [compConLib1(fn,infile,outfile,op,editFlag,traceFlag)
               for fn in funList]
  SHUT INPUTSTREAM
  res
 
compConLib1(fun,infileOrNil,outfileOrNil,auxOp,editFlag,traceFlag) ==
  $PRETTYPRINT: local := 'T
  $LISPLIB: local := 'T
  $lisplibAttributes: local := NIL
  $lisplibPredicates: local := NIL
  $lisplibForm: local := NIL
  $lisplibAbbreviation: local := NIL
  $lisplibParents: local := NIL
  $lisplibAncestors: local := NIL
  $lisplibKind: local := NIL
  $lisplibModemap: local := NIL
  $lisplibModemapAlist: local := NIL
  $lisplibCategoriesExtended: local := NIL -- this is always nil. why? (tpd)
  $lisplibSlot1 : local := NIL   --used by NRT mechanisms
  $lisplibOperationAlist: local := NIL
  $lisplibOpAlist: local:= NIL
  $lisplibSuperDomain: local := NIL
  $libFile: local := NIL
  $lisplibVariableAlist: local := NIL
  $lisplibSignatureAlist: local := NIL
  if null atom fun and null CDR fun then fun:= CAR fun -- unwrap nullary
  libName:= getConstructorAbbreviation fun
  infile:= infileOrNil or getFunctionSourceFile fun or
    throwKeyedMsg("S2IL0004",[fun])
  SETQ(_/EDITFILE,infile)
  outfile := outfileOrNil or
    [libName,'OUTPUT,$listingDirectory]   --always QUIET
  _$ERASE(libName,'OUTPUT,$listingDirectory)
  outstream:= DEFSTREAM(outfile,'OUTPUT)
  val:= _/D_,2_,LIB(fun,infile,outstream,auxOp,editFlag,traceFlag)
  val
 
compDefineLisplib(df:=["DEF",[op,:.],:.],m,e,prefix,fal,fn) ==
  --fn= compDefineCategory OR compDefineFunctor
  sayMSG fillerSpaces(72,'"-")
  $LISPLIB: local := 'T
  $op: local := op
  $lisplibAttributes: local := NIL
  $lisplibPredicates: local := NIL -- set by makePredicateBitVector
  $lisplibCategoriesExtended: local := NIL -- this is always nil. why? (tpd)
  $lisplibForm: local := NIL
  $lisplibKind: local := NIL
  $lisplibAbbreviation: local := NIL
  $lisplibParents: local := NIL
  $lisplibAncestors: local := NIL
  $lisplibModemap: local := NIL
  $lisplibModemapAlist: local := NIL
  $lisplibSlot1 : local := NIL   -- used by NRT mechanisms
  $lisplibOperationAlist: local := NIL
  $lisplibSuperDomain: local := NIL
  $libFile: local := NIL
  $lisplibVariableAlist: local := NIL
--  $lisplibRelatedDomains: local := NIL   --from ++ Related Domains: see c-doc
  $lisplibCategory: local := nil        
  --for categories, is rhs of definition; otherwise, is target of functor
  --will eventually become the "constructorCategory" property in lisplib
  --set in compDefineCategory1 if category, otherwise in finalizeLisplib
  libName := getConstructorAbbreviation op
  BOUNDP '$compileDocumentation and $compileDocumentation =>
     compileDocumentation libName
  sayMSG ['"   initializing ",$spadLibFT,:bright libName,
    '"for",:bright op]
  initializeLisplib libName
  sayMSG ['"   compiling into ",$spadLibFT,:bright libName]
  -- res:= FUNCALL(fn,df,m,e,prefix,fal)
  -- sayMSG ['"   finalizing ",$spadLibFT,:bright libName]
  -- finalizeLisplib libName
  -- following guarantee's compiler output files get closed.
  ok := false;
  UNWIND_-PROTECT(
      PROGN(res:= FUNCALL(fn,df,m,e,prefix,fal),
            sayMSG ['"   finalizing ",$spadLibFT,:bright libName],
            finalizeLisplib libName,
            ok := true),
      RSHUT $libFile)
  if ok then lisplibDoRename(libName)
  filearg := $FILEP(libName,$spadLibFT,$libraryDirectory)
  RPACKFILE filearg
  FRESH_-LINE $algebraOutputStream
  sayMSG fillerSpaces(72,'"-")
  unloadOneConstructor(op,libName)
  LOCALDATABASE(LIST GETDATABASE(op,'ABBREVIATION),NIL)
  $newConlist := [op, :$newConlist]  ---------->  bound in function "compiler"
  if $lisplibKind = 'category
    then updateCategoryFrameForCategory op
     else updateCategoryFrameForConstructor op
  res
 
compileDocumentation libName ==
  filename := MAKE_-INPUT_-FILENAME(libName,$spadLibFT)
  $FCOPY(filename,[libName,'DOCLB])
  stream := RDEFIOSTREAM [['FILE,libName,'DOCLB],['MODE, :'O]]
  lisplibWrite('"documentation",finalizeDocumentation(),stream)
--  if $lisplibRelatedDomains then 
--    lisplibWrite('"relatedDomains",$lisplibRelatedDomains,stream)
  RSHUT(stream)
  RPACKFILE([libName,'DOCLB])
  $REPLACE([libName,$spadLibFT],[libName,'DOCLB])
  ['dummy, $EmptyMode, $e]

getLisplibVersion libName ==
  stream := RDEFIOSTREAM [['FILE,libName,$spadLibFT],['MODE, :'I]]
  version:= CADR rread('VERSION, stream,nil)
  RSHUT(stream)
  version
 
initializeLisplib libName ==
  _$ERASE(libName,'ERRORLIB,$libraryDirectory)
  SETQ(ERRORS,0) -- ERRORS is a fluid variable for the compiler
  $libFile:= writeLib1(libName,'ERRORLIB,$libraryDirectory)
  ADDOPTIONS('FILE,$libFile)
  $lisplibForm := nil             --defining form for lisplib
  $lisplibModemap := nil          --modemap for constructor form
  $lisplibKind := nil             --category, domain, or package
  $lisplibModemapAlist := nil  --changed in "augmentLisplibModemapsFromCategory"
  $lisplibAbbreviation := nil
  $lisplibAncestors := nil
  $lisplibOpAlist := nil  --operations alist for new runtime system
  $lisplibOperationAlist := nil   --old list of operations for functor/package
  $lisplibSuperDomain:= nil
  -- next var changed in "augmentLisplibDependents"
  $lisplibVariableAlist := nil    --this and the next are used by "luke"
  $lisplibSignatureAlist := nil
  if pathnameTypeId(_/EDITFILE) = 'SPAD
    then LAM_,FILEACTQ('VERSION,['_/VERSIONCHECK,_/MAJOR_-VERSION])
 
finalizeLisplib libName ==
  lisplibWrite('"constructorForm",removeZeroOne $lisplibForm,$libFile)
  lisplibWrite('"constructorKind",kind:=removeZeroOne $lisplibKind,$libFile)
  lisplibWrite('"constructorModemap",removeZeroOne $lisplibModemap,$libFile)
  $lisplibCategory:= $lisplibCategory or $lisplibModemap.mmTarget
  -- set to target of modemap for package/domain constructors;
  -- to the right-hand sides (the definition) for category constructors
  lisplibWrite('"constructorCategory",$lisplibCategory,$libFile)
  lisplibWrite('"sourceFile",namestring _/EDITFILE,$libFile)
  lisplibWrite('"modemaps",removeZeroOne $lisplibModemapAlist,$libFile)
  opsAndAtts:= getConstructorOpsAndAtts(
    $lisplibForm,kind,$lisplibModemap)
  lisplibWrite('"operationAlist",removeZeroOne CAR opsAndAtts,$libFile)
  --lisplibWrite('"attributes",CDR opsAndAtts,$libFile)
  --if kind='category then NRTgenInitialAttributeAlist CDR opsAndAtts
  if kind='category then
     $pairlis : local := [[a,:v] for a in rest $lisplibForm
                                 for v in $FormalMapVariableList]
     $NRTslot1PredicateList : local := []
     NRTgenInitialAttributeAlist CDR opsAndAtts
  lisplibWrite('"superDomain",removeZeroOne $lisplibSuperDomain,$libFile)
  lisplibWrite('"signaturesAndLocals",
    removeZeroOne mergeSignatureAndLocalVarAlists($lisplibSignatureAlist,
                                    $lisplibVariableAlist),$libFile)
  lisplibWrite('"attributes",removeZeroOne $lisplibAttributes,$libFile)
  lisplibWrite('"predicates",removeZeroOne  $lisplibPredicates,$libFile)
  lisplibWrite('"abbreviation",$lisplibAbbreviation,$libFile)
  lisplibWrite('"parents",removeZeroOne $lisplibParents,$libFile)
  lisplibWrite('"ancestors",removeZeroOne $lisplibAncestors,$libFile)
  lisplibWrite('"documentation",finalizeDocumentation(),$libFile)
  lisplibWrite('"slot1Info",removeZeroOne $lisplibSlot1,$libFile)
  if $profileCompiler then profileWrite()
  if $lisplibForm and null CDR $lisplibForm then
    MAKEPROP(CAR $lisplibForm,'NILADIC,'T)
  ERRORS ^=0 =>    -- ERRORS is a fluid variable for the compiler
    sayMSG ['"   Errors in processing ",kind,'" ",:bright libName,'":"]
    sayMSG ['"     not replacing ",$spadLibFT,'" for",:bright libName]

lisplibDoRename(libName) ==
  _$REPLACE([libName,$spadLibFT,$libraryDirectory],
    [libName,'ERRORLIB,$libraryDirectory])
 
lisplibError(cname,fname,type,cn,fn,typ,error) ==
  sayMSG bright ['"  Illegal ",$spadLibFT]
  error in '(duplicateAbb  wrongType) =>
    sayKeyedMsg("S2IL0007",
      [namestring [fname,$spadLibFT],type,cname,typ,cn])
  error is 'abbIsName =>
    throwKeyedMsg("S2IL0008",[fname,typ,namestring [fn,$spadLibFT]])
 
getPartialConstructorModemapSig(c) ==
  (s := getConstructorSignature c) => rest s
  throwEvalTypeMsg("S2IL0015",[c])
 
mergeSignatureAndLocalVarAlists(signatureAlist, localVarAlist) ==
  -- this function makes a single Alist for both signatures
  -- and local variable types, to be stored in the LISPLIB
  -- for the function being compiled
  [[funcName,:[signature,:LASSOC(funcName,localVarAlist)]] for
    [funcName, :signature] in signatureAlist]
 
Operators u ==
  ATOM u => []
  ATOM first u =>
    answer:="union"/[Operators v for v in rest u]
    MEMQ(first u,answer) => answer
    [first u,:answer]
  "union"/[Operators v for v in u]
 
getConstructorOpsAndAtts(form,kind,modemap) ==
  kind is 'category => getCategoryOpsAndAtts(form)
  getFunctorOpsAndAtts(form,modemap)
 
getCategoryOpsAndAtts(catForm) ==
  -- returns [operations,:attributes] of CAR catForm
  [transformOperationAlist getSlotFromCategoryForm(catForm,1),
    :getSlotFromCategoryForm(catForm,2)]
 
getFunctorOpsAndAtts(form,modemap) ==
  [transformOperationAlist getSlotFromFunctor(form,1,modemap),
    :getSlotFromFunctor(form,2,modemap)]
 
getSlotFromFunctor([name,:args],slot,[[.,target,:argMml],:.]) ==
  slot = 1 => $lisplibOperationAlist
  t := compMakeCategoryObject(target,$e) or
      systemErrorHere '"getSlotFromFunctor"
  t.expr.slot
 
getSlot1 domainName ==
  $e: local:= $CategoryFrame
  fn:= getLisplibName domainName
  p := pathname [fn,$spadLibFT,'"*"]
  not isExistingFile(p) =>
    sayKeyedMsg("S2IL0003",[namestring p])
    NIL
  (sig := getConstructorSignature domainName) =>
    [.,target,:argMml] := sig
    for a in $FormalMapVariableList for m in argMml repeat
      $e:= put(a,'mode,m,$e)
    t := compMakeCategoryObject(target,$e) or
      systemErrorHere '"getSlot1"
    t.expr.1
  sayKeyedMsg("S2IL0022",[namestring p,'"constructor modemap"])
  NIL
 
transformOperationAlist operationAlist ==
  --  this transforms the operationAlist which is written out onto LISPLIBs.
  --  The original form of this list is a list of items of the form:
  --        ((<op> <signature>) (<condition> (ELT $ n)))
  --  The new form is an op-Alist which has entries (<op> . signature-Alist)
  --      where signature-Alist has entries (<signature> . item)
  --        where item has form (<slotNumber> <condition> <kind>)
  --          where <kind> =
  --             NIL  => function
  --             CONST => constant ... and others
  newAlist:= nil
  for [[op,sig,:.],condition,implementation] in operationAlist repeat
    kind:=
      implementation is [eltEtc,.,n] and eltEtc in '(CONST ELT) => eltEtc
      implementation is [impOp,:.] =>
        impOp = 'XLAM => implementation
        impOp in '(CONST Subsumed) => impOp
        keyedSystemError("S2IL0025",[impOp])
      implementation = 'mkRecord => 'mkRecord
      keyedSystemError("S2IL0025",[implementation])
    signatureItem:=
      if u:= ASSOC([op,sig],$functionLocations) then n := [n,:rest u]
      kind = 'ELT =>
        condition = 'T => [sig,n]
        [sig,n,condition]
      [sig,n,condition,kind]
    itemList:= [signatureItem,:LASSQ(op,newAlist)]
    newAlist:= insertAlist(op,itemList,newAlist)
  newAlist
 
sayNonUnique x ==
  sayBrightlyNT '"Non-unique:"
  pp x
 
-- flattenOperationAlist operationAlist ==
--   --new form is (<op> <signature> <slotNumber> <condition> <kind>)
--   [:[[op,:x] for x in y] for [op,:y] in operationAlist]
 
getSlotFromDomain(dom,op,oldSig) ==
  --  returns the slot number in the domain where the function whose
  --  signature is oldSig may be found in the domain dom
  oldSig:= removeOPT oldSig
  dom:= removeOPT dom
  sig:= SUBST("$",dom,oldSig)
  loadIfNecessary first dom
  isPackageForm dom => getSlotFromPackage(dom,op,oldSig)
  domain:= evalDomain dom
  n:= findConstructorSlotNumber(dom,domain,op,sig) =>
    (slot:= domain.n).0 = function Undef =>
      throwKeyedMsg("S2IL0023A",[op,formatSignature sig,dom])
    slot
  throwKeyedMsg("S2IL0024A",[op,formatSignature sig,dom])
 
findConstructorSlotNumber(domainForm,domain,op,sig) ==
  null domain.1 => getSlotNumberFromOperationAlist(domainForm,op,sig)
  sayMSG ['"   using slot 1 of ",domainForm]
  constructorArglist:= rest domainForm
  nsig:=#sig
  tail:= or/[r for [[op1,sig1],:r] in domain.1 | op=op1 and nsig=#sig1 and
    "and"/[compare for a in sig for b in sig1]] where compare() ==
      a=b => true
      FIXP b => a=constructorArglist.b
      isSuperDomain(bustUnion b,bustUnion a,$CategoryFrame)
  tail is [.,["ELT",.,n]] => n
  systemErrorHere '"findSlotNumber"
 
bustUnion d ==
  d is ["Union",domain,utype] and utype='"failed" => domain
  d
 
getSlotNumberFromOperationAlist(domainForm,op,sig) ==
  constructorName:= CAR domainForm
  constructorArglist:= CDR domainForm
  operationAlist:=
    GETDATABASE(constructorName, 'OPERATIONALIST) or
      keyedSystemError("S2IL0026",[constructorName])
  entryList:= QLASSQ(op,operationAlist) or return nil
  tail:= or/[r for [sig1,:r] in entryList | sigsMatch(sig,sig1,domainForm)] =>
    first tail
  nil
 
sigsMatch(sig,sig1,domainForm) ==
  --  does signature "sig" match "sig1", where integers 1,2,.. in
  --  sig1 designate corresponding arguments of domainForm
  while sig and sig1 repeat
    partsMatch:=
      (item:= CAR sig)=(item1:= CAR sig1) => true --ok, go to next iteration
      FIXP item1 => item = domainForm.item1       --item1=n means nth arg
      isSuperDomain(bustUnion item,bustUnion item1,$CategoryFrame)
    null partsMatch => return nil
    sig:= rest sig; sig1 := rest sig1
  sig or sig1 => nil
  true
 
findDomainSlotNumber(domain,op,sig) == --using slot 1 of the domain
  nsig:=#sig
  tail:= or/[r for [[op1,sig1],:r] in domain.1 | op=op1 and nsig=#sig1 and
    "and"/[a=b or isSuperDomain(bustUnion b,bustUnion a,$CategoryFrame)
      for a in sig for b in sig1]]
  tail is [.,["ELT",.,n]] => n
  systemErrorHere '"findDomainSlotNumber"
 
 
getConstructorModemap form ==
  GETDATABASE(opOf form, 'CONSTRUCTORMODEMAP)
 
getConstructorSignature form ==
  (mm := GETDATABASE(opOf(form),'CONSTRUCTORMODEMAP)) =>
    [[.,:sig],:.] := mm
    sig
  NIL
 
--% from MODEMAP BOOT
 
augModemapsFromDomain1(name,functorForm,e) ==
  GETL(KAR functorForm,"makeFunctionList") =>
    addConstructorModemaps(name,functorForm,e)
  atom functorForm and (catform:= getmode(functorForm,e)) =>
    augModemapsFromCategory(name,name,functorForm,catform,e)
  mappingForm:= getmodeOrMapping(KAR functorForm,e) =>
    ["Mapping",categoryForm,:functArgTypes]:= mappingForm
    catform:= substituteCategoryArguments(rest functorForm,categoryForm)
    augModemapsFromCategory(name,name,functorForm,catform,e)
  stackMessage [functorForm," is an unknown mode"]
  e
 
getSlotFromCategoryForm ([op,:argl],index) ==
  u:= eval [op,:MAPCAR('MKQ,TAKE(#argl,$FormalMapVariableList))]
  null VECP u =>
    systemErrorHere '"getSlotFromCategoryForm"
  u . index
 
 
--% constructor evaluation
--  The following functions are used by the compiler but are modified
--  here for use with new LISPLIB scheme
 
mkEvalableCategoryForm c ==       --from DEFINE
  c is [op,:argl] =>
    op="Join" => ["Join",:[mkEvalableCategoryForm x for x in argl]]
    op is "DomainSubstitutionMacro" =>
        --$extraParms :local
        --catobj := EVAL c -- DomainSubstitutionFunction makes $extraParms
        --mkEvalableCategoryForm sublisV($extraParms, catobj)
        mkEvalableCategoryForm CADR argl
    op is "mkCategory" => c
    MEMQ(op,$CategoryNames) =>
      ([x,m,$e]:= compOrCroak(c,$EmptyMode,$e); m=$Category => x)
    --loadIfNecessary op
    GETDATABASE(op,'CONSTRUCTORKIND) = 'category or
      get(op,"isCategory",$CategoryFrame) =>
        [op,:[quotifyCategoryArgument x for x in argl]]
    [x,m,$e]:= compOrCroak(c,$EmptyMode,$e)
    m=$Category => x
  MKQ c
 
isDomainForm(D,e) ==
  --added for MPOLY 3/83 by RDJ
  MEMQ(KAR D,$SpecialDomainNames) or isFunctor D or
    -- ((D is ['Mapping,target,:.]) and isCategoryForm(target,e)) or
     ((getmode(D,e) is ['Mapping,target,:.]) and isCategoryForm(target,e)) or
       isCategoryForm(getmode(D,e),e) or isDomainConstructorForm(D,e)
 
isDomainConstructorForm(D,e) ==
  D is [op,:argl] and (u:= get(op,"value",e)) and
    u is [.,["Mapping",target,:.],:.] and
      isCategoryForm(EQSUBSTLIST(argl,$FormalMapVariableList,target),e)
 
isFunctor x ==
  op:= opOf x
  not IDENTP op => false
  $InteractiveMode =>
    MEMQ(op,'(Union SubDomain Mapping Record)) => true
    MEMQ(GETDATABASE(op,'CONSTRUCTORKIND),'(domain package))
  u:= get(op,'isFunctor,$CategoryFrame)
    or MEMQ(op,'(SubDomain Union Record)) => u
  constructor? op =>
    prop := get(op,'isFunctor,$CategoryFrame) => prop
    if GETDATABASE(op,'CONSTRUCTORKIND) = 'category
      then updateCategoryFrameForCategory op
      else updateCategoryFrameForConstructor op
    get(op,'isFunctor,$CategoryFrame)
  nil
 
 
 
