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


import nlib
import c_-util
import debug

namespace BOOT
module lisplib


++
$functionLocations := []

--% Standard Library Creation Functions
 
readLib(fn,ft) == readLib1(fn,ft,"*")
 
readLib1(fn,ft,fm) ==
  -- see if it exists first
  p := pathname [fn,ft,fm] or return nil
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
  p := pathname [fn,ft,'"*"] or return nil
  cache => hasFileProperty(p,id,fn)
  hasFilePropertyNoCache(p,id,fn)
 
hasFilePropertyNoCache(p,id,abbrev) ==
  -- it is assumed that the file exists and is a proper pathname
  -- startTimingProcess 'diskread
  fnStream:= readLibPathFast p
  null fnStream => NIL
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

++ Return a path to the loadable module that contains the
++ definition of the constructor indicated by `cname'.  
++ Error if the file container of the module does not exist.
findModule: %Symbol -> %Maybe %String
findModule cname ==
  m := getConstructorModuleFromDB cname or return nil
  existingFile? m => m
  strap := algebraBootstrapDir() =>
    m := strconc(strap,PATHNAME_-NAME m,'".",$faslType)
    existingFile? m => m
    systemError ['"missing module for ",:bright cname]
  systemError ['"missing module for ",:bright cname]
 
loadLibIfNotLoaded libName ==
  -- replaces old SpadCondLoad
  -- loads is library is not already loaded
  $PrintOnly = 'T => NIL
  GETL(libName,'LOADED) => NIL
  loadLib libName
 
loadLib cname ==
  startTimingProcess 'load
  fullLibName := findModule cname or return nil
  systemdir? := isSystemDirectory(pathnameDirectory fullLibName)
  update? := $forceDatabaseUpdate or not systemdir? 
  not update? =>
     loadLibNoUpdate(cname, cname, fullLibName)
  kind := getConstructorKindFromDB cname
  if $printLoadMsgs then
    sayKeyedMsg("S2IL0002",[namestring fullLibName,kind,cname])
  loadModule(fullLibName,cname)
  clearConstructorCache cname
  updateDatabase(cname,cname,systemdir?)
  installConstructor(cname,kind)
  u := getConstructorModemapFromDB cname
  updateCategoryTable(cname,kind)
  coSig :=
      u =>
          [[.,:sig],:.] := u
          [NIL,:[categoryForm?(x) for x in rest sig]]
      NIL
  -- in following, add property value false or NIL to possibly clear
  -- old value
  if null rest getConstructorFormFromDB cname then
      property(cname,'NILADIC) := true
    else
      REMPROP(cname,'NILADIC)
  property(cname,'LOADED) := fullLibName
  if $InteractiveMode then $CategoryFrame := $EmptyEnvironment
  stopTimingProcess 'load
  'T

loadLibNoUpdate(cname, libName, fullLibName) ==
  kind := getConstructorKindFromDB cname
  if $printLoadMsgs then
    sayKeyedMsg("S2IL0002",[namestring fullLibName,kind,cname])
  if CATCH('VERSIONCHECK,loadModule(fullLibName,cname)) = -1
    then 
      PRINC('"   wrong library version...recompile ")
      PRINC(fullLibName)
      TERPRI()
      TOPLEVEL()
    else
     clearConstructorCache cname
     installConstructor(cname,kind)
     property(cname,'LOADED) := fullLibName
     if $InteractiveMode then $CategoryFrame := $EmptyEnvironment
     stopTimingProcess 'load
  'T
 
loadIfNecessary u == loadLibIfNecessary(u,true)
 
loadIfNecessaryAndExists u == loadLibIfNecessary(u,nil)
 
loadLibIfNecessary(u,mustExist) ==
  u = '$EmptyMode => u
  cons? u => loadLibIfNecessary(first u,mustExist)
  value:=
    functionp(u) or macrop(u) => u
    GETL(u,'LOADED) => u
    loadLib u => u
  null $InteractiveMode and ((null (y:= getProplist(u,$CategoryFrame)))
    or (null LASSOC('isFunctor,y)) and (null LASSOC('isCategory,y))) =>
      y:= getConstructorKindFromDB u =>
         y = "category" =>
            updateCategoryFrameForCategory u
         updateCategoryFrameForConstructor u
      throwKeyedMsg("S2IL0005",[u])
  value
 
convertOpAlist2compilerInfo(opalist) ==
   "append"/[[formatSig(op,sig) for sig in siglist]
                for [op,:siglist] in opalist] where
      formatSig(op, [typelist, slot,:stuff]) ==
          pred := if stuff then first stuff else true
          impl := if rest stuff then second stuff else "ELT" -- handles 'CONST
          [[op, typelist], pred, [impl, '$, slot]]
   
updateCategoryFrameForConstructor(constructor) ==
   opAlist := getConstructorOperationsFromDB constructor
   [[dc,:sig],[pred,impl]] := getConstructorModemapFromDB constructor
   $CategoryFrame := put(constructor,'isFunctor,
       convertOpAlist2compilerInfo(opAlist),
       addModemap(constructor, dc, sig, pred, impl,
           put(constructor, 'mode, ['Mapping,:sig], $CategoryFrame)))

updateCategoryFrameForCategory(category) ==
   [[dc,:sig],[pred,impl]] := getConstructorModemapFromDB category
   $CategoryFrame :=
     put(category, 'isCategory, 'T,
         addModemap(category, dc, sig, pred, impl, $CategoryFrame))

loadFunctor u ==
  cons? u => loadFunctor first u
  loadLibIfNotLoaded u
  u
 
makeConstructorsAutoLoad() ==
  for cnam in allConstructors() repeat
    cnam in $CategoryNames => nil
    REMPROP(cnam,'LOADED)
--    fn:=getConstructorAbbreviationFromDB cnam
    if niladicConstructorFromDB cnam
     then PUT(cnam,'NILADIC,'T)
     else REMPROP(cnam,'NILADIC)
    systemDependentMkAutoload(constructor? cnam,cnam)
 
systemDependentMkAutoload(fn,cnam) ==
    FBOUNDP(cnam) => "next"
    asharpName := asharpConstructorFromDB cnam =>
         kind := getConstructorKindFromDB cnam
         cosig := getDualSignatureFromDB cnam
         file := getConstructorModuleFromDB cnam
         SET_-LIB_-FILE_-GETTER(file, cnam)
         kind = 'category =>
              ASHARPMKAUTOLOADCATEGORY(file, cnam, asharpName, cosig)
         ASHARPMKAUTOLOADFUNCTOR(file, cnam, asharpName, cosig)
    SETF(SYMBOL_-FUNCTION cnam,mkAutoLoad(fn, cnam))

autoLoad(abb,cname) ==
  -- builtin constructors are always loaded.  By definition, there
  -- is no way to unload them and load them again.
  cname in $BuiltinConstructorNames => cname
  if not GETL(cname,'LOADED) then loadLib cname
  SYMBOL_-FUNCTION cname

setAutoLoadProperty(name) ==
--  abb := constructor? name
  REMPROP(name,'LOADED)
  SETF(SYMBOL_-FUNCTION name,mkAutoLoad(constructor? name, name))

unloadOneConstructor(cnam,fn) ==
    REMPROP(cnam,'LOADED)
    SETF(SYMBOL_-FUNCTION cnam,mkAutoLoad(fn, cnam))

--% Compilation
 
compileConstructorLib(l,op,editFlag,traceFlag) ==
  --this file corresponds to /C,1
  '_? in l => return editFile '(_/C TELL _*)
  optionList:= _/OPTIONS l
  funList:= TRUNCLIST(l,optionList) or [_/FN]
  options:= [[UPCASE first x,:rest x] for x in optionList]
  infile:=  _/MKINFILENAM _/GETOPTION(options,'FROM_=)
  outfile:= _/MKINFILENAM _/GETOPTION(options,'TO_=)
  res:= [compConLib1(fn,infile,outfile,op,editFlag,traceFlag)
               for fn in funList]
  SHUT $InputStream
  res
 
compConLib1(fun,infileOrNil,outfileOrNil,auxOp,editFlag,traceFlag) ==
  $PrettyPrint: local := 'T
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
  $lisplibSlot1 : local := NIL   --used by NRT mechanisms
  $lisplibOperationAlist: local := NIL
  $lisplibOpAlist: local:= NIL
  $lisplibSuperDomain: local := NIL
  $libFile: local := NIL
  $lisplibVariableAlist: local := NIL
  $lisplibSignatureAlist: local := NIL
  if cons? fun and null rest fun then fun:= first fun -- unwrap nullary
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
  --fn= compDefineCategory1 OR compDefineFunctor1
  sayMSG fillerSpaces(72,'"-")
  $LISPLIB: local := 'T
  $op: local := op
  $lisplibAttributes: local := NIL
  $lisplibPredicates: local := NIL -- set by makePredicateBitVector
  $lisplibForm: local := NIL
  $lisplibKind: local := NIL
  $lisplibAbbreviation: local := NIL
  $lisplibParents: local := NIL
  $lisplibAncestors: local := NIL
  $lisplibModemap: local := NIL
  $lisplibModemapAlist: local := NIL
  $lisplibSlot1 : local := NIL   -- used by NRT mechanisms
  $lisplibOperationAlist: local := NIL
  $lisplibOpAlist: local := nil  --operations alist for new runtime system
  $lisplibSignatureAlist: local := nil
  $lisplibSuperDomain: local := NIL
  $libFile: local := NIL
  $lisplibVariableAlist: local := NIL
--  $lisplibRelatedDomains: local := NIL   --from ++ Related Domains: see c-doc
  $lisplibCategory: local := nil        
  --for categories, is rhs of definition; otherwise, is target of functor
  --will eventually become the "constructorCategory" property in lisplib
  --set in compDefineCategory1 if category, otherwise in finalizeLisplib
  libName := getConstructorAbbreviation op
  $compileDocumentation => compileDocumentation libName
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
            leaveIfErrors(libName),
            sayMSG ['"   finalizing ",$spadLibFT,:bright libName],
            ok := finalizeLisplib libName),
      RSHUT $libFile)
  if ok then lisplibDoRename(libName)
  filearg := $FILEP(libName,$spadLibFT,$libraryDirectory)
  RPACKFILE filearg
  FRESH_-LINE $algebraOutputStream
  sayMSG fillerSpaces(72,'"-")
  unloadOneConstructor(op,libName)
  LOCALDATABASE([SYMBOL_-NAME getConstructorAbbreviationFromDB op],NIL)
  $newConlist := [op, :$newConlist]  ---------->  bound in function "compiler"
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
  version:= second rread('VERSION, stream,nil)
  RSHUT(stream)
  version
 
initializeLisplib libName ==
  _$ERASE(libName,'ERRORLIB,$libraryDirectory)
  resetErrorCount()
  $libFile := writeLib1(libName,'ERRORLIB,$libraryDirectory)
  ADDOPTIONS('FILE,$libFile)
  if pathnameTypeId(_/EDITFILE) = 'SPAD
    then LAM_,FILEACTQ('VERSION,['_/VERSIONCHECK,_/MAJOR_-VERSION])

++ If compilation produces an error, issue inform user and
++ return to toplevel reader.
leaveIfErrors libName ==
  errorCount() ~= 0 =>
    sayMSG ['"   Errors in processing ",$lisplibKind,'" ",:bright libName,'":"]
    sayMSG ['"     not replacing ",$spadLibFT,'" for",:bright libName]
    spadThrow()

++ Finalize `libName' compilation; returns true if everything is OK.
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
  lisplibWrite('"operationAlist",removeZeroOne first opsAndAtts,$libFile)
  --lisplibWrite('"attributes",rest opsAndAtts,$libFile)
  --if kind='category then NRTgenInitialAttributeAlist rest opsAndAtts
  if kind='category then
     $pairlis : local := pairList($lisplibForm,$FormalMapVariableList)
     $NRTslot1PredicateList : local := []
     NRTgenInitialAttributeAlist rest opsAndAtts
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
  if $lisplibForm and null rest $lisplibForm then
    property(first $lisplibForm,'NILADIC) := true
  leaveIfErrors libName
  true

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
  atom u => []
  atom first u =>
    answer:="union"/[Operators v for v in rest u]
    MEMQ(first u,answer) => answer
    [first u,:answer]
  "union"/[Operators v for v in u]
 
getConstructorOpsAndAtts(form,kind,modemap) ==
  kind is 'category => getCategoryOpsAndAtts(form)
  getFunctorOpsAndAtts(form,modemap)
 
getCategoryOpsAndAtts(catForm) ==
  -- returns [operations,:attributes] of first catForm
  [transformOperationAlist getSlotFromCategoryForm(catForm,1),
    :getSlotFromCategoryForm(catForm,2)]
 
getFunctorOpsAndAtts(form,modemap) ==
  [transformOperationAlist getSlotFromFunctor(form,1,modemap),
    :getSlotFromFunctor(form,2,modemap)]
 
getSlotFromFunctor([name,:args],slot,[[.,target,:argMml],:.]) ==
  slot = 1 => $lisplibOperationAlist
  t := compMakeCategoryObject(target,$e) or
      systemErrorHere "getSlotFromFunctor"
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
      systemErrorHere ["getSlot1",domainName]
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
      if u:= assoc([op,sig],$functionLocations) then n := [n,:rest u]
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
 
findConstructorSlotNumber(domainForm,domain,op,sig) ==
  null domain.1 => getSlotNumberFromOperationAlist(domainForm,op,sig)
  sayMSG ['"   using slot 1 of ",domainForm]
  constructorArglist:= rest domainForm
  nsig:=#sig
  tail:= or/[r for [[op1,sig1],:r] in domain.1 | op=op1 and nsig=#sig1 and
    "and"/[compare for a in sig for b in sig1]] where compare() ==
      a=b => true
      FIXP b => a=constructorArglist.b
      isSubset(bustUnion a,bustUnion b,$CategoryFrame)
  tail is [.,["ELT",.,n]] => n
  systemErrorHere ["findConstructorSlotNumber",domainForm]
 
bustUnion d ==
  d is ["Union",domain,utype] and utype='"failed" => domain
  d
 
getSlotNumberFromOperationAlist(domainForm,op,sig) ==
  constructorName:= first domainForm
  constructorArglist:= rest domainForm
  operationAlist:=
    getConstructorOperationsFromDB constructorName or
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
      (item:= first sig)=(item1:= first sig1) => true --ok, go to next iteration
      FIXP item1 => item = domainForm.item1       --item1=n means nth arg
      isSubset(bustUnion item1,bustUnion item,$CategoryFrame)
    null partsMatch => return nil
    sig:= rest sig; sig1 := rest sig1
  sig or sig1 => nil
  true
 
findDomainSlotNumber(domain,op,sig) == --using slot 1 of the domain
  nsig:=#sig
  tail:= or/[r for [[op1,sig1],:r] in domain.1 | op=op1 and nsig=#sig1 and
    "and"/[a=b or isSubset(bustUnion a,bustUnion b,$CategoryFrame)
      for a in sig for b in sig1]]
  tail is [.,["ELT",.,n]] => n
  systemErrorHere '"findDomainSlotNumber"
 

getConstructorSignature: %Symbol -> %Form
getConstructorSignature ctor ==
  ([[.,:sig],:.] := getConstructorModemapFromDB ctor) => sig
  -- If we have a local or forward declaration take it.
  -- Note: constructors are not overloadable.
  rest getmode(ctor,$e)
 
getSlotFromCategoryForm ([op,:argl],index) ==
  u:= eval [op,:MAPCAR('MKQ,TAKE(#argl,$FormalMapVariableList))]
  not vector? u =>
    systemErrorHere '"getSlotFromCategoryForm"
  u . index
 
 
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
    MEMQ(op,$DomainNames) => true
    getConstructorKindFromDB op in '(domain package)
  u:= get(op,'isFunctor,$CategoryFrame)
    or op in '(SubDomain Union Record Enumeration) => u
  ab := getConstructorAbbreviationFromDB op =>
    if getConstructorKindFromDB op = "category"
      then updateCategoryFrameForCategory op
      else loadExports ab or updateCategoryFrameForConstructor op
    get(op,'isFunctor,$CategoryFrame)
  nil

--%

getIndexPathname: %String -> %String
getIndexPathname dir ==
  strconc(ensureTrailingSlash dir, $IndexFilename)

getAllIndexPathnames: %String -> %List
getAllIndexPathnames dir ==
  -- GCL's semantics of Common Lisp's `DIRECTORY *' differs from the
  -- rest of everybody else' semantics.  Namely, GCL would return a
  -- a list of drirectories AND files.  Pretty much like `ls *'.
  -- Everybody else strips out directories.
)if %hasFeature KEYWORD::GCL
  [getIndexPathname NAMESTRING d for d in DIRECTORY strconc(dir,'"*.NRLIB")]
)else
  DIRECTORY strconc(dir,'"*.NRLIB/",$IndexFilename)
)endif
  

getAllAldorObjectFiles: %String -> %List
getAllAldorObjectFiles dir ==
  asys := DIRECTORY strconc(dir,'"*.asy")
  asos := DIRECTORY strconc(dir,'"*.ao")
  -- don't include both a `x.asy' and `x.ao', and retain
  -- only sensical .asy files.
  dupAOs := MAPCAN(function PATHNAME_-NAME,asys)
  [asys,[f for f in asos 
          | PATHNAME_-NAME f='"ao" and not member(PATHNAME_-NAME f,dupAOs)]]
    


++ returns an open stream for the index file, if present, 
++ in directory designated by 'dir'.
openIndexFileIfPresent: %String -> %Thing
openIndexFileIfPresent dir ==
  OPEN(getIndexPathname dir,KEYWORD::DIRECTION,KEYWORD::INPUT,
    KEYWORD::IF_-DOES_-NOT_-EXIST,nil)

++
getIndexTable: %String -> %Thing
getIndexTable dir ==
  indexFile := getIndexPathname dir
  existingFile? indexFile =>
    WITH_-OPEN_-FILE(stream indexFile, 
      GET_-INDEX_-TABLE_-FROM_-STREAM stream)
  -- index file doesn't exist but mark this directory as a Lisplib.
  WITH_-OPEN_-FILE(stream(indexFile,KEYWORD::DIRECTION,KEYWORD::OUTPUT),
    nil)

--%
compDefineExports(form,ops,sig,e) ==
  not $LISPLIB => systemErrorHere "compDefineExports"
  op := first form
  -- Ensure constructor parameters appear as formals
  sig := SUBLIS($pairlis, sig)
  ops := SUBLIS($pairlis,ops)
  form := SUBLIS($pairlis,form)
  -- In case we are not compiling the capsule, the slot numbers are
  -- most likely bogus.  Nullify them so people don't think they
  -- bear any meaningful semantics (well, they should not think
  -- these are forwarding either).
  if $compileExportsOnly then
    for entry in ops repeat
      fixupSigloc entry where
        fixupSigloc entry ==
          [opsig,pred,funsel] := entry
          if pred ~= 'T then 
            entry.rest.first := simpBool pred
          funsel is [op,a,:.] and op in '(ELT CONST) =>
            entry.rest.rest.first := [op,a,nil]
    ops := listSort(function GGREATERP, ops, function first)
  libName := getConstructorAbbreviation op
  exportsFile := strconc(STRING libName,'".sig")
  removeFile exportsFile
  withOutputFile(s,exportsFile, 
    PRETTYPRINT(
      ["SETQ", "$CategoryFrame",
        ["put", quoteForm op, quoteForm "isFunctor", quoteForm ops,
          ["addModemap", quoteForm op, quoteForm form,
             quoteForm sig, true, quoteForm op,
               ["put", quoteForm op, quoteForm "mode",
                 quoteForm ["Mapping",:sig], "$CategoryFrame"]]]], s))
  [op,["Mapping",:sig],e]
