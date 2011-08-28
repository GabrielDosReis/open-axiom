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


import nlib
import c_-util
import debug
import daase

namespace BOOT
module lisplib


++
$functionLocations := []

--=======================================================================
--                Generate Slot 2 Attribute Alist
--=======================================================================
NRTgenInitialAttributeAlist(db,attributeList) ==
  --alist has form ((item pred)...) where some items are constructor forms
  alist := [x for x in attributeList | -- throw out constructors
    not symbolMember?(opOf first x,allConstructors())]
  dbAttributes(db) := simplifyAttributeAlist(db,
    [[a,:b] for [a,b] in applySubst($pairlis,alist) | a isnt 'nothing])

simplifyAttributeAlist(db,al) ==
  al is [[a,:b],:r] =>
    u := [x for x in r | x is [=a,:b]] 
    null u => [first al,:simplifyAttributeAlist(db,rest al)]
    pred := simpBool makePrefixForm([b,:ASSOCRIGHT u],'OR)
    $NRTslot1PredicateList := insert(pred,$NRTslot1PredicateList)
    s := [x for x in r | x isnt [=a,:b]]
    [[a,:pred],:simplifyAttributeAlist(db,s)]
  nil
 
NRTgenFinalAttributeAlist e ==
  [[a,:k] for [a,:b] in $NRTattributeAlist
     | (k := predicateBitIndex(b,e)) ~= -1]
 
predicateBitIndex(x,e) == 
  pn(x,false,e) where
    pn(x,flag,e) ==
      u := simpBool transHasCode(x,e)
      u is 'T  =>  0
      u = nil => -1
      p := POSN1(u,$NRTslot1PredicateList) => p + 1
      not flag => pn(predicateBitIndexRemop x,true,e)
      systemError nil

predicateBitIndexRemop p==
--transform attribute predicates taken out by removeAttributePredicates
  p is [op,:argl] and op in '(AND and %and OR or %or NOT not %not) => 
    simpBool makePrefixForm([predicateBitIndexRemop x for x in argl],op)
  p is ["has",'$,['ATTRIBUTE,a]] => LASSOC(a,$NRTattributeAlist)
  p
 
predicateBitRef(x,e) ==
  x is 'T => 'T
  ['testBitVector,'pv_$,predicateBitIndex(x,e)]
 
makePrefixForm(u,op) ==
  u := MKPF(u,op)
  u is ''T => 'T
  u

--=======================================================================
--               Generate Slot 3 Predicate Vector
--=======================================================================
makePredicateBitVector(pl,e) ==   --called by buildFunctor
  if $insideCategoryPackageIfTrue then
    pl := union(pl,$categoryPredicateList)
  $predGensymAlist := nil --bound by buildFunctor, used by optHas
  for p in removeAttributePredicates pl repeat
    pred := simpBool transHasCode(p,e)
    pred isnt [.,:.] => 'skip                --skip over T and nil
    if isHasDollarPred pred then 
      lasts := insert(pred,lasts)
      for q in stripOutNonDollarPreds pred repeat firsts := insert(q,firsts)
    else 
      firsts := insert(pred,firsts)
  firstPl := applySubst($pairlis,reverse! orderByContainment firsts)
  lastPl  := applySubst($pairlis,reverse! orderByContainment lasts)
  firstCode:= 
    ['buildPredVector,0,0,mungeAddGensyms(firstPl,$predGensymAlist)]
  lastCode := augmentPredCode(# firstPl,lastPl)
  $lisplibPredicates := [:firstPl,:lastPl] --what is stored under 'predicates
  [$lisplibPredicates,firstCode,:lastCode]  --$pairlis set by compDefineFunctor1

augmentPredCode(n,lastPl) ==
  ['%list,:pl] := mungeAddGensyms(lastPl,$predGensymAlist)
  delta := 2 ** n
  l := [(u := MKPF([x,['augmentPredVector,"$",delta]],'AND); 
         delta:=2 * delta; u) for x in pl]

augmentPredVector(dollar,value) ==
  domainPredicates(dollar) := value + domainPredicates dollar

isHasDollarPred pred ==
  pred is [op,:r] =>
    op in '(AND and %and OR or %or NOT not %not) => 
      or/[isHasDollarPred x for x in r]
    op in '(HasCategory HasAttribute) => first r is '$
  false

stripOutNonDollarPreds pred ==
  pred is [op,:r] and op in '(AND and %and OR or %or NOT not %not) => 
    "append"/[stripOutNonDollarPreds x for x in r]
  not isHasDollarPred pred => [pred]
  nil

removeAttributePredicates pl ==
  [fn p for p in pl] where
    fn p ==
      p is [op,:argl] and op in '(AND and %and OR or %or NOT not %not) => 
          makePrefixForm(fnl argl,op)
      p is ["has",'$,['ATTRIBUTE,a]] =>
        sayBrightlyNT '"Predicate: "
        PRINT p
        sayBrightlyNT '"  replaced by: "
        PRINT LASSOC(a,$NRTattributeAlist)
      p
    fnl p == [fn x for x in p]
 
transHasCode(x,e) ==
  x isnt [.,:.] => x
  op := x.op
  op in '(HasCategory HasAttribute) => x
  op="has" => compHasFormat(x,e)
  [transHasCode(y,e) for y in x]
 
mungeAddGensyms(u,gal) ==
  ['%list,:[fn(x,gal,0) for x in u]] where fn(x,gal,n) ==
    x isnt [.,:.] => x
    g := LASSOC(x,gal) =>
      n = 0 => ["%LET",g,x]
      g
    [first x,:[fn(y,gal,n + 1) for y in rest x]]
 
orderByContainment pl ==
  null pl or null rest pl => pl
  max := first pl
  for x in rest pl repeat
    if (y := CONTAINED(max,x)) then
      if null assoc(max,$predGensymAlist)
      then $predGensymAlist := [[max,:gensym()],:$predGensymAlist]
    else if CONTAINED(x,max)
         then if null assoc(x,$predGensymAlist) then $predGensymAlist := [[x,:gensym()],:$predGensymAlist]
    if y then max := x
  [max,:orderByContainment remove(pl,max)]
 
buildBitTable(:l) == fn(reverse l,0) where fn(l,n) ==
  null l => n
  n := n + n
  if first l then n := n + 1
  fn(rest l,n)
 
buildPredVector(init,n,l) == fn(init,2 ** n,l) where fn(acc,n,l) ==
  null l => acc
  if first l then acc := acc + n
  fn(acc,n + n,rest l)

testBitVector(vec,i) ==
--bit vector indices are always 1 larger than position in vector
  i = 0 => true
  LOGBITP(i - 1,vec)
 
bitsOf n ==
  n = 0 => 0
  1 + bitsOf(n quo 2)
 
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
  -- this may someday not write nil keys, but it will now
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
  null fnStream => nil
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
      for [arg,count,:inst] in tableValue($ConstructorCache,key) repeat
        isNestedInstantiation(inst.0,deps) =>
          HREMPROP($ConstructorCache,key,arg)
 
isNestedInstantiation(form,deps) ==
  form is [op,:argl] =>
    symbolMember?(op,deps) => true
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
  $PrintOnly => nil
  property(libName,'LOADED) => nil
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
  u := getConstructorModemap cname
  updateCategoryTable(cname,kind)
  coSig :=
      u =>
          [[.,:sig],:.] := u
          [nil,:[categoryForm?(x) for x in rest sig]]
      nil
  property(cname,'LOADED) := fullLibName
  if $InteractiveMode then $CategoryFrame := $EmptyEnvironment
  stopTimingProcess 'load
  'T

loadLibNoUpdate(cname, libName, fullLibName) ==
  kind := getConstructorKindFromDB cname
  if $printLoadMsgs then
    sayKeyedMsg("S2IL0002",[namestring fullLibName,kind,cname])
  loadModule(fullLibName,cname)
  clearConstructorCache cname
  installConstructor(cname,kind)
  property(cname,'LOADED) := fullLibName
  if $InteractiveMode then $CategoryFrame := $EmptyEnvironment
  stopTimingProcess 'load
  'T
 
loadIfNecessary u == loadLibIfNecessary(u,true)
 
loadIfNecessaryAndExists u == loadLibIfNecessary(u,nil)
 
loadLibIfNecessary(u,mustExist) ==
  u is '$EmptyMode => u
  cons? u => loadLibIfNecessary(first u,mustExist)
  value:=
    functionp(u) or macrop(u) => u
    property(u,'LOADED) => u
    loadLib u => u
  null $InteractiveMode and ((null (y:= getProplist(u,$CategoryFrame)))
    or (null symbolLassoc('isFunctor,y)) and (null symbolLAssoc('isCategory,y))) =>
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
   [[dc,:sig],[pred,impl]] := getConstructorModemap constructor
   $CategoryFrame := put(constructor,'isFunctor,
       convertOpAlist2compilerInfo(opAlist),
       addModemap(constructor, dc, sig, pred, impl,
           put(constructor, 'mode, ['Mapping,:sig], $CategoryFrame)))

updateCategoryFrameForCategory(category) ==
   [[dc,:sig],[pred,impl]] := getConstructorModemap category
   $CategoryFrame :=
     put(category, 'isCategory, 'T,
         addModemap(category, dc, sig, pred, impl, $CategoryFrame))

loadFunctor u ==
  cons? u => loadFunctor first u
  loadLibIfNotLoaded u
  u
 
makeConstructorsAutoLoad() ==
  for cnam in allConstructors() repeat
    builtinCategoryName? cnam => nil
    property(cnam,'LOADED) := nil
    systemDependentMkAutoload(getConstructorAbbreviationFromDB cnam,cnam)
 
systemDependentMkAutoload(fn,cnam) ==
    FBOUNDP cnam => "next"
    symbolFunction(cnam) := mkAutoLoad cnam

mkAutoLoad ctor ==
  function((:args) +-> (autoLoad ctor; apply(ctor,args)))

autoLoad cname ==
  -- builtin constructors are always loaded.  By definition, there
  -- is no way to unload them and load them again.
  builtinConstructor? cname => cname
  if constructorDB cname = nil then
    makeDB cname
  if property(cname,'LOADED) = nil then
    loadLib cname
  symbolFunction cname

setAutoLoadProperty(name) ==
  property(name,'LOADED) := nil
  symbolFunction(name) := mkAutoLoad name

unloadOneConstructor cnam ==
    property(cnam,'LOADED) := nil
    symbolFunction(cnam) := mkAutoLoad cnam
    --FIXME: should not we clear other fields too?

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
  $lisplibPredicates: local := nil
  $lisplibParents: local := nil
  $lisplibAncestors: local := nil
  $lisplibModemapAlist: local := nil
  $lisplibSlot1 : local := nil   --used by NRT mechanisms
  $lisplibOperationAlist: local := nil
  $lisplibOpAlist: local:= nil
  $libFile: local := nil
  $lisplibVariableAlist: local := nil
  $lisplibSignatureAlist: local := nil
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
  sayMSG fillerSpaces(72,char "-")
  $LISPLIB: local := 'T
  $op: local := op
  $lisplibPredicates: local := nil -- set by makePredicateBitVector
  $lisplibParents: local := nil
  $lisplibAncestors: local := nil
  $lisplibModemapAlist: local := nil
  $lisplibSlot1 : local := nil   -- used by NRT mechanisms
  $lisplibOperationAlist: local := nil
  $lisplibOpAlist: local := nil  --operations alist for new runtime system
  $lisplibSignatureAlist: local := nil
  $libFile: local := nil
  $lisplibVariableAlist: local := nil
--  $lisplibRelatedDomains: local := nil   --from ++ Related Domains: see c-doc
  $lisplibCategory: local := nil        
  --for categories, is rhs of definition; otherwise, is target of functor
  --will eventually become the "constructorCategory" property in lisplib
  --set in compDefineCategory1 if category, otherwise in finalizeLisplib
  libName := getConstructorAbbreviation op
  $compileDocumentation => compileDocumentation(op,libName)
  sayMSG ['"   initializing ",$spadLibFT,:bright libName,
    '"for",:bright op]
  initializeLisplib libName
  sayMSG ['"   compiling into ",$spadLibFT,:bright libName]
  -- res:= FUNCALL(fn,df,m,e,prefix,fal)
  -- sayMSG ['"   finalizing ",$spadLibFT,:bright libName]
  -- finalizeLisplib libName
  -- following guarantee's compiler output files get closed.
  ok := false;
  try
    res:= FUNCALL(fn,df,m,e,prefix,fal)
    leaveIfErrors(libName,dbConstructorKind constructorDB $op)
    sayMSG ['"   finalizing ",$spadLibFT,:bright libName]
    ok := finalizeLisplib(op,libName)
  finally RSHUT $libFile
  if ok then lisplibDoRename(libName)
  filearg := $FILEP(libName,$spadLibFT,$libraryDirectory)
  RPACKFILE filearg
  FRESH_-LINE $algebraOutputStream
  sayMSG fillerSpaces(72,char "-")
  unloadOneConstructor op
  LOCALDATABASE([symbolName getConstructorAbbreviationFromDB op],nil)
  $newConlist := [op, :$newConlist]  ---------->  bound in function "compiler"
  res
 
compileDocumentation(ctor,libName) ==
  filename := MAKE_-INPUT_-FILENAME(libName,$spadLibFT)
  $FCOPY(filename,[libName,'DOCLB])
  stream := RDEFIOSTREAM [['FILE,libName,'DOCLB],['MODE, :'O]]
  lisplibWrite('"documentation",finalizeDocumentation ctor,stream)
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

mkCtorDBForm ctor ==
  ['constructorDB,quoteForm ctor]

writeInfo(ctor,info,key,prop,file) ==
  if info ~= nil then
    insn := ['%store,[prop,mkCtorDBForm ctor],quoteForm info]
    LAM_,FILEACTQ(key,expandToVMForm insn)
  lisplibWrite(symbolName key,info,file)
  
writeKind(ctor,kind,file) ==
  writeInfo(ctor,kind,'constructorKind,'dbConstructorKind,file)

writeConstructorForm(ctor,form,file) ==
  writeInfo(ctor,form,'constructorForm,'dbConstructorForm,file)

writeSuperDomain(ctor,domPred,file) ==
  writeInfo(ctor,domPred,'superDomain,'dbSuperDomain,file)

writeOperations(ctor,ops,file) ==
  writeInfo(ctor,ops,'operationAlist,'dbOperations,file)

writeConstructorModemap(ctor,mm,file) ==
  writeInfo(ctor,mm,'constructorModemap,'dbConstructorModemap,file)

++ If compilation produces an error, issue inform user and
++ return to toplevel reader.
leaveIfErrors(libName,kind) ==
  errorCount() ~= 0 =>
    sayMSG ['"   Errors in processing ",kind,'" ",:bright libName,'":"]
    sayMSG ['"     not replacing ",$spadLibFT,'" for",:bright libName]
    spadThrow()

++ Finalize `libName' compilation; returns true if everything is OK.
finalizeLisplib(ctor,libName) ==
  db := constructorDB ctor
  kind := dbConstructorKind constructorDB ctor
  form := dbConstructorForm constructorDB ctor
  mm := getConstructorModemap ctor
  writeConstructorForm(ctor,form,$libFile)
  writeKind(ctor,kind,$libFile)
  writeConstructorModemap(ctor,removeZeroOne mm,$libFile)
  $lisplibCategory := $lisplibCategory or mm.mmTarget
  -- set to target of mm for package/domain constructors;
  -- to the right-hand sides (the definition) for category constructors
  lisplibWrite('"constructorCategory",$lisplibCategory,$libFile)
  lisplibWrite('"sourceFile",namestring _/EDITFILE,$libFile)
  lisplibWrite('"modemaps",removeZeroOne $lisplibModemapAlist,$libFile)
  opsAndAtts := getConstructorOpsAndAtts(form,kind,mm)
  writeOperations(ctor,removeZeroOne first opsAndAtts,$libFile)
  if kind='category then
     $pairlis : local := pairList(form,$FormalMapVariableList)
     $NRTslot1PredicateList : local := []
     NRTgenInitialAttributeAlist(db,rest opsAndAtts)
  writeSuperDomain(ctor,dbSuperDomain constructorDB ctor,$libFile)
  lisplibWrite('"signaturesAndLocals",
    removeZeroOne mergeSignatureAndLocalVarAlists($lisplibSignatureAlist,
                                    $lisplibVariableAlist),$libFile)
  lisplibWrite('"attributes",removeZeroOne dbAttributes db,$libFile)
  lisplibWrite('"predicates",removeZeroOne  $lisplibPredicates,$libFile)
  lisplibWrite('"abbreviation",dbAbbreviation constructorDB ctor,$libFile)
  lisplibWrite('"parents",removeZeroOne $lisplibParents,$libFile)
  lisplibWrite('"ancestors",removeZeroOne $lisplibAncestors,$libFile)
  lisplibWrite('"documentation",finalizeDocumentation ctor,$libFile)
  lisplibWrite('"slot1Info",removeZeroOne $lisplibSlot1,$libFile)
  if $profileCompiler then profileWrite()
  leaveIfErrors(libName,kind)
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
  u isnt [.,:.] => []
  first u isnt [.,:.] =>
    answer:="union"/[Operators v for v in rest u]
    symbolMember?(first u,answer) => answer
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
    nil
  (sig := getConstructorSignature domainName) =>
    [.,target,:argMml] := sig
    for a in $FormalMapVariableList for m in argMml repeat
      $e:= put(a,'mode,m,$e)
    t := compMakeCategoryObject(target,$e) or
      systemErrorHere ["getSlot1",domainName]
    categoryExports t.expr
  sayKeyedMsg("S2IL0022",[namestring p,'"constructor modemap"])
  nil
 
transformOperationAlist operationAlist ==
  --  this transforms the operationAlist which is written out onto LISPLIBs.
  --  The original form of this list is a list of items of the form:
  --        ((<op> <signature>) (<condition> (ELT $ n)))
  --  The new form is an op-Alist which has entries (<op> . signature-Alist)
  --      where signature-Alist has entries (<signature> . item)
  --        where item has form (<slotNumber> <condition> <kind>)
  --          where <kind> =
  --             nil  => function
  --             CONST => constant ... and others
  newAlist:= nil
  for [[op,sig,:.],condition,implementation] in operationAlist repeat
    kind:=
      implementation is [eltEtc,.,n] and eltEtc in '(CONST ELT) => eltEtc
      implementation is [impOp,:.] =>
        impOp is 'XLAM => implementation
        impOp in '(CONST Subsumed) => impOp
        keyedSystemError("S2IL0025",[impOp])
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
  null categoryExports domain =>
    getSlotNumberFromOperationAlist(domainForm,op,sig)
  sayMSG ['"   using slot 1 of ",domainForm]
  constructorArglist:= rest domainForm
  nsig:=#sig
  tail:= or/[r for [[op1,sig1],:r] in categoryExports domain | op=op1 and nsig=#sig1 and
    "and"/[compare for a in sig for b in sig1]] where compare() ==
      a=b => true
      integer? b => a=constructorArglist.b
      isSubset(bustUnion a,bustUnion b,$CategoryFrame)
  tail is [.,["ELT",.,n]] => n
  systemErrorHere ["findConstructorSlotNumber",domainForm]
 
bustUnion d ==
  d is ["Union",domain,'"failed"] => domain
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
      integer? item1 => item = domainForm.item1       --item1=n means nth arg
      isSubset(bustUnion item1,bustUnion item,$CategoryFrame)
    null partsMatch => return nil
    sig:= rest sig; sig1 := rest sig1
  sig or sig1 => nil
  true
 
findDomainSlotNumber(domain,op,sig) == --using slot 1 of the domain
  nsig:=#sig
  tail:= or/[r for [[op1,sig1],:r] in categoryExports domain | op=op1 and nsig=#sig1 and
    "and"/[a=b or isSubset(bustUnion a,bustUnion b,$CategoryFrame)
      for a in sig for b in sig1]]
  tail is [.,["ELT",.,n]] => n
  systemErrorHere '"findDomainSlotNumber"
 

getConstructorSignature: %Symbol -> %Form
getConstructorSignature ctor ==
  ([[.,:sig],:.] := getConstructorModemap ctor) => sig
  -- If we have a local or forward declaration take it.
  -- Note: constructors are not overloadable.
  rest getmode(ctor,$e)
 
getSlotFromCategoryForm (x,index) ==
  u:= eval [x.op,:[MKQ f for f in $FormalMapVariableList for . in 1..#x.args]]
  not vector? u =>
    systemErrorHere '"getSlotFromCategoryForm"
  u . index
 
 
isDomainForm(D,e) ==
  --added for MPOLY 3/83 by RDJ
  symbolMember?(KAR D,$SpecialDomainNames) or isFunctor D or
    -- ((D is ['Mapping,target,:.]) and isCategoryForm(target,e)) or
     ((getmode(D,e) is ['Mapping,target,:.]) and isCategoryForm(target,e)) or
       isCategoryForm(getmode(D,e),e) or isDomainConstructorForm(D,e)
 
isDomainConstructorForm(D,e) ==
  D is [op,:argl] and (u:= get(op,"value",e)) and
    u is [.,["Mapping",target,:.],:.] and
      isCategoryForm(applySubst(pairList($FormalMapVariableList,argl),target),e)
 
isFunctor x ==
  op:= opOf x
  not ident? op => false
  $InteractiveMode =>
    builtinFunctorName? op => true
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

getAllIndexPathnames: %String -> %List %Thing
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
  

getAllAldorObjectFiles: %String -> %List %Thing
getAllAldorObjectFiles dir ==
  asys := DIRECTORY strconc(dir,'"*.asy")
  asos := DIRECTORY strconc(dir,'"*.ao")
  -- don't include both a `x.asy' and `x.ao', and retain
  -- only sensical .asy files.
  dupAOs := MAPCAN(function PATHNAME_-NAME,asys)
  [asys,[f for f in asos 
          | PATHNAME_-NAME f is '"ao" and not member(PATHNAME_-NAME f,dupAOs)]]
    


++ returns an open stream for the index file, if present, 
++ in directory designated by 'dir'.
openIndexFileIfPresent: %String -> %Thing
openIndexFileIfPresent dir ==
  inputTextFile getIndexPathname dir

++
getIndexTable: %String -> %Thing
getIndexTable dir ==
  indexFile := getIndexPathname dir
  existingFile? indexFile =>
    try
      stream := inputTextFile indexFile
      GET_-INDEX_-TABLE_-FROM_-STREAM stream
    finally (if stream ~= nil then closeStream stream)
  -- index file doesn't exist but mark this directory as a Lisplib.
  try stream := outputTextFile indexFile
  finally (if stream ~= nil then closeStream stream)

--%
compDefineExports(form,ops,sig,e) ==
  not $LISPLIB => systemErrorHere "compDefineExports"
  op := first form
  -- Ensure constructor parameters appear as formals
  sig := applySubst($pairlis, sig)
  ops := applySubst($pairlis,ops)
  form := applySubst($pairlis,form)
  -- In case we are not compiling the capsule, the slot numbers are
  -- most likely bogus.  Nullify them so people don't think they
  -- bear any meaningful semantics (well, they should not think
  -- these are forwarding either).
  if $compileExportsOnly then
    for entry in ops repeat
      fixupSigloc entry where
        fixupSigloc entry ==
          [opsig,pred,funsel] := entry
          if pred isnt 'T then 
            entry.rest.first := simpBool pred
          funsel is [op,a,:.] and op in '(ELT CONST) =>
            entry.rest.rest.first := [op,a,nil]
    ops := listSort(function GGREATERP, ops, function first)
  libName := getConstructorAbbreviation op
  exportsFile := strconc(symbolName libName,'".sig")
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
