-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2013, Gabriel Dos Reis.
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


import database
import debug

namespace BOOT
module lisplib

--=======================================================================
--                Generate Slot 2 Attribute Alist
--=======================================================================
genInitialAttributeAlist(db,attributeList) ==
  --alist has form ((item pred)...) where some items are constructor forms
  alist := [x for x in attributeList | -- throw out constructors
              constructorDB opOf first x = nil]
  dbAttributes(db) := simplifyAttributeAlist(db,
    [[a,:b] for [a,b] in dbSubstituteFormals(db,alist) | a isnt 'nothing])

simplifyAttributeAlist(db,al) ==
  al is [[a,:b],:r] =>
    u := [x for x in r | x is [=a,:b]] 
    null u => [first al,:simplifyAttributeAlist(db,rest al)]
    pred := simpBool makePrefixForm([b,:ASSOCRIGHT u],'OR)
    $NRTslot1PredicateList := insert(pred,$NRTslot1PredicateList)
    s := [x for x in r | x isnt [=a,:b]]
    [[a,:pred],:simplifyAttributeAlist(db,s)]
  nil
 
genFinalAttributeAlist(db,e) ==
  [[a,:k] for [a,:b] in $NRTattributeAlist
     | (k := predicateBitIndex(db,b,e)) ~= -1]
 
predicateBitIndex(db,x,e) == 
  pn(db,x,false,e) where
    pn(db,x,flag,e) ==
      u := simpBool transHasCode(db,x,e)
      u is 'T  =>  0
      u is false => -1
      p := valuePosition(u,$NRTslot1PredicateList) => p + 1
      not flag => pn(db,predicateBitIndexRemop x,true,e)
      systemError nil

predicateBitIndexRemop p==
--transform attribute predicates taken out by removeAttributePredicates
  p is [op,:argl] and op in '(AND and %and OR or %or NOT not %not) => 
    simpBool makePrefixForm([predicateBitIndexRemop x for x in argl],op)
  p is ["has",'$,['ATTRIBUTE,a]] => LASSOC(a,$NRTattributeAlist)
  p
 
predicateBitRef(db,x,e) ==
  x is 'T => 'T
  ['testBitVector,'pv_$,predicateBitIndex(db,x,e)]
 
makePrefixForm(u,op) ==
  u := MKPF(u,op)
  u is ''T => 'T
  u

--=======================================================================
--               Generate Slot 3 Predicate Vector
--=======================================================================
makePredicateBitVector(db,pl,e) ==   --called by buildFunctor
  if $insideCategoryPackageIfTrue then
    pl := union(pl,$categoryPredicateList)
  $predGensymAlist := nil --bound by buildFunctor, used by optHas
  for p in removeAttributePredicates pl repeat
    pred := simpBool transHasCode(db,p,e)
    pred isnt [.,:.] => 'skip                --skip over T and nil
    if isHasDollarPred pred then 
      lasts := insert(pred,lasts)
      for q in stripOutNonDollarPreds pred repeat firsts := insert(q,firsts)
    else 
      firsts := insert(pred,firsts)
  firstPl := dbSubstituteFormals(db,reverse! orderByContainment firsts)
  lastPl  := dbSubstituteFormals(db,reverse! orderByContainment lasts)
  firstCode:= 
    ['buildPredVector,0,0,mungeAddGensyms(firstPl,$predGensymAlist)]
  lastCode := augmentPredCode(# firstPl,lastPl)
  dbPredicates(db) := [:firstPl,:lastPl]
  [dbPredicates db,firstCode,:lastCode]

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
      any?(function isHasDollarPred,r)
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
 
transHasCode(db,x,e) ==
  x isnt [.,:.] => x
  op := x.op
  op in '(HasCategory HasAttribute) => x
  op="has" => compHasFormat(db,x,e)
  [transHasCode(db,y,e) for y in x]
 
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
 
writeLib(fn,ft) == RDEFIOSTREAM [['FILE,fn,ft,nil],'(MODE . OUTPUT)]
 
lisplibWrite(prop,val,filename) ==
  -- this may someday not write nil keys, but it will now
  rwrite(prop,val,filename)
 
--% Loading

++ Return a path to the loadable module that contains the definition
++ of the constructor indicated by `db' if it exists.  Otherwise return nil.
dbLocateModule db ==
  m :=
    $buildingSystemAlgebra =>
      getSystemModulePath symbolName dbAbbreviation db
    getConstructorModuleFromDB dbConstructor db
  existingFile? m => m
  strap := algebraBootstrapDir() =>
    m := strconc(strap,PATHNAME_-NAME m,'".",$faslType)
    existingFile? m => m
    nil
  nil

++ Return a path to the loadable module that contains the
++ definition of the constructor indicated by `cname'.  
++ Error if the file container of the module does not exist.
findModule db ==
  dbLocateModule db
    or systemError ['"missing module for ",:bright dbConstructor db]
 
loadLibIfNotLoaded libName ==
  -- loads is library is not already loaded
  $PrintOnly => nil
  db := constructorDB libName or return nil
  dbLoaded? db => nil
  loadLib libName
 
loadLib cname ==
  builtinConstructor? cname => nil  -- these don't have nrlib yet.
  startTimingProcess 'load
  db := constructorDB cname or return nil
  fullLibName := findModule db
  systemdir? := isSystemDirectory(pathnameDirectory fullLibName)
  update? := $forceDatabaseUpdate or not systemdir? 
  not update? =>
     loadLibNoUpdate(cname, cname, fullLibName)
  kind := dbConstructorKind db
  if $printLoadMsgs then
    sayKeyedMsg("S2IL0002",[namestring fullLibName,kind,cname])
  loadModule(fullLibName,cname)
  clearConstructorCache cname
  updateDatabase(cname,cname,systemdir?)
  installConstructor(cname,kind)
  updateCategoryTable(cname,kind)
  dbLoadPath(db) := fullLibName
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
  dbLoadPath(constructorDB cname) := fullLibName
  if $InteractiveMode then $CategoryFrame := $EmptyEnvironment
  stopTimingProcess 'load
  'T
 
loadIfNecessary u ==
  u is '$EmptyMode => u
  cons? u => loadIfNecessary first u
  value:=
    functionp(u) or macrop(u) => u
    dbLoaded? constructorDB u => u
    loadLib u => u
  not $InteractiveMode and (null (y:= getProplist(u,$CategoryFrame))
    or (null symbolTarget('isFunctor,y)) and (null symbolTarget('isCategory,y))) =>
      y:= getConstructorKindFromDB u =>
         y = "category" =>
            updateCategoryFrameForCategory u
         updateCategoryFrameForConstructor u
      throwKeyedMsg("S2IL0005",[u])
  value

genericLoadDB(db,f) ==
  try
    startTimingProcess 'load
    dbBeingDefined? db => nil
    dbLoaded? db => db
    ctor := dbConstructor db
    builtinConstructor? ctor => nil
    lib := apply(f,db,nil) or return nil
    loadModule(lib,ctor)
    dbLoadPath(db) := lib
    db
  finally stopTimingProcess 'load

++ Load the module associated with `db' and return the module's path.
loadDB db ==
  genericLoadDB(db,function findModule)

loadDBIfCan db ==
  dbLoaded? db => db
  genericLoadDB(db,function dbLocateModule)
 
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

makeConstructorsAutoLoad() ==
  for cnam in allConstructors() repeat
    builtinCategoryName? cnam => nil
    if db := constructorDB cnam then
      dbLoadPath(db) := nil
    systemDependentMkAutoload(getConstructorAbbreviationFromDB cnam,cnam)
 
systemDependentMkAutoload(fn,cnam) ==
    functionSymbol? cnam => "next"
    symbolFunction(cnam) := mkAutoLoad cnam

mkAutoLoad ctor ==
  function((:args) +-> (autoLoad ctor; apply(ctor,args)))

autoLoad cname ==
  -- builtin constructors are always loaded.  By definition, there
  -- is no way to unload them and load them again.
  builtinConstructor? cname => cname
  db := constructorDB cname or makeDB cname
  if not dbLoaded? db then
    loadLib cname
  symbolFunction cname

setAutoLoadProperty name ==
  if db := constructorDB name then
    dbLoadPath(db) := nil
  symbolFunction(name) := mkAutoLoad name

unloadOneConstructor cnam ==
  setAutoLoadProperty cnam
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
  $lisplibOperationAlist: local := nil
  if cons? fun and null rest fun then fun:= first fun -- unwrap nullary
  libName:= getConstructorAbbreviation fun
  infile:= infileOrNil or getFunctionSourceFile fun or
    throwKeyedMsg("S2IL0004",[fun])
  $editFile := infile
  outfile := outfileOrNil or [libName,'OUTPUT,nil]   --always QUIET
  removeFile makeFullFilePath [libName,'OUTPUT,nil]
  outstream:= DEFSTREAM(outfile,'OUTPUT)
  val:= _/D_,2_,LIB(fun,infile,outstream,auxOp,editFlag,traceFlag)
  val
 
compDefineLisplib(db,df:=["DEF",[op,:.],:.],m,e,fal,fn) ==
  --fn= compDefineCategory1 OR compDefineFunctor1
  sayMSG fillerSpaces(72,char "-")
  $op: local := op
  $lisplibOperationAlist: local := nil
  --for categories, is rhs of definition; otherwise, is target of functor
  --will eventually become the "constructorCategory" property in lisplib
  --set in compDefineCategory1 if category, otherwise in finalizeLisplib
  libName := dbAbbreviation db
  if dbSourceFile db = nil then
    dbSourceFile(db) := namestring $editFile
  $compileDocumentation => compileDocumentation(db,libName)
  sayMSG ['"   initializing stationary directory for",:bright op]
  lib := initializeLisplib(db,libName)
  sayMSG ['"   compiling into ",:bright libStationaryDirname lib]
  -- following guarantees compiler output files get closed.
  try
    res:= apply(fn,[db,df,m,e,fal])
    leaveIfErrors(libName,dbConstructorKind db)
    sayMSG ['"   finalizing ",$spadLibFT,:bright libName]
    finalizeLisplib(db,libName)
    RECOMPILE_-LIB_-FILE_-IF_-NECESSARY
      -- Let's close the code stream before handing over to the backend
      st := libCodeStream lib
      closeStream st
      filePath st
  finally
    RSHUT dbLibstream db
  lisplibDoRename db
  sayMSG fillerSpaces(72,char "-")
  unloadOneConstructor op
  $buildingSystemAlgebra => res
  LOCALDATABASE([symbolName dbAbbreviation db],nil)
  $newConlist := [op, :$newConlist]  ---------->  bound in function "compiler"
  res
 
compileDocumentation(db,libName) ==
  filename := libDirname dbLibstream db
  $FCOPY(filename,[libName,'DOCLB])
  stream := RDEFIOSTREAM [['FILE,libName,'DOCLB],['MODE, :'O]]
  lisplibWrite('"documentation",finalizeDocumentation db,stream)
  RSHUT(stream)
  RPACKFILE([libName,'DOCLB])
  $REPLACE([libName,$spadLibFT],[libName,'DOCLB])
  ['dummy, $EmptyMode, $e]

initializeLisplib(db,libName) ==
  resetErrorCount()
  tmpdir := acquireTemporaryPathname()
  removeFile tmpdir
  checkMkdir tmpdir
  [stream,:table] := getIndexIOStreamAndTable tmpdir
  lsp := outputTextFile strconc(tmpdir,'"/code.lsp")
  libdir := makeFullFilePath [libName,$spadLibFT,nil]
  dbLibstream(db) := mk%Libstream('OUTPUT,libdir,table,stream,lsp,nil,tmpdir)


mkCtorDBForm db ==
  ['constructorDB,quote dbConstructor db]

writeInfo(db,info,key,prop) ==
  if info ~= nil then
    insn := ['%store,[prop,mkCtorDBForm db],quote info]
    printBackendStmt(dbLibstream db,expandToVMForm insn)
  lisplibWrite(symbolName key,info,dbLibstream db)

++ Like writeInfo, but only write to the load unit.
writeLoadInfo(db,info,key,prop) ==
  info = nil => nil
  insn := ['%store,[prop,mkCtorDBForm db],info]
  printBackendStmt(dbLibstream db,expandToVMForm insn)

writeTemplate db ==
  dbConstructorKind db = 'category => nil
  writeLoadInfo(db,dbTemplate db,'template,'dbTemplate)

writeLookupFunction db ==
  fun := dbLookupFunction db =>
    writeLoadInfo(db,quote fun,'lookupFunction,'dbLookupFunction)
  nil

writeCategoryDefault db ==
  pac := dbConstructorDefault db
  insn := ['%store,['dbConstructorDefault,mkCtorDBForm db],quote pac]
  printBackendStmt(dbLibstream db,expandToVMForm insn)

writeKind db ==
  writeInfo(db,dbConstructorKind db,'constructorKind,'dbConstructorKind)

writeAbbreviation db ==
  writeInfo(db,dbAbbreviation db,'abbreviation,'dbAbbreviation)

writeConstructorForm db ==
  writeInfo(db,dbConstructorForm db,'constructorForm,'dbConstructorForm)

writeCategory db ==
  writeInfo(db,dbCategory db,'constructorCategory,'dbCategory)

writeSourceFile db ==
  writeInfo(db,dbSourceFile db,'sourceFile,'dbSourceFile)

writeInteractiveModemaps db ==
  -- Note: these are never used when building the system algebra.
  --       But we need them when building the complete set of databases
  writeInfo(db,dbModemaps db,'modemaps,'dbModemaps)

writeDocumentation(db,doc) ==
  -- FIXME: Do we want to store documentation in object files?
  lisplibWrite('"documentation",doc,dbLibstream db)

writeSuperDomain db ==
  writeInfo(db,dbSuperDomain db,'superDomain,'dbSuperDomain)

writePredicates db ==
  writeInfo(db,dbPredicates db,'predicates,'dbPredicates)

writeOperations(db,ops) ==
  writeInfo(db,ops,'operationAlist,'dbOperations)

writeAttributes db ==
 writeInfo(db,dbAttributes db,'attributes,'dbAttributes)

writeConstructorModemap db ==
  writeInfo(db,dbConstructorModemap db,
    'constructorModemap,'dbConstructorModemap)

writeDualSignature db ==
  writeInfo(db,dbDualSignature db,'dualSignature,'dbDualSignature)

writeAncestors db ==
  writeInfo(db,dbAncestors db,'ancestors,'dbAncestors)

writePrincipals db ==
  writeInfo(db,dbPrincipals db,'parents,'dbPrincipals)

writeCapsuleLevelDefinitions db ==
  writeInfo(db,dbCapsuleDefinitions db,
    'signaturesAndLocals,'dbCapsuleDefinitions)

++ If compilation produces an error, issue inform user and
++ return to toplevel reader.
leaveIfErrors(libName,kind) ==
  errorCount() ~= 0 =>
    sayMSG ['"   Errors in processing ",kind,'" ",:bright libName,'":"]
    sayMSG ['"     not replacing ",$spadLibFT,'" for",:bright libName]
    spadThrow()

++ Finalize `libName' compilation; returns true if everything is OK.
finalizeLisplib(db,libName) ==
  form := dbConstructorForm db
  writeTemplate db
  do   -- shared slot; careful.
    dbConstructorKind db = 'category => writeCategoryDefault db
    writeLookupFunction db
  writeConstructorForm db
  writeKind db
  writeConstructorModemap db
  writeDualSignature db
  -- set to target of dbConstructorModemap for package/domain constructors;
  -- to the right-hand sides (the definition) for category constructors
  if dbConstructorKind db = 'category then
    writeCategory db
  writeSourceFile db
  writeInteractiveModemaps db
  opsAndAtts :=
    dbConstructorKind db = 'category => getCategoryOpsAndAtts db
    getFunctorOpsAndAtts db
  writeOperations(db,first opsAndAtts)
  if dbConstructorKind db = 'category then
     $NRTslot1PredicateList : local := []
     genInitialAttributeAlist(db,rest opsAndAtts)
  writeSuperDomain db
  writeCapsuleLevelDefinitions db
  writeAttributes db
  writePredicates db
  writeAbbreviation db
  writePrincipals db
  writeAncestors db
  if not $bootStrapMode then
    writeDocumentation(db,finalizeDocumentation db)
  if $profileCompiler then profileWrite dbLibstream db
  leaveIfErrors(libName,dbConstructorKind db)
  true

lisplibDoRename db ==
  lib := dbLibstream db
  output := dbOutputPath db =>
    modpath := filePathString
      makeFilePath(type <- $faslType,defaults <- filePath libCodeStream lib)
    do
      renameFile(modpath,output) = 0 => nil
      copyFile(modpath,output)
      removeFile libStationaryDirname lib
    output
  removeFile libDirname lib
  renameFile(libStationaryDirname lib,libDirname lib) = 0 => libDirname lib
  moveLibdirByCopy lib
 
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
 
getCategoryOpsAndAtts db ==
  catForm := dbConstructorForm db
  -- returns [operations,:attributes] of first catForm
  [transformOperationAlist getSlotFromCategoryForm(catForm,1),
    :getSlotFromCategoryForm(catForm,2)]
 
getFunctorOpsAndAtts db ==
  [transformOperationAlist $lisplibOperationAlist,
    :getSlotFromFunctor dbConstructorModemap(db).mmTarget]
 
getSlotFromFunctor(target) ==
  t := compMakeCategoryObject(target,$e) or
      systemErrorHere "getSlotFromFunctor"
  categoryRef(t.expr,2)
 
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
    itemList := [[sig,n,condition,kind],:symbolTarget(op,newAlist)]
    newAlist := insertAlist(op,itemList,newAlist)
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
  entryList:= symbolTarget(op,operationAlist) or return nil
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
  categoryRef(u,index)
 
 
isDomainForm(D,e) ==
  op := opOf D
  not ident? op => false
  op is '%when => and/[isDomainForm(e,c) for [.,c] in D.args]
  --db := constructorDB op => dbConstructorKind db in '(domain package)
  symbolMember?(op,$SpecialDomainNames) or isFunctor op or
     ((getmode(op,e) is ['Mapping,target,:.]) and isCategoryForm(target,e)) or
       isCategoryForm(getmode(op,e),e) or isDomainConstructorForm(D,e)
 
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
  u := get(op,'isFunctor,$CategoryFrame) => u
  builtinFunctorName? op => true
  kind := getConstructorKindFromDB op
  kind = nil or kind = 'category => false
  updateCategoryFrameForConstructor op
  get(op,'isFunctor,$CategoryFrame)

--%

getIndexPathname: %String -> %String
getIndexPathname dir ==
  strconc(ensureTrailingSlash dir, $IndexFilename)

getAllIndexPathnames: %String -> %List %Thing
getAllIndexPathnames dir ==
  -- GCL's semantics of Common Lisp's `DIRECTORY *' differs from
  -- everybody else's.  Namely, GCL would return a
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
