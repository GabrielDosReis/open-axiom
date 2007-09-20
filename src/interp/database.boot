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


SETANDFILEQ($getUnexposedOperations,true)

--% Functions for manipulating MODEMAP DATABASE

augLisplibModemapsFromCategory(form is [op,:argl],body,signature) ==
  sl := [["$",:"*1"],:[[a,:p] for a in argl
    for p in rest $PatternVariableList]]
  form:= SUBLIS(sl,form)
  body:= SUBLIS(sl,body)
  signature:= SUBLIS(sl,signature)
  opAlist:= SUBLIS(sl,$domainShell.(1)) or return nil
  nonCategorySigAlist:=
    mkAlistOfExplicitCategoryOps substitute("*1","$",body)
  domainList:=
    [[a,m] for a in rest form for m in rest signature |
      isCategoryForm(m,$EmptyEnvironment)]
  catPredList:= [['ofCategory,:u] for u in [["*1",form],:domainList]]
  for (entry:= [[op,sig,:.],pred,sel]) in opAlist |
    member(sig,LASSOC(op,nonCategorySigAlist)) repeat
      pred':= MKPF([pred,:catPredList],'AND)
      modemap:= [["*1",:sig],[pred',sel]]
      $lisplibModemapAlist:=
        [[op,:interactiveModemapForm modemap],:$lisplibModemapAlist]

augmentLisplibModemapsFromFunctor(form,opAlist,signature) ==
  form:= [formOp,:argl]:= formal2Pattern form
  opAlist:= formal2Pattern opAlist
  signature:= formal2Pattern signature
  for u in form for v in signature repeat
    if MEMQ(u,$PatternVariableList) then
      -- we are going to be EVALing categories containing these
      -- pattern variables
      $e:=put(u,'mode,v,$e)
  nonCategorySigAlist:=
    mkAlistOfExplicitCategoryOps first signature or return nil
  for (entry:= [[op,sig,:.],pred,sel]) in opAlist |
    or/[(sig in catSig) for catSig in
      allLASSOCs(op,nonCategorySigAlist)] repeat
        skip:=
          argl and CONTAINED("$",rest sig) => 'SKIP
          nil
        sel:= substitute(form,"$",sel)
        patternList:= listOfPatternIds sig
        --get relevant predicates
        predList:=
          [[a,m] for a in argl for m in rest signature
            | MEMQ(a,$PatternVariableList)]
        sig:= substitute(form,"$",sig)
        pred':= MKPF([pred,:[mkDatabasePred y for y in predList]],'AND)
        l:=listOfPatternIds predList
        if "OR"/[null MEMQ(u,l) for u in argl] then
          sayMSG ['"cannot handle modemap for",:bright op,
                          '"by pattern match" ]
          skip:= 'SKIP
        modemap:= [[form,:sig],[pred',sel,:skip]]
        $lisplibModemapAlist:= [[op,:interactiveModemapForm modemap],
          :$lisplibModemapAlist]

rebuildCDT(filemode) ==
  clearConstructorAndLisplibCaches()
  $databaseQueue:local :=nil
  $e: local := [[NIL]]    -- We may need to evaluate Categories
  buildDatabase(filemode,false)
  $IOindex:= 1
  $InteractiveFrame:= [[NIL]]
  0

buildDatabase(filemode,expensive) ==
  $InteractiveMode: local:= true
  $constructorList := nil       --looked at by buildLibdb
  $ConstructorCache:= MAKE_-HASHTABLE('ID)
  SAY '"Making constructor autoload"
  makeConstructorsAutoLoad()
  SAY '"Building category table"
  genCategoryTable()
  SAY '"Building libdb.text"
  buildLibdb()
  SAY '"splitting libdb.text"
  dbSplitLibdb()
  SAY '"creating browse constructor index"
  dbAugmentConstructorDataTable()
  SAY '"Building browse.lisp"
  buildBrowsedb()
  SAY '"Building constructor users database"
  mkUsersHashTable()
  SAY '"Saving constructor users database"
  saveUsersHashTable()
  SAY '"Building constructor dependents database"
  mkDependentsHashTable()
  SAY '"Saving constructor dependents database"
  saveDependentsHashTable()
  SAY '"Building glossary files"
  buildGloss()

saveUsersHashTable() ==
  _$ERASE('USERS,'DATABASE,'a)
  stream:= writeLib1('USERS,'DATABASE,'a)
  for k in MSORT HKEYS $usersTb repeat
    rwrite(k, HGET($usersTb, k), stream)
  RSHUT stream

saveDependentsHashTable() ==
  _$ERASE('DEPENDENTS,'DATABASE,'a)
  stream:= writeLib1('DEPENDENTS,'DATABASE,'a)
  for k in MSORT HKEYS $depTb repeat
    rwrite(k, HGET($depTb, k), stream)
  RSHUT stream

getUsersOfConstructor(con) ==
  stream := readLib1('USERS, 'DATABASE, 'a)
  val := rread(con, stream, nil)
  RSHUT stream
  val

getDependentsOfConstructor(con) ==
  stream := readLib1('DEPENDENTS, 'DATABASE, 'a)
  val := rread(con, stream, nil)
  RSHUT stream
  val

putModemapIntoDatabase(name,modemap,fileName) ==
  $forceAdd: local:= nil
  mml:= ASSOC(name,$databaseQueue)
  if mml = [] then
    $databaseQueue:=[[name, modemap],:$databaseQueue]
  else
    or/[modemap=map' for map' in CDR mml] => "already there"
    newEntry:= [modemap,:CDR mml]
    RPLACD(mml,newEntry)
  newEntry

orderPredicateItems(pred1,sig,skip) ==
  pred:= signatureTran pred1
  pred is ["AND",:l] => orderPredTran(l,sig,skip)
  pred

orderPredTran(oldList,sig,skip) ==
  lastPreds:=nil
  --(1) make two kinds of predicates appear last:
  -----  (op *target ..) when *target does not appear later in sig
  -----  (isDomain *1 ..)
  for pred in oldList repeat
    ((pred is [op,pvar,.] and MEMQ(op,'(isDomain ofCategory))
       and pvar=first sig and ^(pvar in rest sig)) or
        (^skip and pred is ['isDomain,pvar,.] and pvar="*1")) =>
          oldList:=delete(pred,oldList)
          lastPreds:=[pred,:lastPreds]
--sayBrightlyNT "lastPreds="
--pp lastPreds

  --(2a) lastDependList=list of all variables that lastPred forms depend upon
  lastDependList := "UNIONQ"/[listOfPatternIds x for x in lastPreds]
--sayBrightlyNT "lastDependList="
--pp lastDependList

  --(2b) dependList=list of all variables that isDom/ofCat forms depend upon
  dependList :=
    "UNIONQ"/[listOfPatternIds y for x in oldList |
      x is ['isDomain,.,y] or x is ['ofCategory,.,y]]
--sayBrightlyNT "dependList="
--pp dependList

  --(3a) newList= list of ofCat/isDom entries that don't depend on
  for x in oldList repeat
    if (x is ['ofCategory,v,body]) or (x is ['isDomain,v,body]) then
      indepvl:=listOfPatternIds v
      depvl:=listOfPatternIds body
    else
      indepvl := listOfPatternIds x
      depvl := nil
    (INTERSECTIONQ(indepvl,dependList) = nil)
        and INTERSECTIONQ(indepvl,lastDependList) =>
      somethingDone := true
      lastPreds := [:lastPreds,x]
      oldList := delete(x,oldList)
--if somethingDone then
--  sayBrightlyNT "Again lastPreds="
--  pp lastPreds
--  sayBrightlyNT "Again oldList="
--  pp oldList

  --(3b) newList= list of ofCat/isDom entries that don't depend on
  while oldList repeat
    for x in oldList repeat
      if (x is ['ofCategory,v,body]) or (x is ['isDomain,v,body]) then
        indepvl:=listOfPatternIds v
        depvl:=listOfPatternIds body
      else
        indepvl := listOfPatternIds x
        depvl := nil
      (INTERSECTIONQ(indepvl,dependList) = nil) =>
        dependList:= setDifference(dependList,depvl)
        newList:= [:newList,x]
--  sayBrightlyNT "newList="
--  pp newList

  --(4) noldList= what is left over
    (noldList:= setDifference(oldList,newList)) = oldList =>
--    sayMSG '"NOTE: Parameters to domain have circular dependencies"
      newList := [:newList,:oldList]
      return nil
    oldList:=noldList
--  sayBrightlyNT "noldList="
--  pp noldList

  for pred in newList repeat
    if pred is ['isDomain,x,y] or x is ['ofCategory,x,y] then
      ids:= listOfPatternIds y
      if and/[id in fullDependList for id in ids] then
        fullDependList:= insertWOC(x,fullDependList)
      fullDependList:= UNIONQ(fullDependList,ids)

  newList:=[:newList,:lastPreds]

--substitute (isDomain ..) forms as completely as possible to avoid false paths
  newList := isDomainSubst newList
  answer := [['AND,:newList],:INTERSECTIONQ(fullDependList,sig)]
--sayBrightlyNT '"answer="
--pp answer

isDomainSubst u == main where
  main ==
    u is [head,:tail] =>
      nhead :=
        head is ['isDomain,x,y] => ['isDomain,x,fn(y,tail)]
        head
      [nhead,:isDomainSubst rest u]
    u
  fn(x,alist) ==
    atom x =>
      IDENTP x and MEMQ(x,$PatternVariableList) and (s := findSub(x,alist)) => s
      x
    [CAR x,:[fn(y,alist) for y in CDR x]]
  findSub(x,alist) ==
    null alist => nil
    alist is [['isDomain,y,z],:.] and x = y => z
    findSub(x,rest alist)

signatureTran pred ==
  atom pred => pred
  pred is ['has,D,catForm] and isCategoryForm(catForm,$e) =>
    ['ofCategory,D,catForm]
  [signatureTran p for p in pred]

interactiveModemapForm mm ==
  --  create modemap form for use by the interpreter.  This function
  --  replaces all specific domains mentioned in the modemap with pattern
  --  variables, and predicates
  mm := replaceVars(COPY mm,$PatternVariableList,$FormalMapVariableList)
  [pattern:=[dc,:sig],pred] := mm
  pred := [fn x for x in pred] where fn x ==
    x is [a,b,c] and a ^= 'isFreeFunction and atom c => [a,b,[c]]
    x
--pp pred
  [mmpat, patternAlist, partial, patvars] :=
    modemapPattern(pattern,sig)
--pp [pattern, mmpat, patternAlist, partial, patvars]
  [pred,domainPredicateList] :=
    substVars(pred,patternAlist,patvars)
--pp [pred,domainPredicateList]
  [pred,:dependList]:=
    fixUpPredicate(pred,domainPredicateList,partial,rest mmpat)
--pp [pred,dependList]
  [cond, :.] := pred
  [mmpat, cond]

modemapPattern(mmPattern,sig) ==
  --  Returns a list of the pattern of a modemap, an Alist of the
  --  substitutions made, a boolean flag indicating whether
  --  the result type is partial, and a list of unused pattern variables
  patternAlist := nil
  mmpat := nil
  patvars := $PatternVariableList
  partial := false
  for xTails in tails mmPattern repeat
    x := first xTails
    if x is ['Union,dom,tag] and tag = '"failed" and xTails=sig then
      x := dom
      partial := true
    patvar := rassoc(x,patternAlist)
    not null patvar => mmpat := [patvar,:mmpat]
    patvar := first patvars
    patvars := rest patvars
    mmpat := [patvar,:mmpat]
    patternAlist := [[patvar,:x],:patternAlist]
  [NREVERSE mmpat,patternAlist,partial,patvars]

substVars(pred,patternAlist,patternVarList) ==
  --make pattern variable substitutions
  domainPredicates := nil
  for [[patVar,:value],:.] in tails patternAlist repeat
    pred := substitute(patVar,value,pred)
    patternAlist := nsubst(patVar,value,patternAlist)
    domainPredicates := substitute(patVar,value,domainPredicates)
    if ^MEMQ(value,$FormalMapVariableList) then
      domainPredicates := [["isDomain",patVar,value],:domainPredicates]
  everything := [pred,patternAlist,domainPredicates]
  for var in $FormalMapVariableList repeat
    CONTAINED(var,everything) =>
      replacementVar := first patternVarList
      patternVarList := rest patternVarList
      pred := substitute(replacementVar,var,pred)
      domainPredicates := substitute(replacementVar,var,domainPredicates)
  [pred, domainPredicates]

fixUpPredicate(predClause, domainPreds, partial, sig) ==
  --  merge the predicates in predClause and domainPreds into a
  --  single predicate
  [predicate, fn, :skip] := predClause
  if first predicate = "AND" then
    predicates := APPEND(domainPreds,rest predicate)
  else if predicate ^= MKQ "T"
--was->then predicates:= REVERSE [predicate, :domainPreds]
       then predicates:= [predicate, :domainPreds]
       else predicates := domainPreds or [predicate]
  if #predicates > 1 then
    pred := ["AND",:predicates]
    [pred,:dependList]:=orderPredicateItems(pred,sig,skip)
  else
    pred := orderPredicateItems(first predicates,sig,skip)
    dependList:= if pred is ['isDomain,pvar,[.]] then [pvar] else nil
  pred := moveORsOutside pred
  if partial then pred := ["partial", :pred]
  [[pred, fn, :skip],:dependList]

moveORsOutside p ==
  p is ['AND,:q] =>
    q := [moveORsOutside r for r in q]
    x := or/[r for r in q | r is ['OR,:s]] =>
      moveORsOutside(['OR,:[['AND,:SUBST(t,x,q)] for t in CDR x]])
    ['AND,:q]
  p

replaceVars(x,oldvars,newvars) ==
  --  replace every identifier in oldvars with the corresponding
  --  identifier in newvars in the expression x
  for old in oldvars for new in newvars repeat
    x := substitute(new,old,x)
  x

getDomainFromMm mm ==
  -- Returns the Domain (or package or category) of origin from a pattern
  -- modemap
  [., cond] := mm
  if cond is ['partial, :c] then cond := c
  condList :=
    cond is ['AND, :cl] => cl
    cond is ['OR, ['AND, :cl],:.] => cl  --all cl's should give same info
    [cond]
  val :=
    for condition in condList repeat
      condition is ['isDomain, "*1", dom] => return opOf dom
      condition is ['ofCategory, "*1", cat] => return opOf cat
  null val =>
    keyedSystemError("S2GE0016",
      ['"getDomainFromMm",'"Can't find domain in modemap condition"])
  val

getFirstArgTypeFromMm mm ==
  -- Returns the type of the first argument or nil
  [pats, cond] := mm
  [.,.,:args] := pats
  null args => nil
  arg1 := first args
  if cond is ['partial, :c] then cond := c
  condList :=
    cond is ['AND, :cl] => cl
    cond is ['OR, ['AND, :cl],:.] => cl  --all cl's should give same info
    [cond]
  type := nil
  for condition in condList while not type repeat
      if condition is ['isDomain, a1, dom] and a1=arg1 then type := dom
  type

isFreeFunctionFromMm mm ==
  -- This returns true is the modemap represents a free function, ie,
  -- one not coming from a domain or category.
  [., cond] := mm
  isFreeFunctionFromMmCond cond

isFreeFunctionFromMmCond cond ==
  -- This returns true is the modemap represents a free function, ie,
  -- one not coming from a domain or category.
  if cond is ['partial, :c] then cond := c
  condList :=
    cond is ['AND, :cl] => cl
    cond is ['OR, ['AND, :cl],:.] => cl  --all cl's should give same info
    [cond]
  iff := false
  for condition in condList while not iff repeat
      if condition is ['isFreeFunction, :.] then iff := true
  iff

getAllModemapsFromDatabase(op,nargs) ==
  $getUnexposedOperations: local := true
  startTimingProcess 'diskread
  ans := getSystemModemaps(op,nargs)
  stopTimingProcess 'diskread
  ans

getModemapsFromDatabase(op,nargs) ==
  $getUnexposedOperations: local := false
  startTimingProcess 'diskread
  ans := getSystemModemaps(op,nargs)
  stopTimingProcess 'diskread
  ans

getSystemModemaps(op,nargs) ==
  mml:= GETDATABASE(op,'OPERATION) =>
    mms := NIL
    for (x := [[.,:sig],.]) in mml repeat
      (NUMBERP nargs) and (nargs ^= #QCDR sig) => 'iterate
      $getUnexposedOperations or isFreeFunctionFromMm(x) or
        isExposedConstructor(getDomainFromMm(x)) => mms := [x,:mms]
      'iterate
    mms
  nil

getInCoreModemaps(modemapList,op,nargs) ==
  mml:= LASSOC (op,modemapList) =>
    mml:= CAR mml
    [x for (x:= [[dc,:sig],.]) in mml |
      (NUMBERP nargs => nargs=#rest sig; true) and
        (cfn := abbreviate (domName := getDomainFromMm x)) and
          ($getUnexposedOperations or isExposedConstructor(domName))]
  nil

mkAlistOfExplicitCategoryOps target ==
  if target is ['add,a,:l] then
    target:=a
  target is ['Join,:l] =>
    "union"/[mkAlistOfExplicitCategoryOps cat for cat in l]
  target is ['CATEGORY,.,:l] =>
    l:= flattenSignatureList ['PROGN,:l]
    u:=
      [[atomizeOp op,:sig] for x in l | x is ['SIGNATURE,op,sig,:.]]
            where
              atomizeOp op ==
                atom op => op
                op is [a] => a
                keyedSystemError("S2GE0016",
                  ['"mkAlistOfExplicitCategoryOps",'"bad signature"])
    opList:= REMDUP ASSOCLEFT u
    [[x,:fn(x,u)] for x in opList] where
      fn(op,u) ==
        u is [[a,:b],:c] => (a=op => [b,:fn(op,c)]; fn(op,c))
  isCategoryForm(target,$e) => nil
  keyedSystemError("S2GE0016",
    ['"mkAlistOfExplicitCategoryOps",'"bad signature"])

flattenSignatureList(x) ==
  atom x => nil
  x is ['SIGNATURE,:.] => [x]
  x is ['IF,cond,b1,b2] =>
     append(flattenSignatureList b1, flattenSignatureList b2)
  x is ['PROGN,:l] =>
     ll:= []
     for x in l repeat
        x is ['SIGNATURE,:.] => ll:=cons(x,ll)
        ll:= append(flattenSignatureList x,ll)
     ll
  nil

mkDatabasePred [a,t] ==
  isCategoryForm(t,$e) => ['ofCategory,a,t]
  ['ofType,a,t]

formal2Pattern x ==
  SUBLIS(pairList($FormalMapVariableList,rest $PatternVariableList),x)

updateDatabase(fname,cname,systemdir?) ==
 -- for now in NRUNTIME do database update only if forced
  not $forceDatabaseUpdate => nil
  $newcompMode = 'true => nil
  -- these modemaps are never needed in the old scheme
  if oldFname := constructor? cname then
    clearClams()
    clearAllSlams []
    if GETL(cname, 'LOADED) then
      clearConstructorCaches()
  if $forceDatabaseUpdate or not systemdir? then
    clearClams()
    clearAllSlams []

removeCoreModemaps(modemapList,c) ==
  newUserModemaps:= nil
  c := opOf unabbrev c
  for [op,mmList] in modemapList repeat
    temp:= nil
    for mm in mmList repeat
      cname := getDomainFromMm mm
      if cname ^= c then temp:= [:temp,mm]
    if temp then newUserModemaps:= [:newUserModemaps,[op,temp]]
  newUserModemaps

addCoreModemap(modemapList,op,modemap,cname) ==
  entry:= ASSQ(op,modemapList) =>
    RPLAC(CADR entry,[modemap,:CADR entry])
    modemapList
  modeMapList:= [:modemapList,[op,[ modemap]]]

REMOVER(lst,item) ==
  --destructively removes item from lst
  not PAIRP lst =>
    lst=item => nil
    lst
  first lst=item => rest lst
  RPLNODE(lst,REMOVER(first lst,item),REMOVER(rest lst,item))

allLASSOCs(op,alist) ==
  [value for [key,:value] in alist | key = op]

loadDependents fn ==
  isExistingFile [fn,$spadLibFT,"*"] =>
    MEMQ("dependents",RKEYIDS(fn,$spadLibFT)) =>
      stream:= readLib1(fn,$spadLibFT,"*")
      l:= rread('dependents,stream,nil)
      RSHUT stream
      for x in l repeat
        x='SubDomain => nil
        loadIfNecessary x

--% Miscellaneous Stuff

getOplistForConstructorForm (form := [op,:argl]) ==
  --  The new form is an op-Alist which has entries (<op> . signature-Alist)
  --    where signature-Alist has entries (<signature> . item)
  --      where item has form (<slotNumber> <condition> <kind>)
  --        where <kind> =  ELT | CONST | Subsumed | (XLAM..) ..
  pairlis:= [[fv,:arg] for fv in $FormalMapVariableList for arg in argl]
  opAlist := getOperationAlistFromLisplib op
  [:getOplistWithUniqueSignatures(op,pairlis,signatureAlist)
      for [op,:signatureAlist] in opAlist]

getOplistWithUniqueSignatures(op,pairlis,signatureAlist) ==
  alist:= nil
  for [sig,:[slotNumber,pred,kind]] in signatureAlist | kind ^= 'Subsumed repeat
    alist:= insertAlist(SUBLIS(pairlis,[op,sig]),
                SUBLIS(pairlis,[pred,[kind,nil,slotNumber]]),
                alist)
  alist

--% Code For Modemap Insertion

insertModemap(new,mmList) ==
  null mmList => [new]
--isMoreSpecific(new,old:= first mmList) => [new,:mmList]
--[old,:insertModemap(new,rest mmList)]
  [new,:mmList]

--% Exposure Group Code

dropPrefix(fn) ==
  member(fn.0,[char "?",char "-",char "+"]) => SUBSTRING(fn,1,nil)
  fn

--moved to util.lisp
--++loadExposureGroupData() ==
--++  egFile := ['INTERP,'EXPOSED]
--++--  null MAKE_-INPUT_-FILENAME(egFile) =>
--++--    throwKeyedMsg("S2IL0003",[namestring egFile])
--++  stream:= DEFIOSTREAM(['(MODE . INPUT),['FILE,:egFile]],80,0)
--++  $globalExposureGroupAlist := NIL
--++  egName  := NIL
--++  egFiles := NIL
--++  while (not PLACEP (x:= READ_-LINE stream)) repeat
--++    x := DROPTRAILINGBLANKS x
--++    SIZE(x) = 0 => 'iterate                         -- blank line
--++    (x.0 = char "#") or (x.0 = char "*") => 'iterate    -- comment
--++    x.0 = char " " =>
--++       -- possible exposure group member name and library name
--++       null egName =>
--++     throwKeyedMsg("S2IZ0069A",[namestring egFile,x])
--++       x := dropLeadingBlanks x
--++       -- should be two tokens on the line
--++       p := STRPOS('" ",x,1,NIL)
--++       NULL p =>
--++     throwKeyedMsg("S2IZ0069B",[namestring egFile,x])
--++       n := object2Identifier SUBSTRING(x,0,p)
--++       x := dropLeadingBlanks SUBSTRING(x,p+1,NIL)
--++       SIZE(x) = 0 =>
--++     throwKeyedMsg("S2IZ0069B",[namestring egFile,x])
--++       egFiles := [[n,:object2Identifier x],:egFiles]
--++    -- have a new group name
--++    if egName then $globalExposureGroupAlist :=
--++      [[egName,:nreverse egFiles],:$globalExposureGroupAlist]
--++    egFiles := NIL
--++    STRPOS('" ",x,1,NIL) =>
--++      throwKeyedMsg("S2IZ0069C",[namestring egFile,x])
--++    egName := object2Identifier x
--++  if egFiles then $globalExposureGroupAlist :=
--++      [[egName,:nreverse egFiles],:$globalExposureGroupAlist]
--++  SHUT stream
--++  $globalExposureGroupAlist := nreverse $globalExposureGroupAlist
--++  'done

isExposedConstructor name ==
  -- this function checks the local exposure data in the frame to
  -- see if the given constructor is exposed. The format of
  -- $localExposureData is a vector with
  --   slot 0: list of groups exposed in the frame
  --   slot 1: list of constructors explicitly exposed
  --   slot 2: list of constructors explicitly hidden
  -- check if it is explicitly hidden
  MEMQ(name,'(Union Record Mapping)) => true
  MEMQ(name,$localExposureData.2) => false
  -- check if it is explicitly exposed
  MEMQ(name,$localExposureData.1) => true
  -- check if it is in an exposed group
  found := NIL
  for g in $localExposureData.0 while not found repeat
    null (x := GETALIST($globalExposureGroupAlist,g)) => 'iterate
    if GETALIST(x,name) then found := true
  found

displayExposedGroups() ==
  sayKeyedMsg("S2IZ0049A",[$interpreterFrameName])
  if null $localExposureData.0
    then centerAndHighlight '"there are no exposed groups"
    else for g in $localExposureData.0 repeat
      centerAndHighlight g

displayExposedConstructors() ==
  sayKeyedMsg("S2IZ0049B",NIL)
  if null $localExposureData.1
    then centerAndHighlight
      '"there are no explicitly exposed constructors"
    else for c in $localExposureData.1 repeat
      centerAndHighlight c

displayHiddenConstructors() ==
  sayKeyedMsg("S2IZ0049C",NIL)
  if null $localExposureData.2
    then centerAndHighlight
      '"there are no explicitly hidden constructors"
    else for c in $localExposureData.2 repeat
      centerAndHighlight c


