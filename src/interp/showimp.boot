-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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


import c_-util
namespace BOOT

$returnNowhereFromGoGet := false

showSummary dom ==
  showPredicates dom
  showAttributes dom
  showFrom dom
  showImp dom

--=======================================================================
--          Show Where Functions in Domain are Implemented
--=======================================================================
showImp(dom,:options) ==
  sayBrightly '"-------------Operation summary-----------------"
  missingOnlyFlag := KAR options
  domainForm := devaluate dom
  [nam,:$domainArgs] := domainForm
  $predicateList: local := getConstructorPredicates nam
  predVector := domainPredicates dom
  u := getDomainOpTable(dom,true)
  --sort into 4 groups: domain exports, unexports, default exports, others
  for (x := [.,.,:key]) in u repeat
    key = domainForm => domexports := [x,:domexports]
    integer? key => unexports := [x,:unexports]
    defaultPackageForm? key => defexports := [x,:defexports]
    key is 'nowhere => nowheres := [x,:nowheres]
    key is 'constant => constants := [x,:constants]
    others := [x,:others]   --add chain domains go here
  sayBrightly
    nowheres => ['"Functions exported but not implemented by",
      :bright form2String domainForm,'":"] 
    [:bright form2String domainForm,'"implements all exported operations"]  
  showDomainsOp1(nowheres,'nowhere)
  missingOnlyFlag => 'done

  --first display those exported by the domain, then add chain guys
  u := [:domexports,:constants,:reverse! sortBy(function CDDR,others)]
  while u repeat
    [.,.,:key] := first u
    sayBrightly
      key is 'constant => 
        ["Constants implemented by",:bright form2String key,'":"]
      ["Functions implemented by",:bright form2String key,'":"]
    u := showDomainsOp1(u,key)
  u := reverse! sortBy(function CDDR,defexports)
  while u repeat
    [.,.,:key] := first u
    defop := makeSymbol(subString((s := PNAME first key),0,maxIndex s))
    domainForm := [defop,:CDDR key]
    sayBrightly ["Default functions from",:bright form2String domainForm,'":"]
    u := showDomainsOp1(u,key)
  u := reverse! sortBy(function CDDR,unexports)
  while u repeat
    [.,.,:key] := first u
    sayBrightly ["Not exported: "]
    u := showDomainsOp1(u,key)

--=======================================================================
--          Show Information Directly From Domains
--=======================================================================
showFrom(D,:option) ==
  ops := KAR option
  alist := nil
  domainForm := devaluate D
  [nam,:.] := domainForm
  $predicateList: local := getConstructorPredicates nam
  for (opSig := [op,sig]) in getDomainSigs1(D,ops) repeat
    u := from?(D,op,sig)
    x := assoc(u,alist) => x.rest := [opSig,:rest x]
    alist := [[u,opSig],:alist]
  for [conform,:l] in alist repeat
    sayBrightly concat('"From ",form2String conform,'":")
    for [op,sig] in l repeat sayBrightly ['"   ",:formatOpSignature(op,sig)]
 
--=======================================================================
--               Functions implementing showFrom
--=======================================================================
getDomainOps D ==
  conname := insantiationCtor D
  $predicateList: local := getConstructorPredicates conname
  removeDuplicates listSort(function GLESSEQP,ASSOCLEFT getDomainOpTable(D,nil))
 
getDomainSigs(D,:option) ==
  conname := instantiationCtor D
  $predicateList: local := getConstructorPredicates conname
  getDomainSigs1(D,first option)
  
getDomainSigs1(D,ops) == listSort(function GLESSEQP,u) where
  u() == [x for x in getDomainOpTable(D,nil)
            | null ops or symbolMember?(first x,ops)]
 
getDomainDocs(D,:option) ==
  conname := instantiationCtor D
  $predicateList: local := getConstructorPredicates conname
  ops := KAR option
  [[op,sig,:getInheritanceByDoc(D,op,sig)] for [op,sig] in getDomainSigs1(D,ops)]
 
--=======================================================================
--          Getting Inheritance Info from Documentation in Lisplib
--=======================================================================
from?(D,op,sig) == KAR KDR getInheritanceByDoc(D,op,sig)

getExtensionsOfDomain domain ==
  u := getDomainExtensionsOfDomain domain
  cats := getCategoriesOfDomain domain
  for x in u repeat
    cats := union(cats,getCategoriesOfDomain eval x)
  [:u,:cats]

getDomainExtensionsOfDomain domain ==
  acc := nil
  d := domain
  while (u := devaluateSlotDomain(5,d)) repeat
    acc := [u,:acc]
    d := eval u
  acc

devaluateSlotDomain(u,dollar) ==
  u = '$ => devaluate dollar
  integer? u and vector? (y := dollar.u) => devaluate y
  u is ['%eval,y] => MKQ eval y
  u is ['QUOTE,y] => u
  u is [op,:argl] => [op,:[devaluateSlotDomain(x,dollar) for x in argl]]
  devaluate evalSlotDomain(u,dollar)
 
getCategoriesOfDomain domain ==
  predkeyVec := first vectorRef(domain,4)
  catforms := second vectorRef(domain,4)
  [fn for i in 0..maxIndex predkeyVec | test] where 
     test() == arrayRef(predkeyVec,i) and 
       (x := vectorRef(catforms,i)) isnt ['DomainSubstitutionMacro,:.]
     fn() ==
       vector? x => devaluate x
       devaluateSlotDomain(x,domain)

getInheritanceByDoc(D,op,sig,:options) ==
--gets inheritance and documentation information by looking in the LISPLIB      
--for each ancestor of the domain
  catList := KAR options or getExtensionsOfDomain D
  getDocDomainForOpSig(op,sig,devaluate D,D) or
    or/[fn for x in catList] or '(NIL NIL)
      where fn() == getDocDomainForOpSig(op,sig,substDomainArgs(D,x),D)
 
getDocDomainForOpSig(op,sig,dollar,D) ==
  (u := LASSOC(op,getConstructorDocumentationFromDB first dollar))
    and (doc := or/[[d,dollar] for [s,:d] in u | compareSig(sig,s,D,dollar)])
 
--=======================================================================
--               Functions implementing showImp
--=======================================================================
showDomainsOp1(u,key) ==
  while u and first u is [op,sig,: =key] repeat
    sayBrightly ['"   ",:formatOpSignature(op,sig)]
    u := rest u
  u

getDomainRefName(dom,nam) ==
  cons? nam => [getDomainRefName(dom,x) for x in nam]
  not integer? nam => nam
  slot := dom.nam
  vector? slot => slot.0
  slot is ['%store,:.] => 
    getDomainRefName(dom,getDomainSeteltForm slot)
  slot

getDomainSeteltForm ['%store,.,form] ==
  form is ['evalSlotDomain,u,d] => devaluateSlotDomain(u,d)
  vector? form => systemError()
  form
 
showPredicates dom ==
  sayBrightly '"--------------------Predicate summary-------------------"
  conname := instantiationCtor dom
  predvector := domainPredicates dom
  predicateList := getConstructorPredicates conname
  for i in 1.. for p in predicateList repeat
    prefix := 
      testBitVector(predvector,i) => '"true : "
      '"false: "
    sayBrightly [prefix,:pred2English p]
 
showAttributes dom ==
  sayBrightly '"--------------------Attribute summary-------------------"
  conname := instantiationCtor dom
  abb := getConstructorAbbreviation conname
  predvector := domainPredicates dom
  for [a,:p] in vectorRef(dom,2) repeat
    prefix :=
      testBitVector(predvector,p) => '"true : "
      '"false: "
    sayBrightly concat(prefix,form2String a)

showGoGet dom ==
  numvec := CDDR vectorRef(dom,4)
  for i in 6..maxIndex dom | (slot := vectorRef(dom,i)) is ['newGoGet,dol,index,:op] repeat
    numOfArgs := arrayRef(numvec,index)
    whereNumber := arrayRef(numvec,index := index + 1)
    signumList := 
      [formatLazyDomainForm(dom,arrayRef(numvec,index + i)) for i in 0..numOfArgs]
    index := index + numOfArgs + 1
    namePart := 
      concat(bright "from",form2String formatLazyDomainForm(dom,whereNumber))
    sayBrightly [i,'": ",:formatOpSignature(op,signumList),:namePart]

formatLazyDomain(dom,x) ==
  vector? x => devaluate x
  x is [dollar,slotNumber,:form] => formatLazyDomainForm(dom,form)
  systemError nil
 
formatLazyDomainForm(dom,x) ==
  x = 0 => ["$"]
  integer? x => formatLazyDomain(dom,dom.x)
  x isnt [.,:.] => x
  x is ['%eval,y] => (y isnt [.,:.] => [y]; y)
  [first x,:[formatLazyDomainForm(dom,y) for y in rest x]]
 


--=======================================================================
--                     Display Template
--=======================================================================
dc(:r) ==
  con := KAR r
  options := KDR r
  ok := constructorDB con or (con := abbreviation? con)
  null ok =>
    sayBrightly '"Format is: dc(<constructor name or abbreviation>,option)"
    sayBrightly 
      '"options are: all (default), slots, atts, cats, data, ops, optable"
  option := KAR options
  option = 'all or null option => dcAll con
  option = 'slots   =>  dcSlots con
  option = 'atts    =>  dcAtts  con
  option = 'cats    =>  dcCats  con
  option = 'data    =>  dcData  con
  option = 'ops     =>  dcOps   con
  option = 'size    =>  dcSize( con,'full)
  option = 'optable =>  dcOpTable con

dcSlots con ==
  name := abbreviation? con or con
  $infovec: local := getInfovec name
  template := $infovec.0
  for i in 5..maxIndex template repeat
    sayBrightlyNT bright i
    item := template.i
    item is [n,:op] and integer? n => dcOpLatchPrint(op,n)
    null item and i > 5 => sayBrightly ['"arg  ",strconc('"#",toString(i - 5))]
    item isnt [.,:.] => sayBrightly ['"fun  ",item]
    item is ['%closure,.,['FUNCALL,[.,a],b]] => sayBrightly ['"constant ",a]
    sayBrightly concat('"lazy ",form2String formatSlotDomain i)
 
dcOpLatchPrint(op,index) ==
  numvec := getCodeVector()
  numOfArgs := numvec.index
  whereNumber := numvec.(index := index + 1)
  signumList := dcSig(numvec,index + 1,numOfArgs)
  index := index + numOfArgs + 1
  namePart := concat(bright "from",
    dollarPercentTran form2String formatSlotDomain whereNumber)
  sayBrightly ['"latch",:formatOpSignature(op,signumList),:namePart]
 
getInfovec name ==
  u := property(name,'infovec) => u
  dbLoaded? constructorDB name => nil
  fullLibName := getConstructorModuleFromDB name or return nil
  startTimingProcess 'load
  loadLibNoUpdate(name, name, fullLibName)
  property(name,'infovec)
 
getOpSegment index ==
  numOfArgs := (vec := getCodeVector()).index
  [vec.i for i in index..(index + numOfArgs + 3)]

getCodeVector() ==
  proto4 := $infovec.3
  u := CDDR proto4
  vector? u => u           --old style
  rest u                 --new style

formatSlotDomain x ==
  x = 0 => ["$"]
  x = 2 => ["$$"]
  integer? x =>
    val := $infovec.0.x
    null val => [strconc('"#",toString (x  - 5))]
    formatSlotDomain val
  x isnt [.,:.] => x
  x is ['%eval,y] => (y isnt [.,:.] => [y]; y)
  [first x,:[formatSlotDomain y for y in rest x]]
 
--=======================================================================
--                     Display OpTable
--=======================================================================
dcOpTable con ==
  name := abbreviation? con or con
  $infovec: local := getInfovec name
  template := $infovec.0
  $predvec: local := getConstructorPredicates con
  opTable := $infovec.1
  for i in 0..maxIndex opTable repeat
    op := opTable.i
    i := i + 1
    startIndex := opTable.i
    stopIndex :=
      i + 1 > maxIndex opTable => maxIndex getCodeVector()
      opTable.(i + 2)
    curIndex := startIndex
    while curIndex < stopIndex repeat
      curIndex := dcOpPrint(op,curIndex)
 
dcOpPrint(op,index) ==
  numvec := getCodeVector()
  segment := getOpSegment index
  numOfArgs := numvec.index
  index := index + 1
  predNumber := numvec.index
  index := index + 1
  signumList := dcSig(numvec,index,numOfArgs)
  index := index + numOfArgs + 1
  slotNumber := numvec.index
  suffix :=
    predNumber = 0 => nil
    [:bright '"if",:pred2English $predvec.(predNumber - 1)]
  kind := 'ELT
  namePart := bright
    slotNumber = 0 => '"subsumed by next entry"
    slotNumber = 1 => '"missing"
    name := $infovec.0.slotNumber
    name isnt [.,:.] => name
    name is ['%closure,'%constant,
              ["FUNCALL", ['%function, impl],"$"]] =>
      kind := 'CONST
      impl
    '"looked up"
  sayBrightly [:formatOpSignature(op,signumList,kind),:namePart, :suffix]
  index + 1
 
dcSig(numvec,index,numOfArgs) ==
  [formatSlotDomain numvec.(index + i) for i in 0..numOfArgs]
 
dcPreds con ==
  name := abbreviation? con or con
  $infovec: local := getInfovec name
  $predvec:= getConstructorPredicates con
  for i in 0..maxIndex $predvec repeat
    sayBrightlyNT bright (i + 1)
    sayBrightly pred2English $predvec.i
 
dcAtts con ==
  name := abbreviation? con or con
  $infovec: local := getInfovec name
  $predvec:= getConstructorPredicates con
  attList := $infovec.2
  for [a,:predNumber] in attList for i in 0.. repeat
    sayBrightlyNT bright i
    suffix :=
      predNumber = 0 => nil
      [:bright '"if",:pred2English $predvec.(predNumber - 1)]
    sayBrightly [a,:suffix]
 
dcCats con ==
  name := abbreviation? con or con
  $infovec: local := getInfovec name
  u := $infovec.3
  vector? CDDR u => dcCats1 con    --old style slot4
  $predvec:= getConstructorPredicates con
  catpredvec := first u
  catinfo := second u
  catvec := third u
  for i in 0..maxIndex catvec repeat
    sayBrightlyNT bright i
    form := catvec.i
    predNumber := catpredvec.i
    suffix :=
      predNumber = 0 => nil
      [:bright '"if",:pred2English $predvec.(predNumber - 1)]
    extra :=
      null (info := catinfo.i) => nil
      ident? info => bright '"package"
      bright '"instantiated"
    sayBrightly concat(form2String formatSlotDomain form,suffix,extra)
 
dcCats1 con ==
  $predvec:= getConstructorPredicates con
  u := $infovec.3
  catvec := second u
  catinfo := first u
  for i in 0..maxIndex catvec repeat
    sayBrightlyNT bright i
    [form,:predNumber] := catvec.i
    suffix :=
      predNumber = 0 => nil
      [:bright '"if",:pred2English $predvec.(predNumber - 1)]
    extra :=
      null (info := catinfo.i) => nil
      ident? info => bright '"package"
      bright '"instantiated"
    sayBrightly concat(form2String formatSlotDomain form,suffix,extra)
 
dcData con ==
  name := abbreviation? con or con
  $infovec: local := getInfovec name
  sayBrightly '"Operation data from slot 1"
  PRINT_-FULL $infovec.1
  vec := getCodeVector()
  vec := (cons? vec => rest vec; vec)
  sayBrightly ['"Information vector has ",# vec,'" entries"]
  dcData1 vec

dcData1 vec ==
  n := maxIndex vec
  tens := n quo 10
  for i in 0..tens repeat
    start := 10*i
    sayBrightlyNT rightJustifyString(toString start,6)
    sayBrightlyNT '"  |"
    for j in start..MIN(start + 9,n) repeat
      sayBrightlyNT rightJustifyString(STRINGIMAGE vec.j,6)
    sayNewLine()
  vec

dcSize(:options) ==
  con := KAR options
  options := rest options
  null con => dcSizeAll()
  quiet := 'quiet in options
  full := 'full in options
  name := abbreviation? con or con
  infovec := getInfovec name
  template := infovec.0
  maxindex := maxIndex template
  latch := 0  --# of go get slots
  lazy  := 0  --# of lazy domain slots
  fun   := 0  --# of function slots
  lazyNodes := 0 --# of nodes needed for lazy domain slots
  for i in 5..maxindex repeat
    (item := template.i) isnt [.,:.] =>   fun := fun + 1
    integer? first item    => latch := latch + 1
    'T                 =>  
       lazy := lazy + 1
       lazyNodes := lazyNodes + numberOfNodes item
  tSize := sum(vectorSize(1 + maxindex),nodeSize(lazyNodes + latch))
  -- functions are free in the template vector
  oSize := vectorSize(# infovec.1)
  aSize := numberOfNodes infovec.2
  slot4 := infovec.3
  catvec := 
    vector? CDDR slot4 => second slot4
    third slot4
  n := maxIndex catvec
  cSize := sum(nodeSize(2),vectorSize(# first slot4),vectorSize(n + 1),
               nodeSize(+/[numberOfNodes catvec.i for i in 0..n]))
  codeVector :=
    vector? CDDR slot4 => CDDR slot4
    CDDDR slot4
  vSize := halfWordSize(# codeVector)
  itotal := sum(tSize,oSize,aSize,cSize,vSize)
  if null quiet then sayBrightly ['"infovec total = ",itotal,'" BYTES"]
  if null quiet then
    lookupFun := getLookupFun infovec
    suffix := (lookupFun = 'lookupIncomplete => '"incomplete"; '"complete")
    sayBrightly ['"template    = ",tSize]
    sayBrightly ['"operations  = ",oSize,'" (",suffix,'")"]
    sayBrightly ['"attributes  = ",aSize]
    sayBrightly ['"categories  = ",cSize]
    sayBrightly ['"data vector = ",vSize]
  if null quiet then
    sayBrightly ['"number of function slots (one extra node) = ",fun]
    sayBrightly ['"number of latch slots (2 extra nodes) = ",latch]
    sayBrightly ['"number of lazy slots (no extra nodes) = ",lazy]
    sayBrightly ['"size of domain vectors = ",1 + maxindex,'" slots"]
  vtotal := itotal + nodeSize(fun)       --fun   slot is ($ . function)
  vtotal := vtotal + nodeSize(2 * latch) --latch slot is (newGoGet $ . code)
  --NOTE: lazy slots require no cost     --lazy  slot is lazyDomainForm
  if null quiet then sayBrightly ['"domain size = ",vtotal,'" BYTES"] 
  etotal := nodeSize(fun + 2 * latch) + vectorSize(1 + maxindex)
  if null quiet then sayBrightly ['"cost per instantiation = ",etotal,'" BYTES"]
  vtotal

dcSizeAll() ==
  count := 0
  total := 0
  for x in allConstructors() | cons? property(x,'infovec) repeat
    count := count + 1
    s := dcSize(x,'quiet)
    sayBrightly [s,'" : ",x]
    total := total + s
  sayBrightly '"------------total-------------"
  sayBrightly [count," constructors; ",total," BYTES"]  
    
sum(:l) == +/l

nodeSize(n) == 12 * n

vectorSize(n) == 4 * (1 + n)

halfWordSize(n) == 
  n < 128 => n quo 2
  n < 256 => n
  2 * n

numberOfNodes(x) ==
  x isnt [.,:.] => 0
  1 + numberOfNodes first x + numberOfNodes rest x

template con ==
  con := abbreviation? con or con
  ppTemplate getInfovec(con).0

ppTemplate vec ==
  for i in 0..maxIndex vec repeat
    sayBrightlyNT bright i
    pp vec.i

infovec con == 
  con := abbreviation? con or con
  u := getInfovec con
  sayBrightly '"---------------slot 0 is template-------------------"
  ppTemplate u.0
  sayBrightly '"---------------slot 1 is op table-------------------"
  PRINT_-FULL u.1
  sayBrightly '"---------------slot 2 is attribute list-------------"
  PRINT_-FULL u.2
  sayBrightly '"---------------slot 3.0 is catpredvec---------------"
  PRINT_-FULL u.3.0
  sayBrightly '"---------------slot 3.1 is catinfovec---------------"
  PRINT_-FULL u.3.1
  sayBrightly '"---------------slot 3.2 is catvec-------------------"
  PRINT_-FULL u.3.2
  sayBrightly '"---------------tail of slot 3 is datavector---------"
  dcData1 CDDDR u.3
  'done

dcAll con ==
  con := abbreviation? con or con
  $infovec : local := getInfovec con
  complete? := 
    #$infovec = 4 => false
    $infovec.4 = 'lookupComplete
  sayBrightly '"----------------Template-----------------"
  dcSlots con
  sayBrightly
    complete? => '"----------Complete Ops----------------"
    '"----------Incomplete Ops---------------"
  dcOpTable con
  sayBrightly '"----------------Atts-----------------"
  dcAtts con
  sayBrightly '"----------------Preds-----------------"
  dcPreds con
  sayBrightly '"----------------Cats-----------------"
  dcCats con
  sayBrightly '"----------------Data------------------"
  dcData con
  sayBrightly '"----------------Size------------------"
  dcSize(con,'full)
  'done

dcOps conname ==
  for [op,:u] in reverse getConstructorOperationsFromDB conname repeat
    for [sig,slot,pred,key,:.] in u repeat
      suffix := 
        pred isnt [.,:.] => nil
        concat('" if ",pred2English pred)
      key is 'Subsumed =>
        sayBrightly [:formatOpSignature(op,sig),'" subsumed by ",:formatOpSignature(op,slot),:suffix]
      sayBrightly [:formatOpSignature(op,sig,key),:suffix]
  
