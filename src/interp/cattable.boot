-- Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
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


import simpbool
import c_-util
namespace BOOT
module cattable where
  hasCat: (%Instantiation,%Instantiation) -> %Code

hasCat(dom,cat) ==
  cat.op is "Type"  -- every domain is a Type
    or constructorHasCategoryFromDB [dom.op,:cat.op]

showCategoryTable con ==
  [[b,:val] for [[a,:b],:val] in entries $HasCategoryTable
     | symbolEq?(a,con) and val ~= nil]

displayCategoryTable(:options) ==
  conList := IFCAR options
  SETQ($ct,hashTable 'EQ)
  for [[a,:b],:val] in entries $HasCategoryTable repeat
    tableValue($ct,a) := [[b,:val],:tableValue($ct,a)]
  for [id,:val] in entries $ct | null conList or symbolMember?(id,conList) repeat
    sayMSG [:bright id,'"extends:"]
    PRINT val

generateCategoryTable ancestors ==
  $HasCategoryTable := makeTable function EQUAL
  generateAncestorCategoryTable ancestors
  domainTable :=
    [addDomainToTable(con,getConstrCat getConstructorCategory con)
      for con in allConstructors() | not builtinFunctorName? con
        and getConstructorKindFromDB con is "domain"]
  -- $nonLisplibDomains, $noCategoryDomains are set in BUILDOM BOOT
  specialDs := setDifference($nonLisplibDomains,$noCategoryDomains)
  domainTable:= [:[addDomainToTable(id, getConstrCat eval([id]).3)
    for id in specialDs | id ~= 'Cross], :domainTable]
  for [id,:entry] in domainTable repeat
    for [a,:b] in encodeCategoryAlist(id,entry) repeat
      tableValue($HasCategoryTable,[id,:a]) := b
  simplifyAncestorCategoryTable ancestors
  simpCategoryTable()

simplifyAncestorCategoryTable ancestors ==
  for [id,:.] in entries ancestors repeat
    for u in getConstructorAncestorsFromDB id repeat
      u.rest := simpHasPred rest u

simpCategoryTable() ==
  for [key,:entry] in entries $HasCategoryTable repeat
    null entry => tableRemove!($HasCategoryTable,key)
    change :=
      opOf entry isnt [.,:.] => simpHasPred entry
      [[x,:npred] for [x,:pred] in entry | npred := simpHasPred pred]
    tableValue($HasCategoryTable,key) := change

simpHasPred(pred,:options) == main where
  main() ==
    $hasArgs: local := IFCDR IFCAR options
    simp pred
  simp pred ==
    pred is [op,:r] =>
      op is "has" => simpHas(pred,first r,second r)
      op is 'HasCategory => simp ["has",first r,simpDevaluate second r]
      op is 'HasSignature =>
         [op,sig] := simpDevaluate second r
         ["has",first r,['SIGNATURE,op,sig]]
      op is 'HasAttribute =>
        form := ["has",a := first r,['ATTRIBUTE,b := simpDevaluate second r]]
        simpHasAttribute(form,a,b)
      op in '(AND OR NOT) =>
        null (u := MKPF([simp p for p in r],op)) => nil
        u is '%true or u is '(QUOTE T) => true
        simpBool u
      op is 'hasArgs => ($hasArgs => $hasArgs = r; pred)
      null r and opOf op = "has" => simp first pred
      pred is '%true or pred is '(QUOTE T) => true
      op1 := symbolTarget(op,'((and . AND)(or . OR)(not . NOT))) =>
        simp [op1,:r]
      simp first pred   --REMOVE THIS HACK !!!!
    pred in '(T etc) => pred
    null pred => nil
    pred
  simpDevaluate a == eval substitute('QUOTE,'devaluate,a)
  simpHas(pred,a,b) ==
    b is ['ATTRIBUTE,attr] => simpHasAttribute(pred,a,attr)
    b is ['SIGNATURE,op,sig,:.] => simpHasSignature(pred,a,op,sig)
    ident? a or hasIdent b => pred
    npred := evalHas pred
    ident? npred or null hasIdent npred => npred
    pred
  evalHas (pred := ["has",d,cat]) ==
    x := hasCat(d,cat)
    y := rest cat =>
      npred := or/[p for [args,:p] in x | y = args] => simp npred
      false  --if not there, it is false
    x

simpHasSignature(pred,conform,op,sig) == --eval w/o loading
  ident? conform => pred
  [conname,:args] := conform
  n := #sig
  u := symbolTarget(op,getConstructorOperationsFromDB conname)
  candidates := [x for (x := [sig1,:.]) in u | #sig1 = #sig]  or return false
  match := or/[x for (x := [sig1,:.]) in candidates
                | sig = sublisFormal(args,sig1)] or return false
  simpHasPred(match is [sig,.,:p] and sublisFormal(args,p) or true)

simpHasAttribute(pred,conform,attr) ==  --eval w/o loading
  ident? conform => pred
  conname := conform.op
  getConstructorKindFromDB conname is "category" =>
      simpCatHasAttribute(conform,attr)
  infovec := dbInfovec conname
  k := LASSOC(attr,infovec.2) or return nil --if not listed then false
  k = 0 => true
  $domain => kTestPred k    --from koOps
  predvec := $predvec or sublisFormal(conform.args,
      getConstructorPredicates conname)
  simpHasPred predvec.(k - 1)

simpCatHasAttribute(domform,attr) ==
  conform := getConstructorForm opOf domform
  catval :=  eval mkEvalable conform
  if KDR attr isnt [.,:.] then
    attr := IFCAR attr
  pred :=
    u := LASSOC(attr,catval.2) => first u
    return false                            --exit: not there
  pred = true => true
  eval applySubst(pairList(conform.args,domform.args),pred)

hasIdent pred ==
  pred is [op,:r] =>
    op is 'QUOTE => false
    or/[hasIdent x for x in r]
  pred is '_$ => false
  ident? pred => true
  false

addDomainToTable(id,catl) ==
  alist:= nil
  for cat in catl repeat
    cat is ['CATEGORY,:.] => nil
    cat is ['IF,pred,cat1,:.] =>
      newAlist:=
        [[a,:quickAnd(pred,b)] for [a,:b] in
          [[cat1,:true],:getCategoryExtensionAlist cat1]]
      alist:= [:alist,:newAlist]
    alist:= [:alist,[cat,:true],:getCategoryExtensionAlist cat]
  [id,:alist]

domainHput(table,key:=[id,:a],b) ==
  tableValue(table,key) := b

generateAncestorCategoryTable ancestors ==
  --generates hashtable with key=categoryName and value of the form
  --     ((form . pred) ..) meaning that
  --           "IF pred THEN ofCategory(key,form)"
  --  where form can involve #1, #2, ... the parameters of key
  for con in allConstructors()  repeat
    getConstructorKindFromDB con is "category" =>
      addToCategoryTable(ancestors,con)
  for [id,:item] in entries ancestors repeat
    for u in item repeat
      u.rest := simpCatPredicate simpBool rest u
    tableValue(ancestors,id) := listSort(function GLESSEQP,item)

addToCategoryTable(ancestors,con) ==
  -- adds an entry to $tempCategoryTable with key=con and alist entries
  u := getConstructorModemap(con).mmDC --domain
  alist := getCategoryExtensionAlist u
  tableValue(ancestors,first u) := alist
  alist

encodeCategoryAlist(id,alist) ==
  newAl:= nil
  for [a,:b] in alist repeat
    [key,:argl] := a
    newEntry:=
      argl => [[argl,:b]]
      b
    u:= assoc(key,newAl) =>
      argl => u.rest := encodeUnion(id,first newEntry,rest u)
      if newEntry ~= rest u then
        p:= moreGeneralCategoryPredicate(id,newEntry,rest u) => u.rest := p
        sayMSG '"Duplicate entries:"
        PRINT [newEntry,rest u]
    newAl:= [[key,:newEntry],:newAl]
  newAl

encodeUnion(id,new:=[a,:b],alist) ==
  u := assoc(a,alist) =>
    u.rest := moreGeneralCategoryPredicate(id,b,rest u)
    alist
  [new,:alist]

moreGeneralCategoryPredicate(id,new,old) ==
  old is 'T or new is 'T => 'T
  old is ["has",a,b] and new is ["has",=a,c] =>
    tempExtendsCat(b,c) => new
    tempExtendsCat(c,b) => old
    ['OR,old,new]
  mkCategoryOr(new,old)

mkCategoryOr(new,old) ==
  old is ['OR,:l] => simpCategoryOr(new,l)
  ['OR,old,new]

simpCategoryOr(new,l) ==
  newExtendsAnOld:= false
  anOldExtendsNew:= false
  ["has",a,b] := new
  newList:= nil
  for pred in l repeat
    pred is ["has",=a,c] =>
      tempExtendsCat(c,b) => anOldExtendsNew:= true
      if tempExtendsCat(b,c) then newExtendsAnOld:= true
      newList:= [pred,:newList]
    newList:= [pred,:newList]
  if not newExtendsAnOld then newList:= [new,:newList]
  newList is [.] => first newList
  ['OR,:newList]

tempExtendsCat(b,c) ==
  or/[first c = a for [[a,:.],:.] in getConstructorAncestorsFromDB first b]

getCategoryExtensionAlist cform ==
  --avoids substitution as much as possible
  u := getConstructorAncestorsFromDB cform.op => formalSubstitute(cform,u)
  mkCategoryExtensionAlist cform

formalSubstitute(form:=[.,:argl],u) ==
  isFormalArgumentList argl => u
  applySubst(pairList($FormalMapVariableList,argl),u)

isFormalArgumentList argl ==
  and/[symbolEq?(x,fa) for x in argl for fa in $FormalMapVariableList]

mkCategoryExtensionAlist cform ==
  not cons? cform => nil
  cop := cform.op
  builtinCategoryName? cop => mkCategoryExtensionAlistBasic cform
  catlist := formalSubstitute(cform, first getConstructorExports(cform, true))
  extendsList:= nil
  for [cat,:pred] in catlist repeat
    newList := [[cat,:true],:getCategoryExtensionAlist cat]
    finalList :=
      pred is 'T => newList
      [[a,:quickAnd(b,pred)] for [a,:b] in newList]
    extendsList:= catPairUnion(extendsList,finalList,cop,cat)
  extendsList

-- following code to handle Unions Records Mapping etc.
mkCategoryExtensionAlistBasic cform ==
  cop := cform.op
  category :=      -- changed by RSS on 7/29/87
    macrop cop => eval cform
    apply(cop, cform.args)
  extendsList := [[x,:'T] for x in categoryPrincipals category]
  for [cat,pred,:.] in categoryAncestors category repeat
    newList := [[cat,:true],:getCategoryExtensionAlist cat]
    finalList :=
      pred = true => newList
      [[a,:quickAnd(b,pred)] for [a,:b] in newList]
    extendsList:= catPairUnion(extendsList,finalList,cop,cat)
  extendsList

catPairUnion(oldList,newList,op,cat) ==
  for pair in newList repeat
    u:= assoc(first pair,oldList) =>
      rest u = rest pair => nil
      u.rest := addConflict(rest pair,rest u) where addConflict(new,old) ==
        quickOr(new,old)
    oldList:= [pair,:oldList]
  oldList

simpCatPredicate p ==
  p is ['OR,:l] =>
    (u:= simpOrUnion l) is [p] => p
    ['OR,:u]
  p

simpOrUnion l ==
  if l then simpOrUnion1(first l,simpOrUnion rest l)
  else l

simpOrUnion1(x,l) ==
  null l => [x]
  p:= mergeOr(x,first l) => [p,:rest l]
  [first l,:simpOrUnion1(x,rest l)]

mergeOr(x,y) ==
  x is ["has",a,b] and y is ["has",=a,c] =>
    testExtend(b,c) => y
    testExtend(c,b) => x
    nil
  nil

testExtend(a,b) ==
  a is ['ATTRIBUTE,a'] =>
    a' is [.,:.] and constructor? a'.op and testExtend(a',b)
  (u:= getConstructorAncestorsFromDB a.op) and (val:= LASSOC(b,u)) =>
    formalSubstitute(a,val)
  nil

getConstrCat(x) ==
-- gets a different representation of the constructorCategory from the
-- lisplib, which is a list of named categories or conditions
  x:= if x is ['Join,:y] then y else [x]
  cats:= nil
  for y in x repeat
    y is ['CATEGORY,.,:z] =>
      for zz in z repeat cats := makeCatPred(zz, cats, true)
    cats:= [y,:cats]
  cats:= reverse! cats
  cats


makeCatPred(zz, cats, thePred) ==
  if zz is ['IF,curPred := ["has",z1,z2],ats,.] then
    ats := if ats is ['PROGN,:atl] then atl else [ats]
    for at in ats repeat
      if at is ['ATTRIBUTE,z3] and cons? z3 and
        constructor? z3.op then
          cats:= [['IF,quickAnd(["has",z1,z2], thePred),z3,'%noBranch],:cats]
      at is ['IF, pred, :.] =>
        cats := makeCatPred(at, cats, curPred)
  cats

getConstructorExports(conform,:options) == categoryParts(conform,
  getConstructorCategory opOf conform,IFCAR options)

categoryParts(conform,category,:options) == main where
  main() ==
    addCtor? := IFCAR options  --means to include constructors as well
    $attrlist: local := nil
    $oplist  : local := nil
    $conslist: local := nil
    conname := opOf conform
    for x in exportsOf(category) repeat build(x,true)
    $attrlist := listSort(function GLESSEQP,$attrlist)
    $oplist   := listSort(function GLESSEQP,$oplist)
    res := [$attrlist,:$oplist]
    if addCtor? then
      res := [listSort(function GLESSEQP,$conslist),:res]
    if getConstructorKindFromDB conname is "category" then
      tvl := take(#conform.args,$TriangleVariableList)
      res := applySubst(pairList(tvl,$FormalMapVariableList),res)
    res
  build(item,pred) ==
    item is ['SIGNATURE,op,sig,:.] => $oplist := [[op,sig,:pred],:$oplist]
    item is ['ATTRIBUTE,attr] =>
      constructor? opOf attr =>
        $conslist := [[attr,:pred],:$conslist]
        nil
      opOf attr is 'nothing => 'skip
      $attrlist := [[opOf attr,IFCDR attr,:pred],:$attrlist]
    item is ['TYPE,op,type] =>
        $oplist := [[op,[type],:pred],:$oplist]
    item is ['IF,pred1,s1,s2] =>
      build(s1,quickAnd(pred,pred1))
      s2 => build(s2,quickAnd(pred,['NOT,pred1]))
    null item => 'ok
    item is "%noBranch" => 'ok
    item is ['PROGN,:r] => for x in r repeat build(x,pred)
    systemError '"build error"
  exportsOf(target) ==
    target is ['CATEGORY,.,:r] => r
    target is ['Join,:r,f] =>
      for x in r repeat $conslist := [[x,:true],:$conslist]
      exportsOf f
    $conslist := [[target,:true],:$conslist]
    nil

--------------------> NEW DEFINITION (override in patches.lisp.pamphlet)
compressHashTable ht ==
-- compresses hash table ht, to give maximal sharing of cells
  sayBrightlyNT '"compressing hash table..."
  $found: local := hashTable 'EQUAL
  for [x,:y] in entries ht repeat compressSexpr(y,nil,nil)
  sayBrightly   "done"
  ht

compressSexpr(x,left,right) ==
-- recursive version of compressHashTable
  x isnt [.,:.] => nil
  u:= tableValue($found,x) =>
    left => left.first := u
    right => right.rest := u
    nil
  compressSexpr(first x,x,nil)
  compressSexpr(rest x,nil,x)
  tableValue($found,x) := x

updateCategoryTable(cname,kind) ==
  kind is 'domain =>
    updateCategoryTableForDomain(cname,getConstrCat(
      getConstructorCategory cname))

updateCategoryTableForDomain(cname,category) ==
  clearCategoryTable(cname)
  [cname,:domainEntry]:= addDomainToTable(cname,category)
  for [a,:b] in encodeCategoryAlist(cname,domainEntry) repeat
    tableValue($HasCategoryTable,[cname,:a]) := b

clearCategoryTable($cname) ==
  MAPHASH('clearCategoryTable1,$HasCategoryTable)

clearCategoryTable1(key,val) ==
  key.op = $cname => tableRemove!($HasCategoryTable,key)
  nil
