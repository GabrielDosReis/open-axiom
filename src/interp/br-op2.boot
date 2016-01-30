-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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


import br_-op1
namespace BOOT

--=======================================================================
--                   Operation Description
--=======================================================================


htSayConstructor(key,u) ==
  u is ['CATEGORY,kind,:r] =>
    htSay('"a ",kind,'" ")
    htSayExplicitExports(r)
  key is "is" =>
    htSay '"the domain "
    bcConform(u,true)
  htSay
    key is "is" => '"the domain "
    kind := getConstructorKindFromDB opOf u
    kind is "domain" => '"an element of "
    '"a domain of "
  u is ['Join,:middle,r] =>
    rest middle =>
      htSay '"categories "
      bcConform(first middle,true)
      for x in rest middle repeat
        htSay '", "
        bcConform(x,true)
      r is ['CATEGORY,.,:r] =>
        htSay '" and "
        htSayExplicitExports(r)
      htSay '" and "
      bcConform(r,true)
    htSay '"category "
    bcConform(first middle,true)
    r is ['CATEGORY,.,:r] =>
     htSay '" "
     htSayExplicitExports(r)
    htSay '" and "
    bcConform(r,true)
  htSay(kind,'" ")
  bcConform(u,true)

htSayExplicitExports r ==
  htSay '"with explicit exports"
  $displayReturnValue => nil
  htSay '":"
  for x in r repeat
    htSay '"\newline "
    x is ['SIGNATURE,op,sig,:.] =>
      ops := escapeSpecialChars STRINGIMAGE op
      htMakePage [['bcLinks,[ops,'"",'oPage,ops]]]
      htSay '": "
      bcConform ['Mapping,:sig]
    x is ['ATTRIBUTE,a] =>
      s := form2HtString a
      htMakePage [['bcLinks,[ops,'"",'aPage,s]]]
    x is ['IF,:.] =>
      htSay('"{\em if ...}")
    systemError()

displayBreakIntoAnds pred ==
  pred is [op,:u] and op in '(and AND) => u
  [pred]

htSayValue t ==
  t is ['Mapping,target,:source] =>
      htSay('"a function from ")
      htSayTuple source
      htSay '" to "
      htSayArgument target
  t is '(Category) => htSay('"a category")
  t is [op,:.] and op in '(Join CATEGORY) or constructor? opOf t =>
    htSayConstructor(nil,t)
  htSay('"an element of domain ")
  htSayArgument t                            --continue for operations

htSayArgument t == --called only for operations not for constructors
  null $signature => htSay ['"{\em ",t,'"}"]
  t in '(_$ _%) =>
    $conkind = '"category" and $conlength > 20 =>
      $generalSearch? => htSay '"{\em D} of the origin category"
      addWhereList("$",'is,nil)
      htSayStandard '"{\em $}"
      htSaySaturn '"{\em \%}"
    htSayStandard '"{\em $}"
    htSaySaturn '"{\em \%}"
  not ident? t => bcConform(t,true)
  k := position(t,$conargs)
  if k > -1 then
    typeOfArg := (rest $signature).k
    addWhereList(t,'member,typeOfArg)
  htSay('"{\em ",t,'"}")

addWhereList(id,kind,typ) ==
  $whereList := insert([id,kind,:typ],$whereList)

htSayTuple t ==
  null t => htSay '"()"
  null rest t => htSayArgument first t
  htSay '"("
  htSayArgument first t
  for d in rest t repeat
    htSay '","
    htSayArgument d
  htSay '")"

dbGetDisplayFormForOp(op,sig,doc) ==
  dbGetFormFromDocumentation(op,sig,doc) or dbGetContrivedForm(op,sig)

dbGetFormFromDocumentation(op,sig,x) ==
  doc := (string? x => x; first x)
  string? doc and
     (stringPrefix?('"\spad{",doc) and (k := 6) or
       stringPrefix?('"\s{",doc) and (k := 3)) =>
    n := charPosition(char "}",doc,k)
    s := subString(doc,k,n - k)
    parse := ncParseFromString s
    parse is [=op,:.] and #parse = #sig => parse
  nil

dbMakeContrivedForm(op,sig,:options) ==
  $chooseDownCaseOfType : local := IFCAR options
  $NumberList : local := '(i j k l m n i1 j1 k1 l1 m1 n1 i2 j2 k2 l2 m2 n2 i3 j3 k3 l3 m3 n3 i4 j4 k4 l4 m4 n4 )
  $ElementList: local := '(x y z u v w x1 y1 z1 u1 v1 w1 x2 y2 z2 u2 v2 w2 x3 y3 z3 u3 v3 w3 x4 y4 z4 u4 v4 w4 )
  $FunctionList:local := '(f g h d e F G H)
  $DomainList:  local := '(R S D E T A B C M N P Q U V W)
  dbGetContrivedForm(op,sig)

dbGetContrivedForm(op,sig) ==
  op is '"0" => [0]
  op is '"1" => [1]
  [op,:[dbChooseOperandName s for s in rest sig]]

dbChooseOperandName(typ) ==
  typ is ['Mapping,:.] =>
    x := first $FunctionList
    $FunctionList := rest $FunctionList
    x
  name := opOf typ
  kind :=
    name is "$" => 'domain
    getConstructorKindFromDB name
  s := symbolName opOf typ
  kind isnt 'category =>
    anySubstring?('"Integer",s,0) or anySubstring?('"Number",s,0) =>
      x := first $NumberList
      $NumberList := rest $NumberList
      x
    x :=
      $chooseDownCaseOfType =>
        y := DOWNCASE typ
        x :=
          member(y,$ElementList) => y
          first $ElementList
      first $ElementList
    $ElementList := remove($ElementList,x)
    x
  x := first $DomainList
  $DomainList := rest $DomainList
  x

getSubstSigIfPossible sig ==
  getSubstSignature sig or sig

--
--  while (u := getSubstSignature sig) repeat
--     sig := u
--  sig

fullSubstitute(x,y,z) ==  --substitutes deeply: x for y in list z
  z = y => x
  z isnt [.,:.] => z
  [fullSubstitute(x,y,u) for u in z]

getSubstCandidates sig ==
  candidates := nil
  for x in sig for i in 1.. | x is [.,.,:.] repeat
    getSubstQualify(x,i,sig) => candidates := getSubstInsert(x,candidates)
    y := or/[getSubstQualify(y,i,sig) for y in rest x | y is [.,.,:.]] =>
      candidates := insert(y,candidates)
  candidates

getSubstSignature sig ==
    candidates := getSubstCandidates sig
    null candidates => nil
    D := first $DomainList
    $DomainList := rest $DomainList
    winner := first candidates
    newsig := fullSubstitute(D,winner,sig)
    sig :=
      null rest candidates => newsig
      count := NUMOFNODES newsig
      for x in rest candidates repeat
        trial := fullSubstitute(D,x,sig)
        trialCount := NUMOFNODES trial
        trialCount < count =>
          newsig := trial
          count  := trialCount
          winner := x
      newsig
    addWhereList(D,'is,winner)
    newsig

getSubstQualify(x,i,sig) ==
    or/[CONTAINED(x,y) for y in sig for j in 1.. | j ~= i] => x
    false

getSubstInsert(x,candidates) ==
    return insert(x,candidates)
    null candidates => [x]
    or/[CONTAINED(x,y) for y in candidates] => candidates
    y := or/[CONTAINED(y,x) for y in candidates] => substitute(x,y,candidates)
    candidates


--=======================================================================
--                      Who Uses
--=======================================================================
whoUsesOperation(htPage,which,key) ==  --see dbPresentOps
  key is 'filter => koaPageFilterByName(htPage,'whoUsesOperation)
  opAlist := htpProperty(htPage,'opAlist)
  conform := htpProperty(htPage,'conform)
  conargs := rest conform
  opl := nil
  for [op,:alist] in opAlist repeat
    for [sig,:.] in alist repeat
      opl := [[op,:applySubst(pairList(conform.args,$FormalMapVariableList),sig)],:opl]
  opl := reverse! opl
  u := whoUses(opl,conform)
  prefix := pluralSay(#u,'"constructor uses",'"constructors use")
  suffix :=
    opAlist is [[op1,.]] =>
      ['" operation {\em ",escapeSpecialChars STRINGIMAGE op1,'":",form2HtString ['Mapping,:sig],'"}"]
    ['" these operations"]
  page := htInitPage([:prefix,:suffix],htCopyProplist htPage)
  nopAlist := nil
  for [name,:opsigList] in u repeat
    for opsig in opsigList repeat
      sofar    := LASSOC(opsig,nopAlist)
      nopAlist := insertAlist(opsig,[name,:LASSOC(opsig,nopAlist)],nopAlist)
  usedList := nil
  for [pair := [op,:sig],:namelist] in nopAlist repeat
    ops := escapeSpecialChars STRINGIMAGE op
    usedList := [pair,:usedList]
    htSay('"Users of {\em ",ops,'": ")
    bcConform ['Mapping,:sublisFormal(conargs,sig)]
    htSay('"}\newline")
    bcConTable listSort(function GLESSEQP,removeDuplicates namelist)
  noOneUses := SETDIFFERENCE(opl,usedList)
  if #noOneUses > 0 then
    htSay('"No constructor uses the ")
    htSay
      #noOneUses = 1 => '"operation: "
      [#noOneUses,'" operations:"]
    htSay '"\newline "
    for [op,:sig] in noOneUses repeat
      htSay('"\tab{2}{\em ",escapeSpecialChars STRINGIMAGE op,'": ")
      bcConform ['Mapping,:sublisFormal(conargs,sig)]
      htSay('"}\newline")
  htSayStandard '"\endscroll "
  dbPresentOps(page,which,'usage)
  htShowPageNoScroll()

whoUses(opSigList,conform) ==
  opList := removeDuplicates ASSOCLEFT opSigList
  numOfArgsList := removeDuplicates [-1 + #sig for [.,:sig] in opSigList]
  acc  := nil
  $conname : local := first conform
  domList := getUsersOfConstructor $conname
  for name in allConstructors() | symbolMember?(name,domList) repeat
    $infovec : local := dbInfovec name
    null $infovec => 'skip           --category
    template := $infovec . 0
    found := false
    opacc := nil
    for i in 7..maxIndex template repeat
      item := vectorRef(template,i)
      item isnt [n,:op] or not symbolMember?(op,opList) => 'skip
      index := n
      numvec := getCodeVector()
      numOfArgs := arrayRef(numvec,index)
      not scalarMember?(numOfArgs,numOfArgsList) => 'skip
      whereNumber := arrayRef(numvec,index := index + 1)
      vectorRef(template,whereNumber) isnt [= $conname,:.] => 'skip
      signumList := dcSig(numvec,index + 1,numOfArgs)
      opsig := or/[pair for (pair := [op1,:sig]) in opSigList | op1 = op and whoUsesMatch?(signumList,sig,nil)]
        => opacc := [opsig,:opacc]
    if opacc then acc := [[name,:opacc],:acc]
  acc

whoUsesMatch?(signumList,sig,al) ==
  #signumList = #sig and whoUsesMatch1?(signumList,sig,al)

whoUsesMatch1?(signumList,sig,al) ==
  signumList is [subject,:r] and sig is [pattern,:s] =>
    x := LASSOC(pattern,al) =>
      x = subject => whoUsesMatch1?(r,s,al)
      false
    pattern is '_$ =>
      subject is [= $conname,:.] => whoUsesMatch1?(r,s,[['_$,:subject],:al])
      false
    whoUsesMatch1?(r,s,[[pattern,:subject],:al])
  true

--=======================================================================
--                   Get Attribute/Operation Alist
--=======================================================================

koAttrs(conform,domname) ==
  [conname,:args] := conform
  "category" = getConstructorKindFromDB conname =>
      koCatAttrs(conform,domname)
  $infovec: local := dbInfovec conname or return nil
  $predvec: local :=
    $domain => domainPredicares $domain
    getConstructorPredicates conname
  u := [[a,:pred] for [a,:i] in $infovec . 2 | a isnt 'nil and (pred := sublisFormal(args,kTestPred i))]
                                               ---------  CHECK for a = nil
  listSort(function GLESSEQP,fn u) where fn u ==
    alist := nil
    for [a,:pred] in u repeat
      op := opOf a
      args := IFCDR a
      alist := insertAlist(op,insertAlist(args,[pred],LASSOC(op,alist)),alist)
    alist

koOps(conform,domname,:options) == main where
--returns alist of form ((op (sig . pred) ...) ...)
  main() ==
    $packageItem: local := nil
--  relatives? := IFCAR options
    ours :=
--    relatives? = 'onlyRelatives => nil
      fn(conform,domname)
--    if relatives? then
--      relatives := relativesOf(conform,domname)
--      if domname then relatives :=
--      applySubst(pairList(['_$,:conform.args],[domname,:domname.args]),relatives)
--      --kill all relatives that have a sharp variable remaining in them
--      for x in relatives repeat
--      or/[y for y in CDAR x | isSharpVar y] => 'skip
--      acc := [x,:acc]
--      relatives := reverse! acc
--      for (pair := [pakform,:.]) in relatives repeat
--      $packageItem := sublisFormal(rest conform,pair)
--      ours := merge(fn(pakform,nil),ours)
    listSort(function GLESSEQP,trim ours)
  trim u == [pair for pair in u | IFCDR pair]
  fn(conform,domname) ==
    conform := domname or conform
    [conname,:args] := conform
    subargs: local := args
    ----------> new <------------------
    u := koCatOps(conform,domname) => u
--    "category" = getConstructorKindFromDB conname =>
--        koCatOps(conform,domname)
    ----------> new <------------------
    $infovec: local := dbInfovec conname--------> removed 94/10/24
    exposureTail :=
      null $packageItem => '(NIL NIL)
      isExposedConstructor opOf conform => [conform,:'(T)]
      [conform,:'(NIL)]
    for [op,:u] in getConstructorOperationsFromDB conname repeat
      op1 := zeroOneConvert op
      acc :=
       [[op1,:[[sig,npred,:exposureTail] for [sig,slot,pred,key,:.] in sublisFormal(subargs,u) |
         (key ~= 'Subsumed) and (npred := simpHasPred pred)]],:acc]
    acc
  merge(alist,alist1) == --alist1 takes precedence
    for [op,:al] in alist1 repeat
      u := LASSOC(op,alist) =>
        for [sig,:item] in al | not LASSOC(sig,u) repeat
          u := insertAlist(sig,item,u)
        alist := insertAlist(op,u,DELASC(op,alist)) --add the merge of two alists
      alist := insertAlist(op,al,alist)  --add the whole inner alist
    alist

zeroOneConvert x ==
  x is 'Zero => 0
  x is 'One  => 1
  x

kFormatSlotDomain x == fn formatSlotDomain x where fn x ==
  x isnt [.,:.] => x
  (op := first x) is '_$ => '_$
  op is 'local => second x
  op is ":" => [":",second x,fn third x]
  ident? op and isConstructorName op => [fn y for y in x]
  integer? op => op
  op is 'QUOTE and second x isnt [.,:.] => second x
  x

koCatOps(conform,domname) ==
  conname := opOf conform
  oplist := reverse getConstructorOperationsFromDB conname
  oplist := sublisFormal(IFCDR domname or IFCDR conform ,oplist)
  --check below for integer? key to avoid subsumed signatures
  [[zeroOneConvert op,:nalist] for [op,:alist] in oplist | nalist := koCatOps1(alist)]

koCatOps1 alist == [x for item in alist | x := pair] where
  pair() ==
    [sig,:r] := item
    null r => [sig,true]
    [key,:options] := r
    null (pred := IFCAR options) =>
      IFCAR IFCDR options = 'ASCONST => [sig,'ASCONST]
      [sig,true]
    npred := simpHasPred pred => [sig,npred]
    false

koCatAttrs(catform,domname) ==
  $if: local := hashTable 'EQ
  catname   := opOf catform
  koCatAttrsAdd(domname or catform,true)
  ancestors := ancestorsOf(catform,domname)
  for [conform,:pred] in ancestors repeat koCatAttrsAdd(conform,pred)
  hashTable2Alist $if

hashTable2Alist tb ==
  [[op,:tableValue(tb,op)] for op in listSort(function GLESSEQP,HKEYS $if)]

koCatAttrsAdd(catform,pred) ==
  for [name,argl,:p] in first getConstructorExports catform repeat
    npred  := quickAnd(pred,p)
    exists := tableValue($if,name)
    if existingPred := LASSOC(argl,exists)_
        then npred := quickOr(npred,existingPred)
    if not (name in '(nil nothing)) _
        then tableValue($if,name) := [[argl,simpHasPred npred],:exists]

--=======================================================================
--            Filter by Category
--=======================================================================

koaPageFilterByCategory(htPage,calledFrom) ==
  opAlist := htpProperty(htPage,'opAlist)
  which   := htpProperty(htPage,'which)
  page := htInitPageNoScroll(htCopyProplist htPage,
             dbHeading(opAlist,which,htpProperty(htPage,'heading)))
  htSay('"Select a category ancestor below or ")
  htMakePage [['bcLispLinks,['"filter",'"on:",calledFrom,'filter]]]
  htMakePage [['bcStrings, [13,'"",'filter,'EM]]]
  htSay('"\beginscroll ")
  conform := htpProperty(htPage,'conform)
  domname := htpProperty(htPage,'domname)
  ancestors := ASSOCLEFT ancestorsOf(conform,domname)
  htpSetProperty(page,'ancestors,listSort(function GLESSEQP,ancestors))
  bcNameCountTable(ancestors,'form2HtString,'koaPageFilterByCategory1,true)
  htShowPage()

dbHeading(items,which,heading,:options) ==
  names?   := IFCAR options
  count :=
    names? => #items
    +/[#(rest x) for x in items]
  capwhich := capitalize which
  prefix :=
    count < 2 =>
      names? => pluralSay(count,strconc(capwhich,'" Name"),nil)
      pluralSay(count,capwhich,nil)
    names? => pluralSay(count,nil,strconc(capwhich,'" Names"))
    pluralSay(count,nil,pluralize capwhich)
  [:prefix,'" for ",:heading]

koaPageFilterByCategory1(htPage,i) ==
  ancestor := (htpProperty(htPage,'ancestors)) . i
  ancestorList := [ancestor,:ASSOCLEFT ancestorsOf(ancestor,nil)]
  newOpAlist := nil
  which    := htpProperty(htPage,'which)
  opAlist  := htpProperty(htPage,'opAlist)
  domname  := htpProperty(htPage,'domname)
  conform  := htpProperty(htPage,'conform)
  heading  := htpProperty(htPage,'heading)
  docTable := dbDocTable(domname or conform)
  for [op,:alist] in opAlist repeat
    nalist := [[origin,:item] for item in alist | split]
      where split() ==
        [sig,pred,:aux] := item
        u := dbGetDocTable(op,sig,docTable,which,aux)
        origin := IFCAR u
        doc    := IFCDR u
        true
    for [origin,:item] in nalist | origin repeat
      member(origin,ancestorList) =>
        newEntry   := [item,:LASSOC(op,newOpAlist)]
        newOpAlist := insertAlist(op,newEntry,newOpAlist)
  falist := nil
  for [op,:alist] in newOpAlist repeat
    falist := [[op,:reverse! alist],:falist]
  htpSetProperty(htPage,'fromcat,['" from category {\sf ",form2HtString ancestor,'"}"])
  dbShowOperationsFromConform(htPage,which,falist)

--=======================================================================
--           New code for search operation alist for exact matches
--=======================================================================

opPageFast opAlist == --called by oSearch
  htPage := htInitPage(nil,nil)
  htpSetProperty(htPage,'opAlist,opAlist)
  htpSetProperty(htPage,'expandOperations,'lists)
  which := '"operation"
--dbResetOpAlistCondition(htPage,which,opAlist)
  dbShowOp1(htPage,opAlist,which,'names)

opPageFastPath opstring ==
--return nil
  x := STRINGIMAGE opstring
  charPosition(char "*",x,0) < #x => nil     --quit if name has * in it
  op := (string? x => makeSymbol x; x)
  mmList := getAllModemapsFromDatabase(op,nil) or return nil
  opAlist := [[op,:[item for mm in mmList]]] where item() ==
    [predList, origin, sig] := modemap2Sig(op, mm)
    predicate := predList and mkpf(predList,'AND)
    exposed? := isExposedConstructor opOf origin
    [sig, predicate, origin, exposed?]
  opAlist

modemap2Sig(op,mm) ==
  [dcSig, conds] := mm
  [dc, :sig] := dcSig
  partial? :=
    conds is ['partial,:r] => conds := r
    false
  condlist := modemap2SigConds conds
  [origin, vlist, flist] := getDcForm(dc, condlist) or return nil
  subcondlist := applySubst(pairList(vlist,flist),condlist)
  [predList,vlist, flist] := getSigSubst(subcondlist, nil, vlist, flist)
  if partial? then
    target := dcSig . 1
    ntarget := ['Union, target, '"failed"]
    dcSig := substitute(ntarget, target, dcSig)
  alist := findSubstitutionOrder? pairList(vlist, flist) or systemError()
  predList := substInOrder(alist, predList)
  nsig := substInOrder(alist, sig)
  if hasPatternVar nsig or hasPatternVar predList then
    pp '"--------------"
    pp op
    pp predList
    pp nsig
    pp mm
--pause nsig
  [predList, origin, substitute("%", origin, nsig)]

modemap2SigConds conds ==
  conds is ['OR,:r] => modemap2SigConds first r
  conds is ['AND,:r] => r
  [conds]

hasPatternVar x ==
  ident? x and (x ~= "**") => isPatternVar x
  x isnt [.,:.] => false
  or/[hasPatternVar y for y in x]

getDcForm(dc, condlist) ==
  [ofWord,id,cform] := or/[x for x in condlist | x is [k,=dc,:.]
     and k in '(ofCategory isDomain)] or return nil
  conform := getConstructorForm opOf cform
  ofWord is 'ofCategory =>
    [conform, ["*1", :rest cform], ["%", :rest conform]]
  ofWord is 'isDomain =>
    [conform, ["*1", :rest cform], ["%", :rest conform]]
  systemError()

getSigSubst(u, pl, vl, fl) ==
  u is [item, :r] =>
    item is ['AND,:s] =>
       [pl, vl, fl] := getSigSubst(s, pl, vl, fl)
       getSigSubst(r, pl, vl, fl)
    [key, v, f] := item
    key is 'isDomain => getSigSubst(r, pl, [v, :vl], [f, :fl])
    key is 'ofCategory => getSigSubst(r, pl, ['D, :vl], [f, :fl])
    key is 'ofType    => getSigSubst(r, pl, vl, fl)
    key is "has" => getSigSubst(r, [item, :pl], vl, fl)
    key is 'not => getSigSubst(r, [item, :pl], vl, fl)
    systemError()
  [pl, vl, fl]
