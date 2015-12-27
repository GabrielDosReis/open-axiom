-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
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


import bc_-util
namespace BOOT

lefts u ==
   [x for [x,:.] in entries _*HASCATEGORY_-HASH_* | rest x = u]

--============================================================================
--              Build Library Database (libdb.text,...)
--============================================================================
--Formal for libdb.text:
--  constructors    Cname\#\I\sig \args   \abb \comments (C is C, D, P, X)
--  operations      Op  \#\E\sig \conname\pred\comments (E is one of U/E)
--  attributes      Aname\#\E\args\conname\pred\comments
--  I = <x if exposed><d if category with a default package>
buildLibdb(:options) ==  --called by buildDatabase (database.boot)
  domainList := IFCAR options  --build local libdb if list of domains is given
  $OpLst: local := nil
  $AttrLst: local := nil
  $DomLst : local := nil
  $CatLst : local := nil
  $PakLst : local := nil
  $DefLst : local := nil
  removeFile '"temp.text"
  $outStream: local := MAKE_-OUTSTREAM '"temp.text"
  if null domainList then
    comments :=
      '"\spad{Union(A,B,...,C)} is a primitive type in AXIOM used to represent objects of type \spad{A} or of type \spad{B} or...or of type \spad{C}."
    writedb
      buildLibdbString ['"dUnion",1,'"x",'"special",'"(A,B,...,C)",'union,comments]
    comments :=
      '"\spad{Record(a:A,b:B,...,c:C)} is a primitive type in AXIOM used to represent composite objects made up of objects of type \spad{A}, \spad{B},..., \spad{C} which are indexed by _"keys_" (identifiers) \spad{a},\spad{b},...,\spad{c}."
    writedb
      buildLibdbString ['"dRecord",1,'"x",'"special",'"(a:A,b:B,...,c:C)",'RECORD,comments]
    comments :=
      '"\spad{Mapping(T,S)} is a primitive type in AXIOM used to represent mappings from source type \spad{S} to target type \spad{T}. Similarly, \spad{Mapping(T,A,B)} denotes a mapping from source type \spad{(A,B)} to target type \spad{T}."
    writedb
      buildLibdbString ['"dMapping",1,'"x",'"special",'"(T,S)",'MAPPING,comments]
    comments :=
      '"\spad{Enumeration(a,b,...,c)} is a primitive type in AXIOM used to represent the object composed of the symbols \spad{a},\spad{b},..., and \spad{c}."
    writedb
      buildLibdbString ['"dEnumeration",1,'"x",'"special",'"(a,b,...,c)",'ENUM,comments]
  $conname: local := nil
  $conform: local := nil
  $exposed?:local := nil
  $doc:     local := nil
  $kind:    local := nil
  constructorList := domainList or allConstructors()
  for con in constructorList repeat
    writedb buildLibdbConEntry con
    [attrlist,:oplist] := getConstructorExports $conform
    buildLibOps oplist
    buildLibAttrs attrlist
  SHUT $outStream
  domainList => 'done         --leave new database in temp.text
  runCommand
    $machineType = 'RIOS => '"sort -f -T /tmp -y200 _"temp.text_"  > _"libdb.text_""
    $machineType = 'SPARC => '"sort -f  _"temp.text_"  > _"libdb.text_""
    '"sort  _"temp.text_"  > _"libdb.text_""
  renameFile('"libdb.text", '"olibdb.text")
  removeFile '"temp.text"

buildLibdbConEntry conname ==
    builtinConstructor? conname => nil
    abb:= getConstructorAbbreviationFromDB conname
    $conname := conname
    conform := getConstructorFormFromDB conname
    $conform := dbMkForm substitute("T","T$",conform)
    null $conform => nil
    $exposed? := (isExposedConstructor conname => '"x"; '"n")
    $doc      := getConstructorDocumentationFromDB conname
    kind  := getConstructorKindFromDB conname
    if kind = 'domain
      and getConstructorModemap conname is [[.,t,:.],:.]
       and t is ['CATEGORY,'package,:.] then kind := 'package
    $kind :=
      isDefaultPackageName conname => 'x
      DOWNCASE stringChar(symbolName kind,0)
    argl := rest $conform
    conComments :=
      symbolTarget('constructor,$doc) is [[=nil,:r]] =>
        libdbTrim concatWithBlanks r
      '""
    argpart:= subString(form2HtString ['f,:argl],1)
    sigpart:= libConstructorSig $conform
    header := strconc($kind,symbolName conname)
    buildLibdbString [header,#argl,$exposed?,sigpart,argpart,abb,conComments]

dbMkForm x ==
  x isnt [.,:.] and [x] or x

buildLibdbString [x,:u] ==
  strconc(STRINGIMAGE x,strconc/[strconc('"`",STRINGIMAGE y) for y in u])

libConstructorSig [conname,:argl] ==
  [[.,:sig],:.] := substitute("T","T$",getConstructorModemap conname)
  formals := take(#argl,$FormalMapVariableList)
  sig := applySubst(pairList($TriangleVariableList,formals),sig)
  keys := [g(f,sig,i) for f in formals for i in 1..] where
    g(x,u,i) ==  --does x appear in any but i-th element of u?
      or/[CONTAINED(x,y) for y in u for j in 1.. | j ~= i]
  sig := fn applySubst(pairList($FormalMapVariableList,argl),sig) where
    fn x ==
      x isnt [.,:.] => x
      x is ['Join,a,:r] => ['Join,fn a,'etc]
      x is ['CATEGORY,:.] => 'etc
      [fn y for y in x]
  sig := [first sig,:[(k => [":",a,s]; s)
            for a in argl for s in rest sig for k in keys]]
  form2LispString ['Mapping,:sig]

concatWithBlanks r ==
  r is [head,:tail] =>
    tail => strconc(head,'" ",concatWithBlanks tail)
    head
  '""

writedb(u) ==
  not string? u => nil        --skip if not a string
  --positions for tick(1), dashes(2), and address(9), i.e. 12
  writeLine(addPatchesToLongLines(u,500),$outStream)

addPatchesToLongLines(s,n) ==
  #s > n => strconc(subString(s,0,n),
              addPatchesToLongLines(strconc('"--",subString(s,n)),n))
  s

buildLibOps oplist == for [op,sig,:pred] in oplist repeat buildLibOp(op,sig,pred)

buildLibOp(op,sig,pred) ==
--operations      OKop  \#\sig \conname\pred\comments (K is U or C)
  nsig := applySubst(pairList($FormalMapVariableList,$conform.args),sig)
  pred := applySubst(pairList($FormalMapVariableList,$conform.args),pred)
  nsig := substitute("T","T$",nsig)   --this ancient artifact causes troubles!
  pred := substitute("T","T$",pred)
  sigpart:= form2LispString ['Mapping,:nsig]
  predString := (pred = 'T => '""; form2LispString pred)
  sop :=
    (s := STRINGIMAGE op) = '"One" => '"1"
    s = '"Zero" => '"0"
    s
  header := strconc('"o",sop)
  conform:= strconc($kind,form2LispString $conform)
  comments:= libdbTrim concatWithBlanks LASSOC(sig,LASSOC(op,$doc))
  checkCommentsForBraces('operation,sop,sigpart,comments)
  writedb
    buildLibdbString [header,# rest sig,$exposed?,sigpart,conform,predString,comments]

libdbTrim s ==
  k := maxIndex s
  k < 0 => s
  for i in 0..k repeat
    stringChar(s,i) = $Newline => stringChar(s,i) := char " "
  trimString s

checkCommentsForBraces(kind,sop,sigpart,comments) ==
  count := 0
  for i in 0..maxIndex comments repeat
    c := stringChar(comments,i)
    c = char "{" => count := count + 1
    c = char "}" =>
      count := count - 1
      count < 0 => missingLeft := true
  if count < 0 or missingLeft then
    tail :=
      kind = 'attribute => [sop,'"(",sigpart,'")"]
      [sop,'": ",sigpart]
    sayBrightly ['"(",$conname,'" documentation) missing left brace--> ",:tail]
  if count > 0 then
    sayBrightly ['"(",$conname,'" documentation) missing right brace--> ",:tail]
  if count ~= 0 or missingLeft then pp comments

buildLibAttrs attrlist ==
  for [name,argl,:pred] in attrlist repeat buildLibAttr(name,argl,pred)

buildLibAttr(name,argl,pred) ==
--attributes      AKname\#\args\conname\pred\comments (K is U or C)
  header := strconc('"a",STRINGIMAGE name)
  argPart:= subString(form2LispString ['f,:argl],1)
  pred := applySubst(pairList($FormalMapVariableList,$conform.args),pred)
  predString := (pred = 'T => '""; form2LispString pred)
  header := strconc('"a",STRINGIMAGE name)
  conname := strconc($kind,form2LispString $conname)
  comments:= concatWithBlanks LASSOC(['attribute,:argl],LASSOC(name,$doc))
  checkCommentsForBraces('attribute,STRINGIMAGE name,argl,comments)
  writedb
    buildLibdbString [header,# argl,$exposed?,argPart,conname,predString,comments]

dbAugmentConstructorDataTable() ==
  instream := MAKE_-INSTREAM '"libdb.text"
  while not eof? instream repeat
    fp   := getFileCursor instream
    line := readLine instream
    cname := makeSymbol dbName line
    entry := getCDTEntry(cname,true) =>  --skip over Mapping, Union, Record
       [name,abb,:.] := entry
       entry.rest.rest := PUTALIST(CDDR entry,'dbLineNumber,fp)
--     if xname := constructorHasExamplePage entry then
--       entry.rest := PUTALIST(CDDR entry,'dbExampleFile,xname)
       args := IFCDR getConstructorFormFromDB name
       if args then 
         entry.rest.rest := PUTALIST(CDDR entry,'constructorArgs,args)
  'done

dbHasExamplePage conname ==
  sname    := STRINGIMAGE conname
  abb      := getConstructorAbbreviationFromDB conname
  ucname   := stringUpcase STRINGIMAGE abb
  pathname :=strconc(systemRootDirectory(),'"/share/hypertex/pages/",ucname,'".ht")
  isExistingFile pathname => makeSymbol strconc(sname,'"XmpPage")
  nil

dbRead(n) ==
  instream := MAKE_-INSTREAM strconc(systemRootDirectory(), '"/algebra/libdb.text")
  setFileCursor(instream,n)
  line := readLine instream
  SHUT instream
  line ~= %nothing => line
  nil

dbReadComments(n) ==
  n = 0 => '""
  instream := MAKE_-INSTREAM strconc(systemRootDirectory(),'"/algebra/comdb.text")
  setFileCursor(instream,n)
  line := readLine instream
  k := dbTickIndex(line,1,1)
  line := subString(line,k + 1)
  while (x := readLine instream) ~= %nothing and
    (k := maxIndex x) and (j := dbTickIndex(x,1,1)) and (j < k) and
      x.(j := j + 1) = char "-" and x.(j := j + 1) = char "-" repeat
        xtralines := [subString(x,j + 1),:xtralines]
  closeFile instream
  strconc(line, strconc/reverse! xtralines)

dbSplitLibdb() ==
  instream := MAKE_-INSTREAM  '"olibdb.text"
  outstream:= MAKE_-OUTSTREAM '"libdb.text"
  comstream:= MAKE_-OUTSTREAM '"comdb.text"
  writeInteger(0,    comstream)
  writeChar($tick,comstream)
  writeLine('"",  comstream)
  while (line := readLine instream) ~= %nothing repeat
    outP := getFileCursor outstream
    comP := getFileCursor comstream
    [prefix,:comments] := dbSplit(line,6,1)
    PRINC(prefix,outstream)
    writeChar($tick ,outstream)
    null comments =>
      writeInteger(0,outstream)
      writeNewline outstream
    PRINC(comP,outstream)
    writeNewline outstream
    PRINC(outP  ,comstream)
    writeChar($tick ,comstream)
    PRINC(first comments,comstream)
    writeNewline comstream
    for c in rest comments repeat
      PRINC(outP  ,comstream)
      writeChar($tick ,comstream)
      PRINC(c, comstream)
      writeNewline comstream
  SHUT instream
  SHUT outstream
  SHUT comstream
  removeFile '"olibdb.text"

dbSplit(line,n,k) ==
  k := charPosition($tick,line,k + 1)
  n = 1 => [subString(line,0,k),:dbSpreadComments(subString(line,k + 1),0)]
  dbSplit(line,n - 1,k)

dbSpreadComments(line,n) ==
  line = '"" => nil
  k := charPosition(char "-",line,n + 2)
  k >= maxIndex line => [subString(line,n)]
  stringChar(line,k + 1) ~= char "-" =>
    u := dbSpreadComments(line,k)
    [strconc(subString(line,n,k - n),first u),:rest u]
  [subString(line,n,k - n),:dbSpreadComments(subString(line,k),0)]

--============================================================================
--                  Build Glossary
--============================================================================
buildGloss() ==  --called by buildDatabase (database.boot)
--starting with gloss.text, build glosskey.text and glossdef.text
  $constructorName : local := nil
  $exposeFlag : local := true
  $outStream: local := MAKE_-OUTSTREAM '"temp.text"
  $x : local := nil
  $attribute? : local := true     --do not surround first word
  pathname := strconc(systemRootDirectory(),'"doc/gloss.text")
  instream := MAKE_-INSTREAM pathname
  keypath  := '"glosskey.text"
  removeFile keypath
  outstream:= MAKE_-OUTSTREAM keypath
  htpath   := '"gloss.ht"
  removeFile htpath
  htstream:= MAKE_-OUTSTREAM htpath
  defpath  := '"glossdef.text"
  defstream:= MAKE_-OUTSTREAM defpath
  pairs := getGlossLines instream
  writeString('"\begin{page}{GlossaryPage}{G l o s s a r y}\beginscroll\beginmenu",htstream)
  for [name,:line] in pairs repeat
    outP  := getFileCursor outstream
    defP  := getFileCursor defstream
    lines := spreadGlossText transformAndRecheckComments(name,[line])
    PRINC(name, outstream)
    writeChar($tick,outstream)
    PRINC(defP, outstream)
    writeNewline outstream
    writeString('"\item\newline{\em \menuitemstyle{}}{\em ",htstream)
    PRINC(name,        htstream)
    writeLine('"}\space{}",htstream)
    for x in lines repeat
      PRINC(outP, defstream)
      writeChar($tick,defstream)
      PRINC(x,    defstream)
      writeNewline defstream
    writeLine(strconc/lines,htstream)
  writeString('"\endmenu\endscroll",htstream)
  writeString('"\lispdownlink{Search}{(|htGloss| _"\stringvalue{pattern}_")} for glossary entry matching \inputstring{pattern}{24}{*}",htstream)
  writeString('"\end{page}",htstream)
  SHUT instream
  SHUT outstream
  SHUT defstream
  SHUT htstream
  SHUT $outStream

spreadGlossText(line) ==
--this function breaks up a line into chunks
--eventually long line is put into gloss.text as several chunks as follows:
----- key1`this is the first chunk
----- XXX`and this is the second
----- XXX`and this is the third
----- key2`and this is the fourth
--where XXX is the file position of key1
--this is because grepping will only pick up the first 512 characters
  line = '"" => nil
  maxIndex line > 500 => [subString(line,0,500),:spreadGlossText(subString(line,500))]
  [line]

getGlossLines instream ==
--instream has text of the form:
----- key1`this is the first line
----- and this is the second
----- key2'and this is the third
--result is
----- key1'this is the first line and this is the second
----- key2'and this is the third
  keys := nil
  text := nil
  lastLineHadTick := false
  while (line := readLine instream) ~= %nothing repeat
    #line = 0 => 'skip
    n := charPosition($tick,line,0)
    last := IFCAR text
    n > maxIndex line =>  --this line is continuation of previous line; concat it
      fill :=
        #last = 0 =>
          lastLineHadTick => '""
          '"\blankline "
        #last > 0 and stringChar(last,maxIndex last) ~= $charBlank =>
          $charBlank
        '""
      lastLineHadTick := false
      text := [strconc(last,fill,line),:rest text]
    lastLineHadTick := true
    keys := [subString(line,0,n),:keys]
    text := [subString(line,n + 1),:text]
  ASSOCRIGHT listSort(function GLESSEQP,[[DOWNCASE key,key,:def] for key in keys for def in text])
  --this complication sorts them after lower casing the keys

--============================================================================
--                  Build Users HashTable
-- This database is written out as USERS.DATABASE (database.boot) and read using
-- function getUsersOfConstructor. See functions whoUses and kcuPage in browser.
--============================================================================
mkUsersHashTable() ==  --called by buildDatabase (database.boot)
  $usersTb := makeTable function scalarEq?
  for x in allConstructors() repeat
    for conform in getImports x repeat
      name := opOf conform
      if not (name in '(QUOTE)) then
        tableValue($usersTb,name) := insert(x,tableValue($usersTb,name))
  for [k,:v] in entries $usersTb repeat
    tableValue($usersTb,k) := listSort(function GLESSEQP,v)
  for x in allConstructors() | isDefaultPackageName x repeat
    tableValue($usersTb,x) := getDefaultPackageClients x
  $usersTb

getDefaultPackageClients con ==  --called by mkUsersHashTable
  catname := makeSymbol subString(s := symbolName con,0,maxIndex s)
  for [catAncestor,:.] in childrenOf([catname]) repeat
    pakname := makeDefaultPackageName symbolName catAncestor.op
    if getCDTEntry(pakname,true) then acc := [pakname,:acc]
    acc := union([CAAR x for x in domainsOf([catAncestor],nil)],acc)
  listSort(function GLESSEQP,acc)

--============================================================================
--               Build Dependents Hashtable
-- This hashtable is written out by database.boot as DEPENDENTS.DATABASE
-- and read back in by getDependentsOfConstructor (see database.boot)
-- This information is used by function kcdePage when a user asks for the
-- dependents of a constructor.
--============================================================================
mkDependentsHashTable() == --called by buildDatabase (database.boot)
  $depTb := makeTable function scalarEq?
  for nam in allConstructors() | not builtinConstructor? nam repeat
    for con in getArgumentConstructors nam repeat
      tableValue($depTb,con) := [nam,:tableValue($depTb,con)]
  for [k,:v] in entries $depTb repeat
    tableValue($depTb,k) := listSort(function GLESSEQP,v)
  $depTb

getArgumentConstructors con == --called by mkDependentsHashTable
  argtypes := IFCDR IFCAR getConstructorModemap con or return nil
  fn argtypes where
    fn(u) == "union"/[gn x for x in u]
    gn(x) ==
      x isnt [.,:.] => nil
      x is ['Join,:r] => fn(r)
      x is ['CATEGORY,:.] => nil
      constructor? first x => [first x,:fn rest x]
      fn rest x

getImports conname == --called by mkUsersHashTable
  conform := getConstructorFormFromDB conname
  infovec := dbInfovec conname or return nil
  template := infovec.0
  u := [doImport(i,template)
          for i in 5..(maxIndex template) | test]  where
    test() == template.i is [op,:.] and ident? op
              and not (op in '(Mapping Union Record Enumeration CONS QUOTE local %constant))
    doImport(x,template) ==
      x is [op,:args] =>
        op = 'QUOTE or op = '%eval => first args
        op = 'local => first args
        op = 'Record =>
          ['Record,:[[":",second y,doImport(third y,template)] for y in args]]

--TTT next three lines: handles some tagged/untagged Union case.
        op = 'Union=>
          args is [['_:,:x1],:x2] =>
--          CAAR args = '_: => -- tagged!
               ['Union,:[[":",second y,doImport(third y,template)] for y in args]]
          [op,:[doImport(y,template) for y in args]]

        [op,:[doImport(y,template) for y in args]]
      integer? x => doImport(template.x,template)
      x = '$ => '$
      x = "$$" => "$$"
      string? x => x
      systemError '"bad argument in template"
  listSort(function GLESSEQP,applySubst(pairList($FormalMapVariableList,conform.args),u))


--============================================================================
--                 Get Hierarchical Information
--============================================================================

$parentsCache := nil

parentsOf con == --called by kcpPage, ancestorsRecur
  if null $parentsCache then 
     $parentsCache := hashTable 'EQ
  tableValue($parentsCache,con) or
    parents := getParentsFor loadDBIfNecessary constructorDB con
    tableValue($parentsCache,con) := parents
    parents

++ Like `parentsOf', except that also handles builtin constructors.
genericParentsOf form ==
  builtinConstructor? form.op => parentsOfBuiltinInstance form
  parentsOf form.op

parentsOfForm(form is [op,:argl]) ==
  parents := genericParentsOf form
  argl = nil or argl = (newArgl := getConstructorFormFromDB(op).args) =>
    parents
  applySubst(pairList(newArgl,argl),parents)

descendantsOf(conform,domform) ==  --called by kcdPage
  "category" = getConstructorKindFromDB(conname := opOf conform) =>
    cats := catsOf(conform,domform)
    [op,:argl] := conform
    null argl or argl = (newArgl := rest getConstructorFormFromDB op)
        => cats
    applySubst(pairList(newArgl,argl),cats)
  'notAvailable

childrenOf conform ==
  [pair for pair in descendantsOf(conform,nil) |
    childAssoc(conform,parentsOfForm first pair)]

childAssoc(form,alist) ==
  null (argl := rest form) => assoc(form,alist)
  u := assocCar(opOf form, alist) => childArgCheck(argl,rest first u) and u
  nil

assocCar(x, al) == or/[pair for pair in al | x = CAAR pair]

childArgCheck(argl, nargl) ==
  and/[fn for x in argl for y in nargl for i in 0..] where
    fn() ==
      x = y or constructor? opOf y => true
      isSharpVar y => i = symbolPosition(y,$FormalMapVariableList)
      false

ancestorsOf(conform,domform) ==  --called by kcaPage, originsInOrder,...
  "category" = getConstructorKindFromDB(conname := opOf conform) =>
       alist := getConstructorAncestorsFromDB conname
       argl := IFCDR domform or IFCDR conform
       [pair for [a,:b] in alist | pair] where pair() ==
         left :=  sublisFormal(argl,a)
         right := sublisFormal(argl,b)
         if domform then right := simpHasPred right
         right = false => nil
         [left,:right]
  computeAncestorsOf(conform,domform)

computeAncestorsOf(conform,domform) ==
  $done: local := hashTable 'EQUAL
  $if:   local := hashTable 'EQ
  ancestorsRecur(conform,domform,true,true)
  acc := nil
  for op in listSort(function GLESSEQP,HKEYS $if) repeat
    for pair in tableValue($if,op) repeat acc := [pair,:acc]
  reverse! acc

ancestorsRecur(conform,domform,pred,firstTime?) == --called by ancestorsOf
  op      := opOf conform
  pred = tableValue($done,conform) => nil   --skip if already processed
  parents :=
    firstTime? => dbPrincipals constructorDB op
    genericParentsOf conform
  originalConform := genericInstanceForm conform
  if conform ~= originalConform then
    parents := applySubst(pairList(originalConform.args,conform.args),parents)
  for [newform,:p] in parents repeat
    if domform and domform.args then
      newdomform := applySubst(pairList(conform.args,domform.args),newform)
      p          := applySubst(pairList(conform.args,domform.args),p)
    newPred := quickAnd(pred,p)
    ancestorsAdd(simpHasPred newPred,newdomform or newform)
    ancestorsRecur(newform,newdomform,newPred,false)
  tableValue($done,conform) := pred            --mark as already processed

ancestorsAdd(pred,form) == --called by ancestorsRecur
  null pred => nil
  op := IFCAR form or form
  alist := tableValue($if,op)
  existingNode := assoc(form,alist) =>
    existingNode.rest := quickOr(rest existingNode,pred)
  tableValue($if,op) := [[form,:pred],:alist]

domainsOf(conform,domname,:options) ==
  $hasArgList := IFCAR options
  conname := opOf conform
  u := [key for [key,:.] in entries _*HASCATEGORY_-HASH_*
    | key is [anc,: =conname]]
  --u is list of pairs (a . b) where b() = conname
  --we sort u then replace each b by the predicate for which this is true
  s := listSort(function GLESSEQP,copyTree u)
  s := [[first pair,:constructorHasCategoryFromDB pair] for pair in s]
  transKCatAlist(conform,domname,listSort(function GLESSEQP,s))

catsOf(conform,domname,:options) ==
  $hasArgList := IFCAR options
  conname := opOf conform
  alist := nil
  for key in allConstructors() repeat
    for item in getConstructorAncestorsFromDB key | conname = CAAR item repeat
      [[op,:args],:pred] := item
      newItem :=
        args => [[args,:pred],:LASSOC(key,alist)]
        pred
      alist := insertShortAlist(key,newItem,alist)
  transKCatAlist(conform,domname,listSort(function GLESSEQP,alist))

transKCatAlist(conform,domname,s) == main where
  main() ==
    domname => --accept only exact matches after substitution
      domargs := rest domname
      acc := nil
      rest conform =>
        for pair in s repeat --pair has form [con,[conargs,:pred],...]]
          leftForm := getConstructorForm first pair
          for (ap := [args,:pred]) in rest pair repeat
            match? :=
              domargs = args => true
              HAS__SHARP__VAR args => domargs = sublisFormal(KDR domname,args)
              nil
            null match? => 'skip
            npred := sublisFormal(KDR leftForm,pred)
            acc := [[leftForm,:npred],:acc]
        reverse! acc
      --conform has no arguments so each pair has form [con,:pred]
      for pair in s repeat
        leftForm := getConstructorForm first pair or systemError nil
        pair.first := leftForm
        pair.rest := sublisFormal(KDR leftForm,rest pair)
      s
    --no domname, so look for special argument combinations
    acc := nil
    KDR conform =>
      farglist := take(#rest conform,$FormalMapVariableList)
      for pair in s repeat --pair has form [con,[conargs,:pred],...]]
        leftForm := getConstructorForm first pair
        for (ap := [args,:pred]) in rest pair repeat
          hasArgsForm? := args ~= farglist
          npred := sublisFormal(KDR leftForm,pred)
          if hasArgsForm? then
            subargs := sublisFormal(KDR leftForm,args)
            hpred :=
--            $hasArgsList => mkHasArgsPred subargs
              ['hasArgs,:subargs]
            npred := quickAnd(hpred,npred)
          acc := [[leftForm,:npred],:acc]
      reverse! acc
    for pair in s repeat --pair has form [con,:pred]
      leftForm := getConstructorForm first pair
      pair.first := leftForm
      pair.rest := sublisFormal(KDR leftForm,rest pair)
    s

mkHasArgsPred subargs ==
--$hasArgsList gives arguments of original constructor,e.g. LODO(A,M)
--M is required to be Join(B,...); in looking for the domains of B
--  we can find that if B has special value C, it can
  systemError subargs

sublisFormal(args,exp,:options) == main where
  main() ==  --use only on LIST structures; see also sublisFormalAlist
    $formals: local := IFCAR options or $FormalMapVariableList
    null args => exp
    sublisFormal1(args,exp,#args - 1)
  sublisFormal1(args,x,n) ==    --[sublisFormal1(args,y) for y in x]
    x is [.,:.] =>
      acc := nil
      y := x
      while cons? y repeat
        acc := [sublisFormal1(args,first y,n),:acc]
        y := rest y
      r := reverse! acc
      if y then
        nd := lastNode r
        nd.rest := sublisFormal1(args,y,n)
      r
    ident? x =>
      j := or/[i for f in $formals for i in 0..n | sameObject?(f,x)] =>
          args.j
      x
    x

--=======================================================================
--            Build Table of Lower Case Constructor Names
--=======================================================================

buildDefaultPackageNamesHT() ==
  $defaultPackageNamesHT := makeTable function scalarEq?
  for nam in allConstructors() | isDefaultPackageName nam repeat
    tableValue($defaultPackageNamesHT,nam) := true
  $defaultPackageNamesHT

$defaultPackageNamesHT := buildDefaultPackageNamesHT()

--=======================================================================
--            Code for Private Libdbs
--=======================================================================
-- $createLocalLibDb := false

extendLocalLibdb conlist ==   --  called by compileSpad2Cmd
  not $createLocalLibDb => nil
  null conlist => nil
  buildLibdb conlist          --> puts datafile into temp.text
  $newConstructorList := setUnion(conlist, $newConstructorList)
  localLibdb := '"libdb.text"
  not PROBE_-FILE '"libdb.text" =>
    RENAME_-FILE('"temp.text",'"libdb.text")
  oldlines := purgeNewConstructorLines(dbReadLines localLibdb, conlist)
  newlines := dbReadLines '"temp.text"
  dbWriteLines(MSORT union(oldlines,newlines), '"libdb.text")
  removeFile '"temp.text"

