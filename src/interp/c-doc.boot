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


import c_-util
import daase
namespace BOOT

$checkPrenAlist ==
  [[char "(",:char ")"],[char "{",:char "}"],[char "[",:char "]"]]

batchExecute() ==
  _/RF_-1 '(GENCON INPUT)

++ returns the documentation for operator `op' with given
++ `modemap', supposedly defined in domain or category
++ constructed from `conName'.
getDoc(conName,op,modemap) ==
  [dc,target,sl,pred,D] := simplifyModemap modemap
  sig := [target,:sl]
  cons? dc =>
    sig := MSUSBT('$,dc,sig)
    sig := applySubst(pairList(dc.args,$FormalMapVariableList),sig)
    getDocForDomain(conName,op,sig)
  if argList := IFCDR getOfCategoryArgument pred then
     applySubst(pairList(argList,$FormalMapArgumentList),sig)
  sig := MSUBST('$,dc,sig)
  getDocForCategory(conName,op,sig)

++ Given a predicate `pred' for a modemap, returns the first
++ argument to the ofCategory predicate it contains.  Return
++ nil otherwise.
getOfCategoryArgument pred ==
  pred is [fn,:.] and fn in '(AND OR NOT) =>
    or/[getOfCategoryArgument x for x in rest pred]
  pred is ['ofCategory,'_*1,form] => form
  nil

getDocForCategory(name,op,sig) ==
  getOpDoc(getConstructorAbbreviationFromDB name,op,sig) or
    or/[getOpDoc(getConstructorAbbreviationFromDB x,op,sig)
         for x in whatCatCategories name]

getDocForDomain(name,op,sig) ==
  getOpDoc(getConstructorAbbreviationFromDB name,op,sig) or
    or/[getOpDoc(getConstructorAbbreviationFromDB x,op,sig)
          for x in whatCatExtDom name]

++ returns the documentation, known to the global DB, for a operator
++ `op' and given signature `sigPart'.  The operator `op' is assumed
++ to have been defined in the domain or catagory `abb'.
getOpDoc(abb,op,:sigPart) ==
  u := symbolTarget(op,getConstructorDocumentationFromDB abb)
  $argList : local := $FormalMapVariableList
  _$: local := '_$
  sigPart is [sig] => or/[d for [s,:d] in u | sig = s]
  u

++ Parse the content of source file `fn' only for the purpose of
++ documentation.
readForDoc fn ==
  $bootStrapMode: local:= true
  _/RQ_-LIB_-1 [fn,'SPAD]

++ record the documentation location for an operator wih given
++ signature.
recordSignatureDocumentation(opSig,lineno) ==
  recordDocumentation(rest postTransform opSig,lineno)

recordAttributeDocumentation(['ATTRIBUTE,att],lineno) ==
  name := opOf att
  upperCase? stringChar(symbolName name,0) => nil
  recordDocumentation([name,['attribute,:IFCDR postTransform att]],lineno)

recordDocumentation(key,lineno) ==
  recordHeaderDocumentation lineno
  u:= collectComBlock lineno
  --record nil to mean "there was no documentation"
  $maxSignatureLineNumber := lineno
  $docList := [[key,:u],:$docList]
  -- leave first of $docList alone as required by collectAndDeleteAssoc

recordHeaderDocumentation lineno ==
  if $maxSignatureLineNumber = 0 then
    al := [p for (p := [n,:u]) in $COMBLOCKLIST
               | null n or null lineno or n < lineno]
    $COMBLOCKLIST := SETDIFFERENCE($COMBLOCKLIST,al)
    $headerDocumentation := ASSOCRIGHT al
    if $headerDocumentation then $maxSignatureLineNumber := 1 --see postDef
    $headerDocumentation

collectComBlock x ==
  $COMBLOCKLIST is [[=x,:val],:.] =>
    u := [:val,:collectAndDeleteAssoc x]
    $COMBLOCKLIST := rest $COMBLOCKLIST
    u
  collectAndDeleteAssoc x

collectAndDeleteAssoc x ==
--u is (.. (x . a) .. (x . b) .. )  ==> (a b ..) deleting entries from u
--assumes that the first element is useless
  for y in tails $COMBLOCKLIST | (s := rest y) repeat
    while s and first s is [=x,:r] repeat
      res := [:res,:r]
      s := rest s
      y.rest := s
  res

finalizeDocumentation db ==
  ctor := dbConstructor db
  unusedCommentLineNumbers := [x for (x := [n,:r]) in $COMBLOCKLIST | r]
  docList := substitute("$","%",transDocList($op,$docList))
  if u := [sig for [sig,:doc] in docList | null doc] then
    for y in u repeat
      y is 'constructor => noHeading := true
      y is [x,b] and b is ['attribute,:r] =>
        attributes := [[x,:r],:attributes]
      signatures := [y,:signatures]
    if noHeading or signatures or attributes or unusedCommentLineNumbers then
      sayKeyedMsg("S2CD0001",nil)
      bigcnt := 1
      if noHeading or signatures or attributes then
        sayKeyedMsg("S2CD0002",[strconc(STRINGIMAGE bigcnt,'"."),ctor])
        bigcnt := bigcnt + 1
        litcnt := 1
        if noHeading then
          sayKeyedMsg("S2CD0003",
            [strconc('"(",toString litcnt,'")"),ctor])
          litcnt := litcnt + 1
        if signatures then
          sayKeyedMsg("S2CD0004",
            [strconc('"(",toString litcnt,'")")])
          litcnt := litcnt + 1
          for [op,sig] in signatures repeat
            s := formatOpSignature(op,sig)
            sayMSG
              s isnt [.,:.] => ['%x9,s]
              ['%x9,:s]
        if attributes then
          sayKeyedMsg("S2CD0005",
            [strconc('"(",toString litcnt,'")")])
          litcnt := litcnt + 1
          for x in attributes repeat
            a := form2String x
            sayMSG
              a isnt [.,:.] => ['%x9,a]
              ['%x9,:a]
      if unusedCommentLineNumbers then
        sayKeyedMsg("S2CD0006",[strconc(toString bigcnt,'"."),ctor])
        for [n,r] in unusedCommentLineNumbers repeat
          sayMSG ['"   ",:bright n,'"   ",r]
  form := dbConstructorForm db
  hn [[:fn(sig,$e,form.args),:doc] for [sig,:doc] in docList] where
    fn(x,e,args) ==
      x isnt [.,:.] => [x,nil]
      if #x > 2 then x := take(2,x)
      applySubst(pairList(args,$FormalMapVariableList),macroExpand(x,e))
    hn u ==
     -- ((op,sig,doc), ...)  --> ((op ((sig doc) ...)) ...)
      opList := removeDuplicates ASSOCLEFT u
      [[op,:[[sig,doc] for [op1,sig,doc] in u | op = op1]] for op in opList]

--=======================================================================
--             Transformation of ++ comments
--=======================================================================
transDocList($constructorName,doclist) == --returns ((key line)...)
--called ONLY by finalizeDocumentation
--if $exposeFlag then messages go to file $outStream; flag=nil by default
  sayBrightly ['"   Processing ",$constructorName,'" for Browser database:"]
  commentList := transDoc($constructorName,doclist)
  acc := nil
  for entry in commentList repeat
    entry is ['constructor,x] =>
      conEntry => checkDocError ['"Spurious comments: ",x]
      conEntry := entry
    acc := [entry,:acc]
  conEntry => [conEntry,:acc]
  checkDocError1 ['"Missing Description"]
  acc

$attribute? := nil

++ Given a functor `conname', and a list of documenation strings,
++ sanity-check the documentation.  In particular extract information
++ such as `Description', etc.
transDoc(conname,doclist) ==
--$exposeFlag and not isExposedConstructor conname => nil
--skip over unexposed constructors when checking system files
  $x: local := nil
  rlist := reverse doclist
  for [$x,:lines] in rlist repeat
    $attribute? : local := $x is [.,[key]] and key is 'attribute
    null lines =>
      $attribute? => nil
      checkDocError1 ['"Not documented!!!!"]
    u := checkTrim($x,(string? lines => [lines]; $x is 'constructor => first lines; lines))
    $argl : local := nil    --set by checkGetArgs
-- tpd: related domain information doesn't exist
--    if v := checkExtract('"Related Domains:",u) then
--      $lisplibRelatedDomains:=[w for x in gn(v) | w := fn(x)] where
--        gn(v) ==  --note: unabbrev checks for correct number of arguments
--          s := checkExtractItemList v
--          parse := ncParseFromString s  --is a single conform or a tuple
--          null parse => nil
--          parse is ['Tuple,:r] => r
--          [parse]
--        fn(x) ==
--          expectedNumOfArgs := checkNumOfArgs x
--          null expectedNumOfArgs =>
--            checkDocError ['"Unknown constructor name?: ",opOf x]
--            x
--          expectedNumOfArgs ~= (n := #(IFCDR x)) =>
--            n = 0 => checkDocError1
--              ['"You must give arguments to the _"Related Domain_": ",x]
--            checkDocError
--              ['"_"Related Domain_" has wrong number of arguments: ",x]
--            nil
--          n=0 and x isnt [.,:.] => [x]
--          x
    longline :=
      $x is 'constructor =>
        v :=checkExtract('"Description:",u) or u and
              checkExtract('"Description:",
                [strconc('"Description: ",first u),:rest u])
        transformAndRecheckComments('constructor,v or u)
      transformAndRecheckComments($x,u)
    acc := [[$x,longline],:acc]  --processor assumes a list of lines
  reverse! acc

checkExtractItemList l ==  --items are separated by commas or end of line
  acc := nil               --l is list of remaining lines
  while l repeat           --stop when you get to a line with a colon
    m := maxIndex first l
    k := charPosition(char ":",first l,0)
    k <= m => return nil
    acc := [first l,:acc]
    l := rest l
  strconc/[x for x in reverse! acc]

++ Translate '%' in signature to '%%' for proper printing.
escapePercent x ==
  x is [y, :z] =>
    y1 := escapePercent y
    z1 := escapePercent z
    sameObject?(y, y1) and sameObject?(z, z1) => x
    [y1, :z1]
  x = "%" => "%%"
  x

transformAndRecheckComments(name,lines) ==
  $checkingXmptex? := false
  $x            : local := name
  $name         : local := 'GlossaryPage
  $origin       : local := 'gloss
  $recheckingFlag : local := false
  $exposeFlagHeading : local := 
    name isnt [.,:.] => ['"     -- ",name]
    concat('"     --",formatOpSignature(name.0, escapePercent name.1))
  if not $exposeFlag then sayBrightly $exposeFlagHeading
  u := checkComments(name,lines)
  $recheckingFlag := true
  checkRewrite(name,[u])
  $recheckingFlag := false
  u

checkRewrite(name,lines) == main where   --similar to checkComments from c-doc
  main() ==
    $checkErrorFlag: local := true
    margin := 0
    lines := checkRemoveComments lines
    u := lines
    if $checkingXmptex? then
      u := [checkAddIndented(x,margin) for x in u]
    $argl := checkGetArgs first u      --set $argl
    u2 := nil
    verbatim := nil
    for x in u repeat
        w := newString2Words x
        verbatim =>
          w and first w is '"\end{verbatim}" =>
            verbatim := false
            u2 := append(u2, w)
          u2 := append(u2, [x])
        w and first w is '"\begin{verbatim}" =>
            verbatim := true
            u2 := append(u2, w)
        u2 := append(u2, w)
    u := u2
    u := checkAddSpaces u
    u := checkSplit2Words u
    u := checkAddMacros u
    u := checkTexht u
--  checkBalance u
    okBefore := not $checkErrorFlag
    checkArguments u
    if $checkErrorFlag then u := checkFixCommonProblem u
    checkRecordHash u
--  u := checkTranVerbatim u
    checkDecorateForHt u

checkTexht u ==
  count := 0
  acc   := nil
  while u repeat
    x := first u
    if x is '"\texht" and (u := IFCDR u) then
        if not (IFCAR u = $charLbrace) then
           checkDocError '"First left brace after \texht missing"
        count := 1  -- drop first argument including braces of \texht
        while ((y := IFCAR (u := rest u))~= $charRbrace or count > 1) repeat
          if y = $charLbrace then count := count + 1
          if y = $charRbrace then count := count - 1
        x :=  IFCAR (u := rest u)  -- drop first right brace of 1st arg
    if x is '"\httex" and (u := IFCDR u) and (IFCAR u = $charLbrace) then
        acc := [IFCAR u,:acc]      --left  brace: add it
        while (y := IFCAR (u := rest u)) ~= $charRbrace repeat (acc := [y,:acc])
        acc := [IFCAR u,:acc]      --right brace: add it
        x :=  IFCAR (u := rest u)  --left brace:  forget it
        while IFCAR (u := rest u) ~= $charRbrace repeat 'skip
        x :=  IFCAR (u := rest u)  --forget right brace: move to next char
    acc := [x,:acc]
    u := rest u
  reverse! acc

checkRecordHash u ==
  while u repeat
    x := first u
    if string? x and stringChar(x,0) = $charBack then
      if member(x,$HTlinks) and (u := checkLookForLeftBrace IFCDR u)
           and (u := checkLookForRightBrace IFCDR u)
             and (u := checkLookForLeftBrace IFCDR u) and (u := IFCDR u) then
        htname := intern IFCAR u
        entry := tableValue($htHash,htname) or [nil]
        tableValue($htHash,htname) := [first entry,:[[$name,:$origin],:rest entry]]
      else if member(x,$HTlisplinks) and (u := checkLookForLeftBrace IFCDR u)
            and (u := checkLookForRightBrace IFCDR u)
              and (u := checkLookForLeftBrace IFCDR u) and (u := IFCDR u) then
        htname := intern checkGetLispFunctionName checkGetStringBeforeRightBrace u
        entry := tableValue($lispHash,htname) or [nil]
        tableValue($lispHash,htname) := [first entry,:[[$name,:$origin],:rest entry]]
      else if ((p := member(x,'("\gloss" "\spadglos")))
                 or (q := member(x,'("\glossSee" "\spadglosSee"))))
                    and (u := checkLookForLeftBrace IFCDR u)
                      and (u := IFCDR u) then
          if q then
             u := checkLookForRightBrace u
             u := checkLookForLeftBrace IFCDR u
             u := IFCDR u
          htname := intern checkGetStringBeforeRightBrace u
          entry := tableValue($glossHash,htname) or [nil]
          tableValue($glossHash,htname) := [first entry,:[[$name,:$origin],:rest entry]]
      else if x is '"\spadsys" and (u := checkLookForLeftBrace IFCDR u) and (u := IFCDR u) then
          s := checkGetStringBeforeRightBrace u
          if stringChar(s,0) = char ")" then s := subString(s,1)
          parse := checkGetParse s
          null parse => checkDocError ['"Unparseable \spadtype: ",s]
          not member(opOf parse,$currentSysList) =>
            checkDocError ['"Bad system command: ",s]
          parse isnt [.,:.] or (parse isnt ['set,arg]) => 'ok  ---assume ok
          not spadSysChoose($setOptions,arg) =>
            checkDocError ['"Incorrect \spadsys: ",s]
            entry := tableValue($sysHash,htname) or [nil]
            tableValue($sysHash,htname) := [first entry,:[[$name,:$origin],:rest entry]]
      else if x is '"\spadtype" and (u := checkLookForLeftBrace IFCDR u) and (u := IFCDR u) then
          s := checkGetStringBeforeRightBrace u
          parse := checkGetParse s
          null parse => checkDocError ['"Unparseable \spadtype: ",s]
          n := checkNumOfArgs parse
          null n => checkDocError ['"Unknown \spadtype: ", s]
          parse isnt [.,:.] and n > 0 => 'skip
          null (key := checkIsValidType parse) =>
            checkDocError ['"Unknown \spadtype: ", s]
          key isnt [.,:.] => 'ok
          checkDocError ['"Wrong number of arguments: ",form2HtString key]
      else if x in '("\spadop" "\keyword") and (u := checkLookForLeftBrace IFCDR u) and (u := IFCDR u) then
          x := intern checkGetStringBeforeRightBrace u
          not (property(x,'Led) or property(x,'Nud)) =>
            checkDocError ['"Unknown \spadop: ",x]
    u := rest u
  'done

checkGetParse s == ncParseFromString removeBackslashes s

++ remove non-leading backslash characters from string `s'.
removeBackslashes s ==
    s is '"" => '""
    (k := charPosition($charBack,s,0)) < #s =>
      k = 0 => removeBackslashes subString(s,1)
      strconc(subString(s,0,k),removeBackslashes subString(s,k + 1))
    s

++ returns the arity (as known to the global DB) of the functor
++ instantiated by `conform'.  Returns nil when `conform' does
++ not imply  aknown functor.
checkNumOfArgs conform ==
  conname := opOf conform
  constructor? conname or (conname := abbreviation? conname) =>
    #getConstructorArgsFromDB conname
  nil  --signals error

++ returns ok if correct, form if wrong number of arguments, nil if unknown
++ The check is down recursively on the argument to the instantiated functor.
checkIsValidType form == main where
  main() ==
    form isnt [.,:.] => 'ok
    [op,:args] := form
    conname := (constructor? op => op; abbreviation? op)
    null conname => nil
    fn(form,getDualSignature conname)
  fn(form,coSig) ==
    #form ~= #coSig => form
    or/[null checkIsValidType x for x in rest form for flag in rest coSig | flag]
      => nil
    'ok

checkGetLispFunctionName s ==
  n := #s
  (k := charPosition(char "|",s,1)) and k < n and
    (j := charPosition(char "|",s,k + 1)) and j < n => subString(s,k + 1,j-k-1)
  checkDocError ['"Ill-formed lisp expression : ",s]
  'illformed

checkGetStringBeforeRightBrace u ==
  acc := nil
  while u repeat
    x := first u
    x = $charRbrace => return strconc/(reverse! acc)
    acc := [x,:acc]
    u := rest u

--  checkTranVerbatim u ==
--    acc := nil
--    while u repeat
--      x := first u
--      x is '"\begin" and checkTranVerbatimMiddle u is [middle,:r] =>
--        acc := [$charRbrace,:middle,$charLbrace,'"\spadpaste",:acc]
--        u := r
--      if x is '"\spadcommand" then x := '"\spadpaste"
--      acc := [x,:acc]
--      u := rest u
--    reverse! acc
--
--  checkTranVerbatimMiddle u ==
--      (y := IFCAR (v := IFCDR u)) = $charLbrace and
--        (y := IFCAR (v := IFCDR v)) is '"verbatim" and
--          (y := IFCAR (v := IFCDR v)) = $charRbrace =>
--             w := IFCDR v
--             middle := nil
--             while w and (z := first w) isnt '"\end" repeat
--               middle := [z,:middle]
--               w := rest w
--             if (y := IFCAR (w := IFCDR w)) = $charLbrace and
--               (y := IFCAR (w := IFCDR w))  is '"verbatim" and
--                 (y := IFCAR (w := IFCDR w)) = $charRbrace then
--                    u := IFCDR w
--             else
--                checkDocError '"Missing \end{verbatim}"
--                u := w
--             [middle,:u]
--
--  checkTranVerbatim1 u ==
--    acc := nil
--    while u repeat
--      x := first u
--      x is '"\begin" and (y := IFCAR (v := IFCDR u)) = $charLbrace and
--        (y := IFCAR (v := IFCDR v)) is '"verbatim" and
--          (y := IFCAR (v := IFCDR v)) = $charRbrace =>
--             w := IFCDR v
--             middle := nil
--             while w and (z := first w) isnt '"\end" repeat
--               middle := [z,:middle]
--               w := rest w
--             if (y := IFCAR (w := IFCDR w)) = $charLbrace and
--               (y := IFCAR (w := IFCDR w))  is '"verbatim" and
--                 (y := IFCAR (w := IFCDR w)) = $charRbrace then
--                    u := IFCDR w
--             acc := [$charRbrace,:middle,$charLbrace,'"\spadpaste",:acc]
--      if x is '"\spadcommand" then x := '"\spadpaste"
--      acc := [x,:acc]
--      u := rest u
--    reverse! acc

appendOver [head,:tail] ==
 acc := lastNode head
 for x in tail repeat
   end := lastNode x
   acc.rest := x
   acc := end
 head

checkRemoveComments lines ==
  while lines repeat
    do
      line := checkTrimCommented first lines
      if firstNonBlankPosition line >= 0 then acc := [line,:acc]
    lines := rest lines
  reverse! acc

++ return the part of `line' that is not a comment.  A comment
++ is introduced by a leading percent character (%), or a double
++ percent character (%%).
checkTrimCommented line ==
  n := #line
  k := htcharPosition(char "%",line,0)
  --line beginning with % is a comment
  k = 0 => '""
  --remarks beginning with %% are comments
  k >= n - 1 or line.(k + 1) ~= char "%" => line
  k < #line => subString(line,0,k)
  line

htcharPosition(char,line,i) ==
  m := #line
  k := charPosition(char,line,i)
  k = m => k
  k > 0 =>
    line.(k - 1) ~= $charBack => k
    htcharPosition(char,line,k + 1)
  0

checkAddMacros u ==
  acc := nil
  verbatim := false
  while u repeat
    x := first u
    acc :=
      x is '"\end{verbatim}" =>
        verbatim := false
        [x, :acc]
      verbatim => [x, :acc]
      x is '"\begin{verbatim}" =>
        verbatim := true
        [x, :acc]
      y := LASSOC(x,$HTmacs) => [:y,:acc]
      [x,:acc]
    u := rest u
  reverse! acc

checkComments(nameSig,lines) == main where
  main() ==
    $checkErrorFlag: local := false
    margin := checkGetMargin lines
    if null $attribute? and nameSig isnt 'constructor then 
      lines :=
        [checkTransformFirsts(first nameSig,first lines,margin),:rest lines]
    u := checkIndentedLines(lines, margin)
    $argl := checkGetArgs first u      --set $argl
    u2 := nil
    verbatim := nil
    for x in u repeat
        w := newString2Words x
        verbatim =>
          w and first w is '"\end{verbatim}" =>
            verbatim := false
            u2 := append(u2, w)
          u2 := append(u2, [x])
        w and first w is '"\begin{verbatim}" =>
            verbatim := true
            u2 := append(u2, w)
        u2 := append(u2, w)
    u := u2
    u := checkAddSpaces u
    u := checkIeEg u
    u := checkSplit2Words u
    checkBalance u
    okBefore := null $checkErrorFlag
    checkArguments u
    if $checkErrorFlag then u := checkFixCommonProblem u
    v := checkDecorate u
    res := strconc/[y for y in v]
    res := checkAddPeriod res
    if $checkErrorFlag then pp res
    res

checkIndentedLines(u, margin) ==
  verbatim := false
  u2 := nil
  for x in u repeat
    k := firstNonBlankPosition x
    k = -1 =>
        verbatim => u2 := [:u2, $charFauxNewline]
        u2 := [:u2, '"\blankline "]
    s := subString(x, k)
    s is '"\begin{verbatim}" =>
        verbatim := true
        u2 := [:u2, s]
    s is '"\end{verbatim}" =>
        verbatim := false
        u2 := [:u2, s]
    verbatim => u2 := [:u2, subString(x, margin)]
    margin = k => u2 := [:u2, s]
    u2 := [:u2, strconc('"\indented{",toString(k-margin),'"}{",checkAddSpaceSegments(s,0),'"}")]
  u2

newString2Words l ==
  not string? l => [l]
  m := maxIndex l
  m = -1 => nil
  i := 0
  [w while newWordFrom(l,i,m) is [w,i]]

newWordFrom(l,i,m) ==
  while i <= m and stringChar(l,i) = char " " repeat i := i + 1
  i > m => nil
  buf := '""
  ch := stringChar(l,i)
  ch = $charFauxNewline => [$stringFauxNewline, i+ 1]
  done := false
  while i <= m and not done repeat
    ch := stringChar(l,i)
    ch = $charBlank or ch = $charFauxNewline => done := true
    buf := strconc(buf, charString ch)
    i := i + 1
  [buf,i]

checkAddPeriod s ==  --No, just leave blank at the end (rdj: 10/18/91)
  m := maxIndex s
  lastChar := stringChar(s,m)
  lastChar = char "!" or lastChar = char "?" or lastChar = char "." => s
  lastChar = char "," or lastChar = char ";" =>
    stringChar(s,m) := char "."
    s
  s

checkGetArgs u ==
  not string? u => nil
  m := maxIndex u
  k := firstNonBlankPosition(u)
  k > 0 => checkGetArgs subString(u,k)
  stringPrefix?('"\spad{",u) =>
    k := getMatchingRightPren(u,6,char "{",char "}") or m
    checkGetArgs subString(u,6,k-6)
  (i := charPosition(char "(",u,0)) > m => nil
  stringChar(u,m) ~= char ")" => nil
  while (k := charPosition($charComma,u,i + 1)) < m repeat
    acc := [trimString subString(u,i + 1,k - i - 1),:acc]
    i := k
  reverse! [subString(u,i + 1,m - i - 1),:acc]

checkGetMargin lines ==
  while lines repeat
    do
      x := first lines
      k := firstNonBlankPosition x
      k = -1 => nil
      margin := (margin => MIN(margin,k); k)
    lines := rest lines
  margin or 0

firstNonBlankPosition(x,:options) ==
  start := IFCAR options or 0
  k := -1
  for i in start..maxIndex x repeat
    if stringChar(x,i) ~= $charBlank then return (k := i)
  k

checkAddIndented(x,margin) ==
  k := firstNonBlankPosition x
  k = -1 => '"\blankline "
  margin = k => x
  strconc('"\indented{",toString(k-margin),'"}{",checkAddSpaceSegments(subString(x,k),0),'"}")

checkAddSpaceSegments(u,k) ==
  m := maxIndex u
  i := charPosition($charBlank,u,k)
  m < i => u
  j := i
  while (j := j + 1) < m and stringChar(u,j) = char " " repeat 'continue
  n := j - i   --number of blanks
  n > 1 => strconc(subString(u,0,i),'"\space{",
             toString n,'"}",checkAddSpaceSegments(subString(u,i + n),0))
  checkAddSpaceSegments(u,j)

checkTrim($x,lines) == main where
  main() ==
    s := [wherePP first lines]
    for x in rest lines repeat
      j := wherePP x
      if not scalarMember?(j,s) then
        checkDocError [$x,'" has varying indentation levels"]
        s := [j,:s]
    [trim y for y in lines]
  wherePP(u) ==
    k := charPosition($charPlus,u,0)
    k = #u or charPosition($charPlus,u,k + 1) ~= k + 1 =>
      systemError '" Improper comment found"
    k
  trim(s) ==
    k := wherePP(s)
    return subString(s,k + 2)
    m := maxIndex s
    n := k + 2
    for j in (k + 2)..m while stringChar(s,j) = $charBlank repeat (n := n + 1)
    subString(s,n)

checkExtract(header,lines) ==
  while lines repeat
    line := first lines
    k := firstNonBlankPosition line           --k gives margin of Description:
    substring?(header,line,k) => return nil
    lines := rest lines
  null lines => nil
  u := first lines
  j := charPosition(char ":",u,k)
  margin := k
  firstLines :=
    (k := firstNonBlankPosition(u,j + 1)) ~= -1 =>
      [subString(u,j + 1),:rest lines]
    rest lines
  --now look for another header; if found skip all rest of these lines
  acc := nil
  for line in firstLines repeat
    do
      m := #line
      (k := firstNonBlankPosition line) = -1 => 'skip  --include if blank
      k > margin                             => 'skip  --include if idented
      not upperCase? stringChar(line,k)      => 'skip  --also if not upcased
      (j := charPosition(char ":",line,k)) = m   => 'skip  --or if not colon, or
      (i := charPosition(char " ",line,k+1)) < j => 'skip  --blank before colon
      return nil
    acc := [line,:acc]
  reverse! acc

checkFixCommonProblem u ==
  acc := nil
  while u repeat
    x := first u
    x = $charLbrace and member(next := IFCAR rest u,$HTspadmacros) and
                       (IFCAR IFCDR rest u ~= $charLbrace) =>
      checkDocError ['"Reversing ",next,'" and left brace"]
      acc := [$charLbrace,next,:acc]  --reverse order of brace and command
      u := rest rest u
    acc := [x,:acc]
    u := rest u
  reverse! acc

checkDecorate u ==
  count := 0           -- number of enclosing opening braces
  spadflag := false    --means OK to wrap single letter words with \s{}
  mathSymbolsOk := false
  acc := nil
  verbatim := false    -- true if inside `verbatim' environment
  while u repeat
    x := first u

    if not verbatim then
      if x is '"\em" then
        if count > 0 then
          mathSymbolsOk := count - 1
          spadflag := count - 1
        else checkDocError ['"\em must be enclosed in braces"]
      if string? x and x in '("\spadpaste" "\spad" "\spadop") then
        mathSymbolsOk := count
      if string? x and x in '("\s" "\spadtype" "\spadsys" "\example" "\andexample" "\spadop" "\spad" "\spadignore" "\spadpaste" "\spadcommand" "\footnote") then
        spadflag := count
      else if x = $charLbrace then
        count := count + 1
      else if x = $charRbrace then
        count := count - 1
        if mathSymbolsOk = count then mathSymbolsOk := false
        if spadflag = count then spadflag := false
      else if not mathSymbolsOk and x in '("+" "*" "=" "==" "->") then
        if $checkingXmptex? then
          checkDocError ["Symbol ",x,'" appearing outside \spad{}"]

    acc :=
      x is '"\end{verbatim}" =>
        verbatim := false
        [x, :acc]
      verbatim => [x, :acc]
      x is '"\begin{verbatim}" =>
        verbatim := true
        [x, :acc]

      x is '"\begin" and first(v := IFCDR u) = $charLbrace and
        first(v := IFCDR v) is '"detail" and first(v := IFCDR v) = $charRbrace
          =>
            u := v
            ['"\blankline ",:acc]
      x is '"\end" and first(v := IFCDR u) = $charLbrace and
        first(v := IFCDR v) is '"detail" and first(v := IFCDR v) = $charRbrace
          =>
            u := v
            acc
      char? x and x = char "$" or x is '"$"  => ['"\$",:acc]
      char? x and x = char "%" or x is '"%"  => ['"\%",:acc]
      char? x and x = char "," or x is '","  => 
        spadflag => ['",",:acc]
        ['",{}",:acc]
      x is '"\spad" => ['"\spad",:acc]
      string? x and digit? stringChar(x,0) => [x,:acc]
      not spadflag and
        (char? x and alphabetic? x and not charMember?(x,$charExclusions) or
          member(x,$argl)) => [$charRbrace,x,$charLbrace,'"\spad",:acc]
      not spadflag and string? x and ((x.0 ~= $charBack and digit?(x.(maxIndex x))) or x in '("true" "false")) =>
        [$charRbrace,x,$charLbrace,'"\spad",:acc]  --wrap x1, alpha3, etc
      xcount := (string? x => # x; 0)
      xcount = 3 and x.1 = char "t" and x.2 = char "h" =>
        ['"th",$charRbrace,x.0,$charLbrace,'"\spad",:acc]
      xcount = 4 and x.1 = char "-" and x.2 = char "t" and x.3 = char "h" =>
        ['"-th",$charRbrace,x.0,$charLbrace,'"\spad",:acc]
      not spadflag and (xcount = 2 and x.1 = char "i" or  --wrap ei, xi, hi
         xcount > 0 and xcount < 4 and not x in '("th" "rd" "st") and
           hasNoVowels x) =>                    --wrap words with no vowels
             [$charRbrace,x,$charLbrace,'"\spad",:acc]
      [checkAddBackSlashes x,:acc]
    u := rest u
  reverse! acc

hasNoVowels x ==
  max := maxIndex x
  stringChar(x,max) = char "y" => false
  and/[not isVowel stringChar(x,i) for i in 0..max]

isVowel c ==
  c=char "a" or c=char "e" or c=char "i" or c=char "o" or c=char "u" or
    c=char "A" or c=char "E" or c=char "I" or c=char "O" or c=char "U"


checkAddBackSlashes s ==
  (char? s and (c := s)) or (#s = 1 and (c := stringChar(s,0))) =>
    charMember?(s,$charEscapeList) => strconc($charBack,charString c)
    s
  k := 0
  m := maxIndex s
  insertIndex := nil
  while k <= m repeat
    do
      c := stringChar(s,k)
      c = $charBack => k := k + 2
      charMember?(c,$charEscapeList) => return (insertIndex := k)
    k := k + 1
  insertIndex => checkAddBackSlashes strconc(subString(s,0,insertIndex),$charBack,s.k,subString(s,insertIndex + 1))
  s

checkAddSpaces u ==
  null u => nil
  null rest u => u
  space := $charBlank
  u2 := nil
  for i in 1.. for f in u repeat
    -- want newlines before and after begin/end verbatim and between lines
    -- since this might be written to a file, we can't really use
    -- newline characters. The Browser and HD will do the translation
    -- later.
    if f is '"\begin{verbatim}" then
        space := $charFauxNewline
        if null u2 then u2 := [space]

    if i > 1 then u2 := [:u2, space, f]
    else u2 := [:u2, f]

    if f is '"\end{verbatim}" then
        u2 := [:u2, space]
        space := $charBlank
  u2

checkIeEg u ==
  acc := nil
  verbatim := false
  while u repeat
    x := first u
    acc :=
      x is '"\end{verbatim}" =>
        verbatim := false
        [x, :acc]
      verbatim => [x, :acc]
      x is '"\begin{verbatim}" =>
        verbatim := true
        [x, :acc]
      z := checkIeEgfun x => [:reverse! z,:acc]
      [x,:acc]
    u := rest u
  reverse! acc

checkIeEgfun x ==
  CHARP x => nil
  x is '"" => nil
  m := maxIndex x
  for k in 0..(m - 3) repeat
    x.(k + 1) = $charPeriod and x.(k + 3) = $charPeriod and
     (x.k = char "i" and x.(k + 2) = char "e" and (key := '"that is")
       or x.k = char "e" and x.(k + 2) = char "g" and (key := '"for example")) =>
          firstPart := (k > 0 => [subString(x,0,k)]; nil)
          result := [:firstPart,'"\spadignore{",subString(x,k,4),'"}",
                     :checkIeEgfun subString(x,k+4)]
  result

checkSplit2Words u ==
  acc := nil
  while u repeat
    x := first u
    acc :=
      x is '"\end{verbatim}" =>
        verbatim := false
        [x, :acc]
      verbatim => [x, :acc]
      x is '"\begin{verbatim}" =>
        verbatim := true
        [x, :acc]
      z := checkSplitBrace x => [:reverse! z,:acc]
      [x,:acc]
    u := rest u
  reverse! acc

checkSplitBrace x ==
  CHARP x => [x]
  #x = 1 => [x.0]
  (u := checkSplitBackslash x)
     and rest u  => "append"/[checkSplitBrace y for y in u]
  m := maxIndex x
  (u := checkSplitOn x)
     and rest u  => "append"/[checkSplitBrace y for y in u]
  (u := checkSplitPunctuation x)
     and rest u  => "append"/[checkSplitBrace y for y in u]
  [x]

checkSplitBackslash x ==
  not string? x => [x]
  m := maxIndex x
  (k := charPosition($charBack,x,0)) < m =>
    m = 1 or alphabetic?(x . (k + 1)) =>        --starts with a backslash so..
      (k := charPosition($charBack,x,1)) < m => --..see if there is another
         [subString(x,0,k),:checkSplitBackslash subString(x,k)]  -- yup
      [x]                                       --no, just return line
    k = 0 => --starts with backspace but x.1 is not a letter; break it up
      [subString(x,0,2),:checkSplitBackslash subString(x,2)]
    u := subString(x,0,k)
    v := subString(x,k,2)
    k + 1 = m => [u,v]
    [u,v,:checkSplitBackslash subString(x,k + 2)]
  [x]

checkSplitPunctuation x ==
  not string? x => [x]
  m := maxIndex x
  m < 1 => [x]
  lastchar := x.m
  lastchar = $charPeriod and stringChar(x,m - 1) = $charPeriod =>
    m = 1 => [x]
    m > 3 and stringChar(x,m-2) = $charPeriod =>
      [:checkSplitPunctuation subString(x,0,m-2),'"..."]
    [:checkSplitPunctuation subString(x,0,m-1),'".."]
  lastchar = $charPeriod or lastchar = $charSemiColon or lastchar = $charComma
    => [subString(x,0,m),lastchar]
  m > 1 and stringChar(x,m - 1) = $charQuote =>
    [subString(x,0,m - 1),subString(x,m-1)]
  (k := charPosition($charBack,x,0)) < m =>
    k = 0 =>
      m = 1 or tableValue($htMacroTable,x) or alphabetic? x.1 => [x]
      v := subString(x,2)
      [subString(x,0,2),:checkSplitPunctuation v]
    u := subString(x,0,k)
    v := subString(x,k)
    [:checkSplitPunctuation u,:checkSplitPunctuation v]
  (k := charPosition($charDash,x,1)) < m =>
    u := subString(x,k + 1)
    [subString(x,0,k),$charDash,:checkSplitPunctuation u]
  [x]

checkSplitOn(x) ==
  not string? x => [x]
  l := $charSplitList
  m := maxIndex x
  while l repeat
    char := first l
    do
      m = 0 and stringChar(x,0) = char => return (k := -1)  --special exit
      k := charPosition(char,x,0)
      k > 0 and x.(k - 1) = $charBack => [x]
      k <= m => return k
    l := rest l
  null l => [x]
  k = -1 => [char]
  k = 0 => [char,subString(x,1)]
  k = maxIndex x => [subString(x,0,k),char]
  [subString(x,0,k),char,:checkSplitOn subString(x,k + 1)]


checkBalance u ==
  checkBeginEnd u
  stack := nil
  while u is [x,:u] repeat
    not char? x => nil
    closer := scalarTarget(x,$checkPrenAlist) => --is it an open bracket?
      stack := [closer,:stack]                   --yes, push the open bracket
    open := rassoc(x,$checkPrenAlist) =>         --it is a close bracket!
      stack is [top,:restStack] => --does corresponding open bracket match?
        x = top => stack := restStack            --yes: just pop the stack
        checkDocError
          ['"Mismatch: left ",checkSayBracket open,'" matches right ",checkSayBracket x]
      checkDocError ['"Missing left ",checkSayBracket x]
  stack ~= nil =>
    for x in reverse! stack repeat
      checkDocError ['"Missing right ",checkSayBracket x]
  nil

++ returns the class of the parenthesis x
++    pren    ::= '(' | ')'
++    brace   ::= '{' | '}'
++    bracket ::= '[' | ']'
checkSayBracket x ==
  x = char "(" or x = char ")" => '"pren"
  x = char "{" or x = char "}" => '"brace"
  '"bracket"

checkBeginEnd u ==
  beginEndStack := nil
  while u repeat
    IDENTITY
      x := first u
      string? x and x.0 = $charBack and #x > 2 and not tableValue($htMacroTable,x)
        and not (x is '"\spadignore") and IFCAR IFCDR u = $charLbrace
          and not
            (substring?('"\radiobox",x,0) or substring?('"\inputbox",x,0))=>
             --allow 0 argument guys to pass through
              checkDocError ["Unexpected HT command: ",x]
      x is '"\beginitems" =>
        beginEndStack := ["items",:beginEndStack]
      x is '"\begin" =>
        u is [.,=$charLbrace,y,:r] and first r = $charRbrace =>
          if not member(y,$beginEndList) then
            checkDocError ['"Unknown begin type: \begin{",y,'"}"]
          beginEndStack := [y,:beginEndStack]
          u := r
        checkDocError ['"Improper \begin command"]
      x is '"\item" =>
        member(IFCAR beginEndStack,'("items" "menu")) => nil
        null beginEndStack =>
          checkDocError ['"\item appears outside a \begin-\end"]
        checkDocError ['"\item appears within a \begin{",IFCAR beginEndStack,'"}.."]
      x is '"\end" =>
        u is [.,=$charLbrace,y,:r] and first r = $charRbrace =>
          y = IFCAR beginEndStack =>
            beginEndStack := rest beginEndStack
            u := r
          checkDocError ['"Trying to match \begin{",IFCAR beginEndStack,'"} with \end{",y,"}"]
        checkDocError ['"Improper \end command"]
    u := rest u
  beginEndStack => checkDocError ['"Missing \end{",first beginEndStack,'"}"]
  'ok

checkArguments u ==
  while u repeat
    do
      x := first u
      null (k := tableValue($htMacroTable,x)) => 'skip
      k = 0 => 'skip
      k > 0 => checkHTargs(x,rest u,k,nil)
      checkHTargs(x,rest u,-k,true)
    u := rest u
  u

checkHTargs(keyword,u,nargs,integerValue?) ==
--u should start with an open brace ...
   nargs = 0 => 'ok
   if not (u := checkLookForLeftBrace u) then
     return checkDocError ['"Missing argument for ",keyword]
   if not (u := checkLookForRightBrace IFCDR u) then
     return checkDocError ['"Missing right brace for ",keyword]
   checkHTargs(keyword,rest u,nargs - 1,integerValue?)

checkLookForLeftBrace(u) ==   --return line beginning with left brace
  while u repeat
    x := first u
    if x = $charLbrace then return u
    x ~= $charBlank => return nil
    u := rest u
  u

checkLookForRightBrace(u) ==  --return line beginning with right brace
  count := 0
  while u repeat
    x := first u
    do
      x = $charRbrace =>
        count = 0 => return (found := u)
        count := count - 1
      x = $charLbrace => count := count + 1
    u := rest u
  found

checkInteger s ==
  CHARP s => false
  s is '"" => false
  and/[digit? stringChar(s,i) for i in 0..maxIndex s]

checkTransformFirsts(opname,u,margin) ==
--case 1: \spad{...
--case 2: form(args)
--case 3: form arg
--case 4: op arg
--case 5: arg op arg
  namestring :=
    opname is ['Zero] => '"0"
    opname is ['One] => '"1"
    symbolName opname
  margin > 0 =>
    s := leftTrim u
    strconc(fillerSpaces margin,checkTransformFirsts(opname,s,0))
  m := maxIndex u
  m < 2 => u
  stringChar(u,0) = $charBack => u
  alphabetic? u.0 =>
    i := checkSkipToken(u,0,m) or return u
    j := checkSkipBlanks(u,i,m) or return u
    open := stringChar(u,j)
    open = char "[" and (close := char "]") or
          open = char "("  and (close := char ")") =>
      k := getMatchingRightPren(u,j + 1,open,close)
      namestring ~= (firstWord := subString(u,0,i)) =>
        checkDocError ['"Improper first word in comments: ",firstWord]
        u
      null k =>
         if open = char "["
           then checkDocError ['"Missing close bracket on first line: ", u]
           else checkDocError ['"Missing close parenthesis on first line: ", u]
         u
      strconc('"\spad{",subString(u,0,k + 1),'"}",subString(u,k + 1))
    k := checkSkipToken(u,j,m) or return u
    infixOp := makeSymbol subString(u,j,k - j)
    null property(infixOp,'Led) =>                                --case 3
      namestring ~= (firstWord := subString(u,0,i)) =>
        checkDocError ['"Improper first word in comments: ",firstWord]
        u
      #(p := symbolName infixOp) = 1 and (open := p.0) and
        (close := scalarTarget(open,$checkPrenAlist)) =>  --have an open bracket
          l := getMatchingRightPren(u,k + 1,open,close)
          if l > maxIndex u then l := k - 1
          strconc('"\spad{",subString(u,0,l + 1),'"}",subString(u,l + 1))
      strconc('"\spad{",subString(u,0,k),'"}",subString(u,k))
    l := checkSkipBlanks(u,k,m) or return u
    n := checkSkipToken(u,l,m) or return u
    namestring ~= symbolName infixOp =>
      checkDocError ['"Improper initial operator in comments: ",infixOp]
      u
    strconc('"\spad{",subString(u,0,n),'"}",subString(u,n))   --case 5
  true =>          -- not alphabetic? u.0 =>
    i := checkSkipToken(u,0,m) or return u
    namestring ~= (firstWord := subString(u,0,i)) =>
      checkDocError ['"Improper first word in comments: ",firstWord]
      u
    prefixOp := makeSymbol subString(u,0,i)
    not property(prefixOp,'Nud) =>
      u ---what could this be?
    j := checkSkipBlanks(u,i,m) or return u
    u.j = char "(" =>                                            --case 4
      j := getMatchingRightPren(u,j + 1,char "(",char ")")
      j > m => u
      strconc('"\spad{",subString(u,0,j + 1),'"}",subString(u,j + 1))
    k := checkSkipToken(u,j,m) or return u
    namestring ~= (firstWord := subString(u,0,i)) =>
      checkDocError ['"Improper first word in comments: ",firstWord]
      u
    strconc('"\spad{",subString(u,0,k),'"}",subString(u,k))

getMatchingRightPren(u,j,open,close) ==
  count := 0
  m := maxIndex u
  for i in j..m repeat
    c := stringChar(u,i)
    do
      c = close =>
        count = 0 => return (found := i)
        count := count - 1
      c = open => count := count + 1
  found

checkSkipBlanks(u,i,m) ==
  while i < m and stringChar(u,i) = $charBlank repeat i := i + 1
  i = m => nil
  i

checkSkipToken(u,i,m) ==
  alphabetic? stringChar(u,i) => checkSkipIdentifierToken(u,i,m)
  checkSkipOpToken(u,i,m)

checkSkipOpToken(u,i,m) ==
  while i < m and
    (not(checkAlphabetic(u.i)) and not(member(u.i,$charDelimiters))) repeat
      i := i + 1
  i = m => nil
  i

checkSkipIdentifierToken(u,i,m) ==
  while i < m and checkAlphabetic u.i repeat i := i + 1
  i = m => nil
  i

++ returns true if character `c' is alphabetic.
checkAlphabetic c ==
  alphabetic? c or digit? c or charMember?(c,$charIdentifierEndings)

--=======================================================================
--        Code for creating a personalized report for ++ comments
--=======================================================================
docreport(nam) ==
--creates a report for person "nam" using file "whofiles"
  removeFile '"docreport.input"
  runCommand strconc('"echo _")bo setOutStream('",STRINGIMAGE nam,'")_" > temp.input")
  runCommand '"cat docreport.header temp.input > docreport.input"
  runCommand strconc('"awk '/",STRINGIMAGE nam,'"/ {printf(_")co %s.spad\n_",$2)}' whofiles > temp.input")
  runCommand '"cat docreport.input temp.input > temp1.input"
  runCommand '"cat temp1.input docreport.trailer > docreport.input"
  removeFile '"temp.input"
  removeFile '"temp1.input"
  $editFile := '"docreport.input"
  _/RQ()

setOutStream nam ==
  filename := strconc('"/tmp/",STRINGIMAGE nam,".docreport")
  $outStream := MAKE_-OUTSTREAM filename

whoOwns(con) ==
  null $exposeFlag => nil
--con=constructor name (id beginning with a capital), returns owner as a string
  filename := getConstructorSourceFileFromDB con
  quoteChar := char "_""
  runCommand strconc('"awk '$2 == ",quoteChar,filename,quoteChar,'" {print $1}' whofiles > /tmp/temp")
  instream := MAKE_-INSTREAM '"/tmp/temp"
  value := readLine instream
  SHUT instream
  value ~= %nothing => value
  nil

--=======================================================================
--             Report Documentation Error
--=======================================================================
++ True if we are compiling only documentation.
$compileDocumentation := false

checkDocError1 u ==
--when compiling for documentation, ignore certain errors
  $compileDocumentation => nil
  checkDocError u

checkDocError u ==
  $checkErrorFlag := true
  msg :=
    $recheckingFlag =>
      $constructorName => checkDocMessage u
      concat('"> ",u)
    $constructorName => checkDocMessage u
    u
  if $exposeFlag and $exposeFlagHeading then
    sayBrightly1($exposeFlagHeading,$outStream)
    sayBrightly $exposeFlagHeading
    $exposeFlagHeading := nil
  sayBrightly msg
  if $exposeFlag then sayBrightly1(msg,$outStream)
  --if called by checkDocFile (see file checkdoc.boot)

++ Augment `u' with information about the owner of the source file
++ containing the current functor definition being processed.
checkDocMessage u ==
  sourcefile := getConstructorSourceFileFromDB $constructorName
  person := whoOwns $constructorName or '"---"
  middle :=
    not null $x => ['"(",$x,'"): "]
    ['": "]
  concat(person,'">",sourcefile,'"-->",$constructorName,middle,u)

checkDecorateForHt u ==
  count := 0
  spadflag := false    --means OK to wrap single letter words with \s{}
  while u repeat
    x := first u
    do
      if x is '"\em" then
        if count > 0 then spadflag := count - 1
        else checkDocError ['"\em must be enclosed in braces"]
      if x in '("\s" "\spadop" "\spadtype" "\spad" "\spadpaste" "\spadcommand" "\footnote") then spadflag := count
      else if x = $charLbrace then count := count + 1
      else if x = $charRbrace then
        count := count - 1
        if spadflag = count then spadflag := false
      else if not spadflag and x in '("+" "*" "=" "==" "->") then
        if $checkingXmptex? then
          checkDocError ["Symbol ",x,'" appearing outside \spad{}"]
      x is '"$" or x is '"%" => checkDocError ['"Unescaped ",x]
--      not spadflag and string? x and (member(x,$argl) or #x = 1
--        and alphabetic? x.0) and not (x in '("a" "A")) =>
--          checkDocError1 ['"Naked ",x]
--      not spadflag and string? x and (not x.0 = $charBack and not digit?(x.0) and digit? stringChar(x,maxIndex x) or x in '("true" "false"))
--        => checkDocError1 ["Naked ",x]
    u := rest u
  u

