-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2012, Gabriel Dos Reis.
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

-- New file as of 6/95
$aixTestSaturn := false
--These will be set in patches.lisp:
--$saturn := false  --true to write SATURN output to $browserOutputStream
$standard := true  --true to write browser output on AIX
$saturnAmpersand := '"\&\&"
$saturnFileNumber --true to write DOS files for Thinkpad (testing only)
   := false
$kPageSaturnArguments := nil  --bound by $kPageSaturn
$atLeastOneUnexposed := false
$saturnContextMenuLines := nil
$saturnContextMenuIndex := 0
$saturnMacros := '(
  "\def\unixcommand#1#2{{\em #1}}"_
  "\def\lispFunctionLink#1#2{\lispLink[d]{#1}{{\bf #2}}}"_
  "\def\lispTypeLink#1#2{\lispLink[d]{#1}{{\sf #2}}}"_
  "\def\menuitemstyle{\menubutton}"_
  "\def\browseTitle#1{\windowTitle{#1}\section{#1}}"_
  "\def\ttrarrow{$\rightarrow$}"_
  "\def\spadtype#1{\lispLink[d]{\verb!(|spadtype| '|#1|)!}{\sf #1}}"_
  "\def\spad#1{{\em #1}}"_
  "\def\spadfun#1{{\em #1}}"_
 )
$FormalFunctionParameterList := '(_#_#1 _#_#2 _#_#3 _#_#4 _#_#5 _#_#6 _#_#7 _#_#8 _#_#9 _#_#10 _#_#11 _#_#12 _#_#13 _#_#14 _#_#15)

on() ==
  $saturn := true
  $standard := false
off()==
  $saturn := false
  $standard := true

--=======================================================================
--            Function for testing SATURN output
--=======================================================================
-- protectedEVAL x ==
--  $saturn =>
--    protectedEVAL0(x, true, false)
--    if $aixTestSaturn then protectedEVAL0(x, false, true)
--  protectedEVAL1 x
--
--protectedEVAL0(x, $saturn, $standard) ==
--  protectedEVAL1 x
--
--protectedEVAL1 x ==
--  error := true
--  val := nil
--  UNWIND_-PROTECT((val := saturnEVAL x; error := nil),
--                   error => (resetStackLimits(); sendHTErrorSignal()))
--  val
--
--saturnEVAL x ==
--  fn :=
--    $aixTestSaturn => '"/tmp/sat.text"
--    '"/windows/temp/browser.text"
--  $saturn =>
--    saturnEvalToFile(x, fn)
--    displayTextFile  '"/tmp/sat.text"
--  eval x


--=======================================================================
--            Functions to write DOS files to disk
--=======================================================================
ts(command) ==
  $saturn := true
  $saturnFileNumber := false
  $standard := false
  saturnEvalToFile(command, '"/tmp/sat.text")

ut() ==
  $saturn := false
  $standard := true
  'done

onDisk() ==
  $saturnFileNumber := 1
  obey '"dosdir"

offDisk() ==
  $saturnFileNumber := false

page() ==
  $standard => $curPage
  $saturnPage
--=======================================================================
--            Functions that affect $saturnPage
--=======================================================================

htSay(x,:options) ==  --say for possibly both $saturn and standard code
  htSayBind(x, options)

htSayCold x ==
  htSay '"\lispLink{}{"
  htSay x
  htSay '"}"

htSayIfStandard(x, :options) ==  --do only for $standard
  $standard => htSayBind(x,options)

htSayStandard(x, :options) ==  --do AT MOST for $standard
  $saturn: local := nil
  htSayBind(x, options)

htSaySaturn(x, :options) ==    --do AT MOST for $saturn
  $standard: local := nil
  htSayBind(x, options)

htSayBind(x, options) ==
  bcHt x
  for y in options repeat bcHt y

bcHt line ==
  $newPage =>  --this path affects both saturn and old lines
    text :=
      cons? line => [['text, :line]]
      string? line => line
      [['text, line]]
    if $saturn then htpAddToPageDescription($saturnPage, text)
    if $standard then htpAddToPageDescription($curPage, text)
  cons? line =>
    $htLineList := append!(reverse! mapStringize copyList line, $htLineList)
  $htLineList := [basicStringize line, :$htLineList]

--=======================================================================
--                        New issueHT
--=======================================================================

htShowPage() ==
-- show the page which has been computed
  htSayStandard '"\endscroll"
  htShowPageNoScroll()

htShowPageNoScroll() ==
-- show the page which has been computed
  htSayStandard '"\autobuttons"
  if $standard then
    htpSetPageDescription($curPage, reverse! htpPageDescription $curPage)
  if $saturn then
    htpSetPageDescription($saturnPage, reverse! htpPageDescription $saturnPage)
  $newPage := false
  ----------------------
  if $standard then
    $htLineList := nil
    htMakePage htpPageDescription $curPage
    if $htLineList then line := apply(function strconc, reverse! $htLineList)
    issueHTStandard line
  ----------------------
  if $saturn then
    $htLineList := nil
    htMakePage htpPageDescription $saturnPage
    if $htLineList then line := apply(function strconc, reverse! $htLineList)
    issueHTSaturn line
  ----------------------
  endHTPage()

--------------------> NEW DEFINITION <--------------------------
issueHTSaturn line == --called by htMakePageNoScroll and htMakeErrorPage
  if $saturn then
     $marg      : local := 0
     $linelength: local := 80
     writeSaturn '"\inputonce{<AXIOM>/doc/browser/browmacs.tex}"
     writeSaturnPrefix()
     writeSaturn(line)
     writeSaturnSuffix()
     if $saturnFileNumber then
       fn := strconc('"sat", STRINGIMAGE $saturnFileNumber, '".tex")
       obey strconc('"doswrite -a saturn.tex ",fn, '".tex")
       $saturnFileNumber := $saturnFileNumber + 1

writeSaturnPrefix() ==
  $saturnContextMenuLines =>
    index :=
      toString ($saturnContextMenuIndex := $saturnContextMenuIndex + 1)
    writeSaturnLines
      ['"\newmenu{BCM", index,
          '"}{",:reverse! $saturnContextMenuLines,
            '"}\usemenu{BCM", index,'"}{\vbox{"]

writeSaturnSuffix() ==
  $saturnContextMenuLines => saturnPRINTEXP '"}}"

issueHTStandard line == --called by htMakePageNoScroll and htMakeErrorPage
  if $standard then
  --unescapeStringsInForm line
    sockSendInt($MenuServer, $SendLine)
    sockSendString($MenuServer, line)

htMakeErrorPage htPage ==
  $newPage := false
  $htLineList := nil
  if $standard then $curPage := htPage
  if $saturn then $saturnPage := htPage
  htMakePage htpPageDescription htPage
  line := apply(function strconc, reverse! $htLineList)
  issueHT line
  endHTPage()

writeSaturnLines lines ==
  for line in lines repeat
   if line ~= '"" and stringChar(line,0) = char "\" then saturnTERPRI()
   saturnPRINTEXP line

writeSaturn(line) ==
  k := 0
  n := maxIndex line
  while  --advance k if true
      k > n => false
      stringChar(line,k) ~= char "\" => true
      code := isBreakSegment?(line, k + 1,n) => false
      true
    repeat (k := k + 1)
  k > n => writeSaturnPrint(line)
  segment := subString(line,0,k)
  writeSaturnPrint(segment)
  code = 1 =>
    writeSaturnPrint('"\\")
    writeSaturn subString(line,k + 2)
  code = 2 =>
    writeSaturnPrint('"  &")
    writeSaturn subString(line,k + 4)
  code = 3 =>
    writeSaturnPrint('"\item")
    writeSaturn subString(line,k + 5)
  code = 4 =>
    writeSaturnPrint('"\newline")
    writeSaturn subString(line,k + 8)
  code = 5 =>
    writeSaturnPrint('"\table{")
    $marg := $marg + 3
    writeSaturnTable subString(line,k + 7)
  code = 6 =>
    i := charPosition(char "}",line,k + 4)
    tabCode := subString(line,k, i - k + 1)
    writeSaturnPrint tabCode
    line := subString(line,i + 1)
    writeSaturn line
  code = 7 =>
    saturnTERPRI()
    writeSaturn subString(line, k + 2)
  code = 8 =>
    i :=
      substring?('"\beginmenu",  line,k) => k + 9
      substring?('"\beginscroll",line,k) => k + 11
      charPosition(char "}",line,k)
    if char "[" = line.(i + 1) then
      i := charPosition(char "]", line, i + 2)
    beginCode := subString(line,k, i - k + 1)
    writeSaturnPrint(beginCode)
    line := subString(line,i + 1)
    writeSaturn line
  code = 9 =>
    i :=
      substring?('"\endmenu",line,k)   => k + 7
      substring?('"\endscroll",line,k) => k + 9
      charPosition(char "}",line,k)
    endCode := subString(line,k, i - k + 1)
    writeSaturnPrint(endCode)
    line := subString(line,i + 1)
    $marg := $marg - 3
    writeSaturn line
  systemError code

isBreakSegment?(line, k, n) ==
  k > n => nil
  char2 := line . k
  char2 = (char "\") => 1
  char2 = (char "&") =>
    substring?('"&\&", line, k) => 2
    nil
  char2 = char "i" =>
    substring?('"item",line,k) => 3
    nil
  char2 = char "n" =>
    substring?('"newline",line,k) => 4
    nil
  char2 = char "t" =>
    (k := k + 2) > n => nil
    line.(k - 1) = char "a" and line.k = char "b" =>
      (k := k + 1) > n => nil
      line.k = char "{" => 6
      substring?('"table",line,k - 3) => 5
      nil
  char2 = char "!" => 7
  char2 = char "b" =>
    substring?('"begin",line,k) => 8
    nil
  char2 = (char "e")  =>
    substring?('"end",line,k) => 9
    nil
  nil

writeSaturnPrint s ==
  for i in 0..($marg - 1) repeat saturnPRINTEXP '" "
  saturnPRINTEXP s
  saturnTERPRI()

saturnPRINTEXP s ==
  $browserOutputStream => PRINC(s,$browserOutputStream)
  PRINC s

saturnTERPRI() ==
  $browserOutputStream => writeNewline $browserOutputStream
  TERPRI()

writeSaturnTable line ==
  open := charPosition(char "{",line,0)
  close:= charPosition(char "}",line,0)
  open < close =>
    close := findBalancingBrace(line,open + 1,maxIndex line,0) or error '"no balancing brace"
    writeSaturnPrint subString(line,0,close + 1)
    writeSaturnTable subString(line,close + 1)
  $marg := $marg - 3
  writeSaturnPrint subString(line,0,close + 1)
  writeSaturn subString(line, close + 1)

findBalancingBrace(s,k,n,level) ==
  k > n => nil
  c := s . k
  c = char "{" => findBalancingBrace(s, k + 1, n, level + 1)
  c = char "}" =>
    level = 0 => k
    findBalancingBrace(s, k + 1, n, level - 1)
  findBalancingBrace(s, k + 1, n, level)

--=======================================================================
--            htMakePage and friends
--=======================================================================
htMakePageStandard itemList ==
  $saturn => nil
  htMakePage itemList

htMakePageSaturn itemList ==
  $standard => nil
  htMakePage itemList

htMakePage itemList ==
  if $newPage then
    if $saturn then htpAddToPageDescription($saturnPage, saturnTran itemList)
    if $standard then htpAddToPageDescription($curPage, itemList)
  htMakePage1 itemList

htMakePage1 itemList ==
-- make a page given the description in itemList
  for u in itemList repeat
    itemType := 'text
    items :=
      string? u => u
      u isnt [.,:.] => STRINGIMAGE u
      string? first u => u
      u is ['text, :s] => s
      itemType := first u
      rest u
    itemType = 'text              => iht items
--      $saturn => bcHt items
--      $standard => iht items
    itemType = 'lispLinks         => htLispLinks items
    itemType = 'lispmemoLinks     => htLispMemoLinks items
    itemType = 'bcLinks           => htBcLinks items               --->
    itemType = 'bcLinksNS         => htBcLinks(items,true)
    itemType = 'bcLispLinks       => htBcLispLinks items           --->
    itemType = 'radioButtons      => htRadioButtons items
    itemType = 'bcRadioButtons    => htBcRadioButtons items
    itemType = 'inputStrings      => htInputStrings items
    itemType = 'domainConditions  => htProcessDomainConditions items
    itemType = 'bcStrings         => htProcessBcStrings items
    itemType = 'toggleButtons     => htProcessToggleButtons items
    itemType = 'bcButtons         => htProcessBcButtons items
    itemType = 'doneButton        => htProcessDoneButton items
    itemType = 'doitButton        => htProcessDoitButton items
    systemError '"unexpected branch"

saturnTran x ==
  x is [[kind, [s1, s2, :callTail]]] and kind in '(bcLinks bcLispLinks) =>
    text := saturnTranText s2
    fs :=  getCallBackFn callTail
    y := isMenuItemStyle? s1 =>  ----> y is text for button in 2nd column
      t1 :=  mkDocLink(fs, mkMenuButton())
      y = '"" =>
        s2 = '"" => t1
        mkTabularItem [t1, text]
      t2 :=  mkDocLink(fs, y)
      mkTabularItem [t1, t2, text]
    t := mkDocLink(fs, s1)
    [:t, :text]
  x is [['text,:r],:.] => r
  error nil

mkBold s ==
  secondPart :=
    s isnt [.,:.] => [s, '"}"]
    [:s, '"}"]
  ['"{\bf ", :secondPart]

mkMenuButton() == [menuButton()]

menuButton() == '"\menuitemstyle{}"
-- Saturn must translate \menuitemstyle ==> {\menuButton}

--------------------> NEW DEFINITION (override in ht-util.boot.pamphlet)
--replaces htMakeButton
getCallBackFn form ==
  func := mkCurryFun(first form, rest form)
  strconc('"(|htDoneButton| '|", func, '"| ",htpName page(), '")")

mkDocLink(code,s) ==
  if code isnt [.,:.] then code := [code]
  if s isnt [.,:.]    then s    := [s]
  ['"\lispLink[d]{\verb!", :code, '"!}{", :s, '"}"]

saturnTranText x ==
  string? x         => [unTab x]
  null x            => nil
  r is [s,fn,:.] and s = '"\unixcommand{" => ['"{\it ",s,'".spad}"]
  x is [['text, :s],:r] => unTab [:s, :saturnTranText r]
  error nil

isMenuItemStyle? s ==
  15 = ('"\menuitemstyle{" < s) => subString(s,15,(maxIndex s) - 15)
  nil

getCallBack callTail ==
  LASSOC(callTail, $callTailList) or
    callTail is [fn] => callTail
    error nil

--=======================================================================
--              Redefinitions from hypertex.boot
--=======================================================================

endHTPage() ==
  $standard => sockSendInt($MenuServer, $EndOfPage)
  nil

--=======================================================================
--              Redefinitions from ht-util.boot
--=======================================================================
htSayHrule() == bcHt
  $saturn => '"\hrule{}\newline{}"
  '"\horizontalline{}\newline{}"

htDoneButton(func, htPage, :optionalArgs) ==
------> Handle argument values passed from page if present
  if optionalArgs ~= nil then
    htpSetInputAreaAlist(htPage,first optionalArgs)
  typeCheckInputAreas htPage =>
    htMakeErrorPage htPage
  not functionSymbol? func =>
    systemError ['"unknown function", func]
  FUNCALL(symbolFunction func, htPage)

htBcLinks(links,:options) ==
  skipStateInfo? := IFCAR options
  [links,options] := beforeAfter('options,links)
  for [message, info, func, :value] in links repeat
    link :=
      $saturn => '"\lispLink[d]"
      '"\lispdownlink"
    htMakeButton(link,message,
                   mkCurryFun(func, value),skipStateInfo?)
    bcIssueHt info

htBcLispLinks links ==
  [links,options] := beforeAfter('options,links)
  for [message, info, func, :value] in links repeat
    link :=
      $saturn => '"\lispLink[n]"
      '"\lisplink"
    htMakeButton(link ,message, mkCurryFun(func, value))
    bcIssueHt info

htMakeButton(htCommand, message, func,:options) ==
  $saturn => htMakeButtonSaturn(htCommand, message, func, options)
  skipStateInfo? := IFCAR options
  iht [htCommand, '"{"]
  bcIssueHt message
  skipStateInfo? =>
    iht ['"}{(|htDoneButton| '|", func, '"| ",htpName $curPage, '")}"]
  iht ['"}{(|htDoneButton| '|", func, '"| (PROGN "]
  for [id, ., ., ., type, :.] in htpInputAreaAlist $curPage repeat
    iht ['"(|htpSetLabelInputString| ", htpName $curPage, '"'|", id, '"| "]
    if type = 'string then
      iht ['"_"\stringvalue{", id, '"}_""]
    else
      iht ['"_"\boxvalue{", id, '"}_""]
    iht '") "
  iht [htpName $curPage, '"))}"]

htMakeButtonSaturn(htCommand, message, func,options) ==
  skipStateInfo? := IFCAR options
  iht htCommand
  skipStateInfo? =>
    iht ['"{\verb!(|htDoneButton| '|", func, '"| ",htpName page(), '")!}{"]
    bcIssueHt message
    iht '"}"
  iht ['"{\verb!(|htDoneButton| '|", func, '"| "]
  if $kPageSaturnArguments then
    iht '"(PROGN "
    for id in $kPageSaturnArguments for var in $PatternVariableList  repeat
      iht ['"(|htpSetLabelInputString| ", htpName page(), '"'|", var, '"| "]
      iht ["'|!\", id, '"\verb!|"]
      iht '")"
    iht htpName $saturnPage
    iht '")"
  else
    iht htpName $saturnPage
  iht '")!}{"
  bcIssueHt message
  iht '"}"

htpAddToPageDescription(htPage, pageDescrip) ==
  newDescript :=
    string? pageDescrip => [pageDescrip, :htPage.7]
    append!(reverse! copyList pageDescrip, htPage.7)
  htPage.7 := newDescript


htProcessBcStrings strings ==
  for [numChars, default, stringName, spadType, :filter] in strings repeat
    mess2 := '""
    if null LASSOC(stringName, htpInputAreaAlist page()) then
      setUpDefault(stringName, ['string, default, spadType, filter])
    if htpLabelErrorMsg(page(), stringName) then
      iht ['"\centerline{{\em ", htpLabelErrorMsg(page(), stringName), '"}}"]
      mess2 := strconc(mess2, bcSadFaces())
      htpSetLabelErrorMsg(page(), stringName, nil)
    iht ['"\inputstring{", stringName, '"}{",
         numChars, '"}{", htpLabelDefault(page(),stringName), '"} ", mess2]

setUpDefault(name, props) ==
  htpAddInputAreaProp(page(), name, props)

htInitPage(title, propList) ==
-- start defining a hyperTeX page
  htInitPageNoScroll(propList, title)
  htSayStandard '"\beginscroll "
  page()

--------------------> NEW DEFINITION <--------------------------
htInitPageNoScroll(propList, :options) ==
--start defining a hyperTeX page
  $atLeastOneUnexposed := nil     --reset every time a new page is initialized
  $saturnContextMenuLines := nil
  title := IFCAR options
  $curPage :=
    $standard => htpMakeEmptyPage(propList)
    nil
  if $saturn then $saturnPage := htpMakeEmptyPage(propList)
  $newPage := true
  $htLineList := nil
  if title then
    if $standard then htSayStandard ['"\begin{page}{", htpName $curPage, '"}{"]
    htSaySaturn '"\browseTitle{"
    htSay title
    htSaySaturn '"}"
    htSayStandard '"} "
  page()
--------------------> NEW DEFINITION <--------------------------
htInitPageNoHeading(propList) ==
--start defining a hyperTeX page
  $curPage :=
    $standard => htpMakeEmptyPage(propList)
  if $saturn then $saturnPage := htpMakeEmptyPage(propList)
  $newPage := true
  $htLineList := nil
  page()

--------------------> NEW DEFINITION <--------------------------
htpMakeEmptyPage(propList,:options) ==
  name := IFCAR options or GENTEMP()
  if not $saturn then
    $activePageList := [name, :$activePageList]
  val := vector [name, nil, nil, nil, nil, nil, propList, nil]
  symbolValue(name) :=  val

--=======================================================================
--              Redefinitions from br-con.boot
--=======================================================================
kPage(line,:options) == --any cat, dom, package, default package
--constructors    Cname\#\E\sig \args   \abb \comments (C is C, D, P, X)
  parts := dbXParts(line,7,1)
  [kind,name,nargs,xflag,sig,args,abbrev,comments] := parts
  form := IFCAR options
  isFile := null kind
  kind := kind or '"package"
  parts.first := kind
  conform         := mkConform(kind,name,args)
  $kPageSaturnArguments: local := rest conform
  conname         := opOf conform
  capitalKind     := capitalize kind
  signature       := ncParseFromString sig
  sourceFileName  := getConstructorSourceFileFromDB makeSymbol name
  constrings      :=
    KDR form => dbConformGenUnder form
    [strconc(name,args)]
  emString        := ['"{\sf ",:constrings,'"}"]
  heading := [capitalKind,'" ",:emString]
  if not isExposedConstructor conname then heading := ['"Unexposed ",:heading]
  if name=abbrev then abbrev := asyAbbreviation(conname,nargs)
  page := htInitPageNoScroll nil
  htAddHeading heading
  htSayStandard("\beginscroll ")
  htpSetProperty(page,'argSublis,mkConArgSublis rest conform)
  htpSetProperty(page,'isFile,true)
  htpSetProperty(page,'parts,parts)
  htpSetProperty(page,'heading,heading)
  htpSetProperty(page,'kind,kind)
  htpSetProperty(page,'conform,conform)
  htpSetProperty(page,'signature,signature)
  ---what follows is stuff from kiPage with domain = nil
  $conformsAreDomains := nil
  dbShowConsDoc1(page,conform,nil)
  if kind ~= 'category and nargs > 0 then addParameterTemplates(page,conform)
  if $atLeastOneUnexposed then htSay '"\newline{}{\em *} = unexposed"
  htSayStandard("\endscroll ")
  kPageContextMenu page
  htShowPageNoScroll()

kPageContextMenu page ==
  $saturn => kPageContextMenuSaturn page
  [kind,name,nargs,xpart,sig,args,abbrev,comments] := htpProperty(page,'parts)
  conform := htpProperty(page,'conform)
  conname := opOf conform
  htBeginTable()
  htSay '"{"
  htMakePage [['bcLinks,['Ancestors,'"",'kcaPage,nil]]]
  htSay '"}{"
  htMakePage [['bcLinks,['Attributes,'"",'koPage,'"attribute"]]]
  if kind = '"category" then
    htSay '"}{"
    htMakePage [['bcLinks,['Children,'"",'kccPage,nil]]]
  htSay '"}{"
  htMakePage [['bcLinks,['Dependents,'"",'kcdePage,nil]]]
  if kind = '"category" then
    htSay '"}{"
    htMakePage [['bcLinks,['Descendents,'"",'kcdPage,nil]]]
  if kind = '"category" then
    htSay '"}{"
    htMakePage [['bcLinks,['Domains,'"",'kcdoPage,nil]]]
  htSay '"}{"
  if kind ~= '"category" and (pathname := dbHasExamplePage conname)
    then htMakePage [['bcLinks,['Examples,'"",'kxPage,pathname]]]
    else htSay '"{\em Examples}"
  htSay '"}{"
  htMakePage [['bcLinks,['Exports,'"",'kePage,nil]]]
  htSay '"}{"
  htMakePage [['bcLinks,['Operations,'"",'koPage,'"operation"]]]
  htSay '"}{"
  htMakePage [['bcLinks,['Parents,'"",'kcpPage,'"operation"]]]
  if kind ~= '"category" then
    htSay '"}{"
    htMakePage [['bcLinks,["Search Path",'"",'ksPage,nil]]]
  if kind ~= '"category" then
    htSay '"}{"
    htMakePage [['bcLinks,['Users,'"",'kcuPage,nil]]]
    htSay '"}{"
    htMakePage [['bcLinks,['Uses,'"",'kcnPage,nil]]]
  htSay '"}"
  if $standard then htEndTable()

kPageContextMenuSaturn page ==
  $newPage    : local := nil
  [kind,name,nargs,xpart,sig,args,abbrev,comments] := htpProperty(page,'parts)
  $htLineList : local := nil
  conform := htpProperty(page,'conform)
  conname := opOf conform
  htMakePage [['bcLinks,['"\&Ancestors",'"",'kcaPage,nil]]]
  htMakePage [['bcLinks,['"Attri\&butes",'"",'koPage,'"attribute"]]]
  if kind = '"category" then
    htMakePage [['bcLinks,['"\&Children",'"",'kccPage,nil]]]
  htMakePage [['bcLinks,['"\&Dependents",'"",'kcdePage,nil]]]
  if kind = '"category" then
    htMakePage [['bcLinks,['"Desce\&ndents",'"",'kcdPage,nil]]]
  if kind = '"category" then
    htMakePage [['bcLinks,['"Do\&mains",'"",'kcdoPage,nil]]]
  if kind ~= '"category" and (name := saturnHasExamplePage conname)
    then saturnExampleLink name
    else htSayCold '"E\&xamples"
  htMakePage [['bcLinks,['"\&Exports",'"",'kePage,nil]]]
  htMakePage [['bcLinks,['"\&Operations",'"",'koPage,'"operation"]]]
  htMakePage [['bcLinks,['"\&Parents",'"",'kcpPage,'"operation"]]]
  htMakePage [['bcLinks,['"Search O\&rder",'"",'ksPage,nil]]]
  if kind ~= '"category" or dbpHasDefaultCategory? xpart
    then
       htMakePage [['bcLinks,['"\&Users",'"",'kcuPage,nil]]]
       htMakePage [['bcLinks,['"U\&ses",'"",'kcnPage,nil]]]
    else
       htSayCold '"\&Users"
       htSayCold '"U\&ses"
  $saturnContextMenuLines := $htLineList

saturnExampleLink lname ==
  htSay '"\docLink{\csname "
  htSay strconc(second lname, '"\endcsname}{E&xamples}")

$exampleConstructors := nil

saturnHasExamplePage conname ==
  if not $exampleConstructors then
     $exampleConstructors := getSaturnExampleList()
  objectAssoc(conname, $exampleConstructors)
  
getSaturnExampleList() == 
  file := strconc(systemRootDirectory(), "/doc/axug/examples.lsp")
  not PROBE_-FILE file => nil
  fp := MAKE_-INSTREAM file
  lst := VMREAD fp
  SHUT fp
  lst
  
dbPresentCons(htPage,kind,:exclusions) ==
  $saturn => dbPresentConsSaturn(htPage,kind,exclusions)
  htpSetProperty(htPage,'exclusion,first exclusions)
  cAlist := htpProperty(htPage,'cAlist)
  empty? := null cAlist
  one?   := null rest cAlist
  one? := empty? or one?
  exposedUnexposedFlag := $includeUnexposed? --used to be star?       4/92
  star?  := true     --always include information on exposed/unexposed   4/92
  if $standard then htBeginTable()
  htSay '"{"
  if one? or 'abbrs in exclusions
    then htSay '"{\em Abbreviations}"
    else htMakePage [['bcLispLinks,['"Abbreviations",'"",'dbShowCons,'abbrs]]]
  htSay '"}{"
  if one? or 'conditions in exclusions or "and"/[rest x = true for x in cAlist]
    then htSay '"{\em Conditions}"
    else htMakePage [['bcLispLinks,['"Conditions",'"",'dbShowCons,'conditions]]]
  htSay '"}{"
  if empty? or 'documentation in exclusions
    then htSay '"{\em Descriptions}"
    else htMakePage [['bcLispLinks,['"Descriptions",'"",'dbShowCons,'documentation]]]
  htSay '"}{"
  if one? or null rest cAlist
    then htSay '"{\em Filter}"
    else htMakePage
      [['bcLinks,['"Filter",'"",'htFilterPage,['dbShowCons,'filter]]]]
  htSay '"}{"
  if one? or 'kinds in exclusions or kind ~= 'constructor
    then htSay '"{\em Kinds}"
    else htMakePage [['bcLispLinks,['"Kinds",'"",'dbShowCons,'kinds]]]
  htSay '"}{"
  if one? or 'names in exclusions
    then htSay '"{\em Names}"
    else htMakePage [['bcLispLinks,['"Names",'"",'dbShowCons,'names]]]
  htSay '"}{"
  if one? or 'parameters in exclusions or not ("or"/[CDAR x for x in cAlist])
    then htSay '"{\em Parameters}"
    else htMakePage [['bcLispLinks,['"Parameters",'"",'dbShowCons,'parameters]]]
  htSay '"}{"
  if $exposedOnlyIfTrue
    then
      if one?
      then htSay '"{\em Unexposed Also}"
      else htMakePage [['bcLinks,['"Unexposed Also",'"",'dbShowCons,'exposureOff]]]
    else
      if one?
      then htSay '"{\em Exposed Only}"
      else htMakePage [['bcLinks,['"Exposed Only",'"",'dbShowCons,'exposureOn]]]
  htSay '"}"
  if $standard then htEndTable()

dbPresentConsSaturn(htPage,kind,exclusions) ==
  $htLineList : local := nil
  $newPage    : local := nil
  htpSetProperty(htPage,'exclusion,first exclusions)
  cAlist := htpProperty(htPage,'cAlist)
  empty? := null cAlist
  one?   := null KDR cAlist
  one? := empty? or one?
  exposedUnexposedFlag := $includeUnexposed? --used to be star?       4/92
  star?  := true     --always include information on exposed/unexposed   4/92
  if $standard then htBeginTable()
  if one? or 'abbrs in exclusions
    then htSayCold '"\&Abbreviations"
    else htMakePage [['bcLispLinks,['"\&Abbreviations",'"",'dbShowCons,'abbrs]]]
  if one? or 'conditions in exclusions or "and"/[rest x = true for x in cAlist]
    then htSayCold '"\&Conditions"
    else htMakePage [['bcLispLinks,['"\&Conditions",'"",'dbShowCons,'conditions]]]
  if empty? or 'documentation in exclusions
    then htSayCold '"\&Descriptions"
    else htMakePage [['bcLispLinks,['"\&Descriptions",'"",'dbShowCons,'documentation]]]
  if one? or null rest cAlist
    then htSayCold '"\&Filter"
    else htMakeSaturnFilterPage ['dbShowCons, 'filter]
  if one? or 'kinds in exclusions or kind ~= 'constructor
    then htSayCold '"\&Kinds"
    else htMakePage [['bcLispLinks,['"\&Kinds",'"",'dbShowCons,'kinds]]]
  if one? or 'names in exclusions
    then htSayCold '"\&Names"
    else htMakePage [['bcLispLinks,['"\&Names",'"",'dbShowCons,'names]]]
  if one? or 'parameters in exclusions or not ("or"/[CDAR x for x in cAlist])
    then htSayCold '"\&Parameters"
    else htMakePage [['bcLispLinks,['"\&Parameters",'"",'dbShowCons,'parameters]]]
  htSaySaturn '"\hrule"
  if $exposedOnlyIfTrue
    then
      if one? then htSayCold '"\&Unexposed Also"
      else htMakePage [['bcLinks,['"\&Unexposed Also",'"",'dbShowCons,'exposureOff]]]
    else
      if one? then htSayCold '"\Exposed Only\&y"
      else htMakePage [['bcLinks,['"Exposed Onl\&y",'"",'dbShowCons,'exposureOn]]]
  if $standard then htEndTable()
  $saturnContextMenuLines := $htLineList

htFilterPage(htPage,args) ==
  htInitPage("Filter String",htCopyProplist htPage)
  htSay "\centerline{Enter filter string (use {\em *} for wild card):}"
  htSay '"\centerline{"
  htMakePage [['bcStrings, [50,'"",'filter,'EM]]]
  htSay '"}\vspace{1}\centerline{"
  htMakePage [['bcLispLinks,['"\fbox{Filter}",'"",:args]]]
  htSay '"}"
  htShowPage()

htMakeSaturnFilterPage [fn2Call,:args] ==
  htSay '"\inputboxLink[\lispLink[d]{\verb+(|"
  htSay fn2Call
  htSay '"| "
  htSay htpName $saturnPage
  for x in args repeat
    htSay '" '|"
    htSay x
    htSay '"|"
  htSay '" _"+_\FILTERSTRING\verb+_")+}{}]{\FILTERSTRING}{*}"
  htSay '"{\centerline{Enter filter string (use {\em *} for wild card):}}"
  htSay '"{Filter Page}{\&Filter}"

dbShowConsKinds cAlist ==
  cats := doms := paks := defs := nil
  for x in cAlist repeat
    op := CAAR x
    kind := dbConstructorKind constructorDB op
    kind  = 'category => cats := [x,:cats]
    kind = 'domain    => doms := [x,:doms]
    kind = 'package   => paks := [x,:paks]
    defs := [x,:defs]
  lists := [reverse! cats,reverse! doms,reverse! paks,reverse! defs]
  htBeginMenu 'description
  htSayStandard '"\indent{1}"
  kinds := +/[1 for x in lists | #x > 0]
  firstTime := true
  for kind in '("category" "domain" "package" "default package") for x in lists | #x > 0 repeat
    if firstTime then firstTime := false
                 else htSaySaturn '"\\"
    htSaySaturn '"\item["
    htSayStandard '"\item"
    if kinds = 1
       then htSay menuButton()
       else htMakePage
         [['bcLinks,[menuButton(),'"",'dbShowConsKindsFilter,[kind,x]]]]
    htSaySaturn '"]"
    htSayStandard '"\tab{1}"
    htSay('"{\em ",c := #x,'" ")
    htSay(c > 1 => pluralize kind; kind)
    htSay '":}"
    htSaySaturn '"\\"
    bcConTable removeDuplicates [CAAR y for y in x]
  htEndMenu 'description
  htSayStandard '"\indent{0}"

addParameterTemplates(page, conform) ==
---------------> from kPage <-----------------------
  parlist := [STRINGIMAGE par for par in rest conform]
  manuelsCode? := "MAX"/[#s for s in parlist] > 10
  w := (manuelsCode? => 55; 23)
  htSaySaturn '"\colorbuttonbox{lightgray}{"
  htSay '"Optional argument value"
  htSay
    rest parlist => '"s:"
    '":"
  htSaySaturn '"}"
  if rest conform then htSaySaturn '"\newline{}"
  htSaySaturn '"\begin{tabular}{p{.25in}l}"
  firstTime := true
  odd := false
  argSublis := htpProperty(page,'argSublis)
  for parname in $PatternVariableList for par in rest conform repeat
    htSayStandard (odd or manuelsCode? => "\newline";"\tab{29}")
    if firstTime then firstTime := false
                 else htSaySaturn '"\\"
    odd := not odd
    argstring :=
      $conArgstrings is [a,:r] => ($conArgstrings := r; a)
      '""
    htMakePageStandard [['text,'"{\em ",par,'"} = "],
        ['bcStrings,[w - #STRINGIMAGE par,argstring,parname,'EM]]]
    if $saturn then
      setUpDefault(parname, ['string, '"", 'EM, nil])
    htSaySaturn '"{\em "
    htSaySaturn par
    htSaySaturn '" = }"
    htSaySaturnAmpersand()
    htSaySaturn '"\colorbuttonbox{lightgray}{\inputbox[2.5in]{\"
    htSaySaturn applySubst(argSublis,par)
    htSaySaturn '"}{"
    htSaySaturn argstring
    htSaySaturn '"}}"
  htEndTabular()

kPageArgs([op,:args],[.,.,:source]) ==
  htSaySaturn '"\begin{tabular}{p{.25in}lp{0in}}"
  firstTime := true
  coSig := rest getDualSignature op
  for x in args for t in source for pred in coSig repeat
    if firstTime then firstTime := false
                 else
                   htSaySaturn '"\\"
                   htSayStandard '", and"
    htSayStandard '"\newline "
    htSaySaturnAmpersand()
    typeForm := (t is [":",.,t1] => t1; t)
    if pred = true
      then htMakePage [['bcLinks,[x,'"",'kArgPage,x]]]
      else htSay('"{\em ",x,'"}")
    htSayStandard( '"\tab{",toString(#PNAME x),'"}, ")
    htSaySaturnAmpersand()
    htSay
      pred => '"a domain of category "
      '"an element of the domain "
    bcConform(typeForm,true)
  htEndTabular()

--=======================================================================
--              Redefinitions from br-op1.boot
--=======================================================================

dbConform form ==
--one button for the main constructor page of a type
  $saturn => ["\lispLink[d]{\verb!(|conPage| '",:form2Fence dbOuttran form,'")!}{",
           :form2StringList opOf form,"}"]
  ["\conf{",:form2StringList opOf form,'"}{",:form2Fence dbOuttran form,'"}"]

htTab s == 
  $standard => htSayStandard ('"\tab{",s,'"}")
  nil

dbGatherThenShow(htPage,opAlist,which,data,constructorIfTrue,word,fn) ==
  single? := null rest data
  htBeginMenu 'description
  bincount := 0
  for [thing,exposeFlag,:items] in data repeat
    htSaySaturn '"\item["
    htSayStandard ('"\item")
    if single? then htSay(menuButton())
    else
      htMakePageStandard
        [['bcLinks,[menuButton(),'"",'dbShowOps,which,bincount]]]
      button := mkButtonBox (1 + bincount)
      htMakePageSaturn [['bcLinks,[button,'"",'dbShowOps,which,bincount]]]
    htSaySaturn '"]"
    htSay '"{\em "
    htSay
      thing = 'nowhere => '"implemented nowhere"
      thing = 'constant => '"constant"
      thing = '_$ => '"by the domain"
      integer? thing => '"unexported"
      constructorIfTrue =>
        htSay word
        thing isnt [.,:.] => '" an unknown constructor"
        '""
      thing isnt [.,:.] => '"unconditional"
      '""
    htSay '"}"
    if cons? thing then
      if constructorIfTrue then htSay('" {\em ",dbShowKind thing,'"}")
      htSay '" "
      FUNCALL(fn,thing)
    htSay('":\newline ")
    dbShowOpSigList(which,items,(1 + bincount) * 8192)
    bincount := bincount + 1
  htEndMenu 'description

dbPresentOps(htPage,which,:exclusions) ==
  $saturn => dbPresentOpsSaturn(htPage,which,exclusions)
  fromConPage? := (conname := opOf htpProperty(htPage,'conform))
  usage? := nil
  star? := not fromConPage? or which = '"package operation"
  implementation? := 
    $UserLevel = 'development and $conformsAreDomains --and not $includeUnexposed?
  rightmost? := star? or (implementation? and not $includeUnexposed?)
  if integer? first exclusions then exclusions := ['documentation]
  htpSetProperty(htPage,'exclusion,first exclusions)
  opAlist :=
    which = '"operation" => htpProperty(htPage,'opAlist)
    htpProperty(htPage,'attrAlist)
  empty? := null opAlist
  one?   := opAlist is [entry] and 2 = #entry
  one? := empty? or one?
  htBeginTable()
  htSay '"{"
  if one? or 'conditions in exclusions
                 or (htpProperty(htPage,'condition?) = 'no)
      then htSay '"{\em Conditions}"
      else htMakePage [['bcLispLinks,['"Conditions",'"",'dbShowOps,which,'conditions]]]
  htSay '"}{"
  if empty? or 'documentation in exclusions
    then htSay '"{\em Descriptions}"
    else htMakePage [['bcLispLinks,['"Descriptions",'"",'dbShowOps,which,'documentation]]]
  htSay '"}{"
  if null IFCDR opAlist
    then htSay '"{\em Filter}"
    else htMakePage [['bcLinks,['"Filter ",'"",'htFilterPage,['dbShowOps,which,'filter]]]]
  htSay '"}{"
  if one? or 'names in exclusions or null KDR opAlist
    then htSay '"{\em Names}"
    else htMakePage [['bcLispLinks,['"Names",'"",'dbShowOps,which,'names]]]
  if not star? then
    htSay '"}{"
    if not implementation? or 'implementation in exclusions or which = '"attribute" or
      ((conname := opOf htpProperty(htPage,'conform))
        and getConstructorKindFromDB conname = "category")
    then htSay '"{\em Implementations}"
    else htMakePage
      [['bcLispLinks,['"Implementations",'"",'dbShowOps,which,'implementation]]]
  htSay '"}{"
  if one? or 'origins in exclusions
    then htSay '"{\em Origins}"
    else htMakePage [['bcLispLinks,['"Origins",'"",'dbShowOps,which,'origins]]]
  htSay '"}{"
  if one? or 'parameters in exclusions --also test for some parameter
      or not dbDoesOneOpHaveParameters? opAlist
    then htSay '"{\em Parameters}"
    else htMakePage [['bcLispLinks,['"Parameters",'"",'dbShowOps,which,'parameters]]]
  htSay '"}{"
  if which ~= '"attribute" then
    if one? or 'signatures in exclusions
      then htSay '"{\em Signatures}"
      else htMakePage [['bcLispLinks,['"Signatures",'"",'dbShowOps,which,'signatures]]]
  htSay '"}"
  if star? then
    htSay '"{"
    if $exposedOnlyIfTrue
    then if one?
         then htSay '"{\em Unexposed Also}"
         else htMakePage [['bcLinks,['"Unexposed Also",'"",'dbShowOps,which,'exposureOff]]]
    else if one?
         then htSay '"{\em Exposed Only}"
         else htMakePage [['bcLinks,['"Exposed Only",'"",'dbShowOps, which,'exposureOn]]]
    htSay '"}"
  htEndTable()

dbPresentOpsSaturn(htPage,which,exclusions) ==
  $htLineList : local := nil
  $newPage    : local := nil
  fromConPage? := (conname := opOf htpProperty(htPage,'conform))
  usage? := nil
  star? := not fromConPage? or which = '"package operation"
  implementation? := 
    $UserLevel = 'development and $conformsAreDomains --and not $includeUnexposed?
  rightmost? := star? or (implementation? and not $includeUnexposed?)
  if integer? first exclusions then exclusions := ['documentation]
  htpSetProperty(htPage,'exclusion,first exclusions)
  opAlist :=
    which = '"operation" => htpProperty(htPage,'opAlist)
    htpProperty(htPage,'attrAlist)
  empty? := null opAlist
  one?   := opAlist is [entry] and 2 = #entry
  one? := empty? or one?
  if one? or 'conditions in exclusions
                 or (htpProperty(htPage,'condition?) = 'no)
      then htSayCold '"\&Conditions"
      else htMakePage [['bcLispLinks,['"\&Conditions",'"",'dbShowOps,which,'conditions]]]
  if empty? or 'documentation in exclusions
    then htSayCold '"\&Descriptions"
    else htMakePage [['bcLispLinks,['"\&Descriptions",'"",'dbShowOps,which,'documentation]]]
  if null IFCDR opAlist
    then htSayCold '"\&Filter"
    else htMakeSaturnFilterPage ['dbShowOps, which, 'filter]
  if not implementation? or 'implementation in exclusions or which = '"attribute" or
      ((conname := opOf htpProperty(htPage,'conform))
        and getConstructorKindFromDB conname = "category")
    then htSayCold '"\&Implementations"
    else htMakePage
      [['bcLispLinks,['"\&Implementations",'"",'dbShowOps,which,'implementation]]]
  if one? or 'names in exclusions or null KDR opAlist
    then htSayCold '"\&Names"
    else htMakePage [['bcLispLinks,['"\&Names",'"",'dbShowOps,which,'names]]]
  if one? or 'origins in exclusions
    then htSayCold '"\&Origins"
    else htMakePage [['bcLispLinks,['"\&Origins",'"",'dbShowOps,which,'origins]]]
  if one? or 'parameters in exclusions --also test for some parameter
      or not dbDoesOneOpHaveParameters? opAlist
    then htSayCold '"\&Parameters"
    else htMakePage [['bcLispLinks,['"\&Parameters",'"",'dbShowOps,which,'parameters]]]
  if which ~= '"attribute" then
    if one? or 'signatures in exclusions
      then htSayCold '"\&Signatures"
      else htMakePage [['bcLispLinks,['"\&Signatures",'"",'dbShowOps,which,'signatures]]]
  if star? then
    htSay '"\hrule"
    if $exposedOnlyIfTrue
      then if one? then htSayCold '"\&Unexposed Also"
      else htMakePage [['bcLinks,['"\&Unexposed Also",'"",'dbShowOps,which,'exposureOff]]]
    else
      if one? then htSayCold '"Exposed Onl\&y"
      else htMakePage [['bcLinks,['"Exposed Onl\&y",'"",'dbShowOps,which,'exposureOn]]]
  $saturnContextMenuLines := $htLineList

--=======================================================================
--              Redefinitions from br-search.boot
--=======================================================================

htShowPageStar() ==
  $saturn => htShowPageStarSaturn()
  htSayStandard '"\endscroll "
  if $exposedOnlyIfTrue then
    htMakePage [['bcLinks,['"Unexposed Also",'"",'repeatSearch,nil]]]
  else
    htMakePage [['bcLinks,['"Exposed Only",'"",'repeatSearch,'T]]]
  htShowPageNoScroll()

htShowPageStarSaturn() ==
  $newPage    : local := nil
  $htLineList : local := nil
  if $exposedOnlyIfTrue then
    htMakePage [['bcLinks,['"Unexposed Also",'"",'repeatSearch,nil]]]
  else
    htMakePage [['bcLinks,['"Exposed Only",'"",'repeatSearch,'T]]]
  $saturnContextMenuLines := $htLineList
  htShowPageNoScroll()

--=======================================================================
--              Redefinitions from br-op2.boot
--=======================================================================

++ returns true if op designate a niladic constructor.  Note that
++ constructors are symbols whereas ordinary operations are strings.
operationIsNiladicConstructor op ==
  ident? op => niladicConstructor? op
  false

++ Like operationIsNiladicConstructor() except that we just want
++ to know whether `op' is a constructor, arity is unimportant.
operationIsConstructor op ==
  ident? op => getDualSignature op
  nil

--------------> NEW DEFINITION (see br-op2.boot.pamphlet)
displayDomainOp(htPage,which,origin,op,sig,predicate,
                doc,index,chooseFn,unexposed?,$generalSearch?) ==
  $chooseDownCaseOfType : local := true   --see dbGetContrivedForm
  $whereList  : local := nil
  $NumberList : local := '(i j k l m n i1 j1 k1 l1 m1 n1 i2 j2 k2 l2 m2 n2 i3 j3 k3 l3 m3 n3 i4 j4 k4 l4 m4 n4 )
  $ElementList: local := '(x y z u v w x1 y1 z1 u1 v1 w1 x2 y2 z2 u2 v2 w2 x3 y3 z3 u3 v3 w3 x4 y4 z4 u4 v4 w4 )
  $FunctionList:local := '(f g h d e F G H)
  $DomainList:  local := '(D R S E T A B C M N P Q U V W)
  exactlyOneOpSig     := null index
  conform   := htpProperty(htPage,'domname) or htpProperty(htPage,'conform)
                 or origin
  if $generalSearch? then $DomainList := rest $DomainList
  opform :=
    which = '"attribute" =>
      null sig => [op]
      [op,sig]
    which = '"constructor" => origin
    dbGetDisplayFormForOp(op,sig,doc)
  htSayStandard('"\newline")
  -----------------------------------------------------------
  htSaySaturn '"\item["
  if exactlyOneOpSig
    then htSay menuButton()
    else htMakePage
      [['bcLinks,[menuButton(),'"",chooseFn,which,index]]]
  htSaySaturn '"]"
  htSayStandard '"\tab{2}"
  op   := IFCAR opform
  args := IFCDR opform
  ops := escapeSpecialChars STRINGIMAGE op
  n := #sig
  do
    n = 2 and symbolTarget('Nud,PROPLIST op) => 
      htSay(ops,'" {\em ",quickForm2HtString KAR args,'"}")
    n = 3 and symbolTarget('Led,PROPLIST op) => 
      htSay('"{\em ",quickForm2HtString KAR args,'"} ",ops,'" {\em ",quickForm2HtString KAR KDR args,'"}")
    if unexposed? and $includeUnexposed? then
      htSayUnexposed()
    htSay(ops)
    predicate='ASCONST or operationIsNiladicConstructor op or op in '(0 1) => 'skip
    which = '"attribute" and null args => 'skip
    htSay('"(")
    if IFCAR args then htSay('"{\em ",quickForm2HtString IFCAR args,'"}")
    for x in IFCDR args repeat
      htSay('",{\em ",quickForm2HtString x,'"}")
    htSay('")")
  -----------prepare to print description---------------------
  constring := form2HtString conform
  conname   := first conform
  $conkind   : local := htpProperty(htPage,'kind) -- a string e.g. "category"
                          or STRINGIMAGE getConstructorKindFromDB conname
  $conlength : local := #constring
  $conform   : local := conform
  $conargs   : local := rest conform
  if which = '"operation" then
    $signature : local :=
      builtinFunctorName? conname => nil
      getConstructorModemap(conname).mmSignature
    --RDJ: this next line is necessary until compiler bug is fixed
    --that forgets to substitute #variables for t#variables;
    --check the signature for SegmentExpansionCategory, e.g.
    tvarlist := take(# $conargs,$TriangleVariableList)
    $signature := applySubst(pairList(tvarlist,$FormalMapVariableList),$signature)
  $sig :=
    which = '"attribute" or which = '"constructor" => sig
    $conkind ~= '"package" => sig
    symbolsUsed := [x for x in rest conform | ident? x]
    $DomainList := setDifference($DomainList,symbolsUsed)
    getSubstSigIfPossible sig
  -----------------------------------------------------------
  htSaySaturn '"\begin{tabular}{lp{0in}}"
  -----------------------------------------------------------
  if which in '("operation" "constructor") then
    $displayReturnValue: local := nil
    if args then
      htSayStandard('"\newline\tab{2}{\em Arguments:}")
      htSaySaturn '"{\em Arguments:}"
      htSaySaturnAmpersand()
      firstTime := true
      coSig := KDR operationIsConstructor op  --check if op is constructor
      for a in args for t in rest $sig repeat
            if not firstTime then
              htSaySaturn '"\\ "
              htSaySaturnAmpersand()
            firstTime := false
            htSayIndentRel(15, true)
            position := KAR relatives
            relatives := KDR relatives
            if KAR coSig and t ~= '(Type)
              then htMakePage [['bcLinks,[a,'"",'kArgPage,a]]]
              else htSay('"{\em ",form2HtString(a),'"}")
            htSay ", "
            coSig := KDR coSig
            htSayValue t
            htSayIndentRel(-15,true)
            htSayStandard('"\newline ")
      htSaySaturn '"\\"
    if first $sig then
      $displayReturnValue := true
      htSayStandard('"\newline\tab{2}")
      htSay '"{\em Returns:}"
      htSaySaturnAmpersand()
      htSayIndentRel(15, true)
      htSayValue first $sig
      htSayIndentRel(-15, true)
      htSaySaturn '"\\"
  -----------------------------------------------------------
  if origin and ($generalSearch? or origin ~= conform) and op~=opOf origin then
    htSaySaturn '"{\em Origin:}"
    htSaySaturnAmpersand()
    htSayStandard('"\newline\tab{2}{\em Origin:}")
    htSayIndentRel(15)
    if not isExposedConstructor opOf origin and $includeUnexposed?
       then htSayUnexposed()
    bcConform(origin,true)
    htSayIndentRel(-15)
    htSaySaturn '"\\"
  -----------------------------------------------------------
  if not (predicate in '(T ASCONST)) then
    pred := sublisFormal(KDR conform,predicate)
    count := #pred
    htSaySaturn '"{\em Conditions:}"
    htSayStandard('"\newline\tab{2}{\em Conditions:}")
    firstTime := true
    for p in displayBreakIntoAnds substitute($conform,"$",pred) repeat
      if not firstTime then htSaySaturn '"\\"
      htSayIndentRel(15,count > 1)
      firstTime := false
      htSaySaturnAmpersand()
      bcPred(p,$conform,true)
      htSayIndentRel(-15,count > 1)
      htSayStandard('"\newline ")
    htSaySaturn '"\\"
  -----------------------------------------------------------
  if $whereList then
    count := #$whereList
    htSaySaturn '"{\em Where:}"
    htSayStandard('"\newline\tab{2}{\em Where:}")
    firstTime := true
    if symbolAssoc("$",$whereList) then
      htSayIndentRel(15,true)
      htSaySaturnAmpersand()
      htSayStandard '"{\em \$} is "
      htSaySaturn '"{\em \%} is "
      htSay
        $conkind = '"category" => '"of category "
        '"the domain "
      bcConform(conform,true,true)
      firstTime := false
      htSayIndentRel(-15,true)
    for [d,key,:t] in $whereList | d ~= "$" repeat
      htSayIndentRel(15,count > 1)
      if not firstTime then htSaySaturn '"\\ "
      htSaySaturnAmpersand()
      firstTime := false
      htSay("{\em ",d,"} is ")
      htSayConstructor(key,sublisFormal(KDR conform,t))
      htSayIndentRel(-15,count > 1)
    htSaySaturn '"\\"
  -----------------------------------------------------------
  if doc ~= nil and doc isnt '"" and (doc isnt [d] or d ~= '"") then
    htSaySaturn '"{\em Description:}"
    htSaySaturnAmpersand()
    htSayStandard('"\newline\tab{2}{\em Description:}")
    htSayIndentRel(15)
    if doc = $charFauxNewline then htSay $charNewline
    else
       ndoc:= 
          -- we are confused whether doc is a string or a list of strings
          cons? doc =>  [SUBSTITUTE($charNewline, $charFauxNewline, i) for i in doc]
          SUBSTITUTE($charNewline, $charFauxNewline,doc)
       htSay ndoc 
--  htSaySaturn '"\\"
    htSayIndentRel(-15)
  --------> print abbr and source file for constructors <---------
  if which = '"constructor" then
    if (abbr := getConstructorAbbreviationFromDB conname) then
      htSaySaturn '"\\"
      htSaySaturn '"{\em Abbreviation:}"
      htSaySaturnAmpersand()
      htSayStandard('"\tab{2}{\em Abbreviation:}")
      htSayIndentRel(15)
      htSay abbr
      htSayIndentRel(-15)
      htSayStandard('"\newline{}")
    if ( $saturn and (link := saturnHasExamplePage conname)) then
      htSaySaturn '"\\"
      htSaySaturn '"{\em Examples:}"
      htSaySaturnAmpersand()
      htSayIndentRel(15)
      htSay '"\spadref{"
      htSay second link
      htSay '"}"
      htSayIndentRel(-15)
      htSayStandard('"\newline{}")
    htSaySaturn '"\\"
    htSaySaturn '"{\em Source File:}"
    htSaySaturnAmpersand()
    htSayStandard('"\tab{2}{\em Source File:}")
    htSayIndentRel(15)
    htSaySourceFile conname
    htSayIndentRel(-15)
  ------------------> remove profile printouts for now <-------------------
  if $standard and
    exactlyOneOpSig and (infoAlist := htpProperty(htPage,'infoAlist)) then
      displayInfoOp(htPage,infoAlist,op,sig)
  -----------------------------------------------------------
  htSaySaturn '"\end{tabular}"

htSaySourceFile conname ==
  sourceFileName := (getConstructorSourceFileFromDB conname or '"none")
  filename :=  extractFileNameFromPath sourceFileName
  htMakePage [['text,'"\unixcommand{",filename,'"}{", textEditor(), '" ",
              sourceFileName, '" ", conname, '"}"]]

htSayIndentRel(n,:options) ==
  flag := IFCAR options
  m := abs n
  if flag then m := m + 2
  if $standard then htSayStandard
    n > 0 =>
      flag => ['"\indent{",toString m,'"}\tab{-2}"]
      ['"\indent{",toString m,'"}\tab{0}"]
    n < 0 => ['"\indent{0}\newline "]

htSayUnexposed() ==
  htSay '"{\em *}"
  $atLeastOneUnexposed := true
--=======================================================================
--                       Page Operations
--=======================================================================

htEndTabular() ==
  htSaySaturn '"\end{tabular}"

htPopSaturn s ==
  pageDescription := $saturnPage.7
  pageDescription is [=s,:b] => $saturnPage.7 := rest pageDescription
  nil

htBeginTable() ==
  htSaySaturn '"\begin{dirlist}[lv]"
  htSayStandard '"\table{"

htEndTable() ==
  htSaySaturn '"\end{dirlist}"
  htSayStandard '"}"

htBeginMenu(kind,:options) ==
  skip := IFCAR options
  if $saturn then
    kind = 'description => htSaySaturn '"\begin{description}"
    htSaySaturn '"\begin{tabular}"
    htSaySaturn
      kind = 3 => '"{llp{0in}}"
      kind = 2 => '"{lp{0in}}"
      error nil
  null skip => htSayStandard '"\beginmenu "
  nil

htEndMenu(kind) ==
  if $saturn then
    kind = 'description => htSaySaturn '"\end{description}"
    htPopSaturn '"\\"
    htSaySaturn '"\end{tabular}"
  htSayStandard '"\endmenu "

htSayConstructorName(nameShown, name) ==
  if $saturn then
    code := ['"(|conPage| '|", name, '"|)"]
    htSaySaturn mkDocLink(code,nameShown)
  if $standard then
    htSayStandard ["\lispdownlink{",nameShown,'"}{(|conPage| '|",name,'"|)}"]

htAddHeading(title) ==
  htNewPage title
  page()

------------> called by htAddHeading, htInitPageNoScroll <-----------
htNewPage title ==
  if $saturn then
    htSaySaturn '"\browseTitle{"
    htSaySaturn title
    htSaySaturn '"}"
  if $standard then htSayStandard('"\begin{page}{", htpName $curPage, '"}{")
  htSayStandard title
  htSayStandard '"}"

--=======================================================================
--                       Utilities
--=======================================================================
mkTabularItem u == [:first u,:fn rest u] where fn x ==
  null x => nil
  [$saturnAmpersand, x,:fn rest x]

htSaySaturnAmpersand() == htSaySaturn $saturnAmpersand

htBlank(:options) ==
  options is [n] =>
    htSaySaturn(strconc/['"\phantom{*}" for i in 1..n])
    htSayStandard strconc('"\space{",toString n,'"}")
  htSaySaturn '"\phantom{*}"
  htSayStandard '"\space{1}"

unTab s ==
  string? s => unTab1 s
  s isnt [.,:.] => s
  [unTab1 first s, :rest s]

unTab1 s ==
  ('"\tab{" < s) = 5 and (k := charPosition(char "}", s, 4)) =>
      subString(s, k + 1)
  s

satBreak() ==
  htSaySaturn '"\\ "
  htSayStandard '"\item "

htBigSkip() ==
  htSaySaturn '"\bigskip{}"
  htSayStandard '"\vspace{1}\newline "

htSaturnBreak() == htSaySaturn '"\!"

satDownLink(s,code) ==
  htSaySaturn '"\lispFunctionLink{\verb!"
  htSaySaturn code
  htSaySaturn '"!}{"
  htSaySaturn s
  htSaySaturn '"}"
  ------------------
  htSayStandard '"\lispdownlink{"
  htSayStandard s
  htSayStandard '"}{"
  htSayStandard code
  htSayStandard '"}"

satTypeDownLink(s,code) ==
  htSaySaturn '"\lispLink[d]{\verb!"
  htSaySaturn code
  htSaySaturn '"!}{"
  htSaySaturn s
  htSaySaturn '"}"
  ------------------
  htSayStandard '"\lispdownlink{"
  htSayStandard s
  htSayStandard '"}{"
  htSayStandard code
  htSayStandard '"}"

mkButtonBox n == strconc('"\buttonbox{", STRINGIMAGE n, '"}")

--=======================================================================
--      Create separate databases for operations, constructors
--=======================================================================
-----------> use br-data.boot definition
--dbSplitLibdb() ==
--This function splits lidbd.text into files to make searching quicker.
--  alibdb.text      attributes
--  clibdb.text      categories
--  dlibdb.text      domains
--  plibdb.text      packages
--  olibdb.text      operations
--  xlibdb.text      default packages
--These files have the same format as the single file libdb.text did in old
--  version: e.g. <key><name>`<args>`<exposure>`<sig>`<args>`<abbrev>`<doc>
--  for constructors where <key> is a single character, one of acdopx
--  (identifying it as an attribute, category, domain, operator, package,
--  or default package), its name, number of arguments, whether exposed or
--  unexposed, its signature (sometimes abbreviated), its arguments as given
--  in the original definition, its abbreviation, and documentation.
--  For example, domain Matrix has line "dMatrix`1`x`<sig>`(R)`MATRIX`<com>"
--  where <sig> is "(Ring)->Join(MatrixCategory(R,Vector(R),Vector(R)),etc)".
--  The comment field <com> contains the character address of the comments
--  for Matrix in file comdb.text.
--There is thus ONE file comdb.text for documentation of all structures
--  (to facilitate a general search through all documentation)
--  into for comments. The format of entries in comdb.text are lines with
--  two fields of the form d<nnnnn>`<ccccc>, where <nnnnn> is the character
--  address of the line "dMatrix`.." in dlibdb.text (the first character
--  "d" tells which lidbdb file it comes from, the <ccccc> is the
--  documentation for Matrix.
--NOTE: In each file, the first character, one of acdpox, is retained
--  so that lines have the same format as the previous version of the browser
--  (this minimized the number of lines of code that had to be changed from
--  previous version of the browser).
--  key := nil    --dummy first key
--  instream  := MAKE_-INSTREAM  '"libdb.text"
--  comstream := MAKE_-OUTSTREAM '"comdb.text"
--  PRINC(0,    comstream)
--  PRINC($tick,comstream)
--  PRINC('"",  comstream)
--  TERPRI(comstream)
--  while (line := readLine instream) ~= %nothing repeat
--    comP := FILE_-POSITION comstream
--    if key ~= line.0 then
--      if outstream then SHUT outstream
--      key := line . 0
--      outstream := MAKE_-OUTSTREAM strconc(STRINGIMAGE key,'"libdb.text")
--    outP := FILE_-POSITION outstream
--    [prefix,:comments] := dbSplit(line,6,1)
--    PRINC(prefix,outstream)
--    PRINC($tick ,outstream)
--    null comments =>
--      PRINC(0,outstream)
--      TERPRI(outstream)
--    PRINC(comP,outstream)
--    TERPRI(outstream)
--    PRINC(key,   comstream)   --identifies file the backpointer is to
--    PRINC(outP  ,comstream)
--    PRINC($tick ,comstream)
--    PRINC(first comments,comstream)
--    TERPRI(comstream)
--    for c in rest comments repeat
--      PRINC(key,   comstream)   --identifies file the backpointer is to
--      PRINC(outP  ,comstream)
--      PRINC($tick ,comstream)
--      PRINC(c, comstream)
--      TERPRI(comstream)
--  SHUT instream
--  SHUT outstream
--  SHUT comstream
--removeFile '"libdb.text"

dbSort(x,y) ==
  sin := STRINGIMAGE x
  sout:= STRINGIMAGE y
  runCommand strconc('"sort -f _"",sin,'".text_" > _"", sout, '".text_"")
  removeFile strconc(sin, '".text")


bcConform1 form == main where
  main() ==
    form is ['ifp,form1,:pred] =>
      hd form1
      bcPred pred
    hd form
  hd form ==
    form isnt [.,:.] =>
      -- string literals, e.g. "failed", are constructor arguments
      -- too, until we fix that.
      string? form or not (ident? form and isConstructorName form) =>
        s := 
          string? form => strconc('"_"",form,'"_"")
          STRINGIMAGE form
        stringChar(s,0) = char "#" =>
           (n := symbolPosition(form,$FormalFunctionParameterList)) =>
              htSay form2HtString ($FormalMapVariableList . n)
           htSay '"\"
           htSay form
        htSay escapeSpecialChars s
      s := STRINGIMAGE form
      $italicHead? => htSayItalics s
      $bcMultipleNames =>
        satTypeDownLink(s, ['"(|conPageChoose| '|",s,'"|)"])
      satTypeDownLink(s, ["(|conPage| '|",s,'"|)"])
    (head := form.op) = 'QUOTE =>
      htSay('"'")
      hd second form
    head = 'SIGNATURE =>
      htSay(second form,'": ")
      mapping third form
    head = 'Mapping and rest form => rest form => mapping rest form
    head = ":" =>
      hd second form
      htSay '": "
      hd third form
    form.args and dbEvalableConstructor? form
       => bcConstructor(form,head)
    hd head
    null (r := form.args) => nil
    tl form.args
  mapping [target,:source] ==
    tuple source
    bcHt
      $saturn => '" {\ttrarrow} "
      '" -> "
    hd target
  tuple u ==
    null u => bcHt '"()"
    null rest u => hd u
    bcHt '"("
    hd first u
    for x in rest u repeat
      bcHt '","
      hd x
    bcHt '")"
  tl u ==
    bcHt '"("
    firstTime := true
    for x in u repeat
      if not firstTime then bcHt '","
      firstTime := false
      hd x
    bcHt '")"
  say x ==
    if $italics? then bcHt '"{\em "
    if x = 'etc then x := '"..."
    bcHt escapeSpecialIds STRINGIMAGE x
    if $italics? then bcHt '"}"

--=======================================================================
--            Code for Private Libdbs
--=======================================================================
--extendLocalLibdb conlist ==     --called by function "compiler"(see above)
--  buildLibdb conlist          --> puts datafile into temp.text
--  $newConstructorList := union(conlist, $newConstructorList)
--  localLibdb := '"libdb.text"
--  not isExistingFile '"libdb.text" => RENAME_-FILE('"temp.text",'"libdb.text")
--  oldlines := purgeNewConstructorLines(dbReadLines localLibdb, conlist)
--  newlines := dbReadLines '"temp.text"
--  dbWriteLines(MSORT union(oldlines,newlines), '"libdb.text")
--  removeFile '"temp.text"

purgeNewConstructorLines(lines, conlist) ==
  [x for x in lines | not screenLocalLine(x, conlist)]

-- Got rid of debugging statement and deleted screenLocalLine1, MCD 26/3/96
--screenLocalLine(line,conlist) ==
--  u := screenLocalLine1(line,conlist)
--  if u then
--    sayBrightly ['"Purging--->", line]
--  u

-- screenLocalLine1(line, conlist) ==
screenLocalLine(line, conlist) ==
  k := dbKind line
  con := makeSymbol
    k = char "o" or k = char "a" =>
      s := dbPart(line,5,1)
      k := charPosition(char "(",s,1)
      subString(s,1,k - 1)
    dbName line
  symbolMember?(con, conlist)

purgeLocalLibdb() ==   --called by the user through a clear command?
  $newConstructorList := nil
  removeFile '"libdb.text"

--moveFile(before,after) ==
--  $saturn => MOVE_-FILE(before, after)
--  RENAME_-FILE(before, after)
--  --obey strconc('"mv ", before, '" ", after)

-- deleted JHD/MCD, since already one in pathname.boot
--removeFile fn ==
--  $saturn => DELETE_-FILE fn
--  obey strconc('"rm ",fn)

--=======================================================================
--            from DAASE.LISP
--=======================================================================
--library(args) ==
--  $newConlist: local := nil
--  LOCALDATABASE(args,$options)
--  extendLocalLibdb $newConlist
--  TERSYSCOMMAND()


