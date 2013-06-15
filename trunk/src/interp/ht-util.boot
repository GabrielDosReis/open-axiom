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


import sys_-macros
namespace BOOT

-- HyperTeX Utilities for generating basic Command pages

$bcParseOnly := true

-- List of issued hypertex lines
$htLineList := nil

-- pointer to the page we are currently defining
$curPage := nil

-- List of currently active window named
$activePageList := nil

--%

htpDestroyPage(pageName) ==
  symbolMember?(pageName,$activePageList) =>
    symbolValue(pageName) := nil
    $activePageList := remove!($activePageList,pageName)

htpName htPage ==
-- a symbol whose value is the page
  htPage.0

htpSetName(htPage, val) ==
  htPage.0 := val

htpDomainConditions htPage ==
-- List of Domain conditions
  htPage.1

htpSetDomainConditions(htPage, val) ==
  htPage.1 := val

htpDomainVariableAlist htPage ==
-- alist of pattern variables and conditions
  htPage.2

htpSetDomainVariableAlist(htPage, val) ==
  htPage.2 := val

htpDomainPvarSubstList htPage ==
-- alist of user pattern variables to system vars
  htPage.3

htpSetDomainPvarSubstList(htPage, val) ==
  htPage.3 := val

htpRadioButtonAlist htPage ==
-- alist of radio button group names and labels
  htPage.4

htpButtonValue(htPage, groupName) ==
  for buttonName in LASSOC(groupName, htpRadioButtonAlist htPage) repeat
    (stripSpaces htpLabelInputString(htPage, buttonName)) = '"t" =>
      return buttonName

htpSetRadioButtonAlist(htPage, val) ==
  htPage.4 := val

htpInputAreaAlist htPage ==
-- Alist of input-area labels, and default values
  htPage.5

htpSetInputAreaAlist(htPage, val) ==
  htPage.5 := val

htpAddInputAreaProp(htPage, label, prop) ==
  htPage.5 := [[label, nil, nil, nil, :prop], :htPage.5]

htpPropertyList htPage ==
-- Association list of user-defined properties
  htPage.6

htpProperty(htPage, propName) ==
  LASSOC(propName, htPage.6)

htpSetProperty(htPage, propName, val) ==
  pair := assoc(propName, htPage.6)
  pair => pair.rest := val
  htPage.6 := [[propName, :val], :htPage.6]

htpLabelInputString(htPage, label) ==
-- value user typed as input string on page
  props := LASSOC(label, htpInputAreaAlist htPage)
  props and string? (s := props.0) =>
    s = '"" => s
    trimString s
  nil

htpLabelFilteredInputString(htPage, label) ==
-- value user typed as input string on page
  props := LASSOC(label, htpInputAreaAlist htPage)
  props =>
    #props > 5 and props.6 =>
      FUNCALL(symbolFunction props.6, props.0)
    replacePercentByDollar props.0
  nil

replacePercentByDollar s == fn(s,0,maxIndex s) where
  fn(s,i,n) ==
    i > n => '""
    (m := charPosition(char "%",s,i)) > n => subString(s,i)
    strconc(subString(s,i,m - i),'"$",fn(s,m + 1,n))

htpSetLabelInputString(htPage, label, val) ==
-- value user typed as input string on page
  props := LASSOC(label, htpInputAreaAlist htPage)
  props => props.0 := STRINGIMAGE val
  nil

htpLabelSpadValue(htPage, label) ==
-- Scratchpad value of parsed and evaled inputString, as (type . value)
  props := LASSOC(label, htpInputAreaAlist htPage)
  props => props.1
  nil

htpSetLabelSpadValue(htPage, label, val) ==
-- value user typed as input string on page
  props := LASSOC(label, htpInputAreaAlist htPage)
  props => props.1 := val
  nil

htpLabelErrorMsg(htPage, label) ==
-- error message associated with input area
  props := LASSOC(label, htpInputAreaAlist htPage)
  props => props.2
  nil

htpSetLabelErrorMsg(htPage, label, val) ==
-- error message associated with input area
  props := LASSOC(label, htpInputAreaAlist htPage)
  props => props.2 := val
  nil

htpLabelType(htPage, label) ==
-- either 'string or 'button
  props := LASSOC(label, htpInputAreaAlist htPage)
  props => props.3
  nil

htpLabelDefault(htPage, label) ==
-- default value for the input area
  msg := htpLabelInputString(htPage, label) =>
    msg = '"t" => 1
    msg = '"nil" => 0
    msg
  props := LASSOC(label, htpInputAreaAlist htPage)
  props =>
    props.4
  nil


htpLabelSpadType(htPage, label) ==
-- pattern variable for target domain for input area
  props := LASSOC(label, htpInputAreaAlist htPage)
  props => props.5
  nil

htpLabelFilter(htPage, label) ==
-- string to string mapping applied to input area strings before parsing
  props := LASSOC(label, htpInputAreaAlist htPage)
  props => props.6
  nil

htpPageDescription htPage ==
-- a list of all the commands issued to create the basic-command page
  htPage.7

htpSetPageDescription(htPage, pageDescription) ==
  htPage.7 := pageDescription

iht line ==
-- issue a single hyperteTeX line, or a group of lines
  $newPage => nil
  cons? line =>
    $htLineList := append!(reverse! mapStringize copyList line, $htLineList)
  $htLineList := [basicStringize line, :$htLineList]

bcIssueHt line ==
  cons? line => htMakePage1 line
  iht line

mapStringize l ==
  l isnt [.,:.] => l
  l.first := basicStringize first l
  l.rest := mapStringize rest l
  l

basicStringize s ==
  string? s =>
    s = '"\$"      => '"\%"
    s = '"{\em $}" => '"{\em \%}"
    s
  s = '_$ => '"\%"
  toString s

stringize s ==
  string? s => s
  toString s

htQuote s ==
-- wrap quotes around a piece of hyperTeX
  iht '"_""
  iht s
  iht '"_""

htProcessToggleButtons buttons ==
  iht '"\newline\indent{5}\beginitems "
  for [message, info, defaultValue, buttonName] in buttons repeat
    if null LASSOC(buttonName, htpInputAreaAlist $curPage) then
      setUpDefault(buttonName, ['button, defaultValue])
    iht ['"\item{\em\inputbox[", htpLabelDefault($curPage, buttonName), '"]{",
         buttonName, '"}{\htbmfile{pick}}{\htbmfile{unpick}}\space{}"]
    bcIssueHt message
    iht '"\space{}}"
    bcIssueHt info
  iht '"\enditems\indent{0} "

htProcessBcButtons buttons ==
  for [defaultValue, buttonName] in buttons repeat
    if null LASSOC(buttonName, htpInputAreaAlist $curPage) then
      setUpDefault(buttonName, ['button, defaultValue])
    k := htpLabelDefault($curPage,buttonName)
    k = 0 => iht ['"\off{",buttonName,'"}"]
    k = 1 => iht ['"\on{", buttonName,'"}"]
    iht ['"\inputbox[", htpLabelDefault($curPage, buttonName), '"]{",
         buttonName, '"}{\htbmfile{pick}}{\htbmfile{unpick}}"]

bcSadFaces() ==
  '"\space{1}{\em\htbitmap{error}\htbitmap{error}\htbitmap{error}}"

htLispLinks(links,:option) ==
  [links,options] := beforeAfter('options,links)
  indent := symbolTarget('indent,options) or 5
  iht '"\newline\indent{"
  iht stringize indent
  iht '"}\beginitems"
  for [message, info, func, :value] in links repeat
    iht '"\item["
    call := (IFCAR option => '"\lispmemolink"; '"\lispdownlink")
    htMakeButton(call,message, mkCurryFun(func, value))
    iht ['"]\space{}"]
    bcIssueHt info
  iht '"\enditems\indent{0} "

htLispMemoLinks(links) == htLispLinks(links,true)

beforeAfter(x,u) == [[y for [y,:r] in tails u while x ~= y],r]

mkCurryFun(fun, val) ==
  name := gensym()
  importSymbol name
  code :=
    ['DEFUN, name, '(arg), ['APPLY, MKQ fun, ['CONS, 'arg, MKQ val]]]
  eval code
  name

htRadioButtons [groupName, :buttons] ==
  htpSetRadioButtonAlist($curPage, [[groupName, :buttonNames buttons],
                                    : htpRadioButtonAlist $curPage])
  boxesName := gensym()
  iht ['"\newline\indent{5}\radioboxes{", boxesName,
     '"}{\htbmfile{pick}}{\htbmfile{unpick}}\beginitems "]
  defaultValue := '"1"
  for [message, info, buttonName] in buttons repeat
    if null LASSOC(buttonName, htpInputAreaAlist $curPage) then
      setUpDefault(buttonName, ['button, defaultValue])
      defaultValue := '"0"
    iht ['"\item{\em\radiobox[", htpLabelDefault($curPage, buttonName), '"]{",
         buttonName, '"}{",boxesName, '"}\space{}"]
    bcIssueHt message
    iht '"\space{}}"
    bcIssueHt info
  iht '"\enditems\indent{0} "

htBcRadioButtons [groupName, :buttons] ==
  htpSetRadioButtonAlist($curPage, [[groupName, :buttonNames buttons],
                                    : htpRadioButtonAlist $curPage])
  boxesName := gensym()
  iht ['"\radioboxes{", boxesName,
     '"}{\htbmfile{pick}}{\htbmfile{unpick}} "]
  defaultValue := '"1"
  for [message, info, buttonName] in buttons repeat
    if null LASSOC(buttonName, htpInputAreaAlist $curPage) then
      setUpDefault(buttonName, ['button, defaultValue])
      defaultValue := '"0"
    iht ['"{\em\radiobox[", htpLabelDefault($curPage, buttonName), '"]{",
         buttonName, '"}{",boxesName, '"}"]
    bcIssueHt message
    iht '"\space{}}"
    bcIssueHt info

buttonNames buttons ==
  [buttonName for [.,., buttonName] in buttons]

htInputStrings strings ==
  iht '"\newline\indent{5}\beginitems "
  for [mess1, mess2, numChars, default, stringName, spadType, :filter]
   in strings repeat
    if null LASSOC(stringName, htpInputAreaAlist $curPage) then
      setUpDefault(stringName, ['string, default, spadType, filter])
    if htpLabelErrorMsg($curPage, stringName) then
      iht ['"\centerline{{\em ", htpLabelErrorMsg($curPage, stringName), '"}}"]

      mess2 := strconc(mess2, bcSadFaces())
      htpSetLabelErrorMsg($curPage, stringName, nil)
    iht '"\item "
    bcIssueHt mess1
    iht ['"\inputstring{", stringName, '"}{",
         numChars, '"}{", htpLabelDefault($curPage,stringName), '"} "]
    bcIssueHt mess2
  iht '"\enditems\indent{0}\newline "

htProcessDomainConditions condList ==
  htpSetDomainConditions($curPage, renamePatternVariables condList)
  htpSetDomainVariableAlist($curPage, computeDomainVariableAlist())

renamePatternVariables condList ==
  htpSetDomainPvarSubstList($curPage,
    renamePatternVariables1(condList, nil, $PatternVariableList))
  substFromAlist(condList, htpDomainPvarSubstList $curPage)

renamePatternVariables1(condList, substList, patVars) ==
  null condList => substList
  [cond, :restConds] := condList
  cond is ['isDomain, pv, pattern] or cond is ['ofCategory, pv, pattern]
    or cond is ['Satisfies, pv, cond] =>
      if pv = $EmptyMode then nsubst := substList
      else nsubst := [[pv, :first patVars], :substList]
      renamePatternVariables1(restConds, nsubst, rest patVars)
  substList

substFromAlist(l, substAlist) ==
  for [pvar, :replace] in substAlist repeat
    l := substitute(replace, pvar, l)
  l

computeDomainVariableAlist() ==
  [[pvar, :pvarCondList pvar] for [., :pvar] in
    htpDomainPvarSubstList $curPage]

pvarCondList pvar ==
  reverse! pvarCondList1([pvar], nil, htpDomainConditions $curPage)

pvarCondList1(pvarList, activeConds, condList) ==
  null condList => activeConds
  [cond, : restConds] := condList
  cond is [., pv, pattern] and symbolMember?(pv,pvarList) =>
    pvarCondList1(append!(pvarList, pvarsOfPattern pattern),
                  [cond, :activeConds], restConds)
  pvarCondList1(pvarList, activeConds, restConds)

pvarsOfPattern pattern ==
  not LISTP pattern => nil
  [pvar for pvar in rest pattern | symbolMember?(pvar,$PatternVariableList)]

htMakeTemplates(templateList, numLabels) ==
  templateList := [templateParts template for template in templateList]
  [[substLabel(i, template) for template in templateList]
    for i in 1..numLabels] where substLabel(i, template) ==
      cons? template =>
        makeSymbol strconc(first template, toString i, rest template)
      template

templateParts template ==
  not string? template => template
  i := findString('"%l", template)
  null i => template
  [subSequence(template, 0, i), : subSequence(template, i+2)]

htMakeDoneButton(message, func) ==
  bcHt '"\newline\vspace{1}\centerline{"
  if message = '"Continue" then
    bchtMakeButton('"\lispdownlink", "\ContinueBitmap", func)
  else
    bchtMakeButton('"\lispdownlink",strconc('"\box{", message, '"}"), func)
  bcHt '"} "

htProcessDoneButton [label , func] ==
  iht '"\newline\vspace{1}\centerline{"

  if label = '"Continue" then
    htMakeButton('"\lispdownlink", "\ContinueBitmap", func)
  else if label = '"Push to enter names" then
    htMakeButton('"\lispdownlink",'"\ControlBitmap{ClickToSet}", func)
  else
    htMakeButton('"\lispdownlink", strconc('"\box{", label, '"}"), func)

  iht '"} "

bchtMakeButton(htCommand, message, func) ==
  bcHt [htCommand, '"{", message,
       '"}{(|htDoneButton| '|", func, '"| (PROGN "]
  for [id, ., ., ., type, :.] in htpInputAreaAlist $curPage repeat
    bcHt ['"(|htpSetLabelInputString| ", htpName $curPage, '"'|", id, '"| "]
    if type = 'string then
      bcHt ['"_"\stringvalue{", id, '"}_""]
    else
      bcHt ['"_"\boxvalue{", id, '"}_""]
    bcHt '") "
  bcHt [htpName $curPage, '"))} "]

htProcessDoitButton [label, command, func] ==
  fun := mkCurryFun(func, [command])
  iht '"\newline\vspace{1}\centerline{"
  htMakeButton('"\lispcommand", strconc('"\box{", label, '"}"), fun)
  iht '"} "
  iht '"\vspace{2}{Select \  \UpButton{} \  to go back one page.}"
  iht '"\newline{Select \  \ExitButton{QuitPage} \  to remove this window.}"

htMakeDoitButton(label, command) ==
  -- use bitmap button if just plain old "Do It"
  if label = '"Do It" then
    bcHt '"\newline\vspace{1}\centerline{\lispcommand{\DoItBitmap}{(|doDoitButton| "
  else
    bcHt ['"\newline\vspace{1}\centerline{\lispcommand{\box{", label,
       '"}}{(|doDoitButton| "]
  bcHt htpName $curPage
  bcHt ['" _"", htEscapeString command, '"_""]
  bcHt '")}}"

  bcHt '"\vspace{2}{Select \  \UpButton{} \  to go back one page.}"
  bcHt '"\newline{Select \  \ExitButton{QuitPage} \  to remove this window.}"

doDoitButton(htPage, command) ==
  executeInterpreterCommand command

executeInterpreterCommand command ==
  PRINC command
  finishLine $OutputStream
  ncSetCurrentLine(command)
  CATCH($SpadReaderTag, parseAndInterpret command)
  not $leanMode and printPrompt()

typeCheckInputAreas htPage ==
  -- This needs to be severly beefed up
  inputAlist := nil
  errorCondition := false
  for entry in htpInputAreaAlist htPage
   | entry is [stringName, ., ., ., 'string, ., spadType, filter] repeat
    condList :=
      LASSOC(LASSOC(spadType,htpDomainPvarSubstList htPage),
             htpDomainVariableAlist htPage)
    string := htpLabelFilteredInputString(htPage, stringName)
    $bcParseOnly =>
      null ncParseFromString string =>
        htpSetLabelErrorMsg(htPage, '"Syntax Error", '"Syntax Error")
      nil
    val := checkCondition(htpLabelInputString(htPage, stringName),
                          string, condList)
    string? val =>
      errorCondition := true
      htpSetLabelErrorMsg(htPage, stringName, val)
    htpSetLabelSpadValue(htPage, stringName, val)
  errorCondition

checkCondition(s1, string, condList) ==
  condList is [['Satisfies, pvar, pred]] =>
    val := FUNCALL(pred, string)
    string? val => val
    ['(String), :wrap s1]
  condList isnt [['isDomain, pvar, pattern]] =>
    systemError '"currently invalid domain condition"
  pattern is '(String) => ['(String), :wrap s1]
  val := parseAndEval string
  string? val =>
    val = '"Syntax Error " => '"Error: Syntax Error "
    condErrorMsg pattern
  [type, : data] := val
  newType := CATCH($SpadReaderTag, resolveTM(type, pattern))
  null newType =>
    condErrorMsg pattern
  coerceInt(val, newType)

condErrorMsg type ==
  typeString := form2String type
  if cons? typeString then typeString := apply(function strconc, typeString)
  strconc('"Error: Could not make your input into a ", typeString)

parseAndEval string ==
  $InteractiveMode: local := true
  $e: local := $InteractiveFrame
  $QuietCommand: local := true
  parseAndEval1 string

parseAndEval1 string ==
  syntaxError := false
  pform :=
    v := applyWithOutputToString('ncParseFromString, [string])
    first v => first v
    syntaxError := true
    rest v
  syntaxError =>
     '"Syntax Error "
  pform =>
    val := applyWithOutputToString('processInteractive, [pform, nil])
    first val => first val
    '"Type Analysis Error"
  nil

makeSpadCommand(:l) ==
  opForm := strconc(first l, '"(")
  lastArg := last l
  l := rest l
  argList := nil
  for arg in l while arg ~= lastArg repeat
    argList := [strconc(arg, '", "), :argList]
  argList := reverse! [lastArg, :argList]
  strconc(opForm, apply(function strconc, argList), '")")

htMakeInputList stringList ==
-- makes an input form for constructing a list
  lastArg := last stringList
  argList := nil
  for arg in stringList while arg ~= lastArg repeat
    argList := [strconc(arg, '", "), :argList]
  argList := reverse! [lastArg, :argList]
  bracketString apply(function strconc, argList)


-- predefined filter strings
bracketString string == strconc('"[",string,'"]")

quoteString string == strconc('"_"", string, '"_"")

$funnyQuote := abstractChar 127
$funnyBacks := abstractChar 128

htEscapeString str ==
  str := SUBSTITUTE($funnyQuote, char "_"", str)
  SUBSTITUTE($funnyBacks, char "\", str)

unescapeStringsInForm form ==
  string? form =>
    for i in 0..maxIndex form repeat
      stringChar(form,i) = $funnyQuote => stringChar(form,i) := char "_""
      stringChar(form,i) = $funnyBacks => stringChar(form,i) := char "\"
  cons? form =>
    unescapeStringsInForm first form
    unescapeStringsInForm rest form
    form
  form





