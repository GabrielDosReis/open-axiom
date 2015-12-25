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


import ht_-util
namespace BOOT

$historyDisplayWidth := 120
$newline := abstractChar 10

downlink page ==
  htInitPage('"Bridge",nil)
  htSay('"\replacepage{", page, '"}")
  htShowPage()

dbNonEmptyPattern pattern ==
  null pattern => '"*"
  pattern := STRINGIMAGE pattern
  #pattern > 0 => pattern
  '"*"

htSystemVariables() == main where
  main() ==
    not $fullScreenSysVars => htSetVars()
    classlevel := $UserLevel
    $levels : local := '(compiler development interpreter)
    $heading  : local := nil
    while classlevel ~= first $levels repeat $levels := rest $levels
    table := reverse! fn($setOptions,nil,true)
    htInitPage('"System Variables",nil)
    htSay '"\beginmenu"
    lastHeading := nil
    for [heading,name,message,.,key,variable,options,func] in table repeat
      htSay('"\newline\item ")
      if heading = lastHeading then htSay '"\tab{8}" else
        htSay(heading,'"\tab{8}")
        lastHeading := heading
      htSay('"{\em ",name,"}\tab{22}",message)
      htSay('"\tab{80}")
      key = 'FUNCTION =>
         null options => htMakePage [['bcLinks,['"reset",'"",func,nil]]]
         [msg,class,var,valuesOrFunction,:.] := first options  --skip first message
         functionTail(name,class,var,valuesOrFunction)
         for option in rest options repeat
           option is ['break,:.] => 'skip
           [msg,class,var,valuesOrFunction,:.] := option
           htSay('"\newline\tab{22}", msg,'"\tab{80}")
           functionTail(name,class,var,valuesOrFunction)
      val := eval variable
      displayOptions(name,key,variable,val,options)
    htSay '"\endmenu"
    htShowPage()
   where
    functionTail(name,class,var,valuesOrFunction) ==
      val := eval var
      valuesOrFunction isnt [.,:.] =>
        htMakePage '((domainConditions (isDomain STR (String))))
        htMakePage [['bcLinks,['"reset",'"",'htSetSystemVariableKind,[var,name,nil]]]]
        htMakePage [['bcStrings,[30,STRINGIMAGE val,name,valuesOrFunction]]]
      displayOptions(name,class,var,val,valuesOrFunction)
    displayOptions(name,class,variable,val,options) ==
      class = 'INTEGER =>
        htMakePage [['bcLispLinks,[[['text,options.0,'"-",options.1 or '""]],'"",'htSetSystemVariableKind,[variable,name,'PARSE_-INTEGER]]]]
        htMakePage '((domainConditions (isDomain INT (Integer))))
        htMakePage  [['bcStrings,[5,STRINGIMAGE val,name,'INT]]]
      class = 'STRING =>
        htSay('"{\em ",val,'"}\space{1}")
      for x in options repeat
        val = x or val = true and x = 'on or null val and x = 'off =>
          htSay('"{\em ",x,'"}\space{1}")
        htMakePage [['bcLispLinks,[x,'" ",'htSetSystemVariable,[variable,x]]]]
    fn(t,al,firstTime) ==
      t isnt [.,:.] => al
      if firstTime then $heading := opOf first t
      fn(rest t,gn(first t,al),firstTime)
    gn(t,al) ==
      [.,.,class,key,.,options,:.] := t
      not symbolMember?(class,$levels) => al
      key = 'LITERALS or key = 'INTEGER or key = 'STRING => [[$heading,:t],:al]
      key = 'TREE => fn(options,al,false)
      key = 'FUNCTION => [[$heading,:t],:al]
      systemError key

htSetSystemVariableKind(htPage,[variable,name,fun]) ==
  value := htpLabelInputString(htPage,name)
  if string? value and fun then value := apply(fun,[value])
--SCM::what to do???  if not integer? value then userError ???
  symbolValue(variable) := value
  htSystemVariables ()

htSetSystemVariable(htPage,[name,value]) ==
  value :=
    value = 'on => true
    value = 'off => nil
    value
  symbolValue(name) := value
  htSystemVariables ()

htGloss(pattern) == htGlossPage(nil,dbNonEmptyPattern pattern or '"*",true)

htGlossPage(htPage,pattern,tryAgain?) ==
  $wildCard: local := char "*"
  pattern = '"*" => downlink 'GlossaryPage
  filter := pmTransFilter pattern
  grepForm := mkGrepPattern(filter,'none)
  $key: local := 'none
  results := applyGrep(grepForm,'gloss)
  defstream := MAKE_-INSTREAM strconc(systemRootDirectory(),'"doc/glossdef.text")
  lines := gatherGlossLines(results,defstream)
  -- removeFile pathname 
  --SHUT instream
  heading :=
    pattern = '"" => '"Glossary"
    null lines => ['"No glossary items match {\em ",pattern,'"}"]
    ['"Glossary items matching {\em ",pattern,'"}"]
  null lines =>
    tryAgain? and #pattern > 0 =>
      (pattern.(k := maxIndex(pattern))) = char "s" =>
        htGlossPage(htPage,subString(pattern,0,k),true)
      upperCase? pattern.0 =>
        htGlossPage(htPage,DOWNCASE pattern,false)
      errorPage(htPage,['"Sorry",nil,['"\centerline{",:heading,'"}"]])
    errorPage(htPage,['"Sorry",nil,['"\centerline{",:heading,'"}"]])
  htInitPageNoScroll(nil,heading)
  htSay('"\beginscroll\beginmenu")
  for line in lines repeat
    tick := charPosition($tick,line,1)
    htSay('"\item{\em \menuitemstyle{}}\tab{0}{\em ",escapeString subString(line,0,tick),'"} ",subString(line,tick + 1))
  htSay '"\endmenu "
  htSay '"\endscroll\newline "
  htMakePage [['bcLinks,['"Search",'"",'htGlossSearch,nil]]]
  htSay '" for glossary entry matching "
  htMakePage [['bcStrings, [24,'"*",'filter,'EM]]]
  htShowPageNoScroll()

gatherGlossLines(results,defstream) ==
  acc := nil
  for keyline in results repeat
    n := charPosition($tick,keyline,0)
    keyAndTick := subString(keyline,0,n + 1)
    byteAddress := string2Integer subString(keyline,n + 1)
    setFileCursor(defstream,byteAddress)
    line := readLine defstream
    k := charPosition($tick,line,1)
    pointer := subString(line,0,k)
    def := subString(line,k + 1)
    xtralines := nil
    while (x := readLine defstream) ~= %nothing and
      (j := charPosition($tick,x,1)) and (nextPointer := subString(x,0,j))
        and (nextPointer = pointer) repeat
          xtralines := [subString(x,j + 1),:xtralines]
    acc := [strconc(keyAndTick,def, strconc/reverse! xtralines),:acc]
  reverse acc

htGlossSearch(htPage,junk) ==  htGloss htpLabelInputString(htPage,'filter)

htGreekSearch(filter) ==
  ss := dbNonEmptyPattern filter
  s := pmTransFilter ss
  s is ['error,:.] => bcErrorPage s
  not s => errorPage(nil,[['"Missing search string"],nil,
    '"\vspace{2}\centerline{To select one of the greek letters:}\newline ",
      '"\centerline{{\em first} enter a search key into the input area}\newline ",
        '"\centerline{{\em then } move the mouse cursor to the work {\em search} and click}"])
  filter := patternCheck s
  names := '(alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu nu pi)
  for x in names repeat
    superMatch?(filter,PNAME x) => matches := [x,:matches]
    nonmatches := [x,:nonmatches]
  matches    := reverse! matches
  nonmatches := reverse! nonmatches
  htInitPage('"Greek Names",nil)
  null matches =>
    htInitPage(['"Greek names matching search string {\em ",ss,'"}"],nil)
    htSay("\vspace{2}\centerline{Sorry, but no greek letters match your search string}\centerline{{\em ",ss,"}}\centerline{Click on the up-arrow to try again}")
    htShowPage()
  htInitPage(['"Greek letters matching search string {\em ",ss,'"}"],nil)
  if nonmatches
    then htSay('"The greek letters that {\em match} your search string {\em ",ss,'"}:")
    else htSay('"Your search string {\em ",ss,"} matches all of the greek letters:")
  htSay('"{\em \table{")
  for x in matches repeat htSay('"{",x,'"}")
  htSay('"}}\vspace{1}")
  if nonmatches then
    htSay('"The greek letters that {\em do not match} your search string:{\em \table{")
    for x in nonmatches repeat htSay('"{",x,'"}")
    htSay('"}}")
  htShowPage()

htTextSearch(filter) ==
  s := pmTransFilter dbNonEmptyPattern filter
  s is ['error,:.] => bcErrorPage s
  not s => errorPage(nil,[['"Missing search string"],nil,
    '"\vspace{2}\centerline{To select one of the lines of text:}\newline ",
      '"\centerline{{\em first} enter a search key into the input area}\newline ",
        '"\centerline{{\em then } move the mouse cursor to the work {\em search} and click}"])
  filter := s
  lines := ['"{{\em Fruit flies} *like* a {\em banana and califlower ears.}}",
            '"{{\em Sneak Sears Silas with Savings Snatch}}"]
  for x in lines repeat
    superMatch?(filter,x) => matches := [x,:matches]
    nonmatches := [x,:nonmatches]
  matches    := reverse! matches
  nonmatches := reverse! nonmatches
  htInitPage('"Text Matches",nil)
  null matches =>
    htInitPage(['"Lines matching search string {\em ",s,'"}"],nil)
    htSay("\vspace{2}\centerline{Sorry, but no lines match your search string}\centerline{{\em ",s,"}}\centerline{Click on the up-arrow to try again}")
    htShowPage()
  htInitPage(['"Lines matching search string {\em ",s,'"}"],nil)
  if nonmatches
    then htSay('"The lines that {\em match} your search string {\em ",s,'"}:")
    else htSay('"Your search string {\em ",s,"} matches both lines:")
  htSay('"{\em \table{")
  for x in matches repeat htSay('"{",x,'"}")
  htSay('"}}\vspace{1}")
  if nonmatches then
    htSay('"The line that {\em does not match} your search string:{\em \table{")
    for x in nonmatches repeat htSay('"{",x,'"}")
    htSay('"}}")
  htShowPage()

htTutorialSearch pattern ==
  s := dbNonEmptyPattern pattern or return
    errorPage(nil,['"Empty search key",nil,'"\vspace{3}\centerline{You must enter some search string"])
  s := mkUnixPattern s
  source := '"$AXIOM/share/hypertex/pages/ht.db"
  target := '"/tmp/temp.text.$SPADNUM"
  runCommand strconc('"$AXIOM/lib/hthits",'" _"",s,'"_" ",source,'" > ",target)
  lines := dbReadLines 'temp
  htInitPageNoScroll(nil,['"Tutorial Pages mentioning {\em ",pattern,'"}"])
  htSay('"\beginscroll\table{")
  for line in lines repeat
    [name,title,.] := dbParts(line,3,0)
    htSay ['"{\downlink{",title,'"}{",name,'"}}"]
  htSay '"}"
  htShowPage()

mkUnixPattern s ==
  u := mkUpDownPattern s
  starPositions := reverse [i for i in 1..(-1 + maxIndex u) | u.i = $wild]
  for i in starPositions repeat
    u := strconc(subString(u,0,i),'".*",subString(u,i + 1))
  if stringChar(u,0) ~= $wild
  then u := strconc('"[^a-zA-Z]",u)
  else u := subString(u,1)
  if stringChar(u,k := maxIndex u) ~= $wild
  then u := strconc(u,'"[^a-zA-Z]")
  else u := subString(u,0,k)
  u


