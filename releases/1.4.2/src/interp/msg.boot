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


import sys_-macros
import astr
namespace BOOT

$erLocMsgDatabaseName     := pathname '(co_-eng msgs a)
$erGlbMsgDatabaseName     := pathname '(co_-eng msgs i)
$newcompErrorCount :=           0

$imPrTagGuys == ['unimple, 'bug, 'debug, 'say, 'warn]
$toWhereGuys == ['fileOnly, 'screenOnly ]
$imPrGuys    == ['imPr]
$repGuys     == ['noRep, 'rep]
$attrCats    == ['$imPrGuys, '$toWhereGuys, '$repGuys]

$LINELENGTH := 80
$preLength := 11
$LOGLENGTH := $LINELENGTH - 6
$specificMsgTags := []
$showKeyNum   :=        nil

$compErrorPrefix :=    '"Error"
$compBugPrefix :=      '"Bug!"

$ncMsgList := []

--%

--%  Messages for the USERS of the compiler.
-- The program being compiled has a minor error.
-- Give a message and continue processing.
ncSoftError(pos, erMsgKey, erArgL,:optAttr) ==
  $newcompErrorCount := $newcompErrorCount + 1
  desiredMsg erMsgKey =>
    processKeyedError 
       msgCreate ('error, pos, erMsgKey, erArgL, $compErrorPrefix,optAttr)
 
-- The program being compiled is seriously incorrect.
-- Give message and throw to a recovery point.
ncHardError(pos, erMsgKey, erArgL,:optAttr) ==
  $newcompErrorCount := $newcompErrorCount + 1
  desiredMsg erMsgKey =>
    processKeyedError 
       msgCreate('error,pos,erMsgKey, erArgL, $compErrorPrefix,optAttr)
  ncError()
 
-- Bug in the compiler: something which shouldn't have happened did.
ncBug (erMsgKey, erArgL,:optAttr) ==
  $newcompErrorCount := $newcompErrorCount + 1
  processKeyedError 
        msgCreate('bug,$nopos, erMsgKey, erArgL,$compBugPrefix,optAttr)
  BREAK()
  ncAbort()
 
--% Lower level functions
 
--msgObject  tag -- catagory of msg
--                    -- attributes as a-list
--                        'imPr  => dont save for list processing
--                        toWhere, screen or file
--                        'norep => only display once in list
--           pos -- position with possible FROM/TO tag
--           key -- key for message database
--          argL -- arguments to be placed in the msg test
--        prefix -- things like "Error: "
--          text -- the actual text
 
msgCreate(tag,posWTag,key,argL,optPre,:optAttr) ==
    if cons? key then tag := 'old
    msg := [tag,posWTag,key,argL,optPre,nil]
    if first optAttr then
        setMsgForcedAttrList(msg,first optAttr)
    putDatabaseStuff msg
    initImPr    msg
    initToWhere msg
    msg
 
processKeyedError msg ==
    getMsgTag? msg = 'old  =>                                 --temp
        erMsg := getMsgKey msg                                --temp
        if pre := getMsgPrefix? msg then                      --temp
          erMsg := ['"%b", pre, '"%d", :erMsg]                --temp
        sayBrightly ['"old msg from ",_
          CallerName 4,:erMsg]                  --temp
    msgImPr? msg =>
      msgOutputter msg
    $ncMsgList := [msg,:$ncMsgList]
 
---------------------------------
--%getting info from db.
putDatabaseStuff msg ==
    [text,attributes] := getMsgInfoFromKey msg
    if attributes then setMsgUnforcedAttrList(msg,attributes)
    setMsgText(msg,text)
 
getMsgInfoFromKey msg ==
    $msgDatabaseName : local := []
    msgText :=
        msgKey := getMsgKey? msg =>   --temp  oldmsgs use key tostoretext
           dbL := [$erLocMsgDatabaseName,$erGlbMsgDatabaseName]
           getErFromDbL (msgKey,dbL)
        getMsgKey msg                  --temp oldmsgs
    msgText := segmentKeyedMsg  msgText
    [msgText,attributes] := removeAttributes msgText
    msgText := substituteSegmentedMsg(msgText, getMsgArgL msg)
    [msgText,attributes]
 
 
getErFromDbL (erMsgKey,dbL) ==
    erMsg := nil
    while null erMsg   repeat
        dbName := first dbL
        dbL    := rest dbL
        $msgDatabaseName      := dbName
        lastName := null dbL
--        fileFound := '"co_-eng.msgs"
        fileFound := '"s2_-us.msgs"
        if fileFound or lastName then
            erMsg := fetchKeyedMsg(erMsgKey,not lastName)
    erMsg
 
-----------------------
--%character position marking
 
processChPosesForOneLine msgList ==
    chPosList := posPointers msgList
    for msg in msgList repeat
        if getMsgFTTag? msg then
            putFTText (msg,chPosList)
        posLetter := rest assoc(poCharPosn getMsgPos msg,chPosList)
        oldPre := getMsgPrefix msg
        setMsgPrefix (msg,strconc(oldPre,_
                     makeString($preLength - 4 - # oldPre),posLetter) )
    leaderMsg := makeLeaderMsg chPosList
    append!(msgList,[leaderMsg])  --a back cons
 
posPointers msgList ==
--gets all the char posns for msgs on one line
--associates them with a uppercase letter
    pointers  := '"ABCDEFGHIJKLMONPQRS"
    increment := 0
    posList:= []
    ftPosList := []
    for msg in msgList repeat
       pos := poCharPosn getMsgPos msg
       if pos ~= IFCAR posList then
         posList := [pos,:posList]
       if getMsgFTTag? msg = 'FROMTO then
         ftPosList := [poCharPosn getMsgPos2 msg,:ftPosList]
    for toPos in ftPosList repeat
           posList := insertPos(toPos,posList)
    for pos in posList repeat
        posLetterList := [[pos,:pointers.increment],:posLetterList]
        increment := increment + 1
    posLetterList
 
insertPos(newPos,posList) ==
--insersts a position in the proper place of a positon list
--used for the 2nd pos of a fromto
    done := false
    bot  := [0,:posList]
    top  := []
    while not done repeat
        top  := [first bot,:top]
        bot  := rest bot
        pos  := first bot
        done :=
          pos < newPos => false
          pos = newPos => true
          pos > newPos =>
            top := [newPos,:top]
            true
    [rest reverse top,:bot]
 
putFTText (msg,chPosList) ==
    tag := getMsgFTTag? msg
    pos := poCharPosn getMsgPos msg
    charMarker := rest assoc(pos,chPosList)
    tag = 'FROM =>
        markingText := ['"(from ",charMarker,'" and on) "]
        setMsgText(msg,[:markingText,:getMsgText msg])
    tag = 'TO =>
        markingText := ['"(up to ",charMarker,'") "]
        setMsgText(msg,[:markingText,:getMsgText msg])
    tag = 'FROMTO =>
       pos2 := poCharPosn getMsgPos2 msg
       charMarker2 := rest assoc(pos2,chPosList)
       markingText := ['"(from ",charMarker,'" up to ",_
           charMarker2,'") "]
       setMsgText(msg,[:markingText,:getMsgText msg])
 
rep (c,n)  ==
    n > 0 => makeString(n, c)
    '""
 
--called from parameter list of nc message functions
From   pos == ['FROM,   pos]
To     pos == ['TO,     pos]
FromTo (pos1,pos2) == ['FROMTO, pos1, pos2]
 
------------------------
--%processing error lists
processMsgList (erMsgList,lineList) ==
    $outputList :local := []--grows in queueUp errors
    $noRepList :local := []--grows in queueUp errors
    erMsgList  := erMsgSort erMsgList
    for line in lineList repeat
        msgLine := makeMsgFromLine line
        $outputList := [msgLine,:$outputList]
        globalNumOfLine := poGlobalLinePosn getMsgPos msgLine
        erMsgList :=
             queueUpErrors(globalNumOfLine,erMsgList)
    $outputList := append(erMsgList,$outputList)  --the nopos's
    st := '"---------SOURCE-TEXT-&-ERRORS------------------------"
    listOutputter reverse $outputList
 
erMsgSort erMsgList ==
    [msgWPos,msgWOPos] := erMsgSep erMsgList
    msgWPos  := listSort(function erMsgCompare, msgWPos)
    msgWOPos := reverse msgWOPos
    [:msgWPos,:msgWOPos]
 
erMsgCompare(ob1,ob2)==
    pos1 :=  getMsgPos ob1
    pos2 :=  getMsgPos ob2
    compareposns(pos2,pos1)
 
erMsgSep erMsgList ==
    msgWPos  := []
    msgWOPos := []
    for msg in erMsgList repeat
        if poNopos? getMsgPos msg then
          msgWOPos := [msg,:msgWOPos]
        else
          msgWPos  := [msg,:msgWPos]
    [msgWPos,msgWOPos]
 
getLinePos line  == first line
getLineText line == rest line
 
queueUpErrors(globalNumOfLine,msgList)==
    thisPosMsgs  := []
    notThisLineMsgs := []
    for msg in msgList _
      while thisPosIsLess(getMsgPos msg,globalNumOfLine) repeat
    --these are msgs that refer to positions from earlier compilations
        if not redundant (msg,notThisPosMsgs) then
           notThisPosMsgs := [msg,:notThisPosMsgs]
        msgList := rest msgList
    for msg in msgList _
      while thisPosIsEqual(getMsgPos msg,globalNumOfLine) repeat
       if not redundant (msg,thisPosMsgs) then
           thisPosMsgs := [msg,:thisPosMsgs]
       msgList := rest msgList
    if thisPosMsgs then
        thisPosMsgs := processChPosesForOneLine  thisPosMsgs
        $outputList := append!(thisPosMsgs,$outputList)
    if notThisPosMsgs then
        $outputList := append!(notThisPosMsgs,$outputList)
    msgList
 
redundant(msg,thisPosMsgs) ==
    found := nil
    if msgNoRep? msg then
        for item in $noRepList repeat
            sameMsg?(msg,item) => return (found := true)
        $noRepList := [msg,$noRepList]
    found or listMember?(msg,thisPosMsgs)
 
sameMsg? (msg1,msg2) ==
    (getMsgKey   msg1 = getMsgKey  msg2) and _
    (getMsgArgL  msg1 = getMsgArgL msg2)
 
 
thisPosIsLess(pos,num) ==
    poNopos? pos => nil
    poGlobalLinePosn pos < num
 
thisPosIsEqual(pos,num) ==
    poNopos? pos => nil
    poGlobalLinePosn pos = num
 
--%outputting stuff
 
listOutputter outputList ==
    for msg in outputList repeat
        msgOutputter msg
 
msgOutputter msg  ==
    st := getStFromMsg msg
    shouldFlow := not (leader? msg or line? msg)
    if toScreen? msg then
       if shouldFlow then
          st := flowSegmentedMsg(st,$LINELENGTH,0)
       sayBrightly st
    if toFile? msg then
       if shouldFlow then
          st := flowSegmentedMsg(st,$LOGLENGTH,0)
       alreadyOpened := alreadyOpened? msg
        
toScreen? msg ==
  getMsgToWhere msg ~= 'fileOnly

toFile? msg   ==
  getMsgToWhere msg ~= 'screenOnly
 
 
alreadyOpened? msg ==
  not msgImPr? msg
 
getStFromMsg msg ==
    $optKeyBlanks : local := '""  --set in setOptKeyBlanks()
    setOptKeyBlanks()
    preStL := getPreStL getMsgPrefix? msg
    getMsgTag  msg = 'line =>
          [$optKeyBlanks, :preStL, getMsgText msg]
    posStL := getPosStL msg
    optKey :=
        $showKeyNum =>
            msgKey := getMsgKey? msg => PNAME msgKey
            '"no key  "
        '""
    st :=[posStL,getMsgLitSym msg,_
          optKey,:preStL,_
          tabbing msg,:getMsgText msg]
 
tabbing msg ==
    chPos := 2
    if getMsgPrefix? msg then
      chPos := chPos + $preLength - 1
    if $showKeyNum then chPos := chPos + 8
    ["%t",:chPos]
 
setOptKeyBlanks() ==
    $optKeyBlanks :=
        $showKeyNum => '"%x8"
        '""
 
getPosStL msg ==
    not showMsgPos? msg => '""
    msgPos := getMsgPos msg
    howMuch :=
        msgImPr? msg =>
            decideHowMuch (msgPos,$lastPos)
        listDecideHowMuch (msgPos,$lastPos)
    $lastPos := msgPos
    fullPrintedPos := ppos msgPos
    printedFileName :=  ['"%x2",'"[",:remLine fullPrintedPos,'"]" ]
    printedLineNum  :=  ['"%x2",'"[",:remFile fullPrintedPos,'"]" ]
    printedOrigin   :=  ['"%x2",'"[",:fullPrintedPos,'"]" ]
    howMuch  = 'ORG  => [$optKeyBlanks,:printedOrigin, '"%l"]
    howMuch  = 'LINE => [$optKeyBlanks,:printedLineNum, '"%l"]
    howMuch  = 'FILE => [$optKeyBlanks,:printedFileName, '"%l"]
    howMuch  = 'ALL  => [$optKeyBlanks,:printedFileName, '"%l",_
                         $optKeyBlanks,:printedLineNum,  '"%l"]
    '""
 
showMsgPos? msg ==
  not msgImPr? msg and not msgLeader? msg
 
 
remFile positionList ==
        IFCDR IFCDR positionList
 
remLine positionList ==
        [IFCAR positionList]
 
decideHowMuch(pos,oldPos) ==
--when printing a msg, we wish not to show pos infor that was
--shown for a previous msg with identical pos info.
--org prints out the word noposition or console
    ((poNopos? pos) and (poNopos? oldPos)) or _
      ((poPosImmediate? pos) and (poPosImmediate? oldPos))  => 'NONE
    (poNopos? pos) or (poPosImmediate? pos) => 'ORG
    (poNopos? oldPos) or (poPosImmediate? oldPos) => 'ALL
    poFileName oldPos ~= poFileName pos => 'ALL
    poLinePosn oldPos ~= poLinePosn pos => 'LINE
    'NONE
 
listDecideHowMuch(pos,oldPos) ==
    ((poNopos? pos) and (poNopos? oldPos)) or _
      ((poPosImmediate? pos) and (poPosImmediate? oldPos))  => 'NONE
    (poNopos? pos)     => 'ORG
    (poNopos? oldPos)  => 'NONE
    poGlobalLinePosn pos < poGlobalLinePosn oldPos =>
        poPosImmediate? pos => 'ORG
        'LINE
    --(poNopos? pos) or (poPosImmediate? pos) => 'ORG
    'NONE
 
getPreStL optPre ==
    null optPre => [makeString 2]
    spses :=
      (extraPlaces := ($preLength - (# optPre) - 3)) > 0 =>
        makeString extraPlaces
      '""
    ['"%b", optPre,spses,'":", '"%d"]
 
-------------------
--%   a-list stuff
desiredMsg (erMsgKey,:optCatFlag) ==
    isKeyQualityP(erMsgKey,'show)   => true
    isKeyQualityP(erMsgKey,'stifle) => false
    not null optCatFlag  => first optCatFlag
    true
 
isKeyQualityP (key,qual)  ==
    --returns pair if found, else nil
    found := false
    while not found and (qualPair := assoc(key,$specificMsgTags)) repeat
        if rest qualPair = qual then found := true
    qualPair
 
-----------------------------
--% these functions handle the attributes
 
initImPr msg  ==
  symbolMember?(getMsgTag msg,$imPrTagGuys) =>
    setMsgUnforcedAttr (msg,'$imPrGuys,'imPr)
 
initToWhere msg  ==
    member ('trace,getMsgCatAttr (msg,'catless)) =>
          setMsgUnforcedAttr (msg,'$toWhereGuys,'screenOnly)
 
msgImPr? msg ==
    (getMsgCatAttr (msg,'$imPrGuys) = 'imPr)
 
msgNoRep? msg ==
    (getMsgCatAttr (msg,'$repGuys) = 'noRep)
 
msgLeader? msg ==
    getMsgTag msg = 'leader
 
getMsgToWhere msg ==
    getMsgCatAttr (msg,'$toWhereGuys)
 
getMsgCatAttr  (msg,cat) ==
    IFCDR objectAssoc(cat, ncAlist msg)
 
setMsgForcedAttrList (msg,aL) ==
    for attr in aL repeat
        setMsgForcedAttr(msg,whichCat attr,attr)
 
setMsgUnforcedAttrList (msg,aL) ==
    for attr in aL repeat
        setMsgUnforcedAttr(msg,whichCat attr,attr)
 
setMsgForcedAttr(msg,cat,attr) ==
    cat = 'catless => setMsgCatlessAttr(msg,attr)
    ncPutQ(msg,cat,attr)
 
setMsgUnforcedAttr(msg,cat,attr) ==
    cat = 'catless => setMsgCatlessAttr(msg,attr)
    objectAssoc(cat, ncAlist msg) = nil => ncPutQ(msg,cat,attr)
 
setMsgCatlessAttr(msg,attr) ==
    ncPutQ(msg,'catless,CONS (attr, IFCDR objectAssoc("catless", ncAlist msg)))
 
whichCat attr ==
    found := 'catless
    for cat in $attrCats repeat
        -- ??? a cat is a vector.
        if listMember?(attr,eval cat) then
          found := cat
          return found
    found
 
--------------------------------------
--% these functions directly interact with the message object
 
makeLeaderMsg chPosList ==
    st := makeString($preLength- 3)
    oldPos := -1
    for [posNum,:posLetter] in reverse chPosList repeat
        st := strconc(st, _
            rep(char ".", (posNum - oldPos - 1)),posLetter)
        oldPos := posNum
    ['leader,$nopos,'nokey,nil,nil,[st]]
 
makeMsgFromLine line ==
    posOfLine  := getLinePos line
    textOfLine := getLineText line
    globalNumOfLine := poGlobalLinePosn posOfLine
    localNumOfLine  :=
        i := poLinePosn posOfLine
        stNum := toString i
        strconc(rep(char " ", ($preLength - 7 - # stNum)),_
         stNum)
    ['line,posOfLine,nil,nil, strconc('"Line", localNumOfLine),_
        textOfLine]
 
getMsgTag msg == ncTag msg
 
getMsgTag? msg ==
   IFCAR member (getMsgTag msg,_
       ['line,'old,'error,'warn,'bug,'unimple,'remark,'stat,'say,'debug])
 
leader? msg == getMsgTag msg = 'leader
line?   msg == getMsgTag msg = 'line
 
getMsgPosTagOb msg == msg.1
 
getMsgPos msg ==
    getMsgFTTag? msg => second getMsgPosTagOb msg
    getMsgPosTagOb msg
 
getMsgPos2 msg ==
    getMsgFTTag? msg => third getMsgPosTagOb msg
    ncBug('"not a from to",[])
 
getMsgFTTag? msg == IFCAR member (IFCAR getMsgPosTagOb msg,_
                      ['FROM,'TO,'FROMTO])
 
getMsgKey msg == msg.2
 
getMsgKey? msg == ident? (val := getMsgKey msg) => val
 
getMsgArgL msg == msg.3
 
getMsgPrefix? msg ==
    (pre := msg.4) = 'noPre => nil
    pre
 
getMsgPrefix  msg == msg.4
 
 
getMsgLitSym msg ==
    getMsgKey? msg => '" "
    '"*"
 
getMsgText msg == msg.5
 
setMsgPrefix (msg,val) == msg.4 := val

setMsgText (msg,val) == msg.5 := val
 
 

 
