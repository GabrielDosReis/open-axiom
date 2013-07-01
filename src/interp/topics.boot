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
import sys_-utility
namespace BOOT

$topicsDefaults := '(
  (basic elt setelt qelt qsetelt eval xRange yRange zRange map map! qsetelt!)
  (conversion coerce convert retract)
  (hidden retractIfCan Zero One)
  (predicate _< _=)
  (algebraic _+ _- _* _*_* _/ quo rem exquo)
  (trignometric acos acot acsc asec asin atan cos cot csc sec sin tan)
  (hyperbolic acosh acoth acsch asech asinh atanh cosh coth csch sech sinh tanh)
  (destructive setelt qsetelt)
  (extraction xRange yRange zRange elt qelt)
  (transformation map map!))

$topicSynonyms := '(
  (b . basic)
  (h . hidden)
  (e . extended)
  (a . algebraic)
  (g . algebraic)
  (c . construct)
  (d . destructive)
  (v . conversion)
  (m . miscellaneous)
  (x . extraction)
  (p . predicate)
  (tg . trignometric)
  (hy . hyperbolic)
  (t . transformation))

$groupAssoc := '((extended . 1) (basic . 2) (hidden . 4))

$topicHash := hashTable 'EQ
GETHASH("basic",$topicHash) := 2
GETHASH("algebraic",$topicHash) := 4
GETHASH("miscellaneous",$topicHash) := 13
GETHASH("extraction",$topicHash) := 6
GETHASH("conversion",$topicHash) := 7
GETHASH("hidden",$topicHash) := 3
GETHASH("extended",$topicHash) := 1
GETHASH("destructive",$topicHash) := 5
GETHASH("transformation",$topicHash) := 10
GETHASH("hyperbolic",$topicHash) := 12
GETHASH("construct",$topicHash) := 9
GETHASH("predicate",$topicHash) := 8
GETHASH("trignometric",$topicHash) := 11


--=======================================================================
--           Create Hashtable of Operation Properties
--=======================================================================
--called at build-time before making DOCUMENTATION property
mkTopicHashTable() ==                         --given $groupAssoc = ((extended . 1)(basic . 2)(xx . 4)..)
  $defaultsHash := hashTable 'EQ        --keys are ops, value is list of topic names
  for [kind,:items] in $topicsDefaults repeat --$topicsDefaults is ((<topic> op ...) ..)
    for item in items repeat 
      tableValue($defaultsHash,item) := [kind,:tableValue($defaultsHash,item)]
  $conTopicHash  := hashTable 'EQL      --key is constructor name; value is
  instream := inputTextFile '"topics.data"           
  while not EOFP instream repeat
    line := readLine instream
    while blankLine? line repeat line := readLine instream
    m := maxIndex line                        --file "topics.data" has form:
    m = -1 => 'skip                           --1   ConstructorName:
    stringChar(line,0) = char "-" => 'skip    --2      constructorName or operation name
    line := trimString line                   --3-n    ...
    m := maxIndex line                        --     (blank line) ...
    stringChar(line,m) ~= char ":" => systemError('"wrong heading")
    con := makeSymbol subString(line,0,m)
    alist := [lst while (line := readLine instream) ~= %nothing and 
       not blankLine? line and
         stringChar(line,0) ~= char "-" for i in 1..
           | lst := string2OpAlist line]
    alist => tableValue($conTopicHash,con) := alist
  --initialize table of topic classes
  $topicHash := hashTable 'EQ           --$topicHash has keys: topic and value: index
  for [x,:c] in $groupAssoc repeat tableValue($topicHash,x) := c
  $topicIndex := rest last $groupAssoc

  --replace each property list by a topic code
  --store under each construct an OR of all codes
  for [con,:item] in entries $conTopicHash repeat
    conCode := 0
    for pair in item repeat 
      pair.rest := code := topicCode rest pair
      conCode := LOGIOR(conCode,code)
    tableValue($conTopicHash,con) := 
      [['constructor,:conCode],:tableValue($conTopicHash,con)]
  SHUT instream

--reduce integers stored under names to 1 + its power of 2
  for [key,:item] in entries $topicHash repeat 
    tableValue($topicHash,key) := INTEGER_-LENGTH item

  $conTopicHash   --keys are ops or 'constructor', values are codes

blankLine? line ==
  #line = 0 or and/[stringChar(line,j) = char " " for j in 0..maxIndex line]

string2OpAlist s ==
  m := #s
  k := skipBlanks(s,0,m) or return nil
  upperCase? stringChar(s,k) => nil       --skip constructor names
  k := 0
  while (k := skipBlanks(s,k,m)) repeat
    acc := [makeSymbol subString(s,k,-k + (k := charPosition(char " ",s,k + 1))),:acc]
  acc := reverse! acc
  --now add defaults 
  if u := getDefaultProps first acc then acc := [first acc,:u,:rest acc]
  acc

getDefaultProps name ==
  u := tableValue($defaultsHash,name)
  if stringChar(s := symbolName name,m := maxIndex s) = char "?" then u := ['p,:u]
  if stringChar(s,m) = char "!" then u := ['destructive,:u]
  u
  
skipBlanks(u,i,m) ==
  while i < m and u.i = $charBlank repeat i := i + 1
  i >= m => nil
  i

--=======================================================================
--           Compute Topic Code for Operation
--=======================================================================
topicCode lst ==
  u := [y for x in lst] where y() ==
    rename := LASSOC(x,$topicSynonyms) => rename
    x
  if setIntersection('(basic extended hidden),u) = nil then u := ['extended,:u]
  bitIndexList := nil
  for x in removeDuplicates u repeat
    bitIndexList := [fn x,:bitIndexList] where fn x ==
      k := tableValue($topicHash,x) => k
      tableValue($topicHash,x) := $topicIndex := $topicIndex * 2
      $topicIndex
  code := +/[i for i in bitIndexList]

--=======================================================================
--           Add Codes to Documentation Property
--=======================================================================
--called to modify DOCUMENTATION property for each "con"
addTopic2Documentation(con,docAlist) ==
  alist := tableValue($conTopicHash,con) or return docAlist
  [y for x in docAlist] where y() ==
    [op,:pairlist] := x
    code := LASSOC(op,alist) or 0
    for sigDoc in pairlist repeat 
      sigDoc is [.,.] => sigDoc.rest.rest := code
      systemError sigDoc
  docAlist
    
--=======================================================================
--           Test: Display Topics for a given constructor
--=======================================================================
td con ==
  $topicClasses := ASSOCRIGHT mySort
      [[val,:key] for [key,:val] in entries $topicHash]      
  hash := hashTable 'EQ
  tdAdd(con,hash)
  tdPrint hash 

tdAdd(con,hash) ==
  v := tableValue($conTopicHash,con)
  u := addTopic2Documentation(con,v)
--u := getConstructorDocumentationFromDB con
  for pair in u | integer? (code := myLastAtom pair) and (op := first pair) ~= 'construct repeat
    for x in (names := code2Classes code) repeat
      tableValue(hash,x) := insert(op,tableValue(hash,x))

tdPrint hash ==
  for key in mySort HKEYS hash repeat 
    sayBrightly [key,'":"]
    sayBrightlyNT '"   "
    for x in tableValue(hash,key) repeat sayBrightlyNT ['" ",x]
    finishLine $OutputStream

topics con ==
  --assumes that DOCUMENTATION property already has #s added
  $topicClasses := ASSOCRIGHT mySort
      [[val,:key] for [key,:val] in entries $topicHash]      
  hash := hashTable 'EQ
  tdAdd(con,hash)
  for x in removeDuplicates [CAAR y for y in ancestorsOf(getConstructorForm con,nil)] repeat
    tdAdd(x,hash)
  for [x,:y] in entries hash repeat
    tableValue(hash,x) := mySort y
  tdPrint hash 

code2Classes cc ==
  cc := 2*cc
  [x while cc ~= 0 for x in $topicClasses | odd? (cc := cc quo 2)]
  
myLastAtom x == 
  while x is [.,:x] repeat nil
  x

--=======================================================================
--           Transfer Codes to opAlist 
--=======================================================================

transferClassCodes(conform,opAlist) ==
  transferCodeCon(opOf conform,opAlist)
  for x in ancestorsOf(conform,nil) repeat
    transferCodeCon(CAAR x,opAlist)

transferCodeCon(con,opAlist) ==
  for pair in getConstructorDocumentationFromDB con
    | integer? (code := myLastAtom pair) repeat
      u := ASSOC(pair.op,opAlist) => lastNode(u).rest := code

--=======================================================================
--           Filter Operation by Topic
--=======================================================================

filterByTopic(opAlist,topic) ==
  bitNumber := tableValue($topicHash,topic)
  [x for x in opAlist 
    | integer? (code := myLastAtom x) and LOGBITP(bitNumber,code)]

listOfTopics(conname) ==
  doc := getConstructorDocumentationFromDB conname
  u := symbolTarget('constructor,doc) or return nil
  code := myLastAtom u
--not integer? code => nil
  mySort [key for [key,:val] in entries $topicHash | LOGBITP(val,code)]

