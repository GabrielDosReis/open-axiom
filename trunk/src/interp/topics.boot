-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007, Gabriel Dos Reis.
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

)package "BOOT"

$topicsDefaults := '(
  (basic elt setelt qelt qsetelt eval xRange yRange zRange map map_! qsetelt_!)
  (conversion coerce convert retract)
  (hidden retractIfCan Zero One)
  (predicate _< _=)
  (algebraic _+ _- _* _*_* _/ quo rem exquo)
  (trignometric acos acot acsc asec asin atan cos cot csc sec sin tan)
  (hyperbolic acosh acoth acsch asech asinh atanh cosh coth csch sech sinh tanh)
  (destructive setelt qsetelt)
  (extraction xRange yRange zRange elt qelt)
  (transformation map map_!))

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

--=======================================================================
--           Create Hashtable of Operation Properties
--=======================================================================
--called at build-time before making DOCUMENTATION property
mkTopicHashTable() ==                         --given $groupAssoc = ((extended . 1)(basic . 2)(xx . 4)..)
  $defaultsHash := MAKE_-HASHTABLE 'ID        --keys are ops, value is list of topic names
  for [kind,:items] in $topicsDefaults repeat --$topicsDefaults is ((<topic> op ...) ..)
    for item in items repeat 
      HPUT($defaultsHash,item,[kind,:HGET($defaultsHash,item)])
  $conTopicHash  := MAKE_-HASHTABLE 'EQL      --key is constructor name; value is
  instream := OPEN '"topics.data"           
  while not EOFP instream repeat
    line := READLINE instream
    while blankLine? line repeat line := READLINE instream
    m := MAXINDEX line                        --file "topics.data" has form:
    m = -1 => 'skip                           --1   ConstructorName:
    line.0 = char '_- => 'skip                --2      constructorName or operation name
    line := trimString line                   --3-n    ...
    m := MAXINDEX line                        --     (blank line) ...
    line.m ^= (char '_:) => systemError('"wrong heading")
    con := INTERN SUBSTRING(line,0,m)
    alist := [lst while not EOFP instream and 
       not (blankLine? (line := READLINE instream)) and
         line.0 ^= char '_- for i in 1..
           | lst := string2OpAlist line]
    alist => HPUT($conTopicHash,con,alist)
  --initialize table of topic classes
  $topicHash := MAKE_-HASHTABLE 'ID           --$topicHash has keys: topic and value: index
  for [x,:c] in $groupAssoc repeat HPUT($topicHash,x,c)
  $topicIndex := CDR LAST $groupAssoc

  --replace each property list by a topic code
  --store under each construct an OR of all codes
  for con in HKEYS $conTopicHash repeat
    conCode := 0
    for pair in HGET($conTopicHash,con) repeat 
      RPLACD(pair,code := topicCode CDR pair)
      conCode := LOGIOR(conCode,code)
    HPUT($conTopicHash,con,
      [['constructor,:conCode],:HGET($conTopicHash,con)])      
  SHUT instream

--reduce integers stored under names to 1 + its power of 2
  for key in HKEYS $topicHash repeat 
    HPUT($topicHash,key,INTEGER_-LENGTH HGET($topicHash,key))

  $conTopicHash   --keys are ops or 'constructor', values are codes

blankLine? line ==
  MAXINDEX line = -1 or and/[line . j = (char '_ ) for j in 0..MAXINDEX line]

string2OpAlist s ==
  m := #s
  k := skipBlanks(s,0,m) or return nil
  UPPER_-CASE_-P s.k => nil       --skip constructor names
  k := 0
  while (k := skipBlanks(s,k,m)) repeat
    acc := [INTERN SUBSTRING(s,k,-k + (k := charPosition(char '_ ,s,k + 1))),:acc]
  acc := NREVERSE acc
  --now add defaults 
  if u := getDefaultProps first acc then acc := [first acc,:u,:rest acc]
  acc

getDefaultProps name ==
  u := HGET($defaultsHash,name)
  if (s := PNAME name).(m := MAXINDEX s) = char '? then u := ['p,:u]
  if s.m = char '_! then u := ['destructive,:u]
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
  if null intersection('(basic extended hidden),u) then u := ['extended,:u]
  bitIndexList := nil
  for x in REMDUP u repeat
    bitIndexList := [fn x,:bitIndexList] where fn x ==
      k := HGET($topicHash,x) => k
      HPUT($topicHash,x,$topicIndex := $topicIndex * 2)
      $topicIndex
  code := +/[i for i in bitIndexList]

--=======================================================================
--           Add Codes to Documentation Property
--=======================================================================
--called to modify DOCUMENTATION property for each "con"
addTopic2Documentation(con,docAlist) ==
  alist := HGET($conTopicHash,con) or return docAlist
  [y for x in docAlist] where y() ==
    [op,:pairlist] := x
    code := LASSOC(op,alist) or 0
    for sigDoc in pairlist repeat 
      sigDoc is [.,.] => RPLACD(rest sigDoc,code)
      systemError sigDoc
  docAlist
    
--=======================================================================
--           Test: Display Topics for a given constructor
--=======================================================================
td con ==
  $topicClasses := ASSOCRIGHT mySort
      [[HGET($topicHash,key),:key] for key in HKEYS $topicHash]      
  hash := MAKE_-HASHTABLE 'ID
  tdAdd(con,hash)
  tdPrint hash 

tdAdd(con,hash) ==
  v := HGET($conTopicHash,con)
  u := addTopic2Documentation(con,v)
--u := GETDATABASE(con,'DOCUMENTATION) 
  for pair in u | FIXP (code := myLastAtom pair) and (op := CAR pair) ^= 'construct repeat
    for x in (names := code2Classes code) repeat HPUT(hash,x,insert(op,HGET(hash,x)))

tdPrint hash ==
  for key in mySort HKEYS hash repeat 
    sayBrightly [key,'":"]
    sayBrightlyNT '"   "
    for x in HGET(hash,key) repeat sayBrightlyNT ['" ",x]
    TERPRI()

topics con ==
  --assumes that DOCUMENTATION property already has #s added
  $topicClasses := ASSOCRIGHT mySort
      [[HGET($topicHash,key),:key] for key in HKEYS $topicHash]      
  hash := MAKE_-HASHTABLE 'ID
  tdAdd(con,hash)
  for x in REMDUP [CAAR y for y in ancestorsOf(getConstructorForm con,nil)] repeat
    tdAdd(x,hash)
  for x in HKEYS hash repeat HPUT(hash,x,mySort HGET(hash,x))
  tdPrint hash 

code2Classes cc ==
  cc := 2*cc
  [x while cc ^= 0 for x in $topicClasses | ODDP (cc := QUOTIENT(cc,2))]
  
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
  for pair in GETDATABASE(con,'DOCUMENTATION) 
    | FIXP (code := myLastAtom pair) repeat
      u := ASSOC(QCAR pair,opAlist) => RPLACD(LASTNODE u,code)

--=======================================================================
--           Filter Operation by Topic
--=======================================================================

filterByTopic(opAlist,topic) ==
  bitNumber := HGET($topicHash,topic)
  [x for x in opAlist 
    | FIXP (code := myLastAtom x) and LOGBITP(bitNumber,code)]

listOfTopics(conname) ==
  doc := GETDATABASE(conname,'DOCUMENTATION)
  u := ASSOC('constructor,doc) or return nil
  code := myLastAtom u
--null FIXP code => nil
  mySort [key for key in HKEYS($topicHash) | LOGBITP(HGET($topicHash,key),code)]

