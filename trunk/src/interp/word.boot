-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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


--=======================================================================
--                        Build Directories
--=======================================================================
buildFunctionTable(dicts) ==
  sayKeyedMsg("S2GL0011",NIL)
  buildWordTable getListOfFunctionNames dicts
 
buildWordTable u ==
  table:= MAKE_-HASHTABLE 'ID
  for s in u repeat
    key := UPCASE s.0
    HPUT(table,key,[[s,:wordsOfString s],:HGET(table,key)])
  for key in HKEYS table repeat
    HPUT(table,key,
      listSort(function GLESSEQP,removeDupOrderedAlist
        listSort(function GLESSEQP, HGET(table,key),function CAR),
          function CADR))
  table
 
writeFunctionTables(filemode) ==
  $functionTable := NIL
  writeFunctionTable(filemode,'SPADU,'(SPAD))
  $functionTable := NIL
  writeFunctionTable(filemode,'SPADD,'(SPADSYS))
  $functionTable := NIL
  writeFunctionTable(filemode,'SPADC,'(SPADSYS SCRATCHPAD_-COMPILER))
  $functionTable := NIL
  'done
 
writeFunctionTable(filemode,name,dicts) ==
  _$ERASE makePathname(name,'DATABASE,filemode)
  stream:= writeLib1(name,'DATABASE,filemode)
  if not $functionTable then
    $functionTable:= buildFunctionTable dicts
  for key in HKEYS $functionTable repeat
    rwrite(object2Identifier key,HGET($functionTable,key),stream)
  RSHUT stream
  'done
 
readFunctionTable() ==
  sayKeyedMsg("S2GL0011",NIL)
  name :=
    $wordDictionary = 'user => 'SPADU
    $wordDictionary = 'development => 'SPADD
    'SPADC
  stream:= readLib(name,'DATABASE)
  table:= MAKE_-HASHTABLE 'ID
  for key in RKEYIDS makePathname(name,'DATABASE,"*") repeat
    HPUT(table,kk:=object2Identifier key, rread(kk,stream,nil))
  RSHUT stream
  table
 
removeDupOrderedAlist u ==
  -- removes duplicate entries in ordered alist
  -- (where duplicates are adjacent)
  for x in tails u repeat
    (y := rest x) and first first x = first first y => RPLACD(x,rest y)
  u
 
getListOfFunctionNames(fnames) ==
  -- fnames is a list of directories
  res := nil
  for fn in fnames repeat
    null IOSTATE(fn,'DIRECT,'_*) => 'iterate
    stream:= DEFIOSTREAM(['(MODE . INPUT),['FILE,fn,'DIRECT,'_*]],80,0)
    while (not PLACEP (x:= READ_-LINE stream)) repeat
      (s := SIZE x) < 26 => 'iterate
      res:= [SUBSTRING(x,26,NIL),:res]
    SHUT stream
  res
 
wordsOfString(s) == [UPCASE x for x in wordsOfStringKeepCase s]
 
wordsOfStringKeepCase s == wordsOfString1(s,0) or [COPY s]
 
wordsOfString1(s,j) ==
  k := or/[i for i in j..SUB1(MAXINDEX(s)) | isBreakCharacter s.i] =>
    tailWords:=
      isBreakCharacter s.(k+1) =>
        n:= or/[i for i in (k+2)..SUB1(MAXINDEX(s))|not isBreakCharacter s.i]
        null n => [SUBSTRING(s,k,nil)]
        n > k+1 => [SUBSTRING(s,k,n-k-1),:wordsOfString1(s,n-1)]
      m := or/[i for i in (k+2)..SUB1(MAXINDEX(s)) | isBreakCharacter s.i] =>
        [SUBSTRING(s,k,m-k),:wordsOfString1(s,m)]
      [SUBSTRING(s,k,nil)]
    k > j+1 => [SUBSTRING(s,j,k-j),:tailWords]
    tailWords
  nil
 
isBreakCharacter x == null SMALL__LITER x
 
-- SETANDFILEQ($functionTable,buildFunctionTable())
 
--=======================================================================
--               Augment Function Directories
--=======================================================================
add2WordFunctionTable fn ==
--called from DEF
  $functionTable and
    null LASSOC(s := PNAME fn,HGET($functionTable,(key := UPCASE s.0))) =>
      HPUT($functionTable,key,[[s,:wordsOfString s],:HGET($functionTable,key)])
 
--=======================================================================
--                       Guess Function Name
--=======================================================================
guess word ==
  u := bootFind word => INTERN u
  nil
 
bootFind word ==
  not $useWordFacility => NIL
  list:= bootSearch word
  PNAME word in list => nil --mismatch of directories: pretend it was not found
  null list => centerAndHighlight('"no match found",80,'" ")
  1 = #list => doYouWant? first list
  pickANumber(word,list)
 
doYouWant? nam ==
  center80 ['"Do you mean",:bright nam,'"?"]
  center80 ['"If so, type",:bright 'y,"or",:bright 'yes]
  center80 ['"Anything else means",:bright 'no]
  x := UPCASE queryUser nil
  MEMQ(STRING2ID_-N(x,1),'(Y YES)) => nam
  nil
 
pickANumber(word,list) ==
  clearScreen()
  centerNoHighlight(['"You asked for",:bright word],80,'"-")
  centerAndHighlight('"Do you mean one of the following?",80,'" ")
  n:= #list
  xx:= (n > 99 => 3; n > 9 => 2; 1)
  maxWidth:= 38 - 2*(1+xx)
  [short,long] := say2Split(list,nil,nil,maxWidth)
  extra:=
    REMAINDER(length := # short,2) ^= 0 => 1
    0
  halfLength:= length/2
  firstList:= TAKE(halfLength,short)
  secondList:= TAKE(-halfLength,short)
  secondStartIndex:= halfLength + extra
  shortList:=
    "append"/[[[:bright i,fillerSpaces(xx-WIDTH i,'" "),x],
      [:bright(i+secondStartIndex),fillerSpaces(xx-WIDTH (i+halfLength),'" "),y]]
        for i in 1.. for x in firstList for y in secondList]
  say2PerLineThatFit shortList
  i:= 1 + halfLength
  if extra=1 then
    sayBrightly [:bright i,fillerSpaces(xx-WIDTH i,'" "),list.(i-1)]
  for x in long for i in (1+length).. repeat
    sayBrightly [:bright i,fillerSpaces(xx-WIDTH i,'" "),x]
  center80 ['"If so: type a number between",:bright 1,'"and",:bright n,"and ENTER"]
  center80 ['"Anything else means",:bright 'no]
  y := queryUser nil
  x:= string2Integer y
  FIXP x and x >= 1 and x <= #list => list.(x-1)
  nil
 
bootSearch word ==
--if not $functionTable then $functionTable:= buildFunctionTable()
  if not $functionTable then $functionTable:= readFunctionTable()
  key := PNAME word
  list :=
    hasWildCard? key =>
      pattern := patternTran key -- converts * to &
      pattern.0 ^= '_& =>
        [x for [x,:.] in HGET($functionTable,UPCASE pattern.0)|
          match?(pattern,COPY x)]
      "append"/[[x for [x,:.] in HGET($functionTable,k)| match?(pattern,COPY x)]
                  for k in HKEYS $functionTable]
    findApproximateWords(PNAME word,$functionTable)
  list
 
findApproximateWords(word,table) ==
  words:= wordsOfString word
  upperWord:= UPCASE COPY word
  n := #words
  threshold:=
    n = 1 => 3
    4
  alist:= HGET(table,UPCASE word.0)
 
  --first try to break up as list of words
  firstTry := [x for [x,:wordList] in alist | p] where p ==
    n = #wordList =>
      sum := 0
      for entry in wordList for part in words while sum < threshold repeat
        sum:= sum + deltaWordEntry(part,entry)
      sum < threshold => true
      n < 3 => false
      sum := 0
      badWord := false
      for entry in wordList for part in words while sum < threshold repeat
        k:= deltaWordEntry(part,entry)
        k < 2 => sum:= sum + k
        null badWord => badWord := true
        sum := 1000
      sum < threshold
    n+1 = #wordList =>    --assume one word is missing
      sum := 0
      badWord := false
      for entries in tails wordList for part in words
           while sum < threshold repeat
        entry := first entries
        k:= deltaWordEntry(part,entry)
        k < 2 => sum:= sum + k
        null badWord =>
          badWord := true
          entries := rest entries      --skip this bad word
          entry := first entries
          k := deltaWordEntry(part,entry)
          k < 2 => sum := sum + k
          sum := 1000
        sum := 1000
      sum < threshold
    n-1 = #wordList =>    --assume one word too many
      sum := 0
      badWord := false
      for entry in wordList for parts in tails words
           while sum < threshold repeat
        part := first parts
        k:= deltaWordEntry(part,entry)
        k < 2 => sum:= sum + k
        null badWord =>
          badWord := true
          parts := rest parts      --skip this bad word
          part := first parts
          k := deltaWordEntry(part,entry)
          k < 2 => sum := sum + k
          sum := 1000
        sum := 1000
      sum < threshold
    false
  firstTry => firstTry
 
  --no winners, so try flattening to upper case and checking again
 
  wordSize := SIZE word
  lastThreshold := MAX(3,wordSize/2)
  vec := GETREFV lastThreshold
  for [x,:.] in alist repeat
    k := deltaWordEntry(upperWord,UPCASE COPY x)
    k < lastThreshold => vec.k := [x,:vec.k]
  or/[vec.k for k in 0..MAXINDEX vec]
 
guessFromList(key,stringList) ==
  threshold := MAX(3,(SIZE key)/2)
  vec := GETREFV threshold
  for x in stringList repeat
    k := deltaWordEntry(key,x)
    k < threshold => vec.k := [x,:vec.k]
  or/[vec.k for k in 0..MAXINDEX vec]
 
deltaWordEntry(word,entry) ==
  word = entry => 0
  ABS(diff := SIZE word - SIZE entry) > 4 => 1000
  canForgeWord(word,entry)
 
--+ Note these are optimized definitions below-- see commented out versions
--+   to understand the algorithm
canForgeWord(word,entry) ==
  forge(word,0,MAXINDEX word,entry,0,MAXINDEX entry,0)
 
forge(word,w,W,entry,e,E,n) ==
  w > W =>
    e > E => n
    QSADD1 QSPLUS(E-e,n)
  e > E => QSADD1 QSPLUS(W-w,n)
  word.w = entry.e => forge(word,w+1,W,entry,e+1,E,n)
  w=W or e=E => forge(word,w+1,W,entry,e+1,E,QSADD1 n)
  word.w=entry.(e+1) =>
    word.(w+1) = entry.e => forge(word,w+2,W,entry,e+2,E,QSADD1 n)
    forge(word,w+1,W,entry,e+2,E,QSADD1 n)
  word.(w+1)=entry.e => forge(word,w+2,W,entry,e+1,E,QSADD1 n)
 
  (deltaW := W-w) > 1 and (deltaE := E-e) > 1 =>
    --if word is long, can we delete chars to match 2 consective chars
    deltaW >= deltaE and
      (k := or/[j for j in (w+2)..(W-1) | word.j = entry.e])
        and word.(k+1) = entry.(e+1) =>
          forge(word,k+2,W,entry,e+2,E,QSPLUS(k-w,n))
    deltaW <= deltaE and
    --if word is short, can we insert chars so as to match 2 consecutive chars
      (k := or/[j for j in (e+2)..(E-1) | word.w = entry.j])
        and word.(w+1) = entry.(k+1) =>
          forge(word,w+2,W,entry,k+2,E,QSPLUS(n,k-e))
    forge(word,w+1,W,entry,e+1,E,QSADD1 n)
  --check for two consecutive matches down the line
  forge(word,w+1,W,entry,e+1,E,QSADD1 n)
 
--+ DO NOT REMOVE DEFINITIONS BELOW which explain the algorithm
--+ canForgeWord(word,entry) ==--
--+ [d,i,s,t] := forge(word,0,MAXINDEX word,entry,0,MAXINDEX entry,0,0,0,0)
--+ --d=deletions, i=insertions, s=substitutions, t=transpositions
--+ --list is formed only for tuning purposes-- remove later on
--+ d + i + s + t
 
--+forge(word,w,W,entry,e,E,d,i,s,t) ==
--+  w > W =>
--+    e > E => [d,i,s,t]
--+    [d,E-e+i+1,s,t]
--+  e > E => [W-w+d+1,i,s,t]
--+  word.w = entry.e => forge(word,w+1,W,entry,e+1,E,d,i,s,t)
--+  w=W or e=E => forge(word,w+1,W,entry,e+1,E,d,i,s+1,t)
--+  word.w=entry.(e+1) =>
--+    word.(w+1) = entry.e => forge(word,w+2,W,entry,e+2,E,d,i,s,t+1)
--+    forge(word,w+1,W,entry,e+2,E,d,i+1,s,t)
--+  word.(w+1)=entry.e => forge(word,w+2,W,entry,e+1,E,d+1,i,s,t)
--+
--+  (deltaW := W-w) > 1 and (deltaE := E-e) > 1 =>
--+    --if word is long, can we delete chars to match 2 consective chars
--+    deltaW >= deltaE and
--+      (k := or/[j for j in (w+2)..(W-1) | word.j = entry.e])
--+        and word.(k+1) = entry.(e+1) =>
--+          forge(word,k+2,W,entry,e+2,E,d+k-w,i,s,t)
--+    deltaW <= deltaE and
--+    --if word is short, can we insert chars so as to match 2 consecutive chars
--+      (k := or/[j for j in (e+2)..(E-1) | word.w = entry.j])
--+        and word.(w+1) = entry.(k+1) =>
--+          forge(word,w+2,W,entry,k+2,E,d,i+k-e,s,t)
--+    forge(word,w+1,W,entry,e+1,E,d,i,s+1,t)
--+  --check for two consecutive matches down the line
--+  forge(word,w+1,W,entry,e+1,E,d,i,s+1,t)
 
--=======================================================================
--                          String Pattern Matching
--=======================================================================
patternTran pattern ==
  null hasWildCard? pattern and LITER pattern.0 and
    UPCASE copy pattern = pattern =>
      name:= abbreviation? INTERN pattern
        or browseError [:bright pattern,
          '"is not a constructor abbreviation"]
      DOWNCASE PNAME name
  maskConvert DOWNCASE pattern
 
hasWildCard? str ==
  or/[str.i = '_? and (i=0 or not(str.(i-1) = '__ )) for i in 0..MAXINDEX str]
 
maskConvert str ==
--replace all ? not preceded by an underscore by &
  buf:= GETSTR(#str)
  j:= 0  --index into res
  final := MAXINDEX str
  for i in 0..final repeat
    char := str.i
    if char = '__ and i < final then
      i:= i+1
      char := str.i
     else if char = '_? then char := '_&
    SUFFIX(char,buf)
  buf
 
 
infix?(s,t,x) == #s + #t >= #x and prefix?(s,x) and suffix?(t,x)
 
prefix?(s,t) == substring?(s,t,0)
 
suffix?(s,t) ==
  m := #s; n := #t
  if m > n then return false
  substring?(s,t,(n-m))
 
obSearch x ==
  vec:= OBARRAY()
  pattern:= PNAME x
  [y for i in 0..MAXINDEX OBARRAY() |
    (IDENTP (y := vec.i) or CVEC y) and match?(pattern,COPY y)]
 
