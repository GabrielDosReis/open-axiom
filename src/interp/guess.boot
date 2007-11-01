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


$minThreshold := 3
$maxThreshold := 7
 
--=======================================================================
--                        Build Directories
--=======================================================================
buildOperationWordTable() ==
  $opWordTable := buildWordTable [PNAME x for x in allOperations()]
 
buildWordTable u ==
  table:= MAKE_-HASHTABLE 'ID
  for s in u repeat
    words := wordsOfString s
    key := UPCASE s.0
    HPUT(table,key,[[s,:words],:HGET(table,key)])
  for key in HKEYS table repeat
    HPUT(table,key,
      listSort(function GLESSEQP,removeDupOrderedAlist
        listSort(function GLESSEQP, HGET(table,key),function CAR),
          function CADR))
  table

measureWordTable u ==
  +/[+/[#entry for entry in HGET(u,key)] for key in HKEYS u]

removeDupOrderedAlist u ==
  -- removes duplicate entries in ordered alist
  -- (where duplicates are adjacent)
  for x in tails u repeat
    (y := rest x) and first first x = first first y => RPLACD(x,rest y)
  u
 
wordsOfString(s) == [UPCASE x for x in wordsOfStringKeepCase s]
 
wordsOfStringKeepCase s == wordsOfString1(s,0) or [COPY s]
 
wordsOfString1(s,j) ==
  k := or/[i for i in j..SUB1(MAXINDEX(s)) | UPPER_-CASE_-P s.i] =>
    tailWords:=
      UPPER_-CASE_-P s.(k+1) =>
        n:= or/[i for i in (k+2)..SUB1(MAXINDEX(s))|not UPPER_-CASE_-P s.i]
        null n => [SUBSTRING(s,k,nil)]
        n > k+1 => [SUBSTRING(s,k,n-k-1),:wordsOfString1(s,n-1)]
      m := or/[i for i in (k+2)..SUB1(MAXINDEX(s)) | UPPER_-CASE_-P s.i] =>
        [SUBSTRING(s,k,m-k),:wordsOfString1(s,m)]
      [SUBSTRING(s,k,nil)]
    k > j+1 => [SUBSTRING(s,j,k-j),:tailWords]
    tailWords
  nil

wordKeys s == 
  REMDUP [UPCASE s.0,:fn(s,1,-1,MAXINDEX s,nil)] where fn(s,i,lastKeyIndex,n,acc) ==
    i > n => acc
    UPPER_-CASE_-P s.i =>
--    i = lastKeyIndex + 1 => fn(s,i + 1,i,n,[s.i,:rest acc])
      fn(s,i + 1,i,n,[s.i,:acc])
    fn(s,i + 1,lastKeyIndex,n,acc)

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
findWords(word,table) == 
  $lastWord := word
  $lastTable:= table
  $totalWords:= nil
  $countThreshold := $minThreshold
  $lastMinimum := -1
  res := findApproximateWords(word,table) 
  if null res then
    $countThreshold := $countThreshold + 2
    res := findApproximateWords(word,table) 
  $lastAlist := mySort res =>
--    $lastMinimum := CAR LAST $lastAlist
--    $lastWords := wordSort CDAR $lastAlist
--    $totalWords:= $lastWords
--    $lastAlist := CDR  $lastAlist
--    $totalWords
      $lastMinimum := CAAR $lastAlist
      $lastWords := wordSort CDAR $lastAlist
      $totalWords:= $lastWords
      $lastAlist := CDR  $lastAlist
      $totalWords
  $lastWords := nil

wordSort u == REMDUP listSort(function GLESSEQP,u)

more() == moreWords($lastWord,$lastTable)

moreWords(word,table) ==
  $lastAlist =>
     $lastMinimum := CAR LAST pp $lastAlist
     numberOfLastWords := #$lastWords
     $lastWords := "append"/(ASSOCRIGHT $lastAlist)
     if #$lastWords > numberOfLastWords then 
       trialLastAlist := 
         [p for p in $lastAlist | p.0 < $maxThreshold]
       trialLastWords := "append"/(ASSOCRIGHT trialLastAlist)
       if #trialLastWords > numberOfLastWords then
         $lastWords := trialLastWords         
     $totalWords:= wordSort [:$lastWords,:$totalWords]
     $lastAlist := nil
     $totalWords
  $countThreshold := $countThreshold + 2
  $lastAlist := findApproximateWords(word,table)
  moreWords(word,table)

findApproximateWords(word,table) ==
  count := $countThreshold
  words:= wordsOfString word
  upperWord:= UPCASE COPY word
  n := #words
  threshold:=
    n = 1 => count
    count+1
 
  --first try to break up as list of words
  alist:= nil
  for i in 1..#words repeat
    $penalty :local := (i = 1 => 0; 1)
    wordAlist:= HGET(table,UPCASE (first words).0)
    for [x,:wordList] in wordAlist repeat
      k := findApproxWordList(words,wordList,n,threshold,#wordList) 
      k =>
        k := k + $penalty
        k <= $lastMinimum => 'skip
        alist := consAlist(k,x,alist)
      
    if i = 1 and null alist then
        --no winners, so try flattening to upper case and checking again
        wordSize := SIZE word
        lastThreshold := MAX(threshold - 1,wordSize/2)
        for [x,:.] in wordAlist repeat
          k := deltaWordEntry(upperWord,UPCASE x)
          k < lastThreshold => alist := consAlist(k,x,alist)

    rotateWordList words

  alist

consAlist(x,y,alist) ==
  u := ASSOC(x,alist) => 
    RPLACD(u,[y,:CDR u])
    alist
  [[x,y],:alist]

findApproxWordList(words,wordList,n,threshold,w) ==
  val := findApproxWordList1(words,wordList,n,threshold,w)
  null val => val
--pp [val,:wordList]
  val

findApproxWordList1(words,wordList,n,threshold,w) ==
  two := threshold - 2
  n = w => 
      k := findApproxSimple(words,wordList,threshold) => k 

      n < 3 => false
      threshold := threshold - 1
      sum := 0      --next, throw out one bad word

      badWord := false
      for entry in wordList for part in words while sum < threshold repeat
        k:= deltaWordEntry(part,entry)
        k < two => sum:= sum + k
        null badWord => badWord := true
        sum := 1000
      sum < threshold => 
--      pp [2,sum,wordList]
        sum + 2

  n+1 = w =>    --assume one word is missing
      sum := 0
      badWord := false
      for entries in tails wordList for part in words
           while sum < threshold repeat
        entry := first entries
        k:= deltaWordEntry(part,entry)
        k < two => sum:= sum + k
        null badWord =>
          badWord := true
          entries := rest entries      --skip this bad word
          entry := first entries
          k := deltaWordEntry(part,entry)
          k < two => sum := sum + k
          sum := 1000
        sum := 1000
      sum < threshold => 
--      pp [3,sum,wordList]
        sum + 2
      false
  n-1 = w =>    --assume one word too many
      sum := 0  --here: KEEP it hard to satisfy
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
          return (sum := 1000)
        return (sum := 1000)
      sum < threshold =>
--      pp [4,sum,wordList]
        $penalty = 1 => sum
        sum + 1
      false
  false

findApproxSimple(words,wordList,threshold) ==
  sum := 0
  --first try matching words in order
  for entry in wordList for part in words while sum < threshold repeat
    sum:= sum + deltaWordEntry(part,entry)
  sum < threshold => 
--  pp ['"--->",sum,:wordList]
    sum
  nil
      
rotateWordList u ==
  v := u
  p := CAR v
  while QCDR v repeat
    RPLACA(v,CADR v) 
    v := QCDR v
  RPLACA(v,p)
  u

deltaWordEntry(word,entry) ==
  word = entry => 0
  word.0 ^= entry.0 => 1000
  #word > 2 and stringPrefix?(word,entry) => 1
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
 
mySort u == listSort(function GLESSEQP,u)
