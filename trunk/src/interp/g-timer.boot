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


import macros
import g_-util
namespace BOOT

--% Code instrumentation facilities
--  These functions can be used with arbitrary lists of
--  named stats (listofnames) grouped in classes (listofclasses)
--  and with measurement types (property, classproperty).

printTimeIfTrue := false
$printStorageIfTrue := false
 
printNamedStatsByProperty(listofnames, prop) ==
  total := +/[property(name,prop) for [name,:.] in listofnames]
  for [name,:.] in listofnames repeat
    n := property(name, prop)
    strname := STRINGIMAGE name
    strval  := STRINGIMAGE n
    sayBrightly concat(bright strname,
      fillerSpaces(70-#strname-#strval,'"."),bright strval)
  sayBrightly bright fillerSpaces(72,'"-")
  sayBrightly concat(bright '"Total",
    fillerSpaces(65-# STRINGIMAGE total,'"."),bright STRINGIMAGE total)
 
makeLongStatStringByProperty _
 (listofnames, listofclasses, prop, classprop, units, flag) ==
  total := 0
  str := '""
  otherStatTotal := property('other, prop)
  for [name,class,:ab] in listofnames repeat
    name = 'other => 'iterate
    cl := first LASSOC(class,listofclasses)
    n := property(name, prop)
    PUT(cl,classprop, n + property(cl,classprop))
    total := total + n
    if n >= 0.01
      then timestr := normalizeStatAndStringify n
      else
        timestr := '""
        otherStatTotal := otherStatTotal + n
    str := makeStatString(str,timestr,ab,flag)
  otherStatTotal := otherStatTotal
  PUT('other, prop, otherStatTotal)
  if otherStatTotal > 0 then
    str := makeStatString(str,normalizeStatAndStringify otherStatTotal,'O,flag)
    total := total + otherStatTotal
    cl := first symbolLassoc('other,listofnames)
    cl := first LASSOC(cl,listofclasses)
    PUT(cl,classprop, otherStatTotal + property(cl,classprop))
  if flag ~= 'long then
    total := 0
    str := '""
    for [class,name,:ab] in listofclasses repeat
      n := property(name, classprop)
      n = 0.0 => 'iterate
      total := total + n
      timestr := normalizeStatAndStringify n
      str := makeStatString(str,timestr,ab,flag)
  total := strconc(normalizeStatAndStringify total,'" ", units)
  str = '"" =>  total
  strconc(str, '" = ", total)
 
normalizeStatAndStringify t ==
  RNUMP t =>
      t := roundStat t
      t = 0.0 => '"0"
      FORMAT(nil,'"~,2F",t)
  integer? t =>
      K := 1024
      M := K*K
      t > 9*M => strconc(STRINGIMAGE((t + 512*K) quo M), '"M")
      t > 9*K => strconc(STRINGIMAGE((t + 512) quo K),   '"K")
      STRINGIMAGE t
  STRINGIMAGE t
 
significantStat t ==
   RNUMP t => (t > 0.01)
   integer?  t => (t > 100)
   true
 
roundStat t ==
  not RNUMP t => t
  QUOTIENT(FIX (0.5 + t * 1000.0), 1000.0)
 
makeStatString(oldstr,time,abb,flag) ==
  time = '"" => oldstr
  opening := (flag = 'long => '"("; '" (")
  oldstr = '"" => strconc(time,opening,abb,'")")
  strconc(oldstr,'" + ",time,opening,abb,'")")
 
peekTimedName() == IFCAR $timedNameStack
 
popTimedName() ==
  name := IFCAR $timedNameStack
  $timedNameStack := IFCDR $timedNameStack
  name
 
pushTimedName name ==
  PUSH(name,$timedNameStack)
 
--currentlyTimedName() == first $timedNameStack
 
startTimingProcess name ==
  updateTimedName peekTimedName()
  pushTimedName name
  if name = 'load then          statRecordLoadEvent()
 
stopTimingProcess name ==
  (name ~= peekTimedName()) and not $InteractiveMode =>
    keyedSystemError("S2GL0015",[name,peekTimedName()])
  updateTimedName peekTimedName()
  popTimedName()
 
--% Instrumentation specific to the interpreter
$oldElapsedSpace := 0
$oldElapsedGCTime := 0.0
$oldElapsedTime := 0.0
$gcTimeTotal := 0.0
 
-- $timedNameStack is used to hold the names of sections of the
-- code being timed.
 
$timedNameStack := '(other)
 
$interpreterTimedNames == '(
-- name         class abbrev
  (algebra        2 .   B) _
  (analysis       1 .   A) _
  (coercion       1 .   C) _
  (compilation    3 .   T) _
  (debug          3 .   D) _
  (evaluation     2 .   E) _
  (gc             4 .   G) _
  (history        3 .   H) _
  (instantiation  3 .   I) _
  (load           3 .   L) _
  (modemaps       1 .   M) _
  (optimization   3 .   Z) _
  (querycoerce    1 .   Q) _
  (other          3 .   O) _
  (diskread       3 .   K) _
  (print          3 .   P) _
  (resolve        1 .   R) _
  )
 
$interpreterTimedClasses == '(
-- number class name    short name
  ( 1    interpreter     .  IN) _
  ( 2    evaluation      .  EV) _
  ( 3    other           .  OT) _
  ( 4    reclaim         .  GC) _
  )
 
initializeTimedNames(listofnames,listofclasses) ==
  for [name,:.] in listofnames repeat
    PUT(name, 'TimeTotal, 0.0)
    PUT(name, 'SpaceTotal,  0)
  for [.,name,:.] in listofclasses repeat
    PUT( name, 'ClassTimeTotal, 0.0)
    PUT( name, 'ClassSpaceTotal,  0)
  $timedNameStack := '(other)
  computeElapsedTime()
  PUT('gc, 'TimeTotal, 0.0)
  PUT('gc, 'SpaceTotal,  0)
  NIL
 
updateTimedName name ==
  count := (property(name,'TimeTotal) or 0) + computeElapsedTime()
  PUT(name,'TimeTotal, count) 
 
printNamedStats listofnames ==
  printNamedStatsByProperty(listofnames, 'TimeTotal)
  sayBrightly '" "
  sayBrightly '"Space (in bytes):"
  printNamedStatsByProperty(listofnames, 'SpaceTotal)
 
makeLongTimeString(listofnames,listofclasses) ==
  makeLongStatStringByProperty(listofnames, listofclasses,  _
                               'TimeTotal, 'ClassTimeTotal, _
                               '"sec", $printTimeIfTrue)
 
makeLongSpaceString(listofnames,listofclasses) ==
  makeLongStatStringByProperty(listofnames, listofclasses,    _
                               'SpaceTotal, 'ClassSpaceTotal, _
                               '"bytes", $printStorageIfTrue)
 
computeElapsedTime() ==
  -- in total time lists, first is VIRTCPU and second is TOTCPU
  currentTime:= elapsedUserTime()
  currentGCTime:= elapsedGcTime()
  gcDelta := currentGCTime - $oldElapsedGCTime
  elapsedSeconds:=
     1.* (currentTime-$oldElapsedTime-gcDelta)/$timerTicksPerSecond
  PUT('gc, 'TimeTotal,property('gc,'TimeTotal) +
                   1.*QUOTIENT(gcDelta,$timerTicksPerSecond))
  $oldElapsedTime := elapsedUserTime()
  $oldElapsedGCTime := elapsedGcTime()
  elapsedSeconds
 
computeElapsedSpace() ==
  currentElapsedSpace := HEAPELAPSED()
  elapsedBytes := currentElapsedSpace - $oldElapsedSpace
  $oldElapsedSpace := currentElapsedSpace
  elapsedBytes
 
timedAlgebraEvaluation(code) ==
  startTimingProcess 'algebra
  r := eval code
  stopTimingProcess 'algebra
  r
 
timedOptimization(code) ==
  startTimingProcess 'optimization
  $getDomainCode : local := NIL
  r := simplifyVMForm code
  if $reportOptimization then
    sayBrightlyI bright '"Optimized intermediate code:"
    pp r
  stopTimingProcess 'optimization
  r
 
timedEVALFUN(code) ==
  startTimingProcess 'evaluation
  r := timedEvaluate code
  stopTimingProcess 'evaluation
  r
 
timedEvaluate code ==
  code is ['%list,:a] and #a > 200 =>
    "append"/[eval ['%list,:x] for x in splitIntoBlocksOf200 a]
  eval code
 
displayHeapStatsIfWanted() ==
   $printStorageIfTrue => sayBrightly OLDHEAPSTATS()
 
--EVALANDFILEACTQ(
--  PUTGCEXIT function displayHeapStatsIfWanted )
 
--% stubs for the stats summary fns
statRecordInstantiationEvent() == nil
statRecordLoadEvent()          == nil
 
statisticsSummary()  == '"No statistics available."
