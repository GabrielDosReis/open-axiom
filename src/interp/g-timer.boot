-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2016, Gabriel Dos Reis.
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
    strname := symbolName name
    strval  := toString n
    sayBrightly concat(bright strname,
      fillerSpaces(70-#strname-#strval,char "."),bright strval)
  sayBrightly bright fillerSpaces(72,char "-")
  sayBrightly concat(bright '"Total",
    fillerSpaces(65-# toString total,char "."),bright toString total)
 
makeLongStatStringByProperty _
 (listofnames, listofclasses, prop, classprop, units, flag) ==
  total := 0
  str := '""
  otherStatTotal := property('other, prop)
  for [name,class,:ab] in listofnames repeat
    name = 'other => 'iterate
    cl := first LASSOC(class,listofclasses)
    n := property(name, prop)
    property(cl,classprop) := n + property(cl,classprop)
    total := total + n
    if n >= 0.01
      then timestr := normalizeStatAndStringify n
      else
        timestr := '""
        otherStatTotal := otherStatTotal + n
    str := makeStatString(str,timestr,ab,flag)
  otherStatTotal := otherStatTotal
  property('other, prop) := otherStatTotal
  if otherStatTotal > 0 then
    str := makeStatString(str,normalizeStatAndStringify otherStatTotal,'O,flag)
    total := total + otherStatTotal
    cl := first symbolTarget('other,listofnames)
    cl := first LASSOC(cl,listofclasses)
    property(cl,classprop) := otherStatTotal + property(cl,classprop)
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
  float? t =>
      t := roundStat t
      t = 0.0 => '"0"
      formatToString('"~,2F",t)
  integer? t =>
      K := 1024
      M := K*K
      t > 9*M => strconc(toString((t + 512*K) quo M), '"M")
      t > 9*K => strconc(toString((t + 512) quo K),   '"K")
      toString t
  STRINGIMAGE t
 
significantStat t ==
   float? t => (t > 0.01)
   integer?  t => (t > 100)
   true
 
roundStat t ==
  not float? t => t
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
    property(name, 'TimeTotal) := 0.0
    property(name, 'SpaceTotal) := 0
  for [.,name,:.] in listofclasses repeat
    property(name, 'ClassTimeTotal) := 0.0
    property(name, 'ClassSpaceTotal) := 0
  $timedNameStack := '(other)
  computeElapsedTime()
  property('gc, 'TimeTotal) := 0.0
  property('gc, 'SpaceTotal) := 0
  nil
 
updateTimedName name ==
  count := (property(name,'TimeTotal) or 0) + computeElapsedTime()
  property(name,'TimeTotal) := count
 
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
  property('gc, 'TimeTotal) := property('gc,'TimeTotal) +
                   1.*QUOTIENT(gcDelta,$timerTicksPerSecond)
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
  $getDomainCode : local := nil
  r := simplifyVMForm code
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
 
--% stubs for the stats summary fns
statRecordInstantiationEvent() == nil
statRecordLoadEvent()          == nil
 
statisticsSummary()  == '"No statistics available."
