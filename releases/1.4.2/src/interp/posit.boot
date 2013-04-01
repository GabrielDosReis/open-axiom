-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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
import astr
namespace BOOT
module posit where
  %Position <=> %List %Form
  tokType: %List %Form -> %Symbol
  tokPart: %List %Form -> %Thing
  tokPosn: %List %Form -> %Position

$nopos ==
  ['noposition]

poNoPosition()    == $nopos
pfNoPosition() == poNoPosition()
 
poNoPosition? pos == pos is ["noposition",:.]
pfNoPosition? pos == poNoPosition? pos
 
pfSourceText pf ==
    lnString poGetLineObject pfPosn pf
 
pfPosOrNopos pf ==
    poIsPos? (pos := pfSourcePosition pf) => pos
    poNoPosition()
 
poIsPos? pos ==
    cons? pos and cons? first pos and #first pos = 5
 
lnCreate(extBl, st, gNo, :optFileStuff) ==
    lNo :=
        num := IFCAR optFileStuff => num
        0
    fN  := IFCAR IFCDR optFileStuff
    [extBl, st, gNo, lNo, fN]
 
lnString lineObject ==
    lineObject.1
 
lnExtraBlanks lineObject ==
    lineObject.0
 
lnGlobalNum lineObject   ==
    lineObject.2
 
lnSetGlobalNum(lineObject, num) ==
    lineObject.2 := num
 
lnLocalNum lineObject    ==
    lineObject.3
 
lnFileName lineObject    ==
    (fN := lnFileName? lineObject)  => fN
    ncBug('"there is no file name in %1", [lineObject] )
 
lnFileName? lineObject   ==
    (fN := lineObject.4) isnt [.,:.]  => nil
    fN
 
lnPlaceOfOrigin lineObject ==
    lineObject.4
 
lnImmediate? lineObject  ==
    not lnFileName? lineObject
 
poGetLineObject posn ==
    first posn

pfGetLineObject posn == poGetLineObject posn
 
pfSourceStok x==
       if pfLeaf? x
       then x
       else if null pfParts x
            then 'NoToken
            else pfSourceStok pfFirst x
 
pfSourceToken form ==
    if pfLeaf? form
    then pfLeafToken form
    else if null pfParts form
         then 'NoToken
         else pfSourceToken(pfFirst form)

--constructer and selectors for leaf tokens

tokConstruct(hd,tok,:pos)==
         a := [hd,:tok]
         IFCAR pos =>
             pfNoPosition? first pos=> a
             ncPutQ(a,"posn",first pos)
             a
         a

tokType x== ncTag x

tokPart x== rest x

tokPosn x==
     a := objectAssoc("posn",ncAlist x)
     if a then rest a else pfNoPosition()

pfAbSynOp form ==
    hd := first form
    IFCAR hd or hd

pfAbSynOp?(form, op) ==
    hd := first form
    sameObject?(hd, op) or hd is [=op,:.]

pfLeaf? form ==
  pfAbSynOp form in
       '(id idsy symbol string char float expression integer
          Document error)

pfLeaf(x,y,:z)      == tokConstruct(x,y, IFCAR z or pfNoPosition())
pfLeafToken form    == tokPart form
pfLeafPosition form == tokPosn form

pfTree(x,y)   == [x,:y]
pfParts  form == rest form
pfFirst  form == second form
pfSecond form == third form

pfPosn pf == pfSourcePosition pf
 
pfSourcePosition form ==
    --null form => pfNoPosition()
    pfLeaf? form => pfLeafPosition form
    parts := pfParts form
    pos := $nopos
    for p in parts while poNoPosition? pos repeat
        pos := pfSourcePosition p
    pos
 
pfSourcePositions form ==
    if pfLeaf? form
    then
     a:=tokPosn form
     if null a
     then nil
     else [a]
    else  pfSourcePositionlist pfParts form
 
pfSourcePositionlist x==
      if null x
      then nil
      else append(pfSourcePositions first x,pfSourcePositionlist rest x)
 
 
poCharPosn posn == rest posn

pfCharPosn posn == poCharPosn posn
 
poLinePosn posn       ==
    posn => lnLocalNum  poGetLineObject posn  --vector? posn =>
    CDAR posn

pfLinePosn posn == poLinePosn posn
 
poGlobalLinePosn posn ==
    posn => lnGlobalNum poGetLineObject posn
    ncBug('"old style pos objects have no global positions",[])

pfGlobalLinePosn posn == poGlobalLinePosn posn
 
poFileName posn       ==
    posn => lnFileName poGetLineObject posn
    CAAR posn

pfFileName posn == poFileName posn
 
poFileName? posn       ==
    posn = ['noposition] => nil
    posn => lnFileName? poGetLineObject posn
    CAAR posn

pfFileName? posn == poFileName? posn
 
poPlaceOfOrigin posn ==
    lnPlaceOfOrigin poGetLineObject posn

pfPlaceOfOrigin posn == poPlaceOfOrigin posn
 
poNopos? posn ==
    posn = ['noposition]

pfNopos? posn == poNopos? posn

poPosImmediate? txp==
    poNopos? txp => nil
    lnImmediate? poGetLineObject txp

pfPosImmediate? txp == poPosImmediate? txp
 
poImmediate? txp==
    lnImmediate? poGetLineObject txp

pfImmediate? txp == poImmediate? txp
 
 
compareposns(a,b)==
 c:=poGlobalLinePosn a
 d:=poGlobalLinePosn b
 if c=d then poCharPosn a>=poCharPosn b else c>=d

pfPrintSrcLines(pf) ==
  lines := pfSourcePositions pf
  lno := 0
  for l in lines repeat
    line := first l
    if lno < lnGlobalNum(line) then 
      formatToStdout('"   ~A~%",  lnString line)
      lno := lnGlobalNum(line)

