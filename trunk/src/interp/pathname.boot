-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2008, Gabriel Dos Reis.
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


import nlib
)package "BOOT"

-- This file implements the Common Lisp pathname functions for
-- Lisp/VM.  On VM, a filename is 3-list consisting of the filename,
-- filetype and filemode. We also UPCASE everything.
 
-- This file also contains some other VM specific functions for
-- dealing with files.
 
--% Common Lisp Pathname Functions
 
pathname? p == 
  null p or PATHNAMEP p
 
pathname p ==
  null p => p
  PATHNAMEP p => p
  not PAIRP p => PATHNAME p
  if #p>2 then p:=[p.0,p.1]
  PATHNAME APPLY(FUNCTION MAKE_-FILENAME, p)
 
namestring p == 
  null p => nil
  NAMESTRING pathname p
 
pathnameName p == 
  null p => nil
  PATHNAME_-NAME pathname p
 
pathnameType p == 
  null p => nil
  PATHNAME_-TYPE pathname p
 
pathnameTypeId p == 
  null p => nil
  UPCASE object2Identifier pathnameType p
 
pathnameDirectory p ==
   null p => nil
   NAMESTRING MAKE_-PATHNAME(KEYWORD::DIRECTORY,PATHNAME_-DIRECTORY pathname p)
 
isExistingFile f ==
--  p := pathname f
  --member(p,$existingFiles) => true
  if MAKE_-INPUT_-FILENAME f
    then
      --$existingFiles := [p,:$existingFiles]
      true
    else false
 
--% Scratchpad II File Name Functions
 
makePathname(name,type,dir) ==
  -- Common Lisp version of this will have to be written
  -- using MAKE-PATHNAME and the optional args.
  pathname [object2String name,object2String type]
 
mergePathnames(a,b) ==
  (fn := pathnameName(a)) = '"*" => b
  fn ^= pathnameName(b) => a
  (ft := pathnameType(a)) = '"*" => b
  ft ^= pathnameType(b) => a
  (fm := pathnameDirectory(a)) = ['"*"] => b
  a
 
isSystemDirectory dir == 
  EVERY(function CHAR_=,systemRootDirectory(),dir)
 
-- the next function is an improved version of the one in DEBUG LISP
 
_/MKINFILENAM(infile) == CATCH('FILNAM, newMKINFILENAM infile)
 
newMKINFILENAM(infile) ==
  NULL infile => nil
  file := infile := pathname infile
  repeat
    fn := pathnameName file
    nfile := $FINDFILE (file,$sourceFileTypes)
    null nfile =>
      nfile := file
      if fn = '"*" or fn = '"NIL" then sayKeyedMsg("S2IL0016",NIL)
      else              sayKeyedMsg("S2IL0003",[namestring file])
      ans := queryUserKeyedMsg("S2IL0017",NIL)
      if (SIZE(ans) > 0) and ('")" = SUBSTRING(ans,0,1)) then n := 2
      else n := 1
      nfn := UPCASE STRING2ID_-N(ans,n)
      (nfn = 0) or (nfn = 'QUIT) =>
        sayKeyedMsg("S2IL0018",NIL)
        THROW('FILENAM,NIL)
      nfn = 'CREATE => return 'fromThisLoop
      file := pathname ans
    return 'fromThisLoop
  if nfile then pathname nfile
  else NIL
 
 
getFunctionSourceFile fun ==
  null (f := getFunctionSourceFile1 fun) => NIL
  if MAKE_-INPUT_-FILENAME(f) then updateSourceFiles f
  f
 
getFunctionSourceFile1 fun ==
  -- returns NIL or [fn,ft,fm]
  (file := KDR GETL(fun,'DEFLOC)) => pathname file
  null ((fileinfo := FUNLOC fun) or
    (fileinfo := FUNLOC unabbrev fun)) =>
      u := bootFind fun => getFunctionSourceFile1 SETQ($FUNCTION,INTERN u)
      NIL
  3 = #fileinfo =>
    [fn,ft,$FUNCTION] := fileinfo
    newMKINFILENAM pathname [fn,ft]
  [fn,$FUNCTION] := fileinfo
  newMKINFILENAM pathname [fn]
 
updateSourceFiles p ==
  p := pathname p
  p := pathname [pathnameName p, pathnameType p, '"*"]
  if MAKE_-INPUT_-FILENAME p and pathnameTypeId p in '(BOOT LISP META) then
    $sourceFiles := insert(p, $sourceFiles)
  p
