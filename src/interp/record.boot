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


--                         Usage

-- )bo inputFile2RecordFile('"<idir>fn.input",'"<odir>a.b")
--   converts input file "fn" to a record file stored at "<odir>fn.record".
--   If you give one argument, <idir> is used for <odir>

-- )bo htFile2RecordFile('"<idir>fn.ht",'"<odir>a.b")
--    converts HT file "fn" to a record file stored at "<odir>fn.record".
--    If you give one argument, record file goes to "<idir>fn.record".
--    A file "<odir>fn.input" is produced as a side-effect.

-- )bo htFile2InputFile('"<idir>fn.input",'"<odir>a.b")
--    converts input file "fn" to an input file stored at "<odir>fn.input"

-- )bo printRecordFile('"<idir>fn.record") to display results recorded

-- )bo verifyRecordFile('"<idir>fn.record") to verfiy that same output
--    results from running original fn.input file


import nlib
import pathname
namespace BOOT

--=======================================================================
--                      Global Variables
--=======================================================================
$backslash := char "\"
$testOutputLineFlag := nil   -- referenced by charyTop, prnd to stash lines
$testOutputLineStack := nil  -- saves lines to be printed (needed to convert
                             -- lines for use in hypertex)
$runTestFlag := nil          -- referenced by maPrin to stash output
                             -- by recordAndPrint to not print type/time
$mkTestFlag := nil           -- referenced by READLN to stash input
                             -- by maPrin to stash output
                             -- by recordAndPrint to write i/o onto $testStream
$mkTestInputStack := nil     -- saves input for $testStream (see READLN)
$mkTestOutputStack := nil    -- saves output for $testStream (see maPrin)
$mkTestOutputType := nil     -- saves the type for $testStream
 
--=======================================================================
--                Function for Creating a `record' file
--=======================================================================
inputFile2RecordFile(pathname,:option) ==
  ifn := filePathName pathname
  not isExistingFile pathname => throwKeyedMsg("S2IL0003",[namestring ifn])
  opath := KAR option or pathname
  odirect := pathnameDirectory opath
  opathname := htMkPath(odirect,ifn,'"rec")
  SETQ(_*PRINT_-ARRAY_*, true)
  $mkTestFlag: local := true
  $runTestFlag: local := false
  $mkTestInputStack: local := nil
  $mkTestOutputStack: local := nil
  $mkTestOutputType: local := nil
  $currentLine: local := nil
  removeFile opathname
  $testStream := outputTextFile opathname
  CATCH($SpadReaderTag,_/READ(pathname,nil))
  --for trailing system commands
  if not null $currentLine then recordAndPrintTest '(ForSystemCommands)
  closeFile $testStream
  opathname 
--=======================================================================
--                Function for Displaying a `record' file
--=======================================================================
printRecordFile(pathname,:option) ==
  $LINELENGTH : local := KAR option or 76
  $printTimeIfTrue: local := nil
  $printTypeIfTrue: local := true
  stream := DEFIOSTREAM([['FILE,:pathname], '(MODE . INPUT)],80,0)
  repeat
    null (PEEK_-CHAR ( true, stream , nil, nil )) => return nil
    [i,t,:o] := dewritify VMREAD stream
    sayNewLine()
    for x in i repeat sayBrightly x
    sayNewLine()
    for x in o repeat maPrin x
    if t ~= $Void then printTypeAndTime(nil,t)
 
testPrin(u,w) == --same as maPrin but lines are stored in $testOutputLineList
                 --these lines are needed for pasting into HT files
  $LINELENGTH: local := w
  $mkTestFlag: local := nil
  $testOutputLineFlag: local := true
  $testOutputLineList: local := nil
  maPrin copyTree u
  res := reverse $testOutputLineList
  for x in res repeat sayBrightly x
  res
 
--=======================================================================
--     Function for converting a maPrin expression to HyperTeX format
--=======================================================================
hyperize(u,w) ==
  $LINELENGTH: local := w
  $mkTestFlag: local := nil
  $testOutputLineFlag: local := true
  $testOutputLineList: local := nil
  maPrin copyTree u
  res := reverse $testOutputLineList
  null res => '""
  null rest res => first res
  strconc/[first res,:[strconc('"\newline ",x) for x in rest res]]
 
verbatimize u ==
  u = '"" => u
  strconc('"\begin{verbatim}",u,'"\end{verbatim}")
--=======================================================================
--                Function for Verifying a `record' file
--=======================================================================
verifyRecordFile(pathname) ==
  ifn := filePathName pathname
  sayBrightly ['"Verifying",:bright ifn]
  not isExistingFile pathname => throwKeyedMsg("S2IL0003",[namestring ifn])
  stream := MAKE_-INSTREAM pathname
  clearCmdAll()
  result := 'ok
  for j in 1.. repeat
    null (PEEK_-CHAR ( true, stream ,nil,nil ))=>return nil
    [i,t,:o] := dewritify VMREAD stream
    null i => return nil
    t = 'ForSystemCommands => 
      return testInput2Output(i,nil)  
        --read trailing system commands
    [typ,:output] := testInput2Output(i,j)
    typ = t =>
      output = o => 'ok
      result := 'error
      wasIs(o,output)
    result := 'error
    wasIs(o,output,t,typ)
  suffix := (result = 'ok => '"is ok"; '"has errors")
  sayBrightly [:bright ifn,suffix]

testInput2Output(lines,n) ==
  $mkTestOutputStack: local := nil
  $mkTestOutputType: local := nil
  $mkTestFlag: local := nil
  $runTestFlag: local := true
  $testOutput: local := nil
  evaluateLines lines
  null n => nil     --return from reading trailing system commands
  typ := $mkTestOutputType
  output := reverse! $mkTestOutputStack
  [prefix2String typ,:output]

evaluateLines lines ==
  file := outputTextFile '"/tmp/temp.input"
  for line in lines repeat
    stringPrefix?('")r",line) => 'skip
    stringPrefix?('")undo )redo",line) => 'skip
    writeLine(line, file)
  closeFile file
  $editFile: local := '"/tmp/temp.input"
  _/RF()
    -- can't use $editFile since it might be reset
  removeFile '"/tmp/temp.input"


wasIs(old,new,:typePart) ==
  sayBrightly '"*************************************************************"
  if old ~= new then 
    sayBrightly '"Was ----------> "
    for x in old repeat maPrin x
    sayBrightly '"Is -----------> "
    for x in new repeat maPrin x
  typePart is [oldtype,newtype] and oldtype ~= newtype =>
    sayBrightlyNT ['" Type was ---> ",oldtype]
    pp old
    sayBrightlyNT ['" Type is  ---> ",newtype]
    pp new

--=======================================================================
--              Creating Input Files from HT Files
--=======================================================================
htFile2InputFile(pathname,:option) ==
  ifn := pathnameName pathname
  not isExistingFile pathname => throwKeyedMsg("S2IL0003",[namestring ifn])
  opath := KAR option or pathname
  odirect := pathnameDirectory opath
  opathname := htMkPath(odirect,ifn,'"input")
  removeFile opathname
  $htStream : local := MAKE_-INSTREAM pathname
  alist := [[htGetPageName u,:htGetSpadCommands()] 
              while (u := htExampleFind '"\begin{page}")]
  closeFile $htStream
  outStream := MAKE_-OUTSTREAM opathname
  for [pageName,:commands] in alist repeat
    writeString('"-- ",outStream)
    PRINC(pageName,outStream)
    writeNewline outStream 
    writeLine('")cl all",outStream)
    for x in commands repeat 
      PRINC(htCommandToInputLine x,outStream)
      writeNewline outStream
    writeNewline outStream
  closeFile outStream
  opathname
 
htCommandToInputLine s == fn(s,0) where fn(s,init) ==
--similar to htTrimAtBackSlash except removes all \
  k := or/[i for i in init..maxIndex s | stringChar(s,i) = char "\"] =>
    member(s.(k + 1),[char "f",char "b"]) => subString(s,init,k - init)
    strconc(subString(s,init,k - init),fn(s,k + 1))
  subString(s,init)
 
htTrimAtBackSlash s ==
  backslash := char "\"
  k := or/[i for i in 0..maxIndex s | stringChar(s,i) = backslash 
          and member(s.(i + 1),[char "f",char "b"])] => subString(s,0,k - 1)
  s
 
htMkPath(directory,name,typ) ==
  nameType := strconc(name,'".",typ)
  null directory => nameType
  strconc(directory,nameType)
      
--=======================================================================
--              Creating Record File from HT Files
--=======================================================================
htFile2RecordFile(pathname,:option) ==
  inputFile2RecordFile htFile2InputFile(pathname,KAR option)

--=======================================================================
--           Function to record and print values into $testStream  
--=======================================================================
recordAndPrintTest md ==  --called by recordAndPrint
  input :=
    string? $currentLine => [$currentLine]
    fn $currentLine where fn x ==
      x is [y,:r] =>     
        stringChar(y,k := maxIndex y) = char "__" => 
          u := fn r
          [strconc(subString(y,0,k),'" ",first u),:rest u]
        [y,:fn r]
      x
  output := reverse! $mkTestOutputStack -- set by maPrin
  PRINT(writify [input,prefix2String md,:output],$testStream)
  $mkTestInputStack := nil
  $mkTestOutputStack := nil
 
 
