-- Copyright (C) 2007-2013 Gabriel Dos Reis.
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
--

-- This file defines some utility functions common to both the compiler
-- and interpreter.

import sys_-constants
import vmlisp
import hash
namespace BOOT

module sys_-utility where
  probeReadableFile : %String -> %Maybe %String
  remove!: (%List %Thing,%Thing) -> %List %Thing
  displayTextFile: %Thing -> %Void
  upwardCut: (%Thing, %List %Thing) -> %List %Thing
  symbolPosition: (%Symbol,%List %Symbol) -> %Maybe %Short
  valuePosition: (%Thing,%List %Thing) -> %Maybe %Short
  symbolTarget: (%Symbol,%List %Thing) -> %Maybe %Thing
  scalarAssoc: (%Thing,%List %Thing) -> %Maybe %Pair(%Thing,%Thing)
  scalarTarget: (%Thing,%List %Thing) -> %Maybe %Thing

--%
$COMBLOCKLIST := nil

++ Constants describing byte order
%littleEndian == KEYWORD::%littleEndian
%bigEndian == KEYWORD::%bigEndian
%unknownEndian == KEYWORD::%unknownEndian

++ The byte order of the host machine running OpenAxiom.
%hostByteOrder() ==
  getHostByteOrder() = 1 => %littleEndian
  getHostByteOrder() = 2 => %bigEndian
  %unknownEndian

--%

++ getVMType returns an approximation of the underlying object type
++ representation of a domain, as a Lisp type specifier as seen by
++ the runtime system.
getVMType d ==
  ident? d => 
    d is "*" => d
    "%Thing"
  string? d => "%Thing"            -- literal flag parameter
  case (d' := devaluate d) of
    Void => "%Void"
    Identifier => "%Symbol"
    Boolean => "%Boolean"
    Byte => "%Byte"
    Character => "%Char"
    SingleInteger => "%Short"
    Integer => "%Integer"
    NonNegativeInteger => ["%IntegerSection",0]
    PositiveInteger => ["%IntegerSection",1]
    IntegerMod => "%Integer"
    DoubleFloat => "%DoubleFloat"
    String => "%String"
    List => ["%List",getVMType second d']
    Vector => ["%Vector",getVMType second d']
    PrimitiveArray => ["%SimpleArray", getVMType second d']
    Pair => ["%Pair",getVMType second d',getVMType third d']
    Union => ["%Pair",'%Short,'%Thing]
    Record =>
      #rest d' > 2 => "%Shell"
      ["%Pair",'%Thing,'%Thing]
    IndexedList => ["%List", getVMType second d']
    Int8 => ["SIGNED-BYTE", 8]
    Int16 => ["SIGNED-BYTE", 16]
    Int32 => ["SIGNED-BYTE", 32]
    UInt8 => ["UNSIGNED-BYTE", 8]
    UInt16 => ["UNSIGNED-BYTE", 16]
    UInt32 => ["UNSIGNED-BYTE", 32]
    otherwise => "%Thing"                 -- good enough, for now.

--%

++ returns true if `f' is bound to a macro.
macrop: %Thing -> %Boolean
macrop f ==
  ident? f and not null MACRO_-FUNCTION f

++ returns true if `f' is bound to a function
functionp: %Thing -> %Boolean
functionp f ==
  ident? f => functionSymbol? f and null MACRO_-FUNCTION f
  function? f

++ returns true if `x' is contained in `y'.
CONTAINED: (%Thing,%Thing) -> %Boolean
CONTAINED(x,y) == main where
  main() ==
    symbol? x => eq(x,y)
    equal(x,y)
  eq(x,y) ==
    cons? y => eq(x, first y) or eq(x, rest y)
    symbolEq?(x,y)
  equal(x,y) ==
    y isnt [.,:.] => x = y
    equal(x, first y) or equal(x, rest y)

++ Returns all the keys of association list `x'
-- ??? Should not this be named `alistAllKeys'?
ASSOCLEFT: %Thing -> %Thing
ASSOCLEFT x ==
  x isnt [.,:.] => x
  [first p for p in x]

++ Returns all the datums of association list `x'.
-- ??? Should not this be named `alistAllValues'?
ASSOCRIGHT: %Thing -> %Thing
ASSOCRIGHT x ==
  x isnt [.,:.] => x
  [rest p for p in x]

++ Put the association list pair `(x . y)' into `l', erasing any 
++ previous association for `x'.
ADDASSOC: (%Thing,%Thing,%Alist(%Thing,%Thing)) -> %Alist(%Thing,%Thing)
ADDASSOC(x,y,l) ==
  l isnt [.,:.] => [[x,:y],:l]
  x = first first l => [[x,:y],:rest l]
  [first l,:ADDASSOC(x,y,rest l)]

++ Remove any assocation pair `(u . x)' from list `v'.
DELLASOS: (%Thing,%Alist(%Thing,%Thing)) -> %Alist(%Thing,%Thing)
DELLASOS(u,v) ==
  v isnt [.,:.] => nil
  u = first first v => rest v
  [first v,:DELLASOS(u,rest v)]


++ Return the datum associated with key `x' in association list `y'.
-- ??? Should not this be named `alistValue'?
LASSOC: (%Thing,%Alist(%Thing,%Thing)) -> %Thing
LASSOC(x,y) ==
  y isnt [.,:.] => nil
  x = first first y => rest first y
  LASSOC(x,rest y)

++ Return the key associated with datum `x' in association list `y'.
rassoc: (%Thing,%Alist(%Thing,%Thing)) -> %Thing
rassoc(x,y) ==
  y isnt [.,:.] => nil
  x = rest first y => first first y
  rassoc(x,rest y)

++ Reclaim unreachable objects.
RECLAIM() ==
)if %hasFeature KEYWORD::GCL
  SI::GBC true
)elseif %hasFeature KEYWORD::SBCL
  SB_-EXT::GC()
)elseif %hasFeature KEYWORD::CLISP
  EXT::GC()
)else
  nil
)endif

++
makeAbsoluteFilename: %String -> %String
makeAbsoluteFilename name ==
  strconc(systemRootDirectory(),name)

++ returns true if `file' exists as a pathname.
existingFile? file ==
  PROBE_-FILE file => true
  false

probeReadableFile file ==
  readablep file > 0 => file
  nil

++
$REPLACE(filespec1,filespec2) ==
  filespec1 := makeFullFilePath filespec1
  removeFile filespec1
  renameFile(makeFullFilePath filespec2, filespec1)

++
checkMkdir path ==
  mkdir path = 0 => true
  systemError ['"cannot create directory",:bright path]

++ return the pathname to the system module designated by `m'.
getSystemModulePath m ==
  d := systemAlgebraDirectory() => strconc(d,m,'".",$faslType)
  strconc(systemRootDirectory(),'"algebra/",m,'".",$faslType)

++ load module in `path' that supposedly will define the function 
++ indicated by `name'.
loadModule: (%String,%Symbol) -> %Thing
loadModule(path,name) ==
  FMAKUNBOUND name
  LOAD path

--% numerics
log10 x ==
  LOG(x,10)

bitand: (%Short,%Short) -> %Short
bitand(x,y) ==
  BOOLE(BOOLE_-AND,x,y)

bitior: (%Short,%Short) -> %Short
bitior(x,y) ==
  BOOLE(BOOLE_-IOR,x,y)

--% Hash table

hashTable cmp ==
  testFun :=
    cmp in '(ID EQ) => function sameObject?
    cmp = 'EQL => function scalarEq?
    cmp = 'EQUAL => function EQUAL
    error '"bad arg to hashTable"
  MAKE_-HASH_-TABLE(test <- testFun)

--% Trees to Graphs

minimalise x ==
  min(x,hashTable 'EQUAL) where
    min(x,ht) ==
      y := tableValue(ht,x)
      y => y
      cons? x =>
        z := min(first x,ht)
        if not sameObject?(z,first x) then x.first := z
        z := min(rest x,ht)
        if not sameObject?(z,rest x) then x.rest := z
        hashCheck(x,ht)
      vector? x =>
        for i in 0..maxIndex x repeat
          x.i := min(x.i,ht)
        hashCheck(x,ht)
      string? x => hashCheck(x,ht)
      x
    hashCheck(x,ht) ==
      y := tableValue(ht,x)
      y => y
      tableValue(ht,x) := x
      x

--% File IO
$InputIOMode == KEYWORD::INPUT
$OutputIOMode == KEYWORD::OUTPUT
$BothWaysIOMode == KEYWORD::IO
$ClosedIOMode == KEYWORD::CLOSED

++ return a binary stream open for `file' in mode `mode'; nil
++ if something went wrong.  This function is used by the Algebra.
openBinaryFile(file,mode) ==
  mode = $InputIOMode =>
    OPEN(file,direction <- mode,
      if_-does_-not_-exist <- nil,element_-type <- "%Byte")
  OPEN(file,direction <- mode,
    if_-exists <- KEYWORD::SUPERSEDE,element_-type <- "%Byte")

++ Write byte `b' to output binary file `ofile'.
writeByteToFile(ofile,b) ==
  writeByte(b,ofile)

--%
stringImage x ==
  symbol? x => symbolName x
  string? x => strconc('"_"",x,'"_"")
  toString x

--% Socket I/O

++ Attempt to establish a client TCP/IP socket connection.  The IP numeric
++ address is specified by the first argument; second argument is the
++ version of IP used (4 or 6); third argument is the desired port.
++ Return %nothing on failure, otherwise the file descriptor corresponding
++ to the obtained client socket.
connectToHostAndPort(addr,prot,port) ==
  (socket := doConnectToHostAndPort(addr,prot,port)) < 0 => %nothing
  socket

++ Attempt to read a byte from the socket `s'.  If unsuccessful,
++ return %nothing.
readByteFromStreamSocket s ==
  (byte := doReadByteFromStreamSocket s) < 0 => %nothing
  COERCE(byte,"%Byte")

writeByteToStreamSocket(s,b) ==
  (byte := doWriteByteToStreamSocket(s,b)) < 0 => %nothing
  COERCE(byte,"%Byte")

--%
makeByteBuffer(n,b == 0) ==
  MAKE_-ARRAY(n,element_-type <- "%Byte",initial_-element <- b)

++ Return the position of the symbol `s' in the list `l', if present.
++ Otherwise return nil.
symbolPosition(s,l) ==
  or/[i for i in 0.. for x in l | symbolEq?(s,x)]

valuePosition(s,l) ==
  or/[i for i in 0.. for x in l | valueEq?(s,x)]

--% assoc

scalarAssoc(c,l) ==
  or/[scalarEq?(c,first x) and leave x for x in l | cons? x] or nil

stringAssoc(s,l) ==
  or/[stringEq?(s,first x) and leave x for x in l | cons? x] or nil

--% lassoc

symbolTarget(s,l) ==
  p := symbolAssoc(s,l) => rest p
  nil

scalarTarget(s,l) ==
  p := scalarAssoc(s,l) => rest p
  nil

--%
remove!(l,x) ==
  l = nil => nil
  valueEq?(first l,x) => rest l
  p := l
  repeat
    p isnt [.,.,:.] => return l
    valueEq?(second p,x) =>
      p.rest := p.rest.rest
      return l
    p := rest p
      
sortBy(k,l) ==
  SORT(copyList l,function GGREATERP,key <- k)

++ Return the list of objects that follow x in l, including x itself.
++ Otherwise return nil.
upwardCut(x,l) ==
  repeat
    l isnt [.,:.] => return nil
    sameObject?(x,first l) => return l
    l := rest l

--%
displayTextFile f ==
  try
    stream := inputTextFile f
    while (line := readLine stream) ~= %nothing repeat
      writeLine(line,$OutputStream)
  finally
    stream ~= nil => closeStream stream

--%
macro last x ==
  lastNode(x).first

--%
macro loopBody x ==
  take(-2,x).first

--%
macro constructorDB ctor ==
  property(ctor,'DATABASE)
  
--%
structure %Libstream ==
  Record(mode: %IOMode, dir: %Pathname,tbl: %Thing, idxst: %Stream,
    cdst: %Stream,insnst: %Stream,tmpdir: %Pathname) with
      libIOMode == (.mode)
      libDirname == (.dir)
      libIndexTable == (.tbl)
      libIndexStream == (.idxst)
      libCodeStream == (.cdst)
      libInsnStream == (.insnst)
      libStationaryDirname == (.tmpdir)

makeLibstream(m,p,idx==nil,st==nil) ==
  mk%Libstream(m,p,idx,st,nil,nil,nil)

makeFilename(filearg,filetype==nil) ==
  if ident? filetype then
    filetype := symbolName filetype
  filePath? filearg => filePathString
    filePathType filearg ~= nil => filearg
    makeFilePath(directory <- filePathDirectory filearg,
      name <- filePathName filearg, type <- filetype)
  string? filearg and filePathType filearg ~= nil and filetype = nil => filearg
  string? filearg and string? filetype and filePathType filearg ~= nil
    and stringEq?(filePathType filearg,filetype) => filearg
  filearg is [.,:.] =>
    makeFilename(first filearg,second filearg or filetype)
  if string? filetype then
    filetype := makeSymbol filetype
  ft := rest symbolAssoc(filetype,$FILETYPE_-TABLE) or filetype
  ft = nil =>
    string? filearg => filearg
    ident? filearg => symbolName filearg
    toString filearg
  strconc(toString filearg,'".",toString ft)

makeFullFilePath(filearg,filetype==nil) ==
  filePathString mergeFilePaths makeFilename(filearg,filetype)
  
getDirectoryList ft ==
  here := getWorkingDirectory()
  ft in '("NRLIB" "DAASE" "EXPOSED") =>
    $UserLevel = 'development => [here,:$LIBRARY_-DIRECTORY_-LIST]
    $LIBRARY_-DIRECTORY_-LIST
  home := filePathString userHomeDirectory()
  dirs := 
    stringMember?(home,$DIRECTORY_-LIST) => $DIRECTORY_-LIST
    [home,:$DIRECTORY_-LIST]
  stringMember?(here,dirs) => dirs
  [here,:dirs]

makeInputFilename(filearg,filetype == nil) ==
  filename := makeFilename(filearg,filetype)
  dirname := filePathDirectory filename
  dirname = nil or dirname is [KEYWORD::RELATIVE,:.] =>
    or/[probeReadableFile strconc(dir,filename)
         for dir in getDirectoryList filePathType filename]
           or probeReadableFile filename
  probeReadableFile filename

findFile(spec,exts) ==
  name :=
    spec is [.,:.] => first spec
    spec
  if spec is [.,:.] then
    exts := [second spec,:exts]
  or/[makeInputFilename(name,ext) for ext in exts]
