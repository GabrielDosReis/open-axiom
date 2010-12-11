-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2010, Gabriel Dos Reis.
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


import includer
import scanner
import pile
import parser
import ast
namespace BOOTTRAN
module translator

++ If non nil, holds the name of the current module being translated.
$currentModuleName := nil

++ Stack of foreign definitions to cope with CLisp's odd FFI interface.
$foreignsDefsForCLisp := []

genModuleFinalization(stream) ==
  %hasFeature KEYWORD::CLISP =>
    $foreignsDefsForCLisp = nil => nil
    $currentModuleName = nil =>
       coreError '"current module has no name"
    init := 
      ["DEFUN", INTERN strconc($currentModuleName,"InitCLispFFI"), nil,
        ["MAPC",["FUNCTION", "FMAKUNBOUND"],
          ["QUOTE",[second d for d in $foreignsDefsForCLisp]]],
          :[["EVAL",["QUOTE",d]] for d in $foreignsDefsForCLisp]]
    REALLYPRETTYPRINT(init,stream)
  nil

genOptimizeOptions stream ==
  REALLYPRETTYPRINT
    (["PROCLAIM",["QUOTE",["OPTIMIZE",:$LispOptimizeOptions]]],stream)

AxiomCore::%sysInit() ==
  SETQ(_*LOAD_-VERBOSE_*,false)
  if %hasFeature KEYWORD::GCL then
    SETF(SYMBOL_-VALUE
      bfColonColon("COMPILER","*COMPILE-VERBOSE*"),false)
    SETF(SYMBOL_-VALUE 
      bfColonColon("COMPILER","SUPPRESS-COMPILER-WARNINGS*"),false)
    SETF(SYMBOL_-VALUE 
      bfColonColon("COMPILER","SUPPRESS-COMPILER-NOTES*"),true)

++ Make x, the current package
setCurrentPackage: %Thing -> %Thing
setCurrentPackage x ==
  SETQ(_*PACKAGE_*,x)

++ Compiles the input Lisp file designated by lspFileName.
shoeCOMPILE_-FILE: %String -> %Thing
shoeCOMPILE_-FILE lspFileName ==
  COMPILE_-FILE lspFileName

 
BOOTTOCL(fn, out) ==
  UNWIND_-PROTECT(
    PROGN(startCompileDuration(),
      callingPackage := _*PACKAGE_*,
      IN_-PACKAGE '"BOOTTRAN",
      result := BOOTTOCLLINES(nil,fn, out),
      setCurrentPackage callingPackage,
      result),
    endCompileDuration())
 
++ (bootclam "filename") translates the file "filename.boot" to
++ the common lisp file "filename.clisp" , producing, for each function
++ a hash table to store previously computed values indexed by argument
++ list.
BOOTCLAM(fn, out) == 
  $bfClamming := true
  BOOTCLAMLINES(nil,fn, out)
 
BOOTCLAMLINES(lines, fn, out) ==
   BOOTTOCLLINES(lines, fn, out)

BOOTTOCLLINES(lines, fn, outfn)==
   infn:=shoeAddbootIfNec fn
   shoeOpenInputFile(a,infn, shoeClLines(a,fn,lines,outfn))
 
shoeClLines(a,fn,lines,outfn)==
  a=nil => shoeNotFound fn
  $GenVarCounter := 0
  shoeOpenOutputFile(stream,outfn,_
    (genOptimizeOptions stream;
     (for line in lines repeat shoeFileLine(line,stream);
       shoeFileTrees(shoeTransformStream a,stream));
      genModuleFinalization(stream)))
  outfn
 
++ (boottoclc "filename") translates the file "filename.boot" to
++ the common lisp file "filename.clisp" with the original boot
++ code as comments
BOOTTOCLC(fn, out)==
  UNWIND_-PROTECT(
    PROGN(startCompileDuration(),
      callingPackage := _*PACKAGE_*,
      IN_-PACKAGE '"BOOTTRAN",
      result := BOOTTOCLCLINES(nil, fn, out),
      setCurrentPackage callingPackage,
      result),
    endCompileDuration())
 
BOOTTOCLCLINES(lines, fn, outfn)==
  infn:=shoeAddbootIfNec fn
  shoeOpenInputFile(a,infn, shoeClCLines(a,fn,lines,outfn))
  
 
shoeClCLines(a,fn,lines,outfn)==
  a=nil => shoeNotFound fn
  $GenVarCounter := 0
  shoeOpenOutputFile(stream,outfn,
    (genOptimizeOptions stream;
     for line in lines repeat shoeFileLine (line,stream);
      shoeFileTrees(shoeTransformToFile(stream,
	  shoeInclude bAddLineNumber(bRgen a,bIgen 0)),stream);
      genModuleFinalization(stream)))
  outfn
 
++ (boottomc "filename") translates the file "filename.boot"
++ to machine code and loads it one item at a time
BOOTTOMC: %String -> %Thing 
BOOTTOMC fn==
   callingPackage := _*PACKAGE_*
   IN_-PACKAGE '"BOOTTRAN"
   $GenVarCounter  := 0
   infn:=shoeAddbootIfNec fn
   result := shoeOpenInputFile(a,infn,shoeMc(a,fn))
   setCurrentPackage callingPackage
   result
 
shoeMc(a,fn)==
  a=nil => shoeNotFound fn
  shoePCompileTrees shoeTransformStream a
  shoeConsole strconc(fn,'" COMPILED AND LOADED")
 
EVAL_-BOOT_-FILE fn ==
   b := _*PACKAGE_*
   IN_-PACKAGE '"BOOTTRAN"
   infn:=shoeAddbootIfNec fn
   outfn := strconc(shoeRemovebootIfNec fn,'".",_*LISP_-SOURCE_-FILETYPE_*)
   shoeOpenInputFile(a,infn,shoeClLines(a,infn,[],outfn))
   setCurrentPackage b
   LOAD outfn
 
++ (boot "filename") translates the file "filename.boot"
++ and prints the result at the console
BO: %String -> %Thing 
BO fn==
  b := _*PACKAGE_*
  IN_-PACKAGE '"BOOTTRAN"
  $GenVarCounter := 0
  infn:=shoeAddbootIfNec fn
  shoeOpenInputFile(a,infn,shoeToConsole(a,fn))
  setCurrentPackage b
 
BOCLAM fn==
  callingPackage := _*PACKAGE_*
  IN_-PACKAGE '"BOOTTRAN"
  $GenVarCounter := 0
  $bfClamming := true
  infn:=shoeAddbootIfNec fn
  result := shoeOpenInputFile(a,infn,shoeToConsole(a,fn))
  setCurrentPackage callingPackage
  result
 
shoeToConsole(a,fn)==
  a=nil => shoeNotFound fn
  shoeConsoleTrees shoeTransformToConsole
    shoeInclude bAddLineNumber(bRgen a,bIgen 0)

-- (stout "string") translates the string "string"
-- and prints the result at the console

STOUT string == 
  PSTOUT [string]
 
string2BootTree string ==   
  callingPackage := _*PACKAGE_*
  IN_-PACKAGE '"BOOTTRAN"
  $GenVarCounter := 0
  a := shoeTransformString [string]
  result :=
    bStreamNull a => nil
    stripm(first a,callingPackage,FIND_-PACKAGE '"BOOTTRAN")
  setCurrentPackage callingPackage
  result

 
STEVAL string==
   callingPackage := _*PACKAGE_*
   IN_-PACKAGE '"BOOTTRAN"
   $GenVarCounter := 0
   a:=  shoeTransformString [string]
   result := 
      bStreamNull a => nil
      fn:=stripm(first a,_*PACKAGE_*,FIND_-PACKAGE '"BOOTTRAN")
      EVAL fn
   setCurrentPackage callingPackage
   result
 
-- (sttomc "string") translates the string "string"
-- to common lisp, and compiles it.
 
STTOMC string==
   callingPackage := _*PACKAGE_*
   IN_-PACKAGE '"BOOTTRAN"
   $GenVarCounter := 0
   a:=  shoeTransformString [string]
   result := 
      bStreamNull a => nil
      shoePCompile first a
   setCurrentPackage callingPackage
   result
 
 
shoeCompileTrees s==
    while not bStreamNull s repeat
         shoeCompile first s
         s := rest s

shoeCompile: %Ast -> %Thing 
shoeCompile fn==
    fn is ['DEFUN,name,bv,:body] =>
          COMPILE (name,['LAMBDA,bv,:body])
    EVAL fn
 
shoeTransform str==
    bNext(function shoeTreeConstruct,
      bNext(function shoePileInsert,
        bNext(function shoeLineToks, str)))
 
shoeTransformString s==
    shoeTransform shoeInclude bAddLineNumber(s,bIgen 0)

shoeTransformStream s ==
  shoeTransformString bRgen s
--  shoeTransform shoeInclude bAddLineNumber(bRgen s,bIgen 0)
 
shoeTransformToConsole str==
    bNext(function shoeConsoleItem,
      bNext(function shoePileInsert,
        bNext(function shoeLineToks, str)))
 
shoeTransformToFile(fn,str)==
    bFileNext(fn,
      bNext(function shoePileInsert,
        bNext(function shoeLineToks, str)))
 
shoeConsoleItem (str)==
  dq := first str
  shoeConsoleLines shoeDQlines dq
  [shoeParseTrees dq,:rest str]

bFileNext(fn,s) ==
  bDelay(function bFileNext1,[fn,s])
 
bFileNext1(fn,s)==
  bStreamNull s=> ["nullstream"]
  dq := first s
  shoeFileLines(shoeDQlines dq,fn)
  bAppend(shoeParseTrees dq,bFileNext(fn,rest s))
 
shoeParseTrees dq==
  toklist := dqToList dq
  toklist = nil => []
  shoeOutParse toklist
 
shoeTreeConstruct (str)==
  [shoeParseTrees first str, :rest str]
 
shoeDQlines dq==
  a:= CDAAR shoeLastTokPosn dq
  b:= CDAAR shoeFirstTokPosn dq
  streamTake (a-b+1,first shoeFirstTokPosn dq)
 
streamTake(n,s)==
  bStreamNull s => nil
  n=0 => nil
  [first s,:streamTake(n-1, rest s)]
 
shoeFileLines (lines,fn) ==
  shoeFileLine( '" ",fn)
  for line in lines repeat 
    shoeFileLine (shoeAddComment line,fn)
  shoeFileLine ('" ",fn)
 
shoeConsoleLines lines ==
  shoeConsole '" "
  for line in lines repeat
    shoeConsole shoeAddComment line
  shoeConsole '" "
 
shoeFileLine(x, stream) ==
    writeLine(x, stream)
    x
 
shoeFileTrees(s,st)==
  while not bStreamNull s repeat
    a:= first s
    if a is ["+LINE",:.]
    then shoeFileLine(second a,st)
    else 
      REALLYPRETTYPRINT(a,st)
      TERPRI st
    s:= rest s
 
 
shoePPtoFile(x, stream) ==
  SHOENOTPRETTYPRINT(x, stream)
  x
 
shoeConsoleTrees s ==
  while not bStreamPackageNull s repeat
    fn:=stripm(first s,_*PACKAGE_*,FIND_-PACKAGE '"BOOTTRAN")
    REALLYPRETTYPRINT fn
    s:= rest s
 
shoeAddComment l==  
  strconc('"; ", first l)

shoeOutParse stream ==
  $inputStream := stream
  $stack := []
  $stok := nil
  $ttok := nil
  $op :=nil
  $wheredefs := []
  $typings := []
  $returns := []
  $bpCount := 0
  $bpParenCount := 0
  bpFirstTok()
  found := CATCH('TRAPPOINT,bpOutItem()) --try bpOutItem() catch TRAPPOINT
  found = "TRAPPED" => nil
  not bStreamNull $inputStream =>
    bpGeneralErrorHere()
    nil
  $stack = nil =>
    bpGeneralErrorHere()
    nil
  first $stack
 
++ Generate a global signature declaration for symbol `n'.
genDeclaration(n,t) ==
  t is ["%Mapping",valType,argTypes] =>
    if bfTupleP argTypes then argTypes := rest argTypes
    if argTypes ~= nil and symbol? argTypes 
    then argTypes := [argTypes]
    ["DECLAIM",["FTYPE",["FUNCTION",argTypes,valType],n]]
  ["DECLAIM",["TYPE",t,n]]


++ Translate the signature declaration `d' to its Lisp equivalent.
translateSignatureDeclaration d ==
  case d of 
    %Signature(n,t) => genDeclaration(n,t)
    otherwise => coreError '"signature expected"  

++ A non declarative expression `expr' appears at toplevel and its
++ translation needs embeddeding in an `EVAL-WHEN'.
translateToplevelExpression expr ==
  expr' := rest rest shoeCompTran ["LAMBDA",nil,expr]
  -- replace "DECLARE"s with "DECLAIM"s, as the former can't appear
  -- at toplevel.
  for t in expr' repeat
    t is ["DECLARE",:.] =>
      t.first := "DECLAIM"
  expr' :=
    #expr' > 1 => ["PROGN",:expr']
    first expr'
  $InteractiveMode => expr'
  shoeEVALANDFILEACTQ expr'

translateToplevel(b,export?) ==
  atom b => [b]  -- generally happens in interactive mode.
  b is ["TUPLE",:xs] => coreError '"invalid AST"
  case b of
    %Signature(op,t) => [genDeclaration(op,t)]
    %Definition(op,args,body) => rest bfDef(op,args,body)

    %Module(m,ds) =>
      $currentModuleName := m 
      $foreignsDefsForCLisp := nil
      [["PROVIDE", STRING m],
        :[first translateToplevel(d,true) for d in ds]]

    %Import(m) => 
      if getOptionValue "import" ~= '"skip" then
        bootImport STRING m
      [["IMPORT-MODULE", STRING m]]

    %ImportSignature(x, sig) =>
      genImportDeclaration(x, sig)

    %TypeAlias(lhs, rhs) => [genTypeAlias(lhs,rhs)]

    %ConstantDefinition(lhs,rhs) =>
      sig := nil
      if lhs is ["%Signature",n,t] then
        sig := genDeclaration(n,t)
        lhs := n
      $constantIdentifiers := [lhs,:$constantIdentifiers]
      [["DEFCONSTANT",lhs,rhs]]

    %Assignment(lhs,rhs) =>
      sig := nil
      if lhs is ["%Signature",n,t] then
        sig := genDeclaration(n,t)
        lhs := n
      $InteractiveMode => [["SETF",lhs,rhs]]
      [["DEFPARAMETER",lhs,rhs]]

    %Macro(op,args,body) => bfMDef(op,args,body)

    %Structure(t,alts) => [bfCreateDef alt for alt in alts]

    %Namespace n =>
      $activeNamespace := STRING n
      [["IN-PACKAGE",STRING n]]

    %Lisp s => shoeReadLispString(s,0)

    otherwise =>
      [translateToplevelExpression b]


shoeAddbootIfNec s == 
  shoeAddStringIfNec('".boot",s)
 
shoeRemovebootIfNec s == 
  shoeRemoveStringIfNec('".boot",s)

shoeAddStringIfNec(str,s)==
  a:=STRPOS(str,s,0,nil)
  a=nil => strconc(s,str)
  s
 
shoeRemoveStringIfNec(str,s)==
  n := SEARCH(str,s,KEYWORD::FROM_-END,true)
  n = nil => s
  SUBSTRING(s,0,n)
 
-- DEFUSE prints the definitions not used and the words used and
-- not defined in the input file and common lisp.
 
DEFUSE fn==
  infn := strconc(fn,'".boot")
  shoeOpenInputFile(a,infn,shoeDfu(a,fn))
 
--%
$bootDefined := nil
$bootDefinedTwice := nil
$bootUsed := nil
$lispWordTable := nil

shoeDfu(a,fn)==
  a=nil => shoeNotFound fn
  $lispWordTable :=MAKE_-HASHTABLE ("EQ")
  DO_-SYMBOLS(i(FIND_-PACKAGE "LISP"),HPUT($lispWordTable,i,true))
  $bootDefined :=MAKE_-HASHTABLE "EQ"
  $bootUsed :=MAKE_-HASHTABLE "EQ"
  $bootDefinedTwice := nil
  $GenVarCounter := 0
  $bfClamming := false
  shoeDefUse shoeTransformStream a
  out := strconc(fn,'".defuse")
  shoeOpenOutputFile(stream,out,shoeReport stream)
  out
 
shoeReport stream==
  shoeFileLine('"DEFINED and not USED",stream)
  a:=[i for i in HKEYS $bootDefined | not GETHASH(i,$bootUsed)]
  bootOut(SSORT a,stream)
  shoeFileLine('"             ",stream)
  shoeFileLine('"DEFINED TWICE",stream)
  bootOut(SSORT $bootDefinedTwice,stream)
  shoeFileLine('"             ",stream)
  shoeFileLine('"USED and not DEFINED",stream)
  a:=[i for i in HKEYS $bootUsed |
	     not GETHASH(i,$bootDefined)]
  for i in SSORT a repeat
     b := strconc(PNAME i,'" is used in ")
     bootOutLines( SSORT GETHASH(i,$bootUsed),stream,b)
 
shoeDefUse(s)==
  while not bStreamPackageNull s repeat
    defuse([],first s)
    s:=rest s
 
defuse(e,x)==
  x:=stripm(x,_*PACKAGE_*,FIND_-PACKAGE '"BOOTTRAN")
  $used :=nil
  [nee,niens]:=
     x is ['DEFUN,name,bv,:body] => [name,['LAMBDA,bv,:body]]
     x is ['DEFMACRO,name,bv,:body] => [name,['LAMBDA,bv,:body]]
     x is ["EVAL_-WHEN",.,["SETQ",id,exp]]=>[id,exp]
     x is ["SETQ",id,exp]=>[id,exp]
     ["TOP-LEVEL", x]
  if GETHASH(nee,$bootDefined)
  then
     $bootDefinedTwice:=
	    nee="TOP-LEVEL"=> $bootDefinedTwice
	    [nee,:$bootDefinedTwice]
  else HPUT($bootDefined,nee,true)
  defuse1 (e,niens)
  for i in $used repeat
     HPUT($bootUsed,i,[nee,:GETHASH(i,$bootUsed)])
 
defuse1(e,y)==
  atom y =>
      IDENTP y =>
	 $used:=
	      MEMQ(y,e)=>$used
	      MEMQ(y,$used)=>$used
	      defusebuiltin y =>$used
	      UNION([y],$used)
      []
  y is ["LAMBDA",a,:b]=> defuse1 (append(unfluidlist a,e),b)
  y is ["PROG",a,:b]=>
	 [dol,ndol]:=defSeparate a
	 for i in dol repeat
	       HPUT($bootDefined,i,true)
	 defuse1 (append(ndol,e),b)
  y is ["QUOTE",:a] => []
  y is ["+LINE",:a] => []
  for i in y repeat defuse1(e,i)
 
defSeparate x==
  x = nil => [[],[]]
  f := first x
  [x1,x2] := defSeparate rest x
  bfBeginsDollar f => [[f,:x1],x2]
  [x1,[f,:x2]]

unfluidlist x==
  x = nil => []
  atom x => [x]
  x is ["&REST",y]=> [y]
  [first x,:unfluidlist rest x]
 
defusebuiltin x ==  
  GETHASH(x,$lispWordTable)
 
bootOut (l,outfn)==
  for i in l repeat shoeFileLine(strconc ('"   ",PNAME i),outfn)
 
CLESSP(s1,s2)==
  not(SHOEGREATERP(s1,s2))

SSORT l == 
  SORT(l,function CLESSP)
 
bootOutLines(l,outfn,s)==
  l = nil => shoeFileLine(s,outfn)
  a := PNAME first l
  #s + #a > 70 =>
    shoeFileLine(s,outfn)
    bootOutLines(l,outfn,'" ")
  bootOutLines(rest l,outfn,strconc(s,'" ",a))
 
 
-- (xref "fn") produces a cross reference listing in "fn.xref"
-- It contains each name
-- used in "fn.boot", together with a list of functions that use it.
 
XREF fn==
  infn := strconc(fn,'".boot")
  shoeOpenInputFile(a,infn,shoeXref(a,fn))
 
shoeXref(a,fn)==
  a = nil => shoeNotFound fn
  $lispWordTable  :=MAKE_-HASHTABLE ("EQ")
  DO_-SYMBOLS(i(FIND_-PACKAGE "LISP"),HPUT($lispWordTable,i,true))
  $bootDefined  :=MAKE_-HASHTABLE "EQ"
  $bootUsed  :=MAKE_-HASHTABLE "EQ"
  $GenVarCounter  :=0
  $bfClamming :=false
  shoeDefUse shoeTransformStream a
  out := strconc(fn,'".xref")
  shoeOpenOutputFile(stream,out,shoeXReport stream)
  out
 
 
shoeXReport stream==
   shoeFileLine('"USED and where DEFINED",stream)
   c:=SSORT HKEYS $bootUsed
   for i in c repeat
      a := strconc(PNAME i,'" is used in ")
      bootOutLines( SSORT GETHASH(i,$bootUsed),stream,a)
 
FBO (name,fn)== 
  shoeGeneralFC(function BO,name,fn)
 
FEV(name,fn)== 
  shoeGeneralFC(function EVAL_-BOOT_-FILE,name,fn)
 
shoeGeneralFC(f,name,fn)==
   $bfClamming :=false
   $GenVarCounter  := 0
   infn:=shoeAddbootIfNec fn
   a:= shoeOpenInputFile(a,infn,shoeFindName2(fn,name, a))
   filename:= if # name > 8 then SUBSTRING(name,0,8) else name
   a =>  FUNCALL(f, strconc('"/tmp/",filename))
   nil
 
shoeFindName2(fn,name,a)==
  lines:=shoeFindLines(fn,name,a)
  lines =>
    filename:= if # name > 8 then SUBSTRING(name,0,8) else name
    filename := strconc('"/tmp/",filename,'".boot")
    shoeOpenOutputFile(stream, filename,
	 for line in lines repeat shoeFileLine (line,stream))
    true
  false
 
shoeTransform2 str==
    bNext(function shoeItem,
      streamTake(1, bNext(function shoePileInsert,
           bNext(function shoeLineToks, str))))
 
shoeItem (str)==
  dq:=first str
  [[[first line for line in  shoeDQlines dq]],:rest str]
 
stripm (x,pk,bt)==
  atom x =>
    IDENTP x =>
      SYMBOL_-PACKAGE x = bt => INTERN(PNAME x,pk)
      x
    x
  [stripm(first x,pk,bt),:stripm(rest x,pk,bt)]
 
shoePCompile  fn==
    fn:=stripm(fn,_*PACKAGE_*,FIND_-PACKAGE '"BOOTTRAN")
    fn is ['DEFUN,name,bv,:body]=>
          COMPILE (name,['LAMBDA,bv,:body])
    EVAL fn
 
FC(name,fn)==
   $GenVarCounter  := 0
   infn:=shoeAddbootIfNec fn
   shoeOpenInputFile(a,infn,shoeFindName(fn,name, a))
 
shoeFindName(fn,name,a)==
  lines:=shoeFindLines(fn,name,a)
  shoePCompileTrees shoeTransformString lines
 
shoePCompileTrees s==
  while not bStreamNull s repeat
    REALLYPRETTYPRINT shoePCompile first s
    s := rest s
 
bStreamPackageNull s==
  a := _*PACKAGE_*
  IN_-PACKAGE '"BOOTTRAN"
  b:=bStreamNull s
  setCurrentPackage a
  b
 
PSTTOMC string==
  $GenVarCounter := 0
  shoePCompileTrees shoeTransformString string
 
BOOTLOOP() ==
  a:=READ_-LINE()
  #a=0=>
       writeLine '"Boot Loop; to exit type ] "
       BOOTLOOP()
  b:=shoePrefix? ('")console",a)
  b =>
       stream:= _*TERMINAL_-IO_*
       PSTTOMC bRgen stream
       BOOTLOOP()
  a.0='"]".0 => nil
  PSTTOMC [a]
  BOOTLOOP()
 
BOOTPO() ==
  a:=READ_-LINE()
  #a=0=>
       writeLine '"Boot Loop; to exit type ] "
       BOOTPO()
  b:=shoePrefix? ('")console",a)
  b =>
       stream:= _*TERMINAL_-IO_*
       PSTOUT bRgen stream
       BOOTPO()
  a.0='"]".0 => nil
  PSTOUT [a]
  BOOTPO()
 
PSTOUT string==
  callingPackage := _*PACKAGE_*
  IN_-PACKAGE '"BOOTTRAN"
  $GenVarCounter := 0
  result := shoeConsoleTrees shoeTransformString string
  setCurrentPackage callingPackage
  result


defaultBootToLispFile file ==
  strconc(pathBasename file, '".clisp")

getIntermediateLispFile(file,options) ==
  out := NAMESTRING getOutputPathname(options)
  out ~= nil => 
    strconc(shoeRemoveStringIfNec
       (strconc('".",$effectiveFaslType),out),'".clisp")
  defaultBootToLispFile file

translateBootFile(progname, options, file) ==
  outFile := getOutputPathname options or defaultBootToLispFile file
  BOOTTOCL(file, ENOUGH_-NAMESTRING outFile)

retainFile? ext ==
  Option 'all in $FilesToRetain or Option 'yes in $FilesToRetain => true
  Option 'no in $FilesToRetain => false
  Option ext in $FilesToRetain

compileBootHandler(progname, options, file) ==
  intFile := BOOTTOCL(file, getIntermediateLispFile(file,options))
  errorCount() ~= 0 => nil
  intFile => 
    objFile := compileLispHandler(progname, options, intFile)
    if not retainFile? 'lisp then
      DELETE_-FILE intFile
    objFile
  nil

associateRequestWithFileType(Option '"translate", '"boot", 
                             function translateBootFile)

associateRequestWithFileType(Option '"compile", '"boot", 
                             function compileBootHandler)

--% Runtime support

++ Load native dynamically linked module
loadNativeModule m ==
  %hasFeature KEYWORD::SBCL =>
    FUNCALL(bfColonColon("SB-ALIEN","LOAD-SHARED-OBJECT"),m, 
             KEYWORD::DONT_-SAVE, true)
  %hasFeature KEYWORD::CLISP =>
    EVAL [bfColonColon("FFI","DEFAULT-FOREIGN-LIBRARY"), m]
  %hasFeature KEYWORD::ECL =>
    EVAL [bfColonColon("FFI","LOAD-FOREIGN-LIBRARY"), m]
  %hasFeature KEYWORD::CLOZURE =>
    EVAL [bfColonColon("CCL","OPEN-SHARED-LIBRARY"), m]
  coreError '"don't know how to load a dynamically linked module"

loadSystemRuntimeCore() ==
  %hasFeature KEYWORD::ECL or %hasFeature KEYWORD::GCL => nil
  loadNativeModule strconc('"libopen-axiom-core",$NativeModuleExt)
