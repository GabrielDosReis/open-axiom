-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
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
module translator (evalBootFile, loadNativeModule, loadSystemRuntimeCore,
  string2BootTree, genImportDeclaration)

++ If non nil, holds the name of the current module being translated.
$currentModuleName := nil

++ Stack of foreign definitions to cope with CLisp's odd FFI interface.
$foreignsDefsForCLisp := []

reallyPrettyPrint(x,st == _*STANDARD_-OUTPUT_*) ==
  prettyPrint(x,st)
  writeNewline st

genModuleFinalization(stream) ==
  %hasFeature KEYWORD::CLISP =>
    $foreignsDefsForCLisp = nil => nil
    $currentModuleName = nil =>
       coreError '"current module has no name"
    init := 
      ["DEFUN", makeSymbol strconc($currentModuleName,'"InitCLispFFI"), nil,
        ["MAPC",["FUNCTION", "FMAKUNBOUND"],
          quote [second d for d in $foreignsDefsForCLisp]],
          :[["EVAL",quote d] for d in $foreignsDefsForCLisp]]
    reallyPrettyPrint(init,stream)
  nil

genOptimizeOptions stream ==
  reallyPrettyPrint
    (["PROCLAIM",quote ["OPTIMIZE",:$LispOptimizeOptions]],stream)

AxiomCore::%sysInit() ==
  SETQ(_*LOAD_-VERBOSE_*,false)
  if %hasFeature KEYWORD::GCL then
    symbolValue(bfColonColon("COMPILER","*COMPILE-VERBOSE*")) := false
    symbolValue(bfColonColon("COMPILER","SUPPRESS-COMPILER-WARNINGS*")) := false
    symbolValue(bfColonColon("COMPILER","SUPPRESS-COMPILER-NOTES*")) := true

++ Make x, the current package
setCurrentPackage: %Thing -> %Thing
setCurrentPackage x ==
  SETQ(_*PACKAGE_*,x)

++ Compiles the input Lisp file designated by lspFileName.
shoeCOMPILE_-FILE: %String -> %Thing
shoeCOMPILE_-FILE lspFileName ==
  COMPILE_-FILE lspFileName

 
BOOTTOCL(fn, out) ==
  try
    startCompileDuration()
    in namespace BOOTTRAN do
      BOOTTOCLLINES(nil,fn, out)
  finally endCompileDuration()
 
++ (bootclam "filename") translates the file "filename.boot" to
++ the common lisp file "filename.clisp" , producing, for each function
++ a hash table to store previously computed values indexed by argument
++ list.
BOOTCLAM(fn, out) == 
  $bfClamming: local := true
  BOOTCLAMLINES(nil,fn, out)
 
BOOTCLAMLINES(lines, fn, out) ==
   BOOTTOCLLINES(lines, fn, out)

BOOTTOCLLINES(lines, fn, outfn)==
   try
     a := inputTextFile shoeAddbootIfNec fn
     shoeClLines(a,fn,lines,outfn)
   finally closeStream a
 
shoeClLines(a,fn,lines,outfn)==
  a=nil => shoeNotFound fn
  try
    stream := outputTextFile outfn
    genOptimizeOptions stream
    for line in lines repeat
      shoeFileLine(line,stream)
    shoeFileTrees(shoeTransformStream a,stream)
    genModuleFinalization stream
    outfn
  finally closeStream stream
 
++ (boottoclc "filename") translates the file "filename.boot" to
++ the common lisp file "filename.clisp" with the original boot
++ code as comments
BOOTTOCLC(fn, out)==
  try
    startCompileDuration()
    in namespace BOOTTRAN do
      BOOTTOCLCLINES(nil, fn, out)
  finally endCompileDuration()
 
BOOTTOCLCLINES(lines, fn, outfn)==
  try
    a := inputTextFile shoeAddbootIfNec fn
    shoeClCLines(a,fn,lines,outfn)
  finally closeStream a
 
shoeClCLines(a,fn,lines,outfn)==
  a=nil => shoeNotFound fn
  try
    stream := outputTextFile outfn
    genOptimizeOptions stream
    for line in lines repeat
       shoeFileLine(line,stream)
    shoeFileTrees(shoeTransformToFile(stream,
      shoeInclude bAddLineNumber(bRgen a,bIgen 0)),stream)
    genModuleFinalization(stream)
    outfn
  finally closeStream stream
 
++ (boottomc "filename") translates the file "filename.boot"
++ to machine code and loads it one item at a time
BOOTTOMC: %String -> %Thing 
BOOTTOMC fn==
   callingPackage := namespace .
   IN_-PACKAGE '"BOOTTRAN"
   try
     a := inputTextFile shoeAddbootIfNec fn
     shoeMc(a,fn)
   finally
     closeStream a
     setCurrentPackage callingPackage
 
shoeMc(a,fn)==
  a=nil => shoeNotFound fn
  shoePCompileTrees shoeTransformStream a
  shoeConsole strconc(fn,'" COMPILED AND LOADED")
 
evalBootFile fn ==
   b := namespace .
   IN_-PACKAGE '"BOOTTRAN"
   infn:=shoeAddbootIfNec fn
   outfn := strconc(shoeRemovebootIfNec fn,'".",'"lisp")
   try
     a := inputTextFile infn
     shoeClLines(a,infn,[],outfn)
   finally
     closeStream a
     setCurrentPackage b
   LOAD outfn
 
++ (boot "filename") translates the file "filename.boot"
++ and prints the result at the console
BO: %String -> %Thing 
BO fn==
  b := namespace .
  IN_-PACKAGE '"BOOTTRAN"
  try
    a := inputTextFile shoeAddbootIfNec fn
    shoeToConsole(a,fn)
  finally
    closeStream a
    setCurrentPackage b
 
BOCLAM fn==
  callingPackage := namespace .
  IN_-PACKAGE '"BOOTTRAN"
  $bfClamming: local := true
  try
    a := inputTextFile shoeAddbootIfNec fn
    shoeToConsole(a,fn)
  finally
    closeStream a
    setCurrentPackage callingPackage
 
shoeToConsole(a,fn)==
  a=nil => shoeNotFound fn
  shoeConsoleTrees shoeTransformToConsole
    shoeInclude bAddLineNumber(bRgen a,bIgen 0)

-- (stout "string") translates the string "string"
-- and prints the result at the console

STOUT string == 
  PSTOUT [string]
 
string2BootTree string ==   
  callingPackage := namespace .
  IN_-PACKAGE '"BOOTTRAN"
  a := shoeTransformString [string]
  result :=
    bStreamNull a => nil
    stripm(first a,callingPackage,namespace BOOTTRAN)
  setCurrentPackage callingPackage
  result

 
STEVAL string==
   callingPackage := namespace .
   IN_-PACKAGE '"BOOTTRAN"
   a:=  shoeTransformString [string]
   result := 
      bStreamNull a => nil
      fn:=stripm(first a,namespace .,namespace BOOTTRAN)
      EVAL fn
   setCurrentPackage callingPackage
   result
 
-- (sttomc "string") translates the string "string"
-- to common lisp, and compiles it.
 
STTOMC string==
   callingPackage := namespace .
   IN_-PACKAGE '"BOOTTRAN"
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
      reallyPrettyPrint(a,st)
      TERPRI st
    s:= rest s
 
shoeConsoleTrees s ==
  while not bStreamPackageNull s repeat
    fn:=stripm(first s,namespace .,namespace BOOTTRAN)
    reallyPrettyPrint fn
    s:= rest s
 
shoeAddComment l==  
  strconc('"; ", first l)

shoeOutParse toks ==
  ps := makeParserState toks
  $stok := nil
  $ttok := nil
  $op :=nil
  $wheredefs := []
  $typings := []
  $returns := []
  $bpCount := 0
  bpFirstTok ps
  found :=
    try bpOutItem ps
    catch(e: BootParserException) => e
  found = 'TRAPPED => nil
  not bStreamNull parserTokens ps =>
    bpGeneralErrorHere()
    nil
  parserTrees ps = nil =>
    bpGeneralErrorHere()
    nil
  first parserTrees ps
 
++ Generate a global signature declaration for symbol `n'.
genDeclaration(n,t) ==
  t is ["%Mapping",:.] => ["DECLAIM",["FTYPE",bfType t,n]]
  t is ["%Forall",vars,t'] =>
    vars = nil => genDeclaration(n,t')
    if symbol? vars then
      vars := [vars]
    genDeclaration(n,applySubst([[v,:"*"] for v in vars],t'))
  ["DECLAIM",["TYPE",bfType t,n]]

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
  #expr' > 1 => ["PROGN",:expr']
  first expr'

inAllContexts x ==
  ["EVAL-WHEN",[KEYWORD::COMPILE_-TOPLEVEL,
                  KEYWORD::LOAD_-TOPLEVEL,
                    KEYWORD::EXECUTE], x]

exportNames ns ==
  ns = nil => nil
  [inAllContexts ["EXPORT",quote ns]]

packageBody(x,p) ==
  x is ['%Import,['%Namespace,ns]] =>
    user :=
      p = nil => nil
      [symbolName p]
    ns is 'System =>
      ['COND,
        [['%hasFeature,KEYWORD::COMMON_-LISP],['USE_-PACKAGE,'"COMMON-LISP",:user]],
           ['T,['USE_-PACKAGE,'"LISP",:user]]]
    z :=
      ns is ['DOT,'System,'Foreign] =>
        %hasFeature KEYWORD::SBCL => 'SB_-ALIEN
        %hasFeature KEYWORD::ECL => 'FFI
        return nil
      ident? ns => ns
      bpTrap()
    ['USE_-PACKAGE,symbolName z,:user]
  x is ['PROGN,:.] => [x.op,:[packageBody(y,p) for y in x.args]]
  x

translateToplevel(b,export?) ==
  b isnt [.,:.] => [b]  -- generally happens in interactive mode.
  b is ["TUPLE",:xs] => coreError '"invalid AST"
  case b of
    %Signature(op,t) => [genDeclaration(op,t)]
    %Definition(op,args,body) => bfDef(op,args,body).args

    %Module(m,ns,ds) =>
      $currentModuleName := m 
      $foreignsDefsForCLisp := nil
      [["PROVIDE", symbolName m], :exportNames ns,
        :[first translateToplevel(d,true) for d in ds]]

    %Import(m) =>
      m is ['%Namespace,n] => [inAllContexts packageBody(b,nil)]
      if getOptionValue "import" ~= '"skip" then
        bootImport symbolName m
      [["IMPORT-MODULE", symbolName m]]

    %ImportSignature(x, sig) =>
      genImportDeclaration(x, sig)

    %TypeAlias(lhs, rhs) => [genTypeAlias(lhs,rhs)]

    %ConstantDefinition(lhs,rhs) =>
      lhs is ['%Namespace,ns] =>
        [['DEFPACKAGE,symbolName ns],inAllContexts packageBody(rhs,ns)]
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

    %Structure(t,alts) =>
      alts is ['%Record,fields,accessors] => bfRecordDef(t,fields,accessors)
      alts is [['Enumeration,:csts]] => [bfEnum(t,csts)]
      [bfCreateDef alt for alt in alts]

    %Namespace n =>
      $activeNamespace := symbolName n
      [["IN-PACKAGE",symbolName n]]

    %Lisp s => shoeReadLispString(s,0)

    otherwise =>
      [translateToplevelExpression b]

shoeAddbootIfNec s ==
  ext := '".boot"
  n1 := #ext - 1
  n2 := #s - n1 - 1
  and/[stringChar(ext,k) = stringChar(s,n2 + k) for k in 0..n1] => s
  strconc(s,ext)
 
shoeRemovebootIfNec s == 
  shoeRemoveStringIfNec('".boot",s)

shoeRemoveStringIfNec(str,s)==
  n := stringSuffix?(str,s) => subString(s,0,n)
  s
 
-- DEFUSE prints the definitions not used and the words used and
-- not defined in the input file and common lisp.
 
DEFUSE fn==
  try
    a := inputTextFile strconc(fn,'".boot")
    shoeDfu(a,fn)
  finally closeStream a
 
--%
$bootDefined := nil
$bootDefinedTwice := nil
$bootUsed := nil
$lispWordTable := nil

shoeDfu(a,fn)==
  a=nil => shoeNotFound fn
  $lispWordTable: local := makeTable function symbolEq?
  DO_-SYMBOLS(i(namespace LISP),tableValue($lispWordTable,i) := true)
  $bootDefined: local := makeTable function symbolEq?
  $bootUsed:local := makeTable function symbolEq?
  $bootDefinedTwice: local := nil
  $bfClamming: local := false
  shoeDefUse shoeTransformStream a
  try
    stream := outputTextFile strconc(fn,'".defuse")
    shoeReport stream
  finally closeStream stream
 
shoeReport stream==
  shoeFileLine('"DEFINED and not USED",stream)
  a := [i for [i,:b] in entries $bootDefined | not b]
  bootOut(SSORT a,stream)
  shoeFileLine('"             ",stream)
  shoeFileLine('"DEFINED TWICE",stream)
  bootOut(SSORT $bootDefinedTwice,stream)
  shoeFileLine('"             ",stream)
  shoeFileLine('"USED and not DEFINED",stream)
  a := [i for [i,:b] in entries $bootUsed | not b]
  for i in SSORT a repeat
     b := strconc(symbolName i,'" is used in ")
     bootOutLines( SSORT tableValue($bootUsed,i),stream,b)
 
shoeDefUse(s)==
  while not bStreamPackageNull s repeat
    defuse([],first s)
    s:=rest s
 
defuse(e,x)==
  x:=stripm(x,namespace .,namespace BOOTTRAN)
  $used :=nil
  [nee,niens]:=
     x is ['DEFUN,name,bv,:body] => [name,['LAMBDA,bv,:body]]
     x is ['DEFMACRO,name,bv,:body] => [name,['LAMBDA,bv,:body]]
     x is ["EVAL_-WHEN",.,["SETQ",id,exp]]=>[id,exp]
     x is ["SETQ",id,exp]=>[id,exp]
     ["TOP-LEVEL", x]
  if tableValue($bootDefined,nee)
  then
     $bootDefinedTwice:=
	    nee="TOP-LEVEL"=> $bootDefinedTwice
	    [nee,:$bootDefinedTwice]
  else tableValue($bootDefined,nee) := true
  defuse1 (e,niens)
  for i in $used repeat
     tableValue($bootUsed,i) := [nee,:tableValue($bootUsed,i)]
 
defuse1(e,y)==
  y isnt [.,:.] =>
      symbol? y =>
	 $used:=
	      symbolMember?(y,e)=>$used
	      symbolMember?(y,$used)=>$used
	      defusebuiltin y =>$used
	      UNION([y],$used)
      []
  y is ["LAMBDA",a,:b]=> defuse1 (append(unfluidlist a,e),b)
  y is ["PROG",a,:b]=>
	 [dol,ndol]:=defSeparate a
	 for i in dol repeat
	       tableValue($bootDefined,i) := true
	 defuse1 (append(ndol,e),b)
  y is ['QUOTE,:a] => []
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
  x isnt [.,:.] => [x]
  x is ["&REST",y]=> [y]
  [first x,:unfluidlist rest x]
 
defusebuiltin x ==  
  tableValue($lispWordTable,x)
 
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
  try
    a := inputTextFile strconc(fn,'".boot")
    shoeXref(a,fn)
  finally closeStream a
 
shoeXref(a,fn)==
  a = nil => shoeNotFound fn
  $lispWordTable: local := makeTable function symbolEq?
  DO_-SYMBOLS(i(namespace LISP),tableValue($lispWordTable,i) := true)
  $bootDefined: local := makeTable function symbolEq?
  $bootUsed: local := makeTable function symbolEq?
  $bfClamming: local := false
  shoeDefUse shoeTransformStream a
  out := strconc(fn,'".xref")
  try
    stream := outputTextFile out
    shoeXReport stream
    out
  finally closeStream stream
 
 
shoeXReport stream==
   shoeFileLine('"USED and where DEFINED",stream)
   c := SSORT [k for [k,:.] in entries $bootUsed]
   for i in c repeat
      a := strconc(symbolName i,'" is used in ")
      bootOutLines( SSORT tableValue($bootUsed,i),stream,a)
 
shoeItem (str)==
  dq:=first str
  [[[first line for line in  shoeDQlines dq]],:rest str]
 
stripm (x,pk,bt)==
  x isnt [.,:.] =>
    symbol? x =>
      symbolScope x = bt => makeSymbol(symbolName x,pk)
      x
    x
  [stripm(first x,pk,bt),:stripm(rest x,pk,bt)]
 
shoePCompile  fn==
    fn:=stripm(fn,namespace .,namespace BOOTTRAN)
    fn is ['DEFUN,name,bv,:body]=>
          COMPILE (name,['LAMBDA,bv,:body])
    EVAL fn
 
shoePCompileTrees s==
  while not bStreamNull s repeat
    reallyPrettyPrint shoePCompile first s
    s := rest s
 
bStreamPackageNull s==
  in namespace BOOTTRAN do
    bStreamNull s
 
PSTTOMC string==
  shoePCompileTrees shoeTransformString string
 
BOOTLOOP() ==
  a := readLine $stdin
  #a=0=>
    writeLine '"Boot Loop; to exit type ] "
    BOOTLOOP()
  shoePrefix? ('")console",a) =>
    stream := $stdio
    PSTTOMC bRgen stream
    BOOTLOOP()
  stringChar(a,0) = char "]" => nil
  PSTTOMC [a]
  BOOTLOOP()
 
BOOTPO() ==
  a := readLine $stdin
  #a=0=>
    writeLine '"Boot Loop; to exit type ] "
    BOOTPO()
  shoePrefix? ('")console",a) =>
    stream := $stdio
    PSTOUT bRgen stream
    BOOTPO()
  stringChar(a,0) = char "]" => nil
  PSTOUT [a]
  BOOTPO()
 
PSTOUT string==
  in namespace BOOTTRAN do
    shoeConsoleTrees shoeTransformString string

defaultBootToLispFile file ==
  strconc(pathBasename file, '".clisp")

getIntermediateLispFile(file,options) ==
  out := NAMESTRING getOutputPathname(options)
  out ~= nil => 
    strconc(shoeRemoveStringIfNec(strconc('".",$faslType),out),'".clisp")
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
