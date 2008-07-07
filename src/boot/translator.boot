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
--


import includer
import scanner
import pile
import parser
import ast
module translator
namespace BOOTTRAN

++ If non nil, holds the name of the current module being translated.
$currentModuleName := nil

++ Stack of foreign definitions to cope with CLisp's odd FFI interface.
$foreignsDefsForCLisp := []

genModuleFinalization(stream) ==
  %hasFeature KEYWORD::CLISP =>
    null $foreignsDefsForCLisp => nil
    $currentModuleName = nil =>
       coreError '"current module has no name"
    init := 
      ["DEFUN", INTERN strconc($currentModuleName,"InitCLispFFI"), nil,
        ["MAPC",["FUNCTION", "FMAKUNBOUND"],
          ["QUOTE",[second d for d in $foreignsDefsForCLisp]]],
          :[["EVAL",["QUOTE",d]] for d in $foreignsDefsForCLisp]]
    REALLYPRETTYPRINT(init,stream)
  nil

+++ True if we are translating code written in Old Boot.
$translatingOldBoot := false

AxiomCore::%sysInit() ==
  if rest ASSOC(Option '"boot", %systemOptions()) = '"old"
  then $translatingOldBoot := true

++ Make x, the current package
setCurrentPackage: %Thing -> %Thing
setCurrentPackage x ==
  SETQ(_*PACKAGE_*,x)

++ Compiles the input Lisp file designated by lspFileName.
shoeCOMPILE_-FILE: %String -> %Thing
shoeCOMPILE_-FILE lspFileName ==
  COMPILE_-FILE lspFileName

 
BOOTTOCL(fn, out) ==
  callingPackage := _*PACKAGE_*
  IN_-PACKAGE '"BOOTTRAN"
  result := BOOTTOCLLINES(nil,fn, out)
  setCurrentPackage callingPackage
  result
 
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
   -- The default floating point number is double-float.
   SETQ(_*READ_-DEFAULT_-FLOAT_-FORMAT_*, 'DOUBLE_-FLOAT)
   infn:=shoeAddbootIfNec fn
   shoeOpenInputFile(a,infn, shoeClLines(a,fn,lines,outfn))
 
shoeClLines(a,fn,lines,outfn)==
  a=nil => shoeNotFound fn
  $GenVarCounter := 0
  shoeOpenOutputFile(stream,outfn,_
    ((for line in lines repeat shoeFileLine(line,stream);
       shoeFileTrees(shoeTransformStream a,stream));
      genModuleFinalization(stream)))
  outfn
 
++ (boottoclc "filename") translates the file "filename.boot" to
++ the common lisp file "filename.clisp" with the original boot
++ code as comments
BOOTTOCLC(fn, out)==
  callingPackage := _*PACKAGE_*
  IN_-PACKAGE '"BOOTTRAN"
  result := BOOTTOCLCLINES(nil, fn, out)
  setCurrentPackage callingPackage
  result
 
BOOTTOCLCLINES(lines, fn, outfn)==
  infn:=shoeAddbootIfNec fn
  shoeOpenInputFile(a,infn, shoeClCLines(a,fn,lines,outfn))
  
 
shoeClCLines(a,fn,lines,outfn)==
  a=nil => shoeNotFound fn
  $GenVarCounter := 0
  shoeOpenOutputFile(stream,outfn,
    (for line in lines repeat shoeFileLine (line,stream);
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
   outfn:=CONCAT(shoeRemovebootIfNec fn,'".",_*LISP_-SOURCE_-FILETYPE_*)
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
  cons(shoeParseTrees dq, rest str)

bFileNext(fn,s) ==
  bDelay(function bFileNext1,[fn,s])
 
bFileNext1(fn,s)==
  bStreamNull s=> ["nullstream"]
  dq := first s
  shoeFileLines(shoeDQlines dq,fn)
  bAppend(shoeParseTrees dq,bFileNext(fn,rest s))
 
shoeParseTrees dq==
  toklist := dqToList dq
  null toklist => []
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
    WRITE_-LINE(x, stream)
    x
 
shoeFileTrees(s,st)==
  while not bStreamNull s repeat
    a:= first s
    if EQCAR (a,"+LINE")
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

++ True if objects of type native type `t' are sensible to GC.
needsStableReference? t ==
  %hasFeature KEYWORD::GCL => false    -- 
  %hasFeature KEYWORD::SBCL or %hasFeature KEYWORD::CLISP 
    or %hasFeature KEYWORD::ECL => t = "pointer" or t = "buffer"
  true          -- don't know; conservatively answer `yes'.


++ coerce argument `a' to native type `t', in preparation for
++ a call to a native functions.
coerceToNativeType(a,t) ==
  %hasFeature KEYWORD::GCL => a
  %hasFeature KEYWORD::SBCL =>
    t = "buffer" => [bfColonColon("SB-SYS","VECTOR-SAP"),a]
    t = "string" => a     -- 'string's are automatically converted.
    needsStableReference? t =>
      fatalError '"don't know how to coerce argument for native type"
    a
  %hasFeature KEYWORD::CLISP or %hasFeature KEYWORD::ECL =>
    needsStableReference? t =>
      fatalError '"don't know how to coerce argument for native type"
    a   
  fatalError '"don't know how to coerce argument for native type"

++ filter out arguments that need stable references during call
++ to native function, and convert all arguments as necessary.
prepareArgumentsForNativeCall(args,types) ==
  unstableArgs := [a for a in args for t in types
                     | needsStableReference? t]
  preparedArgs := [coerceToNativeType(a,t)
                     for a in args for t in types]
  [unstableArgs,preparedArgs]


++ Generate an import declaration for `op' as equivalent of the
++ foreign signature `sig'.  Here, `foreign' operationally means that
++ the entity is from the C language world. 
genImportDeclaration(op, sig) ==
  sig isnt ["Signature", op', m] => coreError '"invalid signature"
  m isnt ["Mapping", t, s] => coreError '"invalid function type"
  if not null s and SYMBOLP s then s := [s]

  -- we don't deal with non-trivial return values (yet)
  needsStableReference? t =>
    fatalError '"non trivial return type for native function"

  %hasFeature KEYWORD::GCL => 
    [["DEFENTRY", op, [nativeType x for x in s], 
       [nativeType t, SYMBOL_-NAME op']]]

  args := [GENSYM() for x in s]
  %hasFeature KEYWORD::SBCL =>
    [unstableArgs,newArgs] := prepareArgumentsForNativeCall(args,s)
    null unstableArgs =>
      [["DEFUN",op,args,
	[INTERN('"ALIEN-FUNCALL",'"SB-ALIEN"),
	  [INTERN('"EXTERN-ALIEN",'"SB-ALIEN"),SYMBOL_-NAME op',
	    ["FUNCTION",nativeType t,:[nativeType x for x in s]]], :args]]]
    [["DEFUN",op,args,
      [bfColonColon("SB-SYS","WITH-PINNED-OBJECTS"),unstableArgs,       
	[INTERN('"ALIEN-FUNCALL",'"SB-ALIEN"),
	  [INTERN('"EXTERN-ALIEN",'"SB-ALIEN"),SYMBOL_-NAME op',
	    ["FUNCTION",nativeType t,:[nativeType x for x in s]]], :newArgs]]]]

  %hasFeature KEYWORD::CLISP =>
    -- there is a curious bug in the CLisp's FFI support whereby
    -- foreign declarations compiled separately will have the wrong
    -- types when used in other modules.  We work around that problem
    -- by defining forwarding functions to the foreign declarations
    -- in the same module the latter are declared.
    foreignDecl := 
      n := INTERN strconc(SYMBOL_-NAME op, '"%clisp-hack")
      [bfColonColon("FFI","DEF-CALL-OUT"),n,
	[KEYWORD::NAME,SYMBOL_-NAME op'],
	  [KEYWORD::ARGUMENTS,:[[a, 
	    bfColonColon("FFI", nativeType x)] for x in s for a in args]],
	      [KEYWORD::RETURN_-TYPE,bfColonColon("FFI",nativeType t)],
		[KEYWORD::LANGUAGE,KEYWORD::STDC]]
    forwardingFun := 
      ["DEFUN",op,args,
         [n,:[coerceToNativeType(a,t) for a in args for x in s]]]
    $foreignsDefsForCLisp := [foreignDecl,:$foreignsDefsForCLisp]
    [forwardingFun]

  %hasFeature KEYWORD::ECL =>
    [["DEFUN",op, args,
      [bfColonColon("FFI","C-INLINE"),args,[nativeType x for x in s],
        nativeType t, "strconc"/callTemplate(op',#args),
           KEYWORD::ONE_-LINER, true]]] where
	      callTemplate(op,n) ==
		[SYMBOL_-NAME op,'"(",:[:sharpArg i for i in 0..(n-1)],'")"]
	      sharpArg i == 
		i = 0 => ['"#0"]
		['",",'"#", STRINGIMAGE i]
  fatalError '"import declaration not implemented for this Lisp"

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
  found := try bpOutItem() catch TRAPPOINT
  found = "TRAPPED" => nil
  not bStreamNull $inputStream =>
    bpGeneralErrorHere()
    nil
  null $stack =>
    bpGeneralErrorHere()
    nil
  first $stack
 
++ Generate a global signature declaration for symbol `n'.
genDeclaration(n,t) ==
  t is ["Mapping",valType,argTypes] =>
    if bfTupleP argTypes then argTypes := rest argTypes
    if not null argTypes and SYMBOLP argTypes 
    then argTypes := [argTypes]
    ["DECLAIM",["FTYPE",["FUNCTION",argTypes,valType],n]]
  ["DECLAIM",["TYPE",t,n]]


++ Translate the signature declaration `d' to its Lisp equivalent.
translateSignatureDeclaration d ==
  case d of 
    Signature(n,t) => genDeclaration(n,t)
    otherwise => coreError '"signature expected"  

++ A non declarative expression `expr' appears at toplevel and its
++ translation needs embeddeding in an `EVAL-WHEN'.
translateToplevelExpression expr ==
  expr' := rest rest shoeCompTran ["LAMBDA",["x"],expr]
  -- replace "DECLARE"s with "DECLAIM"s, as the former can't appear
  -- at toplevel.
  for t in expr' repeat
    t is ["DECLARE",:.] =>
      RPLACA(t,"DECLAIM")
  expr' :=
    #expr' > 1 => ["PROGN",:expr']
    first expr'
  $InteractiveMode => expr'
  shoeEVALANDFILEACTQ expr'

maybeExportDecl(d,export?) ==
  export? => d
  d

translateToplevel(b,export?) ==
  atom b => [b]  -- generally happens in interactive mode.
  b is ["TUPLE",:xs] => [maybeExportDecl(x,export?) for x in xs]
  case b of
    Signature(op,t) =>
      [maybeExportDecl(genDeclaration(op,t),export?)]

    %Module(m,ds) =>
      $currentModuleName := m 
      $foreignsDefsForCLisp := nil
      [["PROVIDE", STRING m],
        :[translateToplevel(d,true) for d in ds]]

    Import(m) => 
      [["IMPORT-MODULE", STRING m]]

    ImportSignature(x, sig) =>
      genImportDeclaration(x, sig)

    %TypeAlias(lhs, rhs) => 
      [maybeExportDecl(genTypeAlias(lhs,rhs),export?)]

    ConstantDefinition(lhs,rhs) =>
      sig := nil
      if lhs is ["%Signature",n,t] then
        sig := maybeExportDecl(genDeclaration(n,t),export?)
        lhs := n
      [maybeExportDecl(["DEFCONSTANT",lhs,rhs],export?)]

    %Assignment(lhs,rhs) =>
      sig := nil
      if lhs is ["%Signature",n,t] then
        sig := maybeExportDecl(genDeclaration(n,t),export?)
        lhs := n
      $InteractiveMode => [["SETF",lhs,rhs]]
      [maybeExportDecl(["DEFPARAMETER",lhs,rhs],export?)]

    namespace(n) =>
      [["IN-PACKAGE",STRING n]]

    otherwise =>
      [translateToplevelExpression b]


bpOutItem()==
  $op := nil
  bpComma() or bpTrap()
  b:=bpPop1()
  EQCAR(b,"+LINE")=> bpPush [ b ]
  b is ["L%T",l,r] and IDENTP l =>
	       bpPush [["DEFPARAMETER",l,r]]
  bpPush translateToplevel(b,false)
 
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
	    cons(nee,$bootDefinedTwice)
  else HPUT($bootDefined,nee,true)
  defuse1 (e,niens)
  for i in $used repeat
     HPUT($bootUsed,i,cons(nee,GETHASH(i,$bootUsed)))
 
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
  null x => [[],[]]
  f := first x
  [x1,x2] := defSeparate rest x
  bfBeginsDollar f => [[f,:x1],x2]
  [x1,cons(f,x2)]

unfluidlist x==
  NULL x => []
  atom x=> [x]
  x is ["&REST",y]=> [y]
  cons(first x,unfluidlist rest x)
 
defusebuiltin x ==  
  GETHASH(x,$lispWordTable)
 
bootOut (l,outfn)==
  for i in l repeat shoeFileLine (CONCAT ('"   ",PNAME i),outfn)
 
CLESSP(s1,s2)==
  not(SHOEGREATERP(s1,s2))

SSORT l == 
  SORT(l,function CLESSP)
 
bootOutLines(l,outfn,s)==
  if null l
  then shoeFileLine(s,outfn)
  else
     a:=PNAME first l
     if #s +#a > 70
     then
          shoeFileLine(s,outfn)
          bootOutLines(l,outfn,'" ")
     else bootOutLines(rest l,outfn,CONCAT(s,'" ",a))
 
 
-- (xref "fn") produces a cross reference listing in "fn.xref"
-- It contains each name
-- used in "fn.boot", together with a list of functions that use it.
 
XREF fn==
  infn:=CONCAT(fn,'".boot")
  shoeOpenInputFile(a,infn,shoeXref(a,fn))
 
shoeXref(a,fn)==
  if null a
  then shoeNotFound fn
  else
     $lispWordTable  :=MAKE_-HASHTABLE ("EQ")
     DO_-SYMBOLS(i(FIND_-PACKAGE "LISP"),HPUT($lispWordTable,i,true))
     $bootDefined  :=MAKE_-HASHTABLE "EQ"
     $bootUsed  :=MAKE_-HASHTABLE "EQ"
     $GenVarCounter  :=0
     $bfClamming :=false
     shoeDefUse shoeTransformStream a
     out:=CONCAT(fn,'".xref")
     shoeOpenOutputFile(stream,out,shoeXReport stream)
     out
 
 
shoeXReport stream==
   shoeFileLine('"USED and where DEFINED",stream)
   c:=SSORT HKEYS $bootUsed
   for i in c repeat
      a:=CONCAT(PNAME i,'" is used in ")
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
   a =>  FUNCALL(f, CONCAT('"/tmp/",filename))
   nil
 
shoeFindName2(fn,name,a)==
  lines:=shoeFindLines(fn,name,a)
  lines =>
    filename:= if # name > 8 then SUBSTRING(name,0,8) else name
    filename := CONCAT ('"/tmp/",filename,'".boot")
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
  cons([[first line for line in  shoeDQlines dq]],rest str)
 
stripm (x,pk,bt)==
  atom x =>
    IDENTP x =>
      SYMBOL_-PACKAGE x = bt => INTERN(PNAME x,pk)
      x
    x
  CONS(stripm(first x,pk,bt),stripm(rest x,pk,bt))
 
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
 
BOOTLOOP ()==
  a:=READ_-LINE()
  #a=0=>
       WRITE_-LINE '"Boot Loop; to exit type ] "
       BOOTLOOP()
  b:=shoePrefix? ('")console",a)
  b =>
       stream:= _*TERMINAL_-IO_*
       PSTTOMC bRgen stream
       BOOTLOOP()
  a.0='"]".0 => nil
  PSTTOMC [a]
  BOOTLOOP()
 
BOOTPO ()==
  a:=READ_-LINE()
  #a=0=>
       WRITE_-LINE '"Boot Loop; to exit type ] "
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
  out ^= nil => 
    strconc(shoeRemoveStringIfNec($effectiveFaslType,out),'".clisp")
  defaultBootToLispFile file

translateBootFile(progname, options, file) ==
  outFile := getOutputPathname options or defaultBootToLispFile file
  BOOTTOCL(file, ENOUGH_-NAMESTRING outFile)

compileBootHandler(progname, options, file) ==
  intFile := BOOTTOCL(file, getIntermediateLispFile(file,options))
  errorCount() ^= 0 => nil
  intFile => 
    objFile := compileLispHandler(progname, options, intFile)
    DELETE_-FILE intFile
    objFile
  nil

associateRequestWithFileType(Option '"translate", '"boot", 
                             function translateBootFile)

associateRequestWithFileType(Option '"compile", '"boot", 
                             function compileBootHandler)

--% System wide properties

++ Returns the root directory of the running system.
++ A directory specified on command line takes precedence
++ over directory specified at configuration time.
systemRootDirectory() ==
  dir := ASSOC(Option '"system", %systemOptions()) =>
    ensureTrailingSlash rest dir
  $systemInstallationDirectory

++ Returns the directory containing the core runtime support
++ libraries, either as specified on command line, or as inferred
++ from the system root directory.

systemLibraryDirectory() ==
  dir := ASSOC(Option "syslib",%systemOptions()) =>
    ensureTrailingSlash rest dir
  strconc(systemRootDirectory(),'"lib/")


--% Runtime support

++ Load native dynamically linked module
loadNativeModule m ==
  %hasFeature KEYWORD::SBCL =>
    FUNCALL(bfColonColon("SB-ALIEN","LOAD-SHARED-OBJECT"),m)
  %hasFeature KEYWORD::CLISP =>
    EVAL [bfColonColon("FFI","DEFAULT-FOREIGN-LIBRARY"), m]
  coreError '"don't know how to load a dynamically linked module"

loadSystemRuntimeCore() ==
  loadNativeModule strconc(systemLibraryDirectory(),
    '"libopen-axiom-core",$NativeModuleExt)
