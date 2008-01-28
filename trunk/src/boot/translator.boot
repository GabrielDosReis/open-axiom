-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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
--


module '"boot-translator"
import '"includer"
import '"scanner"
import '"pile"
import '"parser"
import '"ast"

)package "BOOTTRAN"

+++ True if we are translating code written in Old Boot.
$translatingOldBoot := false

AxiomCore::%sysInit() ==
  if cdr ASSOC(Option '"boot", %systemOptions()) = '"old"
  then $translatingOldBoot := true

-- Make x, the current package
setCurrentPackage x ==
  SETQ(_*PACKAGE_*,x)

-- Compiles the input Lisp file designated by lspFileName.
shoeCOMPILE_-FILE lspFileName ==
  COMPILE_-FILE lspFileName

-- (boottocl "filename") translates the file "filename.boot" to
-- the common lisp file "filename.clisp"
 
BOOTTOCL(fn, out) ==
  BOOTTOCLLINES(nil,fn, out)
 
-- (bootclam "filename") translates the file "filename.boot" to
-- the common lisp file "filename.clisp" , producing, for each function
-- a hash table to store previously computed values indexed by argument
-- list.
 
BOOTCLAM(fn, out) == 
  $bfClamming := true
  BOOTCLAMLINES(nil,fn, out)
 
BOOTCLAMLINES(lines, fn, out) ==
   BOOTTOCLLINES(lines, fn, out)

BOOTTOCLLINES(lines, fn, outfn)==
   -- The default floating point number is double-float.
   SETQ(_*READ_-DEFAULT_-FLOAT_-FORMAT_*, 'DOUBLE_-FLOAT)
   callingPackage := _*PACKAGE_*
   IN_-PACKAGE '"BOOTTRAN"
   infn:=shoeAddbootIfNec fn
   result := shoeOpenInputFile(a,infn,
                shoeClLines(a,fn,lines,outfn))
   setCurrentPackage callingPackage
   result
 
shoeClLines(a,fn,lines,outfn)==
      if null a
      then shoeNotFound fn
      else
       $GenVarCounter:local := 0
       shoeOpenOutputFile(stream,outfn,
         (for line in lines repeat shoeFileLine (line,stream);
           shoeFileTrees(shoeTransformStream a,stream)))
       outfn
 
-- (boottoclc "filename") translates the file "filename.boot" to
-- the common lisp file "filename.clisp" with the original boot
-- code as comments
 
BOOTTOCLC(fn, out)==BOOTTOCLCLINES(nil, fn, out)
 
BOOTTOCLCLINES(lines, fn, outfn)==
  callingPackage := _*PACKAGE_*
  IN_-PACKAGE '"BOOTTRAN"
  infn:=shoeAddbootIfNec fn
  result := shoeOpenInputFile(a,infn,
               shoeClCLines(a,fn,lines,outfn))
  setCurrentPackage callingPackage
  result
  
 
shoeClCLines(a,fn,lines,outfn)==
      if null a
      then shoeNotFound fn
      else
       $GenVarCounter:local := 0
       shoeOpenOutputFile(stream,outfn,
         (for line in lines repeat shoeFileLine (line,stream);
           shoeFileTrees(shoeTransformToFile(stream,
               shoeInclude bAddLineNumber(bRgen a,bIgen 0)),stream)))
       outfn
 
-- (boottomc "filename") translates the file "filename.boot"
-- to machine code and loads it one item at a time
 
BOOTTOMC fn==
   callingPackage := _*PACKAGE_*
   IN_-PACKAGE '"BOOTTRAN"
   $GenVarCounter:local := 0
   infn:=shoeAddbootIfNec fn
   result := shoeOpenInputFile(a,infn,shoeMc(a,fn))
   setCurrentPackage callingPackage
   result
 
shoeMc(a,fn)==
   if null a
   then shoeNotFound fn
   else
     shoePCompileTrees shoeTransformStream a
     shoeConsole CONCAT(fn,'" COMPILED AND LOADED")
 
EVAL_-BOOT_-FILE fn ==
   b := _*PACKAGE_*
   IN_-PACKAGE '"BOOTTRAN"
   infn:=shoeAddbootIfNec fn
   outfn:=CONCAT(shoeRemovebootIfNec fn,'".",_*LISP_-SOURCE_-FILETYPE_*)
   shoeOpenInputFile(a,infn,shoeClLines(a,infn,[],outfn))
   setCurrentPackage b
   LOAD outfn
 
-- (boot "filename") translates the file "filename.boot"
-- and prints the result at the console
 
BO fn==
     b := _*PACKAGE_*
     IN_-PACKAGE '"BOOTTRAN"
     $GenVarCounter:local := 0
     infn:=shoeAddbootIfNec fn
     shoeOpenInputFile(a,infn,shoeToConsole(a,fn))
     setCurrentPackage b
 
BOCLAM fn==
     callingPackage := _*PACKAGE_*
     IN_-PACKAGE '"BOOTTRAN"
     $GenVarCounter:local := 0
     $bfClamming:local := true
     infn:=shoeAddbootIfNec fn
     result := shoeOpenInputFile(a,infn,shoeToConsole(a,fn))
     setCurrentPackage callingPackage
     result
 
shoeToConsole(a,fn)==
     if null a
     then shoeNotFound fn
     else
        shoeConsoleTrees shoeTransformToConsole
          shoeInclude bAddLineNumber(bRgen a,bIgen 0)
 
-- (stout "string") translates the string "string"
-- and prints the result at the console
 
STOUT string==   PSTOUT [string]
--   $GenVarCounter:local := 0
--   $bfClamming:local:=false
--   shoeConsoleTrees shoeTransformString [string]
 
STEVAL string==
   callingPackage := _*PACKAGE_*
   IN_-PACKAGE '"BOOTTRAN"
   $GenVarCounter:local := 0
   a:=  shoeTransformString [string]
   result := 
      bStreamPackageNull a => nil
      fn:=stripm(CAR a,_*PACKAGE_*,FIND_-PACKAGE '"BOOTTRAN")
      EVAL fn
   setCurrentPackage callingPackage
   result
 
-- (sttomc "string") translates the string "string"
-- to common lisp, and compiles it.
 
STTOMC string==
   callingPackage := _*PACKAGE_*
   IN_-PACKAGE '"BOOTTRAN"
   $GenVarCounter:local := 0
   a:=  shoeTransformString [string]
   result := 
      bStreamPackageNull a => nil
      shoePCompile car a
   setCurrentPackage callingPackage
   result
 
 
shoeCompileTrees s==
    while not bStreamNull s repeat
         shoeCompile car s
         s:=cdr s
 
shoeCompile fn==
    fn is ['DEFUN,name,bv,:body]=>
          COMPILE (name,['LAMBDA,bv,:body])
    EVAL fn
 
shoeTransform str==
    bNext(function shoeTreeConstruct,
      bNext(function shoePileInsert,
        bNext(function shoeLineToks, str)))
 
shoeTransformString s==
    shoeTransform shoeInclude bAddLineNumber(s,bIgen 0)
shoeTransformStream s==shoeTransformString bRgen s
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
        dq:=CAR str
        shoeConsoleLines shoeDQlines dq
        cons(shoeParseTrees dq,CDR str)
 
bFileNext(fn,s)==bDelay(function bFileNext1,[fn,s])
 
bFileNext1(fn,s)==
      bStreamNull s=> ["nullstream"]
      dq:=CAR s
      shoeFileLines(shoeDQlines dq,fn)
      bAppend(shoeParseTrees dq,bFileNext(fn,cdr s))
 
shoeParseTrees dq==
        toklist := dqToList dq
        null toklist => []
        shoeOutParse toklist
 
shoeTreeConstruct (str)==
        cons(shoeParseTrees CAR str,CDR str)
 
shoeDQlines dq==
        a:= CDAAR shoeLastTokPosn dq
        b:= CDAAR shoeFirstTokPosn dq
        streamTake (a-b+1,CAR shoeFirstTokPosn dq)
 
streamTake(n,s)==
    if bStreamNull s
    then nil
    else if EQL(n,0)
         then nil
         else cons(car s,streamTake(n-1,cdr s))
 
shoeFileLines (lines,fn) ==
        shoeFileLine( '" ",fn)
        for line in lines repeat shoeFileLine (shoeAddComment line,fn)
        shoeFileLine ('" ",fn)
 
shoeConsoleLines lines ==
        shoeConsole '" "
        for line in lines repeat shoeConsole shoeAddComment line
        shoeConsole '" "
 
shoeFileLine(x, stream) ==
    WRITE_-LINE(x, stream)
    x
 
shoeFileTrees(s,st)==
        while not bStreamNull s repeat
            a:=CAR s
            if EQCAR (a,"+LINE")
            then shoeFileLine(CADR a,st)
            else 
              REALLYPRETTYPRINT(a,st)
              TERPRI st
            s:=CDR s
 
 
shoePPtoFile(x, stream) ==
    SHOENOTPRETTYPRINT(x, stream)
    x
 
shoeConsoleTrees s ==
    while not bStreamPackageNull s repeat
--      while not bStreamNull s repeat
          fn:=stripm(CAR s,_*PACKAGE_*,FIND_-PACKAGE '"BOOTTRAN")
          REALLYPRETTYPRINT fn
          s:=CDR s
 
shoeAddComment l==  CONCAT('"; ",CAR l)

++ Generate an import declaration for `op' as equivalent of the
++ foreign signature `sig'.  Here, `foreign' operationally means that
++ the entity is from the C language world. 
genImportDeclaration(op, sig) ==
  sig isnt ["Signature", op', m] => coreError '"invalid signature"
  m isnt ["Mapping", t, s] => coreError '"invalid function type"
  %hasFeature KEYWORD::GCL => 
    if SYMBOLP s then s := [s]
    ["DEFENTRY", op, s, [t, SYMBOL_-NAME op']]
  fatalError '"import declaration not implemented for this Lisp"

shoeOutParse stream ==
    $inputStream :local:= stream
    $stack:local       :=nil
    $stok:local := nil
    $ttok:local := nil
    $op:local :=nil
    $wheredefs:local:=nil
    $typings:local:=nil
    $returns:local :=nil
    $bpCount:local:=0
    $bpParenCount:local:=0
    bpFirstTok()
    found:=CATCH("TRAPPOINT",bpOutItem())
    if found="TRAPPED"
    then  nil
    else if not bStreamNull $inputStream
         then
          bpGeneralErrorHere()
          nil
         else if null $stack
              then
                bpGeneralErrorHere()
                nil
              else CAR $stack
 
bpOutItem()==
    bpComma() or bpTrap()
    b:=bpPop1()
    EQCAR(b,"TUPLE")=> bpPush cdr b
    EQCAR(b,"+LINE")=> bpPush [ b ]
    b is ["L%T",l,r] and IDENTP l =>
                 bpPush [["DEFPARAMETER",l,r]]
    case b of
      Module(m) => 
        bpPush [shoeCompileTimeEvaluation ["PROVIDE", m]]

      Import(m) => 
        bpPush [["IMPORT-MODULE", m]]

      ImportSignature(x, sig) =>
        bpPush [genImportDeclaration(x, sig)]

      TypeAlias(t, args, rhs) => 
        bpPush [["DEFTYPE", t, args, ["QUOTE", rhs]]]

      ConstantDefinition(n, e) =>
        bpPush [["DEFCONSTANT", n, e]]

      otherwise =>
        b:=shoeCompTran ["LAMBDA",["x"],b]
        bpPush [shoeEVALANDFILEACTQ CADDR b]
 
--shoeStartsAt (sz,name,stream)==
--   bStreamNull stream => ['nullstream]
--   a:=CAAR stream
--   if #a<sz
--   then shoeStartsAt(sz,name,CDR stream)
--   else if SUBSTRING(a,0,sz)=name and (#a>sz and not shoeIdChar(a.sz))
--        then stream
--        else shoeStartsAt(sz,name,CDR stream)
 
--FC(name,fn)==
--   $bfClamming:local:=false
--   $GenVarCounter:local := 0
--   infn:=shoeAddbootIfNec fn
--   shoeOpenInputFile(a,infn,shoeFindName(fn,name, a))
 
--shoeFindName(fn,name,a)==
--  shoeFindAndDoSomething(FUNCTION shoeCompile,fn,name,a)
--shoeTransform1 str==
--    bNext(function shoeTreeConstruct,
--      streamTake(1, bNext(function shoePileInsert,
--           bNext(function shoeLineToks, str))))
 
--BOOT_-DO_-SOMETHING_-TO_-DEFINITION_-FROM_-FILE(fun,fn,symbol)==
--   $bfClamming:local:=false
--   infn:=shoeAddbootIfNec NAMESTRING fn
--   name:=PNAME symbol
--   shoeOpenInputFile(a,infn,shoeFindAndDoSomething(fun,fn,name, a))
 
--shoeFindAndDoSomething(fun,fn,name,a)==
--   if null a
--   then shoeNotFound fn
--   else
--      b:=shoeStartsAt(#name,name, shoeInclude
--                        bAddLineNumber(bRgen a,bIgen 0))
--      if bStreamNull b
--      then shoeConsole CONCAT (name,'" not found in ",fn)
--      else
--         $GenVarCounter:local := 0
--         shoeLoop(fun,shoeTransform1 b)
 
--BOOT_-COMPILE_-DEFINITION_-FROM_-FILE(fn,symbol)==
--   BOOT_-DO_-SOMETHING_-TO_-DEFINITION_-FROM_-FILE
--         (FUNCTION shoeCompile,fn,symbol)
 
--BOOT_-EVAL_-DEFINITION_-FROM_-FILE(fn,symbol)==
--   BOOT_-DO_-SOMETHING_-TO_-DEFINITION_-FROM_-FILE
--         (FUNCTION EVAL,fn,symbol)
 
--BOOT_-PRINT_-DEFINITION_-FROM_-FILE(fn,symbol)==
--   BOOT_-DO_-SOMETHING_-TO_-DEFINITION_-FROM_-FILE
--         (FUNCTION REALLYPRETTYPRINT,fn,symbol)
 
--shoeLoop(fun, s)==
--    while not bStreamNull s repeat
--         FUNCALL(fun, car s)
--         s:=cdr s
 
shoeAddbootIfNec s==shoeAddStringIfNec('".boot",s)
 
shoeRemovebootIfNec s==shoeRemoveStringIfNec('".boot",s)
shoeAddStringIfNec(str,s)==
       a:=STRPOS(str,s,0,nil)
       if null a
       then CONCAT(s,str)
       else s
 
shoeRemoveStringIfNec(str,s)==
       a:=STRPOS(str,s,0,nil)
       if null a
       then s
       else SUBSTRING(s,0,a)
 
-- DEFUSE prints the definitions not used and the words used and
-- not defined in the input file and common lisp.
 
DEFUSE fn==
  infn:=CONCAT(fn,'".boot")
  shoeOpenInputFile(a,infn,shoeDfu(a,fn))
 
shoeDfu(a,fn)==
  if null a
  then shoeNotFound fn
  else
     $lispWordTable:local :=MAKE_-HASHTABLE ("EQ")
     DO_-SYMBOLS(i(FIND_-PACKAGE "LISP"),HPUT($lispWordTable,i,true))
     $bootDefined:local :=MAKE_-HASHTABLE "EQ"
     $bootUsed:local :=MAKE_-HASHTABLE "EQ"
     $bootDefinedTwice:local:=nil
     $GenVarCounter:local :=0
     $bfClamming:local:=false
     shoeDefUse shoeTransformStream a
     out:=CONCAT(fn,'".defuse")
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
             b:=CONCAT(PNAME i,'" is used in ")
             bootOutLines( SSORT GETHASH(i,$bootUsed),stream,b)
 
shoeDefUse(s)==
        while not bStreamPackageNull s repeat
            defuse([],CAR s)
            s:=CDR s
 
defuse(e,x)==
     x:=stripm(x,_*PACKAGE_*,FIND_-PACKAGE '"BOOTTRAN")
     $used:local:=nil
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
     ATOM y =>
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
      if null x
      then [[],[]]
      else
         f:=car x
         [x1,x2]:=defSeparate cdr x
         if bfBeginsDollar f
         then [cons(f,x1),x2]
         else [x1,cons(f,x2)]
unfluidlist x==
    NULL x => []
    ATOM x=> [x]
    x is ["&REST",y]=> [y]
    cons(car x,unfluidlist cdr x)
 
defusebuiltin x==  GETHASH(x,$lispWordTable)
 
bootOut (l,outfn)==
      for i in l repeat shoeFileLine (CONCAT ('"   ",PNAME i),outfn)
 
CLESSP(s1,s2)==not(SHOEGREATERP(s1,s2))
SSORT l == SORT(l,function CLESSP)
 
bootOutLines(l,outfn,s)==
  if null l
  then shoeFileLine(s,outfn)
  else
     a:=PNAME car l
     if #s +#a > 70
     then
          shoeFileLine(s,outfn)
          bootOutLines(l,outfn,'" ")
     else bootOutLines(cdr l,outfn,CONCAT(s,'" ",a))
 
 
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
     $lispWordTable:local :=MAKE_-HASHTABLE ("EQ")
     DO_-SYMBOLS(i(FIND_-PACKAGE "LISP"),HPUT($lispWordTable,i,true))
     $bootDefined:local :=MAKE_-HASHTABLE "EQ"
     $bootUsed:local :=MAKE_-HASHTABLE "EQ"
     $GenVarCounter:local :=0
     $bfClamming:local:=false
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
 
--FC (name,fn)== shoeGeneralFC(function BOOT,name,fn)
 
FBO (name,fn)== shoeGeneralFC(function BO,name,fn)
 
FEV(name,fn)== shoeGeneralFC(function EVAL_-BOOT_-FILE,name,fn)
 
shoeGeneralFC(f,name,fn)==
   $bfClamming:local:=false
   $GenVarCounter:local := 0
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
        dq:=CAR str
        cons([[CAR line for line in  shoeDQlines dq]],CDR str)
 
--shoeLines lines ==  [CAR line for line in lines]
 
--shoeFindAndDoSomething2(fun,fn,name,a)==
--   if null a
--   then shoeNotFound fn
--   else
--      [lines,b]:=shoePackageStartsAt([],#name,name, shoeInclude
--                        bAddLineNumber(bRgen a,bIgen 0))
--      if bStreamNull b
--      then
--           shoeConsole CONCAT (name,'" not found in ",fn)
--           []
--      else
--         if null lines
--         then shoeConsole '")package not found"
--         $GenVarCounter:local := 0
--         shoeLoopPackage(fun,shoeTransform2 b,lines)
 
--shoeLoopPackage(fun, s,lines)==
--    while not bStreamNull s repeat
--         FUNCALL(fun, append (reverse lines,car s))
--         s:=cdr s
--    true
stripm (x,pk,bt)==
   ATOM x =>
             IDENTP x =>
                 SYMBOL_-PACKAGE x = bt => INTERN(PNAME x,pk)
                 x
             x
   CONS(stripm(CAR x,pk,bt),stripm(CDR x,pk,bt))
 
shoePCompile  fn==
    fn:=stripm(fn,_*PACKAGE_*,FIND_-PACKAGE '"BOOTTRAN")
    fn is ['DEFUN,name,bv,:body]=>
          COMPILE (name,['LAMBDA,bv,:body])
    EVAL fn
 
FC(name,fn)==
   $GenVarCounter:local := 0
   infn:=shoeAddbootIfNec fn
   shoeOpenInputFile(a,infn,shoeFindName(fn,name, a))
 
shoeFindName(fn,name,a)==
     lines:=shoeFindLines(fn,name,a)
     shoePCompileTrees shoeTransformString lines
 
shoePCompileTrees s==
    while not bStreamPackageNull s repeat
         REALLYPRETTYPRINT shoePCompile car s
         s:=cdr s
 
bStreamPackageNull s==
         a := _*PACKAGE_*
         IN_-PACKAGE '"BOOTTRAN"
         b:=bStreamNull s
         setCurrentPackage a
         b
 
PSTTOMC string==
   $GenVarCounter:local := 0
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
   $GenVarCounter:local := 0
   result := shoeConsoleTrees shoeTransformString string
   setCurrentPackage callingPackage
   result


defaultBootToLispFile file ==
  CONCAT(shoeRemovebootIfNec file,'".clisp")

translateBootFile(progname, options, file) ==
  outFile := getOutputPathname(options)
  BOOTTOCL(file, ENOUGH_-NAMESTRING outFile)

compileBootHandler(progname, options, file) ==
  intFile := BOOTTOCL(file, defaultBootToLispFile file)
  intFile => 
    objFile := compileLispHandler(progname, options, intFile)
    DELETE_-FILE intFile
    objFile
  nil

associateRequestWithFileType(Option '"translate", '"boot", 
                             function translateBootFile)
associateRequestWithFileType(Option '"compile", '"boot", 
                             function compileBootHandler)
