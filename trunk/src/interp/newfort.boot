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


import macros
namespace BOOT

$fortranArrayStartingIndex := 0

--% Translation of Expression to FORTRAN
assignment2Fortran1(name,e) ==
  $fortError : local := nil
  checkLines fortran2Lines statement2Fortran ["=",name,e]

integerAssignment2Fortran1(name,e) ==
  $fortError : local := nil
  $fortInts2Floats : local := nil
  checkLines fortran2Lines statement2Fortran ["=",name,e]

statement2Fortran e ==
  -- takes an object of type Expression and returns a list of
  -- strings. Any part of the expression which is a list starting
  -- with 'FORTRAN is merely passed on in the list of strings. The
  -- list of strings may contain '"%l".
  -- This is used when formatting e.g. a DO loop from Lisp
  $exp2FortTempVarIndex : local := 0
  $fortName : local := "DUMMY"
  $fortInts2Floats : local := nil
  fortranCleanUp exp2Fort1 segment fortPre exp2FortOptimize outputTran e

expression2Fortran e ==
  -- takes an object of type Expression and returns a list of
  -- strings. Any part of the expression which is a list starting
  -- with 'FORTRAN is merely passed on in the list of strings. The
  -- list of strings may contain '"%l".
  $exp2FortTempVarIndex : local := 0
  $fortName : local := newFortranTempVar()
  $fortInts2Floats : local := nil
  fortranCleanUp exp2Fort1 segment fortPre exp2FortOptimize outputTran e

expression2Fortran1(name,e) ==
  -- takes an object of type Expression and returns a list of
  -- strings. Any part of the expression which is a list starting
  -- with 'FORTRAN is merely passed on in the list of strings. The
  -- list of strings may contain '"%l".
  $exp2FortTempVarIndex : local := 0
  $fortName : local := name
  fortranCleanUp exp2Fort1 segment fortPre exp2FortOptimize outputTran e

newFortranTempVar() ==
  $exp2FortTempVarIndex := 1 + $exp2FortTempVarIndex
  newVar := makeSymbol strconc('"T",toString $exp2FortTempVarIndex)
  updateSymbolTable(newVar,$defaultFortranType)
  newVar
 
fortranCleanUp l ==
  -- takes reversed list and cleans up a bit, putting it in
  -- correct order
  oldTok := nil
  m := nil
  for e in l repeat
    if not (oldTok = '"-" and e = '"+") then m := [e,:m]
    oldTok := e
  m
 
exp2Fort1 l ==
  s := nil
  for e in l repeat s := [:exp2Fort2(e,0,nil),:s]
  s
 
exp2Fort2(e,prec,oldOp) ==
  null e    => nil
  e isnt [.,:.] => [object2String e]
  e is [ "=",lhs,rhs] or e is [ '"=",lhs,rhs] =>
    ['"%l",:exp2Fort2(rhs,prec,'"="),'"=",:exp2Fort2(lhs,prec,'"=")]
 
  unaryOps    := ['"-",'"^",'"~"]
  unaryPrecs  := [700,260,50]
  binaryOps   := ['"|",'"**",'"/",'".LT.",'".GT.",'".EQ.",'".LE.",'".GE.", _
                  '"OVER",'".AND.",'".OR."]
  binaryPrecs := [0, 900, 800, 400, 400, 400, 400, 400, 800, 70, 90]
  naryOps     := ['"-",'"+",'"*",'",",'" ",'"ROW",'""]
  naryPrecs   := [700,  700, 800,  110,   0,     0,  0]
  nonUnaryOps := append(binaryOps,naryOps)
  [op,:args] := e
  op := object2String op
  nargs := #args
  nargs = 0 => exp2FortFn(op,args,0)
  nargs = 1 =>
    (p := position(op,unaryOps)) > -1 =>
      nprec := unaryPrecs.p
      s := [:exp2Fort2(first args,nprec,op),op]
      op = '"-" and first args isnt [.,:.] => s
      op = oldOp and member(op,['"*",'"+"]) => s
      nprec <= prec => ['")",:s,'"("]
      s
    exp2FortFn(op,args,nargs)
  op = '"CMPLX" =>
    ['")",:exp2Fort2(second args, prec, op),'",",:exp2Fort2(first args,prec,op),'"("]
  member(op,nonUnaryOps) =>
    if nargs > 0 then arg1 := first args
    nargs = 1 and member(op, '("+" "*")) => exp2Fort2(arg1,prec,op)
    if nargs > 1 then arg2 := second args
    p := position(op,binaryOps)
    if p = -1
      then
        p := position(op,naryOps)
        nprec := naryPrecs.p
      else nprec := binaryPrecs.p
    s := nil
    for arg in args repeat
      op = '"+" and (arg is [m,a]) and member(m,'(_- "=")) =>
        if not s then s := ['junk]
        s:= [op,:exp2Fort2(a,nprec,op),'"-",:rest s]
      s := [op,:exp2Fort2(arg,nprec,op),:s]
    s := rest s
    op = oldOp and member(op,['"*",'"+"]) => s
    nprec <= prec => ['")",:s,'"("]
    s
  exp2FortFn(op,args,nargs)
 
 
exp2FortFn(op,args,nargs) ==
  s := ['"(",op]
  while args repeat
    s := ['",",:exp2Fort2(first args,0,op),:s]
    args := rest args
  if nargs > 0 then ['")",:rest s]
  else ['")",:s]
 
 
--% Optimization of Expression
 
exp2FortOptimize e ==
  -- $fortranOptimizationLevel means:
  --   0         just extract arrays
  --   1         extract common subexpressions
  --   2         try to optimize computing of powers
  $exprStack : local := nil
  e isnt [.,:.] => [e]
  $fortranOptimizationLevel = 0 =>
    e1 := exp2FortOptimizeArray e
    reverse! [e1,:$exprStack]
  e := minimalise e
  for e1 in exp2FortOptimizeCS  e repeat
    e2 := exp2FortOptimizeArray e1
    $exprStack := [e2,:$exprStack]
  reverse! $exprStack

 
exp2FortOptimizeCS e ==
  $fortCsList : local := nil
  $fortCsHash : local := hashTable 'EQ
  $fortCsExprStack : local := nil
  $fortCsFuncStack : local := nil
  f := exp2FortOptimizeCS1 e
  reverse! [f,:$fortCsList]
 
-- bug fix to beenHere 
-- Thu Nov 05 12:01:46 CUT 1992 , Author: TTT
-- Used in exp2FortOprtimizeCS 
-- Original file : newfort.boot
beenHere(e,n) ==
  n.0 := n.0 + 1                      -- increase count (initially 1)
  n.0 = 2 =>                          -- first time back again
    var := n.1 := newFortranTempVar() -- stuff n.1 with new var
    exprStk := n.2                    -- get expression
    if exprStk then
-- using COPY-TREE : RPLAC does not smash $fortCsList
-- which led to inconsistencies in assignment of temp. vars.
      $fortCsList := copyTree [['"=",var,e],:$fortCsList]
      loc := first exprStk
      fun := first n.3
      fun = 'CAR =>
        loc.first := var
      fun = 'CDR =>
        if cons? rest loc
          then loc.rest := [var]
          else loc.rest := var
      SAY '"whoops"
    var
  n.1                     -- been here before, so just get variable


exp2FortOptimizeCS1 e ==
  -- we do nothing with atoms or simple lists containing atoms
  e isnt [.,:.] or (first e isnt [.,:.] and null rest e) => e
  e is [op,arg] and object2Identifier op = "-" and arg isnt [.,:.] => e

  -- see if we have been here before
  not (object2Identifier first e in '(ROW AGGLST)) and
    (n := tableValue($fortCsHash,e)) => beenHere(e,n) -- where

  -- descend sucessive CARs of CDRs of e
  f := e
  while f repeat
    pushCsStacks(f,'CAR) where pushCsStacks(x,y) ==
      $fortCsExprStack := [x,:$fortCsExprStack]
      $fortCsFuncStack := [y,:$fortCsFuncStack]
    f.first := exp2FortOptimizeCS1 first f
    popCsStacks(0) where popCsStacks(x) ==
      $fortCsFuncStack := rest $fortCsFuncStack
      $fortCsExprStack := rest $fortCsExprStack
    g := rest f
    -- check to see of we have an non-nil atomic CDR
    g and g isnt [.,:.] =>
      pushCsStacks(f,'CDR)
      f.rest := exp2FortOptimizeCS1 g
      popCsStacks(0)
      f := nil
    f := g

  object2Identifier first e in '(ROW AGGLST) => e

  -- see if we have already seen this expression
  n := tableValue($fortCsHash,e)
  null n =>
    n := vector [1,nil,$fortCsExprStack,$fortCsFuncStack]
    tableValue($fortCsHash,e) := n
    e
  beenHere(e,n)


 
exp2FortOptimizeArray e ==
  -- this handles arrays
  e isnt [.,:.] => e
  [op,:args] := e
  op1 := object2Identifier op
  op1 in '(BRACE BRACKET) =>
    args is [['AGGLST,:elts]] =>
      LISTP first elts and first first elts in '(BRACE BRACKET) => fortError1 e
      -- var := newFortranTempVar()
      var := $fortName
      $exprStack := [[op,var,['AGGLST,:exp2FortOptimizeArray elts]],
        :$exprStack]
      var
  op1 = 'MATRIX =>
    -- var := newFortranTempVar()
    var := $fortName
    -- args looks like [nil,[ROW,...],[ROW,...]]
    $exprStack := [[op,var,:exp2FortOptimizeArray args],:$exprStack]
    var
  [exp2FortOptimizeArray op,:exp2FortOptimizeArray args]

 
--% FORTRAN Line Breaking
 
fortran2Lines f ==
  -- f is a list of strings
  -- returns: a list of strings where each string is a valid
  -- FORTRAN line in fixed form
 
  -- collect strings up to first %l or end of list. Then feed to
  -- fortran2Lines1.
  fs := nil
  lines := nil
  while f repeat
    while f and (ff := first(f)) ~= '"%l" repeat
      fs := [ff,:fs]
      f := rest f
    if f and first(f) = '"%l" then f := rest f
    lines := append(fortran2Lines1 reverse! fs,lines)
    fs := nil
  reverse! lines
 
fortran2Lines1 f ==
  -- f is a list of strings making up 1 FORTRAN statement
  -- return: a reverse list of FORTRAN lines
  normPref := makeString $fortIndent
  contPref := strconc("     &",makeString($fortIndent-6))
  lines := nil
  ll := $fortIndent
  while f repeat
    ok := true
    line := normPref
    ff := first f
    while ok repeat
      (ll + (sff := # ff)) <= $fortLength =>
        ll := ll + sff
        line := strconc(line,ff)
        f := rest f
        if f then ff := first f
        else ok := nil
      -- fill the line out to exactly $fortLength spaces if possible by splitting
      -- up symbols.  This is helpful when doing the segmentation
      -- calculations, and also means that very long strings (e.g. numbers
      -- with more than $fortLength-$fortIndent digits) are printed in a
      -- legal format. MCD
      if (ll < $fortLength) and (ll + sff) > $fortLength then
        spaceLeft := $fortLength - ll
        line := strconc(line,subSequence(ff,0,spaceLeft))
        ff := subSequence(ff,spaceLeft)
      lines := [line,:lines]
      ll := $fortIndent
      line := contPref
    if ll > $fortIndent then lines := [line,:lines]
  lines
 
-- The Fortran error functions
fortError1 u ==
  $fortError := "t"
  sayErrorly("Fortran translation error",
             "   No corresponding Fortran structure for:")
  mathPrint u
 
fortError(u,v) ==
  $fortError := "t"
  msg := strconc('"   ",STRINGIMAGE u);
  sayErrorly("Fortran translation error",msg)
  mathPrint v
 
--% Top Level Things to Call
-- The names are the same as those used in the old fortran code

dispStatement x ==
  $fortError : local := nil
  displayLines fortran2Lines statement2Fortran x


getStatement(x,ints2Floats?) ==
  $fortInts2Floats : local := ints2Floats?
  $fortError : local := nil
  checkLines fortran2Lines statement2Fortran x

fortexp0 x ==
  f := expression2Fortran x
  p := position('"%l",f)
  p < 0 => f
  l := nil
  while p < 0 repeat
    [t,:f] := f
    l := [t,:l]
  reverse! ['"...",:l]

++ This formatting routine is essentially used to print
++ values/expressions used to instantiate constructors.
formatAsFortranExpression x ==
  $fortInts2Floats: local := false
  fortranCleanUp exp2Fort1 segment fortPre outputTran x

 
dispfortexp x ==
  if x isnt [.,:.] or x is [op,:.] and
    not (object2Identifier op in '(_= MATRIX construct ))
  then
      var := makeSymbol strconc('"R",object2String $IOindex)
      x := ['"=",var,x]
  dispfortexp1 x
 
dispfortexpf (xf, fortranName) ==
  $fortError : local := nil
  linef := fortran2Lines BUTLAST(expression2Fortran1(fortranName,xf),2)
  displayLines linef

dispfortexpj (xj, fortranName) ==
  $fortName : local := fortranName
  $fortError : local := nil
  linej := fortran2Lines BUTLAST(expression2Fortran1(fortranName,xj),2)
  displayLines linej


dispfortexp1 x ==
  $fortError : local := nil
  displayLines fortran2Lines expression2Fortran x

getfortexp1 x ==
  $fortError : local := nil
  checkLines fortran2Lines expression2Fortran x

displayLines1 lines ==
  for l in lines repeat
    PRINC(l,$fortranOutputStream)
    writeNewline $fortranOutputStream

displayLines lines ==
  if not $fortError then displayLines1 lines
 
checkLines lines ==
  $fortError => []
  lines

dispfortarrayexp (fortranName,m) ==
  $fortError : local := nil
  displayLines fortran2Lines BUTLAST(expression2Fortran1(fortranName,m),2)

getfortarrayexp(fortranName,m,ints2floats?) ==
  $fortInts2Floats : local := ints2floats?
  $fortError : local := nil
  checkLines fortran2Lines BUTLAST(expression2Fortran1(fortranName,m),2)

 
-- Globals
$currentSubprogram := nil
$symbolTable := nil
 


--fix [x,exp x]
 
------------ exp2FortSpecial.boot --------------------
 
exp2FortSpecial(op,args,nargs) ==
  op = "CONCAT" and first args in ["<",">","<=",">=","~","and","or"] =>
    mkFortFn(first args,CDADAR rest args,#(CDADAR rest args))
  op = "CONCAT" and second(args)="EQ" =>
    mkFortFn("EQ",[first args, third args],2)
  --the next line is NEVER used by FORTRAN code but is needed when
  --  called to get a linearized form for the browser
  op = 'QUOTE =>
    (arg := first args) isnt [.,:.] => STRINGIMAGE arg
    tailPart := strconc/[strconc('",",x) for x in rest arg]
    strconc('"[",first arg,tailPart,'"]")
  op = "PAREN" =>
    args := first args
    not(first(args)="CONCATB") => fortError1 [op,:args]
    -- Have a matrix element
    mkMat(args)
  op = "SUB" =>
    $fortInts2Floats : local := nil
    mkFortFn(first args,rest args,#(rest args))
  op in ["BRACE","BRACKET"] =>
    args is [var,['AGGLST,:elts]] =>
      var := object2String var
      si := $fortranArrayStartingIndex
      hidim := #elts - 1 + si
      if LISTP first elts and #elts=1 and first elts is [sOp,:sArgs] then
        member(sOp, ['"SEGMENT","SEGMENT"]) =>
          #sArgs=1 => fortError1 first elts
          not(integer?(first sArgs) and integer?(second sArgs)) =>
            fortError("Cannot expand segment: ",first elts)
          first sArgs > second sArgs => fortError1
            '"Lower bound of segment exceeds upper bound."
          for e in first sArgs .. second sArgs for i in si.. repeat
            $exprStack := [["=",[var,object2String i],fortPre1(e)],:$exprStack]
      for e in elts for i in si.. repeat
        $exprStack := [["=",[var,object2String i],fortPre1(e)],:$exprStack]
    fortError1 [op,:args]
  op in ["CONCAT","CONCATB"] =>
    nargs = 0 => nil
    nargs = 1 => fortPre1 first args
    nargs = 2 and member(second args, ["!",'"!"]) =>
      mkFortFn("FACTORIAL",[first args],1)
    fortError1 [op,:args]
  member(op, ['"MATRIX","MATRIX"]) =>
    args is [var, =nil,:rows] =>
      var := object2String var
      nrows := #rows - 1
      ncols := #(rest first rows) - 1
      si := $fortranArrayStartingIndex
      for r in rows for rx in si.. repeat
        for c in rest r for cx in si.. repeat
          $exprStack := [["=",[var,object2String rx,object2String cx],
                          fortPre1(c)],:$exprStack]
    fortError1 [op,:args]
  fortError1 [op,:args]

mkMat(args) ==
  $fortInts2Floats : local := nil
  mkFortFn(second args,rest rest args,#(rest rest args))

 
mkFortFn(op,args,nargs) ==
  [fortranifyFunctionName(STRINGIMAGE op,nargs), 
   :[fortPre1 x for x in args]]
 
fortranifyFunctionName(op,nargs) ==
  op = '"<" => '".LT."
  op = '">" => '".GT."
  op = '"<=" => '".LE."
  op = '">=" => '".GE."
  op = '"EQ" => '".EQ."
  op = '"and" => '".AND."
  op = '"or" => '".OR."
  op = '"~" => '".NOT."
  fortranifyIntrinsicFunctionName(op,nargs)

fortranifyIntrinsicFunctionName(op,nargs) ==
  $useIntrinsicFunctions =>
    intrinsic := if op = '"acos" then '"ACOS"
    else if op = '"asin" then '"ASIN"
    else if op = '"atan" then
      nargs = 2 => '"ATAN2"
      '"ATAN"
    else if op = '"cos" then '"COS"
    else if op = '"cosh" then '"COSH"
    else if op = '"cot" then '"COTAN"
    else if op = '"erf" then '"ERF"
    else if op = '"exp" then '"EXP"
    else if op = '"log" then '"LOG"
    else if op = '"log10" then '"LOG10"
    else if op = '"sin" then '"SIN"
    else if op = '"sinh" then '"SINH"
    else if op = '"sqrt" then '"SQRT"
    else if op = '"tan" then '"TAN"
    else if op = '"tanh" then '"TANH"
    intrinsic =>
      $intrinsics := ADJOIN(intrinsic,$intrinsics)
      intrinsic
    op
  $fortranPrecision = 'double =>
    op = '"acos" => '"DACOS"
    op = '"asin" => '"DASIN"
    op = '"atan" =>
      nargs = 2 => '"DATAN2"
      '"DATAN"
    op = '"cos" => '"DCOS"
    op = '"cosh" => '"DCOSH"
    op = '"cot" => '"DCOTAN"
    op = '"erf" => '"DERF"
    op = '"exp" => '"DEXP"
    op = '"log" => '"DLOG"
    op = '"log10" => '"DLOG10"
    op = '"sin" => '"DSIN"
    op = '"sinh" => '"DSINH"
    op = '"sqrt" => '"DSQRT"
    op = '"tan" => '"DTAN"
    op = '"tanh" => '"DTANH"
    op = '"abs" => '"DABS"
    op
  op = '"acos" => '"ACOS"
  op = '"asin" => '"ASIN"
  op = '"atan" =>
    nargs = 2 => '"ATAN2"
    '"ATAN"
  op = '"cos" => '"COS"
  op = '"cosh" => '"COSH"
  op = '"cot" => '"COTAN"
  op = '"erf" => '"ERF"
  op = '"exp" => '"EXP"
  op = '"log" => '"ALOG"
  op = '"log10" => '"ALOG10"
  op = '"sin" => '"SIN"
  op = '"sinh" => '"SINH"
  op = '"sqrt" => '"SQRT"
  op = '"tan" => '"TAN"
  op = '"tanh" => '"TANH"
  op = '"abs" => '"ABS"
  op

--------------------------format.boot------------------------------------------

-- These functions are all used by FortranCode and FortranProgram.
-- Those used by FortranCode have been changed to return a list of
-- lines rather than print them directly, thus allowing us to catch
-- and display type declarations for temporary variables.
--  MCD 25/3/93

indentFortLevel(i) ==
  $maximumFortranExpressionLength := $maximumFortranExpressionLength -2*i
  $fortIndent := $fortIndent + 2*i

changeExprLength(i) ==
  $maximumFortranExpressionLength := $maximumFortranExpressionLength + i

fortFormatDo(var,lo,hi,incr,lab) ==
  $fortError : local := nil
  $fortInts2Floats : local := nil
  incr=1 =>
    checkLines fortran2Lines
      ['"DO ",STRINGIMAGE lab,'" ",STRINGIMAGE var,'"=",:statement2Fortran lo,_
       '",", :statement2Fortran hi]
  checkLines fortran2Lines
    ['"DO ",STRINGIMAGE lab,'" ",STRINGIMAGE var,'"=",:statement2Fortran lo,_
     '",", :statement2Fortran hi,'",",:statement2Fortran incr]

fortFormatIfGoto(switch,label) ==
  changeExprLength(-8) -- Leave room for IF( ... )GOTO
  $fortError : local := nil
  if first(switch) = "NULL" then switch := second switch
  r := reverse! statement2Fortran switch
  changeExprLength(8)
  l := ['")GOTO ",STRINGIMAGE label]
  while r and not(first(r) = '"%l") repeat
    l := [first(r),:l]
    r := rest(r)
  checkLines fortran2Lines reverse! [:reverse! l,'"IF(",:r]

fortFormatLabelledIfGoto(switch,label1,label2) ==
  changeExprLength(-8) -- Leave room for IF( ... )GOTO
  $fortError : local := nil
  if LISTP(switch) and first(switch) = "NULL" then switch := second switch
  r := reverse! statement2Fortran switch
  changeExprLength(8)
  l := ['")GOTO ",STRINGIMAGE label2]
  while r and not(first(r) = '"%l") repeat
    l := [first(r),:l]
    r := rest(r)
  labString := STRINGIMAGE label1
  for i in #(labString)..5 repeat labString := strconc(labString,'" ")
  lines := fortran2Lines reverse! [:reverse! l,'"IF(",:r]
  lines := [strconc(labString,subSequence(first lines,6)),:rest lines]
  checkLines lines

fortFormatIf(switch) ==
  changeExprLength(-8) -- Leave room for IF( ... )THEN
  $fortError : local := nil
  if LISTP(switch) and first(switch) = "NULL" then switch := second switch
  r := reverse! statement2Fortran switch
  changeExprLength(8)
  l := ['")THEN"]
  while r and not(first(r) = '"%l") repeat
    l := [first(r),:l]
    r := rest(r)
  checkLines fortran2Lines reverse! [:reverse! l,'"IF(",:r]

fortFormatElseIf(switch) ==
  -- Leave room for IF( ... )THEN
  changeExprLength(-12)
  $fortError : local := nil
  if LISTP(switch) and first(switch) = "NULL" then switch := second switch
  r := reverse! statement2Fortran switch
  changeExprLength(12)
  l := ['")THEN"]
  while r and not(first(r) = '"%l") repeat
    l := [first(r),:l]
    r := rest(r)
  checkLines fortran2Lines reverse! [:reverse! l,'"ELSEIF(",:r]

fortFormatHead(returnType,name,args) ==
  $fortError : local := nil
  $fortranSegment : local := nil
  -- if returnType = '"_"_(_)_"" then 
  if returnType = '"void" then
    asp := ['"SUBROUTINE "]
    changeExprLength(l := -11)
  else
    asp := [s := checkType STRINGIMAGE returnType,'" FUNCTION "]
    changeExprLength(l := -10-#(s))
  displayLines fortran2Lines [:asp,:statement2Fortran [name,:CDADR args] ]
  changeExprLength(-l)

checkType ty ==
  ty := stringUpcase STRINGIMAGE ty
  $fortranPrecision = "double" =>
    ty = '"REAL" => '"DOUBLE PRECISION"
    ty = '"COMPLEX" => '"DOUBLE COMPLEX"
    ty
  ty

mkParameterList l ==
  [par2string(u) for u in l] where par2string u ==
      u isnt [.,:.] => STRINGIMAGE u
      u := rest second u
      apply(function strconc,[STRINGIMAGE(first u),'"(",_
               :rest [:['",",:statement2Fortran(v)] for v in rest u],'")"])

macro nameLen n ==
 +/[1+#(u) for u in n]

fortFormatTypes(typeName,names) ==
  null names => return nil
  $fortError : local := nil
  $fortranSegment : local := nil
  $fortInts2Floats : local := nil
  typeName := checkType typeName
  typeName = '"CHARACTER" =>
    fortFormatCharacterTypes([unravel(u) for u in names])
      where unravel u ==
              u isnt [.,:.] => u
              CDADR u
  fortFormatTypes1(typeName,mkParameterList names)

fortFormatTypes1(typeName,names) ==
  l := $maximumFortranExpressionLength-1-#(typeName)
  while nameLen(names) > l repeat
    n := []
    ln := 0
    while (ln := ln + #(first names) + 1) < l repeat
      n := [first names,:n]
      names := rest names
    displayLines fortran2Lines [typeName,'" ",:addCommas n]
  displayLines fortran2Lines [typeName,'" ",:addCommas names]

insertEntry(size,el,aList) ==
  entry := assoc(size,aList)
  null entry => [[size,:[el]],:aList]
  entry.rest := [el,:rest entry]
  aList

fortFormatCharacterTypes(names) ==
  sortedByLength := []
  genuineArrays  := []
  for u in names repeat
    u isnt [.,:.] => sortedByLength := insertEntry(0,u,sortedByLength)
    #u=2 => sortedByLength := insertEntry(second u,first u,sortedByLength)
    genuineArrays := [u,:genuineArrays]
  for u in sortedByLength repeat
    fortFormatTypes1(mkCharName first u, [STRINGIMAGE(s) for s in rest(u)]) where
       mkCharName v == strconc("CHARACTER*(",STRINGIMAGE v,")")
  if (not null genuineArrays) then
    fortFormatTypes1('"CHARACTER",mkParameterList2 genuineArrays) where
       mkParameterList2 l ==
         [par2string(u) for u in l] where par2string u ==
             apply(function strconc,[STRINGIMAGE(first u),'"(",_
                      :rest [:['",",:statement2Fortran(v)] for v in rest u],'")"])

fortFormatIntrinsics(l) ==
  $fortError : local := nil
  null l => return nil
  displayLines fortran2Lines ['"INTRINSIC ",:addCommas(l)]
  
 
------------------ fortDec.boot --------------------
 
-- This file contains the stuff for creating and updating the Fortran symbol
-- table.
 
currentSP () ==
  -- Return the name of the current subprogram being generated
  $currentSubprogram or "MAIN"
 
updateSymbolTable(name,type) ==
    fun := ['$elt,'SYMS,'declare!]
    coercion := ['_:_:,STRING type,'FST]
    $insideCompileBodyIfTrue: local := false
    interpret([fun,quote name,coercion])
 
addCommas l ==
  not l => nil
  r := [STRINGIMAGE first l]
  for e in rest l repeat r := [STRINGIMAGE e,'",",:r]
  reverse r

$intrinsics := []
initialiseIntrinsicList() == 
  $intrinsics := []

getIntrinsicList() ==
  $intrinsics

 
-------------------- fortPre.boot ------------------
 
fortPre l ==
  -- Essentially, the idea is to fix things so that we know what size of
  -- expression we will generate, which helps segment large expressions
  -- and do transformations to double precision output etc..
  $exprStack : local := nil -- sometimes we will add elements to this in
                            -- other functions, for example when extracing
                            -- lists etc.
  for e in l repeat if new := fortPre1 e then
     $exprStack := [new,:$exprStack]
  reverse $exprStack
 
fortPre1 e ==
  -- replace spad function names by Fortran equivalents
  -- where appropriate, replace integers by floats
  -- extract complex numbers
  -- replace powers of %e by calls to EXP
  -- replace x**2 by x*x etc.
  -- replace ROOT by either SQRT or **(1./ ... )
  -- replace N-ary by binary functions
  -- strip the '%' character off objects like %pi etc..
  null e => nil
  integer?(e) =>
    $fortInts2Floats =>
      e >= 0 => fix2FortranFloat(e)
      ['"-", fix2FortranFloat(-e)]
    e
  isFloat(e) => checkPrecision(e)
  -- Keep strings as strings:
  -- string?(e) => strconc(STRING(34),e,STRING(34))
  string?(e) => e
  e = "%e" => fortPre1 ["exp" , 1]
  imags := ['"%i","%i"]
  member(e, imags) => ['"CMPLX",fortPre1(0),fortPre1(1)]
  -- other special objects
  STRINGIMAGE(e).0 = char "%" => subSequence(STRINGIMAGE e,1)
  e isnt [.,:.] => e
  [op, :args] := e
  member(op,["**" , '"**"]) =>
    [rand,exponent] := args
    rand = "%e" => fortPre1 ["exp", exponent]
    (ident? rand or string? rand) and exponent=2 => ["*", rand, rand]
    (integer? exponent and abs(exponent) < 32768) => ["**",fortPre1 rand,exponent]
    ["**", fortPre1 rand,fortPre1 exponent]
  op = "ROOT" =>
    #args = 1 => fortPreRoot ["sqrt", first args]
    [ "**" , fortPreRoot first args , [ "/" , fortPreRoot(1), fortPreRoot second args] ]
  if member(op,['"OVER", "OVER"]) then op := '"/"
  specialOps  := '(BRACKET BRACE SUB AGGLST SUPERSUB MATRIX SEGMENT ALTSUPERSUB
                   PAREN CONCAT CONCATB QUOTE STRING SIGMA  STEP IN SIGMA2
                   INTSIGN  PI PI2 INDEFINTEGRAL)
  symbolMember?(op,specialOps) => exp2FortSpecial(op,args,#args)
  member(op,['"*", "*", '"+", "+", '"-", "-"]) and (#args > 2) =>
    binaryExpr := fortPre1 [op,first args, second args]
    for i in 3..#args repeat
      binaryExpr := [op,binaryExpr,fortPre1 NTH(i-1,args)]
    binaryExpr
  -- Now look for any complex objects
  #args = 2 =>
    [arg1,arg2] := args
    member(op, ["*",'"*"]) and member(arg2, imags) => 
      ['"CMPLX",fortPre1(0),fortPre1(arg1)]
    member(op,["+",'"+"]) and member(arg2,imags) => 
      ['"CMPLX",fortPre1(arg1),fortPre1(1)]
    member(op,["+",'"+"]) and arg2 is [mop,m1,m2] and member(mop,["*",'"*"]) =>
      member(m2,imags) => ['"CMPLX",fortPre1(arg1),fortPre1(m1)]
      member(m1,imags) => ['"CMPLX",fortPre1(arg1),fortPre1(m2)]
      ["+",fortPre1 arg1,fortPre1 arg2]
    member(op,["+",'"+"]) and arg1 is [mop,m1,m2] and member(mop,["*",'"*"]) =>
      member(m2,imags) => ['"CMPLX",fortPre1(arg2),fortPre1(m1)]
      member(m1,imags) => ['"CMPLX",fortPre1(arg2),fortPre1(m2)]
      ["+",fortPre1 arg1,fortPre1 arg2]
    mkFortFn(op,args,2)
  mkFortFn(op,args,#args)

fortPreRoot e ==
-- To set $fortInts2Floats 
  $fortInts2Floats : local := true
  fortPre1 e
 
fix2FortranFloat e ==
  -- Return a Fortran float for a given integer.
  $fortranPrecision = "double" => strconc(STRINGIMAGE(e),".0D0")
  strconc(STRINGIMAGE(e),'".")
 
isFloat e ==
  float?(e) or string?(e) and FIND(char ".",e)
 
removeCharFromString(c,s) ==
  -- find c's position in s.
  k := nil
  for i in 0..maxIndex s while k = nil repeat
    stringChar(s,i) = c => k := i
  k = nil => s
  -- make a copy without c.
  s' := makeString(#s - 1)
  for i in 0..(k-1) repeat
    stringChar(s',i) := stringChar(s,i)
  for i in k..maxIndex s' repeat
    stringChar(s',i) := stringChar(s,i+1)
  s'

checkPrecision e ==
  -- Do we have a string?
  string? e and codePoint stringChar(e,0) = 34 => e
  e := removeCharFromString(char " ",STRINGIMAGE e)
  $fortranPrecision = "double" =>
    iPart := subSequence(e,0,(period:=POSITION(char ".",e))+1)
    expt  := if ePos := POSITION(char "E",e) then subSequence(e,ePos+1) else "0"
    rPart :=
      ePos => subSequence(e,period+1,ePos)
      period+1 < # e => subSequence(e,period+1)
      "0"
    strconc(iPart,rPart,'"D",expt)
  e
 
----------------- segment.boot -----------------------
 
fortExpSize e ==
  -- computes a tree reflecting the number of characters of the printed
  -- expression.
  -- The first element of a list is the "total so far", while subsequent
  -- elements are the sizes of the components.
  --
  -- This function overestimates the size because it assumes that e.g.
  -- (+ x (+ y z)) will be printed as "x+(y+z)" rather than "x+y+z"
  -- which is the actual case.
  e isnt [.,:.] => # STRINGIMAGE e
  #e > 3 => 2+fortSize [fortExpSize x for x in e]
  #e < 3 => 2+fortSize [fortExpSize x for x in e]
  [op,arg1,arg2] := e
  op := STRINGIMAGE op
  op = '"CMPLX" => 3+fortSize [fortExpSize arg1,fortExpSize arg2]
  narys := ['"+",'"*"] -- those nary ops we changed to binary
  member(op,narys) =>
    LISTP arg1 and not(op=STRINGIMAGE first arg1) =>
      2+fortSize [fortExpSize x for x in e]
    LISTP arg2 and not(op=STRINGIMAGE first arg2) =>
      2+fortSize [fortExpSize x for x in e]
    1+fortSize [fortExpSize arg1,fortExpSize arg2]
  2+fortSize [fortExpSize x for x in e]
 
fortSize e ==
  +/[elen u for u in e] where
    elen z ==
      z isnt [.,:.] => z
      first z
 
tempLen () == 1 + # toString $exp2FortTempVarIndex
 
segment l ==
  not $fortranSegment => l
  s := nil
  for e in l repeat
    if LISTP(e) and first member(e,["=",'"="]) then
      var := NTH(1,e)
      exprs := segment1(third e,
                        $maximumFortranExpressionLength-1-fortExpSize var)
      s:= [:[['"=",var,first exprs],:rest exprs],:s]
    else if LISTP(e) and first e = '"RETURN" then
      exprs := segment1(second e,
                        $maximumFortranExpressionLength-2-fortExpSize first e)
      s := [:[[first e,first exprs],:rest exprs],:s]
    else s:= [e,:s]
  reverse s
 
segment1(e,maxSize) ==
  (size := fortExpSize e) < maxSize => [e]
  expressions := nil;
  newE := [first e]
  -- Assume we have to replace each argument with a temporary variable, and
  -- that the temporary variable may be larger than we expect.
  safeSize := maxSize -  (#e-1)*(tempLen()+1) - fortExpSize newE
  for i in 2..#e repeat
    subSize := fortExpSize NTH(i-1,e)
    -- We could have a check here for symbols which are simply too big
    -- for Fortran (i.e. more than the maximum practical expression length)
    subSize <= safeSize =>
      safeSize := safeSize - subSize
      newE := [:newE,NTH(i-1,e)]
    -- this ones too big.
    exprs := segment2(NTH(i-1,e),safeSize)
    expressions := [:(rest exprs),:expressions]
    newE := [:newE,(first exprs)]
    safeSize := safeSize - fortExpSize first exprs
  [newE,:expressions]
 
segment2(e,topSize) ==
  maxSize := $maximumFortranExpressionLength -tempLen()-1
  e isnt [.,:.] => [e]
  exprs := nil
  newE  := [first e]
  topSize := topSize - fortExpSize newE
  for i in 2..#e repeat
    subE := NTH(i-1,e)
    (subSize := fortExpSize subE) > maxSize =>
      subE := segment2(subE,maxSize)
      exprs := [:(rest subE),:exprs]
      if (subSize := fortExpSize first subE) <= topSize then
        newE := [:newE,first subE]
        topSize := topSize - subSize
      else
        newVar := newFortranTempVar()
        newE := [:newE,newVar]
        exprs:=[['"=",newVar,first subE],:exprs]
        topSize := topSize - fortExpSize newVar
    newE := [:newE,subE]
    topSize := topSize - subSize
  topSize > 0 => [newE,:exprs]
  newVar := newFortranTempVar()
  [newVar,['"=",newVar,newE],:exprs]
 
