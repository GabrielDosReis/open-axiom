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
--

--
-- Abstract:
--   This file defines the Boot grammar and parser.  The parser
--   is hand-written based on `parser combinators' technology.
--


import includer
import scanner
import ast
namespace BOOTTRAN
module parser
 

bpFirstToken()==
  $stok:=
    $inputStream = nil => shoeTokConstruct("ERROR","NOMORE",shoeTokPosn $stok)
    first $inputStream
  $ttok := shoeTokPart $stok
  true
 
bpFirstTok()==
  $stok:=
    $inputStream = nil => shoeTokConstruct("ERROR","NOMORE",shoeTokPosn $stok)
    first $inputStream
  $ttok:=shoeTokPart $stok
  $bpParenCount>0 and $stok is ["KEY",:.] =>
    $ttok is "SETTAB" =>
      $bpCount:=$bpCount+1
      bpNext()
    $ttok is "BACKTAB" =>
      $bpCount:=$bpCount-1
      bpNext()
    $ttok is "BACKSET" =>
      bpNext()
    true
  true
 
bpNext() ==
  $inputStream := rest($inputStream)
  bpFirstTok()
 
bpNextToken() ==
  $inputStream := rest($inputStream)
  bpFirstToken()
 
bpState()== [$inputStream,$stack,$bpParenCount,$bpCount]
--[$inputStream,:$stack]
 
bpRestore(x)==
  $inputStream:=first x
  bpFirstToken()
  $stack:=second x
  $bpParenCount:=third x
  $bpCount:=CADDDR x
  true
 
bpPush x==$stack:=[x,:$stack]
 
bpPushId()==
  $stack:= [bfReName $ttok,:$stack]
 
bpPop1()==
  a:=first $stack
  $stack:=rest $stack
  a
 
bpPop2()==
  a:=second $stack
  $stack.rest := CDDR $stack
  a
 
bpPop3()==
  a:=third $stack
  $stack.rest.rest := CDDDR $stack
  a
 
bpIndentParenthesized f==
  $bpCount:local:=0
  a:=$stok
  bpEqPeek "OPAREN" =>
    $bpParenCount:=$bpParenCount+1
    bpNext()
    apply(f,nil) and bpFirstTok() and
	    (bpEqPeek "CPAREN" or bpParenTrap(a)) =>
      $bpParenCount:=$bpParenCount-1
      bpNextToken()
      $bpCount=0 => true
      $inputStream:=append( bpAddTokens $bpCount,$inputStream)
      bpFirstToken()
      $bpParenCount=0 =>
	bpCancel()
	true
      true
    bpEqPeek "CPAREN" =>
      bpPush  bfTuple []
      $bpParenCount:=$bpParenCount-1
      bpNextToken()
      true
    bpParenTrap(a)
  false
 
bpParenthesized f==
  a := $stok
  bpEqKey "OPAREN" =>
    apply(f,nil) and (bpEqKey "CPAREN" or bpParenTrap(a)) => true
    bpEqKey "CPAREN" =>
      bpPush bfTuple []
      true
    bpParenTrap(a)
  false
 
bpBracket f==
  a := $stok
  bpEqKey "OBRACK" =>
    apply(f,nil) and (bpEqKey "CBRACK" or bpBrackTrap(a)) =>
      bpPush bfBracket bpPop1()
    bpEqKey "CBRACK" => bpPush []
    bpBrackTrap(a)
  false
 
bpPileBracketed f==
  bpEqKey "SETTAB" => 
    bpEqKey "BACKTAB" => true
    apply(f,nil) and (bpEqKey "BACKTAB" or bpPileTrap()) =>
      bpPush bfPile bpPop1()
    false
  false
 
bpListof(f,str1,g)==
  apply(f,nil) =>
    bpEqKey str1 and (apply(f,nil) or bpTrap()) =>
      a:=$stack
      $stack:=nil
      while bpEqKey str1 and (apply(f,nil) or bpTrap()) repeat 0
      $stack:=[reverse! $stack,:a]
      bpPush FUNCALL(g, [bpPop3(),bpPop2(),:bpPop1()])
    true
  false
 
 
-- to do ,<backset>
bpListofFun(f,h,g)==
  apply(f,nil) =>
    apply(h,nil) and (apply(f,nil) or bpTrap()) =>
      a:=$stack
      $stack:=nil
      while apply(h,nil) and (apply(f,nil) or bpTrap()) repeat 0
      $stack:=[reverse! $stack,:a]
      bpPush FUNCALL(g, [bpPop3(),bpPop2(),:bpPop1()])
    true
  false
 
bpList(f,str1)==
  apply(f,nil) =>
    bpEqKey str1 and (apply(f,nil) or bpTrap()) =>
      a:=$stack
      $stack:=nil
      while bpEqKey str1 and (apply(f,nil) or bpTrap()) repeat 0
      $stack:=[reverse! $stack,:a]
      bpPush [bpPop3(),bpPop2(),:bpPop1()]
    bpPush [bpPop1()]
  bpPush nil
 
bpOneOrMore f==
  apply(f,nil)=>
    a:=$stack
    $stack:=nil
    while apply(f,nil) repeat 0
    $stack:=[reverse! $stack,:a]
    bpPush [bpPop2(),:bpPop1()]
  false
 
 
-- s must transform the head of the stack
bpAnyNo s==
  while apply(s,nil) repeat 0
  true
 
 
-- AndOr(k,p,f)= k p
bpAndOr(keyword,p,f)==
  bpEqKey keyword and (apply(p,nil) or bpTrap())
    and bpPush FUNCALL(f, bpPop1())
 
bpConditional f==
  bpEqKey "IF" and (bpWhere() or bpTrap()) and (bpEqKey "BACKSET" or true) =>
    bpEqKey "SETTAB" =>
      bpEqKey "THEN" =>
	(apply(f,nil) or bpTrap()) and bpElse(f) and bpEqKey "BACKTAB"
      bpMissing "THEN"
    bpEqKey "THEN" => (apply(f,nil) or bpTrap()) and bpElse(f)
    bpMissing "then"
  false
 
bpElse(f)==
  a:=bpState()
  bpBacksetElse() =>
    (apply(f,nil) or bpTrap()) and
      bpPush bfIf(bpPop3(),bpPop2(),bpPop1())
  bpRestore a
  bpPush bfIfThenOnly(bpPop2(),bpPop1())
 
bpBacksetElse()==
  bpEqKey "BACKSET" => bpEqKey "ELSE"
  bpEqKey "ELSE"
 
bpEqPeek s == 
  $stok is ["KEY",:.] and symbolEq?(s,$ttok)
 
bpEqKey s ==
  $stok is ["KEY",:.] and symbolEq?(s,$ttok) and bpNext()

bpEqKeyNextTok s ==   
  $stok is ["KEY",:.] and symbolEq?(s,$ttok) and bpNextToken()
 
bpPileTrap()   == bpMissing  "BACKTAB"
bpBrackTrap(x) == bpMissingMate("]",x)
bpParenTrap(x) == bpMissingMate(")",x)
 
bpMissingMate(close,open)==
  bpSpecificErrorAtToken(open, '"possibly missing mate")
  bpMissing close
 
bpMissing s==
  bpSpecificErrorHere strconc(PNAME s,'" possibly missing")
  throw 'TRAPPED : BootParserException
 
bpCompMissing s == bpEqKey s or bpMissing s
 
bpTrap()==
  bpGeneralErrorHere()
  throw 'TRAPPED : BootParserException
 
bpRecoverTrap()==
  bpFirstToken()
  pos1 := shoeTokPosn $stok
  bpMoveTo 0
  pos2 := shoeTokPosn $stok
  bpIgnoredFromTo(pos1, pos2)
  bpPush  [['"pile syntax error"]]
 
bpListAndRecover(f)==
  a := $stack
  b := nil
  $stack := nil
  done := false
  c := $inputStream
  while not done repeat
    found :=
      try apply(f,nil)
      catch(e: BootParserException) => e
    if found is "TRAPPED"
    then
       $inputStream:=c
       bpRecoverTrap()
    else if not found
	 then
	   $inputStream:=c
	   bpGeneralErrorHere()
	   bpRecoverTrap()
    if bpEqKey "BACKSET"
    then
       c := $inputStream
    else if bpEqPeek "BACKTAB"  or $inputStream = nil
	 then
	    done := true
	 else
	   $inputStream := c
	   bpGeneralErrorHere()
	   bpRecoverTrap()
	   if bpEqPeek "BACKTAB"  or $inputStream = nil
	   then done:=true
	   else
	       bpNext()
	       c := $inputStream
    b := [bpPop1(),:b]
  $stack := a
  bpPush reverse! b
 
bpMoveTo n==
   $inputStream = nil  => true
   bpEqPeek "BACKTAB" =>
     n=0  => true
     bpNextToken()
     $bpCount:=$bpCount-1
     bpMoveTo(n-1)
   bpEqPeek "BACKSET" =>
     n=0  => true
     bpNextToken()
     bpMoveTo n
   bpEqPeek "SETTAB"  =>
     bpNextToken()
     bpMoveTo(n+1)
   bpEqPeek "OPAREN"  =>
     bpNextToken()
     $bpParenCount:=$bpParenCount+1
     bpMoveTo n
   bpEqPeek "CPAREN"  =>
     bpNextToken()
     $bpParenCount:=$bpParenCount-1
     bpMoveTo n
   bpNextToken()
   bpMoveTo n
 
-- A fully qualified name could be interpreted as a left reduction
-- of an '::' infix operator.  At the moment, we don't use
-- that general interpretation.

-- When this routine is called, a symbol is already pushed on the
-- stack.  When this routine finished execution, we have either
-- reduced a '::' and a name, or nothing.  In either case, a 
-- symbol is present on the stack.
bpQualifiedName() ==
  bpEqPeek "COLON-COLON" =>
    bpNext()
    $stok is ["ID",:.] and bpPushId() and bpNext()
      and bpPush bfColonColon(bpPop2(), bpPop1())
  false

++ Name:
++   ID
++   Name :: ID
bpName() ==
  $stok is ["ID",:.] =>
    bpPushId()
    bpNext()
    bpAnyNo function bpQualifiedName
  false


++ Constant:
++   INTEGER
++   FLOAT
++   LISP
++   LISPEXPR
++   LINE
++   QUOTE S-Expression
++   STRING
bpConstTok() ==
  shoeTokType $stok in '(INTEGER FLOAT) =>
    bpPush $ttok
    bpNext()
  $stok is ["LISP",:.] => bpPush %Lisp $ttok and bpNext()
  $stok is ["LISPEXP",:.] => bpPush $ttok and bpNext()
  $stok is ["LINE",:.] => bpPush ["+LINE", $ttok] and bpNext()
  bpEqPeek "QUOTE" =>
    bpNext()
    (bpSexp() or bpTrap()) and
         bpPush bfSymbol bpPop1()
  bpString()


++ Subroutine of bpExportItem.  Parses tails of ExportItem.
bpExportItemTail() ==
  bpEqKey "BEC" and (bpAssign() or bpTrap()) and
    bpPush %Assignment(bpPop2(), bpPop1())
      or bpSimpleDefinitionTail()

++ ExportItem:
++   Structure
++   TypeAliasDefinition
++   Signature
++   Signature := Where
++   Signature  == Where
bpExportItem() ==
  bpEqPeek "STRUCTURE" => bpStruct()
  a := bpState()
  bpName() =>
    bpEqPeek "COLON" =>
      bpRestore a
      bpSignature() or bpTrap()
      bpExportItemTail() or true
    bpRestore a
    bpTypeAliasDefition()
  false

++ ExportItemList:
++    Signature
++    ExportItemList Signature
bpExportItemList() ==
  bpListAndRecover function bpExportItem

++ ModuleInterface:
++   WHERE pile-bracketed ExporItemList
bpModuleInterface() ==
  bpEqKey "WHERE" =>
    bpPileBracketed function bpExportItemList
      or (bpExportItem() and bpPush [bpPop1()])
        or bpTrap()
  bpPush nil

++ ModuleExports:
++   OPAREN IdList CPAREN
bpModuleExports() ==
  bpParenthesized function bpIdList => bpPush bfUntuple bpPop1()
  bpPush nil

++ Parse a module definitoin
++   Module:
++     MODULE Name OptionalModuleExports OptionalModuleInterface
bpModule() ==
  bpEqKey "MODULE" => 
    bpName() or bpTrap()
    bpModuleExports()
    bpModuleInterface()
    bpPush %Module(bpPop3(),bpPop2(),bpPop1())
  nil

++ Parse a module import, or a import declaration for a foreign entity.
++ Import:
++    IMPORT Signature FOR Name
++    IMPORT Name
++    IMPORT Namespace
bpImport() ==
  bpEqKey "IMPORT" =>
    bpNamespace() => bpPush %Import bpPop1()
    a := bpState()
    bpName() or bpTrap()
    bpEqPeek "COLON" =>
      bpRestore a
      (bpSignature() or bpTrap()) and 
        (bpEqKey "FOR" or bpTrap()) and
           (bpName() or bpTrap()) and
              bpPush %ImportSignature(bpPop1(), bpPop1())
    bpPush %Import bpPop1()
  false

++
++ Namespace:
++    NAMESPACE Name
bpNamespace() ==
  bpEqKey "NAMESPACE" and (bpName() or bpDot()) and
    bpPush bfNamespace bpPop1()

-- Parse a type alias defnition:
--    type-alias-definition: 
--          identifier <=> logical-expression
bpTypeAliasDefition() ==
  (bpTerm function bpIdList or bpTrap()) and 
    bpEqKey "TDEF" and bpLogical() and
      bpPush %TypeAlias(bpPop2(), bpPop1())

++ Parse a signature declaration
++  Signature:
++    Name COLON Mapping
bpSignature() ==
  bpName() and bpEqKey "COLON" and bpTyping()
    and bpPush %Signature(bpPop2(), bpPop1())

++ SimpleMapping:
++   Application
++   Application -> Application
bpSimpleMapping() ==
  bpApplication() =>
    bpEqKey "ARROW" and (bpApplication() or bpTrap()) and
      bpPush %Mapping(bpPop1(), [bpPop1()])
    true
  false

++ ArgtypeList:
++   ( ArgtypeSequence )
++ ArgtypeSequence:
++   Application
++   Application , ArgtypeSequence
bpArgtypeList() ==
  bpTuple function bpApplication

++ Parse a mapping expression
++   Mapping:
++     ArgtypeList -> Application
bpMapping() ==
  bpParenthesized function bpArgtypeList and 
     bpEqKey "ARROW" and bpApplication() and 
       bpPush %Mapping(bpPop1(), bfUntuple bpPop1())

bpCancel()==
  a := bpState()
  bpEqKeyNextTok  "SETTAB" =>
    bpCancel() =>
      bpEqKeyNextTok  "BACKTAB" => true
      bpRestore a
      false
    bpEqKeyNextTok "BACKTAB" => true
    bpRestore a
    false
  false

bpAddTokens n==
  n=0 => nil
  n>0=> [shoeTokConstruct("KEY","SETTAB",shoeTokPosn $stok),:bpAddTokens(n-1)]
  [shoeTokConstruct("KEY","BACKTAB",shoeTokPosn $stok),:bpAddTokens(n+1)]
 
bpExceptions()==
  bpEqPeek "DOT" or bpEqPeek "QUOTE" or
       bpEqPeek "OPAREN" or bpEqPeek "CPAREN" or
          bpEqPeek "SETTAB" or bpEqPeek "BACKTAB"
             or bpEqPeek "BACKSET"
 
 
bpSexpKey()==
  $stok is ["KEY",:.] and not bpExceptions()=>
    a := $ttok has SHOEINF
    a = nil =>  bpPush $ttok and bpNext()
    bpPush a and bpNext()
  false
 
bpAnyId()==
  bpEqKey "MINUS"  and ($stok is ["INTEGER",:.] or bpTrap()) and
          bpPush MINUS $ttok and bpNext() or
             bpSexpKey() or
                   shoeTokType $stok in '(ID INTEGER STRING FLOAT)
                      and  bpPush $ttok and  bpNext()
 
bpSexp()==
  bpAnyId() or
      bpEqKey "QUOTE"  and  (bpSexp() or bpTrap())
         and bpPush bfSymbol bpPop1() or
             bpIndentParenthesized function bpSexp1
 
bpSexp1()== bpFirstTok() and
  bpSexp() and
   (bpEqKey "DOT" and bpSexp() and bpPush [bpPop2(),:bpPop1()] or
         bpSexp1() and bpPush [bpPop2(),:bpPop1()]) or
             bpPush nil
 
bpPrimary1() ==
  bpParenthesizedApplication() or
    bpDot() or
      bpConstTok() or 
        bpConstruct() or
          bpCase() or
            bpStruct() or
              bpPDefinition() or
                bpBPileDefinition()

bpParenthesizedApplication() ==
  bpName() and bpAnyNo function bpArgumentList

bpArgumentList() ==
  bpPDefinition() and 
    bpPush bfApplication(bpPop2(), bpPop1())
 
bpPrimary()==  bpFirstTok() and (bpPrimary1() or bpPrefixOperator())
 
bpDot()== bpEqKey "DOT" and bpPush bfDot ()
 
bpPrefixOperator()==
   $stok is ["KEY",:.] and
     $ttok has SHOEPRE and bpPushId() and  bpNext()
 
bpInfixOperator()==
  $stok is ["KEY",:.] and
    $ttok has SHOEINF and bpPushId() and  bpNext()
 
bpSelector()==
  bpEqKey "DOT" and (bpPrimary()
     and bpPush(bfElt(bpPop2(),bpPop1()))
	or bpPush bfSuffixDot bpPop1() )
 
bpApplication()==
   bpPrimary() and bpAnyNo function bpSelector and
      (bpApplication() and
            bpPush(bfApplication(bpPop2(),bpPop1())) or true)
              or bpNamespace()
 
++ Typing:
++   SimpleType
++   Mapping
++   FORALL Variable DOT Typing
bpTyping() ==
  bpEqKey "FORALL" =>
    bpVariable() or bpTrap()
    (bpDot() and bpPop1()) or bpTrap()
    bpTyping() or bpTrap()
    bpPush %Forall(bpPop2(), bpPop1())
  bpMapping() or bpSimpleMapping()

++ Tagged:
++   Name : Typing
bpTagged()==
  bpApplication() and
     (bpEqKey "COLON" and (bpTyping() or bpTrap()) and
       bpPush bfTagged(bpPop2(),bpPop1()) or true)
 
bpExpt()== bpRightAssoc('(POWER),function bpTagged)
 
bpInfKey s ==
 $stok is ["KEY",:.] and
   symbolMember?($ttok,s) and bpPushId() and bpNext()
 
bpInfGeneric s==
  bpInfKey s and  (bpEqKey "BACKSET" or true)
 
bpRightAssoc(o,p)==
  a := bpState()
  apply(p,nil) =>
    while  bpInfGeneric o and (bpRightAssoc(o,p) or bpTrap()) repeat
      bpPush bfInfApplication(bpPop2(),bpPop2(),bpPop1())
    true
  bpRestore a
  false
 
bpLeftAssoc(operations,parser)==
  apply(parser,nil) =>
    while bpInfGeneric(operations) and (apply(parser,nil) or bpTrap())
     repeat
       bpPush bfInfApplication(bpPop2(),bpPop2(),bpPop1())
    true
  false
 
bpString()==
  shoeTokType $stok is "STRING" and
    bpPush(["QUOTE",makeSymbol $ttok]) and bpNext()
 
bpThetaName() ==
  $stok is ["ID",:.] and $ttok has SHOETHETA =>
    bpPushId()
    bpNext()
  false
 
bpReduceOperator()==
   bpInfixOperator() or bpString() or bpThetaName()
 
bpReduce()==
  a := bpState()
  bpReduceOperator() and bpEqKey "SLASH" =>
    bpEqPeek "OBRACK" => 
      (bpDConstruct() or bpTrap()) and
	bpPush bfReduceCollect(bpPop2(),bpPop1())
    (bpApplication() or bpTrap()) and
      bpPush bfReduce(bpPop2(),bpPop1())
  bpRestore a
  false
 
bpTimes()==
    bpReduce() or bpLeftAssoc('(TIMES  SLASH),function bpExpt)

bpEuclid() ==
  bpLeftAssoc('(QUO REM),function bpTimes)
 
bpMinus()==
  bpInfGeneric '(MINUS) and (bpEuclid() or bpTrap())
    and bpPush(bfApplication(bpPop2(),bpPop1()))
      or bpEuclid()
 
bpArith()==bpLeftAssoc('(PLUS MINUS),function bpMinus)
 
bpIs()==
  bpArith() and
     bpInfKey '(IS ISNT) and (bpPattern() or bpTrap()) =>
       bpPush bfISApplication(bpPop2(),bpPop2(),bpPop1())
     bpEqKey "HAS" and (bpApplication() or bpTrap()) =>
       bpPush bfHas(bpPop2(), bpPop1())
     true
 
bpBracketConstruct(f)==
  bpBracket f and bpPush bfConstruct bpPop1()
 
bpCompare()==
  bpIs() and (bpInfKey  '(SHOEEQ SHOENE LT LE GT GE IN)
     and (bpIs() or bpTrap())
	and bpPush bfInfApplication(bpPop2(),bpPop2(),bpPop1())
	    or true)
              or bpLeave()
                or bpThrow()
 
bpAnd() == 
  bpLeftAssoc('(AND),function bpCompare)

bpThrow() ==
  bpEqKey "THROW" and bpApplication() =>
    -- Allow user-supplied matching type tag
    if bpEqKey "COLON" then
      bpApplication() or bpTrap()
      bpPush %Pretend(bpPop2(),bpPop1())
    bpPush bfThrow bpPop1()
  nil

++  Try:
++    try Assign CatchItems
bpTry() ==
  bpEqKey "TRY" =>
    bpAssign()
    cs := []
    while bpHandler "CATCH" repeat
      bpCatchItem()
      cs := [bpPop1(),:cs]
    bpHandler "FINALLY" =>
      bpFinally() and
        bpPush bfTry(bpPop2(),reverse! [bpPop1(),:cs])
    cs = nil => bpTrap() -- missing handlers
    bpPush bfTry(bpPop1(),reverse! cs)
  nil            

bpCatchItem() ==
  (bpExceptionVariable() or bpTrap()) and
    (bpEqKey "EXIT" or bpTrap()) and
      (bpAssign() or bpTrap()) and
        bpPush %Catch(bpPop2(),bpPop1())

bpExceptionVariable() ==
  t := $stok
  bpEqKey "OPAREN" and 
    (bpSignature() or bpTrap()) and
      (bpEqKey "CPAREN" or bpMissing t)
        or bpTrap()

bpFinally() ==
  (bpAssign() or bpTrap()) and
    bpPush %Finally bpPop1()

bpHandler key ==
  s := bpState()
  (bpEqKey "BACKSET" or bpEqKey "SEMICOLON") and bpEqKey key => true
  bpRestore s
  false

++ Leave:
++   LEAVE Logical
bpLeave() ==
  bpEqKey "LEAVE" and (bpLogical() or bpTrap()) and
    bpPush bfLeave bpPop1()

++ Do:
++  IN Namespace Do
++  DO Assign
bpDo() ==
  bpEqKey "IN" =>
    bpNamespace() or bpTrap()
    bpDo() or bpTrap()
    bpPush bfAtScope(bpPop2(),bpPop1())
  bpEqKey "DO" and (bpAssign() or bpTrap()) and bpPush bfDo bpPop1()

++ Return:
++   RETURN Assign
++   Leave
++   Throw
++   And
bpReturn()==
  (bpEqKey "RETURN" and  (bpAssign() or bpTrap()) and
	 bpPush bfReturnNoName bpPop1()) 
    or bpLeave()
      or bpThrow()
        or bpAnd()
          or bpDo()
 
 
bpLogical()== bpLeftAssoc('(OR),function bpReturn)
 
bpExpression()==
  bpEqKey "COLON" and (bpLogical() and
     bpPush bfApplication ("COLON",bpPop1())
           or bpTrap()) or bpLogical()
 
bpStatement()==
  bpConditional function bpWhere or bpLoop()  
    or bpExpression()
      or bpTry()
 
bpLoop()==
  bpIterators() and
   (bpCompMissing "REPEAT" and
      (bpWhere() or bpTrap()) and
         bpPush bfLp(bpPop2(),bpPop1()))
             or
               bpEqKey "REPEAT" and (bpLogical() or bpTrap()) and
                    bpPush bfLoop1 bpPop1 ()
 
bpSuchThat()==bpAndOr("BAR",function bpWhere,function bfSuchthat)
 
bpWhile()==bpAndOr ("WHILE",function bpLogical,function bfWhile)
 
bpUntil()==bpAndOr ("UNTIL",function bpLogical,function bfUntil)
 
bpFormal() ==
  bpVariable() or bpDot()

bpForIn()==
  bpEqKey "FOR" and (bpFormal() or bpTrap()) and (bpCompMissing "IN")
      and ((bpSeg()  or bpTrap()) and
       (bpEqKey "BY" and (bpArith() or bpTrap()) and
        bpPush bfForInBy(bpPop3(),bpPop2(),bpPop1())) or
         bpPush bfForin(bpPop2(),bpPop1()))
 
bpSeg()==
   bpArith() and
      (bpEqKey "SEG" and
       (bpArith() and bpPush(bfSegment2(bpPop2(),bpPop1()))
         or bpPush(bfSegment1(bpPop1()))) or true)
 
bpIterator()==
  bpForIn() or bpSuchThat() or bpWhile() or bpUntil()
 
bpIteratorList()==
  bpOneOrMore function bpIterator
    and bpPush bfIterators bpPop1 ()
 
bpCrossBackSet()==
  bpEqKey "CROSS" and (bpEqKey "BACKSET" or true)
 
bpIterators()==
  bpListofFun(function bpIteratorList,
    function bpCrossBackSet,function bfCross)
 
bpAssign()==
  a := bpState()
  bpStatement() =>
    bpEqPeek "BEC" =>
      bpRestore a
      bpAssignment() or bpTrap()
    bpEqPeek "GIVES" =>
      bpRestore a
      bpLambda() or bpTrap()
    true
  bpRestore a
  false
 
bpAssignment()==
  bpAssignVariable() and
    bpEqKey "BEC" and
      (bpAssign() or bpTrap()) and
	 bpPush bfAssign (bpPop2(),bpPop1())
 
++ Parse a lambda expression
++   Lambda ::= Variable +-> Assign
bpLambda() ==
  bpVariable() and
    bpEqKey "GIVES" and
      (bpAssign() or bpTrap()) and
        bpPush bfLambda(bpPop2(),bpPop1())

-- should only be allowed in sequences
bpExit()==
  bpAssign() and (bpEqKey "EXIT" and
      ((bpWhere() or bpTrap()) and
	 bpPush bfExit (bpPop2(),bpPop1()))
	   or true)

bpDefinition()==
  bpEqKey "MACRO" =>
    bpName() and bpStoreName() and bpCompoundDefinitionTail function %Macro
      or bpTrap()
  a := bpState()
  bpExit() =>
    bpEqPeek "DEF" =>
       bpRestore a
       bpDef()
    bpEqPeek "TDEF" =>
       bpRestore a
       bpTypeAliasDefition()
    true
  bpRestore a
  false
 
bpStoreName()==
  $op := first $stack
  $wheredefs := nil
  $typings := nil
  true

bpDef() ==  
  bpName() and bpStoreName() and bpDefTail function %Definition
 
bpDDef() ==  bpName() and bpDefTail function %Definition

++ Parse the remaining of a simple definition.
bpSimpleDefinitionTail() ==
  bpEqKey "DEF" and
    (bpWhere() or bpTrap())
      and bpPush %ConstantDefinition(bpPop2(), bpPop1())

++ Parse the remaining of a compound definition.
bpCompoundDefinitionTail f ==
  bpVariable() and 
    bpEqKey "DEF" and (bpWhere() or bpTrap()) and 
      bpPush apply(f,[bpPop3(),bpPop2(),bpPop1()])


++ Parse the remainding of a definition.  When we reach this point
++ we know we must parse a definition and we have already parsed
++ the name of the main operator in the definition.
bpDefTail f ==
  bpSimpleDefinitionTail()
    or bpCompoundDefinitionTail f
 
bpWhere()==
    bpDefinition() and
       (bpEqKey "WHERE" and (bpDefinitionItem() or bpTrap())
           and bpPush bfWhere(bpPop1(),bpPop1()) or true)
 
bpDefinitionItem()==
  a := bpState()
  bpDDef() => true
  bpRestore a
  bpBDefinitionPileItems() => true
  bpRestore a
  bpPDefinitionItems() => true
  bpRestore a
  bpWhere()
 
bpDefinitionPileItems()==
  bpListAndRecover function bpDefinitionItem
    and bpPush %Pile bpPop1()
 
bpBDefinitionPileItems()== bpPileBracketed function bpDefinitionPileItems
 
bpSemiColonDefinition()==bpSemiListing
  (function bpDefinitionItem,function %Pile)
 
bpPDefinitionItems()==
  bpParenthesized function bpSemiColonDefinition
 
bpComma()== 
  bpModule() or bpImport() or bpNamespace() or
    bpTuple function bpWhere
 
bpTuple(p) ==
  bpListofFun(p,function bpCommaBackSet,function bfTuple)
 
bpCommaBackSet() == 
  bpEqKey "COMMA" and (bpEqKey "BACKSET" or true)
 
bpSemiColon() == 
  bpSemiListing (function bpComma,function bfSequence)
 
bpSemiListing(p,f) ==
  bpListofFun(p,function bpSemiBackSet,f)
 
bpSemiBackSet()== 
  bpEqKey "SEMICOLON" and (bpEqKey "BACKSET" or true)
 
bpPDefinition()==
  bpIndentParenthesized function bpSemiColon
 
bpPileItems()==
  bpListAndRecover function bpSemiColon and bpPush bfSequence bpPop1()
 
bpBPileDefinition()==
  bpPileBracketed function bpPileItems
 
bpIteratorTail()==
  (bpEqKey "REPEAT" or true) and bpIterators()
 
bpConstruct()==
  bpBracket function bpConstruction
 
bpConstruction()==
  bpComma() and
     (bpIteratorTail() and
	  bpPush bfCollect (bpPop2(),bpPop1()) or
	     bpPush bfTupleConstruct bpPop1())
 
bpDConstruct()==
  bpBracket function bpDConstruction
 
bpDConstruction()==
  bpComma() and
     (bpIteratorTail() and
	  bpPush bfDCollect (bpPop2(),bpPop1()) or
	     bpPush bfDTuple bpPop1())
 
 
 
--PATTERN
 
--bpNameOrDot() == bpName() or bpDot() or bpEqual()
 
bpPattern()==
  bpBracketConstruct function bpPatternL
    or bpName() or bpConstTok()
 
bpEqual()==
   bpEqKey "SHOEEQ" and (bpApplication() or bpConstTok() or
                bpTrap()) and bpPush bfEqual bpPop1()
 
bpRegularPatternItem() ==
   bpEqual() or
     bpConstTok() or bpDot() or
      bpName() and
         ((bpEqKey "BEC" and (bpPattern() or bpTrap())
              and bpPush bfAssign(bpPop2(),bpPop1())) or true)
                    or bpBracketConstruct function bpPatternL
 
bpRegularPatternItemL()==
      bpRegularPatternItem() and bpPush [bpPop1()]
 
bpRegularList()==
       bpListof(function bpRegularPatternItemL,"COMMA",function bfAppend)
 
bpPatternColon()==
     bpEqKey "COLON" and (bpRegularPatternItem() or bpTrap())
              and bpPush [bfColon bpPop1()]
 
 
-- only one colon
bpPatternL() == bpPatternList() and bpPush bfTuple bpPop1()
 
bpPatternList()==
  bpRegularPatternItemL() =>
    while (bpEqKey  "COMMA" and (bpRegularPatternItemL() or
	(bpPatternTail()
	  and bpPush append(bpPop2(),bpPop1())
	    or bpTrap();false) )) repeat
	      bpPush append(bpPop2(),bpPop1())
    true
  bpPatternTail()
 
bpPatternTail()==
  bpPatternColon() and
      (bpEqKey "COMMA" and (bpRegularList() or bpTrap())
	   and bpPush append (bpPop2(),bpPop1()) or true)
 
-- BOUND VARIABLE

++ We are parsing parameters in a function definition.  We have
++ just seen a parameter name; we are attempting to see whether
++ it might be followed by a type annotation, or whether it actually
++ a form with a specific pattern structure, or whether it has
++ a default value.
bpRegularBVItemTail() ==
  bpEqKey "COLON" and (bpApplication() or bpTrap()) and 
    bpPush bfTagged(bpPop2(), bpPop1())
      or bpEqKey "BEC" and (bpPattern() or bpTrap()) and
	 bpPush bfAssign(bpPop2(),bpPop1())
	   or bpEqKey "IS" and (bpPattern() or bpTrap()) and
	      bpPush bfAssign(bpPop2(),bpPop1())
             or bpEqKey "DEF" and (bpApplication() or bpTrap()) and
                bpPush %DefaultValue(bpPop2(), bpPop1())


bpRegularBVItem() ==
  bpBVString() 
    or bpConstTok() 
      or (bpName() and (bpRegularBVItemTail() or true))
        or bpBracketConstruct function bpPatternL
 
bpBVString()==
  shoeTokType $stok is "STRING" and
      bpPush(["BVQUOTE",makeSymbol $ttok]) and bpNext()
 
bpRegularBVItemL() ==
  bpRegularBVItem() and bpPush [bpPop1()]
 
bpColonName()==
  bpEqKey "COLON" and (bpName() or bpBVString() or bpTrap())
 
 
-- at most one colon at end
bpBoundVariablelist()==
  bpRegularBVItemL() =>
    while (bpEqKey  "COMMA" and (bpRegularBVItemL() or
	(bpColonName()
	  and bpPush  bfColonAppend(bpPop2(),bpPop1())
	    or bpTrap();false) )) repeat
	       bpPush append(bpPop2(),bpPop1())
    true
  bpColonName() and bpPush bfColonAppend(nil,bpPop1())
 

bpVariable()==
    bpParenthesized function bpBoundVariablelist and
       bpPush bfTupleIf bpPop1()
         or bpBracketConstruct function bpPatternL
                or bpName() or bpConstTok()
 
bpAssignVariable()==
      bpBracketConstruct function bpPatternL or bpAssignLHS()
 
bpAssignLHS()==
  not bpName() => false
  bpEqKey "COLON" =>          -- variable declaration
    bpApplication() or bpTrap()
    bpPush bfLocal(bpPop2(),bpPop1())
  bpArgumentList() and 
    (bpEqPeek "DOT" 
      or (bpEqPeek "BEC" and bpPush bfPlace bpPop1()) 
        or bpTrap())
  bpEqKey "DOT" =>            -- field path
    bpList(function bpPrimary,"DOT") and 
      bpChecknull() and
        bpPush bfTuple([bpPop2(),:bpPop1()])
  true

bpChecknull()==
  a := bpPop1()
  a = nil => bpTrap()
  bpPush a

bpStruct()==
   bpEqKey "STRUCTURE" and
      (bpName() or bpTrap()) and
        (bpEqKey "DEF" or bpTrap()) and
           bpTypeList() and bpPush %Structure(bpPop2(),bpPop1())
 
bpTypeList() == 
  bpPileBracketed function bpTypeItemList
    or bpTerm function bpIdList and bpPush [bpPop1()]

bpTypeItem() ==
  bpTerm function bpIdList
 
bpTypeItemList() ==  
  bpListAndRecover function bpTypeItem
 
bpTerm idListParser ==
  (bpName() or bpTrap()) and
    ((bpParenthesized idListParser and
      bpPush bfNameArgs (bpPop2(),bpPop1()))
	or bpName() and bpPush bfNameArgs(bpPop2(),bpPop1()))
	 or bpPush(bfNameOnly bpPop1())
 
bpIdList()== 
  bpTuple function bpName
 
bpCase()==
  bpEqKey "CASE" and
    (bpWhere() or bpTrap()) and
       (bpEqKey "OF" or bpMissing "OF") and
	     bpPiledCaseItems()
 
bpPiledCaseItems()==
   bpPileBracketed function bpCaseItemList and
       bpPush bfCase(bpPop2(),bpPop1())

bpCaseItemList()==
   bpListAndRecover function bpCaseItem

bpCasePatternVar() ==
  bpName() or bpDot()

bpCasePatternVarList() ==
  bpTuple function bpCasePatternVar
 
bpCaseItem()==
    (bpTerm function bpCasePatternVarList or bpTrap()) and
       (bpEqKey "EXIT" or bpTrap()) and
         (bpWhere() or bpTrap()) and
            bpPush bfCaseItem (bpPop2(),bpPop1())


++ Main entry point into the parser module.
bpOutItem()==
  $op := nil
  bpComma() or bpTrap()
  b := bpPop1()
  bpPush 
    b is ["+LINE",:.] => [ b ]
    b is ["L%T",l,r] and symbol? l => 
      $InteractiveMode => [["SETQ",l,r]]
      [["DEFPARAMETER",l,r]]
    translateToplevel(b,false)
 
