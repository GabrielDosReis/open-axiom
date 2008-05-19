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
 

++ true when the current function definition has its parameters 
++ written round parenthesis.
$sawParenthesizedHead := false

bpFirstToken()==
      $stok:=
          if null $inputStream
          then shoeTokConstruct("ERROR","NOMORE",shoeTokPosn $stok)
          else first $inputStream
      $ttok:=shoeTokPart $stok
      true
 
bpFirstTok()==
      $stok:=
          if null $inputStream
          then shoeTokConstruct("ERROR","NOMORE",shoeTokPosn $stok)
          else first $inputStream
      $ttok:=shoeTokPart $stok
      $bpParenCount>0 and EQCAR($stok,"KEY") =>
             EQ($ttok,"SETTAB")=>
                $bpCount:=$bpCount+1
                bpNext()
             EQ($ttok,"BACKTAB")=>
                $bpCount:=$bpCount-1
                bpNext()
             EQ($ttok,"BACKSET")=>
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
--cons($inputStream,$stack)
 
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
       RPLACD($stack,CDDR $stack)
       a
 
bpPop3()==
       a:=third $stack
       RPLACD(rest $stack,CDDDR $stack)
       a
 
bpIndentParenthesized f==
    $bpCount:local:=0
    a:=$stok
    if bpEqPeek "OPAREN"
    then
      $bpParenCount:=$bpParenCount+1
      bpNext()
      if apply(f,nil) and bpFirstTok() and
              (bpEqPeek "CPAREN" or bpParenTrap(a))
      then
            $bpParenCount:=$bpParenCount-1
            bpNextToken()
            $bpCount=0 => true
            $inputStream:=append( bpAddTokens $bpCount,$inputStream)
            bpFirstToken()
            $bpParenCount=0 =>
                     bpCancel()
                     true
            true
      else if bpEqPeek "CPAREN"
           then
              bpPush  bfTuple []
              $bpParenCount:=$bpParenCount-1
              bpNextToken()
              true
           else bpParenTrap(a)
    else false
 
bpParenthesized f==
    a:=$stok
    if bpEqKey "OPAREN"
    then
      if apply(f,nil) and (bpEqKey "CPAREN" or bpParenTrap(a))
      then true
      else if bpEqKey "CPAREN"
           then
              bpPush  bfTuple []
              true
           else bpParenTrap(a)
    else false
 
bpBracket f==
    a:=$stok
    if bpEqKey "OBRACK"
    then
      if apply(f,nil) and (bpEqKey "CBRACK" or bpBrackTrap(a))
      then bpPush bfBracket bpPop1 ()
      else if bpEqKey "CBRACK"
           then bpPush  []
           else bpBrackTrap(a)
    else false
 
bpPileBracketed f==
              if bpEqKey "SETTAB"
              then if bpEqKey "BACKTAB"
                   then true
                   else if apply(f,nil) and
                            (bpEqKey "BACKTAB" or bpPileTrap())
                        then bpPush bfPile bpPop1()
                        else false
              else false
 
bpListof(f,str1,g)==
    if apply(f,nil)
    then
        if bpEqKey str1 and (apply(f,nil) or bpTrap())
        then
          a:=$stack
          $stack:=nil
          while bpEqKey str1 and (apply(f,nil) or bpTrap()) repeat 0
          $stack:=[NREVERSE $stack,:a]
          bpPush FUNCALL(g, [bpPop3(),bpPop2(),:bpPop1()])
        else
          true
    else false
 
 
-- to do ,<backset>
bpListofFun(f,h,g)==
    if apply(f,nil)
    then
        if apply(h,nil) and (apply(f,nil) or bpTrap())
        then
          a:=$stack
          $stack:=nil
          while apply(h,nil) and (apply(f,nil) or bpTrap()) repeat 0
          $stack:=[NREVERSE $stack,:a]
          bpPush FUNCALL(g, bfListOf [bpPop3(),bpPop2(),:bpPop1()])
        else
          true
    else false
 
bpList(f,str1,g)==
    if apply(f,nil)
    then
        if bpEqKey str1 and (apply(f,nil) or bpTrap())
        then
          a:=$stack
          $stack:=nil
          while bpEqKey str1 and (apply(f,nil) or bpTrap()) repeat 0
          $stack:=[NREVERSE $stack,:a]
          bpPush FUNCALL(g,  [bpPop3(),bpPop2(),:bpPop1()])
        else
          bpPush FUNCALL(g, [bpPop1()])
    else bpPush FUNCALL(g, [])
 
bpOneOrMore f==
       apply(f,nil)=>
         a:=$stack
         $stack:=nil
         while apply(f,nil) repeat 0
         $stack:=[NREVERSE $stack,:a]
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
  if  bpEqKey "IF" and (bpWhere() or bpTrap()) and
                   (bpEqKey "BACKSET" or true)
  then
           if bpEqKey "SETTAB"
           then if bpEqKey "THEN"
                then  (apply(f,nil) or bpTrap()) and bpElse(f) and bpEqKey "BACKTAB"
                else  bpMissing "THEN"
           else if bpEqKey "THEN"
                then (apply(f,nil) or bpTrap()) and bpElse(f)
                else bpMissing "then"
  else false
 
bpElse(f)==
           a:=bpState()
           if bpBacksetElse()
           then  (apply(f,nil) or bpTrap()) and
                 bpPush bfIf(bpPop3(),bpPop2(),bpPop1())
           else
              bpRestore a
              bpPush bfIfThenOnly(bpPop2(),bpPop1())
 
bpBacksetElse()==
    if bpEqKey "BACKSET"
    then bpEqKey "ELSE"
    else bpEqKey "ELSE"
 
bpEqPeek s ==  EQCAR($stok,"KEY") and EQ(s,$ttok)
 
bpEqKey s ==   EQCAR($stok,"KEY") and EQ(s,$ttok) and bpNext()
bpEqKeyNextTok s ==   EQCAR($stok,"KEY") and EQ(s,$ttok) and
                  bpNextToken()
 
bpPileTrap()   == bpMissing  "BACKTAB"
bpBrackTrap(x) == bpMissingMate("]",x)
bpParenTrap(x) == bpMissingMate(")",x)
 
bpMissingMate(close,open)==
   bpSpecificErrorAtToken(open, '"possibly missing mate")
   bpMissing close
 
bpMissing s==
   bpSpecificErrorHere(CONCAT(PNAME s,'" possibly missing"))
   throw TRAPPOINT "TRAPPED"
 
bpCompMissing s == bpEqKey s or bpMissing s
 
bpTrap()==
   bpGeneralErrorHere()
   throw TRAPPOINT "TRAPPED"
 
bpRecoverTrap()==
  bpFirstToken()
  pos1 := shoeTokPosn $stok
  bpMoveTo 0
  pos2 := shoeTokPosn $stok
  bpIgnoredFromTo(pos1, pos2)
  bpPush  [['"pile syntax error"]]
 
bpListAndRecover(f)==
   a:=$stack
   b:=nil
   $stack:=nil
   done:=false
   c:=$inputStream
   while not done repeat
--   $trapped:local:=false
     found:=try apply(f,nil) catch TRAPPOINT
     if found="TRAPPED"
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
        c:=$inputStream
     else if bpEqPeek "BACKTAB"  or null $inputStream
          then
             done:=true
          else
            $inputStream:=c
            bpGeneralErrorHere()
            bpRecoverTrap()
            if bpEqPeek "BACKTAB"  or null $inputStream
            then done:=true
            else
                bpNext()
                c:=$inputStream
     b:=[bpPop1(),:b]
   $stack:=a
   bpPush NREVERSE b
 
bpMoveTo n==
     null $inputStream  => true
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
    EQCAR($stok, "ID") and bpPushId() and bpNext()
      and bpPush bfColonColon(bpPop2(), bpPop1())
  false

++ Name:
++   ID
++   Name :: ID
bpName() ==
  EQCAR( $stok,"ID") =>
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
     MEMQ(shoeTokType $stok, '(INTEGER FLOAT)) =>
          bpPush $ttok
          bpNext()
     EQCAR($stok,"LISP")=> bpPush bfReadLisp $ttok and bpNext()
     EQCAR($stok,"LISPEXP")=> bpPush $ttok and bpNext()
     EQCAR($stok,"LINE")=> bpPush ["+LINE", $ttok] and bpNext()
     bpEqPeek "QUOTE" =>
          bpNext()
          (bpSexp() or bpTrap()) and
               bpPush bfSymbol bpPop1()
     bpString()


++ ExportItemList:
++    Signature
++    ExportItemList Signature
bpExportItemList() ==
  bpListAndRecover function bpSignature

++ Exports:
++   pile-bracketed ExporItemList
bpExports() ==
  bpPileBracketed function bpExportItemList

++ Parse a module definitoin
++   Module:
++     MODULE QUOTE String
bpModule() ==
  bpEqKey "MODULE" => 
    bpName() or bpTrap()
    bpEqKey "WHERE" =>
      bpExports() and bpPush %Module(bpPop2(), bpPop1())
    bpPush %Module(bpPop1(),nil)

++ Parse a module import, or a import declaration for a foreign entity.
++ Import:
++    IMPORT Name for Signature
++    IMPORT QUOTE String
bpImport() ==
  bpEqKey "IMPORT" =>
    bpName() or bpTrap()
    bpEqKey "FOR" =>
      (bpSignature() or bpTrap()) and 
         bpPush ImportSignature(bpPop2(), bpPop1())
    bpPush Import bpPop1()
  false

-- Parse a type alias defnition:
--    type-alias-definition: 
--          identifier <=> logical-expression
bpTypeAliasDefition() ==
  (bpTerm() or bpTrap()) and 
    bpEqKey "TDEF" and bpLogical() and
      bpPush %TypeAlias(bpPop2(), bpPop1())

++ Parse a signature declaration
++  Signature:
++    Name COLON Mapping
bpSignature() ==
  bpName() and bpEqKey "COLON" and bpMapping()
    and bpPush Signature(bpPop2(), bpPop1())

++ Parse a mapping expression
++   Mapping:
++      (Name | IdList) -> Name
bpMapping() ==
  (bpName() or bpParenthesized function bpIdList) and 
     bpEqKey "ARROW" and bpName() and 
       bpPush Mapping(bpPop1(), bfUntuple bpPop1())

bpCancel()==
    a:=bpState()
    if bpEqKeyNextTok  "SETTAB"
    then if bpCancel()
         then  if bpEqKeyNextTok  "BACKTAB"
               then true
               else
                   bpRestore a
                   false
         else
           if bpEqKeyNextTok "BACKTAB"
           then true
           else
              bpRestore a
              false
    else false
bpAddTokens n==
         n=0 => nil
         n>0=> cons(shoeTokConstruct("KEY","SETTAB",shoeTokPosn $stok),bpAddTokens(n-1))
         cons(shoeTokConstruct("KEY","BACKTAB",shoeTokPosn $stok),bpAddTokens(n+1))
 
bpExceptions()==
     bpEqPeek "DOT" or bpEqPeek "QUOTE" or
          bpEqPeek "OPAREN" or bpEqPeek "CPAREN" or
             bpEqPeek "SETTAB" or bpEqPeek "BACKTAB"
                or bpEqPeek "BACKSET"
 
 
bpSexpKey()==
      EQCAR( $stok,"KEY") and not bpExceptions()=>
               a:=GET($ttok,"SHOEINF")
               null a=>  bpPush $ttok and bpNext()
               bpPush a and bpNext()
      false
 
bpAnyId()==
  bpEqKey "MINUS"  and (EQCAR($stok,"INTEGER") or bpTrap()) and
          bpPush MINUS $ttok and bpNext() or
             bpSexpKey() or
                   MEMQ(shoeTokType $stok, '(ID INTEGER STRING FLOAT))
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
   bpName() or
     bpDot() or
      bpConstTok() or
       bpConstruct() or
        bpCase() or
         bpStruct() or
          bpPDefinition() or
           bpBPileDefinition()
 
bpPrimary()==  bpFirstTok() and (bpPrimary1() or bpPrefixOperator())
 
bpDot()== bpEqKey "DOT" and bpPush bfDot ()
 
bpPrefixOperator()==
   EQCAR( $stok,"KEY") and
     GET($ttok,"SHOEPRE") and bpPushId() and  bpNext()
 
bpInfixOperator()==
  EQCAR( $stok,"KEY") and
    GET($ttok,"SHOEINF") and bpPushId() and  bpNext()
 
bpSelector()==
            bpEqKey "DOT" and (bpPrimary()
               and bpPush(bfElt(bpPop2(),bpPop1()))
                  or bpPush bfSuffixDot bpPop1() )
 
bpOperator()==   bpPrimary() and bpAnyNo function bpSelector
 
bpApplication()==
   bpPrimary() and bpAnyNo function bpSelector and
      (bpApplication() and
            bpPush(bfApplication(bpPop2(),bpPop1())) or true)
 
++ Typing:
++   SimpleType
++   Mapping
bpTyping() ==
  bpApplication() and
    (bpEqKey "ARROW" and (bpApplication() or bpTrap()) and
      bpPush Mapping(bpPop1(), bfUntuple bpPop1()) or true) or bpMapping()

++ Tagged:
++   Name : Typing
bpTagged()==
      bpApplication() and
         (bpEqKey "COLON" and (bpTyping() or bpTrap()) and
           bpPush bfTagged(bpPop2(),bpPop1()) or true)
 
bpExpt()== bpRightAssoc('(POWER),function bpTagged)
 
bpInfKey s==
 EQCAR( $stok,"KEY") and
   MEMBER($ttok,s) and bpPushId() and bpNext()
 
bpInfGeneric s== bpInfKey s and  (bpEqKey "BACKSET" or true)
 
bpRightAssoc(o,p)==
    a:=bpState()
    if apply(p,nil)
    then
       while  bpInfGeneric o and (bpRightAssoc(o,p) or bpTrap()) repeat
             bpPush bfInfApplication(bpPop2(),bpPop2(),bpPop1())
       true
    else
       bpRestore a
       false
 
bpLeftAssoc(operations,parser)==
    if apply(parser,nil)
    then
       while bpInfGeneric(operations) and
         (apply(parser,nil) or bpTrap())
           repeat
             bpPush bfInfApplication(bpPop2(),bpPop2(),bpPop1())
       true
    else false
 
bpString()==
     EQ(shoeTokType $stok,"STRING") and
         bpPush(["QUOTE",INTERN $ttok]) and bpNext()
 
bpThetaName() ==
        if EQCAR( $stok,"ID") and GET($ttok,"SHOETHETA")
        then
           bpPushId()
           bpNext()
        else false
 
bpReduceOperator()==
         bpInfixOperator() or bpString()
                or bpThetaName()
 
bpReduce()==
     a:=bpState()
     if bpReduceOperator() and bpEqKey "SLASH"
     then
        bpEqPeek "OBRACK" => (bpDConstruct() or bpTrap()) and
                     bpPush bfReduceCollect(bpPop2(),bpPop1())
        (bpApplication() or bpTrap()) and
                bpPush bfReduce(bpPop2(),bpPop1())
     else
        bpRestore a
        false
 
bpTimes()==
    bpReduce() or bpLeftAssoc('(TIMES  SLASH),function bpExpt)
 
bpMinus()==
   bpInfGeneric '(MINUS) and (bpTimes() or bpTrap())
        and bpPush(bfApplication(bpPop2(),bpPop1()))
          or bpTimes()
 
bpArith()==bpLeftAssoc('(PLUS MINUS),function bpMinus)
 
bpIs()==
     bpArith() and (bpInfKey '(IS ISNT) and (bpPattern() or bpTrap())
        and bpPush bfISApplication(bpPop2(),bpPop2(),bpPop1())
           or true)
 
bpBracketConstruct(f)==
        bpBracket f and bpPush bfConstruct bpPop1 ()
 
bpCompare()==
     bpIs() and (bpInfKey  '(SHOEEQ SHOENE LT LE GT GE IN)
        and (bpIs() or bpTrap())
           and bpPush bfInfApplication(bpPop2(),bpPop2(),bpPop1())
               or true)
 
bpAnd()== bpLeftAssoc('(AND),function bpCompare)

bpThrow() ==
  bpEqKey "THROW" and bpApplication() and 
    bpPush bfThrow bpPop1()

++  Try:
++    try Assign CatchItems
bpTry() ==
  bpEqKey "TRY" and bpAssign() and 
    (bpEqKey "BACKSET" or true) and 
      (bpEqKey "CATCH" or bpMissing "CATCH") and
        (bpPiledCatchItems() or bpSimpleCatch() or bpTrap()) and
           bpPush bfTry(bpPop2(), bpPop1())

++ SimpleCatch:
++   catch Name
bpSimpleCatch() ==
  bpCatchItem() and bpPush [bpPop1()]

bpPiledCatchItems() ==
  bpPileBracketed function bpCatchItemList

bpCatchItemList() ==
  bpListAndRecover function bpCatchItem

bpExceptionHead() ==
  (bpName() or bpTrap()) and
    ((bpParenthesized function bpIdList and
      bpPush bfNameArgs (bpPop2(),bpPop1()))
	or bpName() and bpPush bfNameArgs(bpPop2(),bpPop1()))
          or true

bpExceptionTail() ==
  bpEqKey "EXIT" and (bpAssign() or bpTrap()) and
    bpPush Exit(bpPop2(),bpPop1())

++ Exception:
++   ExpcetionHead
++   ExceptionHead => Assign
bpException() ==
  bpExceptionHead() and (bpExceptionTail() or true)

++ Catch:
++   catch Exception
bpCatchItem() ==
  (bpException() or bpTrap()) and 
    bpPush %Catch bpPop1()

bpReturn()==
  (bpEqKey "RETURN" and  (bpAnd() or bpTrap()) and
	 bpPush bfReturnNoName bpPop1()) 
    or bpThrow()
      or bpAnd()
 
 
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
 
bpForIn()==
  bpEqKey "FOR" and (bpVariable() or bpTrap()) and (bpCompMissing "IN")
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
 
bpIteratorList()==bpOneOrMore function bpIterator
       and bpPush bfIterators bpPop1 ()
 
bpCrossBackSet()== bpEqKey "CROSS" and (bpEqKey "BACKSET" or true)
 
bpIterators()==
         bpListofFun(function bpIteratorList,
              function bpCrossBackSet,function bfCross)
 
bpAssign()==
            a:=bpState()
            if bpStatement()
            then
                 if bpEqPeek "BEC"
                 then
                   bpRestore a
                   bpAssignment() or bpTrap()
                 else true
            else
                 bpRestore a
                 false
 
bpAssignment()==
  bpAssignVariable() and
    bpEqKey "BEC" and
      (bpAssign() or bpTrap()) and
	 bpPush bfAssign (bpPop2(),bpPop1())
 
-- should only be allowed in sequences
bpExit()==
     bpAssign() and (bpEqKey "EXIT" and
         ((bpWhere() or bpTrap()) and
            bpPush bfExit (bpPop2(),bpPop1()))
              or true)

++ returns true if the next token introduces a definition.
bpBeginDefinition() ==
  bpEqPeek "DEF" or 
    $sawParenthesizedHead and bpEqPeek "COLON"

bpDefinition()==
        a:=bpState()
        bpExit() =>
             bpBeginDefinition() =>
                bpRestore a
                bpDef()
             bpEqPeek "TDEF" =>
                bpRestore a
                bpTypeAliasDefition()
             bpEqPeek "MDEF" =>
                bpRestore a
                bpMdef()
             true
        bpRestore a
        false
 
bpStoreName()==
  $op := first $stack
  $wheredefs := nil
  $typings := nil
  $returnType := true           -- assume we may return anything
  true

bpReturnType() ==
  -- a return type is acceptable for a function definition only
  -- if its parameters are written in round parenthesis.  
  -- In particular, we reject the situation `foo x:Integer == ...'
  $sawParenthesizedHead and bpEqKey "COLON" =>
    bpApplication() or bpTrap()
    $returnType := bpPop1()
    true
  true
 
bpDef() ==  
  bpName() and bpStoreName() and
    bpDefTail() and bpPush bfCompDef bpPop1 ()
 
bpDDef() ==  bpName() and bpDefTail()

++ Parse the remaining of a simple definition.
bpSimpleDefinitionTail() ==
  bpEqKey "DEF" and
    (bpWhere() or bpTrap())
      and bpPush ConstantDefinition(bpPop2(), bpPop1())

++ Parse the remaining of a compound definition.
bpCompoundDefinitionTail() ==
  bpVariable()  and bpReturnType()  and
    bpEqKey "DEF" and (bpWhere() or bpTrap())
      and bpPush bfDefinition(bpPop3(),bpPop2(),bpPop1())


++ Parse the remainding of a definition.  When we reach this point
++ we know we must parse a definition and we have already parsed
++ the name of the main operator in the definition.
bpDefTail() ==
  bpSimpleDefinitionTail()
    or bpCompoundDefinitionTail()
 
 
bpMDefTail()==
 --   bpEqKey "MDEF" and
 --   (bpWhere() or bpTrap())
 --     and bpPush bfMDefinition1(bpPop2(),bpPop1())
 --      or
           (bpVariable() or bpTrap()) and
             bpEqKey "MDEF" and (bpWhere() or bpTrap())
                 and bpPush bfMDefinition(bpPop3(),bpPop2(),bpPop1())
 
bpMdef()== bpName() and bpStoreName() and bpMDefTail()
 
bpWhere()==
    bpDefinition() and
       (bpEqKey "WHERE" and (bpDefinitionItem() or bpTrap())
           and bpPush bfWhere(bpPop1(),bpPop1()) or true)
 
bpDefinitionItem()==
          a:=bpState()
          if bpDDef()
          then true
          else
             bpRestore a
             if bpBDefinitionPileItems()
             then true
             else
               bpRestore a
               if bpPDefinitionItems()
               then true
               else
                   bpRestore a
                   bpWhere()
 
bpDefinitionPileItems()==
    bpListAndRecover function bpDefinitionItem
           and bpPush bfDefSequence bpPop1()
 
bpBDefinitionPileItems()== bpPileBracketed function bpDefinitionPileItems
 
bpSemiColonDefinition()==bpSemiListing
    (function bpDefinitionItem,function bfDefSequence)
 
bpPDefinitionItems()==bpParenthesized function bpSemiColonDefinition
 
bpComma()== 
  bpModule() or bpImport() or
    bpTuple function bpWhere
 
bpTuple(p)==bpListofFun(p,function bpCommaBackSet,function bfTuple)
 
bpCommaBackSet()== bpEqKey "COMMA" and (bpEqKey "BACKSET" or true)
 
bpSemiColon()==bpSemiListing (function bpComma,function bfSequence)
 
bpSemiListing(p,f)==bpListofFun(p,function bpSemiBackSet,f)
 
bpSemiBackSet()== bpEqKey "SEMICOLON" and (bpEqKey "BACKSET" or true)
 
bpPDefinition()==  bpIndentParenthesized function bpSemiColon
 
bpPileItems()==
    bpListAndRecover function bpSemiColon and bpPush bfSequence bpPop1()
 
bpBPileDefinition()==  bpPileBracketed function bpPileItems
 
bpIteratorTail()==
     (bpEqKey "REPEAT" or true) and bpIterators()
 
--bpExpression()==  bpLogical()
 
bpConstruct()==bpBracket function bpConstruction
 
bpConstruction()==
     bpComma() and
        (bpIteratorTail() and
             bpPush bfCollect (bpPop2(),bpPop1()) or
                bpPush bfTupleConstruct bpPop1())
 
bpDConstruct()==bpBracket function bpDConstruction
 
bpDConstruction()==
     bpComma() and
        (bpIteratorTail() and
             bpPush bfDCollect (bpPop2(),bpPop1()) or
                bpPush bfDTuple bpPop1())
 
 
 
--PATTERN
 
--bpNameOrDot() == bpName() or bpDot() or bpEqual()
 
bpPattern()== bpBracketConstruct function bpPatternL
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
     if bpRegularPatternItemL()
     then
        while (bpEqKey  "COMMA" and (bpRegularPatternItemL() or
            (bpPatternTail()
              and bpPush append(bpPop2(),bpPop1())
                or bpTrap();false) )) repeat
                  bpPush append(bpPop2(),bpPop1())
        true
     else bpPatternTail()
 
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
     EQ(shoeTokType $stok,"STRING") and
         bpPush(["BVQUOTE",INTERN $ttok]) and bpNext()
 
bpRegularBVItemL() ==
      bpRegularBVItem() and bpPush [bpPop1()]
 
bpColonName()==
     bpEqKey "COLON" and (bpName() or bpBVString() or bpTrap())
 
 
-- at most one colon at end
bpBoundVariablelist()==
     if bpRegularBVItemL()
     then
        while (bpEqKey  "COMMA" and (bpRegularBVItemL() or
            (bpColonName()
              and bpPush  bfColonAppend(bpPop2(),bpPop1())
                or bpTrap();false) )) repeat
                   bpPush append(bpPop2(),bpPop1())
        true
     else bpColonName() and bpPush bfColonAppend(nil,bpPop1())
 

++ Mark the start of parameter list enclosed in round parenthesis
bpBeginParameterList() ==
  $sawParenthesizedHead := false
  true

++ Mark the end of parameter list enclosed in round parenthesis
bpEndParameterList() ==
  $sawParenthesizedHead := true

bpVariable()==
  bpBeginParameterList() and
    bpParenthesized function bpBoundVariablelist and
       bpPush bfTupleIf bpPop1() and bpEndParameterList()
         or bpBracketConstruct function bpPatternL
                or bpName() or bpConstTok()
 
bpAssignVariable()==
      bpBracketConstruct function bpPatternL or bpAssignLHS()
 
bpAssignLHS()==
   bpName() and (bpEqKey "COLON" and (bpApplication() or bpTrap())
     and bpPush bfLocal(bpPop2(),bpPop1())
        or bpEqKey "DOT" and bpList(function bpPrimary,"DOT",
              function bfListOf)
          and bpChecknull() and
            bpPush bfTuple([bpPop2(),:bpPop1()])
                 or true)
bpChecknull()==
      a:=bpPop1()
      if null a
      then bpTrap()
      else bpPush a

bpStruct()==
   bpEqKey "STRUCTURE" and
      (bpName() or bpTrap()) and
        (bpEqKey "DEF" or bpTrap()) and
           bpTypeList() and bpPush bfStruct(bpPop2(),bpPop1())
 
bpTypeList() == bpPileBracketed function bpTypeItemList
       or bpTerm() and bpPush [bpPop1()]
 
bpTypeItemList() ==  bpListAndRecover function bpTerm
 
bpTerm() ==
          (bpName() or bpTrap()) and
            ((bpParenthesized function bpIdList and
              bpPush bfNameArgs (bpPop2(),bpPop1()))
                or bpName() and bpPush bfNameArgs(bpPop2(),bpPop1()))
                 or bpPush(bfNameOnly bpPop1())
 
bpIdList()== bpTuple function bpName
 
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
 
bpCaseItem()==
    (bpTerm() or bpTrap()) and
       (bpEqKey "EXIT" or bpTrap()) and
         (bpWhere() or bpTrap()) and
            bpPush bfCaseItem (bpPop2(),bpPop1())

