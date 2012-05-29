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
 
--%
--% Snapshot of the parser state
--%

structure %ParserState ==
  Record(toks: %List %Tokens, trees: %List %Ast, pren: %Short, scp: %Short)
    with
      parserTokens == (.toks)       -- remaining token sequence
      parserTrees == (.trees)       -- list of successful parse trees
      parserNesting == (.pren)      -- parenthesis nesting level
      parserScope == (.scp)         -- scope nesting level

makeParserState toks ==
  mk%ParserState(toks,nil,0,0)

--%
--% Translator global state
--%
structure %Translator ==
  Record(ipath: %String, fdefs: %List %Thing, sigs: %List %Thing,
    xports: %List %Identifier, csts: %List %Binding) with
      inputFilePath == (.ifile)        -- path to the input file
      functionDefinitions == (.fdefs)  -- functions defined in this TU
      globalSignatures == (.sigs)      -- signatures proclaimed by this TU
      exportedNames == (.xports)       -- names exported by this TU
      constantBindings == (.csts)      -- constants defined in this TU

makeTranslator ip ==
  mk%Translator(ip,nil,nil,nil,nil)

--%


bpFirstToken()==
  $stok:=
    $inputStream = nil => mk%Token("ERROR","NOMORE",tokenPosition $stok)
    first $inputStream
  $ttok := tokenValue $stok
  true
 
bpFirstTok()==
  $stok:=
    $inputStream = nil => mk%Token("ERROR","NOMORE",tokenPosition $stok)
    first $inputStream
  $ttok := tokenValue $stok
  $bpParenCount > 0 and tokenClass $stok = "KEY" =>
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
 
bpRequire(ps,f) ==
  apply(f,ps,nil) or bpTrap()

bpState() ==
  [$inputStream,$stack,$bpParenCount,$bpCount]

 
bpRestore(x)==
  $inputStream:=first x
  bpFirstToken()
  $stack:=second x
  $bpParenCount:=third x
  $bpCount:=CADDDR x
  true
 
bpPush(ps,x) ==
  $stack:=[x,:$stack]
 
bpPushId ps ==
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
 
bpIndentParenthesized(ps,f) ==
  $bpCount:local:=0
  a:=$stok
  bpEqPeek "OPAREN" =>
    $bpParenCount:=$bpParenCount+1
    bpNext()
    apply(f,ps,nil) and bpFirstTok() and
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
      bpPush(ps,bfTuple [])
      $bpParenCount:=$bpParenCount-1
      bpNextToken()
      true
    bpParenTrap(a)
  false
 
bpParenthesized(ps,f) ==
  a := $stok
  bpEqKey "OPAREN" =>
    apply(f,ps,nil) and (bpEqKey "CPAREN" or bpParenTrap(a)) => true
    bpEqKey "CPAREN" =>
      bpPush(ps,bfTuple [])
      true
    bpParenTrap(a)
  false
 
bpBracket(ps,f) ==
  a := $stok
  bpEqKey "OBRACK" =>
    apply(f,ps,nil) and (bpEqKey "CBRACK" or bpBrackTrap(a)) =>
      bpPush(ps,bfBracket bpPop1())
    bpEqKey "CBRACK" => bpPush(ps,[])
    bpBrackTrap(a)
  false
 
bpPileBracketed(ps,f) ==
  bpEqKey "SETTAB" => 
    bpEqKey "BACKTAB" => true
    apply(f,ps,nil) and (bpEqKey "BACKTAB" or bpPileTrap()) =>
      bpPush(ps,bfPile bpPop1())
    false
  false
 
bpListof(ps,f,str1,g)==
  apply(f,ps,nil) =>
    bpEqKey str1 and bpRequire(ps,f) =>
      a:=$stack
      $stack:=nil
      while bpEqKey str1 and bpRequire(ps,f) repeat nil
      $stack:=[reverse! $stack,:a]
      bpPush(ps,FUNCALL(g, [bpPop3(),bpPop2(),:bpPop1()]))
    true
  false
 
 
-- to do ,<backset>
bpListofFun(ps,f,h,g)==
  apply(f,ps,nil) =>
    apply(h,ps,nil) and bpRequire(ps,f) =>
      a:=$stack
      $stack:=nil
      while apply(h,ps,nil) and bpRequire(ps,f) repeat nil
      $stack:=[reverse! $stack,:a]
      bpPush(ps,FUNCALL(g, [bpPop3(),bpPop2(),:bpPop1()]))
    true
  false
 
bpList(ps,f,str1)==
  apply(f,ps,nil) =>
    bpEqKey str1 and bpRequire(ps,f) =>
      a:=$stack
      $stack:=nil
      while bpEqKey str1 and bpRequire(ps,f) repeat nil
      $stack:=[reverse! $stack,:a]
      bpPush(ps,[bpPop3(),bpPop2(),:bpPop1()])
    bpPush(ps,[bpPop1()])
  bpPush(ps,nil)
 
bpOneOrMore(ps,f) ==
  apply(f,ps,nil)=>
    a:=$stack
    $stack:=nil
    while apply(f,ps,nil) repeat nil
    $stack:=[reverse! $stack,:a]
    bpPush(ps,[bpPop2(),:bpPop1()])
  false
 
 
-- s must transform the head of the stack
bpAnyNo(ps,s) ==
  while apply(s,ps,nil) repeat nil
  true
 
 
-- AndOr(k,p,f)= k p
bpAndOr(ps,keyword,p,f)==
  bpEqKey keyword and bpRequire(ps,p)
    and bpPush(ps,FUNCALL(f, bpPop1()))
 
bpConditional(ps,f) ==
  bpEqKey "IF" and bpRequire(ps,function bpWhere) and (bpEqKey "BACKSET" or true) =>
    bpEqKey "SETTAB" =>
      bpEqKey "THEN" =>
	bpRequire(ps,f) and bpElse(ps,f) and bpEqKey "BACKTAB"
      bpMissing "THEN"
    bpEqKey "THEN" => bpRequire(ps,f) and bpElse(ps,f)
    bpMissing "then"
  false
 
bpElse(ps,f)==
  a:=bpState()
  bpBacksetElse() =>
    bpRequire(ps,f) and
      bpPush(ps,bfIf(bpPop3(),bpPop2(),bpPop1()))
  bpRestore a
  bpPush(ps,bfIfThenOnly(bpPop2(),bpPop1()))
 
bpBacksetElse()==
  bpEqKey "BACKSET" => bpEqKey "ELSE"
  bpEqKey "ELSE"
 
bpEqPeek s == 
  tokenClass $stok = "KEY" and symbolEq?(s,$ttok)
 
bpEqKey s ==
  tokenClass $stok = "KEY" and symbolEq?(s,$ttok) and bpNext()

bpEqKeyNextTok s ==   
  tokenClass $stok = "KEY" and symbolEq?(s,$ttok) and bpNextToken()
 
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
 
bpRecoverTrap ps ==
  bpFirstToken()
  pos1 := tokenPosition $stok
  bpMoveTo 0
  pos2 := tokenPosition $stok
  bpIgnoredFromTo(pos1, pos2)
  bpPush(ps,[['"pile syntax error"]])
 
bpListAndRecover(ps,f)==
  a := $stack
  b := nil
  $stack := nil
  done := false
  c := $inputStream
  while not done repeat
    found :=
      try apply(f,ps,nil)
      catch(e: BootParserException) => e
    if found is "TRAPPED"
    then
       $inputStream:=c
       bpRecoverTrap ps
    else if not found
	 then
	   $inputStream:=c
	   bpGeneralErrorHere()
	   bpRecoverTrap ps
    if bpEqKey "BACKSET"
    then
       c := $inputStream
    else if bpEqPeek "BACKTAB"  or $inputStream = nil
	 then
	    done := true
	 else
	   $inputStream := c
	   bpGeneralErrorHere()
	   bpRecoverTrap ps
	   if bpEqPeek "BACKTAB"  or $inputStream = nil
	   then done:=true
	   else
	       bpNext()
	       c := $inputStream
    b := [bpPop1(),:b]
  $stack := a
  bpPush(ps,reverse! b)
 
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
bpQualifiedName ps ==
  bpEqPeek "COLON-COLON" =>
    bpNext()
    tokenClass $stok = "ID" and bpPushId ps and bpNext()
      and bpPush(ps,bfColonColon(bpPop2(), bpPop1()))
  false

++ Name:
++   ID
++   Name :: ID
bpName ps ==
  tokenClass $stok = "ID" =>
    bpPushId ps
    bpNext()
    bpAnyNo(ps,function bpQualifiedName)
  false

++ Constant:
++   INTEGER
++   FLOAT
++   LISP
++   LISPEXPR
++   LINE
++   QUOTE S-Expression
++   STRING
bpConstTok ps ==
  tokenClass $stok in '(INTEGER FLOAT) =>
    bpPush(ps,$ttok)
    bpNext()
  tokenClass $stok = "LISP" => bpPush(ps,%Lisp $ttok) and bpNext()
  tokenClass $stok = "LISPEXP" => bpPush(ps,$ttok) and bpNext()
  tokenClass $stok = "LINE" => bpPush(ps,["+LINE", $ttok]) and bpNext()
  bpEqPeek "QUOTE" =>
    bpNext()
    bpRequire(ps,function bpSexp) and
      bpPush(ps,bfSymbol bpPop1())
  bpString ps or bpFunction ps

bpChar ps ==
  tokenClass $stok = "ID" and $ttok is "char" =>
    a := bpState()
    bpApplication ps =>
      s := bpPop1()
      s is ["char",.] => bpPush(ps,s)
      bpRestore a
      false
    false
  false

++ Subroutine of bpExportItem.  Parses tails of ExportItem.
bpExportItemTail ps ==
  bpEqKey "BEC" and bpRequire(ps,function bpAssign) and
    bpPush(ps,%Assignment(bpPop2(), bpPop1()))
      or bpSimpleDefinitionTail ps

++ ExportItem:
++   Structure
++   TypeAliasDefinition
++   Signature
++   Signature := Where
++   Signature  == Where
bpExportItem ps ==
  bpEqPeek "STRUCTURE" => bpStruct ps
  a := bpState()
  bpName ps =>
    bpEqPeek "COLON" =>
      bpRestore a
      bpRequire(ps,function bpSignature)
      bpExportItemTail ps or true
    bpRestore a
    bpTypeAliasDefition ps
  false

++ ExportItemList:
++    Signature
++    ExportItemList Signature
bpExportItemList ps ==
  bpListAndRecover(ps,function bpExportItem)

++ ModuleInterface:
++   WHERE pile-bracketed ExporItemList
bpModuleInterface ps ==
  bpEqKey "WHERE" =>
    bpPileBracketed(ps,function bpExportItemList)
      or (bpExportItem ps and bpPush(ps,[bpPop1()]))
        or bpTrap()
  bpPush(ps,nil)

++ ModuleExports:
++   OPAREN IdList CPAREN
bpModuleExports ps ==
  bpParenthesized(ps,function bpIdList) => bpPush(ps,bfUntuple bpPop1())
  bpPush(ps,nil)

++ Parse a module definitoin
++   Module:
++     MODULE Name OptionalModuleExports OptionalModuleInterface
bpModule ps ==
  bpEqKey "MODULE" => 
    bpRequire(ps,function bpName)
    bpModuleExports ps
    bpModuleInterface ps
    bpPush(ps,%Module(bpPop3(),bpPop2(),bpPop1()))
  nil

++ Parse a module import, or a import declaration for a foreign entity.
++ Import:
++    IMPORT Signature FOR Name
++    IMPORT Name
++    IMPORT NAMESPACE LongName
bpImport ps ==
  bpEqKey "IMPORT" =>
    bpEqKey "NAMESPACE" =>
      bpLeftAssoc(ps,'(DOT),function bpName) and
        bpPush(ps,%Import bfNamespace bpPop1())
          or bpTrap()
    a := bpState()
    bpRequire(ps,function bpName)
    bpEqPeek "COLON" =>
      bpRestore a
      bpRequire(ps,function bpSignature) and 
        (bpEqKey "FOR" or bpTrap()) and
           bpRequire(ps,function bpName) and
              bpPush(ps,%ImportSignature(bpPop1(), bpPop1()))
    bpPush(ps,%Import bpPop1())
  false

++
++ Namespace:
++    NAMESPACE Name
bpNamespace ps ==
  bpEqKey "NAMESPACE" and (bpName ps or bpDot ps) and
    bpPush(ps,bfNamespace bpPop1())

-- Parse a type alias defnition:
--    type-alias-definition: 
--          identifier <=> logical-expression
bpTypeAliasDefition ps ==
  (bpTerm(ps,function bpIdList) or bpTrap()) and 
    bpEqKey "TDEF" and bpLogical ps and
      bpPush(ps,%TypeAlias(bpPop2(), bpPop1()))

++ Parse a signature declaration
++  Signature:
++    Name COLON Mapping
bpSignature ps ==
  bpName ps and bpEqKey "COLON" and bpRequire(ps,function bpTyping)
    and bpPush(ps,%Signature(bpPop2(), bpPop1()))

++ SimpleMapping:
++   Application
++   Application -> Application
bpSimpleMapping ps ==
  bpApplication ps =>
    bpEqKey "ARROW" and bpRequire(ps,function bpApplication) and
      bpPush(ps,%Mapping(bpPop1(), [bpPop1()]))
    true
  false

++ ArgtypeList:
++   ( ArgtypeSequence )
++ ArgtypeSequence:
++   SimpleMapping
++   SimpleMapping , ArgtypeSequence
bpArgtypeList ps ==
  bpTuple(ps,function bpSimpleMapping)

++ Parse a mapping expression
++   Mapping:
++     ArgtypeList -> Application
bpMapping ps ==
  bpParenthesized(ps,function bpArgtypeList) and 
     bpEqKey "ARROW" and bpApplication ps and 
       bpPush(ps,%Mapping(bpPop1(), bfUntuple bpPop1()))

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
  n>0=> [mk%Token("KEY","SETTAB",tokenPosition $stok),:bpAddTokens(n-1)]
  [mk%Token("KEY","BACKTAB",tokenPosition $stok),:bpAddTokens(n+1)]
 
bpExceptions()==
  bpEqPeek "DOT" or bpEqPeek "QUOTE" or
       bpEqPeek "OPAREN" or bpEqPeek "CPAREN" or
          bpEqPeek "SETTAB" or bpEqPeek "BACKTAB"
             or bpEqPeek "BACKSET"
 
 
bpSexpKey ps ==
  tokenClass $stok = "KEY" and not bpExceptions() =>
    a := $ttok has SHOEINF
    a = nil => bpPush(ps,keywordId $ttok) and bpNext()
    bpPush(ps,a) and bpNext()
  false
 
bpAnyId ps ==
  bpEqKey "MINUS"  and (tokenClass $stok = "INTEGER" or bpTrap()) and
          bpPush(ps,-$ttok) and bpNext() or
             bpSexpKey ps or
                   tokenClass $stok in '(ID INTEGER STRING FLOAT)
                      and  bpPush(ps,$ttok) and  bpNext()
 
bpSexp ps ==
  bpAnyId ps or
      bpEqKey "QUOTE"  and bpRequire(ps,function bpSexp)
         and bpPush(ps,bfSymbol bpPop1()) or
             bpIndentParenthesized(ps,function bpSexp1)
 
bpSexp1 ps == bpFirstTok() and
  bpSexp ps and
   (bpEqKey "DOT" and bpSexp ps and bpPush(ps,[bpPop2(),:bpPop1()]) or
         bpSexp1 ps and bpPush(ps,[bpPop2(),:bpPop1()])) or
             bpPush(ps,nil)
 
bpPrimary1 ps ==
  bpParenthesizedApplication ps or
    bpDot ps or
      bpConstTok ps or 
        bpConstruct ps or
          bpCase ps or
            bpStruct ps or
              bpPDefinition ps or
                bpBPileDefinition ps

bpParenthesizedApplication ps ==
  bpName ps and bpAnyNo(ps,function bpArgumentList)

bpArgumentList ps ==
  bpPDefinition ps and 
    bpPush(ps,bfApplication(bpPop2(), bpPop1()))
 
bpPrimary ps ==
  bpFirstTok() and (bpPrimary1 ps or bpPrefixOperator ps )
 
bpDot ps ==
  bpEqKey "DOT" and bpPush(ps,bfDot())
 
bpPrefixOperator ps ==
   tokenClass $stok = "KEY" and
     $ttok has SHOEPRE and bpPushId ps and  bpNext()
 
bpInfixOperator ps ==
  tokenClass $stok = "KEY" and
    $ttok has SHOEINF and bpPushId ps and  bpNext()
 
bpSelector ps ==
  bpEqKey "DOT" and (bpPrimary ps
     and bpPush(ps,bfElt(bpPop2(),bpPop1()))
	or bpPush(ps,bfSuffixDot bpPop1()))
 
bpApplication ps==
   bpPrimary ps and bpAnyNo(ps,function bpSelector) and
      (bpApplication ps and
            bpPush(ps,bfApplication(bpPop2(),bpPop1())) or true)
              or bpNamespace ps
 
++ Typing:
++   SimpleType
++   Mapping
++   FORALL Variable DOT Typing
bpTyping ps ==
  bpEqKey "FORALL" =>
    bpRequire(ps,function bpVariable)
    (bpDot ps and bpPop1()) or bpTrap()
    bpRequire(ps,function bpTyping)
    bpPush(ps,%Forall(bpPop2(), bpPop1()))
  bpMapping ps or bpSimpleMapping ps

++ Typed:
++   Application : Typing
++   Application @ Typing
bpTyped ps ==
  bpApplication ps and
     bpEqKey "COLON" =>
       bpRequire(ps,function bpTyping) and
         bpPush(ps,bfTagged(bpPop2(),bpPop1()))
     bpEqKey "AT" =>
       bpRequire(ps,function bpTyping) and
         bpPush(ps,bfRestrict(bpPop2(), bpPop1()))
     true
 
bpExpt ps == bpRightAssoc(ps,'(POWER),function bpTyped)
 
bpInfKey(ps,s) ==
  tokenClass $stok = "KEY" and
    symbolMember?($ttok,s) and bpPushId ps and bpNext()
 
bpInfGeneric(ps,s) ==
  bpInfKey(ps,s) and (bpEqKey "BACKSET" or true)
 
bpRightAssoc(ps,o,p)==
  a := bpState()
  apply(p,ps,nil) =>
    while  bpInfGeneric(ps,o) and (bpRightAssoc(ps,o,p) or bpTrap()) repeat
      bpPush(ps,bfInfApplication(bpPop2(),bpPop2(),bpPop1()))
    true
  bpRestore a
  false
 
bpLeftAssoc(ps,operations,parser)==
  apply(parser,ps,nil) =>
    while bpInfGeneric(ps,operations) and bpRequire(ps,parser)
     repeat
       bpPush(ps,bfInfApplication(bpPop2(),bpPop2(),bpPop1()))
    true
  false
 
bpString ps ==
  tokenClass $stok = "STRING" and
    bpPush(ps,quote makeSymbol $ttok) and bpNext()
 
bpFunction ps ==
  bpEqKey "FUNCTION" and bpRequire(ps,function bpPrimary1)
    and bpPush(ps,bfFunction bpPop1())

bpThetaName ps ==
  tokenClass $stok = "ID" and $ttok has SHOETHETA =>
    bpPushId ps
    bpNext()
  false
 
bpReduceOperator ps ==
   bpInfixOperator ps or bpString ps or bpThetaName ps
 
bpReduce ps==
  a := bpState()
  bpReduceOperator ps and bpEqKey "SLASH" =>
    bpEqPeek "OBRACK" => 
      bpRequire(ps,function bpDConstruct) and
	bpPush(ps,bfReduceCollect(bpPop2(),bpPop1()))
    bpRequire(ps,function bpApplication) and
      bpPush(ps,bfReduce(bpPop2(),bpPop1()))
  bpRestore a
  false
 
bpTimes ps ==
    bpReduce ps or bpLeftAssoc(ps,'(TIMES  SLASH),function bpExpt)

bpEuclid ps ==
  bpLeftAssoc(ps,'(QUO REM),function bpTimes)
 
bpMinus ps ==
  bpInfGeneric(ps,'(MINUS)) and bpRequire(ps,function bpEuclid)
    and bpPush(ps,bfApplication(bpPop2(),bpPop1()))
      or bpEuclid ps
 
bpArith ps ==
  bpLeftAssoc(ps,'(PLUS MINUS),function bpMinus)
 
bpIs ps ==
  bpArith ps and
     bpInfKey(ps,'(IS ISNT)) and bpRequire(ps,function bpPattern) =>
       bpPush(ps,bfISApplication(bpPop2(),bpPop2(),bpPop1()))
     bpEqKey "HAS" and bpRequire(ps,function bpApplication) =>
       bpPush(ps,bfHas(bpPop2(), bpPop1()))
     true
 
bpBracketConstruct(ps,f)==
  bpBracket(ps,f) and bpPush(ps,bfConstruct bpPop1())
 
bpCompare ps ==
  bpIs ps and (bpInfKey(ps,'(SHOEEQ SHOENE LT LE GT GE IN))
     and bpRequire(ps,function bpIs)
	and bpPush(ps,bfInfApplication(bpPop2(),bpPop2(),bpPop1()))
	    or true)
              or bpLeave ps
                or bpThrow ps
 
bpAnd ps == 
  bpLeftAssoc(ps,'(AND),function bpCompare)

bpThrow ps ==
  bpEqKey "THROW" and bpApplication ps =>
    -- Allow user-supplied matching type tag
    if bpEqKey "COLON" then
      bpRequire(ps,function bpApplication)
      bpPush(ps,%Pretend(bpPop2(),bpPop1()))
    bpPush(ps,bfThrow bpPop1())
  nil

++  Try:
++    try Assign CatchItems
bpTry ps ==
  bpEqKey "TRY" =>
    bpAssign ps
    cs := []
    while bpHandler "CATCH" repeat
      bpCatchItem ps
      cs := [bpPop1(),:cs]
    bpHandler "FINALLY" =>
      bpFinally ps and
        bpPush(ps,bfTry(bpPop2(),reverse! [bpPop1(),:cs]))
    cs = nil => bpTrap() -- missing handlers
    bpPush(ps,bfTry(bpPop1(),reverse! cs))
  nil            

bpCatchItem ps ==
  bpRequire(ps,function bpExceptionVariable) and
    (bpEqKey "EXIT" or bpTrap()) and
      bpRequire(ps,function bpAssign) and
        bpPush(ps,%Catch(bpPop2(),bpPop1()))

bpExceptionVariable ps ==
  t := $stok
  bpEqKey "OPAREN" and 
    bpRequire(ps,function bpSignature) and
      (bpEqKey "CPAREN" or bpMissing t)
        or bpTrap()

bpFinally ps ==
  bpRequire(ps,function bpAssign) and
    bpPush(ps,%Finally bpPop1())

bpHandler key ==
  s := bpState()
  (bpEqKey "BACKSET" or bpEqKey "SEMICOLON") and bpEqKey key => true
  bpRestore s
  false

++ Leave:
++   LEAVE Logical
bpLeave ps ==
  bpEqKey "LEAVE" and bpRequire(ps,function bpLogical) and
    bpPush(ps,bfLeave bpPop1())

++ Do:
++  IN Namespace Do
++  DO Assign
bpDo ps ==
  bpEqKey "IN" =>
    bpRequire(ps,function bpNamespace)
    bpRequire(ps,function bpDo)
    bpPush(ps,bfAtScope(bpPop2(),bpPop1()))
  bpEqKey "DO" and bpRequire(ps,function bpAssign) and
    bpPush(ps,bfDo bpPop1())

++ Return:
++   RETURN Assign
++   Leave
++   Throw
++   And
bpReturn ps==
  (bpEqKey "RETURN" and bpRequire(ps,function bpAssign) and
	 bpPush(ps,bfReturnNoName bpPop1())) 
    or bpLeave ps
      or bpThrow ps
        or bpAnd ps
          or bpDo ps
 
 
bpLogical ps ==
  bpLeftAssoc(ps,'(OR),function bpReturn)
 
bpExpression ps ==
  bpEqKey "COLON" and (bpLogical ps and
     bpPush(ps,bfApplication ("COLON",bpPop1()))
           or bpTrap()) or bpLogical ps
 
bpStatement ps ==
  bpConditional(ps,function bpWhere) or bpLoop ps
    or bpExpression ps
      or bpTry ps
 
bpLoop ps ==
  bpIterators ps and
   (bpCompMissing "REPEAT" and
      bpRequire(ps,function bpWhere) and
         bpPush(ps,bfLp(bpPop2(),bpPop1())))
             or
               bpEqKey "REPEAT" and bpRequire(ps,function bpLogical) and
                    bpPush(ps,bfLoop1 bpPop1())
 
bpSuchThat ps ==
  bpAndOr(ps,"BAR",function bpWhere,function bfSuchthat)
 
bpWhile ps ==
  bpAndOr(ps,"WHILE",function bpLogical,function bfWhile)
 
bpUntil ps ==
  bpAndOr(ps,"UNTIL",function bpLogical,function bfUntil)
 
bpFormal ps ==
  bpVariable ps or bpDot ps

bpForIn ps ==
  bpEqKey "FOR" and bpRequire(ps,function bpFormal) and (bpCompMissing "IN")
      and (bpRequire(ps,function bpSeg) and
       (bpEqKey "BY" and bpRequire(ps,function bpArith) and
        bpPush(ps,bfForInBy(bpPop3(),bpPop2(),bpPop1()))) or
         bpPush(ps,bfForin(bpPop2(),bpPop1())))
 
bpSeg ps ==
   bpArith ps and
      (bpEqKey "SEG" and
       (bpArith ps and bpPush(ps,bfSegment2(bpPop2(),bpPop1()))
         or bpPush(ps,bfSegment1(bpPop1()))) or true)
 
bpIterator ps ==
  bpForIn ps or bpSuchThat ps or bpWhile ps or bpUntil ps
 
bpIteratorList ps ==
  bpOneOrMore(ps,function bpIterator)
    and bpPush(ps,bfIterators bpPop1())
 
bpCrossBackSet ps ==
  bpEqKey "CROSS" and (bpEqKey "BACKSET" or true)
 
bpIterators ps ==
  bpListofFun(ps,function bpIteratorList,
    function bpCrossBackSet,function bfCross)
 
bpAssign ps ==
  a := bpState()
  bpStatement ps =>
    bpEqPeek "BEC" =>
      bpRestore a
      bpRequire(ps,function bpAssignment)
    bpEqPeek "GIVES" =>
      bpRestore a
      bpRequire(ps,function bpLambda)
    bpEqPeek "LARROW" =>
      bpRestore a
      bpRequire(ps,function bpKeyArg)
    true
  bpRestore a
  false
 
bpAssignment ps ==
  bpAssignVariable ps and
    bpEqKey "BEC" and
      bpRequire(ps,function bpAssign) and
	 bpPush(ps,bfAssign(bpPop2(),bpPop1()))
 
++ Parse a lambda expression
++   Lambda ::= Variable +-> Assign
bpLambda ps ==
  bpVariable ps and
    bpEqKey "GIVES" and
      bpRequire(ps,function bpAssign) and
        bpPush(ps,bfLambda(bpPop2(),bpPop1()))

bpKeyArg ps ==
  bpName ps and bpEqKey "LARROW" and bpLogical ps and
    bpPush(ps,bfKeyArg(bpPop2(),bpPop1()))

-- should only be allowed in sequences
bpExit ps ==
  bpAssign ps and (bpEqKey "EXIT" and
      (bpRequire(ps,function bpWhere) and
	 bpPush(ps,bfExit(bpPop2(),bpPop1())))
	   or true)

bpDefinition ps ==
  bpEqKey "MACRO" =>
    bpName ps and bpStoreName() and
      bpCompoundDefinitionTail(ps,function %Macro)
        or bpTrap()
  a := bpState()
  bpExit ps =>
    bpEqPeek "DEF" =>
       bpRestore a
       bpDef ps
    bpEqPeek "TDEF" =>
       bpRestore a
       bpTypeAliasDefition ps
    true
  bpRestore a
  false
 
bpStoreName()==
  $op := first $stack
  $wheredefs := nil
  $typings := nil
  true

bpDef ps ==  
  bpName ps and bpStoreName() and bpDefTail(ps,function %Definition)
    or bpNamespace ps and bpSimpleDefinitionTail ps
 
bpDDef ps ==
  bpName ps and bpDefTail(ps,function %Definition)

++ Parse the remaining of a simple definition.
bpSimpleDefinitionTail ps ==
  bpEqKey "DEF" and
    bpRequire(ps,function bpWhere)
      and bpPush(ps,%ConstantDefinition(bpPop2(), bpPop1()))

++ Parse the remaining of a compound definition.
bpCompoundDefinitionTail(ps,f) ==
  bpVariable ps and 
    bpEqKey "DEF" and bpRequire(ps,function bpWhere) and 
      bpPush(ps,apply(f,[bpPop3(),bpPop2(),bpPop1()]))


++ Parse the remainding of a definition.  When we reach this point
++ we know we must parse a definition and we have already parsed
++ the name of the main operator in the definition.
bpDefTail(ps,f) ==
  bpSimpleDefinitionTail ps
    or bpCompoundDefinitionTail(ps,f)
 
bpWhere ps ==
    bpDefinition ps and
       (bpEqKey "WHERE" and bpRequire(ps,function bpDefinitionItem)
           and bpPush(ps,bfWhere(bpPop1(),bpPop1())) or true)
 
bpDefinitionItem ps ==
  a := bpState()
  bpDDef ps => true
  bpRestore a
  bpBDefinitionPileItems ps => true
  bpRestore a
  bpPDefinitionItems ps => true
  bpRestore a
  bpWhere ps
 
bpDefinitionPileItems ps ==
  bpListAndRecover(ps,function bpDefinitionItem)
    and bpPush(ps,%Pile bpPop1())
 
bpBDefinitionPileItems ps ==
  bpPileBracketed(ps,function bpDefinitionPileItems)
 
bpSemiColonDefinition ps ==
  bpSemiListing(ps,function bpDefinitionItem,function %Pile)
 
bpPDefinitionItems ps ==
  bpParenthesized(ps,function bpSemiColonDefinition)
 
bpComma ps == 
  bpModule ps or bpImport ps or bpTuple(ps,function bpWhere)
 
bpTuple(ps,p) ==
  bpListofFun(ps,p,function bpCommaBackSet,function bfTuple)
 
bpCommaBackSet ps == 
  bpEqKey "COMMA" and (bpEqKey "BACKSET" or true)
 
bpSemiColon ps == 
  bpSemiListing(ps,function bpComma,function bfSequence)
 
bpSemiListing(ps,p,f) ==
  bpListofFun(ps,p,function bpSemiBackSet,f)
 
bpSemiBackSet ps == 
  bpEqKey "SEMICOLON" and (bpEqKey "BACKSET" or true)
 
bpPDefinition ps ==
  bpIndentParenthesized(ps,function bpSemiColon)
 
bpPileItems ps ==
  bpListAndRecover(ps,function bpSemiColon) and bpPush(ps,bfSequence bpPop1())
 
bpBPileDefinition ps ==
  bpPileBracketed(ps,function bpPileItems)
 
bpIteratorTail ps ==
  (bpEqKey "REPEAT" or true) and bpIterators ps
 
bpConstruct ps ==
  bpBracket(ps,function bpConstruction)
 
bpConstruction ps==
  bpComma ps and
     (bpIteratorTail ps and
	  bpPush(ps,bfCollect(bpPop2(),bpPop1())) or
	     bpPush(ps,bfTupleConstruct bpPop1()))
 
bpDConstruct ps ==
  bpBracket(ps,function bpDConstruction)
 
bpDConstruction ps ==
  bpComma ps and
     (bpIteratorTail ps and
	  bpPush(ps,bfDCollect(bpPop2(),bpPop1())) or
	     bpPush(ps,bfDTuple bpPop1()))
 
 
 
--PATTERN
 
bpPattern ps ==
  bpBracketConstruct(ps,function bpPatternL)
    or bpChar ps or bpName ps or bpConstTok ps
 
bpEqual ps ==
   bpEqKey "SHOEEQ" and (bpApplication ps or bpConstTok ps or
                bpTrap()) and bpPush(ps,bfEqual bpPop1())
 
bpRegularPatternItem ps ==
   bpEqual ps or
     bpConstTok ps or bpDot ps or
      bpName ps and
         ((bpEqKey "BEC" and bpRequire(ps,function bpPattern)
              and bpPush(ps,bfAssign(bpPop2(),bpPop1()))) or true)
                    or bpBracketConstruct(ps,function bpPatternL)
 
bpRegularPatternItemL ps ==
      bpRegularPatternItem ps and bpPush(ps,[bpPop1()])
 
bpRegularList ps ==
       bpListof(ps,function bpRegularPatternItemL,"COMMA",function bfAppend)
 
bpPatternColon ps ==
     bpEqKey "COLON" and bpRequire(ps,function bpRegularPatternItem)
              and bpPush(ps,[bfColon bpPop1()])
 
 
-- only one colon
bpPatternL ps ==
  bpPatternList ps and bpPush(ps,bfTuple bpPop1())
 
bpPatternList ps ==
  bpRegularPatternItemL ps =>
    while (bpEqKey  "COMMA" and (bpRegularPatternItemL ps or
	(bpPatternTail ps
	  and bpPush(ps,append(bpPop2(),bpPop1()))
	    or bpTrap();false) )) repeat
	      bpPush(ps,append(bpPop2(),bpPop1()))
    true
  bpPatternTail ps
 
bpPatternTail ps ==
  bpPatternColon ps and
      (bpEqKey "COMMA" and bpRequire(ps,function bpRegularList)
	   and bpPush(ps,append(bpPop2(),bpPop1())) or true)
 
-- BOUND VARIABLE

++ We are parsing parameters in a function definition.  We have
++ just seen a parameter name; we are attempting to see whether
++ it might be followed by a type annotation, or whether it actually
++ a form with a specific pattern structure, or whether it has
++ a default value.
bpRegularBVItemTail ps ==
  bpEqKey "COLON" and bpRequire(ps,function bpApplication) and 
    bpPush(ps,bfTagged(bpPop2(), bpPop1()))
      or bpEqKey "BEC" and bpRequire(ps,function bpPattern) and
	 bpPush(ps,bfAssign(bpPop2(),bpPop1()))
	   or bpEqKey "IS" and bpRequire(ps,function bpPattern) and
	      bpPush(ps,bfAssign(bpPop2(),bpPop1()))
             or bpEqKey "DEF" and bpRequire(ps,function bpApplication) and
                bpPush(ps,%DefaultValue(bpPop2(), bpPop1()))


bpRegularBVItem ps ==
  bpBVString ps 
    or bpConstTok ps 
      or (bpName ps and (bpRegularBVItemTail ps or true))
        or bpBracketConstruct(ps,function bpPatternL)
 
bpBVString ps ==
  tokenClass $stok = "STRING" and
      bpPush(ps,["BVQUOTE",makeSymbol $ttok]) and bpNext()
 
bpRegularBVItemL ps ==
  bpRegularBVItem ps and bpPush(ps,[bpPop1()])
 
bpColonName ps ==
  bpEqKey "COLON" and (bpName ps or bpBVString ps or bpTrap())
 
 
-- at most one colon at end
bpBoundVariablelist ps ==
  bpRegularBVItemL ps =>
    while (bpEqKey  "COMMA" and (bpRegularBVItemL ps or
	(bpColonName ps
	  and bpPush(ps,bfColonAppend(bpPop2(),bpPop1()))
	    or bpTrap();false) )) repeat
	       bpPush(ps,append(bpPop2(),bpPop1()))
    true
  bpColonName ps and bpPush(ps,bfColonAppend(nil,bpPop1()))
 

bpVariable ps ==
    bpParenthesized(ps,function bpBoundVariablelist) and
       bpPush(ps,bfTupleIf bpPop1())
         or bpBracketConstruct(ps,function bpPatternL)
                or bpName ps or bpConstTok ps
 
bpAssignVariable ps ==
      bpBracketConstruct(ps,function bpPatternL) or bpAssignLHS ps
 
bpAssignLHS ps ==
  not bpName ps => false
  bpEqKey "COLON" =>          -- variable declaration
    bpRequire(ps,function bpApplication)
    bpPush(ps,bfLocal(bpPop2(),bpPop1()))
  bpArgumentList ps and 
    (bpEqPeek "DOT" 
      or (bpEqPeek "BEC" and bpPush(ps,bfPlace bpPop1())) 
        or bpTrap())
  bpEqKey "DOT" =>            -- field path
    bpList(ps,function bpPrimary,"DOT") and 
      bpChecknull ps and
        bpPush(ps,bfTuple([bpPop2(),:bpPop1()]))
  true

bpChecknull ps ==
  a := bpPop1()
  a = nil => bpTrap()
  bpPush(ps,a)

bpStruct ps ==
   bpEqKey "STRUCTURE" and
      bpRequire(ps,function bpName) and
        (bpEqKey "DEF" or bpTrap()) and
           (bpRecord ps or bpTypeList ps) and
             bpPush(ps,%Structure(bpPop2(),bpPop1()))
 
++ Record:
++   "Record" "(" FieldList ")"
bpRecord ps ==
  s := bpState()
  bpName ps and bpPop1() is "Record" =>
    (bpParenthesized(ps,function bpFieldList) or bpTrap()) and
      bpGlobalAccessors ps and
        bpPush(ps,%Record(bfUntuple bpPop2(),bpPop1()))
  bpRestore s
  false

++ FieldList:
++    Signature
++    Signature , FieldList
bpFieldList ps ==
  bpTuple(ps,function bpSignature)

bpGlobalAccessors ps ==
  bpEqKey "WITH" =>
    bpPileBracketed(ps,function bpAccessorDefinitionList) or bpTrap()
  bpPush(ps,nil)

bpAccessorDefinitionList ps ==
  bpListAndRecover(ps,function bpAccessorDefinition)

++ AccessorDefinition:
++   Name DEF FieldSection
bpAccessorDefinition ps ==
  bpRequire(ps,function bpName) and
    (bpEqKey "DEF" or bpTrap()) and
       bpRequire(ps,function bpFieldSection) and
         bpPush(ps,%AccessorDef(bpPop2(),bpPop1()))

++ FieldSection:
++    "(" DOT Name ")"
bpFieldSection ps ==
  bpParenthesized(ps,function bpSelectField)

bpSelectField ps ==
  bpEqKey "DOT" and bpName ps

bpTypeList ps == 
  bpPileBracketed(ps,function bpTypeItemList)
    or bpTypeItem ps and bpPush(ps,[bpPop1()])

bpTypeItem ps ==
  bpTerm(ps,function bpIdList)
 
bpTypeItemList ps ==  
  bpListAndRecover(ps,function bpTypeItem)
 
bpTerm(ps,idListParser) ==
  bpRequire(ps,function bpName) and
    ((bpParenthesized(ps,idListParser) and
      bpPush(ps,bfNameArgs(bpPop2(),bpPop1())))
	or bpName ps and bpPush(ps,bfNameArgs(bpPop2(),bpPop1())))
	 or bpPush(ps,bfNameOnly bpPop1())
 
bpIdList ps == 
  bpTuple(ps,function bpName)
 
bpCase ps ==
  bpEqKey "CASE" and
    bpRequire(ps,function bpWhere) and
       (bpEqKey "OF" or bpMissing "OF") and
	     bpPiledCaseItems ps
 
bpPiledCaseItems ps ==
   bpPileBracketed(ps,function bpCaseItemList) and
       bpPush(ps,bfCase(bpPop2(),bpPop1()))

bpCaseItemList ps ==
   bpListAndRecover(ps,function bpCaseItem)

bpCasePatternVar ps ==
  bpName ps or bpDot ps

bpCasePatternVarList ps ==
  bpTuple(ps,function bpCasePatternVar)
 
bpCaseItem ps ==
    (bpTerm(ps,function bpCasePatternVarList) or bpTrap()) and
       (bpEqKey "EXIT" or bpTrap()) and
         bpRequire(ps,function bpWhere) and
            bpPush(ps,bfCaseItem(bpPop2(),bpPop1()))


++ Main entry point into the parser module.
bpOutItem ps ==
  $op: local := nil
  $GenVarCounter: local := 0
  bpRequire(ps,function bpComma)
  b := bpPop1()
  t :=
    b is ["+LINE",:.] => [ b ]
    b is ["L%T",l,r] and symbol? l => 
      $InteractiveMode => [["SETQ",l,r]]
      [["DEFPARAMETER",l,r]]
    translateToplevel(b,false)
  bpPush(ps,t)
 
