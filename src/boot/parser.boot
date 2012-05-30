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


bpFirstToken ps ==
  $stok:=
    parserTokens ps = nil => mk%Token("ERROR","NOMORE",tokenPosition $stok)
    first parserTokens ps
  $ttok := tokenValue $stok
  true
 
bpFirstTok ps ==
  $stok:=
    parserTokens ps = nil => mk%Token("ERROR","NOMORE",tokenPosition $stok)
    first parserTokens ps
  $ttok := tokenValue $stok
  $bpParenCount > 0 and tokenClass $stok = "KEY" =>
    $ttok is "SETTAB" =>
      $bpCount:=$bpCount+1
      bpNext ps
    $ttok is "BACKTAB" =>
      $bpCount:=$bpCount-1
      bpNext ps
    $ttok is "BACKSET" =>
      bpNext ps
    true
  true
 
bpNext ps ==
  parserTokens(ps) := rest parserTokens ps
  bpFirstTok ps
 
bpNextToken ps ==
  parserTokens(ps) := rest parserTokens ps
  bpFirstToken ps
 
bpRequire(ps,f) ==
  apply(f,ps,nil) or bpTrap()

bpState ps ==
  [parserTokens ps,parserTrees ps,$bpParenCount,$bpCount]

 
bpRestore(ps,x)==
  parserTokens(ps) := first x
  bpFirstToken ps
  parserTrees(ps) := second x
  $bpParenCount:=third x
  $bpCount:=CADDDR x
  true
 
bpPush(ps,x) ==
  parserTrees(ps) := [x,:parserTrees ps]
 
bpPushId ps ==
  parserTrees(ps) := [bfReName $ttok,:parserTrees ps]
 
bpPop1 ps ==
  a := first parserTrees ps
  parserTrees(ps) := rest parserTrees ps
  a
 
bpPop2 ps ==
  a := second parserTrees ps
  parserTrees(ps).rest := CDDR parserTrees ps
  a
 
bpPop3 ps ==
  a := third parserTrees ps
  parserTrees(ps).rest.rest := CDDDR parserTrees ps
  a
 
bpIndentParenthesized(ps,f) ==
  $bpCount:local:=0
  a:=$stok
  bpEqPeek "OPAREN" =>
    $bpParenCount:=$bpParenCount+1
    bpNext ps
    apply(f,ps,nil) and bpFirstTok ps and
	    (bpEqPeek "CPAREN" or bpParenTrap(a)) =>
      $bpParenCount:=$bpParenCount-1
      bpNextToken ps
      $bpCount=0 => true
      parserTokens(ps) := append(bpAddTokens $bpCount,parserTokens ps)
      bpFirstToken ps
      $bpParenCount=0 =>
	bpCancel ps
	true
      true
    bpEqPeek "CPAREN" =>
      bpPush(ps,bfTuple [])
      $bpParenCount:=$bpParenCount-1
      bpNextToken ps
      true
    bpParenTrap(a)
  false
 
bpParenthesized(ps,f) ==
  a := $stok
  bpEqKey(ps,"OPAREN") =>
    apply(f,ps,nil) and (bpEqKey(ps,"CPAREN") or bpParenTrap(a)) => true
    bpEqKey(ps,"CPAREN") =>
      bpPush(ps,bfTuple [])
      true
    bpParenTrap(a)
  false
 
bpBracket(ps,f) ==
  a := $stok
  bpEqKey(ps,"OBRACK") =>
    apply(f,ps,nil) and (bpEqKey(ps,"CBRACK") or bpBrackTrap(a)) =>
      bpPush(ps,bfBracket bpPop1 ps)
    bpEqKey(ps,"CBRACK") => bpPush(ps,[])
    bpBrackTrap(a)
  false
 
bpPileBracketed(ps,f) ==
  bpEqKey(ps,"SETTAB") => 
    bpEqKey(ps,"BACKTAB") => true
    apply(f,ps,nil) and (bpEqKey(ps,"BACKTAB") or bpPileTrap()) =>
      bpPush(ps,bfPile bpPop1 ps)
    false
  false
 
bpListof(ps,f,str1,g)==
  apply(f,ps,nil) =>
    bpEqKey(ps,str1) and bpRequire(ps,f) =>
      a := parserTrees ps
      parserTrees(ps) := nil
      while bpEqKey(ps,str1) and bpRequire(ps,f) repeat nil
      parserTrees(ps) := [reverse! parserTrees ps,:a]
      bpPush(ps,FUNCALL(g, [bpPop3 ps,bpPop2 ps,:bpPop1 ps]))
    true
  false
 
 
-- to do ,<backset>
bpListofFun(ps,f,h,g)==
  apply(f,ps,nil) =>
    apply(h,ps,nil) and bpRequire(ps,f) =>
      a := parserTrees ps
      parserTrees(ps) := nil
      while apply(h,ps,nil) and bpRequire(ps,f) repeat nil
      parserTrees(ps) := [reverse! parserTrees ps,:a]
      bpPush(ps,FUNCALL(g, [bpPop3 ps,bpPop2 ps,:bpPop1 ps]))
    true
  false
 
bpList(ps,f,str1)==
  apply(f,ps,nil) =>
    bpEqKey(ps,str1) and bpRequire(ps,f) =>
      a := parserTrees ps
      parserTrees(ps) := nil
      while bpEqKey(ps,str1) and bpRequire(ps,f) repeat nil
      parserTrees(ps) := [reverse! parserTrees ps,:a]
      bpPush(ps,[bpPop3 ps,bpPop2 ps,:bpPop1 ps])
    bpPush(ps,[bpPop1 ps])
  bpPush(ps,nil)
 
bpOneOrMore(ps,f) ==
  apply(f,ps,nil)=>
    a := parserTrees ps
    parserTrees(ps) := nil
    while apply(f,ps,nil) repeat nil
    parserTrees(ps) := [reverse! parserTrees ps,:a]
    bpPush(ps,[bpPop2 ps,:bpPop1 ps])
  false
 
 
-- s must transform the head of the stack
bpAnyNo(ps,s) ==
  while apply(s,ps,nil) repeat nil
  true
 
 
-- AndOr(k,p,f)= k p
bpAndOr(ps,keyword,p,f)==
  bpEqKey(ps,keyword) and bpRequire(ps,p)
    and bpPush(ps,FUNCALL(f, bpPop1 ps))
 
bpConditional(ps,f) ==
  bpEqKey(ps,"IF") and bpRequire(ps,function bpWhere) and (bpEqKey(ps,"BACKSET") or true) =>
    bpEqKey(ps,"SETTAB") =>
      bpEqKey(ps,"THEN") =>
	bpRequire(ps,f) and bpElse(ps,f) and bpEqKey(ps,"BACKTAB")
      bpMissing "THEN"
    bpEqKey(ps,"THEN") => bpRequire(ps,f) and bpElse(ps,f)
    bpMissing "then"
  false
 
bpElse(ps,f)==
  a := bpState ps
  bpBacksetElse ps =>
    bpRequire(ps,f) and
      bpPush(ps,bfIf(bpPop3 ps,bpPop2 ps,bpPop1 ps))
  bpRestore(ps,a)
  bpPush(ps,bfIfThenOnly(bpPop2 ps,bpPop1 ps))
 
bpBacksetElse ps ==
  bpEqKey(ps,"BACKSET") => bpEqKey(ps,"ELSE")
  bpEqKey(ps,"ELSE")
 
bpEqPeek s == 
  tokenClass $stok = "KEY" and symbolEq?(s,$ttok)
 
bpEqKey(ps,s) ==
  tokenClass $stok = "KEY" and symbolEq?(s,$ttok) and bpNext ps

bpEqKeyNextTok(ps,s) ==   
  tokenClass $stok = "KEY" and symbolEq?(s,$ttok) and bpNextToken ps
 
bpPileTrap()   == bpMissing  "BACKTAB"
bpBrackTrap(x) == bpMissingMate("]",x)
bpParenTrap(x) == bpMissingMate(")",x)
 
bpMissingMate(close,open)==
  bpSpecificErrorAtToken(open, '"possibly missing mate")
  bpMissing close
 
bpMissing s==
  bpSpecificErrorHere strconc(PNAME s,'" possibly missing")
  throw 'TRAPPED : BootParserException
 
bpCompMissing(ps,s) ==
  bpEqKey(ps,s) or bpMissing s
 
bpTrap()==
  bpGeneralErrorHere()
  throw 'TRAPPED : BootParserException
 
bpRecoverTrap ps ==
  bpFirstToken ps
  pos1 := tokenPosition $stok
  bpMoveTo(ps,0)
  pos2 := tokenPosition $stok
  bpIgnoredFromTo(pos1, pos2)
  bpPush(ps,[['"pile syntax error"]])
 
bpListAndRecover(ps,f)==
  a := parserTrees ps
  b := nil
  parserTrees(ps) := nil
  done := false
  c := parserTokens ps
  while not done repeat
    found :=
      try apply(f,ps,nil)
      catch(e: BootParserException) => e
    if found is "TRAPPED"
    then
       parserTokens(ps) := c
       bpRecoverTrap ps
    else if not found
	 then
	   parserTokens(ps) := c
	   bpGeneralErrorHere()
	   bpRecoverTrap ps
    if bpEqKey(ps,"BACKSET")
    then
       c := parserTokens ps
    else if bpEqPeek "BACKTAB" or parserTokens ps = nil
	 then
	    done := true
	 else
	   parserTokens(ps) := c
	   bpGeneralErrorHere()
	   bpRecoverTrap ps
	   if bpEqPeek "BACKTAB"  or parserTokens ps = nil
	   then done:=true
	   else
	       bpNext ps
	       c := parserTokens ps
    b := [bpPop1 ps,:b]
  parserTrees(ps) := a
  bpPush(ps,reverse! b)
 
bpMoveTo(ps,n) ==
   parserTokens ps = nil  => true
   bpEqPeek "BACKTAB" =>
     n=0  => true
     bpNextToken ps
     $bpCount:=$bpCount-1
     bpMoveTo(ps,n-1)
   bpEqPeek "BACKSET" =>
     n=0  => true
     bpNextToken ps
     bpMoveTo(ps,n)
   bpEqPeek "SETTAB"  =>
     bpNextToken ps
     bpMoveTo(ps,n+1)
   bpEqPeek "OPAREN"  =>
     bpNextToken ps
     $bpParenCount:=$bpParenCount+1
     bpMoveTo(ps,n)
   bpEqPeek "CPAREN"  =>
     bpNextToken ps
     $bpParenCount:=$bpParenCount-1
     bpMoveTo(ps,n)
   bpNextToken ps
   bpMoveTo(ps,n)
 
-- A fully qualified name could be interpreted as a left reduction
-- of an '::' infix operator.  At the moment, we don't use
-- that general interpretation.

-- When this routine is called, a symbol is already pushed on the
-- stack.  When this routine finished execution, we have either
-- reduced a '::' and a name, or nothing.  In either case, a 
-- symbol is present on the stack.
bpQualifiedName ps ==
  bpEqPeek "COLON-COLON" =>
    bpNext ps
    tokenClass $stok = "ID" and bpPushId ps and bpNext ps
      and bpPush(ps,bfColonColon(bpPop2 ps, bpPop1 ps))
  false

++ Name:
++   ID
++   Name :: ID
bpName ps ==
  tokenClass $stok = "ID" =>
    bpPushId ps
    bpNext ps
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
    bpNext ps
  tokenClass $stok = "LISP" => bpPush(ps,%Lisp $ttok) and bpNext ps
  tokenClass $stok = "LISPEXP" => bpPush(ps,$ttok) and bpNext ps
  tokenClass $stok = "LINE" => bpPush(ps,["+LINE", $ttok]) and bpNext ps
  bpEqPeek "QUOTE" =>
    bpNext ps
    bpRequire(ps,function bpSexp) and
      bpPush(ps,bfSymbol bpPop1 ps)
  bpString ps or bpFunction ps

bpChar ps ==
  tokenClass $stok = "ID" and $ttok is "char" =>
    a := bpState ps
    bpApplication ps =>
      s := bpPop1 ps
      s is ["char",.] => bpPush(ps,s)
      bpRestore(ps,a)
      false
    false
  false

++ Subroutine of bpExportItem.  Parses tails of ExportItem.
bpExportItemTail ps ==
  bpEqKey(ps,"BEC") and bpRequire(ps,function bpAssign) and
    bpPush(ps,%Assignment(bpPop2 ps, bpPop1 ps))
      or bpSimpleDefinitionTail ps

++ ExportItem:
++   Structure
++   TypeAliasDefinition
++   Signature
++   Signature := Where
++   Signature  == Where
bpExportItem ps ==
  bpEqPeek "STRUCTURE" => bpStruct ps
  a := bpState ps
  bpName ps =>
    bpEqPeek "COLON" =>
      bpRestore(ps,a)
      bpRequire(ps,function bpSignature)
      bpExportItemTail ps or true
    bpRestore(ps,a)
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
  bpEqKey(ps,"WHERE") =>
    bpPileBracketed(ps,function bpExportItemList)
      or (bpExportItem ps and bpPush(ps,[bpPop1 ps]))
        or bpTrap()
  bpPush(ps,nil)

++ ModuleExports:
++   OPAREN IdList CPAREN
bpModuleExports ps ==
  bpParenthesized(ps,function bpIdList) => bpPush(ps,bfUntuple bpPop1 ps)
  bpPush(ps,nil)

++ Parse a module definitoin
++   Module:
++     MODULE Name OptionalModuleExports OptionalModuleInterface
bpModule ps ==
  bpEqKey(ps,"MODULE") => 
    bpRequire(ps,function bpName)
    bpModuleExports ps
    bpModuleInterface ps
    bpPush(ps,%Module(bpPop3 ps,bpPop2 ps,bpPop1 ps))
  nil

++ Parse a module import, or a import declaration for a foreign entity.
++ Import:
++    IMPORT Signature FOR Name
++    IMPORT Name
++    IMPORT NAMESPACE LongName
bpImport ps ==
  bpEqKey(ps,"IMPORT") =>
    bpEqKey(ps,"NAMESPACE") =>
      bpLeftAssoc(ps,'(DOT),function bpName) and
        bpPush(ps,%Import bfNamespace bpPop1 ps)
          or bpTrap()
    a := bpState ps
    bpRequire(ps,function bpName)
    bpEqPeek "COLON" =>
      bpRestore(ps,a)
      bpRequire(ps,function bpSignature) and 
        (bpEqKey(ps,"FOR") or bpTrap()) and
           bpRequire(ps,function bpName) and
              bpPush(ps,%ImportSignature(bpPop1 ps, bpPop1 ps))
    bpPush(ps,%Import bpPop1 ps)
  false

++
++ Namespace:
++    NAMESPACE Name
bpNamespace ps ==
  bpEqKey(ps,"NAMESPACE") and (bpName ps or bpDot ps) and
    bpPush(ps,bfNamespace bpPop1 ps)

-- Parse a type alias defnition:
--    type-alias-definition: 
--          identifier <=> logical-expression
bpTypeAliasDefition ps ==
  (bpTerm(ps,function bpIdList) or bpTrap()) and 
    bpEqKey(ps,"TDEF") and bpLogical ps and
      bpPush(ps,%TypeAlias(bpPop2 ps, bpPop1 ps))

++ Parse a signature declaration
++  Signature:
++    Name COLON Mapping
bpSignature ps ==
  bpName ps and bpEqKey(ps,"COLON") and bpRequire(ps,function bpTyping)
    and bpPush(ps,%Signature(bpPop2 ps, bpPop1 ps))

++ SimpleMapping:
++   Application
++   Application -> Application
bpSimpleMapping ps ==
  bpApplication ps =>
    bpEqKey(ps,"ARROW") and bpRequire(ps,function bpApplication) and
      bpPush(ps,%Mapping(bpPop1 ps, [bpPop1 ps]))
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
     bpEqKey(ps,"ARROW") and bpApplication ps and 
       bpPush(ps,%Mapping(bpPop1 ps, bfUntuple bpPop1 ps))

bpCancel ps ==
  a := bpState ps
  bpEqKeyNextTok(ps,"SETTAB") =>
    bpCancel ps =>
      bpEqKeyNextTok(ps,"BACKTAB") => true
      bpRestore(ps,a)
      false
    bpEqKeyNextTok(ps,"BACKTAB") => true
    bpRestore(ps,a)
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
    a = nil => bpPush(ps,keywordId $ttok) and bpNext ps
    bpPush(ps,a) and bpNext ps
  false
 
bpAnyId ps ==
  bpEqKey(ps,"MINUS")  and (tokenClass $stok = "INTEGER" or bpTrap()) and
          bpPush(ps,-$ttok) and bpNext ps or
             bpSexpKey ps or
                   tokenClass $stok in '(ID INTEGER STRING FLOAT)
                      and  bpPush(ps,$ttok) and  bpNext ps
 
bpSexp ps ==
  bpAnyId ps or
      bpEqKey(ps,"QUOTE") and bpRequire(ps,function bpSexp)
         and bpPush(ps,bfSymbol bpPop1 ps) or
             bpIndentParenthesized(ps,function bpSexp1)
 
bpSexp1 ps == bpFirstTok ps and
  bpSexp ps and
   (bpEqKey(ps,"DOT") and bpSexp ps and bpPush(ps,[bpPop2 ps,:bpPop1 ps]) or
         bpSexp1 ps and bpPush(ps,[bpPop2 ps,:bpPop1 ps])) or
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
    bpPush(ps,bfApplication(bpPop2 ps, bpPop1 ps))
 
bpPrimary ps ==
  bpFirstTok ps and (bpPrimary1 ps or bpPrefixOperator ps )
 
bpDot ps ==
  bpEqKey(ps,"DOT") and bpPush(ps,bfDot())
 
bpPrefixOperator ps ==
   tokenClass $stok = "KEY" and
     $ttok has SHOEPRE and bpPushId ps and  bpNext ps
 
bpInfixOperator ps ==
  tokenClass $stok = "KEY" and
    $ttok has SHOEINF and bpPushId ps and  bpNext ps
 
bpSelector ps ==
  bpEqKey(ps,"DOT") and (bpPrimary ps
     and bpPush(ps,bfElt(bpPop2 ps,bpPop1 ps))
	or bpPush(ps,bfSuffixDot bpPop1 ps))
 
bpApplication ps==
   bpPrimary ps and bpAnyNo(ps,function bpSelector) and
      (bpApplication ps and
            bpPush(ps,bfApplication(bpPop2 ps,bpPop1 ps)) or true)
              or bpNamespace ps
 
++ Typing:
++   SimpleType
++   Mapping
++   FORALL Variable DOT Typing
bpTyping ps ==
  bpEqKey(ps,"FORALL") =>
    bpRequire(ps,function bpVariable)
    (bpDot ps and bpPop1 ps) or bpTrap()
    bpRequire(ps,function bpTyping)
    bpPush(ps,%Forall(bpPop2 ps, bpPop1 ps))
  bpMapping ps or bpSimpleMapping ps

++ Typed:
++   Application : Typing
++   Application @ Typing
bpTyped ps ==
  bpApplication ps and
     bpEqKey(ps,"COLON") =>
       bpRequire(ps,function bpTyping) and
         bpPush(ps,bfTagged(bpPop2 ps,bpPop1 ps))
     bpEqKey(ps,"AT") =>
       bpRequire(ps,function bpTyping) and
         bpPush(ps,bfRestrict(bpPop2 ps, bpPop1 ps))
     true
 
bpExpt ps == bpRightAssoc(ps,'(POWER),function bpTyped)
 
bpInfKey(ps,s) ==
  tokenClass $stok = "KEY" and
    symbolMember?($ttok,s) and bpPushId ps and bpNext ps
 
bpInfGeneric(ps,s) ==
  bpInfKey(ps,s) and (bpEqKey(ps,"BACKSET") or true)
 
bpRightAssoc(ps,o,p)==
  a := bpState ps
  apply(p,ps,nil) =>
    while  bpInfGeneric(ps,o) and (bpRightAssoc(ps,o,p) or bpTrap()) repeat
      bpPush(ps,bfInfApplication(bpPop2 ps,bpPop2 ps,bpPop1 ps))
    true
  bpRestore(ps,a)
  false
 
bpLeftAssoc(ps,operations,parser)==
  apply(parser,ps,nil) =>
    while bpInfGeneric(ps,operations) and bpRequire(ps,parser)
     repeat
       bpPush(ps,bfInfApplication(bpPop2 ps,bpPop2 ps,bpPop1 ps))
    true
  false
 
bpString ps ==
  tokenClass $stok = "STRING" and
    bpPush(ps,quote makeSymbol $ttok) and bpNext ps
 
bpFunction ps ==
  bpEqKey(ps,"FUNCTION") and bpRequire(ps,function bpPrimary1)
    and bpPush(ps,bfFunction bpPop1 ps)

bpThetaName ps ==
  tokenClass $stok = "ID" and $ttok has SHOETHETA =>
    bpPushId ps
    bpNext ps
  false
 
bpReduceOperator ps ==
   bpInfixOperator ps or bpString ps or bpThetaName ps
 
bpReduce ps ==
  a := bpState ps
  bpReduceOperator ps and bpEqKey(ps,"SLASH") =>
    bpEqPeek "OBRACK" => 
      bpRequire(ps,function bpDConstruct) and
	bpPush(ps,bfReduceCollect(bpPop2 ps,bpPop1 ps))
    bpRequire(ps,function bpApplication) and
      bpPush(ps,bfReduce(bpPop2 ps,bpPop1 ps))
  bpRestore(ps,a)
  false
 
bpTimes ps ==
    bpReduce ps or bpLeftAssoc(ps,'(TIMES  SLASH),function bpExpt)

bpEuclid ps ==
  bpLeftAssoc(ps,'(QUO REM),function bpTimes)
 
bpMinus ps ==
  bpInfGeneric(ps,'(MINUS)) and bpRequire(ps,function bpEuclid)
    and bpPush(ps,bfApplication(bpPop2 ps,bpPop1 ps))
      or bpEuclid ps
 
bpArith ps ==
  bpLeftAssoc(ps,'(PLUS MINUS),function bpMinus)
 
bpIs ps ==
  bpArith ps and
     bpInfKey(ps,'(IS ISNT)) and bpRequire(ps,function bpPattern) =>
       bpPush(ps,bfISApplication(bpPop2 ps,bpPop2 ps,bpPop1 ps))
     bpEqKey(ps,"HAS") and bpRequire(ps,function bpApplication) =>
       bpPush(ps,bfHas(bpPop2 ps, bpPop1 ps))
     true
 
bpBracketConstruct(ps,f)==
  bpBracket(ps,f) and bpPush(ps,bfConstruct bpPop1 ps)
 
bpCompare ps ==
  bpIs ps and (bpInfKey(ps,'(SHOEEQ SHOENE LT LE GT GE IN))
     and bpRequire(ps,function bpIs)
	and bpPush(ps,bfInfApplication(bpPop2 ps,bpPop2 ps,bpPop1 ps))
	    or true)
              or bpLeave ps
                or bpThrow ps
 
bpAnd ps == 
  bpLeftAssoc(ps,'(AND),function bpCompare)

bpThrow ps ==
  bpEqKey(ps,"THROW") and bpApplication ps =>
    -- Allow user-supplied matching type tag
    if bpEqKey(ps,"COLON") then
      bpRequire(ps,function bpApplication)
      bpPush(ps,%Pretend(bpPop2 ps,bpPop1 ps))
    bpPush(ps,bfThrow bpPop1 ps)
  nil

++  Try:
++    try Assign CatchItems
bpTry ps ==
  bpEqKey(ps,"TRY") =>
    bpAssign ps
    cs := []
    while bpHandler(ps,"CATCH") repeat
      bpCatchItem ps
      cs := [bpPop1 ps,:cs]
    bpHandler(ps,"FINALLY") =>
      bpFinally ps and
        bpPush(ps,bfTry(bpPop2 ps,reverse! [bpPop1 ps,:cs]))
    cs = nil => bpTrap() -- missing handlers
    bpPush(ps,bfTry(bpPop1 ps,reverse! cs))
  nil            

bpCatchItem ps ==
  bpRequire(ps,function bpExceptionVariable) and
    (bpEqKey(ps,"EXIT") or bpTrap()) and
      bpRequire(ps,function bpAssign) and
        bpPush(ps,%Catch(bpPop2 ps,bpPop1 ps))

bpExceptionVariable ps ==
  t := $stok
  bpEqKey(ps,"OPAREN") and 
    bpRequire(ps,function bpSignature) and
      (bpEqKey(ps,"CPAREN") or bpMissing t)
        or bpTrap()

bpFinally ps ==
  bpRequire(ps,function bpAssign) and
    bpPush(ps,%Finally bpPop1 ps)

bpHandler(ps,key) ==
  s := bpState ps
  (bpEqKey(ps,"BACKSET") or bpEqKey(ps,"SEMICOLON")) and bpEqKey(ps,key) => true
  bpRestore(ps,s)
  false

++ Leave:
++   LEAVE Logical
bpLeave ps ==
  bpEqKey(ps,"LEAVE") and bpRequire(ps,function bpLogical) and
    bpPush(ps,bfLeave bpPop1 ps)

++ Do:
++  IN Namespace Do
++  DO Assign
bpDo ps ==
  bpEqKey(ps,"IN") =>
    bpRequire(ps,function bpNamespace)
    bpRequire(ps,function bpDo)
    bpPush(ps,bfAtScope(bpPop2 ps,bpPop1 ps))
  bpEqKey(ps,"DO") and bpRequire(ps,function bpAssign) and
    bpPush(ps,bfDo bpPop1 ps)

++ Return:
++   RETURN Assign
++   Leave
++   Throw
++   And
bpReturn ps==
  (bpEqKey(ps,"RETURN") and bpRequire(ps,function bpAssign) and
	 bpPush(ps,bfReturnNoName bpPop1 ps)) 
    or bpLeave ps
      or bpThrow ps
        or bpAnd ps
          or bpDo ps
 
 
bpLogical ps ==
  bpLeftAssoc(ps,'(OR),function bpReturn)
 
bpExpression ps ==
  bpEqKey(ps,"COLON") and (bpLogical ps and
     bpPush(ps,bfApplication ("COLON",bpPop1 ps))
           or bpTrap()) or bpLogical ps
 
bpStatement ps ==
  bpConditional(ps,function bpWhere) or bpLoop ps
    or bpExpression ps
      or bpTry ps
 
bpLoop ps ==
  bpIterators ps and
   (bpCompMissing(ps,"REPEAT") and
      bpRequire(ps,function bpWhere) and
         bpPush(ps,bfLp(bpPop2 ps,bpPop1 ps)))
             or
               bpEqKey(ps,"REPEAT") and bpRequire(ps,function bpLogical) and
                    bpPush(ps,bfLoop1 bpPop1 ps)
 
bpSuchThat ps ==
  bpAndOr(ps,"BAR",function bpWhere,function bfSuchthat)
 
bpWhile ps ==
  bpAndOr(ps,"WHILE",function bpLogical,function bfWhile)
 
bpUntil ps ==
  bpAndOr(ps,"UNTIL",function bpLogical,function bfUntil)
 
bpFormal ps ==
  bpVariable ps or bpDot ps

bpForIn ps ==
  bpEqKey(ps,"FOR") and bpRequire(ps,function bpFormal) and (bpCompMissing(ps,"IN"))
      and (bpRequire(ps,function bpSeg) and
       (bpEqKey(ps,"BY") and bpRequire(ps,function bpArith) and
        bpPush(ps,bfForInBy(bpPop3 ps,bpPop2 ps,bpPop1 ps))) or
         bpPush(ps,bfForin(bpPop2 ps,bpPop1 ps)))
 
bpSeg ps ==
   bpArith ps and
      (bpEqKey(ps,"SEG") and
       (bpArith ps and bpPush(ps,bfSegment2(bpPop2 ps,bpPop1 ps))
         or bpPush(ps,bfSegment1(bpPop1 ps))) or true)
 
bpIterator ps ==
  bpForIn ps or bpSuchThat ps or bpWhile ps or bpUntil ps
 
bpIteratorList ps ==
  bpOneOrMore(ps,function bpIterator)
    and bpPush(ps,bfIterators bpPop1 ps)
 
bpCrossBackSet ps ==
  bpEqKey(ps,"CROSS") and (bpEqKey(ps,"BACKSET") or true)
 
bpIterators ps ==
  bpListofFun(ps,function bpIteratorList,
    function bpCrossBackSet,function bfCross)
 
bpAssign ps ==
  a := bpState ps
  bpStatement ps =>
    bpEqPeek "BEC" =>
      bpRestore(ps,a)
      bpRequire(ps,function bpAssignment)
    bpEqPeek "GIVES" =>
      bpRestore(ps,a)
      bpRequire(ps,function bpLambda)
    bpEqPeek "LARROW" =>
      bpRestore(ps,a)
      bpRequire(ps,function bpKeyArg)
    true
  bpRestore(ps,a)
  false
 
bpAssignment ps ==
  bpAssignVariable ps and
    bpEqKey(ps,"BEC") and
      bpRequire(ps,function bpAssign) and
	 bpPush(ps,bfAssign(bpPop2 ps,bpPop1 ps))
 
++ Parse a lambda expression
++   Lambda ::= Variable +-> Assign
bpLambda ps ==
  bpVariable ps and
    bpEqKey(ps,"GIVES") and
      bpRequire(ps,function bpAssign) and
        bpPush(ps,bfLambda(bpPop2 ps,bpPop1 ps))

bpKeyArg ps ==
  bpName ps and bpEqKey(ps,"LARROW") and bpLogical ps and
    bpPush(ps,bfKeyArg(bpPop2 ps,bpPop1 ps))

-- should only be allowed in sequences
bpExit ps ==
  bpAssign ps and (bpEqKey(ps,"EXIT") and
      (bpRequire(ps,function bpWhere) and
	 bpPush(ps,bfExit(bpPop2 ps,bpPop1 ps)))
	   or true)

bpDefinition ps ==
  bpEqKey(ps,"MACRO") =>
    bpName ps and bpStoreName ps and
      bpCompoundDefinitionTail(ps,function %Macro)
        or bpTrap()
  a := bpState ps
  bpExit ps =>
    bpEqPeek "DEF" =>
       bpRestore(ps,a)
       bpDef ps
    bpEqPeek "TDEF" =>
       bpRestore(ps,a)
       bpTypeAliasDefition ps
    true
  bpRestore(ps,a)
  false
 
bpStoreName ps ==
  $op := first parserTrees ps
  $wheredefs := nil
  $typings := nil
  true

bpDef ps ==  
  bpName ps and bpStoreName ps and bpDefTail(ps,function %Definition)
    or bpNamespace ps and bpSimpleDefinitionTail ps
 
bpDDef ps ==
  bpName ps and bpDefTail(ps,function %Definition)

++ Parse the remaining of a simple definition.
bpSimpleDefinitionTail ps ==
  bpEqKey(ps,"DEF") and
    bpRequire(ps,function bpWhere)
      and bpPush(ps,%ConstantDefinition(bpPop2 ps, bpPop1 ps))

++ Parse the remaining of a compound definition.
bpCompoundDefinitionTail(ps,f) ==
  bpVariable ps and 
    bpEqKey(ps,"DEF") and bpRequire(ps,function bpWhere) and 
      bpPush(ps,apply(f,[bpPop3 ps,bpPop2 ps,bpPop1 ps]))


++ Parse the remainding of a definition.  When we reach this point
++ we know we must parse a definition and we have already parsed
++ the name of the main operator in the definition.
bpDefTail(ps,f) ==
  bpSimpleDefinitionTail ps
    or bpCompoundDefinitionTail(ps,f)
 
bpWhere ps ==
    bpDefinition ps and
       (bpEqKey(ps,"WHERE") and bpRequire(ps,function bpDefinitionItem)
           and bpPush(ps,bfWhere(bpPop1 ps,bpPop1 ps)) or true)
 
bpDefinitionItem ps ==
  a := bpState ps
  bpDDef ps => true
  bpRestore(ps,a)
  bpBDefinitionPileItems ps => true
  bpRestore(ps,a)
  bpPDefinitionItems ps => true
  bpRestore(ps,a)
  bpWhere ps
 
bpDefinitionPileItems ps ==
  bpListAndRecover(ps,function bpDefinitionItem)
    and bpPush(ps,%Pile bpPop1 ps)
 
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
  bpEqKey(ps,"COMMA") and (bpEqKey(ps,"BACKSET") or true)
 
bpSemiColon ps == 
  bpSemiListing(ps,function bpComma,function bfSequence)
 
bpSemiListing(ps,p,f) ==
  bpListofFun(ps,p,function bpSemiBackSet,f)
 
bpSemiBackSet ps == 
  bpEqKey(ps,"SEMICOLON") and (bpEqKey(ps,"BACKSET") or true)
 
bpPDefinition ps ==
  bpIndentParenthesized(ps,function bpSemiColon)
 
bpPileItems ps ==
  bpListAndRecover(ps,function bpSemiColon) and bpPush(ps,bfSequence bpPop1 ps)
 
bpBPileDefinition ps ==
  bpPileBracketed(ps,function bpPileItems)
 
bpIteratorTail ps ==
  (bpEqKey(ps,"REPEAT") or true) and bpIterators ps
 
bpConstruct ps ==
  bpBracket(ps,function bpConstruction)
 
bpConstruction ps==
  bpComma ps and
     (bpIteratorTail ps and
	  bpPush(ps,bfCollect(bpPop2 ps,bpPop1 ps)) or
	     bpPush(ps,bfTupleConstruct bpPop1 ps))
 
bpDConstruct ps ==
  bpBracket(ps,function bpDConstruction)
 
bpDConstruction ps ==
  bpComma ps and
     (bpIteratorTail ps and
	  bpPush(ps,bfDCollect(bpPop2 ps,bpPop1 ps)) or
	     bpPush(ps,bfDTuple bpPop1 ps))
 
 
 
--PATTERN
 
bpPattern ps ==
  bpBracketConstruct(ps,function bpPatternL)
    or bpChar ps or bpName ps or bpConstTok ps
 
bpEqual ps ==
   bpEqKey(ps,"SHOEEQ") and (bpApplication ps or bpConstTok ps or
                bpTrap()) and bpPush(ps,bfEqual bpPop1 ps)
 
bpRegularPatternItem ps ==
   bpEqual ps or
     bpConstTok ps or bpDot ps or
      bpName ps and
         ((bpEqKey(ps,"BEC") and bpRequire(ps,function bpPattern)
              and bpPush(ps,bfAssign(bpPop2 ps,bpPop1 ps))) or true)
                    or bpBracketConstruct(ps,function bpPatternL)
 
bpRegularPatternItemL ps ==
      bpRegularPatternItem ps and bpPush(ps,[bpPop1 ps])
 
bpRegularList ps ==
       bpListof(ps,function bpRegularPatternItemL,"COMMA",function bfAppend)
 
bpPatternColon ps ==
  bpEqKey(ps,"COLON") and bpRequire(ps,function bpRegularPatternItem)
    and bpPush(ps,[bfColon bpPop1 ps])
 
 
-- only one colon
bpPatternL ps ==
  bpPatternList ps and bpPush(ps,bfTuple bpPop1 ps)
 
bpPatternList ps ==
  bpRegularPatternItemL ps =>
    while (bpEqKey(ps,"COMMA") and (bpRegularPatternItemL ps or
	(bpPatternTail ps
	  and bpPush(ps,append(bpPop2 ps,bpPop1 ps))
	    or bpTrap();false) )) repeat
	      bpPush(ps,append(bpPop2 ps,bpPop1 ps))
    true
  bpPatternTail ps
 
bpPatternTail ps ==
  bpPatternColon ps and
      (bpEqKey(ps,"COMMA") and bpRequire(ps,function bpRegularList)
	   and bpPush(ps,append(bpPop2 ps,bpPop1 ps)) or true)
 
-- BOUND VARIABLE

++ We are parsing parameters in a function definition.  We have
++ just seen a parameter name; we are attempting to see whether
++ it might be followed by a type annotation, or whether it actually
++ a form with a specific pattern structure, or whether it has
++ a default value.
bpRegularBVItemTail ps ==
  bpEqKey(ps,"COLON") and bpRequire(ps,function bpApplication) and 
    bpPush(ps,bfTagged(bpPop2 ps, bpPop1 ps))
      or bpEqKey(ps,"BEC") and bpRequire(ps,function bpPattern) and
	 bpPush(ps,bfAssign(bpPop2 ps,bpPop1 ps))
	   or bpEqKey(ps,"IS") and bpRequire(ps,function bpPattern) and
	      bpPush(ps,bfAssign(bpPop2 ps,bpPop1 ps))
             or bpEqKey(ps,"DEF") and bpRequire(ps,function bpApplication) and
                bpPush(ps,%DefaultValue(bpPop2 ps, bpPop1 ps))


bpRegularBVItem ps ==
  bpBVString ps 
    or bpConstTok ps 
      or (bpName ps and (bpRegularBVItemTail ps or true))
        or bpBracketConstruct(ps,function bpPatternL)
 
bpBVString ps ==
  tokenClass $stok = "STRING" and
      bpPush(ps,["BVQUOTE",makeSymbol $ttok]) and bpNext ps
 
bpRegularBVItemL ps ==
  bpRegularBVItem ps and bpPush(ps,[bpPop1 ps])
 
bpColonName ps ==
  bpEqKey(ps,"COLON") and (bpName ps or bpBVString ps or bpTrap())
 
 
-- at most one colon at end
bpBoundVariablelist ps ==
  bpRegularBVItemL ps =>
    while (bpEqKey(ps,"COMMA") and (bpRegularBVItemL ps or
	(bpColonName ps
	  and bpPush(ps,bfColonAppend(bpPop2 ps,bpPop1 ps))
	    or bpTrap();false) )) repeat
	       bpPush(ps,append(bpPop2 ps,bpPop1 ps))
    true
  bpColonName ps and bpPush(ps,bfColonAppend(nil,bpPop1 ps))
 

bpVariable ps ==
    bpParenthesized(ps,function bpBoundVariablelist) and
       bpPush(ps,bfTupleIf bpPop1 ps)
         or bpBracketConstruct(ps,function bpPatternL)
                or bpName ps or bpConstTok ps
 
bpAssignVariable ps ==
      bpBracketConstruct(ps,function bpPatternL) or bpAssignLHS ps
 
bpAssignLHS ps ==
  not bpName ps => false
  bpEqKey(ps,"COLON") =>          -- variable declaration
    bpRequire(ps,function bpApplication)
    bpPush(ps,bfLocal(bpPop2 ps,bpPop1 ps))
  bpArgumentList ps and 
    (bpEqPeek "DOT" 
      or (bpEqPeek "BEC" and bpPush(ps,bfPlace bpPop1 ps)) 
        or bpTrap())
  bpEqKey(ps,"DOT") =>            -- field path
    bpList(ps,function bpPrimary,"DOT") and 
      bpChecknull ps and
        bpPush(ps,bfTuple([bpPop2 ps,:bpPop1 ps]))
  true

bpChecknull ps ==
  a := bpPop1 ps
  a = nil => bpTrap()
  bpPush(ps,a)

bpStruct ps ==
   bpEqKey(ps,"STRUCTURE") and
      bpRequire(ps,function bpName) and
        (bpEqKey(ps,"DEF") or bpTrap()) and
           (bpRecord ps or bpTypeList ps) and
             bpPush(ps,%Structure(bpPop2 ps,bpPop1 ps))
 
++ Record:
++   "Record" "(" FieldList ")"
bpRecord ps ==
  s := bpState ps
  bpName ps and bpPop1 ps is "Record" =>
    (bpParenthesized(ps,function bpFieldList) or bpTrap()) and
      bpGlobalAccessors ps and
        bpPush(ps,%Record(bfUntuple bpPop2 ps,bpPop1 ps))
  bpRestore(ps,s)
  false

++ FieldList:
++    Signature
++    Signature , FieldList
bpFieldList ps ==
  bpTuple(ps,function bpSignature)

bpGlobalAccessors ps ==
  bpEqKey(ps,"WITH") =>
    bpPileBracketed(ps,function bpAccessorDefinitionList) or bpTrap()
  bpPush(ps,nil)

bpAccessorDefinitionList ps ==
  bpListAndRecover(ps,function bpAccessorDefinition)

++ AccessorDefinition:
++   Name DEF FieldSection
bpAccessorDefinition ps ==
  bpRequire(ps,function bpName) and
    (bpEqKey(ps,"DEF") or bpTrap()) and
       bpRequire(ps,function bpFieldSection) and
         bpPush(ps,%AccessorDef(bpPop2 ps,bpPop1 ps))

++ FieldSection:
++    "(" DOT Name ")"
bpFieldSection ps ==
  bpParenthesized(ps,function bpSelectField)

bpSelectField ps ==
  bpEqKey(ps,"DOT") and bpName ps

bpTypeList ps == 
  bpPileBracketed(ps,function bpTypeItemList)
    or bpTypeItem ps and bpPush(ps,[bpPop1 ps])

bpTypeItem ps ==
  bpTerm(ps,function bpIdList)
 
bpTypeItemList ps ==  
  bpListAndRecover(ps,function bpTypeItem)
 
bpTerm(ps,idListParser) ==
  bpRequire(ps,function bpName) and
    ((bpParenthesized(ps,idListParser) and
      bpPush(ps,bfNameArgs(bpPop2 ps,bpPop1 ps)))
	or bpName ps and bpPush(ps,bfNameArgs(bpPop2 ps,bpPop1 ps)))
	 or bpPush(ps,bfNameOnly bpPop1 ps)
 
bpIdList ps == 
  bpTuple(ps,function bpName)
 
bpCase ps ==
  bpEqKey(ps,"CASE") and
    bpRequire(ps,function bpWhere) and
       (bpEqKey(ps,"OF") or bpMissing "OF") and
	     bpPiledCaseItems ps
 
bpPiledCaseItems ps ==
   bpPileBracketed(ps,function bpCaseItemList) and
       bpPush(ps,bfCase(bpPop2 ps,bpPop1 ps))

bpCaseItemList ps ==
   bpListAndRecover(ps,function bpCaseItem)

bpCasePatternVar ps ==
  bpName ps or bpDot ps

bpCasePatternVarList ps ==
  bpTuple(ps,function bpCasePatternVar)
 
bpCaseItem ps ==
    (bpTerm(ps,function bpCasePatternVarList) or bpTrap()) and
       (bpEqKey(ps,"EXIT") or bpTrap()) and
         bpRequire(ps,function bpWhere) and
            bpPush(ps,bfCaseItem(bpPop2 ps,bpPop1 ps))


++ Main entry point into the parser module.
bpOutItem ps ==
  $op: local := nil
  $GenVarCounter: local := 0
  bpRequire(ps,function bpComma)
  b := bpPop1 ps
  t :=
    b is ["+LINE",:.] => [ b ]
    b is ["L%T",l,r] and symbol? l => 
      $InteractiveMode => [["SETQ",l,r]]
      [["DEFPARAMETER",l,r]]
    translateToplevel(b,false)
  bpPush(ps,t)
 
