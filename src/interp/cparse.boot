-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2013, Gabriel Dos Reis.
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

import ptrees
namespace BOOT

-- npTerm introduced between npRemainder and npSum
-- rhs of assignment changed from npStatement to npGives

++ Entry point into the parser.
npParse stream ==
  $inputStream: local := stream
  $stack: local :=nil
  $stok: local := nil
  $ttok: local := nil
  npFirstTok()
  found := CATCH("TRAPPOINT",npItem())
  found = "TRAPPED" =>
    ncSoftError(tokPosn $stok,'S2CY0006, [])
    pfWrong(pfDocument  '"top level syntax error" ,pfListOf nil)
  $inputStream ~= nil =>
    ncSoftError(tokPosn $stok,'S2CY0002,[])
    pfWrong(pfDocument ['"input stream not exhausted"],pfListOf [])
  $stack = nil => 
    ncSoftError(tokPosn $stok,'S2CY0009, [])
    pfWrong(pfDocument ['"stack empty"],pfListOf [])
  first $stack

++ Parse a toplevel item.
++   Item ::= QualifiedDefinition [ SEMICOLON Item ]
npItem()==
  npQualDef() =>
    npEqKey "SEMICOLON" =>
      [a,b] := npItem1 npPop1()
      c := pfEnSequence b
      a => npPush c
      npPush pfNovalue c
    npPush pfEnSequence npPop1 ()
  false

++ Subroutine of npItem1.
npItem1 c==
  npQualDef() =>
    npEqKey "SEMICOLON" =>
      [a,b] := npItem1 npPop1()
      [a,append(c,b)]
    [true,append (c,npPop1())]
  [false,c]

++ Get the first token, if any, from the current input stream.
npFirstTok()==
  $stok :=
    $inputStream = nil => tokConstruct("ERROR","NOMORE",tokPosn $stok)
    first $inputStream
  $ttok := tokPart $stok

++ Get next token from the current input stream.  
npNext() ==
  $inputStream := rest $inputStream
  npFirstTok()

++ Returns a snapshot of the current parser state.  
npState() == 
  [$inputStream,:$stack]

++ Restore the parser state to a prevously saved state `x'.  
npRestore x ==
  $inputStream := first x
  npFirstTok()
  $stack := rest x
  true

++ Push a new parse tree on the current parse tree stack.  
npPush x ==
  $stack := [x,:$stack]

++ If the current token designates an infix operator, push its
++ name on the parsing tree stack, otherwise treat the token
++ has a name.
npPushId() ==
  a := property($ttok,'INFGENERIC)
  $ttok := if a then a else $ttok
  $stack := [tokConstruct("id",$ttok,tokPosn $stok),:$stack]
  npNext()

++ Remove the first item from the parse tree stack, and return it
npPop1() ==
  a := first $stack
  $stack := rest $stack
  a

++ Remove the second item from the parse tree stack, and return it.
npPop2() ==
  a := second $stack
  $stack.rest := $stack.rest.rest
  a

++ Remove the third item from the parse tree stack, and return it.
npPop3() ==
  a := third $stack
  $stack.rest.rest := $stack.rest.rest.rest
  a

++ Parser combinator: parse the syntax `f' enclosed in round
++ round brackets
npParenthesized f ==
   npParenthesize("(",")",f) or npParenthesize("(|","|)",f)

++ Parser combinator: parse the syntax `f' enclosed in brackets
++ as indicaed by `open' and `close'.
npParenthesize (open,close,f)==
  a := $stok
  npEqKey open =>
    apply(f,[]) and (npEqKey close or npMissingMate(close,a)) => true
    npEqKey close => npPush []
    npMissingMate(close,a)
  false

++ Parser combinator: parse a syntax composed of an opening bracket
++ `open', followed by a syntax `f', terminated by the closing
++ bracket `close'.  Use `fn' to construct the resulting parse tree.
npEnclosed(open,close,fn,f)==
  a := $stok
  npEqKey open =>
    npEqKey close  => npPush apply(fn,[a,pfTuple pfListOf []])
    apply(f,[]) and (npEqKey close or npMissingMate(close,a)) =>
      npPush apply(fn,[a,pfEnSequence npPop1()])
    false
  false

++ Parser combinator: parse a round-bracket syntax.
++ Note: The parenthesis are part of the parse tree.
npParened f ==
  npEnclosed("(",")",function pfParen,f) or
    npEnclosed("(|","|)",function pfParen,f)

++ Parser combinator: parse a square-bracket syntax.
++ Note: The brackets are part of the parse tree.
npBracked f ==
  npEnclosed("[","]",function pfBracket,f) or
    npEnclosed("[|","|]",function pfBracketBar,f)

++ Parser combinator: parse a curly-bracket syntax.
++ Note: The braces are part of the parse tree.
npBraced f ==
  npEnclosed("{","}",function pfBrace,f) or
    npEnclosed("{|","|}",function pfBraceBar,f)

++ Parser combinator: parse an angle-bracket syntax.
++ Note: The angles are part of the parse tree.
npAngleBared f ==
  npEnclosed("<|","|>",function pfHide,f)

++ Parser combinator: parse a bracketed syntax.
++ Note: The brackets are part of the parse tree.
npBracketed f==
  npParened f or npBracked f or npBraced f or npAngleBared f

++ Parse combinator: parse a sequence of syntax `f' in a pile.  
npPileBracketed f==
  npEqKey "SETTAB" =>
    npEqKey "BACKTAB" => npPush pfNothing()     -- never happens
    apply(f,[]) and (npEqKey "BACKTAB" or npMissing "backtab") =>
      npPush pfPile npPop1()
    false
  false

++ Parser combinator: parse a either a single syntax `f', or a sequence
++ of syntax `f' separated by syntax `g'. In case of a sequence, use
++ `g' to build the resulting parse tree.
npListofFun(f,h,g)==
  apply(f,[]) =>
    apply(h,[]) and (apply(f,[]) or npTrap()) =>
      a := $stack
      $stack := nil
      while apply(h,[]) and (apply(f,[]) or npTrap()) repeat 0
      $stack := [reverse! $stack,:a]
      npPush apply(g, [[npPop3(),npPop2(),:npPop1()]])
    true
  false

++ Parse combinator: parse a sequence of syntax `f' separated by
++ token `str1'.  Build the resulting parse tree with `g'.
npList(f,str1,g)== -- always produces a list, g is applied to it
  apply(f,[]) =>
    npEqKey str1 and (npEqKey "BACKSET" or true)
                   and (apply(f,[]) or npTrap()) =>
      a := $stack
      $stack := nil
      while npEqKey str1 and (npEqKey "BACKSET" or true) and
                         (apply(f,[]) or npTrap()) repeat 0
      $stack := [reverse! $stack,:a]
      npPush apply(g,[[npPop3(),npPop2(),:npPop1()]])
    npPush apply(g,[[npPop1()]])
  npPush apply(g,[nil])


npPPff f ==
  apply(f,[]) and npPush [npPop1()]

npPPf f ==
  npSemiListing function (() +-> npPPff f)

npPPg f ==
  npListAndRecover function (() +-> npPPf f)
    and npPush pfAppend npPop1()

npPP(f) ==
  npParened function (() +-> npPPf f)
    or npPileBracketed function (() +-> npPPg f) and
      npPush pfEnSequence npPop1()
        or apply(f,[])

npPCff f ==
  apply(f,[]) and npPush [npPop1()]

npPCg f ==
  npListAndRecover function (() +-> npPCff f)
    and npPush pfAppend npPop1()

npPC(f) ==
  npPileBracketed function (() +-> npPCg f) and
    npPush pfEnSequence npPop1()
      or apply(f,[])


++ Parser combinator: Apply the parser `s' any number of time it
++ it is possible.  Note that `s' transforms the the top of the
++ parse tree stack.
npAnyNo s ==
  while apply(s,[]) repeat 0
  true

++ Parser combinator: parse `keyword' followed by the syntax `p',
++ and build the resulting parse tree with `f'.
npAndOr(keyword,p,f)==
  npEqKey keyword and (apply(p,[]) or npTrap()) and
    npPush apply(f,[npPop1()])

++ Parser combinator: parse a right-associative syntax with operand
++ syntax `p', and operator `o'.
++ p o p o p o p = (p o (p o (p o p)))
++ p o p o = (p o p) o
npRightAssoc(o,p)==
  a := npState()
  apply(p,[]) =>
    while npInfGeneric o and (npRightAssoc(o,p)
            or (npPush pfApplication(npPop2(),npPop1());false)) repeat
      npPush pfInfApplication(npPop2(),npPop2(),npPop1())
    true
  npRestore a
  false

++ Parser combinator: parse a left-associative syntax with operand
++ syntax `p', and operators in `operations'.
++ p o p o p o p = (((p o p) o p) o p)
++ p o p o = (p o p) o
npLeftAssoc(operations,parser) ==
  apply(parser,[]) =>
    while npInfGeneric(operations) and
           (apply(parser,[]) or
               (npPush pfApplication(npPop2(),npPop1());false)) repeat
      npPush pfInfApplication(npPop2(),npPop2(),npPop1())
    true
  false

++ Parse an infix operator name.
npInfixOp() ==
  $stok.first.first is "key" and
    property($ttok,"INFGENERIC") and npPushId()

++ Parse an infix operator, either quoted or backquoted.    
npInfixOperator() ==
  npInfixOp() or
    a := npState()
    b := $stok
    npEqKey "'" and npInfixOp() =>
      npPush pfSymb(npPop1(),tokPosn b)
    npRestore a
    npEqKey "BACKQUOTE" and  npInfixOp() =>
      a := npPop1()
      npPush tokConstruct("idsy",tokPart a,tokPosn a)
    npRestore a
    false

++ Parse any infix keyword in the list `s'.    
npInfKey s ==
  $stok.first.first = "key" and symbolMember?($ttok,s) and npPushId()

++ Parse any infix keyword in the list `s', either in plain syntax
++ or quoted form.
npDDInfKey s ==
  npInfKey s or
    a := npState()
    b := $stok
    npEqKey "'" and npInfKey s =>
      npPush pfSymb(npPop1(),tokPosn b)
    npRestore a
    npEqKey "BACKQUOTE" and npInfKey s =>
      a := npPop1()
      npPush tokConstruct("idsy",tokPart a,tokPosn a)
    npRestore a
    false

++ Same as npDDInfKey, except that newline+bakset are accepted.    
npInfGeneric s ==
  npDDInfKey s  and (npEqKey "BACKSET" or true)

++ Parser combinator:  Parse the syntax `f' as either the then-branch,
++ of both branches of a conditional expression.
npConditional f ==
  npEqKey "IF" and (npLogical() or npTrap()) and
                   (npEqKey "BACKSET" or true) =>
    npEqKey "SETTAB" =>
      npEqKey "THEN" =>
        (apply(f,[]) or npTrap()) and npElse f and npEqKey "BACKTAB"
      npMissing "then"
    npEqKey "THEN" => (apply(f,[]) or npTrap()) and npElse f
    npMissing "then"
  false

npElse f ==
  a := npState()
  npBacksetElse() =>
    (apply(f,[]) or npTrap()) and
        npPush pfIf(npPop3(),npPop2(),npPop1())
  npRestore a
  npPush pfIfThenOnly(npPop2(),npPop1())

npBacksetElse()==
  npEqKey "BACKSET" => npEqKey "ELSE"
  npEqKey "ELSE"

npWConditional f==
  npConditional f => npPush pfTweakIf npPop1()
  false

npQuantified f ==
  npEqPeek "FORALL" =>
    npQuantifierVariable "FORALL" and npQuantified f and
      npPush %Forall(npPop2(), npPop1())
  npEqPeek "EXIST" =>
    npQuantifierVariable "EXIST" and npQuantified f and
      npPush %Exist(npPop2(), npPop1())
  apply(f,[])  

-- Parsing functions

-- peek for keyword s, no advance of token stream

npEqPeek s ==
  $stok.first.first = "key" and sameObject?(s,$ttok)

-- test for keyword s, if found advance token stream

npEqKey s ==
  $stok.first.first = "key" and sameObject?(s,$ttok) and npNext()

$npTokToNames ==
  ["~","#","[]","{}", "[||]","{||}"]

npId() ==
  $stok.first.first = "id" =>
     npPush $stok
     npNext()
  $stok.first.first = "key" and symbolMember?($ttok,$npTokToNames) =>
     npPush tokConstruct("id",$ttok,tokPosn $stok)
     npNext()
  false

npSymbolVariable()==
  a := npState()
  npEqKey "BACKQUOTE" and npId() =>
    a:=npPop1()
    npPush tokConstruct("idsy",tokPart a,tokPosn a)
  npRestore a
  false

npName() ==
  npId() or npSymbolVariable()

npConstTok() ==
  tokType $stok in '(integer string char float command) =>
    npPush $stok
    npNext()
  npEqPeek "'" =>
    a := $stok
    b := npState()
    npNext()
    npPrimary1() and npPush pfSymb(npPop1(),tokPosn a) => true
    npRestore b
    false
  false


npPrimary1() ==
   npEncAp function npAtom1 or
   npLet() or
   npFix() or
   npMacro() or
   npBPileDefinition() or npDefn() or
   npRule()

npPrimary2() ==
  npEncAp function npAtom2 -- or  npBPileDefinition()
    or npAdd(pfNothing()) or npWith(pfNothing())


npAtom1() ==
  npPDefinition() or ((npName() or npConstTok() or
    npDollar() or npBDefinition()) and npFromdom())

npAtom2() ==
  (npInfixOperator() or npAmpersand() or npPrefixColon()) and npFromdom()

npDollar() ==
  npEqPeek "$" and
    npPush tokConstruct("id","$",tokPosn $stok)
    npNext()

npPrefixColon() ==
  npEqPeek "COLON" and
    npPush tokConstruct("id",":",tokPosn $stok)
    npNext()

-- silly

npEncAp f ==
  apply(f,[]) and npAnyNo function npEncl and npFromdom()

npEncl()==
  npBDefinition() and npPush pfApplication(npPop2(),npPop1())

npFromdom()==
  npEqKey "$" and (npApplication() or npTrap())
      and npFromdom1 npPop1() and npPush pfFromDom(npPop1(),npPop1())
         or true

npFromdom1 c==
  npEqKey "$" and (npApplication() or npTrap())
    and npFromdom1 npPop1()  and npPush pfFromDom(npPop1(),c)
        or npPush c


npPrimary()==
  npPrimary1() or npPrimary2()

npDotted f ==
  apply(f,[]) and npAnyNo function npSelector

npSelector()==
  npEqKey "DOT" and (npPrimary() or npTrap()) and
    npPush(pfApplication(npPop2(),npPop1()))

npApplication() ==
   npDotted function npPrimary and
      (npApplication2() and
            npPush(pfApplication(npPop2(),npPop1())) or true)


npApplication2() ==
   npDotted function npPrimary1 and
      (npApplication2() and
            npPush(pfApplication(npPop2(),npPop1())) or true)

npTypedForm1(sy,fn) ==
     npEqKey sy  and (npType() or npTrap()) and
        npPush apply(fn,[npPop2(),npPop1()])

npQuiver() ==
  npRightAssoc('(ARROW LARROW),function npApplication)

npTypedForm(sy,fn) ==
     npEqKey sy  and (npQuiver() or npTrap()) and
        npPush apply(fn,[npPop2(),npPop1()])

npRestrict() ==
  npTypedForm("AT",function pfRestrict)

npCoerceTo() ==
  npTypedForm("COERCE",function pfCoerceto)

npPretend() ==
  npTypedForm("PRETEND",function pfPretend)

npTypeStyle()==
  npCoerceTo() or npRestrict() or npPretend()

npTypified() ==
  npQuiver() and npAnyNo function npTypeStyle

npTagged() ==
  npTypedForm("COLON",function pfTagged)

npColon() ==
  npTypified() and npAnyNo function npTagged

npPower() ==
  npRightAssoc('(POWER CARAT),function npColon)

npProduct()==
  npLeftAssoc('(TIMES SLASH BACKSLASH SLASHSLASH
     BACKSLASHBACKSLASH SLASHBACKSLASH BACKSLASHSLASH )
                     ,function npPower)

npRemainder()==
  npLeftAssoc('(REM QUO ),function npProduct)

npTerm()==
   npInfGeneric '(MINUS PLUS) and (npRemainder()
        and npPush(pfApplication(npPop2(),npPop1())) or true)
             or npRemainder()


npSum() ==
  npLeftAssoc('(PLUS MINUS),function npTerm)

npArith() ==
  npLeftAssoc('(MOD),function npSum)

npSegment() ==
  npEqPeek "SEG"  and npPushId() and npFromdom()

npInterval()==
  npArith() and
   (npSegment() and ((npEqPeek "BAR"
      and npPush(pfApplication(npPop1(),npPop1()))) or
     (npArith() and npPush(pfInfApplication(npPop2(),npPop2(),npPop1())))
            or npPush(pfApplication(npPop1(),npPop1()))) or true)

npBy() ==
  npLeftAssoc ('(BY),function npInterval)

npAmpersand() ==
  npEqKey "AMPERSAND" and (npName() or npTrap())

npAmpersandFrom() ==
  npAmpersand()  and npFromdom()

npSynthetic() ==
  npBy() =>
    while npAmpersandFrom()  and (npBy() or
            (npPush pfApplication(npPop2(),npPop1());false)) repeat
      npPush pfInfApplication(npPop2(),npPop2(),npPop1())
    true
  false

npRelation() ==
  npLeftAssoc ('(EQUAL NOTEQUAL LT LE GT GE OANGLE CANGLE),
           function npSynthetic)

npDiscrim()== 
  npLeftAssoc ('(CASE HAS IS ISNT), function npRelation)

npDisjand() ==
  npLeftAssoc('(AND ),function npDiscrim)

npLogical() ==
  npLeftAssoc('(OR ),function npDisjand)

npSuch() ==
  npLeftAssoc('(BAR),function npLogical)

++ Parse a type expression
++   Type:
++     MonoType
++     QuantifiedVariable Type
npType() ==
  npQuantified function npMonoType

npMonoType() ==
  npSuch() and
    a := npPop1()
    npWith(a) or npPush a

npADD() ==
  npMonoType() and
    a := npPop1()
    npAdd(a) or npPush a

npConditionalStatement() ==
  npConditional function npQualifiedDefinition

npExpress1()==
  npConditionalStatement() 
    or npBackTrack(function npADD, "BECOMES", function npAssignment)

npCommaBackSet() ==
  npEqKey "COMMA" and (npEqKey "BACKSET" or true)

npExpress()==
  npExpress1() and
     (npIterators() and
          npPush pfCollect (npPop2(),pfListOf npPop1()) or true)

npZeroOrMore f==
  apply(f,[])=>
    a := $stack
    $stack := nil
    while apply(f,[]) repeat 0
    $stack := [reverse! $stack,:a]
    npPush [npPop2(),:npPop1()]
  npPush nil
  true

npIterators() ==
  npForIn() and npZeroOrMore function npIterator
      and npPush [npPop2(),:npPop1()]  or
       npWhile() and (npIterators() and
             npPush [npPop2(),:npPop1()] or npPush [npPop1()])

npIterator() ==
  npForIn() or npSuchThat() or npWhile()

++ Parse a case-pattern expression.
++   Case:
++     CASE Interval IS PileExit
npCase() ==
  npEqKey "CASE" =>
    (npInterval() or npTrap()) and (npEqKey "IS" or npTrap())
      and (pPP function npPileExit or npTrap())
        and npPush pfCase(npPop2(), pfSequenceToList npPop1())
  false

npStatement() ==
  npCase() or
  npExpress() or
  npLoop() or
  npIterate() or
  npReturn() or
  npBreak() or
  npFree() or
  npImport() or
  npInline() or
  npLocal() or
  npExport() or
  npVoid()

npBackTrack(p1,p2,p3) ==
  a := npState()
  apply(p1,[]) =>
    npEqPeek p2   =>
      npRestore a
      apply(p3,[]) or npTrap()
    true
  false

npMDEF() == 
  npBackTrack(function npStatement,"MDEF",
    function (() +-> npMdef "MDEF"))

npAssign()==
  npBackTrack(function npMDEF,"BECOMES",function npAssignment)

npAssignment()==
  npAssignVariable() and
    (npEqKey "BECOMES" or npTrap()) and
      (npGives() or npTrap()) and
         npPush pfAssign (npPop2(),npPop1())

npAssignVariableName() ==
  npApplication() and
    a := npPop1()
    pfId? a => 
      (npPush a and npDecl() or npPush pfTyped(npPop1(),pfNothing()))
    npPush a

npAssignVariable() ==
  npColon() and npPush pfListOf [npPop1()]

npAssignVariablelist() ==
  npListing function npAssignVariableName

npExit() ==
  npBackTrack(function npAssign,"EXIT",function npPileExit)

npPileExit()==
  npAssign() and (npEqKey "EXIT" or npTrap()) and
      (npStatement() or npTrap())
        and npPush pfExit(npPop2(),npPop1())

npGives() ==
  npBackTrack(function npExit,"GIVES",function npLambda)

npDefinitionOrStatement()==
  npQuantified 
    function (() +-> npBackTrack(function npGives,
                            "DEF",function npDef))

npVoid() ==
  npAndOr("DO",function npStatement,function pfNovalue)

npReturn()==
  npEqKey "RETURN" and
   (npExpress() or npPush pfNothing()) and
    (npEqKey "FROM" and (npName() or npTrap()) and
       npPush pfReturn (npPop2(),npPop1()) or
         npPush pfReturnNoName npPop1())

npLoop()==
  npIterators() and
   (npCompMissing "REPEAT" and
      (npAssign() or npTrap()) and
         npPush pfLp(npPop2(),npPop1()))
             or
               npEqKey "REPEAT" and (npAssign() or npTrap()) and
                    npPush pfLoop1 npPop1 ()

npSuchThat() ==
  npAndOr("BAR",function npLogical,function pfSuchthat)

npWhile() ==
  npAndOr ("WHILE",function npLogical,function pfWhile)

npForIn()==
  npEqKey "FOR" and (npVariable() or npTrap()) and (npCompMissing "IN")
      and ((npBy()  or npTrap()) and
         npPush pfForin(npPop2(),npPop1()))

npBreak()==
  npEqKey "BREAK" and  npPush pfBreak pfNothing()

npIterate()==
  npEqKey "ITERATE" and  npPush pfIterate pfNothing()

npQualType()==
  npType() and npPush pfQualType(npPop1(),pfNothing())

npSQualTypelist() ==
  npListing function npQualType and npPush pfParts npPop1()

npQualTypelist()==
  npPC function npSQualTypelist and npPush pfUnSequence npPop1()

npImport() ==
  npAndOr("IMPORT",function npQualTypelist,function pfImport)

npInline() ==
  npAndOr("INLINE",function npQualTypelist,function pfInline)

npLocalDecl()==
  npEqKey "COLON" and (npType() or npTrap()) and
    npPush pfSpread (pfParts npPop2(),npPop1()) or
     npPush pfSpread (pfParts npPop1(),pfNothing())

npLocalItem() ==
  npTypeVariable() and  npLocalDecl()

npLocalItemlist()==
  npPC function npSLocalItem and npPush pfUnSequence npPop1()

npSLocalItem()==
  npListing function npLocalItem and npPush  pfAppend pfParts npPop1()

npFree()==
  npEqKey "FREE" and (npLocalItemlist() or npTrap()) and
    npPush pfFree npPop1()

npLocal()==
  npEqKey "local" and (npLocalItemlist() or npTrap()) and
    npPush pfLocal npPop1()

npExport()==
  npEqKey "EXPORT" and (npLocalItemlist() or npTrap()) and
    npPush pfExport npPop1()

npLet() ==
  npLetQualified function npDefinitionOrStatement

npDefn() ==
  npEqKey "DEFN" and  npPP function npDef

npFix() ==
  npEqKey "FIX" and npPP function npDef and npPush pfFix npPop1()

npMacro() == 
  npEqKey "MACRO" and  npPP function (() +-> npMdef "DEF")

npRule()==
  npEqKey "RULE" and  npPP function npSingleRule

npAdd(extra)==
  npEqKey "ADD" =>
    a:=npState()
    npDefinitionOrStatement() or npTrap()
    npEqPeek "IN" =>
      npRestore a
      (npVariable() or npTrap()) and
            npCompMissing "IN"  and
                (npDefinitionOrStatement() or npTrap()) and
                   npPush pfAdd(npPop2(),npPop1(),extra)
    npPush pfAdd(pfNothing(),npPop1(),extra)

npDefaultValue()==
  npEqKey "DEFAULT" and
    (npDefinitionOrStatement() or npTrap()) and
       npPush [pfAdd(pfNothing(),npPop1(),pfNothing())]

npWith extra ==
  npEqKey "WITH" and (npCategoryL() or npTrap()) and
    npPush pfWith(extra,npPop1())

npCategoryL()==
  npCategory() and npPush pfUnSequence npPop1 ()

pfUnSequence x==
  pfSequence? x => pfListOf pfAppend pf0SequenceArgs x
  pfListOf x

npCategory() ==
  npPP function npSCategory

npSCategory()==
  npWConditional function npCategoryL => npPush [npPop1()]
  npDefaultValue() => true
  a := npState()
  npPrimary() =>
    npEqPeek "COLON" =>
      npRestore a
      npSignature()
    npRestore a
    npApplication() and npPush [pfAttribute (npPop1())] or npTrap()
  false

npSignatureDefinee() ==
  npName() or npInfixOperator() or npPrefixColon()

npSigDecl()==
  npEqKey "COLON" and (npType() or npTrap()) and
    npPush pfSpread (pfParts npPop2(),npPop1())

npSigItem() ==
  npTypeVariable() and  (npSigDecl() or npTrap())

npSigItemlist() ==
  npListing function npSigItem and
    npPush pfListOf pfAppend pfParts npPop1()

npSignature() ==
  npSigItemlist() and
    npPush pfWDec(pfNothing(),npPop1())

npSemiListing(p) ==
  npListofFun(p,function npSemiBackSet,function pfAppend)

npSemiBackSet() ==
  npEqKey "SEMICOLON" and (npEqKey "BACKSET" or true)

npDecl()==
  npEqKey "COLON" and (npType() or npTrap()) and
    npPush pfTyped (npPop2(),npPop1())

npVariableName() ==
  npName() and
    (npDecl() or npPush pfTyped(npPop1(),pfNothing()))

npVariable() ==
  npParenthesized function npVariablelist
    or (npVariableName() and npPush pfListOf [npPop1()])

npVariablelist() ==
  npListing function npVariableName

++ Parse binders of a quantified expression
++   QuantifiedVariable:
++      Quantifier Variable DOT
++   Quantifier:
++      EXIST
++      FORALL
npQuantifierVariable quantifier ==
  npEqKey quantifier and
    (npVariable() or npTrap()) and
      npEqKey "DOT"


npListing p ==
  npList(p,"COMMA",function pfListOf)

npQualified(f)==
  apply(f,[]) => 
    while npEqKey "WHERE" and (npDefinition() or npTrap()) repeat
      npPush pfWhere(npPop1(),npPop1())
    true
  npLetQualified  f

npLetQualified f==
  npEqKey "%LET" and
    (npDefinition() or npTrap()) and
      npCompMissing "IN"  and
        (apply(f,[]) or npTrap()) and
          npPush pfWhere(npPop2(),npPop1())

npQualifiedDefinition()==
  npQualified function npDefinitionOrStatement

npTuple p ==
  npListofFun(p,function npCommaBackSet,function pfTupleListOf)

npComma() ==
  npTuple function npQualifiedDefinition

npQualDef() ==
  npComma() and npPush [npPop1()]

npDefinitionlist() ==
  npSemiListing(function npQualDef)

npPDefinition() ==
  npParenthesized function npDefinitionlist and
    npPush pfEnSequence npPop1()

npBDefinition()==
  npPDefinition()
    or npBracketed function npDefinitionlist

npPileDefinitionlist()==
  npListAndRecover function npDefinitionlist and
    npPush pfAppend npPop1()


npTypeVariable()==
  npParenthesized function npTypeVariablelist
    or npSignatureDefinee() and npPush pfListOf [npPop1()]

npTypeVariablelist() ==
  npListing function npSignatureDefinee

npBPileDefinition()==
  npPileBracketed function npPileDefinitionlist and
    npPush pfSequence pfListOf npPop1()


npLambda()==
  (npVariable() and
    ((npLambda() or npTrap()) and
       npPush pfLam(npPop2(),npPop1()))) or
         npEqKey "GIVES" and (npDefinitionOrStatement() or npTrap()) or
          npEqKey "COLON" and (npType() or npTrap()) and
            npEqKey "GIVES" and (npDefinitionOrStatement() or npTrap())
               and
                  npPush pfReturnTyped(npPop2(),npPop1())

npDef() ==
  npSuch() =>
    [op,arg,rt] :=  pfCheckItOut(npPop1())
    npDefTail "DEF" or npTrap()
    body := npPop1()
    arg = nil => npPush pfDefinition (op,body)
    npPush pfDefinition (op,pfPushBody(rt,arg,body))
  false

npDefTail kw == 
  npEqKey kw and npDefinitionOrStatement()

npMdef kw ==
  npSuch() =>
    [op,arg] := pfCheckMacroOut(npPop1())
    npDefTail kw or npTrap()
    body := npPop1()
    arg = nil => npPush pfMacro (op,body)
    npPush pfMacro(op,pfPushMacroBody(arg,body))
  false


npSingleRule()==
  npSuch() =>
    npDefTail "DEF" or npTrap()
    npPush pfRule(npPop2(),npPop1())
  false

npDefinitionItem()==
  npImport()  or
    a := npState()
    npStatement() =>
      npEqPeek "DEF" =>
        npRestore a
        npDef()
      npEqPeek "MDEF" =>
        npRestore a
        npMdef "MDEF"
      npRestore a
      npMacro() or npDefn()
        or npName() and npDecl()
    npTrap()

npDefinition() ==
  npPP function npDefinitionItem and
    npPush pfSequenceToList npPop1()

pfSequenceToList x==
  pfSequence? x => pfSequenceArgs x
  pfListOf [x]

--% Diagnostic routines

npMissingMate(close,open)==
  ncSoftError(tokPosn open, 'S2CY0008, [])
  npMissing close
 
npMissing s==
  ncSoftError(tokPosn $stok,'S2CY0007, [PNAME s])
  THROW("TRAPPOINT","TRAPPED")
 
npCompMissing s ==
  npEqKey s or npMissing s
 
npRecoverTrap()==
  npFirstTok()
  pos1 := tokPosn $stok
  npMoveTo 0
  pos2 := tokPosn $stok
  syIgnoredFromTo(pos1, pos2)
  npPush [pfWrong(pfDocument ['"pile syntax error"],pfListOf [])]
 
 
npListAndRecover(f)==
  a := $stack
  b := nil
  $stack := nil
  done := false
  c := $inputStream
  while not done repeat
    found:=CATCH("TRAPPOINT",apply(f,[]))
    if found="TRAPPED" then
       $inputStream:=c
       npRecoverTrap()
    else if not found
         then
           $inputStream:=c
           syGeneralErrorHere()
           npRecoverTrap()
    if npEqKey "BACKSET" then
       c:=$inputStream
    else if npEqPeek "BACKTAB"
         then
            done:=true
         else
           $inputStream:=c
           syGeneralErrorHere()
           npRecoverTrap()
           if npEqPeek "BACKTAB"
           then done:=true
           else
               npNext()
               c:=$inputStream
    b := [npPop1(),:b]
  $stack := a
  npPush reverse! b
 
npMoveTo n==
  $inputStream = nil => true
  npEqPeek "BACKTAB" =>
    n = 0 => true
    (npNext();npMoveTo(n-1))
  npEqPeek "BACKSET" =>
    n = 0 => true
    (npNext();npMoveTo n)
  npEqKey "SETTAB" => npMoveTo(n+1)
  (npNext();npMoveTo n)

--%

_/RF(:x) ==
  $Echo: local := true
  _/RF_-1 nil

_/RQ(:x) ==
  $Echo: local := false
  _/RF_-1 nil

_/RQ_,LIB(:x) ==
  $Echo: local := false
  _/RF_-1 nil


_/RF_-1 x ==
  ifile := makeInputFilename $editFile
  lfile := nil
  type := filePathType ifile
  type = '"boot" =>
    lfile := makeFilePath(type <- '"lisp",defaults <- ifile)
    BOOT(ifile,lfile)
    LOAD lfile
  type = '"lisp" => LOAD ifile
  type = '"input" => ncINTERPFILE(ifile,$Echo)
  SPAD ifile
