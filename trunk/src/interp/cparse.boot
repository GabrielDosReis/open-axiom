-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2009, Gabriel Dos Reis.
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

npParse stream ==
    $inputStream:local := stream
    $stack:local       :=nil
    $stok:local:=nil
    $ttok:local:=nil
    npFirstTok()
    found:=CATCH("TRAPPOINT",npItem())
    if found="TRAPPED"
    then
      ncSoftError(tokPosn $stok,'S2CY0006, [])
      pfWrong(pfDocument  '"top level syntax error" ,pfListOf nil)
    else if not null $inputStream
         then
          ncSoftError(tokPosn $stok,'S2CY0002,[])
          pfWrong(pfDocument ['"input stream not exhausted"],pfListOf [])
         else if null $stack
              then
                 ncSoftError(tokPosn $stok,'S2CY0009, [])
                 pfWrong(pfDocument ['"stack empty"],pfListOf [])
              else
                 first $stack

npItem()==
     npQualDef() =>
            npEqKey "SEMICOLON" =>
                      [a,b]:=npItem1 npPop1 ()
                      c:=pfEnSequence b
                      a => npPush c
                      npPush pfNovalue c
            npPush pfEnSequence npPop1 ()
     false

npItem1 c==
     npQualDef() =>
            npEqKey "SEMICOLON" =>
                      [a,b]:=npItem1 npPop1 ()
                      [a,append(c,b)]
            [true,append (c,npPop1())]
     [false,c]

npFirstTok()==
      $stok:=
          if null $inputStream
          then tokConstruct("ERROR","NOMORE",tokPosn $stok)
          else first $inputStream
      $ttok:=tokPart $stok

npNext() ==
     $inputStream := rest($inputStream)
     npFirstTok()

npState()==cons($inputStream,$stack)

npRestore(x)==
      $inputStream:=first x
      npFirstTok()
      $stack:=rest x
      true

npPush x==$stack:=CONS(x,$stack)

npPushId()==
   a:=GETL($ttok,'INFGENERIC)
   $ttok:= if a then a else $ttok
   $stack:=CONS(tokConstruct("id",$ttok,tokPosn $stok),$stack)
   npNext()

npPop1()==
       a:=first $stack
       $stack:=rest $stack
       a

npPop2()==
       a:= second $stack
       RPLACD($stack,CDDR $stack)
       a

npPop3()==
       a:= third $stack
       RPLACD(rest $stack,CDDDR $stack)
       a

npParenthesized f==
   npParenthesize("(",")",f)   or
   npParenthesize("(|","|)",f)

npParenthesize (open,close,f)==
    a:=$stok
    npEqKey open =>
         APPLY(f,nil) and (npEqKey close or npMissingMate(close,a))=> true
         npEqKey close  =>  npPush  []
         npMissingMate(close,a)
    false

npEnclosed(open,close,fn,f)==
    a:=$stok
    npEqKey open =>
        npEqKey close  => npPush FUNCALL(fn,a,pfTuple pfListOf [])
        APPLY(f,nil) and (npEqKey close or npMissingMate(close,a))=>
                   npPush FUNCALL (fn,a,pfEnSequence npPop1())
        false
    false

npParened f ==
    npEnclosed("(",")",function pfParen,f) or
    npEnclosed("(|","|)",function pfParen,f)

npBracked f ==
    npEnclosed("[","]",function pfBracket,f) or
    npEnclosed("[|","|]",function pfBracketBar,f)

npBraced f ==
    npEnclosed("{","}",function pfBrace,f) or
    npEnclosed("{|","|}",function pfBraceBar,f)

npAngleBared f ==
    npEnclosed("<|","|>",function pfHide,f)

npBracketed f==
  npParened f or npBracked f or npBraced f or npAngleBared f

npPileBracketed f==
 if npEqKey "SETTAB"
 then if npEqKey "BACKTAB"
      then npPush pfNothing()     -- never happens
      else if APPLY(f,nil) and (npEqKey "BACKTAB" or npMissing "backtab")
           then npPush pfPile npPop1()
           else false
 else false

npListofFun(f,h,g)==
    if APPLY(f,nil)
    then
        if APPLY(h,nil) and (APPLY(f,nil) or npTrap())
        then
          a:=$stack
          $stack:=nil
          while APPLY(h,nil) and (APPLY(f,nil) or npTrap()) repeat 0
          $stack:=cons(NREVERSE $stack,a)
          npPush FUNCALL(g, [npPop3(),npPop2(),:npPop1()])
        else
          true
    else false

npList(f,str1,g)== -- always produces a list, g is applied to it
    if APPLY(f,nil)
    then
        if npEqKey str1 and (npEqKey "BACKSET" or true)
                       and (APPLY(f,nil) or npTrap())
        then
          a:=$stack
          $stack:=nil
          while npEqKey str1 and (npEqKey "BACKSET" or true) and
                             (APPLY(f,nil) or npTrap()) repeat 0
          $stack:=cons(NREVERSE $stack,a)
          npPush FUNCALL(g,  [npPop3(),npPop2(),:npPop1()])
        else
          npPush FUNCALL(g, [npPop1()])
    else npPush FUNCALL(g, [])


npPPff f ==
  FUNCALL f and npPush [npPop1()]

npPPf f ==
  npSemiListing function LAMBDA(nil, npPPff f)

npPPg f ==
  npListAndRecover function LAMBDA(nil, npPPf f)
    and npPush pfAppend npPop1()

npPP(f) ==
  npParened function LAMBDA(nil, npPPf f)
    or npPileBracketed function LAMBDA(nil, npPPg f) and
      npPush pfEnSequence npPop1()
        or FUNCALL f

npPCff f ==
  FUNCALL f and npPush [npPop1()]

npPCg f ==
  npListAndRecover function LAMBDA(nil,npPCff f)
    and npPush pfAppend npPop1()

npPC(f) ==
  npPileBracketed function LAMBDA(nil, npPCg f) and
    npPush pfEnSequence npPop1()
      or FUNCALL f


-- s must transform the head of the stack

npAnyNo s==
     while APPLY(s,nil) repeat 0
     true

npAndOr(keyword,p,f)==
   npEqKey keyword and (APPLY(p,nil) or npTrap())
             and npPush FUNCALL(f, npPop1())

npRightAssoc(o,p)==
    a:=npState()
    if APPLY(p,nil)
    then
       while  npInfGeneric o and (npRightAssoc(o,p)
               or (npPush pfApplication(npPop2(),npPop1());false)) repeat
             npPush pfInfApplication(npPop2(),npPop2(),npPop1())
       true
    else
       npRestore a
       false

-- p o p o p o p = (((p o p) o p) o p)
-- p o p o = (p o p) o

npLeftAssoc(operations,parser)==
    if APPLY(parser,nil)
    then
       while npInfGeneric(operations)
         and (APPLY(parser,nil) or
              (npPush pfApplication(npPop2(),npPop1());false))
           repeat
             npPush pfInfApplication(npPop2(),npPop2(),npPop1())
       true
    else false

npInfixOp()==
  CAAR $stok = "key" and
    GETL($ttok,"INFGENERIC") and npPushId()

npInfixOperator()== npInfixOp() or
        a:=npState()
        b:=$stok
        npEqKey "'" and npInfixOp() =>
                npPush pfSymb (npPop1 (),tokPosn b)
        npRestore a
        npEqKey "BACKQUOTE" and  npInfixOp() =>
                a:=npPop1()
                npPush tokConstruct("idsy",tokPart a,tokPosn a)
        npRestore a
        false

npInfKey s==  CAAR $stok="key" and  MEMQ($ttok,s) and npPushId()

npDDInfKey s==
    npInfKey s or
        a:=npState()
        b:=$stok
        npEqKey "'" and npInfKey s =>
                 npPush pfSymb (npPop1 () ,tokPosn b)
        npRestore a
        npEqKey "BACKQUOTE" and  npInfKey s =>
                a:=npPop1()
                npPush tokConstruct("idsy",tokPart a,tokPosn a)
        npRestore a
        false

npInfGeneric s== npDDInfKey s  and
                   (npEqKey "BACKSET" or true)

npConditional f==
  if  npEqKey "IF" and (npLogical() or npTrap()) and
                   (npEqKey "BACKSET" or true)
  then
           if npEqKey "SETTAB"
           then if npEqKey "THEN"
                then  (APPLY(f,nil) or npTrap()) and npElse(f)
                        and npEqKey "BACKTAB"
                else  npMissing "then"
           else if npEqKey "THEN"
                then (APPLY(f,nil) or npTrap()) and npElse(f)
                else npMissing "then"
  else false

npElse(f)==
           a:=npState()
           if npBacksetElse()
           then  (APPLY(f,nil) or npTrap()) and
                 npPush pfIf(npPop3(),npPop2(),npPop1())
           else
              npRestore a
              npPush pfIfThenOnly(npPop2(),npPop1())

npBacksetElse()==
    if npEqKey "BACKSET"
    then npEqKey "ELSE"
    else npEqKey "ELSE"

npWConditional f==
    if npConditional f
    then npPush pfTweakIf npPop1()
    else false

npQuantified f ==
  npEqPeek "FORALL" =>
    npQuantifierVariable "FORALL" and npQuantified f and
      npPush %Forall(npPop2(), npPop1())
  npEqPeek "EXIST" =>
    npQuantifierVariable "EXIST" and npQuantified f and
      npPush %Exist(npPop2(), npPop1())
  apply(f,nil)  

-- Parsing functions

-- peek for keyword s, no advance of token stream

npEqPeek s ==  CAAR $stok="key" and EQ(s,$ttok)

-- test for keyword s, if found advance token stream

npEqKey s ==
    CAAR $stok="key" and EQ(s,$ttok) and npNext()

$npTokToNames:= ["~","#","[]","{}", "[||]","{||}"]

npId() ==
        CAAR $stok="id" =>
               npPush $stok
               npNext()
        CAAR $stok="key" and MEMQ($ttok,$npTokToNames)=>
               npPush tokConstruct("id",$ttok,tokPosn $stok)
               npNext()
        false

npSymbolVariable()==
     a:=npState()
     npEqKey "BACKQUOTE" and  npId()  =>
          a:=npPop1()
          npPush tokConstruct("idsy",tokPart a,tokPosn a)
     npRestore a
     false

npName()==npId() or npSymbolVariable()

npConstTok() ==
     tokType $stok in '(integer string char float command) =>
          npPush $stok
          npNext()
     npEqPeek "'" =>
          a:=$stok
          b:=npState()
          npNext()
          if
              npPrimary1() and npPush pfSymb(npPop1(),tokPosn a)
          then true
          else
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

npPrimary2()== npEncAp function npAtom2 -- or  npBPileDefinition()
               or npAdd(pfNothing()) or npWith(pfNothing())


npAtom1()== npPDefinition() or ((npName() or npConstTok() or
       npDollar() or npBDefinition()) and npFromdom())

npAtom2()== (npInfixOperator() or npAmpersand() or npPrefixColon())
                           and npFromdom()

npDollar()== npEqPeek "$" and
   npPush tokConstruct("id","$",tokPosn $stok)
   npNext()

npPrefixColon()== npEqPeek "COLON" and
   npPush tokConstruct("id",":",tokPosn $stok)
   npNext()

-- silly

npEncAp(f)== APPLY(f,nil) and npAnyNo function npEncl
                   and npFromdom()


npEncl()==  npBDefinition() and npPush pfApplication(npPop2(),npPop1())

npFromdom()==
  npEqKey "$" and (npApplication() or npTrap())
      and npFromdom1 npPop1() and npPush pfFromDom(npPop1(),npPop1())
         or true

npFromdom1 c==
  npEqKey "$" and (npApplication() or npTrap())
    and npFromdom1 npPop1()  and npPush pfFromDom(npPop1(),c)
        or npPush c


npPrimary()==   npPrimary1() or npPrimary2()

npDotted f== APPLY(f,nil) and npAnyNo function npSelector

npSelector()==
            npEqKey "DOT" and (npPrimary() or npTrap()) and
              npPush(pfApplication(npPop2(),npPop1()))

npApplication()==
   npDotted function npPrimary and
      (npApplication2() and
            npPush(pfApplication(npPop2(),npPop1())) or true)


npApplication2()==
   npDotted function npPrimary1 and
      (npApplication2() and
            npPush(pfApplication(npPop2(),npPop1())) or true)

npTypedForm1(sy,fn) ==
     npEqKey sy  and (npType() or npTrap()) and
        npPush FUNCALL(fn,npPop2(),npPop1())

npTypedForm(sy,fn) ==
     npEqKey sy  and (npApplication() or npTrap()) and
        npPush FUNCALL(fn,npPop2(),npPop1())

npRestrict() == npTypedForm("AT",function pfRestrict)

npCoerceTo() == npTypedForm("COERCE",function pfCoerceto)

npPretend() == npTypedForm("PRETEND",function pfPretend)

npTypeStyle()==
 npCoerceTo() or npRestrict() or npPretend()

npTypified ()==npApplication() and npAnyNo function npTypeStyle

npTagged() == npTypedForm1("COLON",function pfTagged)

npColon () == npTypified() and npAnyNo function npTagged

npPower() == npRightAssoc('(POWER CARAT),function npColon)

npProduct()==
    npLeftAssoc('(TIMES SLASH BACKSLASH SLASHSLASH
       BACKSLASHBACKSLASH SLASHBACKSLASH BACKSLASHSLASH )
                       ,function npPower)

npRemainder()==
    npLeftAssoc('(REM QUO )  ,function npProduct)

npTerm()==
   npInfGeneric '(MINUS PLUS) and (npRemainder()
        and npPush(pfApplication(npPop2(),npPop1())) or true)
             or npRemainder()


npSum()==npLeftAssoc('(PLUS MINUS),function npTerm)

npArith()==npLeftAssoc('(MOD),function npSum)

npSegment()==  npEqPeek "SEG"  and npPushId() and npFromdom()

npInterval()==
  npArith() and
   (npSegment() and ((npEqPeek "BAR"
      and npPush(pfApplication(npPop1(),npPop1()))) or
     (npArith() and npPush(pfInfApplication(npPop2(),npPop2(),npPop1())))
            or npPush(pfApplication(npPop1(),npPop1()))) or true)

npBy()== npLeftAssoc ('(BY),function npInterval)

npAmpersand()==  npEqKey "AMPERSAND" and (npName() or npTrap())
npAmpersandFrom()== npAmpersand()  and npFromdom()

npSynthetic()==
    if npBy()
    then
       while npAmpersandFrom()  and (npBy() or
          (npPush pfApplication(npPop2(),npPop1());false)) repeat
             npPush pfInfApplication(npPop2(),npPop2(),npPop1())
       true
    else false

npRelation()==
   npLeftAssoc ('(EQUAL NOTEQUAL LT LE GT GE OANGLE CANGLE),
            function npSynthetic)

npQuiver()  ==    npRightAssoc('(ARROW LARROW),function npRelation)
npDiscrim() == 
  npLeftAssoc ('(CASE HAS IS ISNT), function npQuiver)

npDisjand() ==    npLeftAssoc('(AND ),function npDiscrim)

npLogical()   ==  npLeftAssoc('(OR ),function npDisjand)
npSuch() == npLeftAssoc( '(BAR),function npLogical)

++ Parse a type expression
++   Type:
++     MonoType
++     QuantifiedVariable Type
npType() ==
  npQuantified function npMonoType

npMonoType() ==  npSuch()  and
                a:=npPop1()
                npWith(a) or npPush a

npADD()    ==   npMonoType() and
                a:=npPop1()
                npAdd(a) or npPush a

npConditionalStatement()==npConditional function npQualifiedDefinition

npExpress1()==
  npConditionalStatement() 
    or npBackTrack(function npADD, "BECOMES", function npAssignment)

npCommaBackSet()== npEqKey "COMMA" and (npEqKey "BACKSET" or true)

npExpress()==
     npExpress1() and
        (npIterators() and
             npPush pfCollect (npPop2(),pfListOf npPop1()) or true)

npZeroOrMore f==
       APPLY(f,nil)=>
         a:=$stack
         $stack:=nil
         while APPLY(f,nil) repeat 0
         $stack:=cons(NREVERSE $stack,a)
         npPush cons(npPop2(),npPop1())
       npPush nil
       true

npIterators()==
         npForIn() and npZeroOrMore function npIterator
             and npPush cons(npPop2(),npPop1())  or
              npWhile() and (npIterators() and
                    npPush cons(npPop2(),npPop1()) or npPush [npPop1()])

npIterator()==   npForIn() or npSuchThat() or npWhile()


++ Parse a case-pattern expression.
++   Case:
++     CASE Interval IS PileExit
npCase() ==
  npEqKey "CASE" =>
    (npInterval() or npTrap()) and (npEqKey "IS" or npTrap())
      and (pPP function npPileExit or npTrap())
        and npPush pfCase(npPop2(), pfSequenceToList npPop1())
  false

npStatement()==
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

npBackTrack(p1,p2,p3)==
     a:=npState()
     APPLY(p1,nil) =>
         npEqPeek p2   =>
            npRestore a
            APPLY(p3,nil) or npTrap()
         true
     false

npMDEF() == 
  npBackTrack(function npStatement,"MDEF",
    function LAMBDA(nil, npMdef "MDEF"))

npAssign()== npBackTrack(function npMDEF,"BECOMES",function npAssignment)

npAssignment()==
    npAssignVariable() and
      (npEqKey "BECOMES" or npTrap()) and
        (npGives() or npTrap()) and
           npPush pfAssign (npPop2(),npPop1())

npAssignVariableName()==npApplication() and
      a:=npPop1()
      if pfId? a
      then
         (npPush a and npDecl() or npPush pfTyped(npPop1(),pfNothing()))
      else npPush a

npAssignVariable()== npColon() and npPush pfListOf [npPop1()]

npAssignVariablelist()== npListing function npAssignVariableName

npExit()== npBackTrack(function npAssign,"EXIT",function npPileExit)

npPileExit()==
     npAssign() and (npEqKey "EXIT" or npTrap()) and
         (npStatement() or npTrap())
           and npPush pfExit (npPop2(),npPop1())

npGives()== npBackTrack(function npExit,"GIVES",function npLambda)

npDefinitionOrStatement()==
  npQuantified 
    function LAMBDA(nil, npBackTrack(function npGives,
                            "DEF",function npDef))

npVoid()== npAndOr("DO",function npStatement,function pfNovalue)

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

npSuchThat()==npAndOr("BAR",function npLogical,function pfSuchthat)

npWhile()==npAndOr ("WHILE",function npLogical,function pfWhile)

npForIn()==
  npEqKey "FOR" and (npVariable() or npTrap()) and (npCompMissing "IN")
      and ((npBy()  or npTrap()) and
         npPush pfForin(npPop2(),npPop1()))

npBreak()==
     npEqKey "BREAK" and  npPush pfBreak pfNothing ()

npIterate()==
     npEqKey "ITERATE" and  npPush pfIterate pfNothing ()

npQualType()==
     npType() and
            npPush pfQualType(npPop1(),pfNothing())

npSQualTypelist()== npListing function npQualType
                and npPush pfParts npPop1 ()

npQualTypelist()== npPC function npSQualTypelist
                             and npPush pfUnSequence npPop1 ()

npImport()==npAndOr("IMPORT",function npQualTypelist,function pfImport)

npInline()==npAndOr("INLINE",function npQualTypelist,function pfInline)

npLocalDecl()== npEqKey "COLON" and (npType() or npTrap()) and
             npPush pfSpread (pfParts npPop2(),npPop1()) or
              npPush pfSpread (pfParts npPop1(),pfNothing())

npLocalItem()==npTypeVariable() and  npLocalDecl()

npLocalItemlist()== npPC function npSLocalItem
                             and npPush pfUnSequence npPop1 ()

npSLocalItem()== npListing function npLocalItem
        and npPush  pfAppend pfParts npPop1()

npFree()== npEqKey "FREE" and (npLocalItemlist() or npTrap())
     and npPush pfFree npPop1()

npLocal()== npEqKey "local" and (npLocalItemlist() or npTrap())
     and npPush pfLocal npPop1()
npExport()== npEqKey "EXPORT" and (npLocalItemlist() or npTrap())
     and npPush pfExport npPop1()

npLet()== npLetQualified function npDefinitionOrStatement

npDefn()== npEqKey "DEFN" and  npPP function npDef

npFix()== npEqKey "FIX" and  npPP function npDef
               and npPush pfFix npPop1 ()

npMacro() == 
  npEqKey "MACRO" and  npPP function LAMBDA(nil, npMdef "DEF")

npRule()== npEqKey "RULE" and  npPP function npSingleRule

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
             (npDefinitionOrStatement() or npTrap())
         and  npPush [pfAdd(pfNothing(),npPop1(),pfNothing())]

npWith(extra)==
     npEqKey "WITH" =>
       a:=npState()
       npCategoryL() or npTrap()
       npEqPeek "IN" =>
               npRestore a
               (npVariable() or npTrap()) and
                     npCompMissing "IN"  and
                          (npCategoryL() or npTrap()) and
                              npPush pfWith(npPop2(),npPop1(),extra)
       npPush pfWith(pfNothing(),npPop1(),extra)

npCategoryL()== npCategory() and npPush pfUnSequence npPop1 ()

pfUnSequence x==
        pfSequence? x =>   pfListOf pfAppend pf0SequenceArgs  x
        pfListOf x

npCategory()== npPP function npSCategory

npSCategory()==
  if npWConditional function npCategoryL
  then  npPush [npPop1()]
  else
    if npDefaultValue()
    then true
    else
      a:=npState()
      if npPrimary()
      then if npEqPeek "COLON"
           then
              npRestore a
              npSignature()
           else
              npRestore a
              npApplication() and npPush [pfAttribute (npPop1())]
                           or npTrap()

      else false


npSignatureDefinee()==
   npName() or npInfixOperator() or npPrefixColon()


npSigDecl()== npEqKey "COLON" and (npType() or npTrap()) and
           npPush pfSpread (pfParts npPop2(),npPop1())

npSigItem()==npTypeVariable() and  (npSigDecl() or npTrap())

npSigItemlist()== npListing function npSigItem
        and npPush pfListOf pfAppend pfParts npPop1()

npSignature()==
    npSigItemlist() and
            npPush pfWDec(pfNothing(),npPop1())

npSemiListing (p)==
       npListofFun(p,function npSemiBackSet,function pfAppend)

npSemiBackSet()== npEqKey "SEMICOLON" and (npEqKey "BACKSET" or true)
npDecl()== npEqKey "COLON" and (npType() or npTrap()) and
           npPush pfTyped (npPop2(),npPop1())

npVariableName()==npName() and
      (npDecl() or npPush pfTyped(npPop1(),pfNothing()))

npVariable()== npParenthesized function npVariablelist or
      (npVariableName() and npPush pfListOf [npPop1()])

npVariablelist()== npListing function npVariableName

++ Parse binders of a quantified expression
++   QuantifiedVariable:
++      Quantifier Variable DOT
++   Quantifier:
++      EXIST
++      FORALL
npQuantifierVariable quantifier ==
  npEqKey quantifier and (npVariable() or npTrap())
    and npEqKey "DOT"


npListing (p)==npList(p,"COMMA",function pfListOf)
npQualified(f)==
    if FUNCALL f
    then
     while npEqKey "WHERE" and (npDefinition() or npTrap()) repeat
             npPush pfWhere(npPop1(),npPop1())
     true
    else  npLetQualified  f

npLetQualified f==
      npEqKey "%LET" and
      (npDefinition() or npTrap()) and
      npCompMissing "IN"  and
      (FUNCALL f or npTrap()) and
      npPush pfWhere(npPop2(),npPop1())


npQualifiedDefinition()==
       npQualified function npDefinitionOrStatement

npTuple (p)==
    npListofFun(p,function npCommaBackSet,function pfTupleListOf)
npComma()==  npTuple function npQualifiedDefinition

npQualDef()== npComma() and npPush [npPop1()]

npDefinitionlist ()==npSemiListing(function npQualDef)

npPDefinition ()==
     npParenthesized function npDefinitionlist and
                 npPush pfEnSequence npPop1()

npBDefinition()== npPDefinition() or
            npBracketed function npDefinitionlist

npPileDefinitionlist()==
 npListAndRecover function npDefinitionlist
    and npPush pfAppend npPop1()


npTypeVariable()== npParenthesized function npTypeVariablelist or
           npSignatureDefinee() and npPush pfListOf [npPop1()]

npTypeVariablelist()== npListing function npSignatureDefinee

npBPileDefinition()==
     npPileBracketed function npPileDefinitionlist
       and npPush pfSequence pfListOf npPop1 ()


npLambda()==
     (npVariable() and
      ((npLambda() or npTrap()) and
       npPush pfLam(npPop2(),npPop1()))) or
         npEqKey "GIVES" and (npDefinitionOrStatement() or npTrap()) or
          npEqKey "COLON" and (npType() or npTrap()) and
            npEqKey "GIVES" and (npDefinitionOrStatement() or npTrap())
               and
                  npPush pfReturnTyped(npPop2(),npPop1())

npDef()==
    npSuch() =>
         [op,arg,rt]:=  pfCheckItOut(npPop1())
         npDefTail "DEF" or npTrap()
         body:=npPop1()
         null arg => npPush pfDefinition (op,body)
         npPush pfDefinition (op,pfPushBody(rt,arg,body))
    false

npDefTail kw == 
  npEqKey kw and npDefinitionOrStatement()

npMdef kw ==
    npQuiver() =>
         [op,arg]:=  pfCheckMacroOut(npPop1())
         npDefTail kw or npTrap()
         body:=npPop1()
         null arg => npPush pfMacro (op,body)
         npPush pfMacro (op,pfPushMacroBody(arg,body))
    false


npSingleRule()==
    npQuiver() =>
         npDefTail "DEF" or npTrap()
         npPush pfRule (npPop2(),npPop1())
    false

npDefinitionItem()==
      npImport()  or
          a:=npState()
          npStatement() =>
               npEqPeek "DEF" =>
                  npRestore a
                  npDef()
               npEqPeek "MDEF" =>
                  npRestore a
                  npMdef "MDEF"
               npRestore a
               npMacro() or npDefn()
          npTrap()

npDefinition()== npPP function npDefinitionItem
            and npPush  pfSequenceToList npPop1 ()

pfSequenceToList x==
        pfSequence? x =>  pfSequenceArgs  x
        pfListOf [x]

--% Diagnostic routines

npMissingMate(close,open)==
   ncSoftError(tokPosn open, 'S2CY0008, [])
   npMissing close
 
npMissing s==
   ncSoftError(tokPosn $stok,'S2CY0007, [PNAME s])
   THROW("TRAPPOINT","TRAPPED")
 
npCompMissing s == npEqKey s or npMissing s
 
npRecoverTrap()==
  npFirstTok()
  pos1 := tokPosn $stok
  npMoveTo 0
  pos2 := tokPosn $stok
  syIgnoredFromTo(pos1, pos2)
  npPush [pfWrong(pfDocument ['"pile syntax error"],pfListOf [])]
 
 
npListAndRecover(f)==
   a:=$stack
   b:=nil
   $stack:=nil
   done:=false
   c:=$inputStream
   while not done repeat
     found:=CATCH("TRAPPOINT",APPLY(f,nil))
     if found="TRAPPED"
     then
        $inputStream:=c
        npRecoverTrap()
     else if not found
          then
            $inputStream:=c
            syGeneralErrorHere()
            npRecoverTrap()
     if npEqKey "BACKSET"
     then
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
     b:=cons(npPop1(),b)
   $stack:=a
   npPush NREVERSE b
 
npMoveTo n==
      if null $inputStream
      then true
      else
           if npEqPeek "BACKTAB"
           then if n=0
                then true
                else (npNext();npMoveTo(n-1))
           else if npEqPeek "BACKSET"
                then if n=0
                     then true
                     else (npNext();npMoveTo n)
                 else if npEqKey "SETTAB"
                      then npMoveTo(n+1)
                      else (npNext();npMoveTo n)
