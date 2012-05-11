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

-- This file contains a low-level code for parsing a Spad source file 
-- into an internal AST.  Please note that this AST is the AST used by
-- the Spad compiler, which is different from the AST used by the 
-- interpreter (a VAT). The AST, for the entire file, is a List Syntax
-- Since this is low-level code, I don't expect people to get here,
-- and you should directly use it.  If you think, you need to get to
-- here, then something is already wrong.
-- There is a higher-level interface, written in SPAD, to this
-- code.  See the algebra file spad-parser.spad.
--
-- -- gdr/2007-11-02
--

import preparse
import parse
namespace BOOT

module spad_-parser where
  indentationLocation: %String -> %Maybe %Short
  stringPrefix?: (%String,%String) -> %Boolean

--%

addClose(line,ch) ==
  line.(maxIndex line) = char ";" =>
    ch = char ";" => line
    line.(maxIndex line) := ch
    SUFFIX(char ";",line)
  SUFFIX(ch,line)

escaped?(s,n) ==
  n > 0 and s.(n-1) = char "__"

infixToken? s ==
  STRING2ID_-N(s,1) in '(_then _else)

atEndOfUnit? x ==
  not string? x
  
++ Return the logical indentation position in the `line', after
++ expansion of leading vertical tab characters.
indentationLocation line ==
  loc := 0
  n := #line
  for i in 0.. repeat
    i >= n => return nil
    spaceChar? line.i => loc := loc + 1
    tabChar? line.i => loc := 8 * (loc quo 8 + 1)
    return loc

++ Return true if the string `s1' is a prefix of `s2'.
stringPrefix?(s1,s2) ==
  n1 := #s1
  n1 > #s2 => false
  and/[s1.i = s2.i for i in 0..(n1-1)]

skipIfBlock x ==
  [n,:line] := z := preparseReadLine1 x
  not string? line => z
  #line = 0 => skipIfBlock x
  line.0 = char ")" =>
    stringPrefix?('")if",line) =>
      EVAL string2BootTree storeBlanks!(line,2) => preparseReadLine x
      skipIfBlock x
    stringPrefix?('")elseif",line) =>
      EVAL string2BootTree storeBlanks!(line,7) => preparseReadLine x
      skipIfBlock x
    stringPrefix?('")else",line) or stringPrefix?('")endif",line) =>
      preparseReadLine x
    stringPrefix?('")fin",line) => [n,:%nothing]
    skipIfBlock x
  skipIfBlock x

skipToEndif x ==
  [n,:line] := z := preparseReadLine1 x
  not string? line => z
  stringPrefix?(line,'")endif") => preparseReadLine x
  stringPrefix?(line,'")fin") => [n,:%nothing]
  skipToEndif x

++ `n' is the line number of the current line
++ `oldnums' is the list of line numbers of previous lines
++ `oldlocs' is the list of previous indentation locations
++ `ncblock' is the current comment block
findCommentBlock(n,oldnums,oldlocs,ncblock,lines) ==
  x :=
    [nc,:block] := ncblock
    nc = 0 => [n - 1,:reverse block]
    if $EchoLineStack then
      [n,:$EchoLineStack] := $EchoLineStack
      preparseEcho lines
      $EchoLineStack := [n]
    [or/[n for n in oldnums for l in oldlocs | integer? l and l <= nc],
       :reverse block]
  $COMBLOCKLIST := [x,:$COMBLOCKLIST]

preparseReadLine x ==
  [n,:line] := z := preparseReadLine1 x
  not string? line or #line = 0 => z
  line.0 = char ")" =>
    stringPrefix?('")if",line) =>
      EVAL string2BootTree storeBlanks!(line,3) => preparseReadLine x
      skipIfBlock x
    stringPrefix?('")elseif",line) or stringPrefix?('")else",line) =>
      skipToEndif x
    stringPrefix?('")endif",line) => preparseReadLine x
    stringPrefix?('")fin",line) => [n,:%nothing]
    z
  z

preparseReadLine1 x ==
  if $LineList then
    [line,:$LineList] := $LineList
  else
    line := expandLeadingTabs readLine IN_-STREAM
  $preparseLastLine := line
  not string? line => [$INDEX]
  $INDEX := $INDEX + 1
  line := trimTrailingBlank line
  $EchoLineStack := [copyString line,:$EchoLineStack]
  n := $INDEX
  if #line > 0 and line.maxIndex(line) = char "__" then
    line := strconc(subString(line,0,maxIndex line),rest preparseReadLine1 x)
    $preparseLastLine := line
  [n,:line]

preparseEcho lines ==
  if $Echo then
    for x in reverse lines repeat
      formatToStream(OUT_-STREAM,'"~&;~A~%",x)
  $EchoLineStack := nil

++ The line to be worked on is (CAR SLINES).  
++ It's indentation is (CAR SLOCS).  
++ There is a notion of current indentation. Then:
++
++  A. Add open paren to beginning of following line if following 
++     line's indentation is greater than current, and add close paren
++     to end of last succeeding line with following line's indentation.
++  B. Add semicolon to end of line if following line's indentation is
++     the same.
++  C. If the entire line consists of the single keyword then or else, 
++     leave it alone. 
addParensAndSemisToLine(lines,locs) ==
  sc := first locs   -- first line column number
  sc = nil or sc <= 0 => nil
  count := 0         -- number of semicolons added
  i := 0             -- running local line number
  for x in tails rest lines for y in tails rest locs repeat
    i := i + 1
    nc := first y
    nc = nil => nil
    nc := abs nc
    nc < sc => leave nil
    nc = sc and (y.first := -nc) and not infixToken? first x =>
      z := drop(i - 1,lines)
      z.first := addClose(first z,char ";")
      count := count + 1
  count > 0 =>
    first(lines).(firstNonblankCharPosition first lines - 1) := char "("
    lines := drop(i - 1,lines)
    lines.first := addClose(first lines,char ")")
  nil

++ Add parens and semis to lines to aid parsing.  
parsePiles(locs,lines) ==
  for x in tails append!(lines,['" "])
    for y in tails append!(locs,[nil]) repeat
      addParensAndSemisToLine(x,y)
  lines

preparse st ==
  $COMBLOCKLIST := nil
  $SKIPME := false
  stack := 
    $preparseLastLine =>
      $preparseLastLine is [.,:.] => $preparseLastLine
      [$preparseLastLine]
    nil
  $INDEX := $INDEX - #stack
  u := preparse1 stack
  $SKIPME => preparse st
  if $preparseReportIfTrue then
    PARSEPRINT u
  $headerDocumentation := nil
  $docList := nil
  $maxSignatureLineNumber := 0
  $constructorLineNumber := IFCAR IFCAR u
  u
      

--%  
macro compulsorySyntax s ==
  s or SPAD__SYNTAX__ERROR()

repeatedSyntax(l,p) ==
  n := stackSize $reduceStack
  once := false
  while apply(p,nil) repeat
    once := true
  not once => nil
  x := nil
  for i in (n+1)..stackSize $reduceStack repeat
    x := [popStack1(),:x]
  x = nil => true
  pushReduction(l,x)

--%

parseToken tt ==
  tok := matchCurrentToken tt =>
    pushReduction(makeSymbol strconc(symbolName tt,'"Token"),tokenSymbol tok)
    advanceToken()
    true
  false

parseGlyph s ==
  matchCurrentToken('GLIPH,s) =>
    advanceToken()
    true
  false

parseNBGlyph tok ==
  matchCurrentToken('GLIPH,tok) and $nonblank =>
    advanceToken()
    true
  false

parseString() ==
  parseToken 'SPADSTRING

parseInteger() ==
  parseToken 'NUMBER

parseFloatBasePart() ==
  matchAdvanceGlyph "." =>
    $nonblank and (t := matchCurrentToken 'NUMBER) =>
      t := copyToken t
      advanceToken()
      pushReduction('parseFloatBasePart,tokenNonblank? t)
      pushReduction('parseFloatBasePart,tokenSymbol t)
    pushReduction('parseFloatBasePart,0)
    pushReduction('parseFloatBasePart,0)
  nil

parseFloatBase() ==
  integer? currentSymbol() and currentChar() = char "." and
    nextChar() ~= char "." and parseInteger() =>
      compulsorySyntax parseFloatBasePart()
  integer? currentSymbol() and charUpcase currentChar() = char "E"
    and parseInteger() =>
      pushReduction('parseBase,0)
      pushReduction('parseBase,0)
  digit? currentChar() and currentSymbol() is "." =>
    pushReduction('parseBase,0)
    pushReduction('parseBase,0)
  nil

parseFloatExponent() ==
  not ident? currentSymbol() => nil
  symbolMember?(currentSymbol(),'(e E)) and
    charMember?(currentChar(),[char "+",char "-"]) =>
      advanceToken()
      parseInteger() => true
      matchAdvanceGlyph "+" => compulsorySyntax parseInteger()
      matchAdvanceGlyph "-" =>
        compulsorySyntax parseInteger()
        pushReduction('parseFloatExponent,-popStack1())
      pushReduction('parseFloatExponent,0)
  g := FLOATEXPID currentSymbol() =>
    advanceToken()
    pushReduction('parseFloatExponent,g)
  nil

parseFloat() ==
  parseFloatBase() =>
    $nonblank and parseFloatExponent()
      or pushReduction('parseFloat,0)
    pushReduction('parseFloat,
      MAKE_-FLOAT(popStack4(),popStack2(),popStack2(),popStack1()))
  nil

parseName() ==
  parseToken 'IDENTIFIER and pushReduction('parseName,popStack1())

parseKeyword() ==
  parseToken 'KEYWORD and pushReduction('parseKeyword,popStack1())
  
parseFormalParameter() ==
  parseToken 'ARGUMENT_-DESIGNATOR

parseOperatorFunctionName() ==
  id := makeSymbolOf(matchCurrentToken 'KEYWORD
          or matchCurrentToken 'GLIPH
            or matchCurrentToken 'SPECIAL_-CHAR)
  symbolMember?(id,$OperatorFunctionNames) =>
    pushReduction('parseOperatorFunctionName,id)
    advanceToken()
    true
  false

parseAnyId() ==
  parseName() => true
  parseKeyword() => true
  matchString '"$" =>
    pushReduction('parseAnyId,currentSymbol())
    advanceToken()
    true
  parseOperatorFunctionName()

parseQuad() ==
  matchAdvanceString '"$" and pushReduction('parseQuad,"$")

parsePrimary1() ==
  parseName() =>
    $nonblank and currentSymbol() is "(" =>
      compulsorySyntax parsePrimary1()
      pushReduction('parsePrimary1,[popStack2(),popStack1()])
    true
  parseQuad() or parseString() or parseInteger() or
    parseFormalParameter() => true
  matchSpecial char "'" =>
    compulsorySyntax parseData()
    pushReduction('parsePrimary1,popStack1())
  parseSequence() or parseEnclosure()

parsePrimaryNoFloat() ==
  parsePrimary1() =>
    parseTokenTail() or true
  false

parsePrimary() ==
  parseFloat() or parsePrimaryNoFloat()

parsePrimaryOrQM() ==
  matchAdvanceString '"?" => pushReduction('parsePrimaryOrQM,"?")
  parsePrimary()

parseSpecialKeyWord() ==
  matchCurrentToken 'IDENTIFIER =>
    tokenSymbol(currentToken()) := unAbbreviateKeyword currentSymbol()
  nil

parseSexpr1() ==
  parseInteger() or parseString() => true
  parseAnyId() =>
    parseNBGlyph "=" =>
      compulsorySyntax parseSexpr1()
      SETQ(LABLASOC,[[popStack2(),:nthStack 1],:LABLASOC])
      true
    true
  matchAdvanceSpecial char "'" =>
    compulsorySyntax parseSexpr1()
    pushReduction('parseSexpr1,["QUOTE",popStack1()])
  matchAdvanceGlyph "[" =>
    stackUpdated?($reduceStack) := false
    repeatedSyntax('parseSexpr1,function PARSE_-Sexpr1)
    if not stackUpdated? $reduceStack then
      pushReduction('parseSexpr1,nil)
    compulsorySyntax matchAdvanceGlyph "]"
    pushReduction('parseSexpr1,LIST2VEC popStack1())
  matchAdvanceGlyph "(" =>
    stackUpdated?($reduceStack) := false
    repeatedSyntax('parseSexpr1,function PARSE_-Sexpr1)
    if parseGlyph "." then
      compulsorySyntax parseSexpr1()
      pushReduction('parseSexpr1,append!(popStack2(),popStack1()))
    if not stackUpdated? $reduceStack then
      pushReduction('parseSexpr1,nil)
    compulsorySyntax matchAdvanceGlyph ")"
  nil

parseSexpr() ==
  advanceToken()
  parseSexpr1()

parseData() ==
  SETQ(LABLASOC,nil)
  parseSexpr() and
    pushReduction('parseData,["QUOTE",TRANSLABEL(popStack1(),LABLASOC)])

parseCommand() ==
  matchAdvanceString '")" => --FIXME: remove matchAdvanceString
    compulsorySyntax parseSpecialKeyWord()
    compulsorySyntax parseSpecialCommand()
    pushReduction('parseStatement,nil)
  nil

parseTokenOption() ==
  matchAdvanceString '")" and compulsorySyntax PARSE_-TokenList()
  
parseQualification() ==
  matchAdvanceString '"$" =>
    compulsorySyntax parsePrimary1()
    pushReduction('parseQualification,dollarTran(popStack1(),popStack1()))
  nil

parseTokenTail() ==
  currentSymbol() is "$" and
    (alphabetic? currentChar() or currentChar() = char "$"
      or currentChar() = char "%" or currentChar() = char "(") =>
        tok := copyToken $priorToken
        parseQualification()
        $priorToken := tok
  nil

parseSelector() ==
  $nonblank and currentSymbol() is "." and currentChar() ~= char " "
    and matchAdvanceGlyph "." =>
      compulsorySyntax parsePrimaryNoFloat()
      pushReduction('parseSelector,[popStack2(),popStack1()])
  parseFloat()
    or matchAdvanceGlyph "." and compulsorySyntax parsePrimary() =>
      pushReduction('parseSelector,[popStack2(),popStack1()])
  nil

parseApplication() ==
  parsePrimary() =>
    repeatedSyntax('selectors,function parseSelector)
    parseApplication() and
      pushReduction('parseApplication,[popStack2(),popStack1()])
    true
  nil

parseOperation($ParseMode,rbp) ==
  matchCurrentToken 'IDENTIFIER => nil
  s := currentSymbol()
  not symbol? s or property(s,$ParseMode) = nil => nil
  rbp >= parseLeftBindingPowerOf(s,$ParseMode) => nil
  parseGetSemanticForm(s,$ParseMode,ELEMN(property(s,$ParseMode),5,nil))

parseLedPart rbp ==
  parseOperation('Led,rbp) and
    pushReduction('parseLedPart,popStack1())

parseNudPart rbp ==
  parseOperation('Nud,rbp) or parseReduction() or parseForm() =>
    pushReduction('parseNudPart,popStack1())

parseExpr rbp ==
  parseNudPart rbp =>
    repeatedSyntax('parseExpr,function(() +-> parseLedPart rbp))
    pushReduction('parseExpr,popStack1())
  nil

parseInfix() ==
  pushReduction('parseInfix,currentSymbol())
  advanceToken()
  parseTokenTail()
  compulsorySyntax parseExpression()
  pushReduction('parseInfix,[popStack2(),popStack2(),popStack1()])

parsePrefix() ==
  pushReduction('parsePrefix,currentSymbol())
  advanceToken()
  parseTokenTail()
  compulsorySyntax parseExpression()
  pushReduction('parsePrefix,[popStack2(),popStack1()])

parseLeftBindingPowerOf(x,p) ==
  y := property(x,p) => ELEMN(y,3,0)
  0

parseRightBindingPowerOf(x,p) ==
  y := property(x,p) => ELEMN(y,4,105)
  105

parseGetSemanticForm(x,p,y) ==
  z := EVAL y => z  -- FIXME get rid of EVAL.
  p = "Nud" => parsePrefix()
  p = "Led" => parseInfix()
  nil

parseExpression() ==
  parseExpr parseRightBindingPowerOf(makeSymbolOf $priorToken,$ParseMode)
    and pushReduction('parseExpression,popStack1())

parseSegmentTail() ==
  parseGlyph ".." =>
    seg :=     
      parseExpression() => ["SEGMENT",popStack2(),popStack1()]
      ["SEGMENT",popStack1()]
    pushReduction('parseSegmentTail,seg)
  nil

parseReductionOp() ==
  s := currentSymbol()
  if string? s then
    s := makeSymbol s -- FIXME: abolish string-quoted operators
  ident? s and property(s,'Led) and matchNextToken('GLIPH,"/") =>
    pushReduction('parseReductionOp,s)
    advanceToken()
    advanceToken()
    true
  false

parseReduction() ==
  parseReductionOp() =>
    compulsorySyntax parseExpr 1000
    pushReduction('parseReduction,["%Reduce",popStack2(),popStack1()])
  nil

parseCategory() ==
  matchAdvanceKeyword "if" =>
    compulsorySyntax parseExpression()
    compulsorySyntax matchAdvanceKeyword "then"
    compulsorySyntax parseCategory()
    stackUpdated?($reduceStack) := false
    matchAdvanceKeyword "else" and compulsorySyntax parseCategory()
    if not stackUpdated? $reduceStack then
      pushReduction('alternateCategory,nil)
    pushReduction('parseCategory,["if",popStack3(),popStack2(),popStack1()])
  matchAdvanceGlyph "(" =>
    compulsorySyntax parseCategory()
    stackUpdated?($reduceStack) := false
    repeatedSyntax('unnamedCategory,function(() +->
      matchAdvanceSpecial char ";" and compulsorySyntax parseCategory()))
    if not stackUpdated? $reduceStack then
      pushReduction('unnamedCategory,nil)
    compulsorySyntax matchAdvanceSpecial char ")"
    pushReduction('parseCategory,["CATEGORY",popStack2(),:popStack1()])
  matchAdvanceKeyword "assume" =>
    compulsorySyntax parseName()
    compulsorySyntax matchAdvanceGlyph "=="
    compulsorySyntax parseFormula()
    pushReduction('assumption,['ATTRIBUTE,['%Rule,popStack2(),popStack1()]])
  g := lineNumber $spadLine
  parseApplication() or parseOperatorFunctionName() =>
    matchAdvanceGlyph ":" =>
      compulsorySyntax parseExpression()
      pushReduction('parseCategory,["%Signature",popStack2(),popStack1()])
      recordSignatureDocumentation(nthStack 1,g)
      true
    pushReduction('parseCategory,["ATTRIBUTE",popStack1()])
    recordAttributeDocumentation(nthStack 1,g)
    true
  nil

parseWith() ==
  matchAdvanceKeyword "with" =>
    compulsorySyntax parseCategory()
    pushReduction('parseWith,["with",popStack1()])
  nil

parseInfixWith() ==
  parseWith() and
    pushReduction('parseInfixWith,["Join",popStack2(),popStack1()])

parseElseClause() ==
  currentSymbol() is "if" => parseConditional()
  parseExpression()

parseQuantifier() ==
  matchAdvanceKeyword "forall" =>
    pushReduction('parseQuantifier,'%Forall)
  matchAdvanceKeyword "exist" =>
    pushReduction('parseQuantifier,'%Exist)
  nil

parseQuantifiedVariable() ==
  parseName() =>
    compulsorySyntax matchAdvanceGlyph ":"
    compulsorySyntax parseApplication()
    pushReduction('parseQuantifiedVariable,[":",popStack2(),popStack1()])
  nil

parseQuantifiedVariableList() ==
  matchAdvanceGlyph "(" =>
    compulsorySyntax parseQuantifiedVariable()
    repeatedSyntax('repeatedVars,function(() +->
      matchAdvanceSpecial char "," and parseQuantifiedVariable()))
        and pushReduction('parseQuantifiedVariableList,
              ["%Sequence",popStack2(),:popStack1()])
    compulsorySyntax matchAdvanceSpecial char ")"
  nil

parseFormula() ==
  parseQuantifier() =>
    compulsorySyntax parseQuantifiedVariableList()
    compulsorySyntax matchAdvanceGlyph "."
    compulsorySyntax parseExpression()
    pushReduction('parseFormula,[popStack3(),popStack2(),popStack1()])
  parseExpression()

++ quantified types.  At the moment, these are used only in
++ pattern-mathing cases.
++ -- gdr, 2009-06-14.
parseScheme() ==
  parseQuantifier() =>
    compulsorySyntax parseQuantifiedVariableList()
    compulsorySyntax matchAdvanceGlyph "."
    compulsorySyntax parseExpr 200
    pushReduction('parseScheme,[popStack3(),popStack2(),popStack1()])
  parseApplication()

parseConditional() ==
  matchAdvanceKeyword "if" =>
    compulsorySyntax parseExpression()
    compulsorySyntax matchAdvanceKeyword "then"
    compulsorySyntax parseExpression()
    stackUpdated?($reduceStack) := false
    if matchAdvanceKeyword "else" then
      parseElseClause()
    if not stackUpdated? $reduceStack then
      pushReduction('elseBranch,nil)
    pushReduction('parseConditional,["if",popStack3(),popStack2(),popStack1()])
  nil

parseSemicolon() ==
  matchAdvanceSpecial char ";" =>
    parseExpr 82 or pushReduction('parseSemicolon,"/throwAway")
    pushReduction('parseSemicolon,[";",popStack2(),popStack1()])
  nil

++ We should factorize these boilerplates
parseReturn() ==
  matchAdvanceKeyword "return" =>
    compulsorySyntax parseExpression()
    pushReduction('parseReturn,["return",popStack1()])
  nil

parseThrow() ==
  matchAdvanceKeyword "throw" =>
    compulsorySyntax parseExpression()
    pushReduction('parseReturn,["%Throw",popStack1()])
  nil

parseExit() ==
  matchAdvanceKeyword "exit" =>
    x :=
      parseExpression() => popStack1()
      "$NoValue"
    pushReduction('parseExit,["exit",x])
  nil

parseLeave() ==
  matchAdvanceKeyword "leave" =>
    x :=
      parseExpression() => popStack1()
      "$NoValue"
    pushReduction('parseLeave,["leave",x])
  nil

parseJump() ==
  s := currentSymbol() =>
    advanceToken()
    pushReduction('parseJump,s)
  nil

parseForm() ==
  matchAdvanceKeyword "iterate" =>
    pushReduction('parseForm,["iterate"])
  matchAdvanceKeyword "yield" =>
    compulsorySyntax parseApplication()
    pushReduction('parseForm,["yield",popStack1()])
  parseApplication()

parseVariable() ==
  parseName() =>
    matchAdvanceGlyph ":" =>
      compulsorySyntax parseApplication()
      pushReduction('parseVariable,[":",popStack2(),popStack1()])
    true
  parsePrimary()

parseIterator() ==
  matchAdvanceKeyword "for" =>
    compulsorySyntax parseVariable()
    compulsorySyntax matchAdvanceKeyword "in"
    compulsorySyntax parseExpression()
    matchAdvanceKeyword "by" and compulsorySyntax parseExpr 200 and
      pushReduction('parseIterator,["INBY",popStack3(),popStack2(),popStack1()])
        or pushReduction('parseIterator,["IN",popStack2(),popStack1()])
    matchAdvanceGlyph "|" and compulsorySyntax parseExpr 111 and
      pushReduction('parseIterator,["|",popStack1()])
    true
  matchAdvanceKeyword "while" =>
    compulsorySyntax parseExpr 190
    pushReduction('parseIterator,["WHILE",popStack1()])
  matchAdvanceKeyword "until" =>
    compulsorySyntax parseExpr 190
    pushReduction('parseIterator,["UNTIL",popStack1()])
  nil

parseIteratorTails() ==
  matchAdvanceKeyword "repeat" =>
    stackUpdated?($reduceStack) := false
    repeatedSyntax('parseIteratorTails,function parseIterator)
    if not stackUpdated? $reduceStack then
      pushReduction('crossIterators,nil)
  repeatedSyntax('parseIteratorTails,function parseIterator)

parseLoop() ==
  repeatedSyntax('iterators,function parseIterator) =>
    compulsorySyntax matchAdvanceKeyword "repeat"
    compulsorySyntax parseExpr 110
    pushReduction('parseLoop,["REPEAT",:popStack2(),popStack1()])
  matchAdvanceKeyword "repeat" =>
    compulsorySyntax parseExpr 110
    pushReduction('parseLoop,["REPEAT",popStack1()])
  nil

parseOpenBracket() ==
  s := currentSymbol()
  s is "[" or s is ["elt",.,"["] =>
    do 
      s is ["elt",:.] =>
        pushReduction('parseOpenBracket,["elt",second s,"construct"])
      pushReduction('parseOpenBracket,"construct")
    advanceToken()
    true
  false

parseOpenBrace() ==
  s := currentSymbol()
  s is "{" or s is ["elt",.,"{"] =>
    do 
      s is ["elt",:.] =>
        pushReduction('parseOpenBracket,["elt",second s,"brace"])
      pushReduction('parseOpenBracket,"construct") --FIXME: should be brace
    advanceToken()
    true
  false

parseSequence1() ==
  do
    parseExpression() =>
      pushReduction('parseSequence1,[popStack2(),popStack1()])
    pushReduction('parseSequence1,[popStack1()])
  parseIteratorTails() and
    pushReduction('parseSequence1,["COLLECT",:popStack1(),popStack1()])
  true

parseSequence() ==
  parseOpenBracket() =>
    compulsorySyntax parseSequence1()
    compulsorySyntax matchAdvanceSpecial char "]"
  parseOpenBrace() =>
    compulsorySyntax parseSequence1()
    compulsorySyntax matchAdvanceSpecial char "}"
    pushReduction('parseSequence,["brace",popStack1()])
  nil
    
parseEnclosure() ==
  matchAdvanceGlyph "(" =>
    parseExpr 6 =>
      compulsorySyntax matchAdvanceSpecial char ")"
    matchAdvanceSpecial char ")" =>
      pushReduction('parseEnclosure,["%Comma"])
    SPAD__SYNTAX__ERROR()
  matchAdvanceGlyph "{" =>
    parseExpr 6 =>
      compulsorySyntax matchAdvanceSpecial char "}"
      pushReduction('parseEnclosure,["brace",["construct",popStack1()]])
    matchAdvanceSpecial char "}" =>
      pushReduction('parseEnclosure,["brace"])
    SPAD__SYNTAX__ERROR()
  matchAdvanceGlyph "[|" =>
    parseStatement() =>
      compulsorySyntax matchAdvanceGlyph "|]"
      pushReduction('parseEnclosure,["[||]",popStack1()])
    SPAD__SYNTAX__ERROR()
  nil

parseCatch() ==
  matchSpecial char ";" and matchKeywordNext "catch" =>
    advanceToken()
    advanceToken()
    compulsorySyntax parseGlyph "("
    compulsorySyntax parseQuantifiedVariable()
    compulsorySyntax matchAdvanceSpecial char ")"
    compulsorySyntax parseGlyph "=>"
    compulsorySyntax parseExpression()
    pushReduction('parseCatch,[popStack2(),popStack1()])
  nil

parseFinally() ==
  matchSpecial char ";" and matchKeywordNext "finally" =>
    advanceToken()
    advanceToken()
    compulsorySyntax parseExpression()
  nil

parseTry() ==
  matchAdvanceKeyword "try" =>
    compulsorySyntax parseExpression()
    -- exception handlers: either a finally-expression, or
    -- a series of catch-expressions optionally followed by
    -- a finally-expression.
    parseFinally() =>
      pushReduction('parseTry,["%Try",popStack2(),nil,popStack1()])
    compulsorySyntax repeatedSyntax('handlers,function parseCatch) =>
      stackUpdated?($reduceStack) := false
      parseFinally()
      if not stackUpdated? $reduceStack then
        pushReduction('finalizer,nil)
      pushReduction('parseTry,["%Try",popStack3(),popStack2(),popStack1()])
    SPAD__SYNTAX__ERROR()
  nil

parseMatch() ==
  matchAdvanceKeyword "case" =>
    compulsorySyntax parseExpr 400
    compulsorySyntax matchAdvanceKeyword "is"
    compulsorySyntax parseExpr 110
    pushReduction('parseMatch,["%Match",popStack2(),popStack1()])
  nil

++ domain inlining.  Same syntax as import directive; except
++ deliberate restriction on naming one type at a time.
++ -- gdr, 2009-02-28.
parseInline() ==
  matchAdvanceKeyword "inline" =>
    compulsorySyntax parseExpr 1000
    pushReduction('parseInline,["%Inline",popStack1()])
  nil

parseImport() ==
  matchAdvanceKeyword "import" =>
    compulsorySyntax parseExpr 1000
    matchAdvanceGlyph ":" =>
      compulsorySyntax parseExpression()
      compulsorySyntax matchAdvanceKeyword "from"
      compulsorySyntax parseExpr 1000
      pushReduction('parseImport,
        ["%SignatureImport",popStack3(),popStack2(),popStack1()])
    stackUpdated?($reduceStack) := false
    repeatedSyntax('imports,function(() +-> matchAdvanceSpecial char ","
      and compulsorySyntax parseExpr 1000))
    if not stackUpdated? $reduceStack then
      pushReduction('imports,nil)
    pushReduction('parseImport,["import",popStack2(),:popStack1()])
  nil

parseStatement() ==
  parseExpr 0 =>
    repeatedSyntax('statements,function(() +-> matchAdvanceGlyph ","
      and compulsorySyntax parseExpr 0)) =>
        pushReduction('parseStatement,["Series",popStack2(),:popStack1()])
    true
  false

parseNewExpr() ==
  matchString '")" =>
    processSynonyms()
    compulsorySyntax parseCommand()
  SETQ(DEFINITION__NAME,currentSymbol())
  parseStatement()

--%

isTokenDelimiter() ==
  symbolMember?(currentSymbol(),[")","END__UNIT","NIL"])

parseTokenList() ==
  repeatedSyntax('tokenList,function(() +->
    (isTokenDelimiter() => nil; pushReduction('parseTokenList,currentSymbol());
      advanceToken(); true)))

parseCommandTail() ==
  stackUpdated?($reduceStack) := false
  repeatedSyntax('options,function parseTokenOption)
  if not stackUpdated? $reduceStack then
    pushReduction('options,nil)
  atEndOfLine() and
    pushReduction('parseCommandTail,[popStack2(),:popStack1()])
  systemCommand popStack1()
  true

parseOption() ==
  matchAdvanceString '")" =>  --FIXME: kill matchAdvanceString
    compulsorySyntax repeatedSyntax('options,function parsePrimaryOrQM)

parseTokenCommandTail() ==  
  stackUpdated?($reduceStack) := false
  repeatedSyntax('options,function parseOption)
  if not stackUpdated? $reduceStack then
    pushReduction('options,nil)
  atEndOfLine() and
    pushReduction('parseCommandTail,[popStack2(),:popStack1()])
  systemCommand popStack1()
  true

parseSpecialCommand() ==
  matchAdvanceString '"show" =>   --FIXME: kill matchAdvanceString
    stackUpdated?($reduceStack) := true
    repeatedSyntax('commands,function(() +-> matchAdvanceString '"?"
      or parseExpression()))
    if not stackUpdated? $reduceStack then
      pushReduction('commdnds,nil)
    pushReduction('parseSpecialCommand,["show",popStack1()])
    compulsorySyntax parseCommandTail()
  symbolMember?(currentSymbol(),$noParseCommands) =>
    apply(currentSymbol(),nil)
    true
  symbolMember?(currentSymbol(),$tokenCommands) and parseTokenList() =>
    compulsorySyntax parseTokenCommandTail()
  repeatedSyntax('parseSpecialCommand,function parsePrimaryOrQM) and
    compulsorySyntax parseCommandTail()

--%

++ Given a pathname to a source file containing Spad code, returns
++ a list of (old) AST objects representing the toplevel expressions
++ in that file.
++ ??? system commands are still executed even if they may not be
++ ??? meaningful.  Eventually this code will go away when we 
++ ??? finally use the new parser everwhere.
parseSpadFile sourceFile ==
  $SPAD: local := true                   -- we are parsing Spad, 
  _*EOF_*: local := false                -- end of current input?
  FILE_-CLOSED : local := false          -- current stream closed?
  try
    -- noise to standard output  
    $OutputStream: local := MAKE_-SYNONYM_-STREAM "*STANDARD-OUTPUT*"
    -- we need to tell the post-parsing transformers that we're compiling
    -- Spad because few parse forms have slightly different representations
    -- depending on whether we are interpreter mode or compiler mode.
    $InteractiveMode: local := false
    INIT_-BOOT_/SPAD_-READER()
    -- we need to restore the global input stream state after we
    -- finished messing with it.
    IN_-STREAM: local := MAKE_-INSTREAM sourceFile

    -- If soureFile cannot be processed for whatever reasons
    -- get out of here instead of being stuck later.
    IN_-STREAM = nil =>
      systemError '"cannot open input source file"
    INITIALIZE_-PREPARSE IN_-STREAM

    -- gather parse trees for all toplevel expressions in sourceFile.
    asts := []                                   
    while not eof? IN_-STREAM repeat
      $lineStack: local := preparse IN_-STREAM
      $lineStack = nil => leave nil -- explicit end of input
      LINE: local := CDAR $lineStack
      CATCH('SPAD__READER,parseNewExpr())
      asts := [parseTransform postTransform popStack1(), :asts]
    -- we accumulated the parse trees in reverse order
    reverse! asts
  finally                      -- clean up the mess, and get out of here
    IOCLEAR(IN_-STREAM, OUT_-STREAM)             
    SHUT IN_-STREAM                              

--%

++ Gliphs are symbol clumps. The gliph property of a symbol gives
++ the tree describing the tokens which begin with that symbol.
++ The token reader uses the gliph property to determine the longest token.
++ Thus `:=' is read as one token not as `:' followed by `='.
for x in [
  ["|", [")"], ["]"]],_
  ["*", ["*"]],_
  ["(", ["|"]],_
  ["+", ["-", [">"]]],_
  ["-", [">"]],_
  ["<", ["="], ["<"]],
  ["/", ["\"]],_
  ["\", ["/"]],_
  [">", ["="], [">"]],_
  ["=", ["=", [">"]] ,[">"]],_
  [".", ["."]],_
  ["~", ["="]],_
  ["[", ["|"]],_
  [":", ["="], ["-"], [":"]]_
  ] repeat
     property(first x,'GLIPH) := rest x

++ Generic infix operators
for x in ["-", "=", "*", "rem", "mod", "quo", "div", "/", "^",
           "**", "exquo", "+", "-", "<", ">", "<=", ">=", "~=",
             "and", "or", "/\", "\/", "<<", ">>"] _
     repeat
       property(x,'GENERIC) := true
       
