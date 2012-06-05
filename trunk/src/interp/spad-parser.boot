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

skipIfBlock rs ==
  [n,:line] := z := preparseReadLine1 rs
  not string? line => z
  #line = 0 => skipIfBlock rs
  line.0 = char ")" =>
    stringPrefix?('")if",line) =>
      EVAL string2BootTree storeBlanks!(line,2) => preparseReadLine rs
      skipIfBlock rs
    stringPrefix?('")elseif",line) =>
      EVAL string2BootTree storeBlanks!(line,7) => preparseReadLine rs
      skipIfBlock rs
    stringPrefix?('")else",line) or stringPrefix?('")endif",line) =>
      preparseReadLine rs
    stringPrefix?('")fin",line) => [n,:%nothing]
    skipIfBlock rs
  skipIfBlock rs

skipToEndif rs ==
  [n,:line] := z := preparseReadLine1 rs
  not string? line => z
  stringPrefix?(line,'")endif") => preparseReadLine rs
  stringPrefix?(line,'")fin") => [n,:%nothing]
  skipToEndif rs

++ `n' is the line number of the current line
++ `oldnums' is the list of line numbers of previous lines
++ `oldlocs' is the list of previous indentation locations
++ `ncblock' is the current comment block
findCommentBlock(n,oldnums,oldlocs,ncblock) ==
  x :=
    [nc,:block] := ncblock
    nc = 0 => [n - 1,:reverse block]
    [or/[n for n in oldnums for l in oldlocs | integer? l and l <= nc],
       :reverse block]
  $COMBLOCKLIST := [x,:$COMBLOCKLIST]

preparseReadLine rs ==
  [n,:line] := z := preparseReadLine1 rs
  not string? line or #line = 0 => z
  line.0 = char ")" =>
    stringPrefix?('")if",line) =>
      EVAL string2BootTree storeBlanks!(line,3) => preparseReadLine rs
      skipIfBlock rs
    stringPrefix?('")elseif",line) or stringPrefix?('")else",line) =>
      skipToEndif rs
    stringPrefix?('")endif",line) => preparseReadLine rs
    stringPrefix?('")fin",line) => [n,:%nothing]
    z
  z

preparseReadLine1 rs ==
  if lines := readerLines rs then
    line := first lines
    readerLines(rs) := rest lines
  else
    line := expandLeadingTabs readLine readerInput rs
  $preparseLastLine := line
  not string? line => [$INDEX]
  $INDEX := $INDEX + 1
  line := trimTrailingBlank line
  n := $INDEX
  if #line > 0 and line.maxIndex(line) = char "__" then
    line := strconc(subString(line,0,maxIndex line),rest preparseReadLine1 rs)
    $preparseLastLine := line
  [n,:line]

preparseEcho lines ==
  if $Echo then
    for x in reverse lines repeat
      formatToStream(OUT_-STREAM,'"~&;~A~%",x)

++ The line to be worked on is the first in `lines.
++ It's indentation is the first in `locs'.
++ There is a notion of current indentation. Then:
++
++  A. Add open paren to beginning of following line if following 
++     line's indentation is greater than current, and add close paren
++     to end of last succeeding line with following line's indentation.
++  B. Add semicolon to end of line if following line's indentation is
++     the same.
++  C. If the entire line consists of the single keyword then or else, 
++     leave it alone.
addParensAndSemisToLine: (%List %String,%List %Maybe %Short) -> %Void
addParensAndSemisToLine(lines,locs) ==
  sc := first locs   -- first line column number
  sc = nil or sc <= 0 => nil
  count := 0         -- number of semicolons added
  z := lines
  for x in tails rest lines for y in tails rest locs repeat
    do
      nc := first y
      nc = nil => nil
      nc := abs nc
      nc < sc => leave nil
      nc = sc and (y.first := -nc) and not infixToken? first x =>
        z.first := addClose(first z,char ";")
        count := count + 1
    z := rest z
  count > 0 =>
    first(lines).(firstNonblankCharPosition first lines - 1) := char "("
    z.first := addClose(first z,char ")")
  nil

++ Add parens and semis to lines to aid parsing.  
parsePiles(locs,lines) ==
  for x in tails lines for y in tails locs repeat
    addParensAndSemisToLine(x,y)
  lines

parsePrint l ==
  $preparseReportIfTrue and l ~= nil =>
    formatToStdout '"~&~%       ***       PREPARSE      ***~%~%"
    for x in l repeat
      formatToStdout('"~5d. ~a~%",first x,rest x)
    formatToStdout '"~%"
  nil

preparse rd ==
  $COMBLOCKLIST := nil
  $SKIPME := false
  if $preparseLastLine ~= nil then
    readerDeferLine(rd,$preparseLastLine)
  $INDEX := $INDEX - #readerLines rd
  u := preparse1 rd
  $SKIPME => preparse rd
  parsePrint u
  $headerDocumentation := nil
  $docList := nil
  $maxSignatureLineNumber := 0
  $constructorLineNumber := IFCAR IFCAR u
  u
      

--%  
macro compulsorySyntax(rd,s) ==
  s or SPAD__SYNTAX__ERROR rd

repeatedSyntax(rd,l,p) ==
  n := stackSize $reduceStack
  once := false
  while apply(p,rd,nil) repeat
    once := true
  not once => nil
  x := nil
  for i in (n+1)..stackSize $reduceStack repeat
    x := [popStack1(),:x]
  x = nil => true
  pushReduction(l,x)

--%

parseToken(rd,tt) ==
  tok := matchCurrentToken(rd,tt) =>
    pushReduction(makeSymbol strconc(symbolName tt,'"Token"),tokenSymbol tok)
    advanceToken rd
    true
  false

parseGlyph(rd,s) ==
  matchCurrentToken(rd,'GLIPH,s) =>
    advanceToken rd
    true
  false

parseNBGlyph(rd,tok) ==
  matchCurrentToken(rd,'GLIPH,tok) and $nonblank =>
    advanceToken rd
    true
  false

parseString rd ==
  parseToken(rd,'SPADSTRING)

parseInteger rd ==
  parseToken(rd,'NUMBER)

parseFloatBasePart rd ==
  matchAdvanceGlyph(rd,".") =>
    $nonblank and (t := matchCurrentToken(rd,'NUMBER)) =>
      t := copyToken t
      advanceToken rd
      pushReduction('parseFloatBasePart,tokenNonblank? t)
      pushReduction('parseFloatBasePart,tokenSymbol t)
    pushReduction('parseFloatBasePart,0)
    pushReduction('parseFloatBasePart,0)
  nil

parseFloatBase rd ==
  integer? currentSymbol rd and currentChar rd = char "." and
    nextChar rd ~= char "." and parseInteger rd =>
      compulsorySyntax(rd,parseFloatBasePart rd)
  integer? currentSymbol rd and charUpcase currentChar rd = char "E"
    and parseInteger rd =>
      pushReduction('parseBase,0)
      pushReduction('parseBase,0)
  digit? currentChar rd and currentSymbol rd is "." =>
    pushReduction('parseBase,0)
    pushReduction('parseBase,0)
  nil

floatExponent x ==
  ident? x =>
    s := symbolName x
    charUpcase stringChar(s,0) = char "E" and #s > 1
      and (and/[DIGITP stringChar(s,i) for i in 1..maxIndex s]) =>
        READ_-FROM_-STRING(s,true,nil,KEYWORD::START,1)
    nil
  nil

parseFloatExponent rd ==
  not ident? currentSymbol rd => nil
  symbolMember?(currentSymbol rd,'(e E)) and
    charMember?(currentChar rd,[char "+",char "-"]) =>
      advanceToken rd
      parseInteger rd => true
      matchAdvanceGlyph(rd,"+") => compulsorySyntax(rd,parseInteger rd)
      matchAdvanceGlyph(rd,"-") =>
        compulsorySyntax(rd,parseInteger rd)
        pushReduction('parseFloatExponent,-popStack1())
      pushReduction('parseFloatExponent,0)
  g := floatExponent currentSymbol rd =>
    advanceToken rd
    pushReduction('parseFloatExponent,g)
  nil

parseFloat rd ==
  parseFloatBase rd =>
    $nonblank and parseFloatExponent rd
      or pushReduction('parseFloat,0)
    pushReduction('parseFloat,
      MAKE_-FLOAT(popStack4(),popStack2(),popStack2(),popStack1()))
  nil

parseName rd ==
  parseToken(rd,'IDENTIFIER) and pushReduction('parseName,popStack1())

parseKeyword rd ==
  parseToken(rd,'KEYWORD) and pushReduction('parseKeyword,popStack1())
  
parseFormalParameter rd ==
  parseToken(rd,'ARGUMENT_-DESIGNATOR)

parseOperatorFunctionName rd ==
  id := makeSymbolOf(matchCurrentToken(rd,'KEYWORD)
          or matchCurrentToken(rd,'GLIPH)
            or matchCurrentToken(rd,'SPECIAL_-CHAR))
  symbolMember?(id,$OperatorFunctionNames) =>
    pushReduction('parseOperatorFunctionName,id)
    advanceToken rd
    true
  false

parseAnyId rd ==
  parseName rd => true
  parseKeyword rd => true
  matchString(rd,'"$") =>
    pushReduction('parseAnyId,currentSymbol rd)
    advanceToken rd
    true
  parseOperatorFunctionName rd

parseQuad rd ==
  matchAdvanceString(rd,'"$") and pushReduction('parseQuad,"$")

parsePrimary1 rd ==
  parseName rd =>
    $nonblank and currentSymbol rd is "(" =>
      compulsorySyntax(rd,parsePrimary1 rd)
      pushReduction('parsePrimary1,[popStack2(),popStack1()])
    true
  parseQuad rd or parseString rd or parseInteger rd or
    parseFormalParameter rd => true
  matchSpecial(rd,char "'") =>
    compulsorySyntax(rd,parseData rd)
    pushReduction('parsePrimary1,popStack1())
  parseSequence rd or parseEnclosure rd

parsePrimaryNoFloat rd ==
  parsePrimary1 rd =>
    parseTokenTail rd or true
  false

parsePrimary rd ==
  parseFloat rd or parsePrimaryNoFloat rd

parsePrimaryOrQM rd ==
  matchAdvanceString(rd,'"?") => pushReduction('parsePrimaryOrQM,"?")
  parsePrimary rd

parseSpecialKeyWord rd ==
  matchCurrentToken(rd,'IDENTIFIER) =>
    tokenSymbol(currentToken rd) := unAbbreviateKeyword currentSymbol rd
  nil

parseSexpr1 rd ==
  parseInteger rd or parseString rd or parseAnyId rd => true
  matchAdvanceSpecial(rd,char "'") =>
    compulsorySyntax(rd,parseSexpr1 rd)
    pushReduction('parseSexpr1,["QUOTE",popStack1()])
  matchAdvanceGlyph(rd,"[") =>
    stackUpdated?($reduceStack) := false
    repeatedSyntax(rd,'parseSexpr1,function PARSE_-Sexpr1)
    if not stackUpdated? $reduceStack then
      pushReduction('parseSexpr1,nil)
    compulsorySyntax(rd,matchAdvanceGlyph(rd,"]"))
    pushReduction('parseSexpr1,LIST2VEC popStack1())
  matchAdvanceGlyph(rd,"(") =>
    stackUpdated?($reduceStack) := false
    repeatedSyntax(rd,'parseSexpr1,function PARSE_-Sexpr1)
    if parseGlyph(rd,".") then
      compulsorySyntax(rd,parseSexpr1 rd)
      pushReduction('parseSexpr1,append!(popStack2(),popStack1()))
    if not stackUpdated? $reduceStack then
      pushReduction('parseSexpr1,nil)
    compulsorySyntax(rd,matchAdvanceGlyph(rd,")"))
  nil

parseSexpr rd ==
  advanceToken rd
  parseSexpr1 rd

parseData rd ==
  parseSexpr rd and pushReduction('parseData,["QUOTE",popStack1()])

parseCommand rd ==
  matchAdvanceString(rd,'")") => --FIXME: remove matchAdvanceString
    compulsorySyntax(rd,parseSpecialKeyWord rd)
    compulsorySyntax(rd,parseSpecialCommand rd)
    pushReduction('parseStatement,nil)
  nil

parseTokenOption rd ==
  matchAdvanceString(rd,'")") and compulsorySyntax(rd,PARSE_-TokenList rd)
  
dollarTran(dom,x) ==
  x is [.,:.] => [['elt,dom,first x],:rest x]
  ['elt,dom,x]

parseQualification rd ==
  matchAdvanceString(rd,'"$") =>
    compulsorySyntax(rd,parsePrimary1 rd)
    pushReduction('parseQualification,dollarTran(popStack1(),popStack1()))
  nil

parseTokenTail rd ==
  currentSymbol rd is "$" and
    (alphabetic? currentChar rd or currentChar rd = char "$"
      or currentChar rd = char "%" or currentChar rd = char "(") =>
        tok := copyToken $priorToken
        parseQualification rd
        $priorToken := tok
  nil

parseSelector rd ==
  $nonblank and currentSymbol rd is "." and currentChar rd ~= char " "
    and matchAdvanceGlyph(rd,".") =>
      compulsorySyntax(rd,parsePrimaryNoFloat rd)
      pushReduction('parseSelector,[popStack2(),popStack1()])
  parseFloat rd
    or matchAdvanceGlyph(rd,".") and compulsorySyntax(rd,parsePrimary rd) =>
      pushReduction('parseSelector,[popStack2(),popStack1()])
  nil

parseApplication rd ==
  parsePrimary rd =>
    repeatedSyntax(rd,'selectors,function parseSelector)
    parseApplication rd and
      pushReduction('parseApplication,[popStack2(),popStack1()])
    true
  nil

parseOperation(rd,$ParseMode,rbp) ==
  matchCurrentToken(rd,'IDENTIFIER) => nil
  s := currentSymbol rd
  not symbol? s or property(s,$ParseMode) = nil => nil
  rbp >= parseLeftBindingPowerOf(s,$ParseMode) => nil
  parseGetSemanticForm(rd,s,$ParseMode,ELEMN(property(s,$ParseMode),5,nil))

parseLedPart(rd,rbp) ==
  parseOperation(rd,'Led,rbp) and
    pushReduction('parseLedPart,popStack1())

parseNudPart(rd,rbp) ==
  parseOperation(rd,'Nud,rbp) or parseReduction rd or parseForm rd =>
    pushReduction('parseNudPart,popStack1())

parseExpr(rd,rbp) ==
  parseNudPart(rd,rbp) =>
    repeatedSyntax(rd,'parseExpr,function(rd +-> parseLedPart(rd,rbp)))
    pushReduction('parseExpr,popStack1())
  nil

parseInfix rd ==
  pushReduction('parseInfix,currentSymbol rd)
  advanceToken rd
  parseTokenTail rd
  compulsorySyntax(rd,parseExpression rd)
  pushReduction('parseInfix,[popStack2(),popStack2(),popStack1()])

parsePrefix rd ==
  pushReduction('parsePrefix,currentSymbol rd)
  advanceToken rd
  parseTokenTail rd
  compulsorySyntax(rd,parseExpression rd)
  pushReduction('parsePrefix,[popStack2(),popStack1()])

parseLeftBindingPowerOf(x,p) ==
  y := property(x,p) => ELEMN(y,3,0)
  0

parseRightBindingPowerOf(x,p) ==
  y := property(x,p) => ELEMN(y,4,105)
  105

parseGetSemanticForm(rd,x,p,y) ==
  z :=
    ident? y => apply(y,rd,nil)
    EVAL y  -- FIXME get rid of EVAL.
  z ~= nil => z
  p = "Nud" => parsePrefix rd
  p = "Led" => parseInfix rd
  nil

parseExpression rd ==
  parseExpr(rd,parseRightBindingPowerOf(makeSymbolOf $priorToken,$ParseMode))
    and pushReduction('parseExpression,popStack1())

parseSegmentTail rd ==
  parseGlyph(rd,"..") =>
    seg :=     
      parseExpression rd => ["SEGMENT",popStack2(),popStack1()]
      ["SEGMENT",popStack1()]
    pushReduction('parseSegmentTail,seg)
  nil

parseReductionOp rd ==
  s := currentSymbol rd
  if string? s then
    s := makeSymbol s -- FIXME: abolish string-quoted operators
  ident? s and property(s,'Led) and matchNextToken(rd,'GLIPH,"/") =>
    pushReduction('parseReductionOp,s)
    advanceToken rd
    advanceToken rd
    true
  false

parseReduction rd ==
  parseReductionOp rd =>
    compulsorySyntax(rd,parseExpr(rd,1000))
    pushReduction('parseReduction,["%Reduce",popStack2(),popStack1()])
  nil

parseCategory rd ==
  matchAdvanceKeyword(rd,"if") =>
    compulsorySyntax(rd,parseExpression rd)
    compulsorySyntax(rd,matchAdvanceKeyword(rd,"then"))
    compulsorySyntax(rd,parseCategory rd)
    stackUpdated?($reduceStack) := false
    matchAdvanceKeyword(rd,"else") and compulsorySyntax(rd,parseCategory rd)
    if not stackUpdated? $reduceStack then
      pushReduction('alternateCategory,nil)
    pushReduction('parseCategory,["if",popStack3(),popStack2(),popStack1()])
  matchAdvanceGlyph(rd,"(") =>
    compulsorySyntax(rd,parseCategory rd)
    stackUpdated?($reduceStack) := false
    repeatedSyntax(rd,'unnamedCategory,function( rd +->
      matchAdvanceSpecial(rd,char ";") and compulsorySyntax(rd,parseCategory rd)))
    if not stackUpdated? $reduceStack then
      pushReduction('unnamedCategory,nil)
    compulsorySyntax(rd,matchAdvanceSpecial(rd,char ")"))
    pushReduction('parseCategory,["CATEGORY",popStack2(),:popStack1()])
  matchAdvanceKeyword(rd,"assume") =>
    compulsorySyntax(rd,parseName rd)
    compulsorySyntax(rd,matchAdvanceGlyph(rd,"=="))
    compulsorySyntax(rd,parseFormula rd)
    pushReduction('assumption,['ATTRIBUTE,['%Rule,popStack2(),popStack1()]])
  g := lineNumber readerSourceLine rd
  parseApplication rd or parseOperatorFunctionName rd =>
    matchAdvanceGlyph(rd,":") =>
      compulsorySyntax(rd,parseExpression rd)
      pushReduction('parseCategory,["%Signature",popStack2(),popStack1()])
      recordSignatureDocumentation(nthStack 1,g)
      true
    pushReduction('parseCategory,["ATTRIBUTE",popStack1()])
    recordAttributeDocumentation(nthStack 1,g)
    true
  nil

parseWith rd ==
  matchAdvanceKeyword(rd,"with") =>
    compulsorySyntax(rd,parseCategory rd)
    pushReduction('parseWith,["with",popStack1()])
  nil

parseInfixWith rd ==
  parseWith rd and
    pushReduction('parseInfixWith,["Join",popStack2(),popStack1()])

parseElseClause rd ==
  currentSymbol rd is "if" => parseConditional rd
  parseExpression rd

parseQuantifier rd ==
  matchAdvanceKeyword(rd,"forall") =>
    pushReduction('parseQuantifier,'%Forall)
  matchAdvanceKeyword(rd,"exist") =>
    pushReduction('parseQuantifier,'%Exist)
  nil

parseQuantifiedVariable rd ==
  parseName rd =>
    compulsorySyntax(rd,matchAdvanceGlyph(rd,":"))
    compulsorySyntax(rd,parseApplication rd)
    pushReduction('parseQuantifiedVariable,[":",popStack2(),popStack1()])
  nil

parseQuantifiedVariableList rd ==
  matchAdvanceGlyph(rd,"(") =>
    compulsorySyntax(rd,parseQuantifiedVariable rd)
    repeatedSyntax(rd,'repeatedVars,function(rd +->
      matchAdvanceSpecial(rd,char ",") and parseQuantifiedVariable rd))
        and pushReduction('parseQuantifiedVariableList,
              ["%Sequence",popStack2(),:popStack1()])
    compulsorySyntax(rd,matchAdvanceSpecial(rd,char ")"))
  nil

parseFormula rd ==
  parseQuantifier rd =>
    compulsorySyntax(rd,parseQuantifiedVariableList rd)
    compulsorySyntax(rd,matchAdvanceGlyph(rd,"."))
    compulsorySyntax(rd,parseExpression rd)
    pushReduction('parseFormula,[popStack3(),popStack2(),popStack1()])
  parseExpression rd

++ quantified types.  At the moment, these are used only in
++ pattern-mathing cases.
++ -- gdr, 2009-06-14.
parseScheme rd ==
  parseQuantifier rd =>
    compulsorySyntax(rd,parseQuantifiedVariableList rd)
    compulsorySyntax(rd,matchAdvanceGlyph(rd,"."))
    compulsorySyntax(rd,parseExpr(rd,200))
    pushReduction('parseScheme,[popStack3(),popStack2(),popStack1()])
  parseApplication rd

parseConditional rd ==
  matchAdvanceKeyword(rd,"if") =>
    compulsorySyntax(rd,parseExpression rd)
    compulsorySyntax(rd,matchAdvanceKeyword(rd,"then"))
    compulsorySyntax(rd,parseExpression rd)
    stackUpdated?($reduceStack) := false
    if matchAdvanceKeyword(rd,"else") then
      parseElseClause rd
    if not stackUpdated? $reduceStack then
      pushReduction('elseBranch,nil)
    pushReduction('parseConditional,["if",popStack3(),popStack2(),popStack1()])
  nil

parseSemicolon rd ==
  matchAdvanceSpecial(rd,char ";") =>
    parseExpr(rd,82) or pushReduction('parseSemicolon,"/throwAway")
    pushReduction('parseSemicolon,[";",popStack2(),popStack1()])
  nil

++ We should factorize these boilerplates
parseReturn rd ==
  matchAdvanceKeyword(rd,"return") =>
    compulsorySyntax(rd,parseExpression rd)
    pushReduction('parseReturn,["return",popStack1()])
  nil

parseThrow rd ==
  matchAdvanceKeyword(rd,"throw") =>
    compulsorySyntax(rd,parseExpression rd)
    pushReduction('parseReturn,["%Throw",popStack1()])
  nil

parseExit rd ==
  matchAdvanceKeyword(rd,"exit") =>
    x :=
      parseExpression rd => popStack1()
      "$NoValue"
    pushReduction('parseExit,["exit",x])
  nil

parseLeave rd ==
  matchAdvanceKeyword(rd,"leave") =>
    x :=
      parseExpression rd => popStack1()
      "$NoValue"
    pushReduction('parseLeave,["leave",x])
  nil

parseJump rd ==
  s := currentSymbol rd =>
    advanceToken rd
    pushReduction('parseJump,s)
  nil

parseForm rd ==
  matchAdvanceKeyword(rd,"iterate") =>
    pushReduction('parseForm,["iterate"])
  matchAdvanceKeyword(rd,"yield") =>
    compulsorySyntax(rd,parseApplication rd)
    pushReduction('parseForm,["yield",popStack1()])
  parseApplication rd

parseVariable rd ==
  parseName rd =>
    matchAdvanceGlyph(rd,":") =>
      compulsorySyntax(rd,parseApplication rd)
      pushReduction('parseVariable,[":",popStack2(),popStack1()])
    true
  parsePrimary rd

parseIterator rd ==
  matchAdvanceKeyword(rd,"for") =>
    compulsorySyntax(rd,parseVariable rd)
    compulsorySyntax(rd,matchAdvanceKeyword(rd,"in"))
    compulsorySyntax(rd,parseExpression rd)
    matchAdvanceKeyword(rd,"by") and compulsorySyntax(rd,parseExpr(rd,200)) and
      pushReduction('parseIterator,["INBY",popStack3(),popStack2(),popStack1()])
        or pushReduction('parseIterator,["IN",popStack2(),popStack1()])
    matchAdvanceGlyph(rd,"|") and compulsorySyntax(rd,parseExpr(rd,111)) and
      pushReduction('parseIterator,["|",popStack1()])
    true
  matchAdvanceKeyword(rd,"while") =>
    compulsorySyntax(rd,parseExpr(rd,190))
    pushReduction('parseIterator,["WHILE",popStack1()])
  matchAdvanceKeyword(rd,"until") =>
    compulsorySyntax(rd,parseExpr(rd,190))
    pushReduction('parseIterator,["UNTIL",popStack1()])
  nil

parseIteratorTails rd ==
  matchAdvanceKeyword(rd,"repeat") =>
    stackUpdated?($reduceStack) := false
    repeatedSyntax(rd,'parseIteratorTails,function parseIterator)
    if not stackUpdated? $reduceStack then
      pushReduction('crossIterators,nil)
  repeatedSyntax(rd,'parseIteratorTails,function parseIterator)

parseLoop rd ==
  repeatedSyntax(rd,'iterators,function parseIterator) =>
    compulsorySyntax(rd,matchAdvanceKeyword(rd,"repeat"))
    compulsorySyntax(rd,parseExpr(rd,110))
    pushReduction('parseLoop,["REPEAT",:popStack2(),popStack1()])
  matchAdvanceKeyword(rd,"repeat") =>
    compulsorySyntax(rd,parseExpr(rd,110))
    pushReduction('parseLoop,["REPEAT",popStack1()])
  nil

parseOpenBracket rd ==
  s := currentSymbol rd
  s is "[" or s is ["elt",.,"["] =>
    do 
      s is ["elt",:.] =>
        pushReduction('parseOpenBracket,["elt",second s,"construct"])
      pushReduction('parseOpenBracket,"construct")
    advanceToken rd
    true
  false

parseOpenBrace rd ==
  s := currentSymbol rd
  s is "{" or s is ["elt",.,"{"] =>
    do 
      s is ["elt",:.] =>
        pushReduction('parseOpenBracket,["elt",second s,"brace"])
      pushReduction('parseOpenBracket,"construct") --FIXME: should be brace
    advanceToken rd
    true
  false

parseSequence1 rd ==
  do
    parseExpression rd =>
      pushReduction('parseSequence1,[popStack2(),popStack1()])
    pushReduction('parseSequence1,[popStack1()])
  parseIteratorTails rd and
    pushReduction('parseSequence1,["COLLECT",:popStack1(),popStack1()])
  true

parseSequence rd ==
  parseOpenBracket rd =>
    compulsorySyntax(rd,parseSequence1 rd)
    compulsorySyntax(rd,matchAdvanceSpecial(rd,char "]"))
  parseOpenBrace rd =>
    compulsorySyntax(rd,parseSequence1 rd)
    compulsorySyntax(rd,matchAdvanceSpecial(rd,char "}"))
    pushReduction('parseSequence,["brace",popStack1()])
  nil
    
parseEnclosure rd ==
  matchAdvanceGlyph(rd,"(") =>
    parseExpr(rd,6) =>
      compulsorySyntax(rd,matchAdvanceSpecial(rd,char ")"))
    matchAdvanceSpecial(rd,char ")") =>
      pushReduction('parseEnclosure,["%Comma"])
    SPAD__SYNTAX__ERROR rd
  matchAdvanceGlyph(rd,"{") =>
    parseExpr(rd,6) =>
      compulsorySyntax(rd,matchAdvanceSpecial(rd,char "}"))
      pushReduction('parseEnclosure,["brace",["construct",popStack1()]])
    matchAdvanceSpecial(rd,char "}") =>
      pushReduction('parseEnclosure,["brace"])
    SPAD__SYNTAX__ERROR rd
  matchAdvanceGlyph(rd,"[|") =>
    parseStatement rd =>
      compulsorySyntax(rd,matchAdvanceGlyph(rd,"|]"))
      pushReduction('parseEnclosure,["[||]",popStack1()])
    SPAD__SYNTAX__ERROR rd
  nil

parseCatch rd ==
  matchSpecial(rd,char ";") and matchKeywordNext(rd,"catch") =>
    advanceToken rd
    advanceToken rd
    compulsorySyntax(rd,parseGlyph(rd,"("))
    compulsorySyntax(rd,parseQuantifiedVariable rd)
    compulsorySyntax(rd,matchAdvanceSpecial(rd,char ")"))
    compulsorySyntax(rd,parseGlyph(rd,"=>"))
    compulsorySyntax(rd,parseExpression rd)
    pushReduction('parseCatch,[popStack2(),popStack1()])
  nil

parseFinally rd ==
  matchSpecial(rd,char ";") and matchKeywordNext(rd,"finally") =>
    advanceToken rd
    advanceToken rd
    compulsorySyntax(rd,parseExpression rd)
  nil

parseTry rd ==
  matchAdvanceKeyword(rd,"try") =>
    compulsorySyntax(rd,parseExpression rd)
    -- exception handlers: either a finally-expression, or
    -- a series of catch-expressions optionally followed by
    -- a finally-expression.
    parseFinally rd =>
      pushReduction('parseTry,["%Try",popStack2(),nil,popStack1()])
    compulsorySyntax(rd,repeatedSyntax(rd,'handlers,function parseCatch)) =>
      stackUpdated?($reduceStack) := false
      parseFinally rd
      if not stackUpdated? $reduceStack then
        pushReduction('finalizer,nil)
      pushReduction('parseTry,["%Try",popStack3(),popStack2(),popStack1()])
    SPAD__SYNTAX__ERROR rd
  nil

parseMatch rd ==
  matchAdvanceKeyword(rd,"case") =>
    compulsorySyntax(rd,parseExpr(rd,400))
    compulsorySyntax(rd,matchAdvanceKeyword(rd,"is"))
    compulsorySyntax(rd,parseExpr(rd,110))
    pushReduction('parseMatch,["%Match",popStack2(),popStack1()])
  nil

++ domain inlining.  Same syntax as import directive; except
++ deliberate restriction on naming one type at a time.
++ -- gdr, 2009-02-28.
parseInline rd ==
  matchAdvanceKeyword(rd,"inline") =>
    compulsorySyntax(rd,parseExpr(rd,1000))
    pushReduction('parseInline,["%Inline",popStack1()])
  nil

parseImport rd ==
  matchAdvanceKeyword(rd,"import") =>
    compulsorySyntax(rd,parseExpr(rd,1000))
    matchAdvanceGlyph(rd,":") =>
      compulsorySyntax(rd,parseExpression rd)
      compulsorySyntax(rd,matchAdvanceKeyword(rd,"from"))
      compulsorySyntax(rd,parseExpr(rd,1000))
      pushReduction('parseImport,
        ["%SignatureImport",popStack3(),popStack2(),popStack1()])
    stackUpdated?($reduceStack) := false
    repeatedSyntax(rd,'imports,function(rd +-> matchAdvanceSpecial(rd,char ",")
      and compulsorySyntax(rd,parseExpr(rd,1000))))
    if not stackUpdated? $reduceStack then
      pushReduction('imports,nil)
    pushReduction('parseImport,["import",popStack2(),:popStack1()])
  nil

parseStatement rd ==
  parseExpr(rd,0) =>
    repeatedSyntax(rd,'statements,function(rd +-> matchAdvanceGlyph(rd,",")
      and compulsorySyntax(rd,parseExpr(rd,0)))) =>
        pushReduction('parseStatement,["Series",popStack2(),:popStack1()])
    true
  false

parseNewExpr rd ==
  matchString(rd,'")") =>
    processSynonyms()
    compulsorySyntax(rd,parseCommand rd)
  SETQ(DEFINITION__NAME,currentSymbol rd)
  parseStatement rd

--%

isTokenDelimiter rd ==
  symbolMember?(currentSymbol rd,[")","END__UNIT","NIL"])

parseTokenList rd ==
  repeatedSyntax(rd,'tokenList,function(rd +->
    (isTokenDelimiter rd => nil; pushReduction('parseTokenList,currentSymbol rd);
      advanceToken rd; true)))

parseCommandTail rd ==
  stackUpdated?($reduceStack) := false
  repeatedSyntax(rd,'options,function parseTokenOption)
  if not stackUpdated? $reduceStack then
    pushReduction('options,nil)
  atEndOfLine rd and
    pushReduction('parseCommandTail,[popStack2(),:popStack1()])
  systemCommand popStack1()
  true

parseOption rd ==
  matchAdvanceString(rd,'")") =>  --FIXME: kill matchAdvanceString
    compulsorySyntax(rd,repeatedSyntax(rd,'options,function parsePrimaryOrQM))

parseTokenCommandTail rd ==  
  stackUpdated?($reduceStack) := false
  repeatedSyntax(rd,'options,function parseOption)
  if not stackUpdated? $reduceStack then
    pushReduction('options,nil)
  atEndOfLine rd and
    pushReduction('parseCommandTail,[popStack2(),:popStack1()])
  systemCommand popStack1()
  true

parseSpecialCommand rd ==
  matchAdvanceString(rd,'"show") =>   --FIXME: kill matchAdvanceString
    stackUpdated?($reduceStack) := true
    repeatedSyntax(rd,'commands,function(rd +-> matchAdvanceString(rd,'"?")
      or parseExpression rd))
    if not stackUpdated? $reduceStack then
      pushReduction('commdnds,nil)
    pushReduction('parseSpecialCommand,["show",popStack1()])
    compulsorySyntax(rd,parseCommandTail rd)
  symbolMember?(currentSymbol rd,$noParseCommands) =>
    apply(currentSymbol rd,nil)
    true
  symbolMember?(currentSymbol rd,$tokenCommands) and parseTokenList rd =>
    compulsorySyntax(rd,parseTokenCommandTail rd)
  repeatedSyntax(rd,'parseSpecialCommand,function parsePrimaryOrQM) and
    compulsorySyntax(rd,parseCommandTail rd)

--%

translateSpad x ==
  $Index: local := 0
  _*PRETTY_-PRINT_*: local := true
  $MACROASSOC: local := nil
  $NEWSPAD: local := true
  $currentFunction: local := nil
  $topOp: local := nil
  $semanticErrorStack: local := []
  $warningStack: local := []
  $returnMode: local := $EmptyMode
  $leaveLevelStack: local := []
  $insideFunctorIfTrue: local := false
  $insideExpressionIfTrue: local := false
  $insideCoerceInteractiveHardIfTrue: local := false
  $insideWhereIfTrue: local := false
  $insideCategoryIfTrue: local := false
  $insideCapsuleFunctionIfTrue: local := false
  $form: local := nil
  $e: local := $EmptyEnvironment
  $genSDVar: local := 0
  $previousTime: local := TEMPUS_-FUGIT()
  compileParseTree x  

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
    -- we need to restore the global input stream state after we
    -- finished messing with it.
    IN_-STREAM: local := MAKE_-INSTREAM sourceFile
    rd := makeReader IN_-STREAM
    INIT_-BOOT_/SPAD_-READER rd

    -- If soureFile cannot be processed for whatever reasons
    -- get out of here instead of being stuck later.
    readerInput rd = nil => systemError '"cannot open input source file"
    INITIALIZE_-PREPARSE rd

    -- gather parse trees for all toplevel expressions in sourceFile.
    asts := []                                   
    while not eof? readerInput rd repeat
      $lineStack: local := preparse rd
      $lineStack = nil => leave nil -- explicit end of input
      LINE: local := CDAR $lineStack
      CATCH($SpadReaderTag,parseNewExpr rd)
      asts := [parseTransform postTransform popStack1(), :asts]
    -- we accumulated the parse trees in reverse order
    reverse! asts
  finally                      -- clean up the mess, and get out of here
    ioClear! rd
    SHUT readerInput rd

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
       
