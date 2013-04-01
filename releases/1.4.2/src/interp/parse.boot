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


import postpar
namespace BOOT

module parse

--% Transformation of Parser Output

++ If non nil, holds the operator being being defined.
$defOp := nil

++ When true, means that we are building a compile time value.  For
++ the parse tree transformer, this means that some assumtpions
++ are made about certain operators, regardless of their types
++ and semantics.  For example, in `a and b => x' the guard `a and b'
++ is assumed to have the standard semantics of (short-circuted)
++ conjunction of two Boolean expressions.
++ That usage is being phased out though.
$normalizeTree := false

++ True if we know we are parsing a form supposed to designate a type.
$parsingType := false

--%

washOperatorName x ==
  string? x =>
    stackWarning('"String syntax for %1b in signature is deprecated.",[x])
    makeSymbol x
  x

parseTransform: %ParseForm -> %Form 
parseTransform x ==
  $defOp: local:= nil
  parseTran substitute('$,'%,x) -- for new compiler compatibility

parseTran: %ParseForm -> %Form
parseTran x ==
  x isnt [.,:.] => x
  [op,:argl]:= x
  u := g(op) where g op == (op is ["elt",op,x] => g x; op)
  u="construct" =>
    r:= parseConstruct ["construct",:argl]
    op is ["elt",:.] => [parseTran op,:rest r]
    r
  symbol? u and (fn:= property(u,'parseTran)) => FUNCALL(fn,x)
  [parseTran op,:parseTranList argl]

parseType t ==
  $parsingType: local := true
  parseTran t

parseTypeList l ==
  l = nil => nil
  [parseType first l, :parseTypeList rest l]

parseTranList: %List %Form -> %List %Form
parseTranList l ==
  l isnt [.,:.] => parseTran l
  [parseTran first l,:parseTranList rest l]
 
parseConstruct: %ParseForm -> %Form
parseConstruct u ==
  $insideConstructIfTrue: local:= true
  [first u,:parseTranList rest u]

parseIs: %ParseForm -> %Form 
parseIs t == 
  t isnt ["is",a,b] => systemErrorHere ["parseIs",t]
  ["is",parseTran a,transIs parseTran b]
 
parseIsnt: %ParseForm -> %Form
parseIsnt t == 
  t isnt ["isnt",a,b] => systemErrorHere ["parseIsnt",t]
  ["isnt",parseTran a,transIs parseTran b]
 

transIs: %ParseForm -> %Form
transIs u ==
  isListConstructor u => ["construct",:transIs1 u]
  u

isListConstructor: %ParseForm -> %Boolean 
isListConstructor u == 
  u is [op,:.] and (not null (op in '(construct append cons)))

transIs1: %ParseForm -> %Form 
transIs1 u ==
  u is ["construct",:l] => [transIs x for x in l]
  u is ["append",x,y] =>
    h:= [":",transIs x]
    (v:= transIs1 y) is [":",z] => [h,z]
    v="nil" => second h
    v isnt [.,:.] => [h,[":",v]]
    [h,:v]
  u is ["cons",x,y] =>
    h:= transIs x
    (v:= transIs1 y) is [":",z] => [h,z]
    v="nil" => [h]
    v isnt [.,:.] => [h,[":",v]]
    [h,:v]
  u
 
parseAssign: %ParseForm -> %Form
parseAssign t ==
  t isnt [":=",x,y] => systemErrorHere ["parseAssign",t]
  p := [":=",parseTran x,parseTranCheckForRecord(y,opOf x)]
  opOf x = "cons" => [":=",transIs p.1,p.2]
  p
 

parseColon: %ParseForm -> %Form 
parseColon u ==
  u isnt [":",:.] => systemErrorHere ["parseColon",u]
  u is [":",x] => [":",parseTran x]
  u is [":",x,typ] => [":",parseTran x,parseType typ]
  u

-- ??? This parser is unused at the moment.
parseBigelt: %ParseForm -> %Form
parseBigelt t ==
  t isnt [.,typ,consForm] => systemErrorHere ["parseBigelt",t]
  [["elt",typ,"makeRecord"],:transUnCons consForm]

transUnCons: %ParseForm -> %Form 
transUnCons u ==
  u isnt [.,:.] => systemErrorHere ["transUnCons",u]
  u is ["APPEND",x,y] =>
    y = nil => x
    systemErrorHere ["transUnCons",u]
  u is ["CONS",x,y] =>
    y isnt [.,:.] => [x,:y]
    [x,:transUnCons y]

parseCoerce: %ParseForm -> %Form 
parseCoerce t ==
  t isnt [.,x,typ] => systemErrorHere ["parseCoerce",t]
  ["::",parseTran x,parseType typ]

parseAtSign: %ParseForm -> %Form 
parseAtSign t ==
  t isnt [.,x,typ] => systemErrorHere ["parseAtSign",t]
  ["@",parseTran x,parseType typ]
 

parsePretend: %ParseForm -> %Form
parsePretend t ==
  t isnt ["pretend",x,typ] => systemErrorHere ["parsePretend",t]
  ["pretend",parseTran x,parseType typ]
 
parseHas: %ParseForm -> %Form 
parseHas t ==
  t isnt ["has",x,y] => systemErrorHere ["parseHas",t]
  ["has",x,fn y] where
    fn y ==
      y is [":" ,op,["Mapping",:map]] =>
         ["SIGNATURE",washOperatorName op,map]
      y is ["Join",:u] => ["Join",:[fn z for z in u]]
      y is ["CATEGORY",kind,:u] => ["CATEGORY",kind,:[fn z for z in u]]
      kk:= getConstructorKindFromDB opOf y
      kk = "domain" or kk = "category" => makeNonAtomic y
      y is ["ATTRIBUTE",:.] => y
      y is ["SIGNATURE",:.] => y
      y is [":",op,type] => ["SIGNATURE",washOperatorName op,[type],"constant"]
      ["ATTRIBUTE",y]
 
parseDEF: %ParseForm -> %Form
parseDEF t ==
  t isnt ["DEF",$lhs,tList,body] => systemErrorHere ["parseDEF",t]
  setDefOp $lhs
  ["DEF",parseLhs $lhs,parseTypeList tList,
    parseTranCheckForRecord(body,opOf $lhs)]
 
parseLhs: %ParseForm -> %Form
parseLhs x ==
  x isnt [.,:.] => parseTran x
  first x isnt [.,:.] => [parseTran first x,:[transIs parseTran y for y in rest x]]
  parseTran x
 

parseMDEF: %ParseForm -> %Form
parseMDEF t ==
  t isnt ["MDEF",$lhs,tList,body] => systemErrorHere ["parseMDEF",t]
  ["MDEF",$lhs,parseTypeList tList,parseTranCheckForRecord(body,opOf $lhs)]

parseTranCheckForRecord: (%ParseForm,%ParseForm) -> %Form
parseTranCheckForRecord(x,op) ==
  x := parseTran x
  x is ["Record",:l] =>
    or/[y for y in l | y isnt [":",.,.]] =>
      postError ['"   Constructor",:bright x,'"has missing label"]
    x
  x
 
doParseCategory: %ParseForm -> %Form
doParseCategory t ==
  t isnt ["CATEGORY",:x] => systemErrorHere ["doParseCategory",t]
  $parsingType: local := true
  l := parseTranList x
  key :=
    CONTAINED("$",l) => 'domain
    'package
  ["CATEGORY",key,:l]
 
parseAnd: %ParseForm -> %Form 
parseAnd t ==
  t isnt ["and",:u] => systemErrorHere ["parseAnd",t]
  u = nil => "true"
  u is [a] => a
  parseIf ["IF",parseTran first u,parseAnd ["and",:rest u],"false"]
 

parseOr: %ParseForm -> %Form
parseOr t ==
  t isnt ["or",:u] => systemErrorHere ["parseOr",t]
  u = nil => "false"
  u is [a] => a
  (x:= parseTran first u) is ["not",y] => 
    parseIf ["IF",y,parseOr ["or",:rest u],"true"]
  parseIf ["IF",x,"true",parseOr ["or",:rest u]]
 
doParseExit: %ParseForm -> %Form
doParseExit t ==
  t isnt ["exit",a,:b] => systemErrorHere ["doParseExit",t]
  --  note: I wanted to convert 1s to 0s here to facilitate indexing in
  --   comp code; unfortunately, parseTran-ning is sometimes done more
  --   than once so that the count can be decremented more than once
  a:= parseTran a
  b:= parseTran b
  b =>
    not integer? a =>
      (MOAN('"first arg ",a,'" for exit must be integer"); ["exit",1,a])
    ["exit",a,:b]
  ["exit",1,a]
 

doParseLeave: %ParseForm -> %Form
doParseLeave t ==
  t isnt ["leave",a,:b] => systemErrorHere ["doParseLeave",t]
  a:= parseTran a
  b:= parseTran b
  b =>
    not integer? a =>
      (MOAN('"first arg ",a,'" for 'leave' must be integer"); ["leave",1,a])
    ["leave",a,:b]
  ["leave",1,a]
 

parseJoin: %ParseForm -> %Form
parseJoin t ==
  t isnt ["Join",:l] => systemErrorHere ["parseJoin",t]
  ["Join",:fn parseTypeList l] where
    fn l ==
      l = nil => nil
      l is [["Join",:x],:y] => [:x,:fn y]
      [first l,:fn rest l]

parseInBy: %ParseForm -> %Form
parseInBy t ==
  t isnt ["INBY",i,n,inc] => systemErrorHere ["parseInBy",t]
  (u:= parseIn ["IN",i,n]) isnt ["STEP",i,a,j,:r] =>
    postError ["   You cannot use",:bright '"by",
      '"except for an explicitly indexed sequence."]
  inc:= parseTran inc
  ["STEP",i,a,parseTran inc,:r]

parseSegment: %ParseForm -> %Form 
parseSegment p ==
  p is ["SEGMENT",a,:b] =>
    b => ["SEGMENT",parseTran a, parseTran first b]
    ["SEGMENT",parseTran a]
  -- SEGMENT is being elted from a domain
  ["SEGMENT",:rest p]
 
parseIn: %ParseForm -> %Form
parseIn t ==
  t isnt ["IN",i,n] => systemErrorHere ["parseIn",t]
  i:= parseTran i
  n:= parseTran n
  n is ["SEGMENT",a] => ["STEP",i,a,1]
  n is ["reverse",["SEGMENT",a]] =>
    postError ['"  You cannot reverse an infinite sequence."]
  n is ["SEGMENT",a,b] => (b => ["STEP",i,a,1,b]; ["STEP",i,a,1])
  n is ["reverse",["SEGMENT",a,b]] =>
    b => ["STEP",i,b,-1,a]
    postError ['"  You cannot reverse an infinite sequence."]
  n is ["tails",s] => ["ON",i,s]
  ["IN",i,n]
 
parseIf: %ParseForm -> %Form
parseIf t ==
  t isnt ["IF",p,a,b] => t
  ifTran(parseTran p,parseTran a,parseTran b) where
    ifTran(p,a,b) ==
      p="true"  => a
      p="false"  => b
      p is ["not",p'] => ifTran(p',b,a)
      p is ["IF",p',a',b'] => ifTran(p',ifTran(a',copyTree a,copyTree b),ifTran(b',a,b))
      p is ["SEQ",:l,["exit",1,p']] =>
        ["SEQ",:l,["exit",1,ifTran(p',incExitLevel a,incExitLevel b)]]
         --this assumes that l has no exits
      a is ["IF", =p,a',.] => ["IF",p,a',b]
      b is ["IF", =p,.,b'] => ["IF",p,a,b']
      not $parsingType and
        (makeSimplePredicateOrNil p is ["SEQ",:s,["exit",1,val]]) =>
           parseTran ["SEQ",:s,["exit",1,incExitLevel ["IF",val,a,b]]]
      ["IF",p,a,b]
 
makeSimplePredicateOrNil: %ParseForm -> %Form
makeSimplePredicateOrNil p ==
  isSimple p => nil
  u:= isAlmostSimple p => u
  wrapSEQExit [[":=",g:= gensym(),p],g]
 

parseWhere: %List %Form -> %Form
parseWhere t == 
  t isnt ["where",:l] => systemErrorHere ["parseWhere",t]
  ["where",:parseTranList l]
 
 
parseSeq: %List %Form -> %Form
parseSeq t ==
  t isnt ["SEQ",:l] => systemErrorHere ["parseSeq",t]
  l isnt [:.,["exit",:.]] =>
    postError ['"   Invalid ending to block: ",last l]
  transSeq parseTranList l
 

transSeq: %List %Form -> %Form
transSeq l ==
  l = nil => nil
  l is [x] => decExitLevel x
  [item,:tail] := l
  item is ["SEQ",:l,["exit",1,["IF",p,["exit", =2,q],"%noBranch"]]] and
    (and/[x is [":=",:.] for x in l]) =>
      ["SEQ",:[decExitLevel x for x in l],["exit",1,["IF",decExitLevel p,
        decExitLevel q,transSeq tail]]]
  item is ["IF",a,["exit",1,b],"%noBranch"] =>
    ["IF",decExitLevel a,decExitLevel b,transSeq tail]
  item is ["IF",a,"%noBranch",["exit",1,b]] =>
    ["IF",decExitLevel a,transSeq tail,decExitLevel b]
  item is ["IF",a,["exit",1,b],c] =>
    ["IF",decExitLevel a,decExitLevel b,transSeq [c,:tail]]
  (y:= transSeq tail) is ["SEQ",:s] => ["SEQ",item,:s]
  ["SEQ",item,["exit",1,incExitLevel y]]
 
superSub: (%Symbol, %List %Form ) -> %Form
superSub(name,x) ==
  for u in x repeat y:= [:y,:u]
  code:=
    x is [[u]] => $quadSymbol
    strconc('"_(",scriptTranRow first x,scriptTran rest x,'"_)")
  [makeSymbol strconc(symbolName name,"$",code),:y]
 
scriptTran: %List %Form -> %String
scriptTran x ==
  x = nil => '""
  strconc('";",scriptTranRow first x,scriptTran rest x)
 
scriptTranRow: %List %Form -> %String
scriptTranRow x ==
  x = nil => '""
  strconc($quadSymbol,scriptTranRow1 rest x)

scriptTranRow1: %List %Form -> %String 
scriptTranRow1 x ==
  x = nil => '""
  strconc('",",$quadSymbol,scriptTranRow1 rest x)
 
parseVCONS: %List %Form -> %Form
parseVCONS l == 
  ["VECTOR",:parseTranList rest l]

--% Register special parsers.

for x in [[":", :"parseColon"],_
	  ["::", :"parseCoerce"],_
	  ["@", :"parseAtSign"],_
	  ["and", :"parseAnd"],_
	  ["CATEGORY", :"doParseCategory"],_
	  ["construct", :"parseConstruct"],_
	  ["DEF", :"parseDEF"],_
	  ["exit", :"doParseExit"],_
	  ["has", :"parseHas"],_
	  ["IF", :"parseIf"],_
	  ["IN", :"parseIn"],_
	  ["INBY", :"parseInBy"],_
	  ["is", :"parseIs"],_
	  ["isnt", :"parseIsnt"],_
	  ["Join", :"parseJoin"],_
	  ["leave", :"doParseLeave"],_
	  [":=", :"parseAssign"],_
	  ["MDEF", :"parseMDEF"],_
	  ["or", :"parseOr"],_
	  ["pretend", :"parsePretend"],_
	  ["SEGMENT", :"parseSegment"],_
	  ["SEQ", :"parseSeq"],_
	  ["VCONS", :"parseVCONS"],_
	  ["where", :"parseWhere"]] repeat
  property(first x,'parseTran) := rest x
