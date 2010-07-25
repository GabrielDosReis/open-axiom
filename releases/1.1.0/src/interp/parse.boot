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


import '"postpar"
)package "BOOT"

--% Transformation of Parser Output

++ If non nil, holds the operator being being defined.
$defOp := nil

$oldParserExpandAbbrs := false
 
parseTransform x ==
  $defOp: local:= nil
  x := substitute('$,'%,x) -- for new compiler compatibility
  parseTran x

parseTran x ==
  $op: local
  atom x => parseAtom x
  [$op,:argl]:= x
  u := g($op) where g op == (op is ["elt",op,x] => g x; op)
  u="construct" =>
    r:= parseConstruct argl
    $op is ["elt",:.] => [parseTran $op,:rest r]
    r
  SYMBOLP u and (fn:= GETL(u,'parseTran)) => FUNCALL(fn,argl)
  [parseTran $op,:parseTranList argl]
 

parseAtom x ==
 -- next line for compatibility with new compiler
  x = "break" => parseLeave ["$NoValue"]
  x
 
parseTranList l ==
  atom l => parseTran l
  [parseTran first l,:parseTranList rest l]
 
parseConstruct u ==
  $insideConstructIfTrue: local:= true
  l:= parseTranList u
  ["construct",:l]
 
parseUpArrow u ==  parseTran ["**",:u]
 
parseLeftArrow u == parseTran ["LET",:u]
 
parseIs [a,b] == ["is",parseTran a,transIs parseTran b]
 
parseIsnt [a,b] == ["isnt",parseTran a,transIs parseTran b]
 
transIs u ==
  isListConstructor u => ["construct",:transIs1 u]
  u
 
isListConstructor u == u is [op,:.] and op in '(construct append cons)
 
transIs1 u ==
  u is ["construct",:l] => [transIs x for x in l]
  u is ["append",x,y] =>
    h:= [":",transIs x]
    (v:= transIs1 y) is [":",z] => [h,z]
    v="nil" => first rest h
    atom v => [h,[":",v]]
    [h,:v]
  u is ["cons",x,y] =>
    h:= transIs x
    (v:= transIs1 y) is [":",z] => [h,z]
    v="nil" => [h]
    atom v => [h,[":",v]]
    [h,:v]
  u
 
parseLET [x,y] ==
  p := ["LET",parseTran x,parseTranCheckForRecord(y,opOf x)]
  opOf x = "cons" => ["LET",transIs p.1,p.2]
  p
 
parseLETD [x,y] == ["LETD",parseTran x,parseTran parseType y]
 
parseColon u ==
  u is [x] => [":",parseTran x]
  u is [x,typ] =>
    $InteractiveMode =>
      $insideConstructIfTrue=true => ["TAG",parseTran x,parseTran typ]
      [":",parseTran x,parseTran parseType typ]
    [":",parseTran x,parseTran typ]
 
parseBigelt [typ,consForm] ==
  [["elt",typ,"makeRecord"],:transUnCons consForm]
 
transUnCons u ==
  atom u => systemErrorHere '"transUnCons"
  u is ["APPEND",x,y] =>
    null y => x
    systemErrorHere '"transUnCons"
  u is ["CONS",x,y] =>
    atom y => [x,:y]
    [x,:transUnCons y]
 
parseCoerce [x,typ] ==
  $InteractiveMode => ["::",parseTran x,parseTran parseType typ]
  ["::",parseTran x,parseTran typ]
 
parseAtSign [x,typ] ==
  $InteractiveMode => ["@",parseTran x,parseTran parseType typ]
  ["@",parseTran x,parseTran typ]
 
parsePretend [x,typ] ==
  $InteractiveMode => ["pretend",parseTran x,parseTran parseType typ]
  ["pretend",parseTran x,parseTran typ]
 
parseType x ==
  x := substitute($EmptyMode,$quadSymbol,x)
  x is ["typeOf",val] => ["typeOf",parseTran val]
  $oldParserExpandAbbrs => parseTypeEvaluate unabbrevAndLoad x
  x
 
parseTypeEvaluate form ==
  form is [op,:argl] =>
    newType? op => form
    $op: local:= op
    op = "Mapping" =>
      [op,:[parseTypeEvaluate a for a in argl]]
    op = "Union" =>
      isTaggedUnion form =>
        [op,:[['_:,sel,parseTypeEvaluate type] for
          ['_:,sel,type] in argl]]
      [op,:[parseTypeEvaluate a for a in argl]]
    op = 'Record =>
      [op,:[['_:,sel,parseTypeEvaluate type] for ['_:,sel,type] in argl]]
    cmm :=
      fn := constructor? op =>
        p := pathname [fn,$spadLibFT,'"*"] =>
          isExistingFile p => getConstructorModemap(abbreviation? fn)
          nil
      nil
    cmm is [[.,.,:argml],:.] => [op,:parseTypeEvaluateArgs(argl,argml)]
    throwKeyedMsg("S2IL0015",[op])
  form
 
parseTypeEvaluateArgs(argl,argml) ==
  [argVal for arg in argl for md in argml for i in 1..] where argVal() ==
      isCategoryForm(md,$CategoryFrame) => parseTypeEvaluate arg
      arg
 
 
parseTypeError(x,md,i) == throwKeyedMsg("S2IP0003",[i,$op,md])
 
specialModeTran form ==
  form is [op,:argl] =>
    not ATOM op => form --added 10/5/84 by SCM
    (s0:= (sop:= PNAME op).0) = "*" =>
      n:= #sop
      n=1=> form
      argKey:= sop.1
      numArgs:= #argl - (argKey="1" => 1; 0)
      zeroOrOne:= argKey="0" or argKey="1"
      isDmp :=
        numArgs < 10 =>
          n=6 and ('"DMP"=SUBSTRING(sop,3,3)) and zeroOrOne
        true =>
          n=7 and ('"DMP"=SUBSTRING(sop,4,3)) and zeroOrOne
      isDmp =>
        if argKey="0" then
          extraDomain:= $EmptyMode
          vl:= argl
         else
          [:vl,extraDomain] := argl
        ["DistributedMultivariatePolynomial",["construct",:vl],
            specialModeTran extraDomain]
      n=4 and (s3:= sop.3) = "M" and zeroOrOne =>
        specialModeTran
          extraDomain:= (argKey="0" => [$EmptyMode]; nil)
          (n:= PARSE_-INTEGER PNAME sop.2)=1 =>
            ["SquareMatrix",:argl,:extraDomain]
          n=2 => ["RectangularMatrix",:argl,:extraDomain]
          form
      isUpOrMp :=
        numArgs < 10 =>
          n=4 and (s3:= sop.3) = "P" and zeroOrOne or
            n=5 and (s3:= sop.3)="R" and sop.4="F" and zeroOrOne
        true =>
          n=5 and (s3:= sop.4) = "P" and zeroOrOne or
            n=6 and (s3:= sop.4)="R" and sop.5="F" and zeroOrOne
      isUpOrMp =>
        polyForm:=
          domainPart:= (argKey="0" => $EmptyMode; last argl)
          argPart:= (argKey="0" => argl; drop(-1,argl))
          numArgs < 10 and (n:= PARSE_-INTEGER PNAME sop.2)=1
            => ["UP",:argPart,domainPart]
          ["MP",["construct",:argPart],domainPart]
        specialModeTran
          s3 = "R" => [$QuotientField,polyForm]
          polyForm
      [first form,:[specialModeTran x for x in rest form]]
    [first form,:[specialModeTran x for x in rest form]]
  form
 
parseHas [x,y] ==
  if $InteractiveMode then
    x:=
      get(x,'value,$CategoryFrame) is [D,m,.]
        and m in '((Mode) (Domain) (SubDomain (Domain))) => D
      parseType x
  mkand [["has",x,u] for u in fn y] where
    mkand x ==
      x is [a] => a
      ["and",:x]
    fn y ==
      if $InteractiveMode then y:= unabbrevAndLoad y
      y is [":" ,op,["Mapping",:map]] =>
         op:= (STRINGP op => INTERN op; op)
         [["SIGNATURE",op,map]]
      y is ["Join",:u] => "append"/[fn z for z in u]
      y is ["CATEGORY",:u] => "append"/[fn z for z in u]
      kk:= GETDATABASE(opOf y,'CONSTRUCTORKIND)
      kk = "domain" or kk = "category" => [makeNonAtomic y]
      y is ["ATTRIBUTE",:.] => [y]
      y is ["SIGNATURE",:.] => [y]
      $InteractiveMode => parseHasRhs y
      [["ATTRIBUTE",y]]
 
parseHasRhs u ==   --$InteractiveMode = true
  get(u,'value,$CategoryFrame) is [D,m,.]
    and m in '((Mode) (Domain) (SubDomain (Domain))) => m
  y := abbreviation? u =>
    loadIfNecessary y => [unabbrevAndLoad y]
    [["ATTRIBUTE",u]]
  [["ATTRIBUTE",u]]
 
parseDEF [$lhs,tList,specialList,body] ==
  setDefOp $lhs
  ["DEF",parseLhs $lhs,parseTranList tList,parseTranList specialList,
    parseTranCheckForRecord(body,opOf $lhs)]
 
parseLhs x ==
  atom x => parseTran x
  atom first x => [parseTran first x,:[transIs parseTran y for y in rest x]]
  parseTran x
 
parseMDEF [$lhs,tList,specialList,body] ==
  ["MDEF",parseTran $lhs,parseTranList tList,parseTranList specialList,
    parseTranCheckForRecord(body,opOf $lhs)]
 
parseTranCheckForRecord(x,op) ==
  (x:= parseTran x) is ["Record",:l] =>
    or/[y for y in l | y isnt [":",.,.]] =>
      postError ['"   Constructor",:bright x,'"has missing label"]
    x
  x
 
parseCases [expr,ifClause] ==
  casefn(expr,ifClause) where
    casefn(x,ifExpr) ==
      ifExpr="%noBranch" => ["ifClauseError",x]
      ifExpr is ["IF",a,b,c] => ["IF",parseTran a,parseTran b,casefn(x,c)]
      postError ['"   CASES format error: cases ",x," of ",ifExpr]
 
parseCategory x ==
  l:= parseTranList parseDropAssertions x
  key:=
    CONTAINED("$",l) => "domain"
    "package"
  ["CATEGORY",key,:l]
 
parseDropAssertions x ==
--note: the COPY of this list is necessary-- do not replace by RPLACing version
  x is [y,:r] =>
    y is ["IF","asserted",:.] => parseDropAssertions r
    [y,:parseDropAssertions r]
  x
 
parseGreaterThan [x,y] ==
  [substitute("<",">",$op),parseTran y,parseTran x]
 
parseGreaterEqual u == parseTran ["not",[substitute("<",">=",$op),:u]]
 
parseLessEqual u == parseTran ["not",[substitute(">","<=",$op),:u]]
 
parseNotEqual u == parseTran ["not",[substitute("=","^=",$op),:u]]
 
parseDollarGreaterThan [x,y] ==
  [substitute("$<","$>",$op),parseTran y,parseTran x]
 
parseDollarGreaterEqual u ==
  parseTran ["not",[substitute("$<","$>=",$op),:u]]
 
parseDollarLessEqual u ==
  parseTran ["not",[substitute("$>","$<=",$op),:u]]
 
parseDollarNotEqual u ==
  parseTran ["not",[substitute("$=","$^=",$op),:u]]
 
parseAnd u ==
  $InteractiveMode => ["and",:parseTranList u]
  null u => "true"
  null rest u => first u
  parseIf [parseTran first u,parseAnd rest u,"false"]
 
parseOr u ==
  $InteractiveMode => ["or",:parseTranList u]
  null u => "false"
  null rest u => first u
  (x:= parseTran first u) is ["not",y] => parseIf [y,parseOr rest u,"true"]
  true => parseIf [x,"true",parseOr rest u]
 
parseEquivalence [a,b] == parseIf [a,b,parseIf [b,:'(false true)]]
 
parseImplies [a,b] == parseIf [a,b,"true"]
 
parseExclusiveOr [a,b] == parseIf [a,parseIf [b,:'(false true)],b]
 
parseExit [a,:b] ==
  --  note: I wanted to convert 1s to 0s here to facilitate indexing in
  --   comp code; unfortunately, parseTran-ning is sometimes done more
  --   than once so that the count can be decremented more than once
  a:= parseTran a
  b:= parseTran b
  b =>
    null INTEGERP a =>
      (MOAN('"first arg ",a,'" for exit must be integer"); ["exit",1,a])
    ["exit",a,:b]
  ["exit",1,a]
 
parseLeave [a,:b] ==
  a:= parseTran a
  b:= parseTran b
  b =>
    null INTEGERP a =>
      (MOAN('"first arg ",a,'" for 'leave' must be integer"); ["leave",1,a])
    ["leave",a,:b]
  ["leave",1,a]
 
parseReturn [a,:b] ==
  a:= parseTran a
  b:= parseTran b
  b =>
    (if a^=1 then MOAN '"multiple-level 'return' not allowed"; ["return",1,:b])
  ["return",1,a]
 
parseJoin l ==
  ["Join",:fn parseTranList l] where
    fn l ==
      null l => nil
      l is [["Join",:x],:y] => [:x,:fn y]
      [first l,:fn rest l]
 
parseInBy [i,n,inc] ==
  (u:= parseIn [i,n]) isnt ["STEP",i,a,j,:r] =>
    postError ["   You cannot use",:bright '"by",
      '"except for an explicitly indexed sequence."]
  inc:= parseTran inc
  ["STEP",i,a,parseTran inc,:r]
 
parseSegment p ==
  p is [a,b] =>
    b => ["SEGMENT",parseTran a, parseTran b]
    ["SEGMENT",parseTran a]
  ["SEGMENT",:p]
 
parseIn [i,n] ==
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
 
parseIf t ==
  t isnt [p,a,b] => t
  ifTran(parseTran p,parseTran a,parseTran b) where
    ifTran(p,a,b) ==
      null($InteractiveMode) and p="true"  => a
      null($InteractiveMode) and p="false"  => b
      p is ["not",p'] => ifTran(p',b,a)
      p is ["IF",p',a',b'] => ifTran(p',ifTran(a',COPY a,COPY b),ifTran(b',a,b))
      p is ["SEQ",:l,["exit",1,p']] =>
        ["SEQ",:l,["exit",1,ifTran(p',incExitLevel a,incExitLevel b)]]
         --this assumes that l has no exits
      a is ["IF", =p,a',.] => ["IF",p,a',b]
      b is ["IF", =p,.,b'] => ["IF",p,a,b']
      makeSimplePredicateOrNil p is ["SEQ",:s,["exit",1,val]] =>
        parseTran ["SEQ",:s,["exit",1,incExitLevel ["IF",val,a,b]]]
      ["IF",p,a,b]
 
makeSimplePredicateOrNil p ==
  isSimple p => nil
  u:= isAlmostSimple p => u
  true => wrapSEQExit [["LET",g:= GENSYM(),p],g]
 
parseWhere l == ["where",:mapInto(l, function parseTran)]
 
 
parseSeq l ==
  not l is [:.,["exit",:.]] =>
    postError ['"   Invalid ending to block: ",last l]
  transSeq mapInto(l,function parseTran)
 
transSeq l ==
  null l => nil
  null rest l => decExitLevel first l
  [item,:tail]:= l
  item is ["SEQ",:l,["exit",1,["IF",p,["exit", =2,q],"%noBranch"]]] and
    (and/[x is ["LET",:.] for x in l]) =>
      ["SEQ",:[decExitLevel x for x in l],["exit",1,["IF",decExitLevel p,
        decExitLevel q,transSeq tail]]]
  item is ["IF",a,["exit",1,b],"%noBranch"] =>
    ["IF",decExitLevel a,decExitLevel b,transSeq tail]
  item is ["IF",a,"%noBranch",["exit",1,b]] =>
    ["IF",decExitLevel a,transSeq tail,decExitLevel b]
  (y:= transSeq tail) is ["SEQ",:s] => ["SEQ",item,:s]
  ["SEQ",item,["exit",1,incExitLevel y]]
 
transCategoryItem x ==
  x is ["SIGNATURE",lhs,rhs] =>
    lhs is ["LISTOF",:y] =>
      "append" /[transCategoryItem ["SIGNATURE",z,rhs] for z in y]
    atom lhs =>
      if STRINGP lhs then lhs:= INTERN lhs
      rhs is ["Mapping",:m] =>
        m is [.,"constant"] => LIST ["SIGNATURE",lhs,[first m],"constant"]
        LIST ["SIGNATURE",lhs,m]
      $transCategoryAssoc:= [[lhs,:rhs],:$transCategoryAssoc]
      NIL
    [op,:argl]:= lhs
    extra:= nil
    if rhs is ["Mapping",:m] then
      if rest m then extra:= rest m
                 --should only be 'constant' or 'variable'
      rhs:= first m
    LIST ["SIGNATURE",op,[rhs,:SUBLIS($transCategoryAssoc,argl)],:extra]
  LIST x
 
superSub(name,x) ==
  for u in x repeat y:= [:y,:u]
  code:=
    x is [[u]] => $quadSymbol
    STRCONC("_(",scriptTranRow first x,scriptTran rest x,"_)")
  [INTERNL(PNAME name,"$",code),:y]
 
scriptTran x ==
  null x => ""
  STRCONC(";",scriptTranRow first x,scriptTran rest x)
 
scriptTranRow x ==
  null x => ""
  STRCONC($quadSymbol,scriptTranRow1 rest x)
 
scriptTranRow1 x ==
  null x => ""
  STRCONC(",",$quadSymbol,scriptTranRow1 rest x)
 
parseVCONS l == ["VECTOR",:parseTranList l]

--% Register special parsers.

for x in [["<=", :"parseLessEqual"],_
	  [">", :"parseGreaterThan"],_
	  [">=", :"parseGreaterEqual"],_
	  ["$<=", :"parseDollarLessEqual"],_
	  ["$>", :"parseDollarGreaterThan"],_
	  ["$>=", :"parseDollarGreaterEqual"],_
	  ["$^=", :"parseDollarNotEqual"],_
	  ["^=", :"parseNotEqual"],_
	  ["_:", :"parseColon"],_
	  ["_:_:", :"parseCoerce"],_
	  ["@", :"parseAtSign"],_
	  ["and", :"parseAnd"],_
	  ["CATEGORY", :"parseCategory"],_
	  ["construct", :"parseConstruct"],_
	  ["DEF", :"parseDEF"],_
	  ["eqv", :"parseEquivalence"],_
	  ["exit", :"parseExit"],_
	  ["has", :"parseHas"],_
	  ["IF", :"parseIf"],_
	  ["implies", :"parseImplies"],_ 
	  ["IN", :"parseIn"],_
	  ["INBY", :"parseInBy"],_
	  ["is", :"parseIs"],_
	  ["isnt", :"parseIsnt"],_
	  ["Join", :"parseJoin"],_
	  ["leave", :"parseLeave"],_
	  ["LET", :"parseLET"],_
	  ["LETD", :"parseLETD"],_
	  ["MDEF", :"parseMDEF"],_
	  ["or", :"parseOr"],_
	  ["pretend", :"parsePretend"],_
	  ["return", :"parseReturn"],_
	  ["SEGMENT", :"parseSegment"],_
	  ["SEQ", :"parseSeq"],_
	  ["VCONS", :"parseVCONS"],_
	  ["where", :"parseWhere"],_
          ["xor", :"parseExclusiveOr"]] repeat
  MAKEPROP(car x, "parseTran", cdr x)