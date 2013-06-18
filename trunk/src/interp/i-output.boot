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


import sys_-utility
import sys_-macros
namespace BOOT

--Modified JHD February 1993: see files miscout.input for some tests of this
-- General principle is that maprin0 is the top-level routine,
-- which calls maprinChk to print the object (placing certain large
-- matrices on a look-aside list), then calls maprinRows to print these.
-- These prints call maprinChk recursively, and maprinChk has to ensure that
-- we do not end up in an infinite recursion: matrix1 = matrix2 ...

--% Output display routines

$defaultSpecialCharacters == [
    abstractChar( 28),      -- upper left corner
    abstractChar( 27),      -- upper right corner
    abstractChar( 30),      -- lower left corner
    abstractChar( 31),      -- lower right corner
    abstractChar( 79),      -- vertical bar
    abstractChar( 45),      -- horizontal bar
    abstractChar(144),      -- APL quad
    abstractChar(173),      -- left bracket
    abstractChar(189),      -- right bracket
    abstractChar(192),      -- left brace
    abstractChar(208),      -- right brace
    abstractChar( 59),      -- top    box tee
    abstractChar( 62),      -- bottom box tee
    abstractChar( 63),      -- right  box tee
    abstractChar( 61),      -- left   box tee
    abstractChar( 44),      -- center box tee
    abstractChar(224)       -- back slash
     ]

$plainSpecialCharacters0 == [
    abstractChar( 78),      -- upper left corner   (+)
    abstractChar( 78),      -- upper right corner  (+)
    abstractChar( 78),      -- lower left corner   (+)
    abstractChar( 78),      -- lower right corner  (+)
    abstractChar( 79),      -- vertical bar
    abstractChar( 96),      -- horizontal bar      (-)
    abstractChar(111),      -- APL quad            (?)
    abstractChar(173),      -- left bracket
    abstractChar(189),      -- right bracket
    abstractChar(192),      -- left brace
    abstractChar(208),      -- right brace
    abstractChar( 78),      -- top    box tee      (+)
    abstractChar( 78),      -- bottom box tee      (+)
    abstractChar( 78),      -- right  box tee      (+)
    abstractChar( 78),      -- left   box tee      (+)
    abstractChar( 78),      -- center box tee      (+)
    abstractChar(224)       -- back slash
     ]

$plainSpecialCharacters1 == [
    abstractChar(107),      -- upper left corner   (,)
    abstractChar(107),      -- upper right corner  (,)
    abstractChar(125),      -- lower left corner   (')
    abstractChar(125),      -- lower right corner  (')
    abstractChar( 79),      -- vertical bar
    abstractChar( 96),      -- horizontal bar      (-)
    abstractChar(111),      -- APL quad            (?)
    abstractChar(173),      -- left bracket
    abstractChar(189),      -- right bracket
    abstractChar(192),      -- left brace
    abstractChar(208),      -- right brace
    abstractChar( 78),      -- top    box tee      (+)
    abstractChar( 78),      -- bottom box tee      (+)
    abstractChar( 78),      -- right  box tee      (+)
    abstractChar( 78),      -- left   box tee      (+)
    abstractChar( 78),      -- center box tee      (+)
    abstractChar(224)       -- back slash
     ]

$plainSpecialCharacters2 == [
    abstractChar( 79),      -- upper left corner   (|)
    abstractChar( 79),      -- upper right corner  (|)
    abstractChar( 79),      -- lower left corner   (|)
    abstractChar( 79),      -- lower right corner  (|)
    abstractChar( 79),      -- vertical bar
    abstractChar( 96),      -- horizontal bar      (-)
    abstractChar(111),      -- APL quad            (?)
    abstractChar(173),      -- left bracket
    abstractChar(189),      -- right bracket
    abstractChar(192),      -- left brace
    abstractChar(208),      -- right brace
    abstractChar( 78),      -- top    box tee      (+)
    abstractChar( 78),      -- bottom box tee      (+)
    abstractChar( 78),      -- right  box tee      (+)
    abstractChar( 78),      -- left   box tee      (+)
    abstractChar( 78),      -- center box tee      (+)
    abstractChar(224)       -- back slash
     ]

$plainSpecialCharacters3 == [
    abstractChar( 96),      -- upper left corner   (-)
    abstractChar( 96),      -- upper right corner  (-)
    abstractChar( 96),      -- lower left corner   (-)
    abstractChar( 96),      -- lower right corner  (-)
    abstractChar( 79),      -- vertical bar
    abstractChar( 96),      -- horizontal bar      (-)
    abstractChar(111),      -- APL quad            (?)
    abstractChar(173),      -- left bracket
    abstractChar(189),      -- right bracket
    abstractChar(192),      -- left brace
    abstractChar(208),      -- right brace
    abstractChar( 78),      -- top    box tee      (+)
    abstractChar( 78),      -- bottom box tee      (+)
    abstractChar( 78),      -- right  box tee      (+)
    abstractChar( 78),      -- left   box tee      (+)
    abstractChar( 78),      -- center box tee      (+)
    abstractChar(224)       -- back slash
     ]

$plainRTspecialCharacters == [
    '_+,      -- upper left corner   (+)
    '_+,      -- upper right corner  (+)
    '_+,      -- lower left corner   (+)
    '_+,      -- lower right corner  (+)
    '_|,      -- vertical bar
    '_-,      -- horizontal bar      (-)
    '_?,      -- APL quad            (?)
    '_[,      -- left bracket
    '_],      -- right bracket
    '_{,      -- left brace
    '_},      -- right brace
    '_+,      -- top    box tee      (+)
    '_+,      -- bottom box tee      (+)
    '_+,      -- right  box tee      (+)
    '_+,      -- left   box tee      (+)
    '_+,      -- center box tee      (+)
    '_\       -- back slash
     ]

++ End of Transmission character; usually to the Algebra Output
++ Stream in lean mode.
$RecordSeparator == abstractChar 30

macro makeCharacter n ==
  makeSymbol(charString abstractChar n)

$RTspecialCharacters == [
    makeCharacter 218,      -- upper left corner   (+)
    makeCharacter 191,      -- upper right corner  (+)
    makeCharacter 192,      -- lower left corner   (+)
    makeCharacter 217,      -- lower right corner  (+)
    makeCharacter 179,      -- vertical bar
    makeCharacter 196,      -- horizontal bar      (-)
    $quadSymbol,      -- APL quad            (?)
    '_[,      -- left bracket
    '_],      -- right bracket
    '_{,      -- left brace
    '_},      -- right brace
    makeCharacter 194,      -- top    box tee      (+)
    makeCharacter 193,      -- bottom box tee      (+)
    makeCharacter 180,      -- right  box tee      (+)
    makeCharacter 195,      -- left   box tee      (+)
    makeCharacter 197,      -- center box tee      (+)
    '_\       -- back slash
     ]

$specialCharacters := $RTspecialCharacters

$specialCharacterAlist == '(
  (ulc  .  0)_
  (urc  .  1)_
  (llc  .  2)_
  (lrc  .  3)_
  (vbar .  4)_
  (hbar .  5)_
  (quad .  6)_
  (lbrk .  7)_
  (rbrk .  8)_
  (lbrc .  9)_
  (rbrc . 10)_
  (ttee . 11)_
  (btee . 12)_
  (rtee . 13)_
  (ltee . 14)_
  (ctee . 15)_
  (bslash . 16)_
  )


MATBORCH == '"*"

_*TALLPAR := false

--% Output functions dispatch tables.

for x in '((+ WIDTH sumWidth)
	   (_- APP appneg)
	   (_- WIDTH minusWidth)
	   (_/ APP appfrac)
	   (_/ SUBSPAN fracsub)
	   (_/ SUPERSPAN fracsuper)
	   (_/ WIDTH fracwidth)
	   (AGGSET APP argsapp)
	   (AGGSET SUBSPAN agggsub)
	   (AGGSET SUPERSPAN agggsuper)
	   (AGGSET WIDTH agggwidth)
	   (binom APP binomApp)
	   (binomSUBSPAN binomSub)
	   (binom SUPERSPAN binomSuper)
	   (binom WIDTH binomWidth)
	   (ALTSUPERSUB APP       altSuperSubApp)
	   (ALTSUPERSUB SUBSPAN   altSuperSubSub)
	   (ALTSUPERSUB SUPERSPAN altSuperSubSuper)
	   (ALTSUPERSUB WIDTH     altSuperSubWidth)
	   (BOX APP boxApp)
	   (BOX SUBSPAN boxSub)
	   (BOX SUPERSPAN boxSuper)
	   (BOX WIDTH boxWidth)
	   (BRACKET SUBSPAN qTSub)
	   (BRACKET SUPERSPAN qTSuper)
	   (BRACKET WIDTH qTWidth)
	   (CENTER APP centerApp)
	   (EXT APP appext)
	   (EXT SUBSPAN extsub)
	   (EXT SUPERSPAN extsuper)
	   (EXT WIDTH extwidth)
	   (MATRIX APP appmat)
	   (MATRIX SUBSPAN matSub)
	   (MATRIX SUPERSPAN matSuper)
	   (MATRIX WIDTH matWidth)
	   (NOTHING APP       nothingApp)
	   (NOTHING SUPERSPAN nothingSuper)
	   (NOTHING SUBSPAN   nothingSub)
	   (NOTHING WIDTH     nothingWidth)
	   (OVER APP appfrac)
	   (OVER SUBSPAN fracsub)
	   (OVER SUPERSPAN fracsuper)
	   (OVER WIDTH fracwidth)
	   (OVERLABEL APP overlabelApp)
	   (OVERLABEL SUPERSPAN overlabelSuper)
	   (OVERLABEL WIDTH overlabelWidth)
	   (OVERBAR APP overbarApp)
	   (OVERBAR SUPERSPAN overbarSuper)
	   (OVERBAR WIDTH overbarWidth)
	   (PAREN APP appparu1)
	   (PAREN SUBSPAN qTSub)
	   (PAREN SUPERSPAN qTSuper)
	   (PAREN WIDTH qTWidth)
	   (ROOT APP       rootApp)
	   (ROOT SUBSPAN   rootSub)
	   (ROOT SUPERSPAN rootSuper)
	   (ROOT WIDTH     rootWidth)
	   (ROW WIDTH eq0)
	   (SC APP appsc)
	   (SC SUBSPAN agggsub)
	   (SC SUPERSPAN agggsuper)
	   (SC WIDTH widthSC)
	   (SETQ APP appsetq)
	   (SETQ WIDTH letWidth)
	   (SLASH APP       slashApp)
	   (SLASH SUBSPAN   slashSub)
	   (SLASH SUPERSPAN slashSuper)
	   (SLASH WIDTH     slashWidth)
	   (SUB APP appsub)
	   (SUB SUBSPAN subSub)
	   (SUB SUPERSPAN subSuper)
	   (SUB WIDTH suScWidth)
	   (SUPERSUB APP superSubApp)
	   (SUPERSUB SUBSPAN superSubSub)
	   (SUPERSUB SUPERSPAN superSubSuper)
	   (SUPERSUB WIDTH superSubWidth)
	   (VCONCAT APP vconcatapp)
	   (VCONCAT SUBSPAN vConcatSub)
	   (VCONCAT SUPERSPAN vConcatSuper)
	   (VCONCAT WIDTH vConcatWidth)
	   (BINOMIAL APP binomialApp)
	   (BINOMIAL SUBSPAN binomialSub)
	   (BINOMIAL SUPERSPAN binomialSuper)
	   (BINOMIAL WIDTH binomialWidth)
	   (ZAG APP zagApp)
	   (ZAG SUBSPAN zagSub)
	   (ZAG SUPERSPAN zagSuper)
	   (ZAG WIDTH zagWidth)) 
  repeat
    property(first x, second x) := third x


for x in '((+ APP plusApp)
	   (* APP timesApp)
	   (* WIDTH timesWidth)
	   (** APP exptApp)
	   (** WIDTH exptWidth)
	   (** SUBSPAN exptSub)
	   (** SUPERSPAN exptSuper)
	   (_^  APP exptApp)
	   (_^  WIDTH exptWidth)
	   (_^  SUBSPAN exptSub)
	   (_^  SUPERSPAN exptSuper)
	   (STEP APP stepApp)
	   (STEP WIDTH stepWidth)
	   (STEP SUBSPAN stepSub)
	   (STEP SUPERSPAN stepSuper)
	   (IN APP inApp)
	   (IN WIDTH inWidth)
	   (IN SUBSPAN inSub)
	   (IN SUPERSPAN inSuper)
	   (AGGLST APP aggApp)
	   (AGGLST SUBSPAN aggSub)
	   (AGGLST SUPERSPAN aggSuper)
	   (CONCATB APP concatbApp)
	   (CONCATB SUBSPAN concatSub)
	   (CONCATB SUPERSPAN concatSuper)
	   (CONCATB WIDTH concatbWidth)
	   (CONCAT APP concatApp)
	   (CONCAT  SUBSPAN concatSub)
	   (CONCAT SUPERSPAN concatSuper)
	   (CONCAT WIDTH concatWidth)
	   (QUOTE APP quoteApp)
	   (QUOTE SUBSPAN quoteSub)
	   (QUOTE SUPERSPAN quoteSuper)
	   (QUOTE WIDTH quoteWidth)
	   (STRING APP stringApp)
	   (STRING SUBSPAN eq0)
	   (STRING SUPERSPAN eq0)
	   (STRING WIDTH stringWidth)
	   (SIGMA APP sigmaApp)
	   (SIGMA SUBSPAN sigmaSub)
	   (SIGMA SUPERSPAN sigmaSup)
	   (SIGMA WIDTH sigmaWidth)
	   (SIGMA2 APP sigma2App)
	   (SIGMA2 SUBSPAN sigma2Sub)
	   (SIGMA2 SUPERSPAN sigma2Sup)
	   (SIGMA2 WIDTH sigma2Width)
	   (INTSIGN APP intApp)
	   (INTSIGN SUBSPAN intSub)
	   (INTSIGN SUPERSPAN intSup)
	   (INTSIGN WIDTH intWidth)
	   (INDEFINTEGRAL APP indefIntegralApp)
	   (INDEFINTEGRAL SUBSPAN indefIntegralSub)
	   (INDEFINTEGRAL SUPERSPAN indefIntegralSup)
	   (INDEFINTEGRAL WIDTH indefIntegralWidth)
	   (PI APP piApp)
	   (PI SUBSPAN piSub)
	   (PI SUPERSPAN piSup)
	   (PI WIDTH piWidth)
	   (PI2 APP pi2App)
	   (PI2 SUBSPAN pi2Sub)
	   (PI2 SUPERSPAN pi2Sup)
	   (PI2 WIDTH pi2Width)
	   (AGGLST WIDTH aggWidth)
	   (BRACKET APP bracketApp)
	   (BRACE APP braceApp)
	   (BRACE WIDTH qTWidth)) 
  repeat
    property(first x, second x) := third x

for x in ["*","+","AND","OR","PROGN"] repeat
  property(x,'NARY) := true

for x in '((_= "=")
          (_: ":")
          (_not "not ")
          (_| " | ")
          (_SEGMENT ".."))
  repeat
    property(first x,'PREFIXOP) := second x

for x in '((_:_= " := ")
           (_/ "/")
           (_+ "+")
           (_* "*")
           (_*_* "**")
           (_^ "^")
           (_: ":")
           (_:_: "::")
           (_@ "@")
           (SEL ".")
           (_exquo " exquo ")
           (_div " div ")
           (_quo " quo ")
           (_rem " rem ")
           (_case " case ")
           (_and " and ")
           (_/_\ " /\ ")
           (_or " or ")
           (_\_/ " \/ ")
           (TAG ": ")
           (_+_-_> " +-> ")
           (RARROW " -> ")
           (SEGMENT "..")
           (_in " in ")
           (EL* ":")
           (JOIN " JOIN ")
           (EQUATNUM "  ")
           (IQUOTIENT "//")
           (_= "= ")
           (_>_= " >= ")
           (_> " > ")
           (_<_= " <= ")
           (_< " < ")
           (_| " | ")
           (_+ " + ")
           (_- " - ")
           (MEMBER " in ")
           (NMEMBER " nin ")
           (WHERE " WHERE ")
           (AT " AT ")
           (MAX " MAX ")
           (MIN " MIN "))
   repeat
     property(first x,'INFIXOP) := second x

property('TAG,'Led) := '(TAG TAG 122 121)
property('EQUATNUM,'Nud) := '(dummy dummy 0 0)
property('EQUATNUM,'Led) := '(dummy dummy 10000 0)
property('%LET,'Led) := '(_:_= %LET 125 124)
property('RARROW,'Led) := '(_=_= DEF 122 121)
property('SEGMENT,'Led) := '(_._. SEGMENT 401 699 (P_:Seg))
property('SEGMENT,'isSuffix) := true
property('EQUAL1,'CHRYBNAM) := 'EQ
property('COND,'Nud) := '(_if _if 130 0)
property('CONS,'Led) := '(CONS CONS 1000 1000)
property('APPEND,'Led) := '(APPEND APPEND 1000 1000)

--%

$collectOutput := false

++ Start a a new line if we are in 2-d ASCII art display mode.
newlineIfDisplaying() ==
  if not $collectOutput then
    writeNewline $algebraOutputStream


specialChar(symbol) ==
  -- looks up symbol in $specialCharacterAlist, gets the index
  -- into the abstractChar table, and returns the appropriate character
  null (code := IFCDR objectAssoc(symbol,$specialCharacterAlist)) => '"?"
  $specialCharacters.code

rbrkSch() == symbolName specialChar 'rbrk
lbrkSch() == symbolName specialChar 'lbrk
quadSch() == symbolName specialChar 'quad

++ List of binary operators
$BinaryOperators ==
  ["**", "^", "*", "/", "//", "\", "\\", "rem", "quo", "exquo", "+", "-",
    "/\", "\/", "=", "~=", "<", "<=", ">", ">=", "and", "or", ">>", "<<",
      "by", "has", "case", "->", "..", "|"]
  

binaryInfix? x ==
  ident? x => symbolMember?(x,$BinaryOperators)
  string? x => symbolMember?(makeSymbol x,$BinaryOperators)
  false

stringApp([.,u],x,y,d) ==
  appChar(strconc($DoubleQuote,atom2String u,$DoubleQuote),x,y,d)

stringWidth u ==
  u is [.,u] or THROW('outputFailure,'outputFailure)
  2+#u

obj2String o ==
  o isnt [.,:.] =>
    string? o => o
    o = " " => '" "
    o = ")" => '")"
    o = "(" => '"("
    STRINGIMAGE o
  apply(function strconc,[obj2String o' for o' in o])

APP(u,x,y,d) ==
  u isnt [.,:.] => appChar(atom2String u,x,y,d)
  u is [[op,:.],a] and (s:= GETL(op,'PREFIXOP)) =>
    GETL(op,'isSuffix) => appChar(s,x+WIDTH a,y,APP(a,x,y,d))
    APP(a,x+#s,y,appChar(s,x,y,d))
  u is [[id,:.],:.] =>
    fn := GETL(id,'APP) => FUNCALL(fn,u,x,y,d)
    not integer? id and (d':= appInfix(u,x,y,d))=> d'
    appelse(u,x,y,d)
  appelse(u,x,y,d)

atom2String x ==
  ident? x => symbolName x
  string? x => x
  stringer x

-- General convention in the "app..." functions:
-- Added from an attempt to fix bugs by JHD: 2 Aug 89
-- the first argument is what has to be printed
-- the second - x - is the horizontal distance along the page
--      at which to start
-- the third - y - is some vertical hacking control
-- the foruth - d - is the "layout" so far
-- these functions return an updated "layout so far" in general

appChar(string,x,y,d) ==
  if CHARP string then string := PNAME string
  line:= LASSOC(y,d) =>
    if maxIndex string = 1 and stringChar(string,0) = char "%" then
      stringChar(string,1) = char "b" =>
        bumpDeltaIfTrue:= true
        stringChar(string,0) := abstractChar 29
        stringChar(string,1) := abstractChar 200
      stringChar(string,1) = char "d" =>
        bumpDeltaIfTrue:= true
        stringChar(string,0) := abstractChar 29
        stringChar(string,1) := abstractChar 65
    shiftedX:= (y=0 => x+$highlightDelta; x)
      --shift x for brightening characters -- presently only if y=0
    RPLACSTR(line,shiftedX,n:=#string,string,0,n)
    if bumpDeltaIfTrue=true then $highlightDelta:= $highlightDelta+1
    d
  appChar(string,x,y,append!(d,[[y,:makeString(10+$LINELENGTH+$MARGIN,char " ")]]))

print(x,domain) ==
  dom:= devaluate domain
  $InteractiveMode: local:= true
  $dontDisplayEquatnum: local:= true
  output(x,dom)

++ Write x as an asgard form on the standard output.
outputAsgardForm(x,t) ==
  f := ['%OBJECT,x,devaluate t]
  WRITE(f,stream <- $algebraOutputStream)
  freshLine $algebraOutputStream

mathprintWithNumber(x,t) ==
  x:= outputTran x
  $asgardForm => outputAsgardForm(x,t)
  maprin
    $IOindex => ['EQUATNUM,$IOindex,x]
    x

mathprint(x,out == $OutputStream) == 
   x := outputTran x
   maprin x

sayMath u ==
  for x in u repeat acc:= concat(acc,linearFormatName x)
  sayMSG acc

--% Output transformations

outputTran x ==
  member(x,'("failed" "nil" "prime" "sqfr" "irred")) =>
    strconc('"_"",x,'"_"")
  string? x => x
  vector? x =>
    outputTran ['BRACKET,['AGGLST,:[x.i for i in 0..maxIndex x]]]
  integer? x =>
    x < 0 => ["-",MINUS x]
    x
  x isnt [.,:.] =>
    x=$EmptyMode => specialChar 'quad
    x
  x is [c,var,mode] and c in '(_pretend _: _:_: _@) =>
    var := outputTran var
    if cons? var then var := ['PAREN,var]
    ['CONCATB,var,c,obj2String prefix2String mode]
  x is ['ADEF,vars,.,.,body] =>
    vars :=
        vars is [x] => x
        ["tuple",:vars]
    outputTran ["+->", vars, body]
  x is ['MATRIX,:m] => outputTranMatrix m
  x is ['matrix,['construct,c]] and
    c is ['COLLECT,:m,d] and d is ['construct,e] and e is ['COLLECT,:.] =>
      outputTran ['COLLECT,:m,e]
  x is ['%list,:l] => outputTran ['BRACKET,['AGGLST,:l]]
  x is ["%Map",:l] => outputMapTran l
  x is ['brace, :l]    =>
    ['BRACE,  ['AGGLST,:[outputTran y for y in l]]]
  x is ["return",l] => ["return",outputTran l]
  x is ["return",.,:l] => ["return",:outputTran l]
  x is ["construct",:l] =>
    ['BRACKET,['AGGLST,:[outputTran y for y in l]]]

  x is [["$elt",domain,"float"], x, y, z] and (domain = $DoubleFloat or
    domain is ['Float]) and integer? x and integer? y and integer? z and
        z > 0  and (float := getFunctionFromDomain("float",domain,[$Integer,$Integer,$PositiveInteger])) =>
            f := SPADCALL(x,y,z,float)
            o := coerceInteractive(objNewWrap(f, domain), '(OutputForm))
            objValUnwrap o

  [op,:l]:= flattenOps x
  --needed since "op" is string in some spad code
  if string? op then (op := makeSymbol op; x:= [op,:l])
  op = 'LAMBDA_-CLOSURE => 'Closure
  x is ['break,:.] => 'break
  x is ['SEGMENT,a] =>
    a' := outputTran a
    if LISTP a' then a' := ['PAREN,a']
    ['SEGMENT,a']
  x is ['SEGMENT,a,b] =>
    a' := outputTran a
    b' := outputTran b
    if LISTP a' then a' := ['PAREN,a']
    if LISTP b' then b' := ['PAREN,b']
    ['SEGMENT,a',b']

  op is ["$elt",targ,fun] or not $InteractiveMode and op is ["elt",targ,fun] =>
    -- l has the args
    targ' := obj2String prefix2String targ
    if 2 = # targ then targ' := ['PAREN,targ']
    ['CONCAT,outputTran [fun,:l],'"$",targ']
  x is ["$elt",targ,c] or not $InteractiveMode and x is ["elt",targ,c] =>
    targ' := obj2String prefix2String targ
    if 2 = # targ then targ' := ['PAREN,targ']
    ['CONCAT,outputTran c,'"$",targ']
  x is ["-",a,b] =>
    a := outputTran a
    b := outputTran b
    integer? b =>
      b < 0 => ["+",a,-b]
      ["+",a,["-",b]]
    b is ["-",c] => ["+",a,c]
    ["+",a,["-",b]]

  -- next stuff translates exp(log(foo4)/foo3) into ROOT(foo4,foo3)
  (x is ["**",'"%e",foo1]) and (foo1 is ['"/",foo2, foo3]) and
    integer?(foo3) and (foo2 is ['log,foo4]) =>
       foo3 = 2 => ['ROOT,outputTran foo4]
       ['ROOT,outputTran foo4,outputTran foo3]
  (x is ["**",'"%e",foo1]) and (foo1 is [op',foo2, foo3]) and
    (op' = '"*") and ((foo3 is ['log,foo4]) or (foo2 is ['log,foo4])) =>
       foo3 is ['log,foo4] =>
         ["**", outputTran foo4, outputTran foo2]
       foo4 := second foo2
       ["**", outputTran foo4, outputTran foo3]
  op = 'IF       => outputTranIf x
  op = 'COLLECT  => outputTranCollect x
  op = 'REDUCE   => outputTranReduce x
  op = 'REPEAT   => outputTranRepeat x
  op = 'SEQ      => outputTranSEQ x
  op in '(cons nconc) => outputConstructTran x
  l:= [outputTran y for y in l]
  op = "*" =>
     l is [a] => outputTran a
     l is [["-",a],:b] =>
       -- now this is tricky because we've already outputTran the list
       -- expect trouble when outputTran hits b again 
       -- some things object to being outputTran twice ,e.g.matrices
       -- same thing a bit lower down for "/" 
       a=1 => outputTran ["-",[op,:b]]
       outputTran ["-",[op,a,:b]]
     [op,:"append"/[(ss is ["*",:ll] => ll; [ss]) for ss in l]]
  op = "+" =>
     l is [a] => outputTran a
     [op,:"append"/[(ss is ["+",:ll] => ll; [ss]) for ss in l]]
  op = "/" =>
    if $fractionDisplayType = 'horizontal then op := 'SLASH
    else op := 'OVER
    l is [["-",a],:b] => outputTran ["-",[op,a,:b]]
    [outputTran op,:l]
  op="|" and l is [["tuple",:u],pred] =>
    ['PAREN,["|",['AGGLST,:l],pred]]
  op="tuple"  => ['PAREN,['AGGLST,:l]]
  op='LISTOF => ['AGGLST,:l]
  ident? op and not (op in '(_* _*_*) ) and
    char "*" = stringChar(symbolName op,0) => mkSuperSub(op,l)
  [outputTran op,:l]

-- The next two functions are designed to replace successive instances of
-- binary functions with the n-ary equivalent, cutting down on recursion
-- in outputTran and in partciular allowing big polynomials to be printed
-- without stack overflow.  MCD.
flattenOps l ==
  [op, :args ] := l
  member(op,['"+",'"*","+","*"]) =>
    [op,:checkArgs(op,args)]
  l

checkArgs(op,tail) ==
  head := []
  while tail repeat
    term := first tail
    term isnt [.,:.] =>
      head := [term,:head]
      tail := rest tail
    not LISTP term => -- never happens?
      head := [term,:head]
      tail := rest tail
    op=first term =>
      tail := [:rest term,:rest tail]
    head := [term,:head]
    tail := rest tail
  reverse head
   
outputTranSEQ ['SEQ,:l,exitform] ==
  if exitform is ['exit,.,a] then exitform := a
  ['SC,:[outputTran x for x in l],outputTran exitform]

outputTranIf ['IF,x,y,z] ==
  y = '%noBranch =>
    ['CONCATB,'if,['CONCATB,'not,outputTran x],'then,outputTran z]
  z = '%noBranch =>
    ['CONCATB,'if,outputTran x,'then,outputTran y]
  y' := outputTran y
  z' := outputTran z
--y' is ['SC,:.] or z' is ['SC,:.] =>
-- ['CONCATB,'if,outputTran x,
--   ['SC,['CONCATB,'then,y'],['CONCATB,'else,z']]]
--['CONCATB,'if,outputTran x,'then,outputTran y,'else,outputTran z]
  ['CONCATB,'if,outputTran x,
    ['SC,['CONCATB,'then,y'],['CONCATB,'else,z']]]

outputMapTran l ==
  null l => nil         -- should not happen

  -- display subscripts linearly
  $linearFormatScripts : local := true

  -- get the real names of the parameters
  alias := get($op,'alias,$InteractiveFrame)

  rest l =>             -- if multiple forms, call repeatedly
    ['SC,:[outputMapTran0(ll,alias) for ll in l]]
  outputMapTran0(first l,alias)

outputMapTran0(argDef,alias) ==
  arg := first argDef
  def := rest  argDef
  [arg',:def'] := simplifyMapPattern(argDef,alias)
  arg' := outputTran arg'
  if null arg' then arg' := '"()"
  ['CONCATB,$op,outputTran arg',"==",outputTran def']

outputTranReduce ['REDUCE,op,.,body] ==
  ['CONCAT,op,"/",outputTran body]

outputTranRepeat ["REPEAT",:itl,body] ==
  body' := outputTran body
  itl =>
    itlist:= outputTranIteration itl
    ['CONCATB,itlist,'repeat,body']
  ['CONCATB,'repeat,body']

outputTranCollect [.,:itl,body] ==
  itlist:= outputTranIteration itl
  ['BRACKET,['CONCATB,outputTran body,itlist]]

outputTranIteration itl ==
  null rest itl => outputTranIterate first itl
  ['CONCATB,outputTranIterate first itl,outputTranIteration rest itl]

outputTranIterate x ==
  x is ['STEP,n,init,step,:final] =>
    init' := outputTran init
    if LISTP init then init' := ['PAREN,init']
    final' :=
      final =>
        LISTP first final => [['PAREN,outputTran first final]]
        [outputTran first final]
      nil
    ['STEP,outputTran n,init',outputTran step,:final']
  x is ["IN",n,s] => ["IN",outputTran n,outputTran s]
  x is [op,p] and op in '(_| UNTIL WHILE) =>
    op:= DOWNCASE op
    ['CONCATB,op,outputTran p]
  throwKeyedMsg("S2IX0008",['outputTranIterate,['"illegal iterate: ",x]])

outputConstructTran x ==
  x is [op,a,b] =>
    a:= outputTran a
    b:= outputTran b
    op="cons" =>
      b is ['construct,:l] => ['construct,a,:l]
      ['BRACKET,['AGGLST,:[a,[":",b]]]]
    op="nconc" =>
      aPart :=
        a is ['construct,c] and c is ['SEGMENT,:.] => c
        [":",a]
      b is ['construct,:l] => ['construct,aPart,:l]
      ['BRACKET,['AGGLST,aPart,[":",b]]]
    [op,a,b]
  x isnt [.,:.] => x
  [outputTran first x,:outputConstructTran rest x]

outputTranMatrix x ==
  not vector? x =>
    -- assume that the only reason is that we've been done before
    ["MATRIX",:x]
    --keyedSystemError("S2GE0016",['"outputTranMatrix",
    -- '"improper internal form for matrix found in output routines"])
  ["MATRIX",nil,:[outtranRow x.i for i in 0..maxIndex x]] where
    outtranRow x ==
      not vector? x =>
        keyedSystemError("S2GE0016",['"outputTranMatrix",
          '"improper internal form for matrix found in output routines"])
      ["ROW",:[outputTran x.i for i in 0..maxIndex x]]

mkSuperSub(op,argl) ==
  $linearFormatScripts => linearFormatForm(op,argl)
--  l := [(string? f => f; STRINGIMAGE f)
--    for f in linearFormatForm(op,argl)]
--  strconc/l
  s:= PNAME op
  indexList:= [readInteger PNAME d for i in 1.. while
    (digit? (d:= s.(idxmax:= i)))]
  cleanOp:= makeSymbol (strconc/[PNAME s.i for i in idxmax..maxIndex s])
  -- if there is just a subscript use the SUB special form
  #indexList=2 =>
    subPart:= ['SUB,cleanOp,:take(indexList.1,argl)]
    l:= drop(indexList.1,argl) => [subPart,:l]
    subPart
  -- otherwise use the SUPERSUB form
  superSubPart := nil
  for i in rest indexList repeat
    scripts :=
      this:= take(i,argl)
      argl:= drop(i,argl)
      i=0 => ['AGGLST]
      i=1 => first this
      ['AGGLST,:this]
    superSubPart := [scripts,:superSubPart]
  superSub := ['SUPERSUB,cleanOp,:reverse superSubPart]
  argl => [superSub,:argl]
  superSub

timesApp(u,x,y,d) ==
  rightPrec:= getOpBindingPower("*","Led","right")
  firstTime:= true
  for arg in rest u repeat
    op:= keyp arg
    if not firstTime and (needBlankForRoot(lastOp,op,arg) or
       needStar(wasSimple,wasQuotient,wasNumber,arg,op) or
        wasNumber and op = 'ROOT and subspan arg = 1) then
      d:= APP(BLANK,x,y,d)
      x:= x+1
    [d,x]:= appInfixArg(arg,x,y,d,rightPrec,"left",nil) --app in a right arg
    wasSimple:= arg isnt [.,:.] and not integer? arg or isRationalNumber arg
    wasQuotient:= isQuotient op
    wasNumber:= integer? arg
    lastOp := op
    firstTime:= nil
  d

needBlankForRoot(lastOp,op,arg) ==
  lastOp ~= "^" and lastOp ~= "**" and not(subspan(arg)>0) => false
  op = "**" and keyp second arg = 'ROOT => true
  op = "^" and keyp second arg = 'ROOT => true
  op = 'ROOT and CDDR arg => true
  false

stepApp([.,a,init,one,:optFinal],x,y,d) ==
  d:= appChar('"for ",x,y,d)
  d:= APP(a,w:=x+4,y,d)
  d:= appChar('" in ",w:=w+WIDTH a,y,d)
  d:= APP(init,w:=w+4,y,d)
  d:= APP('"..",w:=w+WIDTH init,y,d)
  if optFinal then d:= APP(first optFinal,w+2,y,d)
  d

stepSub [.,a,init,one,:optFinal] ==
  m:= MAX(subspan a,subspan init)
  optFinal => MAX(m,subspan first optFinal)
  m

stepSuper [.,a,init,one,:optFinal] ==
  m:= MAX(superspan a,superspan init)
  optFinal => MAX(m,superspan first optFinal)
  m

stepWidth [.,a,init,one,:optFinal] ==
   10+WIDTH a+WIDTH init+(optFinal => WIDTH first optFinal; 0)

inApp([.,a,s],x,y,d) ==  --for [IN,a,s]
  d:= appChar('"for ",x,y,d)
  d:= APP(a,x+4,y,d)
  d:= appChar('" in ",x+WIDTH a+4,y,d)
  APP(s,x+WIDTH a+8,y,d)

inSub [.,a,s] == MAX(subspan a,subspan s)

inSuper [.,a,s] == MAX(superspan a,superspan s)

inWidth [.,a,s] == 8+WIDTH a+WIDTH s

centerApp([.,u],x,y,d) ==
  d := APP(u,x,y,d)

concatApp([.,:l],x,y,d) == concatApp1(l,x,y,d,0)

concatbApp([.,:l],x,y,d) == concatApp1(l,x,y,d,1)

concatApp1(l,x,y,d,n) ==
  for u in l repeat
    d:= APP(u,x,y,d)
    x:=x+WIDTH u+n
  d

concatSub [.,:l] == "MAX"/[subspan x for x in l]

concatSuper [.,:l] == "MAX"/[superspan x for x in l]

concatWidth [.,:l] == +/[WIDTH x for x in l]

concatbWidth [.,:l] == +/[1+WIDTH x for x in l]-1

exptApp([.,a,b],x,y,d) ==
  pren:= exptNeedsPren a
  d:=
    pren => appparu(a,x,y,d)
    APP(a,x,y,d)
  x':= x+WIDTH a+(pren => 2;0)
  y':= 1+y+superspan a+subspan b + (0=superspan a => 0; -1)
  APP(b,x',y',d)

exptNeedsPren a ==
  a isnt [.,:.] and null (integer? a and a < 0)  => false
  key:= keyp a
  key = "OVER" => true  -- added JHD 2/Aug/90
  (key="SUB") or (null GETL(key,"Nud") and null GETL(key,"Led")) => false
  true

exptSub u == subspan second u

exptSuper [.,a,b] == superspan a+height b+(superspan a=0 => 0;-1)

exptWidth [.,a,b] == WIDTH a+WIDTH b+(exptNeedsPren a => 2;0)

needStar(wasSimple,wasQuotient,wasNumber,cur,op) ==
  wasQuotient or isQuotient op => true
  wasSimple =>
    cur isnt [.,:.] or keyp cur="SUB" or isRationalNumber cur or op="**" or op = "^" or
      (op isnt [.,:.] and not integer? op and null GETL(op,"APP"))
  wasNumber =>
    integer?(cur) or isRationalNumber cur or
        ((op="**" or op ="^") and integer?(second cur))

isQuotient op ==
  op="/" or op="OVER"

timesWidth u ==
  rightPrec:= getOpBindingPower("*","Led","right")
  firstTime:= true
  w:= 0
  for arg in rest u repeat
    op:= keyp arg
    if not firstTime and needStar(wasSimple,wasQuotient,wasNumber,arg,op) then
      w:= w+1
    if infixArgNeedsParens(arg, rightPrec, "left") then w:= w+2
    w:= w+WIDTH arg
    wasSimple:= arg isnt [.,:.] and not integer? arg --or isRationalNumber arg
    wasQuotient:= isQuotient op
    wasNumber:= integer? arg
    firstTime:= nil
  w

plusApp([.,frst,:rst],x,y,d) ==
  appSum(rst,x+WIDTH frst,y,APP(frst,x,y,d))

appSum(u,x,y,d) ==
  for arg in u repeat
    infixOp:=
      syminusp arg => "-"
      "+"
    opString:= GETL(infixOp,"INFIXOP") or '","
    d:= APP(opString,x,y,d)
    x:= x+WIDTH opString
    arg:= absym arg --negate a neg. number or remove leading "-"
    rightPrec:= getOpBindingPower(infixOp,"Led","right")
    if infixOp = "-" then rightPrec:=rightPrec  +1
    -- that +1 added JHD 2 Aug 89 to prevent x-(y+z) printing as x-y+z
    -- Sutor found the example:
    -- )cl all
    -- p : P[x] P I := x - y - z
    -- p :: P[x] FR P I
    -- trailingCoef %
    [d,x]:= appInfixArg(arg,x,y,d,rightPrec,"left",nil) --app in a right arg
  d

appInfix(e,x,y,d) ==
  op := keyp e
  leftPrec:= getOpBindingPower(op,"Led","left")
  leftPrec = 1000 => return nil --no infix operator is allowed default value
  rightPrec:= getOpBindingPower(op,"Led","right")
  #e < 2 => throwKeyedMsg("S2IX0008",['appInfix,
      '"fewer than 2 arguments to an infix function"])
  opString:= GETL(op,"INFIXOP") or '","
  opWidth:= WIDTH opString
  [.,frst,:rst]:= e
  null rst =>
    GETL(op,"isSuffix") =>
      [d,x]:= appInfixArg(frst,x,y,d,leftPrec,"right",opString)
      d:= appChar(opString,x,y,d)
    THROW('outputFailure,'outputFailure)
  [d,x]:= appInfixArg(frst,x,y,d,leftPrec,"right",opString) --app in left arg
  for arg in rst repeat
    d:= appChar(opString,x,y,d) --app in the infix operator
    x:= x+opWidth
    [d,x]:= appInfixArg(arg,x,y,d,rightPrec,"left",opString) --app in right arg
  d

appconc(d,x,y,w) == append!(d,[[[x,:y],:w]])

infixArgNeedsParens(arg, prec, leftOrRight) ==
  prec > getBindingPowerOf(leftOrRight, arg) + 1

appInfixArg(u,x,y,d,prec,leftOrRight,string) ==
  insertPrensIfTrue:= infixArgNeedsParens(u,prec,leftOrRight)
  d:=
    insertPrensIfTrue => appparu(u,x,y,d)
    APP(u,x,y,d)
  x:= x+WIDTH u
  if string then d:= appconc(d,x,y,string)
  [d,(insertPrensIfTrue => x+2; x)]

leftBindingPowerOf(x, ind) ==
  y := GETL(x, ind)
  y => ELEMN(y, 3, 0)
  0

rightBindingPowerOf(x, ind) ==
  y := GETL(x, ind)
  y => ELEMN(y, 4, 105)
  105

getBindingPowerOf(key,x) ==
  --binding powers can be found in file NEWAUX LISP
  x is ['REDUCE,:.] => (key='left => 130; key='right => 0)
  x is ["REPEAT",:.] => (key="left" => 130; key="right" => 0)
  x is ['%when,:.] => (key="left" => 130; key="right" => 0)
  x is [op,:argl] =>
    if op is [a,:.] then op:= a
    op = 'SLASH => getBindingPowerOf(key,["/",:argl]) - 1
    op = 'OVER  => getBindingPowerOf(key,["/",:argl])
    (n:= #argl)=1 =>
      key="left" and (m:= getOpBindingPower(op,"Nud","left")) => m
      key="right" and (m:= getOpBindingPower(op,"Nud","right")) => m
      1000
    n>1 =>
      key="left" and (m:= getOpBindingPower(op,"Led","left")) => m
      key="right" and (m:= getOpBindingPower(op,"Led","right")) => m
      op="ELT" => 1002
      1000
    1000
  1002

getOpBindingPower(op,LedOrNud,leftOrRight) ==
  if op in '(SLASH OVER) then op := "/"
  exception:=
    leftOrRight="left" => 0
    105
  bp:=
    leftOrRight="left" => leftBindingPowerOf(op,LedOrNud)
    rightBindingPowerOf(op,LedOrNud)
  bp ~= exception => bp
  1000

--% Brackets
bracketApp(u,x,y,d) ==
  u is [.,u] or THROW('outputFailure,'outputFailure)
  d:= appChar(specialChar 'lbrk,x,y,d)
  d:=APP(u,x+1,y,d)
  appChar(specialChar 'rbrk,x+1+WIDTH u,y,d)

--% Braces
braceApp(u,x,y,d) ==
  u is [.,u] or THROW('outputFailure,'outputFailure)
  d:= appChar(specialChar 'lbrc,x,y,d)
  d:=APP(u,x+1,y,d)
  appChar(specialChar 'rbrc,x+1+WIDTH u,y,d)

--% Aggregates
aggWidth u ==
  rest u is [a,:l] => WIDTH a + +/[1+WIDTH x for x in l]
  0

aggSub u == subspan rest u

aggSuper u == superspan rest u

aggApp(u,x,y,d) == aggregateApp(rest u,x,y,d,",")

aggregateApp(u,x,y,d,s) ==
  if u is [a,:l] then
    d:= APP(a,x,y,d)
    x:= x+WIDTH a
    for b in l repeat
      d:= APP(s,x,y,d)
      d:= APP(b,x+1,y,d)
      x:= x+1+WIDTH b
  d

--% Function to compute Width

outformWidth u ==  --WIDTH as called from OUTFORM to do a COPY
  string? u =>
    u = $EmptyString => 0
    stringChar(u,0) = char "%" and
      (stringChar(u,1) = char "b" or stringChar(u,1) = char "d") => 1
    #u
  u isnt [.,:.] => # atom2String u
  WIDTH copyTree u

WIDTH u ==
  string? u =>
    u = $EmptyString => 0
    stringChar(u,0) = char "%" and
      (stringChar(u,1) = char "b" or stringChar(u,1) = char "d") => 1
    #u
  integer? u => 
    if (u < 1) then 
      negative := 1
      u := -u
    else
      negative := 0
    -- Try and be fairly exact for smallish integers:
    u = 0 => 1
    u < $DoubleFloatMaximum => 1+negative+FLOOR ((log10 u) + 0.0000001)
    -- Rough guess: integer-length returns log2 rounded up, so divide it by
    -- roughly log2(10). This should return an over-estimate, but for objects
    -- this big does it matter?
    FLOOR(INTEGER_-LENGTH(u)/3.3)
  u isnt [.,:.] => # atom2String u
  putWidth u is [[.,:n],:.] => n
  THROW('outputFailure,'outputFailure)

putWidth u ==
  u isnt [.,:.] or u is [[.,:n],:.] and integer? n => u
  op:= keyp u
--integer? op => nil
  leftPrec:= getBindingPowerOf("left",u)
  rightPrec:= getBindingPowerOf("right",u)
  [firstEl,:l] := u
  interSpace:=
    symbol? firstEl and GETL(firstEl,"INFIXOP") => 0
    1
  argsWidth:=
    l is [firstArg,:restArg] =>
      u.rest.first := putWidth firstArg
      for y in tails restArg repeat 
        y.first := putWidth first y
      widthFirstArg:=
        0=interSpace and infixArgNeedsParens(firstArg,leftPrec,"right")=>
          2+WIDTH firstArg
        WIDTH firstArg
      widthFirstArg + +/[interSpace+w for x in restArg] where w() ==
        0=interSpace and infixArgNeedsParens(x, rightPrec, "left") =>
          2+WIDTH x
        WIDTH x
    0
  newFirst:=
    (oldFirst:= first u) isnt [.,:.] =>
      fn:= GETL(oldFirst,"WIDTH") =>
        [oldFirst,:FUNCALL(fn,[oldFirst,:l])]
      if l then ll := rest l else ll := nil
      [oldFirst,:opWidth(oldFirst,ll)+argsWidth]
    [putWidth oldFirst,:2+WIDTH oldFirst+argsWidth]
  u.first := newFirst
  u

opWidth(op,has2Arguments) ==
  op = "EQUATNUM" => 4
  integer? op => 2 + # STRINGIMAGE op
  null has2Arguments =>
    a:= GETL(op,"PREFIXOP") => # a
    2 + # PNAME op
  a:= GETL(op,"INFIXOP") => # a
  2 + # PNAME op

matrixBorder(x,y1,y2,d,leftOrRight) ==
  y1 = y2 =>
    c :=
      leftOrRight = 'left => specialChar('lbrk)
      specialChar('rbrk)
    APP(c,x,y1,d)
  for y in y1..y2 repeat
    c :=
      y = y1 =>
        leftOrRight = 'left => specialChar('llc)
        specialChar('lrc)
      y = y2 =>
        leftOrRight = 'left => specialChar('ulc)
        specialChar('urc)
      specialChar('vbar)
    d := APP(c,x,y,d)
  d

isRationalNumber x == nil

widthSC u == 10000

--% The over-large matrix package

$demoFlag := false


maprinSpecial(x,$MARGIN,$LINELENGTH) == maprin0 x
-- above line changed JHD 13/2/93 since it used to call maPrin

maprin x ==
  if $demoFlag=true then recordOrCompareDemoResult x
  CATCH('output,maprin0 x)
  $leanMode and 
    WRITE($RecordSeparator,stream <- $algebraOutputStream)
  nil

maprin0 x ==
  $MatrixCount:local :=0
  $MatrixList:local :=nil
  maprinChk x
  if $MatrixList then maprinRows $MatrixList
  -- above line moved JHD 28/2/93 to catch all routes through maprinChk

maprinChk x ==
  null $MatrixList => maPrin x
  x isnt [.,:.] and (u:= assoc(x,$MatrixList)) =>
    $MatrixList := remove($MatrixList,u)
    maPrin deMatrix rest u
  x is ["=",arg,y]  =>     --case for tracing with )math and printing matrices
    u:=assoc(y,$MatrixList) =>
      -- we don't want to print matrix1 = matrix2 ...
      $MatrixList := remove($MatrixList,u)
      maPrin ["=",arg, deMatrix rest u]
    maPrin x
  x is ['EQUATNUM,n,y] =>
    $MatrixList is [[name,:value]] and y=name =>
      $MatrixList:=[]   -- we are pulling this one off
      maPrin ['EQUATNUM,n, deMatrix value]
    ident? y => --------this part is never called
      -- Not true: JHD 28/2/93
      -- m:=[[1,2,3],[4,5,6],[7,8,9]]
      -- mm:=[[m,1,0],[0,m,1],[0,1,m]]
      -- and try to print mm**5
      u := assoc(y,$MatrixList)
      --$MatrixList := deleteAssoc(first u,$MatrixList)
      -- deleteAssoc no longer exists
      $MatrixList := remove($MatrixList,u)
      maPrin ['EQUATNUM,n,rest u]
      newlineIfDisplaying()
    maPrin x
  maPrin x
  -- above line added JHD 13/2/93 since otherwise x gets lost

maprinRows matrixList ==
  newlineIfDisplaying()
  while matrixList repeat
    y := reverse! matrixList
    --Makes the matrices come out in order, since CONSed on backwards
    matrixList:=nil
    firstName := first first y
    for [name,:m] in y for n in 0.. repeat
      newlineIfDisplaying()
      andWhere := (name = firstName => '"where "; '"and ")
      line := strconc(andWhere, PNAME name)
      maprinChk ["=",line,m]
      -- note that this could place a new element on $MatrixList, hence the loop

deMatrix m ==
    ['BRACKET,['AGGLST,
        :[['BRACKET,['AGGLST,:rest row]] for row in CDDR m]]]

LargeMatrixp(u,width, dist) ==
  --  sees if there is a matrix wider than 'width' in the next 'dist'
  --  part of u, a sized charybdis structure.
  --  nil if not, first such matrix if there is one
  u isnt [.,:.] => nil
  CDAR u <= width => nil
       --CDAR is the width of a charybdis structure
  op:=CAAR u
  op = 'MATRIX => largeMatrixAlist u
         --We already know the structure is more than 'width' wide
  op in '(%LET RARROW SEGMENT _- CONCAT CONCATB PAREN BRACKET BRACE) =>
      --Each of these prints the arguments in a width 3 smaller
    dist:=dist-3
    width:=width-3
    ans:=
      for v in rest u repeat
        (ans:=LargeMatrixp(v,width,dist)) => return largeMatrixAlist ans
        dist:=dist - WIDTH v
        dist<0 => return nil
    ans
      --Relying that falling out of a loop gives nil
  op in '(_+ _* ) =>
      --Each of these prints the first argument in a width 3 smaller
    (ans:=LargeMatrixp(second u,width-3,dist)) => largeMatrixAlist ans
    n:=3+WIDTH second u
    dist:=dist-n
    ans:=
      for v in CDDR u repeat
        (ans:=LargeMatrixp(v,width,dist)) => return largeMatrixAlist ans
        dist:=dist - WIDTH v
        dist<0 => return nil
    ans
      --Relying that falling out of a loop gives nil
  ans:=
    for v in rest u repeat
      (ans:=LargeMatrixp(v,width,dist)) => return largeMatrixAlist ans
      dist:=dist - WIDTH v
      dist<0 => return nil
  ans
    --Relying that falling out of a loop gives nil

largeMatrixAlist u ==
  u is [op,:r] =>
    op is ['MATRIX,:.] => deMatrix u
    largeMatrixAlist op or largeMatrixAlist r
  nil

PushMatrix m ==
    --Adds the matrix to the look-aside list, and returns a name for it
  name:=
    for v in $MatrixList repeat
        m = rest v => return first v
  name => name
  name := makeSymbol strconc('"matrix",toString($MatrixCount:=$MatrixCount+1))
  $MatrixList:=[[name,:m],:$MatrixList]
  name

quoteApp([.,a],x,y,d) == APP(a,x+1,y,appChar(PNAME "'",x,y,d))

quoteSub [.,a] == subspan a

quoteSuper [.,a] == superspan a

quoteWidth [.,a] == 1 + WIDTH a

SubstWhileDesizing(u,m) ==
    -- arg. m is always nil (historical: EU directive to increase argument lists 1991/XGII)     
    --Replaces all occurrences of matrix m by name in u
    --Taking out any outdated size information as it goes
  u isnt [.,:.] => u
  [[op,:n],:l]:=u
  --name := RASSOC(u,$MatrixList) => name
  -- doesn't work since RASSOC seems to use an EQ test, and returns the
  -- pair anyway. JHD 28/2/93
  op = 'MATRIX =>
    l':=SubstWhileDesizingList(rest l,m)
    u :=
      -- rest l=l' => u
      -- this was a CONS-saving optimisation, but it doesn't work JHD 28/2/93
      [op,nil,:l']
    PushMatrix u
  l':=SubstWhileDesizingList(l,m)
  -- [op,:l']
  op isnt [.,:.] => [op,:l']
  [SubstWhileDesizing(op,m),:l']

--;SubstWhileDesizingList(u,m) ==
--;  -- m is always nil (historical)
--;  u is [a,:b] =>
--;    a':=SubstWhileDesizing(a,m)
--;    b':=SubstWhileDesizingList(b,m)
--;-- MCD & TTT think that this test is unnecessary and expensive
--;--    a=a' and b=b' => u
--;    [a',:b']
--;  u

SubstWhileDesizingList(u,m) ==
   u is [a,:b] =>
     res:= 
       a isnt [.,:.] => [a] 
       [SubstWhileDesizing(a,m)] 
     tail:=res
     for i in b repeat
        if i isnt [.,:.] then tail.rest := [i]
        else tail.rest := [SubstWhileDesizing(i,m)]
        tail:=rest tail
     res   
   u  

--% Printing of Sigmas , Pis and Intsigns

sigmaSub u ==
       --The depth function for sigmas with lower limit only
  MAX(1 + height second u, subspan third u)

sigmaSup u ==
       --The height function for sigmas with lower limit only
  MAX(1, superspan third u)

sigmaApp(u,x,y,d) ==
  u is [.,bot,arg] or THROW('outputFailure,'outputFailure)
  bigopAppAux(bot,nil,arg,x,y,d,'sigma)

sigma2App(u,x,y,d) ==
  [.,bot,top,arg]:=u
  bigopAppAux(bot,top,arg,x,y,d,'sigma)

bigopWidth(bot,top,arg,kind) ==
  kindWidth := (kind = 'pi => 5; 3)
  MAX(kindWidth,WIDTH bot,(top => WIDTH top; 0)) + 2 + WIDTH arg

macro half x ==
  x quo 2

bigopAppAux(bot,top,arg,x,y,d,kind) ==
  botWidth := (bot => WIDTH bot; 0)
  topWidth := WIDTH top
  opWidth :=
    kind = 'pi => 5
    3
  maxWidth := MAX(opWidth,botWidth,topWidth)
  xCenter := half(maxWidth-1) + x
  d:=APP(arg,x+2+maxWidth,y,d)
  d:=
      bot isnt [.,:.] and # atom2String bot = 1 => APP(bot,xCenter,y-2,d)
      APP(bot,x + half(maxWidth - botWidth),y-2-superspan bot,d)
  if top then
    d:=
      top isnt [.,:.] and # atom2String top = 1 => APP(top,xCenter,y+2,d)
      APP(top,x + half(maxWidth - topWidth),y+2+subspan top,d)
  delta := (kind = 'pi => 2; 1)
  opCode :=
    kind = 'sigma =>
      [['(0 .  0),:'">"],_
       ['(0 .  1),:specialChar('hbar)],_
       ['(0 . -1),:specialChar('hbar)],_
       ['(1 .  1),:specialChar('hbar)],_
       ['(1 . -1),:specialChar('hbar)],_
       ['(2 .  1),:specialChar('urc )],_
       ['(2 . -1),:specialChar('lrc )]]
    kind = 'pi =>
      [['(0 .  1),:specialChar('ulc )],_
       ['(1 .  0),:specialChar('vbar)],_
       ['(1 .  1),:specialChar('ttee)],_
       ['(1 . -1),:specialChar('vbar)],_
       ['(2 .  1),:specialChar('hbar)],_
       ['(3 .  0),:specialChar('vbar)],_
       ['(3 .  1),:specialChar('ttee)],_
       ['(3 . -1),:specialChar('vbar)],_
       ['(4 .  1),:specialChar('urc )]]
    THROW('outputFailure,'outputFailure)
  xLate(opCode,xCenter - delta,y,d)

sigmaWidth [.,bot,arg] == bigopWidth(bot,nil,arg,'sigma)
sigma2Width [.,bot,top,arg] == bigopWidth(bot,top,arg,'sigma)

sigma2Sub u ==
       --The depth function for sigmas with 2 limits
  MAX(1 + height second u, subspan fourth u)

sigma2Sup u ==
       --The depth function for sigmas with 2 limits
  MAX(1 + height third u, superspan fourth u)

piSub u ==
       --The depth function for pi's (products)
  MAX(1 + height second u, subspan third u)

piSup u ==
       --The height function for pi's (products)
  MAX(1, superspan third u)

piApp(u,x,y,d) ==
  u is [.,bot,arg] or THROW('outputFailure,'outputFailure)
  bigopAppAux(bot,nil,arg,x,y,d,'pi)

piWidth [.,bot,arg] == bigopWidth(bot,nil,arg,'pi)
pi2Width [.,bot,top,arg] == bigopWidth(bot,top,arg,'pi)

pi2Sub u ==
       --The depth function for pi's with 2 limits
  MAX(1 + height second u, subspan fourth u)

pi2Sup u ==
       --The depth function for pi's with 2 limits
  MAX(1 + height third u, superspan fourth u)

pi2App(u,x,y,d) ==
  [.,bot,top,arg]:=u
  bigopAppAux(bot,top,arg,x,y,d,'pi)

overlabelSuper [.,a,b] == 1 + height a + superspan b

overlabelWidth [.,a,b] == WIDTH b

overlabelApp([.,a,b], x, y, d) ==
  underApp:= APP(b,x,y,d)
  endPoint := x + WIDTH b - 1
  middle := (x + endPoint) quo 2
  h := y + superspan b + 1
  d := APP(a,middle,h + 1,d)
  apphor(x,x+WIDTH b-1,y+superspan b+1,d,"|")

overbarSuper u == 1 + superspan u.1

overbarWidth u == WIDTH u.1

overbarApp(u,x,y,d) ==
  underApp:= APP(u.1,x,y,d)
  apphor(x,x+WIDTH u.1-1,y+superspan u.1+1,d,UNDERBAR)

indefIntegralSub u ==
   -- form is INDEFINTEGRAL(expr,dx)
   MAX(1,subspan u.1,subspan u.2)

indefIntegralSup u ==
   -- form is INDEFINTEGRAL(expr,dx)
   MAX(1,superspan u.1,superspan u.2)

indefIntegralApp(u,x,y,d) ==
   -- form is INDEFINTEGRAL(expr,dx)
  [.,expr,dx]:=u
  d := APP(expr,x+4,y,d)
  d := APP(dx,x+5+WIDTH expr,y,d)
  xLate( [['(0 . -1),:specialChar('llc) ],_
          ['(1 . -1),:specialChar('lrc) ],_
          ['(1 .  0),:specialChar('vbar)],_
          ['(1 .  1),:specialChar('ulc) ],_
          ['(2 .  1),:specialChar('urc) ]], x,y,d)

indefIntegralWidth u ==
  -- form is INDEFINTEGRAL(expr,dx)
  # u ~= 3 => THROW('outputFailure,'outputFailure)
  5 + WIDTH u.1 + WIDTH u.2

intSub u ==
   MAX(1 + height u.1, subspan u.3)

intSup u ==
   MAX(1 + height u.2, superspan u.3)

intApp(u,x,y,d) ==
  [.,bot,top,arg]:=u
  d:=APP(arg,x+4+MAX(-4 + WIDTH bot, WIDTH top),y,d)
  d:=APP(bot,x,y-2-superspan bot,d)
  d:=APP(top,x+3,y+2+subspan top,d)
  xLate( [['(0 . -1),:specialChar('llc) ],_
          ['(1 . -1),:specialChar('lrc) ],_
          ['(1 .  0),:specialChar('vbar)],_
          ['(1 .  1),:specialChar('ulc) ],_
          ['(2 .  1),:specialChar('urc) ]], x,y,d)

intWidth u ==
  # u < 4 => THROW('outputFailure,'outputFailure)
  MAX(-4 + WIDTH u.1, WIDTH u.2) + WIDTH u.3 + 5

xLate(l,x,y,d) ==
  for [[a,:b],:c] in l repeat
    d:= appChar(c,x+a,y+b,d)
  d

concatTrouble(u,d,start,lineLength,$addBlankIfTrue) ==
  [x,:l] := splitConcat(u,lineLength,true)
  null l =>
    sayMSG ['"%l",'"%b",'"  Too wide to Print",'"%d"]
    THROW('output,nil)
  charybdis(fixUp x,start,lineLength)
  for y in l repeat
    if d then prnd(start,d)
    if lineLength > 2 then
       charybdis(fixUp y,start+2,lineLength-2) -- JHD needs this to avoid lunacy
      else charybdis(fixUp y,start,1) -- JHD needs this to avoid lunacy
  BLANK
 where
  fixUp x ==
    rest x =>
      $addBlankIfTrue => ['CONCATB,:x]
      ["CONCAT",:x]
    first x

splitConcat(list,maxWidth,firstTimeIfTrue) ==
  null list => nil
  -- split list l into a list of n lists, each of which
  -- has width < maxWidth
  totalWidth:= 0
  oneOrZero := ($addBlankIfTrue => 1; 0)
  l := list
  maxW:= (firstTimeIfTrue => maxWidth; maxWidth-2)
  maxW < 1 => [[x] for x in l] -- JHD 22.8.95, otherwise things can break
  for x in tails l
    while (width := oneOrZero + WIDTH first x + totalWidth) < maxW repeat
      l:= x
      totalWidth:= width
  x:= rest l
  l.rest := nil
  [list,:splitConcat(x,maxWidth,nil)]

spadPrint(x,m) ==
  m = $NoValueMode => x
  newlineIfDisplaying()
  output(x,m)
  newlineIfDisplaying()

texFormat expr ==
  tf := $TexFormat
  formatFn := 
    getFunctionFromDomain("convert",tf,[$OutputForm,$Integer])
  displayFn := getFunctionFromDomain("display",tf,[tf])
  SPADCALL(SPADCALL(expr,$IOindex,formatFn),displayFn)
  finishLine $texOutputStream
  nil

texFormat1 expr ==
  tf := $TexFormat
  formatFn := getFunctionFromDomain("coerce",tf, [$OutputForm])
  displayFn := getFunctionFromDomain("display",tf,[tf])
  SPADCALL(SPADCALL(expr,formatFn),displayFn)
  finishLine $texOutputStream
  nil

mathmlFormat expr ==
  mml := $MathMLFormat
  mmlrep := $String
  formatFn := getFunctionFromDomain("coerce",mml,[$OutputForm])
  displayFn := getFunctionFromDomain("display",mml,[mmlrep])
  SPADCALL(SPADCALL(expr,formatFn),displayFn)
  finishLine $mathmlOutputStream
  nil

output(expr,domain) ==
  if isWrapped expr then expr := unwrap expr
  isMapExpr expr =>
    if $texFormat     then texFormat expr
    if $mathmlFormat  then mathmlFormat expr
    if $algebraFormat then mathprintWithNumber(expr,domain)
  categoryForm? domain or member(domain,'((Mode) (Domain) (Type))) =>
    if $algebraFormat then
      mathprintWithNumber(outputDomainConstructor expr,domain)
    if $texFormat     then
      texFormat outputDomainConstructor expr
  T := coerceInteractive(objNewWrap(expr,domain),$OutputForm) =>
    x := objValUnwrap T
    if $fortranFormat then
      dispfortexp x
      if not $collectOutput then
        writeNewline $fortranOutputStream
      flushOutput $fortranOutputStream
    if $algebraFormat then
      mathprintWithNumber(x,domain)
    if $texFormat     then texFormat x
    if $mathmlFormat  then mathmlFormat x
  sayMSG [:bright '"LISP",'"output:",'"%l",expr or '"NIL"]

outputNumber(start,linelength,num) ==
  if start > 1 then blnks := fillerSpaces(start-1,char " ")
  else blnks := '""
  under := '"__"
  firsttime:=(linelength>3)
  if linelength>2 then
     linelength:=linelength-1 
  while # num > linelength repeat
    if $collectOutput then
       $outputLines := [strconc(blnks, subString(num,0,linelength),under),
                        :$outputLines]
    else
      sayMSG [blnks, subString(num,0,linelength),under]
    num := subString(num,linelength)
    if firsttime then 
         blnks:=strconc(blnks,'" ")
         linelength:=linelength-1
         firsttime:=nil
  if $collectOutput then
    $outputLines := [strconc(blnks, num), :$outputLines]
  else
    sayMSG [blnks, num]

outputString(start,linelength,str) ==
  if start > 1 then blnks := fillerSpaces(start-1,char " ")
  else blnks := '""
  while # str > linelength repeat
    if $collectOutput then
       $outputLines := [strconc(blnks, subString(str,0,linelength)),
                        :$outputLines]
    else
      sayMSG [blnks, subString(str,0,linelength)]
    str := subString(str,linelength)
  if $collectOutput then
    $outputLines := [strconc(blnks, str), :$outputLines]
  else
    sayMSG [blnks, str]

outputDomainConstructor form ==
  if VECTORP form then form := devaluate form
  (u:= prefix2String form) isnt [.,:.] => u
  v:= [object2String(x) for x in u]
  return makeSymbol apply(function strconc,v)

getOutputAbbreviatedForm form ==
  form is [op,:argl] =>
    op is "Mapping" => formatMapping argl
    builtinConstructor? op => outputDomainConstructor form
    u := getConstructorAbbreviationFromDB op or op
    null argl => u
    ml:= getPartialConstructorModemapSig(op)
    argl:= [fn for x in argl for m in ml] where fn() ==
      categoryForm?(m) => outputDomainConstructor x
      x' := coerceInteractive(objNewWrap(x,m),$OutputForm)
      x' => objValUnwrap x'
      '"unprintableObject"
    [u,:argl]
  form

outputOp x ==
  x is [op,:args] and (GETL(op,"LED") or GETL(op,"NUD")) =>
    n:=
      GETL(op,"NARY") => 2
      #args
    newop:= makeSymbol strconc('"*",toString n,PNAME op)
    [newop,:[outputOp y for y in args]]
  x

--% %Map PRINTER (FROM EV BOOT)

printMap u ==
  printBasic specialChar 'lbrk
  initialFlag:= isInitialMap u
  if u is [x,:l] then
    printMap1(x,initialFlag and x is [[n],:.] and n=1)
    for y in l repeat (printBasic " , "; printMap1(y,initialFlag))
  printBasic specialChar 'rbrk
  newlineIfDisplaying()

isInitialMap u ==
  u is [[[n],.],:l] and integer? n and
    (and/[x is [[ =i],.] for x in l for i in n+1..])

printMap1(x,initialFlag) ==
  initialFlag => printBasic second x
  if CDAR x then printBasic first x else printBasic CAAR x
  printBasic " E "
  printBasic second x

printBasic x ==
  x=$One => writeInteger(1,$algebraOutputStream)
  x=$Zero => writeInteger(0,$algebraOutputStream)
  ident? x => writeString(symbolName x,$algebraOutputStream)
  x isnt [.,:.] => PRIN1(x,$algebraOutputStream)
  PRIN1(x,$algebraOutputStream)

charybdis(u,start,linelength) ==
  keyp u='EQUATNUM and null (CDDR u) =>
    charybdis(['PAREN,u.1],start,linelength)
  charyTop(u,start,linelength)

charyTop(u,start,linelength) ==
  u is ['SC,:l] or u is [['SC,:.],:l] =>
    for a in l repeat charyTop(a,start,linelength)
    '" "
  u is [['CONCATB,:.],:m,[['SC,:.],:l]] =>
    charyTop(['CONCATB,:m],start,linelength)
    charyTop(['SC,:l],start+2,linelength-2)
  u is ['CENTER,a] =>
    b := charyTopWidth a
    (w := WIDTH(b)) > linelength-start => charyTop(a,start,linelength)
    charyTop(b,half(linelength-start-w),linelength)
  v := charyTopWidth u
  keyp u='ELSE => charyElse(u,v,start,linelength)
  WIDTH(v) > linelength => charyTrouble(u,v,start,linelength)
  d := APP(v,start,0,nil)
  n := superspan v
  m := - subspan v
-->
  $testOutputLineFlag =>
    $testOutputLineList :=
      [:ASSOCRIGHT reverse! sortBy(function first,d),:$testOutputLineList]
  until n < m repeat
    scylla(n,d)
    n := n - 1
  '" "

charyTopWidth u ==
    u isnt [.,:.] => u
    first u isnt [.,:.] => putWidth u
    integer? CDAR u => u
    putWidth u

charyTrouble(u,v,start,linelength) ==
  al:= LargeMatrixp(u,linelength,2*linelength) =>
    --$MatrixList =>
      --[[m,:m1]] := al
      --maPrin sublisMatAlist(m,m1,u)
      --above three lines commented out JHD 25/2/93 since don't work
    --u := SubstWhileDesizing(u,first first al)
    u := SubstWhileDesizing(u,nil)
    maprinChk u
  charyTrouble1(u,v,start,linelength)

sublisMatAlist(m,m1,u) ==
  u is [op,:r] =>
    op is ['MATRIX,:.] and u=m => m1
    op1 := sublisMatAlist(m,m1,op)
    r1 := [sublisMatAlist(m,m1,s) for s in r]
    op = op1 and r1 = r => u
    [op1,:r1]
  u

charyTrouble1(u,v,start,linelength) ==
  integer? u => outputNumber(start,linelength,atom2String u)
  u isnt [.,:.] => outputString(start,linelength,atom2String u)
  sameObject?(x:= keyp u,'_-) => charyMinus(u,v,start,linelength)
  x in '(_+ _* AGGLST) => charySplit(u,v,start,linelength)
  x='EQUATNUM => charyEquatnum(u,v,start,linelength)
  d := GETL(x,'INFIXOP) => charyBinary(d,u,v,start,linelength)
  x = 'OVER  =>
    charyBinary(GETL("/",'INFIXOP),u,v,start,linelength)
  3=#u and GETL(x,'Led) =>
    d:= PNAME first GETL(x,'Led)
    charyBinary(d,u,v,start,linelength)
  x='CONCAT =>
    concatTrouble(rest v,d,start,linelength,nil)
  x='CONCATB =>
    (rest v) is [loop, 'repeat, body] =>
      charyTop(['CONCATB,loop,'repeat],start,linelength)
      charyTop(body,start+2,linelength-2)
    (rest v) is [wu, loop, 'repeat, body] and
      (keyp wu) is ['CONCATB,wu',.] and wu' in '(while until) =>
        charyTop(['CONCATB,wu,loop,'repeat],start,linelength)
        charyTop(body,start+2,linelength-2)
    concatTrouble(rest v,d,start,linelength,true)
  GETL(x,'INFIXOP) => charySplit(u,v,start,linelength)
  x='PAREN and
    (sameObject?(keyp u.1,'AGGLST) and (v:= ",") or sameObject?(keyp u.1,'AGGSET) and
      (v:= ";")) => bracketagglist(rest u.1,start,linelength,v,"_(","_)")
  x='PAREN and sameObject?(keyp u.1,'CONCATB) =>
    bracketagglist(rest u.1,start,linelength," ","_(","_)")
  x='BRACKET and (sameObject?(keyp u.1,'AGGLST) and (v:= ",")) =>
    bracketagglist(rest u.1,start,linelength,v,
                   specialChar 'lbrk, specialChar 'rbrk)
  x='BRACE and (sameObject?(keyp u.1,'AGGLST) and (v:= ",")) =>
    bracketagglist(rest u.1,start,linelength,v,
                   specialChar 'lbrc, specialChar 'rbrc)
  x='EXT => longext(u,start,linelength)
  x='MATRIX => MATUNWND()
  x='ELSE => charyElse(u,v,start,linelength)
  x='SC => charySemiColon(u,v,start,linelength)
  charybdis(x,start,linelength)
  if rest u then charybdis(['ELSE,:rest u],start,linelength)
  -- changed from charybdis(...) by JHD 2 Aug 89, since rest u might be null
  '" "

charySemiColon(u,v,start,linelength) ==
  for a in rest u repeat
    charyTop(a,start,linelength)
  nil

charyMinus(u,v,start,linelength) ==
  charybdis('"-",start,linelength)
  charybdis(v.1,start+3,linelength-3)
  '" "

charyBinary(d,u,v,start,linelength) ==
  member(d,'(" := " " = ")) =>
    charybdis(['CONCATB,v.1,d],start,linelength)
    charybdis(v.2,start+2,linelength-2)
    '" "
  charybdis(v.1,start+2,linelength-2)
  if d then prnd(start,d)
  charybdis(v.2,start+2,linelength-2)
  '" "

charyEquatnum(u,v,start,linelength) ==
  charybdis(['PAREN,u.1],start,linelength)
  charybdis(u.2,start,linelength)
  '" "

charySplit(u,v,start,linelength) ==
  v:= [first v.0,:rest v]
  m:= rest v
  WIDTH v.1 > linelength-2 =>
    charybdis(v.1,start+2,linelength-2)
    null (CDDR v) => '" "
    dm:= CDDR v
    ddm:= rest dm
    split2(u,dm,ddm,start,linelength)
  for i in 0.. repeat
    dm := rest m
    ddm := rest dm
    dm.rest := nil
    WIDTH v > linelength - 2 => return nil
    v.first := first v.0
    dm.rest := ddm
    m := rest m
  v.first := first v.0
  m.rest := nil
  charybdis(v,start + 2,linelength - 2)
  split2(u,dm,ddm,start,linelength)

split2(u,dm,ddm,start,linelength) ==
--prnd(start,(d:= GETL(keyp u,'INFIXOP) => d; opSrch(keyp u,OPLIST)))
  prnd(start,(d:= GETL(keyp u,'INFIXOP) => d; '","))
  dm.rest := ddm
  m:= WIDTH [keyp u,:dm]<linelength-2
  charybdis([keyp u,:dm],(m => start+2; start),(m => linelength-2; linelength))
  '" "

charyElse(u,v,start,linelength) ==
  charybdis(v.1,start+3,linelength-3)
  null (CDDR u) => '" "
  prnd(start,'",")
  charybdis(['ELSE,:CDDR v],start,linelength)
  '" "

scylla(n,v) ==
  y := LASSOC(n,v)
  null y => nil
  if string?(y) then y := trimTrailingBlank copyString y
  if $collectOutput then
    $outputLines := [y, :$outputLines]
  else
    PRINC(y,$algebraOutputStream)
    writeNewline $algebraOutputStream
  nil

keyp(u) ==
  u isnt [.,:.] => nil
  first u isnt [.,:.] => first u
  CAAR u

absym x ==
  integer? x and (x < 0) => -x
  cons? x and (keyp(x) = '_-) => second x
  x

agg(n,u) ==
  (n = 1) => second u
  agg(n - 1, rest u)

aggwidth u ==
  null u => 0
  null rest u => WIDTH first u
  1 + (WIDTH first u) + (aggwidth rest u)

argsapp(u,x,y,d) == appargs(rest u,x,y,d)

subspan u ==
  u isnt [.,:.] => 0
  integer? rest u => subspan first u
  (cons? first u             and_
   CAAR u isnt [.,:.]        and_
   not integer? CAAR u    and_
   GETL(CAAR u, 'SUBSPAN)    )    =>
   apply(GETL(CAAR u, 'SUBSPAN), [u])
  MAX(subspan first u, subspan rest u)

agggsub u == subspan rest u

superspan u ==
  u isnt [.,:.] => 0
  integer? rest u => superspan first u
  (cons? first u               and_
   CAAR u isnt [.,:.]          and_
   not integer? CAAR u      and_
   GETL(CAAR u, 'SUPERSPAN)    )    =>
   apply(GETL(CAAR u, 'SUPERSPAN), [u])
  MAX(superspan first u, superspan rest u)

agggsuper u == superspan rest u

agggwidth u == aggwidth rest u

appagg(u,x,y,d) == appagg1(u,x,y,d,'",")

appagg1(u,x,y,d,s) ==
  null u => d
  null rest u => APP(first u,x,y,d)
  temp := x + WIDTH first u
  temparg1 := APP(first u,x,y,d)
  temparg2 := APP(s,temp,y,temparg1)
  appagg1(rest u, 1 + temp, y, temparg2,s)

--Note the similarity between the definition below of appargs and above
--of appagg. (why?)

appargs(u,x,y,d) == appargs1(u,x,y,d,'";")

--Note that the definition of appargs1 below is identical to that of
--appagg1 above except that the former calls appargs and the latter
--calls appagg.

appargs1(u,x,y,d,s) ==
  null u => d
  null rest u => APP(first u,x,y,d)
  temp := x + WIDTH first u
  temparg1 := APP(first u,x,y,d)
  temparg2 := APP(s,temp,y,temparg1)
  true => appargs(rest u, 1 + temp, y, temparg2)

apprpar(x, y, y1, y2, d) ==
  (null (_*TALLPAR) or (y2 - y1 < 2)) => APP('")", x, y, d)
  true => APP('")", x, y2, apprpar1(x, y, y1, y2 - 1, d))

apprpar1(x, y, y1, y2, d) ==
  (y1 = y2) => APP('")", x, y2, d)
  true => APP('")", x, y2, apprpar1(x, y, y1, y2 - 1, d))

applpar(x, y, y1, y2, d) ==
  (null (_*TALLPAR) or (y2 - y1 < 2)) => APP('"(", x, y, d)
  true => APP('"(", x, y2, applpar1(x, y, y1, y2 - 1, d))

applpar1(x, y, y1, y2, d) ==
  (y1 = y2) => APP('"(", x, y2, d)
  true => APP('"(", x, y2, applpar1(x, y, y1, y2 - 1, d))

--The body of the function appelse assigns 6 local variables.
--It then finishes by calling apprpar.

appelse(u,x,y,d) ==
  w := WIDTH CAAR u
  b := y - subspan rest u
  p := y + superspan rest u
  temparg1 := APP(keyp u, x, y, d)
  temparg2 := applpar(x + w, y, b, p, temparg1)
  temparg3 := appagg(rest u, x + 1 + w, y, temparg2)
  apprpar(x + 1 + w + aggwidth rest u, y, b, p, temparg3)

appext(u,x,y,d) ==
  xptr := x
  yptr := y - (subspan second u + superspan agg(3,u) + 1)
  d := APP(second u,x,y,d)
  d := APP(agg(2,u),xptr,yptr,d)
  xptr := xptr + WIDTH agg(2,u)
  d := APP('"=", xptr, yptr,d)
  d := APP(agg(3,u), 1 + xptr, yptr, d)
  yptr := y + 1 + superspan second u + SUBSPAD agg(4,u)
  d := APP(agg(4,u), x, yptr, d)
  temp := 1 + WIDTH agg(2,u) +  WIDTH agg(3,u)
  n := MAX(WIDTH second u, WIDTH agg(4,u), temp)
  if first(z := agg(5,u)) is ["EXT",:.] and
   (n=3 or (n > 3 and cons? z) ) then
     n := 1 + n
  d := APP(z, x + n, y, d)

apphor(x1,x2,y,d,char) ==
  temp := (x1 = x2 => d; apphor(x1, x2 - 1, y, d,char))
  APP(char, x2, y, temp)

syminusp x ==
  integer? x => x < 0
  cons? x and sameObject?(keyp x,'_-)

appsum(u, x, y, d) ==
  null u => d
  ac := absym first u
  sc :=
    syminusp first u => '"-"
    true => '"+"
  dp := member(keyp absym first u, '(_+ _-))
  tempx := x + WIDTH ac + (dp => 5; true => 3)
  tempdblock :=
    temparg1 := APP(sc, x + 1, y, d)
    dp =>
      bot := y - subspan ac
      top := y + superspan ac
      temparg2 := applpar(x + 3, y, bot, top, temparg1)
      temparg3 := APP(ac, x + 4, y, temparg2)
      apprpar(x + 4 + WIDTH ac, y, bot, top, temparg3)
    true => APP(ac, x + 3, y, temparg1)
  appsum(rest u, tempx, y, tempdblock)

appneg(u, x, y, d) ==
  appsum([u], x - 1, y, d)

appparu(u, x, y, d) ==
  bot := y - subspan u
  top := y + superspan u
  temparg1 := applpar(x, y, bot, top, d)
  temparg2 := APP(u, x + 1, y, temparg1)
  apprpar(x + 1 + WIDTH u, y, bot, top, temparg2)

appparu1(u, x, y, d) ==
  appparu(second u, x, y, d)

appsc(u, x, y, d) ==
  appagg1(rest u, x, y, d, '";")

appsetq(u, x, y, d) ==
  w := WIDTH first u
  temparg1 := APP(second u, x, y, d)
  temparg2 := APP('":", x + w, y, temparg1)
  APP(second rest u, x + 2 + w, y, temparg2)

appsub(u, x, y, d) ==
  temparg1 := x + WIDTH second u
  temparg2 := y - 1 - superspan CDDR u
  temparg3 := APP(second u, x, y, d)
  appagg(CDDR u, temparg1, temparg2, temparg3)

eq0(u) == 0

height(u) ==
  superspan(u) + 1 + subspan(u)

extsub(u) ==
  MAX(subspan agg(5, u), height(agg(3, u)), subspan second u  )

extsuper(u) ==
  MAX(superspan second u + height agg(4, u), superspan agg(5, u) )

extwidth(u) ==
  n := MAX(WIDTH second u,
           WIDTH agg(4, u),
           1 + WIDTH agg(2, u) + WIDTH agg(3, u) )
  nil or
         (first(z := agg(5, u)) is ["EXT",:.] and _
          (n=3 or ((n > 3) and cons? z) )  =>
          n := 1 + n)
  true => n + WIDTH agg(5, u)

appfrac(u, x, y, d) ==
  -- Added "1+" to both QUOTIENT statements so that when exact centering is
  -- not possible, expressions are offset to the right rather than left.
  -- MCD 16-8-95
  w := WIDTH u
  tempx := x + (1+w - WIDTH second rest u) quo 2
  tempy := y - superspan second rest u - 1
  temparg3 := APP(second rest u, tempx, tempy, d)
  temparg4 := apphor(x, x + w - 1, y, temparg3,specialChar('hbar))
  APP(second u,
        x + (1+w - WIDTH second u) quo 2,
          y + 1 + subspan second u,
            temparg4)

fracsub(u) == height second rest u

fracsuper(u) == height second u

fracwidth(u) ==
  numw := WIDTH (num := second u)
  denw := WIDTH (den := third u)
  if num is [[op,:.],:.] and op = 'OVER then numw := numw + 2
  if den is [[op,:.],:.] and op = 'OVER then denw := denw + 2
  MAX(numw,denw)

slashSub u ==
  MAX(1,subspan(second u),subspan(second rest u))

slashSuper u ==
  MAX(1,superspan(second u),superspan(second rest u))

slashApp(u, x, y, d) ==
  -- to print things as a/b as opposed to
  --      a
  --      -
  --      b
  temparg1 := APP(second u, x, y, d)
  temparg2 := APP('"/", x + WIDTH second u, y, temparg1)
  APP(second rest u,
     x + 1 + WIDTH second u, y, temparg2)

slashWidth(u) ==
  -- to print things as a/b as opposed to
  --      a
  --      -
  --      b
  1 + WIDTH second u + WIDTH second rest u

longext(u, i, n) ==
  x := reverse u
  y := first x
  u := remWidth(REVERSEWOC(['" ",:rest x]))
  charybdis(u, i, n)
  newlineIfDisplaying()
  charybdis(['ELSE, :[y]], i, n)
  '" "

appvertline(char, x, yl, yu, d) ==
  yu < yl => d
  temparg :=  appvertline(char, x, yl, yu - 1, d)
  true => APP(char, x, yu, temparg)

appHorizLine(xl, xu, y, d) ==
  xu < xl => d
  temparg :=  appHorizLine(xl, xu - 1, y, d)
  true => APP(MATBORCH, xu, y, temparg)

rootApp(u, x, y, d) ==
  widB := WIDTH u.1
  supB := superspan u.1
  subB := subspan u.1
  if #u > 2 then
    widR := WIDTH u.2
    subR := subspan u.2
    d    := APP(u.2,  x, y - subB + 1 + subR, d)
  else
    widR := 1
  d := APP(u.1, x + widR + 1, y, d)
  d := apphor(x+widR+1, x+widR+widB, y+supB+1, d, specialChar('hbar))
  d := appvertline(specialChar('vbar), x+widR, y - subB, y + supB, d)
  d := APP(specialChar('ulc), x+widR, y + supB+1, d)
  d := APP(specialChar('urc), x + widR + widB + 1, y + supB+1, d)
  d := APP(specialChar('bslash), x + widR - 1, y - subB, d)

boxApp(u, x, y, d) ==
  CDDR u => boxLApp(u, x, y, d)
  a := 1 + superspan u.1
  b := 1 + subspan u.1
  w := 2 + WIDTH u.1
  d := appvertline(specialChar('vbar), x,y - b + 1, y + a - 1, d)
  d := appvertline(specialChar('vbar), x + w + 1, y - b,y + a,d)
  d := apphor(x + 1, x + w, y - b, d, specialChar('hbar))
  d := apphor(x + 1, x + w, y + a, d, specialChar('hbar))
  d := APP(specialChar('ulc), x,         y + a, d)
  d := APP(specialChar('urc), x + w + 1, y + a, d)
  d := APP(specialChar('llc), x,         y - b, d)
  d := APP(specialChar('lrc), x + w + 1, y - b, d)
  d := APP(u.1, 2 + x, y, d)

boxLApp(u, x, y, d) ==
  la := superspan u.2
  lb := subspan u.2
  lw := 2 + WIDTH u.2
  lh := 2 + la + lb
  a := superspan u.1+1
  b := subspan u.1+1
  w := MAX(lw, 2 + WIDTH u.1)
  top := y + a + lh
  d := appvertline(MATBORCH, x, y - b, top, d)
  d := appHorizLine(x + 1, x + w, top, d)
  d := APP(u.2, 2 + x, y + a + lb + 1, d)
  d := appHorizLine(x + 1, x + lw, y + a, d)
  nil or
     lw < w => d := appvertline(MATBORCH, x + lw + 1, y + a, top - 1, d)
  d := APP(u.1, 2 + x, y, d)
  d := appHorizLine(x + 1, x + w, y - b, d)
  d := appvertline(MATBORCH, x + w + 1, y - b, top, d)

boxSub(x) ==
  subspan x.1+1

boxSuper(x) ==
  null rest x => 0
  hl :=
    null CDDR x => 0
    true => 2 + subspan x.2 + superspan x.2
  true => hl+1 + superspan x.1

boxWidth(x) ==
  null rest x => 0
  wl :=
    null CDDR x => 0
    true => WIDTH x.2
  true => 4 + MAX(wl, WIDTH x.1)

nothingWidth x ==
    0
nothingSuper x ==
    0
nothingSub x ==
    0
nothingApp(u, x, y, d) ==
    d

zagApp(u, x, y, d) ==
    w := WIDTH u
    denx := x + (w - WIDTH second rest u) quo 2
    deny := y - superspan second rest u - 1
    d    := APP(second rest u, denx, deny, d)
    numx := x + (w - WIDTH second u) quo 2
    numy := y+1 + subspan second u
    d    := APP(second u, numx, numy, d)
    a := 1 + zagSuper u
    b := 1 + zagSub u
    d := appvertline(specialChar('vbar), x,         y - b, y - 1, d)
    d := appvertline(specialChar('vbar), x + w - 1, y + 1, y + a, d)
    d := apphor(x, x + w - 2, y, d, specialChar('hbar))
    d := APP(specialChar('ulc), x, y, d)
    d := APP(specialChar('lrc), x + w - 1, y, d)

zagSub(u) ==
    height second rest u

zagSuper(u) ==
    height second u

zagWidth(x) ==
   #x = 1 => 0
   #x = 2 => 4 + WIDTH x.1
   4 + MAX(WIDTH x.1, WIDTH x.2)

rootWidth(x) ==
   #x <= 2 => 3 + WIDTH x.1
   2 + WIDTH x.1 + WIDTH x.2

rootSub(x) ==
   subspan x.1

rootSuper(x) ==
   normal := 1 + superspan x.1
   #x <= 2 => normal
   (radOver := height x.2 - height x.1) < 0 => normal
   normal + radOver

appmat(u, x, y, d) ==
   rows := CDDR u
   p := matSuper u
   q := matSub u
   d := matrixBorder(x, y - q, y + p, d, 'left)
   x := 1 + x
   yc := 1 + y + p
   w := second u
   wl := CDAR w
   subl := rest second w
   superl := rest second rest w
   repeat
      null rows => return(matrixBorder(x + WIDTH u - 2,
                                       y - q,
                                       y + p,
                                       d,
                                       'right))
      xc := x
      yc := yc - 1 - first superl
      w := wl
      row := CDAR rows
      repeat
            if flag = '"ON" then
               flag := '"OFF"
               return(nil)
            null row =>
                  repeat
                     yc := yc - 1 - first subl
                     subl := rest subl
                     superl := rest superl
                     rows := rest rows
                     return(flag  := '"ON"; nil)
            d := APP(first row,
                     xc + (first w - WIDTH first row) quo 2,
                     yc,
                     d)
            xc := xc + 2 + first w
            row := rest row
            w := rest w

matSuper(x) ==
  (x := x.1) => -1 + (first x.1 + first x.2) quo 2
  true => ERROR('MAT)

matSub(x) ==
  (x := x.1) => (-1 + first x.1 + first x.2) quo 2
  true => ERROR('MAT)

matWidth(x) ==
  y := CDDR x  -- list of rows, each of form ((ROW . w) element element ...)
  numOfColumns := # CDAR y
  widthList := matLSum2 matWList(y, [0 for . in 1..numOfColumns])
    --returns ["max width of entries in column i" for i in 1..numberOfRows]
  subspanList := matLSum matSubList y
  superspanList := matLSum matSuperList y
  x.rest.first := [widthList, subspanList, superspanList]
  CAAR x.1

matLSum(x) ==
  [sumoverlist x + # x,:x]

matLSum2(x) ==
  [sumoverlist x + 2*(# x),:x]

matWList(x, y) ==
  null x => y
  true => matWList(rest x, matWList1(CDAR x, y) )

matWList1(x, y) ==
  null x => nil
  true => [MAX(WIDTH first x, first y),:matWList1(rest x, rest y)]

matSubList(x) ==  --computes the max/[subspan(e) for e in "row named x"]
  null x => nil
  true => [matSubList1(CDAR x, 0),:matSubList(rest x)]

matSubList1(x, y) ==
  null x => y
  true => matSubList1(rest x, MAX(y, subspan first x) )

matSuperList(x) ==  --computes the max/[superspan(e) for e in "row named x"]
  null x => nil
  true => [matSuperList1(CDAR x, 0),:matSuperList(rest x)]

matSuperList1(x, y) ==
  null x => y
  true => matSuperList1(rest x, MAX(y, superspan first x) )

minusWidth(u) ==
  -1 + sumWidthA rest u

-- opSrch(name, x) ==
--   LASSOC(name, x) or '","

bracketagglist(u, start, linelength, tchr, open, close) ==
  u := [['CONCAT, open, first u],
           :[['CONCAT, '" ", y] for y in rest u]]
  repeat
    s := 0
    for x in tails u repeat
             lastx := x
             ((s := s + WIDTH first x + 1) >= linelength) => return(s)
             null rest x => return(s := -1)
    nil or
       s = -1 => (nextu := nil)
       sameObject?(lastx, u) => ((nextu := rest u); u.rest := nil)
       true => ((nextu := lastx); PREDECESSOR(lastx, u).rest := nil)
    for x in tails u repeat
           x.first := ['CONCAT, first x, tchr]
    if null nextu then last(u).rest.rest.first := close
    x := ASSOCIATER('CONCAT, [ichr,:u])
    charybdis(ASSOCIATER('CONCAT, u), start, linelength)
    newlineIfDisplaying()
    ichr := '" "
    u := nextu
    null u => return(nil)

prnd(start, op) ==
-->
  $testOutputLineFlag =>
    string := strconc(fillerSpaces MAX(0,start - 1),op)
    $testOutputLineList := [string,:$testOutputLineList]
  writeString(fillerSpaces MAX(0,start - 1),$algebraOutputStream)
  $collectOutput =>
    string := strconc(fillerSpaces MAX(0,start - 1),op)
    $outputLines := [string, :$outputLines]
  PRINC(op,$algebraOutputStream)
  writeNewline $algebraOutputStream

qTSub(u) ==
  subspan second u

qTSuper(u) ==
  superspan second u

qTWidth(u) ==
  2 + WIDTH second u

remWidth(x) ==
  x isnt [.,:.] => x
  true => [(first x isnt [.,:.] => first x; true => CAAR x),
              :[remWidth y for y in rest x]]

subSub(u) ==
  height CDDR u

subSuper u ==
  superspan u.1

letWidth u ==
  5 + WIDTH u.1 + WIDTH u.2

sumoverlist(u) == +/[x for x in u]

sumWidth u ==
  WIDTH u.1 + sumWidthA CDDR u

sumWidthA u ==
  null u => 0
  ( member(keyp absym first u,'(_+ _-)) => 5; true => 3) +
    WIDTH absym first u +
      sumWidthA rest u

superSubApp(u, x, y, di) ==
  a := first (u := rest u)
  b := first (u := rest u)
  c := first (u := KDR u) or '((NOTHING . 0))
  d := KAR   (u := KDR u) or '((NOTHING . 0))
  e := KADR  u            or '((NOTHING . 0))
  aox := MAX(wd := WIDTH d, we := WIDTH e)
  ar := superspan a
  ab := subspan a
  aw := WIDTH a
  di := APP(d, x + (aox - wd), 1 + ar + y + subspan d, di)
  di := APP(a, x + aox, y, di)
  di := APP(c, aox + aw + x, 1 + y + ar + subspan c, di)
  di := APP(e, x + (aox - we), y - 1 - MAX(superspan e, ab), di)
  di := APP(b, aox + aw + x, y - 1 - MAX(ab, superspan b), di)
  return di

stringer x ==
  string? x => x
  char "|" = stringChar(s := STRINGIMAGE x, 0) =>
    RPLACSTR(s, 0, 1, "", nil, nil)
  s

superSubSub u ==
  a:= first (u:= rest u)
  b:= KAR (u := KDR u)
  e:= KAR KDR KDR KDR u
  return subspan a + MAX(height b, height e)

binomApp(u,x,y,d) ==
  [num,den] := rest u
  ysub := y - 1 - superspan den
  ysup := y + 1 + subspan num
  wden := WIDTH den
  wnum := WIDTH num
  w := MAX(wden,wnum)
  d := APP(den,x+1+ half(w - wden),ysub,d)
  d := APP(num,x+1+ half(w - wnum),ysup,d)
  hnum := height num
  hden := height den
  w := 1 + w
  for j in 0..(hnum - 1) repeat
    d := appChar(specialChar 'vbar,x,y + j,d)
    d := appChar(specialChar 'vbar,x + w,y + j,d)
  for j in 1..(hden - 1) repeat
    d := appChar(specialChar 'vbar,x,y - j,d)
    d := appChar(specialChar 'vbar,x + w,y - j,d)
  d := appChar(specialChar 'ulc,x,y + hnum,d)
  d := appChar(specialChar 'urc,x + w,y + hnum,d)
  d := appChar(specialChar 'llc,x,y - hden,d)
  d := appChar(specialChar 'lrc,x + w,y - hden,d)

binomSub u == height third u
binomSuper u == height second u
binomWidth u == 2 + MAX(WIDTH second u, WIDTH third u)

altSuperSubApp(u, x, y, di) ==
  a  := first (u := rest u)
  ar := superspan a
  ab := subspan a
  aw := WIDTH a
  di := APP(a, x, y, di)
  x  := x + aw

  sublist := everyNth(u := rest u, 2)
  suplist := everyNth(IFCDR u, 2)

  ysub := y - 1 - apply('MAX, [ab, :[superspan s for s in sublist]])
  ysup := y + 1 + apply('MAX, [ar, :[subspan   s for s in sublist]])
  for sub in sublist for sup in suplist repeat
      wsub := WIDTH sub
      wsup := WIDTH sup
      di := APP(sub, x, ysub, di)
      di := APP(sup, x, ysup, di)
      x := x + 1 + MAX(wsub, wsup)
  di

everyNth(l, n) ==
    [(e := l.0; for i in 1..n while l repeat l := rest l; e) while l]


altSuperSubSub u ==
  span := subspan second u
  sublist := everyNth(CDDR u, 2)
  for sub in sublist repeat
      h := height sub
      if h > span then span := h
  span

altSuperSubSuper u ==
  span := superspan second u
  suplist := everyNth(IFCDR CDDR u, 2)
  for sup in suplist repeat
      h := height sup
      if h > span then span := h
  span

altSuperSubWidth u ==
  w := WIDTH second u
  suplist := everyNth(IFCDR CDDR u, 2)
  sublist := everyNth(CDDR u, 2)
  for sup in suplist for sub in sublist repeat
      wsup := WIDTH sup
      wsub := WIDTH sub
      w := w + 1 + MAX(wsup, wsub)
  w

superSubWidth u ==
  a := first (u := rest u)
  b := first (u := rest u)
  c := first (u := KDR u) or '((NOTHING . 0))
  d := KAR   (u := KDR u) or '((NOTHING . 0))
  e := KADR  u            or '((NOTHING . 0))
  return MAX(WIDTH d, WIDTH e) + MAX(WIDTH b, WIDTH c) + WIDTH a

superSubSuper u ==
  a:= first (u := rest u)
  c:= KAR (u := KDR KDR u)
  d:= KADR u
  return superspan a + MAX(height c, height d)

suScWidth u ==
  WIDTH u.1 + aggwidth CDDR u

vconcatapp(u, x, y, d) ==
  w := vConcatWidth u
  y := y + superspan u.1 + 1
  for a in rest u repeat
      y := y - superspan a - 1
      xoff := (w - WIDTH a) quo 2
      d := APP(a, x + xoff, y, d)
      y := y - subspan a
  d

binomialApp(u, x, y, d) ==
  [.,b,a] := u
  w := vConcatWidth u
  d := APP('"(",x,y,d)
  x := x + 1
  y1 := y - height a
  xoff := (w - WIDTH a) quo 2
  d := APP(a, x + xoff, y1, d)
  y2 := y + height b
  xoff := (w - WIDTH b) quo 2
  d := APP(b, x + xoff, y2, d)
  x := x + w
  APP('")",x,y,d)

vConcatSub u ==
  subspan u.1 + +/[height a for a in CDDR u]
vConcatSuper u ==
  superspan u.1
vConcatWidth u ==
  w := 0
  for a in rest u repeat if (wa := WIDTH a) > w then w := wa
  w
binomialSub u ==  height u.2 + 1

binomialSuper u == height u.1 + 1

binomialWidth u == 2 + MAX(WIDTH u.1, WIDTH u.2)

mathPrint u ==
  newlineIfDisplaying()
  (u := string? mathPrint1(mathPrintTran u, nil) =>
   PSTRING u; nil)

mathPrintTran u ==
  u isnt [.,:.] => u
  true =>
    for x in tails u repeat
          x.first := mathPrintTran first x
    u

mathPrint1(x,fg) ==
  if fg then newlineIfDisplaying()
  maPrin x
  if fg then newlineIfDisplaying()

maPrin u ==
  null u => nil
-->
  if $runTestFlag or $mkTestFlag then
    $mkTestOutputStack := [copyTree u, :$mkTestOutputStack]
  $highlightDelta := 0
  c := CATCH('outputFailure,charybdis(u, $MARGIN, $LINELENGTH))
  c ~= 'outputFailure => c
  sayKeyedMsg("S2IX0009",nil)
  u is ['EQUATNUM,num,form] or u is [['EQUATNUM,:.],num,form] =>
    charybdis(['EQUATNUM,num], $MARGIN, $LINELENGTH)
    if not $collectOutput then
      writeNewline $algebraOutputStream
      PRETTYPRINT(form,$algebraOutputStream)
    form
  if not $collectOutput then
    PRETTYPRINT(u,$algebraOutputStream)
  nil


--% Rendering of InputForm

$allClassicOps == 
  ["~","#","not","**","^","*","/","rem","quo","+","-","@","::", "pretend"]

isUnaryPrefix op ==
  op in '(_~ _# _- _not)

primaryForm2String x ==
  x = nil => '""
  string? x => x
  x = $EmptyMode => specialChar 'quad
  ident? x => 
    x = "$" => '"%"
    x = "$$" => '"%%"
    symbolName x
  x isnt [.,:.] => toString x
  strconc('"(",inputForm2String x, '")")

callForm2String x ==
  x isnt [.,:.] => primaryForm2String x
  [op,:args] := x

  member(op,$allClassicOps) => primaryForm2String x

  #args = 0 =>
    op = "Zero" => '"0"
    op = "One" => '"1"
    constructor? op => primaryForm2String op
    strconc(inputForm2String op, '"()")
  op = "$elt" => typedForm2String("$", second args, first args)
  op is ["$elt",t,op'] => typedForm2String("$",[op',:args], t)
  "strconc"/[inputForm2String op, '"(",:args','")"] where
    args' := [stringify(a,i) for a in args for i in 0..]
    stringify(a,i) ==
      i = 0 => inputForm2String a
      strconc('",",inputForm2String a)
  
typedForm2String(s,x,t) ==
  s = "pretend" =>
    strconc(callForm2String x, '" pretend ", callForm2String t)
  strconc(callForm2String x, symbolName s, callForm2String t)

expForm2String x ==
  x is [op,lhs,rhs] and op in '(** _^) =>
    strconc(expForm2String lhs,'"^", callForm2String rhs)
  callForm2String x

unaryForm2String x ==
  x is [op,arg] and isUnaryPrefix op =>
    strconc(inputForm2String op, inputForm2String arg)
  expForm2String x

multForm2String x ==
  x isnt ["*",lhs,rhs] => unaryForm2String x
  strconc(multForm2String lhs,'"*", multForm2String rhs)

divForm2String x ==
  x isnt ["/",lhs,rhs] => multForm2String x
  strconc(divForm2String lhs,'"/", expForm2String rhs)

remForm2String x ==
  x isnt ["rem",lhs,rhs] => divForm2String x  
  strconc(divForm2String lhs,'" rem ", multForm2String rhs)

quoForm2String x ==
  x isnt ["quo",lhs,rhs] => remForm2String x
  strconc(quoForm2String lhs,'" quo ", remForm2String rhs)

plusForm2String x ==
  x isnt ["+",lhs,rhs] => quoForm2String x
  strconc(plusForm2String lhs,'" + ", plusForm2String rhs)
  
minusForm2String x ==
  x isnt ["-",lhs,rhs] => plusForm2String x
  strconc(minusForm2String lhs,'" - ", minusForm2String rhs)

parms2String x ==
  null x => "()"
  ident? x => x
  x is [var] => var
  if x is ["tuple",:.] then x := rest x
  paren [parm xs for xs in tails x] where
    paren l == "strconc"/['"(",:l,'")"]
    parm xs == 
      null rest xs => first xs
      strconc(first xs, '", ")

inputForm2String x ==
  x isnt [.,:.] => primaryForm2String x
  [op,:args] := x
  isUnaryPrefix op and #args = 1 => unaryForm2String x
  #args = 2 =>
    op in '(** _^) => expForm2String x
    op = "*" => multForm2String x
    op = "/" => divForm2String x
    op = "rem" => remForm2String x
    op = "quo" => quoForm2String x
    op = "+" => plusForm2String x
    op = "-" => minusForm2String x
    op in '(_@ _:_: pretend) =>
      typedForm2String(op, first args, second args)
    op = "+->" =>
       strconc(parms2String second x, '" ", first x, '" ",
          inputForm2String third x)
    callForm2String x
  callForm2String x

inputForm2OutputForm x ==
  makeSymbol inputForm2String x

-- function for turning strings in tex format

str2Outform s ==
  parse := ncParseFromString s or systemError '"String for TeX will not parse"
  parse2Outform parse

parse2Outform x ==
  x is [op,:argl] =>
    nargl := [parse2Outform y for y in argl]
    op = 'construct => ['BRACKET,['ARGLST,:[parse2Outform y for y in argl]]]
    op = 'brace and nargl is [[BRACKET,:r]] => ['BRACE,:r]
    [op,:nargl]
  x

str2Tex s ==
  outf := str2Outform s
  val := coerceInt(objNew(wrap outf, '(OutputForm)), '(TexFormat))
  val := objValUnwrap val
  first val.1

