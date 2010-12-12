-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2010, Gabriel Dos Reis.
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


import macros
namespace BOOT

--% Functions for display formatting system objects

-- some of these are redundant and should be compacted
$formatSigAsTeX := 1
$permitWhere := false

++
$abbreviateJoin := false

--% Formatting modemaps

sayModemap m ==
  -- sayMSG formatModemap displayTranModemap m
  sayMSG formatModemap old2NewModemaps displayTranModemap m

sayModemapWithNumber(m,n) ==
  msg := reverse cleanUpSegmentedMsg reverse ["%i","%i",'" ",
    strconc(lbrkSch(),object2String n,rbrkSch()),
      :formatModemap displayTranModemap m,"%u","%u"]
  sayMSG flowSegmentedMsg(reverse msg,$LINELENGTH,3)

displayOpModemaps(op,modemaps) ==
  TERPRI()
  count:= #modemaps
  phrase:= (count=1 => 'modemap;'modemaps)
  sayMSG ['"%b",count,'"%d",phrase,'" for",'"%b",op,'"%d",'":"]
  for modemap in modemaps repeat sayModemap modemap

displayTranModemap (mm is [[x,:sig],[pred,:y],:z]) ==
  -- The next 8 lines are a HACK to deal with the "partial" definition
  -- JHD/RSS
  if pred is ['partial,:pred'] then
    [b,:c]:=sig
    sig:=[['Union,b,'"failed"],:c]
    mm:=[[x,:sig],[pred',:y],:z]
  else if pred = 'partial then
    [b,:c]:=sig
    sig:=[['Union,b,'"failed"],:c]
    mm:=[[x,:sig],y,:z]
  mm' := EQSUBSTLIST('(m n p q r s t i j k l),
    MSORT listOfPredOfTypePatternIds pred,mm)
  EQSUBSTLIST('(D D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13 D14),
    MSORT listOfPatternIds [sig,[pred,:y]],mm')

listOfPredOfTypePatternIds p ==
  p is ['AND,:lp] or p is ['OR,:lp] =>
    UNIONQ([:listOfPredOfTypePatternIds p1 for p1 in lp],NIL)
  p is [op,a,.] and op = 'ofType =>
    isPatternVar a => [a]
    nil
  nil

removeIsDomains pred ==
  pred is ['isDomain,a,b] => true
  pred is ['AND,:predl] =>
    MKPF([x for x in predl | x isnt ['isDomain,:.]],'AND)
  pred

canRemoveIsDomain? pred ==
  -- returns nil OR an alist for substitutions of domains ordered so that
  -- after substituting for each pair in turn, no left-hand names remain
  alist :=
    pred is ['isDomain,a,b] => [[a,:b],:alist]
    pred is ['AND,:predl] =>
      [[a,:b] for pred in predl | pred is ['isDomain,a,b]]
  findSubstitutionOrder? alist

findSubstitutionOrder? alist == fn(alist,nil) where
  -- returns NIL or an appropriate substituion order
  fn(alist,res) ==
    null alist => nreverse res
    choice := or/[x for (x:=[a,:b]) in alist | null containedRight(a,alist)] =>
      fn(delete(choice,alist),[choice,:res])
    nil

containedRight(x,alist)== or/[CONTAINED(x,y) for [.,:y] in alist]

removeIsDomainD pred ==
  pred is ['isDomain,'D,D] =>
    [D,nil]
  pred is ['AND,:preds] =>
    D := nil
    for p in preds while not D repeat
      p is ['isDomain,'D,D1] =>
        D := D1
        npreds := delete(['isDomain,'D,D1],preds)
    D =>
      1 = #npreds => [D,first npreds]
      [D,['AND,:npreds]]
    nil
  nil

formatModemap modemap ==
  [[dc,target,:sl],pred,:.]:= modemap
  if alist := canRemoveIsDomain? pred then
    dc:= substInOrder(alist,dc)
    pred:= substInOrder(alist,removeIsDomains pred)
    target:= substInOrder(alist,target)
    sl:= substInOrder(alist,sl)
  else if removeIsDomainD pred is [D,npred] then
    pred := substitute(D,'D,npred)
    target := substitute(D,'D,target)
    sl := substitute(D,'D,sl)
  predPart:= formatIf pred
  targetPart:= prefix2String target
  argTypeList:=
    null sl => nil
    concat(prefix2String first sl,fn(rest sl)) where
      fn l ==
        null l => nil
        concat(",",prefix2String first l,fn rest l)
  argPart:=
    #sl<2 => argTypeList
    ['"_(",:argTypeList,'"_)"]
  fromPart:=
    if dc = 'D and D
      then concat('"%b",'"from",'"%d",prefix2String D)
      else concat('"%b",'"from",'"%d",prefix2String dc)
  firstPart:= concat('" ",argPart,'" -> ",targetPart)
  sayWidth firstPart + sayWidth fromPart > 74 => --allow 5 spaces for " [n]"
    fromPart:= concat('" ",fromPart)
    secondPart :=
      sayWidth fromPart + sayWidth predPart < 75 =>
        concat(fromPart,predPart)
      concat(fromPart,'"%l",predPart)
    concat(firstPart,'"%l",secondPart)
  firstPart:= concat(firstPart,fromPart)
  sayWidth firstPart + sayWidth predPart < 80 =>
    concat(firstPart,predPart)
  concat(firstPart,'"%l",predPart)

substInOrder(alist,x) ==
  alist is [[a,:b],:y] => substInOrder(y,substitute(b,a,x))
  x

reportOpSymbol op1 ==
  -- Don't forget that "^" is another name for "**"
  if op1 = "^" then
    sayMessage ['"  ",op1, '" is another name for", :bright '"**"]
    op1 := "**"
  op := (string? op1 => INTERN op1; op1)
  modemaps := getAllModemapsFromDatabase(op,nil)
  null modemaps =>
    ok := true
    sayKeyedMsg("S2IF0010",[op1])
    if SIZE PNAME op1 < 3 then
      x := UPCASE queryUserKeyedMsg("S2IZ0060",[op1])
      null (STRING2ID_-N(x,1) in '(Y YES)) =>
        ok := nil
        sayKeyedMsg("S2IZ0061",[op1])
    ok => apropos [op1]
  sayNewLine()
  -- filter modemaps on whether they are exposed
  mmsE := mmsU := NIL
  for mm in modemaps repeat
    isFreeFunctionFromMm(mm) or isExposedConstructor getDomainFromMm(mm) => mmsE := [mm,:mmsE]
    mmsU := [mm,:mmsU]
  if mmsE then
    sayMms(op,mmsE,'"exposed")
  if mmsU then
    if mmsE then sayNewLine()
    sayMms(op,mmsU,'"unexposed")
  nil
 where
  sayMms(op,mms,label) ==
    m := # mms
    sayMSG
      m = 1 =>
        ['"There is one",:bright label,'"function called",
          :bright op,'":"]
      ['"There are ",m,:bright label,'"functions called",
        :bright op,'":"]
    for mm in mms for i in 1.. repeat
      sayModemapWithNumber(mm,i)

formatOpType (form:=[op,:argl]) ==
  null argl => unabbrev op
  form2String [unabbrev op, :argl]

formatOperationAlistEntry (entry:= [op,:modemaps]) ==
  -- alist has entries of the form: ((op sig) . pred)
  -- opsig on this list => op is defined only when the predicate is true
  ans:= nil
  for [sig,.,:predtail] in modemaps repeat
    pred := (predtail is [p,:.] => p; 'T)
    -- operation is always defined
    ans :=
      [concat(formatOpSignature(op,sig),formatIf pred),:ans]
  ans

formatOperation([[op,sig],.,[fn,.,n]],domain) ==
  opSigString := formatOpSignature(op,sig)
  integer? n and function Undef = KAR domain.n =>
    if integer? $commentedOps then $commentedOps := $commentedOps + 1
    concat(" --",opSigString)
  opSigString

formatOpSignature(op,sig) ==
  concat('"%b",formatOpSymbol(op,sig),'"%d",": ",formatSignature sig)

formatOpConstant op ==
  concat('"%b",formatOpSymbol(op,'($)),'"%d",'": constant")

formatOpSymbol(op,sig) ==
  if op = 'Zero then op := "0"
  else if op = 'One then op := "1"
  null sig => op
  quad := specialChar 'quad
  n := #sig
  (op = 'elt) and (n = 3) =>
    (second(sig) = '_$) =>
      string? (sel := third(sig)) =>
        [quad,".",sel]
      [quad,".",quad]
    op
  string? op or GETL(op,"Led") or GETL(op,"Nud") =>
    n = 3 =>
      if op = 'SEGMENT then op := '".."
      op = 'in => [quad,'" ",op,'" ",quad]
-- stop exquo from being displayed as infix (since it is not accepted
-- as such by the interpreter)
      op = 'exquo => op
      [quad,op,quad]
    n = 2 =>
      not GETL(op,"Nud") => [quad,op]
      [op,quad]
    op
  op

formatAttribute x ==
  atom x => ["  ",x]
  x is [op,:argl] =>
    for x in argl repeat
      argPart:= NCONC(argPart,concat(",",formatAttributeArg x))
    argPart => concat("  ",op,"_(",rest argPart,"_)")
    ["  ",op]

formatAttributeArg x ==
  string? x and x ='"*" => "_"*_""
  atom x => formatOpSymbol (x,nil)
  x is [":",op,["Mapping",:sig]] =>
    concat('"%b",formatOpSymbol(op,sig),": ",'"%d",formatMapping sig)
  prefix2String0 x

formatMapping sig ==
  strconc/concat("Mapping(",formatSignature sig,")")

dollarPercentTran x ==
    -- Translate $ to %. We actually return %% so that the message
    -- printer will display a single %
    x is [y,:z] =>
        y1 := dollarPercentTran y
        z1 := dollarPercentTran z
        EQ(y, y1) and EQ(z, z1) => x
        [y1, :z1]
    x is "$" or x is '"$" => "%%"
    x is "T$" or x is '"T$" => "T"
    x

formatSignatureAsTeX sig == 
  $formatSigAsTeX: local := 2
  formatSignature0 sig

formatSignature sig ==
  $formatSigAsTeX: local := 1
  formatSignature0 sig

formatSignatureArgs sml ==
  $formatSigAsTeX: local := 1
  formatSignatureArgs0 sml
  
formatSignature0 sig ==
  null sig => "() -> ()"
  integer? sig => '"hashcode"
  [tm,:sml] := sig
  sourcePart:= formatSignatureArgs0 sml
  targetPart:= prefix2String0 tm
  dollarPercentTran concat(sourcePart,concat(" -> ",targetPart))

formatSignatureArgs0(sml) ==
-- formats the arguments of a signature
  null sml => ["_(_)"]
  null rest sml => prefix2String0 first sml
  argList:= prefix2String0 first sml
  for m in rest sml repeat
    argList:= concat(argList,concat(",",prefix2String0 m))
  concat("_(",concat(argList,"_)"))

--% Conversions to string form

expr2String x ==
  atom (u:= prefix2String0 x) => u
  strconc/[atom2String y for y in u]

-- exports (this is a badly named bit of sillyness)
prefix2StringAsTeX form ==
  form2StringAsTeX form

prefix2String form ==
  form2String form

-- local version
prefix2String0 form ==
  form2StringLocal form

--  SUBRP form => formWrapId BPINAME form
--  atom form =>
--    form=$EmptyMode or form=$quadSymbol => formWrapId specialChar 'quad
--    string? form => formWrapId form
--    IDENTP form => 
--      constructor? form => app2StringWrap(formWrapId form, [form])
--      formWrapId form
--    formWrapId STRINGIMAGE form

form2StringWithWhere u ==
  $permitWhere : local := true
  $whereList: local := nil
  s:= form2String u
  $whereList => concat(s,'"%b",'"where",'"%d","%i",$whereList,'"%u")
  s

form2StringWithPrens form ==
  null (argl := rest form) => [first form]
  null rest argl => [first form,"(",first argl,")"]
  form2String form

formString u ==
  x := form2String u
  atom x => STRINGIMAGE x
  strconc/[STRINGIMAGE y for y in x]

form2String u == 
  $formatSigAsTeX: local := 1
  form2StringLocal u

form2StringAsTeX u == 
  $formatSigAsTeX: local := 2
  form2StringLocal u

form2StringLocal u ==
--+
  $NRTmonitorIfTrue : local := nil
  $fortInts2Floats  : local := nil
  form2String1 u

constructorName con ==
  $abbreviateTypes => abbreviate con
  con

form2String1 u ==
  atom u => 
    u=$EmptyMode or u=$quadSymbol => formWrapId specialChar 'quad
    IDENTP u =>
      constructor? u => app2StringWrap(formWrapId u, [u])
      formWrapId u
    SUBRP u => formWrapId BPINAME u
    string? u => formWrapId u
    formWrapId toString u
  u1 := u
  [op,:argl] := u
  op='Join or op= 'mkCategory => formJoin1(op,argl)
  $InteractiveMode and IDENTP op and (u:= constructor? op) =>
    null argl => app2StringWrap(formWrapId constructorName op, u1)
    op = "NTuple"  => [ form2String1 first argl, '"*"]
    op = "Map"     => ['"(",:formatSignature0 [argl.1,argl.0],'")"]
    op = "Record" => record2String(argl)
    null (conSig := getConstructorSignature op) =>
      application2String(constructorName op,[form2String1(a) for a in argl], u1)
    ml := rest conSig
    if not freeOfSharpVars ml then
      ml:=SUBLIS([[pvar,:val] for pvar in $FormalMapVariableList
        for val in argl], ml)
    argl:= formArguments2String(argl,ml)
      -- extra null check to handle mutable domain hack.
    null argl => constructorName op
    application2String(constructorName op,argl, u1)
  op = "Mapping" => ['"(",:formatSignature argl,'")"]
  op = "Record" => record2String(argl)
  op = "Union"  =>
    application2String(op,[form2String1 x for x in argl], u1)
  op = ":" =>
      null argl => [ '":" ]
      null rest argl => [ '":", form2String1 first argl ]
      formDecl2String(argl.0,argl.1)
  op = "#" and cons? argl and LISTP first argl =>
    STRINGIMAGE #first argl
  op = 'Join => formJoin2String argl
  op = "ATTRIBUTE" => form2String1 first argl
  op='Zero => '"0"
  op='One => '"1"
  op = 'AGGLST => tuple2String argl
  op = 'BRACKET =>
    argl' := form2String1 first argl
    ['"[",:(atom argl' => [argl']; argl'),'"]"]
  op = 'PAREN =>
    argl' := form2String1 first argl
    ['"(",:(atom argl' => [argl']; argl'),'")"]
  op = "SIGNATURE" =>
     [operation,sig] := argl
     concat(operation,'": ",formatSignature sig)
  op = 'COLLECT => formCollect2String argl
  op = 'construct =>
    concat(lbrkSch(),
           tuple2String [form2String1 x for x in argl],rbrkSch())
  op = "SEGMENT" =>
    null argl => '".."
    lo := form2String1 first argl
    argl := rest argl
    (null argl) or null (first argl) => [lo, '".."]
    [lo, '"..", form2String1 first argl]
  isBinaryInfix op => formatAsFortranExpresion [op,:argl]
  -- COMPILED_-FUNCTION_-P(op) => form2String1 coerceMap2E(u1,NIL)
  application2String(op,[form2String1 x for x in argl], u1)

formWrapId id == 
  $formatSigAsTeX = 1 => PNAME id
  $formatSigAsTeX = 2 => 
    sep := '"`"
    FORMAT(NIL,'"\verb~a~a~a",sep, id, sep)
  error '"Bad formatSigValue"

formArguments2String(argl,ml) == [fn(x,m) for x in argl for m in ml] where
  fn(x,m) ==
    x=$EmptyMode or x=$quadSymbol => specialChar 'quad
    string?(x) or IDENTP(x) => x
    x is ['_:,:.] => form2String1 x
    isValidType(m) and cons?(m) and
      (getConstructorKindFromDB first(m) = "domain") =>
        (x' := coerceInteractive(objNewWrap(x,m),$OutputForm)) =>
          form2String1 objValUnwrap x'
        form2String1 x
    form2String1 x

formDecl2String(left,right) ==
  $declVar: local := left
  whereBefore := $whereList
  ls:= form2StringLocal left
  rs:= form2StringLocal right
  NE($whereList,whereBefore) and $permitWhere => ls
  concat(form2StringLocal ls,'": ",rs)

formJoin1(op,u) ==
  if op = 'Join then [:argl,last] := u else (argl := nil; last := [op,:u])
  last is [id,.,:r] and id in '(mkCategory CATEGORY) =>
    $abbreviateJoin = true => concat(formJoin2 argl,'"%b",'"with",'"%d",'"...")
    $permitWhere = true =>
      opList:= formatJoinKey(r,id)
      $whereList:= concat($whereList,"%l",$declVar,": ",
        formJoin2 argl,'"%b",'"with",'"%d","%i",opList,"%u")
      formJoin2 argl
    opList:= formatJoinKey(r,id)
    suffix := concat('"%b",'"with",'"%d","%i",opList,"%u")
    concat(formJoin2 argl,suffix)
  formJoin2 u

formatJoinKey(r,key) ==
  key = 'mkCategory =>
    r is [opPart,catPart,:.] =>
      opString :=
        opPart is ['LIST,:u] =>
          "append"/[concat("%l",formatOpSignature(op,sig),formatIf pred)
            for ['QUOTE,[[op,sig],pred]] in u]
        nil
      catString :=
        catPart is ['LIST,:u] =>
          "append"/[concat("%l",'" ",form2StringLocal con,formatIf pred)
            for ['QUOTE,[con,pred]] in u]
        nil
      concat(opString,catString)
    '"?? unknown mkCategory format ??"
  -- otherwise we have the CATEGORY form
  "append"/[fn for x in r] where fn() ==
    x is ['SIGNATURE,op,sig] => concat("%l",formatOpSignature(op,sig))
    x is ['ATTRIBUTE,a] => concat("%l",formatAttribute a)
    x

formJoin2 argl ==
-- argl is a list of categories NOT containing a "with"
  null argl => '""
  1=#argl => form2StringLocal argl.0
  application2String('Join,[form2StringLocal x for x in argl], NIL)

formJoin2String (u:=[:argl,last]) ==
  last is ["CATEGORY",.,:atsigList] =>
    postString:= concat('"_(",formTuple2String atsigList,'"_)")
    #argl=1 => concat(first argl,'" with ",postString)
    concat(application2String('Join,argl, NIL)," with ",postString)
  application2String('Join,u, NIL)

formCollect2String [:itl,body] ==
  ['"_(",body,:"append"/[formIterator2String x for x in itl],'"_)"]

formIterator2String x ==
  x is ["STEP",y,s,.,:l] =>
    tail:= (l is [f] => form2StringLocal f; nil)
    concat('"for ",y,'" in ",s,'"..",tail)
  x is ["tails",y] => concat('"tails ",formatIterator y)
  x is ["reverse",y] => concat('"reverse ",formatIterator y)
  x is ["|",y,p] => concat(formatIterator y,'" | ",form2StringLocal p)
  x is ["until",p] => concat('"until ",form2StringLocal p)
  x is ["while",p] => concat('"while ",form2StringLocal p)
  systemErrorHere ["formatIterator",x]

tuple2String argl ==
  null argl => nil
  string := first argl
  if member(string, '("failed" "nil" "prime" "sqfr" "irred"))
    then string := strconc('"_"",string,'"_"")
    else string :=
      atom string => object2String string
      [f x for x in string]
  for x in rest argl repeat
    if member(x,'("failed" "nil" "prime" "sqfr" "irred")) then
      x := strconc('"_"",x,'"_"")
    string:= concat(string,concat(",",f x))
  string
 where
  f x ==
    atom x => object2String x
    -- [f first x,:f rest x]
    [f y for y in x]

script2String s ==
  null s => '""   -- just to be safe
  if atom s then s := [s]
  linearFormatForm(first s, rest s)

linearFormatName x ==
  atom x => x
  linearFormat x

linearFormat x ==
  atom x => x
  x is [op,:argl] and atom op =>
    argPart:=
      argl is [a,:l] => [a,:"append"/[[",",x] for x in l]]
      nil
    [op,"(",:argPart,")"]
  [linearFormat y for y in x]

numOfSpadArguments id ==
  char("*") = (s:= PNAME id).0 =>
      +/[n for i in 1.. while integer? (n:=readInteger PNAME s.i)]
  keyedSystemError("S2IF0012",[id])

linearFormatForm(op,argl) ==
  s:= PNAME op
  indexList:= [readInteger PNAME d for i in 1.. while
    (digit? (d:= s.(maxIndex:= i)))]
  cleanOp:= INTERN (strconc/[PNAME s.i for i in maxIndex..MAXINDEX s])
  fnArgs:=
    indexList.0 > 0 =>
      concat('"(",formatArgList take(-indexList.0,argl),'")")
    nil
  if #indexList > 1 then
    scriptArgs:= formatArgList take(indexList.1,argl)
    argl := drop(indexList.1,argl)
    for i in rest rest indexList repeat
      subArglist:= take(i,argl)
      argl:= drop(i,argl)
      scriptArgs:= concat(scriptArgs,";",formatArgList subArglist)
  scriptArgs:=
    scriptArgs => concat(specialChar 'lbrk,scriptArgs, specialChar 'rbrk)
    nil
  l := [(string? f => f; STRINGIMAGE f) for f in
       concat(cleanOp,scriptArgs,fnArgs)]
  strconc/l

formatArgList l ==
  null l => nil
  acc:= linearFormat first l
  for x in rest l repeat
    acc:= concat(acc,",",linearFormat x)
  acc

formTuple2String argl ==
  null argl => nil
  string:= form2StringLocal first argl
  for x in rest argl repeat
    string:= concat(string,concat(",",form2StringLocal x))
  string

isInternalFunctionName(op) ==
  (not IDENTP(op)) or (op = "*") or (op = "**") => NIL
  (1 = SIZE(op':= PNAME op)) or (char("*") ~= op'.0) => NIL
  -- if there is a semicolon in the name then it is the name of
  -- a compiled spad function
  null (e := STRPOS('"_;",op',1,NIL)) => NIL
  (char(" ") = (y := op'.1)) or (char("*") = y) => NIL
  table := MAKETRTTABLE('"0123456789",NIL)
  s := STRPOSL(table,op',1,true)
  null(s) or s > e => NIL
  SUBSTRING(op',s,e-s)

application2String(op,argl, linkInfo) ==
  null argl =>
    (op' := isInternalFunctionName(op)) => op'
    app2StringWrap(formWrapId op, linkInfo)
  op = "[||]" => concat('"[|",concat(prefix2String0 argl,'"|]"))
  1=#argl =>
    arg := first argl
    arg is ["<",:.] or arg is ["(",:.] => concat(op,arg)
    concat(app2StringWrap(formWrapId op, linkInfo)," ",arg)
--op in '(UP SM) =>
--  newop:= (op = "UP" => "P";"M")
--  concat(newop,concat(lbrkSch(),argl.0,rbrkSch(),argl.1))
--op='RM  =>concat("M",concat(lbrkSch(),
--                     argl.0,",",argl.1,rbrkSch(),argl.2))
--op='MP =>concat("P",concat(argl.0,argl.1))
  op='SEGMENT =>
    null argl => '".."
    (null rest argl) or (null second argl) =>
      concat(first argl, '"..")
    concat(first argl, concat('"..", second argl))
  concat(app2StringWrap(formWrapId op, linkInfo) ,
                        concat("_(",concat(tuple2String argl,"_)")))

app2StringConcat0(x,y) ==
  FORMAT(NIL, '"~a ~a", x, y)

app2StringWrap(string, linkInfo) ==
  not linkInfo => string
  $formatSigAsTeX = 1 => string
  $formatSigAsTeX = 2 =>
    str2 :=  "app2StringConcat0"/form2Fence linkInfo
    sep := '"`"
    FORMAT(NIL, '"\lispLink{\verb!(|conPage| '~a)!}{~a}", 
          str2, string)
  error "Bad value for $formatSigAsTeX"

record2String x ==
  argPart := NIL
  for [":",a,b] in x repeat argPart:=
    concat(argPart,'",",a,'": ",form2StringLocal b)
  null argPart => '"Record()"
  concat('"Record_(",rest argPart,'"_)")

plural(n,string) ==
  suffix:=
    n = 1 => '""
    '"s"
  [:bright n,string,suffix]

formatIf pred ==
  not pred => nil
  member(pred,'(T %true (QUOTE T))) => nil
  concat('"%b",'"if",'"%d",pred2English pred)

formatPredParts s ==
  s is ['QUOTE,s1] => formatPredParts s1
  s is ['LIST,:s1] => [formatPredParts s2 for s2 in s1]
  s is ['devaluate,s1] => formatPredParts s1
  s is ['getDomainView,s1,.] => formatPredParts s1
  s is ['SUBST,a,b,c] =>    -- this is a signature
    s1 := formatPredParts substitute(formatPredParts a,b,c)
    s1 isnt [fun,sig] => s1
    ['SIGNATURE,fun,[formatPredParts(r) for r in sig]]
  s

pred2English x ==
  x is ['IF,cond,thenClause,elseClause] =>
    c := concat('"if ",pred2English cond)
    t := concat('" then ",pred2English thenClause)
    e := concat('" else ",pred2English elseClause)
    concat(c,t,e)
  x is ['AND,:l] =>
    tail:="append"/[concat(bright '"and",pred2English x) for x in rest l]
    concat(pred2English first l,tail)
  x is ['OR,:l] =>
    tail:= "append"/[concat(bright '"or",pred2English x) for x in rest l]
    concat(pred2English first l,tail)
  x is ['NOT,l] =>
    concat('"not ",pred2English l)
  x is [op,a,b] and op in '(_has ofCategory) =>
    concat(pred2English a,'"%b",'"has",'"%d",form2String abbreviate b)
  x is [op,a,b] and op in '(HasSignature HasAttribute HasCategory) =>
    concat(prefix2String0 formatPredParts a,'"%b",'"has",'"%d",
      prefix2String0 formatPredParts b)
  x is [op,a,b] and op in '(ofType getDomainView) =>
    if b is ['QUOTE,b'] then b := b'
    concat(pred2English a,'": ",form2String abbreviate b)
  x is [op,a,b] and op in '(isDomain domainEqual) =>
    concat(pred2English a,'" = ",form2String abbreviate b)
  x is [op,:.] and (translation := LASSOC(op,'(
    (_< . " < ") (_<_= . " <= ")
      (_> . " > ") (_>_= . " >= ") (_=  . " = ") (_~_= . " _~_= ")))) =>
        concat(pred2English a,translation,pred2English b)
  x is ['ATTRIBUTE,form] =>
    concat("attribute: ",form2String form)
  form2String x

mathObject2String x ==
  CHARACTERP x => COERCE([x],'STRING)
  object2String x

object2String x ==
  string? x => x
  IDENTP x  => PNAME x
  null x    => '""
  cons?  x  => strconc(object2String first x, object2String rest x)
  toString x

object2Identifier x ==
  IDENTP x  => x
  string? x => INTERN x
  INTERN toString x

blankList x == "append"/[[BLANK,y] for y in x]

pkey keyStuff ==
    if atom keyStuff then keyStuff := [keyStuff]
    allMsgs := ['" "]
    while not null keyStuff repeat
        dbN := NIL
        argL := NIL
        key := first keyStuff
        keyStuff := IFCDR keyStuff
        next := IFCAR keyStuff
        while cons? next repeat
            if first next = 'dbN then dbN := second next
            else argL := next
            keyStuff  := IFCDR keyStuff
            next      := IFCAR keyStuff
        oneMsg  := returnStLFromKey(key,argL,dbN)
        allMsgs := ['" ", :NCONC (oneMsg,allMsgs)]
    allMsgs

string2Float s ==
  -- takes a string, calls the parser on it and returns a float object
  p := ncParseFromString s
  p isnt [["$elt", FloatDomain, "float"], x, y, z] =>
    systemError '"string2Float: did not get a float expression"
  flt := getFunctionFromDomain("float", FloatDomain,
    [$Integer, $Integer, $PositiveInteger])
  SPADCALL(x, y, z, flt)



form2Fence form == 
  -- body of dbMkEvalable
  [op, :.] := form
  kind := getConstructorKindFromDB op
  kind = 'category => form2Fence1 form
  form2Fence1 mkEvalable form

form2Fence1 x ==
  x is [op,:argl] =>
    op = "QUOTE" => ['"(QUOTE ",:form2FenceQuote first argl,'")"]
    ['"(", FORMAT(NIL, '"|~a|", op),:"append"/[form2Fence1 y for y in argl],'")"]
  null x => '""
  IDENTP x => FORMAT(NIL, '"|~a|", x)
  ['"  ", x]

form2FenceQuote x ==
  integer? x => [STRINGIMAGE x]
  symbol? x => [FORMAT(NIL, '"|~a|", x)]
  string? x => ['"_"",x,'"_""]
  atom    x => systemErrorHere ["form2FenceQuote",x]
  ['"(",:form2FenceQuote first x,:form2FenceQuoteTail rest x]

form2FenceQuoteTail x ==
  null x => ['")"]
  atom x => ['" . ",:form2FenceQuote x,'")"]
  ['" ",:form2FenceQuote first x,:form2FenceQuoteTail rest x]

form2StringList u ==
  atom (r := form2String u) => [r]
  r

--% Type Formatting Without Abbreviation

formatUnabbreviatedSig sig ==
  null sig => ['"() -> ()"]
  [target,:args] := dollarPercentTran sig
  target := formatUnabbreviated target
  null args => ['"() -> ",:target]
  null rest args => [:formatUnabbreviated first args,'" -> ",:target]
  args := formatUnabbreviatedTuple args
  ['"(",:args,'") -> ",:target]

formatUnabbreviatedTuple t ==
  -- t is a list of types
  null t => t
  atom t => [t]
  t0 := formatUnabbreviated t.op
  null rest t => t0
  [:t0,'",",:formatUnabbreviatedTuple rest t]

formatUnabbreviated t ==
  null t =>
    ['"()"]
  atom t =>
    [t]
  t is [p,sel,arg] and p = ":" =>
    [sel,'": ",:formatUnabbreviated arg]
  t is ['Union,:args] =>
    ['Union,'"(",:formatUnabbreviatedTuple args,'")"]
  t is ['Mapping,:args] =>
    formatUnabbreviatedSig args
  t is ['Record,:args] =>
    ['Record,'"(",:formatUnabbreviatedTuple args,'")"]
  t is [arg] =>
    t
  t is [arg,arg1] =>
    [arg,'" ",:formatUnabbreviated arg1]
  t is [arg,:args] =>
    [arg,'"(",:formatUnabbreviatedTuple args,'")"]
  t

  
