-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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


-- HOW THE TRANSLATOR WORKS

-- Unit of code is markedUp as follows (unit= item in a capsule pile, e.g.)
--   (WI/.. a b)            means    source code a --> markedUpCode b
--   (REPPER/.. . . a)      means    source code for a ---> (rep a) or (per a)
-- Source code is extracted, modified from markedUpCode, and stacked
-- Entire constructor is then assembled and prettyprinted


import macros
namespace BOOT

REMPROP("and",'parseTran)
REMPROP("or",'parseTran)
REMPROP("not",'parseTran)
property("and",'special) := 'compAnd
property("or",'special) := 'compOr
property("not",'special) := 'compNot
SETQ($monitorWI,nil)
SETQ($monitorCoerce,nil)
SETQ($markPrimitiveNumbers,nil)  -- '(Integer SmallInteger))
SETQ($markNumberTypes,'(Integer SmallInteger PositiveInteger NonNegativeInteger))

--======================================================================
--              Master Markup Function
--======================================================================
 

WI(a,b) == b

mkWi(fn,:r) ==            
--  if $monitorWI and r isnt ['WI,:.] and not (r is ['AUTOSUBSET,p,.,y] and(MEMQ(KAR p,'(NonNegativeInteger PositiveInteger)) or y='_$fromCoerceable_$)) then
--    if $monitorWI and r isnt ['WI,:.] then
--    sayBrightlyNT ['"From ",fn,'": "]
--    pp r
  r is ['WI,a,b] =>
    a = b => a            --don't bother
    b is ['WI,=a,.] => b
    r
  r
 
--======================================================================
--        Capsule Function Transformations
--======================================================================
tcheck T == 
  if T isnt [.,.,.] then systemError 'tcheck
  T
  
markComp(x,T) ==                                         --for comp
  tcheck T
  x ~= first T => [mkWi('comp,'WI,x,first T),:rest T]                  
  T

markAny(key,x,T) ==
  tcheck T
  x ~= first T => [mkWi(key,'WI,x,first T),:rest T]
  T

markConstruct(x,T) == 
  tcheck T
  markComp(x,T)

markParts(x,T) ==  --x is ['PART,n,y]                     --for compNoStacking
  tcheck T
  [mkWi('makeParts,'WI,x,first T),:rest T]
   
yumyum kind == kind
markCoerce(T,T',kind) ==                                 --for coerce
  tcheck T
  tcheck T'
  if kind = 'AUTOSUBSET then yumyum(kind)
  string? T.mode and T'.mode = '(String) => T'
  markKillAll T.mode = T'.mode => T'
  -- reduce (AUTOSUBSET a b (WI c (AUTOSUBSET b a c))) ==> c
  u :=
    $partExpression is [.,.,y] and T.expr = y => ['WI,y,$partExpression]
    T.expr
  res := [markCoerceChk mkWi('coerce,kind,T.mode,T'.mode,
           mkWi('coerce,'WI,u,T'.expr)),:rest T']
  res
  
markCoerceChk x ==
  x is ['AUTOSUBSET,a,b,['WI,c,['AUTOSUBSET,=b, =a, =c]]] => c
  x

markMultipleExplicit(nameList, valList, T) ==
  tcheck T
  [mkWi('setqMultipleExplicit, 'WI,
    ["%LET", ["%Comma",:nameList], ["%Comma",:valList]],
    T.expr), :rest T]

markRetract(x,T) ==
  tcheck T
  [mkWi('smallIntegerStep,'RETRACT,nil,['REPLACE,['retract,x]],T.expr),:rest T]

markSimpleReduce(x,T) ==
  tcheck T
  [mkWi('compreduce,'LAMBDA, nil, ["REPLACE",x], T.expr), :rest T]

markCompAtom(x,T) ==                                     --for compAtom
  tcheck T
  $convert2NewCompiler =>
    [mkWi('compAtom,'ATOM,nil,['REPLACE,[x]],T.expr),:rest T]
  T

markCase(x, tag, T) ==
  tcheck T
  [mkWi('compCase1, 'LAMBDA, nil, ["REPLACE",["case",x,tag]], T.expr), 
    :rest T]

markCaseWas(x,T) == 
  tcheck T
  [mkWi('compCase1,'WI,x,T.expr),:rest T]

markAutoWas(x,T) == 
  tcheck T
  [mkWi('autoCoerce,'WI,x,T.expr),:rest T]

markCallCoerce(x,m,T) ==
  tcheck T
  [mkWi('%call,'WI,["::",x,m], T.expr),: rest T]

markCoerceByModemap(x,source,target,T, killColonColon?) == 
  tcheck T
  source is ["Union",:l] and member(target,l) =>
    tag := genCaseTag(target, l, 1) or return nil
    markAutoCoerceDown(x, tag, markAutoWas(x,T), killColonColon?)
  target is ["Union",:l] and member(source,l) =>
    markAutoCoerceUp(x,markAutoWas(x, T))
  [mkWi('markCoerceByModemap,'WI,x,T.expr),:rest T]
   
markAutoCoerceDown(x,tag,T,killColonColon?) ==
  tcheck T
  patch := ["dot",getSourceWI x,tag]
  if killColonColon? then patch := ["REPLACE",["UNCOERCE",patch]]
  [mkWi('coerceExtraHard,'LAMBDA, nil,patch,T.expr), :rest T]

markAutoCoerceUp(x,T) ==
--  y := getSourceWI x
--  y := 
--    string? y => INTERN y
--    y   
  tcheck T  
  [mkWi('coerceExtraHard,'LAMBDA, nil,["REPLACE",['construct, "##1"]],T.expr),
     -----want to capture by ##1 what is there                ------11/2/94
    :rest T]

markCompSymbol(x,T) ==                                   --for compSymbol
  tcheck T
  [mkWi('compSymbol,'ATOM,nil,['REPLACE,["@",x,$Symbol]],T.expr),:rest T]

markStepSI(ostep,nstep) ==                               --for compIterator
  ['STEP,:r] := ostep
  ['ISTEP,i,:s] := nstep
--$localLoopVariables := insert(i,$localLoopVariables)
  markImport 'SmallInteger
  mkWi('markStepSI,'WI,ostep,['ISTEP,
    mkWi('markStep,'FREESI,nil,['REPLACE,          ['PAREN,['free,i]]],i),:s])
--                                    i],i),:s])
markStep(i) == mkWi('markStep,'FREE,nil,['REPLACE, ['PAREN,['free,i]]],i)
--                                    i],i)

markPretend(T,T') ==
  tcheck T
  tcheck T'
  [mkWi('pretend,'COLON,"pretend",T.mode,T.expr),:rest T']

markAt(T) == 
  tcheck T
  [mkWi('compAtom,'COLON,"@",T.mode,T.expr),:rest T]

markCompColonInside(op,T) ==                         --for compColonInside
  tcheck T
  $convert2NewCompiler =>
    [mkWi('compColonInside,'COLON,op,T.mode,T.expr),:rest T]
  T

markLisp(T,m) ==                                     --for compForm1
  tcheck T
  $convert2NewCompiler =>
    [mkWi('compForm1,'COLON,'Lisp,T.mode,T.expr),:rest T]
  T

markLambda(vl,body,mode,T) ==                       --for compWithMappingMode
  tcheck T
  if mode isnt ['Mapping,:ml] then error '"markLambda"
  args := [[":",$PerCentVariableList.i,t] for i in 0.. for t in rest ml]
  left := [":",['PAREN,:args],first ml]
  fun := ['_+_-_>,left,SUBLISLIS($PerCentVariableList,vl,body)] 
  [mkWi('compWithMappingMode,'LAMBDA,nil,['REPLACE,fun],T.expr),:rest T]

markMacro(before,after) ==                            --for compMacro
  $convert2NewCompiler => 
    if before is [x] then before := x
    $def := ['MDEF,before,'(NIL),'(NIL),after]
    if $insideFunctorIfTrue 
      then $localMacroStack := [[before,:after],:$localMacroStack]
      else $globalMacroStack:= [[before,:after],:$globalMacroStack]
    mkWi('macroExpand,'MI,before,after) 
  after

markInValue(y ,e) ==
  y1 := markKillAll y
  [y', m, e] := T := comp(y1, $EmptyMode, e) or return nil
  markImport m
  m = "$" and LASSOC('value,getProplist('Rep,e)) is [a,:.] and 
         opOf a in '(List Vector) => [markRepper('rep, y'), 'Rep, e]
  T

markReduceIn(it, pr)       ==   markReduceIterator("in",it,pr)
markReduceStep(it, pr)     ==   markReduceIterator("step", it, pr)
markReduceWhile(it, pr)    ==   markReduceIterator("while", it, pr)
markReduceUntil(it, pr)    ==   markReduceIterator("until", it, pr)
markReduceSuchthat(it, pr) == markReduceIterator("suchthat", it, pr)
markReduceIterator(kind, it, pr) == [mkWi(kind, 'WI, it, first pr), :rest pr]
markReduceBody(body,T)     ==  
  tcheck T
  [mkWi("reduceBody",'WI,body,first T), :rest T]
markReduce(form, T)        ==  
  tcheck T
  [SETQ($funk,mkWi("reduce", 'WI,form,first T)), :rest T]

markRepeatBody(body,T)     ==  
  tcheck T
  [mkWi("repeatBody",'WI,body,first T), :rest T]

markRepeat(form, T)        ==  
  tcheck T
  [mkWi("repeat", 'WI,form,first T), :rest T]
  
markTran(form,form',[dc,:sig],env) ==  --from compElt/compFormWithModemap
  dc ~= 'Rep or not ('_$ in sig) => mkWi('markTran,'WI,form,['%call,:form'])
  argl := [u for t in rest sig for arg in rest form'] where u() ==
    t='_$ => 
      argSource := getSourceWI arg
      IDENTP argSource and getmode(argSource,env) = 'Rep => arg
      markRepper('rep,arg)
    arg
  form' := ['%call,first form',:argl]
  wi := mkWi('markTran,'WI,form,form')
  first sig = '_$ => markRepper('per,wi)
  wi
 
markRepper(key,form) == ['REPPER,nil,key,form]
 
markDeclaredImport d == markImport(d,true)

markImport(d,:option) ==   --from compFormWithModemap/genDeltaEntry/compImport
  if CONTAINED('PART,d) then pause d
  declared? := IFCAR option
  null d or d = $Representation => nil
  d is [op,:.] and op in '(Boolean Mapping Void Segment UniversalSegment) => nil
  string? d or (IDENTP d and stringchar(symbolName d,0) = char '_#) => nil
  d in '(_$ _$NoValueMode _$EmptyMode Void) => nil
-------=======+> WHY DOESN'T THIS WORK????????????
--if (d' := macroExpand(d,$e)) ~= d then markImport(d',declared?)
  dom := markMacroTran d
--if IDENTP dom and dom = d and not getmode(dom,$e) then dom := ['MyENUM, d]
  categoryForm? dom => nil
  $insideCapsuleFunctionIfTrue => 
    $localImportStack := insert(dom,$localImportStack)
    if IFCAR option then $localDeclareStack := insert(dom,$localDeclareStack)
  if $globalImportStack then
    $globalImportStack := insert(dom,$globalImportStack)
    if IFCAR option then $globalDeclareStack := insert(dom,$globalDeclareStack)

markMacroTran name ==     --called by markImport
  atom name => name
  u := or/[x for [x,:y] in $globalMacroStack | y = name] => u
  u := or/[x for [x,:y] in $localMacroStack  | y = name] => u
  [op,:argl] := name
  op in '(Record Union) => 
--  pp ['"Cannot find: ",name]
    name
  [op,:[markMacroTran x for x in argl]]
   
markSetq(originalLet,T) ==                                --for compSetq
  $convert2NewCompiler => 
    $coerceList : local := nil
    ["%LET",form,originalBody] := originalLet
    id := markLhs form
    not $insideCapsuleFunctionIfTrue =>
      $from : local := '"Setq"
      code := T.expr
      markEncodeChanges(code,nil)
      noriginalLet := markSpliceInChanges originalBody
      if IDENTP id then $domainLevelVariableList := insert(id,$domainLevelVariableList) 
      nlet := ["%LET",id,noriginalLet]
      entry := [originalLet,:nlet]
      $importStack := [nil,:$importStack]
      $freeStack   := [nil,:$freeStack]
      capsuleStack('"Setq", entry)
--    [markKillMI T.expr,:rest T]
      [code,:rest T]
    if MEMQ(id,$domainLevelVariableList) then 
      $markFreeStack := insert(id,$markFreeStack)
    T
  T

markCapsuleExpression(originalExpr, T) ==
  $coerceList: local := nil
  $from: local := '"Capsule expression"
  code := T.expr
  markEncodeChanges(code, nil)
  noriginal := markSpliceInChanges originalExpr
  nexpr := noriginal
  entry := [originalExpr,:nexpr]
  $importStack := [nil,:$importStack]
  $freeStack   := [nil,:$freeStack]
  capsuleStack('"capsuleExpression", entry)
  [code,:rest T]

markLhs x ==
  x is [":",a,.] => a
  atom x => x
  x                  --ignore

capsuleStack(name,entry) ==
--  if $monitorWI then
--    sayBrightlyNT ['"Stacking ",name,'": "]
--    pp entry
  $capsuleStack := [COPY entry,:$capsuleStack] 
  $predicateStack := [$predl, :$predicateStack]
  signature := 
    $insideCapsuleFunctionIfTrue => $signatureOfForm
    nil
  $signatureStack := [signature, :$signatureStack]
 
foobar(x) == x 
 
foobum(x) == x         --from doIT


--======================================================================
--        Capsule Function Transformations
--======================================================================
--called from compDefineCapsuleFunction
markChanges(originalDef,T,sig) == 
  $convert2NewCompiler => 
    if $insideCategoryIfTrue and $insideFunctorIfTrue then
      originalDef := markCatsub(originalDef)
      T := [markCatsub(T.expr),
             markCatsub(T.mode),T.env]
      sig := markCatsub(sig)
      $importStack := markCatsub($importStack)
--  T := coerce(T,first sig)         ---> needed to wrap a "per" around a Rep type
    code := T.expr
    $e : local := T.env
    $coerceList : local := nil
    $hoho := code
    ['DEF,form,.,.,originalBody] := originalDef
    signature := markFindOriginalSignature(form,sig)
    $from : local := '"compDefineFunctor1"
    markEncodeChanges(code,nil)
    frees := 
      null $markFreeStack => nil
      [['free,:mySort removeDuplicates $markFreeStack]]
    noriginalBody := markSpliceInChanges originalBody
    nbody := augmentBodyByLoopDecls noriginalBody
    ndef := ['DEF,form,signature,[nil for x in form],nbody]
    $freeStack   := [frees,:$freeStack]
    --------------------> import code <------------------
    imports      := $localImportStack
    subtractions := union($localDeclareStack,union($globalDeclareStack,
                      union($globalImportStack,signature)))
    if $insideCategoryIfTrue and $insideFunctorIfTrue then
      imports      := markCatsub imports
      subtractions := markCatsub subtractions
    imports      := [markMacroTran d for d in imports]
    subtractions := [markMacroTran d for d in subtractions]
    subtractions := union(subtractions, getImpliedImports imports)
    $importStack := [reduceImports SETDIFFERENCE(imports,subtractions),:$importStack]
    -------------------> import code <------------------
    entry := [originalDef,:ndef]
    capsuleStack('"Def",entry)
  nil

reduceImports x ==
  [k, o] := reduceImports1 x
  SETDIFFERENCE(o,k)

reduceImports1 x ==
  kills := nil
  others:= nil
  for y in x repeat 
    y is ['List,a] =>
      [k,o] := reduceImports1 [a]
      kills := union(y,union(k,kills))
      others:= union(o, others)
    rassoc(y,$globalImportDefAlist) => kills := insert(y,kills)
    others := insert(y, others)
  [kills, others]

getImpliedImports x ==
  x is [[op,:r],:y] => 
    op in '(List Enumeration) => union(r, getImpliedImports y)
    getImpliedImports y
  nil  
 
augmentBodyByLoopDecls body ==
  null $localLoopVariables => body
  lhs := 
    $localLoopVariables is [.] => first $localLoopVariables
    ['LISTOF,:$localLoopVariables]
  form := [":",lhs,$SmallInteger]
  body is ['SEQ,:r] => ['SEQ,form,:r]
  ['SEQ,form,['exit,1,body]]
    
markFindOriginalSignature(form,sig) ==
  target := $originalTarget
  id     := opOf form
  n      := #form
  cat :=
    target is ['Join,:.,u] => u
    target
  target isnt ['CATEGORY,.,:v] => sig
  or/[sig' for x in v | x is ['SIGNATURE,=id,sig'] and #sig' = n 
    and markFindCompare(sig',sig)] or sig

markFindCompare(sig',sig) ==
  macroExpand(sig',$e) = sig
       
--======================================================================
--        Capsule Function: Encode Changes on $coerceList
--======================================================================
--(WI a b) mean Was a Is b
--(WI c (WI d e) b) means Was d Is b
--(AUTOxxx p q (WI a b))     means a::q for reason xxx=SUBSET or HARD
--(ATOM nil (REPLACE (x)) y) means replace y by x
--(COLON :: A B)             means rewrite as A :: B  (or A @ B or A : B)
--(LAMBDA nil (REPLACE fn) y)means replace y by fn
--(REPPER nil per form)      means replace form by per(form)
--(FREESI nil (REPLACE decl) y) means replace y by fn

markEncodeChanges(x,s) ==
--x is a piece of target code
--s is a stack [a, b, ..., c] such that a < b < ...
--calls ..markPath.. to find the location of i in a in c (the orig expression),
--  where i is derived from x (it is the source component of x);
--  if markPath fails to find a path for i in c, then x is wrong!

--first time only: put ORIGNAME on property list of operators with a ; in name
  if null s then markOrigName x
  x is [fn,a,b,c] and MEMQ(fn,$markChoices) =>
    x is ['ATOM,.,['REPLACE,[y],:.],:.] and y in '(false true) => 'skip
    ---------------------------------------------------------------------- 
    if c then   ----> special case: DON'T STACK A nil!!!!
      i := getSourceWI c
      t := getTargetWI c
  --  sayBrightly ['"=> ",i,'" ---> "]
  --  sayBrightly ['" from ",a,'" to ",b]
      s := [i,:s]
--    pp '"==========="
--    pp x
    markRecord(a,b,s)
    markEncodeChanges(t,s)
  x is ['WI,p,q] or x is ['MI,p,q] =>
    i := getSourceWI p
    r := getTargetWI q
    r is [fn,a,b,c] and MEMQ(fn,$markChoices) =>
      t := getTargetWI c
--      sayBrightly ['"==> ",i,'" ---> "]
--      sayBrightly ['" from ",a,'" to ",b]
      s := [i,:s]
      markRecord(a,b,s)
      markEncodeChanges(t,s)
    i is [fn,:.] and fn in '(REPEAT COLLECT) => markEncodeLoop(i,r,s)
    t := getTargetWI r
    markEncodeChanges(t,[i,:s])
  x is ['PROGN,a,:.] and s is [[op,:.],:.] and op in '(REPEAT COLLECT) =>
    markEncodeChanges(a,s)
  x is ['TAGGEDreturn,a,[y,:.]] => markEncodeChanges(y,s)
  x is ['CATCH,a,y] => markEncodeChanges(y,s)
  atom x => nil
--  first x = IFCAR IFCAR s =>
--    for y in x for r in first s repeat markEncodeChanges(y,[r,:s])
  for y in x repeat markEncodeChanges(y,s)

markOrigName x ==
  x is [op,:r] =>
    op = 'TAGGEDreturn and x is [.,a,[y,:.]] => markOrigName y
    for y in r repeat markOrigName y     
    IDENTP op =>
      s := symbolName op
      k := charPosition(char '_;, s, 0)
      k > MAXINDEX s => nil
      origName := INTERN subString(s, k + 1)
      property(op, 'ORIGNAME) := origName
      REMPROP(op,'PNAME)
    markOrigName op
  nil

markEncodeLoop(i, r, s) ==  
  [.,:itl1, b1] := i   --op is REPEAT or COLLECT
  if r is ["%LET",.,a] then r := a
  r is [op1,:itl2,b2] and op1 in '(REPEAT COLLECT) =>
    for it1 in itl1 for it2 in itl2 repeat markEncodeChanges(it2,[it1,:s])
    markEncodeChanges(b2, [b1,:s])
  markEncodeChanges(r, [i,:s])
  
getSourceWI x ==
--Subfunction of markEncodeChanges
  x is ['WI,a,b] or x is ['MI,a,b] =>
    a is ['WI,:.] or a is ['MI,:.] => getSourceWI a
    markRemove a
  markRemove x

markRemove x ==
  atom x => x
  x is ['WI,a,b] or x is ['MI,a,b]  => markRemove a
  x is [fn,a,b,c] and MEMQ(fn,$markChoices) => 
    markRemove c
--x is ['TAGGEDreturn,:.] => x
  x is ['TAGGEDreturn,a,[x,m,t]] => ['TAGGEDreturn,a,[markRemove x,m,t]]
  [markRemove y for y in x]
 
getTargetWI x ==
--Subfunction of markEncodeChanges
  x is ['WI,a,b] or x is ['MI,a,b] => getTargetWI b
  x is ['PART,.,a] => getTargetWI a
  x

$shout1 := false
$shout2 := false
  
markRecord(source,target,u) ==
--Record changes on $coerceList
  if source='_$ and target='Rep then 
    target := 'rep
  if source='Rep and target='_$ then
    target := 'per
  item := first u
  FIXP item or item = $One or item = $Zero => nil
  item is ["-",a] and (FIXP a or a = $One or a = $Zero) => nil
  string? item => nil
  item is [op,.,t] and op in '( _:_: _@ _pretend)
    and macroExpand(t,$e) = target => nil
  $source: local := source
  $target: local := target
  path := markPath u or return nil       -----> early exit
  path := 
    path = 0 => nil     --wrap the WHOLE thing
    path
  if $shout2 then
      pp '"========="
      pp path
      ipath := reverse path
      for x in u repeat
        pp x
        ipath => 
           pp first ipath
           ipath := rest ipath
  entry := [source,target,:path]
  if $monitorCoerce then
    sayBrightlyNT ['"From ",$from,'": "]
    pp entry
  $coerceList := [COPY entry,:$coerceList]

--======================================================================
--  Capsule Function: Find dewey decimal path across a list
--======================================================================
markPath u ==        --u has nested structure: u0 < u1 < u2 ...
  whole := LAST u
  part  := first u
  $path := u
  u is [.] => 0      --means THE WHOLE THING
  v := reverse markPath1 u
--  pp '"======mark path======"
--  foobar v
--  pp v
--  pp markKillAll part
--  pp markKillAll whole
--  pp $source
--  pp $target
  null v => nil
  $pathStack := [[v,:u],:$pathStack]
--  pp '"----------------------------"
--  ppFull v
--  pp '"----------------------------"
  v

markPath1 u ==   
-- u is a list [a, b, ... c]
-- This function calls markGetPath(a,b) to find the location of a in b, etc.
-- The result is the successful path from a to c
-- A error printout occurs if no such path can be found
  u is [a,b,:r] =>  -- a < b < ...
    a = b => markPath1 rest u       ---> allow duplicates on path
    path := markGetPath(a,b) or return nil    -----> early exit
    if $shout1 then
      pp '"========="
      pp path
      pp a
      pp b
    [:first path,:markPath1 rest u]
  nil

$pathErrorStack := nil

markGetPath(x,y) ==    -- x < y  ---> find its location
  u := markGetPaths(x,y) 
  u is [w] => u
  $amb := [u,x,y]
  key :=
    null u => '"no match"
    '"ambiguous"
  sayBrightly ['"-----",key,'"--------"]
  $pathErrorStack := [$path,:$pathErrorStack]
  pp "CAUTION: this can cause RPLAC errors"
  pp "Paths are: "
  pp u
  for p in $path for i in 1..3 repeat pp p
  $x: local := x
  $y: local := y
  pp '"---------------------"
  pp x
  pp y
  foobar key
--  pp [key, $amb]
  null u => [1729] --return something that will surely fail if no path
  [first u]

markTryPaths() == markGetPaths($x,$y)

markPaths(x,y,s) ==    --x < y; find location s of x in y (initially s=nil)
--NOTES: This location is what it will be in the source program with
--  all PART information removed. 
  if $shout then
    pp '"-----"
    pp x
    pp y
    pp s
  x = y => s         --found it!  exit
  markPathsEqual(x,y) => s
  y is [['elt,.,op],:r] and (u := markPaths(x,[op,:r],s)) => u
  x is ['elt,:r] and (u := markPaths(r,y,s)) => u
  y is ['elt,:r] and (u := markPaths(x,r,s)) => u
  x is [op,:u] and op in '(LIST VECTOR) and y is ['construct,:v] and
    (p := markPaths(['construct,:u],y,s)) => p
  atom y => nil
  y is ["%LET",a,b] and IDENTP a => 
    markPaths(x,b,markCons(2,s)) --and IDENTP x
  y is ["%LET",a,b] and GENSYMP a => markPaths(x,b,s)     --for loops
  y is ['IF,a,b,:.] and GENSYMP a => markPaths(x,b,s)   --for loops
  y is ['IF,a,b,c] and (p := (markPathsEqual(x,b) => 2;
                              markPathsEqual(x,c) => 3;
                              nil)) => markCons(p,s)
--  x is ['exit,a,b] and y is ['exit,a,c] and (p := mymy markPathsEqual(b,c)) =>
--     markCons(p,s)
  y is ['%call,:r] => markPaths(x,r,s)                 --for loops
  y is [fn,m,y1] and fn in '(PART CATCH THROW) => markPaths(x,y1,s) or
    append/[markPaths(x,u,markCons(i,s)) for u in y1 for i in 0..]
  append/[markPaths(x,u,markCons(i,s)) for u in y for i in 0..]

mymy x == x

markCons(i,s) == [[i,:x] for x in s]

markPathsEqual(x,y) ==
  x = y => true
  x is ["::",.,a] and y is ["::",.,b] and 
    a = $Integer and b = $NonNegativeInteger => true
  y is [fn,.,z] and fn in '(PART CATCH THROW) and markPathsEqual(x,z) => true
  y is ["%LET",a,b] and GENSYMP a and markPathsEqual(x,b) => true
  y is ['IF,a,b,:.] and GENSYMP a => markPathsEqual(x,b)  -------> ??? 
  y is ['%call,:r] => markPathsEqual(IFCDR x,r)
  x is ['REDUCE,.,.,c,:.] and c is ['COLLECT,:u] and 
    y is ['PROGN,.,repeet,:.] and repeet is ['REPEAT,:v] => markPathsEqual(u,v)
  atom y or atom x => 
    IDENTP y and IDENTP x and y = GETL(x,'ORIGNAME)  => true --> see 
--  IDENTP y and IDENTP x and anySubstring?(symbolName y,symbolName x,0) => true
    IDENTP y and (z := markPathsMacro y) => markPathsEqual(x,z)
    false
  "and"/[markPathsEqual(u,v) for u in x for v in y]

markPathsMacro y ==
  LASSOC(y,$localMacroStack) or LASSOC(y,$globalMacroStack)
--======================================================================
--      Capsule Function: DO the transformations
--======================================================================
--called by markChanges (inside capsule), markSetq (outside capsule)
$hohum := false
markSpliceInChanges body ==
--  pp '"before---->"
--  pp $coerceList
  $coerceList := reverse SORTBY('CDDR,$coerceList)
--  pp '"after----->"
--  pp $coerceList
  $cl := $coerceList
--if CONTAINED('REPLACE,$cl) then hoho $cl
  body :=
    body is ['WI,:.] => 
--      hehe body
      markKillAll body
    markKillAll body
--NOTE!! Important that $coerceList be processed in this order
--since it must operate from the inside out. For example, a progression
--u --> u::Rep --> u :: Rep :: $ can only be correct. Here successive
--entries can have duplicate codes
  for [code,target,:loc] in $coerceList repeat
    $data: local := [code, target, loc]
    if $hohum then 
      pp '"---------->>>>>"
      pp $data
      pp body
      pp '"-------------------------->"
    body := markInsertNextChange body
  body

--pause() == 12
markInsertNextChange body ==
--  if BOUNDP '$sayChanges and $sayChanges then 
--    sayBrightlyNT '"Inserting change: "
--    pp $data
--    pp body
--    pause()
  [code, target, loc] := $data
  markInsertChanges(code,body,target,loc)

markInsertChanges(code,form,t,loc) ==
--RePLACe x at location "loc" in form as follows:
--  t is ['REPLACE,r]:   by r
--  t is 'rep/per:       by (rep x) or (per x)
--  code is @ : ::       by (@ x t) (: x t) (:: x t)
--  code is Lisp         by (pretend form t)
--  otherwise            by (:: form t)
  loc is [i,:r] =>
    x := form
    for j in 0..(i-1) repeat 
      if cons? x then x := rest x
    atom x => 
        pp '"Translator RPLACA error"
        pp $data
        foobum form
        form
    if $hohum then pp [i, '" >>> ", x]
    SETQ($CHANGE,COPY x)
    if x is ['elt,:y] and r then x := y
    x.first := markInsertChanges(code,first x,t,rest loc)
    chk(x,100)
    form
--  pp ['"Making change: ",code,form,t]
  t is ['REPLACE,r] => SUBST(form,"##1",r)
  form is ['SEQ,:y,['exit,1,z]] => 
    ['SEQ,:[markInsertSeq(code,x,t) for x in y],
      ['exit,1,markInsertChanges(code,z,t,nil)]]
  code = '_pretend or code = '_: => 
    form is [op,a,.] and op in '(_@ _: _:_: _pretend) => ['_pretend,a,t]
    [code,form,t]
  code in '(_@ _:_: _pretend) =>  
    form is [op,a,b] and op in '(_@ _: _:_: _pretend) =>
      op in '(_: _pretend) => form
      op = code and b = t => form
      markNumCheck(code,form,t)
    FIXP form and MEMQ(opOf t,$markPrimitiveNumbers) => ['_@,form,t]
    [code,form,t]
  code in '(_@ _:_: _:) and form is [op,a] and 
    (op='rep and t = 'Rep or op='per and t = "$") => form
  code = 'Lisp => 
    t = $EmptyMode => form
    ["pretend",form,t]
  t in '(rep per) => 
    t = 'rep and form is ["per",:.] => second form
    t = 'per and form is ["rep",:.] => second form
    [t,form]
  code is [op,x,t1] and op in '(_@ _: _:_: _pretend) and t1 = t => form
  FIXP form and MEMQ(opOf t,$markPrimitiveNumbers) => ['_@,form,t]
  markNumCheck("::",form,t)

markNumCheck(op,form,t) ==
  op = "::" and opOf t in '(Integer) =>
     s := form = $One and 1 or form = $Zero and 0 => ['DOLLAR, s , t]
     FIXP form                   => ["@", form, t]
     form is ["-", =$One]        => ['DOLLAR, -1,   t]
     form is ["-", n] and FIXP n => ["@", MINUS n, t]
     [op, form, t]
  [op,form,t]

markInsertSeq(code,x,t) ==
  x is ['exit,y] => ['exit,markInsertChanges(code,y,t,nil)]
  atom x => x
  [markInsertSeq(code,y,t) for y in x]
--======================================================================
--               Prettyprint of translated program
--======================================================================
markFinish(body,T) ==
--called by compDefineCategory2, compDefineFunctor1 (early jumpout)
  SETQ($cs,$capsuleStack)
  SETQ($ps,$predicateStack)
  SETQ($ss,$signatureStack)
  SETQ($os,$originalTarget)
  SETQ($gis,$globalImportStack)
  SETQ($gds,$globalDeclareStack)
  SETQ($gms,$globalMacroStack)
  SETQ($as, $abbreviationStack)
  SETQ($lms,$localMacroStack)
  SETQ($map,$macrosAlreadyPrinted)
  SETQ($gs,$importStack)
  SETQ($fs,$freeStack)
  SETQ($b,body)
  SETQ($t,T)
  SETQ($e,T.env)
--if $categoryTranForm then SETQ($t,$categoryTranForm . 1)
  atom CDDR T => systemError()
  T.rest.rest.first := $EmptyEnvironment
  chk(CDDR T,101)
  markFinish1()
  T

reFinish() ==
  $importStack := $gs
  $freeStack := $fs
  $capsuleStack := $cs
  $predicateStack := $ps
  $signatureStack := $ss
  $originalTarget := $os
  $globalMacroStack := $gms
  $abbreviationStack:= $as
  $globalImportStack := $gis
  $globalDeclareStack := $gds
  $localMacroStack := $lms
  $macrosAlreadyPrinted := $map
  $abbreviationsAlreadyPrinted := nil
  markFinish1()
 
markFinish1() ==
  body := $b
  T    := $t
  $predGensymAlist: local := nil
--$capsuleStack := $cs
--$predicateStack := $ps
  form := T. expr
  ['Mapping,:sig] := T.mode
  if $insideCategoryIfTrue and $insideFunctorIfTrue then
     $importStack       := [delete($categoryNameForDollar,x) for x in $importStack]
     $globalImportStack := delete($categoryNameForDollar,$globalImportStack)
  $commonImports : local := getCommonImports()
  globalImports := 
    reverse orderByContainment removeDuplicates [:$commonImports,:$globalImportStack]
  $finalImports: local := SETDIFFERENCE(globalImports,$globalDeclareStack)
  $capsuleStack := 
    [mkNewCapsuleItem(freepart,imports,x) for freepart in $freeStack 
       for imports in $importStack for x in $capsuleStack] 
  $extraDefinitions := combineDefinitions()
  addDomain := nil
  initbody :=
    $b is ['add,a,b] => 
      addDomain := a
      b
    $b is [op,:.] and constructor? op =>
      addDomain := $b
      nil
    $b
  body := markFinishBody initbody
  importCode := [['import,x] for x in $finalImports]
  leadingMacros := markExtractLeadingMacros(globalImports,body)
  body := markRemImportsAndLeadingMacros(leadingMacros,body)
  initcapsule := 
    body => ['CAPSULE,:leadingMacros,:importCode,:body]
    nil
  capsule := 
--  null initcapsule => addDomain
    addDomain => ['add,addDomain,initcapsule]
    initcapsule
  nsig :=
    $categoryPart => sig
    ['Type,:rest sig]
  for x in reverse $abbreviationStack |not member(x,$abbreviationsAlreadyPrinted) repeat 
     markPrintAbbreviation x
     $abbreviationsAlreadyPrinted := insert(x,$abbreviationsAlreadyPrinted)
  for x in reverse $globalMacroStack|not member(x,$macrosAlreadyPrinted) repeat
    $def := ['MDEF,first x,'(NIL),'(NIL),rest x]
    markPrint(true)
    $macrosAlreadyPrinted := insert(x,$macrosAlreadyPrinted)
  if $insideCategoryIfTrue and not $insideFunctorIfTrue then
    markPrintAttributes $b
  $def := ['DEF,form,nsig,[nil for x in form],capsule]
  markPrint()

stop x == x

getNumberTypesInScope() ==
  union([y for x in $localImportStack | MEMQ(y := opOf x,$markNumberTypes)], 
        [y for x in $globalImportStack| MEMQ(y := opOf x,$markNumberTypes)])

getCommonImports() ==
  importList := [x for x in $importStack for y in $capsuleStack |
                   KAR KAR y = 'DEF]
  hash := hashTable 'EQUAL
  for x in importList repeat
    for y in x repeat HPUT(hash,y,1 + (HGET(hash,y) or 0))
  threshold := FLOOR (.5 * #importList)
  [x for x in HKEYS hash | HGET(hash,x) >= threshold]
  
markPrintAttributes addForm ==
  capsule :=
    addForm is ['add,a,:.] => 
      a is ['CATEGORY,:.] => a
      a is ['Join,:.] => first LASTNODE a
      first LASTNODE addForm
    addForm
  if capsule is ['CAPSULE,:r] then
    capsule := first LASTNODE r
  capsule isnt ['CATEGORY,.,:lst] => nil
  for x in lst | x is ['ATTRIBUTE,att] repeat
    markSay(form2String att)
    markSay('": Category == with")
    markTerpri()
    markTerpri()

getCommons u ==
  common := KAR u
  while common and u is [x,:u] repeat common := intersection(x,common)
  common

markExtractLeadingMacros(globalImports,body) ==
  [x for x in body | x is ['MDEF,[a],:.] and member(a,globalImports)]
  
markRemImportsAndLeadingMacros(leadingMacros,body) ==
  [x for x in body | x isnt ['import,:.] and not member(x,leadingMacros)]

mkNewCapsuleItem(frees,i,x) ==
  [originalDef,:ndef] := x
  imports := reverse orderByContainment removeDuplicates SETDIFFERENCE(i,$finalImports)
  importPart := [["import",d] for d in imports]
  nbody := 
    ndef is ["%LET",.,x] => x
    ndef is ['DEF,.,.,.,x] => x
    ndef
  newerBody :=
    newPart := [:frees,:importPart] =>
      nbody is ['SEQ,:y] => ['SEQ,:newPart,:y]
      ['SEQ,:newPart,['exit,1,nbody]]
    nbody
  newerDef := 
    ndef is ["%LET",a,x] => ["%LET",a,newerBody]
    ndef is ['DEF,a,b,c,x] => ['DEF,a,b,c,newerBody]
    newerBody
  entry := [originalDef,:newerDef]
  entry

markFinishBody capsuleBody ==
  capsuleBody is ['CAPSULE,:itemlist] =>
    if $insideCategoryIfTrue and $insideFunctorIfTrue then
       itemlist := markCatsub itemlist
    [:[markFinishItem x for x in itemlist],:$extraDefinitions]
  nil

markCatsub x == SUBST("$",$categoryNameForDollar,x)
 
markFinishItem x ==
  $macroAlist : local := [:$localMacroStack,:$globalMacroStack]
  if $insideCategoryIfTrue and $insideFunctorIfTrue then
    $macroAlist := [["$",:$categoryNameForDollar],:$macroAlist]
  x is ['DEF,form,.,.,body] =>
    "or"/[new for [old,:new] in $capsuleStack |
        old is ['DEF,oform,.,.,obody] 
          and markCompare(form,oform) and markCompare(body,obody)] or
            pp '"------------MISSING----------------"
            $f := form
            $b := body
            newform := "or"/[x for [old,:new] in $capsuleStack | 
              old is ['DEF,oform,.,.,obody] and oform = $f]
            $ob:= (newform => obody; nil)
            pp $f
            pp $b
            pp $ob
            foobum x
            pp x
            x
  x is ["%LET",lhs,rhs] =>
    "or"/[new for [old,:new] in $capsuleStack |
        old is ["%LET",olhs,orhs]
          and markCompare(lhs,olhs) and markCompare(rhs,orhs)]
            or x
  x is ['IF,p,a,b] => ['IF,p,markFinishItem a,markFinishItem b]
  x is ['SEQ,:l,['exit,n,a]] =>
    ['SEQ,:[markFinishItem y for y in l],['exit,n,markFinishItem a]]
  "or"/[new for [old,:new] in $capsuleStack | markCompare(x,old)] =>
    new
  x
 
markCompare(x,y) == 
  markKillAll(SUBLIS($macroAlist,x)) = markKillAll(SUBLIS($macroAlist,y))

diffCompare(x,y) == diff(SUBLIS($macroAlist,x),markKillAll(SUBLIS($macroAlist,y)))
 
--======================================================================
--               Print functions
--======================================================================
markPrint(:options) ==   --print $def 
  noTrailingSemicolonIfTrue := IFCAR options
--$insideCategoryIfTrue and $insideFunctorIfTrue => nil
  $DEFdepth : local := 0
  [op,form,sig,sclist,body] := markKillAll $def
  if $insideCategoryIfTrue then
    if op = 'DEF and $insideFunctorIfTrue then
      T := $categoryTranForm . 1
      form := T . expr
      sig  := rest (T . mode)
    form := SUBLISLIS(rest markConstructorForm opOf form,
              $TriangleVariableList,form)
    sig  := SUBLISLIS(rest markConstructorForm opOf form,
              $TriangleVariableList,sig)
  nbody := body
  if $insideCategoryIfTrue then
    if $insideFunctorIfTrue then
      nbody := replaceCapsulePart body
      nbody :=
        $catAddForm => ['withDefault, $catAddForm, nbody]
        nbody
    else      
      ['add,a,:r] := $originalBody
      xtraLines := 
        "append"/[[strconc(name,'": Category == with"),'""] 
           for name in markCheckForAttributes a]
      nbody :=
        $originalBody is ['add,a,b] =>
          b isnt ['CAPSULE,:c] => error(false)
          [:l,x] := c
          [:markTranCategory a,['default,['SEQ,:l,['exit,1,x]]]]
        markTranCategory $originalBody      
  signature :=
    $insideFunctorIfTrue => [markTranJoin $originalTarget,:rest sig]
    $insideCategoryIfTrue => ['Category,:rest sig]
    '(NIL)
  $bootForm:= 
    op = 'MDEF => [op,form,signature,sclist,body]
    [op,form,signature,sclist,nbody]
  bootLines:= lisp2Boot $bootForm
  $bootLines:= [:xtraLines,:bootLines]
  moveAroundLines()
  markSay $bootLines
  markTerpri()
  'done

replaceCapsulePart body == 
  body isnt ['add,['CAPSULE,:c]] => body
  $categoryTranForm . 0 isnt ['add,exports,['CAPSULE,:.]] => error(false) 
  [:l,x] := c
  [:markTranCategory exports,['default,['SEQ,:l,['exit,1,x]]]]

foo(:x) == 
 arg := IFCAR x or $bootForm
 markSay lisp2Boot arg

markPrintAbbreviation [kind,a,:b] == 
  markSay '"--)abbrev "
  markSay kind
  markSay '" "
  markSay a
  markSay '" "
  markSay b
  markTerpri()

markSay s == 
  cons? s =>
    for x in s repeat
      (markSay(lispStringList2String x); markTerpri())
  PRINTEXP s
  if $outStream then PRINTEXP(s,$outStream)

markTerpri() ==
  TERPRI()
  if $outStream then TERPRI($outStream)

markTranJoin u ==                      --subfunction of markPrint
  u is ['Join,:.] => markTranCategory u
  u

markTranCategory cat ==               
  cat is ['CATEGORY,:.] => cat
  cat is ['Join,:r] =>
    r is [:s,b] and b is ['CATEGORY,k,:t] => ['CATEGORY,k,:s,:markSigTran t] 
    ['CATEGORY,'domain,:markSigTran r]
  ['CATEGORY,'domain,cat]

markSigTran t == [markElt2Apply x for x in t]

markElt2Apply x ==
  x is ["SIGNATURE", "elt", :r] => ['SIGNATURE, 'apply, :r]
  x

markCheckForAttributes cat ==          --subfunction of markPrint
  cat is ['Join,:r] => markCheckForAttributes last r
  cat is ['CATEGORY,.,:r] => [u for x in r | u := fn(x)] where fn(x) ==
    x is ['ATTRIBUTE,form,:.] => 
      name := opOf form
      MEMQ(name,$knownAttributes) => nil
      $knownAttributes := [name,:$knownAttributes]
      name
    nil
  nil

--======================================================================
--        Put in PARTs in code
--======================================================================
$partChoices := '(construct IF)
$partSkips   := '(CAPSULE with add)
unpart x ==
  x is ['PART,.,y] => y
  x

markInsertParts df ==
  $partNumber := 0
  ["DEF",form,a,b,body] := df
--if form is [op,:r] and (u := LASSOC(op,$opRenameAlist)) 
--  then form := [u,:r]
  ['DEF,form,a,b,markInsertBodyParts body]
  
markInsertBodyParts u ==
  u is ['Join,:.] or u is ['CATEGORY,:.] => u
  u is ['DEF,f,a,b,body] => ['DEF,f,a,b,markInsertBodyParts body]
  u is ['SEQ,:l,['exit,n,x]] =>
    ['SEQ,:[markInsertBodyParts y for y in l],
           ['exit,n,markInsertBodyParts x]]
  u is [op,:l] and op in '(REPEAT COLLECT) => markInsertRepeat u
  u is ["%LET",["%Comma",:s],b] =>
    ["%LET",["%Comma",:[markWrapPart x for x in s]],markInsertBodyParts b]
--u is ["%LET",a,b] and constructor? opOf b => u
  u is ["%LET",a,b] and a is [op,:.] =>
    ["%LET",[markWrapPart x for x in a],markInsertBodyParts b]
  u is [op,a,b] and op in '(add with IN %LET) =>
    [op,markInsertBodyParts a,markInsertBodyParts b]
  u is [op,a,b] and op in '(_: _:_: pretend _@) =>
    [op,markInsertBodyParts a,b]
  u is [op,a,:x] and op in '(STEP _return _leave exit reduce) => 
    [op,a,:[markInsertBodyParts y for y in x]]
  u is [op,:x] and markPartOp? op => [op,:[markWrapPart y for y in x]]
  u is [op,:.] and constructor? op => u
  atom u => markWrapPart u
            ------------           <--------------94/10/11
  [markInsertBodyParts x for x in u]

markPartOp? op ==
  MEMQ(op,$partChoices) => true
  MEMQ(op,$partSkips)   => false
  if op is ['elt,.,o] then op := o
  GETL(op,'special) => false
  true

markWrapPart y ==
----------------new definition----------94/10/11
  atom y => 
    y = '%noBranch => y
    GETL(y, 'SPECIAL) => y 
    $partNumber := $partNumber + 1
    ['PART,$partNumber, y] 
  ['PART,$partNumber := $partNumber + 1,markInsertBodyParts y]

markInsertRepeat [op,:itl,body] ==
  nitl := [markInsertIterator x for x in itl]
  nbody := 
--->IDENTP body => markWrapPart body
----------------new definition----------94/10/11
    markInsertBodyParts body
  [op,:nitl,nbody]

markInsertIterator x ==
  x is ['STEP,k,:r]  => ['STEP,markWrapPart k,:[markWrapPart x for x in r]]
  x is ['IN,p,q]     => ['IN,markWrapPart p,markWrapPart q]
  x is ["|",p]       => ["|",markWrapPart p]
  x is ['WHILE,p]    => ['WHILE,markWrapPart p]
  x is ['UNTIL,p]    => ['UNTIL,markWrapPart p]
  systemError()
  
--======================================================================
--        Kill Function: MarkedUpCode --> Code
--======================================================================

markKillExpr m ==    --used to kill all but PART information for compilation
  m is [op,:.] =>
    op in '(MI WI) => markKillExpr third m
    op in '(AUTOHARD AUTOSUBSET AUTOREP) => markKillExpr fourth m
    m is ['TAGGEDreturn,a,[x,m,e]] => ['TAGGEDreturn, a, [markKillExpr x,m,e]]
    [markKillExpr x for x in m]
  m
 
markKillButIfs m ==    --used to kill all but PART information for compilation
  m is [op,:.] =>
    op = 'IF => m
    op = 'PART        => markKillButIfs third m
    op in '(MI WI) => markKillButIfs third m
    op in '(AUTOHARD AUTOSUBSET AUTOREP) => markKillButIfs fourth m
    m is ['TAGGEDreturn,a,[x,m,e]] => ['TAGGEDreturn, a, [markKillButIfs x,m,e]]
    [markKillButIfs x for x in m]
  m
 
markKillAll m ==      --used to prepare code for compilation
  m is [op,:.] =>
    op = 'PART        => markKillAll third m
    op in '(MI WI) => markKillAll third m
    op in '(AUTOHARD AUTOSUBSET AUTOREP) => markKillAll fourth m
    m is ['TAGGEDreturn,a,[x,m,e]] => ['TAGGEDreturn, a, [markKillAll x,m,e]]
    [markKillAll x for x in m]
  m
 
--======================================================================
--                Moving lines up/down 
--======================================================================
moveAroundLines() ==
  changeToEqualEqual $bootLines
  $bootLines := moveImportsAfterDefinitions $bootLines  

changeToEqualEqual lines ==
--rewrite A := B as A == B whenever A is an identifier and
--                                  B is a constructor name (after macro exp.)
  origLines := lines
  while lines is [x, :lines] repeat
    N := MAXINDEX x
    (n := charPosition($blank, x, 8)) > N => nil
    n = 0 => nil
    not alphabetic? (x . (n - 1)) => nil
    not substring?('":= ", x, n+1) => nil
    m := n + 3
    while (m := m + 1) <= N and alphabetic? (x . m) repeat nil
    m = n + 2 => nil
    not upperCase? (x . (n + 4)) => nil
    word := INTERN subString(x, n + 4, m - n - 4)
    expandedWord := macroExpand(word,$e)
    not (word in '(Record Union Mapping)
      or getConstructorFormFromDB opOf expandedWord) => nil
    sayMessage '"Converting input line:"
    sayMessage ['"WAS: ", x]
    x . (n + 1) := char '_= ;
    sayMessage ['"IS:  ", x]
    TERPRI()
  origLines
    
sayMessage x == 
  u := 
    atom x => ['">> ", x]
    ['">> ",: x]
  sayBrightly u
  
moveImportsAfterDefinitions lines ==
  al := nil
  for x in lines for i in 0.. repeat
    N := MAXINDEX x
    m := firstNonBlankPosition x
    m < 0 => nil
    ((n := charPosition($blank ,x,1 + m)) < N) and
      substring?('"== ", x, n+1) => 
        name := subString(x, m, n - m)
        defineAlist := [[name, :i], :defineAlist]
    (k := leadingSubstring?('"import from ",x, 0)) =>
      importAlist := [[subString(x,k + 12), :i], :importAlist]
--  pp defineAlist
--  pp importAlist
  for [name, :i] in defineAlist repeat
    or/[fn for [imp, :j] in importAlist] where fn() ==
      substring?(name,imp,0) =>
        moveAlist := [[i,:j], :moveAlist]
      nil
  null moveAlist => lines
  moveLinesAfter(mySort moveAlist, lines)

leadingSubstring?(part, whole, :options) ==
  after := IFCAR options or 0
  substring?(part, whole, k := firstNonBlankPosition(whole, after)) => k
  false

stringIsWordOf?(s, t, startpos) ==
  maxindex := MAXINDEX t
  (n := stringPosition(s, t, startpos)) > maxindex => nil
  wordDelimiter? t . (n - 1)
  n = maxindex or wordDelimiter? t . (n + #s)

wordDelimiter? c == or/[CHAR_=(c,('"() ,;").i) for i in 0..4]

moveLinesAfter(alist, lines) ==
  n := #lines
  acc := nil
  for i in 0..(n - 1) for x in lines repeat
    (p :=  ASSOC(i, alist)) and string? rest p => acc := [rest p, x, :acc]
    (p :=  lookupRight(i, alist)) and (first p) > i => p.rest := x
    acc := [x, :acc]
  reverse acc  
  
lookupRight(x, al) == 
  al is [p, :al] =>
    x = rest p => p
    lookupRight(x, al)
  nil

--======================================================================
--                Utility Functions
--======================================================================
  
ppEnv [ce,:.] ==
  for env in ce repeat
    for contour in env repeat
      pp contour
    
diff(x,y) ==
  for [p,q] in (r := diff1(x,y)) repeat 
    pp '"------------"
    pp p
    pp q
  #r
 
diff1(x,y) ==
  x = y => nil
  atom x or atom y => [[x,y]]
  #x ~= #y => [x,y]
  append/[diff1(u,v) for u in x for v in y]
    
markConstructorForm name ==  --------> same as getConstructorForm
  name = 'Union   => '(Union  (_: a A) (_: b B))
  name = 'UntaggedUnion => '(Union A B)
  name = 'Record  => '(Record (_: a A) (_: b B))
  name = 'Mapping => '(Mapping T S)
  getConstructorFromDB name

--======================================================================
--                new path functions
--======================================================================

$newPaths := false
  
markGetPaths(x,y) == 
  $newPaths => 
--  res := reverseDown mkGetPaths(x, y)
    res := mkGetPaths(x, y)
--    oldRes := markPaths(x,y,[nil])
--    if res ~= oldRes then $badStack := [[x, :y], :$badStack]
--    oldRes
  markPaths(x,y,[nil])
 
mkCheck() ==
  for [x, :y] in removeDuplicates $badStack repeat
    pp '"!!-------------------------------!!"
    res := mkGetPaths(x, y)
    oldRes := markPaths(x, y, [nil])
    pp x
    pp y
    sayBrightlyNT '"new: "
    pp res
    sayBrightlyNT '"old: "
    pp oldRes

reverseDown u == [reverse x for x in u]

mkCheckRun() ==
  for [x, :y] in removeDuplicates $badStack repeat
    pp mkGetPaths(x,y)

mkGetPaths(x,y) ==
  u := removeDuplicates mkPaths(x,y) => getLocationsOf(u,y,nil)
  nil   

mkPaths(x,y) ==   --x < y; find location s of x in y (initially s=nil)
  markPathsEqual(x,y) => [y]
  atom y => nil
  x is [op, :u] and op in '(LIST VECTOR) and y is ['construct,:v] 
    and markPathsEqual(['construct,:u],y) => [y]
  (y is ["%LET",a,b] or y is ['IF,a,b,:.]) and GENSYMP a and markPathsEqual(x,b) => [y]
  y is ['%call,:r] => 
--  markPathsEqual(x,y1) => [y]
    mkPaths(x,r) => [y]
  y is ['PART,.,y1] => mkPaths(x,y1)
  y is [fn,.,y1] and fn in '(CATCH THROW) =>
--  markPathsEqual(x,y1) => [y]
    mkPaths(x,y1) => [y]
  y is [['elt,.,op],:r] and (u := mkPaths(x,[op,:r])) => u
  x is ['elt,:r] and (u := mkPaths(r,y)) => u
  y is ['elt,:r] and (u := mkPaths(x,r)) => u
  append/[u for z in y | u := mkPaths(x,z)]

getLocationsOf(u,y,s) == [getLocOf(x,y,s) for x in u]

getLocOf(x,y,s) ==
  x = y or x is ['elt,:r] and r = y => s
  y is ['PART,.,y1] => getLocOf(x,y1,s)
  if y is ['elt,:r] then y := r
  atom y => nil
  or/[getLocOf(x,z,[i, :s]) for i in 0.. for z in y]

  
--======================================================================
--           Combine Multiple Definitions Into One
--======================================================================

combineDefinitions() ==
--$capsuleStack has form   (def1  def2  ..)
--$signatureStack has form (sig1  sig2  ..) where sigI = nil if not a def
--$predicateStack has form (pred1 pred2 ..)
--record in $hash: alist of form [[sig, [predl, :body],...],...] under each op
  $hash  := MAKE_-HASH_-TABLE()
  for defs in $capsuleStack 
    for sig in $signatureStack 
      for predl in $predicateStack | sig repeat
--      pp [defs, sig, predl]
        [["DEF",form,:.],:.] := defs
        item := [predl, :defs]
        op := opOf form
        oldAlist := HGET($hash,opOf form) 
        pair := ASSOC(sig, oldAlist) => pair.rest := [item,:rest pair]
        HPUT($hash, op, [[sig, item], :oldAlist])
--extract and combine multiple definitions
  Xdeflist := nil
  for op in HKEYS $hash repeat
    $acc: local := nil
    for [sig,:items] in HGET($hash,op) | (k := #items) > 1 repeat
      for i in 1.. for item in items repeat
        [predl,.,:def]    := item
        ['DEF, form, :.] := def
        ops := PNAME op
        opName := INTERN(strconc(ops,'"X",STRINGIMAGE i))
        form.first := opName
--      rplacaSubst(op, opName, def)
        $acc := [[form,:predl], :$acc]
      Xdeflist := [buildNewDefinition(op,sig,$acc),:Xdeflist]
  reverse Xdeflist
               
rplacaSubst(x, y, u) == (fn(x, y, u); u) where fn(x,y,u) ==
  atom u => nil
  while u is [p, :q] repeat
    if EQ(p, x) then u.first := y
    if cons? p then fn(x, y, p)
    u := q
    
buildNewDefinition(op,theSig,formPredAlist) ==
  newAlist := [fn for item in formPredAlist] where fn() ==
    [form,:predl] := item
    pred :=
      null predl => 'T
      boolBin simpHasPred markKillAll MKPF(predl,"and") 
    [pred, :form]
  --make sure that T comes as last predicate
  outerPred := boolBin simpHasPred MKPF(ASSOCLEFT newAlist,"or")
  theForm := CDAR newAlist
  alist := moveTruePred2End newAlist
  theArgl := rest theForm
  theAlist := [[pred, first form, :theArgl] for [pred,:form] in alist]
  theNils := [nil for x in theForm]
  thePred :=
     outerPred in '(T %true) => nil
     outerPred
  def := ['DEF, theForm, theSig, theNils, ifize theAlist]
  value :=
    thePred => ['IF, thePred, def, '%noBranch]
    def
  stop value 
  value

boolBin x ==
  x is [op,:argl] =>
    op in '(AND OR) and argl is [a, b, :c] and c => boolBin [op, boolBin [op, a, b], :c]
    [boolBin y for y in x]
  x

ifize [[pred,:value],:r] ==
  null r => value
  ['IF, pred, value, ifize r]
  
moveTruePred2End alist ==
  truthPair := or/[pair for pair in alist | pair is ["T",:.]] =>
    [:delete(truthPair, alist), truthPair]      
  [:a, [lastPair, lastValue]] := alist
  [:a, ["T", lastValue]]

PE e ==
  for x in CAAR e for i in 1.. repeat
    ppf [i, :x]

ppf x ==
  _*PRETTYPRINT_* : local := true
  PRINT_-FULL x


--%
for x in [["%LET", :"compSetq"],_
          ["Join", :"compJoin"],_
          ["Record", :"compCat"],_
          ["Union", :"compCat"],_
          ["_:", :"compColon"],_
          ["_:_:", :"compCoerce"],_
          ["CAPSULE", :"compCapsule"],_
          ["has", :"compHas"],_
          ["is", :"compIs"],_
          ["add", :"compAdd"],_
          ["CONS", :"compCons"],_
          ["IF", :"compIf"],_
          ["exit", :"compExit"],_
          ["return", :"compReturn"],_
          ["return", :"compLeave"],_
          ["elt", :"compElt"],_
          ["DEF", :"compDefine"],_
          ["MDEF", :"compMacro"],_
          ["SubsetCategory", :"compSubsetCategory"],_
          ["SubDomain", :"compSubDomain"],_
          ["case", :"compCase"],_
          ["RecordCategory", :"compConstructorCategory"],_
          ["ListCategory", :"compConstructorCategory"],_
          ["VectorCategory", :"compConstructorCategory"],_
          ["UnionCategory", :"compConstructorCategory"],_
          ["CATEGORY", :"compCategory"],_
          ["COLLECT", :"compRepeatOrCollect"],_
          ["COLLECTV", :"compCollectV"],_
          ["REPEAT", :"compRepeatOrCollect"],_
          ["REDUCE", :"compReduce"],_
          ["where", :"compWhere"],_
          ["_|", :"compSuchthat"],_
          ["construct", "compConstruct"],_
          ["SEQ", :"compSeq"],_
          ["SETQ", :"compSetq"],_
          ["VECTOR", :"compVector"]] repeat
  property(first x, 'special) := rest x
