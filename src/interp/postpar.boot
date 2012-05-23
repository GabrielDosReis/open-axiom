-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
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


import sys_-macros
namespace BOOT

module postpar

++ The type of parse trees.
%ParseTree <=> 
  %Number or %Symbol or %String or %Pair(%Thing,%Thing)

++ The result of processing a parse tree.
%ParseForm <=>
  %Number or %Symbol or %String or %Pair(%Thing,%Thing)

$postStack := []

--% Yet Another Parser Transformation File
--These functions are used by for BOOT and SPAD code
--(see new2OldLisp, e.g.)

postTransform: %ParseTree -> %ParseForm
postTransform y ==
  x:= y
  u:= postTran x
  if u is ["%Comma",:l,[":",y,t]] and (and/[ident? x for x in l]) then u:=
    [":",["LISTOF",:l,y],t]
  postTransformCheck u
  u

displayPreCompilationErrors() ==
  n:= #($postStack:= removeDuplicates reverse! $postStack)
  n=0 => nil
  errors:=
    1<n => '"errors"
    '"error"
  heading:=
    $topOp ~= '$topOp => ['"   ",$topOp,'" has"]
    ['"   You have"]
  sayBrightly [:heading,'"%b",n,'"%d",'"precompilation ",errors,'":"]
  if 1<n then
    (for x in $postStack for i in 1.. repeat sayMath ['"   ",i,'"_) ",:x])
    else sayMath ['"    ",:first $postStack]
  TERPRI()

postTran: %ParseTree -> %ParseForm
postTran x ==
  x isnt [.,:.] =>
    postAtom x
  op := first x
  op is 'QUOTE => x
  symbol? op and (f:= property(op,'postTran)) => FUNCALL(f,x)
  op is ["elt",a,b] =>
    u:= postTran [b,:rest x]
    [postTran op,:rest u]
  postForm x

postTranList: %List %ParseTree -> %List %ParseForm
postTranList x == 
  [postTran y for y in x]

postBigFloat: %ParseTree -> %ParseTree
postBigFloat x ==
  [.,mant,:expon] := x
  postTran [['elt,$Float,"float"],[",",[",",mant,expon],10]]

postAdd: %ParseTree -> %ParseForm
postAdd x ==
  x isnt ["add",a,:b] => systemErrorHere ["postAdd",x]
  b=nil => postCapsule a
  ["add",postTran a,postCapsule first b]

checkWarning: %Thing -> %Thing
checkWarning msg == 
  postError concat('"Parsing error: ",msg)
 
checkWarningIndentation: () -> %Thing
checkWarningIndentation() ==
  checkWarning ['"Apparent indentation error following",:bright "add"]

postCapsule: %ParseTree -> %ParseForm
postCapsule x ==
  x isnt [op,:.] => checkWarningIndentation()
  integer? op or op = "==" => ["CAPSULE",postBlockItem x]
  op = ";" => ["CAPSULE",:postBlockItemList postFlatten(x,";")]
  op = "if" => ["CAPSULE",postBlockItem x]
  checkWarningIndentation()

postColon: %ParseTree -> %ParseForm
postColon u ==
  u is [":",x] => [":",postTran x]
  u is [":",x,y] => [":",postTran x,:postType y]

postAtSign: %ParseTree -> %ParseForm
postAtSign t == 
  t isnt ["@",x,y] => systemErrorHere ["postAtSign",t]
  ["@",postTran x,:postType y]

postPretend: %ParseTree -> %ParseForm
postPretend t == 
  t isnt ["pretend",x,y] => systemErrorHere ["postPretend",t]
  ["pretend",postTran x,:postType y]

postConstruct: %ParseTree -> %ParseForm
postConstruct u ==
  u is ["construct",b] =>
    a:= (b is [",",:.] => ["%Comma",:postFlatten(b,",")]; b)
    a is ["SEGMENT",p,q] => ["construct",postTranSegment(p,q)]
    a is ["%Comma",:l] =>
      or/[x is [":",y] for x in l] => postMakeCons l
      or/[x is ["SEGMENT",:.] for x in l] => tuple2List l
      ["construct",:postTranList l]
    ["construct",postTran a]
  u

postError: %Thing -> %Thing
postError msg ==
  BUMPERRORCOUNT 'precompilation
  xmsg:=
    $defOp ~= nil => [$defOp,'": ",:msg]
    msg
  $postStack:= [xmsg,:$postStack]
  nil

postMakeCons: %ParseTree -> %ParseForm
postMakeCons l ==
  null l => "nil"
  l is [[":",a],:l'] =>
    l' => ["append",postTran a,postMakeCons l']
    postTran a
  ["cons",postTran first l,postMakeCons rest l]

postAtom: %Atom -> %ParseForm
postAtom x ==
  x is 0 => $Zero
  x is 1 => $One
  x is "," => "%Comma"
  ident? x =>
    niladicConstructor? x => [x]
    normalizeName x
  x

postBlock: %ParseTree -> %ParseForm
postBlock t ==
  t isnt ["%Block",:l,x] => systemErrorHere ["postBlock",t]
  ["SEQ",:postBlockItemList l,["exit",postTran x]]

postBlockItemList: %List %ParseTree -> %List %ParseTree
postBlockItemList l == 
  [postBlockItem x for x in l]

postBlockItem: %ParseTree -> %ParseForm
postBlockItem x ==
  x:= postTran x
  x is ["%Comma",:l,[":",y,t]] and (and/[ident? x for x in l]) =>
    [":",["LISTOF",:l,y],t]
  x

postCategory: %ParseTree -> %ParseForm
postCategory u ==
  u isnt ["CATEGORY",:l] => systemErrorHere ["postCategory",u]
  --RDJ: ugh_ please -- someone take away need for PROGN as soon as possible
  null l => u
  op :=
    $insidePostCategoryIfTrue => "PROGN"
    "CATEGORY"
  [op,:[fn x for x in l]] where fn x ==
    $insidePostCategoryIfTrue: local := true
    postTran x

postComma: %ParseTree -> %ParseForm
postComma u == 
  post%Comma ["%Comma",:postFlatten(u,",")]

++ post-parse `x' as the left hand side of a definition.
postLhsOfDefinition x ==
  x is [":",op,t] => [":",postLhsOfDefinition op,:postType t]
  x is [op,:args] =>
    args := postTranList args
    if args is [['%Comma,:args']] then
      args := args'
    [internalName op,:args]
  internalName x

postDef: %ParseTree -> %ParseForm
postDef t ==
  t isnt [defOp,lhs,rhs] => systemErrorHere ["postDef",t]
  lhs is ["macro",name] => postMDef ["==>",name,rhs]

  recordHeaderDocumentation nil
  if $maxSignatureLineNumber ~= 0 then
    $docList := [["constructor",:$headerDocumentation],:$docList]
    $maxSignatureLineNumber := 0
    --reset this for next constructor; see recordDocumentation
  lhs := postLhsOfDefinition lhs
  [form,targetType]:=
    lhs is [":",:.] => rest lhs
    [lhs,nil]
  if form isnt [.,:.] then form := [form]
  newLhs:=
    form isnt [.,:.] => form
    [op,:argl]:= [(x is [":",a,.] => a; x) for x in form]
    [op,:postDefArgs argl]
  argTypeList:=
    form isnt [.,:.] => nil
    [(x is [":",.,t] => t; nil) for x in rest form]
  typeList:= [targetType,:argTypeList]
  if form isnt [.,:.] then form := [form]
  ["DEF",newLhs,typeList,postTran rhs]

postDefArgs: %List %ParseTree -> %List %ParseForm
postDefArgs argl ==
  null argl => argl
  argl is [[":",a],:b] =>
    b ~= nil => postError
      ['"   Argument",:bright a,'"of indefinite length must be last"]
    a isnt [.,:.] or a is ['QUOTE,:.] => a
    postError
      ['"   Argument",:bright a,'"of indefinite length must be a name"]
  [first argl,:postDefArgs rest argl]

postMDef: %ParseTree -> %ParseForm
postMDef(t) ==
  [.,lhs,rhs] := t
  lhs :=
    lhs is [.,:.] => [internalName x for x in lhs]
    internalName lhs
  [form,targetType]:=
    lhs is [":",:.] => lhs.args
    [lhs,nil]
  newLhs :=
    form is [.,:.] => [(x is [":",a,:.] => a; x) for x in form]
    form
  typeList :=
    form is [.,:.] =>
      [targetType,:[(x is [":",.,t] => t; nil) for x in rest form]]
    nil
  ["MDEF",newLhs,typeList,postTran rhs]

postElt: %ParseTree -> %ParseForm
postElt u ==
  u isnt [.,a,b] => systemErrorHere ["postElt",u]
  a := postTran a
  b is ["%Sequence",:.] => [["elt",a,"makeRecord"],:postTranList rest b]
  ["elt",a,postTran b]


postExit: %ParseTree -> %ParseForm
postExit t == 
  t isnt ["=>",a,b] => systemErrorHere ["postExit",t]
  ["IF",postTran a,["exit",postTran b],"%noBranch"]


postFlatten: (%ParseTree, %Symbol) -> %ParseForm
postFlatten(x,op) ==
  x is [ =op,a,b] => [:postFlatten(a,op),:postFlatten(b,op)]
  [x]

postForm: %ParseTree -> %ParseForm
postForm u ==
  u isnt [op,:argl] => systemErrorHere ["postForm",u]
  x:=
    op isnt [.,:.] => [op,:postTranList argl]
    u := postTranList u
    if u is [["%Comma",:.],:.] then
      postError ['"  ",:bright u,
        '"is illegal because tuples cannot be applied!",'"%l",
          '"   Did you misuse infix dot?"]
    u
  x is [.,["%Comma",:y]] => [first x,:y]
  x

postIf: %ParseTree -> %ParseForm
postIf t ==
  t isnt ["if",:l] => t
  ["IF",:[(null (x:= postTran x) => "%noBranch"; x)
    for x in l]]

postJoin: %ParseTree -> %ParseForm
postJoin ["Join",a,:l] ==
  a:= postTran a
  l:= postTranList l
  if l is [b] and b is [name,:.] and name in '(ATTRIBUTE SIGNATURE) then l
    := [["CATEGORY",b]]
  al:=
    a is ["%Comma",:c] => c
    [a]
  ["Join",:al,:l]

postMapping: %ParseTree -> %ParseForm
postMapping u  ==
  u isnt ["->",source,target] => u
  ["Mapping",postTran target,:unComma postTran source]

postRepeat: %ParseTree -> %ParseForm
postRepeat t == 
  t isnt ["REPEAT",:m,x] => systemErrorHere ["postRepeat",t]
  ["REPEAT",:postIteratorList m,postTran x]

postCollect: %ParseTree -> %ParseForm
postCollect t ==
  t isnt [constructOp,:m,x] => systemErrorHere ["postCollect",t]
  x is [["elt",D,"construct"],:y] =>
    postCollect [["elt",D,"COLLECT"],:m,["construct",:y]]
  itl:= postIteratorList m
  x:= (x is ["construct",r] => r; x)  --added 84/8/31
  y:= postTran x
  finish(constructOp,itl,y) where
    finish(op,itl,y) ==
      y is [":",a] => ["REDUCE","append",0,[op,:itl,a]]
      y is ["%Comma",:l] =>
        newBody:=
          or/[x is [":",y] for x in l] => postMakeCons l
          or/[x is ["SEGMENT",:.] for x in l] => tuple2List l
          ["construct",:postTranList l]
        ["REDUCE","append",0,[op,:itl,newBody]]
      [op,:itl,y]

postIteratorList: %List %ParseTree -> %List %ParseForm
postIteratorList x ==
  x is [p,:l] =>
    (p:= postTran p) is ["IN",y,u] =>
      u is ["|",a,b] => [["IN",y,postInSeq a],["|",b],:postIteratorList l]
      [["IN",y,postInSeq u],:postIteratorList l]
    [p,:postIteratorList l]
  x

postin: %ParseTree -> %ParseForm
postin arg ==
  arg isnt ["in",i,seq] => systemErrorHere ["postin",arg]
  ["in",postTran i, postInSeq seq]

postIn: %ParseTree -> %ParseForm
postIn arg ==
  arg isnt ["IN",i,seq] => systemErrorHere ["postIn",arg]
  ["IN",postTran i,postInSeq seq]

postInSeq: %ParseTree -> %ParseForm
postInSeq seq ==
  seq is ["SEGMENT",p,q] => postTranSegment(p,q)
  seq is ["%Comma",:l] => tuple2List l
  postTran seq

postTranSegment: (%ParseTree, %ParseTree) -> %ParseForm
postTranSegment(p,q) == 
  ["SEGMENT",postTran p,(q => postTran q; nil)]

tuple2List: %ParseTree -> %ParseForm
tuple2List l ==
  l is [a,:l'] =>
    u:= tuple2List l'
    a is ["SEGMENT",p,q] =>
      null u => ["construct",postTranSegment(p,q)]
      ["nconc",["construct",postTranSegment(p,q)],tuple2List l']
    null u => ["construct",postTran a]
    ["cons",postTran a,tuple2List l']
  nil

SEGMENT: %ParseTree -> %ParseForm
SEGMENT(a,b) == 
  [i for i in a..b]

postReduce: %ParseTree -> %ParseForm
postReduce t ==
  t isnt ["%Reduce",op,expr] => systemErrorHere ["postReduce",t]
  expr is ["COLLECT",:.] =>
    ["REDUCE",op,0,postTran expr]
  postReduce ["%Reduce",op,["COLLECT",["IN",g:= gensym(),expr],
    ["construct",  g]]]

postFlattenLeft: (%ParseTree, %Symbol) -> %ParseForm
postFlattenLeft(x,op) ==--
  x is [ =op,a,b] => [:postFlattenLeft(a,op),b]
  [x]

postSemiColon: %ParseTree -> %ParseForm
postSemiColon u == 
  postBlock ["%Block",:postFlattenLeft(u,";")]

postSequence: %ParseTree -> %ParseForm
postSequence t == 
  t isnt ["%Sequence",:l] => systemErrorHere ["postSequence",t]
  ['(elt $ makeRecord),:postTranList l]

postSignature: %ParseTree -> %ParseForm
postSignature t ==
  t isnt ["%Signature",op,sig] => systemErrorHere ["postSignature",t]
  op :=
     integer? op => internalName op
     postAtom
       string? op =>
         stackWarning('"String syntax for %1b in signature is deprecated.",[op])
         makeSymbol op
       op
  sig is ["->",:.] =>
    sig1 := postType sig
    ["SIGNATURE",op,:removeSuperfluousMapping killColons sig1]
  ["SIGNATURE",op,:postType ["->","constant",sig]]

killColons: %ParseTree -> %ParseForm
killColons x ==
  x isnt [.,:.] => x
  x is [op,:.] and op in '(Record Union %Forall %Exist) => x
  x is [":",.,y] => killColons y
  [killColons first x,:killColons rest x]

postSlash: %ParseTree -> %ParseForm
postSlash t ==
  t isnt ['_/,a,b] => systemErrorHere ["postSlash",t]
  string? a => postTran ["%Reduce",makeSymbol a,b]
  ['_/,postTran a,postTran b]

removeSuperfluousMapping: %ParseTree -> %ParseForm
removeSuperfluousMapping sig1 ==
  --get rid of this asap
  sig1 is [x,:y] and x is ["Mapping",:.] => [rest x,:y]
  sig1

postType: %ParseTree -> %ParseForm
postType typ ==
  typ is ["->",source,target] =>
    source="constant" => [[postTran target],"constant"]
    [["Mapping",postTran target,:unComma postTran source]]
  typ is ["->",target] => [["Mapping",postTran target]]
  [postTran typ]

post%Comma: %ParseTree -> %ParseForm
post%Comma u ==
  u is ["%Comma"] => u
  u is ["%Comma",:l,a] => (["%Comma",:postTranList rest u])
--u is ["%Comma",:l,a] => (--a:= postTran a; ["%Comma",:postTranList rest u])
    --RDJ: don't understand need for above statement that is commented out

postWhere: %ParseTree -> %ParseForm
postWhere t ==
  t isnt ["where",a,b] => systemErrorHere ["postWhere",t]
  x:=
    b is ["%Block",:c] => c
    [b]
  ["where",postTran a,:postTranList x]

postWith: %ParseTree -> %ParseForm
postWith t ==
  t isnt ["with",a] => systemErrorHere ["postWidth",t]
  $insidePostCategoryIfTrue: local := true
  a:= postTran a
  a is [op,:.] and op in '(SIGNATURE ATTRIBUTE IF) => ["CATEGORY",a]
  a is ["PROGN",:b] => ["CATEGORY",:b]
  a

postTransformCheck: %ParseTree -> %ParseForm
postTransformCheck x ==
  $defOp: local:= nil
  postcheck x

postcheck: %ParseTree -> %ParseForm
postcheck x ==
  x isnt [.,:.] => nil
  x is ["DEF",form,[target,:.],:.] =>
    setDefOp form
    postcheck rest rest x
  x is ['QUOTE,:.] => nil
  postcheck first x
  postcheck rest x

setDefOp: %ParseForm -> %Thing
setDefOp f ==
  if f is [":",g,:.] then f := g
  f := (f isnt [.,:.] => f; first f)
  if $topOp then $defOp:= f else $topOp:= f

unComma: %ParseForm -> %ParseForm
unComma x ==
  x is ["%Comma",:y] => y
  [x]

--% %Match

postAlternatives alts ==
    alts is ["%Block",:cases] => ["%Block",:[tranAlt c for c in cases]]
    tranAlt alts
  where
    tranAlt c ==
      c is ["=>",pred,conseq] => 
        ["=>",postTran pred,postTran conseq]
      postTran c

postMatch: %ParseTree -> %ParseForm
postMatch t ==
  t isnt ["%Match",expr,alts] => systemErrorHere ["postMatch",t]
  alts :=
    alts is [";",:.] => ["%Block",:postFlattenLeft(alts,";")]
    alts
  ["%Match",postTran expr, postAlternatives alts]

--% Register special parse tree tranformers.

for x in [["with", :"postWith"],_
	  ["/", :"postSlash"],_
	  ["construct", :"postConstruct"],_
	  ["%Block", :"postBlock"],_
	  ["COLLECT", :"postCollect"],_
	  [":BF:", :"postBigFloat"],_
	  ["in", :"postin"],_
	  ["IN", :"postIn"],_
	  ["REPEAT", :"postRepeat"],_
	  ["add", :"postAdd"],_
	  ["%Reduce", :"postReduce"],_
	  [",", :"postComma"],_
	  [";", :"postSemiColon"],_
	  ["where", :"postWhere"],_
	  [":", :"postColon"],_
	  ["@", :"postAtSign"],_
	  ["pretend", :"postPretend"],_
	  ["if", :"postIf"],_
	  ["Join", :"postJoin"],_
	  ["%Signature", :"postSignature"],_
	  ["CATEGORY", :"postCategory"],_
	  ["==", :"postDef"],_
	  ["==>", :"postMDef"],_
	  ["->", :"postMapping"],_
	  ["=>", :"postExit"],_
          ["%Match",:"postMatch"],_
	  ["%Comma", :"post%Comma"]] repeat
  property(first x, 'postTran) := rest x

