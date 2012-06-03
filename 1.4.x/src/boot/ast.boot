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
--

--
-- Abstract:
--   This file defines the AST data structure and helper functions
--   for representing Boot programs.
--

import includer
namespace BOOTTRAN
module ast (quote)

++ True means that Boot functions should be translated to use
++ hash tables to remember values.  By default, functions are
++ translated with the obvious semantics, e.g. no caching.
$bfClamming := false

++ List of identifiers defined as constants in the current
++ translation unit.
$constantIdentifiers := nil

++ When non-nil holds the scope nominated  in the most recent
++ namespace definition.
$activeNamespace := nil

structure %Ast ==
  %Command(%String)                     -- includer command
  %Lisp(%String)                        -- )lisp command
  %Module(%Symbol,%List,%List)          -- module declaration
  %Namespace(%Symbol)                   -- namespace AxiomCore
  %Import(%Ast)                         -- import module; import namespace foo
  %ImportSignature(%Symbol,%Signature)  -- import function declaration
  %Record(%List,%List)                  -- Record(num: %Short, den: %Short)
  %AccessorDef(%Symbol,%Ast)            -- numerator == (.num)
  %TypeAlias(%Head, %List)              -- type alias definition
  %Signature(%Symbol,%Mapping)          -- op: S -> T
  %Mapping(%Ast, %List)                 -- (S1, S2) -> T
  %Forall(%List,%Ast)                   -- forall a . a -> a
  %Dynamic %Ast                         -- x: local
  %SuffixDot(%Ast)                      -- x . 
  %Quote(%Ast)                          -- 'x
  %EqualPattern(%Ast)                   -- =x        -- patterns
  %Colon(%Symbol)                       -- :x
  %QualifiedName(%Symbol,%Symbol)       -- m::x
  %Restrict(%Ast,%Ast)                  -- x@t
  %DefaultValue(%Symbol,%Ast)           -- opt. value for function param.
  %Key(%Symbol,%Ast)                    -- k <- x
  %Bracket(%Ast)                        -- [x, y]
  %UnboundedSegment(%Ast)               -- 3..
  %BoundedSgement(%Ast,%Ast)            -- 2..4
  %Tuple(%List)                         -- a, b, c, d
  %ColonAppend(%Ast,%Ast)               -- [:y] or [x, :y]
  %Is(%Ast,%Ast)                        -- e is p    -- patterns
  %Isnt(%Ast,%Ast)                      -- e isnt p  -- patterns
  %Reduce(%Ast,%Ast)                    -- +/[...]
  %PrefixExpr(%Symbol,%Ast)             -- #v
  %Call(%Ast,%Sequence)                 -- f(x, y , z)
  %InfixExpr(%Symbol,%Ast,%Ast)         -- x + y
  %ConstantDefinition(%Symbol,%Ast)     -- x == y
  %Definition(%Symbol,%Ast,%Ast)        -- f x == y
  %Macro(%Symbol,%List,%Ast)            -- macro m x == y
  %Lambda(%List,%Ast)                   -- x +-> x**2
  %SuchThat(%Ast)                       -- | p
  %Assignment(%Ast,%Ast)                -- x := y
  %While(%Ast)                          -- while p           -- iterator
  %Until(%Ast)                          -- until p           -- iterator
  %For(%Ast,%Ast,%Ast)                  -- for x in e by k   -- iterator
  %Implies(%Ast,%Ast)                   -- p => x
  %Iterators(%List)                     -- list of iterators
  %Cross(%List)                         -- iterator cross product
  %Repeat(%Sequence,%Ast)               -- while p repeat s
  %Pile(%Sequence)                      -- pile of expression sequence
  %Append(%Sequence)                    -- concatenate lists
  %Case(%Ast,%Sequence)                 -- case x of ...
  %Return(%Ast)                         -- return x
  %Leave(%Ast)                          -- leave x
  %Throw(%Ast)                          -- throw OutOfRange 3
  %Catch(%Signature,%Ast)               -- catch(x: OutOfRange) => print x
  %Finally(%Ast)                        -- finally closeStream f
  %Try(%Ast,%Sequence)                  -- try x / y catch DivisionByZero
  %Where(%Ast,%Sequence)                -- e where f x == y
  %Structure(%Ast,%Sequence)            -- structure Foo == ...

--%
--% Data type for translation units data
--%
structure %LoadUnit ==
  Record(fdefs: %List %Thing,sigs: %List %Thing,xports: %List %Identifier,_
    csts: %List %Binding,varno: %Short,letno: %Short,isno: %Short,_
    sconds: %List %Thing,op: %Identifier) with
      functionDefinitions == (.fdefs)  -- functions defined in this TU
      globalSignatures == (.sigs)      -- signatures proclaimed by this TU
      exportedNames == (.xports)       -- names exported by this TU
      constantBindings == (.csts)      -- constants defined in this TU
      currentGensymNumber == (.varno)  -- current gensym sequence number
      letVariableNumer == (.letno)     -- let variable sequence number
      isVariableNumber == (.isno)      -- is variable sequence number
      sideConditions == (.sconds)      -- list of side declarations
      enclosingFunction  == (.op)      -- name of current enclosing function

makeLoadUnit() ==
  mk%LoadUnit(nil,nil,nil,nil,0,0,0,nil,nil)

pushFunctionDefinition(tu,def) ==
  functionDefinitions(tu) := [def,:functionDefinitions tu]

--%

-- TRUE if we are currently building the syntax tree for an 'is' 
-- expression.
$inDefIS := false


++ returns a `quote' ast for x.
quote x ==
  ['QUOTE,x]

bfSpecificErrorHere msg ==
  throw msg : BootSpecificError

--%

bfGenSymbol: %LoadUnit -> %Symbol 
bfGenSymbol tu ==
  currentGensymNumber(tu) := currentGensymNumber tu + 1
  makeSymbol strconc('"bfVar#",toString currentGensymNumber tu)

bfLetVar: %LoadUnit -> %Symbol
bfLetVar tu ==
  letVariableNumer(tu) := letVariableNumer tu + 1
  makeSymbol strconc('"LETTMP#",toString letVariableNumer tu)

bfIsVar: %LoadUnit -> %Symbol
bfIsVar tu ==
  isVariableNumber(tu) := isVariableNumber tu + 1
  makeSymbol strconc('"ISTMP#",toString isVariableNumber tu)

bfColon: %Thing -> %Form
bfColon x== 
  ["COLON",x]

bfColonColon: (%Symbol,%Symbol) -> %Symbol
bfColonColon(package, name) == 
  %hasFeature KEYWORD::CLISP and package in '(EXT FFI) =>
    symbolBinding(symbolName name,package)
  makeSymbol(symbolName name, package)

bfSymbol: %Thing -> %Thing 
bfSymbol x==
  string? x=> x
  quote x

bfFunction x ==
  ["FUNCTION",x]
 
bfDot: () -> %Symbol
bfDot() == 
  "DOT"
 
bfSuffixDot: %Form -> %Form
bfSuffixDot x ==
  [x,"DOT"]

bfEqual: %Form -> %Form
bfEqual(name) == 
  ["EQUAL",name]

bfBracket: %Thing -> %Thing 
bfBracket(part) == 
  part
 
bfPile: %List %Form -> %List %Form
bfPile(part) == 
  part
 
bfDo x ==
  x

bfAtScope(s,x) ==
  ["LET",[["*PACKAGE*",s]],x]

bfAppend: %List %List %Form -> %List %Form
bfAppend ls ==
  ls isnt [l,:ls] => nil
  r := copyList l
  p := r
  repeat
    ls isnt [l,:ls] => return r
    l = nil => nil
    lastNode(p).rest := copyList l
    p := rest p
 
bfColonAppend: (%List %Form,%Form) -> %Form
bfColonAppend(x,y) ==
  x = nil => 
    y is ["BVQUOTE",:a] => ["&REST",['QUOTE,:a]]
    ["&REST",y]
  [first x,:bfColonAppend(rest x,y)]

bfBeginsDollar: %Thing -> %Boolean 
bfBeginsDollar x ==  
  stringChar(symbolName x,0) = char "$"
 
compFluid id == 
  ["%Dynamic",id]
 
compFluidize x==
  x = nil => nil
  symbol? x and bfBeginsDollar x => compFluid x
  atomic? x => x
  [compFluidize(first x),:compFluidize(rest x)]
 
bfPlace x ==
  ["%Place",:x]

bfTuple x == 
  ["TUPLE",:x]
 
bfTupleP x ==
  x is ["TUPLE",:.]

++ If `bf' is a tuple return its elements; otherwise `bf'.
bfUntuple bf ==
  bfTupleP bf => rest bf
  bf
 
bfTupleIf x==
  bfTupleP x => x
  bfTuple x
 
bfTupleConstruct b ==
  a :=
    bfTupleP b => rest b
    [b]
  or/[x is ["COLON",.] for x in a] => bfMakeCons a
  ["LIST",:a]
 
bfConstruct b ==
  a :=
    bfTupleP b => rest b
    [b]
  bfMakeCons a
 
bfMakeCons l ==
  l = nil => nil
  l is [["COLON",a],:l1] =>
    l1 => ['append,a,bfMakeCons l1]
    a
  ['CONS,first l,bfMakeCons rest l]
 
bfFor(tu,lhs,u,step) ==
  u is ["tails",:.] => bfForTree(tu,'ON,lhs,second u)
  u is ["SEGMENT",:.] => bfSTEP(tu,lhs,second u,step,third u)
  u is ['entries,:.] => bfIterateTable(tu,lhs,second u)
  bfForTree(tu,'IN,lhs,u)
 
bfForTree(tu,OP,lhs,whole)==
  whole :=
    bfTupleP whole => bfMakeCons rest whole
    whole
  lhs isnt [.,:.] => bfINON(tu,[OP,lhs,whole])
  lhs :=
    bfTupleP lhs => second lhs
    lhs
  lhs is ["L%T",:.] =>
    G := second lhs
    [:bfINON(tu,[OP,G,whole]),:bfSuchthat(tu,bfIS(tu,G,third lhs))]
  G := bfGenSymbol tu
  [:bfINON(tu,[OP,G,whole]),:bfSuchthat(tu,bfIS(tu,G,lhs))]
 
 
bfSTEP(tu,id,fst,step,lst)==
  if id is "DOT" then
    id := bfGenSymbol tu
  initvar := [id]
  initval := [fst]
  inc :=
    step isnt [.,:.] => step
    g1 := bfGenSymbol tu
    initvar := [g1,:initvar]
    initval := [step,:initval]
    g1
  final :=
    lst isnt [.,:.] => lst
    g2 := bfGenSymbol tu
    initvar := [g2,:initvar]
    initval := [lst,:initval]
    g2
  ex :=
     lst = nil => []
     integer? inc =>
       pred :=
	 inc < 0 => "<"
	 ">"
       [[pred,id,final]]
     [['COND,[['MINUSP,inc],
	   ["<",id,final]],['T,[">",id,final]]]]
  suc := [['SETQ,id,["+",id,inc]]]
  [[initvar,initval,suc,[],ex,[]]]
 
++ Build a hashtable-iterator form.
bfIterateTable(tu,e,t) ==
  ['%tbliter,e,t,gensym()]
 
bfINON(tu,x) ==
  [op,id,whole] := x
  op is "ON" => bfON(tu,id,whole)
  bfIN(tu,id,whole)
 
bfIN(tu,x,E)==
  g := bfGenSymbol tu
  vars := [g]
  inits := [E]
  exitCond := ['NOT,['CONSP,g]]
  if x isnt "DOT" then
    vars := [:vars,x]
    inits := [:inits,nil]
    exitCond := ['OR,exitCond,['PROGN,['SETQ,x,['CAR,g]] ,'NIL]]
  [[vars,inits,[['SETQ,g,['CDR, g]]],[],[exitCond],[]]]
 
bfON(tu,x,E)==
  if x is "DOT" then
    x := bfGenSymbol tu
  -- allow a list variable to iterate over its own tails.
  var := init := nil
  if not symbol? E or not symbolEq?(x,E) then
    var := [x]
    init := [E]
  [[var,init,[['SETQ,x,['CDR, x]]],[],[['NOT,['CONSP,x]]],[]]]
 
bfSuchthat(tu,p) ==
  [[[],[],[],[p],[],[]]]
 
bfWhile(tu,p) ==
  [[[],[],[],[],[bfNOT p],[]]]
 
bfUntil(tu,p) ==
  g := bfGenSymbol tu
  [[[g],[nil],[['SETQ,g,p]],[],[g],[]]]
 
bfIterators x ==
  ["ITERATORS",:x]
 
bfCross x ==
  ["CROSS",:x]
 
bfLp(tu,iters,body)==
  iters is ["ITERATORS",:.] => bfLp1(tu,rest iters,body)
  bfLpCross(tu,rest iters,body)
 
bfLpCross(tu,iters,body)==
  rest iters = nil => bfLp(tu,first iters,body)
  bfLp(tu,first iters,bfLpCross(tu,rest iters,body))
 
bfSep(iters)==
  iters = nil => [[],[],[],[],[],[]]
  f := first iters
  r := bfSep rest iters
  [[:i,:j] for i in f for j in r]
 
bfReduce(tu,op,y)==
  a :=
    op is ['QUOTE,:.] => second op
    op
  op := bfReName a
  init := a has SHOETHETA or op has SHOETHETA
  g := bfGenSymbol tu
  g1 := bfGenSymbol tu
  body := ['SETQ,g,[op,g,g1]]
  init = nil =>
    g2 := bfGenSymbol tu
    init := ['CAR,g2]
    ny := ['CDR,g2]
    it := ["ITERATORS",:[[[[g],[init],[],[],[],[g]]],bfIN(tu,g1,ny)]]
    bfMKPROGN [['L%T,g2,y],bfLp(tu,it,body)]
  init := first init
  it := ["ITERATORS",:[[[[g],[init],[],[],[],[g]]],bfIN(tu,g1,y)]]
  bfLp(tu,it,body)
 
bfReduceCollect(tu,op,y)==
  y is ["COLLECT",:.] => 
    body := second y
    itl := third y
    a :=
      op is ['QUOTE,:.] => second op
      op
    a is "append!" => bfDoCollect(tu,body,itl,'lastNode,'skipNil)
    a is "append" => bfDoCollect(tu,['copyList,body],itl,'lastNode,'skipNil)
    op := bfReName a
    init := a has SHOETHETA or op has SHOETHETA
    bfOpReduce(tu,op,init,body,itl)
  seq :=
    y = nil => bfTuple nil
    second y
  bfReduce(tu,op,bfTupleConstruct seq)
 
-- delayed collect
 
bfDCollect(y,itl) == 
  ["COLLECT",y,itl]
 
bfDTuple x == 
  ["DTUPLE",x]
 
bfCollect(tu,y,itl) ==
  y is ["COLON",a] =>
    a is ['CONS,:.] or a is ['LIST,:.] =>
      bfDoCollect(tu,a,itl,'lastNode,'skipNil)
    bfDoCollect(tu,['copyList,a],itl,'lastNode,'skipNil)
  y is ["TUPLE",:.] =>
    bfDoCollect(tu,bfConstruct y,itl,'lastNode,'skipNil)
  bfDoCollect(tu,['CONS,y,'NIL],itl,'CDR,nil)
 
bfMakeCollectInsn(expr,prev,head,adv) ==
  firstTime := bfMKPROGN
    [['SETQ,head,expr],['SETQ,prev,(adv is 'CDR => head; [adv,head])]]
  otherTime := bfMKPROGN [['RPLACD,prev,expr],['SETQ,prev,[adv,prev]]]
  bfIf(['NULL,head],firstTime,otherTime)

bfDoCollect(tu,expr,itl,adv,k) ==
  head := bfGenSymbol tu            -- pointer to the result
  prev := bfGenSymbol tu            -- pointer to the previous cell
  body :=
    k is 'skipNil =>
      x := bfGenSymbol tu
      ['LET,[[x,expr]],
         bfIf(['NULL,x],'NIL,bfMakeCollectInsn(x,prev,head,adv))]
    bfMakeCollectInsn(expr,prev,head,adv)
  extrait := [[[head,prev],['NIL,'NIL],nil,nil,nil,[head]]]
  bfLp2(tu,extrait,itl,body)

++ Given the list of loop iterators, return 2-list where the first
++ component is the list of all non-table iterators and the second
++ is the list of all-table iterators,
separateIterators iters ==
  x := nil
  y := nil
  for iter in iters repeat
    iter is ['%tbliter,:.] => y := [rest iter,:y]
    x := [iter,:x]
  [reverse! x,reverse! y]

bfTableIteratorBindingForm(tu,keyval,end?,succ) ==
  -- FIXME: most of the repetitions below could be avoided
  -- FIXME: with better bfIS1 implementation.
  keyval is ['CONS,key,val] =>
    if key is 'DOT then key := gensym()
    if val is 'DOT then val := gensym()
    ident? key and ident? val =>
      ['MULTIPLE_-VALUE_-BIND,[end?,key,val],[succ]]
    ident? key =>
      v := gensym()
      ['MULTIPLE_-VALUE_-BIND,[end?,key,v],[succ],bfLET(tu,val,v)]
    k := gensym()
    ident? val =>
      ['MULTIPLE_-VALUE_-BIND,[end?,k,val],[succ],bfLET(tu,key,k)]
    v := gensym()
    ['MULTIPLE_-VALUE_-BIND,[end?,k,v],[succ],bfLET(tu,key,k),bfLET(tu,val,v)]
  k := gensym()
  v := gensym()
  ['MULTIPLE_-VALUE_-BIND,[end?,k,v],[succ],bfLET(tu,keyval,['CONS,k,v])]

++ Expand the list of table iterators into a tuple form with
++   (a) list of table iteration initialization
++   (b) for each iteration, local bindings of key value
++   (c) a list of exit conditions
bfExpandTableIters(tu,iters) ==
  inits := nil
  localBindings := nil
  exits := nil
  for [e,t,g] in iters repeat
    inits := [[g,t],:inits]
    x := gensym()   -- exit guard
    exits := [['NOT,x],:exits]
    localBindings := [bfTableIteratorBindingForm(tu,e,x,g),:localBindings]
  [inits,localBindings,exits] -- NOTE: things are returned in reverse order.

bfLp1(tu,iters,body)==
  [iters,tbls] := separateIterators iters
  [vars,inits,sucs,filters,exits,value] := bfSep bfAppend iters
  [tblInits,tblLocs,tblExits] := bfExpandTableIters(tu,tbls)
  nbody :=
    filters = nil => body
    bfAND [:filters,body]
  value :=
    value = nil => "NIL"
    first value
  exits :=
    exits = nil and tblExits = nil => nbody
    bfIf(bfOR [:exits,:tblExits],["RETURN",value],nbody)
  for locBinding in tblLocs repeat
    exits := [:locBinding,exits]
  loop := ["LOOP",exits,:sucs]
  if vars then loop := 
    ["LET",[[v, i] for v in vars for i in inits],loop]
  for x in tblInits repeat
    loop := ['WITH_-HASH_-TABLE_-ITERATOR,x,loop]
  loop
 
bfLp2(tu,extrait,itl,body)==
  itl is ["ITERATORS",:.] => bfLp1(tu,[extrait,:rest itl],body)
  iters := rest itl
  bfLpCross(tu,[["ITERATORS",extrait,:CDAR iters],:rest iters],body)
 
bfOpReduce(tu,op,init,y,itl)==
  g := bfGenSymbol tu
  body:=
    op is "AND" =>
      bfMKPROGN [["SETQ",g,y], ['COND, [['NOT,g],['RETURN,'NIL]]]]
    op is "OR" => bfMKPROGN [["SETQ",g,y], ['COND, [g,['RETURN,g]]]]
    ['SETQ,g,[op,g,y]]
  init = nil =>
    g1 := bfGenSymbol tu
    init := ['CAR,g1]
    y := ['CDR,g1]          -- ??? bogus self-assignment/initialization
    extrait := [[[g],[init],[],[],[],[g]]]
    bfMKPROGN [['L%T,g1,y],bfLp2(tu,extrait,itl,body)]
  init := first init
  extrait := [[[g],[init],[],[],[],[g]]]
  bfLp2(tu,extrait,itl,body)
 
bfLoop1(tu,body) == 
  bfLp(tu,bfIterators nil,body)
 
bfSegment1(lo) ==
  ["SEGMENT",lo,nil]
 
bfSegment2(lo,hi) ==
  ["SEGMENT",lo,hi]
 
bfForInBy(tu,variable,collection,step)==
  bfFor(tu,variable,collection,step)
 
bfForin(tu,lhs,U)==
  bfFor(tu,lhs,U,1)
 
bfSignature(a,b)==
  b is "local" =>  compFluid a
  ['%Signature,a,b]
 
bfTake(n,x)==
  x = nil => x
  n=0 => nil
  [first x,:bfTake(n-1,rest x)]
 
bfDrop(n,x)==
  x = nil or n = 0 => x
  bfDrop(n-1,rest x)
 
bfReturnNoName a ==
  ["RETURN",a]

bfLeave x ==
  ["%Leave",x]
 
bfSUBLIS(p,e)==
  e isnt [.,:.] => bfSUBLIS1(p,e)
  e.op is 'QUOTE => e
  [bfSUBLIS(p,first e),:bfSUBLIS(p,rest e)]
 
+++ Returns e/p, where e is an atom.  We assume that the
+++ DEFs form a system admitting a fix point; otherwise we may
+++ loop forever.  That can happen only if nullary goats
+++ are recursive -- which they are not supposed to be.
+++ We don't enforce that restriction though.
bfSUBLIS1(p,e)==
   p = nil => e
   f := first p
   sameObject?(first f,e) => bfSUBLIS(p, rest f)
   bfSUBLIS1(rest p,e)
 
defSheepAndGoats(tu,x)==
  case x of 
    %Definition(op,args,body) =>
      argl :=
        bfTupleP args => rest args
	[args]
      argl = nil =>
	opassoc := [[op,:body]]
	[opassoc,[],[]]
      op1 := makeSymbol strconc(symbolName enclosingFunction tu,'",",symbolName op)
      opassoc := [[op,:op1]]
      defstack := [[op1,args,body]]
      [opassoc,defstack,[]]
    %Pile defs => defSheepAndGoatsList(tu,defs)
    otherwise => [[],[],[x]]
 
defSheepAndGoatsList(tu,x)==
  x = nil => [[],[],[]]
  [opassoc,defs,nondefs]    := defSheepAndGoats(tu,first x)
  [opassoc1,defs1,nondefs1] := defSheepAndGoatsList(tu,rest x)
  [[:opassoc,:opassoc1],[:defs,:defs1],[:nondefs,:nondefs1]]

--% LET
 
bfLetForm(lhs,rhs) ==   
  ['L%T,lhs,rhs]
 
bfLET1(tu,lhs,rhs) ==
  symbol? lhs        => bfLetForm(lhs,rhs)
  lhs is ['%Dynamic,.] or lhs is ['%Signature,:.] => bfLetForm(lhs,rhs)
  symbol? rhs and not bfCONTAINED(rhs,lhs) =>
    rhs1 := bfLET2(tu,lhs,rhs)
    rhs1 is ["L%T",:.]   => bfMKPROGN [rhs1,rhs]
    rhs1 is ["PROGN",:.] => [:rhs1,:[rhs]]
    if symbol? first rhs1 then rhs1 := [rhs1,:nil]
    bfMKPROGN [:rhs1,rhs]
  rhs is ["L%T",:.] and symbol?(name := second rhs) =>
    -- handle things like [a] := x := foo
    l1 := bfLET1(tu,name,third rhs)
    l2 := bfLET1(tu,lhs,name)
    l2 is ["PROGN",:.] => bfMKPROGN [l1,:rest l2]
    if symbol? first l2 then l2 := [l2,:nil]
    bfMKPROGN [l1,:l2,name]
  g := bfLetVar tu
  rhs1 := ['L%T,g,rhs]
  let1 := bfLET1(tu,lhs,g)
  let1 is ["PROGN",:.] => bfMKPROGN [rhs1,:rest let1]
  if symbol? first let1 then let1 := [let1,:nil]
  bfMKPROGN [rhs1,:let1,g]
 
bfCONTAINED(x,y)==
  sameObject?(x,y) => true
  y isnt [.,:.] => false
  bfCONTAINED(x,first y) or bfCONTAINED(x,rest y)
 
bfLET2(tu,lhs,rhs) ==
  lhs = nil => nil
  symbol? lhs => bfLetForm(lhs,rhs)
  lhs is ['%Dynamic,.] => bfLetForm(lhs,rhs)
  lhs is ['L%T,a,b] =>
    a := bfLET2(tu,a,rhs)
    (b := bfLET2(tu,b,rhs)) = nil => a
    b isnt [.,:.] => [a,b]
    cons? first b => [a,:b]
    [a,b]
  lhs is ['CONS,var1,var2] =>
    var1 is "DOT" or var1 is ['QUOTE,:.] =>
      bfLET2(tu,var2,addCARorCDR('CDR,rhs))
    l1 := bfLET2(tu,var1,addCARorCDR('CAR,rhs))
    var2 = nil or var2 is "DOT" =>l1
    if cons? l1 and first l1 isnt [.,:.] then
      l1 := [l1,:nil]
    symbol? var2 =>
      [:l1,bfLetForm(var2,addCARorCDR('CDR,rhs))]
    l2 := bfLET2(tu,var2,addCARorCDR('CDR,rhs))
    if cons? l2 and first l2 isnt [.,:.] then
      l2 := [l2,:nil]
    [:l1,:l2]
  lhs is ['append,var1,var2] =>
    patrev := bfISReverse(var2,var1)
    rev := ['reverse,rhs]
    g := bfLetVar tu
    l2 := bfLET2(tu,patrev,g)
    if cons? l2 and first l2 isnt [.,:.] then
      l2 := [l2,:nil]
    var1 is "DOT" => [['L%T,g,rev],:l2]
    first lastNode l2 is ['L%T, =var1, val1] =>
      [['L%T,g,rev],:reverse rest reverse l2,
       bfLetForm(var1,['reverse!,val1])]
    [['L%T,g,rev],:l2,bfLetForm(var1,['reverse!,var1])]
  lhs is ["EQUAL",var1] => ['COND,[bfQ(var1,rhs),var1]]
  -- The original expression may be one that involves literals as 
  -- sub-patterns, e.g.
  --      ['SEQ, :l, ['exit, 1, x]] := item
  -- We continue the processing as if that expression had been written
  --      item is ['SEQ, :l, ['exit, 1, x]]
  -- and generate appropriate codes.
  --                  -- gdr/2007-04-02.
  isPred :=
    $inDefIS => bfIS1(tu,rhs,lhs)
    bfIS(tu,rhs,lhs)
  ['COND,[isPred,rhs]]
 
 
bfLET(tu,lhs,rhs) ==
  letno := letVariableNumer tu
  try
    letVariableNumer(tu) := 0
    bfLET1(tu,lhs,rhs)
  finally letVariableNumer(tu) := letno
 
addCARorCDR(acc,expr) ==
  expr isnt [.,:.] => [acc,expr]
  acc is 'CAR and expr is ["reverse",:.] =>
      ["CAR",["lastNode",:rest expr]]
  funs := '(CAR CDR CAAR CDAR CADR CDDR CAAAR CADAR CAADR CADDR
            CDAAR CDDAR CDADR CDDDR)
  p := bfPosition(first expr,funs)
  p = -1 => [acc,expr]
  funsA := '(CAAR CADR CAAAR CADAR CAADR CADDR CAAAAR CAADAR CAAADR
             CAADDR CADAAR CADDAR CADADR CADDDR)
  funsR := '(CDAR CDDR CDAAR CDDAR CDADR CDDDR CDAAAR CDADAR CDAADR
             CDADDR CDDAAR CDDDAR CDDADR CDDDDR)
  acc is 'CAR => [funsA.p,:rest expr]
  [funsR.p,:rest expr]
 
bfPosition(x,l) ==  bfPosn(x,l,0)
bfPosn(x,l,n) ==
  l = nil => -1
  x = first l => n
  bfPosn(x,rest l,n+1)
 
--% IS
 
bfISApplication(tu,op,left,right)==
  op is "IS"      => bfIS(tu,left,right)
  op is "ISNT"    => bfNOT bfIS(tu,left,right)
  [op ,left,right]
 
bfIS(tu,left,right)==
  isno := isVariableNumber tu
  try
    isVariableNumber(tu) := 0
    $inDefIS: local :=true
    bfIS1(tu,left,right)
  finally isVariableNumber(tu) := isno
 
bfISReverse(x,a) ==
  x is ['CONS,:.] =>
    third x = nil => ['CONS,second x, a]
    y := bfISReverse(third x, nil)
    y.rest.rest.first := ['CONS,second x,a]
    y
  bfSpecificErrorHere '"Error in bfISReverse"
 
bfIS1(tu,lhs,rhs) ==
  rhs = nil => ['NULL,lhs]
  rhs = true => ['EQ,lhs,rhs]
  bfString? rhs => bfAND [['STRINGP,lhs],["STRING=",lhs,rhs]]
  bfChar? rhs or integer? rhs => ['EQL,lhs,rhs]
  rhs isnt [.,:.] => ['PROGN,bfLetForm(rhs,lhs),'T]
  rhs.op is 'QUOTE =>
    [.,a] := rhs
    symbol? a => ['EQ,lhs,rhs]
    string? a => bfAND [['STRINGP,lhs],["STRING=",lhs,a]]
    ["EQUAL",lhs,rhs]
  rhs.op is 'L%T =>
    [.,c,d] := rhs
    l := bfLET(tu,c,lhs)
    bfAND [bfIS1(tu,lhs,d),bfMKPROGN [l,'T]]
  rhs is ["EQUAL",a] => bfQ(lhs,a)
  rhs is ['CONS,a,b] and a is "DOT" and b is "DOT" => ['CONSP,lhs]
  cons? lhs =>
    g := bfIsVar tu
    bfMKPROGN [['L%T,g,lhs],bfIS1(tu,g,rhs)]
  rhs.op is 'CONS =>
    [.,a,b] := rhs
    a is "DOT" =>
      b = nil => bfAND [['CONSP,lhs],['NULL,['CDR,lhs]]]
      b is "DOT" => ['CONSP,lhs]
      bfAND [['CONSP,lhs],bfIS1(tu,['CDR,lhs],b)]
    b = nil =>
      bfAND [['CONSP,lhs],['NULL,['CDR,lhs]],bfIS1(tu,['CAR,lhs],a)]
    b is "DOT" => bfAND [['CONSP,lhs],bfIS1(tu,['CAR,lhs],a)]
    a1 := bfIS1(tu,['CAR,lhs],a)
    b1 := bfIS1(tu,['CDR,lhs],b)
    a1 is ['PROGN,c,'T] and b1 is ['PROGN,:cls] =>
      bfAND [['CONSP,lhs],bfMKPROGN [c,:cls]]
    bfAND [['CONSP,lhs],a1,b1]
  rhs.op is 'append =>
    [.,a,b] := rhs
    patrev := bfISReverse(b,a)
    g := bfIsVar tu
    rev := bfAND [['CONSP,lhs],['PROGN,['L%T,g,['reverse,lhs]],'T]]
    l2 := bfIS1(tu,g,patrev)
    if cons? l2 and first l2 isnt [.,:.] then
      l2 := [l2,:nil]
    a is "DOT" => bfAND [rev,:l2]
    bfAND [rev,:l2,['PROGN,bfLetForm(a,['reverse!,a]),'T]]
  bfSpecificErrorHere '"bad IS code is generated"


bfHas(expr,prop) ==
  symbol? prop => ["GET",expr, quote prop]
  bfSpecificErrorHere('"expected identifier as property name")
  
bfKeyArg(k,x) ==
  ['%Key,k,x]

bfExpandKeys l ==
  args := nil
  while l is [a,:l] repeat
    a is ['%Key,k,x] =>
      args := [x,makeSymbol(stringUpcase symbolName k,'"KEYWORD"),:args]
    args := [a,:args]
  reverse! args      

bfApplication(bfop, bfarg) ==
  bfTupleP bfarg => [bfop,:bfExpandKeys rest bfarg]
  [bfop,bfarg]
 
-- returns the meaning of x in the appropriate Boot dialect.
bfReName x==
  a := x has SHOERENAME => first a
  x

sequence?(x,pred) ==
  x is ['QUOTE,seq] and cons? seq and
    "and"/[apply(pred,y,nil) for y in seq]

idList? x ==
  x is ["LIST",:.] and "and"/[defQuoteId arg for arg in x.args]

charList? x ==
  x is ["LIST",:.] and "and"/[bfChar? arg for arg in x.args]

stringList? x ==
  x is ["LIST",:.] and "and"/[bfString? arg for arg in x.args]

++ Generate code for a membership test `x in seq' where `seq'
++ is a sequence (e.g. a list)
bfMember(var,seq) ==
  integer? var or sequence?(seq,function integer?) =>
    seq is ['QUOTE,[x]] => ["EQL",var,x]
    ["scalarMember?",var,seq]
  defQuoteId var or sequence?(seq,function symbol?) =>
    seq is ['QUOTE,[x]] => ["EQ",var, quote x]
    ["symbolMember?",var,seq]
  idList? seq =>
    seq.args is [.] => ["EQ",var,:seq.args]
    symbol? var and seq.args is [x,y] =>
      bfOR [["EQ",var,x],["EQ",var,y]]
    ["symbolMember?",var,seq]
  bfChar? var or sequence?(seq,function char?) =>
    seq is ['QUOTE,[x]] => ["CHAR=",var,x]
    ["charMember?",var,seq]
  charList? seq =>
    seq.args is [.] => ["CHAR=",var,:seq.args]
    symbol? var and seq.args is [x,y] =>
      bfOR [["CHAR=",var,x],["CHAR=",var,y]]
    ["charMember?",var,seq]
  bfString? var or sequence?(seq,function string?) =>
    seq is ['QUOTE,[x]] => ["STRING=",var,x]
    ["stringMember?",var,seq]
  stringList? seq =>
    seq.args is [.] => ["STRING=",var,:seq.args]
    symbol? var and seq.args is [x,y] =>
      bfOR [["STRING=",var,x],["STRING=",var,y]]
    ["stringMember?",var,seq]
  ["MEMBER",var,seq]
  
bfInfApplication(op,left,right)==
  op is "EQUAL" => bfQ(left,right)
  op is "/="    => bfNOT bfQ(left,right)
  op is ">"     => bfLessp(right,left)
  op is "<"     => bfLessp(left,right)
  op is "<="    => bfNOT bfLessp(right,left)
  op is ">="    => bfNOT bfLessp(left,right)
  op is "OR"    => bfOR [left,right]
  op is "AND"   => bfAND [left,right]
  op is "IN"    => bfMember(left,right)
  [op,left,right]
 
bfNOT x==
  x is ["NOT",a]=> a
  x is ["NULL",a]=> a
  ["NOT",x]
 
bfFlatten(op, x) ==
  x is [=op,:.] => rest x
  [x]
 
bfOR l  ==
  l = nil => false
  rest l = nil => first l
  ["OR",:[:bfFlatten("OR",c) for c in l]]
 
bfAND l ==
  l = nil => true
  rest l = nil => first l
  ["AND",:[:bfFlatten("AND",c) for c in l]]
 
 
defQuoteId x==  
  x is ['QUOTE,:.] and symbol? second x
 
bfChar? x ==
  char? x or cons? x and x.op in '(char CODE_-CHAR SCHAR)
 
bfNumber? x==
  integer? x or float? x or
    cons? x and x.op in '(SIZE LENGTH CHAR_-CODE MAXINDEX _+ _-)

bfString? x ==
  string? x
    or cons? x and first x in '(STRING SYMBOL_-NAME subString)

bfQ(l,r)==
  bfChar? l or bfChar? r => ["CHAR=",l,r]
  bfNumber? l or bfNumber? r => ["EQL",l,r]
  defQuoteId l or defQuoteId r => ["EQ",l,r]
  l = nil => ["NULL",r]
  r = nil => ["NULL",l]
  l = true or r = true => ["EQ",l,r]
  bfString? l or bfString? r => ["STRING=",l,r]
  l is "%nothing" or r is "%nothing" => ["EQ",l,r]
  ["EQUAL",l,r]
 
bfLessp(l,r)==
  (integer? l or float? l) and l = 0 => ["PLUSP",r]
  (integer? r or float? r) and r = 0 => ["MINUSP", l]
  bfChar? l or bfChar? r => ["CHAR<",l,r]
  bfString? l or bfString? r => ["STRING<",l,r]
  ["<",l,r]

bfLambda(vars,body) ==
  -- FIXME: Check that we have only names in vars.
  vars := 
    bfTupleP vars => rest vars
    [vars]
  ["LAMBDA",vars,body]
 
bfMDef(tu,op,args,body) ==
  argl :=
    bfTupleP args => rest args
    [args]
  lamex := ["MLAMBDA",argl,backquote(body,argl)]
  def := [op,lamex]
  [shoeComp def,:[:shoeComps bfDef1(tu,d) for d in sideConditions tu]]
 
bfGargl(tu,argl) ==
  argl = nil => [[],[],[],[]]
  [a,b,c,d] := bfGargl(tu,rest argl)
  first argl is "&REST" =>
    [[first argl,:b],b,c,
       [["CONS",quote "LIST",first d],:rest d]]
  f := bfGenSymbol tu
  [[f,:a],[f,:b],[first argl,:c],[f,:d]]
 
bfDef1(tu,[op,args,body]) ==
  argl :=
    bfTupleP args => rest args
    [args]
  [quotes,control,arglp,body] := bfInsertLet(tu,argl,body)
  quotes => shoeLAM(tu,op,arglp,control,body)
  [[op,["LAMBDA",arglp,body]]]
 
shoeLAM(tu,op,args,control,body) ==
  margs := bfGenSymbol tu
  innerfunc:= makeSymbol strconc(symbolName op,'",LAM")
  [[innerfunc,["LAMBDA",args,body]],
     [op,["MLAMBDA",["&REST",margs],["CONS", quote innerfunc,
                    ["WRAP",margs,quote control]]]]]
 
bfDef(tu,op,args,body) ==
 $bfClamming =>
   [.,op1,arg1,:body1] := shoeComp first bfDef1(tu,[op,args,body])
   bfCompHash(tu,op1,arg1,body1)
 bfTuple
  [:shoeComps bfDef1(tu,d) for d in  [[op,args,body],:sideConditions tu]]
 
shoeComps  x==
  [shoeComp def for def in x]

shoeComp x==
  a := shoeCompTran second x
  a is ["LAMBDA",:.] => ["DEFUN",first x,second a,:CDDR a]
  ["DEFMACRO",first x,second a,:CDDR a]

++ Translate function parameter list to Lisp.
++ We are processing a function definition.  `p2' is the list of
++ parameters we have seen so far, and we are about to add a 
++ parameter `p1'.  Check that the new specification is coherent
++ with the previous one.  In particular, check that restrictions
++ on parameters with default values are satisfied.  Return the
++ new augmented parameter list.
bfParameterList(p1,p2) ==
  p2=nil and p1 is [.,:.] => p1
  p1 is ["&OPTIONAL",:.] =>
    p2 isnt ["&OPTIONAL",:.] => bfSpecificErrorHere '"default value required"
    [first p1,:rest p1,:rest p2]
  p2 is ["&OPTIONAL",:.] =>   [p1,first p2,:rest p2]
  [p1,:p2]
 
bfInsertLet(tu,x,body)==
  x = nil => [false,nil,x,body]
  x is ["&REST",a] =>
    a is ['QUOTE,b] => [true,'QUOTE,["&REST",b],body]
    [false,nil,x,body]
  [b,norq,name1,body1] :=  bfInsertLet1(tu,first x,body)
  [b1,norq1,name2,body2] :=  bfInsertLet(tu,rest x,body1)
  [b or b1,[norq,:norq1],bfParameterList(name1,name2),body2]
 
bfInsertLet1(tu,y,body)==
  y is ["L%T",l,r] => [false,nil,l,bfMKPROGN [bfLET(tu,r,l),body]]
  symbol? y => [false,nil,y,body]
  y is ["BVQUOTE",b] => [true,'QUOTE,b,body]
  g := bfGenSymbol tu
  y isnt [.,:.] => [false,nil,g,body]
  case y of
    %DefaultValue(p,v) => [false,nil,["&OPTIONAL",[p,v]],body]
    otherwise => [false,nil,g,bfMKPROGN [bfLET(tu,compFluidize y,g),body]]
 
shoeCompTran x==
  [lamtype,args,:body] := x
  fluidVars := ref []
  locVars := ref []
  dollarVars := ref []
  shoeCompTran1(body,fluidVars,locVars,dollarVars)
  deref(locVars) := setDifference(setDifference(deref locVars,deref fluidVars),shoeATOMs args)
  body :=
    body' := body
    if fvars := setDifference(deref dollarVars,deref fluidVars) then
      body' := [["DECLARE",["SPECIAL",:fvars]],:body']
    vars := deref locVars => declareLocalVars(vars,body')
    maybeAddBlock body'
  if fl := shoeFluids args then
    body := [["DECLARE",["SPECIAL",:fl]],:body]
  [lamtype,args,:body]

declareLocalVars(vars,stmts) ==
  stmts is [["LET*",inits,:stmts]] =>
    [["LET*",[:inits,:vars],:maybeAddBlock stmts]]
  [["LET*",vars,:maybeAddBlock stmts]]

maybeAddBlock stmts ==
  [:decls,expr] := stmts
  hasReturn? expr =>
    decls = nil => [["BLOCK","NIL",:stmts]]
    [:decls,["BLOCK","NIL",expr]]
  stmts

hasReturn? x ==
  x isnt [.,:.] => false
  x.op is 'RETURN => true
  x.op in '(LOOP PROG BLOCK LAMBDA DECLARE) => false
  or/[hasReturn? t for t in x]

shoeFluids x==
  ident? x and bfBeginsDollar x => [x]
  atomic? x => nil
  [:shoeFluids first x,:shoeFluids rest x]

shoeATOMs x ==
  ident? x => [x]
  atomic? x => nil
  [:shoeATOMs first x,:shoeATOMs rest x]

++ Return true if `x' is an identifier name that designates a
++ dynamic (e.g. Lisp special) variable.  
isDynamicVariable x ==
  symbol? x and bfBeginsDollar x =>
    symbolMember?(x,$constantIdentifiers) => false
    readOnly? x => false
    symbolGlobal? x or $activeNamespace = nil => true
    y := symbolBinding(symbolName x,$activeNamespace) => not readOnly? y
    true
  false
 
shoeCompTran1(x,fluidVars,locVars,dollarVars) ==
  x isnt [.,:.] =>
    if isDynamicVariable x and not symbolMember?(x,deref dollarVars) then
      deref(dollarVars) := [x,:deref dollarVars]
    x
  U := first x
  U is 'QUOTE => x
  x is ["CASE",y,:zs] =>
    second(x) := shoeCompTran1(y,fluidVars,locVars,dollarVars)
    while zs ~= nil repeat
      second(first zs) :=
        shoeCompTran1(second first zs,fluidVars,locVars,dollarVars)
      zs := rest zs
    x
  x is ["L%T",l,r] =>
    third(x) := shoeCompTran1(r,fluidVars,locVars,dollarVars)
    l is ['%Dynamic,y] =>
      if not symbolMember?(y,deref fluidVars) then
        deref(fluidVars) := [y,:deref fluidVars]
      -- Defer translation of operator for this form.
      second(x) := y
      x
    l is ['%Signature,:.] => x    -- local binding with explicit typing
    x.op := "SETQ"
    symbol? l =>
      bfBeginsDollar l =>
        if not symbolMember?(l,deref dollarVars) then
          deref(dollarVars) := [l,:deref dollarVars]
        x
      if not symbolMember?(l,deref locVars) then
        deref(locVars) := [l,:deref locVars]
      x
    x
  U is "%Leave" => (x.op := "RETURN"; x)
  U in '(PROG LAMBDA) =>
    newbindings := nil
    for y in second x repeat
      not symbolMember?(y,deref locVars)=>
	deref(locVars) := [y,:deref(locVars)]
	newbindings := [y,:newbindings]
    rest(x).rest := shoeCompTran1(CDDR x,fluidVars,locVars,dollarVars)
    deref(locVars) := [y for y in deref locVars |
                         not symbolMember?(y,newbindings)]
    x
  -- literal vectors.
  x is ['vector,elts] =>
    do
      elts is 'NIL =>
        x.op := 'VECTOR
        x.args := nil
      elts is ['LIST,:.] =>
        x.op := 'VECTOR
        x.args := shoeCompTran1(elts.args,fluidVars,locVars,dollarVars)
      elts isnt [.,:.] =>
        elts := shoeCompTran1(elts,fluidVars,locVars,dollarVars)
        x.op := 'MAKE_-ARRAY
        x.args := [['LIST_-LENGTH,elts],KEYWORD::INITIAL_-CONTENTS,elts]
      x.op := 'COERCE
      x.args := [shoeCompTran1(elts,fluidVars,locVars,dollarVars),quote 'VECTOR]
    x
  x is ['%Namespace,n] =>
    n is "DOT" => "*PACKAGE*"
    ["FIND-PACKAGE",symbolName n]
  x.first := shoeCompTran1(first x,fluidVars,locVars,dollarVars)
  x.rest := shoeCompTran1(rest x,fluidVars,locVars,dollarVars)
  bindFluidVars! x
 
bindFluidVars! x ==
  x is [["L%T",['%Signature,v,t],expr],:stmts] =>
    x.first :=
      stmts = nil => ["LET",[[v,expr]],['DECLARE,['TYPE,t]],v]
      ["LET",[[v,expr]],['DECLARE,['TYPE,t]],:bindFluidVars! stmts]
    x.rest := nil
    x
  if x is [["L%T",:init],:stmts] then
    x.first := groupFluidVars([init],[first init],stmts)
    x.rest := nil
  x is ["PROGN",y] => y
  x

groupFluidVars(inits,vars,stmts) ==
  stmts is [["LET",inits',["DECLARE",["SPECIAL",:vars']],:stmts']]
    and inits' is [.] =>
      groupFluidVars([:inits,:inits'],[:vars,:vars'],stmts')
  stmts is [["LET*",inits',["DECLARE",["SPECIAL",:vars']],:stmts']] =>
    groupFluidVars([:inits,:inits'],[:vars,:vars'],stmts')
  inits is [.] =>
    ["LET",inits,["DECLARE",["SPECIAL",:vars]],bfMKPROGN stmts]
  ["LET*",inits,["DECLARE",["SPECIAL",:vars]],bfMKPROGN stmts]

bfRestrict(x,t) ==
  ["THE",t,x]

bfAssign(tu,l,r)==
  bfTupleP l => bfSetelt(second l,CDDR l ,r)
  l is ["%Place",:l'] => ["SETF",l',r]
  bfLET(tu,l,r)
 
bfSetelt(e,l,r)==
  rest l = nil => defSETELT(e,first l,r)
  bfSetelt(bfElt(e,first l),rest l,r)
 
bfElt(expr,sel)==
  y := symbol? sel and sel has SHOESELFUNCTION
  y =>
    integer? y => ["ELT",expr,y]
    [y,expr]
  ["ELT",expr,sel]
 
defSETELT(var,sel,expr)==
  y := symbol? sel and sel has SHOESELFUNCTION
  y =>
    integer? y => ["SETF",["ELT",var,y],expr]
    y is "CAR" => ["RPLACA",var,expr]
    y is "CDR" => ["RPLACD",var,expr]
    ["SETF",[y,var],expr]
  ["SETF",["ELT",var,sel],expr]
 
bfIfThenOnly(a,b)==
  b1 :=
    b is ["PROGN",:.] => rest b
    [b]
  ["COND",[a,:b1]]
 
bfIf(a,b,c)==
  b1 :=
    b is ["PROGN",:.] => rest b
    [b]
  c is ["COND",:.] => ["COND",[a,:b1],:rest c]
  c1 :=
    c is ["PROGN",:.] => rest c
    [c]
  ["COND",[a,:b1],['T,:c1]]
 
bfExit(a,b)==  
  ["COND",[a,["IDENTITY",b]]]
 
bfFlattenSeq l ==
  l = nil => l
  [x,:xs] := l
  x isnt [.,:.] =>
    xs = nil => l
    bfFlattenSeq xs
  x.op is 'PROGN => bfFlattenSeq [:x.args,:xs]
  [x,:bfFlattenSeq xs]

bfMKPROGN l==
  l := bfFlattenSeq l
  l = nil => nil
  l is [.] => first l
  ["PROGN",:l]
 
++ The body of each branch of a COND form is an implicit PROGN.  
++ For readability purpose, we want to refrain from including
++ any explicit PROGN.
bfWashCONDBranchBody x ==
  x is ["PROGN",:y] => y
  [x]

bfAlternative(a,b) ==
  a is ["AND",:conds,["PROGN",stmt,='T]] =>
    [["AND",:conds], :bfWashCONDBranchBody bfMKPROGN [stmt,b]]
  [a,:bfWashCONDBranchBody b]

bfSequence l ==
  l = nil => nil
  transform := [bfAlternative(a,b) for x in l while
                  x is ["COND",[a,["IDENTITY",b]]]]
  no := #transform
  before := bfTake(no,l)
  aft := bfDrop(no,l)
  before = nil =>
    l is [f] =>
      f is ["PROGN",:.] => bfSequence rest f
      f
    bfMKPROGN [first l,bfSequence rest l]
  aft = nil => ["COND",:transform]
  ["COND",:transform,bfAlternative('T,bfSequence aft)]
 
bfWhere(tu,context,expr)==
  [opassoc,defs,nondefs] := defSheepAndGoats(tu,context)
  a:=[[first d,second d,bfSUBLIS(opassoc,third d)]
               for d in defs]
  sideConditions(tu) := [:a,:sideConditions tu]
  bfMKPROGN bfSUBLIS(opassoc,append!(nondefs,[expr]))
 
--shoeReadLispString(s,n)==
--    n>= # s => nil
--    [exp,ind]:=shoeReadLisp(s,n)
--    exp = nil => nil
--    [exp,:shoeReadLispString(s,ind)]
 
bfCompHash(tu,op,argl,body) ==
  auxfn:= makeSymbol strconc(symbolName op,'";")
  computeFunction:= ["DEFUN",auxfn,argl,:body]
  bfTuple [computeFunction,:bfMain(tu,auxfn,op)]
 
shoeCompileTimeEvaluation x ==
  ["EVAL-WHEN", [KEYWORD::COMPILE_-TOPLEVEL], x]

bfMain(tu,auxfn,op)==
  g1 := bfGenSymbol tu
  arg :=["&REST",g1]
  computeValue := ['APPLY,["FUNCTION",auxfn],g1]
  cacheName := makeSymbol strconc(symbolName op,'";AL")
  g2:= bfGenSymbol tu
  getCode := ['GETHASH,g1,cacheName]
  secondPredPair := [['SETQ,g2,getCode],g2]
  putCode := ['SETF ,getCode,computeValue]
  thirdPredPair:= ['T,putCode]
  codeBody:= ['PROG,[g2],
               ['RETURN,['COND,secondPredPair,thirdPredPair]]]
  mainFunction:= ["DEFUN",op,arg,codeBody]
 
  cacheType:=     'hash_-table
  cacheResetCode := ['SETQ,cacheName,['MAKE_-HASHTABLE,quote "UEQUAL"]]
  cacheCountCode := ['hashCount,cacheName]
  cacheVector:=
      [op,cacheName,cacheType,cacheResetCode,cacheCountCode]
  defCode := ["DEFPARAMETER",cacheName,['MAKE_-HASHTABLE,quote "UEQUAL"]]
  [defCode,mainFunction,
    ["SETF",["GET",quote op,quote 'cacheInfo],quote cacheVector]]


bfNamespace x ==
  ['%Namespace,x]

bfNameOnly: %Thing -> %Form
bfNameOnly x==
  x is "t" => ["T"]
  [x]

bfNameArgs: (%Thing,%Thing) -> %List %Form
bfNameArgs (x,y)==
  y :=
    y is ["TUPLE",:.] => rest y
    [y]
  [x,:y]
 
bfCreateDef: (%LoadUnit,%Thing) -> %Form
bfCreateDef(tu,x) ==
  x is [f] => ["DEFCONSTANT",f,["LIST",quote f]]
  a := [bfGenSymbol tu for i in rest x]
  ["DEFUN",first x,a,["CONS",quote first x,["LIST",:a]]]

bfCaseItem: (%Thing,%Thing) -> %Form
bfCaseItem(x,y) ==
  [x,y]

bfCase: (%LoadUnit,%Thing,%Thing) -> %Form
bfCase(tu,x,y)==
  -- Introduce a temporary to hold the value of the scrutinee.
  -- To minimize the number of GENSYMS and assignments, we want
  -- to do this only when the scrutinee is not reduced yet.
  g := 
    x isnt [.,:.] => x 
    bfGenSymbol tu
  body := ["CASE",["CAR", g], :bfCaseItems(g,y)]
  sameObject?(g,x) => body
  ["LET",[[g,x]],body]

bfCaseItems: (%Thing,%List %Form) -> %List %Form
bfCaseItems(g,x) ==  
  [bfCI(g,i,j) for [i,j] in x]

bfCI: (%Thing,%Thing,%Thing) -> %Form
bfCI(g,x,y)==
  a := rest x
  a = nil => [first x,y]
  b := [[i,bfCARCDR(j,g)] for i in a for j in 1.. | i isnt "DOT"]
  b = nil => [first x,y]
  [first x,["LET",b,y]]

bfCARCDR: (%Short,%Thing) -> %Form
bfCARCDR(n,g) ==
  [makeSymbol strconc('"CA",bfDs n,'"R"),g]

bfDs: %Short -> %String 
bfDs n == 
  n = 0 => '""
  strconc('"D",bfDs(n-1))

bfEnum(t,csts) ==
  ['DEFTYPE,t,nil,backquote(['MEMBER,:csts],nil)]

bfRecordDef(tu,s,fields,accessors) ==
  parms := [x for f in fields | f is ['%Signature,x,.]]
  fun := makeSymbol strconc('"mk",symbolName s)
  ctor := makeSymbol strconc('"MAKE-",symbolName s)
  recDef := ["DEFSTRUCT",
               [s,[bfColonColon("KEYWORD","COPIER"),
                    makeSymbol strconc('"copy",symbolName s)]],
                      :[x for ['%Signature,x,.] in fields]]
  ctorDef :=
    args := [:[bfColonColon("KEYWORD",p),p] for p in parms]
    ["DEFMACRO",fun,parms,["LIST",quote ctor,:args]]
  accDefs :=
    accessors = nil => nil
    x := bfGenSymbol tu
    [["DEFMACRO",acc,[x],
       ["LIST",quote makeSymbol strconc(symbolName s,'"-",symbolName f),x]]
         for ['%AccessorDef,acc,f] in accessors]
  [recDef,ctorDef,:accDefs]

bfHandlers(n,e,hs) == main(n,e,hs,nil) where
  main(n,e,hs,xs) ==
    hs = nil =>
      ["COND",
        :reverse!
          [[true,["THROW",KEYWORD::OPEN_-AXIOM_-CATCH_-POINT,n]],:xs]]
    hs is [['%Catch,['%Signature,v,t],s],:hs'] =>
      t := 
        symbol? t => quote [t] -- instantiate niladic type ctor
        quote t
      main(n,e,hs',[[bfQ(["CAR",e],t),["LET",[[v,["CDR",e]]],s]],:xs])
    bfSpecificErrorHere '"invalid handler message"

codeForCatchHandlers(g,e,cs) ==
  ehTest := ['AND,['CONSP,g],
              bfQ(['CAR,g],KEYWORD::OPEN_-AXIOM_-CATCH_-POINT)]
  ["LET",[[g,["CATCH",KEYWORD::OPEN_-AXIOM_-CATCH_-POINT,e]]],
    ["COND",[ehTest,bfHandlers(g,["CDR",g],cs)],[true,g]]]

++ Generate code for try-catch expressions.
bfTry: (%Thing,%List %Form) -> %Thing
bfTry(e,cs) ==
  g := gensym()
  cs is [:cs',f] and f is ['%Finally,s] =>
    cs' = nil => ["UNWIND-PROTECT",e,s]
    ["UNWIND-PROTECT",codeForCatchHandlers(g,e,cs'),s]
  codeForCatchHandlers(g,e,cs)

++ Generate code for `throw'-expressions
bfThrow e ==
  t := nil
  x := nil
  if e is ['%Signature,:.] then
    t := third e
    x := second e
  else
    t := "SystemException"
    x := e
  t :=
    symbol? t => quote [t]
    quote t
  ["THROW",KEYWORD::OPEN_-AXIOM_-CATCH_-POINT,
    ["CONS",KEYWORD::OPEN_-AXIOM_-CATCH_-POINT,["CONS",t,x]]]

--%

bfType x ==
  x is ['%Mapping,t,s] =>
    if bfTupleP s then
      s := s.args
    if ident? s then
      s := [s]
    ['FUNCTION,[bfType y for y in s],bfType t]
  x is [.,:.] => [x.op,:[bfType y for y in x.args]]
  x

--% Type alias definition

backquote: (%Form,%List %Symbol) -> %Form
backquote(form,params) ==
  params = nil => quote  form
  form isnt [.,:.] =>
    symbolMember?(form,params) => form
    integer? form or string? form => form
    quote form
  ["LIST",:[backquote(t,params) for t in form]]

genTypeAlias(head,body) ==
  [op,:args] := head
  ["DEFTYPE",op,args,backquote(body,args)]

--%
--% Native Interface Translation
--%

-- The Native Interface Translation support the following datatypes
--     void:  No value, useful only as function return type.
--
--     char:  Character type, corresponds to C type 'char'.
--
--     byte:  8-bit data type for the unit of information; corresponds 
--            to C type 'unsigned char' on 8-bit char machines.
--
--     Note:  We require 2's complement representation.
--
--     int8:  8-bit signed integer data type; int8_t in ISO C.
--    uint8:  8-bit unsigned integer data type; uint8_t in ISO C.
--    int16:  16-bit signed integer data type; int16_t is ISO C.
--   uint16:  16-bit unsigned integer data type; uint16_t in ISO C.
--    int32:  32-bit signed integer data type; int32_t in ISO C.
--   uint32:  32-bit unsigned integer data type; uint32_t in ISO C.
--    int64:  64-bit signed integer data type; int64_t in ISO C.
--   uint64:  64-bit unsigned integer data type; uint64_t in ISO C.
--
--      int:  Native integer data type.  Ideally should be wide enough
--            to represent native address space.  However, only ECL
--            and GCL seems to give that guarantee at the moment.
--
--    float:  single precision datatype for floating poing values.
--  float32   Corresponds to C type 'float'.  On most architecture,
--            this is a 32-bit precision IEEE 756 data type.
--
--   double:  double precision datatype for floating point values.
--  float64   Corresponds to C type 'double'.  On most architecture,
--            this is a 64-bit precision IEEE 756 data type.
--
--   string:  a data type for strings of characters.  The general
--            semantics is that a string is passed by value (e.g.
--            copied into a separate storage) to a native
--            function.  In many cases, that is appropriate (e.g.
--            mkdir "foo") if just wasteful.  In other cases, that is
--            not appropriate, as the native function may expect a
--            pass-by-reference semantics, e.g. modify the argument.
--            Consequently, argument types may be combined with the
--            modifiers `readonly' and `writeonly'.  Note that a
--            function return type may not use modifiers.
--            Corresponds to C's notion of NUL-terminated string,
--            'char*'.  In particular, the length of a string is
--            stored as separate datum part of the data being
--            transmitted.
--
--   buffer:  A data type constructor for array of simple data 
--            (e.g. array of bytes, array of float, array of double).
--            This is used to communicate data between native
--            functions and OpenAxiom functions.  The `buffer' type
--            constructor must be used in conjunction with one of the 
--            modifiers `readonly', `writeonly', or `readwrite', and 
--            instantiated with one of `char', `byte', `int', `float',
--            and `double'.  It cannot be used as function return type.
--            Note that the length of the array is not stored as 
--            part of the data being transmitted.
--
--  pointer:  A data type constructor for pointer to simple data 
--            This is used to communicate pointer to foreign data 
--            between native functions and OpenAxiom functions.
--            The `buffer' type constructor must be used in
--            conjunction with one of the modifiers `readonly',
--            `writeonly', or `readwrite'.

$NativeSimpleDataTypes ==
  '(char   byte    int   pointer
    int8   uint8
    int16  uint16
    int32  uint32
    int64  uint64
    float  float32
    double float64)

$NativeSimpleReturnTypes ==
  [:$NativeSimpleDataTypes,:'(void string)]

++ Returns true if `t' is a simple native data type.
isSimpleNativeType t ==
  objectMember?(t,$NativeSimpleReturnTypes)

coreSymbol: %Symbol -> %Symbol
coreSymbol s ==
  makeSymbol(symbolName s, "AxiomCore")

bootSymbol: %Symbol -> %Symbol
bootSymbol s ==
  makeSymbol symbolName s


unknownNativeTypeError t ==
  fatalError strconc('"unsupported native type: ", PNAME t)


nativeType t ==
  t = nil => t
  t isnt [.,:.] =>
    t' := rest objectAssoc(coreSymbol t,$NativeTypeTable) => 
      t' := 
	%hasFeature KEYWORD::SBCL => bfColonColon("SB-ALIEN", t')
	%hasFeature KEYWORD::CLISP => bfColonColon("FFI",t')
	t'
      -- ??? decree we have not discovered Unicode yet.
      t is "string" and %hasFeature KEYWORD::SBCL =>
	[t',KEYWORD::EXTERNAL_-FORMAT,KEYWORD::ASCII,
	   KEYWORD::ELEMENT_-TYPE, "BASE-CHAR"]
      t'
    t in '(byte uint8) =>
      %hasFeature KEYWORD::SBCL => [bfColonColon("SB-ALIEN","UNSIGNED"),8]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","UINT8")
      %hasFeature KEYWORD::ECL or %hasFeature KEYWORD::CLOZURE =>
        KEYWORD::UNSIGNED_-BYTE
      nativeType "char"           -- approximate by 'char' for GCL
    t is "int16" =>
      %hasFeature KEYWORD::SBCL => [bfColonColon("SB-ALIEN","SIGNED"),16]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","INT16")
      %hasFeature KEYWORD::ECL and %hasFeature KEYWORD::UINT16_-T =>
         KEYWORD::INT16_-T
      %hasFeature KEYWORD::CLOZURE => KEYWORD::SIGNED_-HALFWORD
      unknownNativeTypeError t
    t is "uint16" =>
      %hasFeature KEYWORD::SBCL => [bfColonColon("SB-ALIEN","UNSIGNED"),16]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","UINT16")
      %hasFeature KEYWORD::ECL and %hasFeature KEYWORD::UINT16_-T =>
         KEYWORD::UINT16_-T
      %hasFeature KEYWORD::CLOZURE => KEYWORD::UNSIGNED_-HALFWORD
      unknownNativeTypeError t
    t is "int32" =>
      %hasFeature KEYWORD::SBCL => [bfColonColon("SB-ALIEN","SIGNED"),32]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","INT32")
      %hasFeature KEYWORD::ECL and %hasFeature KEYWORD::UINT32_-T =>
         KEYWORD::INT32_-T
      %hasFeature KEYWORD::CLOZURE => KEYWORD::SIGNED_-FULLWORD
      unknownNativeTypeError t
    t is "uint32" =>
      %hasFeature KEYWORD::SBCL => [bfColonColon("SB-ALIEN","UNSIGNED"),32]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","INT32")
      %hasFeature KEYWORD::ECL and %hasFeature KEYWORD::UINT32_-T =>
         KEYWORD::UINT32_-T
      %hasFeature KEYWORD::CLOZURE => KEYWORD::UNSIGNED_-FULLWORD
      unknownNativeTypeError t
    t is "int64" =>
      %hasFeature KEYWORD::SBCL => [bfColonColon("SB-ALIEN","SIGNED"),64]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","INT64")
      %hasFeature KEYWORD::ECL and %hasFeature KEYWORD::UINT64_-T =>
         KEYWORD::INT64_-T
      %hasFeature KEYWORD::CLOZURE => KEYWORD::SIGNED_-DOUBLEWORD
      unknownNativeTypeError t
    t is "uint64" =>
      %hasFeature KEYWORD::SBCL => [bfColonColon("SB-ALIEN","UNSIGNED"),64]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","UINT64")
      %hasFeature KEYWORD::ECL and %hasFeature KEYWORD::UINT64_-T =>
         KEYWORD::UINT64_-T
      %hasFeature KEYWORD::CLOZURE => KEYWORD::UNSIGNED_-DOUBLEWORD
      unknownNativeTypeError t
    t is "float32" => nativeType "float"
    t is "float64" => nativeType "double"
    t is "pointer" =>
      %hasFeature KEYWORD::GCL => "fixnum"
      %hasFeature KEYWORD::ECL => KEYWORD::POINTER_-VOID
      %hasFeature KEYWORD::SBCL => ["*",bfColonColon("SB-ALIEN","VOID")]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","C-POINTER")
      %hasFeature KEYWORD::CLOZURE => KEYWORD::ADDRESS
      unknownNativeTypeError t
    unknownNativeTypeError t
  -- composite, reference type.
  first t is "buffer" =>
    %hasFeature KEYWORD::GCL => "OBJECT"
    %hasFeature KEYWORD::ECL => KEYWORD::OBJECT
    %hasFeature KEYWORD::SBCL => ["*",nativeType second t]
    %hasFeature KEYWORD::CLISP => bfColonColon("FFI","C-POINTER")
    %hasFeature KEYWORD::CLOZURE => [KEYWORD::_*, nativeType second t]
    unknownNativeTypeError t
  first t is "pointer" =>
    -- we don't bother looking at what the pointer points to.
    nativeType "pointer"
  unknownNativeTypeError t

++ Check that `t' is a valid return type for a native function, and
++ returns its translation
nativeReturnType t ==
  objectMember?(t,$NativeSimpleReturnTypes) => nativeType t
  coreError strconc('"invalid return type for native function: ", 
              PNAME t)

++ Check that `t' is a valid parameter type for a native function,
++ and returns its translation.
nativeArgumentType t ==
  objectMember?(t,$NativeSimpleDataTypes) => nativeType t
  -- Allow 'string'  for `pass-by-value'
  t is "string" => nativeType t
  -- anything else must use a modified reference type.
  t isnt [.,:.] or #t ~= 2 => 
     coreError '"invalid argument type for a native function"
  [m,[c,t']] := t
  -- Require a modifier.
  not (m in '(readonly writeonly readwrite)) =>
    coreError '"missing modifier for argument type for a native function"
  -- Only 'pointer' and 'buffer' can be instantiated.
  not (c in '(buffer pointer)) =>
    coreError '"expected 'buffer' or 'pointer' type instance"
  not objectMember?(t',$NativeSimpleDataTypes) =>
    coreError '"expected simple native data type"
  nativeType second t
  
++ True if objects of type native type `t' are sensible to GC.
needsStableReference? t ==
  t is [m,:.] and m in '(readonly writeonly readwrite)

++ coerce argument `a' to native type `t', in preparation for
++ a call to a native functions.
coerceToNativeType(a,t) ==
  -- GCL, ECL, CLISP, and CLOZURE don't do it this way.
  %hasFeature KEYWORD::GCL or %hasFeature KEYWORD::ECL
     or %hasFeature KEYWORD::CLISP or %hasFeature KEYWORD::CLOZURE => a
  %hasFeature KEYWORD::SBCL =>
    not needsStableReference? t => a
    [.,[c,y]] := t
    c is "buffer" => [bfColonColon("SB-SYS","VECTOR-SAP"),a]
    c is "pointer" => [bfColonColon("SB-SYS","ALIEN-SAP"),a]
    needsStableReference? t =>
      fatalError strconc('"don't know how to coerce argument for native type",
        PNAME c)
  fatalError '"don't know how to coerce argument for native type"


++ Generate GCL native translation for import op: s -> t for op'
++ `argtypes' is the list of GCL FFI names for types in `s'.
++ `rettype' is the GCL FFI name for `t'.
genGCLnativeTranslation(op,s,t,op') ==
  argtypes := [nativeArgumentType x for x in s]
  rettype := nativeReturnType t
  -- If a simpel DEFENTRY will do, go for it
  and/[isSimpleNativeType x for x in [t,:s]] =>
    [["DEFENTRY", op, argtypes, [rettype, symbolName op']]]
  -- Otherwise, do it the hard way.
  [["CLINES",ccode], ["DEFENTRY", op, argtypes, [rettype, cop]]] where
    cop := strconc(symbolName op','"__stub")
    ccode := 
      "strconc"/[gclTypeInC t, '" ", cop, '"(",
	 :[cparm(x,a) for x in tails s for a in tails cargs],
	   '") { ", (t isnt "void" => '"return "; ""),
	     symbolName op', '"(",
	       :[gclArgsInC(x,a) for x in tails s for a in tails cargs],
		  '"); }" ]
                where cargs := [mkCArgName i for i in 0..(#s - 1)]
    mkCArgName i == strconc('"x",toString i)
    cparm(x,a) ==
      strconc(gclTypeInC first x, '" ", first a,
	(rest x => '", "; '""))
    gclTypeInC x ==
      objectMember?(x,$NativeSimpleDataTypes) => symbolName x
      x is "void" => '"void"
      x is "string" => '"char*"
      x is [.,["pointer",.]] => "fixnum"
      '"object" 
    gclArgInC(x,a) ==
      objectMember?(x,$NativeSimpleDataTypes) => a
      x is "string" => a   -- GCL takes responsability for the conversion
      [.,[c,y]] := x
      c is "pointer" => a
      y is "char" => strconc(a,'"->st.st__self")
      y is "byte" => strconc(a,'"->ust.ust__self")
      y is "int" => strconc(a,'"->fixa.fixa__self")
      y is "float" => strconc(a,'"->sfa.sfa__self")
      y is "double" => strconc(a,'"->lfa.lfa__self")
      coreError '"unknown argument type"
    gclArgsInC(x,a) ==
      strconc(gclArgInC(first x, first a),
	(rest x => '", "; '""))  

genECLnativeTranslation(op,s,t,op') ==
  args := nil
  argtypes := nil
  for x in s repeat
     argtypes := [nativeArgumentType x,:argtypes]
     args := [gensym(),:args]
  args := reverse args
  rettype := nativeReturnType t
  [["DEFUN",op, args,
    [bfColonColon("FFI","C-INLINE"),args, reverse! argtypes,
      rettype, callTemplate(op',#args,s), 
        KEYWORD::ONE_-LINER, true]]] where
	  callTemplate(op,n,s) ==
	    "strconc"/[symbolName op,'"(",
	      :[sharpArg(i,x) for i in 0..(n-1) for x in s],'")"]
	  sharpArg(i,x) == 
	    i = 0 => strconc('"(#0)",selectDatum x)
	    strconc('",",'"(#", toString i, '")", selectDatum x)
	  selectDatum x ==
	    isSimpleNativeType x => '""
	    [.,[c,y]] := x
            c is "buffer" => 
	      y is "char" or y is "byte" => 
                AxiomCore::$ECLVersionNumber < 90100 => '"->vector.self.ch"
                y is "char" => '"->vector.self.i8"
                '"->vector.self.b8"
	      y is "int" => '"->vector.self.fix"
	      y is "float" => '"->vector.self.sf"
	      y is "double" => '"->vector.self.df"
	      coreError '"unknown argument to buffer type constructor"
            c is "pointer" => '""
            coreError '"unknown type constructor"

genCLISPnativeTranslation(op,s,t,op') ==
  -- check parameter types and return types.
  rettype := nativeReturnType t
  argtypes := [nativeArgumentType x for x in s]

  -- There is a curious bug in the CLisp's FFI support whereby
  -- foreign declarations compiled separately will have the wrong
  -- types when used in other modules.  We work around that problem
  -- by defining forwarding functions to the foreign declarations
  -- in the same module the latter are declared.  Even if and when
  -- that bug is fixed, we still need forwarding function because,
  -- CLISP's FFI takes every step to ensure that Lisp world objects
  -- do not mix with C world object, presumably because they are not
  -- from the same class.  Consequently, we must allocate C-storage,
  -- copy data there, pass pointers to them, and possibly copy
  -- them back.  Ugh.  
  n := makeSymbol strconc(symbolName op, '"%clisp-hack")
  parms := [gensym '"parm" for x in s]  -- parameters of the forward decl.

  -- Now, separate non-simple data from the rest.  This is a triple-list
  -- of the form ((parameter boot-type . ffi-type) ...)
  unstableArgs := nil
  for p in parms for x in s for y in argtypes repeat
    needsStableReference? x =>
      unstableArgs := [[p,x,:y],:unstableArgs]

  -- The actual FFI declaration for the native call.  Note that 
  -- parameter of non-simple datatype are described as being pointers.
  foreignDecl := 
    [bfColonColon("FFI","DEF-CALL-OUT"),n,
      [KEYWORD::NAME,symbolName op'],
	[KEYWORD::ARGUMENTS,:[[a, x] for x in argtypes for a in parms]],
	  [KEYWORD::RETURN_-TYPE, rettype],
	      [KEYWORD::LANGUAGE,KEYWORD::STDC]]

  -- The forwarding function.  We have to introduce local foreign
  -- variables to hold the address of converted Lisp objects.  Then
  -- we have to copy back those that are `writeonly' or `readwrite' to
  -- simulate the reference semantics.  Don't ever try to pass around
  -- gigantic buffer, you might find out that it is insanely inefficient.
  forwardingFun := 
    unstableArgs = nil => ["DEFUN",op,parms, [n,:parms]]
    localPairs := [[a,x,y,:gensym '"loc"] for [a,x,:y] in unstableArgs]
    call := 
      [n,:[actualArg(p,localPairs) for p in parms]] where
	    actualArg(p,pairs) ==
	      a' := rest objectAssoc(p,pairs) => rest rest a'
	      p
    -- Fix up the call if there is any `write' parameter.
    call := 
      fixups := [q | not null (q := copyBack p) for p in localPairs] where
                  copyBack [p,x,y,:a] ==
                    x is ["readonly",:.] => nil
                    ["SETF", p, [bfColonColon("FFI","FOREIGN-VALUE"), a]]
      fixups = nil => [call]
      [["PROG1",call, :fixups]]
    -- Set up local foreign variables to hold address of traveling data
    for [p,x,y,:a] in localPairs repeat
      call := 
        [[bfColonColon("FFI","WITH-FOREIGN-OBJECT"),
            [a, ["FUNCALL",
               ["INTERN",'"getCLISPType",'"BOOTTRAN"], p], p], :call]]
    -- Finally, define the forwarding function.
    ["DEFUN",op,parms,:call]
  $foreignsDefsForCLisp := [foreignDecl,:$foreignsDefsForCLisp]
  [forwardingFun]

getCLISPType a ==
  [bfColonColon("FFI","C-ARRAY"), #a]


genSBCLnativeTranslation(op,s,t,op') ==    
  -- check return type and argument types.
  rettype := nativeReturnType t
  argtypes := [nativeArgumentType x for x in s]

  args := [gensym() for x in s]
  unstableArgs := nil
  newArgs := nil
  for a in args for x in s repeat
    newArgs := [coerceToNativeType(a,x), :newArgs]
    if needsStableReference? x then
      unstableArgs := [a,:unstableArgs]
  
  op' :=
    %hasFeature KEYWORD::WIN32 => strconc('"__",symbolName op')
    symbolName op'
    
  unstableArgs = nil =>
    [["DEFUN",op,args,
      [makeSymbol('"ALIEN-FUNCALL",'"SB-ALIEN"),
	[makeSymbol('"EXTERN-ALIEN",'"SB-ALIEN"), op',
	  ["FUNCTION",rettype,:argtypes]], :args]]]
  [["DEFUN",op,args,
    [bfColonColon("SB-SYS","WITH-PINNED-OBJECTS"), reverse! unstableArgs,
      [makeSymbol('"ALIEN-FUNCALL",'"SB-ALIEN"),
	[makeSymbol('"EXTERN-ALIEN",'"SB-ALIEN"), op',
	  ["FUNCTION",rettype,:argtypes]], :reverse! newArgs]]]]



++ Generate Clozure CL's equivalent of import declaration
genCLOZUREnativeTranslation(op,s,t,op') ==
  -- check parameter types and return types.
  rettype := nativeReturnType t
  argtypes := [nativeArgumentType x for x in s]

  -- Build parameter list for the forwarding function
  parms := [gensym '"parm" for x in s]

  -- Separate string arguments and array arguments from scalars.
  -- These array arguments need to be pinned down, and the string
  -- arguments need to stored in a stack-allocaed NTBS.
  strPairs := nil 
  aryPairs := nil
  for p in parms for x in s repeat
    x is "string" => strPairs := [[p,:gensym '"loc"], :strPairs]
    x is [.,["buffer",.]] => aryPairs := [[p,:gensym '"loc"], :aryPairs]

  -- Build the actual foreign function call.
  -- Note that Clozure CL does not mangle foreign function call for
  -- us, so we're left with more platform dependencies than needed.
  if %hasFeature KEYWORD::DARWIN then
    op' := strconc('"__",op')
  call := [bfColonColon("CCL","EXTERNAL-CALL"), STRING op', :args, rettype]
            where
              args() == [:[x, parm] for x in argtypes for p in parms]
              parm() ==
                    p' := objectAssoc(p, strPairs) => rest p'
                    p' := objectAssoc(p, aryPairs) => rest p'
                    p

  -- If the foreign call returns a C-string, turn it into a Lisp string.
  -- Note that if the C-string was malloc-ed, this will leak storage.
  if t is "string" then
    call := [bfColonColon("CCL","%GET-CSTRING"), call]

  -- If we have array arguments from Boot, bind pointers to initial data.
  for arg in aryPairs repeat
    call := [bfColonColon("CCL", "WITH-POINTER-TO-IVECTOR"),
              [rest arg, first arg], call]

  -- Finally, if we have string arguments from Boot, copy them to
  -- stack-allocated NTBS.
  if strPairs ~= nil then
    call := [bfColonColon("CCL", "WITH-CSTRS"),
              [[rest arg, first arg] for arg in strPairs], call]

  -- Finally, return the definition form
  [["DEFUN", op, parms, call]]

++ Generate an import declaration for `op' as equivalent of the
++ foreign signature `sig'.  Here, `foreign' operationally means that
++ the entity is from the C language world. 
genImportDeclaration(op, sig) ==
  sig isnt ["%Signature", op', m] => coreError '"invalid signature"
  m isnt ["%Mapping", t, s] => coreError '"invalid function type"
  if s ~= nil and symbol? s then s := [s]

  %hasFeature KEYWORD::GCL => genGCLnativeTranslation(op,s,t,op')
  %hasFeature KEYWORD::SBCL => genSBCLnativeTranslation(op,s,t,op')
  %hasFeature KEYWORD::CLISP => genCLISPnativeTranslation(op,s,t,op')
  %hasFeature KEYWORD::ECL => genECLnativeTranslation(op,s,t,op')
  %hasFeature KEYWORD::CLOZURE => genCLOZUREnativeTranslation(op,s,t,op')
  fatalError '"import declaration not implemented for this Lisp"



