-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
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
module ast

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

--% Basic types used in Boot codes.

%Thing <=> true

%Boolean <=> BOOLEAN

%String <=> STRING

%Symbol <=> SYMBOL

%Short <=> FIXNUM

++ Ideally, we would like to say that a List T if either nil or a 
++ cons of a T and List of T. 
%List <=> LIST

%Vector <=> VECTOR

%Sequence <=> SEQUENCE

++ Currently, the Boot processor uses Lisp symbol datatype for names.
++ That causes the BOOTTRAN package to contain more symbols than we would
++ like.  In the future, we want to intern `on demand'.  How that
++ interacts with renaming is to be worked out.
structure %Name == 
  %Name(%Symbol)

structure %Ast ==
  %Command(%String)                     -- includer command
  %Lisp(%String)                        -- )lisp command
  %Module(%Name,%List,%List)            -- module declaration
  %Namespace(%Name)                     -- namespace AxiomCore
  %Import(%String)                      -- import module
  %ImportSignature(%Name,%Signature)    -- import function declaration
  %TypeAlias(%Head, %List)              -- type alias definition
  %Signature(%Name,%Mapping)            -- op: S -> T
  %Mapping(%Ast, %List)                 -- (S1, S2) -> T
  %SuffixDot(%Ast)                      -- x . 
  %Quote(%Ast)                          -- 'x
  %EqualName(%Name)                     -- =x        -- patterns
  %Colon(%Name)                         -- :x
  %QualifiedName(%Name,%Name)           -- m::x
  %DefaultValue(%Name,%Ast)             -- opt. value for function param.
  %Bracket(%Ast)                        -- [x, y]
  %UnboundedSegment(%Ast)               -- 3..
  %BoundedSgement(%Ast,%Ast)            -- 2..4
  %Tuple(%List)                         -- comma-separated expression sequence
  %ColonAppend(%Ast,%Ast)               -- [:y] or [x, :y]
  %Pretend(%Ast,%Ast)                   -- e : t     -- hard coercion
  %Is(%Ast,%Ast)                        -- e is p    -- patterns
  %Isnt(%Ast,%Ast)                      -- e isnt p  -- patterns
  %Reduce(%Ast,%Ast)                    -- +/[...]
  %PrefixExpr(%Name,%Ast)               -- #v
  %Call(%Ast,%Sequence)                 -- f(x, y , z)
  %InfixExpr(%Name,%Ast,%Ast)           -- x + y
  %ConstantDefinition(%Name,%Ast)       -- x == y
  %Definition(%Name,%Ast,%Ast)          -- f x == y
  %Macro(%Name,%List,%Ast)              -- m x ==> y
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
  %Finally(%Ast)                        -- finally closeFile f
  %Try(%Ast,%Sequence)                  -- try x / y catch DivisionByZero
  %Where(%Ast,%Sequence)                -- e where f x == y
  %Structure(%Ast,%Sequence)            -- structure Foo == ...

-- TRUE if we are currently building the syntax tree for an 'is' 
-- expression.
$inDefIS := false


++ returns a `quote' ast for x.
quote x ==
  ["QUOTE",x]

--%

bfGenSymbol: () -> %Symbol 
bfGenSymbol()==
    $GenVarCounter := $GenVarCounter+1
    makeSymbol strconc('"bfVar#",toString $GenVarCounter)

bfColon: %Thing -> %List
bfColon x== 
  ["COLON",x]

bfColonColon: (%Symbol,%Symbol) -> %Symbol
bfColonColon(package, name) == 
  %hasFeature KEYWORD::CLISP and package in '(EXT FFI) =>
    FIND_-SYMBOL(PNAME name,package)
  makeSymbol(PNAME name, package)

bfSymbol: %Thing -> %Thing 
bfSymbol x==
   string? x=> x
   ['QUOTE,x]

 
bfDot: () -> %Symbol
bfDot() == 
  "DOT"
 
bfSuffixDot: %Thing -> %List
bfSuffixDot x ==
  [x,"DOT"]

bfEqual: %Thing -> %List 
bfEqual(name) == 
  ["EQUAL",name]

bfBracket: %Thing -> %Thing 
bfBracket(part) == 
  part
 
bfPile: %List -> %List
bfPile(part) == 
  part
 
bfAppend: %List -> %List
bfAppend x== 
  apply(function append,x)
 
bfColonAppend: (%List,%Thing) -> %List
bfColonAppend(x,y) ==
  x = nil => 
    y is ["BVQUOTE",:a] => ["&REST",["QUOTE",:a]]
    ["&REST",y]
  [first x,:bfColonAppend(rest x,y)]

bfBeginsDollar: %Thing -> %Boolean 
bfBeginsDollar x ==  
  stringChar(PNAME x,0) = char "$"
 
compFluid id == 
  ["FLUID",id]
 
compFluidize x==
  x = nil => nil
  symbol? x and bfBeginsDollar x=>compFluid x
  atom x => x
  x is ["QUOTE",:.] => x
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
    l1 => ['APPEND,a,bfMakeCons l1]
    a
  ['CONS,first l,bfMakeCons rest l]
 
bfFor(bflhs,U,step) ==
  U is ["tails",:.] => bfForTree('ON, bflhs, second U)
  U is ["SEGMENT",:.] => bfSTEP(bflhs,second U,step,third U)
  bfForTree('IN, bflhs, U)
 
bfForTree(OP,lhs,whole)==
         whole :=
	   bfTupleP whole => bfMakeCons rest whole
	   whole
         atom lhs => bfINON [OP,lhs,whole]
         lhs :=
	   bfTupleP lhs => second lhs
	   lhs
         lhs is ["L%T",:.] =>
             G:=second lhs
             [:bfINON [OP,G,whole],:bfSuchthat bfIS(G,third lhs)]
         G:=bfGenSymbol()
         [:bfINON [OP,G,whole],:bfSuchthat bfIS(G,lhs)]
 
 
bfSTEP(id,fst,step,lst)==
  initvar := [id]
  initval := [fst]
  inc :=
    atom step => step
    g1 := bfGenSymbol()
    initvar := [g1,:initvar]
    initval := [step,:initval]
    g1
  final :=
    atom lst => lst
    g2 := bfGenSymbol()
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
 
 
bfINON x==
  [op,id,whole] := x
  op = "ON" => bfON(id,whole)
  bfIN(id,whole)
 
bfIN(x,E)==
  g := bfGenSymbol()
  [[[g,x],[E,nil],[['SETQ,g,['CDR, g]]],[],
      [['OR,['ATOM,g],['PROGN,['SETQ,x,['CAR,g]] ,'NIL]]],[]]]
 
bfON(x,E)==
  [[[x],[E],[['SETQ,x,['CDR, x]]],[],
      [['ATOM,x]],[]]]
 
bfSuchthat p== [[[],[],[],[p],[],[]]]
 
bfWhile p== [[[],[],[],[],[bfNOT p],[]]]
 
bfUntil p==
     g:=bfGenSymbol()
     [[[g],[nil],[['SETQ,g,p]],[],[g],[]]]
 
bfIterators x==["ITERATORS",:x]
 
bfCross x== ["CROSS",:x]
 
bfLp(iters,body)==
  iters is ["ITERATORS",:.] => bfLp1(rest iters,body)
  bfLpCross(rest iters,body)
 
bfLpCross(iters,body)==
  rest iters = nil => bfLp(first iters,body)
  bfLp(first iters,bfLpCross(rest iters,body))
 
bfSep(iters)==
  iters = nil => [[],[],[],[],[],[]]
  f := first iters
  r := bfSep rest iters
  [append(i,j) for i in f for j in r]
 
bfReduce(op,y)==
  a :=
    op is ["QUOTE",:.] => second op
    op
  op := bfReName a
  init := a has SHOETHETA or op has SHOETHETA
  g := bfGenSymbol()
  g1 := bfGenSymbol()
  body := ['SETQ,g,[op,g,g1]]
  init = nil =>
    g2 := bfGenSymbol()
    init := ['CAR,g2]
    ny := ['CDR,g2]
    it := ["ITERATORS",:[[[[g],[init],[],[],[],[g]]],bfIN(g1,ny)]]
    bfMKPROGN [['L%T,g2,y],bfLp(it,body)]
  init := first init
  it := ["ITERATORS",:[[[[g],[init],[],[],[],[g]]],bfIN(g1,y)]]
  bfLp(it,body)
 
bfReduceCollect(op,y)==
  y is ["COLLECT",:.] => 
    body := second y
    itl := third y
    a :=
      op is ["QUOTE",:.] => second op
      op
    op := bfReName a
    init := a has SHOETHETA or op has SHOETHETA
    bfOpReduce(op,init,body,itl)
  seq :=
    y = nil => bfTuple nil
    second y
  bfReduce(op,bfTupleConstruct seq)
 
-- delayed collect
 
bfDCollect(y,itl) == 
  ["COLLECT",y,itl]
 
bfDTuple x == 
  ["DTUPLE",x]
 
bfCollect(y,itl) ==
  y is ["COLON",a] => bf0APPEND(a,itl)
  y is ["TUPLE",:.] =>
    newBody := bfConstruct y
    bf0APPEND(newBody,itl)
  bf0COLLECT(y,itl)
 
bf0COLLECT(y,itl) == 
  bfListReduce('CONS,y,itl)
 
 
bf0APPEND(y,itl)==
  g := bfGenSymbol()
  body := ['SETQ,g,['APPEND,['REVERSE,y],g]]
  extrait := [[[g],[nil],[],[],[],[['NREVERSE,g]]]]
  bfLp2(extrait,itl,body)
 
bfListReduce(op,y,itl)==
  g := bfGenSymbol()
  body := ['SETQ,g,[op,y,g]]
  extrait := [[[g],[nil],[],[],[],[['NREVERSE,g]]]]
  bfLp2(extrait,itl,body)
 
bfLp1(iters,body)==
  [vars,inits,sucs,filters,exits,value] := bfSep bfAppend iters
  nbody :=
    filters = nil => body
    bfAND [:filters,body]
  value :=
    value = nil => "NIL"
    first value
  exits := ["COND",[bfOR exits,["RETURN",value]],['T,nbody]]
  loop := ["LOOP",exits,:sucs]
  if vars then loop := 
    ["LET",[[v, i] for v in vars for i in inits], loop]
  loop
 
bfLp2(extrait,itl,body)==
  itl is ["ITERATORS",:.] => bfLp1([extrait,:rest itl],body)
  iters := rest itl
  bfLpCross([["ITERATORS",extrait,:CDAR iters],:rest iters],body)
 
bfOpReduce(op,init,y,itl)==
  g := bfGenSymbol()
  body:=
    op = "AND" =>
      bfMKPROGN [["SETQ",g,y], ['COND, [['NOT,g],['RETURN,'NIL]]]]
    op = "OR" => bfMKPROGN [["SETQ",g,y], ['COND, [g,['RETURN,g]]]]
    ['SETQ,g,[op,g,y]]
  init = nil =>
    g1 := bfGenSymbol()
    init := ['CAR,g1]
    y := ['CDR,g1]          -- ??? bogus self-assignment/initialization
    extrait := [[[g],[init],[],[],[],[g]]]
    bfMKPROGN [['L%T,g1,y],bfLp2(extrait,itl,body)]
  init := first init
  extrait := [[[g],[init],[],[],[],[g]]]
  bfLp2(extrait,itl,body)
 
bfLoop1 body == 
  bfLp (bfIterators nil,body)
 
bfSegment1(lo) ==
  ["SEGMENT",lo,nil]
 
bfSegment2(lo,hi) ==
  ["SEGMENT",lo,hi]
 
bfForInBy(variable,collection,step)==
  bfFor(variable,collection,step)
 
bfForin(lhs,U)==
  bfFor(lhs,U,1)
 
bfLocal(a,b)==
  b = "FLUID" =>  compFluid a
  b = "fluid" =>  compFluid a
  b = "local" =>  compFluid a
  a
 
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
  atom e=>bfSUBLIS1(p,e)
  e is ["QUOTE",:.] => e
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
 
defSheepAndGoats(x)==
  case x of 
    %Definition(op,args,body) =>
      argl :=
        bfTupleP args => rest args
	[args]
      argl = nil =>
	opassoc := [[op,:body]]
	[opassoc,[],[]]
      op1 := makeSymbol strconc(PNAME $op,'",",PNAME op)
      opassoc := [[op,:op1]]
      defstack := [[op1,args,body]]
      [opassoc,defstack,[]]
    %Pile defs => defSheepAndGoatsList defs
    otherwise => [[],[],[x]]
 
defSheepAndGoatsList(x)==
  x = nil => [[],[],[]]
  [opassoc,defs,nondefs]    := defSheepAndGoats first x
  [opassoc1,defs1,nondefs1] := defSheepAndGoatsList rest x
  [append(opassoc,opassoc1),append(defs,defs1), append(nondefs,nondefs1)]

--% LET
 
bfLetForm(lhs,rhs) ==   
  ['L%T,lhs,rhs]
 
bfLET1(lhs,rhs) ==
  symbol? lhs        => bfLetForm(lhs,rhs)
  lhs is ['FLUID,.] => bfLetForm(lhs,rhs)
  symbol? rhs and not bfCONTAINED(rhs,lhs) =>
    rhs1 := bfLET2(lhs,rhs)
    rhs1 is ["L%T",:.]   => bfMKPROGN [rhs1,rhs]
    rhs1 is ["PROGN",:.] => [:rhs1,:[rhs]]
    if symbol? first rhs1 then rhs1 := [rhs1,:nil]
    bfMKPROGN [:rhs1,rhs]
  rhs is ["L%T",:.] and symbol?(name := second rhs) =>
    -- handle things like [a] := x := foo
    l1 := bfLET1(name,third rhs)
    l2 := bfLET1(lhs,name)
    l2 is ["PROGN",:.] => bfMKPROGN [l1,:rest l2]
    if symbol? first l2 then l2 := [l2,:nil]
    bfMKPROGN [l1,:l2,name]
  g := makeSymbol strconc('"LETTMP#",toString $letGenVarCounter)
  $letGenVarCounter := $letGenVarCounter + 1
  rhs1 := ['L%T,g,rhs]
  let1 := bfLET1(lhs,g)
  let1 is ["PROGN",:.] => bfMKPROGN [rhs1,:rest let1]
  if symbol? first let1 then let1 := [let1,:nil]
  bfMKPROGN [rhs1,:let1,g]
 
bfCONTAINED(x,y)==
    sameObject?(x,y) => true
    atom y=> false
    bfCONTAINED(x,first y) or bfCONTAINED(x,rest y)
 
bfLET2(lhs,rhs) ==
  lhs = nil => nil
  symbol? lhs => bfLetForm(lhs,rhs)
  lhs is ['FLUID,.] => bfLetForm(lhs,rhs)
  lhs is ['L%T,a,b] =>
    a := bfLET2(a,rhs)
    (b := bfLET2(b,rhs)) = nil => a
    atom b => [a,b]
    cons? first b => [a,:b]
    [a,b]
  lhs is ['CONS,var1,var2] =>
    var1 = "DOT" or var1 is ["QUOTE",:.] =>
      bfLET2(var2,addCARorCDR('CDR,rhs))
    l1 := bfLET2(var1,addCARorCDR('CAR,rhs))
    var2 = nil or var2 = "DOT" =>l1
    if cons? l1 and atom first l1 then l1 := [l1,:nil]
    symbol? var2 =>
      [:l1,bfLetForm(var2,addCARorCDR('CDR,rhs))]
    l2 := bfLET2(var2,addCARorCDR('CDR,rhs))
    if cons? l2 and atom first l2 then l2 := [l2,:nil]
    [:l1,:l2]
  lhs is ['APPEND,var1,var2] =>
    patrev := bfISReverse(var2,var1)
    rev := ['REVERSE,rhs]
    g := makeSymbol strconc('"LETTMP#", toString $letGenVarCounter)
    $letGenVarCounter := $letGenVarCounter + 1
    l2 := bfLET2(patrev,g)
    if cons? l2 and atom first l2 then l2 := [l2,:nil]
    var1 = "DOT" => [['L%T,g,rev],:l2]
    last l2 is ['L%T, =var1, val1] =>
      [['L%T,g,rev],:REVERSE rest REVERSE l2,
       bfLetForm(var1,['NREVERSE,val1])]
    [['L%T,g,rev],:l2,bfLetForm(var1,['NREVERSE,var1])]
  lhs is ["EQUAL",var1] => ['COND,[bfQ(var1,rhs),var1]]
  -- The original expression may be one that involves literals as 
  -- sub-patterns, e.g.
  --      ['SEQ, :l, ['exit, 1, x]] := item
  -- We continue the processing as if that expression had been written
  --      item is ['SEQ, :l, ['exit, 1, x]]
  -- and generate appropriate codes.
  --                  -- gdr/2007-04-02.
  isPred :=
    $inDefIS => bfIS1(rhs,lhs)
    bfIS(rhs,lhs)
  ['COND,[isPred,rhs]]
 
 
bfLET(lhs,rhs) ==
  $letGenVarCounter : local := 1
  bfLET1(lhs,rhs)
 
addCARorCDR(acc,expr) ==
  atom expr => [acc,expr]
  acc = 'CAR and expr is ["REVERSE",:.] =>
      ["CAR",["LAST",:rest expr]]
 --   ['last,:rest expr]
  funs := '(CAR CDR CAAR CDAR CADR CDDR CAAAR CADAR CAADR CADDR
            CDAAR CDDAR CDADR CDDDR)
  p := bfPosition(first expr,funs)
  p = -1 => [acc,expr]
  funsA := '(CAAR CADR CAAAR CADAR CAADR CADDR CAAAAR CAADAR CAAADR
             CAADDR CADAAR CADDAR CADADR CADDDR)
  funsR := '(CDAR CDDR CDAAR CDDAR CDADR CDDDR CDAAAR CDADAR CDAADR
             CDADDR CDDAAR CDDDAR CDDADR CDDDDR)
  acc = 'CAR => [funsA.p,:rest expr]
  [funsR.p,:rest expr]
 
bfPosition(x,l) ==  bfPosn(x,l,0)
bfPosn(x,l,n) ==
  l = nil => -1
  x = first l => n
  bfPosn(x,rest l,n+1)
 
--% IS
 
bfISApplication(op,left,right)==
  op = "IS"      => bfIS(left,right)
  op = "ISNT"    => bfNOT bfIS(left,right)
  [op ,left,right]
 
bfIS(left,right)==
    $isGenVarCounter:local :=1
    $inDefIS :local :=true
    bfIS1(left,right)
 
bfISReverse(x,a) ==
  x is ['CONS,:.] =>
    third x = nil => ['CONS,second x, a]
    y := bfISReverse(third x, nil)
    y.rest.rest.first := ['CONS,second x,a]
    y
  bpSpecificErrorHere '"Error in bfISReverse"
  bpTrap()
 
bfIS1(lhs,rhs) ==
  rhs = nil => ['NULL,lhs]
  bfString? rhs => bfAND [['STRINGP,lhs],["STRING=",lhs,rhs]]
  bfChar? rhs or integer? rhs => ['EQL,lhs,rhs]
  atom rhs => ['PROGN,bfLetForm(rhs,lhs),'T]
  rhs is ['QUOTE,a] =>
    symbol? a => ['EQ,lhs,rhs]
    string? a => bfAND [['STRINGP,lhs],["STRING=",lhs,a]]
    ["EQUAL",lhs,rhs]
  rhs is ['L%T,c,d] =>
    l := bfLET(c,lhs)
    bfAND [bfIS1(lhs,d),bfMKPROGN [l,'T]]
  rhs is ["EQUAL",a] => bfQ(lhs,a)
  cons? lhs =>
    g := makeSymbol strconc('"ISTMP#",toString $isGenVarCounter)
    $isGenVarCounter := $isGenVarCounter + 1
    bfMKPROGN [['L%T,g,lhs],bfIS1(g,rhs)]
  rhs is ['CONS,a,b] =>
    a = "DOT" =>
      b = nil => bfAND [['CONSP,lhs],['NULL,['CDR,lhs]]]
      bfAND [['CONSP,lhs],bfIS1(['CDR,lhs],b)]
    b = nil =>
      bfAND [['CONSP,lhs],['NULL,['CDR,lhs]],bfIS1(['CAR,lhs],a)]
    b = "DOT" => bfAND [['CONSP,lhs],bfIS1(['CAR,lhs],a)]
    a1 := bfIS1(['CAR,lhs],a)
    b1 := bfIS1(['CDR,lhs],b)
    a1 is ['PROGN,c,'T] and b1 is ['PROGN,:cls] =>
      bfAND [['CONSP,lhs],bfMKPROGN [c,:cls]]
    bfAND [['CONSP,lhs],a1,b1]
  rhs is ['APPEND,a,b] =>
    patrev := bfISReverse(b,a)
    g := makeSymbol strconc('"ISTMP#",toString $isGenVarCounter)
    $isGenVarCounter := $isGenVarCounter + 1
    rev := bfAND [['CONSP,lhs],['PROGN,['L%T,g,['REVERSE,lhs]],'T]]
    l2 := bfIS1(g,patrev)
    if cons? l2 and atom first l2 then l2 := [l2,:nil]
    a = "DOT" => bfAND [rev,:l2]
    bfAND [rev,:l2,['PROGN,bfLetForm(a,['NREVERSE,a]),'T]]
  bpSpecificErrorHere '"bad IS code is generated"
  bpTrap()


bfHas(expr,prop) ==
  symbol? prop => ["GET",expr,["QUOTE",prop]]
  bpSpecificErrorHere('"expected identifier as property name")
  
bfApplication(bfop, bfarg) ==
  bfTupleP bfarg => [bfop,:rest bfarg]
  [bfop,bfarg]
 
-- returns the meaning of x in the appropriate Boot dialect.
bfReName x==
  a := x has SHOERENAME => first a
  x

sequence?(x,pred) ==
  x is ["QUOTE",seq] and cons? seq and
    "and"/[apply(pred,y,nil) for y in seq]

++ Generate code for a membership test `x in seq' where `seq'
++ is a sequence (e.g. a list)
bfMember(var,seq) ==
  integer? var or sequence?(seq,function integer?) =>
    seq is ["QUOTE",[x]] => ["EQL",var,x]
    ["scalarMember?",var,seq]
  defQuoteId var or sequence?(seq,function symbol?) =>
    seq is ["QUOTE",[x]] => ["EQ",var,["QUOTE",x]]
    ["symbolMember?",var,seq]
  bfChar? var or sequence?(seq,function char?) =>
    seq is ["QUOTE",[x]] => ["CHAR=",var,x]
    ["charMember?",var,seq]
  bfString? var or sequence?(seq,function string?) =>
    seq is ["QUOTE",[x]] => ["STRING=",var,x]
    ["stringMember?",var,seq]
  ["MEMBER",var,seq]
  
bfInfApplication(op,left,right)==
   op = "EQUAL" => bfQ(left,right)
   op = "/="    => bfNOT bfQ(left,right)
   op = ">"     => bfLessp(right,left)
   op = "<"     => bfLessp(left,right)
   op = "<="    => bfNOT bfLessp(right,left)
   op = ">="    => bfNOT bfLessp(left,right)
   op = "OR"    => bfOR [left,right]
   op = "AND"   => bfAND [left,right]
   op = "IN"    => bfMember(left,right)
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
  x is ["QUOTE",:.] and symbol? second x
 
bfChar? x ==
  char? x or cons? x and x.op in '(char CODE_-CHAR SCHAR)
 
bfSmintable x==
  integer? x or cons? x and
    x.op in '(SIZE LENGTH CHAR_-CODE MAXINDEX _+ _-)

bfString? x ==
  string? x
    or cons? x and first x in '(STRING SYMBOL_-NAME subString)

bfQ(l,r)==
  bfChar? l or bfChar? r => ["CHAR=",l,r]
  bfSmintable l or bfSmintable r => ["EQL",l,r]
  defQuoteId l or defQuoteId r => ["EQ",l,r]
  l = nil => ["NULL",r]
  r = nil => ["NULL",l]
  l = true or r = true => ["EQ",l,r]
  bfString? l or bfString? r => ["STRING=",l,r]
  ["EQUAL",l,r]
 
bfLessp(l,r)==
  l = 0 => ["PLUSP",r]
  r = 0 => ["MINUSP", l]
  bfChar? l or bfChar? r => ["CHAR<",l,r]
  bfString? l or bfString? r => ["STRING<",l,r]
  ["<",l,r]

bfLambda(vars,body) ==
  -- FIXME: Check that we have only names in vars.
  vars := 
    bfTupleP vars => rest vars
    [vars]
  ["LAMBDA",vars,body]
 
bfMDef (op,args,body) ==
  argl :=
    bfTupleP args => rest args
    [args]
  [gargl,sgargl,nargl,largl]:=bfGargl argl
  sb:=[[i,:j] for i in nargl for j in sgargl]
  body:= SUBLIS(sb,body)
  sb2 := [["CONS",["QUOTE",i],j] for i in sgargl for j in largl]
  body := ["SUBLIS",["LIST",:sb2],["QUOTE",body]]
  lamex:= ["MLAMBDA",gargl,body]
  def:= [op,lamex]
  [shoeComp def,:[:shoeComps bfDef1 d for d in $wheredefs]]
 
bfGargl argl==
  argl = nil => [[],[],[],[]]
  [a,b,c,d] := bfGargl rest argl
  first argl="&REST" =>
    [[first argl,:b],b,c,
       [["CONS",["QUOTE","LIST"],first d],:rest d]]
  f := bfGenSymbol()
  [[f,:a],[f,:b],[first argl,:c],[f,:d]]
 
bfDef1 [op,args,body] ==
  argl :=
    bfTupleP args => rest args
    [args]
  [quotes,control,arglp,body]:=bfInsertLet (argl,body)
  quotes => shoeLAM(op,arglp,control,body)
  [[op,["LAMBDA",arglp,body]]]
 
shoeLAM (op,args,control,body)==
  margs :=bfGenSymbol()
  innerfunc:= makeSymbol strconc(PNAME op,",LAM")
  [[innerfunc,["LAMBDA",args,body]],
     [op,["MLAMBDA",["&REST",margs],["CONS",["QUOTE", innerfunc],
                    ["WRAP",margs, ["QUOTE", control]]]]]]
 
bfDef(op,args,body) ==
 $bfClamming =>
   [.,op1,arg1,:body1] := shoeComp first bfDef1 [op,args,body]
   bfCompHash(op1,arg1,body1)
 bfTuple
  [:shoeComps bfDef1 d for d in  [[op,args,body],:$wheredefs]]
 
shoeComps  x==
  [shoeComp def for def in x]

shoeComp x==
  a:=shoeCompTran second x
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
  p2=nil and not atom p1 => p1
  p1 is ["&OPTIONAL",:.] =>
    p2 isnt ["&OPTIONAL",:.] => bpSpecificErrorHere '"default value required"
    [first p1,:rest p1,:rest p2]
  p2 is ["&OPTIONAL",:.] =>   [p1,first p2,:rest p2]
  [p1,:p2]
 
bfInsertLet(x,body)==
  x = nil => [false,nil,x,body]
  x is ["&REST",a] =>
    a is ["QUOTE",b] => [true,"QUOTE",["&REST",b],body]
    [false,nil,x,body]
  [b,norq,name1,body1] :=  bfInsertLet1 (first x,body)
  [b1,norq1,name2,body2] :=  bfInsertLet (rest x,body1)
  [b or b1,[norq,:norq1],bfParameterList(name1,name2),body2]
 
bfInsertLet1(y,body)==
  y is ["L%T",l,r] => [false,nil,l,bfMKPROGN [bfLET(r,l),body]]
  symbol? y => [false,nil,y,body]
  y is ["BVQUOTE",b] => [true,"QUOTE",b,body]
  g:=bfGenSymbol()
  atom y => [false,nil,g,body]
  case y of
    %DefaultValue(p,v) => [false,nil,["&OPTIONAL",[p,v]],body]
    otherwise => [false,nil,g,bfMKPROGN [bfLET(compFluidize y,g),body]]
 
shoeCompTran x==
  lamtype:=first x
  args   :=second x
  body   :=CDDR x
  $fluidVars:local:=nil
  $locVars:local:=nil
  $dollarVars:local:=nil
  shoeCompTran1 body
  $locVars:=setDifference(setDifference($locVars,
				 $fluidVars),shoeATOMs args)
  body:=
    lvars:=append($fluidVars,$locVars)
    $fluidVars:=UNION($fluidVars,$dollarVars)
    body' := body
    if $typings then body' := [["DECLARE",:$typings],:body']
    if $fluidVars then
      fvars:=["DECLARE",["SPECIAL",:$fluidVars]]
      body' := [fvars,:body']
    lvars or needsPROG body => shoePROG(lvars,body')
    body'
  fl := shoeFluids args
  body :=
    fl =>
      fvs:=["DECLARE",["SPECIAL",:fl]]
      [fvs,:body]
    body
  [lamtype,args, :body]

needsPROG body ==
  atom body => false
  [op,:args] := body
  op in '(RETURN RETURN_-FROM) => true
  op in '(LET PROG LOOP BLOCK DECLARE LAMBDA) => false
  or/[needsPROG t for t in body] => true
  false

shoePROG(v,b)==
  b = nil => [["PROG", v]]
  [:blist,blast] := b
  [["PROG",v,:blist,["RETURN", blast]]]

shoeFluids x==
  x = nil => nil
  symbol? x and bfBeginsDollar x => [x]
  atom x => nil
  x is ["QUOTE",:.] => nil
  [:shoeFluids first x,:shoeFluids rest x]

shoeATOMs x ==
  x = nil => nil
  atom x => [x]
  [:shoeATOMs first x,:shoeATOMs rest x]

++ Return true if `x' is an identifier name that designates a
++ dynamic (e.g. Lisp special) variable.  
isDynamicVariable x ==
  symbol? x and bfBeginsDollar x =>
    MEMQ(x,$constantIdentifiers) => false
    CONSTANTP x => false
    BOUNDP x or $activeNamespace = nil => true
    y := FIND_-SYMBOL(symbolName x,$activeNamespace) => not CONSTANTP y
    true
  false
 
shoeCompTran1 x==
  atom x=>
    isDynamicVariable x =>
      $dollarVars:=
	 MEMQ(x,$dollarVars)=>$dollarVars
	 [x,:$dollarVars]
    nil
  U:=first x
  U = "QUOTE" => nil
  x is ["L%T",l,r] =>
    x.first := "SETQ"
    shoeCompTran1 r
    symbol? l =>
      not bfBeginsDollar l=>
	$locVars:=
	   MEMQ(l,$locVars)=>$locVars
	   [l,:$locVars]
      $dollarVars:=
	 MEMQ(l,$dollarVars)=>$dollarVars
	 [l,:$dollarVars]
    l is ["FLUID",:.] =>
      $fluidVars:=
	 MEMQ(second l,$fluidVars)=>$fluidVars
	 [second l,:$fluidVars]
      x.rest.first := second l
  U = "%Leave" => x.first := "RETURN"
  U in '(PROG LAMBDA) =>
    newbindings:=nil
    for y in second x repeat
      not MEMQ(y,$locVars)=>
	$locVars := [y,:$locVars]
	newbindings := [y,:newbindings]
    res := shoeCompTran1 CDDR x
    $locVars := [y for y in $locVars | not MEMQ(y,newbindings)]
  shoeCompTran1 first x
  shoeCompTran1 rest x
 
bfTagged(a,b)==
  $op = nil => %Signature(a,b)        -- surely a toplevel decl
  symbol? a =>
    b = "FLUID" =>  bfLET(compFluid a,nil)
    b = "fluid" =>  bfLET(compFluid a,nil)
    b = "local" =>  bfLET(compFluid a,nil)
    $typings := [["TYPE",b,a],:$typings]
    a
  ["THE",b,a]
 
bfAssign(l,r)==
  bfTupleP l => bfSetelt(second l,CDDR l ,r)
  l is ["%Place",:l'] => ["SETF",l',r]
  bfLET(l,r)
 
bfSetelt(e,l,r)==
  rest l = nil => defSETELT(e,first l,r)
  bfSetelt(bfElt(e,first l),rest l,r)
 
bfElt(expr,sel)==
  y:=symbol? sel and sel has SHOESELFUNCTION
  y =>
    integer? y => ["ELT",expr,y]
    [y,expr]
  ["ELT",expr,sel]
 
defSETELT(var,sel,expr)==
  y := symbol? sel and sel has SHOESELFUNCTION
  y =>
    integer? y => ["SETF",["ELT",var,y],expr]
    y = "CAR" => ["RPLACA",var,expr]
    y = "CDR" => ["RPLACD",var,expr]
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
 
bfMKPROGN l==
  a := [:bfFlattenSeq c for c in tails l]
  a = nil => nil
  rest a = nil => first a
  ["PROGN",:a]
 
bfFlattenSeq x ==
  x = nil => nil
  f := first x
  atom f =>
    rest x => nil
    [f]
  f is ["PROGN",:.] =>
    rest x => [i for i in rest f| not atom i]
    rest f
  [f]
 
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
 
bfWhere (context,expr)==
  [opassoc,defs,nondefs] := defSheepAndGoats context
  a:=[[first d,second d,bfSUBLIS(opassoc,third d)]
               for d in defs]
  $wheredefs:=append(a,$wheredefs)
  bfMKPROGN bfSUBLIS(opassoc,nconc(nondefs,[expr]))
 
--shoeReadLispString(s,n)==
--    n>= # s => nil
--    [exp,ind]:=shoeReadLisp(s,n)
--    exp = nil => nil
--    [exp,:shoeReadLispString(s,ind)]
 
bfCompHash(op,argl,body) ==
  auxfn:= makeSymbol strconc(PNAME op,'";")
  computeFunction:= ["DEFUN",auxfn,argl,:body]
  bfTuple [computeFunction,:bfMain(auxfn,op)]
 
shoeCompileTimeEvaluation x ==
  ["EVAL-WHEN", [KEYWORD::COMPILE_-TOPLEVEL], x]

shoeEVALANDFILEACTQ x==  
  ["EVAL-WHEN", [KEYWORD::EXECUTE, KEYWORD::LOAD_-TOPLEVEL], x]
 
bfMain(auxfn,op)==
  g1:= bfGenSymbol()
  arg:=["&REST",g1]
  computeValue := ['APPLY,["FUNCTION",auxfn],g1]
  cacheName:= makeSymbol strconc(PNAME op,'";AL")
  g2:= bfGenSymbol()
  getCode:=   ['GETHASH,g1,cacheName]
  secondPredPair:= [['SETQ,g2,getCode],g2]
  putCode:=   ['SETF ,getCode,computeValue]
  thirdPredPair:= ['T,putCode]
  codeBody:= ['PROG,[g2],
               ['RETURN,['COND,secondPredPair,thirdPredPair]]]
  mainFunction:= ["DEFUN",op,arg,codeBody]
 
  cacheType:=     'hash_-table
  cacheResetCode:=   ['SETQ,cacheName,['MAKE_-HASHTABLE,
                        ["QUOTE","UEQUAL"]]]
  cacheCountCode:= ['hashCount,cacheName]
  cacheVector:=
      [op,cacheName,cacheType,cacheResetCode,cacheCountCode]
  defCode := ["DEFPARAMETER",cacheName,
                ['MAKE_-HASHTABLE,["QUOTE","UEQUAL"]]]
  [defCode,mainFunction,
    shoeEVALANDFILEACTQ
      ["SETF",["GET",
           ["QUOTE", op],["QUOTE",'cacheInfo]],["QUOTE", cacheVector]]]


bfNameOnly: %Thing -> %List 
bfNameOnly x==
  x="t" => ["T"]
  [x]

bfNameArgs: (%Thing,%Thing) -> %List 
bfNameArgs (x,y)==
  y :=
    y is ["TUPLE",:.] => rest y
    [y]
  [x,:y]
 
bfCreateDef: %Thing -> %List
bfCreateDef x==
  x is [f] => ["DEFCONSTANT",f,["LIST",["QUOTE",f]]]
  a := [bfGenSymbol() for i in rest x]
  ["DEFUN",first x,a,["CONS",["QUOTE",first x],["LIST",:a]]]

bfCaseItem: (%Thing,%Thing) -> %List 
bfCaseItem(x,y) ==
  [x,y]

bfCase: (%Thing,%Thing) -> %List
bfCase(x,y)==
  -- Introduce a temporary to hold the value of the scrutinee.
  -- To minimize the number of GENSYMS and assignments, we want
  -- to do this only when the scrutinee is not reduced yet.
  g := 
    atom x => x 
    bfGenSymbol()
  body := ["CASE",["CAR", g], :bfCaseItems(g,y)]
  sameObject?(g,x) => body
  ["LET",[[g,x]],body]

bfCaseItems: (%Thing,%List) -> %List 
bfCaseItems(g,x) ==  
  [bfCI(g,i,j) for [i,j] in x]

bfCI: (%Thing,%Thing,%Thing) -> %List 
bfCI(g,x,y)==
  a := rest x
  a = nil => [first x,y]
  b := [[i,bfCARCDR(j,g)] for i in a for j in 1.. | i ~= "DOT"]
  b = nil => [first x,y]
  [first x,["LET",b,y]]

bfCARCDR: (%Short,%Thing) -> %List 
bfCARCDR(n,g) ==
  [makeSymbol strconc('"CA",bfDs n,'"R"),g]

bfDs: %Short -> %String 
bfDs n == 
  n = 0 => '""
  strconc('"D",bfDs(n-1))

bfHandlers(n,e,hs) == main(n,e,hs,nil) where
  main(n,e,hs,xs) ==
    hs = nil =>
      ["COND",
        :nreverse
          [[true,["THROW",KEYWORD::OPEN_-AXIOM_-CATCH_-POINT,n]],:xs]]
    hs is [['%Catch,['%Signature,v,t],s],:hs'] =>
      t := 
        symbol? t => ["QUOTE",[t]] -- instantiate niladic type ctor
        ["QUOTE",t]
      main(n,e,hs',[[bfQ(["CAR",e],t),["LET",[[v,["CDR",e]]],s]],:xs])
    bpTrap()

codeForCatchHandlers(g,e,cs) ==
  ehTest := ['AND,['CONSP,g],
              bfQ(['CAR,g],KEYWORD::OPEN_-AXIOM_-CATCH_-POINT)]
  ["LET",[[g,["CATCH",KEYWORD::OPEN_-AXIOM_-CATCH_-POINT,e]]],
    ["COND",[ehTest,bfHandlers(g,["CDR",g],cs)],[true,g]]]

++ Generate code for try-catch expressions.
bfTry: (%Thing,%List) -> %Thing
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
  if e is ["%Pretend",:.] then
    t := third e
    x := second e
  else
    t := "SystemException"
    x := e
  t :=
    symbol? t => ["QUOTE",[t]]
    ["QOUTE",t]
  ["THROW",KEYWORD::OPEN_-AXIOM_-CATCH_-POINT,
    ["CONS",KEYWORD::OPEN_-AXIOM_-CATCH_-POINT,["CONS",t,x]]]

--% Type alias definition

backquote(form,params) ==
  params = nil => quote  form
  atom form =>
    form in params => form
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
  t in $NativeSimpleReturnTypes

coreSymbol: %Symbol -> %Symbol
coreSymbol s ==
  makeSymbol(PNAME s, "AxiomCore")

bootSymbol: %Symbol -> %Symbol
bootSymbol s ==
  makeSymbol PNAME s


unknownNativeTypeError t ==
  fatalError strconc('"unsupported native type: ", PNAME t)


nativeType t ==
  t = nil => t
  atom t =>
    t' := rest ASSOC(coreSymbol t,$NativeTypeTable) => 
      t' := 
	%hasFeature KEYWORD::SBCL => bfColonColon("SB-ALIEN", t')
	%hasFeature KEYWORD::CLISP => bfColonColon("FFI",t')
	t'
      -- ??? decree we have not discovered Unicode yet.
      t = "string" and %hasFeature KEYWORD::SBCL =>
	[t',KEYWORD::EXTERNAL_-FORMAT,KEYWORD::ASCII,
	   KEYWORD::ELEMENT_-TYPE, "BASE-CHAR"]
      t'
    t in '(byte uint8) =>
      %hasFeature KEYWORD::SBCL => [bfColonColon("SB-ALIEN","UNSIGNED"),8]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","UINT8")
      %hasFeature KEYWORD::ECL or %hasFeature KEYWORD::CLOZURE =>
        KEYWORD::UNSIGNED_-BYTE
      nativeType "char"           -- approximate by 'char' for GCL
    t = "int16" =>
      %hasFeature KEYWORD::SBCL => [bfColonColon("SB-ALIEN","SIGNED"),16]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","INT16")
      %hasFeature KEYWORD::ECL and %hasFeature KEYWORD::UINT16_-T =>
         KEYWORD::INT16_-T
      %hasFeature KEYWORD::CLOZURE => KEYWORD::SIGNED_-HALFWORD
      unknownNativeTypeError t
    t = "uint16" =>
      %hasFeature KEYWORD::SBCL => [bfColonColon("SB-ALIEN","UNSIGNED"),16]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","UINT16")
      %hasFeature KEYWORD::ECL and %hasFeature KEYWORD::UINT16_-T =>
         KEYWORD::UINT16_-T
      %hasFeature KEYWORD::CLOZURE => KEYWORD::UNSIGNED_-HALFWORD
      unknownNativeTypeError t
    t = "int32" =>
      %hasFeature KEYWORD::SBCL => [bfColonColon("SB-ALIEN","SIGNED"),32]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","INT32")
      %hasFeature KEYWORD::ECL and %hasFeature KEYWORD::UINT32_-T =>
         KEYWORD::INT32_-T
      %hasFeature KEYWORD::CLOZURE => KEYWORD::SIGNED_-FULLWORD
      unknownNativeTypeError t
    t = "uint32" =>
      %hasFeature KEYWORD::SBCL => [bfColonColon("SB-ALIEN","UNSIGNED"),32]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","INT32")
      %hasFeature KEYWORD::ECL and %hasFeature KEYWORD::UINT32_-T =>
         KEYWORD::UINT32_-T
      %hasFeature KEYWORD::CLOZURE => KEYWORD::UNSIGNED_-FULLWORD
      unknownNativeTypeError t
    t = "int64" =>
      %hasFeature KEYWORD::SBCL => [bfColonColon("SB-ALIEN","SIGNED"),64]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","INT64")
      %hasFeature KEYWORD::ECL and %hasFeature KEYWORD::UINT64_-T =>
         KEYWORD::INT64_-T
      %hasFeature KEYWORD::CLOZURE => KEYWORD::SIGNED_-DOUBLEWORD
      unknownNativeTypeError t
    t = "uint64" =>
      %hasFeature KEYWORD::SBCL => [bfColonColon("SB-ALIEN","UNSIGNED"),64]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","UINT64")
      %hasFeature KEYWORD::ECL and %hasFeature KEYWORD::UINT64_-T =>
         KEYWORD::UINT64_-T
      %hasFeature KEYWORD::CLOZURE => KEYWORD::UNSIGNED_-DOUBLEWORD
      unknownNativeTypeError t
    t = "float32" => nativeType "float"
    t = "float64" => nativeType "double"
    t = "pointer" =>
      %hasFeature KEYWORD::GCL => "fixnum"
      %hasFeature KEYWORD::ECL => KEYWORD::POINTER_-VOID
      %hasFeature KEYWORD::SBCL => ["*",bfColonColon("SB-ALIEN","VOID")]
      %hasFeature KEYWORD::CLISP => bfColonColon("FFI","C-POINTER")
      %hasFeature KEYWORD::CLOZURE => KEYWORD::ADDRESS
      unknownNativeTypeError t
    unknownNativeTypeError t
  -- composite, reference type.
  first t = "buffer" =>
    %hasFeature KEYWORD::GCL => "OBJECT"
    %hasFeature KEYWORD::ECL => KEYWORD::OBJECT
    %hasFeature KEYWORD::SBCL => ["*",nativeType second t]
    %hasFeature KEYWORD::CLISP => bfColonColon("FFI","C-POINTER")
    %hasFeature KEYWORD::CLOZURE => [KEYWORD::_*, nativeType second t]
    unknownNativeTypeError t
  first t = "pointer" =>
    -- we don't bother looking at what the pointer points to.
    nativeType "pointer"
  unknownNativeTypeError t

++ Check that `t' is a valid return type for a native function, and
++ returns its translation
nativeReturnType t ==
  t in $NativeSimpleReturnTypes => nativeType t
  coreError strconc('"invalid return type for native function: ", 
              PNAME t)

++ Check that `t' is a valid parameter type for a native function,
++ and returns its translation.
nativeArgumentType t ==
  t in $NativeSimpleDataTypes => nativeType t
  -- Allow 'string'  for `pass-by-value'
  t = "string" => nativeType t
  -- anything else must use a modified reference type.
  atom t or #t ~= 2 => 
     coreError '"invalid argument type for a native function"
  [m,[c,t']] := t
  -- Require a modifier.
  not (m in '(readonly writeonly readwrite)) =>
    coreError '"missing modifier for argument type for a native function"
  -- Only 'pointer' and 'buffer' can be instantiated.
  not (c in '(buffer pointer)) =>
    coreError '"expected 'buffer' or 'pointer' type instance"
  not (t' in $NativeSimpleDataTypes) =>
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
    c = "buffer" => [bfColonColon("SB-SYS","VECTOR-SAP"),a]
    c = "pointer" => [bfColonColon("SB-SYS","ALIEN-SAP"),a]
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
    [["DEFENTRY", op, argtypes, [rettype, PNAME op']]]
  -- Otherwise, do it the hard way.
  [["CLINES",ccode], ["DEFENTRY", op, argtypes, [rettype, cop]]] where
    cop := strconc(PNAME op','"__stub")
    ccode := 
      "strconc"/[gclTypeInC t, '" ", cop, '"(",
	 :[cparm(x,a) for x in tails s for a in tails cargs],
	   '") { ", (t ~= "void" => '"return "; ""),
	     PNAME op', '"(",
	       :[gclArgsInC(x,a) for x in tails s for a in tails cargs],
		  '"); }" ]
                where cargs := [mkCArgName i for i in 0..(#s - 1)]
    mkCArgName i == strconc('"x",toString i)
    cparm(x,a) ==
      strconc(gclTypeInC first x, '" ", first a,
	(rest x => '", "; '""))
    gclTypeInC x ==
      x in $NativeSimpleDataTypes => PNAME x
      x = "void" => '"void"
      x = "string" => '"char*"
      x is [.,["pointer",.]] => "fixnum"
      '"object" 
    gclArgInC(x,a) ==
      x in $NativeSimpleDataTypes => a
      x = "string" => a   -- GCL takes responsability for the conversion
      [.,[c,y]] := x
      c = "pointer" => a
      y = "char" => strconc(a,'"->st.st__self")
      y = "byte" => strconc(a,'"->ust.ust__self")
      y = "int" => strconc(a,'"->fixa.fixa__self")
      y = "float" => strconc(a,'"->sfa.sfa__self")
      y = "double" => strconc(a,'"->lfa.lfa__self")
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
    [bfColonColon("FFI","C-INLINE"),args, nreverse argtypes,
      rettype, callTemplate(op',#args,s), 
        KEYWORD::ONE_-LINER, true]]] where
	  callTemplate(op,n,s) ==
	    "strconc"/[PNAME op,'"(",
	      :[sharpArg(i,x) for i in 0..(n-1) for x in s],'")"]
	  sharpArg(i,x) == 
	    i = 0 => strconc('"(#0)",selectDatum x)
	    strconc('",",'"(#", toString i, '")", selectDatum x)
	  selectDatum x ==
	    isSimpleNativeType x => '""
	    [.,[c,y]] := x
            c = "buffer" => 
	      y = "char" or y = "byte" => 
                AxiomCore::$ECLVersionNumber < 90100 => '"->vector.self.ch"
                y = "char" => '"->vector.self.i8"
                '"->vector.self.b8"
	      y = "int" => '"->vector.self.fix"
	      y = "float" => '"->vector.self.sf"
	      y = "double" => '"->vector.self.df"
	      coreError '"unknown argument to buffer type constructor"
            c = "pointer" => '""
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
  n := makeSymbol strconc(PNAME op, '"%clisp-hack")
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
      [KEYWORD::NAME,PNAME op'],
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
	      a' := rest ASSOC(p,pairs) => rest rest a'
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
    %hasFeature KEYWORD::WIN32 => strconc('"__",PNAME op')
    PNAME op'
    
  unstableArgs = nil =>
    [["DEFUN",op,args,
      [makeSymbol('"ALIEN-FUNCALL",'"SB-ALIEN"),
	[makeSymbol('"EXTERN-ALIEN",'"SB-ALIEN"), op',
	  ["FUNCTION",rettype,:argtypes]], :args]]]
  [["DEFUN",op,args,
    [bfColonColon("SB-SYS","WITH-PINNED-OBJECTS"), nreverse unstableArgs,
      [makeSymbol('"ALIEN-FUNCALL",'"SB-ALIEN"),
	[makeSymbol('"EXTERN-ALIEN",'"SB-ALIEN"), op',
	  ["FUNCTION",rettype,:argtypes]], :nreverse newArgs]]]]



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
    x = "string" => strPairs := [[p,:gensym '"loc"], :strPairs]
    x is [.,["buffer",.]] => aryPairs := [[p,:gensym '"loc"], :aryPairs]

  -- Build the actual foreign function call.
  -- Note that Clozure CL does not mangle foreign function call for
  -- us, so we're left with more platform dependencies than needed.
  if %hasFeature KEYWORD::DARWIN then
    op' := strconc("__",op')
  call := [bfColonColon("CCL","EXTERNAL-CALL"), STRING op', :args, rettype]
            where
              args() == [:[x, parm] for x in argtypes for p in parms]
              parm() ==
                    p' := ASSOC(p, strPairs) => rest p'
                    p' := ASSOC(p, aryPairs) => rest p'
                    p

  -- If the foreign call returns a C-string, turn it into a Lisp string.
  -- Note that if the C-string was malloc-ed, this will leak storage.
  if t = "string" then
    call := [bfColonColon("CCL","GET-CSTRING"), call]

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



