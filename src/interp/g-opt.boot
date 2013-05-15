-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2013, Gabriel Dos Reis.
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


import g_-util
namespace BOOT

--% 

$optimizableConstructorNames := $SystemInlinableConstructorNames

++ Return true if the domain `dom' is an instance of a functor
++ that has been nominated for inlining.
optimizableDomain? dom ==
  symbolMember?(opOf dom,$optimizableConstructorNames)

++ Register the domain `dom' for inlining.
nominateForInlining dom ==
  $optimizableConstructorNames := [opOf dom,:$optimizableConstructorNames]
  
--%

++ return the template of the instantiating functor for
++ the domain form `dom'.
getDomainTemplate dom ==
  dom isnt [.,:.] => nil
  getInfovec first dom

++ Emit code for an indirect call to domain-wide Spad function.  
++ This is usually the case for exported functions.
emitIndirectCall(fn,args,x) ==
  x.first := "SPADCALL"
  fn.first := '%tref
  x.rest := [:args,fn]
  x

--% OPTIMIZER

++ Return the name of the iterator variable `it'.
++ NOTES: iterator variables are usually of the form `[sc,:id]'
++        where `sc' is a storage class, i.e. `%local' or `%free'.
++        Occasionally, they are just symbols with the storage
++        class `%local' implied.
iteratorName it ==
  ident? it => it
  rest it

++ Subroutine of changeVariableDefinitionToStore.
++ Same semantics as changeVariableDefinitionToStore for a loop
++ with body `body', iterator list `iters', return value `val' and
++ currently bound variable list `vars'.
changeLoopVarDefsToStore(iters,body,val,vars) ==
  vars' := vars
  -- Iterator variables are local to the the loop.
  for it in iters repeat
    it is ['STEP,idx,start,inc,:final] =>
      vars' := changeVariableDefinitionToStore(start,vars')
      vars' := changeVariableDefinitionToStore(inc,vars')
      vars' := changeVariableDefinitionToStore(final,vars')
      vars' := [iteratorName it,:vars']
    it is [op,v,l] and op in '(IN ON) =>
      vars' := changeVariableDefinitionToStore(l,vars')
      vars' := [iteratorName it,:vars']
    vars' := changeVariableDefinitionToStore(it,vars')
  changeVariableDefinitionToStore(val,vars')
  -- Variables defined in the loop body are also local to the loop
  changeVariableDefinitionToStore(body,vars')
  vars

++ Change (%LET id expr) to (%store id expr) if `id' is being
++ updated as opposed to being defined. `vars' is the list of
++ all variable definitions in scope.
changeVariableDefinitionToStore(form,vars) ==
  atomic? form or form.op in '(CLOSEDFN XLAM) => vars
  form is ['%LET,v,expr] =>
    vars := changeVariableDefinitionToStore(expr,vars)
    do
      symbolMember?(v,vars) => form.op := '%store
      vars := [v,:vars]
    vars
  form is ['%scope,.,expr] and expr is ['%seq,:.] => 
    changeVariableDefinitionToStore(expr,vars)
    vars
  form is ['%when,:.] =>
    for clause in form.args repeat
      -- variable defined in clause predicates are visible
      -- in subsequent predicates.
      vars := changeVariableDefinitionToStore(first clause,vars)
      -- but those defined in branches are local.
      changeVariableDefinitionToStore(rest clause,vars)
    vars
  -- recursive binding
  form is ['%bind,:.] =>
    vars' := vars
    for [v,init] in second form repeat
      vars' := changeVariableDefinitionToStore(init,vars')
      vars' := [v,:vars']
    changeVariableDefinitionToStore(third form,vars')
    vars
  -- non-recursive binding.
  form is ['LET,:.] =>
    vars' := nil
    for [v,init] in second form repeat
      changeVariableDefinitionToStore(init,vars')
      vars' := [v,:vars']
    changeVariableDefinitionToStore(third form,[:vars',:vars])
    vars
  abstraction? form =>
    changeVariableDefinitionToStore(form.absBody,[:form.absParms,:vars])
    vars
  form is ['%repeat,:iters,body,val] =>
    changeLoopVarDefsToStore(iters,body,val,vars)
  form is ['%closure,fun,env] =>
    changeVariableDefinitionToStore(fun,vars)
    vars
  if form is ['%seq,:.] then
    form.args := spliceSeqArgs form.args
  for x in form repeat
    vars := changeVariableDefinitionToStore(x,vars)
  vars

++ Return true if `x' contains control transfer to a point outside itself.
jumpToToplevel? x ==
  atomic? x => false
  abstraction? x or op is '%closure => false
  op := x.op
  op is '%seq => CONTAINED('%leave,x.args) -- FIXME: what about GO?
  op in '(%exit %leave) => true
  or/[jumpToToplevel? x' for x' in x]

++ Return true if `form' is just one assignment expression.
nonExitingSingleAssignment? form ==
  form is ['%LET,.,rhs]
    and not CONTAINED('%LET,rhs) and not jumpToToplevel? rhs

++ Turns `form' into a `%bind'-expression if it starts with a
++ a sequence of first-time variable definitions.
groupVariableDefinitions form ==
  atomic? form => form
  form is ['%when,[p,s1],['%otherwise,s2]] and sideEffectFree? p =>
    [form.op,[p,groupVariableDefinitions s1],
      ['%otherwise,groupVariableDefinitions s2]]
  form.op is '%when =>
    -- FIXME: we should not be generating store-modifying predicates
    for clause in form.args while not CONTAINED('%LET, first clause) repeat
      second(clause) := groupVariableDefinitions second clause
    form
  form is ['%scope,tag,expr] =>
    mkScope(tag,groupVariableDefinitions expr)
  form is ['%bind,inits,expr] =>
    mkBind(inits,groupVariableDefinitions expr)
  form is ['%lambda,:.] =>
    [form.absKind,form.absParms,groupVariableDefinitions form.absBody]
  form is ['%repeat,:iters,body,val] =>
    [form.op,:iters,groupVariableDefinitions body,val]
  form isnt ['%seq,:stmts,['%exit,val]] => form
  form.args = nil => nil
  form.args is [s] => groupVariableDefinitions s
  defs := [s.args for s in stmts while nonExitingSingleAssignment? s]
  defs = nil => form
  stmts := drop(#defs,stmts)
  expr :=
    stmts = nil => val
    ['%seq,:stmts,['%exit,val]]
  mkBind(defs,expr)

++ Group all %LET-definitions of artificial/temporary variables
++ into %bind-forms, appropriate for inlining in later stages.
groupTranscients! x == walkWith!(x,function f) where
  f x ==
    x is ['%scope,tag,y] and y is ['%seq,:.] =>
      defs := [s.args for s in y.args while s is ['%LET,z,u]
                 and gensym? z and hasNoLeave?(u,tag)]
      defs = nil => x
      resetTo(x,mkBind(defs,mkScope(tag,mkSeq drop(#defs,y.args))))
    x

++ Reduce all applications of XLAM-abstractions to arguments.
++ This is done before simplifyVMForm to expose more opportunities
++ for further reductions.
reduceXLAM! x == walkWith!(x,function f) where
  f x ==
    x is ['%call,y,:args] and y is ['XLAM,:.] =>
     resetTo(x,doInlineCall(args,y.absParms,copyTree y.absBody))
    x

++ Remove throw-away expressions, inline one-time temporaries
++ abstracting conditionals (generated by the parser) and remove
++ unnecessary %seq boxes around singleton expressions.
++ NOTES: this transformation must be run before any grouping
++        of intermediate result bindings.
removeJunk! x == walkWith!(x,function f) where
  f x ==
    x is ['%seq,:.] =>
      x.args := g x.args
      x.args is [y] => resetTo(x,y)
      x
    x
  g xs ==
    xs = nil => nil
    x := first xs
    x is "/throwAway" => g rest xs          -- skip garbages
    x is ['%LET,y,u] and gensym? y and numOfOccurencesOf(y,rest xs) = 1 =>
      g substitute!(u,y,rest xs)
    x is ['%seq,:.] =>                      -- splice sub-sequences
      ys := x.args =>
        lastNode(ys).rest := rest xs
        g ys
      g rest xs                             -- skip empty statements
    rest xs = nil => xs
    sideEffectFree? x => g rest xs          -- skip effect-less statements.
    xs.rest := g rest xs
    xs

inlineLocals! x == walkWith!(x,function f) where
  f x ==
    x is ['%bind,inits,:.] =>
      kept := nil
      while inits is [u,:inits] repeat
        [y,z] := u
        usesVariable?(z,y) or usesVariable?(inits,y) => kept := [u,:kept]
        or/[usesVariable?(z,v) for [v,.] in kept] => kept := [u,:kept]
        canInlineVarDefinition(y,z,x.absBody) =>
          x.absBody := substitute!(z,y,x.absBody)
        kept := [u,:kept]
      kept = nil => resetTo(x,x.absBody)
      x.absParms := reverse! kept
      x
    x

++ Transform an IF-expression into a tower of %when-form.
transformIF! x == walkWith!(x,function f) where
  f x ==
    x is ['IF,p,s1,s2] =>
      s2 is '%noBranch => resetTo(x,['%when,[p,s1]])
      s1 is '%noBranch => resetTo(x,['%when,[['%not,p],s2]])
      s2 is ['%when,:.] => resetTo(x,['%when,[p,s1],:s2.args])
      resetTo(x,['%when,[p,s1],['%otherwise,s2]])
    x

++ Subroutine of packWhen!
mkDefault(g,l) ==
  l = nil => ['CONDERR]
  mkScope(g,mkSeq l)

coagulateWhenSeries(l,tag) ==
  f(l,tag) where
    f(l,tag) ==
      cl := nil
      while l is [s,:.] and (cl' := g(s,tag)) repeat
        cl := [:cl,:cl']
        l := rest l
      cl = nil => nil
      [cl,l]
    g(s,tag) ==
      -- return list of reduced clauses if they all exit from same scope.
      s is ['%when,:.] => exitClauses(s.args,tag)
      nil

++ Return non-nil if the expression `x' exist the scope with tag `g'
++ by normal local transfer or by exiting the enclosing function
++ with a `return' statement.  If non-nil, the return value is the
++ expression operand to the normal exit.
exitScope?(x,g) ==
  x is ['%leave,=g,y] and hasNoLeave?(y,g) => y
  x is ['%return,:.] => x
  nil

unnestWhen! x == f x where
  f x ==
    x is ['%scope,tag,y] =>
      y is ['%seq,:stmts] =>
        while stmts is [s,:.] repeat
          s := f s
          s is ['%when,[p,u],['%otherwise,v]] and exitScope?(u,tag) =>
            stmts.first := ['%when,[p,u]]
            stmts.rest := spliceSeqArgs [f v,:rest stmts]
          stmts.first := s
          stmts := rest stmts
        x
      y is ['%when,[p,u:=['%leave,=tag,.]],['%otherwise,v]] =>
        reset(x,f mkScope(tag,mkSeq [['%when,[p,u]],f v]))
      second(x.args) := f y
      x
    do
      abstraction? x => x.absBody := f x.absBody
      x is ['%leave,.,y] or x is ['%return,.,y] => second(x.args) := f y
      x is ['%when,:.] =>
        for cl in x.args repeat
          second(cl) := f second cl
      x is ['%seq,:.] =>
        for stmts in tails x.args repeat
          stmts.first := f first stmts
      x is ['%repeat,:.] =>
        x.loopBody := unnestWhen! x.loopBody
    x

++ Transform nested-to-tower.
packWhen! x == walkWith!(x,function f) where
  f x ==
    x is ['%when,[p1,['%when,[p2,s]]]] =>
      resetTo(x,f ['%when,[['%and,p1,p2],s]])
    x is ['%when,:cl,['%otherwise,y]] and y is ['%when,:.] =>
      resetTo(x,f ['%when,:cl,:y.args])
    x is ['%leave,g,['%when,[p,['%leave,=g,s]]]] =>
      resetTo(x,['%leave,g,['%when,[p,s]]])
    x is ['%scope,g,['%seq,:w]] and coagulateWhenSeries(w,g) is [u,v] =>
        resetTo(x,f ['%when,:u,['%otherwise,f mkDefault(g,v)]])
    x

spliceSeq! x == walkWith!(x,function f) where
  f x ==
    if x is ['%seq,:.] then
      x.args := spliceSeqArgs x.args
    x

++ Return the list of reduced clauses if every branch is a simple
++ normal exiting expression.  Otherwise, return nil.
exitClauses(l,g) ==
  [[p,x] for cl in l | cl is [p,u] and (x := exitScope?(u,g)) or leave nil]

++ Remove superfluous cancel/scope pairs exposed by running
++ removeJunk!.
cancelScopeLeave! x == walkWith!(x,function f) where
  f x ==
    x is ['%scope,g,u] =>
      u is ['%when,:.] and (v := exitClauses(u.args,g)) =>
        resetTo(x,[u.op,:v])
      y := exitScope?(u,g) => resetTo(x,f y)
      x
    x

++ Remove redundant %leave opode around %return forms.
removeLeave! x == walkWith!(x,function f) where
  f x ==
    x is ['%leave,.,y] and y is ['%return,:.] => resetTo(x,y)
    x

freeIteratorFirstValues iters ==
  [u for it in iters | u := f it] where
    f it ==
      it is ['STEP,['%free,:id],lo,:.] => ['%LET,id,lo]
      it is ['ON,['%free,:id],l] => ['%LET,id,l]
      nil

cleanLoop! x == prefixWalk!(x,function f) where
  f x ==
    x is ['%scope,tag,['%repeat,:itl,body,val]] =>
      body := g(body,tag)
      val := g(val,tag)
      firstVals := freeIteratorFirstValues itl =>
        resetTo(x,mkSeq [:firstVals,f ['%repeat,:itl,body,val]])
      resetTo(x,f ['%repeat,:itl,body,val])
    x
  g(x,tag) ==
    atomic? x => x
    x is ['%leave,=tag,y] =>
      first(x.args) := nil
      second(x.args) := g(y,tag)
      x
    for xs in tails x repeat
      xs.first := g(first xs,tag)
    x

removeScope! x == prefixWalk!(x,function f) where
  f x ==
    x is ['%scope,g,y] =>
      y is ['%when,[p,['%leave,=g,u]],['%leave,=g,v]] =>
        resetTo(x,['%when,[p,u],['%otherwise,v]])
      hasNoLeave?(y,g) => resetTo(x,f y)
      x
    x

++ Transform an intermediate form (output of the elaborator) to
++ a lower intermediate form, applying several transformations
++ generaly intended to improve quality and efficiency.
optimize! x ==
  x := unnestWhen! spliceSeq! packWhen! transformIF! removeLeave! cleanLoop! x
  changeVariableDefinitionToStore(x,nil)
  simplifyVMForm removeScope! cancelScopeLeave! spliceSeq! packWhen!
    inlineLocals! groupTranscients! cancelScopeLeave!
      removeJunk! reduceXLAM! x

++ A non-mutating version of `optimize!'.
optimize x ==
  optimize! copyTree x

optimizeFunctionDef(def) ==
  if $reportOptimization then
    sayBrightlyI bright '"Original LISP code:"
    pp def
 
  expr := optimize! copyTree second def
 
  if $reportOptimization then
    sayBrightlyI bright '"Intermediate VM code:"
    pp expr

  [first def,expr]

resetTo(x,y) ==
  y isnt [.,:.] => y
  sameObject?(x,y) => y
  x.first := y.first
  x.rest := y.rest
  x

++ Simplify the VM form `x'
simplifyVMForm x ==
  x is '%icst0 => 0
  x is '%icst1 => 1
  atomic? x => x
  x.op is 'CLOSEDFN => x
  x.op isnt [.,:.] =>
    symbol? x.op and abstractionOperator? x.op =>
      x.absBody := groupVariableDefinitions simplifyVMForm x.absBody
      x
    for args in tails x.args repeat
      args.first := simplifyVMForm first args
    opt := subrname x.op has OPTIMIZE => resetTo(x,FUNCALL(opt,x))
    x
  for xs in tails x repeat
    xs.first := simplifyVMForm first xs
  x
 
subrname u ==
  ident? u => u
  COMPILED_-FUNCTION_-P u or MBPIP u => BPINAME u
  nil
 
changeLeaveToExit(s,g) ==
  atomic? s => nil
  s.op in '(QUOTE %seq REPEAT COLLECT %collect %repeat) => nil
  s is ['%leave, =g,:u] => (s.first := '%exit; s.rest := u)
  changeLeaveToExit(first s,g)
  changeLeaveToExit(rest s,g)

changeLeaveToGo(s,g) ==
  s isnt [.,:.] or s.op is 'QUOTE => nil
  s is ['%leave, =g,u] =>
    changeLeaveToGo(u,g)
    s.first := "PROGN"
    s.rest := [["%LET",second g,u],["GO",second g]]
  changeLeaveToGo(first s,g)
  changeLeaveToGo(rest s,g)

optScope (x is ['%scope,g,a]) ==
  a isnt [.,:.] => a
  if a is ['%seq,:s,['%leave,=g,u]] then
    changeLeaveToExit(s,g)
    a.rest := [:s,['%exit,u]]
    a := simplifyVMForm a
  a isnt [.,:.] => a
  do
    hasNoLeave?(a,g) => resetTo(x,a)
    changeLeaveToGo(a,g)
    x.first := '%seq
    x.rest := [['%exit,a],second g,['%exit,second g]]
  x
 
optSPADCALL(form is ['SPADCALL,:argl]) ==
  not $InteractiveMode => form
  -- last arg is function/env, but may be a form
  argl is [:argl,fun] and fun is ["ELT",dom,slot] =>
    optCall ['%call,['ELT,dom,slot],:argl]
  form

++ Inline a call with arguments `args'  to a simple function with
++ parameter-list `parms' and body `body'.
doInlineCall(args,parms,body) ==
  -- 1. almost constant functions are easy
  parms = nil => body
  -- 2. identity functions too
  parms is [=body] => first args
  -- 3. We know that the body is essentially side-effect-free (at best)
  --    or simple expression (at worst.)  The issue is whether it is
  --    OK to directly substitute the arguments which may be
  --    arbitrary expressions.  The ideal semantics calls for introducting
  --    a (fresh) temporary to hold the values of the argument expressions.
  --    We attempt to short-circuit the native evaluation as follows:
  --      a. side-effect-free arguments go by themselves.
  --      b. Simple expression arguments go by themselves as long as
  --         the corresponding parameter is used linearly in the body.
  --      c. Others get evaluated to temporaries.
  tmps := nil         -- list of temporaries
  inits := nil        -- list of local bindings
  subst := nil        -- arguments substitution.
  for arg in args for parm in parms repeat
    g := gensym()
    tmps := [g,:tmps]
    n := numOfOccurencesOf(parm,body)
    atomic? arg or (sideEffectFree? arg and n < 2) or n = 1 =>
      subst := [[g,:arg],:subst]
    inits := [[g,arg],:inits]
  -- 4. Alpha-rename the body and substitute simple expression arguments.
  body := applySubst(pairList(parms,reverse! tmps),body)
  body := applySubst!(subst,body)
  -- 5. Deliver.
  inits = nil => body
  ['%bind,reverse! inits,body]    


optCall (x is ['%call,:u]) ==
  u is [['XLAM,vars,body],:args] =>
    vars isnt [.,:.] => body
    #vars > #args => systemErrorHere ['optCall,x]
    resetTo(x,doInlineCall(args,vars,body))
  [fn,:a] := u
  fn isnt [.,:.] =>
    opt := fn has OPTIMIZE => resetTo(x,FUNCALL(opt,u))
    resetTo(x,u)
  fn is ['%apply,name] =>
    do
      ident? name =>
        x.first := '%funcall
        x.rest := [['%head,name],:a,['%tail,name]]
      x.first := 'SPADCALL
      x.rest := [:a,name]
    x
  fn is ['%closure,['%function,op],env] =>
    x.first := op
    x.rest := [:a,env]
    x
  fn is ['%external,op] =>
    x.first := op
    x.rest := a
    opt := op has OPTIMIZE => resetTo(x,apply(opt,x,nil))
    x
  fn is ['ELT,:.] => emitIndirectCall(fn,a,x)
  fn is ['CONST,R,n] => ['spadConstant,R,n]
  systemErrorHere ['optCall,x]
 
optClosure(x is ['%closure,fun,env]) ==
  fun is ['%function,['%lambda,vars,body]] =>
    do
      vars is [:vars',=env] =>
        body is [op,: =vars] => x.args := [['%function,op],env]
        not usesVariable?(body,env) => x.args := [fun,'%nil]
    x
  x

optCons (x is ["CONS",a,b]) ==
  a is "NIL" =>
    b is 'NIL => (x.first := 'QUOTE; x.rest := ['NIL,:'NIL]; x)
    b is ['QUOTE,:c] => (x.first := 'QUOTE; x.rest := ['NIL,:c]; x)
    x
  a is ['QUOTE,a'] =>
    b is 'NIL => (x.first := 'QUOTE; x.rest := [a',:'NIL]; x)
    b is ['QUOTE,:c] => (x.first := 'QUOTE; x.rest := [a',:c]; x)
    x
  x
 
optCond (x is ['%when,:l]) ==
  l is [['%true,c],:.] => c
  l is [['%false,.],:.] => optCond ['%when,:rest l]
  l is [['%otherwise,c]] => c
  if l is [a,[aa,b]] and aa is '%otherwise and b is ['%when,:c] then
    x.rest.rest := c
  if l is [[p1,:c1],[p2,:c2],:.] then
    if (p1 is ['%not,=p2]) or (p2 is ['%not,=p1]) then
      l:=[[p1,:c1],['%otherwise,:c2]]
      x.rest := l
    c1 is ['NIL] and p2 is '%otherwise and first c2 is '%otherwise =>
      return optNot ['%not,p1]
  l is [[p1,['%when,[p2,c2]]]] => optCond ['%when,[['%and,p1,p2],c2]]
  l is [[p1,c1],['%otherwise,'%false]] => optAnd ['%and,p1,c1]
  l is [[p1,c1],['%otherwise,'%true]] => optOr ['%or,optNot ['%not,p1],c1]
  l is [[p1,'%false],['%otherwise,c2]] => optAnd ['%and,optNot ['%not,p1],c2]
  l is [[p1,'%true],['%otherwise,c2]] => optOr ['%or,p1,c2]
  l is [[p1,:c1],[p2,:c2],[p3,:c3]] and p3 is '%otherwise =>
    EqualBarGensym(c1,c3) =>
      optCond ['%when,[['%or,p1,optNot ['%not,p2]],:c1],['%otherwise,:c2]]
    EqualBarGensym(c1,c2) =>
      optCond ['%when,[['%or,p1,p2],:c1],['%otherwise,:c3]]
    x
  for y in tails l repeat
    while y is [[a1,c1],[a2,c2],:y'] and EqualBarGensym(c1,c2) repeat
      a := ['%or,a1,a2]
      first(y).first := a
      y.rest := y'
  x
 
AssocBarGensym(key,l) ==
  for x in l repeat
    cons? x =>
      EqualBarGensym(key,first x) => return x
 
EqualBarGensym(x,y) ==
  $GensymAssoc: local := nil
  fn(x,y) where
    fn(x,y) ==
      x=y => true
      gensym? x and gensym? y =>
        z:= assoc(x,$GensymAssoc) => y=rest z
        $GensymAssoc:= [[x,:y],:$GensymAssoc]
        true
      null x => y is [g] and gensym? g
      null y => x is [g] and gensym? g
      x isnt [.,:.] or y isnt [.,:.] => false
      fn(first x,first y) and fn(rest x,rest y)
 
optSuchthat [.,:u] == ["SUCHTHAT",:u]
 
++ List of VM side effect free operators.
$VMsideEffectFreeOperators ==
  '(SPADfirst ASH FLOAT FLOAT_-SIGN %function %nullStream %nonNullStream
    %nothing %when %false %true %otherwise %2bit %2bool
    %and %or %not %peq %ieq %ilt %ile %igt %ige %head %tail %integer?
    %beq %blt %ble %bgt %bge %bitand %bitior %bitxor %bitnot %bcompl
    %ilength %ibit %icst0 %icst1 %icstmin %icstmax
    %imul %iadd %isub %igcd %ilcm %ipow %imin %imax %ieven? %iodd? %iinc
    %idec %irem %iquo %idivide %idec %irandom %imulmod %iaddmod %isubmod
    %ilshift %irshift
    %feq %flt %fle %fgt %fge %fmul %fadd %fsub %fexp %fmin %fmax %float?
    %fpowi %fdiv %fneg %i2f %fminval %fmaxval %fbase %fprec %ftrunc
    %fsqrt %fpowf %flog %flog2 %flog10 %fmanexp %fNaN? %fdecode
    %fsin  %fcos  %ftan  %fcot  
    %fasin %facos %fatan %facot 
    %fsinh  %fcosh  %ftanh 
    %fasinh %facosh %fatanh
    %val2z %z2val %zlit %zreal %zimag
    %zexp %zlog %zsin %zcos %ztan %zasin %zacos %zatan
    %zsinh %zcosh %ztanh %zasinh %zacosh %zatanh
    %nil %pair %list %pair? %lconcat %llength %lfirst %lsecond %lthird
    %lreverse %lempty? %lcopy %hash %ismall? %string? %f2s STRINGIMAGE
    %ccst %ccstmax %ceq %clt %cle %cgt %cge %c2i %i2c %s2c %c2s %cup %cdown
    %sname
    %strlength %streq %i2s %schar %strlt %strconc
    %strcopy %bytevec2str %str2bytevec
    %array %simpleArray %emptyArray %list2array %array2list
    %initialElement %initialContents
    %vector %aref %vref %vlength %vcopy
    %bitvector
    %bitvecnot %bitvecand %bitvecnand %bivecor %bitvecnor %bitvecxor
    %bitveccopy %bitvecconc %bitveclength %bitvecref %bitveceq %bitveclt
    %before? %equal %sptreq %ident? %property %tref
    %void %retract %pullback %lambda %closure %external
    %type2form)

++ List of simple VM operators
$simpleVMoperators == 
  [:$VMsideEffectFreeOperators,
    :['SPADCALL,'%apply, '%funcall, '%gensym, '%lreverse!, '%strstc]]

++ Return true if the `form' is semi-simple with respect to
++ to the list of operators `ops'.
semiSimpleRelativeTo?(form,ops) ==
  atomic? form => true
  not symbol?(form.op) or not symbolMember?(form.op,ops) => false
  abstraction? form => true    -- always, regardless of body
  form.op is '%when =>
    and/[sideEffectFree? p and semiSimpleRelativeTo?(c,ops)
           for [p,c] in form.args]
  and/[semiSimpleRelativeTo?(f,ops) for f in form.args]

++ Return true if `form' os a side-effect free form.  
sideEffectFree? form ==
  semiSimpleRelativeTo?(form,$VMsideEffectFreeOperators)

++ Return true if `form' is a VM form whose evaluation does not depend
++ on the program point where it is evaluated.
$FloatableOperators ==
  ['%bind,:$VMsideEffectFreeOperators]

floatableVMForm?: %Code -> %Boolean
floatableVMForm? form ==
  semiSimpleRelativeTo?(form,$FloatableOperators)

++ Return true if the VM form `form' is one that we certify to 
++ evaluate to a (compile time) constant.  Note that this is a
++ fairly conservative approximation of compile time constants.
isVMConstantForm: %Code -> %Boolean
isVMConstantForm form ==
  integer? form or string? form => true
  form isnt [op,:args] => false
  op in '(%list %pair %vector) => false
  symbolMember?(op,$VMsideEffectFreeOperators) and 
    "and"/[isVMConstantForm arg for arg in args]

++ Return the set of free variables in the VM form `form'.
findVMFreeVars form ==
  ident? form => [form]
  form isnt [op,:args] => nil  
  op is 'QUOTE => nil
  vars := union/[findVMFreeVars arg for arg in args]
  op isnt [.,:.] => vars
  setUnion(findVMFreeVars op,vars)

++ Return true is `var' is the left hand side of an assignment
++ or a sort of binding in `form'.
modified?(var,form) ==
  atomic? form => false
  form is [op,var',expr,:.] and op in '(%LET LETT SETQ %store) =>
    modified?(var,expr) => true
    symbol? var' => var' = var   -- whole var is assigned
    var' is [.,=var,:.]             -- only part of it is modified
  form is ['%bind,:.] and
    (or/[symbolEq?(var,var') for [var',.] in form.absParms]) => true
  form is ['%repeat,:iters,.,.] and
    (or/[symbolEq?(var,iteratorName i) for i in iters]) => true
  abstraction? form and symbolMember?(var,form.absParms) => true
  or/[modified?(var,f) for f in form]


++ Return the list of variables referenced in `expr'.  
dependentVars expr == main(expr,nil) where
  main(x,vars) ==
    ident? x =>
      symbolMember?(x,vars) => vars
      [x,:vars]
    atomic? x => vars
    x' :=
      cons? x.op => x
      x.args
    for y in x' repeat
      vars := main(y,vars)
    vars

++ Subroutine of optBind.  Return true if the variable `var' locally
++ defined in a binding form can be safely replaced by its initalization
++ `expr' in the `body' of the binding form.
canInlineVarDefinition(var,expr,body) ==
  -- Occasional situation where `expr' evaluates to itself
  gensym? var and sameObject?(var,body) => true
  -- FIXME: We should not be inlining a side-effecting initializer.
  -- If the variable is assigned to, that is a no no.
  modified?(var,body) => false
  -- Similarly, if the initial value depends on variables that are
  -- side-effected latter, it is also a no no.
  or/[modified?(x,body) for x in dependentVars expr] => false
  -- If the initializer is a variable and not modified in body,
  -- and the new var is not modified, then we can inline.
  -- FIXME: except if the modification is done via normal function calls.
  ident? expr => true
  -- Conversatively stay out of loops
  cons? body and body.op in '(%repeat %collect) => false
  -- Linearly used internal temporaries should be replaced, and
  -- so should side-effet free initializers for linear variables.
  usageCount := numOfOccurencesOf(var,body)
  usageCount < 2 and floatableVMForm? expr => true
  gensym? var and usageCount = 1 => true
  -- If the initializer is a variable and the body is
  -- a series of choices with floatable predicates, then
  -- no harm is done by inlining the local `var'.
  ident? expr and body is ['%when,:branches] =>
    and/[floatableVMForm? pred for [pred,:.] in branches]
  false

++ Implement simple-minded LET-inlining.  It seems we can't count
++ on Lisp implementations to do this simple transformation.
++ This transformation will probably be more effective when all
++ type informations are still around.   Which is why we should
++ have a type directed compilation throughout. 
optBind form ==
  form isnt ['%bind,inits,.] => form           -- accept only simple bodies
  while inits ~= nil repeat
    [var,expr] := first inits
    usesVariable?(rest inits,var) => leave nil -- no dependency, please.
    body := third form
    canInlineVarDefinition(var,expr,body) =>
      third(form) := substitute!(expr,var,body)
      inits := rest inits
    leave nil
  null inits => third form                        -- no local var left
  second(form) := inits
  form

optTry form ==
  form isnt ['%try,e,hs,f] or not(floatableVMForm? e) or f ~= nil => form
  e

optList form ==
  form is ['%list] => '%nil
  literalElts := [(x is ['QUOTE,y] => y; leave "failed") for x in form.args]
  literalElts is "failed" => form
  quote literalElts

quoteMode x ==
  x isnt [.,:.] => x
  ident? x.op and constructor? x.op =>
    ['%list,quote x.op,:[quoteMode a for a in x.args]]
  [quoteMode y for y in x]

++ Translate retraction of a value denoted by `e' to sub-domain `m'
++ defined by predicate `pred',
optRetract ["%retract",e,m,pred] ==
  e isnt [.,:.] =>
    cond := simplifyVMForm substitute(e,"#1",pred)
    cond is '%true => e
    ['%when,[cond,e],['%otherwise,['moanRetract,e,quoteMode m]]]
  g := gensym()
  ['%bind,[[g,e]],
    ['%when,[substitute(g,"#1",pred),g],
      ['%otherwise,['moanRetract,g,quoteMode m]]]]

++ We have an expression `x' of some Union type.  Expand an attempted
++ pullback to the `n'th branch of type `t'.
optPullback ['%pullback,x,t,n] ==
  y :=
    ident? x => x
    gensym()
  expr :=
    ['%when,[['%ieq,['%head,y],n],['%tail,y]],
      ['%otherwise,['moanRetract,y,quoteMode t]]]
  symbolEq?(x,y) => expr
  ['%bind,[[y,x]],expr]

--%  Boolean expression transformers

optNot(x is ['%not,a]) ==
  a is '%true => '%false
  a is '%false => '%true
  a is ['%not,b] => b
  a is ['%when,:.] =>
    optCond [a.op, :[[p,optNot ['%not,c]] for [p,c] in a.args]]
  x

optAnd(x is ['%and,a,b]) ==
  a is '%true => b
  b is '%true => a
  a is '%false => '%false
  x

optOr(x is ['%or,a,b]) ==
  a is '%false => b
  b is '%false => a
  a is '%true => '%true
  x

-- Boolean <-> bit conversion.
opt2bit ['%2bit,a] ==
  optCond ['%when,[a,'%icst1],['%otherwise,'%icst0]]

opt2bool ['%2bool,a] ==
  a is '%icst0 => '%false
  a is '%icst1 => '%true
  optIeq ['%ieq,a,'%icst1]

optIeq(x is ['%ieq,a,b]) ==
  integer? a and integer? b =>
    scalarEq?(a,b) => '%true
    '%false
  sameObject?(a,b) => '%true
  x

optIlt(x is ['%ilt,a,b]) ==
  -- 1. Don't delay if both operands are literals.
  integer? a and integer? b =>
    a < b => '%true
    '%false
  -- 2. max(a,b) cannot be negative if either a or b is zero.
  b = 0 and a is ['%imax,:.] and (second a = 0 or third a = 0) => '%false
  -- 3. min(a,b) cannot be positive if either a or b is zero.
  a = 0 and b is ['%imin,:.] and (second b = 0 or third b = 0) => '%false
  x

optIle(x is ['%ile,a,b]) ==
  optNot ['%not,optIlt ['%ilt,b,a]]

optIgt x ==
  optIlt ['%ilt,third x, second x]

optIge x ==
  optNot ['%not,optIlt ['%ilt,second x,third x]]

--% Byte operations

optBle ['%ble,a,b] ==
  optNot ['%not,['%blt,b,a]]

optBgt ['%bgt,a,b] ==
  ['%blt,b,a]

optBge ['%bge,a,b] ==
  optBle ['%ble,b,a]




--% Integer operations

optIadd(x is ['%iadd,a,b]) ==
  integer? a and integer? b => a + b
  integer? a =>
    a = 0 => b
    b is [op,b1,b2] and op in '(%iadd %isub) =>
      integer? b1 => simplifyVMForm [op,['%iadd,a,b1],b2]
      integer? b2 => simplifyVMForm ['%iadd,b1,[op,a,b2]]
      x
    x
  integer? b =>
    b = 0 => a
    a is [op,a1,a2] and op in '('%iadd %isub) =>
      integer? a1 => simplifyVMForm [op,['%iadd,a1,b],a2]
      integer? a2 => simplifyVMForm ['%iadd,a1,[op,b,a2]]
      x
    x
  x

optIaddmod ['%iaddmod,a,b,c] ==
  optIrem ['%irem,optIadd ['%iadd,a,b],c]

optIinc(x is ['%iinc,a]) ==
  integer? a => a + 1
  a is [op,b,c] and op in '(%isub %iadd) =>
    integer? b => simplifyVMForm [op,[op,b,1],c]
    integer? c => simplifyVMForm [op,b,[op,c,1]]
    x
  x

optIsub(x is ['%isub,a,b]) ==
  integer? a and integer? b => a - b
  integer? a =>
    a = 0 => ['%ineg,b]
    b is ['%iadd,b1,b2] =>
      integer? b1 => simplifyVMForm ['%isub,['%isub,a,b1],b2]
      integer? b2 => simplifyVMForm ['%isub,['%isub,a,b2],b1]
      x
    b is ['%isub,b1,b2] =>
      integer? b1 => simplifyVMForm ['%iadd,['%isub,a,b1],b2]
      integer? b2 => simplifyVMForm ['%isub,['%iadd,a,b2],b1]
      x
    x
  integer? b =>
    b = 0 => a
    a is ['%iadd,a1,a2] =>
      integer? a1 => simplifyVMForm ['%iadd,['%isub,a1,b],a2]
      integer? a2 => simplifyVMForm ['%iadd,a1,['%isub,a2,b]]
      x
    a is ['%isub,a1,a2] =>
      integer? a1 => simplifyVMForm ['%isub,['%isub,a1,b],a2]
      integer? a2 => simplifyVMForm ['%isub,a1,['%iadd,a2,b]]
      x
    x
  x

optIsubmod ['%isubmod,a,b,c] ==
  optIrem ['%irem,optIsub ['%isub,a,b],c]

optIdec(x is ['%idec,a]) ==
  integer? a => a - 1
  a is ['%iadd,b,c] =>
    integer? b => simplifyVMForm ['%iadd,['%isub,b,1],c]
    integer? c => simplifyVMForm ['%iadd,b,['%isub,c,1]]
    x
  a is ['%isub,b,c] =>
    integer? b => simplifyVMForm ['%isub,['%isub,b,1],c]
    integer? c => simplifyVMForm ['%isub,b,['%iadd,c,1]]
    x
  x

optImul(x is ['%imul,a,b]) ==
  integer? a and integer? b => a * b
  integer? a and a = 1 => b
  integer? b and b = 1 => a
  x

optImulmod(x is ['%imulmod,a,b,c]) ==
  optIrem ['%irem,optImul ['%imul,a,b],c]

optIneg(x is ['%ineg,a]) ==
  integer? a => -a
  x

optIrem(x is ['%irem,a,b]) ==
  integer? a and integer? b => a rem b
  x

optIquo(x is ['%iquo,a,b]) ==
  integer? a and integer? b => a quo b
  x

--% Arrays

optEmptyArray ['%emptyArray,t] ==
  ['%array,'%icst0,'%elementType,['%type2form,t]]

optSimpleArray ['%simpleArray,t,n,x] ==
  ['%array,n,'%elementType,['%type2form,t],'%initialElement,x]

optList2Array ['%list2array,l,t] ==
  ['%array,['%llength, l],'%elementType,['%type2form,t],'%initialContents,l]

--%  
--% optimizer hash table
--%
 
for x in '((%call         optCall) _
           (%bind        optBind)_
           (%try         optTry)_
           (%not         optNot)_
           (%and         optAnd)_
           (%or          optOr)_
           (%ble         optBle)_
           (%bgt         optBgt)_
           (%bge         optBge)_
           (%ieq         optIeq)_
           (%ilt         optIlt)_
           (%ile         optIle)_
           (%igt         optIgt)_
           (%ige         optIge)_
           (%ineg        optIneg)_
           (%iadd        optIadd)_
           (%iaddmod     optIaddmod)_
           (%iinc        optIinc)_
           (%isub        optIsub)_
           (%isubmod     optIsubmod)_
           (%irem        optIrem)_
           (%iquo        optIquo)_
           (%imul        optImul)_
           (%imulmod     optImulmod)_
           (%2bit        opt2bit)_
           (%2bool       opt2bool)_
           (%list        optList)_
           (SPADCALL     optSPADCALL)_
           (%closure     optClosure)_
           (_|           optSuchthat)_
           (%scope       optScope)_
           (%when        optCond)_
           (%retract     optRetract)_
           (%pullback    optPullback)_
           (%emptyArray  optEmptyArray)_
           (%simpleArray optSimpleArray)_
           (%list2array  optList2Array)) _
   repeat property(first x,'OPTIMIZE) := second x
       --much quicker to call functions if they have an SBC

