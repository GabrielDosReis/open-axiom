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
  atom dom => nil
  getInfovec first dom

++ Emit code for an indirect call to domain-wide Spad function.  
++ This is usually the case for exported functions.
emitIndirectCall(fn,args,x) ==
  x.first := "SPADCALL"
  fn.first := '%tref
  x.rest := [:args,fn]
  x

--% OPTIMIZER

++ Change (%LET id expr) to (%store id expr) if `id' is being
++ updated as opposed to being defined. `vars' is the list of
++ all variable definitions in scope.
changeVariableDefinitionToStore(form,vars) ==
  atomic? form or form.op is 'CLOSEDFN => vars
  form is ['%LET,v,expr] =>
    vars := changeVariableDefinitionToStore(expr,vars)
    if symbolMember?(v,vars) then
      form.op := '%store
    else
      vars := [v,:vars]
    vars
  form.op is '%when =>
    for clause in form.args repeat
      -- variable defined in clause predicates are visible
      -- subsequent predicates
      vars := changeVariableDefinitionToStore(first clause,vars)
      -- but those defined in branches are local.
      changeVariableDefinitionToStore(rest clause,vars)
    vars
  -- local bindings are, well, local.
  form.op in '(%bind LET) =>
    vars' := vars
    for [v,init] in second form repeat
      vars' := changeVariableDefinitionToStore(init,vars')
      vars' := [v,:vars']
    changeVariableDefinitionToStore(third form,vars')
    vars
  abstractionOperator? form.op =>
    changeVariableDefinitionToStore(third form,[:second form,:vars])
    vars
  for x in form repeat
    vars := changeVariableDefinitionToStore(x,vars)
  vars

++ Return true if `x' contains control transfer to a point outside itself.
jumpToToplevel? x ==
  atomic? x => false
  op := x.op
  op is 'SEQ => CONTAINED('THROW,x.args) -- FIXME: what about GO?
  op in '(EXIT THROW %leave) => true
  or/[jumpToToplevel? x' for x' in x]

++ Return true if `form' is just one assignment expression.
nonExitingSingleAssignment? form ==
  form is ['%LET,.,rhs]
    and not CONTAINED('%LET,rhs) and not jumpToToplevel? rhs

++ Turns `form' into a `%bind'-expression if it starts with a
++ a sequence of first-time variable definitions.
groupVariableDefinitions form ==
  atomic? form => form
  form.op is '%when =>
    -- FIXME: we should not be generating store-modifying predicates
    for clause in form.args while not CONTAINED('%LET, first clause) repeat
      second(clause) := groupVariableDefinitions second clause
    form
  form isnt ['SEQ,:stmts,['EXIT,val]] => form
  defs := nil
  for x in stmts while nonExitingSingleAssignment? x  repeat
    defs := [x.args,:defs]
  defs = nil or jumpToToplevel? defs => form
  stmts := drop(#defs,stmts)
  expr :=
    stmts = nil => val
    ['SEQ,:stmts,['EXIT,val]]
  ['%bind,reverse! defs,expr]

optimizeFunctionDef(def) ==
  if $reportOptimization then
    sayBrightlyI bright '"Original LISP code:"
    pp def
 
  def' := simplifyVMForm COPY def
 
  if $reportOptimization then
    sayBrightlyI bright '"Intermediate VM code:"
    pp def'

  [name,[slamOrLam,args,body]] := def'
 
  body':=
    removeTopLevelCatch body where
      removeTopLevelCatch body ==
        body is ["CATCH",g,u] =>
          removeTopLevelCatch replaceThrowByReturn(u,g)
        body
      replaceThrowByReturn(x,g) ==
        fn(x,g)
        x
      fn(x,g) ==
        x is ["THROW", =g,:u] =>
          x.first := "RETURN"
          x.rest := replaceThrowByReturn(u,g)
        atom x => nil
        replaceThrowByReturn(first x,g)
        replaceThrowByReturn(rest x,g)
  changeVariableDefinitionToStore(body',args)
  [name,[slamOrLam,args,groupVariableDefinitions body']]

resetTo(x,y) ==
  atom y => x := y
  sameObject?(x,y) => x
  x.first := y.first
  x.rest := y.rest
  x

++ Simplify the VM form `x'
simplifyVMForm x ==
  x is '%icst0 => 0
  x is '%icst1 => 1
  atomic? x => x
  x.op is 'CLOSEDFN => x
  atom x.op =>
    x is [op,vars,body] and abstractionOperator? op =>
      third(x) := simplifyVMForm body
      x
    if x.op is 'IF then
      resetTo(x,optIF2COND x)
    for args in tails x.args repeat
      args.first := simplifyVMForm first args
    opt := subrname x.op has OPTIMIZE => resetTo(x,FUNCALL(opt,x))
    x
  for xs in tails x repeat
    xs.first := simplifyVMForm first xs
  x
 
subrname u ==
  IDENTP u => u
  COMPILED_-FUNCTION_-P u or MBPIP u => BPINAME u
  nil
 
changeThrowToExit(s,g) ==
  atom s or s.op in '(QUOTE SEQ REPEAT COLLECT %collect %loop) => nil
  s is ["THROW", =g,:u] => (s.first := "EXIT"; s.rest := u)
  changeThrowToExit(first s,g)
  changeThrowToExit(rest s,g)

hasNoThrows(a,g) ==
  a is ["THROW", =g,:.] => false
  atom a => true
  hasNoThrows(first a,g) and hasNoThrows(rest a,g)

changeThrowToGo(s,g) ==
  atom s or first s is 'QUOTE => nil
  s is ["THROW", =g,u] =>
    changeThrowToGo(u,g)
    s.first := "PROGN"
    s.rest := [["%LET",second g,u],["GO",second g]]
  changeThrowToGo(first s,g)
  changeThrowToGo(rest s,g)

++ Change any `(THROW tag (%return expr))' in x to just
++ `(%return expr) since a %return-expression transfers control
++ out of the function body anyway.  Similarly, transform
++ reudant `(THROW tag (THROW tag expr))' to `(THROW tag expr)'.
removeNeedlessThrow x ==
  atomic? x => x
  x is ['THROW,.,y] and y is ['%return,:.] =>
    removeNeedlessThrow third y
    x.op := y.op
    x.args := y.args
  x is ['THROW,g,y] and y is ['THROW,=g,z] =>
    removeNeedlessThrow z
    second(x.args) := z
  for x' in x repeat
    removeNeedlessThrow x'

optCatch (x is ["CATCH",g,a]) ==
  $InteractiveMode => x
  atom a => a
  removeNeedlessThrow a
  if a is ["SEQ",:s,["THROW", =g,u]] then
    changeThrowToExit(s,g)
    a.rest := [:s,["EXIT",u]]
    a := simplifyVMForm a
  if hasNoThrows(a,g) then
    resetTo(x,a)
  else
    changeThrowToGo(a,g)
    x.first := "SEQ"
    x.rest := [["EXIT",a],second g,["EXIT",second g]]
  x
 
optSPADCALL(form is ['SPADCALL,:argl]) ==
  not $InteractiveMode => form
  -- last arg is function/env, but may be a form
  argl is [:argl,fun] and fun is ["ELT",dom,slot] =>
    optCall ['%call,['ELT,dom,slot],:argl]
  form
 
optCall (x is ['%call,:u]) ==
  u is [['XLAM,vars,body],:args] =>
    atom vars => body
    #vars > #args => systemErrorHere ['optCall,x]
    resetTo(x,optXLAMCond applySubst(pairList(vars,args),body))
  [fn,:a] := u
  atom fn =>
    opt := fn has OPTIMIZE => resetTo(x,FUNCALL(opt,u))
    resetTo(x,u)
  fn is ['applyFun,name] =>
    x.first := 'SPADCALL
    x.rest := [:a,name]
    x
  fn is [q,R,n] and q in '(ELT CONST) =>
    q is 'CONST => ['spadConstant,R,n]
    emitIndirectCall(fn,a,x)
  systemErrorHere ['optCall,x]
 
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
 
optMkRecord ["mkRecord",:u] ==
  u is [x] => ['%list,x]
  #u=2 => ['%pair,:u]
  ['%vector,:u]
 
optCond (x is ['%when,:l]) ==
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
  $GensymAssoc: fluid := nil
  fn(x,y) where
    fn(x,y) ==
      x=y => true
      GENSYMP x and GENSYMP y =>
        z:= assoc(x,$GensymAssoc) => y=rest z
        $GensymAssoc:= [[x,:y],:$GensymAssoc]
        true
      null x => y is [g] and GENSYMP g
      null y => x is [g] and GENSYMP g
      atom x or atom y => false
      fn(first x,first y) and fn(rest x,rest y)
 
--Called early, to change IF to conditional form
 
optIF2COND ["IF",a,b,c] ==
  b is "%noBranch" => ['%when,[['%not,a],c]]
  c is "%noBranch" => ['%when,[a,b]]
  c is ["IF",:.] => ['%when,[a,b],:rest optIF2COND c]
  c is ['%when,:p] => ['%when,[a,b],:p]
  ['%when,[a,b],['%otherwise,c]]
 
optXLAMCond x ==
  x is ['%when,u:= [p,c],:l] =>
    p is '%otherwise => c
    ['%when,u,:optCONDtail l]
  atom x => x
  x.first := optXLAMCond first x
  x.rest := optXLAMCond rest x
  x
 
optCONDtail l ==
  null l => nil
  [frst:= [p,c],:l']:= l
  p is '%otherwise => [['%otherwise,c]]
  null rest l => [frst,['%otherwise,["CondError"]]]
  [frst,:optCONDtail l']

++ Determine whether the symbol `g' is the name of a temporary that
++ can be replaced in the form `x', if it is of linear usage and not
++ the name of a program point.  The latter occurs when THROW forms
++ are changed to %LET form followed by a GO form -- see optCatch.
replaceableTemporary?(g,x) ==
  GENSYMP g and numOfOccurencesOf(g,x) < 2 and not jumpTarget?(g,x) where
    jumpTarget?(g,x) ==
      atomic? x => false
      x is ['GO,=g] => true
      or/[jumpTarget?(g,x') for x' in x]

optSEQ ["SEQ",:l] ==
  tryToRemoveSEQ SEQToCOND getRidOfTemps splicePROGN l where
    splicePROGN l ==
      atomic? l => l
      l is [["PROGN",:stmts],:l'] => [:stmts,:l']
      l.rest := splicePROGN rest l
    getRidOfTemps l ==
      null l => nil
      l is [["%LET",g,x],:r] and replaceableTemporary?(g,r) =>
        getRidOfTemps substitute(x,g,r)
      first l is "/throwAway" => getRidOfTemps rest l
      --this gets rid of unwanted labels generated by declarations in SEQs
      [first l,:getRidOfTemps rest l]
    SEQToCOND l ==
      transform:= [[a,b] for x in l while (x is ['%when,[a,["EXIT",b]]])]
      before:= take(#transform,l)
      aft:= after(l,before)
      null before => ["SEQ",:aft]
      null aft => ['%when,:transform,'(%otherwise (conderr))]
      optCond ['%when,:transform,['%otherwise,optSEQ ["SEQ",:aft]]]
    tryToRemoveSEQ l ==
      l is ["SEQ",[op,a]] and op in '(EXIT RETURN THROW) => a
      l
 
optRECORDELT ["RECORDELT",name,ind,len] ==
  len=1 =>
    ind=0 => ['%head,name]
    keyedSystemError("S2OO0002",[ind])
  len=2 =>
    ind=0 => ['%head,name]
    ind=1 => ['%tail,name]
    keyedSystemError("S2OO0002",[ind])
  ['%vref,name,ind]
 
optSETRECORDELT ["SETRECORDELT",name,ind,len,expr] ==
  len=1 =>
    ind = 0 => ['SEQ,['%store,['%head,name],expr],['EXIT,['%head,name]]]
    keyedSystemError("S2OO0002",[ind])
  len=2 =>
    ind = 0 => ['SEQ,['%store,['%head,name],expr],['EXIT,['%head,name]]]
    ind = 1 => ['SEQ,['%store,['%tail,name],expr],['EXIT,['%tail,name]]]
    keyedSystemError("S2OO0002",[ind])
  ['%store,['%vref,name,ind],expr]
 
optRECORDCOPY ["RECORDCOPY",name,len] ==
  len = 1 => ['%list,['%head,name]]
  len = 2 => ['%pair,['%head,name],['%tail,name]]
  ["REPLACE",["MAKE_-VEC",len],name]
 
optSuchthat [.,:u] == ["SUCHTHAT",:u]
 
++ List of VM side effect free operators.
$VMsideEffectFreeOperators ==
  '(SPADfirst ASH IDENTP FLOAT_-RADIX FLOAT FLOAT_-SIGN
    %funcall %nothing %when %false %true %otherwise %2bit %2bool
    %and %or %not %peq %ieq %ilt %ile %igt %ige %head %tail %integer?
    %beq %blt %ble %bgt %bge %bitand %bitior %bitxor %bitnot %bcompl
    %ilength %ibit %icst0 %icst1 %icstmin %icstmax
    %imul %iadd %isub %igcd %ilcm %ipow %imin %imax %ieven? %iodd? %iinc
    %irem %iquo %idivide %idec %irandom
    %feq %flt %fle %fgt %fge %fmul %fadd %fsub %fexp %fmin %fmax %float?
    %fpowi %fdiv %fneg %i2f %fminval %fmaxval %fbase %fprec %ftrunc
    %fsqrt %fpowf %flog %flog2 %flog10 %fmanexp
    %fsin  %fcos  %ftan  %fcot  
    %fasin %facos %fatan %facot 
    %fsinh  %fcosh  %ftanh 
    %fasinh %facosh %fatanh
    %val2z %z2val %zlit %zreal %zimag
    %zexp %zlog %zsin %zcos %ztan %zasin %zacos %zatan
    %zsinh %zcosh %ztanh %zasinh %zacosh %zatanh
    %nil %pair %list %pair? %lconcat %llength %lfirst %lsecond %lthird
    %lreverse %lempty? %hash %ismall? %string? %f2s
    %ccst %ccstmax %ceq %clt %cle %cgt %cge %c2i %i2c %s2c %c2s %cup %cdown
    %sname
    %strlength %streq %i2s %schar %strlt %strconc %strcopy
    %vector %aref %vref %vlength %bytevec2str %str2bytevec
    %bitvector
    %bitvecnot %bitvecand %bitvecnand %bivecor %bitvecnor %bitvecxor
    %bitveccopy %bitvecconc %bitveclength %bitvecref %bitveceq %bitveclt
    %before? %equal %sptreq %ident? %property %tref)

++ List of simple VM operators
$simpleVMoperators == 
  append($VMsideEffectFreeOperators,
    ['STRINGIMAGE,'FUNCALL,'%gensym, '%lreverse!,
      '%strstc,"MAKE-FULL-CVEC"])

++ Return true if the `form' is semi-simple with respect to
++ to the list of operators `ops'.
semiSimpleRelativeTo?(form,ops) ==
  atomic? form => true
  not symbol?(form.op) or not symbolMember?(form.op,ops) => false
  form.op is '%when =>
    and/[sideEffectFree? p and semiSimpleRelativeTo?(c,ops)
           for [p,c] in form.args]
  and/[semiSimpleRelativeTo?(f,ops) for f in form.args]

++ Return true if `form' os a side-effect free form.  
sideEffectFree? form ==
  semiSimpleRelativeTo?(form,$VMsideEffectFreeOperators)

++ Return true if `form' is a simple VM form.
++ See $simpleVMoperators for the definition of simple operators.
isSimpleVMForm form ==
  semiSimpleRelativeTo?(form,$simpleVMoperators)

++ Return true if `form' is a VM form whose evaluation does not depend
++ on the program point where it is evaluated. 
isFloatableVMForm: %Code -> %Boolean
isFloatableVMForm form ==
  atom form => form isnt "$"
  form is ["QUOTE",:.] => true
  symbolMember?(form.op, $simpleVMoperators) and
    "and"/[isFloatableVMForm arg for arg in form.args]
    

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
  IDENTP form => [form]
  form isnt [op,:args] => nil  
  op is "QUOTE" => nil
  vars := union/[findVMFreeVars arg for arg in args]
  atom op => vars
  union(findVMFreeVars op,vars)

++ Return true is `var' is the left hand side of an assignment
++ in `form'.
varIsAssigned(var,form) ==
  atomic? form => false
  form is [op,var',:.] and op in '(%LET LETT SETQ %store) =>
    symbol? var' => var' = var   -- whole var is assigned
    var' is [.,=var]             -- only part of it is modified
  or/[varIsAssigned(var,f) for f in form]


++ Return the list of variables referenced in `expr'.  
dependentVars expr == main(expr,nil) where
  main(x,vars) ==
    IDENTP x =>
      symbolMember?(x,vars) => vars
      [x,:vars]
    atomic? x => vars
    x' :=
      cons? x.op => x
      x.args
    for y in x' repeat
      vars := main(y,vars)
    vars

++ Subroutine of optLET and optBind.  Return true if the variable `var' locally
++ defined in a binding form can be safely replaced by its initalization
++ `expr' in the `body' of the binding form.
canInlineVarDefinition(var,expr,body) ==
  -- FIXME: We should not be inlining a side-effecting initializer.
  -- If the variable is assigned to, that is a no no.
  varIsAssigned(var,body) => false
  -- Similarly, if the initial value depends on variables that are
  -- side-effected latter, it is alos a no no.
  or/[varIsAssigned(x,body) for x in dependentVars expr] => false
  -- Conversatively preserve order of inialization
  cons? body and body.op in '(%bind LET %loop %collect) => false
  -- Linearly used internal temporaries should be replaced, and
  -- so should side-effet free initializers for linear variables.
  usageCount := numOfOccurencesOf(var,body)
  usageCount < 2 and (gensym? var or sideEffectFree? expr) => true
  -- If the initializer is a variable and the body is
  -- a series of choices with side-effect free predicates, then
  -- no harm is done by removing the local `var'.
  IDENTP expr and body is ['%when,:branches] =>
    and/[sideEffectFree? pred for [pred,:.] in branches]
  false

++ Implement simple-minded LET-inlining.  It seems we can't count
++ on Lisp implementations to do this simple transformation.
++ This transformation will probably be more effective when all
++ type informations are still around.   Which is why we should
++ have a type directed compilation throughout. 
optLET u ==
  -- Hands off non-simple cases.
  u isnt ["LET",inits,body] => u
  -- Inline functionally used local variables with their initializers.
  inits := [:newInit for (init := [var,expr]) in inits] where 
    newInit() ==
      canInlineVarDefinition(var,expr,body) =>
        body := substitute(expr,var,body)
        nil  -- remove this initialization
      [init] -- otherwwise keep it.
  null inits => body
  u.rest.first := inits
  u.rest.rest.first := body
  -- Avoid initialization forms that may not be floatable.
  not(and/[isFloatableVMForm init for [.,init] in inits]) => u
  -- Identity function.
  inits is [[=body,init]] => init
  -- Handle only most trivial operators.
  body isnt [op,:args] => u
  -- Well, with case-patterns, it is beneficial to try a bit harder
  -- with conditional forms.
  op is '%when =>
    continue := true      -- shall be continue let-inlining?
    -- Since we do a single pass, we can't reuse the inits list
    -- as we may find later that we can't really inline into
    -- all forms due to excessive conversatism.  So we build a 
    -- substitution list ahead of time.
    substPairs := [[var,:init] for [var,init] in inits]
    for clauses in tails args while continue repeat
      clause := first clauses
      -- we do not attempt more complicated clauses yet.
      clause isnt [test,stmt] => continue := false
      -- Stop inlining at least one test is not simple
      not isSimpleVMForm test => continue := false
      clause.first := applySubst(substPairs,test)
      isSimpleVMForm stmt =>
        clause.rest.first := applySubst(substPairs,stmt)
      continue := false
    continue => body
    u
  not symbolMember?(op,$simpleVMoperators) => u
  not(and/[atomic? arg for arg in args]) => u
  -- Inline only if all parameters are used.  Get cute later.
  not(and/[symbolMember?(x,args) for [x,.] in inits]) => u
  -- Munge inits into list of dotted-pairs.  Lovely Lisp.
  for defs in tails inits repeat
    def := first defs
    atom def => systemErrorHere ["optLET",def] -- cannot happen
    def.rest := second def
  applySubst(inits,body)

optBind form ==
  form isnt ['%bind,inits,.] => form           -- accept only simple bodies
  ok := true
  while ok and inits ~= nil repeat
    [var,expr] := first inits
    usedSymbol?(var,rest inits) => ok := false -- no dependency, please.
    body := third form
    canInlineVarDefinition(var,expr,body) and isSimpleVMForm expr =>
      third(form) := substitute!(expr,var,body)
      inits := rest inits
    ok := false
  null inits => third form                        -- no local var left
  second(form) := inits
  form

optTry form ==
  form isnt ['%try,e,hs,f] or not(isFloatableVMForm e) or f ~= nil => form
  e

optList form ==
  form is ['%list] => '%nil
  form

optCollectVector form ==
  [.,eltType,:iters,body] := form
  fromList := false      -- are we drawing from a list?
  vecSize := nil         -- size of vector
  index := nil           -- loop/vector index.
  for iter in iters while not fromList repeat
    [op,:.] := iter
    op in '(SUCHTHAT WHILE UNTIL) => fromList := true
    op in '(IN ON) => vecSize := [["SIZE",third iter],:vecSize]
    op in '(STEP ISTEP) =>
      -- pick a loop variable that we can use as the loop index.
      [.,var,lo,inc,:etc] := iter
      if lo = 0 and inc = 1 then
        index := var
      if [hi] := etc then
        sz :=
          inc = 1 =>
            lo = 1 => hi
            lo = 0 => MKQSADD1 hi
            MKQSADD1 ["-",hi,lo]
          lo = 1 => ["/",hi,inc]
          lo = 0 => ["/",MKQSADD1 hi,inc]
          ["/",["-",MKQSADD1 hi, lo],inc]
        vecSize := [sz, :vecSize]
    systemErrorHere ["optCollectVector", iter]
  -- if we draw from a list, then just build a list and convert to vector.
  fromList => 
    ["homogeneousListToVector",["getVMType",eltType], ['%collect,:iters,body]]
  vecSize = nil => systemErrorHere ["optCollectVector",form]
  -- get the actual size of the vector.
  vecSize :=
    vecSize is [hi] => hi
    ["MIN",:reverse! vecSize]
  -- if no suitable loop index was found, introduce one.
  if index = nil then
    index := gensym()
    iters := [:iters,['STEP,index,0,1]]
  vec := gensym()
  ['%bind,[[vec,["makeSimpleArray",["getVMType",eltType],vecSize]]],
    ['%loop,:iters,["setSimpleArrayEntry",vec,index,body],vec]]

++ Translate retraction of a value denoted by `e' to sub-domain `m'
++ defined by predicate `pred',
optRetract ["%retract",e,m,pred] ==
  atom e =>
    cond := simplifyVMForm substitute(e,"#1",pred)
    cond is '%true => e
    ["check-subtype",cond,MKQ m,e]
  g := gensym()
  ['%bind,[[g,e]],["check-subtype",substitute(g,"#1",pred),MKQ m,g]]


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

optIeq(x is ['%ieq,a,b]) ==
  integer? a and integer? b =>
    a = b => '%true
    '%false
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
  integer? a and a = 0 => b
  integer? b and b = 0 => a
  x

optIsub(x is ['%isub,a,b]) ==
  integer? a and integer? b => a - b
  integer? a and a = 0 => ['%ineg,b]
  integer? b and b = 0 => a
  x

optImul(x is ['%imul,a,b]) ==
  integer? a and integer? b => a * b
  integer? a and a = 1 => b
  integer? b and b = 1 => a
  x

optIneg(x is ['%ineg,a]) ==
  integer? a => -a
  x

optIrem(x is ['%irem,a,b]) ==
  integer? a and integer? b => a rem b
  x

optIquo(x is ['%iquo,a,b]) ==
  integer? a and integer? b => a quo b
  x

-- Boolean <-> bit conversion.
opt2bit(x is ['%2bit,a]) ==
  a is '%true => 1
  a is '%false => 0
  x

opt2bool(x is ['%2bool,a]) ==
  integer? a =>
    a = 1 => '%true
    '%false
  x

--%  
--% optimizer hash table
--%
 
for x in '( (%call         optCall) _
           (SEQ          optSEQ)_
           (LET          optLET)_
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
           (%isub        optIsub)_
           (%irem        optIrem)_
           (%iquo        optIquo)_
           (%imul        optImul)_
           (%2bit        opt2bit)_
           (%2bool       opt2bool)_
           (%list        optList)_
           (SPADCALL     optSPADCALL)_
           (_|           optSuchthat)_
           (CATCH        optCatch)_
           (%when        optCond)_
           (%retract     optRetract)_
           (%CollectV    optCollectVector)_
           (mkRecord     optMkRecord)_
           (RECORDELT    optRECORDELT)_
           (SETRECORDELT optSETRECORDELT)_
           (RECORDCOPY   optRECORDCOPY)) _
   repeat property(first x,'OPTIMIZE) := second x
       --much quicker to call functions if they have an SBC

