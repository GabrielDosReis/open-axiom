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
  opOf dom in $optimizableConstructorNames

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
  fn.first := "getShellEntry"
  x.rest := [:args,fn]
  x

--% OPTIMIZER

++ Change (%LET id expr) to (%store id expr) if `id' is being
++ updated as opposed to being defined. `vars' is the list of
++ all variable definitions in scope.
changeVariableDefinitionToStore(form,vars) ==
  isAtomicForm form => nil
  form is ['%LET,v,expr] =>
    changeVariableDefinitionToStore(expr,vars)
    if v in vars then form.op := '%store
  for x in form repeat
    changeVariableDefinitionToStore(x,vars)
    x is ['%LET,v,:.] and not (v in vars) =>
      vars := [v,:vars]

++ Return true if `x' contains control transfer to a point outside itself.
jumpToToplevel? x ==
  isAtomicForm x => false
  op := x.op
  op = 'SEQ => CONTAINED('THROW,x.args)
  op in '(EXIT THROW %leave) => true
  or/[jumpToToplevel? x' for x' in x]

++ Return true if `form' is just one assignment expression.
singleAssignment? form ==
  form is ['%LET,.,rhs] and not CONTAINED('%LET,rhs)

++ Turns `form' into a `%bind'-expression if it starts with a
++ a sequence of first-time variable definitions.
groupVariableDefinitions form ==
  isAtomicForm form => form
  form isnt ['SEQ,:stmts,['EXIT,val]] => form
  defs := nil
  for x in stmts while singleAssignment? x  repeat
    defs := [x.args,:defs]
  defs = nil or jumpToToplevel? defs => form
  stmts := drop(#defs,stmts)
  expr :=
    stmts = nil => val
    ['SEQ,:stmts,['EXIT,val]]
  ['%bind,nreverse defs,expr]

optimizeFunctionDef(def) ==
  if $reportOptimization then
    sayBrightlyI bright '"Original LISP code:"
    pp def
 
  def' := optimize COPY def
 
  if $reportOptimization then
    sayBrightlyI bright '"Optimized LISP code:"
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

++ Like `optimize', except that non-atomic form may be reduced to
++ to atomic forms.  In particular, the address of the input may
++ not be the same as that of the output.
simplifyVMForm x ==
  first optimize [x]

optimize x ==
  (opt x; x) where
    opt x ==
      atom x => nil
      (y:= first x)='QUOTE => nil
      y='CLOSEDFN => nil
      y is [["XLAM",argl,body],:a] =>
        optimize rest x
        argl = "ignore" => x.first := body
        if not (# argl<= # a) then
          SAY '"length mismatch in XLAM expression"
          PRETTYPRINT y
        x.first := optimize optXLAMCond SUBLIS(pairList(argl,a),body)
      atom y => optimize rest x
      if first y="IF" then (x.first := optIF2COND y; y:= first x)
      op:= GETL(subrname first y,"OPTIMIZE") =>
        (optimize rest x; x.first := FUNCALL(op,optimize first x))
      x.first := optimize first x
      optimize rest x
 
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
  atom s or first s='QUOTE => nil
  s is ["THROW", =g,u] =>
    changeThrowToGo(u,g)
    s.first := "PROGN"
    s.rest := [["%LET",second g,u],["GO",second g]]
  changeThrowToGo(first s,g)
  changeThrowToGo(rest s,g)

++ Change any `(THROW tag (%return expr))' in x to just
++ `(%return expr) since a return-operator transfer control
++ out of the function body anyway.  Similarly, transform
++ reudant `(THROW tag (THROW tag expr))' to `(THROW tag expr)'.
removeNeedlessThrow x ==
  isAtomicForm x => x
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
    ["CATCH",y,a]:= optimize x
  if hasNoThrows(a,g) then
    x.first := first a
    x.rest := rest a
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
  -- destructively optimizes this new x
  x:= optimize [u]
  -- next should happen only as result of macro expansion
  atom first x => first x
  [fn,:a]:= first x
  atom fn => (x.rest := a; x.first := fn)
  fn is ["applyFun",name] =>
    (x.first := "SPADCALL"; x.rest := [:a,name]; x)
  fn is [q,R,n] and q in '(getShellEntry ELT QREFELT CONST) =>
    not $bootStrapMode and (w:= optCallSpecially(q,x,n,R)) => w
    q="CONST" => ["spadConstant",R,n]
    emitIndirectCall(fn,a,x)
  systemErrorHere ["optCall",x]
 
optCallSpecially(q,x,n,R) ==
    y:= LASSOC(R,$specialCaseKeyList) => optSpecialCall(x,y,n)
    optimizableDomain? R => optSpecialCall(x,R,n)
    (y:= get(R,"value",$e)) and
      optimizableDomain? y.expr =>
        optSpecialCall(x,y.expr,n)
    (
      (y:= lookup(R,$getDomainCode)) and ([op,y,prop]:= y) and
        (yy:= LASSOC(y,$specialCaseKeyList)) =>
         optSpecialCall(x,[op,yy,prop],n)) where
            lookup(a,l) ==
              null l => nil
              [l',:l]:= l
              l' is ["%LET", =a,l',:.] => l'
              lookup(a,l)
    nil
 
optCallEval u ==
  u is ["List",:.] => List Integer()
  u is ["Vector",:.] => Vector Integer()
  u is ["PrimitiveArray",:.] => PrimitiveArray Integer()
  u is ["FactoredForm",:.] => FactoredForm Integer()
  u is ["Matrix",:.] => Matrix Integer()
  eval u
 
optCons (x is ["CONS",a,b]) ==
  a="NIL" =>
    b='NIL => (x.first := 'QUOTE; x.rest := ['NIL,:'NIL]; x)
    b is ['QUOTE,:c] => (x.first := 'QUOTE; x.rest := ['NIL,:c]; x)
    x
  a is ['QUOTE,a'] =>
    b='NIL => (x.first := 'QUOTE; x.rest := [a',:'NIL]; x)
    b is ['QUOTE,:c] => (x.first := 'QUOTE; x.rest := [a',:c]; x)
    x
  x
 
optSpecialCall(x,y,n) ==
  yval := optCallEval y
  CAAAR x="CONST" =>
    KAR yval.n = function Undef =>
      keyedSystemError("S2GE0016",['"optSpecialCall",
        '"invalid constant"])
    MKQ yval.n
  fn := getFunctionReplacement compileTimeBindingOf first yval.n =>
    x.rest := CDAR x
    x.first := fn
    if fn is ["XLAM",:.] then x := simplifyVMForm x
    x is ["EQUAL",:args] => RPLACW(x,DEF_-EQUAL args)
                --DEF-EQUAL is really an optimiser
    x
  [fn,:a]:= first x
  emitIndirectCall(fn,a,x)
 
compileTimeBindingOf u ==
  null(name:= BPINAME u)  => keyedSystemError("S2OO0001",[u])
  name="Undef" => MOAN "optimiser found unknown function"
  name
 
optMkRecord ["mkRecord",:u] ==
  u is [x] => ["LIST",x]
  #u=2 => ["CONS",:u]
  ["VECTOR",:u]
 
optCond (x is ['COND,:l]) ==
  if l is [a,[aa,b]] and TruthP aa and b is ["COND",:c] then
    x.rest.rest := c
  if l is [[p1,:c1],[p2,:c2],:.] then
    if (p1 is ['%not,=p2]) or (p2 is ['%not,=p1]) then
      l:=[[p1,:c1],['%true,:c2]]
      x.rest := l
    c1 is ['NIL] and p2 = '%true and first c2 = '%true =>
      p1 is ['%not,p1']=> return p1'
      return ['%not,p1]
  l is [[p1,:c1],[p2,:c2],[p3,:c3]] and TruthP p3 =>
    EqualBarGensym(c1,c3) =>
      ["COND",[['%or,p1,['%not,p2]],:c1],['%true,:c2]]
    EqualBarGensym(c1,c2) => ["COND",[['%or,p1,p2],:c1],['%true,:c3]]
    x
  for y in tails l repeat
    while y is [[a1,c1],[a2,c2],:y'] and EqualBarGensym(c1,c2) repeat
      a:=['%or,a1,a2]
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
 
--Called early, to change IF to COND
 
optIF2COND ["IF",a,b,c] ==
  b is "%noBranch" => ["COND",[['%not,a],c]]
  c is "%noBranch" => ["COND",[a,b]]
  c is ["IF",:.] => ["COND",[a,b],:rest optIF2COND c]
  c is ["COND",:p] => ["COND",[a,b],:p]
  ["COND",[a,b],['%true,c]]
 
optXLAMCond x ==
  x is ["COND",u:= [p,c],:l] =>
    (p = '%true => c; ["COND",u,:optCONDtail l])
  atom x => x
  x.first := optXLAMCond first x
  x.rest := optXLAMCond rest x
  x
 
optCONDtail l ==
  null l => nil
  [frst:= [p,c],:l']:= l
  p = '%true => [['%true,c]]
  null rest l => [frst,['%true,["CondError"]]]
  [frst,:optCONDtail l']

++ Determine whether the symbol `g' is the name of a temporary that
++ can be replaced in the form `x', if it is of linear usage and not
++ the name of a program point.  The latter occurs when THROW forms
++ are changed to %LET form followed by a GO form -- see optCatch.
replaceableTemporary?(g,x) ==
  GENSYMP g and numOfOccurencesOf(g,x) < 2 and not jumpTarget?(g,x) where
    jumpTarget?(g,x) ==
      isAtomicForm x => false
      x is ['GO,=g] => true
      or/[jumpTarget?(g,x') for x' in x]

optSEQ ["SEQ",:l] ==
  tryToRemoveSEQ SEQToCOND getRidOfTemps splicePROGN l where
    splicePROGN l ==
      isAtomicForm l => l
      l is [["PROGN",:stmts],:l'] => [:stmts,:l']
      l.rest := splicePROGN rest l
    getRidOfTemps l ==
      null l => nil
      l is [["%LET",g,x],:r] and replaceableTemporary?(g,r) =>
        getRidOfTemps substitute(x,g,r)
      first l="/throwAway" => getRidOfTemps rest l
      --this gets rid of unwanted labels generated by declarations in SEQs
      [first l,:getRidOfTemps rest l]
    SEQToCOND l ==
      transform:= [[a,b] for x in l while (x is ["COND",[a,["EXIT",b]]])]
      before:= take(#transform,l)
      aft:= after(l,before)
      null before => ["SEQ",:aft]
      null aft => ["COND",:transform,'(%true (conderr))]
      ["COND",:transform,['%true,optSEQ ["SEQ",:aft]]]
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
  ["QVELT",name,ind]
 
optSETRECORDELT ["SETRECORDELT",name,ind,len,expr] ==
  len=1 =>
    ind=0 => ["PROGN",["RPLACA",name,expr],['%head,name]]
    keyedSystemError("S2OO0002",[ind])
  len=2 =>
    ind=0 => ["PROGN",['%store,['%head,name],expr],['%head,name]]
    ind=1 => ["PROGN",['%store,['%tail,name],expr],['%tail,name]]
    keyedSystemError("S2OO0002",[ind])
  ["QSETVELT",name,ind,expr]
 
optRECORDCOPY ["RECORDCOPY",name,len] ==
  len=1 => ["LIST",['%head,name]]
  len=2 => ["CONS",['%head,name],['%tail,name]]
  ["REPLACE",["MAKE_-VEC",len],name]
 
optSuchthat [.,:u] == ["SUCHTHAT",:u]
 
optQSMINUS u ==
  u is ['QSMINUS,v] =>
    integer? v => -v
    u
  u
 
++ List of VM side effect free operators.
$VMsideEffectFreeOperators ==
  '(CAR CDR LENGTH SIZE EQUAL EQL EQ NOT NULL OR AND
    SPADfirst QVELT _+ _- _* _< _= _<_= _> _>_= ASH INTEGER_-LENGTH
     QEQCAR QCDR QCAR IDENTP SYMBOLP
      GREATERP ZEROP ODDP FLOAT_-RADIX FLOAT FLOAT_-SIGN
       CGREATERP GGREATERP CHAR GET BVEC_-GREATER %false %true
        %and %or %not %peq %ieq %ilt %ile %igt %ige %head %tail %integer?
         %beq %blt %ble %bgt %bge %bitand %bitior %bitnot %bcompl
          %imul %iadd %isub %igcd %ilcm %ipow %imin %imax %ieven? %iodd? %iinc
           %feq %flt %fle %fgt %fge %fmul %fadd %fsub %fexp %fmin %fmax %float?
            %fpow %fdiv %fneg %i2f %fminval %fmaxval %fbase %fprec %ftrunc
             %nil %pair? %lconcat %llength %lfirst %lsecond %lthird
              %lreverse %lempty? %hash %ismall? %string? %f2s
               %ceq %clt %cle %cgt %cge %c2i %i2c %sname
                %vref %vlength %before?)

++ List of simple VM operators
$simpleVMoperators == 
  append($VMsideEffectFreeOperators,
    ['CONS,'LIST,'VECTOR,'STRINGIMAGE,'FUNCALL,'%gensym, '%lreverse_!,
      "MAKE-FULL-CVEC","BVEC-MAKE-FULL","COND"])

++ Return true if the `form' is semi-simple with respect to
++ to the list of operators `ops'.
semiSimpleRelativeTo?(form,ops) ==
  isAtomicForm form => true
  form isnt [op,:args] or not MEMQ(op,ops) => false
  and/[semiSimpleRelativeTo?(f,ops) for f in args]

++ Return true if `form' is a simple VM form.
++ See $simpleVMoperators for the definition of simple operators.
isSimpleVMForm form ==
  semiSimpleRelativeTo?(form,$simpleVMoperators)

++ Return true if `form' is a VM form whose evaluation does not depend
++ on the program point where it is evaluated. 
isFloatableVMForm: %Code -> %Boolean
isFloatableVMForm form ==
  atom form => form ~= "$"
  form is ["QUOTE",:.] => true
  MEMQ(first form, $simpleVMoperators) and
    "and"/[isFloatableVMForm arg for arg in rest form]
    

++ Return true if the VM form `form' is one that we certify to 
++ evaluate to a (compile time) constant.  Note that this is a
++ fairly conservative approximation of compile time constants.
isVMConstantForm: %Code -> %Boolean
isVMConstantForm form ==
  integer? form or string? form => true
  form isnt [op,:args] => false
  MEMQ(op,$VMsideEffectFreeOperators) and 
    "and"/[isVMConstantForm arg for arg in args]

++ Return the set of free variables in the VM form `form'.
findVMFreeVars form ==
  IDENTP form => [form]
  form isnt [op,:args] => nil  
  op = "QUOTE" => nil
  vars := union/[findVMFreeVars arg for arg in args]
  atom op => vars
  union(findVMFreeVars op,vars)

++ Return true is `var' is the left hand side of an assignment
++ in `form'.
varIsAssigned(var,form) ==
  isAtomicForm form => false
  form is [op,=var,:.] and op in '(%LET LETT SETQ %store) => true
  or/[varIsAssigned(var,f) for f in form]

++ Subroutine of optLET.  Return true if the variable `var' locally
++ defined in the LET-form can be safely replaced by its initalization
++ `expr' in the `body' of the LET-form.
canInlineVarDefinition(var,expr,body) ==
  varIsAssigned(var,body) => false
  numOfOccurencesOf(var,body) < 2 => true
  atom expr and not varIsAssigned(expr,body)

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
  op = "COND" =>
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
      clause.first := SUBLIS(substPairs,test)
      isSimpleVMForm stmt =>
        clause.rest.first := SUBLIS(substPairs,stmt)
      continue := false
    continue => body
    u
  not MEMQ(op,$simpleVMoperators) => u
  not(and/[isAtomicForm arg for arg in args]) => u
  -- Inline only if all parameters are used.  Get cute later.
  not(and/[MEMQ(x,args) for [x,.] in inits]) => u
  -- Munge inits into list of dotted-pairs.  Lovely Lisp.
  for defs in tails inits repeat
    def := first defs
    atom def => systemErrorHere ["optLET",def] -- cannot happen
    def.rest := second def
  SUBLIS(inits,body)

optLET_* form ==
  form isnt ["LET*",:.] => form
  ok := true
  while ok for [[var,.],:inits] in tails second form repeat
    if CONTAINED(var,inits) then ok := false
  not ok => form
  form.first := "LET"
  optLET form

optBind form ==
  form isnt ['%bind,inits,.] => form           -- accept only simple bodies
  ok := true
  while ok and inits ~= nil repeat
    [var,expr] := first inits
    usedSymbol?(var,rest inits) => ok := false -- no dependency, please.
    body := third form
    canInlineVarDefinition(var,expr,body) and isSimpleVMForm expr =>
      third(form) := substitute_!(expr,var,body)
      inits := rest inits
    ok := false
  null inits => third form                        -- no local var left
  second(form) := inits
  form

optLIST form ==
  form is ["LIST"] => nil
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
    ["MIN",:nreverse vecSize]
  -- if no suitable loop index was found, introduce one.
  if index = nil then
    index := gensym()
    iters := [:iters,['STEP,index,0,1]]
  vec := gensym()
  ["LET",[[vec,["makeSimpleArray",["getVMType",eltType],vecSize]]],
    ['%loop,:iters,["setSimpleArrayEntry",vec,index,body],vec]]

++ Translate retraction of a value denoted by `e' to sub-domain `m'
++ defined by predicate `pred',
optRetract ["%retract",e,m,pred] ==
  atom e =>
    cond := simplifyVMForm substitute(e,"#1",pred)
    cond = '%true => e
    ["check-subtype",cond,MKQ m,e]
  g := gensym()
  ["LET",[[g,e]],["check-subtype",substitute(g,"#1",pred),MKQ m,g]]


--%  Boolean expression transformers

optNot(x is ['%not,a]) ==
  a = '%true => '%false
  a = '%false => '%true
  a is ['%not,b] => b
  x

optAnd(x is ['%and,a,b]) ==
  a = '%true => b
  b = '%true => a
  a = '%false => '%false
  x

optOr(x is ['%or,a,b]) ==
  a = '%false => b
  b = '%false => a
  a = '%true => '%true
  x

optIeq(x is ['%ieq,a,b]) ==
  integer? a and integer? b =>
    a = b => '%true
    '%false
  x

optIlt(x is ['%ilt,a,b]) ==
  integer? a and integer? b =>
    a < b => '%true
    '%false
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

--%  
--% optimizer hash table
--%
 
for x in '( (%call         optCall) _
           (SEQ          optSEQ)_
           (LET          optLET)_
           (LET_*        optLET_*)_
           (%bind        optBind)_
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
           (%imul        optImul)_
           (LIST         optLIST)_
           (QSMINUS      optQSMINUS)_
           (SPADCALL     optSPADCALL)_
           (_|           optSuchthat)_
           (CATCH        optCatch)_
           (COND         optCond)_
           (%retract     optRetract)_
           (%CollectV    optCollectVector)_
           (mkRecord     optMkRecord)_
           (RECORDELT    optRECORDELT)_
           (SETRECORDELT optSETRECORDELT)_
           (RECORDCOPY   optRECORDCOPY)) _
   repeat property(first x,'OPTIMIZE) := second x
       --much quicker to call functions if they have an SBC

