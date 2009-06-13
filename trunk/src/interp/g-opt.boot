-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2009, Gabriel Dos Reis.
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


import def
namespace BOOT

--% 

$optimizableConstructorNames := $SystemInlinableConstructorNames

--%

++ return the template of the instantiating functor for
++ the domain form `dom'.
getDomainTemplate dom ==
  atom dom => nil
  getInfovec first dom

++ Emit code for an indirect call to domain-wide Spad function.  
++ This is usually the case for exported functions.
emitIndirectCall(fn,args,x) ==
  rplac(first x, "SPADCALL")
  rplac(first fn,"getShellEntry")
  rplac(rest x, [:args,fn])
  x

--% OPTIMIZER
 
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
          rplac(first x,"RETURN")
          rplac(rest x,replaceThrowByReturn(u,g))
        atom x => nil
        replaceThrowByReturn(first x,g)
        replaceThrowByReturn(rest x,g)
  [name,[slamOrLam,args,body']]
 
optimize x ==
  (opt x; x) where
    opt x ==
      atom x => nil
      (y:= first x)='QUOTE => nil
      y='CLOSEDFN => nil
      y is [["XLAM",argl,body],:a] =>
        optimize rest x
        argl = "ignore" => RPLAC(first x,body)
        if not (LENGTH argl<=LENGTH a) then
          SAY '"length mismatch in XLAM expression"
          PRETTYPRINT y
        RPLAC(first x,optimize optXLAMCond SUBLIS(pairList(argl,a),body))
      atom y =>
        optimize rest x
      if first y="IF" then (RPLAC(first x,optIF2COND y); y:= first x)
      op:= GETL(subrname first y,"OPTIMIZE") =>
        (optimize rest x; RPLAC(first x,FUNCALL(op,optimize first x)))
      RPLAC(first x,optimize first x)
      optimize rest x
 
subrname u ==
  IDENTP u => u
  COMPILED_-FUNCTION_-P u or MBPIP u => BPINAME u
  nil
 
optCatch (x is ["CATCH",g,a]) ==
  $InteractiveMode => x
  atom a => a
  if a is ["SEQ",:s,["THROW", =g,u]] then
    changeThrowToExit(s,g) where
      changeThrowToExit(s,g) ==
        atom s or MEMQ(first s,'(QUOTE SEQ REPEAT COLLECT)) => nil
        s is ["THROW", =g,:u] => (rplac(first s,"EXIT"); rplac(rest s,u))
        changeThrowToExit(first s,g)
        changeThrowToExit(rest s,g)
    rplac(rest a,[:s,["EXIT",u]])
    ["CATCH",y,a]:= optimize x
  if hasNoThrows(a,g) where
       hasNoThrows(a,g) ==
         a is ["THROW", =g,:.] => false
         atom a => true
         hasNoThrows(first a,g) and hasNoThrows(rest a,g)
     then (rplac(first x,first a); rplac(rest x,rest a))
   else
    changeThrowToGo(a,g) where
      changeThrowToGo(s,g) ==
        atom s or first s='QUOTE => nil
        s is ["THROW", =g,u] =>
          changeThrowToGo(u,g)
          rplac(first s,"PROGN")
          rplac(rest s,[["%LET",CADR g,u],["GO",CADR g]])
        changeThrowToGo(first s,g)
        changeThrowToGo(rest s,g)
    rplac(first x,"SEQ")
    rplac(rest x,[["EXIT",a],CADR g,["EXIT",CADR g]])
  x
 
optSPADCALL(form is ['SPADCALL,:argl]) ==
  null $InteractiveMode => form
  -- last arg is function/env, but may be a form
  argl is [:argl,fun] and fun is ["ELT",dom,slot] =>
    optCall ['call,['ELT,dom,slot],:argl]
  form
 
optCall (x is ["call",:u]) ==
  -- destructively optimizes this new x
  x:= optimize [u]
  -- next should happen only as result of macro expansion
  atom first x => first x
  [fn,:a]:= first x
  atom fn => (RPLAC(rest x,a); RPLAC(first x,fn))
  fn is ["applyFun",name] =>
    (RPLAC(first x,"SPADCALL"); RPLAC(rest x,[:a,name]); x)
  fn is [q,R,n] and MEMQ(q,'(getShellEntry ELT QREFELT CONST)) =>
    not $bootStrapMode and (w:= optCallSpecially(q,x,n,R)) => w
    q="CONST" => ["spadConstant",R,n]
    emitIndirectCall(fn,a,x)
  systemErrorHere ["optCall",x]
 
optCallSpecially(q,x,n,R) ==
    y:= LASSOC(R,$specialCaseKeyList) => optSpecialCall(x,y,n)
    MEMQ(KAR R,$optimizableConstructorNames) => optSpecialCall(x,R,n)
    (y:= get(R,"value",$e)) and
      MEMQ(opOf y.expr,$optimizableConstructorNames) =>
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
    b='NIL => (rplac(first x,'QUOTE); rplac(rest x,['NIL,:'NIL]); x)
    b is ['QUOTE,:c] => (rplac(first x,'QUOTE); rplac(rest x,['NIL,:c]); x)
    x
  a is ['QUOTE,a'] =>
    b='NIL => (rplac(first x,'QUOTE); rplac(rest x,[a',:'NIL]); x)
    b is ['QUOTE,:c] => (rplac(first x,'QUOTE); rplac(rest x,[a',:c]); x)
    x
  x
 
optSpecialCall(x,y,n) ==
  yval := optCallEval y
  CAAAR x="CONST" =>
    KAR yval.n = function Undef =>
      keyedSystemError("S2GE0016",['"optSpecialCall",
        '"invalid constant"])
    MKQ yval.n
  fn := GETL(compileTimeBindingOf first yval.n,'SPADreplace) =>
    rplac(rest x,CDAR x)
    rplac(first x,fn)
    if fn is ["XLAM",:.] then x:=first optimize [x]
    x is ["EQUAL",:args] => RPLACW(x,DEF_-EQUAL args)
                --DEF-EQUAL is really an optimiser
    x
  [fn,:a]:= first x
  emitIndirectCall(fn,a,x)
 
compileTimeBindingOf u ==
  NULL(name:= BPINAME u)  => keyedSystemError("S2OO0001",[u])
  name="Undef" => MOAN "optimiser found unknown function"
  name
 
optMkRecord ["mkRecord",:u] ==
  u is [x] => ["LIST",x]
  #u=2 => ["CONS",:u]
  ["VECTOR",:u]
 
optCond (x is ['COND,:l]) ==
  if l is [a,[aa,b]] and TruthP aa and b is ["COND",:c] then
    RPLACD(rest x,c)
  if l is [[p1,:c1],[p2,:c2],:.] then
    if (p1 is ["NOT",=p2]) or (p2 is ["NOT",=p1]) then
      l:=[[p1,:c1],['(QUOTE T),:c2]]
      RPLACD( x,l)
    c1 is ['NIL] and p2 = '(QUOTE T) and first c2 = '(QUOTE T) =>
      p1 is ["NOT",p1']=> return p1'
      return ["NOT",p1]
  l is [[p1,:c1],[p2,:c2],[p3,:c3]] and TruthP p3 =>
    EqualBarGensym(c1,c3) =>
      ["COND",[["OR",p1,["NOT",p2]],:c1],[['QUOTE,true],:c2]]
    EqualBarGensym(c1,c2) => ["COND",[["OR",p1,p2],:c1],[['QUOTE,true],:c3]]
    x
  for y in tails l repeat
    while y is [[a1,c1],[a2,c2],:y'] and EqualBarGensym(c1,c2) repeat
      a:=['OR,a1,a2]
      RPLAC(first first y,a)
      RPLAC(rest y,y')
  x
 
AssocBarGensym(key,l) ==
  for x in l repeat
    PAIRP x =>
      EqualBarGensym(key,CAR x) => return x
 
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
  b is "%noBranch" => ["COND",[["NOT",a],c]]
  c is "%noBranch" => ["COND",[a,b]]
  c is ["IF",:.] => ["COND",[a,b],:rest optIF2COND c]
  c is ["COND",:p] => ["COND",[a,b],:p]
  ["COND",[a,b],[$true,c]]
 
optXLAMCond x ==
  x is ["COND",u:= [p,c],:l] =>
    (optPredicateIfTrue p => c; ["COND",u,:optCONDtail l])
  atom x => x
  RPLAC(first x,optXLAMCond first x)
  RPLAC(rest x,optXLAMCond rest x)
  x
 
optPredicateIfTrue p ==
  p is ['QUOTE,:.] => true
  p is [fn,x] and MEMQ(fn,$BasicPredicates) and FUNCALL(fn,x) => true
  nil
 
optCONDtail l ==
  null l => nil
  [frst:= [p,c],:l']:= l
  optPredicateIfTrue p => [[$true,c]]
  null rest l => [frst,[$true,["CondError"]]]
  [frst,:optCONDtail l']
 
optSEQ ["SEQ",:l] ==
  tryToRemoveSEQ SEQToCOND getRidOfTemps l where
    getRidOfTemps l ==
      null l => nil
      l is [["%LET",g,x,:.],:r] and GENSYMP g and 2>numOfOccurencesOf(g,r) =>
        getRidOfTemps substitute(x,g,r)
      first l="/throwAway" => getRidOfTemps rest l
      --this gets rid of unwanted labels generated by declarations in SEQs
      [first l,:getRidOfTemps rest l]
    SEQToCOND l ==
      transform:= [[a,b] for x in l while (x is ["COND",[a,["EXIT",b]]])]
      before:= take(#transform,l)
      aft:= after(l,before)
      null before => ["SEQ",:aft]
      null aft => ["COND",:transform,'((QUOTE T) (conderr))]
      true => ["COND",:transform,['(QUOTE T),optSEQ ["SEQ",:aft]]]
    tryToRemoveSEQ l ==
      l is ["SEQ",[op,a]] and MEMQ(op,'(EXIT RETURN THROW)) => a
      l
 
optRECORDELT ["RECORDELT",name,ind,len] ==
  len=1 =>
    ind=0 => ["QCAR",name]
    keyedSystemError("S2OO0002",[ind])
  len=2 =>
    ind=0 => ["QCAR",name]
    ind=1 => ["QCDR",name]
    keyedSystemError("S2OO0002",[ind])
  ["QVELT",name,ind]
 
optSETRECORDELT ["SETRECORDELT",name,ind,len,expr] ==
  len=1 =>
    ind=0 => ["PROGN",["RPLACA",name,expr],["QCAR",name]]
    keyedSystemError("S2OO0002",[ind])
  len=2 =>
    ind=0 => ["PROGN",["RPLACA",name,expr],["QCAR",name]]
    ind=1 => ["PROGN",["RPLACD",name,expr],["QCDR",name]]
    keyedSystemError("S2OO0002",[ind])
  ["QSETVELT",name,ind,expr]
 
optRECORDCOPY ["RECORDCOPY",name,len] ==
  len=1 => ["LIST",["CAR",name]]
  len=2 => ["CONS",["CAR",name],["CDR",name]]
  ["REPLACE",["MAKE_-VEC",len],name]
 
--mkRecordAccessFunction(ind,len) ==
--  stringOfDs:= $EmptyString
--  for i in 0..(ind-1) do stringOfDs:= STRCONC(stringOfDs,PNAME "D")
--  prefix:= if ind=len-1 then PNAME "C" else PNAME "CA"
--  if $QuickCode then prefix:=STRCONC("Q",prefix)
--  INTERN(STRCONC(prefix,stringOfDs,PNAME "R"))
 
optSuchthat [.,:u] == ["SUCHTHAT",:u]
 
optMINUS u ==
  u is ['MINUS,v] =>
    NUMBERP v => -v
    u
  u
 
optQSMINUS u ==
  u is ['QSMINUS,v] =>
    NUMBERP v => -v
    u
  u
 
opt_- u ==
  u is ['_-,v] =>
    NUMBERP v => -v
    u
  u
 
optLESSP u ==
  u is ['LESSP,a,b] =>
    b = 0 => ['MINUSP,a]
    ['GREATERP,b,a]
  u
 
$simpleVMoperators == 
  '(CONS CAR CDR LENGTH SIZE EQUAL EQL EQ NOT NULL OR AND
    SPADfirst QVELT _+ _- _* _< _= ASH INTEGER_-LENGTH
     QEQCAR QCDR QCAR INTEGERP FLOATP STRINGP IDENTP SYMBOLP
      MINUSP GREATERP)

isSimpleVMForm form ==
  isAtomicForm form => true
  form is [op,:args] and MEMQ(op,$simpleVMoperators) 
    and ("and"/[isAtomicForm arg for arg in args])

++ Return true if `form' is a VM form whose evaluation does not depend
++ on the program point where it is evaluated. 
isFloatableVMForm: %Code -> %Boolean
isFloatableVMForm form ==
  atom form => form ^= "$"
  form is ["QUOTE",:.] => true
  MEMQ(first form, $simpleVMoperators) and
    "and"/[isFloatableVMForm arg for arg in rest form]
    

++ Return true if the VM form `form' is one that we certify to 
++ evaluate to a (compile time) constant.  Note that this is a
++ fairly conservative approximation of compile time constants.
isVMConstantForm: %Code -> %Boolean
isVMConstantForm form ==
  INTEGERP form or STRINGP form => true
  form=nil or form=true => true
  form isnt [op,:args] => false
  op = "QUOTE" => true
  MEMQ(op,$simpleVMoperators) and 
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
  form is [op,=var,:.] and MEMQ(op,'(%LET LETT SETQ)) => true
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
  rplac(second u,inits)
  rplac(third u,body)
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
      rplac(first clause,SUBLIS(substPairs,test))
      isSimpleVMForm stmt =>
        rplac(second clause,SUBLIS(substPairs,stmt))
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
    rplac(rest def, second def)
  SUBLIS(inits,body)

lispize x == first optimize [x]
 
--% optimizer hash table
 
for x in '( (call         optCall) _
           (SEQ          optSEQ)_
           (LET          optLET)_
           (MINUS        optMINUS)_
           (QSMINUS      optQSMINUS)_
           (_-           opt_-)_
           (LESSP        optLESSP)_
           (SPADCALL     optSPADCALL)_
           (_|           optSuchthat)_
           (CATCH        optCatch)_
           (COND         optCond)_
           (mkRecord     optMkRecord)_
           (RECORDELT    optRECORDELT)_
           (SETRECORDELT optSETRECORDELT)_
           (RECORDCOPY   optRECORDCOPY)) _
   repeat MAKEPROP(CAR x,'OPTIMIZE, CADR x)
       --much quicker to call functions if they have an SBC

