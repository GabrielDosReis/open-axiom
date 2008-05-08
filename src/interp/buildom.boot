-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2008, Gabriel Dos Reis.
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


-- This file contains the constructors for the domains that cannot
-- be written in ScratchpadII yet.  They are not cached because they
-- are very cheap to instantiate.
-- SMW and SCM July 86
-- These have been substantially modified to work with the new
-- runtime system. 
-- GDR, March 2008.

import sys_-macros
namespace BOOT

$noCategoryDomains == '(Mode SubDomain)
$nonLisplibDomains == APPEND($Primitives,$noCategoryDomains)

++ Category ancestors for Record, Union, Mapping, and Enumeration domains.
$commonCategoryAncestors ==
  ['(SetCategory), '(BasicType), '(CoercibleTo (OutputForm))]

++ Default category packages for Record, Union, Mapping and 
++ Enumeration domains.
$commonCategoryDefaults ==
  ['(SetCategory_& $), '(BasicType_& $), NIL]

++ The slot number in a domain shell that holds the first parameter to
++ a domain constructor.
$FirstParamSlot == 
  6

--% Record
--  Want to eventually have the elts and setelts.
--  Record is a macro in BUILDOM LISP. It takes out the colons.

isRecord type == 
  type is ["Record",:.]

++ returns the code for the `n'th item recorded in a domain shell,
++ according to the old runtime system.  Note that the old runtime
++ scheme is used only for the handful of constructors created 
++ in this file.
oldSlotCode: %Short -> %Short
oldSlotCode n ==
  2 * ($FirstParamSlot + n)


Record(:args) ==
  srcArgs := [[":", second a, devaluate third a] for a in args]
  -- if we already have this instantiation in store, just hand it back.
  t := lassocShiftWithFunction(srcArgs,
         HGET($ConstructorCache,"Record"), "domainEqualList") =>
    CDRwithIncrement t
  nargs := #args
  dom := newShell(nargs + 10)
  -- JHD added an extra slot to cache EQUAL methods
  dom.0 := ["Record", :srcArgs]
  dom.1 :=
    ["lookupInTable",dom,
	[["=",[[$Boolean,"$","$"],:oldSlotCode nargs]],
	  ["~=",[[$Boolean,"$","$"],:0]],
	    ["coerce",[[$OutputForm,"$"],:oldSlotCode(nargs + 1)]]]]
  dom.2 := NIL
  dom.3 := ["RecordCategory",:QCDR dom.0]
  dom.4 := [$commonCategoryDefaults, $commonCategoryAncestors]
  dom.5 := nil
  for i in $FirstParamSlot.. for a in args repeat dom.i := third a
  dom.($FirstParamSlot + nargs) := [function RecordEqual, :dom]
  dom.($FirstParamSlot + nargs + 1) := [function RecordPrint, :dom]
  dom.($FirstParamSlot + nargs + 2) := [function Undef, :dom]
-- following is cache for equality functions
  dom.($FirstParamSlot + nargs + 3) := if nargs <= 2
	    then [NIL,:NIL]
	    else newShell nargs
  -- remember this instantiation for future re-use.
  haddProp($ConstructorCache,"Record",srcArgs,[1,:dom])
  dom

RecordEqual(x,y,dom) ==
  nargs := #rest(dom.0)
  PAIRP x =>
    b:=
       SPADCALL(first x, first y, first(dom.(nargs + 9)) or
         first RPLACA(dom.(nargs + 9),findEqualFun(dom.$FirstParamSlot)))
    nargs = 1 => b
    b and
       SPADCALL(rest x, rest y, rest (dom.(nargs + 9)) or
         rest RPLACD(dom.(nargs + 9),findEqualFun(dom.($FirstParamSlot+1))))
  VECP x =>
    equalfuns := dom.(nargs + 9)
    and/[SPADCALL(x.i,y.i,equalfuns.i or _
           (equalfuns.i:=findEqualFun(dom.($FirstParamSlot + i))))_
         for i in 0..(nargs - 1)]
  error '"Bug: Silly record representation"

RecordPrint(x,dom) == 
  coerceRe2E(x,dom.3)

coerceVal2E(x,m) ==
   objValUnwrap coerceByFunction(objNewWrap(x,m),$OutputForm)

findEqualFun(dom) ==
  compiledLookup("=",[$Boolean,"$","$"],dom)

coerceRe2E(x,source) ==
  n := # rest source
  n = 1 =>
    ["construct",
     ["=", source.1.1, coerceVal2E(first x,source.1.2)] ]
  n = 2 =>
    ["construct",
     ["=", source.1.1, coerceVal2E(first x,source.1.2)], _
     ["=", source.2.1, coerceVal2E(rest x,source.2.2)] ]
  VECP x =>
    ['construct,
     :[["=",tag,coerceVal2E(x.i, fdom)]
       for i in 0.. for [.,tag,fdom] in rest source]]
  error '"Bug: ridiculous record representation"


--% Union
--  Want to eventually have the coerce to and from branch types.

Union(:args) ==
  srcArgs := [(a is [":",tag,d] => [":",tag,devaluate d]; devaluate a)
                 for a in args]
  t := lassocShiftWithFunction(srcArgs,HGET($ConstructorCache,"Union"),
          "domainEqualList") => CDRwithIncrement t
  nargs := #args
  dom := newShell (nargs + 9)
  dom.0 := ["Union", :srcArgs]
  dom.1 :=
    ["lookupInTable",dom,
       [["=",[[$Boolean,"$","$"],:oldSlotCode nargs]],
	 ["~=",[[$Boolean,"$","$"],:0]],
	   ["coerce",[[$OutputForm,"$"],:oldSlotCode (nargs+1)]]]]
  dom.2 := NIL
  dom.3 := ["UnionCategory",:QCDR dom.0]
  dom.4 := [$commonCategoryDefaults, $commonCategoryAncestors]
  dom.5 := nil
  for i in $FirstParamSlot.. for a in args repeat dom.i := a
  dom.($FirstParamSlot + nargs) := [function UnionEqual, :dom]
  dom.($FirstParamSlot + nargs + 1) := [function UnionPrint, :dom]
  dom.($FirstParamSlot + nargs + 2) := [function Undef, :dom]
  haddProp($ConstructorCache,"Union",srcArgs,[1,:dom])
  dom

UnionEqual(x, y, dom) ==
  ["Union",:branches] := dom.0
  branches := orderUnionEntries branches
  predlist := mkPredList branches
  same := false
  for b in stripUnionTags branches for p in predlist while not same repeat
    typeFun := COERCE(["LAMBDA", '(_#1), p],"FUNCTION")
    FUNCALL(typeFun,x) and FUNCALL(typeFun,y) =>
      STRINGP b => same := (x = y)
      if p is ["EQCAR", :.] then (x := rest x; y := rest y)
      same := SPADCALL(x, y, findEqualFun(evalDomain b))
  same

UnionPrint(x, dom) == coerceUn2E(x, dom.0)

coerceUn2E(x,source) ==
  ["Union",:branches] := source
  branches := orderUnionEntries branches
  predlist := mkPredList branches
  byGeorge := byJane := GENSYM()
  for b in stripUnionTags branches for p in predlist  repeat
    typeFun := COERCE(["LAMBDA", '(_#1), p],"FUNCTION")
    if FUNCALL(typeFun,x) then return
      if p is ["EQCAR", :.] then x := rest x
--    STRINGP b => return x  -- to catch "failed" etc.
      STRINGP b => byGeorge := x  -- to catch "failed" etc.
      byGeorge := coerceVal2E(x,b)
  byGeorge = byJane =>
    error '"Union bug: Cannot find appropriate branch for coerce to E"
  byGeorge

--% Mapping
--  Want to eventually have elt: ($, args) -> target

Mapping(:args) ==
  srcArgs := [devaluate a for a in args]
  t := lassocShiftWithFunction(srcArgs,HGET($ConstructorCache,"Mapping"),
          "domainEqualList") => CDRwithIncrement t
  nargs := #args
  dom := newShell(nargs + 9)
  dom.0 := ["Mapping", :srcArgs]
  dom.1 :=
    ["lookupInTable",dom,
       [["=",[[$Boolean,"$","$"],:oldSlotCode nargs]],
	 ["~=",[[$Boolean,"$","$"],:0]],
	   ["coerce",[[$OutputForm,"$"],:oldSlotCode(nargs + 1)]]]]
  dom.2 := NIL
  dom.3 := '(SetCategory)
  dom.4 := [$commonCategoryDefaults, $commonCategoryAncestors]
  dom.5 := nil
  for i in $FirstParamSlot.. for a in args repeat dom.i := a
  dom.($FirstParamSlot + nargs) := [function MappingEqual, :dom]
  dom.($FirstParamSlot + nargs + 1) := [function MappingPrint, :dom]
  dom.($FirstParamSlot + nargs + 2) := [function Undef, :dom]
  haddProp($ConstructorCache,"Mapping",srcArgs,[1,:dom])
  dom

MappingEqual(x, y, dom) == EQ(x,y)
MappingPrint(x, dom) == coerceMap2E(x)

coerceMap2E(x) ==
  -- nrlib domain
  ARRAYP rest x => ["theMap", BPINAME first x,
    if $testingSystem then 0 else REMAINDER(HASHEQ rest x, 1000)]
  -- aldor 
  ["theMap", BPINAME first x  ]

--% Enumeration

Enumeration(:"args") ==
  t := lassocShiftWithFunction(args,HGET($ConstructorCache,"Enumeration"),
          "domainEqualList") => CDRwithIncrement t
  nargs := #args
  dom := newShell(nargs + 9)
  -- JHD added an extra slot to cache EQUAL methods
  dom.0 := ["Enumeration", :args]
  dom.1 :=
    ["lookupInTable",dom,
	[["=",[[$Boolean,"$","$"],:oldSlotCode nargs]],
	  ["~=",[[$Boolean,"$","$"],:0]],
	    ["coerce",[[$OutputForm,"$"],:oldSlotCode(nargs+1)], 
	      [["$", $Symbol], :oldSlotCode(nargs+2)]]
		  ]]
  dom.2 := NIL
  dom.3 := ["EnumerationCategory",:QCDR dom.0]
  dom.4 := [$commonCategoryDefaults, $commonCategoryAncestors]
  dom.5 := nil
  for i in $FirstParamSlot.. for a in args repeat dom.i := a
  dom.($FirstParamSlot + nargs) := [function EnumEqual, :dom]
  dom.($FirstParamSlot + nargs + 1) := [function EnumPrint, :dom]
  dom.($FirstParamSlot + nargs + 2) := [function createEnum, :dom]
  haddProp($ConstructorCache,"Enumeration",args,[1,:dom])
  dom

EnumEqual(e1,e2,dom) == 
  e1=e2

EnumPrint(enum, dom) == 
  (rest(dom.0)).enum

createEnum(sym, dom) ==
  args := rest(dom.0)
  val := -1
  for v in args for i in 0.. repeat
     sym=v => return(val:=i)
  val<0 => userError ["Cannot coerce",sym,"to",["Enumeration",:args]]
  val

--% INSTANTIATORS

RecordCategory(:"x") == constructorCategory ["Record",:x]

EnumerationCategory(:"x") == constructorCategory ["Enumeration",:x]

UnionCategory(:"x") == constructorCategory ["Union",:x]

constructorCategory (title is [op,:.]) ==
  constructorFunction:= GETL(op,"makeFunctionList") or
              systemErrorHere '"constructorCategory"
  [funlist,.]:= FUNCALL(constructorFunction,"$",title,$CategoryFrame)
  oplist:= [[[a,b],true,c] for [a,b,c] in funlist]
  cat:=
    JoinInner([eval ["SetCategory"],mkCategory("domain",oplist,nil,nil,nil)],
      $EmptyEnvironment)
  cat.(0):= title
  cat

--mkMappingFunList(nam,mapForm,e) == [[],e]
mkMappingFunList(nam,mapForm,e) ==
  nargs := #rest mapForm
  dc := GENSYM()
  sigFunAlist:=
    [["=",[$Boolean,nam ,nam], ["ELT",dc,$FirstParamSlot + nargs]], 
      ["~=",[$Boolean,nam,nam],["ELT",dc,0]],
        ["coerce",[$OutputForm,nam], ["ELT",dc,$FirstParamSlot + nargs + 1]]]
  [substitute(nam,dc,substituteDollarIfRepHack sigFunAlist),e]

mkRecordFunList(nam,["Record",:Alist],e) ==
  len:= #Alist
  dc := GENSYM()
  sigFunAlist:=
    [["construct",[nam,:[A for [.,a,A] in Alist]],"mkRecord"],
      ["=",[$Boolean,nam ,nam],["ELT",dc,$FirstParamSlot + len]],
        ["~=",[$Boolean,nam,nam],["ELT",dc,0]],
	  ["coerce",[$OutputForm,nam],["ELT",dc,$FirstParamSlot+len+1]],:
	   [["elt",[A,nam,PNAME a],["XLAM",["$1","$2"],["RECORDELT","$1",i,len]]]
	       for i in 0.. for [.,a,A] in Alist],:
	     [["setelt",[A,nam,PNAME a,A],["XLAM",["$1","$2","$3"],
	       ["SETRECORDELT","$1",i, len,"$3"]]]
		 for i in 0.. for [.,a,A] in Alist],:
		   [["copy",[nam,nam],["XLAM",["$1"],["RECORDCOPY",
		     "$1",len]]]]]
  [substitute(nam,dc,substituteDollarIfRepHack sigFunAlist),e]

mkNewUnionFunList(name,form is ["Union",:listOfEntries],e) ==
  nargs := #listOfEntries
  dc := name
  m := dollarIfRepHack name
  --2. create coercions from subtypes to subUnion
  cList:=
    [["=",[$Boolean,name ,name],["ELT",dc,$FirstParamSlot+nargs]],
       ["~=",[$Boolean,name,name],["ELT",dc,0]],
	 ["coerce",[$OutputForm,name],["ELT",dc,$FirstParamSlot+nargs+1]],:
	   ("append"/
	    [[["construct",[name,type],["XLAM",["#1"],["CONS",i,"#1"]]],
	      ["elt",[type,name,tag],cdownFun],
		["case",['(Boolean),name,tag],
		   ["XLAM",["#1"],["QEQCAR","#1",i]]]]
		     for [.,tag,type] in listOfEntries for i in 0..])] where
		       cdownFun() ==
			gg:=GENSYM()
			$InteractiveMode =>
			  ["XLAM",["#1"],["PROG1",["QCDR","#1"],
			    ["check-union",["QEQCAR","#1",i],type,"#1"]]]
			["XLAM",["#1"],["PROG2",["LET",gg,"#1"],["QCDR",gg],
			  ["check-union",["QEQCAR",gg,i],type,gg]]]
  [cList,e]

mkEnumerationFunList(nam,["Enumeration",:SL],e) ==
  len:= #SL
  dc := nam
  cList :=
    [nil,
      ["=",[$Boolean,nam ,nam],["ELT",dc,$FirstParamSlot+len]],
        ["~=",[$Boolean,nam ,nam],["ELT",dc,0]],
          ["coerce",[nam, ["Symbol"]], ["ELT", dc,$FirstParamSlot+len+1]],
            ["coerce",[["OutputForm"],nam],["ELT",dc,$FirstParamSlot+len+2]]]
  [substitute(nam, dc, cList),e]

mkUnionFunList(op,form is ["Union",:listOfEntries],e) ==
  first listOfEntries is [":",.,.] => mkNewUnionFunList(op,form,e)
     -- following call to order is a bug, but needs massive recomp to fix
  listOfEntries:= orderUnionEntries listOfEntries
  nargs := #listOfEntries
  --1. create representations of subtypes
  predList:= mkPredList listOfEntries
  g:=GENSYM()
  --2. create coercions from subtypes to subUnion
  cList:=
   [["=",[$Boolean,g ,g],["ELT",op,$FirstParamSlot + nargs]],
     ["~=",[$Boolean,g,g],["ELT",op,0]],
       ["coerce",[$OutputForm,g],["ELT",op,$FirstParamSlot+nargs+1]],:
	("append"/
	 [[["autoCoerce",[g,t],upFun],
	   ["coerce",[t,g],cdownFun],
	   ["autoCoerce",[t,g],downFun], --this should be removed eventually
	   ["case",['(Boolean),g,t],typeFun]]
	     for p in predList for t in listOfEntries])] where
		upFun() ==
		  p is ["EQCAR",x,n] => ["XLAM",["#1"],["CONS",n,"#1"]]
		  ["XLAM",["#1"],"#1"]
		cdownFun() ==
		  gg:=GENSYM()
		  if p is ["EQCAR",x,n] then
		     ref:=["QCDR",gg]
		     q:= ["QEQCAR", gg, n]
		  else
		     ref:=gg
		     q:= substitute(gg,"#1",p)
		  ["XLAM",["#1"],["PROG2",["LET",gg,"#1"],ref,
		       ["check-union",q,t,gg]]]
		downFun() ==
		   p is ["EQCAR",x,.] =>
		     ["XLAM",["#1"],["QCDR","#1"]]
		   ["XLAM",["#1"],"#1"]
		typeFun() ==
		   p is ["EQCAR",x,n] =>
		     ["XLAM",["#1"],["QEQCAR",x,n]]
		   ["XLAM",["#1"],p]
  cList:= substitute(dollarIfRepHack op,g,cList)
  [cList,e]

