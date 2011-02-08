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
$nonLisplibDomains == append($DomainNames,$noCategoryDomains)

++ Category ancestors for Record, Union, Mapping, and Enumeration domains.
$commonCategoryAncestors ==
  ['(SetCategory), '(BasicType), '(CoercibleTo (OutputForm))]

++ Default category packages for Record, Union, Mapping and 
++ Enumeration domains.
$commonCategoryDefaults ==
  ['(SetCategory_& $), '(BasicType_& $), nil]

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
  nargs := #args
  dom := newShell(nargs + 10)
  -- JHD added an extra slot to cache EQUAL methods
  vectorRef(dom,0) := ["Record", :srcArgs]
  vectorRef(dom,1) :=
    ["lookupInTable",dom,
	[["=",[[$Boolean,"$","$"],:oldSlotCode nargs]],
	  ["~=",[[$Boolean,"$","$"],:0]], 
            ["hash",[[$SingleInteger,"$"],:0]],
               ["coerce",[[$OutputForm,"$"],:oldSlotCode(nargs + 1)]]]]
  vectorRef(dom,2) := nil
  vectorRef(dom,3) := ["RecordCategory",:rest dom.0]
  vectorRef(dom,4) := [$commonCategoryDefaults, $commonCategoryAncestors]
  vectorRef(dom,5) := nil
  for i in $FirstParamSlot.. for a in args repeat
    vectorRef(dom,i) := third a
  vectorRef(dom,$FirstParamSlot + nargs) := [function RecordEqual, :dom]
  vectorRef(dom,$FirstParamSlot + nargs + 1) := [function RecordPrint, :dom]
  vectorRef(dom,$FirstParamSlot + nargs + 2) := [function Undef, :dom]
  -- following is cache for equality functions
  vectorRef(dom,$FirstParamSlot + nargs + 3) := if nargs <= 2
	    then [nil,:nil]
	    else newShell nargs
  dom

RecordEqual(x,y,dom) ==
  nargs := #rest(dom.0)
  cons? x =>
    b:=
       SPADCALL(first x, first y, first(dom.(nargs + 9)) or
         first (dom.(nargs + 9).first := findEqualFun(dom.$FirstParamSlot)))
    nargs = 1 => b
    b and
       SPADCALL(rest x, rest y, rest (dom.(nargs + 9)) or
         rest (dom.(nargs + 9).rest := findEqualFun(dom.($FirstParamSlot+1))))
  vector? x =>
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
  vector? x =>
    ['construct,
     :[["=",tag,coerceVal2E(x.i, fdom)]
       for i in 0.. for [.,tag,fdom] in rest source]]
  error '"Bug: ridiculous record representation"


--% Union
--  Want to eventually have the coerce to and from branch types.

Union(:args) ==
  srcArgs := [(a is [":",tag,d] => [":",tag,devaluate d]; devaluate a)
                 for a in args]
  nargs := #args
  dom := newShell (nargs + 9)
  vectorRef(dom,0) := ["Union", :srcArgs]
  vectorRef(dom,1) :=
    ["lookupInTable",dom,
       [["=",[[$Boolean,"$","$"],:oldSlotCode nargs]],
	 ["~=",[[$Boolean,"$","$"],:0]],
           ["hash", [[$SingleInteger,"$"],:0]],
              ["coerce",[[$OutputForm,"$"],:oldSlotCode (nargs+1)]]]]
  vectorRef(dom,2) := nil
  vectorRef(dom,3) := ["UnionCategory",:rest dom.0]
  vectorRef(dom,4) := [$commonCategoryDefaults, $commonCategoryAncestors]
  vectorRef(dom,5) := nil
  for i in $FirstParamSlot.. for a in args repeat
    vectorRef(dom,i) := a
  vectorRef(dom,$FirstParamSlot + nargs) := [function UnionEqual, :dom]
  vectorRef(dom,$FirstParamSlot + nargs + 1) := [function UnionPrint, :dom]
  vectorRef(dom,$FirstParamSlot + nargs + 2) := [function Undef, :dom]
  dom

UnionEqual(x, y, dom) ==
  ["Union",:branches] := vectorRef(dom,0)
  predlist := mkPredList branches
  same := false
  for b in stripUnionTags branches for p in predlist while not same repeat
    typeFun := eval ['%lam,'(_#1),p]
    FUNCALL(typeFun,x) and FUNCALL(typeFun,y) =>
      string? b => same := (x = y)
      if p is ['%ieq,['%head,.],:.] then (x := rest x; y := rest y)
      same := SPADCALL(x, y, findEqualFun(evalDomain b))
  same

UnionPrint(x, dom) == coerceUn2E(x, dom.0)

coerceUn2E(x,source) ==
  ["Union",:branches] := source
  predlist := mkPredList branches
  byGeorge := byJane := gensym()
  for b in stripUnionTags branches for p in predlist  repeat
    typeFun := eval ['%lam,'(_#1),p]
    if FUNCALL(typeFun,x) then return
      if p is ['%ieq,['%head,.],:.] then x := rest x
--    string? b => return x  -- to catch "failed" etc.
      string? b => byGeorge := x  -- to catch "failed" etc.
      byGeorge := coerceVal2E(x,b)
  byGeorge = byJane =>
    error '"Union bug: Cannot find appropriate branch for coerce to E"
  byGeorge

--% Mapping
--  Want to eventually have elt: ($, args) -> target

Mapping(:args) ==
  srcArgs := [devaluate a for a in args]
  nargs := #args
  dom := newShell(nargs + 9)
  vectorRef(dom,0) := ["Mapping", :srcArgs]
  vectorRef(dom,1) :=
    ["lookupInTable",dom,
       [["=",[[$Boolean,"$","$"],:oldSlotCode nargs]],
	 ["~=",[[$Boolean,"$","$"],:0]],
           ["hash", [[$SingleInteger,"$"],:0]],
              ["coerce",[[$OutputForm,"$"],:oldSlotCode(nargs + 1)]]]]
  vectorRef(dom,2) := nil
  vectorRef(dom,3) := $SetCategory
  vectorRef(dom,4) := [$commonCategoryDefaults, $commonCategoryAncestors]
  vectorRef(dom,5) := nil
  for i in $FirstParamSlot.. for a in args repeat
    vectorRef(dom,i) := a
  vectorRef(dom,$FirstParamSlot + nargs) := [function MappingEqual, :dom]
  vectorRef(dom,$FirstParamSlot + nargs + 1) := [function MappingPrint, :dom]
  vectorRef(dom,$FirstParamSlot + nargs + 2) := [function Undef, :dom]
  dom

MappingEqual(x, y, dom) == EQ(x,y)
MappingPrint(x, dom) == coerceMap2E(x)

coerceMap2E(x) ==
  -- nrlib domain
  ARRAYP rest x => ["theMap", BPINAME first x,
    if $testingSystem then 0 else HASHEQ(rest x) rem 1000]
  -- aldor 
  ["theMap", BPINAME first x  ]

--% Enumeration

Enumeration(:"args") ==
  nargs := #args
  dom := newShell(nargs + 9)
  -- JHD added an extra slot to cache EQUAL methods
  vectorRef(dom,0) := ["Enumeration", :args]
  vectorRef(dom,1) :=
    ["lookupInTable",dom,
	[["=",[[$Boolean,"$","$"],:oldSlotCode nargs]],
	  ["~=",[[$Boolean,"$","$"],:0]],
            ["hash", [[$SingleInteger,"$"],:0]],
              ["coerce",[[$OutputForm,"$"],:oldSlotCode(nargs+1)], 
                [["$", $Symbol], :oldSlotCode(nargs+2)]]
                  ]]
  vectorRef(dom,2) := nil
  vectorRef(dom,3) := ["EnumerationCategory",:rest dom.0]
  vectorRef(dom,4) := [$commonCategoryDefaults, $commonCategoryAncestors]
  vectorRef(dom,5) := nil
  for i in $FirstParamSlot.. for a in args repeat dom.i := a
  dom.($FirstParamSlot + nargs) := [function EnumEqual, :dom]
  dom.($FirstParamSlot + nargs + 1) := [function EnumPrint, :dom]
  dom.($FirstParamSlot + nargs + 2) := [function createEnum, :dom]
  dom

EnumEqual(e1,e2,dom) == 
  e1=e2

EnumPrint(enum, dom) == 
  rest(vectorRef(dom,0)).enum

createEnum(sym, dom) ==
  args := vectorRef(dom,0).args
  val := -1
  for v in args for i in 0.. repeat
     sym=v => return(val:=i)
  val<0 => userError ['"Cannot coerce",sym,'"to",["Enumeration",:args]]
  val

--% INSTANTIATORS

RecordCategory(:"x") == constructorCategory ["Record",:x]

EnumerationCategory(:"x") == constructorCategory ["Enumeration",:x]

UnionCategory(:"x") == constructorCategory ["Union",:x]

constructorCategory (title is [op,:.]) ==
  constructorFunction:= GETL(op,"makeFunctionList") or
              systemErrorHere ['"constructorCategory",title]
  [funlist,.]:= FUNCALL(constructorFunction,"$",title,$CategoryFrame)
  oplist:= [[[a,b],true,c] for [a,b,c] in funlist]
  cat:=
    JoinInner([eval $SetCategory,mkCategory("domain",oplist,nil,nil,nil)],
      $EmptyEnvironment)
  vectorRef(cat,0) := title
  cat

--mkMappingFunList(nam,mapForm,e) == [[],e]
mkMappingFunList(nam,mapForm,e) ==
  nargs := #rest mapForm
  dc := gensym()
  sigFunAlist:=
    [["=",[$Boolean,nam ,nam], ["ELT",dc,$FirstParamSlot + nargs]], 
      ["~=",[$Boolean,nam,nam],["ELT",dc,0]],
        ["hash",[$SingleInteger,nam],["ELT",dc,0]],
          ["coerce",[$OutputForm,nam], 
            ["ELT",dc,$FirstParamSlot + nargs + 1]]]
  [substitute(nam,dc,substituteDollarIfRepHack sigFunAlist),e]

mkRecordFunList(nam,["Record",:Alist],e) ==
  len:= #Alist
  dc := gensym()
  sigFunAlist:=
    [["construct",[nam,:[A for [.,a,A] in Alist]],"mkRecord"],
      ["=",[$Boolean,nam ,nam],["ELT",dc,$FirstParamSlot + len]],
        ["~=",[$Boolean,nam,nam],["ELT",dc,0]],
         ["hash",[$SingleInteger,nam],["ELT",dc,0]],
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
        ["hash",[$SingleInteger,name],["ELT",dc,0]],
	 ["coerce",[$OutputForm,name],["ELT",dc,$FirstParamSlot+nargs+1]],:
	   ("append"/
	    [[["construct",[name,type],["XLAM",["#1"],["%makepair",i,"#1"]]],
	      ["elt",[type,name,tag],cdownFun],
		["case",[$Boolean,name,tag],
		   ["XLAM",["#1"],['%ieq,['%head,"#1"],i]]]]
		     for [.,tag,type] in listOfEntries for i in 0..])] where
		       cdownFun() ==
			gg:=gensym()
			$InteractiveMode =>
			  ["XLAM",["#1"],["PROG1",["%tail","#1"],
			    ["check-union",['%ieq,['%head,"#1"],i],type,"#1"]]]
			["XLAM",["#1"],
                          ['%bind,[[gg,"#1"]],
                            ["check-union",['%ieq,['%head,gg],i],type,gg],
                              ["%tail",gg]]]
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
  nargs := #listOfEntries
  --1. create representations of subtypes
  predList:= mkPredList listOfEntries
  g:=gensym()
  --2. create coercions from subtypes to subUnion
  cList:=
   [["=",[$Boolean,g ,g],["ELT",op,$FirstParamSlot + nargs]],
     ["~=",[$Boolean,g,g],["ELT",op,0]],
      ["hash",[$SingleInteger,g],["ELT",op,0]],
       ["coerce",[$OutputForm,g],["ELT",op,$FirstParamSlot+nargs+1]],:
	("append"/
	 [[["autoCoerce",[g,t],upFun],
	   ["coerce",[t,g],cdownFun],
	   ["autoCoerce",[t,g],downFun], --this should be removed eventually
	   ["case",[$Boolean,g,t],typeFun]]
	     for p in predList for t in listOfEntries])] where
		upFun() ==
		  p is ['%ieq,['%head,x],n] =>
                    ["XLAM",["#1"],["%makepair",n,"#1"]]
		  ["XLAM",["#1"],"#1"]
		cdownFun() ==
		  gg:=gensym()
		  if p is ['%ieq,['%head,x],n] then
		     ref:=["%tail",gg]
		     q:= ['%ieq,['%head,gg],n]
		  else
		     ref:=gg
		     q:= substitute(gg,"#1",p)
		  ["XLAM",["#1"],
                    ['%bind,[[gg,"#1"]],["check-union",q,t,gg],ref]]
		downFun() ==
		   p is ['%ieq,['%head,x],.] =>
		     ["XLAM",["#1"],["%tail","#1"]]
		   ["XLAM",["#1"],"#1"]
		typeFun() ==
		   p is ['%ieq,['%head,x],n] =>
		     ["XLAM",["#1"],['%ieq,['%head,x],n]]
		   ["XLAM",["#1"],p]
  cList:= substitute(dollarIfRepHack op,g,cList)
  [cList,e]

--%
for x in '((Record mkRecordFunList)
           (Union mkUnionFunList)
           (Mapping mkMappingFunList)
           (Enumeration mkEnumerationFunList)) 
 repeat
   property(first x, 'makeFunctionList) := second x

