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


import sys_-macros
import c_-util
namespace BOOT

--% Manipulation of Constructor Datat

--=======================================================================
--            Build Table of Lower Case Constructor Names
--=======================================================================
$lowerCaseConTb := nil

mkLowerCaseConTable() ==
--Called at system build time by function BUILD-INTERPSYS (see util.lisp)
--Table is referenced by functions conPageFastPath and grepForAbbrev
  $lowerCaseConTb := MAKE_-HASH_-TABLE()
  for x in allConstructors() repeat augmentLowerCaseConTable x
  $lowerCaseConTb

augmentLowerCaseConTable x ==
  y:=getConstructorAbbreviationFromDB x
  item:=[x,y,nil]
  tableValue($lowerCaseConTb,x) := item
  tableValue($lowerCaseConTb,DOWNCASE x) := item
  tableValue($lowerCaseConTb,y) := item

getCDTEntry(info,isName) ==
  not ident? info => nil
  (entry := tableValue($lowerCaseConTb,info)) =>
    [name,abb,:.] := entry
    isName and sameObject?(name,info) => entry
    not isName and sameObject?(abb,info) => entry
    nil
  entry
 
putConstructorProperty(name,prop,val) ==
  null (entry := getCDTEntry(name,true)) => nil
  entry.rest.rest := PUTALIST(CDDR entry,prop,val)
  true

attribute? name == 
  symbolMember?(name, $BuiltinAttributes)
 
abbreviation? abb ==
  -- if it is an abbreviation, return the corresponding name
  getConstructorFullNameFromDB abb
 
constructor? name ==
  -- if it is a constructor name, return the abbreviation
  getConstructorAbbreviationFromDB name
 
domainForm?: %Form -> %Boolean
domainForm? d ==
  getConstructorKindFromDB opOf d is "domain"

packageForm?: %Form -> %Boolean
packageForm? d ==
  getConstructorKindFromDB opOf d is "package"

categoryFrom?: %Form -> %Boolean
categoryForm? c ==
  op := opOf c
  builtinCategoryName? op => true
  getConstructorKindFromDB op is "category"

-- probably will switch over to 'libName soon
getLisplibName(c) == getConstructorAbbreviation(c)
 
getConstructorAbbreviation: %Symbol -> %Symbol
getConstructorAbbreviation op ==
  getConstructorAbbreviationFromDB op or throwKeyedMsg("S2IL0015",[op])
 
getConstructorUnabbreviation op ==
  abbreviation?(op) or throwKeyedMsg("S2IL0019",[op])
 
mkUserConstructorAbbreviation(c,a,type) ==
  if cons? c then c := first c  --  Existing constructors will be wrapped
  constructorAbbreviationErrorCheck(c,a,type,'abbreviationError)
  clearClams()
  clearConstructorCache(c)
  installConstructor(c,type)
  setAutoLoadProperty(c)
 
abbQuery(x) ==
  abb := getConstructorAbbreviation x =>
   sayKeyedMsg("S2IZ0001",[abb,getConstructorKindFromDB x,x])
  sayKeyedMsg("S2IZ0003",[x])
 
installConstructor(cname,type) ==
  $lowerCaseConTb = nil => nil
  (entry := getCDTEntry(cname,true)) => entry
  item := [cname,getConstructorAbbreviationFromDB cname,nil]
  tableValue($lowerCaseConTb,cname) := item
  tableValue($lowerCaseConTb,DOWNCASE cname) := item
 
constructorNameConflict(name,kind) ==
  userError
    ['"The name",:bright name,'"conflicts with the name of an existing rule",
      "%l",'"please choose another ",kind]

constructorAbbreviationErrorCheck(c,a,typ,errmess) ==
  siz := # (s := symbolName a)
  if typ = "category" and siz > 7
    then throwKeyedErrorMsg('precompilation,"S2IL0021",nil)
  if siz > 8 then throwKeyedErrorMsg('precompilation,"S2IL0006",nil)
  if s ~= stringUpcase s then throwKeyedMsg("S2IL0006",nil)
  abb := getConstructorAbbreviationFromDB c
  name:= getConstructorFullNameFromDB a
  type := getConstructorKindFromDB c
  a=abb and c~=name => lisplibError(c,a,typ,abb,name,type,'duplicateAbb)
  a=name and c~=name => lisplibError(c,a,typ,abb,name,type,'abbIsName)
  c=name and typ~=type => lisplibError(c,a,typ,abb,name,type,'wrongType)
 
abbreviationError(c,a,typ,abb,name,type,error) ==
  sayKeyedMsg("S2IL0009",[a,typ,c])
  error='duplicateAbb =>
    throwKeyedMsg("S2IL0010",[a,typ,name])
  error='abbIsName =>
    throwKeyedMsg("S2IL0011",[a,type])
  error='wrongType =>
    throwKeyedMsg("S2IL0012",[c,type])
  nil
 
abbreviate u ==
  u is ['Union,:arglist] =>
    ['Union,:[abbreviate a for a in arglist]]
  u is [op,:arglist] =>
    abb := getConstructorAbbreviationFromDB(op) =>
      [abb,:condAbbrev(arglist,getPartialConstructorModemapSig(op))]
    u
  getConstructorAbbreviationFromDB(u) or u
 
unabbrev u == unabbrev1(u,nil)
 
unabbrevAndLoad u == unabbrev1(u,true)
 
isNameOfType x ==
  $doNotAddEmptyModeIfTrue:local:= true
  (val := get(x,'value,$InteractiveFrame)) and
    (domain := objMode val) and
      listMember?(domain,$LangSupportTypes) => true
  constructor? opOf unabbrev x
 
unabbrev1(u,modeIfTrue) ==
  u isnt [.,:.] =>
    not ident? u => u                    -- surely not constructor abbrev
    modeIfTrue =>
      d:= isDomainValuedVariable u => u
      a := abbreviation? u =>
        niladicConstructor? a => [a]
        largs := ['_$EmptyMode for arg in
          getPartialConstructorModemapSig(a)]
        unabbrev1([u,:largs],modeIfTrue)
      u
    a:= abbreviation?(u) or u
    niladicConstructor? a => [a]
    a
  [op,:arglist] := u
  op = 'Join => ['Join, :[unabbrev1(x, modeIfTrue) for x in arglist]]
  d:= isDomainValuedVariable op =>
    throwKeyedMsg("S2IL0013",[op,d])
  (r := unabbrevSpecialForms(op,arglist,modeIfTrue)) => r
  (cname := abbreviation? op) or (constructor?(op) and (cname := op)) =>
    (r := unabbrevSpecialForms(cname,arglist,modeIfTrue)) => r
    -- ??? if modeIfTrue then loadIfNecessary cname
    [cname,:condUnabbrev(op,arglist,
      getPartialConstructorModemapSig(cname),modeIfTrue)]
  u
 
unabbrevSpecialForms(op,arglist,modeIfTrue) ==
  op in '(Mapping MappingCategory) =>
    [op,:[unabbrev1(a,modeIfTrue) for a in arglist]]
  op in '(Union UnionCategory)  =>
    [op,:[unabbrevUnionComponent(a,modeIfTrue) for a in arglist]]
  op in '(Record RecordCategory) =>
    [op,:[unabbrevRecordComponent(a,modeIfTrue) for a in arglist]]
  nil
 
unabbrevRecordComponent(a,modeIfTrue) ==
  a is ["Declare",b,T] or a is [":",b,T] =>
    [":",b,unabbrev1(T,modeIfTrue)]
  userError '"wrong format for Record type"
 
unabbrevUnionComponent(a,modeIfTrue) ==
  a is ["Declare",b,T] or a is [":",b,T] =>
    [":",b,unabbrev1(T,modeIfTrue)]
  unabbrev1(a, modeIfTrue)
 
condAbbrev(arglist,argtypes) ==
  res:= nil
  for arg in arglist for type in argtypes repeat
    if categoryForm?(type) then arg:= abbreviate arg
    res:=[:res,arg]
  res
 
condUnabbrev(op,arglist,argtypes,modeIfTrue) ==
  #arglist ~= #argtypes and argtypes isnt [["Tuple",t]] =>
    throwKeyedMsg("S2IL0014",[op,plural(#argtypes,'"argument"),
      bright(#arglist)])
  -- fix up argument list for unary constructor taking tuples.
  t ~= nil =>
    categoryForm? t => 
       [["tuple",:[unabbrev1(arg,modeIfTrue) for arg in arglist]]]
    [["tuple",:arglist]]
  [newArg for arg in arglist for type in argtypes] where newArg() ==
    categoryForm?(type) => unabbrev1(arg,modeIfTrue)
    arg

++ returns true if `op' is the name of a constructor.  
++ Note: From this function point of view, a symbol names a
++ constructor if it is either a builtin constructor, or it is
++ known to the global database as designating a constructor.  In
++ particular neither the category frame, nor the normal frame
++ are consulted.  Consequently, this functions is not appropriate
++ for use in the compiler.
isConstructorName op ==
  getConstructorAbbreviationFromDB op
    or builtinConstructor? op

 
--% Code Being Phased Out
 
nAssocQ(x,l,n) ==
  repeat
    if l isnt [.,:.] then return nil
    if sameObject?(x,first(l).n) then return first l
    l:= rest l
 

