-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2016, Gabriel Dos Reis.
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

import g_-util
namespace BOOT

module c_-data where
  dbInfovec: %Symbol -> %Maybe %FunctorData
  makeDefaultPackageParameters: %Database -> %List %Symbol
  makeDefaultPackageAbbreviation: %Database -> %Symbol
  completeDefaultPackageParameters: %List %Symbol -> %List %Symbol
--% Accessors of domain and category objects

++ Return thr i-th part of a category object
macro categoryRef(c,i) ==
  vectorRef(c,i)

++ Return the i-th part of a domain object.
macro domainRef(d,i) ==
  vectorRef(d,i)

++ Return the canonical form for a domain or category object
macro canonicalForm d ==
  vectorRef(d,0)

++ Return the constructor that instantiates to the domain
++ or category object
macro instantiationCtor d ==
  canonicalForm(d).op

++ Return the canonical forms of the arguments used to instantiate
++ a domain or a category object.
macro instantiationArgs d ==
  canonicalForm(d).args

++ Return the number of arguments used to instantiate a domain object.  
macro instantiationArity d ==
  # instantiationArgs d

++ Return the list of operations exported by a category object
macro categoryExports d ==
  categoryRef(d,1)

++ Return the attribute alist of a category object.
macro categoryAttributes d ==
  categoryRef(d,2)

++ Return a 3-list of data describing the hierarchy of category `c'.
macro categoryAssociatedTypes c ==
  categoryRef(c,4)

++ Return the list of principal ancestors of category `c'.  
macro categoryPrincipals c ==
  first categoryAssociatedTypes c

++ Return the list of [ancestor,predicate,index] data of catagory `c',
++ where `ancestor' is a fundamental ancestor, `index' its sequence number.
macro categoryAncestors c ==
  second categoryAssociatedTypes c

macro categoryLocals c ==
  third categoryAssociatedTypes c

macro categoryParameters c ==
  categoryRef(c,5)

++ Reference a 3-list
++   [lookupFunction,thisDomain,optable]
++ necessary for function lookup in a domain:
macro domainDirectory d ==
  domainRef(d,1)

++ Reference the lookup function of a domain object
macro domainLookupFunction d ==
  first domainDirectory d
  
++ Reference the operator-code table of a domain object.  
macro domainOperatorTable d ==
  third domainDirectory d

++ Reference the list of (attribute, predIndex) pairs for this domain.
macro domainAttributes d ==
  domainRef(d,2)
  
++ Return the predicate values associated with the domain object.
++ This is an integer interpreted as bit vector
macro domainPredicates d ==
  domainRef(d,3)

++ Return a 3-element dotted list of address data for a domain.
macro domainData d ==
  domainRef(d,4)

--%  
structure %CompilationData ==
  Record(subst: %Substitution,idata: %Substitution,bytes: List %Fixnum,
    shell: %Vector %Thing, items: %Buffer %Pair(%SourceEntity,%Code),
      capsule: %List %Thing, base: %Thing,
        lib: %Libstream,outpath: %Pathname) with
            cdSubstitution == (.subst)
            cdImplicits == (.idata)
            cdBytes == (.bytes)
            cdShell == (.shell)
            cdItems == (.items)
            cdCapsule == (.capsule)
            cdBase == (.base)
            cdLib == (.lib)
            cdOutput == (.outpath)

++ Make a fresh compilation data structure.
makeCompilationData() ==
  mk%CompilationData(nil,nil,nil,nil,[nil,:0],nil,nil,nil,nil)

++ Subsitution that replaces parameters with formals.
macro dbFormalSubst db ==
  cdSubstitution dbCompilerData db

++ Return source-level parameters of this constructor.
dbParameters db ==
  dbConstructorForm(db).args

++ Return implicit parameter data associated to `db'.  This
++ information is active only during the elaboration of the
++ constructor associated with `db'.
macro dbImplicitData db ==
  cdImplicits dbCompilerData db

++ Return the list of encoding bytes for a function during elaboration.
++ Transcient data.
macro dbByteList db ==
  cdBytes dbCompilerData db

++ Return the domain shell of the category object (or the category object
++ of the domain) being elaborated.
macro dbDomainShell db ==
  cdShell dbCompilerData db

++ Return a buffer of entities referenced during elaboration
++ of current functor.
macro dbEntityBuffer db ==
  cdItems dbCompilerData db

++ List (in reverse order) of used entities during elaboration of
++ current functor.
macro dbUsedEntities db ==
  first dbEntityBuffer db

++ Number of used entities during elaboration of current functor.
macro dbEntityCount db ==
  rest dbEntityBuffer db

macro dbCapsuleIR db ==
  cdCapsule dbCompilerData db

macro dbLibstream db ==
  cdLib dbCompilerData db

macro dbCodeStream db ==
  libCodeStream dbLibstream db

macro dbInsnStream db ==
  libInsnStream dbLibstream db

macro dbOutputPath db ==
  cdOutput dbCompilerData db

++ Return the existential substitution of `db'.
dbQuerySubst db ==
  x := dbImplicitData db => first x
  nil

++ List of implicit parameters to the constructor.
dbImplicitParameters db ==
  ASSOCLEFT dbQuerySubst db

dbImplicitConstraints db ==
  x := dbImplicitData db => second x

++ Apply the formal substitution or `db'to the form `x'.
macro dbSubstituteFormals(db,x) ==
  applySubst(dbFormalSubst db,x)

++ Apply the query substitution of `db' to the form `x'.  
macro dbSubstituteQueries(db,x) ==
  applySubst(dbQuerySubst db,x)

++ Apply both query and formal variable substitutions of `db' to `x'.
dbSubstituteAllQuantified(db,x) ==
  applySubst([:dbQuerySubst db,:dbFormalSubst db],x)

++ During compilation, return the base domain form of a domain defition.
macro dbBaseDomainForm db ==
  cdBase dbCompilerData db

--%
$SetCategory ==
  '(SetCategory)
  
--%

completeDefaultPackageParameters parms ==
  dollar := first setDifference('(S A B C D E F G H I),parms)
  [dollar,:parms]

makeDefaultPackageParameters db ==
  completeDefaultPackageParameters dbConstructorForm(db).args

makeDefaultPackageAbbreviation db ==
  makeSymbol strconc(symbolName dbAbbreviation db,'"-")

dbInfovec name ==
  getConstructorKindFromDB name is "category" => nil
  loadLibIfNotLoaded(name)
  u := property(name,'infovec) => u
  nil

++ Access to the shell template  associated with an infovec.
macro ivDomainShell iv ==
  first iv

++ Access to the operation table associated with an infovec.
macro ivOptable iv ==
  second iv

++ Access the alist mapping an attribute to the predicate index
++ associated with an infovec.
macro ivAttributePredicateIndexDirectory iv ==
  third iv

++ Access to additional data in the infovec
macro ivAdditionalData iv ==
  fourth iv

++ Access to predicate bitvector as associated with an infovec.
macro ivPredicateBitvector iv ==
  first ivAdditionalData iv

++ Access to the vector of category default package functors
++ associated with an infovec.
macro ivCategoryDefaults iv ==
  second ivAdditionalData iv

++ Access to the principal ancestors of a domain shell associated
++ with an infovec.
macro ivPrincipalAncestors iv ==
  third ivAdditionalData iv

++ Return the exported operation descriptors bytecode vector
++ associated with an infovec.
ivExportBytecodes iv ==
  [.,.,.,:vec] := ivAdditionalData iv
  vec

--% 
