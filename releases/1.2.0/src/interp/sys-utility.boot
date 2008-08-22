-- Copyright (C) 2007-2008 Gabriel Dos Reis.
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

-- This file defines some utility functions common to both the compiler
-- and interpreter.

import sys_-os
import vmlisp
namespace BOOT


--%

++ getVMType returns an approximation of the underlying object type
++ representation of a domain, as a Lisp type specifier as seen by
++ the runtime system.
getVMType d ==
  case (d' := devaluate d) of
    Void => "%Void"
    Boolean => "%Boolean"
    Byte => "%Byte"
    Character => "%Char"
    SingleInteger => "%Short"
    Integer => "%Integer"
    String => "%String"
    List => "%List"
    Vector => ["%Vector",getVMType second d']
    PrimitiveArray => ["%SimpleArray", getVMType second d']
    Pair => "%Pair"
    otherwise => "%Thing"                 -- good enough, for now.

--%

setDynamicBinding: (%Symbol,%Thing) -> %Thing
setDynamicBinding(s,v) ==
  SETF(SYMBOL_-VALUE s,v)

++ returns true if `f' is bound to a macro.
macrop: %Thing -> %Boolean
macrop f ==
  IDENTP f and not null MACRO_-FUNCTION f

++ returns true if `f' is bound to a function
functionp: %Thing -> %Boolean
functionp f ==
  IDENTP f => FBOUNDP f and null MACRO_-FUNCTION f
  FUNCTIONP f

++ remove `item' from `sequence'.
delete: (%Thing,%Sequence) -> %Sequence
delete(item,sequence) ==
  SYMBOLP item => 
    REMOVE(item,sequence,KEYWORD::TEST,function EQ)
  atom item and not ARRAYP item =>
    REMOVE(item,sequence)
  REMOVE(item,sequence,KEYWORD::TEST,function EQUALP)

++ returns true if `x' is contained in `y'.
CONTAINED: (%Thing,%Thing) -> %Boolean
CONTAINED(x,y) == main where
  main() ==
    SYMBOLP x => eq(x,y)
    equal(x,y)
  eq(x,y) ==
    atom y => EQ(x,y)
    eq(x, car y) or eq(x, cdr y)
  equal(x,y) ==
    atom y => EQUAL(x,y)
    equal(x, car y) or equal(x, cdr y)

++ Returns all the keys of association list `x'
-- ??? Should not this be named `alistAllKeys'?
ASSOCLEFT: %Thing -> %Thing
ASSOCLEFT x ==
  atom x => x
  MAPCAR(function first,x)

++ Returns all the datums of association list `x'.
-- ??? Should not this be named `alistAllValues'?
ASSOCRIGHT: %Thing -> %Thing
ASSOCRIGHT x ==
  atom x => x
  MAPCAR(function rest,x)

++ Put the association list pair `(x . y)' into `l', erasing any 
++ previous association for `x'.
ADDASSOC: (%Thing,%Thing,%List) -> %List
ADDASSOC(x,y,l) ==
  atom l => [[x,:y],:l]
  x = first first l => [[x,:y],:cdr l]
  [first l,:ADDASSOC(x,y,rest l)]


++ Remove any assocation pair `(u . x)' from list `v'.
DELLASOS: (%Thing,%List) -> %List
DELLASOS(u,v) ==
  atom v => nil
  u = first first v => rest v
  [first v,:DELLASOS(u,rest v)]


++ Return the datum associated with key `x' in association list `y'.
-- ??? Should not this be named `alistValue'?
LASSOC: (%Thing,%List) -> %Thing
LASSOC(x,y) ==
  atom y => nil
  x = first first y => rest first y
  LASSOC(x,rest y)

++ Return the key associated with datum `x' in association list `y'.
rassoc: (%Thing,%List) -> %Thing
rassoc(x,y) ==
  atom y => nil
  x = rest first y => first first y
  rassoc(x,rest y)

++ Reclaim unreachable objects.
RECLAIM() ==
)if %hasFeature KEYWORD::GCL
  SI::GBC true
)elseif %hasFeature KEYWORD::SBCL
  SB_-EXT::GC()
)elseif %hasFeature KEYWORD::CLISP
  EXT::GC()
)else
  fatalError '"don't know how to collect"
)endif

++
makeAbsoluteFilename: %String -> %String
makeAbsoluteFilename name ==
  CONCATENATE("STRING",systemRootDirectory(),name)

++ returns true if `file' exists as a pathname.
existingFile? file ==
  PROBE_-FILE file

++ original version returned 0 on success, and 1 on failure
++ ??? fix that to return -1 on failure.
$ERASE(:filearg) ==
  -removeFile MAKE_-FULL_-NAMESTRING filearg

++
$REPLACE(filespec1,filespec2) ==
  $ERASE(filespec1 := MAKE_-FULL_-NAMESTRING filespec1)
  renameFile(MAKE_-FULL_-NAMESTRING filespec2, filespec1)

++
checkMkdir path ==
  mkdir path = 0 => true
  systemError ['"cannot create directory",:bright path]

++ return the pathname to the system module designated by `m'.
getSystemModulePath m ==
  CONCAT(systemRootDirectory(),'"algebra/",m,'".",$faslType)

++ load module in `path' that supposedly will define the function 
++ indicated by `name'.
loadModule: (%String,%Symbol) -> %Thing
loadModule(path,name) ==
  FMAKUNBOUND name
  LOAD path

--% numerics
log10 x ==
  LOG(x,10)

bitand: (%Short,%Short) -> %Short
bitand(x,y) ==
  BOOLE(BOOLE_-AND,x,y)

bitior: (%Short,%Short) -> %Short
bitior(x,y) ==
  BOOLE(BOOLE_-IOR,x,y)


--% Back ends 

++ compile a function definition, augmenting the current
++ evaluation environement with the result of the compilation.
COMPILE_-DEFUN(name,body) ==
  EVAL body
  COMPILE name

++ Augment the current evaluation environment with a function definition.
EVAL_-DEFUN(name,body) ==
  EVAL MACROEXPANDALL body

PRINT_-AND_-EVAL_-DEFUN(name,body) ==
  EVAL body
  PRINT_-DEFUN(name,body)


