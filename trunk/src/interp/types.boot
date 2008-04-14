-- Copyright (C) 2007-2008 Gabriel Dos Reis
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

import '"boot-pkg"
)package "BOOT"

++ Basic types used throughout Boot codes.
%Void <=> nil
%Boolean <=> BOOLEAN
%Short <=> FIXNUM
%Integer <=> BIGNUM
%Symbol <=> SYMBOL
%String <=> STRING
%Atom <=> atom
%List <=> LIST
%Vector <=> VECTOR
%Thing <=> true
%Sequence <=> SEQUENCE

%Pair <=> cons

%Maybe a <=> null or a

--% Data structures for the compiler
%Form <=> NUMBER or %Symbol or %String or CONS -- input syntax form
%Env <=> %List                                 -- compiling env
%Mode <=> %Symbol or %String or %List          -- type of forms
%Code <=> %Form                                -- generated code
%Triple <=>                                    -- form + type + env
  cons(%Code,cons(%Mode,cons(%Env,null))) 

%Modemap <=> %List                             -- modemap

%ConstructorKind <=>                           -- kind of ctor instances
  MEMBER("category","domain","package")

%Shell <=> SIMPLE_-VECTOR                      -- constructor instantiation
