-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2015, Gabriel Dos Reis.
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
-- This file collects and documents some of the constants used by either
-- the interpreter or the compiler or both.
--

import types
namespace BOOT

--%
--% Numeric limits
--%

++ Maximum value of character code point values
$CharCodeMaximum ==
  CHAR_-CODE_-LIMIT

++ Minimum for %Short values.
$ShortMinimum ==
  MOST_-NEGATIVE_-FIXNUM

++ Maximum for %Short values.
$ShortMaximum ==
  MOST_-POSITIVE_-FIXNUM

++ Maximum for %DoubleFloat values
$DoubleFloatMinimum ==
  MOST_-NEGATIVE_-DOUBLE_-FLOAT

++ Maximum for %DoubleFloat values
$DoubleFloatMaximum ==
  MOST_-POSITIVE_-DOUBLE_-FLOAT

++ Machine precision for %DoubleFloat
$DoubleFloatEpsilon ==
  DOUBLE_-FLOAT_-EPSILON

--% 

++ Clock time unit per second.
$timerTicksPerSecond == 
  INTERNAL_-TIME_-UNITS_-PER_-SECOND

--
-- Text formatting
--

++ Glyph for a box
$boxString ==
  strconc(charString abstractChar 29, charString abstractChar 226)

++ Glyph for an APL quad
$quadSymbol ==
  $boxString

--% ANSI Escape Sequences.  Note that these days, people
--% will be talking of ISO/IEC 6429.  The practical end result
--% is the same.  
--% The next few definitions provide symbolic names for ANSI
--% espace sequences.

++ The escape character, in string form
$escapeString ==
  charString abstractChar 27

++ Marker to swicth to bold font
$boldString ==
  strconc($escapeString, '"[1m")

++ Marker to switch to normal font
$normalString ==
  strconc($escapeString, '"[0;10m")

++ Marker to switch to reverve video display
$reverseVideoString ==
  strconc($escapeString, '"[7m")

++ Marker to underline text
$underlineString ==
  strconc($escapeString, '"[4m")

++
$highlightFontOn ==
  strconc('" ", $boldString)

++
$highlightFontOff ==
  strconc($normalString, '" ")

-- 
-- User Interface
--

++
++ FIXME: Eventually move this to i-syscmd.boot
$noParseCommands ==
  '(boot      _
    copyright _
    credits   _
    fin       _
    lisp      _
    pquit     _
    quit      _
    suspend   _
    synonym   _
    system)

++
$tokenCommands ==
  '(abbreviations     _
    cd                _
    clear             _
    close             _
    compiler          _
    depends           _
    display           _
    edit              _
    frame             _
    frame             _
    help              _
    history           _
    input             _
    library           _
    load              _
    ltrace            _
    read              _
    savesystem        _
    set               _
    spool             _
    undo              _
    what              _
    with              _
    workfiles         _
    zsystemdevelopment)

++
++ List of pair (command . user level)
$systemCommands ==
  '((abbreviations . compiler)      _
   (boot . development)             _
   (cd . interpreter)               _
   (clear . interpreter)            _
   (close . interpreter)            _
   (compiler . compiler)            _
   (copyright . interpreter)        _
   (credits . interpreter)          _
   (display . interpreter)          _
   (edit . interpreter)             _
   (fin . development)              _
   (frame . interpreter)            _
   (help . interpreter)             _
   (history . interpreter)          _
   (lisp . development)             _
   (library . interpreter)          _
   (load . interpreter)             _
   (ltrace . interpreter)           _
   (pquit . interpreter)            _
   (quit . interpreter)             _
   (read . interpreter)             _
   (savesystem . interpreter)       _
   (set . interpreter)              _
   (show . interpreter)             _
   (spool . interpreter)            _
   (summary . interpreter)          _
   (synonym . interpreter)          _
   (system . interpreter)           _
   (trace . interpreter)            _
   (undo . interpreter)             _
   (what . interpreter)             _
   (with . interpreter)             _
   (workfiles . development)        _
   (zsystemdevelopment . interpreter))

--
-- Old Parser data
--

++ The double quote character in string form
++ FIXME: This constant is used in only one place.  Move it there.
$DoubleQuote ==
  '"_""

++ Internal type tag for big float values.  
++ This must be consistent with the tag checked for in postBigFloat
++ and also set in property.lisp.
++ FIXME: Have all those places use this symbolic constants.
$BFtag ==
  ":BF:"

--
-- Compiler flags
--

++ True if the system should support compilation
++ This constant does not seem very terrible useful.
++ FIXME: Check if it can be removed.
$COMPILE ==
  true

--
-- Common system data
--

++ A list of precomputed formal function formal parameter names.
++ 50 parameters should be enough for everybody, right?
$FormalMapVariableList ==
  '(_#1 _#2 _#3 _#4 _#5 _#6 _#7 _#8 _#9 _#10 _
    _#11 _#12 _#13 _#14 _#15 _#16 _#17 _#18 _#19 _#20 _
    _#21 _#22 _#23 _#24 _#25 _#26 _#27 _#28 _#29 _#30 _
    _#31 _#32 _#33 _#34 _#35 _#36 _#37 _#38 _#39 _#40 _
    _#41 _#42 _#43 _#44 _#45 _#46 _#47 _#48 _#49 _#50)


++ List of precomputed pattern variable names.
$PatternVariableList ==
  '(_*1 _*2 _*3 _*4 _*5 _*6 _*7 _*8 _*9 _*10 _*11 _
    _*12 _*13 _*14 _*15 _*16 _*17 _*18 _*19 _*20 _
    _*21 _*22 _*23 _*24 _*25 _*26 _*27 _*28 _*29 _*30 _
    _*31 _*32 _*33 _*34 _*35 _*36 _*37 _*38 _*39 _*40 _
    _*41 _*42 _*43 _*44 _*45 _*46 _*47 _*48 _*49 _*50)

$ModeVariableList ==
  '(dv_$1 dv_$2 dv_$3 dv_$4 dv_$5 dv_$6 dv_$7 dv_$8 _
    dv_$9 dv_$10 dv_$11 dv_$12 dv_$13 dv_$14 dv_$15 _
    dv_$16 dv_$17 dv_$18 dv_$19 dv_$20)

$DomainVariableList ==
  '(_$1 _$2 _$3 _$4 _$5 _$6 _$7 _$8 _$9 _$10 _$11 _
    _$12 _$13 _$14 _$15 _$16 _$17 _$18 _$19 _$20)

$TriangleVariableList ==
  '(t_#1 t_#2 t_#3 t_#4 t_#5 t_#6 t_#7 t_#8 t_#9 t_#10
    t_#11 t_#12 t_#13 t_#14 t_#15 t_#16 t_#17 t_#18 t_#19 t_#20
    t_#21 t_#22 t_#23 t_#24 t_#25 t_#26 t_#27 t_#28 t_#29 t_#30
    t_#31 t_#32 t_#33 t_#34 t_#35 t_#36 t_#37 t_#38 t_#39 t_#40
    t_#41 t_#42 t_#43 t_#44 t_#45 t_#46 t_#47 t_#48 t_#49 t_#50)


$AtVariables ==
  [makeSymbol strconc('"@",toString i) for i in 1..50]

$QueryVariables ==
  [makeSymbol strconc('"?",toString i) for i in 1..50]

++ List of functions known to be free of side effects
++ FIXME: Check that the names on this list are not renamed.
$SideEffectFreeFunctionList ==
  '(_null    _
    _not     _
    _and     _
    _or      _
    _xor     _
    _case    _
    Zero     _
    One      _
    _:       _
    _:_:     _
    _has     _
    Mapping  _
    _elt     _
    _=       _
    _>       _
    _>_=     _
    _<       _
    _<_=     _
    _is      _
    _isnt)

--% Types

++ The Field category form
$Field ==
  '(Field)

$DivisionRing ==
  '(DivisionRing)

$IntegralDomain ==
  '(IntegralDomain)

$CombinatorialFunctionCategory ==
  '(CombinatorialFunctionCategory)

$Ring ==
  '(Ring)

$Group ==
  '(Group)

$AbelianMonoid ==
  '(AbelianMonoid)

$Monoid ==
  '(Monoid)

++ The Void domain constructor form
$Void ==
  '(Void)

++ The Any domain constructor form
$Any ==
  '(Any)

++ The None domain constructor form.
$None ==
  '(None)

++ The Syntax domain constructor form
$Syntax ==
  '(Syntax)

$BasicOperator ==
  '(BasicOperator)

$AlgebraicNumber ==
  '(AlgebraicNumber)

++ Boolean domain constructor form
$Boolean ==
  '(Boolean)

$Byte == '(Byte)

++ Sized integer types, mostly for system-level programming.
$Int16 == '(Int16)
$UInt16 == '(UInt16)

$Int32 == '(Int32)
$UInt32 == '(UInt32)

$Int64 == '(Int64)
$UInt64 == '(UInt64)

++ The SmallInteger domain constructor form
$SmallInteger ==
  '(SingleInteger)

$SingleInteger ==
  '(SingleInteger)

++ The Integer domain constructor form.
$Integer ==
  '(Integer)


++ The NonNegativeInteger domain constructor form
$NonNegativeInteger ==
  '(NonNegativeInteger)

++ The PositiveInteger domain constructor form
$PositiveInteger ==
  '(PositiveInteger)

++ The fraction field constructor
$QuotientField ==
  'Fraction

++ The RationalNumber domain constructor form
$RationalNumber ==
  '(Fraction (Integer))

++ The domain constructor for Gaussian integers
$ComplexInteger ==
  ["Complex", $Integer]


++ Float domain constructor form
$Float ==
  '(Float)

++ DoubleFloat domain constructor form
$DoubleFloat ==
  '(DoubleFloat)

++ BigFloat domain constructor form
++ FIXME: This does not appear to be used anywhere in the 
++  source codes.  
$BigFloat ==
  '(Float)

++ The String constructor domain form
$String ==
  '(String)

++ The Symbol constructor domain form
$Symbol ==
  '(Symbol)

++ The Identifier domain form
$Identifier ==
  '(Identifier)

++ The 'wildcar' for a type -- "?" in Spad syntax.  This stands for 
++ an unspecified type.
$EmptyMode ==
  "$EmptyMode"

++ The OutputForm domain constructor form
$OutputForm ==
  '(OutputForm)

++ The InputForm domain form
$InputForm ==
  '(InputForm)

++ The domain constructor for functional expression
$FunctionalExpression ==
  'Expression

$TexFormat ==
  '(TexFormat)

$MathMLFormat ==
  '(MathMLFormat)

++ The constructor form for unnamed functions.
$AnonymousFunction ==
  '(AnonymousFunction)

++ Exit domain constructor form
$Exit ==
  '(Exit)

++ The ThrowAwayMode constructor form
$ThrowAwayMode ==
  "$ThrowAwayMode"

++ This mode is used to indicate that the value of expression
++ can be thrown away.
$NoValueMode ==
  "$NoValueMode"

++
$ExitMode ==
  "$ExitMode"

++ A domain form for the OpenAxiom system-level pointer datatype.  
$SystemPointer ==
  '(SystemPointer)

--%
  
++ Category constructor form
$Category ==
  '(Category)

++ The Type category constructor form.
$Type ==
  '(Type)

++ Domain constructor form
++ FIXME: Find where this is used in the system.
$Domain ==
  '(Domain)

++ Mode constructor form
++ FIXME: Where is this used?
$Mode ==
  '(Mode)

$CategoryConstructor ==
  '(CategoryConstructor)

$DomainConstructor ==
  '(DomainConstructor)


++ StringCategory Constructor form
$StringCategory ==
  '(StringCategory)

++ List of language support type forms.
$LangSupportTypes ==
  '((Mode) (Domain) (Type) (Category))

++
$NonMentionableDomainNames ==
  '($ Rep Record Union Mapping Enumeration)

++ These symbols are not constructor names, but they define domains.
$SpecialDomainNames ==
  '(add _
    CAPSULE _
    SubDomain)


$SystemInlinableConstructorNames ==
  '(List               _
    Integer            _ 
    PositiveInteger    _
    NonNegativeInteger _
    SingleInteger      _
    SystemPointer      _
    String             _
    Boolean            _
    Identifier         _
    Symbol             _
    DoubleFloat        _
    PrimitiveArray     _
    Vector             _
    Maybe              _
    Pair               _
    Void)

++ FIXME
$DomainsInScope == 
  '(NIL)

++ List of domains in scope in fresh settings.
$InitialDomainsInScope ==
  '($EmptyMode _
    $NoValueMode)


++ FIXME
$underDomainAlist ==
  nil


++ A list of functors that do not really have modemaps
$DummyFunctorNames ==
  '(Mapping _[_|_|_])

--% 

++ The empty environment
$EmptyEnvironment ==
  '((NIL))

++
$LocalFrame ==
  '((NIL))

++ The empty string constant.
$EmptyString ==
  '""
++ The empty vector constant
$EmptyVector ==
  ["VECTOR"]

++ A symbol denoting failure
$failure ==
  gensym()

++ The initial modemap frame
$InitialModemapFrame ==
  '((NIL))


++ The constant 0.
$Zero ==
  '(Zero)

++ The constant 1.
$One ==
  '(One)

++ Indicate absence of value
$NoValue ==
  "$NoValue"


++
$exitMode == 
  $EmptyMode

++
$leaveMode == 
  $EmptyMode

+++
$IndexFilename ==
  '"index.KAF"

++
$FILETYPE_-TABLE ==
  [["LISPLIB", :"LILIB"],
   ["SPADLIB", :"slib"],
   ["HISTORY", :"hist"],
   ["HELPSPAD", :"help"],
   ["INPUT", :"input"],
   ["SPAD", :"spad"],
   ["BOOT", :"boot"],
   ["LISP", :"lsp"],
   ["META", :"meta"],
   ["OUTPUT", :"splog"],
   ["ERRORLIB", :"erlib"],
   ["DATABASE", :"DAASE"],
   ["SPADDATA", :"sdata"],
   ["SPADFORT", :"sfort"],
   ["SPADFORM", :"sform"],
   ["SPADTEX", :"stex"],
   ["SPADOUT", :"spout"]]

+++
$BuiltinAttributes ==
 '(nil arbitraryExponent approximate complex
   canonical
   partiallyOrderedSet arbitraryPrecision
   noZeroDivisors
   additiveValuation canonicalUnitNormal
   multiplicativeValuation commutative)

+++
BLANK == '" "

++
UNDERBAR == '"__"

++ Lisp catch tag used by some Lisp systems to exit the debugger loop.
$quitTag ==
)if %hasFeature KEYWORD::GCL
  SYSTEM::_*QUIT_-TAG_*
)elseif %hasFeature KEYWORD::SBCL
  QUOTE SB_-INT::TOPLEVEL_-CATCHER
)else
  gensym()
)endif

--% Constants for OpenAxiom IPC
--??? These constants should ideally be coming from a single
--??? source used by both the C and the Lisp world.
$SessionManager == 1
$ViewportServer == 2
$MenuServer == 3
$SessionIO == 4
$InterpWindow == 5
$KillSpad == 6

$CreateFrame == 1
$SwitchFrames == 2
$EndOfOutput == 3
$CallInterp == 4
$EndSession == 5
$LispCommand == 6
$SpadCommand == 7
$SendXEventToHyperTeX == 8
$QuietSpadCommand == 9
$CloseClient == 10
$QueryClients == 11
$QuerySpad == 12
$NonSmanSession == 13
$KillLispSystem == 14


$SpadReaderTag ==
  "SPAD__READER"

$intTopLevel ==
  "top__level"

--%

++ Default optimization level
$defaultOptimizationLevel == 2

++ List of operator names that can be overloaded in libraries.
$OperatorFunctionNames ==
  ["**", "^", "*", "/", "rem", "quo", "mod", "div", "exquo",
    "+", "-", ">", ">=", "=", "~=", "<", "<=", "#", "~", "not",
     "case", "and", "or", "<<", ">>", "/\", "\/", "..", "by" ]

--%
%categoryKind == 'category
%domainKind == 'domain
%packageKind == 'package

