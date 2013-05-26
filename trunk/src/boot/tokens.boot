-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2013, Gabriel Dos Reis.
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

import utility
namespace BOOTTRAN
module tokens ($InteractiveMode, char, subString) where
  char: %Symbol -> %Char

++ If true, means the system is in interactive mode.
$InteractiveMode := false

--%

structure %Token ==
  Record(cls: %Symbol, val: %Thing, pos: %Position) with
    tokenClass == (.cls)
    tokenValue == (.val)
    tokenPosition == (.pos)

makeToken(lp,b,n) ==
  mk%Token(first b,second b,[lp,:n])


--%

++ converts `x', a 1-length symbol, to a character.
char x ==
  stringChar(symbolName x, 0)

shoeStartsId x ==
  alphabetic? x or x in [char "$", char "?", char "%"]

shoeIdChar x ==
  alphanumeric? x or x in [char "'", char "?", char "%", char "!",char "&"]

++ return the sub-string of `s' starting from `f'.
++ When non-nil, `n' designates the length of the sub-string.
subString(s,f,n == nil) ==
  n = nil => subSequence(s,f)
  subSequence(s,f,f + n)

++ Table of Boot keywords and their token name.
shoeKeyWords == [  _
            ['"and","AND"] , _
            ['"by", "BY" ], _
            ['"case","CASE"] , _
            ['"catch","CATCH"], _
            ['"cross","CROSS"] , _
            ['"do",   "DO" ], _
            ['"else", "ELSE"] , _
            ['"finally", "FINALLY"], _
            ['"for",  "FOR"] , _
            ['"forall", "FORALL"] , _
            ['"function", "FUNCTION"] , _
            ['"has", "HAS"] , _
            ['"if", "IF"], _
            ['"import", "IMPORT"], _
            ['"in",  "IN" ], _
            ['"is", "IS"], _
            ['"isnt", "ISNT"] , _
            ['"leave", "LEAVE"], _
            ['"macro", "MACRO"], _
            ['"module", "MODULE"], _
            ['"namespace", "NAMESPACE"], _
            ['"of",   "OF"] , _
            ['"or",   "OR"] , _
            ['"rem",  "REM"], _
            ['"repeat", "REPEAT"] , _
            ['"return", "RETURN"], _
            ['"quo",     "QUO"], _
            ['"structure", "STRUCTURE"], _
            ['"then",  "THEN"], _
            ['"throw", "THROW"], _
            ['"try",   "TRY"], _
            ['"until",  "UNTIL"], _
            ['"with",   "WITH" ], _
            ['"where",  "WHERE"], _
            ['"while",  "WHILE"], _
            ['".", "DOT"], _
            ['":", "COLON"], _
            ['"::", "COLON-COLON"], _
            ['"@",  "AT" ], _
            ['",", "COMMA"], _
            ['";", "SEMICOLON"], _
            ['"*", "TIMES"], _
            ['"**", "POWER"], _
            ['"/",  "SLASH"], _
            ['"+", "PLUS"], _
            ['"-", "MINUS"], _
            ['"<", "LT"], _
            ['">", "GT"] , _
            ['"<=","LE" ], _
            ['">=","GE" ], _
            ['"=", "SHOEEQ"], _
            ['"~=","SHOENE" ], _
            ['"..","SEG" ], _
            ['"#", "LENGTH"], _
            ['"=>","EXIT" ], _
            ['"->", "ARROW"],_
            ['"<-", "LARROW"], _
            ['":=", "BEC"], _
            ['"+->", "GIVES"], _
            ['"==", "DEF"], _
            ['"<=>", "TDEF"], _
            ['"(", "OPAREN"], _
            ['")", "CPAREN"], _
            ['"[", "OBRACK"], _
            ['"]", "CBRACK"], _
            ['"'", "QUOTE"], _
            ['"|", "BAR"]                          ]
 

 
shoeKeyTableCons()==
   KeyTable := makeTable function valueEq?
   for st in shoeKeyWords repeat
      tableValue(KeyTable,first st) := second st
   KeyTable
 
shoeKeyTable:=shoeKeyTableCons()
 
keywordId t ==
  s := or/[k for [k,:v] in entries shoeKeyTable | symbolEq?(v,t)] =>
    makeSymbol s
  t

shoeInsert(s,d) ==
  l := #s
  h := codePoint stringChar(s,0)
  u := d.h
  n := #u
  k:=0
  while l <= #u.k repeat
      k:=k+1
  v := newVector(n+1)
  for i in 0..k-1 repeat
    v.i := u.i
  v.k := s
  for i in k..n-1 repeat
    v.(i+1) := u.i
  d.h := v
  s
 
shoeDictCons()==
  d :=
    a := newVector 256
    b := newVector 1
    b.0 := makeString 0
    for i in 0..255 repeat
      a.i := b
    a
  for [s,:.] in entries shoeKeyTable repeat
    shoeInsert(s,d)
  d
 
shoeDict:=shoeDictCons()
 
 
shoePunCons()==
  a := makeBitVector 256
  for i in 0..255 repeat
    bitref(a,i) := 0
  for [k,:.] in entries shoeKeyTable repeat
    shoeStartsId stringChar(k,0) => nil
    bitref(a,codePoint stringChar(k,0)) := 1
  a
 
shoePun:=shoePunCons()

++ List of prefix operators. 
for i in [ _
             "NOT", _
             "LENGTH"  _
                     ] _
       repeat property(i,'SHOEPRE) := true
 
++ List of infix operators.
for i in [      _
        ["SHOEEQ"    ,"="], _
        ["TIMES"    ,"*"], _
        ["REM",    "rem"],_
        ["QUO",    "quo"],_
        ["PLUS" ,"+"], _
        ["IS"   ,"is"], _
        ["ISNT" ,"isnt"], _
        ["AND"  ,"and"], _
        ["OR"   ,"or"], _
        ["SLASH"    ,"/"], _
        ["POWER"   ,"**"], _
        ["MINUS"    ,"-"], _
        ["LT"    ,"<"], _
        ["GT"    ,">"], _
        ["LE"   ,"<="], _
        ["GE"   ,">="], _
        ["SHOENE"  ,"~="] _
                   ]_
       repeat property(first i,'SHOEINF) := second i
 

++ List of monoid operations and their neutral elements.
++ Note that `CONS' is not a monoid operations but support
++ right reduction. 
for i in [ _
      ["+",         0] , _
      ["gcd",       0] , _
      ["lcm",       1] , _
      ["STRCONC", '""] , _
      ["strconc", '""] , _
      ["CONCAT",  '""] , _
      ["MAX", -999999] , _
      ["MIN",  999999] , _
      ["*",         1] , _
      ["times",     1] , _
      ["CONS",    nil] , _
      ["append",  nil] , _
      ["append!", nil] , _
      ["UNION",   nil] , _
      ["setUnion",  nil] , _
      ["union",   nil] , _
      ["and",    true] , _
      ["or",    false] , _
      ["AND",    true] , _
      ["OR",    false]   _
                         ]
       repeat property(first i,'SHOETHETA) := rest i

for i in [ _
  ["abs",        "ABS"], _
  ["abstractChar", "CODE-CHAR"], _
  ["alphabetic?", "ALPHA-CHAR-P"], _
  ["alphanumeric?", "ALPHANUMERICP"], _
  ["and",          "AND"]  , _
  ["apply",      "APPLY"]  , _
  ["array?",    "ARRAYP"]  , _
  ["arrayRef",    "AREF"]  , _
  ["atom",        "ATOM"]  , _
  ["bitref",   "SBIT"] , _
  ["canonicalFilename", "PROBE-FILE"], _
  ["charByName", "NAME-CHAR"] , _
  ["charDowncase", "CHAR-DOWNCASE"], _
  ["charEq?",   "CHAR=" ], _
  ["charUpcase", "CHAR-UPCASE"], _
  ["charString", "STRING"] , _
  ["char?", "CHARACTERP"]  , _
  ["codePoint", "CHAR-CODE"], _
  ["cons?",      "CONSP"]  , _
  ["copy",        "COPY"]  , _
  ["copyString", "COPY-SEQ"] , _
  ["copyVector", "COPY-SEQ"] , _
  ["croak",      "CROAK"]  , _
  ["digit?",    "DIGIT-CHAR-P"]  , _
  ["exit",        "EXIT"]  , _
  ["false",        'NIL]   , _
  ["fifth",      "FIFTH"]  , _
  ["first",        "CAR"]  , _
  ["filePath",  "PATHNAME"] , _
  ["filePath?", "PATHNAMEP"] , _
  ["filePathDirectory", "PATHNAME-DIRECTORY"] , _
  ["filePathName", "PATHNAME-NAME"] , _
  ["filePathString", "NAMESTRING"] , _
  ["filePathType", "PATHNAME-TYPE"] , _
  ["float?",   "FLOATP"] , _
  ["flushOutput", "FORCE-OUTPUT"], _
  ["fourth",    "CADDDR"]  , _
  ["freshLine", "FRESH-LINE" ], _
  ["function?","FUNCTIONP"] , _
  ["functionSymbol?", "FBOUNDP"] , _
  ["gensym",    "GENSYM"]  , _
  ["genvar",    "GENVAR"]  , _
  ["integer?","INTEGERP"]  , _
  ["LAST",        "last"] , _
  ["list",        "LIST"]  , _
  ["listEq?",    "EQUAL"] , _
  ["lowerCase?", "LOWER-CASE-P"], _
  ["makeFilePath", "MAKE-PATHNAME"] , _
  ["makeSymbol", "INTERN"] , _
  ["mkpf",        "MKPF"]  , _
  ["newVector", "MAKE-ARRAY"], _
  ["nil"           ,NIL ]  , _
  ["not",         "NOT"]  , _
  ["null",        "NULL"]  , _
  ["odd?",        "ODDP"] , _
  ["or",            "OR"]  , _
  ["otherwise",      "T"]  , _
  ["property",     "GET"]  , _
  ["readInteger", "PARSE-INTEGER"], _
  ["readLispFromString", "READ-FROM-STRING"] , _
  ["readOnly?","CONSTANTP"], _
  ["removeDuplicates", "REMDUP"]  , _
  ["rest",         "CDR"]  , _
  ["sameObject?",  "EQ" ] , _
  ["scalarEq?",   "EQL" ] , _
  ["scalarEqual?","EQL" ] , _
  ["second",      "CADR"] , _
  ["setPart",   "SETELT"]  , _
  ["strconc",  "CONCAT"]  , _
  ["stringChar", "SCHAR"] , _
  ["stringDowncase", "STRING-DOWNCASE"] , _
  ["string?",  "STRINGP"]  ,_
  ["stringEq?","STRING="] , _
  ["stringUpcase", "STRING-UPCASE"] , _
  ["subSequence", "SUBSEQ"] , _
  ["symbolBinding", "FIND-SYMBOL"] , _
  ["symbolScope", "SYMBOL-PACKAGE"] , _
  ["symbolEq?", "EQ"], _
  ["symbolFunction", "SYMBOL-FUNCTION"], _
  ["symbolGlobal?", "BOUNDP"], _
  ["symbolName", "SYMBOL-NAME"], _
  ["symbolValue", "SYMBOL-VALUE"], _
  ["symbol?",  "SYMBOLP"]  , _
  ["third",      "CADDR"] , _
  ["toString", "WRITE-TO-STRING"], _
  ["true",           "T"]  , _
  ["upperCase?", "UPPER-CASE-P"], _
  ["valueEq?",    "EQUAL"] , _
  ["vector?", "SIMPLE-VECTOR-P"], _
  ["vectorRef", "SVREF"] , _
  ["writeByte",    "WRITE-BYTE"], _
  ["writeChar",    "WRITE-CHAR"], _
  ["writeInteger", "PRINC"], _
  ["writeLine",    "WRITE-LINE"], _
  ["writeNewline", "TERPRI"], _
  ["writeString",  "WRITE-STRING"], _
  ["PLUS",           "+"]  , _
  ["MINUS",     "-"]  , _
  ["TIMES",          "*"]  , _
  ["POWER",          "EXPT"]  , _
  ['QUO,     'TRUNCATE],_
  ["SLASH",       "/"]  , _
  ["LT",              "<"], _
  ["GT",              ">"] , _
  ["LE",             "<="], _
  ["GE",              ">="], _
  ["SHOEEQ",        "EQUAL"], _
  ["SHOENE",        "/="], _
  ["T",               "T$"]   _
                                ]
       repeat property(first i,'SHOERENAME) := rest i

 
for i in [ _
  ["absKind",           "CAR"] ,_
  ["absParms",         "CADR"] ,_
  ["absBody",         "CADDR"] ,_
  ["loopBody",     "loopBody"] ,_
  ["loopExit",         "last"] ,_
  ["setName",               0] , _
  ["setLabel",              1] , _
  ["setLevel",              2] , _
  ["setType",               3] , _
  ["setVar",                4] , _
  ["setLeaf",               5] , _
  ["setDef",                6] , _
  ["aGeneral",              4] , _
  ["aMode",                 1] , _
  ["aModeSet",              3] , _
  ["aTree",                 0] , _
  ["aValue",                2] , _
  ["args",              "CDR"] , _
  ["attributes",       "CADDR"] , _
  ["cacheCount",     "CADDDDR"] , _
  ["cacheName",         "CADR"] , _
  ["cacheReset",      "CADDDR"] , _
  ["cacheType",        "CADDR"] , _
  ["env",              "CADDR"] , _
  ["expr",               "CAR"] , _
  ["CAR",                "CAR"] , _
  ["mmCondition",      "CAADR"] , _
  ["mmDC",              "CAAR"] , _
  ["mmImplementation","CADADR"] , _
  ["mmSignature",       "CDAR"] , _
  ["mmTarget",         "CADAR"] , _
  ["mmSource",         "CDDAR"] , _
  ["mapOpsig",          "CAR" ] , _
  ["mapOperation",     "CAAR" ] , _
  ["mapSignature",    "CADAR" ] , _
  ["mapTarget",      "CAADAR" ] , _
  ["mapSource",      "CDADAR" ] , _
  ["mapPredicate",     "CADR" ] , _
  ["mapImpl",         "CADDR" ] , _
  ["mapKind",        "CAADDR" ] , _
  ["mode",              "CADR"] , _
  ["op",                 "CAR"] , _
  ["opcode",            "CADR"] , _
  ["opSig",             "CADR"] , _
  ["CDR",               "CDR"] , _
  ["sig",               "CDDR"] , _
  ["source",             "CDR"] , _
  ["streamCode",      "CADDDR"] , _
  ["streamDef",        "CADDR"] , _
  ["streamName",        "CADR"] , _
  ["target",             "CAR"]  _
                             ] _
       repeat property(first i,'SHOESELFUNCTION) := second i
