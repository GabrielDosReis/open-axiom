-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
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

import initial_-env
namespace BOOTTRAN
module tokens

++ converts `x', a 1-length symbol, to a character.
char x ==
  CHAR(symbolName x, 0)

shoeStartsId x ==
  alphabetic? x or x in [char "$", char "?", char "%"]

shoeIdChar x ==
  alphanumeric? x or x in [char "'", char "?", char "%"]

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
            ['"else", "ELSE"] , _
            ['"finally", "FINALLY"], _
            ['"for",  "FOR"] , _
            ['"has", "HAS"] , _
            ['"if", "IF"], _
            ['"import", "IMPORT"], _
            ['"in",  "IN" ], _
            ['"is", "IS"], _
            ['"isnt", "ISNT"] , _
            ['"leave", "LEAVE"], _
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
            ['"where",  "WHERE"], _
            ['"while",  "WHILE"], _
            ['".", "DOT"], _
            ['":", "COLON"], _
            ['"::", "COLON-COLON"], _
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
            ['"^", "NOTRETIRED"], _
            ['"^=","SHOENERETIRED" ], _
            ['"~=","SHOENE" ], _
            ['"..","SEG" ], _
            ['"#", "LENGTH"], _
            ['"=>","EXIT" ], _
            ['"->", "ARROW"],_
            ['":=", "BEC"], _
            ['"+->", "GIVES"], _
            ['"==", "DEF"], _
            ['"==>","MDEF" ], _
            ['"<=>", "TDEF"], _
            ['"(", "OPAREN"], _
            ['")", "CPAREN"], _
            ['"(|", "OBRACK"], _
            ['"|)", "CBRACK"], _
            ['"[", "OBRACK"], _
            ['"]", "CBRACK"], _
            ['"suchthat","BAR"], _
            ['"'", "QUOTE"], _
            ['"|", "BAR"]                          ]
 

 
shoeKeyTableCons()==
   KeyTable:=MAKE_-HASHTABLE("CVEC")
   for st in shoeKeyWords repeat
      HPUT(KeyTable,first st,second st)
   KeyTable
 
shoeKeyTable:=shoeKeyTableCons()
 
shoeSPACE       == QENUM('"    ", 0)
 
shoeESCAPE      == QENUM('"__  ", 0)
shoeLispESCAPE      := QENUM('"!  ", 0)
 
shoeSTRING_CHAR == QENUM('"_"  ", 0)
 
shoePLUSCOMMENT == QENUM('"+   ", 0)
 
shoeMINUSCOMMENT == QENUM('"-   ", 0)
 
shoeDOT          == QENUM('".   ", 0)
 
shoeEXPONENT1   == QENUM('"E   ", 0)
 
shoeEXPONENT2   == QENUM('"e   ", 0)
 
shoeCLOSEPAREN  == QENUM('")   ", 0)
 
--shoeCLOSEANGLE  == QENUM('">   ", 0)
shoeTAB == 9
 
shoeInsert(s,d) ==
  l := #s
  h := QENUM(s,0)
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
  l := HKEYS shoeKeyTable
  d :=
    a := newVector 256
    b := newVector 1
    b.0 := newString 0
    for i in 0..255 repeat
      a.i := b
    a
  for s in l repeat
    shoeInsert(s,d)
  d
 
shoeDict:=shoeDictCons()
 
 
shoePunCons()==
  listing := HKEYS shoeKeyTable
  a := MAKE_-BVEC 256
  for i in 0..255 repeat BVEC_-SETELT(a,i,0)
  for k in listing repeat
     if not shoeStartsId k.0
     then BVEC_-SETELT(a,QENUM(k,0),1)
  a
 
shoePun:=shoePunCons()

++ List of prefix operators. 
for i in [ _
             "NOT", _
--           "COLON", _
--           "SHOEEQ", _
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
      ["APPEND",  nil] , _
      ["append",  nil] , _
      ["UNION",   nil] , _
      ["UNIONQ",  nil] , _
      ["union",   nil] , _
      ["NCONC",   nil] , _
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
  ["append",    "APPEND"]  , _
  ["apply",      "APPLY"]  , _
  ["atom",        "ATOM"]  , _
  ["canonicalFilename", "PROBE-FILE"], _
  ["charString", "STRING"] , _
  ["char?", "CHARACTERP"]  , _
  ["codePoint", "CHAR-CODE"], _
  ["cons?",      "CONSP"]  , _
  ["copy",        "COPY"]  , _
  ["croak",      "CROAK"]  , _
  ["digit?",    "DIGIT-CHAR-P"]  , _
  ["drop",        "DROP"]  , _
  ["exit",        "EXIT"]  , _
  ["false",        'NIL]   , _
  ["first",        "CAR"]  , _
  ["fourth",    "CADDDR"]  , _
  ["function","FUNCTION"] , _
  ["gensym",    "GENSYM"]  , _
  ["genvar",    "GENVAR"]  , _
  ["integer?","INTEGERP"]  , _
  ["lastNode",    "LAST"]  , _
  ["LAST",        "last"] , _
  ["list",        "LIST"]  , _
  ["lowerCase?", "LOWER-CASE-P"], _
  ["mkpf",        "MKPF"]  , _
  ["nconc",      "NCONC"]  , _
  ["newString", "MAKE-STRING"], _
  ["newVector", "MAKE-ARRAY"], _
  ["nil"           ,NIL ]  , _
  ["not",         "NOT"]  , _
  ["nreverse", "NREVERSE"]  , _
  ["null",        "NULL"]  , _
  ["or",            "OR"]  , _
  ["otherwise",      "T"]  , _
  ["property",     "GET"]  , _
  ["readByte", "READ-BYTE"], _
  ["readInteger", "PARSE-INTEGER"], _
  ["readLine", "READ-LINE"], _
  ["readOnly?","CONSTANTP"], _
  ["removeDuplicates", "REMDUP"]  , _
  ["rest",         "CDR"]  , _
  ["reverse",  "REVERSE"]  , _
  ["second",      "CADR"] , _
  ["setDifference", "SETDIFFERENCE"]  , _
  ["setIntersection", "INTERSECTION"]  , _
  ["setPart",   "SETELT"]  , _
  ["setUnion",   "UNION"]  , _
  ["strconc",  "CONCAT"]  , _
  ["string?",  "STRINGP"]  ,_
  ["subSequence", "SUBSEQ"] , _
  ["substitute",  "SUBST"]  , _
  ["substitute!", "NSUBST"]  , _
  ["symbol?",  "SYMBOLP"]  , _
  ["symbolName", "SYMBOL-NAME"], _
  ["take",        "TAKE"]  , 
  ["third",      "CADDR"] , _
  ["toString", "WRITE-TO-STRING"], _
  ["true",           "T"]  , _
  ["upperCase?", "UPPER-CASE-P"], _
  ["vector?", "SIMPLE-VECTOR-P"], _
  ["writeByte", "WRITE-BYTE"], _
  ["writeLine", "WRITE-LINE"], _
  ["PLUS",           "+"]  , _
  ["MINUS",     "-"]  , _
  ["TIMES",          "*"]  , _
  ["POWER",          "EXPT"]  , _
  ['REM,          'REM],_
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
