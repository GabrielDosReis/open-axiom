-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
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

++ Table of Boot keywords and their token name.
shoeKeyWords == [  _
            ['"and","AND"] , _
            ['"by", "BY" ], _
            ['"case","CASE"] , _
            ['"catch","CATCH"], _
            ['"cross","CROSS"] , _
            ['"else", "ELSE"] , _
            ['"for",  "FOR"] , _
            ['"if", "IF"], _
            ['"import", "IMPORT"], _
            ['"in",  "IN" ], _
            ['"is", "IS"], _
            ['"isnt", "ISNT"] , _
            ['"module", "MODULE"], _
            ['"of",   "OF"] , _
            ['"or",   "OR"] , _
            ['"repeat", "REPEAT"] , _
            ['"return", "RETURN"], _
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
            ['"^", "NOT"], _
            ['"^=","SHOENE" ], _
            ['"..","SEG" ], _
            ['"#", "LENGTH"], _
            ['"=>","EXIT" ], _
            ['"->", "ARROW"],_
            ['":=", "BEC"], _
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
      u := ELT(d,h)
      n := #u
      k:=0
      while l <= #(ELT(u,k)) repeat
          k:=k+1
      v := MAKE_-VEC(n+1)
      for i in 0..k-1 repeat VEC_-SETELT(v,i,ELT(u,i))
      VEC_-SETELT(v,k,s)
      for i in k..n-1 repeat VEC_-SETELT(v,i+1,ELT(u,i))
      VEC_-SETELT(d,h,v)
      s
 
shoeDictCons()==
      l:= HKEYS shoeKeyTable
      d :=
          a:=MAKE_-VEC(256)
          b:=MAKE_-VEC(1)
          VEC_-SETELT(b,0,MAKE_-CVEC 0)
          for i in 0..255 repeat VEC_-SETELT(a,i,b)
          a
      for s in l repeat shoeInsert(s,d)
      d
 
shoeDict:=shoeDictCons()
 
 
shoePunCons()==
    listing := HKEYS shoeKeyTable
    a:=MAKE_-BVEC 256
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
       repeat SETF (GET(i,'SHOEPRE),'T)
 
++ List of infix operators.
for i in [      _
        ["SHOEEQ"    ,"="], _
        ["TIMES"    ,"*"], _
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
        ["SHOENE"  ,"^="] _
                   ]_
       repeat SETF (GET(first i,'SHOEINF),second i)
 

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
      ["times",         1] , _
      ["CONS",    NIL] , _
      ["APPEND",  NIL] , _
      ["append",  NIL] , _
      ["UNION",   NIL] , _
      ["UNIONQ",  NIL] , _
      ["union",   NIL] , _
      ["NCONC",   NIL] , _
      ["and",      'T] , _
      ["or",      NIL] , _
      ["AND",      'T] , _
      ["OR",      NIL]   _
                         ]
 
       repeat SETF (GET(first i,'SHOETHETA),CDR i)

for i in [ _
  ["and",          "AND"]  , _
  ["append",    "APPEND"]  , _
  ["apply",      "APPLY"]  , _
  ["atom",        "ATOM"]  , _
  ["car",          "CAR"]  , _
  ["cdr",          "CDR"]  , _
  ["cons",        "CONS"]  , _
  ["copy",        "COPY"]  , _
  ["croak",      "CROAK"]  , _
  ["drop",        "DROP"]  , _
  ["exit",        "EXIT"]  , _
  ["false",        'NIL]   , _
  ["first",        "CAR"]  , _
  ["fourth",    "CADDDR"]  , _
  ["function","FUNCTION"] , _
  ["genvar",    "GENVAR"]  , _
  ["IN",        "MEMBER"]  , _
  ["is",            "IS"]  , _
  ["isnt",        "ISNT"]  , _
  ["lastNode",    "LAST"]  , _
  ["LAST",        "last"] , _
  ["list",        "LIST"]  , _
--  ["member",        "MEMBER"]  , _
  ["mkpf",        "MKPF"]  , _
  ["nconc",      "NCONC"]  , _
  ["nil"           ,NIL ]  , _
  ["not",         "NOT"]  , _
  ["nreverse", "NREVERSE"]  , _
  ["null",        "NULL"]  , _
  ["or",            "OR"]  , _
  ["otherwise",      "T"]  , _
  ["PAIRP",      "CONSP"]  , _
  ["removeDuplicates", "REMDUP"]  , _
  ["rest",         "CDR"]  , _
  ["reverse",  "REVERSE"]  , _
  ["second",      "CADR"] , _
  ["setDifference", "SETDIFFERENCE"]  , _
  ["setIntersection", "INTERSECTION"]  , _
  ["setPart",   "SETELT"]  , _
  ["setUnion",   "UNION"]  , _
  ["size",        "SIZE"]  , _
  ["strconc",  "CONCAT"]  , _
  ["substitute", "SUBST"]  , _
  ["take",        "TAKE"]  , 
  ["third",      "CADDR"] , _
  ["true",           "T"]  , _
  ["PLUS",           "+"]  , _
  ["MINUS",     "-"]  , _
  ["TIMES",          "*"]  , _
  ["POWER",          "EXPT"]  , _
  ["SLASH",       "/"]  , _
  ["LT",              "<"], _
  ["GT",              ">"] , _
  ["LE",             "<="], _
  ["GE",              ">="], _
  ["SHOEEQ",        "EQUAL"], _
  ["SHOENE",        "/="], _
  ["T",               "T$"]   _
                                ]
       repeat SETF (GET(first i,'SHOERENAME),CDR i)

-- For code written in `Old Boot', we would like to warn about
-- the difference in renaming.
for i in [ _
  ["PLUS", "PLUS"], _
  ["and", "AND"], _
  ["append", "APPEND"], _
  ["apply", "APPLY"], _
  ["atom", "ATOM"], _
  ["brace", "REMDUP"], _
  ["car", "CAR"], _
  ["cdr", "CDR"], _
  ["cons", "CONS"], _
  ["copy", "COPY"], _
  ["croak", "CROAK"], _
  ["drop", "DROP"], _
  ["exit", "EXIT"], _
  ["false", "NIL"], _
  ["first", "CAR"], _
  ["genvar", "GENVAR"], _
  ["in", "member"], _
  ["is", "IS"], _
  ["lastNode", "LASTNODE"], _
  ["list", "LIST"], _
  ["mkpf", "MKPF"], _
  ["nconc", "NCONC"], _
  ["nil", "NIL"], _
  ["not", "NOT"], _
  ["nreverse", "NREVERSE"], _
  ["null", "NULL"], _
  ["or", "OR"], _
  ["otherwise", "T"], _
  ["removeDuplicates", "REMDUP"], _
  ["rest", "CDR"], _
  ["return", "RETURN"], _
  ["reverse", "REVERSE"], _
  ["setDifference", "SETDIFFERENCE"], _
  ["setIntersection", "intersection"], _
  ["setPart", "SETELT"], _
  ["setUnion", "union"], _
  ["size", "SIZE"], _
  ["strconc", "STRCONC"], _
  ["substitute", "MSUBST"], _
  ["SUBST", "MSUBST"], _
  ["take", "TAKE"], _
  ["true", "T"], _
  ["where", "WHERE"], _
  ["TIMES", "TIMES"], _
  ["POWER", "EXPT"], _
  ["SHOENE", "NEQUAL"], _
  ["MINUS", "SPADDIFFERENCE"], _
  ["SLASH", "QUOTIENT"], _
  ["=", "EQUAL"], _
  ["SHOEEQ", "EQUAL"], _
  ["ASSOC",  "assoc"], _
  ["DELETE", "delete"], _
  ["GET", "GETL"], _
  ["INTERSECTION", "intersection"], _
  ["LAST", "last"], _
  ["MEMBER", "member"], _
  ["RASSOC", "rassoc"], _
  ["READ", "VMREAD"], _
  ["READ-LINE", "read-line"], _
  ["REDUCE", "SPADREDUCE"], _
  ["REMOVE", "remove"], _
  ["BAR", "SUCHTHAT"], _
  ["T", "T$"], _
  ["IN", "member"], _
  ["UNION", "union"]_
                     ]
  repeat SETF (GET(first i,'OLD_-BOOT),CDR i)

-- The following difference in renaming are verified to be OK.
for i in [ _
  "LT", "LE", _
  "GT", "GE", _
  "SHOENE", _
  "TIMES", "PLUS", _
  "MINUS", "function",_
  "PAIRP"
   ]
  repeat SETF(GET(i, 'RENAME_-OK), true)

  
 
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
       repeat SETF (GET(first i,'SHOESELFUNCTION),second i)
