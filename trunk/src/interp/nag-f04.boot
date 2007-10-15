-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007, Gabriel Dos Reis.
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
--     - Neither the name of The Numerical ALgorithms Group Ltd. nor the
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


)package "BOOT"

f04adf() ==
  htInitPage("F04ADF - Solution of complex simultaneous linear equations, with multiple right-hand sides (Black box)",nil)
  htMakePage '(
    (domainConditions
      (isDomain I (Integer)))
    (text . "\windowlink{Manual Page}{manpageXXf04adf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f04adf| '|NagLinearEquationSolvingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Calculates the approximate solution of a set of complex linear ")
    (text . "equations {\it AX = B} using an {\it LU} factorization with ")
    (text . "partial pivoting, where {\it A} is an n * n matrix, {\it X} is ")
    (text . "an {\it n} by {\it m} matrix of unknowns and {\it B} is an ")    
    (text . "{\it n} by {\it m} matrix of right-hand sides.")
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "{\it n} order of matrix A:")
    (text . "\tab{28} \menuitemstyle{}\tab{30} ")
    (text . "{\it m} number of right-hand sides \htbitmap{great=} 0 :")
    (text . "\newline\tab{2} ")
    (bcStrings (10 3 n I))
    (text . "\tab{30} ")
    (bcStrings (10 1 m I))
--    (text . "\blankline ")
--    (text . "\newline \menuitemstyle{}\tab{2} ")
--    (text . "{\it IA} first dimension of A:")
--    (text . "\tab{32} \menuitemstyle{}\tab{34} ")
--    (text . "{\it IB} first dimension of B:")
--    (text . "\newline\tab{2} ")
--    (bcStrings (10 3 ia I))
--    (text . "\tab{34} ")
--    (bcStrings (10 3 ib I))
--    (text . "\blankline ")
--    (text . "\newline \menuitemstyle{}\tab{2} ")
--    (text . "{\it IC} first dimension of C:")
--    (text . "\newline\tab{2} ")
--    (bcStrings (10 3 ic I))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{} \tab{2} ")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f04adfSolve)
  htShowPage()

f04adfSolve htPage ==
  n := 
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  m := 
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  ia := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ia)
--    objValUnwrap htpLabelSpadValue(htPage, 'ia)
  ib := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ib)
--    objValUnwrap htpLabelSpadValue(htPage, 'ib)
  ic := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ic)
--    objValUnwrap htpLabelSpadValue(htPage, 'ic)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (n = '3 and m = '1) => f04adfDefaultSolve(htPage,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..ia] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            ianam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[12, "0.0 + 0.0*%i", ianam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  bmatList :=
    "append"/[fb(i,m) for i in 1..ib] where fb(i,m) ==
       blabelList := 
         "append"/[gb(i,j) for j in 1..m] where gb(i,j) ==
            bnam := INTERN STRCONC ('"b",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[12, "0.0 + 0.0*%i", bnam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       blabelList := [['text,:prefix],:blabelList]
  start := ('"\blankline \menuitemstyle{} \tab{2} Enter values of {\it b}:")
  bmatList := [['text,:start],:bmatList]  
  equationPart := [
     '(domainConditions
        (isDomain P (Polynomial $EmptyMode))
          (isDomain F (Float))),
            :matList,:bmatList]
  page := htInitPage("F04ADF - Solution of complex simultaneous linear equations, with multiple right-hand sides (Black box)",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f04adfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'ib,ib)
--  htpSetProperty(page,'ic,ic)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()   
       
  

f04adfDefaultSolve (htPage, ifail) ==
  n := '3
  m := '1
  ia := '3
  ib := '3
  ic := '3
  page := htInitPage("F04ADF - Solution of complex simultaneous linear equations, with multiple right-hand sides (Black box)",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (12 "1" a11 F))
    (bcStrings (12 "1 + 2*%i" a12 F))
    (bcStrings (12 "2 + 10*%i" a13 F))
    (text . "\newline \tab{2} ")
    (bcStrings (12 "1 + %i" a21 F))
    (bcStrings (12 "3*%i" a22 F))
    (bcStrings (12 "-5 + 14*%i" a23 F))
    (text . "\newline \tab{2} ")
    (bcStrings (12 "1 + %i" a31 F))
    (bcStrings (12 "5*%i" a32 F))
    (bcStrings (12 "-8 + 20*%i" a33 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it b}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (12 "1" b1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (12 "0" b2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (12 "0" b3 F)))
  htMakeDoneButton('"Continue",'f04adfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'ib,ib)
--  htpSetProperty(page,'ic,ic)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f04adfGen htPage ==
  n := htpProperty(htPage,'n)
  m := htpProperty(htPage,'m)
--  ia := htpProperty(htPage,'ia)
--  ib := htpProperty(htPage,'ib)
--  ic := htpProperty(htPage,'ic)
  ia := n
  ib := n
  ic := n
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  -- will probably need to change this as its a vector not an array
  for i in 1..m repeat
    for j in 1..ib repeat
      right := STRCONC((first y).1," ")
      y := rest y
      bList := [right,:bList]
      bstring := bcwords2liststring bList
    boutList := [bstring,:boutList]
    bList := []
  boutstring := bcwords2liststring boutList
  y := REVERSE y
  k := -1
  matform := [[y.(k := k + 1).1 for j in 0..(n-1)] for i in 0..(ia-1)]
  matstring := bcwords2liststring [bcwords2liststring x for x in matform] 
  prefix := STRCONC('"f04adf(",STRINGIMAGE ia,",",boutstring,",")
  prefix := STRCONC(prefix,STRINGIMAGE ib,", ",STRINGIMAGE n,", ")
  prefix := STRCONC(prefix,STRINGIMAGE m,", ",STRINGIMAGE ic)
  prefix := STRCONC(prefix,", ",matstring,", ",STRINGIMAGE ifail,")")
  bcGen prefix

f04arf() ==
  htInitPage("F04ARF - Solution of real simultaneous linear equations, one right-hand side (Black box)",nil)
  htMakePage '(
    (domainConditions
      (isDomain I (Integer)))
    (text . "\windowlink{Manual Page}{manpageXXf04arf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f04arf| '|NagLinearEquationSolvingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\blankline ")
    (text . "Calculates the approximate solution of a set of real linear ")
    (text . "equations {\it Ax = b} using an {\it LU} factorization with ")
    (text . "pivoting, where {\it A} is an n * n matrix, {\it x} is an n ")
    (text . "element vector of unknowns and {\it b} is an n element ")
    (text . "right-hand side vector.")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
--    (text . "{\it IA} first dimension of A:")
--    (text . "\tab{32} \menuitemstyle{}\tab{34} ")
    (text . "{\it n} order of matrix A:")
    (text . "\newline\tab{2} ")
--    (bcStrings (10 8 ia I))
--    (text . "\tab{34} ")
    (bcStrings (10 3 n I))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{} \tab{2} ")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f04arfSolve)
  htShowPage()

f04arfSolve htPage ==
  n := 
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  ia := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ia)
--    objValUnwrap htpLabelSpadValue(htPage, 'ia)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '3 => f04arfDefaultSolve(htPage,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..ia] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            ianam := INTERN STRCONC ('"ia",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", ianam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  bmatList := 
    "append"/[h(k) for k in 1..n] where h(k) ==
       prefix := ('"\newline \tab{2} ")
       bnam := INTERN STRCONC ('"b",STRINGIMAGE k)
       [['text,:prefix],['bcStrings,[6, "0.0", bnam, 'F]]]
  start := ('"\blankline \menuitemstyle{} \tab{2} Enter values of {\it b}:")
  bmatList := [['text,:start],:bmatList]  
  equationPart := [
     '(domainConditions
        (isDomain P (Polynomial $EmptyMode))
          (isDomain F (Float))),
            :matList,:bmatList]
  page := htInitPage("F04ARF - Solution of real simultaneous linear equations, one right-hand side (Black box)",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f04arfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()   
       
  

f04arfDefaultSolve (htPage, ifail) ==
  n := '3
  ia := '3
  page := htInitPage("F04ARF - Solution of real simultaneous linear equations, one right-hand side (Black box)",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 33 ia11 F))
    (bcStrings (6 16 ia12 F))
    (bcStrings (6 72 ia13 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-24" ia21 F))
    (bcStrings (6 "-10" ia22 F))
    (bcStrings (6 "-57" ia23 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-8" ia31 F))
    (bcStrings (6 "-4" ia32 F))
    (bcStrings (6 "-17" ia33 F))
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 0 ia41 F))
--    (bcStrings (6 0 ia42 F))
--    (bcStrings (6 0 ia43 F))
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 0 ia51 F))
--    (bcStrings (6 0 ia52 F))
--    (bcStrings (6 0 ia53 F))
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 0 ia61 F))
--    (bcStrings (6 0 ia62 F))
--    (bcStrings (6 0 ia63 F))
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 0 ia71 F))
--    (bcStrings (6 0 ia72 F))
--    (bcStrings (6 0 ia73 F))
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 0 ia81 F))
--    (bcStrings (6 0 ia82 F))
--    (bcStrings (6 0 ia83 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it b}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-359" b1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "281" b2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "85" b3 F)))
  htMakeDoneButton('"Continue",'f04arfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f04arfGen htPage ==
  n := htpProperty(htPage,'n)
--  ia := htpProperty(htPage,'ia)
  ia := n
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..n repeat
    right := STRCONC((first y).1," ")
    y := rest y
    bList := [right,:bList]
  bstring := bcwords2liststring bList
  y := REVERSE y
  k := -1
  matform := [[y.(k := k + 1).1 for j in 0..(n-1)] for i in 0..(ia-1)]
  matstring := bcwords2liststring [bcwords2liststring x for x in matform] 
  prefix := STRCONC('"f04arf(",STRINGIMAGE ia,", [",bstring,"],",STRINGIMAGE n)
  prefix := STRCONC(prefix,", ",matstring,", ",STRINGIMAGE ifail,")")
  bcGen prefix

f04asf() ==
  htInitPage("F04ASF - Solution of real symmetric positive-definite simultaneous linear equations, one right-hand side using iterative refinement (Black box)",nil)
  htMakePage '(
    (domainConditions
      (isDomain I (Integer)))
    (text . "\windowlink{Manual Page}{manpageXXf04asf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f04asf| '|NagLinearEquationSolvingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\blankline ")
    (text . "Calculates the accurate solution of a set of real symmetric ")
    (text . "positive-definite linear equations {\it Ax = b} using an a ")
    (text . "Cholesky factorization and iterative refinement, ")
    (text . "where {\it A} is an n * n matrix, {\it x} is an n ")
    (text . "element vector of unknowns and {\it b} is an n element ")
    (text . "right-hand side vector.")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
--    (text . "{\it IA} first dimension of A:")
--    (text . "\tab{32} \menuitemstyle{}\tab{34} ")
    (text . "{\it n} order of matrix A:")
    (text . "\newline\tab{2} ")
--    (bcStrings (10 8 ia I))
--    (text . "\tab{34} ")
    (bcStrings (10 4 n I))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{} \tab{2} ")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f04asfSolve)
  htShowPage()

f04asfSolve htPage ==
  n := 
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  ia := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ia)
--    objValUnwrap htpLabelSpadValue(htPage, 'ia)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
--  (n = '4 and ia = '8) => f04asfDefaultSolve(htPage,ifail)
  n = '4 => f04asfDefaultSolve(htPage,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..ia] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            ianam := INTERN STRCONC ('"ia",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", ianam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  bmatList := 
    "append"/[h(k) for k in 1..n] where h(k) ==
       prefix := ('"\newline \tab{2} ")
       bnam := INTERN STRCONC ('"b",STRINGIMAGE k)
       [['text,:prefix],['bcStrings,[6, "0.0", bnam, 'F]]]
  start := ('"\blankline \menuitemstyle{} \tab{2} Enter values of {\it b}:")
  bmatList := [['text,:start],:bmatList]  
  equationPart := [
     '(domainConditions
        (isDomain P (Polynomial $EmptyMode))
          (isDomain F (Float))),
            :matList,:bmatList]
  page := htInitPage("F04ASF - Solution of real symmetric positive-definite simultaneous linear equations, one right-hand side using iterative refinement (Black box)",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f04asfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()   
       
  

f04asfDefaultSolve (htPage, ifail) ==
  n := '4
  ia := '4
  page := htInitPage("F04ASF - Solution of real symmetric positive-definite simultaneous linear equations, one right-hand side using iterative refinement (Black box)",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 5 ia11 F))
    (bcStrings (6 7 ia12 F))
    (bcStrings (6 6 ia13 F))
    (bcStrings (6 5 ia14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 7 ia21 F))
    (bcStrings (6 10 ia22 F))
    (bcStrings (6 8 ia23 F))
    (bcStrings (6 7 ia24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 6 ia31 F))
    (bcStrings (6 8 ia32 F))
    (bcStrings (6 10 ia33 F))
    (bcStrings (6 9 ia34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 5 ia41 F))
    (bcStrings (6 7 ia42 F))
    (bcStrings (6 9 ia43 F))
    (bcStrings (6 10 ia44 F))
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 0 ia51 F))
--    (bcStrings (6 0 ia52 F))
--    (bcStrings (6 0 ia53 F))
--    (bcStrings (6 0 ia54 F))
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 0 ia61 F))
--    (bcStrings (6 0 ia62 F))
--    (bcStrings (6 0 ia63 F))
--    (bcStrings (6 0 ia64 F))
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 0 ia71 F))
--    (bcStrings (6 0 ia72 F))
--    (bcStrings (6 0 ia73 F))
--    (bcStrings (6 0 ia74 F))
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 0 ia81 F))
--    (bcStrings (6 0 ia82 F))
--    (bcStrings (6 0 ia83 F))
--    (bcStrings (6 0 ia84 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it b}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 23 b1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 32 b2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 33 b3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 31 b4 F)))
  htMakeDoneButton('"Continue",'f04asfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f04asfGen htPage ==
  n := htpProperty(htPage,'n)
--  ia := htpProperty(htPage,'ia)
  ia := n
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..n repeat
    right := STRCONC((first y).1," ")
    y := rest y
    bList := [right,:bList]
  bstring := bcwords2liststring bList
  y := REVERSE y
  k := -1
  matform := [[y.(k := k + 1).1 for j in 0..(n-1)] for i in 0..(ia-1)]
  matstring := bcwords2liststring [bcwords2liststring x for x in matform] 
  prefix := STRCONC('"f04asf(",STRINGIMAGE ia,", [",bstring,"],",STRINGIMAGE n)
  prefix := STRCONC(prefix,", ",matstring,", ",STRINGIMAGE ifail,")")
  bcGen prefix

f04atf() ==
  htInitPage("F04ATF - Solution of real simultaneous linear equations, one right-hand side using iterative refinement (Black box)",nil)
  htMakePage '(
    (domainConditions
      (isDomain I (Integer)))
    (text . "\windowlink{Manual Page}{manpageXXf04atf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f04atf| '|NagLinearEquationSolvingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\blankline ")
    (text . "Calculates the approximate solution of a set of real linear ")
    (text . "equations {\it Ax = b} using an {\it LU} factorization with ")
    (text . "pivoting and iterative refinement, ")
    (text . "where {\it A} is an n * n matrix, {\it x} is an n ")
    (text . "element vector of unknowns and {\it b} is an n element ")
    (text . "right-hand side vector.")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
--    (text . "{\it IA} first dimension of A:")
--    (text . "\tab{32} \menuitemstyle{}\tab{34} ")
    (text . "{\it n} order of matrix A:")
    (text . "\newline\tab{2} ")
--    (bcStrings (10 8 ia I))
--    (text . "\tab{34} ")
    (bcStrings (10 3 n I))
--    (text . "\blankline ")
--    (text . "\newline \menuitemstyle{} \tab{2} ")
--    (text . "{\it IAA} first dimension of AA:")
--    (text . "\newline \tab{2} ")
--    (bcStrings (10 8 iaa I))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{} \tab{2} ")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f04atfSolve)
  htShowPage()

f04atfSolve htPage ==
  n := 
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  ia :=  n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ia)
--    objValUnwrap htpLabelSpadValue(htPage, 'ia)
  iaa := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iaa)
--    objValUnwrap htpLabelSpadValue(htPage, 'iaa)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
--  (n = '3 and ia = '8) => f04atfDefaultSolve(htPage,iaa,ifail)
  n = '3 => f04atfDefaultSolve(htPage,iaa,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..ia] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            ianam := INTERN STRCONC ('"ia",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", ianam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  bmatList := 
    "append"/[h(k) for k in 1..n] where h(k) ==
       prefix := ('"\newline \tab{2} ")
       bnam := INTERN STRCONC ('"b",STRINGIMAGE k)
       [['text,:prefix],['bcStrings,[6, "0.0", bnam, 'F]]]
  start := ('"\blankline \menuitemstyle{} \tab{2} Enter values of {\it b}:")
  bmatList := [['text,:start],:bmatList]  
  equationPart := [
     '(domainConditions
        (isDomain P (Polynomial $EmptyMode))
          (isDomain F (Float))),
            :matList,:bmatList]
  page := htInitPage("F04ATF - Solution of real simultaneous linear equations, one right-hand side using iterative refinement (Black box)",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f04atfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'iaa,iaa)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()   
       
  

f04atfDefaultSolve (htPage, iaa, ifail) ==
  n := '3
  ia := '3
  page := htInitPage("F04ATF - Solution of real simultaneous linear equations, one right-hand side using iterative refinement (Black box)",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 33 ia11 F))
    (bcStrings (6 16 ia12 F))
    (bcStrings (6 72 ia13 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-24" ia21 F))
    (bcStrings (6 "-10" ia22 F))
    (bcStrings (6 "-57" ia23 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-8" ia31 F))
    (bcStrings (6 "-4" ia32 F))
    (bcStrings (6 "-17" ia33 F))
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 0 ia41 F))
--    (bcStrings (6 0 ia42 F))
--    (bcStrings (6 0 ia43 F))
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 0 ia51 F))
--    (bcStrings (6 0 ia52 F))
--    (bcStrings (6 0 ia53 F))
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 0 ia61 F))
--    (bcStrings (6 0 ia62 F))
--    (bcStrings (6 0 ia63 F))
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 0 ia71 F))
--    (bcStrings (6 0 ia72 F))
--    (bcStrings (6 0 ia73 F))
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 0 ia81 F))
--    (bcStrings (6 0 ia82 F))
--    (bcStrings (6 0 ia83 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it b}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-359" b1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "281" b2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "85" b3 F)))
  htMakeDoneButton('"Continue",'f04atfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'iaa,iaa)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f04atfGen htPage ==
  n := htpProperty(htPage,'n)
--  ia := htpProperty(htPage,'ia)
--  iaa := htpProperty(htPage,'iaa)
  ia := n
  iaa := n
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..n repeat
    right := STRCONC((first y).1," ")
    y := rest y
    bList := [right,:bList]
  bstring := bcwords2liststring bList
  y := REVERSE y
  k := -1
  matform := [[y.(k := k + 1).1 for j in 0..(n-1)] for i in 0..(ia-1)]
  matstring := bcwords2liststring [bcwords2liststring x for x in matform] 
  prefix := STRCONC('"f04atf(",matstring,", ",STRINGIMAGE ia,", [",bstring)
  prefix := STRCONC(prefix,"],",STRINGIMAGE n,", ",STRINGIMAGE iaa,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,")")
  bcGen prefix


f04faf() ==
  htInitPage('"F04FAF - Solution of real symmetric positive-definite tridiagonal simultaneous linear equations, one right-hand side (Black box)",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf04adf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f04faf| '|NagLinearEquationSolvingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\blankline ")
    (text . "Calculates the approximate solution of a set of real symmetric ")
    (text . "positive-definite tridiagonal linear equations {\it Tx = b} ")
    (text . "using a modified symmetric Gaussian Elimination algorithm, ")
    (text . "where {\it T} is an n * n matrix, {\it x} is an n ")
    (text . "element vector of unknowns and {\it b} is an n element ")
    (text . "right-hand side vector. {\it T} is factorized as ")
    (text . "\inputbitmap{\htbmdir{}/mkm.bitmap}, where {\it K} is a diagonal matrix ")
    (text . "and {\it M} is a matrix of multipliers. ")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "{\it JOB} to be performed by f04faf: ")
    (radioButtons job
        ("" " = 0. {\it T} is factorized and equations {\it Tx = b} are solved for x." jobZero)
        ("" " = 1. {\it T} assumed to be already factorized by previous call to f04faf, the equations {\it Tx = b} are solved for x." jobOne))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline Order of the matrix T {\it n}:")
    (text . "\newline \tab{2} ")
    (bcStrings (6 5 n PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f04fafSolve)
  htShowPage()

f04fafSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  number := htpButtonValue(htPage,'job)
  job :=
    number = 'jobOne => '1
    '0
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '5  => f04fafDefaultSolve(htPage,job,ifail)
  dList :=
    "append"/[f(i) for i in 1..n] where f(i) ==
      prefix := ('"\newline \tab{2} ")
      dnam := INTERN STRCONC ('"d",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[10, 0.0, dnam, 'F]]]
  prefix := ('"\menuitemstyle{}\tab{2} {\it D} Diagonal elements of T: ")
  prefix := STRCONC(prefix,"\newline \tab{2} ")
  dList := [['text,:prefix],:dList]
  eList := 
    "append"/[g(j) for j in 1..(n-1)] where g(j) ==
      prefix := ('"\newline \tab{2} ")
      enam := INTERN STRCONC ('"e",STRINGIMAGE j)
      [['text,:prefix],['bcStrings,[10, 0.0, enam, 'F]]]
  prefix := ('"\blankline \newline \menuitemstyle{}\tab{2} {\it E} E(2) ")
  prefix := STRCONC(prefix,"to E(N)\newline \tab{2} Job = 0 => super-diagonal")
  prefix := STRCONC(prefix," elements of {\it T}. \newline \tab{2} Job = 1 =>")
  prefix := STRCONC(prefix," off-diagonal elements of {\it M} from previous ")
  prefix := STRCONC(prefix,"call to F04FAF. ")
  eList := [['text,:prefix],:eList] 
  bList := 
    "append"/[h(k) for k in 1..n] where h(k) ==
      prefix := ('"\newline \tab{2} ")
      bnam := INTERN STRCONC ('"b",STRINGIMAGE k)
      [['text,:prefix],['bcStrings,[10, 0.0, bnam, 'F]]]
  prefix := ('"\blankline \newline \menuitemstyle{}\tab{2} {\it B} Right-hand")
  prefix := STRCONC(prefix," side vector b: ")
  bList := [['text,:prefix],:bList] 
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :dList,:eList,:bList]
  page := htInitPage("F04FAF - Solution of real symmetric positive-definite tridiagonal simultaneous linear equations, one right-hand side (Black box)",nil)
  htMakePage equationPart
  htMakeDoneButton('"Continue",'f04fafGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'job,job)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


f04fafDefaultSolve  (htPage,job,ifail) == 
  n := '5
  page := htInitPage('"F04FAF - Solution of real symmetric positive-definite tridiagonal simultaneous linear equations, one right-hand side (Black box)",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} {\it D} Diagonal elements of T:")
    (text . "\newline \tab{2} ")
    (bcStrings (10 4 d1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 10 d2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 29 d3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 25 d4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 5 d5 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} {\it E}\space{1} E(2) to E(N) ")
    (text . "\newline \tab{2} ")
    (text . "Job = 0 => super-diagonal elements of {\it T}. \newline \tab{2}")
    (text . "Job = 1 => off-diagonal elements of {\it M} from ")
    (text . "previous call to F04FAF \newline \tab{2} ")
    (bcStrings (10 "-2" e2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "-6" e3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 15 e4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 8 e5 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} {\it B} Right-hand side vector b:")
    (text . "\newline \tab{2} ")
    (bcStrings (10 6 b1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 9 b2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 2 b3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 14 b4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 7 b5 F)))
  htMakeDoneButton('"Continue",'f04fafGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'job,job)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f04fafGen htPage ==
  n := htpProperty(htPage,'n)
  job := htpProperty(htPage,'job)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..n repeat
    b := STRCONC((first y).1," ")
    bList := [b,:bList]
    y := rest y
  bstring := bcwords2liststring bList
  for i in 1..(n-1) repeat
    e := STRCONC((first y).1," ")
    eList := [e,:eList]
    y := rest y
  eList := ['"0",:eList]
  estring := bcwords2liststring eList
  for i in 1..n repeat
    d := STRCONC((first y).1," ")
    dList := [d,:dList]
    y := rest y
  dstring := bcwords2liststring dList
  prefix := STRCONC('"f04faf(",STRINGIMAGE job,", ",STRINGIMAGE n,",[")
  prefix := STRCONC(prefix,dstring,"], [",estring,"], [",bstring,"], ")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,")")
  bcGen prefix


f04jgf() ==
  htInitPage('"F04JGF - Least-squares (if rank = n) or minimal least-squares (if rank < n) solution of m real equations in n unknowns, rank \inputbitmap{\htbmdir{}/less=.bitmap} it n, m \inputbitmap{\htbmdir{}/great=.bitmap} n",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf04jgf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f04jgf| '|NagLinearEquationSolvingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\blankline ")
    (text . "Finds the solution of a linear least squares problem {\it Ax=b},")
    (text . " where A is a real m by n matrix, (m \inputbitmap{\htbmdir{}/great=.bitmap}")
    (text . " n), x is an n element vector of unknowns and b is an m element ")
    (text . "right-hand side vector.  The routine uses a QU factorization if ")
    (text . "rank A = n and the SVD if A < n. ")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Rows of matrix A, {\it m}: ")
    (text . "\tab{32}  \menuitemstyle{} \tab{34} ")
    (text . "Columns of matrix A, {\it n}: \newline \tab{2} ")
    (bcStrings (6 6 m PI))
    (text . "\tab{34} ")
    (bcStrings (6 4 n PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of A, {\it nra}: ")
--    (text . "\tab{32}  \menuitemstyle{} \tab{34} ")
    (text . "Tolerance, {\it tol}: ")
    (text . "\newline \tab{2} ")
--    (bcStrings (6 8 nra PI))
--    (text . "\tab{34} ")
    (bcStrings (8 "5.0e-4" tol F))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{}\tab{2}")
--    (text . "\newline Dimension of workspace array {\it lwork}: ")
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 32 lwork PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f04jgfSolve)
  htShowPage()

f04jgfSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  nra := m
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nra)
--    objValUnwrap htpLabelSpadValue(htPage, 'nra)
  lwork := 4*n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'lwork)
--    objValUnwrap htpLabelSpadValue(htPage, 'lwork)
  tol := htpLabelInputString(htPage,'tol)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (m = '6 and n = '4)  => f04jgfDefaultSolve(htPage,nra,lwork,tol,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..m] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  bmatList := 
    "append"/[h(k) for k in 1..m] where h(k) ==
       prefix := ('"\newline \tab{2} ")
       bnam := INTERN STRCONC ('"b",STRINGIMAGE k)
       [['text,:prefix],['bcStrings,[6, "0.0", bnam, 'F]]]
  start := ('"\blankline \menuitemstyle{} \tab{2} Enter values of {\it b}:")
  bmatList := [['text,:start],:bmatList]  
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :matList,:bmatList]
  page := htInitPage("F04JGF - Least-squares (if rank = {\it n}) or minimal least-squares (if rank < {\it n}) solution of {\it m} real equations in {\it n} unknowns, rank \inputbitmap{\htbmdir{}/less=.bitmap} {\it n}, {\it m} \inputbitmap{\htbmdir{}/great=.bitmap} {\it n}",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f04jgfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
--  htpSetProperty(page,'nra,nra)
--  htpSetProperty(page,'lwork,lwork)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


f04jgfDefaultSolve  (htPage,nra,lwork,tol,ifail) == 
  n := '4
  m := '6
  page := htInitPage('"F04JGF - Least-squares (if rank = n) or minimal least-squares (if rank < n) solution of m real equations in n unknowns, rank \inputbitmap{\htbmdir{}/less=.bitmap} n, m \inputbitmap{\htbmdir{}/great=.bitmap} n",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.05" a11 F))
    (bcStrings (6 "0.05" a12 F))
    (bcStrings (6 "0.25" a13 F))
    (bcStrings (6 "-0.25" a14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.25" a21 F))
    (bcStrings (6 "0.25" a22 F))
    (bcStrings (6 "0.05" a23 F))
    (bcStrings (6 "-0.05" a24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.35" a31 F))
    (bcStrings (6 "0.35" a32 F))
    (bcStrings (6 "1.75" a33 F))
    (bcStrings (6 "-1.75" a34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.75" a41 F))
    (bcStrings (6 "1.75" a42 F))
    (bcStrings (6 "0.35" a43 F))
    (bcStrings (6 "-0.35" a44 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.30" a51 F))
    (bcStrings (6 "-0.30" a52 F))
    (bcStrings (6 "0.30" a53 F))
    (bcStrings (6 "0.30" a54 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.40" a61 F))
    (bcStrings (6 "-0.40" a62 F))
    (bcStrings (6 "0.40" a63 F))
    (bcStrings (6 "0.40" a64 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it b}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 1 b1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 2 b2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 3 b3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 b4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 5 b5 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 6 b6 F)))
  htMakeDoneButton('"Continue",'f04jgfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
--  htpSetProperty(page,'nra,nra)
--  htpSetProperty(page,'lwork,lwork)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f04jgfGen htPage ==
  n := htpProperty(htPage,'n)
  m := htpProperty(htPage,'m)
--  nra := htpProperty(htPage,'nra)
--  lwork := htpProperty(htPage,'lwork)
  nra := m
  lwork := 4*n
  tol := htpProperty(htPage,'tol)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..m repeat
    b := STRCONC((first y).1," ")
    bList := [b,:bList]
    y := rest y
  bstring := bcwords2liststring bList
  y := REVERSE y
  for i in 1..m repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    matform := [:matform,rowList]
    rowList := []
  for i in 1..n repeat
    null := STRCONC('"0.0"," ")
    nullList := [:nullList,null]
  for i in m..(nra-1) repeat
    matform := [:matform,nullList]
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  prefix := STRCONC('"f04jgf(",STRINGIMAGE m,", ",STRINGIMAGE n,", ")
  prefix := STRCONC(prefix,STRINGIMAGE nra,", ",tol,", ",STRINGIMAGE lwork)
  prefix := STRCONC(prefix,", ",matstring,", [",bstring,"], ")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,")")
  bcGen prefix

f04mcf() ==
  htInitPage("F04MCF - Approximate solution of real symmetric positive-definite variable-bandwidth simultaneous linear equations (coefficient matrix already factorized)",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float)))
    (text . "\windowlink{Manual Page}{manpageXXf04mcf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f04mcf| '|NagLinearEquationSolvingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Computes the approximate solution of a system of real linear ")
    (text . "equations AX = B, where the n by n symmetric positive-definite ")
    (text . "variable-bandwidth matrix A has previously been factorized as ")
    (text . "\htbitmap{ldlt} by F01MCF, X is an n by r matrix of unknowns ")
    (text . "and B is an n by r matrix of right-hand sides. Related systems ")
    (text . "may also be solved. ")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the order of the matrix A, {\it n} ")
    (text ."\htbitmap{great=} 1:")
    (text . "\newline\tab{2} ")
    (bcStrings (9 6 n PI))
    (text . "\blankline")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline Enter the dimension of AL, {\it lal}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (9 14 lal PI))
    (text . "\blankline")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline Enter the number of right-hand sides, {\it ir}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (9 2 ir PI))
--    (text . "\blankline")
--    (text . "\newline ")
--    (text . "\menuitemstyle{}\tab{2}")
--    (text . "\newline Enter the first dimension of B, {\it nrb}: ")
--    (text . "\newline\tab{2} ")
--    (bcStrings (9 6 nrb PI))
--    (text . "\blankline")
--    (text . "\newline ")
--    (text . "\menuitemstyle{}\tab{2}")
--    (text . "\newline Enter the first dimension of X, {\it nrx}: ")
--    (text . "\newline\tab{2} ")
--    (bcStrings (9 6 nrx PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "Type of system to be solved, {\it iselct}:")
    (radioButtons iselct
        ("" "  {\em \htbitmap{ldlt}X = B} is solved" selone)
        ("" "  {\em LDX = B} is solved" seltwo)
        ("" "  {\em D\htbitmap{lt}X = B} is solved" selthree)
        ("" "  {\em L\htbitmap{lt}X = B} is solved" selfour)
        ("" "  {\em LX = B} is solved" selfive)
        ("" "  {\em \htbitmap{lt}X = B} is solved" selsix))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f04mcfSolve)
  htShowPage()

f04mcfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  lal :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'lal)
    objValUnwrap htpLabelSpadValue(htPage, 'lal)
  ir :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ir)
    objValUnwrap htpLabelSpadValue(htPage, 'ir)
  nrb := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nrb)
--    objValUnwrap htpLabelSpadValue(htPage, 'nrb)
  nrx := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nrx)
--    objValUnwrap htpLabelSpadValue(htPage, 'nrx)
  select := htpButtonValue(htPage,'iselct)
  iselct :=
    select = 'selone => '1
    select = 'seltwo => '2
    select = 'selthree => '3
    select = 'selfour => '4
    select = 'selfive => '5
    '6
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (n = '6 and lal = '14 and ir = '2) => f04mcfDefaultSolve(htPage,iselct,ifail)
  labelList :=
    "append"/[fal(i) for i in 1..lal] where fal(i) ==
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      [['bcStrings,[6, "0.0", xnam, 'F]]]
  dList :=
    "append"/[fd(i) for i in 1..n] where fd(i) ==
      dnam := INTERN STRCONC ('"d",STRINGIMAGE i) 
      [['bcStrings,[6, "0.0", dnam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{}\tab{2} Diagonal elements of diagon")
  prefix := STRCONC(prefix,"al matrix D as returned by F01MCF: \newline")
  dList := [['text,:prefix],:dList]
  nrowList :=
    "append"/[gj(j) for j in 1..n] where gj(j) ==
      nam := INTERN STRCONC ('"n",STRINGIMAGE j) 
      [['bcStrings,[6, 0, nam, 'PI]]]
  prefix := ('"\blankline \menuitemstyle{}\tab{2} {\it NROW(n)} the width ")
  prefix := STRCONC(prefix,"of the ith row of A: \newline ")
  nrowList := [['text,:prefix],:nrowList]
  bList :=
    "append"/[f(i,ir) for i in 1..nrb] where f(i,ir) ==
       labelList := 
         "append"/[g(i,j) for j in 1..ir] where g(i,j) ==
            bnam := INTERN STRCONC ('"b",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", bnam, 'F]]]
       prefix := ('"\newline ")
       labelList := [['text,:prefix],:labelList]
  prefix := ('"\blankline \menuitemstyle{}\tab{2} The n by r right-hand side ")
  prefix := STRCONC(prefix,"matrix B: \newline ")
  bList := [['text,:prefix],:bList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList,:dList,:nrowList,:bList]
  page := htInitPage("F04MCF - Approximate solution of real symmetric positive-definite variable-bandwidth simultaneous linear equations (coefficient matrix already factorized)",nil)
  htSay '"\menuitemstyle{}\tab{2} Elements of matrix {\it AL} in row by row "
  htSay '"order as returned by F01MCF: \newline "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f04mcfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'lal,lal)
  htpSetProperty(page,'ir,ir)
--  htpSetProperty(page,'nrb,nrb)
--  htpSetProperty(page,'nrx,nrx)
  htpSetProperty(page,'iselct,iselct)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


f04mcfDefaultSolve  (htPage,iselct,ifail) ==
  n := '6
  lal := '14
  ir := '2
  nrb := '6
  nrx := '6
  page :=   htInitPage("F04MCF - Approximate solution of real symmetric positive-definite variable-bandwidth simultaneous linear equations (coefficient matrix already factorized)",nil)
  htMakePage '(
    (domainConditions 
      (isDomain PI (Positive Integer))
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Elements of matrix {\it AL} in row by ")
    (text . "row order as returned by F01MCF: ") 
    (text . "\newline ")
    (bcStrings (6 "1.0" x1 F))
    (bcStrings (6 "2.0" x2 F))
    (bcStrings (6 "1.0" x3 F))
    (bcStrings (6 "3.0" x4 F))
    (bcStrings (6 "1.0" x5 F))
    (bcStrings (6 "1.0" x6 F))
    (bcStrings (6 "5.0" x7 F))
    (bcStrings (6 "4.0" x8 F))
    (bcStrings (6 "1.5" x9 F))
    (bcStrings (6 "0.5" x10 F))
    (bcStrings (6 "1.0" x11 F))
    (bcStrings (6 "1.5" x12 F))
    (bcStrings (6 "5.0" x13 F))
    (bcStrings (6 "1.0" x14 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} Diagonal elements of diagonal matrix ")
    (text . "D as returned by F01MCF: ") 
    (text . "\newline ")
    (bcStrings (6 "1.0" d1 F))
    (bcStrings (6 "1.0" d2 F))
    (bcStrings (6 "4.0" d3 F))
    (bcStrings (6 "16.0" d4 F))
    (bcStrings (6 "1.0" d5 F))
    (bcStrings (6 "16.0" d6 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} {\it NROW(n)} the width of the ith row ")
    (text . "of A: ") 
    (text . "\newline ")
    (bcStrings (6 1 n1 PI))
    (bcStrings (6 2 n2 PI))
    (bcStrings (6 2 n3 PI))
    (bcStrings (6 1 n4 PI))
    (bcStrings (6 5 n5 PI))
    (bcStrings (6 3 n6 PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} The n by r right-hand side matrix B:")
    (text . "\newline ")
    (bcStrings (6 "6" b11 F))
    (text . "\tab{10} ")
    (bcStrings (6 "-10" b12 PI))
    (text . "\newline ")
    (bcStrings (6 "15" b21 F))
    (text . "\tab{10} ")
    (bcStrings (6 "-21" b22 PI))
    (text . "\newline ")
    (bcStrings (6 "11" b31 F))
    (text . "\tab{10} ")
    (bcStrings (6 "-3" b32 PI))
    (text . "\newline ")
    (bcStrings (6 "0" b41 F))
    (text . "\tab{10} ")
    (bcStrings (6 "24" b42 PI))
    (text . "\newline ")
    (bcStrings (6 "51" b51 F))
    (text . "\tab{10} ")
    (bcStrings (6 "-39" b52 PI))
    (text . "\newline ")
    (bcStrings (6 "46" b61 F))
    (text . "\tab{10} ")
    (bcStrings (6 "67" b62 PI))
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'f04mcfGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'lal,lal)
  htpSetProperty(page,'ir,ir)
--  htpSetProperty(page,'nrb,nrb)
--  htpSetProperty(page,'nrx,nrx)
  htpSetProperty(page,'iselct,iselct)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f04mcfGen htPage ==
  n := htpProperty(htPage,'n)
  lal := htpProperty(htPage,'lal)
  ir := htpProperty(htPage,'ir)
--  nrb := htpProperty(htPage,'nrb)
--  nrx := htpProperty(htPage,'nrx)
  nrb := n
  nrx := n
  iselct := htpProperty(htPage,'iselct)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..nrb repeat
    for j in 1..ir repeat
      elm := STRCONC((first y).1," ")
      rowList := [elm,:rowList]
      y := rest y
    matform := [rowList,:matform]
    rowList := []
  matfrom := REVERSE matform
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  for i in 1..n repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    nrowList := [right,:nrowList]
  nrowstring := bcwords2liststring nrowList
  for i in 1..n repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    dList := [right,:dList]
  dstring := bcwords2liststring dList
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    alList := [right,:alList]
  alstring := bcwords2liststring alList
  prefix := STRCONC('"f04mcf(",STRINGIMAGE n,", [",alstring,"], ")
  prefix := STRCONC(prefix,STRINGIMAGE lal,", [",dstring,"],[",nrowstring)
  prefix := STRCONC(prefix,"]::Matrix Integer,")
  prefix := STRCONC(prefix,STRINGIMAGE ir,", ",matstring,", ",STRINGIMAGE nrb)
  prefix := STRCONC(prefix,", ",STRINGIMAGE iselct,", ",STRINGIMAGE nrx,", ")
  bcGen STRCONC(prefix,STRINGIMAGE ifail,")")


f04axf() ==
  htInitPage('"F04AXF - Approximate solution of a a set of real sparse linear equations after factorization by F01BRF or by F01BSF",nil)
  htMakePage '(
    (domainConditions
       (isDomain EM $EmptyMode)
       (isDomain F (Float)))
    (text . "\windowlink{Manual Page}{manpageXXf04axf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f04axf| '|NagLinearEquationSolvingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "F04AXF calculates the approximate solution of a set of real ")
    (text . "sparse linear equations {\it Ax=b} or ")
    (text . "\htbitmap{aTx=b}, where the {\it n} by {\it n} matrix ")
    (text . "{\it A} has been factorized by F01BRF or F01BSF, {\it x} ")
    (text . "is an {\it n} element vector of unknowns and {\it b} is an ")
    (text . "{\it n} element right-hand side vector. ")
    (text . "\blankline")
    (text . "\newline ")
    (text . "Read the input file to see the example program. ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "\spadcommand{)read f04axf \bound{s0}} "))
  htShowPage()

f04maf() ==
  htInitPage('"F04MAF - Solution of a real sparse symmetric positive-definite system of linear equations after factorization by F01MAF",nil)
  htMakePage '(
    (domainConditions
       (isDomain EM $EmptyMode)
       (isDomain F (Float)))
    (text . "\windowlink{Manual Page}{manpageXXf04maf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f04maf| '|NagLinearEquationSolvingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "F04MAF solves a real sparse symmetric positive-definite system ")
    (text . "of linear equations {\it Ax=b} using a pre-conditioned ")
    (text . "conjugate gradient method, where the {\it n} by {\it n} ")
    (text . "matrix {\it A} has been factorized by F01MAF, {\it x} is an ")
    (text . "{\it n} element vector of unknowns and {\it b} is an {\it n} ")
    (text . "element right-hand side vector. ")
    (text . "\blankline")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "\spadcommand{)read f04maf \bound{s0}} "))
  htShowPage()

f04mbf() ==
  htInitPage('"F04MBF - Real sparse symmetric simultaneous linear equations",nil)
  htMakePage '(
    (domainConditions
       (isDomain EM $EmptyMode)
       (isDomain PI (PositiveInteger))
       (isDomain F (Float)))
    (text . "\windowlink{Manual Page}{manpageXXf04mbf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f04mbf| '|NagLinearEquationSolvingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "F04MBF solve a system of real symmetric linear equations ")
    (text . "({\it A} - \lambda {\it I}){\it x} = {\it b} using a Lanczos ")
    (text . "algorithm, where {\it A} is an {\it n} by {\it n} sparse ")
    (text . "symmetric matrix, {\it x} is an {\it n} vector of unknowns ")
    (text . "and {\it b} is an {\it n} element right-hand side vector. ")
    (text . "\blankline")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the order {\it n} of matrix {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (10 10 n PI))
    (text . "\blankline")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")    
    (text . "Is preconditioning required? ")
    (radioButtons precon
        (""  " Yes" true)
        (""  " No" false))
    (text . "\blankline")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the shift in the equations \lambda, {\it shift} : ")
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.0" shift F))
    (text . "\blankline")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the tolerance for convergence, {\it rtol}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.00001" rtol F))
    (text . "\blankline")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter an upper limit for the number of iterations, {\it itnlim}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (10 100 itnlim PI))
    (text . "\blankline")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the printing level, {\it msglvl}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (10 1 msglvl PI))
    (text . "\blankline")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f04mbfSolve)
  htShowPage()

f04mbfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  msolve := htpButtonValue(htPage,'precon)
  precon :=
    msolve = 'true => 'true
    'false
  shift := htpLabelInputString(htPage,'shift)
  rtol := htpLabelInputString(htPage,'rtol)
  itnlim :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'itnlim)
    objValUnwrap htpLabelSpadValue(htPage, 'itnlim)
  msglvl :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'msglvl)
    objValUnwrap htpLabelSpadValue(htPage, 'msglvl)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (n = '10 and precon ='true) => f04mbfDefaultSolve(htPage,shift,rtol,itnlim,msglvl,ifail)
  bmatList :=
    "append"/[f(i) for i in 1..n] where f(i) ==
       bnam := INTERN STRCONC ('"b",STRINGIMAGE i)
       [['bcStrings,[6, "0.0", bnam, 'F]]]
  amatList :=
    "append"/[h(ia,n) for ia in 1..n] where h(ia,n) ==
       alabelList :=
         "append"/[k(ia,ja) for ja in 1..n] where k(ia,ja) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE ia,STRINGIMAGE ja)
            [['bcStrings,[6, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       alabelList := [['text,:prefix],:alabelList]
  start := ('"\blankline \menuitemstyle{}\tab{2} Enter the matrix {\it A}: ")
  amatList := [['text,:start],:amatList]
  mmatList:=
    precon = 'true =>
     alabelList:=
      "append"/[l(im,n) for im in 1..n] where l(im,n) ==
         mlabelList :=
           "append"/[o(im,jm) for jm in 1..n] where o(im,jm) ==
              mnam := INTERN STRCONC ('"m",STRINGIMAGE im,STRINGIMAGE jm)
              [['bcStrings,[6, "0.0", mnam, 'F]]]
         prefix := ('"\newline \tab{2} ")
         mlabelList := [['text,:prefix],:mlabelList]
     start := ('"\blankline \menuitemstyle{}\tab{2} Enter the matrix {\it m}: ")
     [['text,:start],:alabelList]
    []
  equationPart := [
     '(domainConditions
        (isDomain P (Polynomial $EmptyMode))
          (isDomain F (Float))),
            :bmatList,:amatList,:mmatList]
  page := htInitPage('"F04MBF - Real sparse symmetric simultaneous linear equations",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} "
  htSay '"Enter the right-hand side vector {\it b(n)}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f04mbfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'precon,precon)
  htpSetProperty(page,'shift,shift)
  htpSetProperty(page,'rtol,rtol)
  htpSetProperty(page,'itnlim,itnlim)
  htpSetProperty(page,'msglvl,msglvl)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f04mbfDefaultSolve (htPage,shift,rtol,itnlim,msglvl,ifail) ==
  n := '10
  precon := 'true
  page := htInitPage('"F04MBF - Real sparse symmetric simultaneous linear equations",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the right-hand side vector {\it b(n)}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "6.0" b1 F))
    (bcStrings (6 "4.0" b2 F))
    (bcStrings (6 "4.0" b3 F))
    (bcStrings (6 "4.0" b4 F))
    (bcStrings (6 "4.0" b5 F))
    (bcStrings (6 "4.0" b6 F))
    (bcStrings (6 "4.0" b7 F))
    (bcStrings (6 "4.0" b8 F))
    (bcStrings (6 "4.0" b9 F))
    (bcStrings (6 "6.0" b10 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the matrix {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "2.0" a11 F))
    (bcStrings (6 "1.0" a12 F))
    (bcStrings (6 "0.0" a13 F))
    (bcStrings (6 "0.0" a14 F))
    (bcStrings (6 "0.0" a15 F))
    (bcStrings (6 "0.0" a16 F))
    (bcStrings (6 "0.0" a17 F))
    (bcStrings (6 "0.0" a18 F))
    (bcStrings (6 "0.0" a19 F))
    (bcStrings (6 "3.0" a110 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.0" a21 F))
    (bcStrings (6 "2.0" a22 F))
    (bcStrings (6 "1.0" a23 F))
    (bcStrings (6 "0.0" a24 F))
    (bcStrings (6 "0.0" a25 F))
    (bcStrings (6 "0.0" a26 F))
    (bcStrings (6 "0.0" a27 F))
    (bcStrings (6 "0.0" a28 F))
    (bcStrings (6 "0.0" a29 F))
    (bcStrings (6 "0.0" a210 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a31 F))
    (bcStrings (6 "1.0" a32 F))
    (bcStrings (6 "2.0" a33 F))
    (bcStrings (6 "1.0" a34 F))
    (bcStrings (6 "0.0" a35 F))
    (bcStrings (6 "0.0" a36 F))
    (bcStrings (6 "0.0" a37 F))
    (bcStrings (6 "0.0" a38 F))
    (bcStrings (6 "0.0" a39 F))
    (bcStrings (6 "0.0" a310 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a41 F))
    (bcStrings (6 "0.0" a42 F))
    (bcStrings (6 "1.0" a43 F))
    (bcStrings (6 "2.0" a44 F))
    (bcStrings (6 "1.0" a45 F))
    (bcStrings (6 "0.0" a46 F))
    (bcStrings (6 "0.0" a47 F))
    (bcStrings (6 "0.0" a48 F))
    (bcStrings (6 "0.0" a49 F))
    (bcStrings (6 "0.0" a410 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a51 F))
    (bcStrings (6 "0.0" a52 F))
    (bcStrings (6 "0.0" a53 F))
    (bcStrings (6 "1.0" a54 F))
    (bcStrings (6 "2.0" a55 F))
    (bcStrings (6 "1.0" a56 F))
    (bcStrings (6 "0.0" a57 F))
    (bcStrings (6 "0.0" a58 F))
    (bcStrings (6 "0.0" a59 F))
    (bcStrings (6 "0.0" a510 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a61 F))
    (bcStrings (6 "0.0" a62 F))
    (bcStrings (6 "0.0" a63 F))
    (bcStrings (6 "0.0" a64 F))
    (bcStrings (6 "1.0" a65 F))
    (bcStrings (6 "2.0" a66 F))
    (bcStrings (6 "1.0" a67 F))
    (bcStrings (6 "0.0" a68 F))
    (bcStrings (6 "0.0" a69 F))
    (bcStrings (6 "0.0" a610 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a71 F))
    (bcStrings (6 "0.0" a72 F))
    (bcStrings (6 "0.0" a73 F))
    (bcStrings (6 "0.0" a74 F))
    (bcStrings (6 "0.0" a75 F))
    (bcStrings (6 "1.0" a76 F))
    (bcStrings (6 "2.0" a77 F))
    (bcStrings (6 "1.0" a78 F))
    (bcStrings (6 "0.0" a79 F))
    (bcStrings (6 "0.0" a710 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a81 F))
    (bcStrings (6 "0.0" a82 F))
    (bcStrings (6 "0.0" a83 F))
    (bcStrings (6 "0.0" a84 F))
    (bcStrings (6 "0.0" a85 F))
    (bcStrings (6 "0.0" a86 F))
    (bcStrings (6 "1.0" a87 F))
    (bcStrings (6 "2.0" a88 F))
    (bcStrings (6 "1.0" a89 F))
    (bcStrings (6 "0.0" a810 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a91 F))
    (bcStrings (6 "0.0" a92 F))
    (bcStrings (6 "0.0" a93 F))
    (bcStrings (6 "0.0" a94 F))
    (bcStrings (6 "0.0" a95 F))
    (bcStrings (6 "0.0" a96 F))
    (bcStrings (6 "0.0" a97 F))
    (bcStrings (6 "1.0" a98 F))
    (bcStrings (6 "2.0" a99 F))
    (bcStrings (6 "1.0" a910 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "3.0" a101 F))
    (bcStrings (6 "0.0" a102 F))
    (bcStrings (6 "0.0" a103 F))
    (bcStrings (6 "0.0" a104 F))
    (bcStrings (6 "0.0" a105 F))
    (bcStrings (6 "0.0" a106 F))
    (bcStrings (6 "0.0" a107 F))
    (bcStrings (6 "0.0" a108 F))
    (bcStrings (6 "1.0" a109 F))
    (bcStrings (6 "2.0" a1010 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the matrix {\it m}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "2.0" m11 F))
    (bcStrings (6 "1.0" m12 F))
    (bcStrings (6 "0.0" m13 F))
    (bcStrings (6 "0.0" m14 F))
    (bcStrings (6 "0.0" m15 F))
    (bcStrings (6 "0.0" m16 F))
    (bcStrings (6 "0.0" m17 F))
    (bcStrings (6 "0.0" m18 F))
    (bcStrings (6 "0.0" m19 F))
    (bcStrings (6 "0.0" m110 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.0" m21 F))
    (bcStrings (6 "2.0" m22 F))
    (bcStrings (6 "1.0" m23 F))
    (bcStrings (6 "0.0" m24 F))
    (bcStrings (6 "0.0" m25 F))
    (bcStrings (6 "0.0" m26 F))
    (bcStrings (6 "0.0" m27 F))
    (bcStrings (6 "0.0" m28 F))
    (bcStrings (6 "0.0" m29 F))
    (bcStrings (6 "0.0" m210 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" m31 F))
    (bcStrings (6 "1.0" m32 F))
    (bcStrings (6 "2.0" m33 F))
    (bcStrings (6 "1.0" m34 F))
    (bcStrings (6 "0.0" m35 F))
    (bcStrings (6 "0.0" m36 F))
    (bcStrings (6 "0.0" m37 F))
    (bcStrings (6 "0.0" m38 F))
    (bcStrings (6 "0.0" m39 F))
    (bcStrings (6 "0.0" m310 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" m41 F))
    (bcStrings (6 "0.0" m42 F))
    (bcStrings (6 "1.0" m43 F))
    (bcStrings (6 "2.0" m44 F))
    (bcStrings (6 "1.0" m45 F))
    (bcStrings (6 "0.0" m46 F))
    (bcStrings (6 "0.0" m47 F))
    (bcStrings (6 "0.0" m48 F))
    (bcStrings (6 "0.0" m49 F))
    (bcStrings (6 "0.0" m410 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" m51 F))
    (bcStrings (6 "0.0" m52 F))
    (bcStrings (6 "0.0" m53 F))
    (bcStrings (6 "1.0" m54 F))
    (bcStrings (6 "2.0" m55 F))
    (bcStrings (6 "1.0" m56 F))
    (bcStrings (6 "0.0" m57 F))
    (bcStrings (6 "0.0" m58 F))
    (bcStrings (6 "0.0" m59 F))
    (bcStrings (6 "0.0" m510 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" m61 F))
    (bcStrings (6 "0.0" m62 F))
    (bcStrings (6 "0.0" m63 F))
    (bcStrings (6 "0.0" m64 F))
    (bcStrings (6 "1.0" m65 F))
    (bcStrings (6 "2.0" m66 F))
    (bcStrings (6 "1.0" m67 F))
    (bcStrings (6 "0.0" m68 F))
    (bcStrings (6 "0.0" m69 F))
    (bcStrings (6 "0.0" m610 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" m71 F))
    (bcStrings (6 "0.0" m72 F))
    (bcStrings (6 "0.0" m73 F))
    (bcStrings (6 "0.0" m74 F))
    (bcStrings (6 "0.0" m75 F))
    (bcStrings (6 "1.0" m76 F))
    (bcStrings (6 "2.0" m77 F))
    (bcStrings (6 "1.0" m78 F))
    (bcStrings (6 "0.0" m79 F))
    (bcStrings (6 "0.0" m710 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" m81 F))
    (bcStrings (6 "0.0" m82 F))
    (bcStrings (6 "0.0" m83 F))
    (bcStrings (6 "0.0" m84 F))
    (bcStrings (6 "0.0" m85 F))
    (bcStrings (6 "0.0" m86 F))
    (bcStrings (6 "1.0" m87 F))
    (bcStrings (6 "2.0" m88 F))
    (bcStrings (6 "1.0" m89 F))
    (bcStrings (6 "0.0" m810 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" m91 F))
    (bcStrings (6 "0.0" m92 F))
    (bcStrings (6 "0.0" m93 F))
    (bcStrings (6 "0.0" m94 F))
    (bcStrings (6 "0.0" m95 F))
    (bcStrings (6 "0.0" m96 F))
    (bcStrings (6 "0.0" m97 F))
    (bcStrings (6 "1.0" m98 F))
    (bcStrings (6 "2.0" m99 F))
    (bcStrings (6 "1.0" m910 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" m101 F))
    (bcStrings (6 "0.0" m102 F))
    (bcStrings (6 "0.0" m103 F))
    (bcStrings (6 "0.0" m104 F))
    (bcStrings (6 "0.0" m105 F))
    (bcStrings (6 "0.0" m106 F))
    (bcStrings (6 "0.0" m107 F))
    (bcStrings (6 "0.0" m108 F))
    (bcStrings (6 "1.0" m109 F))
    (bcStrings (6 "2.0" m1010 F))
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'f04mbfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'precon,precon)
  htpSetProperty(page,'shift,shift)
  htpSetProperty(page,'rtol,rtol)
  htpSetProperty(page,'itnlim,itnlim)
  htpSetProperty(page,'msglvl,msglvl)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f04mbfGen htPage ==
  n := htpProperty(htPage,'n)
  precon := htpProperty(htPage,'precon)
  shift := htpProperty(htPage,'shift)
  rtol := htpProperty(htPage,'rtol)
  itnlim := htpProperty(htPage,'itnlim)
  msglvl := htpProperty(htPage,'msglvl)
  ifail := htpProperty(htPage,'ifail)
  lrwork := '1
  liwork := '1
  alist := htpInputAreaAlist htPage
  y := alist
  if (precon = 'true) then
    for i in 1..n repeat
      for j in 1..n repeat
        melm := STRCONC((first y).1," ")
        mrowlist := [melm,:mrowlist]
        y := rest y
      matm := [mrowlist,:matm]
      mrowlist := []
    mstring := bcwords2liststring [bcwords2liststring x for x in matm]
  for k in 1..n repeat
    for l in 1..n repeat
      aelm := STRCONC((first y).1," ")
      arowlist := [aelm,:arowlist]
      y := rest y
    mata := [arowlist,:mata]
    arowlist := []
  astring := bcwords2liststring [bcwords2liststring y for y in mata]
  for z in 1..n repeat
    belm := STRCONC((first y).1," ")
    blist := [belm,:blist]
    y := rest y
  bstring := bcwords2liststring blist
  if (precon = 'false) then
    mstring := astring
  prefix := STRCONC('"f04mbf(",STRINGIMAGE n,",[",bstring,"]::Matrix DoubleFloat,",precon,",")
  prefix := STRCONC(prefix,STRINGIMAGE shift,",",STRINGIMAGE itnlim,",",STRINGIMAGE msglvl,",")
  prefix := STRCONC(prefix,STRINGIMAGE lrwork,",",STRINGIMAGE liwork,",")
  prefix := STRCONC(prefix,STRINGIMAGE rtol,",",STRINGIMAGE ifail,",((")
  prefix := STRCONC(prefix,astring,"::Matrix MachineFloat)::ASP28(APROD)),((")
  prefix := STRCONC(prefix,mstring,"::Matrix MachineFloat)::ASP34(MSOLVE)))")
  linkGen prefix


-- f04qaf() ==
--   htInitPage('"F04QAF - Solution of sparse unsymmetric equations, linear and damped least-squares problems using a Lanczos algorithm",nil)
--   htMakePage '(
--     (domainConditions
--        (isDomain EM $EmptyMode)
--        (isDomain F (Float)))
--     (text . "\windowlink{Manual Page}{manpageXXf04qaf} for this routine ")
--     (text . "\newline ")
--     (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f04qaf| '|NagLinearEquationSolvingPackage|)} for this routine")
--     (text . "\newline \horizontalline ")
--     (text . "\newline ")
--     (text . "F04QAF solves sparse unsymmetric equations, sparse linear ")
--     (text . "least-squares problems and sparse damped least-squares ")
--     (text . "problems, using a Lanczos algorithm. Specifically, the ")
--     (text . "routine can be used to solve a system of linear equations ")
--     (text . "{\it Ax=b}, where {\it A} is an {\it n} by {\it n} real ")
--     (text . "sparse unsymmetric matrix, or can be used to solve linear ")
--     (text . "least-squares problems, so that it minimizes the the value ")
--     (text . "{\htbitmap{newrho}} given by {\htbitmap{rho=r}}, ")
--     (text . "{\it r=b-AX} where {\it A} is an {\it m} by {\it n} real ")
--     (text . "sparse matrix. A damping parameter \lambda may ")
--     (text . "be included in the least squares problem in which case the ")
--     (text . "routine minimizes the value {\htbitmap{newrho}} given by ")
--     (text . "{\htbitmap{rhosq=}}. \newline ")
--     (text . "\blankline ")
--     (text . "\menuitemstyle{}\tab{2}")
--     (text . "\spadcommand{)read f04qaf \bound{s0}} "))
--   htShowPage()

-- f04mbf() ==
--   htInitPage('"F04MBF - Real sparse symmetric simultaneous linear equations",nil)
--   htMakePage '(
--     (domainConditions
--        (isDomain EM $EmptyMode)
--        (isDomain F (Float)))
--     (text . "\windowlink{Manual Page}{manpageXXf04mbf} for this routine ")
--     (text . "\newline ")
--     (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f04mbf| '|NagLinearEquationSolvingPackage|)} for this routine")
--     (text . "\newline \horizontalline ")
--     (text . "\newline ")
--     (text . "\newline ")
--     (text . "F04MBF solve a system of real symmetric linear equations ")
--     (text . "({\it A} - \lambda {\it I}){\it x} = {\it b} using a Lanczos ")
--     (text . "algorithm, where {\it A} is an {\it n} by {\it n} sparse ")
--     (text . "symmetric matrix, {\it x} is an {\it n} vector of unknowns ")
--     (text . "and {\it b} is an {\it n} element right-hand side vector. ")
--     (text . "\blankline")
--     (text . "\newline ")
--     (text . "\menuitemstyle{}\tab{2} ")
--     (text . "\spadcommand{)read f04mbf \bound{s0}} "))
--   htShowPage()

f04qaf() ==
  htInitPage('"F04QAF - Solution of sparse unsymmetric equations, linear and damped least-squares problems using a Lanczos algorithm",nil)
  htMakePage '(
    (domainConditions
       (isDomain EM $EmptyMode)
       (isDomain PI (PositiveInteger))
       (isDomain F (Float)))
    (text . "\windowlink{Manual Page}{manpageXXf04qaf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f04qaf| '|NagLinearEquationSolvingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "F04QAF solves sparse unsymmetric equations, sparse linear ")
    (text . "least-squares problems and sparse damped least-squares ")
    (text . "problems, using a Lanczos algorithm. Specifically, the ")
    (text . "routine can be used to solve a system of linear equations ")
    (text . "{\it Ax=b}, where {\it A} is an {\it n} by {\it n} real ")
    (text . "sparse unsymmetric matrix, or can be used to solve linear ")
    (text . "least-squares problems, so that it minimizes the the value ")
    (text . "{\htbitmap{newrho}} given by {\htbitmap{rho=r}}, ")
    (text . "{\it r=b-AX} where {\it A} is an {\it m} by {\it n} real ")
    (text . "sparse matrix. A damping parameter \lambda may ")
    (text . "be included in the least squares problem in which case the ")
    (text . "routine minimizes the value {\htbitmap{newrho}} given by ")
    (text . "{\htbitmap{rhosq=}}. \newline ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the number of rows of the matrix {\it A}, {\it m}:")
    (text . "\newline \tab{2}")
    (bcStrings (10 13 m PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the number of columns of the matrix {\it A}, {\it n}:")
    (text . "\newline \tab{2}")
    (bcStrings (10 12 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the damping parameter \lambda, {\it damp}:")
    (text . "\newline \tab{2}")
    (bcStrings (10 "0.0" damp F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the tolerance for elements of {\it A}, {\it atol}:")
    (text . "\newline \tab{2}")
    (bcStrings (10 "0.00001" atol F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the tolerance for elements of {\it b}, {\it btol}:")
    (text . "\newline \tab{2}")
    (bcStrings (10 "0.0001" btol F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the maximum number of iterations {\it itnlim}:")
    (text . "\newline \tab{2}")
    (bcStrings (10 100 itnlim PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the printing level {\it msglvl}:")
    (text . "\newline \tab{2}")
    (bcStrings (10 1 msglvl PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f04qafSolve)   
  htShowPage()

f04qafSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  damp := htpLabelInputString(htPage,'damp)
  atol := htpLabelInputString(htPage,'atol)
  btol := htpLabelInputString(htPage,'btol)
  itnlim :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'itnlim)
    objValUnwrap htpLabelSpadValue(htPage, 'itnlim)
  msglvl :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'msglvl)
    objValUnwrap htpLabelSpadValue(htPage, 'msglvl)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (m = '13 and n = '12) => f04qafDefaultSolve(htPage,damp,atol,btol,itnlim,msglvl,ifail)
  bmatList :=
    "append"/[f(i) for i in 1..m] where f(i) ==
       bnam := INTERN STRCONC ('"b",STRINGIMAGE i)
       [['bcStrings,[6, "0.0", bnam, 'F]]]
  amatList :=
    "append"/[h(ia,n) for ia in 1..m] where h(ia,n) ==
       alabelList :=
         "append"/[k(ia,ja) for ja in 1..n] where k(ia,ja) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE ia,STRINGIMAGE ja)
            [['bcStrings,[6, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       alabelList := [['text,:prefix],:alabelList]
  start := ('"\blankline \menuitemstyle{}\tab{2} Enter the matrix {\it A}: ")
  amatList := [['text,:start],:amatList]
  equationPart := [
     '(domainConditions
        (isDomain P (Polynomial $EmptyMode))
          (isDomain F (Float))),
            :bmatList,:amatList]
  page := htInitPage('"F04QAF - Solution of sparse unsymmetric equations, linear and damped least-squares problems using a Lanczos algorithm",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} "
  htSay '"Enter the right-hand side vector {\it b(m)}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f04qafGen)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'damp,damp)
  htpSetProperty(page,'atol,atol)
  htpSetProperty(page,'btol,btol)
  htpSetProperty(page,'itnlim,itnlim)
  htpSetProperty(page,'msglvl,msglvl)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()



f04qafDefaultSolve (htPage,damp,atol,btol,itnlim,msglvl,ifail) ==
  m := '13
  n := '12
  page := htInitPage('"F04QAF - Solution of sparse unsymmetric equations, linear and damped least-squares problems using a Lanczos algorithm",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the right-hand side vector {\it b(n)}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" b1 F))
    (bcStrings (6 "0.0" b2 F))
    (bcStrings (6 "0.0" b3 F))
    (bcStrings (6 "-0.01" b4 F))
    (bcStrings (6 "-0.01" b5 F))
    (bcStrings (6 "0.0" b6 F))
    (bcStrings (6 "0.0" b7 F))
    (bcStrings (6 "-0.01" b8 F))
    (bcStrings (6 "-0.01" b9 F))
    (bcStrings (6 "0.0" b10 F))
    (bcStrings (6 "0.0" b11 F))
    (bcStrings (6 "0.0" b12 F))
    (bcStrings (6 "10.0" b13 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the matrix {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.0" a0101 F))
    (bcStrings (6 "0.0" a0102 F))
    (bcStrings (6 "0.0" a0103 F))
    (bcStrings (6 "-1.0" a0104 F))
    (bcStrings (6 "0.0" a0105 F))
    (bcStrings (6 "0.0" a0106 F))
    (bcStrings (6 "0.0" a0107 F))
    (bcStrings (6 "0.0" a0108 F))
    (bcStrings (6 "0.0" a0109 F))
    (bcStrings (6 "0.0" a0110 F))
    (bcStrings (6 "0.0" a0111 F))
    (bcStrings (6 "0.0" a0112 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a0201 F))
    (bcStrings (6 "1.0" a0202 F))
    (bcStrings (6 "0.0" a0203 F))
    (bcStrings (6 "0.0" a0204 F))
    (bcStrings (6 "-1.0" a0205 F))
    (bcStrings (6 "0.0" a0206 F))
    (bcStrings (6 "0.0" a0207 F))
    (bcStrings (6 "0.0" a0208 F))
    (bcStrings (6 "0.0" a0209 F))
    (bcStrings (6 "0.0" a0210 F))
    (bcStrings (6 "0.0" a0211 F))
    (bcStrings (6 "0.0" a0212 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a0301 F))
    (bcStrings (6 "0.0" a0302 F))
    (bcStrings (6 "1.0" a0303 F))
    (bcStrings (6 "-1.0" a0304 F))
    (bcStrings (6 "0.0" a0305 F))
    (bcStrings (6 "0.0" a0306 F))
    (bcStrings (6 "0.0" a0307 F))
    (bcStrings (6 "0.0" a0308 F))
    (bcStrings (6 "0.0" a0309 F))
    (bcStrings (6 "0.0" a0310 F))
    (bcStrings (6 "0.0" a0311 F))
    (bcStrings (6 "0.0" a0312 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-1.0" a0401 F))
    (bcStrings (6 "0.0" a0402 F))
    (bcStrings (6 "-1.0" a0403 F))
    (bcStrings (6 "4.0" a0404 F))
    (bcStrings (6 "-1.0" a0405 F))
    (bcStrings (6 "0.0" a0406 F))
    (bcStrings (6 "0.0" a0407 F))
    (bcStrings (6 "-1.0" a0408 F))
    (bcStrings (6 "0.0" a0409 F))
    (bcStrings (6 "0.0" a0410 F))
    (bcStrings (6 "0.0" a0411 F))
    (bcStrings (6 "0.0" a0412 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a0501 F))
    (bcStrings (6 "-1.0" a0502 F))
    (bcStrings (6 "0.0" a0503 F))
    (bcStrings (6 "-1.0" a0504 F))
    (bcStrings (6 "4.0" a0505 F))
    (bcStrings (6 "-1.0" a0506 F))
    (bcStrings (6 "0.0" a0507 F))
    (bcStrings (6 "0.0" a0508 F))
    (bcStrings (6 "-1.0" a0509 F))
    (bcStrings (6 "0.0" a0510 F))
    (bcStrings (6 "0.0" a0511 F))
    (bcStrings (6 "0.0" a0512 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a0601 F))
    (bcStrings (6 "0.0" a0602 F))
    (bcStrings (6 "0.0" a0603 F))
    (bcStrings (6 "0.0" a0604 F))
    (bcStrings (6 "-1.0" a0605 F))
    (bcStrings (6 "1.0" a0606 F))
    (bcStrings (6 "0.0" a0607 F))
    (bcStrings (6 "0.0" a0608 F))
    (bcStrings (6 "0.0" a0609 F))
    (bcStrings (6 "0.0" a0610 F))
    (bcStrings (6 "0.0" a0611 F))
    (bcStrings (6 "0.0" a0612 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a0701 F))
    (bcStrings (6 "0.0" a0702 F))
    (bcStrings (6 "0.0" a0703 F))
    (bcStrings (6 "0.0" a0704 F))
    (bcStrings (6 "0.0" a0705 F))
    (bcStrings (6 "0.0" a0706 F))
    (bcStrings (6 "1.0" a0707 F))
    (bcStrings (6 "-1.0" a0708 F))
    (bcStrings (6 "0.0" a0709 F))
    (bcStrings (6 "0.0" a0710 F))
    (bcStrings (6 "0.0" a0711 F))
    (bcStrings (6 "0.0" a0712 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a0801 F))
    (bcStrings (6 "0.0" a0802 F))
    (bcStrings (6 "0.0" a0803 F))
    (bcStrings (6 "-1.0" a0804 F))
    (bcStrings (6 "0.0" a0805 F))
    (bcStrings (6 "0.0" a0806 F))
    (bcStrings (6 "-1.0" a0807 F))
    (bcStrings (6 "4.0" a0808 F))
    (bcStrings (6 "-1.0" a0809 F))
    (bcStrings (6 "0.0" a0810 F))
    (bcStrings (6 "-1.0" a0811 F))
    (bcStrings (6 "0.0" a0812 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a0901 F))
    (bcStrings (6 "0.0" a0902 F))
    (bcStrings (6 "0.0" a0903 F))
    (bcStrings (6 "0.0" a0904 F))
    (bcStrings (6 "-1.0" a0905 F))
    (bcStrings (6 "0.0" a0906 F))
    (bcStrings (6 "0.0" a0907 F))
    (bcStrings (6 "-1.0" a0908 F))
    (bcStrings (6 "4.0" a0909 F))
    (bcStrings (6 "-1.0" a0910 F))
    (bcStrings (6 "0.0" a0911 F))
    (bcStrings (6 "-1.0" a0912 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a1001 F))
    (bcStrings (6 "0.0" a1002 F))
    (bcStrings (6 "0.0" a1003 F))
    (bcStrings (6 "0.0" a1004 F))
    (bcStrings (6 "0.0" a1005 F))
    (bcStrings (6 "0.0" a1006 F))
    (bcStrings (6 "0.0" a1007 F))
    (bcStrings (6 "0.0" a1008 F))
    (bcStrings (6 "-1.0" a1009 F))
    (bcStrings (6 "1.0" a1010 F))
    (bcStrings (6 "0.0" a1011 F))
    (bcStrings (6 "0.0" a1012 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a1101 F))
    (bcStrings (6 "0.0" a1102 F))
    (bcStrings (6 "0.0" a1103 F))
    (bcStrings (6 "0.0" a1104 F))
    (bcStrings (6 "0.0" a1105 F))
    (bcStrings (6 "0.0" a1106 F))
    (bcStrings (6 "0.0" a1107 F))
    (bcStrings (6 "-1.0" a1108 F))
    (bcStrings (6 "0.0" a1109 F))
    (bcStrings (6 "0.0" a1110 F))
    (bcStrings (6 "1.0" a1111 F))
    (bcStrings (6 "0.0" a1112 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a1201 F))
    (bcStrings (6 "0.0" a1202 F))
    (bcStrings (6 "0.0" a1203 F))
    (bcStrings (6 "0.0" a1204 F))
    (bcStrings (6 "0.0" a1205 F))
    (bcStrings (6 "0.0" a1206 F))
    (bcStrings (6 "0.0" a1207 F))
    (bcStrings (6 "0.0" a1208 F))
    (bcStrings (6 "-1.0" a1209 F))
    (bcStrings (6 "0.0" a1210 F))
    (bcStrings (6 "0.0" a1211 F))
    (bcStrings (6 "1.0" a1212 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.0" a1301 F))
    (bcStrings (6 "1.0" a1302 F))
    (bcStrings (6 "1.0" a1303 F))
    (bcStrings (6 "0.0" a1304 F))
    (bcStrings (6 "0.0" a1305 F))
    (bcStrings (6 "1.0" a1306 F))
    (bcStrings (6 "1.0" a1307 F))
    (bcStrings (6 "0.0" a1308 F))
    (bcStrings (6 "0.0" a1309 F))
    (bcStrings (6 "1.0" a1310 F))
    (bcStrings (6 "1.0" a1311 F))
    (bcStrings (6 "1.0" a1312 F))
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'f04qafGen)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'damp,damp)
  htpSetProperty(page,'atol,atol)
  htpSetProperty(page,'btol,btol)
  htpSetProperty(page,'itnlim,itnlim)
  htpSetProperty(page,'msglvl,msglvl)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f04qafGen htPage ==
  m := htpProperty(htPage,'m)
  n := htpProperty(htPage,'n)
  damp := htpProperty(htPage,'damp)
  atol := htpProperty(htPage,'atol)
  btol := htpProperty(htPage,'btol)
  divisor := READ_-FROM_-STRING(atol)
  if (divisor < 1.0e-7) then divisor:=1.0e-7
  conlim := 1.0/divisor
  itnlim := htpProperty(htPage,'itnlim)
  msglvl := htpProperty(htPage,'msglvl)
  ifail := htpProperty(htPage,'ifail)
  lrwork := 1
  liwork := 1
  alist := htpInputAreaAlist htPage
  y := alist
  for k in 1..m repeat
    for l in 1..n repeat
      aelm := STRCONC((first y).1," ")
      arowlist := [aelm,:arowlist]
      y := rest y
    mata := [arowlist,:mata]
    arowlist := []
  astring := bcwords2liststring [bcwords2liststring y for y in mata]
  for z in 1..m repeat
    belm := STRCONC((first y).1," ")
    blist := [belm,:blist]
    y := rest y
  bstring := bcwords2liststring blist
  prefix := STRCONC('"f04qaf(",STRINGIMAGE m,",",STRINGIMAGE n,",")
  prefix := STRCONC(prefix,STRINGIMAGE damp,",")
  prefix := STRCONC(prefix,STRINGIMAGE atol,",",STRINGIMAGE btol,",")
  prefix := STRCONC(prefix,STRINGIMAGE conlim,",",STRINGIMAGE itnlim,",",STRINGIMAGE msglvl,",")
  prefix := STRCONC(prefix,STRINGIMAGE lrwork,",",STRINGIMAGE liwork,",")
  prefix := STRCONC(prefix,"[",bstring,"]::Matrix DoubleFloat,")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,",((",astring,"::Matrix MachineFloat)::ASP30(APROD)))")
  linkGen prefix





