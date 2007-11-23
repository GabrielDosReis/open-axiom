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


import '"macros"
)package "BOOT"

f02aaf() ==
  htInitPage('"F02AAF - All eigenvalues of real symmetric matrix (Black box)",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf02aaf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f02aaf| '|NagEigenPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Calculates all the eigenvalues of a real symmetric matrix ")
    (text . "{\it A} of order {\it n}.")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Order of matrix A, {\it n}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 n PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of A, {\it ia} ")
--    (text . "\inputbitmap{\htbmdir{}/great=.bitmap} n: ")
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 4 ia PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f02aafSolve)
  htShowPage()

f02aafSolve htPage ==
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
  n = '4  => f02aafDefaultSolve(htPage,ia,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..ia] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :matList]
  page := htInitPage("F02AAF - All eigenvalues of real symmetric matrix (Black box)",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f02aafGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


f02aafDefaultSolve  (htPage,ia,ifail) == 
  n := '4
  page := htInitPage('"F02AAF - All eigenvalues of real symmetric matrix (Black box)",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.5" a11 F))
    (bcStrings (6 "0.0" a12 F))
    (bcStrings (6 "2.3" a13 F))
    (bcStrings (6 "-2.6" a14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a21 F))
    (bcStrings (6 "0.5" a22 F))
    (bcStrings (6 "-1.4" a23 F))
    (bcStrings (6 "-0.7" a24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "2.3" a31 F))
    (bcStrings (6 "-1.4" a32 F))
    (bcStrings (6 "0.5" a33 F))
    (bcStrings (6 "0.0" a34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-2.6" a41 F))
    (bcStrings (6 "-0.7" a42 F))
    (bcStrings (6 "0.0" a43 F))
    (bcStrings (6 "0.5" a44 F)))
  htMakeDoneButton('"Continue",'f02aafGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02aafGen htPage ==
  n := htpProperty(htPage,'n)
  -- ia should be = n, unlike the example program
  -- where ia = nmax
--  ia := htpProperty(htPage,'ia)
  ia := n
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  y := REVERSE y
  for i in 1..ia repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    matform := [:matform,rowList]
    rowList := []
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  prefix := STRCONC('"f02aaf(",STRINGIMAGE ia,", ",STRINGIMAGE n,", ")
  prefix := STRCONC(prefix,matstring,", ",STRINGIMAGE ifail,")")
  linkGen prefix

f02abf() ==
  htInitPage('"F02ABF - All eigenvalues and eignevectors of real symmetric matrix (Black box)",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf02abf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f02abf| '|NagEigenPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Calculates all the eigenvalues and eigenvectors of a real ")
    (text . "symmetric matrix ")
    (text . "{\it A} of order {\it n}.")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Order of matrix A, {\it n}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 n PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of A, {\it ia} ")
--    (text . "\inputbitmap{\htbmdir{}/great=.bitmap} n: ")
--    (text . "\tab{32} \menuitemstyle{} \tab{34} ")
--    (text . "First dimension of V, {\it v} ")
--    (text . "\inputbitmap{\htbmdir{}/great=.bitmap} n: ")
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 4 ia PI))
--    (text . "\tab{34} ")
--    (bcStrings (6 4 v PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f02abfSolve)
  htShowPage()

f02abfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  ia := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ia)
--    objValUnwrap htpLabelSpadValue(htPage, 'ia)
  iv := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'v)
--    objValUnwrap htpLabelSpadValue(htPage, 'v)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '4  => f02abfDefaultSolve(htPage,ia,iv,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..ia] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :matList]
  page := htInitPage("F02ABF - All eigenvalues and eigenvectors of real symmetric matrix (Black box)",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f02abfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'iv,iv)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


f02abfDefaultSolve  (htPage,ia,iv,ifail) == 
  n := '4
  page := htInitPage('"F02ABF - All eigenvalues and eigenvectors of real symmetric matrix (Black box)",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.5" a11 F))
    (bcStrings (6 "0.0" a12 F))
    (bcStrings (6 "2.3" a13 F))
    (bcStrings (6 "-2.6" a14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a21 F))
    (bcStrings (6 "0.5" a22 F))
    (bcStrings (6 "-1.4" a23 F))
    (bcStrings (6 "-0.7" a24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "2.3" a31 F))
    (bcStrings (6 "-1.4" a32 F))
    (bcStrings (6 "0.5" a33 F))
    (bcStrings (6 "0.0" a34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-2.6" a41 F))
    (bcStrings (6 "-0.7" a42 F))
    (bcStrings (6 "0.0" a43 F))
    (bcStrings (6 "0.5" a44 F)))
  htMakeDoneButton('"Continue",'f02abfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'iv,iv)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02abfGen htPage ==
  n := htpProperty(htPage,'n)
  -- ia should be = n, unlike the example program
  -- where ia = nmax
--  ia := htpProperty(htPage,'ia)
--  iv  := htpProperty(htPage,'iv)
  ia := n
  iv := n
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  y := REVERSE y
  for i in 1..ia repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    matform := [:matform,rowList]
    rowList := []
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  prefix := STRCONC('"f02abf(",matstring,", ",STRINGIMAGE ia,", ")
  prefix := STRCONC(prefix,STRINGIMAGE n,", ",STRINGIMAGE iv,", ")
  linkGen STRCONC(prefix,STRINGIMAGE ifail,")")

f02adf() ==
  htInitPage('"F02ADF - All eigenvalues of generalized real eigenproblem of the form Ax = \lambda Bx where A and B are symmetric and B is positive definite",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf02adf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f02adf| '|NagEigenPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\blankline ")
    (text . "Calculates all the eigenvalues of Ax = \lambda Bx, where ")
    (text . "A and B are real symmetric matrices of order n and B is positive-definite ")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Order of matrices A and B, {\it n}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 n PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of A, {\it ia}: ")
--    (text . "\tab{32}  \menuitemstyle{} \tab{34} ")
--    (text . "\newline First dimension of B, {\it ib}: ")
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 4 ia PI))
--    (text . "\tab{34} ")
--    (bcStrings (6 4 ib F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f02adfSolve)
  htShowPage()

f02adfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  ia := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ia)
--    objValUnwrap htpLabelSpadValue(htPage, 'ia)
  ib := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ib)
--    objValUnwrap htpLabelSpadValue(htPage, 'ib)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '4  => f02adfDefaultSolve(htPage,ia,ib,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..ia] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  bmatList := 
    "append"/[h(k,n) for k in 1..ib] where h(k,n) ==
       bList := 
         "append"/[l(k,p) for p in 1..n] where l(k,p) ==
            bnam := INTERN STRCONC ('"b",STRINGIMAGE k,STRINGIMAGE p)
            [['bcStrings,[6, "0.0", bnam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       bList := [['text,:prefix],:bList]
  start := ('"\blankline \menuitemstyle{} \tab{2} Enter values of {\it b}:")
  bmatList := [['text,:start],:bmatList]  
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :matList,:bmatList]
  page := htInitPage("F02ADF - All eigenvalues of generalized real eigenproblem of the form Ax = \lambda Bx where A and B are symmetric and B is positive definite",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f02adfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'ib,ib)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


f02adfDefaultSolve  (htPage,ia,ib,ifail) == 
  n := '4
  page := htInitPage('"F02ADF - All eigenvalues of generalized real eigenproblem of the form Ax = \lambda Bx where A and B are symmetric and B is positive definite",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.5" a11 F))
    (bcStrings (6 "1.5" a12 F))
    (bcStrings (6 "6.6" a13 F))
    (bcStrings (6 "4.8" a14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.5" a21 F))
    (bcStrings (6 "6.5" a22 F))
    (bcStrings (6 "16.2" a23 F))
    (bcStrings (6 "8.6" a24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "6.6" a31 F))
    (bcStrings (6 "16.2" a32 F))
    (bcStrings (6 "37.6" a33 F))
    (bcStrings (6 "9.8" a34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "4.8" a41 F))
    (bcStrings (6 "8.6" a42 F))
    (bcStrings (6 "9.8" a43 F))
    (bcStrings (6 "-17.1" a44 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it B}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 1 b11 F))
    (bcStrings (6 3 b12 F))
    (bcStrings (6 4 b13 F))
    (bcStrings (6 1 b14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 3 b21 F))
    (bcStrings (6 13 b22 F))
    (bcStrings (6 16 b23 F))
    (bcStrings (6 11 b24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 b31 F))
    (bcStrings (6 16 b32 F))
    (bcStrings (6 24 b33 F))
    (bcStrings (6 18 b34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 1 b41 F))
    (bcStrings (6 11 b42 F))
    (bcStrings (6 18 b43 F))
    (bcStrings (6 27 b44 F)))
  htMakeDoneButton('"Continue",'f02adfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'ib,ib)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02adfGen htPage ==
  n := htpProperty(htPage,'n)
--  ia := htpProperty(htPage,'ia)
--  ib := htpProperty(htPage,'ib)
  ia := n
  ib := n
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  y := REVERSE y
  for i in 1..ia repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    matform := [:matform,rowList]
    rowList := []
  for i in 1..ib repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    bform := [:bform,rowList]
    rowList := []
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  bstring := bcwords2liststring [bcwords2liststring x for x in bform]
  prefix := STRCONC('"f02adf(",STRINGIMAGE ia,", ",STRINGIMAGE ib,", ")
  prefix := STRCONC(prefix,STRINGIMAGE n,", ",matstring,", ",bstring,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,")")
  linkGen prefix

f02aef() ==
  htInitPage('"F02AEF - All eigenvalues and eigenvectors of generalized real eigenproblem of the form Ax = \lambda Bx where A and B are symmetric and B is positive definite",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf02aef} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f02aef| '|NagEigenPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\blankline ")
    (text . "Calculates all the eigenvalues and eigenvectors of Ax = ")
    (text . "\lambda Bx, where A and B are real symmetric matrices of order ")
    (text . "n and B is positive-definite ")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Order of matrices A and B, {\it n}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 n PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of A, {\it ia}: ")
--    (text . "\tab{32}  \menuitemstyle{} \tab{34} ")
--    (text . "\newline First dimension of B, {\it ib}: ")
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 4 ia PI))
--    (text . "\tab{34} ")
--    (bcStrings (6 4 ib F))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of V, {\it iv}: ")
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 4 iv PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f02aefSolve)
  htShowPage()

f02aefSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  ia := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ia)
--    objValUnwrap htpLabelSpadValue(htPage, 'ia)
  ib := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ib)
--    objValUnwrap htpLabelSpadValue(htPage, 'ib)
  iv := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iv)
--    objValUnwrap htpLabelSpadValue(htPage, 'iv)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '4  => f02aefDefaultSolve(htPage,ia,ib,iv,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..ia] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  bmatList := 
    "append"/[h(k,n) for k in 1..ib] where h(k,n) ==
       bList := 
         "append"/[l(k,p) for p in 1..n] where l(k,p) ==
            bnam := INTERN STRCONC ('"b",STRINGIMAGE k,STRINGIMAGE p)
            [['bcStrings,[6, "0.0", bnam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       bList := [['text,:prefix],:bList]
  start := ('"\blankline \menuitemstyle{} \tab{2} Enter values of {\it b}:")
  bmatList := [['text,:start],:bmatList]  
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :matList,:bmatList]
  page := htInitPage("F02AEF - All eigenvalues and eigenvectors of generalized real eigenproblem of the form Ax = \lambda Bx where A and B are symmetric and B is positive definite",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f02aefGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'ib,ib)
--  htpSetProperty(page,'iv,iv)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


f02aefDefaultSolve  (htPage,ia,ib,iv,ifail) == 
  n := '4
  page := htInitPage('"F02AEF - All eigenvalues and eigenvectors of generalized real eigenproblem of the form Ax = \lambda Bx where A and B are symmetric and B is positive definite",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.5" a11 F))
    (bcStrings (6 "1.5" a12 F))
    (bcStrings (6 "6.6" a13 F))
    (bcStrings (6 "4.8" a14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.5" a21 F))
    (bcStrings (6 "6.5" a22 F))
    (bcStrings (6 "16.2" a23 F))
    (bcStrings (6 "8.6" a24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "6.6" a31 F))
    (bcStrings (6 "16.2" a32 F))
    (bcStrings (6 "37.6" a33 F))
    (bcStrings (6 "9.8" a34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "4.8" a41 F))
    (bcStrings (6 "8.6" a42 F))
    (bcStrings (6 "9.8" a43 F))
    (bcStrings (6 "-17.1" a44 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it B}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 1 b11 F))
    (bcStrings (6 3 b12 F))
    (bcStrings (6 4 b13 F))
    (bcStrings (6 1 b14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 3 b21 F))
    (bcStrings (6 13 b22 F))
    (bcStrings (6 16 b23 F))
    (bcStrings (6 11 b24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 b31 F))
    (bcStrings (6 16 b32 F))
    (bcStrings (6 24 b33 F))
    (bcStrings (6 18 b34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 1 b41 F))
    (bcStrings (6 11 b42 F))
    (bcStrings (6 18 b43 F))
    (bcStrings (6 27 b44 F)))
  htMakeDoneButton('"Continue",'f02aefGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'ib,ib)
--  htpSetProperty(page,'iv,iv)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02aefGen htPage ==
  n := htpProperty(htPage,'n)
--  ia := htpProperty(htPage,'ia)
--  ib := htpProperty(htPage,'ib)
--  iv := htpProperty(htPage,'iv)
  ia := n
  ib := n
  iv := n
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  y := REVERSE y
  for i in 1..ia repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    matform := [:matform,rowList]
    rowList := []
  for i in 1..ib repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    bform := [:bform,rowList]
    rowList := []
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  bstring := bcwords2liststring [bcwords2liststring x for x in bform]
  prefix := STRCONC('"f02aef(",STRINGIMAGE ia,", ",STRINGIMAGE ib,", ")
  prefix := STRCONC(prefix,STRINGIMAGE n,", ",STRINGIMAGE iv,", ")
  prefix := STRCONC(prefix,matstring,", ",bstring,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,")")
  linkGen prefix


f02aff() ==
  htInitPage('"F02AFF - All eigenvalues of real matrix (Black box)",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf02aff} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f02aff| '|NagEigenPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Calculates all the eigenvalues of a real unsymmetric matrix ")
    (text . "{\it A} of order {\it n}.")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Order of matrix A, {\it n}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 n PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of A, {\it ia} ")
--    (text . "\inputbitmap{\htbmdir{}/great=.bitmap} n: ")
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 4 ia PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f02affSolve)
  htShowPage()

f02affSolve htPage ==
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
  n = '4  => f02affDefaultSolve(htPage,ia,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..ia] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :matList]
  page := htInitPage("F02AFF - All eigenvalues of real matrix (Black box)",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f02affGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


f02affDefaultSolve  (htPage,ia,ifail) == 
  n := '4
  page := htInitPage('"F02AFF - All eigenvalues of real matrix (Black box)",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.5" a11 F))
    (bcStrings (6 "0.1" a12 F))
    (bcStrings (6 "4.5" a13 F))
    (bcStrings (6 "-1.5" a14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-22.5" a21 F))
    (bcStrings (6 "3.5" a22 F))
    (bcStrings (6 "12.5" a23 F))
    (bcStrings (6 "-2.5" a24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-2.5" a31 F))
    (bcStrings (6 "0.3" a32 F))
    (bcStrings (6 "4.5" a33 F))
    (bcStrings (6 "-2.5" a34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-2.5" a41 F))
    (bcStrings (6 "0.1" a42 F))
    (bcStrings (6 "4.5" a43 F))
    (bcStrings (6 "2.5" a44 F)))
  htMakeDoneButton('"Continue",'f02affGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02affGen htPage ==
  n := htpProperty(htPage,'n)
  -- ia should be = n, unlike the example program
  -- where ia = nmax
--  ia := htpProperty(htPage,'ia)
  ia := n
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  y := REVERSE y
  for i in 1..ia repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    matform := [:matform,rowList]
    rowList := []
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  prefix := STRCONC('"f02aff(",STRINGIMAGE ia,", ",STRINGIMAGE n,", ")
  prefix := STRCONC(prefix,matstring,", ",STRINGIMAGE ifail,")")
  linkGen prefix

f02agf() ==
  htInitPage('"F02AGF - All eigenvalues and eignevectors of real matrix (Black box)",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf02agf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f02agf| '|NagEigenPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Calculates all the eigenvalues and eigenvectors of a real ")
    (text . "unsymmetric matrix {\it A} of order {\it n}.")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Order of matrix A, {\it n}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 n PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of A, {\it ia} ")
--    (text . "\inputbitmap{\htbmdir{}/great=.bitmap} n: ")
--    (text . "\tab{32} \menuitemstyle{} \tab{34} ")
--    (text . "First dimension of VR, {\it ivr} ")
--    (text . "\inputbitmap{\htbmdir{}/great=.bitmap} n: ")
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 4 ia PI))
--    (text . "\tab{34} ")
--    (bcStrings (6 4 ivr PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of VI, {\it ivi} ")
--    (text . "\inputbitmap{\htbmdir{}/great=.bitmap} n: ")
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 4 ivi PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f02agfSolve)
  htShowPage()

f02agfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  ia := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ia)
--    objValUnwrap htpLabelSpadValue(htPage, 'ia)
  ivr := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ivr)
--    objValUnwrap htpLabelSpadValue(htPage, 'ivr)
  ivi := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ivi)
--    objValUnwrap htpLabelSpadValue(htPage, 'ivi)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '4  => f02agfDefaultSolve(htPage,ia,ivr,ivi,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..ia] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :matList]
  page := htInitPage("F02AGF - All eigenvalues and eigenvectors of real matrix (Black box)",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f02agfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'ivr,ivr)
--  htpSetProperty(page,'ivi,ivi)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


f02agfDefaultSolve  (htPage,ia,ivr,ivi,ifail) == 
  n := '4
  page := htInitPage('"F02AGF - All eigenvalues and eigenvectors of real matrix (Black box)",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.5" a11 F))
    (bcStrings (6 "0.1" a12 F))
    (bcStrings (6 "4.5" a13 F))
    (bcStrings (6 "-1.5" a14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-22.5" a21 F))
    (bcStrings (6 "3.5" a22 F))
    (bcStrings (6 "12.5" a23 F))
    (bcStrings (6 "-2.5" a24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-2.5" a31 F))
    (bcStrings (6 "0.3" a32 F))
    (bcStrings (6 "4.5" a33 F))
    (bcStrings (6 "-2.5" a34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-2.5" a41 F))
    (bcStrings (6 "0.1" a42 F))
    (bcStrings (6 "4.5" a43 F))
    (bcStrings (6 "2.5" a44 F)))
  htMakeDoneButton('"Continue",'f02agfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'ivr,ivr)
--  htpSetProperty(page,'ivi,ivi)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02agfGen htPage ==
  n := htpProperty(htPage,'n)
  -- ia should be = n, unlike the example program
  -- where ia = nmax
--  ia := htpProperty(htPage,'ia)
--  ivr  := htpProperty(htPage,'ivr)
--  ivi  := htpProperty(htPage,'ivi)
  ia := n
  ivr := n
  ivi := n
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  y := REVERSE y
  for i in 1..ia repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    matform := [:matform,rowList]
    rowList := []
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  prefix := STRCONC('"f02agf(",STRINGIMAGE ia,", ",STRINGIMAGE n,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ivr,", ",STRINGIMAGE ivi,", ")
  linkGen STRCONC(prefix,matstring,", ",STRINGIMAGE ifail,")")

f02ajf() ==
  htInitPage('"F02AJF - All eigenvalues of complex matrix (Black box)",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf02ajf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f02ajf| '|NagEigenPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\blankline ")
    (text . "Calculates all the eigenvalues of a complex matrix {\it A} ")
    (text . "of order {\it n}.")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Order of matrix A, {\it n}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 n PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of array containing real parts, ")
--    (text . " {\it iar}: \newline \tab{2} ")
--    (bcStrings (6 4 iar PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of array containing imaginary parts,")
--    (text . " {\it iai}: \newline \tab{2} ")
--    (bcStrings (6 4 iai F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f02ajfSolve)
  htShowPage()

f02ajfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  iar := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iar)
--    objValUnwrap htpLabelSpadValue(htPage, 'iar)
  iai := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iai)
--    objValUnwrap htpLabelSpadValue(htPage, 'iai)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '4  => f02ajfDefaultSolve(htPage,iar,iai,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..iar] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  bmatList := 
    "append"/[h(k,n) for k in 1..iai] where h(k,n) ==
       bList := 
         "append"/[l(k,p) for p in 1..n] where l(k,p) ==
            bnam := INTERN STRCONC ('"b",STRINGIMAGE k,STRINGIMAGE p)
            [['bcStrings,[6, "0.0", bnam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       bList := [['text,:prefix],:bList]
  start := ('"\blankline \menuitemstyle{}\tab{2}Enter imag values of {\it A}:")
  bmatList := [['text,:start],:bmatList]  
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :matList,:bmatList]
  page := htInitPage("F02AJF - All eigenvalues of complex matrix (Black box)",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter real values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f02ajfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'iar,iar)
--  htpSetProperty(page,'iai,iai)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


f02ajfDefaultSolve  (htPage,iar,iai,ifail) == 
  n := '4
  page := htInitPage('"F02AJF - All eigenvalues of complex matrix (Black box)",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter real values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-21.0" a11 F))
    (bcStrings (6 "0.0" a12 F))
    (bcStrings (6 "13.6" a13 F))
    (bcStrings (6 "0.0" a14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a21 F))
    (bcStrings (6 "26.0" a22 F))
    (bcStrings (6 "7.5" a23 F))
    (bcStrings (6 "2.5" a24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-2.0" a31 F))
    (bcStrings (6 "1.68" a32 F))
    (bcStrings (6 "4.5" a33 F))
    (bcStrings (6 "1.5" a34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a41 F))
    (bcStrings (6 "-2.6" a42 F))
    (bcStrings (6 "-2.7" a43 F))
    (bcStrings (6 "2.5" a44 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter imaginary values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-5.0" b11 F))
    (bcStrings (6 "24.6" b12 F))
    (bcStrings (6 "10.2"b13 F))
    (bcStrings (6 "4.0" b14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "22.5" b21 F))
    (bcStrings (6 "-5.0" b22 F))
    (bcStrings (6 "-10.0" b23 F))
    (bcStrings (6 "0.0" b24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.5" b31 F))
    (bcStrings (6 "2.24" b32 F))
    (bcStrings (6 "-5.0" b33 F))
    (bcStrings (6 "2.0" b34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-2.5" b41 F))
    (bcStrings (6 "0.0" b42 F))
    (bcStrings (6 "3.6" b43 F))
    (bcStrings (6 "-5.0" b44 F)))
  htMakeDoneButton('"Continue",'f02ajfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'iar,iar)
--  htpSetProperty(page,'iai,iai)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02ajfGen htPage ==
  n := htpProperty(htPage,'n)
--  iar := htpProperty(htPage,'iar)
--  iai := htpProperty(htPage,'iai)
  iar := n
  iai := n
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  y := REVERSE y
  for i in 1..iar repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    matform := [:matform,rowList]
    rowList := []
  for i in 1..iai repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    bform := [:bform,rowList]
    rowList := []
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  bstring := bcwords2liststring [bcwords2liststring x for x in bform]
  prefix := STRCONC('"f02ajf(",STRINGIMAGE n,", ",STRINGIMAGE iar,", ")
  prefix := STRCONC(prefix,STRINGIMAGE iai,", ",matstring,", ",bstring,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,")")
  linkGen prefix

f02akf() ==
  htInitPage('"F02AKF - All eigenvalues and eigenvectors of complex matrix (Black box)",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf02akf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f02akf| '|NagEigenPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Calculates all the eigenvalues and eigenvectors of a complex ")
    (text . "matrix {\it A} of order {\it n}.")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Order of matrix A, {\it n}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 n PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of array containing real parts, ")
--    (text . " {\it iar}: \newline \tab{2} ")
--    (bcStrings (6 4 iar PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of array containing imaginary parts,")
--    (text . " {\it iai}: \newline \tab{2} ")
--    (bcStrings (6 4 iai F))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} \newline ")
--    (text . "First dimension of array of real parts of the eigenvectors, ")
--    (text . " {\it ivr}: \newline \tab{2} ")
--    (bcStrings (6 4 ivr PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} \newline ")
--    (text . "First dimension of array of imaginary parts of the eigenvectors,")
--    (text . " {\it ivi}: \newline \tab{2} ")
--    (bcStrings (6 4 ivi PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f02akfSolve)
  htShowPage()

f02akfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  iar := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iar)
--    objValUnwrap htpLabelSpadValue(htPage, 'iar)
  iai := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iai)
--    objValUnwrap htpLabelSpadValue(htPage, 'iai)
  ivr := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ivr)
--    objValUnwrap htpLabelSpadValue(htPage, 'ivr)
  ivi := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ivi)
--    objValUnwrap htpLabelSpadValue(htPage, 'ivi)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '4  => f02akfDefaultSolve(htPage,iar,iai,ivr,ivi,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..iar] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  bmatList := 
    "append"/[h(k,n) for k in 1..iai] where h(k,n) ==
       bList := 
         "append"/[l(k,p) for p in 1..n] where l(k,p) ==
            bnam := INTERN STRCONC ('"b",STRINGIMAGE k,STRINGIMAGE p)
            [['bcStrings,[6, "0.0", bnam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       bList := [['text,:prefix],:bList]
  start := ('"\blankline \menuitemstyle{}\tab{2}Enter imag values of {\it A}:")
  bmatList := [['text,:start],:bmatList]  
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :matList,:bmatList]
  page := htInitPage("F02AKF - All eigenvalues and eigenvectors of complex matrix (Black box)",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter real values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f02akfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'iar,iar)
--  htpSetProperty(page,'iai,iai)
--  htpSetProperty(page,'ivr,ivr)
--  htpSetProperty(page,'ivi,ivi)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


f02akfDefaultSolve  (htPage,iar,iai,ivr,ivi,ifail) == 
  n := '4
  page := htInitPage('"F02AKF - All eigenvalues and eigenvectors of complex matrix (Black box)",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter real values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-21.0" a11 F))
    (bcStrings (6 "0.0" a12 F))
    (bcStrings (6 "13.6" a13 F))
    (bcStrings (6 "0.0" a14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a21 F))
    (bcStrings (6 "26.0" a22 F))
    (bcStrings (6 "7.5" a23 F))
    (bcStrings (6 "2.5" a24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-2.0" a31 F))
    (bcStrings (6 "1.68" a32 F))
    (bcStrings (6 "4.5" a33 F))
    (bcStrings (6 "1.5" a34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a41 F))
    (bcStrings (6 "-2.6" a42 F))
    (bcStrings (6 "-2.7" a43 F))
    (bcStrings (6 "2.5" a44 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter imaginary values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-5.0" b11 F))
    (bcStrings (6 "24.6" b12 F))
    (bcStrings (6 "10.2"b13 F))
    (bcStrings (6 "4.0" b14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "22.5" b21 F))
    (bcStrings (6 "-5.0" b22 F))
    (bcStrings (6 "-10.0" b23 F))
    (bcStrings (6 "0.0" b24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.5" b31 F))
    (bcStrings (6 "2.24" b32 F))
    (bcStrings (6 "-5.0" b33 F))
    (bcStrings (6 "2.0" b34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-2.5" b41 F))
    (bcStrings (6 "0.0" b42 F))
    (bcStrings (6 "3.6" b43 F))
    (bcStrings (6 "-5.0" b44 F)))
  htMakeDoneButton('"Continue",'f02akfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'iar,iar)
--  htpSetProperty(page,'iai,iai)
--  htpSetProperty(page,'ivr,ivr)
--  htpSetProperty(page,'ivi,ivi)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02akfGen htPage ==
  n := htpProperty(htPage,'n)
--  iar := htpProperty(htPage,'iar)
--  iai := htpProperty(htPage,'iai)
--  ivr := htpProperty(htPage,'ivr)
--  ivi := htpProperty(htPage,'ivi)
  iar := n
  iai := n
  ivr := n
  ivi := n
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  y := REVERSE y
  for i in 1..iar repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    matform := [:matform,rowList]
    rowList := []
  for i in 1..iai repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    bform := [:bform,rowList]
    rowList := []
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  bstring := bcwords2liststring [bcwords2liststring x for x in bform]
  prefix := STRCONC('"f02akf(",STRINGIMAGE iar,", ",STRINGIMAGE iai,", ")
  prefix := STRCONC(prefix,STRINGIMAGE n,", ",STRINGIMAGE ivr,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ivi,", ",matstring,", ",bstring,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,")")
  linkGen prefix

f02awf() ==
  htInitPage('"F02AWF - All eigenvalues of complex Hermitian matrix (Black box)",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf02awf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f02awf| '|NagEigenPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\blankline ")
    (text . "Calculates all the eigenvalues of a complex Hermitian matrix ")
    (text . "{\it A} of order {\it n}.")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Order of the complex Hermitian matrix A, {\it n}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 n PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of array containing real parts, ")
--    (text . " {\it iar}: \newline \tab{2} ")
--    (bcStrings (6 4 iar PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of array containing imaginary parts,")
--    (text . " {\it iai}: \newline \tab{2} ")
--    (bcStrings (6 4 iai F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f02awfSolve)
  htShowPage()

f02awfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  iar := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iar)
--    objValUnwrap htpLabelSpadValue(htPage, 'iar)
  iai := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iai)
--    objValUnwrap htpLabelSpadValue(htPage, 'iai)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '4  => f02awfDefaultSolve(htPage,iar,iai,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..iar] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  bmatList := 
    "append"/[h(k,n) for k in 1..iai] where h(k,n) ==
       bList := 
         "append"/[l(k,p) for p in 1..n] where l(k,p) ==
            bnam := INTERN STRCONC ('"b",STRINGIMAGE k,STRINGIMAGE p)
            [['bcStrings,[6, "0.0", bnam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       bList := [['text,:prefix],:bList]
  start := ('"\blankline \menuitemstyle{}\tab{2}Enter imaginary values {\it AI}:")
  bmatList := [['text,:start],:bmatList]  
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :matList,:bmatList]
  page := htInitPage("F02AWF - All eigenvalues of complex Hermitian matrix (Black box)",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter real values of {\it AR}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f02awfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'iar,iar)
--  htpSetProperty(page,'iai,iai)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


f02awfDefaultSolve  (htPage,iar,iai,ifail) == 
  n := '4
  page := htInitPage('"F02AWF - All eigenvalues of complex Hermitian matrix (Black box)",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter real values {\it AR}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.5" a11 F))
    (bcStrings (6 "0.0" a12 F))
    (bcStrings (6 "1.84" a13 F))
    (bcStrings (6 "2.08" a14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a21 F))
    (bcStrings (6 "0.5" a22 F))
    (bcStrings (6 "1.12" a23 F))
    (bcStrings (6 "-0.56" a24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.84" a31 F))
    (bcStrings (6 "1.12" a32 F))
    (bcStrings (6 "0.5" a33 F))
    (bcStrings (6 "0.0" a34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "2.08" a41 F))
    (bcStrings (6 "-0.56" a42 F))
    (bcStrings (6 "0.0" a43 F))
    (bcStrings (6 "0.5" a44 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter imaginary values {\it AI}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" b11 F))
    (bcStrings (6 "0.0" b12 F))
    (bcStrings (6 "1.38" b13 F))
    (bcStrings (6 "-1.56" b14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" b21 F))
    (bcStrings (6 "0.0" b22 F))
    (bcStrings (6 "0.84" b23 F))
    (bcStrings (6 "0.42" b24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-1.38" b31 F))
    (bcStrings (6 "-0.84" b32 F))
    (bcStrings (6 "0.0" b33 F))
    (bcStrings (6 "0.0" b34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.56" b41 F))
    (bcStrings (6 "-0.42" b42 F))
    (bcStrings (6 "0.0" b43 F))
    (bcStrings (6 "0.0" b44 F)))
  htMakeDoneButton('"Continue",'f02awfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'iar,iar)
--  htpSetProperty(page,'iai,iai)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02awfGen htPage ==
  n := htpProperty(htPage,'n)
--  iar := htpProperty(htPage,'iar)
--  iai := htpProperty(htPage,'iai)
  iar := n
  iai := n
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  y := REVERSE y
  for i in 1..iar repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    matform := [:matform,rowList]
    rowList := []
  for i in 1..iai repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    bform := [:bform,rowList]
    rowList := []
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  bstring := bcwords2liststring [bcwords2liststring x for x in bform]
  prefix := STRCONC('"f02awf(",STRINGIMAGE n,", ",STRINGIMAGE iar,", ")
  prefix := STRCONC(prefix,STRINGIMAGE iai,", ",matstring,", ",bstring,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,")")
  linkGen prefix

f02axf() ==
  htInitPage('"F02AXF - All eigenvalues and eigenvectors of complex Hermitian matrix (Black box)",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf02axf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f02axf| '|NagEigenPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Calculates all the eigenvalues and eigenvectors of a complex ")
    (text . "Hermitian matrix {\it A} of order {\it n}.")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Order of matrix A, {\it n}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 n PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of array containing real parts, ")
--    (text . " {\it iar}: \newline \tab{2} ")
--    (bcStrings (6 4 iar PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of array containing imaginary parts,")
--    (text . " {\it iai}: \newline \tab{2} ")
--    (bcStrings (6 4 iai F))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} \newline ")
--    (text . "First dimension of array of real parts of the eigenvectors, ")
--    (text . " {\it ivr}: \newline \tab{2} ")
--    (bcStrings (6 4 ivr PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} \newline ")
--    (text . "First dimension of array of imaginary parts of the eigenvectors,")
--    (text . " {\it ivi}: \newline \tab{2} ")
--    (bcStrings (6 4 ivi PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f02axfSolve)
  htShowPage()

f02axfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  iar := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iar)
--    objValUnwrap htpLabelSpadValue(htPage, 'iar)
  iai := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iai)
--    objValUnwrap htpLabelSpadValue(htPage, 'iai)
  ivr := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ivr)
--    objValUnwrap htpLabelSpadValue(htPage, 'ivr)
  ivi := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ivi)
--    objValUnwrap htpLabelSpadValue(htPage, 'ivi)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '4  => f02axfDefaultSolve(htPage,iar,iai,ivr,ivi,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..iar] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  bmatList := 
    "append"/[h(k,n) for k in 1..iai] where h(k,n) ==
       bList := 
         "append"/[l(k,p) for p in 1..n] where l(k,p) ==
            bnam := INTERN STRCONC ('"b",STRINGIMAGE k,STRINGIMAGE p)
            [['bcStrings,[6, "0.0", bnam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       bList := [['text,:prefix],:bList]
  start := ('"\blankline \menuitemstyle{}\tab{2}Enter imaginary values of {\it A}:")
  bmatList := [['text,:start],:bmatList]  
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :matList,:bmatList]
  page := htInitPage("F02AXF - All eigenvalues and eigenvectors of complex Hermitian matrix (Black box)",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter real values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f02axfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'iar,iar)
--  htpSetProperty(page,'iai,iai)
--  htpSetProperty(page,'ivr,ivr)
--  htpSetProperty(page,'ivi,ivi)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


f02axfDefaultSolve  (htPage,iar,iai,ivr,ivi,ifail) == 
  n := '4
  page := htInitPage('"F02AXF - All eigenvalues and eigenvectors of complex Hermitian matrix (Black box)",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter real values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.5" a11 F))
    (bcStrings (6 "0.0" a12 F))
    (bcStrings (6 "1.84" a13 F))
    (bcStrings (6 "2.08" a14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a21 F))
    (bcStrings (6 "0.5" a22 F))
    (bcStrings (6 "1.12" a23 F))
    (bcStrings (6 "-0.56" a24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.84" a31 F))
    (bcStrings (6 "1.12" a32 F))
    (bcStrings (6 "0.5" a33 F))
    (bcStrings (6 "0.0" a34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "2.08" a41 F))
    (bcStrings (6 "-0.56" a42 F))
    (bcStrings (6 "0.0" a43 F))
    (bcStrings (6 "0.5" a44 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter imaginary values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" b11 F))
    (bcStrings (6 "0.0" b12 F))
    (bcStrings (6 "1.38" b13 F))
    (bcStrings (6 "-1.56" b14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" b21 F))
    (bcStrings (6 "0.0" b22 F))
    (bcStrings (6 "0.84" b23 F))
    (bcStrings (6 "0.42" b24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-1.38" b31 F))
    (bcStrings (6 "-0.84" b32 F))
    (bcStrings (6 "0.0" b33 F))
    (bcStrings (6 "0.0" b34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.56" b41 F))
    (bcStrings (6 "-0.42" b42 F))
    (bcStrings (6 "0.0" b43 F))
    (bcStrings (6 "0.0" b44 F)))
  htMakeDoneButton('"Continue",'f02axfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'iar,iar)
--  htpSetProperty(page,'iai,iai)
--  htpSetProperty(page,'ivr,ivr)
--  htpSetProperty(page,'ivi,ivi)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02axfGen htPage ==
  n := htpProperty(htPage,'n)
--  iar := htpProperty(htPage,'iar)
--  iai := htpProperty(htPage,'iai)
--  ivr := htpProperty(htPage,'ivr)
--  ivi := htpProperty(htPage,'ivi)
  iar := n
  iai := n
  ivr := n
  ivi := n
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  y := REVERSE y
  for i in 1..iar repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    matform := [:matform,rowList]
    rowList := []
  for i in 1..iai repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    bform := [:bform,rowList]
    rowList := []
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  bstring := bcwords2liststring [bcwords2liststring x for x in bform]
  prefix := STRCONC('"f02axf(",matstring,", ",STRINGIMAGE iar,", ",bstring)
  prefix := STRCONC(prefix,", ",STRINGIMAGE iai,", ",STRINGIMAGE n,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ivr,", ",STRINGIMAGE ivi,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,")")
  linkGen prefix

f02bbf() ==
  htInitPage('"F02BBF - Selected eigenvalues and eigenvectors of real symmetric matrix (Black box)",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf02bbf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f02bbf| '|NagEigenPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Calculates selected eigenvalues and eigenvectors of a real ")
    (text . "symmetric matrix {\it A} of order {\it n} by reduction to ")
    (text . "tridiagonal form, bisection and inverse iteration, where the ")
    (text . "selected eigenvalues lie within a given interval [{\it l,u}].")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Order of matrix A, {\it n}: ")
    (text . "\tab{32} \menuitemstyle{} \tab{34} ")
    (text . "Max number of eigenvectors, {\it m}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 n PI))
    (text . "\tab{34} ")
    (bcStrings (6 3 m PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Lower end-point of interval {\it l}: ")
    (text . "\tab{32} \menuitemstyle{} \tab{34} ")
    (text . "Upper end-point of interval {\it u}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-2.0" alb F))
    (text . "\tab{34} ")
    (bcStrings (6 "3.0" ub F))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of A, {\it ia} ")
--    (text . "\inputbitmap{\htbmdir{}/great=.bitmap} n: ")
--    (text . "\tab{32} \menuitemstyle{} \tab{34} ")
--    (text . "First dimension of V, {\it v} ")
--    (text . "\inputbitmap{\htbmdir{}/great=.bitmap} n: ")
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 4 ia PI))
--    (text . "\tab{34} ")
--    (bcStrings (6 4 iv PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f02bbfSolve)
  htShowPage()

f02bbfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  alb := htpLabelInputString(htPage,'alb)
  ub := htpLabelInputString(htPage,'ub)
  ia := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ia)
--    objValUnwrap htpLabelSpadValue(htPage, 'ia)
  iv := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iv)
--    objValUnwrap htpLabelSpadValue(htPage, 'iv)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '4  => f02bbfDefaultSolve(htPage,m,alb,ub,ia,iv,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..ia] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :matList]
  page := htInitPage('"F02BBF - Selected eigenvalues and eigenvectors of real symmetric matrix (Black box)",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f02bbfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'alb,alb)
  htpSetProperty(page,'ub,ub)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'iv,iv)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


f02bbfDefaultSolve (htPage,m,alb,ub,ia,iv,ifail) ==
  n := '4
  page := htInitPage('"F02BBF - Selected eigenvalues and eigenvectors of real symmetric matrix (Black box)",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.5" a11 F))
    (bcStrings (6 "0.0" a12 F))
    (bcStrings (6 "2.3" a13 F))
    (bcStrings (6 "-2.6" a14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" a21 F))
    (bcStrings (6 "0.5" a22 F))
    (bcStrings (6 "-1.4" a23 F))
    (bcStrings (6 "-0.7" a24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "2.3" a31 F))
    (bcStrings (6 "-1.4" a32 F))
    (bcStrings (6 "0.5" a33 F))
    (bcStrings (6 "0.0" a34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-2.6" a41 F))
    (bcStrings (6 "-0.7" a42 F))
    (bcStrings (6 "0.0" a43 F))
    (bcStrings (6 "0.5" a44 F)))
  htMakeDoneButton('"Continue",'f02bbfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'alb,alb)
  htpSetProperty(page,'ub,ub)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'iv,iv)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02bbfGen htPage ==
  n := htpProperty(htPage,'n)
  m := htpProperty(htPage,'m)
  alb := htpProperty(htPage,'alb)
  ub := htpProperty(htPage,'ub)
  -- ia should be = n, unlike the example program
  -- where ia = nmax
--  ia := htpProperty(htPage,'ia)
--  iv := htpProperty(htPage,'iv)
  ia := n
  iv := n
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  y := REVERSE y
  for i in 1..ia repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    matform := [:matform,rowList]
    rowList := []
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  prefix := STRCONC('"f02bbf(",STRINGIMAGE ia,", ",STRINGIMAGE n,", ")
  prefix := STRCONC(prefix,alb,", ",ub,", ",STRINGIMAGE m,", ",STRINGIMAGE iv)
  prefix := STRCONC(prefix,", ",matstring,", ",STRINGIMAGE ifail,")")
  linkGen prefix

f02bjf() ==
  htInitPage('"F02BJF - All eigenvalues and optionally eigenvectors of generalized eigenproblem by {\it QZ} algorithm, real matrices (Black box)",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf02bjf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f02bjf| '|NagEigenPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Calculates all the eigenvalues and, if required, all the ")
    (text . "eigenvectors of Ax = \lambda Bx, where A and B are real ")
    (text . "symmetric matrices of order n and B using the QZ algorithm. ")
    (text . "The routine does not actually produce the eigenvalues ")
    (text . "\inputbitmap{\htbmdir{}/lamdaj.bitmap}, but instead returns ")
    (text . "\inputbitmap{\htbmdir{}/alphaj.bitmap} and ")
    (text . "\inputbitmap{\htbmdir{}/betaj.bitmap} ")
    (text . "such that \inputbitmap{\htbmdir{}/lamdaj.bitmap} = ")
    (text . "\inputbitmap{\htbmdir{}/alphaj.bitmap}  / ")
    (text . "\inputbitmap{\htbmdir{}/betaj.bitmap}, ")
    (text . "for j = 1,2,...,n. ")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Order of matrices A and B, {\it n}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 n PI))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of A, {\it ia}: ")
--    (text . "\tab{32}  \menuitemstyle{} \tab{34} ")
--    (text . "\newline First dimension of B, {\it ib}: ")
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 4 ia PI))
--    (text . "\tab{34} ")
--    (bcStrings (6 4 ib F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of V, {\it iv}: ")
--    (text . "\tab{32}  \menuitemstyle{} \tab{34} ")
    (text . "\newline Tolerance, {\it eps}: ")
    (text . "\newline \tab{2} ")
--    (bcStrings (6 4 iv PI))
--    (text . "\tab{34} ")
    (bcStrings (6 "1.0e-4" eps F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Are eigenvectors required: ")
    (radioButtons matv
        ("" "  true" true)
        ("" "  false" false))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f02bjfSolve)
  htShowPage()

f02bjfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  ia := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ia)
--    objValUnwrap htpLabelSpadValue(htPage, 'ia)
  ib := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ib)
--    objValUnwrap htpLabelSpadValue(htPage, 'ib)
  iv := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iv)
--    objValUnwrap htpLabelSpadValue(htPage, 'iv)
  eps := htpLabelInputString(htPage,'eps)
  bool := htpButtonValue(htPage,'matv)
  matv :=
    bool = 'true => '"true"
    '"false"
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '4  => f02bjfDefaultSolve(htPage,ia,ib,iv,eps,matv,ifail)
  matList :=
    "append"/[f(i,n) for i in 1..ia] where f(i,n) ==
       labelList := 
         "append"/[g(i,j) for j in 1..n] where g(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  bmatList := 
    "append"/[h(k,n) for k in 1..ib] where h(k,n) ==
       bList := 
         "append"/[l(k,p) for p in 1..n] where l(k,p) ==
            bnam := INTERN STRCONC ('"b",STRINGIMAGE k,STRINGIMAGE p)
            [['bcStrings,[6, "0.0", bnam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       bList := [['text,:prefix],:bList]
  start := ('"\blankline \menuitemstyle{} \tab{2} Enter values of {\it b}:")
  bmatList := [['text,:start],:bmatList]  
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :matList,:bmatList]
  page := htInitPage('"F02BJF - All eigenvalues and optionally eigenvectors of generalized eigenproblem by {\it QZ} algorithm, real matrices (Black box)",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f02bjfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'ib,ib)
--  htpSetProperty(page,'iv,iv)
  htpSetProperty(page,'eps,eps)
  htpSetProperty(page,'matv,matv)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


f02bjfDefaultSolve  (htPage,ia,ib,iv,eps,matv,ifail) ==
  n := '4
  page := htInitPage('"F02BJF - All eigenvalues and optionally eigenvectors of generalized eigenproblem by {\it QZ} algorithm, real matrices (Black box)",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "3.9" a11 F))
    (bcStrings (6 "12.5" a12 F))
    (bcStrings (6 "-34.5" a13 F))
    (bcStrings (6 "-0.5" a14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "4.3" a21 F))
    (bcStrings (6 "21.5" a22 F))
    (bcStrings (6 "-47.5" a23 F))
    (bcStrings (6 "7.5" a24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "4.3" a31 F))
    (bcStrings (6 "21.5" a32 F))
    (bcStrings (6 "-43.5" a33 F))
    (bcStrings (6 "3.5" a34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "4.4" a41 F))
    (bcStrings (6 "26.0" a42 F))
    (bcStrings (6 "-46.0" a43 F))
    (bcStrings (6 "6.0" a44 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it B}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 1 b11 F))
    (bcStrings (6 2 b12 F))
    (bcStrings (6 "-3" b13 F))
    (bcStrings (6 1 b14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 1 b21 F))
    (bcStrings (6 3 b22 F))
    (bcStrings (6 "-5" b23 F))
    (bcStrings (6 4b24 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 1 b31 F))
    (bcStrings (6 3 b32 F))
    (bcStrings (6 -4 b33 F))
    (bcStrings (6 3 b34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 1 b41 F))
    (bcStrings (6 3 b42 F))
    (bcStrings (6 -4 b43 F))
    (bcStrings (6 4 b44 F)))
  htMakeDoneButton('"Continue",'f02bjfGen)
  htpSetProperty(page,'n,n)
--  htpSetProperty(page,'ia,ia)
--  htpSetProperty(page,'ib,ib)
--  htpSetProperty(page,'iv,iv)
  htpSetProperty(page,'eps,eps)
  htpSetProperty(page,'matv,matv)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02bjfGen htPage ==
  n := htpProperty(htPage,'n)
--  ia := htpProperty(htPage,'ia)
--  ib := htpProperty(htPage,'ib)
--  iv := htpProperty(htPage,'iv)
  ia := n
  ib := n
  iv := n
  eps := htpProperty(htPage,'eps)
  matv := htpProperty(htPage,'matv)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  y := REVERSE y
  for i in 1..ia repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    matform := [:matform,rowList]
    rowList := []
  for i in 1..ib repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    bform := [:bform,rowList]
    rowList := []
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  bstring := bcwords2liststring [bcwords2liststring x for x in bform]
  prefix := STRCONC('"f02bjf(",STRINGIMAGE n,", ",STRINGIMAGE ia,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ib,", ",eps,", ",matv,", ")
  prefix := STRCONC(prefix,STRINGIMAGE iv,", ",matstring,", ",bstring,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,")")
  linkGen prefix


f02fjf() ==
  htInitPage('"F02FJF - Selected eigenvalues and eigenvectors of sparse symmetric eigenproblem",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf02fjf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f02fjf| '|NagEigenPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Finds the {\it m} eigenvalues of largest absolute value and the ")
    (text . "corresponding eigenvectors for the eigenvalue problem ")
    (text . "{\it Cx = \htbitmap{lambda}x}, where {\it C} is a real matrix ")
    (text . "of order {\it n} such that {\it BC = \htbitmap{ctb}} for a ")
    (text . "given positive-definite matrix {\it B}. ")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "Read the input file to see the example program. ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "\spadcommand{)read f02fjf \bound{s0}} "))
  htShowPage()


f02wef() ==
  htInitPage('"F02WEF - SVD of real matrix",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf02wef} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f02wef| '|NagEigenPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Returns all or part of the singular value decomposition of a ")
    (text . "real {\it m} by {\it n} matrix {\it A}.")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Rows of matrix A, {\it m}: ")
    (text . "\tab{32}  \menuitemstyle{} \tab{34} ")
    (text . "Columns of matrix A, {\it n}: \newline \tab{2} ")
    (bcStrings (6 5 m PI))
    (text . "\tab{34} ")
    (bcStrings (6 3 n PI))
    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of A, {\it lda}: ")
--    (text . "\tab{32}  \menuitemstyle{} \tab{34} ")
--    (text . "First dimension of B, {\it ldb}: ")
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 5 lda PI))
--    (text . "\tab{34} ")
--    (bcStrings (6 5 ldb PI))
--    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Number of columns of matrix B, {\it ncolb}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 1 ncolb PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Is the matrix {\it Q} required, {\it wantq}:")
    (radioButtons wantq
        (" " "  true" qtrue)
        (" " "  false" qfalse))
    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of {\it Q}, {\it ldq}: ")
--    (text . "\tab{32}  \menuitemstyle{} \tab{34} ")
--    (text . "First dimension of {\it PT}, {\it ldpt}: ")
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 1 ldq PI))
--    (text . "\tab{34} ")
--    (bcStrings (6 5 ldpt PI))
--    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Is the matrix {\it PT} required, {\it wantp}:")
    (radioButtons wantp
        (" " "  true" ptrue)
        (" " "  false" pfalse))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f02wefSolve)
  htShowPage()

f02wefSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  lda := m
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'lda)
--    objValUnwrap htpLabelSpadValue(htPage, 'lda)
  ldb := m
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ldb)
--    objValUnwrap htpLabelSpadValue(htPage, 'ldb)
  ncolb :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ncolb)
    objValUnwrap htpLabelSpadValue(htPage, 'ncolb)
  operation := htpButtonValue(htPage,'wantq)
  wantq :=
    operation = 'qtrue => '"true"
    '"false"
  ldq := m
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ldq)
--    objValUnwrap htpLabelSpadValue(htPage, 'ldq)
  ldpt := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ldpt)
--    objValUnwrap htpLabelSpadValue(htPage, 'ldpt)
  elements := htpButtonValue(htPage,'wantp)
  wantp :=
    elements = 'ptrue => '"true"
    '"false"
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  ((m = '5 and n = '3) and ncolb = '1)  => 
        f02wefDefaultSolve(htPage,lda,ldb,wantq,ldq,ldpt,wantp,ifail)
  matList :=
    "append"/[fa(i,n) for i in 1..lda] where fa(i,n) ==
       labelList := 
         "append"/[ga(i,j) for j in 1..n] where ga(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[10, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  bList :=
    "append"/[fb(i,ncolb) for i in 1..ldb] where fb(i,ncolb) ==
       pre := ("\newline \tab{2} ")
       labelList :=
         "append"/[gb(i,j) for j in 1..ncolb] where gb(i,j) ==
            bnam := INTERN STRCONC ('"b",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[6, "0.0", bnam, 'F]]]
       labelList := [['text,:pre],:labelList]
  prefix := ("\blankline \menuitemstyle{}\tab{2} Enter values of {\it B}: ")
  bList := [['text,:prefix],:bList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :matList,:bList]
  page := htInitPage('"F02WEF - SVD of real matrix",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f02wefGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
--  htpSetProperty(page,'lda,lda)
--  htpSetProperty(page,'ldb,ldb)
  htpSetProperty(page,'ncolb,ncolb)
  htpSetProperty(page,'wantq,wantq)
--  htpSetProperty(page,'ldq,ldq)
--  htpSetProperty(page,'ldpt,ldpt)
  htpSetProperty(page,'wantp,wantp)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02wefDefaultSolve(htPage,lda,ldb,wantq,ldq,ldpt,wantp,ifail) ==
  n := '3
  m := '5
  ncolb := '1
  page := htInitPage('"F02WEF - SVD of real matrix",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "2.0" a11 F))
    (bcStrings (6 "2.5" a12 F))
    (bcStrings (6 "2.5" a13 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "2.0" a21 F))
    (bcStrings (6 "2.5" a22 F))
    (bcStrings (6 "2.5" a23 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.6" a31 F))
    (bcStrings (6 "-0.4" a32 F))
    (bcStrings (6 "2.8" a33 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "2.0" a41 F))
    (bcStrings (6 "-0.5" a42 F))
    (bcStrings (6 "0.5" a43 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.2" a51 F))
    (bcStrings (6 "-0.3" a52 F))
    (bcStrings (6 "-2.9" a53 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it B}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.1" b11 F))
    (bcStrings (6 "0.9" b12 F))
    (bcStrings (6 "0.6" b13 F))
    (bcStrings (6 "0.0" b14 F))
    (bcStrings (6 "-0.8" b15 F))
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'f02wefGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'lda,lda)
  htpSetProperty(page,'ldb,ldb)
  htpSetProperty(page,'ncolb,ncolb)
  htpSetProperty(page,'wantq,wantq)
  htpSetProperty(page,'ldq,ldq)
  htpSetProperty(page,'ldpt,ldpt)
  htpSetProperty(page,'wantp,wantp)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02wefGen htPage ==
  n := htpProperty(htPage,'n)
  m := htpProperty(htPage,'m)
  lda := htpProperty(htPage,'lda)
  ldb := htpProperty(htPage,'ldb)
  ncolb := htpProperty(htPage,'ncolb)
  wantq := htpProperty(htPage,'wantq)
  ldq := htpProperty(htPage,'ldq)
  ldpt := htpProperty(htPage,'ldpt)
  wantp := htpProperty(htPage,'wantp)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  y := REVERSE y
  for i in 1..lda repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    matform := [:matform,rowList]
    rowList := []
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  for i in 1..ldb repeat
    for j in 1..ncolb repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    bform := [:bform,rowList]
    rowList := []
  bstring := bcwords2liststring [bcwords2liststring x for x in bform]
  prefix := STRCONC('"f02wef(",STRINGIMAGE m,",",STRINGIMAGE n,",")
  prefix := STRCONC(prefix,STRINGIMAGE lda,", ",STRINGIMAGE ncolb,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ldb,", ",wantq,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ldq,", ",wantp,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ldpt,", ",matstring,", ",bstring," ,")
  linkGen STRCONC(prefix,STRINGIMAGE ifail,")")

f02xef() ==
  htInitPage('"F02XEF - SVD of complex matrix",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float))
         (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXf02xef} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|f02xef| '|NagEigenPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Returns all or part of the singular value decomposition of a ")
    (text . "complex {\it m} by {\it n} matrix {\it A}.")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline Rows of matrix A, {\it m}: ")
    (text . "\tab{32}  \menuitemstyle{} \tab{34} ")
    (text . "Columns of matrix A, {\it n}: \newline \tab{2} ")
    (bcStrings (6 5 m PI))
    (text . "\tab{34} ")
    (bcStrings (6 3 n PI))
    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of A, {\it lda}: ")
--    (text . "\tab{32}  \menuitemstyle{} \tab{34} ")
--    (text . "First dimension of B, {\it ldb}: ")
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 5 lda PI))
--    (text . "\tab{34} ")
--    (bcStrings (6 5 ldb PI))
--    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Number of columns of matrix B, {\it ncolb}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 1 ncolb PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Is the matrix {\it Q} required, {\it wantq}:")
    (radioButtons wantq
        (" " "  true" qtrue)
        (" " "  false" qfalse))
    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{} \tab{2} ")
--    (text . "\newline First dimension of {\it Q}, {\it ldq}: ")
--    (text . "\tab{32}  \menuitemstyle{} \tab{34} ")
--    (text . "First dimension of {\it PH}, {\it ldph}: ")
--    (text . "\newline \tab{2} ")
--    (bcStrings (6 5 ldq PI))
--    (text . "\tab{34} ")
--    (bcStrings (6 3 ldph PI))
--    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Is the matrix {\it PH} required, {\it wantp}:")
    (radioButtons wantp
        (" " "  true" ptrue)
        (" " "  false" pfalse))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'f02xefSolve)
  htShowPage()

f02xefSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  lda := m
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'lda)
--    objValUnwrap htpLabelSpadValue(htPage, 'lda)
  ldb := m
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ldb)
--    objValUnwrap htpLabelSpadValue(htPage, 'ldb)
  ncolb :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ncolb)
    objValUnwrap htpLabelSpadValue(htPage, 'ncolb)
  operation := htpButtonValue(htPage,'wantq)
  wantq :=
    operation = 'qtrue => '"true"
    '"false"
  ldq := m
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ldq)
--    objValUnwrap htpLabelSpadValue(htPage, 'ldq)
  ldph := n
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ldph)
--    objValUnwrap htpLabelSpadValue(htPage, 'ldph)
  elements := htpButtonValue(htPage,'wantp)
  wantp :=
    elements = 'ptrue => '"true"
    '"false"
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  ((m = '5 and n = '3) and ncolb = '1)  => 
        f02xefDefaultSolve(htPage,lda,ldb,wantq,ldq,ldph,wantp,ifail)
  matList :=
    "append"/[fa(i,n) for i in 1..lda] where fa(i,n) ==
       labelList := 
         "append"/[ga(i,j) for j in 1..n] where ga(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[15, "0.0", anam, 'F]]]
       prefix := ('"\newline \tab{2} ")
       labelList := [['text,:prefix],:labelList]
  bList :=
    "append"/[fb(i,ncolb) for i in 1..ldb] where fb(i,ncolb) ==
       pre := ("\newline \tab{2} ")
       labelList :=
         "append"/[gb(i,j) for j in 1..ncolb] where gb(i,j) ==
            bnam := INTERN STRCONC ('"b",STRINGIMAGE i, STRINGIMAGE j)
            [['bcStrings,[15, "0.0", bnam, 'F]]]
       labelList := [['text,:pre],:labelList]
  prefix := ("\blankline \menuitemstyle{}\tab{2} Enter values of {\it B}: ")
  bList := [['text,:prefix],:bList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :matList,:bList]
  page := htInitPage('"F02XEF - SVD of complex matrix",nil)
  htSay '"\newline \menuitemstyle{}\tab{2} Enter values of {\it A}: "
  htSay '"\newline \tab{2} "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'f02xefGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'lda,lda)
  htpSetProperty(page,'ldb,ldb)
  htpSetProperty(page,'ncolb,ncolb)
  htpSetProperty(page,'wantq,wantq)
  htpSetProperty(page,'ldq,ldq)
  htpSetProperty(page,'ldph,ldph)
  htpSetProperty(page,'wantp,wantp)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02xefDefaultSolve(htPage,lda,ldb,wantq,ldq,ldph,wantp,ifail) ==
  n := '3
  m := '5
  ncolb := '1
  page := htInitPage('"F02XEF - SVD of complex matrix",nil)
  htMakePage '(
    (domainConditions
       (isDomain F (Float))
         (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it A}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (15 "0.5*%i" a11 F))
    (bcStrings (15 "-0.5 + 1.5*%i" a12 F))
    (bcStrings (15 "-1 + 1*%i" a13 F))
    (text . "\newline \tab{2} ")
    (bcStrings (15 "0.4 + 0.3*%i" a21 F))
    (bcStrings (15 "0.9 + 1.3*%i" a22 F))
    (bcStrings (15 "0.2 + 1.4*%i" a23 F))
    (text . "\newline \tab{2} ")
    (bcStrings (15 "0.4" a31 F))
    (bcStrings (15 "-0.4 + 0.4*%i" a32 F))
    (bcStrings (15 "1.8" a33 F))
    (text . "\newline \tab{2} ")
    (bcStrings (15 "0.3 - 0.4*%i" a41 F))
    (bcStrings (15 "0.1 + 0.7*%i" a42 F))
    (bcStrings (15 "0.0" a43 F))
    (text . "\newline \tab{2} ")
    (bcStrings (15 "-0.3*%i" a51 F))
    (bcStrings (15 "0.3 + 0.3*%i" a52 F))
    (bcStrings (15 "2.4*%i" a53 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it B}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (15 "-0.55+1.05*%i" b11 F))
    (text . "\newline \tab{2} ")
    (bcStrings (15 "0.49+0.93*%i" b12 F))
    (text . "\newline \tab{2} ")
    (bcStrings (15 "0.56-0.16*%i" b13 F))
    (text . "\newline \tab{2} ")
    (bcStrings (15 "0.39+0.23*%i" b14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (15 "1.13+0.83*%i" b15 F))
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'f02xefGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'lda,lda)
  htpSetProperty(page,'ldb,ldb)
  htpSetProperty(page,'ncolb,ncolb)
  htpSetProperty(page,'wantq,wantq)
  htpSetProperty(page,'ldq,ldq)
  htpSetProperty(page,'ldph,ldph)
  htpSetProperty(page,'wantp,wantp)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

f02xefGen htPage ==
  n := htpProperty(htPage,'n)
  m := htpProperty(htPage,'m)
  lda := htpProperty(htPage,'lda)
  ldb := htpProperty(htPage,'ldb)
  ncolb := htpProperty(htPage,'ncolb)
  wantq := htpProperty(htPage,'wantq)
  ldq := htpProperty(htPage,'ldq)
  ldph := htpProperty(htPage,'ldph)
  wantp := htpProperty(htPage,'wantp)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  y := REVERSE y
  for i in 1..lda repeat
    for j in 1..n repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    matform := [:matform,rowList]
    rowList := []
  matstring := bcwords2liststring [bcwords2liststring x for x in matform]
  for i in 1..ldb repeat
    for j in 1..ncolb repeat
      elm := STRCONC((first y).1," ")
      rowList := [:rowList,elm]
      y := rest y
    bform := [:bform,rowList]
    rowList := []
  bstring := bcwords2liststring [bcwords2liststring x for x in bform]
  prefix := STRCONC('"f02xef(",STRINGIMAGE m,",",STRINGIMAGE n,",")
  prefix := STRCONC(prefix,STRINGIMAGE lda,", ",STRINGIMAGE ncolb,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ldb,", ",wantq,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ldq,", ",wantp,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ldph,", ",matstring,", ",bstring," ,")
  linkGen STRCONC(prefix,STRINGIMAGE ifail,")")


