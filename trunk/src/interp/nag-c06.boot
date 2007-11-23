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

c06eaf() ==
  htInitPage('"C06EAF - Single 1-D real discrete Fourier transform ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXc06eaf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|c06eaf| '|NagSeriesSummationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Calculates the discrete Fourier transform of the sequence ")
    (text . "of real data values \space{1} \inputbitmap{\htbmdir{}/xj.bitmap}, for ")
    (text . "j = 0,1,...,n-1. ")
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number of data values: ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 7 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'c06eafSolve)
  htShowPage()

c06eafSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '7 => c06eafDefaultSolve(htPage,ifail)
  labelList :=
    "append"/[f(i) for i in 1..n] where f(i) ==
      prefix := ('"\newline \tab{15} ")
      rnam := INTERN STRCONC ('"r",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[10, 0.0, rnam, 'F]]]
  equationPart := [ 
     '(domainConditions 
        (isDomain F (Float))),
            :labelList]
  page := htInitPage("C06EAF - Single 1-D real discrete Fourier transform ", htpPropertyList htPage)
  htSay '"\menuitemstyle{} \tab{2} "
  htSay '"Enter the sequence to be transformed: "
  htMakePage equationPart
  htSay '"\blankline "
  htSay '"Note : On exit, the transformed sequence is stored "
  htSay '"in Hermitian form "
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'c06eafGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()  


c06eafDefaultSolve (htPage, ifail) ==
  n := '7
  page := htInitPage('"C06EAF - Single 1-D real discrete Fourier transform ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the sequence to be transformed: ")
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.34907" r1 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.54890" r2 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.74776" r3 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.94459" r4 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "1.13850" r5 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "1.32850" r6 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "1.51370" r7 F))
    (text . "\blankline ")
    (text . "Note : On exit, the transformed sequence is stored ")
    (text . "in Hermitian form ")
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'c06eafGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


c06eafGen htPage ==
  n := htpProperty(htPage,'n)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  while y repeat
    left := STRCONC((first y).1," ")
    y := rest y
    reallist := [left,:reallist]
  realstring := bcwords2liststring reallist
  linkGen STRCONC ('"c06eaf(",STRINGIMAGE n,",[",realstring,"],",STRINGIMAGE ifail,")")

c06ebf() ==
  htInitPage('"C06EBF - Single 1-D Hermitian discrete Fourier transform ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXc06ebf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|c06ebf| '|NagSeriesSummationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Calculates the discrete Fourier transform of a Hermitian ")
    (text . "sequence of complex data values. ")
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number of data values: ")
    (text . "\newline \tab{2}")
    (bcStrings (5 7 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'c06ebfSolve)
  htShowPage()

c06ebfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '7 => c06ebfDefaultSolve(htPage,ifail)
  labelList :=
    "append"/[f(i) for i in 1..n] where f(i) ==
      prefix := ('"\newline \tab{15} ")
      rnam := INTERN STRCONC ('"r",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[10, 0.0, rnam, 'F]]]
  equationPart := [ 
     '(domainConditions 
        (isDomain F (Float))),
            :labelList]
  page := htInitPage("C06EBF - Single 1-D real discrete Fourier transform ", htpPropertyList htPage)
  htSay '"\menuitemstyle{} \tab{2} "
  htSay '"Enter the sequence to be transformed, stored in Hermitian form: "
  htMakePage equationPart
  htSay '"\blankline "
  htSay '"Note : On exit, the components of the discrete Fourier transform "
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'c06ebfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()  


c06ebfDefaultSolve (htPage, ifail) ==
  n := '7
  page := htInitPage('"C06EBF - Single 1-D Hermitian discrete Fourier transform ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the sequence to be transformed, stored in Hermitian form: ")
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.34907" r1 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.54890" r2 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.74776" r3 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.94459" r4 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "1.13850" r5 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "1.32850" r6 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "1.51370" r7 F))
    (text . "\blankline ")
    (text . "Note : On exit, the components of the discrete Fourier transform")
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'c06ebfGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


c06ebfGen htPage ==
  n := htpProperty(htPage,'n)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  while y repeat
    left := STRCONC((first y).1," ")
    y := rest y
    reallist := [left,:reallist]
  realstring := bcwords2liststring reallist
  linkGen STRCONC ('"c06ebf(",STRINGIMAGE n,",[",realstring,"],",STRINGIMAGE ifail,")")


c06ecf() ==
  htInitPage('"C06ECF - Single 1-D complex discrete Fourier transform ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXc06ecf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|c06ecf| '|NagSeriesSummationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Calculates the discrete Fourier transform of a complex sequence.")
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number of data values: ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 7 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'c06ecfSolve)
  htShowPage()


c06ecfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '7 => c06ecfDefaultSolve(htPage,ifail)
  labelList :=
    "append"/[f(i) for i in 1..n] where f(i) ==
      prefix := ('"\newline \tab{2} ")
      post   := ('"\tab{32} ")
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      ynam := INTERN STRCONC ('"y",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[10, 0.0, xnam, 'F]], 
       ['text,:post],['bcStrings,[10, 0.0, ynam, 'F]]]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList]
  page := htInitPage("C06ECF - Single 1-D complex discrete Fourier transform ",htpPropertyList htPage)
  htSay '"\menuitemstyle{}\tab{2} Real parts of sequence: \tab{30} "
  htSay '"\menuitemstyle{}\tab{32} Imaginary parts: "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'c06ecfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

c06ecfDefaultSolve (htPage, ifail) ==
  n := '7
  page := htInitPage('"C06ECF - Single 1-D complex discrete Fourier transform ",htpPropertyList htPage)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Real parts of sequence: \tab{30} ")
    (text . "\menuitemstyle{}\tab{32} Imaginary parts: ")
    (text . "\newline \tab{2}")
    (bcStrings (10 "0.34907" x1 F))
    (text . "\tab{32} ")
    (bcStrings (10 "-0.37168" y1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.54890" x2 F))
    (text . "\tab{32} ")
    (bcStrings (10 "-0.35669" y2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.74776" x3 F))
    (text . "\tab{32} ")
    (bcStrings (10 "-0.31175" y3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.94459" x4 F))
    (text . "\tab{32} ")
    (bcStrings (10 "-0.23702" y4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.13850" x5 F))
    (text . "\tab{32} ")
    (bcStrings (10 "-0.13274" y5 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.32850" x6 F))
    (text . "\tab{32} ")
    (bcStrings (10 "0.00074" y6 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.51370" x7 F))
    (text . "\tab{32} ")
    (bcStrings (10 "0.16298" y7 F))
    (text . "\blankline"))
  htMakeDoneButton('"Continue",'c06ecfGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


c06ecfGen htPage ==
  n := htpProperty(htPage,'n)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y
    left :=  STRCONC ((first y).1," ")
    y := rest y
    reallist := [left,:reallist]
    imaglist := [right,:imaglist]
  realstring := bcwords2liststring reallist
  imagstring := bcwords2liststring imaglist
  linkGen STRCONC ('"c06ecf(",STRINGIMAGE n,",[",realstring,"],[",imagstring,"],", STRINGIMAGE ifail,")")


c06ekf() ==
  htInitPage('"C06EKF - Circular convolution or correlation of two real vectors",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXc06ekf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|c06ekf| '|NagSeriesSummationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Calculates the circular convolution or correlation of two real ")
    (text . "vectors of period {\em n} ")
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number of data values: ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 9 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Choose the computation to be performed:")
    (radioButtons job
        ("" "  Convolution" conv)
        ("" "  Correlation" corr))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'c06ekfSolve)
  htShowPage()


c06ekfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  type := htpButtonValue(htPage,'job)
  job :=
    type = 'conv => '1
    '2
  n = '9 => c06ekfDefaultSolve(htPage,job,ifail)
  labelList :=
    "append"/[f(i) for i in 1..n] where f(i) ==
      prefix := ('"\newline \tab{2} ")
      post   := ('"\tab{34} ")
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      ynam := INTERN STRCONC ('"y",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[10, 0.0, xnam, 'F]], 
       ['text,:post],['bcStrings,[10, 0.0, ynam, 'F]]]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList]
  page := htInitPage("C06EKF - Single 1-D complex discrete Fourier transform ",htpPropertyList htPage)
  htSay '"\menuitemstyle{}\tab{2} Elements of period of vector {\em x}: "
  htSay '"\tab{31} "
  htSay '"\menuitemstyle{}\tab{34} Elements of period of vector {\em y}:"
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'c06ekfGen)
  htpSetProperty(page,'job,job)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

c06ekfDefaultSolve (htPage, job, ifail) ==
  n := '9
  page := htInitPage('"C06EKF - Circular convolution or correlation of two real vectors ",htpPropertyList htPage)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Elements of period of vector {\em x}: \tab{32} ")
    (text . "\menuitemstyle{}\tab{34} Elements of period of vector {\em y}: ")
    (text . "\newline \tab{2}")
    (bcStrings (10 "1.00" x1 F))
    (text . "\tab{34} ")
    (bcStrings (10 "0.50" y1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.00" x2 F))
    (text . "\tab{34} ")
    (bcStrings (10 "0.50" y2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.00" x3 F))
    (text . "\tab{34} ")
    (bcStrings (10 "0.50" y3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.00" x4 F))
    (text . "\tab{34} ")
    (bcStrings (10 "0.50" y4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.00" x5 F))
    (text . "\tab{34} ")
    (bcStrings (10 "0.00" y5 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.00" x6 F))
    (text . "\tab{34} ")
    (bcStrings (10 "0.00" y6 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.00" x7 F))
    (text . "\tab{34} ")
    (bcStrings (10 "0.00" y7 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.00" x8 F))
    (text . "\tab{34} ")
    (bcStrings (10 "0.00" y8 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.00" x9 F))
    (text . "\tab{34} ")
    (bcStrings (10 "0.00" y9 F))
    (text . "\blankline"))
  htMakeDoneButton('"Continue",'c06ekfGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'job,job)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


c06ekfGen htPage ==
  n := htpProperty(htPage,'n)
  ifail := htpProperty(htPage,'ifail)
  job := htpProperty(htPage,'job)
  alist := htpInputAreaAlist htPage
  y := alist
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y
    left :=  STRCONC ((first y).1," ")
    y := rest y
    reallist := [left,:reallist]
    imaglist := [right,:imaglist]
  realstring := bcwords2liststring reallist
  imagstring := bcwords2liststring imaglist
  linkGen STRCONC ('"c06ekf(",STRINGIMAGE job,",",STRINGIMAGE n,",[",realstring,"],[",imagstring,"],", STRINGIMAGE ifail,")")

c06fpf() ==
  htInitPage('"C06FPF - Multiple 1-D real discrete Fourier transform ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXc06fpf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|c06fpf| '|NagSeriesSummationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Computes the discrete Fourier transforms of {\it m} real ")
    (text . "sequences, each containing {\it n} data values.")
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number of sequences to be transformed {\it m}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (5 3 m PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number of data values {\it n}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 6 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Type of call:")
    (radioButtons init
        ("" "  Initial" i)
        ("" "  Subsequent" s)
        ("" "  Restart" r))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'c06fpfSolve)
  htShowPage()

c06fpfSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  call := htpButtonValue(htPage,'init)
  init :=
    call = 'i => '"i"
    call = 's => '"s"
    '"r"
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (n = '6 and m = '3) => c06fpfDefaultSolve(htPage,init,ifail)
  matList :=
    "append"/[f(i,m) for i in 1..n] where f(i,n) ==
      labelList :=
        "append"/[g(i,j) for j in 1..n] where g(i,j) ==
          xnam := INTERN STRCONC ('"r",STRINGIMAGE i, STRINGIMAGE j)
          [['bcStrings,[6, 0.0, xnam, 'F]]]
      prefix := ('"\newline \tab{2} ")
      labelList := [['text,:prefix],:labelList]
  trigList := 
    "append"/[h(k) for k in 1..(2*n)] where h(k) ==
      prefix := ('"\newline \tab{2} ")
      trignam := INTERN STRCONC ('"t",STRINGIMAGE k)
      [['text,:prefix],['bcStrings,[6, "0.0", trignam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} Trigonometric coefficients ")
  prefix := STRCONC(prefix,"(not required if initial call) {\it TRIG}: ")
  prefix := STRCONC(prefix,"\newline \tab{2} ")
  trigList := [['text,:prefix],:trigList]
  equationPart := [ 
     '(domainConditions 
        (isDomain F (Float))),
            :matList,:trigList]
  page := htInitPage("C06FPF - Multiple 1-D real discrete Fourier transform ", htpPropertyList htPage)
  htSay '"\menuitemstyle{} \tab{2} "
  htSay '"Enter each sequence to be transformed, {\it x}. "
  htSay '"(Each column to contain a sequence.) "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'c06fpfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'init,init)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()  


c06fpfDefaultSolve (htPage, init,ifail) ==
  n := '6
  m := '3
  page := htInitPage('"C06FPF - Multiple 1-D real discrete Fourier transform ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter each sequence to be transformed, {\it x}. ")
    (text . "(Each column to contain a sequence.) ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.3854" x11 F))
    (bcStrings (6 "0.5417" x21 F))
    (bcStrings (6 "0.9172" x31 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.6772" x12 F))
    (bcStrings (6 "0.2983" x22 F))
    (bcStrings (6 "0.0644" x32 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.1138" x13 F))
    (bcStrings (6 "0.1181" x23 F))
    (bcStrings (6 "0.6037" x33 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.6751" x14 F))
    (bcStrings (6 "0.7255" x24 F))
    (bcStrings (6 "0.6430" x34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.6362" x15 F))
    (bcStrings (6 "0.8638" x25 F))
    (bcStrings (6 "0.0428" x35 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.1424" x16 F))
    (bcStrings (6 "0.8723" x26 F))
    (bcStrings (6 "0.4815" x36 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Trignometric coefficients (not required if initial call) ")
    (text . "{\it TRIG}: \newline \tab{2} ")
    (bcStrings (6 "0.0" t1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t5 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t6 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t7 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t8 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t9 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t10 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t11 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t12 F))
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'c06fpfGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'init,init)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


c06fpfGen htPage ==
  n := htpProperty(htPage,'n)
  m := htpProperty(htPage,'m)
  init := htpProperty(htPage,'init)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..(2*n) repeat
    left := STRCONC((first y).1," ")
    y := rest y
    triglist := [left,:triglist]
  trigstring := bcwords2liststring triglist
  while y repeat
    left := STRCONC((first y).1," ")
    y := rest y
    xlist := [left,:xlist]
  xstring := bcwords2liststring xlist
  prefix := STRCONC ('"c06fpf(",STRINGIMAGE m,", ",STRINGIMAGE n,", _"")
  prefix := STRCONC(prefix,init,"_",[",xstring,"],[",trigstring,"],")
  linkGen STRCONC(prefix,STRINGIMAGE ifail,")")


c06fqf() ==
  htInitPage('"C06FQF - Multiple 1-D Hermitian discrete Fourier transform ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXc06fqf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|c06fqf| '|NagSeriesSummationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Computes the discrete Fourier transforms of {\it m} real ")
    (text . "sequences, each containing {\it n} data values.")
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number of sequences to be transformed {\it m}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (5 3 m PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number of data values {\it n}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 6 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Type of call:")
    (radioButtons init
        ("" "  Initial" i)
        ("" "  Subsequent" s)
        ("" "  Restart" r))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'c06fqfSolve)
  htShowPage()

c06fqfSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  call := htpButtonValue(htPage,'init)
  init :=
    call = 'i => '"i"
    call = 's => '"s"
    '"r"
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (n = '6 and m = '3) => c06fqfDefaultSolve(htPage,init,ifail)
  matList :=
    "append"/[f(i,m) for i in 1..n] where f(i,n) ==
      labelList :=
        "append"/[g(i,j) for j in 1..n] where g(i,j) ==
          xnam := INTERN STRCONC ('"r",STRINGIMAGE i, STRINGIMAGE j)
          [['bcStrings,[6, 0.0, xnam, 'F]]]
      prefix := ('"\newline \tab{2} ")
      labelList := [['text,:prefix],:labelList]
  trigList := 
    "append"/[h(k) for k in 1..(2*n)] where h(k) ==
      prefix := ("\newline \tab{2} ")
      trignam := INTERN STRCONC ('"t",STRINGIMAGE k)
      [['text,:prefix],['bcStrings,[6, "0.0", trignam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} Trignometric coefficients ")
  prefix := STRCONC(prefix,"(not required if initial call) {\it TRIG}: ")
  prefix := STRCONC(prefix,"\newline \tab{2} ")
  trigList := [['text,:prefix],:trigList]
  equationPart := [ 
     '(domainConditions 
        (isDomain F (Float))),
            :matList,:trigList]
  page := htInitPage("C06FQF - Multiple 1-D Hermitian discrete Fourier transform ", htpPropertyList htPage)
  htSay '"\menuitemstyle{} \tab{2} "
  htSay '"Enter each sequence to be transformed, {\it x}. "
  htSay '"(Each column to contain a sequence.) "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'c06fqfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'init,init)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()  


c06fqfDefaultSolve (htPage, init,ifail) ==
  n := '6
  m := '3
  page := htInitPage('"C06FQF - Multiple 1-D Hermitian discrete Fourier transform ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter each sequence to be transformed, {\it x}. ")
    (text . "(Each column to contain a sequence.) ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.3854" x11 F))
    (bcStrings (6 "0.5417" x21 F))
    (bcStrings (6 "0.9172" x31 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.6772" x12 F))
    (bcStrings (6 "0.2983" x22 F))
    (bcStrings (6 "0.0644" x32 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.1138" x13 F))
    (bcStrings (6 "0.1181" x23 F))
    (bcStrings (6 "0.6037" x33 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.6751" x14 F))
    (bcStrings (6 "0.7255" x24 F))
    (bcStrings (6 "0.6430" x34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.6362" x15 F))
    (bcStrings (6 "0.8638" x25 F))
    (bcStrings (6 "0.0428" x35 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.1424" x16 F))
    (bcStrings (6 "0.8723" x26 F))
    (bcStrings (6 "0.4815" x36 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Trignometric coefficients (not required if initial call) ")
    (text . "{\it TRIG}: \newline \tab{2} ")
    (bcStrings (6 "0.0" t1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t5 F))
    (text . "\newline \tab{2} ") 
    (bcStrings (6 "0.0" t6 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t7 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t8 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t9 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t10 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t11 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t12 F))
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'c06fqfGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'init,init)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


c06fqfGen htPage ==
  n := htpProperty(htPage,'n)
  m := htpProperty(htPage,'m)
  init := htpProperty(htPage,'init)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..(2*n) repeat
    left := STRCONC((first y).1," ")
    y := rest y
    triglist := [left,:triglist]
  trigstring := bcwords2liststring triglist
  while y repeat
    left := STRCONC((first y).1," ")
    y := rest y
    xlist := [left,:xlist]
  xstring := bcwords2liststring xlist
  prefix := STRCONC ('"c06fqf(",STRINGIMAGE m,", ",STRINGIMAGE n,", _"")
  prefix := STRCONC(prefix,init,"_",[",xstring,"],[",trigstring,"],")
  linkGen STRCONC(prefix,STRINGIMAGE ifail,")")


c06frf() ==
  htInitPage('"C06FRF - Multiple 1-D complex discrete Fourier transform ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXc06frf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|c06frf| '|NagSeriesSummationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Computes the discrete Fourier transforms of {\it m} complex ")
    (text . "sequences, each containing {\it n} data values.")
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number of sequences to be transformed {\it m}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (5 3 m PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number of data values {\it n}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 6 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Type of call:")
    (radioButtons init
        ("" "  Initial" i)
        ("" "  Subsequent" s)
        ("" "  Restart" r))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'c06frfSolve)
  htShowPage()

c06frfSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  call := htpButtonValue(htPage,'init)
  init :=
    call = 'i => '"i"
    call = 's => '"s"
    '"r"
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (n = '6 and m = '3) => c06frfDefaultSolve(htPage,init,ifail)
  xList :=
    "append"/[fx(i,m) for i in 1..n] where fx(i,n) ==
      labelList :=
        "append"/[gx(i,j) for j in 1..n] where gx(i,j) ==
          xnam := INTERN STRCONC ('"x",STRINGIMAGE i, STRINGIMAGE j)
          [['bcStrings,[6, 0.0, xnam, 'F]]]
      prefix := ('"\newline \tab{2} ")
      labelList := [['text,:prefix],:labelList]
  yList :=
    "append"/[fy(i,m) for i in 1..n] where fy(i,n) ==
      labelList :=
        "append"/[gy(i,j) for j in 1..n] where gy(i,j) ==
          ynam := INTERN STRCONC ('"y",STRINGIMAGE i, STRINGIMAGE j)
          [['bcStrings,[6, 0.0, ynam, 'F]]]
      prefix := ('"\newline \tab{2} ")
      labelList := [['text,:prefix],:labelList]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} Enter the imaginary parts ")
  prefix := STRCONC(prefix,"of each sequence to be transformed, {\it y}. ")
  prefix := STRCONC(prefix,"(Each column to contain the imaginary parts ")
  prefix := STRCONC(prefix,"of a sequence.) \newline \tab{2} ")
  yList := [['text,:prefix],:yList]
  trigList := 
    "append"/[h(k) for k in 1..(2*n)] where h(k) ==
      prefix := ("\newline \tab{2} ")
      trignam := INTERN STRCONC ('"t",STRINGIMAGE k)
      [['text,:prefix],['bcStrings,[6, "0.0", trignam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} Trignometric coefficients ")
  prefix := STRCONC(prefix,"(not required if initial call) {\it TRIG}: ")
  prefix := STRCONC(prefix,"\newline \tab{2} ")
  trigList := [['text,:prefix],:trigList]
  equationPart := [ 
     '(domainConditions 
        (isDomain F (Float))),
            :xList,:yList,:trigList]
  page := htInitPage("C06FRF - Multiple 1-D real discrete Fourier transform ", htpPropertyList htPage)
  htSay '"\menuitemstyle{} \tab{2} "
  htSay '"Enter the real parts of each sequence to be transformed, {\it x}. "
  htSay '"(Each column to contain the real parts of a sequence.) "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'c06frfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'init,init)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()  


c06frfDefaultSolve (htPage, init,ifail) ==
  n := '6
  m := '3
  page := htInitPage('"C06FRF - Multiple 1-D real discrete Fourier transform ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the real parts of each sequence to be transformed, ")
    (text . "{\it x}. (Each column to contain the real parts of a sequence.) ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.3854" x11 F))
    (bcStrings (6 "0.9172" x21 F))
    (bcStrings (6 "0.1156" x31 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.6772" x12 F))
    (bcStrings (6 "0.0644" x22 F))
    (bcStrings (6 "0.0685" x32 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.1138" x13 F))
    (bcStrings (6 "0.6037" x23 F))
    (bcStrings (6 "0.2060" x33 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.6751" x14 F))
    (bcStrings (6 "0.6430" x24 F))
    (bcStrings (6 "0.8630" x34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.6362" x15 F))
    (bcStrings (6 "0.0428" x25 F))
    (bcStrings (6 "0.6967" x35 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.1424" x16 F))
    (bcStrings (6 "0.4815" x26 F))
    (bcStrings (6 "0.2792" x36 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter the imaginary parts of each ")
    (text . "sequence to be transformed, {\it y}. ")
    (text . "(Each column to contain the imaginary parts of a sequence.) ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.5417" y11 F))
    (bcStrings (6 "0.9089" y21 F))
    (bcStrings (6 "0.6214" y31 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.2983" y12 F))
    (bcStrings (6 "0.3118" y22 F))
    (bcStrings (6 "0.8681" y32 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.1181" y13 F))
    (bcStrings (6 "0.3465" y23 F))
    (bcStrings (6 "0.7060" y33 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.7255" y14 F))
    (bcStrings (6 "0.6198" y24 F))
    (bcStrings (6 "0.8652" y34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.8638" y15 F))
    (bcStrings (6 "0.2668" y25 F))
    (bcStrings (6 "0.9190" y35 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.8723" y16 F))
    (bcStrings (6 "0.1614" y26 F))
    (bcStrings (6 "0.3355" y36 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Trignometric coefficients (not required if initial call) ")
    (text . "{\it TRIG}: \newline \tab{2} ")
    (bcStrings (6 "0.0" t1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t5 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t6 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t7 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t8 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t9 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t10 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t11 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" t12 F))
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'c06frfGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'init,init)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


c06frfGen htPage ==
  n := htpProperty(htPage,'n)
  m := htpProperty(htPage,'m)
  init := htpProperty(htPage,'init)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..(2*n) repeat
    left := STRCONC((first y).1," ")
    y := rest y
    triglist := [left,:triglist]
  trigstring := bcwords2liststring triglist
  for i in 1..(m*n) repeat
    left := STRCONC((first y).1," ")
    y := rest y
    ylist := [left,:ylist]
  ystring := bcwords2liststring ylist
  while y repeat
    left := STRCONC((first y).1," ")
    y := rest y
    xlist := [left,:xlist]
  xstring := bcwords2liststring xlist
  prefix := STRCONC ('"c06frf(",STRINGIMAGE m,", ",STRINGIMAGE n,", _"")
  prefix := STRCONC(prefix,init,"_",[",xstring,"],[",ystring,"],[")
  linkGen STRCONC(prefix,trigstring,"],",STRINGIMAGE ifail,")")


c06fuf() ==
  htInitPage('"C06FUF - 2-D complex discrete Fourier transform ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXc06fuf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|c06fuf| '|NagSeriesSummationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Computes the two-dimensional discrete Fourier transform of ")
    (text . "a bivaraite sequence of complex data values; likely to be ")
    (text . "efficient on vector processors. ")
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number of {\it m} of rows of X and Y; ")
    (text . "\htbitmap{great=} 1 \newline \tab{2} ")
    (bcStrings (5 3 m PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number of {\it n} of columns of X and Y; ")
    (text . "\htbitmap{great=} 1 \newline \tab{2} ")
    (bcStrings (5 5 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Type of call:")
    (radioButtons init
        ("" "  Initial" i)
        ("" "  Subsequent" s)
        ("" "  Restart" r))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'c06fufSolve)
  htShowPage()

c06fufSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  call := htpButtonValue(htPage,'init)
  init :=
    call = 'i => '"i"
    call = 's => '"s"
    '"r"
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (n = '5 and m = '3) => c06fufDefaultSolve(htPage,init,ifail)
  xList :=
    "append"/[fx(i,m) for i in 1..n] where fx(i,n) ==
      labelList :=
        "append"/[gx(i,j) for j in 1..n] where gx(i,j) ==
          xnam := INTERN STRCONC ('"x",STRINGIMAGE i, STRINGIMAGE j)
          [['bcStrings,[6, 0.0, xnam, 'F]]]
      prefix := ('"\newline \tab{2} ")
      labelList := [['text,:prefix],:labelList]
  yList :=
    "append"/[fy(i,m) for i in 1..n] where fy(i,n) ==
      labelList :=
        "append"/[gy(i,j) for j in 1..n] where gy(i,j) ==
          ynam := INTERN STRCONC ('"y",STRINGIMAGE i, STRINGIMAGE j)
          [['bcStrings,[6, 0.0, ynam, 'F]]]
      prefix := ('"\newline \tab{2} ")
      labelList := [['text,:prefix],:labelList]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} Enter the imaginary parts ")
  prefix := STRCONC(prefix,"of each sequence to be transformed, {\it y}. ")
  prefix := STRCONC(prefix,"(Each column to contain the imaginary parts ")
  prefix := STRCONC(prefix,"of a sequence.) \newline \tab{2} ")
  yList := [['text,:prefix],:yList]
  trigmList := 
    "append"/[hm(k) for k in 1..(2*m)] where hm(k) ==
      prefix := ("\newline \tab{2} ")
      trignam := INTERN STRCONC ('"tm",STRINGIMAGE k)
      [['text,:prefix],['bcStrings,[6, "0.0", trignam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} Trignometric coefficients ")
  prefix := STRCONC(prefix,"(not required if initial call) {\it TRIGM}: ")
  prefix := STRCONC(prefix,"\newline \tab{2} ")
  trigmList := [['text,:prefix],:trigmList]
  trignList := 
    "append"/[hn(k) for k in 1..(2*n)] where hn(k) ==
      prefix := ("\newline \tab{2} ")
      trignam := INTERN STRCONC ('"tn",STRINGIMAGE k)
      [['text,:prefix],['bcStrings,[6, "0.0", trignam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} {\it TRIGN}: ")
  prefix := STRCONC(prefix,"\newline \tab{2} ")
  trignList := [['text,:prefix],:trignList]
  equationPart := [ 
     '(domainConditions 
        (isDomain F (Float))),
            :xList,:yList,:trigmList,:trignList]
  page := htInitPage("C06FUF - 2-D complex discrete Fourier transform ", htpPropertyList htPage)
  htSay '"\menuitemstyle{} \tab{2} "
  htSay '"Enter the real part of each sequence to be transformed, {\it x}. "
  htSay '"(Each column to contain the real parts of a sequence.) "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'c06fufGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'init,init)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()  


c06fufDefaultSolve (htPage, init,ifail) ==
  n := '5
  m := '3
  page := htInitPage('"C06FUF - 2-D real discrete Fourier transform ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the real parts of each sequence to be transformed, ")
    (text . "{\it x}. (Each column to contain the real parts of a sequence.) ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "1.000" x11 F))
    (bcStrings (6 "0.994" x21 F))
    (bcStrings (6 "0.903" x31 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.999" x12 F))
    (bcStrings (6 "0.989" x22 F))
    (bcStrings (6 "0.885" x32 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.987" x13 F))
    (bcStrings (6 "0.963" x23 F))
    (bcStrings (6 "0.823" x33 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.936" x14 F))
    (bcStrings (6 "0.891" x24 F))
    (bcStrings (6 "0.694" x34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.802" x15 F))
    (bcStrings (6 "0.731" x25 F))
    (bcStrings (6 "0.467" x35 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter the imaginary parts of each ")
    (text . "sequence to be transformed, {\it y}. (Each column to contain ")
    (text . "the imaginary parts of a sequence.) ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.000" y11 F))
    (bcStrings (6 "-0.111" y21 F))
    (bcStrings (6 "-0.430" y31 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-0.040" y12 F))
    (bcStrings (6 "-0.151" y22 F))
    (bcStrings (6 "-0.466" y32 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-0.159" y13 F))
    (bcStrings (6 "-0.268" y23 F))
    (bcStrings (6 "-0.568" y33 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-0.352" y14 F))
    (bcStrings (6 "-0.454" y24 F))
    (bcStrings (6 "-0.720" y34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "-0.597" y15 F))
    (bcStrings (6 "-0.682" y25 F))
    (bcStrings (6 "-0.884" y35 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Trignometric coefficients (not required if initial call) ")
    (text . "{\it TRIGM}: \newline \tab{2} ")
    (bcStrings (6 "0.0" tm1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" tm2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" tm3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" tm4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" tm5 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" tm6 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "{\it TRIGN}: \newline \tab{2} ")
    (bcStrings (6 "0.0" tn1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" tn2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" tn3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" tn4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" tn5 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" tn6 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" tn7 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" tn8 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" tn9 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" tn10 F))
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'c06fufGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'init,init)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


c06fufGen htPage ==
  n := htpProperty(htPage,'n)
  m := htpProperty(htPage,'m)
  init := htpProperty(htPage,'init)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..(2*n) repeat
    left := STRCONC((first y).1," ")
    y := rest y
    trignlist := [left,:trignlist]
  trignstring := bcwords2liststring trignlist
  for i in 1..(2*m) repeat
    left := STRCONC((first y).1," ")
    y := rest y
    trigmlist := [left,:trigmlist]
  trigmstring := bcwords2liststring trigmlist
  for i in 1..(m*n) repeat
    left := STRCONC((first y).1," ")
    y := rest y
    ylist := [left,:ylist]
  ystring := bcwords2liststring ylist
  while y repeat
    left := STRCONC((first y).1," ")
    y := rest y
    xlist := [left,:xlist]
  xstring := bcwords2liststring xlist
  prefix := STRCONC ('"c06fuf(",STRINGIMAGE m,", ",STRINGIMAGE n,", _"")
  prefix := STRCONC(prefix,init,"_",[",xstring,"],[",ystring,"],[",trigmstring)
  linkGen STRCONC(prefix,"],[",trignstring,"],",STRINGIMAGE ifail,")")



c06gbf() ==
  htInitPage('"C06GBF - Complex conjugate of a Hermitian sequence ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXc06gbf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|c06gbf| '|NagSeriesSummationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Forms the complex conjugate of a Hermitian sequence of {\it n} data values")
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number of data values {\it n} ")
    (text . "\space{1} \inputbitmap{\htbmdir{}/great=.bitmap} 1 ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 7 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'c06gbfSolve)
  htShowPage()

c06gbfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '7 => c06gbfDefaultSolve(htPage,ifail)
  labelList :=
    "append"/[f(i) for i in 1..n] where f(i) ==
      prefix := ('"\newline \tab{15} ")
      rnam := INTERN STRCONC ('"r",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[10, 0.0, rnam, 'F]]]
  equationPart := [ 
     '(domainConditions 
        (isDomain F (Float))),
            :labelList]
  page := htInitPage("C06GBF - Complex conjugate of a Hermitian sequence ", htpPropertyList htPage)
  htSay '"\menuitemstyle{} \tab{2} "
  htSay '"Enter the Hermitian sequence to be transformed stored in Hermitian form: "
  htMakePage equationPart
  htSay '"\blankline "
  htSay '"Note : On exit, the imaginary values are negated "
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'c06gbfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()  


c06gbfDefaultSolve (htPage, ifail) ==
  n := '7
  page := htInitPage('"C06GBF - Complex conjugate of a Hermitian sequence ", nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the Hermitian sequence to be transformed ")
    (text . "stored in Hermitian form: ")
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.34907" r1 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.54890" r2 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.74776" r3 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.94459" r4 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "1.13850" r5 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "1.32850" r6 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "1.51370" r7 F))
    (text . "\blankline ")
    (text . "Note : On exit, the imaginary values are negated ")
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'c06gbfGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


c06gbfGen htPage ==
  n := htpProperty(htPage,'n)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  while y repeat
    left := STRCONC((first y).1," ")
    y := rest y
    reallist := [left,:reallist]
  realstring := bcwords2liststring reallist
  linkGen STRCONC ('"c06gbf(",STRINGIMAGE n,",[",realstring,"],",STRINGIMAGE ifail,")")


c06gcf() ==
  htInitPage('"C06GCF - Complex conjugate of complex sequence ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXc06gcf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|c06gcf| '|NagSeriesSummationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Forms the complex conjugate of a sequence of {\it n} data values")
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number of data values {\it n} ")
    (text . "\space{1} \inputbitmap{\htbmdir{}/great=.bitmap} 1 ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 7 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'c06gcfSolve)
  htShowPage()

c06gcfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '7 => c06gcfDefaultSolve(htPage,ifail)
  labelList :=
    "append"/[f(i) for i in 1..n] where f(i) ==
      prefix := ('"\newline \tab{15} ")
      rnam := INTERN STRCONC ('"r",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[10, 0.0, rnam, 'F]]]
  equationPart := [ 
     '(domainConditions 
        (isDomain F (Float))),
            :labelList]
  page := htInitPage("C06GCF - Complex conjugate of a Hermitian sequence ", htpPropertyList htPage)
  htSay '"\menuitemstyle{} \tab{2} "
  htSay '"Enter the imaginary parts of the sequence: "
  htMakePage equationPart
  htSay '"\blankline "
  htSay '"Note : On exit, these values are negated "
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'c06gcfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()  


c06gcfDefaultSolve (htPage, ifail) ==
  n := '7
  page := htInitPage('"C06GCF - Complex conjugate of complex sequence ", nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the imaginary parts of the sequence: ")
    (text . "\newline \tab{15} ")
    (bcStrings (10 "-0.37168" r1 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "-0.35669" r2 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "-0.31175" r3 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "-0.23702" r4 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.00074" r5 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.16298" r6 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "1.51370" r7 F))
    (text . "\blankline ")
    (text . "Note : On exit, these values are negated ")
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'c06gcfGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


c06gcfGen htPage ==
  n := htpProperty(htPage,'n)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  while y repeat
    left := STRCONC((first y).1," ")
    y := rest y
    reallist := [left,:reallist]
  realstring := bcwords2liststring reallist
  linkGen STRCONC ('"c06gcf(",STRINGIMAGE n,",[",realstring,"],",STRINGIMAGE ifail,")")

c06gqf() ==
  htInitPage('"C06GQF - Complex conjugate of multiple Hermitian sequences ",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXc06gqf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|c06gqf| '|NagSeriesSummationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Forms the complex conjugates of {\it m} Hermitian sequences, ")
    (text . "each containing {\it n} data values. ")
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number {\it m} \inputbitmap{\htbmdir{}/great=.bitmap} 1 ")
    (text . "of sequences to be tranformed: ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 3 m PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number {\it n} \inputbitmap{\htbmdir{}/great=.bitmap} 1 ")
    (text . "of data values in each sequence: ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 6 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'c06gqfSolve)
  htShowPage()


c06gqfSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (m = '3 and n = '6)  => c06gqfDefaultSolve(htPage,ifail)
  newList:=
    "append"/[g(i,m) for i in 1..n] where g(i,n) ==
      labelList :=
        "append"/[f(i,j) for j in 1..n] where f(i,j) ==
          rnam := INTERN STRCONC ('"r",STRINGIMAGE i,STRINGIMAGE j)
          [['bcStrings,[6, 0.0, rnam, 'P]]]
      prefix := ('"\newline \tab{2} ")
      labelList := [['text,:prefix],:labelList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :newList]
  page := htInitPage("C06GQF - Complex conjugate of multiple Hermitian sequences ",htpPropertyList htPage)
  htSay '"\newline "
  htSay '"\menuitemstyle{}\tab{2} Please enter each sequence to be "
  htSay '"transformed in Hermitian format. (Each column to contain  "
  htSay '"a sequence.) "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'c06gqfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

c06gqfDefaultSolve (htPage, ifail) ==
  m := '3
  n := '6
  page := htInitPage('"C06GQF - Complex conjugate of multiple Hermitian sequences ",htpPropertyList htPage)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Please enter each sequence to be ")
    (text . "transformed in Hermitian format. ")
    (text . "(Each column to contain a sequence.) ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.3854" x11 F))
    (bcStrings (6 "0.5417" x21 F))
    (bcStrings (6 "0.9172" x31 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.6772" x12 F))
    (bcStrings (6 "0.2983" x22 F))
    (bcStrings (6 "0.0644" x32 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.1138" x13 F))
    (bcStrings (6 "0.1181" x23 F))
    (bcStrings (6 "0.6037" x33 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.6751" x14 F))
    (bcStrings (6 "0.7255" x24 F))
    (bcStrings (6 "0.6430" x34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.6362" x15 F))
    (bcStrings (6 "0.8638" x25 F))
    (bcStrings (6 "0.0428" x35 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.1424" x16 F))
    (bcStrings (6 "0.8723" y26 F))
    (bcStrings (6 "0.4815" y36 F))
    (text . "\blankline"))
  htMakeDoneButton('"Continue",'c06gqfGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


c06gqfGen htPage ==
  n := htpProperty(htPage,'n)
  m := htpProperty(htPage,'m)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y
    reallist := [right,:reallist]
  realstring := bcwords2liststring reallist
  linkGen STRCONC ('"c06gqf(",STRINGIMAGE m,",",STRINGIMAGE n,",[",realstring,"],", STRINGIMAGE ifail,")")



c06gsf() ==
  htInitPage('"C06GSF - Convert Hermitian sequences to general complex sequences", nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXc06gsf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|c06gsf| '|NagSeriesSummationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Takes {\it m} Hermitian sequences, each containing {\it n} data values, ")
    (text . "and forms the real and imaginary parts of the {\it m} ")
    (text . "corresponding complex sequences. \newline ")
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number {\it m} \inputbitmap{\htbmdir{}/great=.bitmap} 1 ")
    (text . "of sequences to be transformed: ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 3 m PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Enter the number {\it n} \inputbitmap{\htbmdir{}/great=.bitmap} 1 ")
    (text . "of data values in each sequence: ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 6 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'c06gsfSolve)
  htShowPage()


c06gsfSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (m = '3 and n = '6)  => c06gsfDefaultSolve(htPage,ifail)
  newList:=
    "append"/[g(i,m) for i in 1..n] where g(i,n) ==
      labelList :=
        "append"/[f(i,j) for j in 1..n] where f(i,j) ==
          rnam := INTERN STRCONC ('"r",STRINGIMAGE i,STRINGIMAGE j)
          [['bcStrings,[6, 0.0, rnam, 'P]]]
      prefix := ('"\newline \tab{2} ")
      labelList := [['text,:prefix],:labelList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :newList]
  page := htInitPage("C06GSF - Convert Hermitian sequences to general complex sequences ",htpPropertyList htPage)
  htSay '"\newline "
  htSay '"\menuitemstyle{}\tab{2} Please enter each sequence to be "
  htSay '"transformed in Hermitian format. (Each column to contain a "
  htSay '"sequence.) "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'c06gsfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

c06gsfDefaultSolve (htPage, ifail) ==
  m := '3
  n := '6
  page := htInitPage('"C06GSF - Convert Hermitian sequences to general complex sequences ",htpPropertyList htPage)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Please enter each sequence to be ")
    (text . "transformed in Hermitian format. (Each column to contain a ")
    (text . "sequence.) ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.3854" x11 F))
    (bcStrings (6 "0.5417" x21 F))
    (bcStrings (6 "0.9172" x31 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.6772" x12 F))
    (bcStrings (6 "0.2983" x22 F))
    (bcStrings (6 "0.0644" x32 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.1138" x13 F))
    (bcStrings (6 "0.1181" x23 F))
    (bcStrings (6 "0.6037" x33 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.6751" x14 F))
    (bcStrings (6 "0.7255" x24 F))
    (bcStrings (6 "0.6430" x34 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.6362" x15 F))
    (bcStrings (6 "0.8638" x25 F))
    (bcStrings (6 "0.0428" x35 F))
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.1424" x16 F))
    (bcStrings (6 "0.8723" y26 F))
    (bcStrings (6 "0.4815" y36 F))
    (text . "\blankline"))
  htMakeDoneButton('"Continue",'c06gsfGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


c06gsfGen htPage ==
  n := htpProperty(htPage,'n)
  m := htpProperty(htPage,'m)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y
    reallist := [right,:reallist]
  realstring := bcwords2liststring reallist
  linkGen STRCONC ('"c06gsf(",STRINGIMAGE m,",",STRINGIMAGE n,",[",realstring,"],", STRINGIMAGE ifail,")")


