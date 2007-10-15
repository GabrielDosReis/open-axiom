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

e02adf() ==
  htInitPage('"E02ADF - Least-squares curve fit, by polynomials, arbitrary data points", nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02adf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02adf| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Determines weighted least-squares polynomial approximations of ")
    (text . "degrees 0,1,...,k to the set of points {\it (} ")
    (text . "\htbitmap{xr}, \htbitmap{yr}{\it )} ")
    (text . "with weights \htbitmap{wr}, for r = 1,2,...,m. ")
    (text . "The polynomials are in the Chebyshev series form, the ")
    (text . "approximation of degree {\it i} being represented as ")
    (text . "\newline \center{\htbitmap{e02adf}} , where ")
    (text . "\htbitmap{xbar} is the normalised argument, which is ")
    (text . "related to the original variable {\it x} by the transformation ")
    (text . "\blankline  \center{\htbitmap{e02adf1}} ")
    (text . ",\htbitmap{xmin} and \htbitmap{xmax} being ")
    (text . "the values of \htbitmap{xr} respectively ")
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Number of data points {\it m}:")
    (text . "\newline \tab{2} ")
    (bcStrings (6 11 m PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Maximum degree required {\it k}:")
    (text . "\newline \tab{2} ")
    (bcStrings (6 3 k PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} \newline ")
    (text . "First dimension of A, {\it nrows} \htbitmap{great=} {\it k+1}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 50 nrows I))
    (text . "\blankline")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02adfSolve)
  htShowPage()

e02adfSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  k :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'k)
    objValUnwrap htpLabelSpadValue(htPage, 'k)
  nrows :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nrows)
    objValUnwrap htpLabelSpadValue(htPage, 'nrows)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (m = '11 and k ='3) => e02adfDefaultSolve(htPage,k,nrows,ifail)
  labelList :=
    "append"/[f(i) for i in 1..m] where f(i) ==
      prefix := ('"\newline \tab{2} ")
      middle := ('"\tab{22} ")
      post   := ('" \tab{42} ")
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      ynam := INTERN STRCONC ('"y",STRINGIMAGE i)
      znam := INTERN STRCONC ('"z",STRINGIMAGE i)
      num := INTERN STRCONC (STRINGIMAGE (QUOTIENT(i,10)),".",STRINGIMAGE (REM(i,10)))
      [['text,:prefix],['bcStrings,[10, num, xnam, 'F]], 
       ['text,:middle],['bcStrings,[10, 0.0, ynam, 'F]],
        ['text,:post],['bcStrings,[10, 0.0, znam, 'F]]]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList]
  page := htInitPage("E02ADF - Least-squares curve fit, by polynomials, arbitrary data points", htpPropertyList htPage)
  htSay '"\menuitemstyle{}\tab{2} Values of \space{1} "
  htSay '"\htbitmap{xr}: \tab{20} \menuitemstyle{}\tab{22} "
  htSay '"Values of \space{1} \htbitmap{fr}: \tab{40}"
  htSay '"\menuitemstyle{}\tab{42} Values of \space{1} "
  htSay '"\htbitmap{dr}: "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02adfGen)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'k,k)
  htpSetProperty(page,'nrows,nrows)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02adfDefaultSolve  (htPage,k,nrows,ifail) ==
  m := '11
  page := htInitPage('"E02ADF - Least-squares curve fit, by polynomials, arbitrary data points", htpPropertyList htPage)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Values of \space{1} ")
    (text . "\htbitmap{xr}: \tab{20} \menuitemstyle{}\tab{22} ")
    (text . "Values of \space{1} \htbitmap{yr}: \tab{40} ")
    (text . "\menuitemstyle{}\tab{42} Values of \space{1} ")
    (text . "\htbitmap{wr}: ")
    (text . "\newline \tab{2}")
    (bcStrings (10 "1.00" x1 F))
    (text . "\tab{22} ")
    (bcStrings (10 "10.40" y1 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "2.10" x2 F))
    (text . "\tab{22} ")
    (bcStrings (10 "7.90" y2 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "3.10" x3 F))
    (text . "\tab{22} ")
    (bcStrings (10 "4.70" y3 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "3.90" x4 F))
    (text . "\tab{22} ")
    (bcStrings (10 "2.50" y4 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "4.90" x5 F))
    (text . "\tab{22} ")
    (bcStrings (10 "1.20" y5 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z5 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "5.80" x6 F))
    (text . "\tab{22} ")
    (bcStrings (10 "2.20" y6 F))
    (text . "\tab{42} ")
    (bcStrings (10 "0.80" z6 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "6.50" x7 F))
    (text . "\tab{22} ")
    (bcStrings (10 "5.10" y7 F))
    (text . "\tab{42} ")
    (bcStrings (10 "0.80" z7 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "7.10" x8 F))
    (text . "\tab{22} ")
    (bcStrings (10 "9.20" y8 F))
    (text . "\tab{42} ")
    (bcStrings (10 "0.70" z8 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "7.80" x9 F))
    (text . "\tab{22} ")
    (bcStrings (10 "16.10" y9 F))
    (text . "\tab{42} ")
    (bcStrings (10 "0.50" z9 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "8.40" x10 F))
    (text . "\tab{22} ")
    (bcStrings (10 "24.50" y10 F))
    (text . "\tab{42} ")
    (bcStrings (10 "0.30" z10 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "9.00" x11 F))
    (text . "\tab{22} ")
    (bcStrings (10 "35.30" y11 F))
    (text . "\tab{42} ")
    (bcStrings (10 "0.20" z11 F))
    (text . "\blankline"))
  htMakeDoneButton('"Continue",'e02adfGen)      
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'k,k)
  htpSetProperty(page,'nrows,nrows)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02adfGen htPage ==
  m := htpProperty(htPage,'m)
  k := htpProperty(htPage,'k)
  nrows := htpProperty(htPage,'nrows)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  kplus1 := k + 1
  y := alist
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    mid :=  STRCONC ((first y).1," ")
    y := rest y
    left :=  STRCONC ((first y).1," ")
    y := rest y
    xlist := [left,:xlist]
    ylist := [mid,:ylist]
    wlist := [right,:wlist]
  xstring := bcwords2liststring xlist
  ystring := bcwords2liststring ylist
  wstring := bcwords2liststring wlist
  prefix := STRCONC('"e02adf(",STRINGIMAGE m,", ",STRINGIMAGE kplus1,", ")
  prefix := STRCONC(prefix,STRINGIMAGE nrows,", [",xstring,"],[",ystring,"],[")
  prefix := STRCONC(prefix,wstring,"],",STRINGIMAGE ifail,")")
  linkGen prefix

e02aef() ==
  htInitPage('"E02AEF - Evaluation of fitted polynomial in one variable from Chebyshev series form", nil)
  htMakePage '(
    (domainConditions 
      (isDomain F (Float))
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02aef} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02aef| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Evaluates a polynomial in Chabyshev series representation ")
    (text . "\newline \center{\htbitmap{e02aef}} ")
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Number of terms in the series {\it n}:")
    (text . "\newline \tab{2} ")
    (bcStrings (6 4 n PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\htbitmap{xbar}: ")
    (text . " \newline \tab{2} ")
    (bcStrings (6 "-1.0" xcap F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02aefSolve)
  htShowPage()

e02aefSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  xcap := htpLabelInputString(htPage,'xcap)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '4 => e02aefDefaultSolve(htPage,xcap,ifail)
  labelList :=
    "append"/[f(i) for i in 1..(n+1)] where f(i) ==
      prefix := ('"\newline \tab{15} ")
      anam := INTERN STRCONC ('"a",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[10, 0.0, anam, 'F]]]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList]
  page := htInitPage("E02AEF - Evaluation of fitted polynomial in one variable from Chebyshev series from", nil)
  htSay '"\menuitemstyle{}\tab{2} Enter the coefficients of {\it a(n+1)}:"
  htSay '"\blankline "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02aefGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'xcap,xcap)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02aefDefaultSolve  (htPage,xcap,ifail) ==
  n := '4
  page := htInitPage('"E02AEF - Evaluation of fitted polynomial in one variable from Chebyshev series form", nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the coeffients of {\it a(n+1)}: ")
    (text . "\blankline ")
    (text . "\newline \tab{15} ")
    (bcStrings (10 "2.0000" a1 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.5000" a2 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.2500" a3 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.1250" a4 F))
    (text . "\newline \tab{15} ")
    (bcStrings (10 "0.0625" a5 F))
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'e02aefGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'xcap,xcap)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02aefGen htPage ==
  n := htpProperty(htPage,'n)
  xcap := htpProperty(htPage,'xcap)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  nplus1 := n + 1
  y := alist
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    arrayList := [right,:arrayList]
  astring := bcwords2liststring arrayList
  prefix := STRCONC('"e02aef(",STRINGIMAGE nplus1,", [",astring ,"], ")
  prefix := STRCONC(prefix,STRINGIMAGE xcap,", ",STRINGIMAGE ifail,")")
  linkGen prefix

e02agf() ==
  htInitPage('"E02AGF - Least-squares polynomial fit, values and derivatives may be constrained, arbitrary data values",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02agf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02agf| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Determines constrained least-squares polynomial approximations ")
    (text . "to the set of points {\it (\htbitmap{xr},\htbitmap{yr})} with ")
    (text . "weights \htbitmap{wr}, for r = 1,2,...,m. The values of the ")
    (text . "approximations and any number of their derivatives must be ")
    (text . "specified at a further set of points \htbitmap{xii}, ")
    (text . "for i = 1,2,...,{\it mf}. The total number of interpolating ")
    (text . "conditions is given by \center{\htbitmap{e02agf}} where ")
    (text . "\htbitmap{pi} is the highest order derivative ")
    (text . "specified at point \htbitmap{xii}.  The values ")
    (text . "\htbitmap{xr} and \htbitmap{xii} all lie ")
    (text . "in the interval [\htbitmap{xmin},")
    (text . "\htbitmap{xmax}]. The polynomials are given in ")
    (text . "Chebyshev series form, the approximation of degree {\it i} being")
    (text . " represented as\blankline  \center{\htbitmap{e02agf1}}")
    (text . "\newline, where \htbitmap{xbar} is the normalised ")
    (text . "argument, related to the original variable {\it x} by the ")
    (text . "transformation \newline \center{\htbitmap{e02adf1}} ")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline Number of data points {\it m}:")
    (text . "\tab{32} \menuitemstyle{}\tab{34}")
    (text . "Maximum degree required {\it k}:")
    (text . "\newline\tab{2} ")
    (bcStrings (6 5 m PI))
    (text . "\tab{34} ")
    (bcStrings (6 4 k PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} \newline ")
    (text . "First dimension of A, {\it nrows \htbitmap{great=} k+1}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 6 nrows I))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline \htbitmap{xmin}: ")
    (text . "\tab{32} \menuitemstyle{}\tab{34}")
    (text . "\htbitmap{xmax}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 "0.0" xmin F))
    (text . "\tab{34} ")
    (bcStrings (6 "4.0" xmax F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline Dimension of {\it xf} & {\it ip}, {\it mf}: ")
    (text . "\tab{32} \menuitemstyle{}\tab{34}")
    (text . "Dimension of {\it yf}, {\it lyf}:")
    (text . "\newline\tab{2} ")
    (bcStrings (6 2 mf PI))
    (text . "\tab{34} ")
    (bcStrings (6 15 lyf PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02agfSolve)
  htShowPage()

e02agfSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  k :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'k)
    objValUnwrap htpLabelSpadValue(htPage, 'kplus1)
  nrows :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nrows)
    objValUnwrap htpLabelSpadValue(htPage, 'nrows)
  xmin := htpLabelInputString(htPage,'xmin)
  xmax := htpLabelInputString(htPage,'xmax)
  mf :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'mf)
    objValUnwrap htpLabelSpadValue(htPage, 'mf)
  lyf :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'lyf)
    objValUnwrap htpLabelSpadValue(htPage, 'lyf)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (m = '5 and k ='4 and mf = '2 and lyf = '15) => e02agfDefaultSolve(htPage,nrows,xmin,xmax,ifail)
  labelList :=
    "append"/[f(i) for i in 1..m] where f(i) ==
      prefix := ('"\newline \tab{2} ")
      middle := ('"\tab{22} ")
      post   := ('" \tab{42} ")
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      ynam := INTERN STRCONC ('"y",STRINGIMAGE i)
      znam := INTERN STRCONC ('"z",STRINGIMAGE i)
      num := INTERN STRCONC (STRINGIMAGE (QUOTIENT(i,10)),".",STRINGIMAGE (REM(i,10)))
      [['text,:prefix],['bcStrings,[10, num, xnam, 'F]], 
       ['text,:middle],['bcStrings,[10, 0.0, ynam, 'F]],
        ['text,:post],['bcStrings,[10, 0.0, znam, 'F]]]
  xfList := 
    "append"/[g(j) for j in 1..mf] where g(j) ==
      xfnam := INTERN STRCONC ('"xf",STRINGIMAGE j)
      [['bcStrings,[6, 0.0, xfnam, 'F]]]
  prefix := ('"\blankline \newline \menuitemstyle{}\tab{2} Values of ")
  prefix := STRCONC(prefix,"{\it xf}: \newline \tab{2} ")
  xfList := [['text,:prefix],:xfList] 
  ipList := 
    "append"/[h(k) for k in 1..mf] where h(k) ==
      ipnam := INTERN STRCONC ('"ip",STRINGIMAGE k)
      [['bcStrings,[6, 0, ipnam, 'PI]]]
  prefix := ('"\blankline \newline \menuitemstyle{}\tab{2} Values of ")
  prefix := STRCONC(prefix,"{\it ip}: \newline \tab{2} ")
  ipList := [['text,:prefix],:ipList] 
  yfList :=
    "append"/[i(l) for l in 1..lyf] where i(l) ==
      prefix := ('"\newline \tab{2} ")
      yfnam := INTERN STRCONC ('"lyf",STRINGIMAGE l)
      [['text,:prefix],['bcStrings,[10, 0.0, yfnam, 'F]]]
  prefix := ('"\blankline \newline \menuitemstyle{}\tab{2} Values of ")
  prefix := STRCONC(prefix,"{\it yf}: \newline \tab{2} ")
  yfList := [['text,:prefix],:yfList] 
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList,:xfList,:ipList,:yfList]
  page := htInitPage("E02AGF - Least-squares polynomial fit, values and derivatives may be constrained, arbitrary data values",nil)
  htSay '"\menuitemstyle{}\tab{2} Values of \space{1} "
  htSay '"\htbitmap{xr}: \tab{20} \menuitemstyle{}\tab{22} "
  htSay '"Values of \space{1} \htbitmap{yr}: \tab{40}"
  htSay '"\menuitemstyle{}\tab{42} Values of \space{1} "
  htSay '"\htbitmap{wr}: "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02agfGen)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'k,k)
  htpSetProperty(page,'nrows,nrows)
  htpSetProperty(page,'nrows,nrows)
  htpSetProperty(page,'xmin,xmin)
  htpSetProperty(page,'xmax,xmax)
  htpSetProperty(page,'mf,mf)
  htpSetProperty(page,'lyf,lyf)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02agfDefaultSolve  (htPage,nrows,xmin,xmax,ifail) == 
  m := '5
  k := '4
  mf := '2
  lyf := '15
  page := htInitPage('"E02AGF - Least-squares polynomial fit, values and derivativesby polynomials, arbitrary data points", htpPropertyList htPage)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\menuitemstyle{}\tab{2} Values of \space{1} ")
    (text . "\htbitmap{xr}: \tab{20} \menuitemstyle{}\tab{22} ")
    (text . "Values of \space{1} \htbitmap{yr}: \tab{40} ")
    (text . "\menuitemstyle{}\tab{42} Values of \space{1} ")
    (text . "\htbitmap{wr}: ")
    (text . "\newline \tab{2}")
    (bcStrings (10 "0.5" x1 F))
    (text . "\tab{22} ")
    (bcStrings (10 "0.03" y1 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.0" z1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.0" x2 F))
    (text . "\tab{22} ")
    (bcStrings (10 "-0.75" y2 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.0" z2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "2.0" x3 F))
    (text . "\tab{22} ")
    (bcStrings (10 "-1.0" y3 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.0" z3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "2.5" x4 F))
    (text . "\tab{22} ")
    (bcStrings (10 "-0.1" y4 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.0" z4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "3.0" x5 F))
    (text . "\tab{22} ")
    (bcStrings (10 "1.75" y5 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.0" z5 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Values of {\it xf}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" xf1 F))
    (bcStrings (6 "4.0" xf2 F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Values of {\it ip}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 1 ip1 PI))
    (bcStrings (6 0 ip2 PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Values of {\it yf}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.0" lyf1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "-2.0" lyf2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "9.0" lyf3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.0" lyf4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.0" lyf5 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.0" lyf6 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.0" lyf7 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.0" lyf8 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.0" lyf9 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.0" lyf10 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.0" lyf11 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.0" lyf12 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.0" lyf13 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.0" lyf14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.0" lyf15 F)))
  htMakeDoneButton('"Continue",'e02agfGen)      
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'k,k)
  htpSetProperty(page,'nrows,nrows)
  htpSetProperty(page,'xmin,xmin)
  htpSetProperty(page,'xmax,xmax)
  htpSetProperty(page,'mf,mf)
  htpSetProperty(page,'lyf,lyf)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02agfGen htPage ==
  m := htpProperty(htPage,'m)
  k := htpProperty(htPage,'k)
  nrows := htpProperty(htPage,'nrows)
  xmin := htpProperty(htPage,'xmin)
  xmax := htpProperty(htPage,'xmax)
  mf := htpProperty(htPage,'mf)
  lyf := htpProperty(htPage,'lyf)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  kplus1 := k + 1
  ipsum := 0
  y := alist
  for i in 1..lyf repeat
    yf := STRCONC((first y).1," ")
    yfList := [yf,:yfList]
    y := rest y
  yfstring := bcwords2liststring yfList
  for i in 1..mf repeat
    iptest := (first y).1
    iptestval := READ_-FROM_-STRING(iptest)
    ipsum := ipsum + iptestval
    ip := STRCONC(iptest," ")
    iptestList := [iptestval,:iptestList]
    ipList := [ip,:ipList]
    y := rest y
  ipstring := bcwords2liststring ipList
  ipmax := APPLY ('MAX, iptestList)
  n := mf + ipsum
  for i in 1..mf repeat
    xf := STRCONC((first y).1," ")
    xfList := [xf,:xfList]
    y := rest y
  xfstring := bcwords2liststring xfList
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    mid :=  STRCONC ((first y).1," ")
    y := rest y
    left :=  STRCONC ((first y).1," ")
    y := rest y
    xlist := [left,:xlist]
    ylist := [mid,:ylist]
    wlist := [right,:wlist]
  xstring := bcwords2liststring xlist
  ystring := bcwords2liststring ylist
  wstring := bcwords2liststring wlist
  wrktest1 := 4*m + 3*kplus1
  wrktest2 := 8*n + 5*ipmax + mf +10
  wrktestlist := [wrktest1,wrktest2]
  wrkmax := APPLY ('MAX, wrktestlist)
  lwrk := wrkmax + 2*n + 2
  liwrk := 2*mf + 2
  prefix := STRCONC('"e02agf(",STRINGIMAGE m,", ",STRINGIMAGE kplus1,", ")
  prefix := STRCONC(prefix,STRINGIMAGE nrows,", ",xmin,", ",xmax,", [",xstring)
  prefix := STRCONC(prefix,"],[",ystring,"],[",wstring,"],",STRINGIMAGE mf)
  prefix := STRCONC(prefix,", [",xfstring,"],[",yfstring,"],")
  prefix := STRCONC(prefix,STRINGIMAGE lyf,", [",ipstring,"]::Matrix Integer,")
  prefix := STRCONC(prefix,STRINGIMAGE lwrk,", ",STRINGIMAGE liwrk,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,")")
  linkGen prefix

e02ahf() ==
  htInitPage('"E02AHF - Derivative of fitted polynomial in Chebyshev series",nil)
  htMakePage '(
    (domainConditions
      (isDomain F (Float)) 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02ahf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02ahf| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Determines the indefinite integral of the Chebyshev series ")
    (text . "representation \newline \center{\htbitmap{e02ahf1}} ")
    (text . "of a polynomial, where \htbitmap{xbar} is the ")
    (text . "normalised argument, related to the original variable x by the ")
    (text . "transformation \blankline \center{\htbitmap{e02adf1}}")
    (text . ",\htbitmap{xmin} and \htbitmap{xmax} being ")
    (text . "minimum and maximum values of {\it x} respectively. The integral")
    (text . " polynomial has the form ")
    (text . "\blankline \center{\htbitmap{e02ahf}}")
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Degree of the polynomial {\it n}:")
    (text . "\newline \tab{2} ")
    (bcStrings (6 6 n PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline \htbitmap{xmin}: ")
    (text . "\tab{32} \menuitemstyle{}\tab{34}")
    (text . "\htbitmap{xmax}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 "-0.5" xmin F))
    (text . "\tab{34} ")
    (bcStrings (6 "2.5" xmax F))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{}\tab{2}")
--    (text . "\newline Dimension of array {\it a}, {\it la}: ")
--    (text . "\tab{32} \menuitemstyle{}\tab{34}")
--    (text . "Dimension of {\it adif}, {\it ladif}: ") 
--    (text . "\newline\tab{2} ")
--    (bcStrings (6 7 la PI))
--    (text . "\tab{34} ")
--    (bcStrings (6 7 ladif PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline Increment of array {\it a}, {\it ia1}: ")
    (text . "\tab{32} \menuitemstyle{}\tab{34}")
    (text . "\newline Increment of array {\it adif}, {\it ladif1}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 1 iaone PI))
    (text . "\tab{34} ")
    (bcStrings (6 1 ladifone PI))
    (text . "\blankline")
    (text . "\newline \menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02ahfSolve)
  htShowPage()

e02ahfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  xmin := htpLabelInputString(htPage,'xmin)
  xmax := htpLabelInputString(htPage,'xmax)
  iaone :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iaone)
    objValUnwrap htpLabelSpadValue(htPage, 'iaone)
  ladifone :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ladifone)
    objValUnwrap htpLabelSpadValue(htPage, 'ladifone)
  la := 1+n*iaone
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'la)
--    objValUnwrap htpLabelSpadValue(htPage, 'la)
  ladif :=1+n*ladifone
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ladif)
--    objValUnwrap htpLabelSpadValue(htPage, 'ladif)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (n = '6 and (la ='7 and ladif = '7)) => 
     e02ahfDefaultSolve(htPage,xmin,xmax,iaone,ladifone,ifail)
  labelList :=
    "append"/[f(i) for i in 1..la] where f(i) ==
      prefix := ('"\newline \tab{15} ")
      anam := INTERN STRCONC ('"a",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[10, "0.0000", anam, 'F]]]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList]
  page := htInitPage("E02AHF - Derivative of fitted polynomial in Chebyshev series",nil)
  htSay '"\menuitemstyle{}\tab{2} Coefficients of {\it a(la)}:"
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02ahfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'la,la)
  htpSetProperty(page,'ladif,ladif)
  htpSetProperty(page,'xmin,xmin)
  htpSetProperty(page,'xmax,xmax)
  htpSetProperty(page,'iaone,iaone)
  htpSetProperty(page,'ladifone,ladifone)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02ahfDefaultSolve  (htPage,xmin,xmax,iaone,ladifone,ifail) ==
  n := '6
  la := '7
  ladif := '7
  page := htInitPage('"E02AHF - Derivative of fitted polynomial in Chebyshev series",nil)
  htMakePage '(
    (domainConditions 
      (isDomain PI (Positive Integer))
       (isDomain F (Float)))
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Coefficients of {\it a(la)}: ")
    (text . "\newline \tab{15}")
    (bcStrings (10 "2.53213" a1 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "1.13032" a2 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "0.27150" a3 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "0.04434" a4 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "0.00547" a5 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "0.00054" a6 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "0.00004" a7 F))
    (text . "\blankline"))
  htMakeDoneButton('"Continue",'e02ahfGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'la,la)
  htpSetProperty(page,'ladif,ladif)
  htpSetProperty(page,'xmin,xmin)
  htpSetProperty(page,'xmax,xmax)
  htpSetProperty(page,'iaone,iaone)
  htpSetProperty(page,'ladifone,ladifone)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02ahfGen htPage ==
  n := htpProperty(htPage,'n)
  la := htpProperty(htPage,'la)
  ladif := htpProperty(htPage,'ladif)
  xmin := htpProperty(htPage,'xmin)
  xmax := htpProperty(htPage,'xmax)
  iaone := htpProperty(htPage,'iaone)
  ladifone := htpProperty(htPage,'ladifone)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  np1 := n + 1
  y := alist
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y
    arrayList := [right,:arrayList]
  astring := bcwords2liststring arrayList
  prefix := STRCONC('"e02ahf(",STRINGIMAGE np1,", ",xmin,", ",xmax,", [")
  prefix := STRCONC(prefix,astring,"], ",STRINGIMAGE iaone,", ")
  prefix := STRCONC(prefix,STRINGIMAGE la,", ",STRINGIMAGE ladifone,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ladif,", ",STRINGIMAGE ifail,")")
  linkGen prefix

e02ajf() ==
  htInitPage('"E02AJF - Integral of fitted polynomial in Chebyshev series form",nil)
  htMakePage '(
    (domainConditions
      (isDomain F (Float)) 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02ajf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02ajf| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Determines the indefinite integral of the Chebyshev series ")
    (text . "representation \newline \center{\htbitmap{e02ahf1}} ")
    (text . "of a polynomial, where \htbitmap{xbar} is the normalis")
    (text . "ed argument, related to the original variable {\it x} by the ")
    (text . "transformation \blankline \center{\htbitmap{e02adf1}}")
    (text . ",\htbitmap{xmin} and \htbitmap{xmax} being ")
    (text . "minimum and maximum values of {\it x} respectively. The integral")
    (text . " polynomial has the form ")
    (text . "\blankline \center{\htbitmap{e02ajf}}")
    (text . "and the integration is with respect to the original variable ")
    (text . "{\it x} \blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Degree of the polynomial {\it n}:")
    (text . "\newline \tab{2} ")
    (bcStrings (6 6 n PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline \htbitmap{xmin}: ")
    (text . "\tab{32} \menuitemstyle{}\tab{34}")
    (text . "\htbitmap{xmax}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 "-0.5" xmin F))
    (text . "\tab{34} ")
    (bcStrings (6 "2.5" xmax F))
--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{}\tab{2}")
--    (text . "\newline Dimension of array {\it a}, {\it la}: ")
--    (text . "\tab{32} \menuitemstyle{}\tab{34}")
--    (text . "Dimension of {\it aint}, {\it laint}: ")
--    (text . "\newline\tab{2} ")
--    (bcStrings (6 7 la PI))
--    (text . "\tab{34} ")
--    (bcStrings (6 8 laint PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline Index increment of {\it a}, {\it ia1}: ")
    (text . "\tab{32} \menuitemstyle{}\tab{34}")
    (text . "Increment of {\it aint}, {\it iaint1}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 1 iaone PI))
    (text . "\tab{34} ")
    (bcStrings (6 1 iaintone PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Constant of integration {\it qatm1}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "0.0" qatmone F))
    (text . "\blankline")
    (text . "\newline \menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02ajfSolve)
  htShowPage()

e02ajfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  xmin := htpLabelInputString(htPage,'xmin)
  xmax := htpLabelInputString(htPage,'xmax)
  iaone :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iaone)
    objValUnwrap htpLabelSpadValue(htPage, 'iaone)
  iaintone :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iaintone)
    objValUnwrap htpLabelSpadValue(htPage, 'iaintone)
  la := 1+n*iaone
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'la)
--    objValUnwrap htpLabelSpadValue(htPage, 'la)
  laint := n*iaintone + 1
  qatmone := htpLabelInputString(htPage,'qatmone)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (n = '6 and (la ='7 and laint = '7)) => 
     e02ajfDefaultSolve(htPage,xmin,xmax,iaone,iaintone,qatmone,ifail)
  labelList :=
    "append"/[f(i) for i in 1..la] where f(i) ==
      prefix := ('"\newline \tab{15} ")
      anam := INTERN STRCONC ('"a",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[10, "0.0000", anam, 'F]]]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList]
  page := htInitPage("E02AJF - Integral of fitted polynomial in Chebyshev series form",nil)
  htSay '"\menuitemstyle{}\tab{2} Coefficients of {\it a(la)}: "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02ajfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'la,la)
  htpSetProperty(page,'laint,laint)
  htpSetProperty(page,'xmin,xmin)
  htpSetProperty(page,'xmax,xmax)
  htpSetProperty(page,'iaone,iaone)
  htpSetProperty(page,'iaintone,iaintone)
  htpSetProperty(page,'qatmone,qatmone)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02ajfDefaultSolve  (htPage,xmin,xmax,iaone,iaintone,qatmone,ifail) ==
  n := '6
  la := '7
  laint := '8
  page := htInitPage('"E02AJF - Integral of fitted polynomial in Chebyshev series form",nil)
  htMakePage '(
    (domainConditions 
      (isDomain PI (Positive Integer))
       (isDomain F (Float)))
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Coefficients of {\it a(la)}: ")
    (text . "\newline \tab{15}")
    (bcStrings (10 "2.53213" a1 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "1.13032" a2 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "0.27150" a3 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "0.04434" a4 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "0.00547" a5 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "0.00054" a6 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "0.00004" a7 F))
    (text . "\blankline"))
  htMakeDoneButton('"Continue",'e02ajfGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'la,la)
  htpSetProperty(page,'laint,laint)
  htpSetProperty(page,'xmin,xmin)
  htpSetProperty(page,'xmax,xmax)
  htpSetProperty(page,'iaone,iaone)
  htpSetProperty(page,'iaintone,iaintone)
  htpSetProperty(page,'qatmone,qatmone)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02ajfGen htPage ==
  n := htpProperty(htPage,'n)
  la := htpProperty(htPage,'la)
  laint := htpProperty(htPage,'laint)
  xmin := htpProperty(htPage,'xmin)
  xmax := htpProperty(htPage,'xmax)
  iaone := htpProperty(htPage,'iaone)
  iaintone := htpProperty(htPage,'iaintone)
  qatmone := htpProperty(htPage,'qatmone)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  np1 := n + 1
  y := alist
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y
    arrayList := [right,:arrayList]
  astring := bcwords2liststring arrayList
  prefix := STRCONC('"e02ajf(",STRINGIMAGE np1,", ",xmin,", ",xmax,", [")
  prefix := STRCONC(prefix,astring,"], ",STRINGIMAGE iaone,", ")
  prefix := STRCONC(prefix,STRINGIMAGE la,", ",qatmone,", ")
  prefix := STRCONC(prefix,STRINGIMAGE iaintone)
  prefix := STRCONC(prefix,", ",STRINGIMAGE laint,", ",STRINGIMAGE ifail,")")
  linkGen prefix

e02akf() ==
  htInitPage('"E02AKF - Evaluation of fitted polynomial in one variable, from Chebyshev series form",nil)
  htMakePage '(
    (domainConditions
      (isDomain F (Float)) 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02akf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02akf| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Evaluates at the point x the Chebyshev series representation ")
    (text . "representation \newline \center{\htbitmap{e02ahf1}} ")
    (text . "of a polynomial, where \htbitmap{xbar} is the normalis")
    (text . "ed argument, related to the original variable {\it x} by the ")
    (text . "transformation \blankline \center{\htbitmap{e02adf1}}")
    (text . ",\htbitmap{xmin} and \htbitmap{xmax} being ")
    (text . "minimum and maximum values of {\it x} respectively. ")
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Degree of the polynomial {\it n}:")
    (text . "\tab{32} \menuitemstyle{}\tab{34}")
    (text . "Evaluation point {\it x}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (6 6 n PI))
    (text . "\tab{34} ")
    (bcStrings (6 "-0.5" x F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline \htbitmap{xmin}: ")
    (text . "\tab{32} \menuitemstyle{}\tab{34}")
    (text . "\htbitmap{xmax}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 "-0.5" xmin F))
    (text . "\tab{34} ")
    (bcStrings (6 "2.5" xmax F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
--    (text . "\newline Dimension of array {\it a}, {\it la} : ")
--    (text . "\tab{32} \menuitemstyle{}\tab{34}")
    (text . "Index increment of {\it a}, {\it ia1}: ")
    (text . "\newline\tab{2} ")
--    (bcStrings (6 7 la PI))
--    (text . "\tab{34} ")
    (bcStrings (6 1 iaone PI))
    (text . "\blankline")
    (text . "\newline \menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02akfSolve)
  htShowPage()

e02akfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  x := htpLabelInputString(htPage,'x)
  xmin := htpLabelInputString(htPage,'xmin)
  xmax := htpLabelInputString(htPage,'xmax)
  iaone :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iaone)
    objValUnwrap htpLabelSpadValue(htPage, 'iaone)
  la := 1+n*iaone
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'la)
--    objValUnwrap htpLabelSpadValue(htPage, 'la)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (n = '6 and la ='7) =>  e02akfDefaultSolve(htPage,xmin,xmax,x,iaone,ifail)
  labelList :=
    "append"/[f(i) for i in 1..la] where f(i) ==
      prefix := ('"\newline \tab{15} ")
      anam := INTERN STRCONC ('"a",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[10, "0.0000", anam, 'F]]]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList]
  page := htInitPage("E02AKF - Evaluation of fitted polynomial in one variable, from Chebyshev series form",nil)
  htSay '"\menuitemstyle{}\tab{2} Coefficients of {\it a(la)}:"
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02akfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'x,x)
  htpSetProperty(page,'la,la)
  htpSetProperty(page,'xmin,xmin)
  htpSetProperty(page,'xmax,xmax)
  htpSetProperty(page,'iaone,iaone)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02akfDefaultSolve  (htPage,xmin,xmax,x,iaone,ifail) ==
  n := '6
  la := '7
  page := htInitPage('"E02AKF - Evaluation of fitted polynomial in one variable, from Chebyshev series form",nil)
  htMakePage '(
    (domainConditions 
      (isDomain PI (Positive Integer))
       (isDomain F (Float)))
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Coefficients of {\it a(la)}: ")
    (text . "\newline \tab{15}")
    (bcStrings (10 "2.53213" a1 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "1.13032" a2 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "0.27150" a3 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "0.04434" a4 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "0.00547" a5 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "0.00054" a6 F))
    (text . "\newline \tab{15}")
    (bcStrings (10 "0.00004" a7 F))
    (text . "\blankline"))
  htMakeDoneButton('"Continue",'e02akfGen)      
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'la,la)
  htpSetProperty(page,'x,x)
  htpSetProperty(page,'xmin,xmin)
  htpSetProperty(page,'xmax,xmax)
  htpSetProperty(page,'iaone,iaone)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02akfGen htPage ==
  n := htpProperty(htPage,'n)
  x := htpProperty(htPage,'x)
  la := htpProperty(htPage,'la)
  xmin := htpProperty(htPage,'xmin)
  xmax := htpProperty(htPage,'xmax)
  iaone := htpProperty(htPage,'iaone)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  np1 := n + 1
  y := alist
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y
    arrayList := [right,:arrayList]
  astring := bcwords2liststring arrayList
  prefix := STRCONC('"e02akf(",STRINGIMAGE np1,", ",xmin,", ",xmax,", [")
  prefix := STRCONC(prefix,astring,"], ",STRINGIMAGE iaone,", ")
  prefix := STRCONC(prefix,STRINGIMAGE la,", ",x,", ",STRINGIMAGE ifail,")")
  linkGen prefix


e02baf() ==
  htInitPage('"E02BAF - Least-squares curve cubic spine fit",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02baf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02baf| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Determines a least-squares cubic spline approximation to the ")
    (text . "set of points {\it (}\htbitmap{xr},")
    (text . "\htbitmap{yr}{\it )} with weights ")
    (text . "\htbitmap{wr}, for r = 1,2,...,m. ")
    (text . "The knots \htbitmap{lamdai}, for i = 1,2,...,")
    (text . "\htbitmap{ncap}+7, are prescribed by the user. The ")
    (text . "spline is given by the B-spline representation \blankline ")
    (text . "\center{\htbitmap{e02baf}} where ")
    (text . "\htbitmap{ncap} is the number of intervals of the ")
    (text . "spline. \blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Number of data points {\it m}:")
    (text . "\newline \tab{2} ")
    (bcStrings (6 14 m PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of intervals in the spline \htbitmap{ncap}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 5 ncap PI))
    (text . "\blankline")
    (text . "\newline")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02bafSolve)
  htShowPage()

e02bafSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  ncap :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ncap)
    objValUnwrap htpLabelSpadValue(htPage, 'ncap)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (m = '14 and ncap ='5) => e02bafDefaultSolve(htPage,ifail)
  labelList :=
    "append"/[f(i) for i in 1..m] where f(i) ==
      prefix := ('"\newline \tab{2} ")
      middle := ('"\tab{22} ")
      post   := ('" \tab{42} ")
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      ynam := INTERN STRCONC ('"y",STRINGIMAGE i)
      znam := INTERN STRCONC ('"z",STRINGIMAGE i)
      num := INTERN STRCONC (STRINGIMAGE (QUOTIENT(i,10)),".",STRINGIMAGE (REM(i,10)))
      [['text,:prefix],['bcStrings,[10, num, xnam, 'F]], 
       ['text,:middle],['bcStrings,[10, 0.0, ynam, 'F]],
        ['text,:post],['bcStrings,[10, 0.0, znam, 'F]]]
  lamdaList := 
    "append"/[g(j) for j in 5..(ncap+3)] where g(j) ==
       anam := INTERN STRCONC ('"a",STRINGIMAGE j)
       [['bcStrings,[6, 0.0, anam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{}\tab{2} Interior knots ")
  prefix := STRCONC(prefix,"\htbitmap{lamdai}, for i = 5,6,...,")
  prefix := STRCONC(prefix,"\htbitmap{ncap} + 3: \newline \tab{2}" )
  lamdaList := [['text,:prefix],:lamdaList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList,:lamdaList]
  page := htInitPage("E02BAF - Least-squares curve cubic spline fit",nil)
  htSay '"\menuitemstyle{}\tab{2} Values of \space{1} "
  htSay '"\htbitmap{xr}: \tab{20} \menuitemstyle{}\tab{22} "
  htSay '"Values of \space{1} \htbitmap{fr}: \tab{40}"
  htSay '"\menuitemstyle{}\tab{42} Values of \space{1} "
  htSay '"\htbitmap{dr}: "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02bafGen)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'ncap,ncap)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02bafDefaultSolve (htPage,ifail) ==
  m := '14
  ncap := '5
  page := htInitPage('"E02BAF - Least-squares curve cubic spline fit",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\menuitemstyle{}\tab{2} Values of \space{1} ")
    (text . "\htbitmap{xr}: \tab{20} \menuitemstyle{}\tab{22} ")
    (text . "Values of \space{1} \htbitmap{yr}: \tab{40} ")
    (text . "\menuitemstyle{}\tab{42} Values of \space{1} ")
    (text . "\htbitmap{wr}: ")
    (text . "\newline \tab{2}")
    (bcStrings (10 "0.20" x1 F))
    (text . "\tab{22} ")
    (bcStrings (10 "0.00" y1 F))
    (text . "\tab{42} ")
    (bcStrings (10 "0.20" z1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.47" x2 F))
    (text . "\tab{22} ")
    (bcStrings (10 "2.00" y2 F))
    (text . "\tab{42} ")
    (bcStrings (10 "0.20" z2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.74" x3 F))
    (text . "\tab{22} ")
    (bcStrings (10 "4.00" y3 F))
    (text . "\tab{42} ")
    (bcStrings (10 "0.30" z3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.09" x4 F))
    (text . "\tab{22} ")
    (bcStrings (10 "6.00" y4 F))
    (text . "\tab{42} ")
    (bcStrings (10 "0.70" z4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.60" x5 F))
    (text . "\tab{22} ")
    (bcStrings (10 "8.00" y5 F))
    (text . "\tab{42} ")
    (bcStrings (10 "0.90" z5 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.90" x6 F))
    (text . "\tab{22} ")
    (bcStrings (10 "8.62" y6 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z6 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "2.60" x7 F))
    (text . "\tab{22} ")
    (bcStrings (10 "9.10" y7 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z7 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "3.10" x8 F))
    (text . "\tab{22} ")
    (bcStrings (10 "8.90" y8 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z8 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "4.00" x9 F))
    (text . "\tab{22} ")
    (bcStrings (10 "8.15" y9 F))
    (text . "\tab{42} ")
    (bcStrings (10 "0.80" z9 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "5.15" x10 F))
    (text . "\tab{22} ")
    (bcStrings (10 "7.00" y10 F))
    (text . "\tab{42} ")
    (bcStrings (10 "0.50" z10 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "6.17" x11 F))
    (text . "\tab{22} ")
    (bcStrings (10 "6.00" y11 F))
    (text . "\tab{42} ")
    (bcStrings (10 "0.70" z11 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "8.00" x12 F))
    (text . "\tab{22} ")
    (bcStrings (10 "4.54" y12 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z12 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "10.00" x13 F))
    (text . "\tab{22} ")
    (bcStrings (10 "3.39" y13 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z13 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "12.00" x14 F))
    (text . "\tab{22} ")
    (bcStrings (10 "2.56" y14 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z14 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "Interior knots \htbitmap{lamdai}, for i = 5,6,...")
    (text . "\htbitmap{ncap} + 3: \newline \tab{2}")
    (bcStrings (6 "1.50" l1 F))
    (bcStrings (6 "2.60" l2 F))
    (bcStrings (6 "4.00" l3 F))
    (bcStrings (6 "8.00" l4 F))
    (text . "\blankline"))
  htMakeDoneButton('"Continue",'e02bafGen)      
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'ncap,ncap)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02bafGen htPage ==
  m := htpProperty(htPage,'m)
  ncap := htpProperty(htPage,'ncap)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  ncap7 := ncap + 7
  y := alist
  for i in (ncap+4)..(ncap+7) repeat
    lambda := STRCONC( "0.0"," ")
    lambdaList := [lambda,:lambdaList]
  for i in 5..(ncap+3) repeat
    lambda := STRCONC ((first y).1," ")
    y := rest y
    lambdaList := [lambda,:lambdaList]
  for i in 1..4 repeat
    lambda := STRCONC( "0.0"," ")
    lambdaList := [lambda,:lambdaList]
  lambdaString := bcwords2liststring lambdaList
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    mid :=  STRCONC ((first y).1," ")
    y := rest y
    left :=  STRCONC ((first y).1," ")
    y := rest y
    xlist := [left,:xlist]
    ylist := [mid,:ylist]
    wlist := [right,:wlist]
  xstring := bcwords2liststring xlist
  ystring := bcwords2liststring ylist
  wstring := bcwords2liststring wlist
  prefix := STRCONC('"e02baf(",STRINGIMAGE m,", ",STRINGIMAGE ncap7,", [")
  prefix := STRCONC(prefix,xstring,"],[",ystring,"],[",wstring,"], [")
  prefix := STRCONC(prefix,lambdaString,"], ",STRINGIMAGE ifail,")")
  linkGen prefix


e02bbf() ==
  htInitPage('"E02BBF - Evaluation of fitted cubic spline, function only",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02bbf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02bbf| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Evaluates at the point {\it x} a cubic spline from its B-spline ")
    (text . "B-spline representation ")
    (text . "\center{\htbitmap{e02baf}} where ")
    (text . "\htbitmap{ncap} is the number of intervals of the ")
    (text . "spline. The spline has knots \htbitmap{lamdai}, for ")
    (text . "i = 1,2,...,\htbitmap{ncap} + 7. \blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Number of intervals in the spline \htbitmap{ncap}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 4 ncap PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Evaluation point {\it x}:")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "2.0" x F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02bbfSolve)
  htShowPage()

e02bbfSolve htPage ==
  ncap :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ncap)
    objValUnwrap htpLabelSpadValue(htPage, 'ncap)
  x := htpLabelInputString(htPage,'x)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  ncap = '4  => e02bbfDefaultSolve(htPage,x,ifail)
  labelList :=
    "append"/[f(i) for i in 1..(ncap+7)] where f(i) ==
      prefix := ('"\newline \tab{2} ")
      middle := ('"\tab{22} ")
      lnam := INTERN STRCONC ('"l",STRINGIMAGE i)
      cnam := INTERN STRCONC ('"c",STRINGIMAGE i)
      num := INTERN STRCONC (STRINGIMAGE (QUOTIENT(i,10)),".",STRINGIMAGE (REM(i,10)))
      [['text,:prefix],['bcStrings,[10, num, lnam, 'F]], 
       ['text,:middle],['bcStrings,[10, 0.0, cnam, 'F]]]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList]
  page := htInitPage("E02BBF - Evaluation of fitted cubic spline, function only",nil)
  htSay '"\menuitemstyle{}\tab{2} Knots \htbitmap{lamdai}: "
  htSay '"\tab{20} \menuitemstyle{}\tab{22} "
  htSay '"Coefficients \space{1} \htbitmap{ci}: "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02bbfGen)
  htpSetProperty(page,'ncap,ncap)
  htpSetProperty(page,'x,x)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02bbfDefaultSolve (htPage,x,ifail) ==
  ncap := '4
  page := htInitPage('"E02BBF - Evaluation of fitted cubic spline, function only",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Knots \space{1}")
    (text . "\htbitmap{lamdai}: \tab{20} \menuitemstyle{}\tab{22} ")
    (text . "Coefficients \space{1} \htbitmap{ci}: ")
    (text . "\newline \tab{2}")
    (bcStrings (10 "1.00" l1 F))
    (text . "\tab{22} ")
    (bcStrings (10 "1.00" c1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.00" l2 F))
    (text . "\tab{22} ")
    (bcStrings (10 "2.00" c2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.00" l3 F))
    (text . "\tab{22} ")
    (bcStrings (10 "4.00" c3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.00" l4 F))
    (text . "\tab{22} ")
    (bcStrings (10 "7.00" c4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "3.00" l5 F))
    (text . "\tab{22} ")
    (bcStrings (10 "6.00" c5 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "6.00" l6 F))
    (text . "\tab{22} ")
    (bcStrings (10 "4.00" c6 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "8.00" l7 F))
    (text . "\tab{22} ")
    (bcStrings (10 "3.00" c7 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "9.00" l8 F))
    (text . "\tab{22} ")
    (bcStrings (10 "0.00" c8 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "9.00" l9 F))
    (text . "\tab{22} ")
    (bcStrings (10 "0.00" c9 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "9.00" l10 F))
    (text . "\tab{22} ")
    (bcStrings (10 "0.00" c10 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "9.00" l11 F))
    (text . "\tab{22} ")
    (bcStrings (10 "0.00" c11 F))
    (text . "\blankline"))
  htMakeDoneButton('"Continue",'e02bbfGen)      
  htpSetProperty(page,'ncap,ncap)
  htpSetProperty(page,'x,x)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02bbfGen htPage ==
  ncap := htpProperty(htPage,'ncap)
  x := htpProperty(htPage,'x)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  ncap7 := ncap + 7
  y := alist
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    left :=  STRCONC ((first y).1," ")
    y := rest y
    lamlist := [left,:lamlist]
    clist := [right,:clist]
  lamstring := bcwords2liststring lamlist
  cstring := bcwords2liststring clist
  prefix := STRCONC('"e02bbf(",STRINGIMAGE ncap7,", [",lamstring,"],[")
  prefix := STRCONC(prefix,cstring,"], ",x,", ",STRINGIMAGE ifail,")")
  linkGen prefix


e02bcf() ==
  htInitPage('"E02BCF - Evaluation of fitted cubic spline, function and derivatives",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02bcf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02bcf| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Evaluates at the point {\it x} a cubic spline and its first ")
    (text . "three derivatives from its B-spline representation ")
    (text . "\center{\htbitmap{e02baf}} where ")
    (text . "\htbitmap{ncap} is the number of intervals of the ")
    (text . "spline. The spline has knots \htbitmap{lamdai}, for ")
    (text . "i = 1,2,...,\htbitmap{ncap} + 7. \blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Number of intervals in the spline \htbitmap{ncap}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 7 ncap PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Evaluation point {\it x}:")
    (text . "\newline \tab{2} ")
    (bcStrings (6 "2.0" x F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "{\it LEFT} specifies whether LH or RH derivatives are required: ")
    (radioButtons deriv
        ("" "  Left-hand derivative" left)
        ("" "  Right-hand derivative" right))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02bcfSolve)
  htShowPage()

e02bcfSolve htPage ==
  ncap :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ncap)
    objValUnwrap htpLabelSpadValue(htPage, 'ncap)
  x := htpLabelInputString(htPage,'x)
  temp := htpButtonValue(htPage,'deriv)
  deriv :=
    temp = 'left => '1
    '2
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  ncap = '7  => e02bcfDefaultSolve(htPage,x,deriv,ifail)
  labelList :=
    "append"/[f(i) for i in 1..(ncap+7)] where f(i) ==
      prefix := ('"\newline \tab{2} ")
      middle := ('"\tab{22} ")
      lnam := INTERN STRCONC ('"l",STRINGIMAGE i)
      cnam := INTERN STRCONC ('"c",STRINGIMAGE i)
      num := INTERN STRCONC (STRINGIMAGE (QUOTIENT(i,10)),".",STRINGIMAGE (REM(i,10)))
      [['text,:prefix],['bcStrings,[10, num, lnam, 'F]], 
       ['text,:middle],['bcStrings,[10, 0.0, cnam, 'F]]]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList]
  page := htInitPage("E02BCF - Evaluation of fitted cubic spline, function and derivatives",nil)
  htSay '"\menuitemstyle{}\tab{2} Knots \htbitmap{lamdai}: "
  htSay '"\tab{20} \menuitemstyle{}\tab{22} "
  htSay '"Coefficients \space{1} \htbitmap{ci}: "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02bcfGen)
  htpSetProperty(page,'ncap,ncap)
  htpSetProperty(page,'x,x)
  htpSetProperty(page,'deriv,deriv)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02bcfDefaultSolve (htPage,x,deriv,ifail) ==
  ncap := '7
  page := htInitPage('"E02BCF - Evaluation of fitted cubic spline, function and derivatives",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\menuitemstyle{}\tab{2} Knots \space{1}")
    (text . "\htbitmap{lamdai}: \tab{20} \menuitemstyle{}\tab{22} ")
    (text . "Coefficients \space{1} \htbitmap{ci}: ")
    (text . "\newline \tab{2}")
    (bcStrings (10 "0.0" l1 F))
    (text . "\tab{22} ")
    (bcStrings (10 "10.00" c1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.00" l2 F))
    (text . "\tab{22} ")
    (bcStrings (10 "12.00" c2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.00" l3 F))
    (text . "\tab{22} ")
    (bcStrings (10 "13.00" c3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.00" l4 F))
    (text . "\tab{22} ")
    (bcStrings (10 "15.00" c4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.00" l5 F))
    (text . "\tab{22} ")
    (bcStrings (10 "22.00" c5 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "3.00" l6 F))
    (text . "\tab{22} ")
    (bcStrings (10 "26.00" c6 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "3.00" l7 F))
    (text . "\tab{22} ")
    (bcStrings (10 "24.00" c7 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "3.00" l8 F))
    (text . "\tab{22} ")
    (bcStrings (10 "18.00" c8 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "4.00" l9 F))
    (text . "\tab{22} ")
    (bcStrings (10 "14.00" c9 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "4.00" l10 F))
    (text . "\tab{22} ")
    (bcStrings (10 "12.00" c10 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "6.00" l11 F))
    (text . "\tab{22} ")
    (bcStrings (10 "0.00" c11 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "6.00" l12 F))
    (text . "\tab{22} ")
    (bcStrings (10 "0.00" c12 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "6.00" l13 F))
    (text . "\tab{22} ")
    (bcStrings (10 "0.00" c13 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "6.00" l14 F))
    (text . "\tab{22} ")
    (bcStrings (10 "0.00" c14 F))
    (text . "\blankline"))
  htMakeDoneButton('"Continue",'e02bcfGen)      
  htpSetProperty(page,'ncap,ncap)
  htpSetProperty(page,'x,x)
  htpSetProperty(page,'deriv,deriv)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02bcfGen htPage ==
  ncap := htpProperty(htPage,'ncap)
  x := htpProperty(htPage,'x)
  deriv := htpProperty(htPage,'deriv)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  ncap7 := ncap + 7
  y := alist
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    left :=  STRCONC ((first y).1," ")
    y := rest y
    lamlist := [left,:lamlist]
    clist := [right,:clist]
  lamstring := bcwords2liststring lamlist
  cstring := bcwords2liststring clist
  prefix := STRCONC('"e02bcf(",STRINGIMAGE ncap7,", [",lamstring,"],[")
  prefix := STRCONC(prefix,cstring,"], ",x,", ",STRINGIMAGE deriv)
  prefix := STRCONC(prefix,", ",STRINGIMAGE ifail,")")
  linkGen prefix



e02bdf() ==
  htInitPage('"E02BDF - Evaluation of fitted cubic spline, definite integral",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02bdf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02bdf| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Evaluates the definite integral of a cubic spline from its ")
    (text . "B-spline representation \center{\htbitmap{e02baf}} ")
    (text . "where \htbitmap{ncap} is the number of intervals of ")
    (text . "the spline. The spline has knots \htbitmap{lamdai}, ")
    (text . "for i = 1,2,...,\htbitmap{ncap} + 7, and the integral ")
    (text . "is evaluated over the range \htbitmap{e02bdf} ")
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Number of intervals in the spline \htbitmap{ncap}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 7 ncap PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02bdfSolve)
  htShowPage()

e02bdfSolve htPage ==
  ncap :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ncap)
    objValUnwrap htpLabelSpadValue(htPage, 'ncap)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  ncap = '7 => e02bdfDefaultSolve(htPage,ifail)
  labelList :=
    "append"/[f(i) for i in 1..(ncap+7)] where f(i) ==
      prefix := ('"\newline \tab{2} ")
      middle := ('"\tab{22} ")
      lnam := INTERN STRCONC ('"l",STRINGIMAGE i)
      cnam := INTERN STRCONC ('"c",STRINGIMAGE i)
      num := INTERN STRCONC (STRINGIMAGE (QUOTIENT(i,10)),".",STRINGIMAGE (REM(i,10)))
      [['text,:prefix],['bcStrings,[10, num, lnam, 'F]], 
       ['text,:middle],['bcStrings,[10, 0.0, cnam, 'F]]]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList]
  page := htInitPage("E02BDF - Evaluation of fitted cubic spline, definite integral",nil)
  htSay '"\menuitemstyle{}\tab{2} Knots \htbitmap{lamdai}: "
  htSay '"\tab{20} \menuitemstyle{}\tab{22} "
  htSay '"Coefficients \space{1} \htbitmap{ci}: "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02bdfGen)
  htpSetProperty(page,'ncap,ncap)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02bdfDefaultSolve(htPage,ifail) ==
  ncap := '7
  page := htInitPage('"E02BDF - Evaluation of fitted cubic spline, definite integral",nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Knots \space{1}")
    (text . "\htbitmap{lamdai}: \tab{20} \menuitemstyle{}\tab{22} ")
    (text . "Coefficients \space{1} \htbitmap{ci}: ")
    (text . "\newline \tab{2}")
    (bcStrings (10 "0.0" l1 F))
    (text . "\tab{22} ")
    (bcStrings (10 "10.00" c1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.00" l2 F))
    (text . "\tab{22} ")
    (bcStrings (10 "12.00" c2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.00" l3 F))
    (text . "\tab{22} ")
    (bcStrings (10 "13.00" c3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.00" l4 F))
    (text . "\tab{22} ")
    (bcStrings (10 "15.00" c4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.00" l5 F))
    (text . "\tab{22} ")
    (bcStrings (10 "22.00" c5 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "3.00" l6 F))
    (text . "\tab{22} ")
    (bcStrings (10 "26.00" c6 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "3.00" l7 F))
    (text . "\tab{22} ")
    (bcStrings (10 "24.00" c7 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "3.00" l8 F))
    (text . "\tab{22} ")
    (bcStrings (10 "18.00" c8 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "4.00" l9 F))
    (text . "\tab{22} ")
    (bcStrings (10 "14.00" c9 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "4.00" l10 F))
    (text . "\tab{22} ")
    (bcStrings (10 "12.00" c10 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "6.00" l11 F))
    (text . "\tab{22} ")
    (bcStrings (10 "0.00" c11 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "6.00" l12 F))
    (text . "\tab{22} ")
    (bcStrings (10 "0.00" c12 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "6.00" l13 F))
    (text . "\tab{22} ")
    (bcStrings (10 "0.00" c13 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "6.00" l14 F))
    (text . "\tab{22} ")
    (bcStrings (10 "0.00" c14 F)))
  htpSetProperty(page,'ncap,ncap)
  htpSetProperty(page,'ifail,ifail)
  htMakeDoneButton('"Continue",'e02bdfGen)      
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02bdfGen htPage ==
  ncap := htpProperty(htPage,'ncap)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  ncap7 := ncap + 7
  y := alist
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    left :=  STRCONC ((first y).1," ")
    y := rest y
    lamlist := [left,:lamlist]
    clist := [right,:clist]
  lamstring := bcwords2liststring lamlist
  cstring := bcwords2liststring clist
  prefix := STRCONC('"e02bdf(",STRINGIMAGE ncap7,", [",lamstring,"],[")
  prefix := STRCONC(prefix,cstring,"], ",STRINGIMAGE ifail,")")
  linkGen prefix



e02bef() ==
  htInitPage('"E02BEF - Least-squares curve cubic spline fit, automatic knot placement",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02bef} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02bef| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Determines a cubic spline approximation to the set of points ")
    (text . "{\it ( \htbitmap{xr},\htbitmap{yr}) } ")
    (text . "with weights \htbitmap{wr}, for r = 1,2,...,m. ")
    (text . "The knots \htbitmap{lamdai}, for i = 1,2,...,n, ")
    (text . "are chosen by the routine, but a single parameter S must be ")
    (text . "specified to control the trade-off between closeness of fit and ")
    (text . "smoothness of fit. This affects the number of knots required ")
    (text . "by the spline, which is given in the B-spline representation ")
    (text . "\center{\htbitmap{e02bef}}, where n-1 is the number of")
    (text . " intervals of the spline. ")
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Number of data points {\it m}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 15 m PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Smoothing factor {\it s}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 "1.0" s F))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2}  ")
    (text . "Over-estimate of number n of knots {\it nest}:\newline\tab{2} ")
    (bcStrings (6 54 nest PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Start value: ")
    (radioButtons start
        ("" "  Cold Start - no values needed for {\it n,lamda,wrk} or {\it iwrk}" cold)
        ("" "  Warm Start - uses knots found in a previous call" warm))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02befSolve)
  htShowPage()

e02befSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  nest :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nest)
    objValUnwrap htpLabelSpadValue(htPage, 'nest)
  lwrk := 4*m +16*nest + 41
  s := htpLabelInputString(htPage,'s)
  initial := htpButtonValue(htPage,'start)
  start :=
    initial = 'cold => '1
    '2
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (m = 15 and start = 1) => e02befDefaultSolve (htPage,nest,lwrk,s,ifail)
  start = 1 => e02befColdSolve (htPage,m,nest,lwrk,s,ifail)
  -- warm start not really possible from hyperdoc
  -- as inputing a workspace array of dimension 1105 is asking too much
  -- user should use the command line, using the previous calculated 
  -- parameters
  htInitPage('"E02BEF - Least-squares curve cubic spline fit, automatic knot placement",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\blankline ")
    (text . "{\center{\it Hyperdoc interface not available for warm starts.}}")
    (text . "\newline ")
    (text . "{\center{\it Please use the command line.}}"))
  htMakeDoneButton('"Continue",'e02bef)
  htShowPage()



e02befColdSolve(htPage,m,nest,lwrk,s,ifail) ==
  labelList :=
    "append"/[f(i) for i in 1..m] where f(i) ==
      prefix := ('"\newline \tab{2} ")
      middle := ('"\tab{22} ")
      post   := ('" \tab{42} ")
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      ynam := INTERN STRCONC ('"y",STRINGIMAGE i)
      znam := INTERN STRCONC ('"z",STRINGIMAGE i)
      num := INTERN STRCONC (STRINGIMAGE (QUOTIENT(i,10)),".",STRINGIMAGE (REM(i,10)))
      [['text,:prefix],['bcStrings,[10, num, xnam, 'F]], 
       ['text,:middle],['bcStrings,[10, 0.0, ynam, 'F]],
        ['text,:post],['bcStrings,[10, 0.0, znam, 'F]]]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList]
  page := htInitPage('"E02BEF - Least-squares curve cubic spline fit, automatic knot placement",nil)
  htSay '"\menuitemstyle{}\tab{2} Values of \space{1} "
  htSay '"\htbitmap{xr}: \tab{20} \menuitemstyle{}\tab{22} "
  htSay '"Values of \space{1} \htbitmap{yr}: \tab{40}"
  htSay '"\menuitemstyle{}\tab{42} Values of \space{1} "
  htSay '"\htbitmap{wr}: "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02befColdGen)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'nest,nest)
  htpSetProperty(page,'lwrk,lwrk)
  htpSetProperty(page,'s,s)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02befDefaultSolve (htPage,nest,lwrk,s,ifail) ==
  m := 15
  page := htInitPage('"E02BEF - Least-squares curve cubic spline fit, automatic knot placement",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Values of \space{1} ")
    (text . "\htbitmap{xr}: \tab{20} \menuitemstyle{}\tab{22} ")
    (text . "Values of \space{1} \htbitmap{yr}: \tab{40} ")
    (text . "\menuitemstyle{}\tab{42} Values of \space{1} ")
    (text . "\htbitmap{wr}: ")
    (text . "\newline \tab{2}")
    (bcStrings (10 "0.00" x1 F))
    (text . "\tab{22} ")
    (bcStrings (10 "-1.1" y1 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z1 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "0.50" x2 F))
    (text . "\tab{22} ")
    (bcStrings (10 "-0.372" y2 F))
    (text . "\tab{42} ")
    (bcStrings (10 "2.00" z2 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.00" x3 F))
    (text . "\tab{22} ")
    (bcStrings (10 "0.431" y3 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.50" z3 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "1.50" x4 F))
    (text . "\tab{22} ")
    (bcStrings (10 "1.69" y4 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z4 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "2.00" x5 F))
    (text . "\tab{22} ")
    (bcStrings (10 "2.11" y5 F))
    (text . "\tab{42} ")
    (bcStrings (10 "3.00" z5 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "2.50" x6 F))
    (text . "\tab{22} ")
    (bcStrings (10 "3.10" y6 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z6 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "3.00" x7 F))
    (text . "\tab{22} ")
    (bcStrings (10 "4.23" y7 F))
    (text . "\tab{42} ")
    (bcStrings (10 "0.50" z7 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "4.00" x8 F))
    (text . "\tab{22} ")
    (bcStrings (10 "4.35" y8 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z8 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "4.50" x9 F))
    (text . "\tab{22} ")
    (bcStrings (10 "4.81" y9 F))
    (text . "\tab{42} ")
    (bcStrings (10 "2.00" z9 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "5.00" x10 F))
    (text . "\tab{22} ")
    (bcStrings (10 "4.61" y10 F))
    (text . "\tab{42} ")
    (bcStrings (10 "2.50" z10 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "5.50" x11 F))
    (text . "\tab{22} ")
    (bcStrings (10 "4.79" y11 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z11 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "6.00" x12 F))
    (text . "\tab{22} ")
    (bcStrings (10 "5.23" y12 F))
    (text . "\tab{42} ")
    (bcStrings (10 "3.00" z12 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "7.00" x13 F))
    (text . "\tab{22} ")
    (bcStrings (10 "6.35" y13 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z13 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "7.50" x14 F))
    (text . "\tab{22} ")
    (bcStrings (10 "7.19" y14 F))
    (text . "\tab{42} ")
    (bcStrings (10 "2.00" z14 F))
    (text . "\newline \tab{2} ")
    (bcStrings (10 "8.00" x15 F))
    (text . "\tab{22} ")
    (bcStrings (10 "7.97" y15 F))
    (text . "\tab{42} ")
    (bcStrings (10 "1.00" z15 F))
    (text . "\blankline"))
  htMakeDoneButton('"Continue",'e02befColdGen)          
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'nest,nest)
  htpSetProperty(page,'lwrk,lwrk)
  htpSetProperty(page,'s,s)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02befColdGen htPage ==
  m := htpProperty(htPage,'m)
  nest := htpProperty(htPage,'nest)
  lwrk := htpProperty(htPage,'lwrk)
  s := htpProperty(htPage,'s)
  cold := '"c"
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    mid :=  STRCONC ((first y).1," ")
    y := rest y
    left :=  STRCONC ((first y).1," ")
    y := rest y
    xlist := [left,:xlist]
    ylist := [mid,:ylist]
    wlist := [right,:wlist]
  xstring := bcwords2liststring xlist
  ystring := bcwords2liststring ylist
  wstring := bcwords2liststring wlist
  -- additional entries needed to get it running
  -- but as Start = c they are not used
  -- mmax := 50
  -- nest := mmax + 4 (54)
  -- lwrk := 4*mmax + 16*nest+41 (1105)
  prefix := STRCONC('"e02bef(_"",cold,"_",",STRINGIMAGE m,", [",xstring,"],[")
  prefix := STRCONC(prefix,ystring,"],[",wstring,"], ",STRINGIMAGE s,", ")
  prefix := STRCONC(prefix,STRINGIMAGE nest,", ",STRINGIMAGE lwrk)
--  prefix := STRCONC(prefix,",0, [[0.0 for i in 1..",STRINGIMAGE nest,"]],")
--  prefix := STRCONC(prefix,STRINGIMAGE ifail,", [[0.0 for i in 1..")
--  prefix := STRCONC(prefix,STRINGIMAGE lwrk,"]], [[0 for i in 1..")
--  prefix := STRCONC(prefix,STRINGIMAGE nest,"]] :: Matrix Integer)")
  prefix := STRCONC(prefix,",0, new(1,",STRINGIMAGE nest,",0.0)$Matrix DoubleFloat,")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,", new(1,",STRINGIMAGE lwrk,",0.0)$Matrix DoubleFloat, ")
  prefix := STRCONC(prefix," new(1,",STRINGIMAGE nest,",0)$Matrix Integer)")
  linkGen prefix

e02def() == 
  htInitPage('"E02DEF - Evaluation of a fitted bicubic spline at a vector of points",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02def} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02def| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Evaluates a bicubic spline at the (\htbitmap{xr},")
    (text . "\htbitmap{yr}), for r = 1,2,...,m, from its B-spline ")
    (text . "representation \htbitmap{e02daf} ")
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Number of evaluation points, {\it m}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 7 m PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Number of (interior & exterior) knots ")
    (text . "\lambda, \htbitmap{px} \htbitmap{great=} 8: \newline\tab{2} ")
    (bcStrings (6 11 px PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Number of (interior & exterior) knots ")
    (text . "\mu, \htbitmap{py} \htbitmap{great=} 8: \newline\tab{2} ")
    (bcStrings (6 10 py PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02defSolve)
  htShowPage()

e02defSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  px :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'px)
    objValUnwrap htpLabelSpadValue(htPage, 'px)
  py :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'py)
    objValUnwrap htpLabelSpadValue(htPage, 'py)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  ((m = '7 and px = '11) and py = '10) => e02defDefaultSolve(htPage,ifail)
  labelList :=
    "append"/[fxy(i) for i in 1..m] where fxy(i) ==
      prefix := ('"\newline \tab{2} ")
      middle := ('"\tab{22} ")
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      ynam := INTERN STRCONC ('"y",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[8, 0.0, xnam, 'F]], 
       ['text,:middle],['bcStrings,[8, 0.0, ynam, 'F]]]
  lamList :=
    "append"/[flam(i) for i in 1..px] where flam(i) ==
      lnam := INTERN STRCONC ('"l",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, lnam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} {\it \lambda(nxest)}: \newline")
  lamList := [['text,:prefix],:lamList]
  muList :=
    "append"/[fmu(i) for i in 1..(py)] where fmu(i) ==
      mnam := INTERN STRCONC ('"m",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, mnam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} {\it \mu(nyest)}:")
  prefix := STRCONC(prefix,"\newline ")
  muList := [['text,:prefix],:muList]
  cList :=
    "append"/[fp(i) for i in 1..((px-4)*(py-4))] where fp(i) ==
      pnam := INTERN STRCONC ('"p",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, pnam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{}\tab{2}Enter values of ")
  prefix := STRCONC(prefix,"{\it c((nxest*4)-(nyest*4))}: \newline ")
  cList := [['text,:prefix],:cList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList,:lamList,:muList,:cList]
  page := htInitPage('"E02DEF - Evaluation of a fitted bicubic spline at a vector of points",nil)
  htSay '"\menuitemstyle{}\tab{2} Values of \htbitmap{xr}: "
  htSay '"\tab{20} \menuitemstyle{}\tab{22} Values of \htbitmap{yr}:"
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02defGen)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'px,px)
  htpSetProperty(page,'py,py)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02defDefaultSolve (htPage,ifail) ==
  m := '7
  px := '11
  py := '10
  page := htInitPage('"E02DEF - Evaluation of a fitted bicubic spline at a vector of points",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Values of \htbitmap{xr}:")
    (text . "\tab{20} \menuitemstyle{} \tab{22} Values of ")
    (text . "\htbitmap{yr}: ")
    (text . "\newline \tab{2} ")
    (bcStrings (8 "1" x1 F))  
    (text . "\tab{22}")
    (bcStrings (8 "0" y1 F))  
    (text . "\newline \tab{2} ")
    (bcStrings (8 "1.1" x2 F))  
    (text . "\tab{22}")
    (bcStrings (8 "0.1" y2 F))  
    (text . "\newline \tab{2} ")
    (bcStrings (8 "1.5" x3 F))  
    (text . "\tab{22}")
    (bcStrings (8 "0.7" y3 F))  
    (text . "\newline \tab{2} ")
    (bcStrings (8 "1.6" x4 F))  
    (text . "\tab{22}")
    (bcStrings (8 "0.4" y4 F))  
    (text . "\newline \tab{2} ")
    (bcStrings (8 "1.9" x5 F))  
    (text . "\tab{22}")
    (bcStrings (8 "0.3" y5 F))  
    (text . "\newline \tab{2} ")
    (bcStrings (8 "1.9" x6 F))  
    (text . "\tab{22}")
    (bcStrings (8 "0.8" y6 F))  
    (text . "\newline \tab{2} ")
    (bcStrings (8 "2" x7 F))  
    (text . "\tab{22}")
    (bcStrings (8 "1" y7 F))  
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} {\it \lambda(nxest)}:")
    (text . "\newline ")
    (bcStrings (8 "1.0" l1 F))
    (bcStrings (8 "1.0" l2 F))
    (bcStrings (8 "1.0" l3 F))
    (bcStrings (8 "1.0" l4 F))
    (bcStrings (8 "1.3" l5 F))
    (bcStrings (8 "1.5" l6 F))
    (bcStrings (8 "1.6" l7 F))
    (bcStrings (8 "2" l8 F))
    (bcStrings (8 "2" l9 F))
    (bcStrings (8 "2" l10 F))
    (bcStrings (8 "2" l11 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} {\it \mu(nyest)}:")
    (text . "\newline ")
    (bcStrings (8 "0" mu1 F))
    (bcStrings (8 "0" mu2 F))
    (bcStrings (8 "0" mu3 F))
    (bcStrings (8 "0" mu4 F))
    (bcStrings (8 "0.4" mu5 F))
    (bcStrings (8 "0.7" mu6 F))
    (bcStrings (8 "1" mu7 F))
    (bcStrings (8 "1" mu8 F))
    (bcStrings (8 "1" mu9 F))
    (bcStrings (8 "1" mu10 F))
    (text . "\blankline \menuitemstyle{}\tab{2} ")
    (text . "Enter values for {\it c((nxest-4)*(nyest-4))}:")
    (text . "\newline ")
    (bcStrings (8 "1" c1 F))    
    (bcStrings (8 "1.1333" c2 F))       
    (bcStrings (8 "1.3667" c3 F))       
    (bcStrings (8 "1.7" c4 F))  
    (bcStrings (8 "1.9" c5 F))  
    (bcStrings (8 "2" c6 F))    
    (bcStrings (8 "1.2" c7 F))  
    (bcStrings (8 "1.3333" c8 F))       
    (bcStrings (8 "1.5667" c9 F))       
    (bcStrings (8 "1.9" c10 F)) 
    (bcStrings (8 "2.1" c11 F)) 
    (bcStrings (8 "2.2" c12 F)) 
    (bcStrings (8 "1.5833" c13 F))      
    (bcStrings (8 "1.7167" c14 F))      
    (bcStrings (8 "1.95" c15 F))        
    (bcStrings (8 "2.2833" c16 F))      
    (bcStrings (8 "2.4833" c17 F))      
    (bcStrings (8 "2.5833" c18 F))      
    (bcStrings (8 "2.1433" c19 F))      
    (bcStrings (8 "2.2767" c20 F))      
    (bcStrings (8 "2.51" c21 F))        
    (bcStrings (8 "2.8433" c22 F))      
    (bcStrings (8 "3.0433" c23 F))      
    (bcStrings (8 "3.1433" c24 F))      
    (bcStrings (8 "2.8667" c25 F))      
    (bcStrings (8 "3" c26 F))   
    (bcStrings (8 "3.2333" c27 F))      
    (bcStrings (8 "3.5667" c28 F))      
    (bcStrings (8 "3.7667" c29 F))      
    (bcStrings (8 "3.8667" c30 F))      
    (bcStrings (8 "3.4667" c31 F))      
    (bcStrings (8 "3.6" c32 F)) 
    (bcStrings (8 "3.8333" c33 F))      
    (bcStrings (8 "4.1667" c34 F))      
    (bcStrings (8 "4.3667" c35 F))      
    (bcStrings (8 "4.4667" c36 F))      
    (bcStrings (8 "4" c37 F))   
    (bcStrings (8 "4.1333" c38 F))      
    (bcStrings (8 "4.3667" c39 F))      
    (bcStrings (8 "4.7" c40 F)) 
    (bcStrings (8 "4.9" c41 F)) 
    (bcStrings (8 "5" c42 F))   
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'e02defGen)      
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'px,px)
  htpSetProperty(page,'py,py)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02defGen htPage ==
  m := htpProperty(htPage,'m)
  px := htpProperty(htPage,'px)
  py := htpProperty(htPage,'py)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  -- c
  for i in 1..((px-4)*(py-4)) repeat
    right := STRCONC ((first y).1," ")
    y := rest y
    cList := [right,:cList]
  cstring := bcwords2liststring cList
  -- mu 
  for i in 1..py repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    muList := [right,:muList]
  mustring := bcwords2liststring muList
  -- lamda
  for i in 1..px repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    lamList := [right,:lamList]
  lamstring := bcwords2liststring lamList
  -- x & y
  while y repeat
    one := STRCONC((first y).1," ")
    y := rest y
    two := STRCONC((first y).1," ")
    y := rest y
    xlist := [two,:xlist]
    ylist := [one,:ylist]
  xstring := bcwords2liststring xlist
  ystring := bcwords2liststring ylist
  prefix := STRCONC('"e02def(",STRINGIMAGE m,", ",STRINGIMAGE px,", ")
  prefix := STRCONC(prefix,STRINGIMAGE py,",[",xstring,"],[",ystring,"],[")
  prefix := STRCONC(prefix,lamstring,"],[",mustring,"],[",cstring,"],")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,")")
  linkGen prefix


e02dff() == 
  htInitPage('"E02DFF - Evaluation of a fitted bicubic spline at a mesh of points",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02dff} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02dff| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Evaluates a bicubic spline at all the points on a rectangular ")
    (text . "grid defined by \htbitmap{mx} points ")
    (text . "\htbitmap{xq}on the x-axis and \htbitmap{my}")
    (text . "points \htbitmap{yr} on the y-axis, from its B-spline ")
    (text . "representation \center{\htbitmap{e02daf}} \newline with knot sets ")
    (text . "\{\lambda\} and \{\mu\}. ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline Grid points on x-axis \htbitmap{mx}: ")
    (text . "\tab{32} \menuitemstyle{}\tab{34}")
    (text . "Grid points on y-axis \htbitmap{my}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 7 mx PI))
    (text . "\tab{34} ")
    (bcStrings (6 6 my PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "Number of (interior & exterior) knots \lambda, ")
    (text . "\htbitmap{px} \htbitmap{great=} 8: \newline\tab{2} ")
    (bcStrings (6 11 px PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "Number of (interior & exterior) knots \mu, ")
    (text . "\htbitmap{py} \htbitmap{great=} 8: \newline\tab{2} ")
    (bcStrings (6 10 py PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02dffSolve)
  htShowPage()

e02dffSolve htPage ==
  mx :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'mx)
    objValUnwrap htpLabelSpadValue(htPage, 'mx)
  my :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'my)
    objValUnwrap htpLabelSpadValue(htPage, 'my)
  px :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'px)
    objValUnwrap htpLabelSpadValue(htPage, 'px)
  py :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'py)
    objValUnwrap htpLabelSpadValue(htPage, 'py)
  nwrk1 := 4*mx + px
  nwrk2 := 4*my + py
  nwrklist := [nwrk1,nwrk2]
  nwrkmin := APPLY ('MIN, nwrklist)
  lwrk := nwrkmin
  liwrk :=
    nwrkmin = nwrk2 => my + py -4
    mx + px -4
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  ((mx = '7 and my = '6) and (px = '11 and py = '10)) => 
    e02dffDefaultSolve(htPage,lwrk,liwrk,ifail)
  xList :=
    "append"/[fx(i) for i in 1..mx] where fx(i) ==
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, xnam, 'F]]]
  yList :=
    "append"/[fy(i) for i in 1..my] where fy(i) ==
      ynam := INTERN STRCONC ('"y",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, ynam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} Enter values of ")
  prefix := STRCONC(prefix,"\htbitmap{yr} : \newline")
  yList := [['text,:prefix],:yList]
  lamList :=
    "append"/[flam(i) for i in 1..px] where flam(i) ==
      lnam := INTERN STRCONC ('"l",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, lnam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{}\tab{2} {\it \lambda(nxest)}:\newline")
  lamList := [['text,:prefix],:lamList]
  muList :=
    "append"/[fmu(i) for i in 1..(py)] where fmu(i) ==
      mnam := INTERN STRCONC ('"m",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, mnam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} {\it mu(nyest)}:")
  prefix := STRCONC(prefix,"\newline ")
  muList := [['text,:prefix],:muList]
  cList :=
    "append"/[fp(i) for i in 1..((px-4)*(py-4))] where fp(i) ==
      pnam := INTERN STRCONC ('"p",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, pnam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{}\tab{2} Enter values of ")
  prefix := STRCONC(prefix,"{\it c((px-4)*(py-4))}: \newline")
  cList := [['text,:prefix],:cList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :xList,:yList,:lamList,:muList,:cList]
  page := htInitPage('"E02DFF - Evaluation of a fitted bicubic spline at a mesh of points",nil)
  htSay '"\menuitemstyle{}\tab{2} Values of \htbitmap{xr}:\newline "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02dffGen)
  htpSetProperty(page,'mx,mx)
  htpSetProperty(page,'my,my)
  htpSetProperty(page,'px,px)
  htpSetProperty(page,'py,py)
  htpSetProperty(page,'lwrk,lwrk)
  htpSetProperty(page,'liwrk,liwrk)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02dffDefaultSolve (htPage,lwrk,liwrk,ifail) ==
  mx := '7
  my := '6
  px := '11
  py := '10
  page := htInitPage('"E02DFF - Evaluation of a fitted bicubic spline at a mesh of points",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of \htbitmap{xr}:")
    (text . "\newline ")
    (bcStrings (8 "1" x1 F))  
    (bcStrings (8 "1.1" x2 F))  
    (bcStrings (8 "1.3" x3 F))  
    (bcStrings (8 "1.4" x4 F))  
    (bcStrings (8 "1.5" x5 F))  
    (bcStrings (8 "1.7" x6 F))  
    (bcStrings (8 "2" x7 F))  
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of \htbitmap{yr}:")
    (text . "\newline ")
    (bcStrings (8 "0" y1 F))  
    (bcStrings (8 "0.2" y2 F))  
    (bcStrings (8 "0.4" y3 F))  
    (bcStrings (8 "0.6" y4 F))  
    (bcStrings (8 "0.8" y5 F))  
    (bcStrings (8 "1" y6 F))  
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it \lambda(nxest)}:")
    (text . "\newline ")
    (bcStrings (8 "1" l1 F))
    (bcStrings (8 "1" l2 F))
    (bcStrings (8 "1" l3 F))
    (bcStrings (8 "1" l4 F))
    (bcStrings (8 "1.3" l5 F))
    (bcStrings (8 "1.5" l6 F))
    (bcStrings (8 "1.6" l7 F))
    (bcStrings (8 "2" l8 F))
    (bcStrings (8 "2" l9 F))
    (bcStrings (8 "2" l10 F))
    (bcStrings (8 "2" l11 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it \mu(nyest)}:")
    (text . "\newline ")
    (bcStrings (8 "0" mu1 F))
    (bcStrings (8 "0" mu2 F))
    (bcStrings (8 "0" mu3 F))
    (bcStrings (8 "0" mu4 F))
    (bcStrings (8 "0.4" mu5 F))
    (bcStrings (8 "0.7" mu6 F))
    (bcStrings (8 "1" mu7 F))
    (bcStrings (8 "1" mu8 F))
    (bcStrings (8 "1" mu9 F))
    (bcStrings (8 "1" mu10 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} Enter values of {\it c((px-4)*(py-4))}:")
    (text . "\newline ")
    (bcStrings (8 "1" c1 F))    
    (bcStrings (8 "1.1333" c2 F))       
    (bcStrings (8 "1.3667" c3 F))       
    (bcStrings (8 "1.7" c4 F))  
    (bcStrings (8 "1.9" c5 F))  
    (bcStrings (8 "2" c6 F))    
    (bcStrings (8 "1.2" c7 F))  
    (bcStrings (8 "1.3333" c8 F))       
    (bcStrings (8 "1.5667" c9 F))       
    (bcStrings (8 "1.9" c10 F)) 
    (bcStrings (8 "2.1" c11 F)) 
    (bcStrings (8 "2.2" c12 F)) 
    (bcStrings (8 "1.5833" c13 F))      
    (bcStrings (8 "1.7167" c14 F))      
    (bcStrings (8 "1.95" c15 F))        
    (bcStrings (8 "2.2833" c16 F))      
    (bcStrings (8 "2.4833" c17 F))      
    (bcStrings (8 "2.5833" c18 F))      
    (bcStrings (8 "2.1433" c19 F))      
    (bcStrings (8 "2.2767" c20 F))      
    (bcStrings (8 "2.51" c21 F))        
    (bcStrings (8 "2.8433" c22 F))      
    (bcStrings (8 "3.0433" c23 F))      
    (bcStrings (8 "3.1433" c24 F))      
    (bcStrings (8 "2.8667" c25 F))      
    (bcStrings (8 "3" c26 F))   
    (bcStrings (8 "3.2333" c27 F))      
    (bcStrings (8 "3.5667" c28 F))      
    (bcStrings (8 "3.7667" c29 F))      
    (bcStrings (8 "3.8667" c30 F))      
    (bcStrings (8 "3.4667" c31 F))      
    (bcStrings (8 "3.6" c32 F)) 
    (bcStrings (8 "3.8333" c33 F))      
    (bcStrings (8 "4.1667" c34 F))      
    (bcStrings (8 "4.3667" c35 F))      
    (bcStrings (8 "4.4667" c36 F))      
    (bcStrings (8 "4" c37 F))   
    (bcStrings (8 "4.1333" c38 F))      
    (bcStrings (8 "4.3667" c39 F))      
    (bcStrings (8 "4.7" c40 F)) 
    (bcStrings (8 "4.9" c41 F)) 
    (bcStrings (8 "5" c42 F))   
    (text . "\blankline"))
  htMakeDoneButton('"Continue",'e02dffGen)      
  htpSetProperty(page,'mx,mx)
  htpSetProperty(page,'my,my)
  htpSetProperty(page,'px,px)
  htpSetProperty(page,'py,py)
  htpSetProperty(page,'lwrk,lwrk)
  htpSetProperty(page,'liwrk,liwrk)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02dffGen htPage ==
  mx := htpProperty(htPage,'mx)
  my := htpProperty(htPage,'my)
  px := htpProperty(htPage,'px)
  py := htpProperty(htPage,'py)
  lwrk := htpProperty(htPage,'lwrk)
  liwrk := htpProperty(htPage,'liwrk)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  -- c
  for i in 1..((px-4)*(py-4)) repeat
    right := STRCONC ((first y).1," ")
    y := rest y
    cList := [right,:cList]
  cstring := bcwords2liststring cList
  -- mu 
  for i in 1..py repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    muList := [right,:muList]
  mustring := bcwords2liststring muList
  -- lamda
  for i in 1..px repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    lamList := [right,:lamList]
  lamstring := bcwords2liststring lamList
  -- y
  for i in 1..my repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    yList := [right,:yList]
  ystring := bcwords2liststring yList
  -- x 
  for i in 1..mx repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    xList := [right,:xList]
  xstring := bcwords2liststring xList
  prefix := STRCONC('"e02dff(",STRINGIMAGE mx,", ",STRINGIMAGE my,", ")
  prefix := STRCONC(prefix,STRINGIMAGE px,", ",STRINGIMAGE py,",[")
  prefix := STRCONC(prefix,xstring,"],[",ystring,"],[",lamstring,"],[")
  prefix := STRCONC(prefix,mustring,"],[",cstring,"],",STRINGIMAGE lwrk,", ")
  prefix := STRCONC(prefix,STRINGIMAGE liwrk,", ",STRINGIMAGE ifail,")")
  linkGen prefix

e02gaf() ==
  htInitPage('"E02GAF - \htbitmap{l1}-approximation by general linear function",nil)
  htMakePage '(
    (domainConditions 
      (isDomain F (Float))
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02gaf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02gaf| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Calculates an \htbitmap{l1} solution to the over determined system")
    (text . " of linear equations {\it Ax = b}, where A is an {\it m} by {\it n")
    (text . "} matrix, {\it x} is an {\it n} element vector, and {\it b} is an ")
    (text . "{\it m} element vector.  The matrix {\it A} need not be of full ")
    (text . "rank. \blankline ")
    (text . "\menuitemstyle{}\tab{2} \newline ")
    (text . "Number of rows of {\it A}, {\it m}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 5 m PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} \newline ")
    (text . "Number of columns of {\it A}, {\it n}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 3 n PI))
--    (text . "\blankline ")
--    (text . "\menuitemstyle{}\tab{2} \newline ")
--    (text . "First dimension of {\it A(la,n+2)}, {\it la}\htbitmap{great=}")
--    (text . " {\it m + 2}: \newline\tab{2} ")
--    (bcStrings (6 7 la PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} \newline ")
    (text . "Tolerance (default is zero), {\it toler}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (10 "0.0" toler F))
    (text . "\blankline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02gafSolve)
  htShowPage()

e02gafSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  la := m+2
--    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'la)
--    objValUnwrap htpLabelSpadValue(htPage, 'la)
  toler := htpLabelInputString(htPage,'toler)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  ((m = 5 and n = 3) and la = 7) => e02gafDefaultSolve (htPage,toler,ifail) 
  labelList :=
    "append"/[fc(i,n) for i in 1..la] where fc(i,n) ==
      tempList := 
        "append"/[fr(i,j) for j in 1..(n+2)] where fr(i,j) == 
          fnam := INTERN STRCONC ('"f",STRINGIMAGE i, STRINGIMAGE j)
          [['bcStrings,[9, 0.0, fnam, 'F]]]
      prefix := ('"\newline ")
      tempList := [['text,:prefix],:tempList]
  bList := 
    "append"/[fb(i) for i in 1..m] where fb(i) ==
      lnam := INTERN STRCONC ('"l",STRINGIMAGE i)
      [['bcStrings,[9, 0.0, lnam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} Values of {\it B(m)}: \newline")
  bList := [['text,:prefix],:bList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList,:bList]
  page := htInitPage('"E02GAF - \htbitmap{l1}-approximation by general linear function",nil)
  htSay '"\menuitemstyle{}\tab{2} Values of {\it A(la,n+2)}:"
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02gafGen)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'la,la)
  htpSetProperty(page,'toler,toler)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02gafDefaultSolve (htPage,toler,ifail) ==
  m := '5
  n := '3
  la := '7
  page := htInitPage('"E02GAF - \htbitmap{l1}-approximation by general linear function",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Values of {\it A(la,n+2)}:")
    (text . "\newline ")
    (bcStrings (9 "1.0" a11 F))  
    (bcStrings (9 "1.0" a12 F))  
    (bcStrings (9 "1.0" a13 F))  
    (bcStrings (9 "0.0" a14 F))  
    (bcStrings (9 "0.0" a15 F))  
    (text . "\newline ")
    (bcStrings (9 "exp(0.2)" a21 F))  
    (bcStrings (9 "exp(-0.2)" a22 F))  
    (bcStrings (9 "1.0" a23 F))  
    (bcStrings (9 "0.0" a24 F))  
    (bcStrings (9 "0.0" a25 F))  
    (text . "\newline ")
    (bcStrings (9 "exp(0.4)" a31 F))  
    (bcStrings (9 "exp(-0.4)" a32 F))  
    (bcStrings (9 "1.0" a33 F))  
    (bcStrings (9 "0.0" a34 F))  
    (bcStrings (9 "0.0" a35 F))  
    (text . "\newline ")
    (bcStrings (9 "exp(0.6)" a41 F))  
    (bcStrings (9 "exp(-0.6)" a42 F))  
    (bcStrings (9 "1.0" a43 F))  
    (bcStrings (9 "0.0" a44 F))  
    (bcStrings (9 "0.0" a45 F))  
    (text . "\newline ")
    (bcStrings (9 "exp(0.8)" a51 F))  
    (bcStrings (9 "exp(-0.8)" a52 F))  
    (bcStrings (9 "1.0" a53 F))  
    (bcStrings (9 "0.0" a54 F))  
    (bcStrings (9 "0.0" a55 F))  
    (text . "\newline ")
    (bcStrings (9 "0.0" a61 F))  
    (bcStrings (9 "0.0" a62 F))  
    (bcStrings (9 "0.0" a63 F))  
    (bcStrings (9 "0.0" a64 F))  
    (bcStrings (9 "0.0" a65 F))  
    (text . "\newline ")
    (bcStrings (9 "0.0" a71 F))  
    (bcStrings (9 "0.0" a72 F))  
    (bcStrings (9 "0.0" a73 F))  
    (bcStrings (9 "0.0" a74 F))  
    (bcStrings (9 "0.0" a75 F))  
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} Values of {\it B(m)}:")
    (text . "\newline ")
    (bcStrings (9 "4.501" b1 F))  
    (bcStrings (9 "4.36" b2 F))  
    (bcStrings (9 "4.333" b3 F))  
    (bcStrings (9 "4.418" b4 F))  
    (bcStrings (9 "4.625" b5 F)))  
  htMakeDoneButton('"Continue",'e02gafGen)      
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'toler,toler)
  htpSetProperty(page,'la,la)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02gafGen htPage ==
  m := htpProperty(htPage,'m)
  n := htpProperty(htPage,'n)
  la := htpProperty(htPage,'la)
  toler := htpProperty(htPage,'toler)
  ifail := htpProperty(htPage,'ifail)
  nplustwo := n + 2
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..m repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    blist := [right,:blist]    
  bstring := bcwords2liststring blist
  y := REVERSE y
  k := -1
  matform := [[y.(k := k + 1).1 for j in 0..(nplustwo-1)] for i in 0..(la-1)]
  matstring := bcwords2liststring [bcwords2liststring x for x in matform] 
  prefix := STRCONC('"e02gaf(",STRINGIMAGE m,", ",STRINGIMAGE la,", ")
  prefix := STRCONC(prefix,STRINGIMAGE nplustwo,", ",STRINGIMAGE toler,", ")
  prefix := STRCONC(prefix,matstring,",[",bstring,"], ")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,")")
  linkGen prefix


e02daf() == 
  htInitPage('"E02DAF - Least-squares surface fit, bicubic splines",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02daf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02daf| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Determines a minimal, least squares bicubic B-spline surface fit")
    (text . "\htbitmap{e02daf} to the set of points ")
    (text . "{\em (\htbitmap{xr},\htbitmap{yr},\htbitmap{fr})} with weights ")
    (text . "\htbitmap{wr}, for r = 1,2,...,m. The user must supply internal ")
    (text . "knot sets {\lambda},in the x-direction and {\mu} in the ")
    (text . "y-direction, ")
    (text . "which can be thought of as dividing the data region into panels;")
    (text . "s(x,y) consists of a seperate bicubic polynomial in each panel, ")
    (text . "the polynomial joining together with second derivative ")
    (text . "continuity. Eight additional (external) knots are added to each ")
    (text . "of the knot sets by this routine. The routine minimizes \Sigma, ")
    (text . "the sum of squares of the weighted residuals ")
    (text . "\htbitmap{e02daf1}, for r = 1,2,...,m, subject to the ")
    (text . "given knot sets. \newline ")
    (text . "A call of this routine should be preceded by a call of E02ZAF ")
    (text . "to provide indexing information. ")
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Number of data points {\it m}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 30 m PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline Knots in x direction {\em px}")
    (text . "\htbitmap{great=} 8: \tab{32} \menuitemstyle{}\tab{34}")
    (text . "Knots in y direction {\em py}:")
    (text . "\newline\tab{2} ")
    (bcStrings (6 8 px PI))
    (text . "\tab{34} ")
    (bcStrings (6 10 py PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline Rank threshold {\em eps}:")
    (text . "\newline\tab{2} ")
    (bcStrings (10 "0.000001" eps F))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Dimension of point {\it npoint}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 43 npoint PI))
    -- include a radio button later to allow switching of
    -- x & y if px <= py 
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02dafSolve)
  htShowPage()

e02dafSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  px :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'px)
    objValUnwrap htpLabelSpadValue(htPage, 'px)
  py :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'py)
    objValUnwrap htpLabelSpadValue(htPage, 'py)
  npoint :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'npoint)
    objValUnwrap htpLabelSpadValue(htPage, 'npoint)
  nc := (px - 4)*(py - 4)
  nws := (2*nc + 1)*(3*py - 6) -2
  eps := htpLabelInputString(htPage,'eps)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  ((m = '30 and px = '8) and py = '10) => e02dafDefaultSolve(htPage,eps,nws,npoint,ifail)
  labelList :=
    "append"/[fxy(i) for i in 1..m] where fxy(i) ==
      prefix := ('"\newline \tab{2} ")
      middle := ('"\tab{17} ")
      next := ('"\tab{32} ")
      end := ('"\tab{47} ")
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      ynam := INTERN STRCONC ('"y",STRINGIMAGE i)
      fnam := INTERN STRCONC ('"f",STRINGIMAGE i)
      wnam := INTERN STRCONC ('"w",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[8, 0.0, xnam, 'F]], 
       ['text,:middle],['bcStrings,[8, 0.0, ynam, 'F]],
         ['text,:next],['bcStrings,[8, 0.0, fnam, 'F]],
           ['text,:end],['bcStrings,[8, 0.0, wnam, 'F]]]
  lamList :=
    "append"/[flam(i) for i in 5..(px-4)] where flam(i) ==
      lnam := INTERN STRCONC ('"l",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, lnam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} lamda(5) to lamda(px-4): ")
  prefix := STRCONC(prefix,"\newline \tab{2} ")
  postfix := ('"\newline \blankline ")
  lamList := [['text,:prefix],:lamList,['text,:postfix]]
  muList :=
    "append"/[fmu(i) for i in 5..(py-4)] where fmu(i) ==
      mnam := INTERN STRCONC ('"m",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, mnam, 'F]]]
  prefix := ('"\menuitemstyle{} \tab{2} mu(5) to mu(py-4):")
  prefix := STRCONC(prefix,"\newline \tab{2} ")
  muList := [['text,:prefix],:muList]
  pList :=
    "append"/[fp(i) for i in 1..npoint] where fp(i) ==
      prefix := ('"\newline \tab{2} ")
      pnam := INTERN STRCONC ('"p",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[8, 0.0, pnam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} Enter values of Point: ")
  pList := [['text,:prefix],:pList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList,:lamList,:muList,:pList]
  page := htInitPage('"E02DAF - Least-squares surface fit, bicubic splines",nil)
  htSay '"\menuitemstyle{}\tab{2} Values of \htbitmap{xr}: "
  htSay '"\tab{15} \menuitemstyle{}\tab{17} Values of \htbitmap{yr}:"
  htSay '"\tab{30} \menuitemstyle{}\tab{32} Values of \htbitmap{fr}:" 
  htSay '"\tab{44} \menuitemstyle{}\tab{46} Values of \htbitmap{wr}:"
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02dafGen)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'px,px)
  htpSetProperty(page,'py,py)
  htpSetProperty(page,'nws,nws)
  htpSetProperty(page,'eps,eps)
  htpSetProperty(page,'npoint,npoint)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02dafDefaultSolve (htPage,eps,nws,npoint,ifail) ==
  m := '30
  px := '8
  py := '10
  page := htInitPage('"E02DAF - Least-squares surface fit, bicubic splines",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Values of \htbitmap{xr}:")
    (text . "\tab{15} \menuitemstyle{} \tab{17} Values of ")
    (text . "\htbitmap{yr}: \tab{30} \menuitemstyle{} \tab{32} ")
    (text . "Values of \htbitmap{fr}: \tab{44} \menuitemstyle{} ")
    (text . "\tab{46} Values of \htbitmap{wr}:")
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.52" x1 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.60" y1 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.93" f1 F))    
    (text . "\tab{47}")
    (bcStrings (8 "10" w1 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.61" x2 F))  
    (text . "\tab{17}")
    (bcStrings (8 "-0.95" y2 F))    
    (text . "\tab{32}")
    (bcStrings (8 "-1.79" f2 F))    
    (text . "\tab{47}")
    (bcStrings (8 "10" w2 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.93" x3 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.87" y3 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.36" f3 F))    
    (text . "\tab{47}")
    (bcStrings (8 "10" w3 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.09" x4 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.84" y4 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.52" f4 F))    
    (text . "\tab{47}")
    (bcStrings (8 "10" w4 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.88" x5 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.17" y5 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.49" f5 F))    
    (text . "\tab{47}")
    (bcStrings (8 "10" w5 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.70" x6 F))  
    (text . "\tab{17}")
    (bcStrings (8 "-0.87" y6 F))    
    (text . "\tab{32}")
    (bcStrings (8 "-1.76" f6 F))    
    (text . "\tab{47}")
    (bcStrings (8 "10" w6 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "1" x7 F))  
    (text . "\tab{17}")
    (bcStrings (8 "1" y7 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.33" f7 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w7 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "1" x8 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.1" y8 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.48" f8 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w8 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.3" x9 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.24" y9 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.65" f9 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w9 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.77" x10 F))  
    (text . "\tab{17}")
    (bcStrings (8 "-0.77" y10 F))    
    (text . "\tab{32}")
    (bcStrings (8 "-1.82" f10 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w10 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.23" x11 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.32" y11 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.92" f11 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w11 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-1" x12 F))  
    (text . "\tab{17}")
    (bcStrings (8 "1" y12 F))    
    (text . "\tab{32}")
    (bcStrings (8 "1" f12 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w12 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.26" x13 F))  
    (text . "\tab{17}")
    (bcStrings (8 "-0.63" y13 F))    
    (text . "\tab{32}")
    (bcStrings (8 "8.88" f13 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w13 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.83" x14 F))  
    (text . "\tab{17}")
    (bcStrings (8 "-0.66" y14 F))    
    (text . "\tab{32}")
    (bcStrings (8 "-2.01" f14 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w14 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.22" x15 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.93" y15 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.47" f15 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w15 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.89" x16 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.15" y16 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.49" f16 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w16 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.80" x17 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.99" y17 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.84" f17 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w17 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.88" x18 F))  
    (text . "\tab{17}")
    (bcStrings (8 "-0.54" y18 F))    
    (text . "\tab{32}")
    (bcStrings (8 "-2.42" f18 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w18 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.68" x19 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.44" y19 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.47" f19 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w19 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.14" x20 F))  
    (text . "\tab{17}")
    (bcStrings (8 "-0.72" y20 F))    
    (text . "\tab{32}")
    (bcStrings (8 "7.15" f20 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w20 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.67" x21 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.63" y21 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.44" f21 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w21 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.90" x22 F))  
    (text . "\tab{17}")
    (bcStrings (8 "-0.40" y22 F))    
    (text . "\tab{32}")
    (bcStrings (8 "-3.34" f22 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w22 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.84" x23 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.20" y23 F))    
    (text . "\tab{32}")
    (bcStrings (8 "2.78" f23 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w23 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.84" x24 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.43" y24 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.44" f24 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w24 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.15" x25 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.28" y25 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.70" f25 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w25 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.91" x26 F))  
    (text . "\tab{17}")
    (bcStrings (8 "-0.24" y26 F))    
    (text . "\tab{32}")
    (bcStrings (8 "-6.52" f26 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w26 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.35" x27 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.86" y27 F))    
    (text . "\tab{32}")
    (bcStrings (8 "0.66" f27 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w27 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.16" x28 F))  
    (text . "\tab{17}")
    (bcStrings (8 "-0.41" y28 F))    
    (text . "\tab{32}")
    (bcStrings (8 "2.32" f28 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w28 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-0.35" x29 F))  
    (text . "\tab{17}")
    (bcStrings (8 "-0.05" y29 F))    
    (text . "\tab{32}")
    (bcStrings (8 "1.66" f29 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w29 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "-1" x30 F))  
    (text . "\tab{17}")
    (bcStrings (8 "-1" y30 F))    
    (text . "\tab{32}")
    (bcStrings (8 "-1" f30 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w30 F))    
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} mu(5) to mu(py-4):")
    (text . "\newline \tab{2}")
    (bcStrings (8 "-0.50" mu5 F))
    (bcStrings (8 "0.00" mu6 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} Enter values for point:")
    (text . "\newline \tab{2}")
    (bcStrings (6 3 p1 PI))     
    (text . "\newline \tab{2}")
    (bcStrings (6 6 p2 PI))     
    (text . "\newline \tab{2}")
    (bcStrings (6 4 p3 PI))     
    (text . "\newline \tab{2}")
    (bcStrings (6 5 p4 PI))     
    (text . "\newline \tab{2}")
    (bcStrings (6 7 p5 PI))     
    (text . "\newline \tab{2}")
    (bcStrings (6 10 p6 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 8 p7 PI))     
    (text . "\newline \tab{2}")
    (bcStrings (6 9 p8 PI))     
    (text . "\newline \tab{2}")
    (bcStrings (6 11 p9 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 13 p10 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 12 p11 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 15 p12 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 14 p13 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 18 p14 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 16 p15 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 17 p16 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 19 p17 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 20 p18 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 21 p19 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 30 p20 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 23 p21 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 26 p22 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 24 p23 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 25 p24 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 27 p25 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 28 p26 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 0 p27 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 29 p28 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 0 p29 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 0 p30 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 2 p31 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 22 p32 PI))   
    (text . "\newline \tab{2}")
    (bcStrings (6 1 p33 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 0 p34 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 0 p35 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 0 p36 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 0 p37 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 0 p38 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 0 p39 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 0 p40 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 0 p41 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 0 p42 PI))    
    (text . "\newline \tab{2}")
    (bcStrings (6 0 p43 PI))    
    (text . "\blankline"))
  htMakeDoneButton('"Continue",'e02dafGen)      
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'px,px)
  htpSetProperty(page,'py,py)
  htpSetProperty(page,'nws,nws)
  htpSetProperty(page,'eps,eps)
  htpSetProperty(page,'npoint,npoint)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02dafGen htPage ==
  m := htpProperty(htPage,'m)
  px := htpProperty(htPage,'px)
  py := htpProperty(htPage,'py)
  nws := htpProperty(htPage,'nws)
  eps := htpProperty(htPage,'eps)
  npoint := htpProperty(htPage,'npoint)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  nadres := (px-7)*(py-7)
  -- point
  for i in 1..npoint repeat
    right := STRCONC ((first y).1," ")
    y := rest y
    pointList := [right,:pointList]
  pstring := bcwords2liststring pointList
  -- mu 
  for i in 1..4 repeat
    muList := ['"0 ",:muList]
  for i in 5..(py-4) repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    muList := [right,:muList]
  for i in (py-3)..py repeat
    muList := ['"0 ",:muList]
  mustring := bcwords2liststring muList
  -- lamda
  for i in 1..4 repeat
    lamList := ['"0 ",:lamList]
  for i in 5..(px-4) repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    lamList := [right,:lamList]
  for i in (px-3)..px repeat
    lamList := ['"0 ",:lamList]
  lamstring := bcwords2liststring lamList
  -- x & y
  while y repeat
    one := STRCONC((first y).1," ")
    y := rest y
    two := STRCONC((first y).1," ")
    y := rest y
    three := STRCONC ((first y).1," ")
    y := rest y    
    four :=  STRCONC ((first y).1," ")
    y := rest y
    xlist := [four,:xlist]
    ylist := [three,:ylist]
    flist := [two,:flist]
    wlist := [one,:wlist]
  xstring := bcwords2liststring xlist
  ystring := bcwords2liststring ylist
  fstring := bcwords2liststring flist
  wstring := bcwords2liststring wlist
  nc := (px-4)*(py-4)
  prefix := STRCONC('"e02daf(",STRINGIMAGE m,", ",STRINGIMAGE px,", ")
  prefix := STRCONC(prefix,STRINGIMAGE py,",[",xstring,"],[",ystring,"],[")
  prefix := STRCONC(prefix,fstring,"],[",wstring,"],[",mustring,"],[")
  prefix := STRCONC(prefix,pstring,"], ",STRINGIMAGE npoint,", ")
  prefix := STRCONC(prefix,STRINGIMAGE nc,", ",STRINGIMAGE nws,", ",eps,", [")
  prefix := STRCONC(prefix,lamstring,"], ",STRINGIMAGE ifail,")")
  linkGen prefix


e02dcf() ==
  htInitPage('"E02DCF - Least-squares curve by bicubic splines with automatic knot placement, data on a rectangular grid",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02dcf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02dcf| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Determines a bicubic spline approximation to a set of points ")
    (text . "given on a rectangular grid defined by \htbitmap{mx} ")
    (text . "points \htbitmap{xq} on the x-axis and ")
    (text . "\htbitmap{my} points \htbitmap{yr} on the ")
    (text . "y-axix. The knots \htbitmap{lamdai}, for i = 1,2,...,")
    (text . "\htbitmap{nx} and \htbitmap{mui}, for ")
    (text . "i = 1,2,...,\htbitmap{ny} are chosen for this routine ")
    (text . ", but a single parameter S must be specified to control the ")
    (text . "trade-off between closeness of fit and smoothness of fit.  This ")
    (text . "affects the number of knots required by the spline, which is ")
    (text . "given in the B-spline representation \htbitmap{e02daf}")
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Grid points on x-axis \htbitmap{mx}: ")    
    (text . "\tab{30} \menuitemstyle{}\tab{32} Grid points on y-axis ")
    (text . "\htbitmap{my}: ")    
    (text . "\newline\tab{2} ")
    (bcStrings (6 11 mx PI))
    (text . "\tab{32} ")
    (bcStrings (6 9 my PI))    
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "Over-estimate of \htbitmap{nx} of knots \lambda of the ") 
    (text . "computed spline, {\it nxest}: \newline\tab{2} ")
    (bcStrings (6 15 nxest PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "Over-estimate of \htbitmap{ny} of knots \mu of the computed ") 
    (text . "spline, {\it nyest}: \newline\tab{2} ")
    (bcStrings (6 13 nyest PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Smoothing factor {\it s}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 "0.1" s F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Start value: ")
    (radioButtons start
        ("" "  Cold Start - no values needed for {\it nx,ny,lamda,mu} or {\it iwrk}" cold)
        ("" "  Warm Start - uses knots found in a previous call" warm))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02dcfSolve)
  htShowPage()

e02dcfSolve htPage ==
  mx :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'mx)
    objValUnwrap htpLabelSpadValue(htPage, 'mx)
  my :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'my)
    objValUnwrap htpLabelSpadValue(htPage, 'my)
  nxest :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nxest)
    objValUnwrap htpLabelSpadValue(htPage, 'nxest)
  nyest :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nyest)
    objValUnwrap htpLabelSpadValue(htPage, 'nyest)
  wrklist := [my,nxest]
  wrkmax := APPLY ('MAX, wrklist)
  lwrk := 4*(mx + my) +11*(nxest + nyest) + nxest*my + wrkmax +54
  liwrk := 3 + mx + my + nxest + nyest
  s := htpLabelInputString(htPage,'s)
  initial := htpButtonValue(htPage,'start)
  start :=
    initial = 'cold => '1
    '2
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  ((mx = 11 and my = 9) and start = 1) => 
                e02dcfDefaultSolve (htPage,nxest,nyest,lwrk,liwrk,s,ifail)
  start = 1 => e02dcfColdSolve (htPage,mx,my,nxest,nyest,lwrk,liwrk,s,ifail)
  -- warm start not really possible from hyperdoc
  -- as inputing a workspace array of dimension 592 is asking too much
  -- user should use the command line, using the previous calculated 
  -- parameters
  htInitPage('"E02DCF - Least-squares curve by bicubic splines with automatic knot placement, data on a rectangular grid",nil)  
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\blankline ")
    (text . "{\center{\em Hyperdoc interface not available for warm starts.}}")
    (text . "\newline ")
    (text . "{\center{\em Please use the command line.}}"))
  htMakeDoneButton('"Continue",'e02dcf)
  htShowPage()



e02dcfColdSolve(htPage,mx,my,nxest,nyest,lwrk,liwrk,s,ifail) ==
  xList :=
    "append"/[f(i) for i in 1..mx] where f(i) ==
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, xnam, 'F]]] 
  yList :=
    "append"/[g(i) for i in 1..my] where g(i) ==
      ynam := INTERN STRCONC ('"g",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, ynam, 'F]]] 
  prefix:= ('"\blankline \menuitemstyle{}\tab{2} Values of {\it y(my)}: \newline ")
  yList := [['text,:prefix],:yList]
  fList :=
    "append"/[h(i) for i in 1..(mx*my)] where h(i) ==
      fnam := INTERN STRCONC ('"g",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, fnam, 'F]]] 
  prefix:=('"\blankline \menuitemstyle{} \tab{2} Values of {\it f(mx*my)}: \newline ")
  fList := [['text,:prefix],:fList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :xList,:yList,:fList]
  page := htInitPage('"E02DCF - Least-squares curve by bicubic splines with automatic knot placement, data on a rectangular grid",nil)
  htSay '"\menuitemstyle{}\tab{2} Values of {\it x(mx)}: \newline "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02dcfColdGen)
  htpSetProperty(page,'mx,mx)
  htpSetProperty(page,'my,my)
  htpSetProperty(page,'nxest,nxest)
  htpSetProperty(page,'nyest,nyest)
  htpSetProperty(page,'lwrk,lwrk)
  htpSetProperty(page,'liwrk,liwrk)
  htpSetProperty(page,'s,s)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02dcfDefaultSolve (htPage,nxest,nyest,lwrk,liwrk,s,ifail) ==
  mx := 11
  my := 9
  page := htInitPage('"E02DCF - Least-squares curve by bicubic splines with automatic knot placement, data on a rectangular grid",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Values of {\it x(mx)}:")
    (text . "\newline ")
    (bcStrings (8 "0" x1 F))
    (bcStrings (8 "0.5" x2 F))
    (bcStrings (8 "1" x3 F))
    (bcStrings (8 "1.5" x4 F))
    (bcStrings (8 "2" x5 F))
    (bcStrings (8 "2.5" x6 F))
    (bcStrings (8 "3" x7 F))
    (bcStrings (8 "3.5" x8 F))
    (bcStrings (8 "4" x9 F))
    (bcStrings (8 "4.5" x10 F))
    (bcStrings (8 "5" x11 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} Values of {\it y(my)}:")
    (text . "\newline ")
    (bcStrings (8 "0" y1 F))
    (bcStrings (8 "0.5" y2 F))
    (bcStrings (8 "1" y3 F))
    (bcStrings (8 "1.5" y4 F))
    (bcStrings (8 "2" y5 F))
    (bcStrings (8 "2.5" y6 F))
    (bcStrings (8 "3" y7 F))
    (bcStrings (8 "3.5" y8 F))
    (bcStrings (8 "4" y9 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} Values of {\it f(mx*my)}:")
    (text . "\newline ")
    (bcStrings (8 "1" f1 F))
    (bcStrings (8 "0.88758" f2 F))
    (bcStrings (8 "0.5403" f3 F))
    (bcStrings (8 "0.070737" f4 F))
    (bcStrings (8 "-0.41515" f5 F))
    (bcStrings (8 "-0.80114" f6 F))
    (bcStrings (8 "-0.97999" f7 F))
    (bcStrings (8 "-0.93446" f8 F))
    (bcStrings (8 "-0.65664" f9 F))
    (bcStrings (8 "1.5" f10 F))
    (bcStrings (8 "1.3564" f11 F))
    (bcStrings (8 "0.82045" f12 F))
    (bcStrings (8 "0.10611" f13 F))
    (bcStrings (8 "-0.62422" f14 F))
    (bcStrings (8 "-1.2317" f15 F))
    (bcStrings (8 "-1.485" f16 F))
    (bcStrings (8 "-1.3047" f17 F))
    (bcStrings (8 "-0.98547" f18 F))
    (bcStrings (8 "2.06" f19 F))
    (bcStrings (8 "1.7552" f20 F))
    (bcStrings (8 "1.0806" f21 F))
    (bcStrings (8 "0.15147" f22 F))
    (bcStrings (8 "-0.83229" f23 F))
    (bcStrings (8 "-1.6023" f24 F))
    (bcStrings (8 "-1.97" f25 F))
    (bcStrings (8 "-1.8729" f26 F))
    (bcStrings (8 "-1.4073" f27 F))
    (bcStrings (8 "2.57" f28 F))
    (bcStrings (8 "2.124" f29 F))
    (bcStrings (8 "1.3508" f30 F))
    (bcStrings (8 "0.17684" f31 F))
    (bcStrings (8 "-1.0404" f32 F))
    (bcStrings (8 "-2.0029" f33 F))
    (bcStrings (8 "-2.475" f34 F))
    (bcStrings (8 "-2.3511" f35 F))
    (bcStrings (8 "-1.6741" f36 F))
    (bcStrings (8 "3" f37 F))
    (bcStrings (8 "2.6427" f38 F))
    (bcStrings (8 "1.6309" f39 F))
    (bcStrings (8 "0.21221" f40 F))
    (bcStrings (8 "-1.2484" f41 F))
    (bcStrings (8 "-2.2034" f42 F))
    (bcStrings (8 "-2.97" f43 F))
    (bcStrings (8 "-2.8094" f44 F))
    (bcStrings (8 "-1.9809" f45 F))
    (bcStrings (8 "3.5" f46 F))
    (bcStrings (8 "3.1715" f47 F))
    (bcStrings (8 "1.8611" f48 F))
    (bcStrings (8 "0.24458" f49 F))
    (bcStrings (8 "-1.4565" f50 F))
    (bcStrings (8 "-2.864" f51 F))
    (bcStrings (8 "-3.265" f52 F))
    (bcStrings (8 "-3.2776" f53 F))
    (bcStrings (8 "-2.2878" f54 F))
    (bcStrings (8 "4.04" f55 F))
    (bcStrings (8 "3.5103" f56 F))
    (bcStrings (8 "2.0612" f57 F))
    (bcStrings (8 "0.28595" f58 F))
    (bcStrings (8 "-1.6946" f59 F))
    (bcStrings (8 "-3.2046" f60 F))
    (bcStrings (8 "-3.96" f61 F))
    (bcStrings (8 "-3.7958" f62 F))
    (bcStrings (8 "-2.6146" f63 F))
    (bcStrings (8 "4.5" f64 F))
    (bcStrings (8 "3.9391" f65 F))
    (bcStrings (8 "2.4314" f66 F))
    (bcStrings (8 "0.31632" f67 F))
    (bcStrings (8 "-1.8627" f68 F))
    (bcStrings (8 "-3.6351" f69 F))
    (bcStrings (8 "-4.455" f70 F))
    (bcStrings (8 "-4.2141" f71 F))
    (bcStrings (8 "-2.9314" f72 F))
    (bcStrings (8 "5.04" f73 F))
    (bcStrings (8 "4.3879" f74 F))
    (bcStrings (8 "2.7515" f75 F))
    (bcStrings (8 "0.35369" f76 F))
    (bcStrings (8 "-2.0707" f77 F))
    (bcStrings (8 "-4.0057" f78 F))
    (bcStrings (8 "-4.97" f79 F))
    (bcStrings (8 "-4.6823" f80 F))
    (bcStrings (8 "-3.2382" f81 F))
    (bcStrings (8 "5.505" f82 F))
    (bcStrings (8 "4.8367" f83 F))
    (bcStrings (8 "2.9717" f84 F))
    (bcStrings (8 "0.38505" f85 F))
    (bcStrings (8 "-2.2888" f86 F))
    (bcStrings (8 "-4.4033" f87 F))
    (bcStrings (8 "-5.445" f88 F))
    (bcStrings (8 "-5.1405" f89 F))
    (bcStrings (8 "-3.595" f90 F))
    (bcStrings (8 "6" f91 F))
    (bcStrings (8 "5.2755" f92 F))
    (bcStrings (8 "3.2418" f93 F))
    (bcStrings (8 "0.42442" f94 F))
    (bcStrings (8 "-2.4769" f95 F))
    (bcStrings (8 "-4.8169" f96 F))
    (bcStrings (8 "-5.93" f97 F))
    (bcStrings (8 "-5.6387" f98 F))
    (bcStrings (8 "-3.9319" f99 F)))
  htMakeDoneButton('"Continue",'e02dcfColdGen)          
  htpSetProperty(page,'mx,mx)
  htpSetProperty(page,'my,my)
  htpSetProperty(page,'nxest,nxest)
  htpSetProperty(page,'nyest,nyest)
  htpSetProperty(page,'lwrk,lwrk)
  htpSetProperty(page,'liwrk,liwrk)
  htpSetProperty(page,'s,s)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02dcfColdGen htPage ==
  mx := htpProperty(htPage,'mx)
  my := htpProperty(htPage,'my)
  nxest := htpProperty(htPage,'nxest)
  nyest := htpProperty(htPage,'nyest)
  lwrk := htpProperty(htPage,'lwrk)
  liwrk := htpProperty(htPage,'liwrk)
  s := htpProperty(htPage,'s)
  cold := '"c"
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..(mx*my) repeat
    end := STRCONC((first y).1," ")
    y := rest y
    fList := [end,:fList]
  fstring := bcwords2liststring fList
  for i in 1..my repeat
    mid :=  STRCONC ((first y).1," ")
    y := rest y
    ylist := [mid,:ylist]
  ystring := bcwords2liststring ylist
  while y repeat
    start :=  STRCONC ((first y).1," ")
    y := rest y
    xlist := [start,:xlist]
  xstring := bcwords2liststring xlist
  -- additional entries needed to get it running
  -- but as Start = c they are not used
  prefix := STRCONC('"e02dcf(_"",cold,"_",",STRINGIMAGE mx,", [",xstring,"],")
  prefix := STRCONC(prefix,STRINGIMAGE my,",[",ystring,"],[",fstring,"], ")
  prefix := STRCONC(prefix,STRINGIMAGE s,", ",STRINGIMAGE nxest,", ")
  prefix := STRCONC(prefix,STRINGIMAGE nyest,", ",STRINGIMAGE lwrk,", ")
  prefix := STRCONC(prefix,STRINGIMAGE liwrk,",0,new(1,", STRINGIMAGE nxest,",0.0)$MATRIX DFLOAT,")
  prefix := STRCONC(prefix,"0,new(1,", STRINGIMAGE nyest,",0.0)$MATRIX DFLOAT,")
  end := STRCONC("new(1,", STRINGIMAGE lwrk,",0.0)$MATRIX DFLOAT,[[0 for i in 1..")
  end := STRCONC(end,STRINGIMAGE liwrk,"]]::Matrix Integer,",STRINGIMAGE ifail,")")
  linkGen STRCONC(prefix,end)


e02ddf() ==
  htInitPage('"E02DDF - Least-squares surface fit by bicubic splines with automatic knot placement, scattered data",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02ddf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02ddf| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "Determines a bicubic spline approximation to a set of scattered")
    (text . " points ( \htbitmap{xr},\htbitmap{yr}, ")
    (text . "\htbitmap{fr})")
    (text . "with weights \htbitmap{wr}, for r = 1,2,...,m. ")
    (text . "The knots \htbitmap{lamdai}, for i = 1,2,...,")
    (text . "\htbitmap{nx} and \htbitmap{mui}, for ")
    (text . "i = 1,2,...,\htbitmap{ny} are chosen by the routine ")
    (text . ", but a single parameter S must be specified to control the ")
    (text . "trade-off between closeness of fit and smoothness of fit.  This ")
    (text . "affects the number of knots required by the spline, which is ")
    (text . "given in the B-spline representation \htbitmap{e02daf}")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Number of data points {\it m}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 30 m PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "Over-estimate of \htbitmap{nx} of knots \lambda of the ") 
    (text . "computed spline, {\it nxest}: \newline\tab{2} ")
    (bcStrings (6 14 nxest PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "Over-estimate of \htbitmap{ny} of knots \mu of the computed ") 
    (text . "spline, {\it nyest}: \newline\tab{2} ")
    (bcStrings (6 14 nyest PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Smoothing factor {\it s}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 "10" s F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Start value: ")
    (radioButtons start
        ("" "  Cold Start - no values needed for {\it nx,ny,lamda,wrk,iwrk}" cold)
        ("" "  Warm Start - uses knots found in a previous call" warm))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02ddfSolve)
  htShowPage()

e02ddfSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  nxest :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nxest)
    objValUnwrap htpLabelSpadValue(htPage, 'nxest)
  nyest :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nyest)
    objValUnwrap htpLabelSpadValue(htPage, 'nyest)
  u := nxest - 4
  v := nyest - 4
  wlist := [u,v]
  w := APPLY ('MAX, wlist)
  lwrk := (7*u*v + 25*w)*(w + 1) + 2*(u + v + 4*m) + 23*w + 56
  liwrk := m + 2*(nxest - 7)*(nyest - 7)
  s := htpLabelInputString(htPage,'s)
  initial := htpButtonValue(htPage,'start)
  start :=
    initial = 'cold => '1
    '2
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (m = 30 and start = 1) => e02ddfDefaultSolve (htPage,nxest,nyest,lwrk,liwrk,s,ifail) 
  start = 1 => e02ddfColdSolve (htPage,m,nxest,nyest,lwrk,liwrk,s,ifail) 
  -- need to change as only wrk(1) is required
  labelList :=
    "append"/[f(i) for i in 1..m] where f(i) ==
      prefix := ('"\newline \tab{2} ")
      middle := ('"\tab{17} ")
      post   := ('"\tab{32} ")
      end    := ('"\tab{47} ")
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      ynam := INTERN STRCONC ('"y",STRINGIMAGE i)
      fnam := INTERN STRCONC ('"f",STRINGIMAGE i)
      wnam := INTERN STRCONC ('"w",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[8, 0.0, xnam, 'F]], 
       ['text,:middle],['bcStrings,[8, 0.0, ynam, 'F]],
        ['text,:post],['bcStrings,[8, 0.0, fnam, 'F]],
         ['text,:end],['bcStrings,[8, 0.0, wnam, 'F]]]
  lamdaList := 
    "append"/[g(i) for i in 1..nxest] where g(i) ==
      lnam := INTERN STRCONC ('"l",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, lnam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} Values of Lamda: \newline")
  lamdaList := [['text,:prefix],:lamdaList]
  muList := 
    "append"/[h(i) for i in 1..nyest] where h(i) ==
      mnam := INTERN STRCONC ('"m",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, mnam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} Values of Mu: \newline")
  muList := [['text,:prefix],:muList]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} Value of nx: \newline \tab{2}")
  nxList := [['text,:prefix],['bcStrings,[8, 10, 'nx, 'PI]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} Value of ny: \newline \tab{2}")
  nyList := [['text,:prefix],['bcStrings,[8, 9, 'ny, 'PI]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} Value of WRK(1): \newline \tab{2}")
  wList := [['text,:prefix],['bcStrings,[8, 0.0, 'wone, 'F]]]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList,:lamdaList,:muList,:nxList,:nyList,:wList]
  page := htInitPage('"E02DDF - Least-squares surface fit by bicubic splines with automatic knot placement, scattered data",nil)
  htSay '"\menuitemstyle{}\tab{2} Values of \space{1} "
  htSay '"\htbitmap{xr}: \tab{15} \menuitemstyle{}\tab{17} "
  htSay '"Values of \space{1} \htbitmap{yr}: \tab{30}"
  htSay '"\menuitemstyle{}\tab{32} Values of \space{1} "
  htSay '"\htbitmap{fr}: \tab{45} \menuitemstyle{} "
  htSay '"\tab{47} Values of \htbitmap{wr}:"
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02ddfWarmGen)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'nxest,nxest)
  htpSetProperty(page,'nyest,nyest)
  htpSetProperty(page,'lwrk,lwrk)
  htpSetProperty(page,'liwrk,liwrk)
  htpSetProperty(page,'s,s)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()




e02ddfColdSolve(htPage,m,nxest,nyest,lwrk,liwrk,s,ifail)  ==
  labelList :=
    "append"/[f(i) for i in 1..m] where f(i) ==
      prefix := ('"\newline \tab{2} ")
      middle := ('"\tab{17} ")
      post   := ('"\tab{32} ")
      end    := ('"\tab{47} ")
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      ynam := INTERN STRCONC ('"y",STRINGIMAGE i)
      fnam := INTERN STRCONC ('"f",STRINGIMAGE i)
      wnam := INTERN STRCONC ('"w",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[8, 0.0, xnam, 'F]], 
       ['text,:middle],['bcStrings,[8, 0.0, ynam, 'F]],
        ['text,:post],['bcStrings,[8, 0.0, fnam, 'F]],
         ['text,:end],['bcStrings,[8, 0.0, wnam, 'F]]]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList]
  page := htInitPage('"E02DDF - Least-squares surface fit by bicubic splines with automatic knot placement, scattered data",nil)
  htSay '"\menuitemstyle{}\tab{2} Values of \space{1} "
  htSay '"\htbitmap{xr}: \tab{15} \menuitemstyle{}\tab{17} "
  htSay '"Values of \space{1} \htbitmap{yr}: \tab{30}"
  htSay '"\menuitemstyle{}\tab{32} Values of \space{1} "
  htSay '"\htbitmap{fr}: \tab{44} \menuitemstyle{} "
  htSay '"\tab{46} Values of \htbitmap{wr}:"
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02ddfColdGen)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'nxest,nxest)
  htpSetProperty(page,'nyest,nyest)
  htpSetProperty(page,'lwrk,lwrk)
  htpSetProperty(page,'liwrk,liwrk)
  htpSetProperty(page,'s,s)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02ddfDefaultSolve (htPage,nxest,nyest,lwrk,liwrk,s,ifail) ==
  m := 30
  page := htInitPage('"E02DDF - Least-squares surface fit by bicubic splines with automatic knot placement, scattered data",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} Values of \htbitmap{xr}:")
    (text . "\tab{15} \menuitemstyle{} \tab{17} Values of ")
    (text . "\htbitmap{yr}: \tab{30} \menuitemstyle{} \tab{32} ")
    (text . "Values of \htbitmap{fr}: \tab{44} \menuitemstyle{} ")
    (text . "\tab{46} Values of \htbitmap{wr}:")
    (text . "\newline \tab{2} ")
    (bcStrings (8 "11.16" x1 F))  
    (text . "\tab{17}")
    (bcStrings (8 "1.24" y1 F))    
    (text . "\tab{32}")
    (bcStrings (8 "22.15" f1 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w1 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "12.85" x2 F))  
    (text . "\tab{17}")
    (bcStrings (8 "3.06" y2 F))    
    (text . "\tab{32}")
    (bcStrings (8 "22.11" f2 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w2 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "19.85" x3 F))  
    (text . "\tab{17}")
    (bcStrings (8 "10.72" y3 F))    
    (text . "\tab{32}")
    (bcStrings (8 "7.97" f3 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w3 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "19.72" x4 F))  
    (text . "\tab{17}")
    (bcStrings (8 "1.39" y4 F))    
    (text . "\tab{32}")
    (bcStrings (8 "16.83" f4 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w4 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "15.91" x5 F))  
    (text . "\tab{17}")
    (bcStrings (8 "7.74" y5 F))    
    (text . "\tab{32}")
    (bcStrings (8 "15.30" f5 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w5 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0" x6 F))  
    (text . "\tab{17}")
    (bcStrings (8 "20" y6 F))    
    (text . "\tab{32}")
    (bcStrings (8 "34.6" f6 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w6 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "20.87" x7 F))  
    (text . "\tab{17}")
    (bcStrings (8 "20" y7 F))    
    (text . "\tab{32}")
    (bcStrings (8 "5.74" f7 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w7 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "3.45" x8 F))  
    (text . "\tab{17}")
    (bcStrings (8 "12.78" y8 F))    
    (text . "\tab{32}")
    (bcStrings (8 "41.24" f8 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w8 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "14.26" x9 F))  
    (text . "\tab{17}")
    (bcStrings (8 "17.87" y9 F))    
    (text . "\tab{32}")
    (bcStrings (8 "10.74" f9 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w9 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "17.43" x10 F))  
    (text . "\tab{17}")
    (bcStrings (8 "3.46" y10 F))    
    (text . "\tab{32}")
    (bcStrings (8 "18.60" f10 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w10 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "22.8" x11 F))  
    (text . "\tab{17}")
    (bcStrings (8 "12.39" y11 F))    
    (text . "\tab{32}")
    (bcStrings (8 "5.47" f11 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w11 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "7.58" x12 F))  
    (text . "\tab{17}")
    (bcStrings (8 "1.98" y12 F))    
    (text . "\tab{32}")
    (bcStrings (8 "29.87" f12 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w12 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "25" x13 F))  
    (text . "\tab{17}")
    (bcStrings (8 "11.87" y13 F))    
    (text . "\tab{32}")
    (bcStrings (8 "4.4" f13 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w13 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0" x14 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0" y14 F))    
    (text . "\tab{32}")
    (bcStrings (8 "58.2" f14 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w14 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "9.66" x15 F))  
    (text . "\tab{17}")
    (bcStrings (8 "20" y15 F))    
    (text . "\tab{32}")
    (bcStrings (8 "4.73" f15 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w15 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "5.22" x16 F))  
    (text . "\tab{17}")
    (bcStrings (8 "14.66" y16 F))    
    (text . "\tab{32}")
    (bcStrings (8 "40.36" f16 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w16 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "17.25" x17 F))  
    (text . "\tab{17}")
    (bcStrings (8 "19.57" y17 F))    
    (text . "\tab{32}")
    (bcStrings (8 "6.43" f17 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w17 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "25" x18 F))  
    (text . "\tab{17}")
    (bcStrings (8 "3.87" y18 F))    
    (text . "\tab{32}")
    (bcStrings (8 "8.74" f18 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w18 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "12.13" x19 F))  
    (text . "\tab{17}")
    (bcStrings (8 "10.79" y19 F))    
    (text . "\tab{32}")
    (bcStrings (8 "13.71" f19 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w19 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "22.23" x20 F))  
    (text . "\tab{17}")
    (bcStrings (8 "6.21" y20 F))    
    (text . "\tab{32}")
    (bcStrings (8 "10.25" f20 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w20 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "11.52" x21 F))  
    (text . "\tab{17}")
    (bcStrings (8 "8.53" y21 F))    
    (text . "\tab{32}")
    (bcStrings (8 "15.74" f21 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w21 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "15.2" x22 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0" y22 F))    
    (text . "\tab{32}")
    (bcStrings (8 "21.6" f22 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w22 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "7.54" x23 F))  
    (text . "\tab{17}")
    (bcStrings (8 "10.69" y23 F))    
    (text . "\tab{32}")
    (bcStrings (8 "19.31" f23 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w23 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "17.32" x24 F))  
    (text . "\tab{17}")
    (bcStrings (8 "13.78" y24 F))    
    (text . "\tab{32}")
    (bcStrings (8 "12.11" f24 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w24 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "2.14" x25 F))  
    (text . "\tab{17}")
    (bcStrings (8 "15.03" y25 F))    
    (text . "\tab{32}")
    (bcStrings (8 "53.1" f25 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w25 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.51" x26 F))  
    (text . "\tab{17}")
    (bcStrings (8 "8.37" y26 F))    
    (text . "\tab{32}")
    (bcStrings (8 "49.43" f26 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w26 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "22.69" x27 F))  
    (text . "\tab{17}")
    (bcStrings (8 "19.63" y27 F))    
    (text . "\tab{32}")
    (bcStrings (8 "3.25" f27 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w27 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "5.47" x28 F))  
    (text . "\tab{17}")
    (bcStrings (8 "17.13" y28 F))    
    (text . "\tab{32}")
    (bcStrings (8 "28.63" f28 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w28 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "21.67" x29 F))  
    (text . "\tab{17}")
    (bcStrings (8 "14.36" y29 F))    
    (text . "\tab{32}")
    (bcStrings (8 "5.52" f29 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w29 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "3.31" x30 F))  
    (text . "\tab{17}")
    (bcStrings (8 "0.33" y30 F))    
    (text . "\tab{32}")
    (bcStrings (8 "44.08" f30 F))    
    (text . "\tab{47}")
    (bcStrings (8 "1" w30 F))    
    (text . "\blankline"))
  htMakeDoneButton('"Continue",'e02ddfColdGen)          
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'nxest,nxest)
  htpSetProperty(page,'nyest,nyest)
  htpSetProperty(page,'lwrk,lwrk)
  htpSetProperty(page,'liwrk,liwrk)
  htpSetProperty(page,'s,s)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02ddfColdGen htPage ==
  m := htpProperty(htPage,'m)
  nxest := htpProperty(htPage,'nxest)
  nyest := htpProperty(htPage,'nyest)
  lwrk := htpProperty(htPage,'lwrk)
  liwrk := htpProperty(htPage,'liwrk)
  s := htpProperty(htPage,'s)
  cold := '"c"
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  while y repeat
    end :=  STRCONC ((first y).1," ")
    y := rest y
    right := STRCONC ((first y).1," ")
    y := rest y    
    mid :=  STRCONC ((first y).1," ")
    y := rest y
    left :=  STRCONC ((first y).1," ")
    y := rest y
    xlist := [left,:xlist]
    ylist := [mid,:ylist]
    flist := [right,:flist]
    wlist := [end,:wlist]
  xstring := bcwords2liststring xlist
  ystring := bcwords2liststring ylist
  fstring := bcwords2liststring flist
  wstring := bcwords2liststring wlist
  -- additional entries nx,ny,lamda,mu,wrk needed to get it running
  -- but they are just set to 0.0
  prefix := STRCONC('"e02ddf(_"",cold,"_",",STRINGIMAGE m,", [",xstring,"],[")
  prefix := STRCONC(prefix,ystring,"],[",fstring,"],[",wstring,"], ")
  prefix := STRCONC(prefix,STRINGIMAGE s,", ",STRINGIMAGE nxest,", ")
  prefix := STRCONC(prefix,STRINGIMAGE nyest,", ",STRINGIMAGE lwrk,", ")
  prefix := STRCONC(prefix,STRINGIMAGE liwrk,", 0,")
  prefix := STRCONC(prefix,"new(1,", STRINGIMAGE nxest,",0.0)$MATRIX DFLOAT,0,")
  prefix := STRCONC(prefix,"new(1,", STRINGIMAGE nyest,",0.0)$MATRIX DFLOAT,")
  prefix := STRCONC(prefix,"new(1,", STRINGIMAGE lwrk,",0.0)$MATRIX DFLOAT,")
--  prefix := STRCONC(prefix,"[[0.0 for i in 1..", STRINGIMAGE nxest,"]],0,")
--  prefix := STRCONC(prefix,"[[0.0 for i in 1..", STRINGIMAGE nyest,"]],")
--  prefix := STRCONC(prefix,"[[0.0 for i in 1..", STRINGIMAGE lwrk,"]],")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,")")
  linkGen prefix

e02ddfWarmGen htPage ==
  m := htpProperty(htPage,'m)
  nxest := htpProperty(htPage,'nxest)
  nyest := htpProperty(htPage,'nyest)
  lwrk := htpProperty(htPage,'lwrk)
  liwrk := htpProperty(htPage,'liwrk)
  s := htpProperty(htPage,'s)
  warm := '"w"
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  wrk := (first y).1
  y := rest y
  for i in 1..lwrk repeat
    wrkList := ['"0.0 ",:wrkList]
  wrkList := [wrk,:wrkList]
  wrkstring := bcwords2liststring wrkList
  ny := STRCONC((first y).1," ")
  y := rest y
  nx := STRCONC((first y).1," ")
  y := rest y
  for i in 1..nyest repeat
    mu := STRCONC ((first y).1, " ")
    y := rest y
    muList := [mu,:muList]
  mustring := bcwords2liststring muList
  for i in 1..nxest repeat
    lam := STRCONC ((first y).1, " ")
    y := rest y
    lamList := [lam,:lamList]
  lamstring := bcwords2liststring lamList
  while y repeat
    end :=  STRCONC ((first y).1," ")
    y := rest y
    right := STRCONC ((first y).1," ")
    y := rest y    
    mid :=  STRCONC ((first y).1," ")
    y := rest y
    left :=  STRCONC ((first y).1," ")
    y := rest y
    xlist := [left,:xlist]
    ylist := [mid,:ylist]
    flist := [right,:flist]
    wlist := [end,:wlist]
  xstring := bcwords2liststring xlist
  ystring := bcwords2liststring ylist
  fstring := bcwords2liststring flist
  wstring := bcwords2liststring wlist
  -- additional entries nx,ny,lamda,mu,wrk needed to get it running
  -- but they are just set to 0.0
  prefix := STRCONC('"e02ddf(_"",warm,"_",",STRINGIMAGE m,", [",xstring,"],[")
  prefix := STRCONC(prefix,ystring,"],[",fstring,"],[",wstring,"], ")
  prefix := STRCONC(prefix,STRINGIMAGE s,", ",STRINGIMAGE nxest,", ")
  prefix := STRCONC(prefix,STRINGIMAGE nyest,", ",STRINGIMAGE lwrk,", ")
  prefix := STRCONC(prefix,STRINGIMAGE liwrk,", ",nx,",[",lamstring,"],",ny)
  prefix := STRCONC(prefix,",[",mustring,"],[",wrkstring,"],")
  prefix := STRCONC(prefix,STRINGIMAGE ifail,")")
  linkGen prefix

e02zaf() == 
  htInitPage('"E02ZAF - Sort 2-D sata into panels for fitting bicubic splines",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe02zaf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e02zaf| '|NagFittingPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "Sorts the set of points {\em (\htbitmap{xr},")
    (text . "\htbitmap{yr})} into panels defined by \space{1}")
    (text . "\htbitmap{px} -8 points \htbitmap{lamdai} ")
    (text . "on the x-axis and \space{1}\htbitmap{py}-8 points ")
    (text . "\htbitmap{muj} on the y axis. The points are ordered ")
    (text . "so that all points in a panel occur before data in succeeding ")
    (text . "panels. Within a panel, the points maintain their original ")
    (text . "order. ") 
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Number of points to be sorted to be sorted {\it m}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 10 m PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline Intercepts + 8 on x axis {\em px}:")
    (text . "\tab{32} \menuitemstyle{}\tab{34}")
    (text . "Intercepts + 8 on y axis {\em py}:")
    (text . "\newline\tab{2} ")
    (bcStrings (6 9 px PI))
    (text . "\tab{34} ")
    (bcStrings (6 10 py PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Dimension of point {\it npoint}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 45 npoint PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{} \tab{2} ")
    (text . "\newline \tab{2} ")
    (text . "Ifail value: ")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e02zafSolve)
  htShowPage()

e02zafSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  px :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'px)
    objValUnwrap htpLabelSpadValue(htPage, 'px)
  py :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'py)
    objValUnwrap htpLabelSpadValue(htPage, 'py)
  npoint :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'npoint)
    objValUnwrap htpLabelSpadValue(htPage, 'npoint)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  ((m = '10 and px = '9) and py = '10) => e02zafDefaultSolve(htPage,npoint,ifail)
  labelList :=
    "append"/[fxy(i) for i in 1..m] where fxy(i) ==
      prefix := ('"\newline \tab{2} ")
      middle := ('"\tab{32} ")
      lnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      cnam := INTERN STRCONC ('"y",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[8, 0.0, lnam, 'F]], 
       ['text,:middle],['bcStrings,[8, 0.0, cnam, 'F]]]
  lamList :=
    "append"/[flam(i) for i in 5..(px-4)] where flam(i) ==
      lnam := INTERN STRCONC ('"l",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, lnam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} {\it \lambda(5) to ")
  prefix := STRCONC(prefix,"\lambda(px-4)}: \newline \tab{2} ")
  lamList := [['text,:prefix],:lamList]
  muList :=
    "append"/[fmu(i) for i in 5..(py-4)] where fmu(i) ==
      mnam := INTERN STRCONC ('"m",STRINGIMAGE i)
      [['bcStrings,[8, 0.0, mnam, 'F]]]
  prefix := ('"\blankline \menuitemstyle{} \tab{2} {\it \mu(5) to \mu(py-4)}: ")
  prefix := STRCONC(prefix,"\newline \tab{2} ")
  muList := [['text,:prefix],:muList]
  equationPart := [
     '(domainConditions 
        (isDomain P (Polynomial $EmptyMode))
         (isDomain S (String))
          (isDomain F (Float))
           (isDomain PI (PositiveInteger))),
            :labelList,:lamList,:muList]
  page := htInitPage('"E02ZAF - Sort 2-D sata into panels for fitting bicubic splines",nil)
  htSay '"\menuitemstyle{}\tab{2} {\it x(m)}: "
  htSay '"\tab{30} \menuitemstyle{}\tab{32} {\it y(m)}: "
  htMakePage equationPart
  htSay '"\blankline "
  htMakeDoneButton('"Continue",'e02zafGen)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'px,px)
  htpSetProperty(page,'py,py)
  htpSetProperty(page,'npoint,npoint)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e02zafDefaultSolve (htPage,npoint,ifail) ==
  m := '10
  px := '9
  py := '10
  page := htInitPage('"E02ZAF - Sort 2-D sata into panels for fitting bicubic splines",nil)
  htMakePage '(
    (domainConditions 
       (isDomain F (Float)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2} {\it x(m)}:")
    (text . "\tab{30} \menuitemstyle{} \tab{32} {\it y(m)}:")
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.00" x1 F))  
    (text . "\tab{32}")
    (bcStrings (8 "0.77" y1 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.70" x2 F))  
    (text . "\tab{32}")
    (bcStrings (8 "1.06" y2 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "1.44" x3 F))  
    (text . "\tab{32}")
    (bcStrings (8 "0.33" y3 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.21" x4 F))  
    (text . "\tab{32}")
    (bcStrings (8 "0.44" y4 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "1.01" x5 F))  
    (text . "\tab{32}")
    (bcStrings (8 "0.50" y5 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "1.84" x6 F))  
    (text . "\tab{32}")
    (bcStrings (8 "0.02" y6 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.71" x7 F))  
    (text . "\tab{32}")
    (bcStrings (8 "1.95" y7 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "1.00" x8 F))  
    (text . "\tab{32}")
    (bcStrings (8 "1.20" y8 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "0.54" x9 F))  
    (text . "\tab{32}")
    (bcStrings (8 "0.04" y9 F))    
    (text . "\newline \tab{2} ")
    (bcStrings (8 "1.531" x10 F))  
    (text . "\tab{32}")
    (bcStrings (8 "0.18" y10 F))    
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} {\it \lambda(5) to \lambda(px-4)}:")
    (text . "\newline \tab{2}")
    (bcStrings (8 "1.00" l5 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} {\it \mu(5) to \mu(py-4)}:")
    (text . "\newline \tab{2}")
    (bcStrings (8 "0.80" mu5 F))
    (bcStrings (8 "1.20" mu6 F))
    (text . "\blankline "))
  htMakeDoneButton('"Continue",'e02zafGen)      
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'px,px)
  htpSetProperty(page,'py,py)
  htpSetProperty(page,'npoint,npoint)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e02zafGen htPage ==
  m := htpProperty(htPage,'m)
  px := htpProperty(htPage,'px)
  py := htpProperty(htPage,'py)
  npoint := htpProperty(htPage,'npoint)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  nadres := (px-7)*(py-7)
  -- mu 
  for i in 1..4 repeat
    muList := ['"0 ",:muList]
  for i in 5..(py-4) repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    muList := [right,:muList]
  for i in (py-3)..py repeat
    muList := ['"0 ",:muList]
  mustring := bcwords2liststring muList
  -- lamda
  for i in 1..4 repeat
    lamList := ['"0 ",:lamList]
  for i in 5..(px-4) repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    lamList := [right,:lamList]
  for i in (px-3)..px repeat
    lamList := ['"0 ",:lamList]
  lamstring := bcwords2liststring lamList
  -- x & y
  while y repeat
    right := STRCONC ((first y).1," ")
    y := rest y    
    left :=  STRCONC ((first y).1," ")
    y := rest y
    xlist := [left,:xlist]
    ylist := [right,:ylist]
  xstring := bcwords2liststring xlist
  ystring := bcwords2liststring ylist
  prefix := STRCONC('"e02zaf(",STRINGIMAGE px,", ",STRINGIMAGE py,",[")
  prefix := STRCONC(prefix,lamstring,"],[",mustring,"], ",STRINGIMAGE m,", [")
  prefix := STRCONC(prefix,xstring,"],[",ystring,"], ",STRINGIMAGE npoint,", ")
  prefix := STRCONC(prefix,STRINGIMAGE nadres,", ",STRINGIMAGE ifail,")")
  linkGen prefix



