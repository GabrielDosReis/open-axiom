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

e04dgf() ==
  htInitPage('"E04DGF - Unconstrained minimum, pre-conditioned conjugate gradient algorithm, function of several variables using 1st derivatives",nil)
  htMakePage '(
    (domainConditions
        (isDomain EM $EmptyMode)
        (isDomain PI (PositiveInteger))
        (isDomain F (Float)))
    (text . "\windowlink{Manual Page}{manpageXXe04dgf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e04dgf| '|NagOptimisationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "E04DGF minimizes {\it F(x)}, an unconstrained nonlinear function")
    (text . " of {\it n} variables, using a pre-conditioned quasi-Newton ")
    (text . "conjugate gradient method. ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the number of variables, {\it n}: ")
    (text . "\newline ")
    (bcStrings (5 2 n PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Change optional parameters:")
    (radioButtons optional
        ("" " No" no)
        ("" " Yes" yes))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e04dgfSolve)
  htShowPage()


e04dgfSolve(htPage) ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  param := htpButtonValue(htPage,'optional)
  optional := 
    param = 'no => '0
    '1
  (n = '2 and optional = 0) => e04dgfDefaultSolve(htPage,ifail,n,optional)
  funcList := [['bcStrings,[55, '"exp(X[1])*(4*X[1]**2+2*X[2]**2+4*X[1]*X[2]+2*X[2]+1)", 'f, 'EM]]]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter initial guess ")
  middle := STRCONC(middle,'"of the solution vector {\it x(n)}: \newline ")
  middle := cons('text,middle)
  vecList := 
    n='2 =>
      [['bcStrings,[8,-1.0,'x1,'F]],['bcStrings,[8,1.0,'x2,'F]]]  
    [fb(i) for i in 1..n] where fb(i) ==
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      ['bcStrings,[8, -1.0, xnam, 'F]]
  funcList := [:funcList,middle,:vecList]
  if optional = 1 then
    opt1Text := '"\blankline \menuitemstyle{}\tab{2} "
    opt1Text := STRCONC(opt1Text,'"Estimated optimal function values, {\it es}: \newline ")
    optList := [['text,:opt1Text],['bcStrings,[20, 1.0, 'es, 'F]]]
    opt2Text := '"\blankline \menuitemstyle{}\tab{2} "
    opt2Text := STRCONC(opt2Text,'"Function precision, {\it fu}: \newline ")
    optList := [:optList,:[['text,:opt2Text],['bcStrings,[20,"0.4373903597E-14",'fu,'F]]]]
    opt3Text := '"\blankline \menuitemstyle{}\tab{2} "
    opt3Text := STRCONC(opt3Text,'"Iteration limit, {\it it}: \newline ")
    optList := [:optList,:[['text,:opt3Text],['bcStrings,[5,50,'it,'PI]]]]
    opt4Text := '"\blankline \menuitemstyle{}\tab{2} "
    opt4Text := STRCONC(opt4Text,'"Linesearch tolerance, {\it lin}: \newline ")
    optList := [:optList,:[['text,:opt4Text],['bcStrings,[20,"0.9",'lin,'F]]]]
    opt5Text := '"\blankline \menuitemstyle{}\tab{2} "
    opt5Text := STRCONC(opt5Text,'"List parameters:")
    optList := [:optList,:[['text,:opt5Text],['radioButtons,'lis,:[[""," Yes",'true],[""," No",'false]]]]]
    opt6Text := '"\blankline \menuitemstyle{}\tab{2} "
    opt6Text := STRCONC(opt6Text,'"Maximum step length, {\it ma}: \newline ")
    optList := [:optList,:[['text,:opt6Text],['bcStrings,[20,"1.0E+20",'ma,'F]]]]
    opt7Text := '"\blankline \menuitemstyle{}\tab{2} "
    opt7Text := STRCONC(opt7Text,'"Optimality tolerance, {\it op}: \newline ")
    optList := [:optList,:[['text,:opt7Text],['bcStrings,[20,"3.26E-12",'op,'F]]]]
    opt9Text := '"\blankline \menuitemstyle{}\tab{2} "
    opt9Text := STRCONC(opt9Text,'"Print level, {\it pr}: \newline ")
    optList := [:optList,:[['text,:opt9Text],['bcStrings,[5,10,'pr,'PI]]]]
    opt10Text := '"\blankline \menuitemstyle{}\tab{2} "
    opt10Text := STRCONC(opt10Text,'"Start objective check at variable, {\it sta}: \newline ")
    optList := [:optList,:[['text,:opt10Text],['bcStrings,[5,1,'sta,'PI]]]]
    opt11Text := '"\blankline \menuitemstyle{}\tab{2} "
    opt11Text := STRCONC(opt11Text,'"Stop objective check at variable, {\it sto}: \newline ")
    optList := [:optList,:[['text,:opt11Text],['bcStrings,[5,2,'sto,'PI]]]]
    opt12Text := '"\blankline \menuitemstyle{}\tab{2} "
    opt12Text := STRCONC(opt12Text,'"Verify level, {\it ver}: \newline ")
    optList := [:optList,:[['text,:opt12Text],['bcStrings,[5,0,'ver,'PI]]]]

--    (text . "\blankline ")
--    (text . "\newline ")
--    (text . "\menuitemstyle{}\tab{2}")
--    (text . "List parameters:")
--    (radioButtons lis
--        ("" " Yes" true)
--        ("" " No" false))
  else
    optList := []
  equationPart := [
     '(domainConditions 
         (isDomain EM $EmptyMode)
          (isDomain F (Float))
            (isDomain I (Integer))),
                :funcList,
                  :optList]
  page := htInitPage('"E04DGF - Unconstrained minimum, pre-conditioned conjugate gradient algorithm, function of several variables using 1st derivatives",nil)
  htSay '"\menuitemstyle{}\tab{2} "
  htSay '"Enter the objective function, {\it F(x)} in terms of X[1]...X[n]: "
  htSay '"\newline "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'e04dgfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'optional,optional)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e04dgfDefaultSolve(htPage,ifail,n,optional) ==
  page := htInitPage('"E04DGF - Unconstrained minimum, pre-conditioned conjugate gradient algorithm, function of several variables using 1st derivatives",nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the objective function, {\it F(x)} in terms of X[1]...X[n]: ")
    (text . "\newline ")
    (bcStrings (55 "exp(X[1])*(4*X[1]**2+2*X[2]**2+4*X[1]*X[2]+2*X[2]+1)" f EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter initial guess of the solution vector, {\it x(n)}: \newline")
    (bcStrings (8 "-1.0" x1 F))
    (bcStrings (8 "1.0" x2 F)))
  htMakeDoneButton('"Continue",'e04dgfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'optional,optional)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e04dgfGen htPage ==
  n := htpProperty(htPage,'n)
  optional := htpProperty(htPage,'optional)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  if (optional = '0) then
    es := '"1.0"
    ma := '"1.0E+20"
    op := '"3.26E-12"
    lin := '"0.9"
    fu := '"0.4373903597E-14"
    it := 50
    pr := 10
    sta := 1
    sto := 2
    ver := 0
    lis := '"true"
    for i in 1..n repeat
      temp := STRCONC ((first y).1," ")
      xlist := [temp,:xlist]    
      y := rest y
    xstring := bcwords2liststring xlist
    f := (first y).1
  else
    ver := STRCONC((first y).1," ")
    y := rest y
    sto := STRCONC((first y).1," ")
    y := rest y
    sta := STRCONC((first y).1," ")
    y := rest y
    pr := STRCONC((first y).1," ")
    y := rest y
    op := STRCONC((first y).1," ")
    y := rest y
    ma := STRCONC((first y).1," ")
    y := rest y
    nolist := (first y).1
    lis :=
      nolist = '" t" => '"false"
      '"true"
    y := rest y
    dummy := first y
    y := rest y
    lin := STRCONC((first y).1," ")
    y := rest y
    it := STRCONC((first y).1," ")
    y := rest y
    fu := STRCONC((first y).1," ")
    y := rest y
    es := STRCONC((first y).1," ")
    y := rest y
    for i in 1..n repeat
      temp := STRCONC ((first y).1," ")
      xlist := [temp,:xlist]    
      y := rest y
    xstring := bcwords2liststring xlist
    f := (first y).1
  prefix := STRCONC("e04dgf(",STRINGIMAGE n,", ",es,", ",fu,",")
  prefix := STRCONC(prefix,STRINGIMAGE it,", ",lin,", ",lis,", ",ma,", ",op)
  prefix := STRCONC(prefix,",",STRINGIMAGE pr,", ",STRINGIMAGE sta,", ")
  middle := STRCONC(STRINGIMAGE sto,", ",STRINGIMAGE ver,", [",xstring,"] ,")
  middle := STRCONC(middle,STRINGIMAGE ifail," ,")
  linkGen STRCONC (prefix,middle,"((",f,")::Expression(Float))::ASP49(OBJFUN))")

e04fdf() ==
  htInitPage('"E04FDF - Unconstrained minimum of a sum of squares, combined Gauss-Newton and modified Newton algorithm using function values only",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe04fdf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e04fdf| '|NagOptimisationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "E04FDF is an easy to use routine for finding an unconstrained ")
    (text . "minimum of a sum of squares of {\it m} nonlinear functions in ")
    (text . "{\it n} variables ({\it m} \htbitmap{great=} {\it n}), i.e., it ")
    (text . "is applicable to problems of the form ")
    (text . "\center{\htbitmap{e04fdf}} where \center{\htbitmap{e04fdf1}}")
    (text . "No derivatives are required.  The routine is intended for ")
    (text . "functions which have continous first and second derivatives, ")
    (text . "though it will usually work if the derivatives have occasional ")
    (text . "discontinuities. ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of functions {\it \htbitmap{fi}(x)}, {\it m}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 15 m PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of variables \htbitmap{xj}, {\it n}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 3 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Dimension of {\it iw}, {\it liw}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 1 liw F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Dimension of {\it w}, {\it lw}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 171 lw F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e04fdfSolve)
  htShowPage()

e04fdfSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  liw :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'liw)
    objValUnwrap htpLabelSpadValue(htPage, 'liw)
  lw :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'lw)
    objValUnwrap htpLabelSpadValue(htPage, 'lw)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (m = '15 and n = '3) => e04fdfDefaultSolve(htPage,liw,lw,ifail)
  funcList := 
    "append"/[fa(i) for i in 1..m] where fa(i) ==
      prefix := ('"\newline {\em Function ")
      prefix := STRCONC(prefix,STRINGIMAGE i,'":} \space{1}")
      funct := ('"XC[1] + 1")
      nam := INTERN STRCONC ('"n",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[42, funct, nam, 'EM]]]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter initial guess ")
  middle := STRCONC(middle,'"of the solution vector {\it x(n)}: \newline ")
  middle := cons('text,middle)
  vecList := 
    [fb(i) for i in 1..n] where fb(i) ==
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      ['bcStrings,[4, '"0.0", xnam, 'F]]
  funcList := [:funcList,middle,:vecList]
  equationPart := [
     '(domainConditions 
        (isDomain EM $EmptyMode)
          (isDomain F (Float))
           (isDomain I (Integer))),
                :funcList]
  page:= htInitPage('"E04FDF - Unconstrained minimum of a sum of squares, combined Gauss-Newton and modified Newton algorithm using function values only",nil)
  htSay '"\menuitemstyle{}\tab{2} "
  htSay '"Enter the functions \htbitmap{fi} below in terms XC[1]...XC[n]: "
  htSay '"\newline "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'e04fdfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'liw,liw)
  htpSetProperty(page,'lw,lw)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e04fdfDefaultSolve (htPage,liw,lw,ifail) ==
  n := '3
  m := '15
  page:= htInitPage('"E04FDF - Unconstrained minimum of a sum of squares, combined Gauss-Newton and modified Newton algorithm using function values only",nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the functions \htbitmap{fi} below ")
    (text . "in terms of XC[1]...XC[n]: ")
    (text . "\newline ")
    (text . "\newline {\em Function 1:} \space{1}")
    (bcStrings (42 "(XC[3]+15*XC[2])**(-1)+XC[1]-0.14" n1 EM))
    (text . "\newline {\em Function 2:} \space{1}")
    (bcStrings (42 "2*(2*XC[3]+14*XC[2])**(-1)+XC[1]-0.18" n2 EM))
    (text . "\newline {\em Function 3:} \space{1}")
    (bcStrings (42 "3*(3*XC[3]+13*XC[2])**(-1)+XC[1]-0.22" n3 EM))
    (text . "\newline {\em Function 4:} \space{1}")
    (bcStrings (42 "4*(4*XC[3]+12*XC[2])**(-1)+XC[1]-0.25" n4 EM))
    (text . "\newline {\em Function 5:} \space{1}")
    (bcStrings (42 "5*(5*XC[3]+11*XC[2])**(-1)+XC[1]-0.29" n5 EM))
    (text . "\newline {\em Function 6:} \space{1}")
    (bcStrings (42 "6*(6*XC[3]+10*XC[2])**(-1)+XC[1]-0.32" n6 EM))
    (text . "\newline {\em Function 7:} \space{1}")
    (bcStrings (42 "7*(7*XC[3]+9*XC[2])**(-1)+XC[1]-0.35" n7 EM))
    (text . "\newline {\em Function 8:} \space{1}")
    (bcStrings (42 "8*(8*XC[3]+8*XC[2])**(-1)+XC[1]-0.39" n8 EM))
    (text . "\newline {\em Function 9:} \space{1}")
    (bcStrings (42 "9*(7*XC[3]+7*XC[2])**(-1)+XC[1]-0.37" n9 EM))
    (text . "\newline {\em Function 10:} \space{1}")
    (bcStrings (42 "10*(6*XC[3]+6*XC[2])**(-1)+XC[1]-0.58" n10 EM))
    (text . "\newline {\em Function 11:} \space{1}")
    (bcStrings (42 "11*(5*XC[3]+5*XC[2])**(-1)+XC[1]-0.73" n11 EM))
    (text . "\newline {\em Function 12:} \space{1}")
    (bcStrings (42 "12*(4*XC[3]+4*XC[2])**(-1)+XC[1]-0.96" n12 EM))
    (text . "\newline {\em Function 13:} \space{1}")
    (bcStrings (42 "13*(3*XC[3]+3*XC[2])**(-1)+XC[1]-1.34" n13 EM))
    (text . "\newline {\em Function 14:} \space{1}")
    (bcStrings (42 "14*(2*XC[3]+2*XC[2])**(-1)+XC[1]-2.1" n14 EM))
    (text . "\newline {\em Function 15:} \space{1}")
    (bcStrings (42 "15*(XC[3]+XC[2])**(-1)+XC[1]-4.39" n15 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter initial guess of the solution vector {\it x(n)}: \newline ")
    (bcStrings (4 "0.5" x1 F))
    (bcStrings (4 "1.0" x2 F))
    (bcStrings (4 "1.5" x3 F)))
  htMakeDoneButton('"Continue",'e04fdfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'liw,liw)
  htpSetProperty(page,'lw,lw)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e04fdfGen htPage ==
  n := htpProperty(htPage, 'n)
  m := htpProperty(htPage, 'm)
  liw := htpProperty(htPage,'liw)
  lw := htpProperty(htPage,'lw)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..n repeat
    temp := STRCONC ((first y).1," ")
    xlist := [temp,:xlist]      
    y := rest y
  xstring := bcwords2liststring xlist    
  for i in 1..m repeat
    temp := STRCONC ((first y).1," ")
    ulist := [temp,:ulist]      
    y := rest y
  ustring := bcwords2liststring ulist    
  prefix := STRCONC("e04fdf(",STRINGIMAGE m,",",STRINGIMAGE n,", ")
  prefix := STRCONC(prefix,STRINGIMAGE liw,",",STRINGIMAGE lw,", [")
  middle := STRCONC(xstring,"],",STRINGIMAGE ifail,",")
  linkGen STRCONC(prefix,middle,"(",ustring,"::Vector Expression(Float))::ASP50(LSFUN1))")


e04gcf() ==
  htInitPage('"E04GCF - Unconstrained minimum of a sum of squares, combined Gauss-Newton and modified Newton algorithm using 1st derivatives",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe04gcf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e04gcf| '|NagOptimisationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "E04GCF is an easy to use quasi-Newton routine for finding an unconstrained ")
    (text . "minimum of a sum of squares of {\it m} nonlinear functions in ")
    (text . "{\it n} variables ({\it m} \htbitmap{great=} {\it n}), i.e., it ")
    (text . "is applicable to problems of the form ")
    (text . "\center{\htbitmap{e04fdf}} where \center{\htbitmap{e04fdf1}}")
    (text . "The routine is intended for ")
    (text . "functions which have continous first and second derivatives, ")
    (text . "though it will usually work if the derivatives have occasional ")
    (text . "discontinuities. ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of functions {\it \htbitmap{fi}(x)}, {\it m}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 15 m PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of variables \htbitmap{xj}, {\it n}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 3 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Dimension of {\it iw}, {\it liw}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 1 liw F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Dimension of {\it w}, {\it lw}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 177 lw F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e04gcfSolve)
  htShowPage()

e04gcfSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  liw :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'liw)
    objValUnwrap htpLabelSpadValue(htPage, 'liw)
  lw :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'lw)
    objValUnwrap htpLabelSpadValue(htPage, 'lw)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (m = '15 and n = '3) => e04gcfDefaultSolve(htPage,liw,lw,ifail)
  funcList := 
    "append"/[fa(i) for i in 1..m] where fa(i) ==
      prefix := ('"\newline {\em Function ")
      prefix := STRCONC(prefix,STRINGIMAGE i,'":} \space{1}")
      funct := ('"XC[1] + 1")
      nam := INTERN STRCONC ('"n",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[42, funct, nam, 'EM]]]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter initial guess ")
  middle := STRCONC(middle,'"of the solution vector {\it x(n)}: \newline ")
  middle := cons('text,middle)
  vecList := 
    [fb(i) for i in 1..n] where fb(i) ==
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      ['bcStrings,[4, '"0.0", xnam, 'F]]
  funcList := [:funcList,middle,:vecList]
  equationPart := [
     '(domainConditions 
        (isDomain EM $EmptyMode)
          (isDomain F (Float))
            (isDomain I (Integer))),
                :funcList]
  page:= htInitPage('"E04GCF - Unconstrained minimum of a sum of squares, combined Gauss-Newton and modified Newton algorithm using 1st derivatives",nil)
  htSay '"\menuitemstyle{}\tab{2} "
  htSay '"Enter the functions \htbitmap{fi} below in terms of XC[1]...XC[n]: "
  htSay '"\newline "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'e04gcfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'liw,liw)
  htpSetProperty(page,'lw,lw)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e04gcfDefaultSolve (htPage,liw,lw,ifail) ==
  n := '3
  m := '15
  page:= htInitPage('"E04GCF - Unconstrained minimum of a sum of squares, combined Gauss-Newton and modified Newton algorithm using 1st derivatives",nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the functions \htbitmap{fi} below ")
    (text . "in terms of XC[1]...XC[n]: ")
    (text . "\newline ")
    (text . "\newline {\em Function 1:} \space{1}")
    (bcStrings (42 "(XC[3]+15*XC[2])**(-1)+XC[1]-0.14" n1 EM))
    (text . "\newline {\em Function 2:} \space{1}")
    (bcStrings (42 "2*(2*XC[3]+14*XC[2])**(-1)+XC[1]-0.18" n2 EM))
    (text . "\newline {\em Function 3:} \space{1}")
    (bcStrings (42 "3*(3*XC[3]+13*XC[2])**(-1)+XC[1]-0.22" n3 EM))
    (text . "\newline {\em Function 4:} \space{1}")
    (bcStrings (42 "4*(4*XC[3]+12*XC[2])**(-1)+XC[1]-0.25" n4 EM))
    (text . "\newline {\em Function 5:} \space{1}")
    (bcStrings (42 "5*(5*XC[3]+11*XC[2])**(-1)+XC[1]-0.29" n5 EM))
    (text . "\newline {\em Function 6:} \space{1}")
    (bcStrings (42 "6*(6*XC[3]+10*XC[2])**(-1)+XC[1]-0.32" n6 EM))
    (text . "\newline {\em Function 7:} \space{1}")
    (bcStrings (42 "7*(7*XC[3]+9*XC[2])**(-1)+XC[1]-0.35" n7 EM))
    (text . "\newline {\em Function 8:} \space{1}")
    (bcStrings (42 "8*(8*XC[3]+8*XC[2])**(-1)+XC[1]-0.39" n8 EM))
    (text . "\newline {\em Function 9:} \space{1}")
    (bcStrings (42 "9*(7*XC[3]+7*XC[2])**(-1)+XC[1]-0.37" n9 EM))
    (text . "\newline {\em Function 10:} \space{1}")
    (bcStrings (42 "10*(6*XC[3]+6*XC[2])**(-1)+XC[1]-0.58" n10 EM))
    (text . "\newline {\em Function 11:} \space{1}")
    (bcStrings (42 "11*(5*XC[3]+5*XC[2])**(-1)+XC[1]-0.73" n11 EM))
    (text . "\newline {\em Function 12:} \space{1}")
    (bcStrings (42 "12*(4*XC[3]+4*XC[2])**(-1)+XC[1]-0.96" n12 EM))
    (text . "\newline {\em Function 13:} \space{1}")
    (bcStrings (42 "13*(3*XC[3]+3*XC[2])**(-1)+XC[1]-1.34" n13 EM))
    (text . "\newline {\em Function 14:} \space{1}")
    (bcStrings (42 "14*(2*XC[3]+2*XC[2])**(-1)+XC[1]-2.1" n14 EM))
    (text . "\newline {\em Function 15:} \space{1}")
    (bcStrings (42 "15*(XC[3]+XC[2])**(-1)+XC[1]-4.39" n15 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter initial guess of the solution vector {\it x(n)}: \newline ")
    (bcStrings (4 "0.5" x1 F))
    (bcStrings (4 "1.0" x2 F))
    (bcStrings (4 "1.5" x3 F)))
  htMakeDoneButton('"Continue",'e04gcfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'liw,liw)
  htpSetProperty(page,'lw,lw)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e04gcfGen htPage ==
  n := htpProperty(htPage, 'n)
  m := htpProperty(htPage, 'm)
  liw := htpProperty(htPage,'liw)
  lw := htpProperty(htPage,'lw)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..n repeat
    temp := STRCONC ((first y).1," ")
    xlist := [temp,:xlist]      
    y := rest y
  xstring := bcwords2liststring xlist    
  for i in 1..m repeat
    temp := STRCONC ((first y).1," ")
    ulist := [temp,:ulist]      
    y := rest y
  ustring := bcwords2liststring ulist    
  prefix := STRCONC("e04gcf(",STRINGIMAGE m,",",STRINGIMAGE n,", ")
  prefix := STRCONC(prefix,STRINGIMAGE liw,",",STRINGIMAGE lw,", [")
  middle := STRCONC(xstring,"],",STRINGIMAGE ifail,",")
  linkGen STRCONC(prefix,middle,"(",ustring,"::Vector Expression(Float))::ASP19(LSFUN2))")


e04jaf() ==
  htInitPage('"E04JAF - Minimum, function of several variables, quasi-Newton algorithm, simple bounds, using function values only",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXe04jaf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e04jaf| '|NagOptimisationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "E04JAF is an easy to use quasi-Newton routine for finding a ")
    (text . "minimum of a nonlinear function {\it F(x)} of {\it n} variables ")
    (text . "\center{\htbitmap{e04fdf1}} possibly subject to fixed upper ")
    (text . "and lower bounds on the variables, i.e., it is applicable to ")
    (text . "problems of the form \blankline Minimize {\it F(x)}, subject to ")
    (text . "\htbitmap{lj} \htbitmap{great=} \htbitmap{xj} \htbitmap{great=} ")
    (text . "\htbitmap{uj} for {\it j} = 1,2,...,n. \blankline ")
    (text . "Function values only are required.  The routine is intended for ")
    (text . "functions which have continuous first and second derivatives, ")
    (text . "though it will usually work if the derivatives have occasional ")
    (text . "discontinuities. ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of variables \htbitmap{xj}, {\it n}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 4 n PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Specify the use of bounds, {\it ibound}:")
    (radioButtons ibound
        (" 0" " All \htbitmap{lj} and \htbitmap{uj} are given individually" iZero)
        (" 1" " No bounds on any of the \htbitmap{xj}" iOne)
        (" 2" " All bounds are of the form 0 \htbitmap{great=} \htbitmap{xj}" iTwo)
        (" 3" " All \htbitmap{lj} are equal and all \htbitmap{uj} are equal" iThree))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Dimension of {\it iw}, {\it liw}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 6 liw F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Dimension of {\it w}, {\it lw}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 54 lw F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e04jafSolve)
  htShowPage()

e04jafSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  boun := htpButtonValue(htPage,'ibound)
  ibound :=
    boun = 'iZero => '0
    boun = 'iOne => '1
    boun = 'iTwo => '2
    '3
  liw :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'liw)
    objValUnwrap htpLabelSpadValue(htPage, 'liw)
  lw :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'lw)
    objValUnwrap htpLabelSpadValue(htPage, 'lw)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '4 => e04jafDefaultSolve(htPage,ibound,liw,lw,ifail)
  funcList := [['bcStrings,[50, '"XC[1]", 'f, 'EM]]]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter lower boundary conditions ")
  middle := STRCONC(middle,'"{\it bl(n)}: \newline ")
  blList := 
    "append"/[fa(i) for i in 1..n] where fa(i) ==
      xnam := INTERN STRCONC ('"bl",STRINGIMAGE i)
      [['bcStrings,[8, '"0.0", xnam, 'F]]]
  blList := [['text,:middle],:blList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter upper boundary ")
  middle := STRCONC(middle,'"conditions {\it bu(n)}: \newline ")
  buList := 
    "append"/[fb(i) for i in 1..n] where fb(i) ==
      xnam := INTERN STRCONC ('"bu",STRINGIMAGE i)
      [['bcStrings,[8, '"0.0", xnam, 'F]]]
  buList := [['text,:middle],:buList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter initial guess ")
  middle := STRCONC(middle,'"of the solution vector {\it x(n)}: \newline ")
  xList := 
    "append"/[fc(i) for i in 1..n] where fc(i) ==
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      [['bcStrings,[8, '"0.0", xnam, 'F]]]
  xList := [['text,:middle],:xList]
  equationPart := [
     '(domainConditions 
        (isDomain EM $EmptyMode)
          (isDomain F (Float))
            (isDomain I (Integer))),
                :funcList,:blList,:buList,:xList]
  page:= htInitPage('"E04JAF - Minimum, function of several variables, quasi-Newton algorithm, simple bounds, using function values only",nil)
  htSay '"\menuitemstyle{}\tab{2} "
  htSay '"Enter the function {\it F(x)} below in terms of XC[1]...XC[n]: "
  htSay '"\newline "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'e04jafGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'ibound,ibound)
  htpSetProperty(page,'liw,liw)
  htpSetProperty(page,'lw,lw)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e04jafDefaultSolve (htPage,ibound,liw,lw,ifail) ==
  n := '4
  page:= htInitPage('"E04JAF - Minimum, function of several variables, quasi-Newton algorithm, simple bounds, using function values only",nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the function {\it F(x)} below in terms of XC[1]...XC[n]: ")
    (text . "\newline ")
    (bcStrings (60 "(XC[1]+10*XC[2])**2+5*(XC[3]-XC[4])**2+(XC[2]-2*XC[3])**4+10*(XC[1]-XC[4])**4" n1 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter lower boundary conditions {\it bl(n)}: \newline ")
    (bcStrings (8 "1" bl1 F))
    (bcStrings (8 "-2" bl2 F))
    (bcStrings (8 "-1.0e-6" bl3 F))
    (bcStrings (8 "1" bl4 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter upper boundary conditions {\it bu(n)}: \newline ")
    (bcStrings (8 "3" bu1 F))
    (bcStrings (8 "0" bu2 F))
    (bcStrings (8 "1.0e6" bu3 F))
    (bcStrings (8 "3" bu4 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter initial guess of the solution vector {\it x(n)}: \newline ")
    (bcStrings (8 "3" x1 F))
    (bcStrings (8 "-1" x2 F))
    (bcStrings (8 "0" x3 F))
    (bcStrings (8 "1" x4 F)))
  htMakeDoneButton('"Continue",'e04jafGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'ibound,ibound)
  htpSetProperty(page,'liw,liw)
  htpSetProperty(page,'lw,lw)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e04jafGen htPage ==
  n := htpProperty(htPage, 'n)
  ibound := htpProperty(htPage, 'ibound)
  liw := htpProperty(htPage,'liw)
  lw := htpProperty(htPage,'lw)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..n repeat
    temp := STRCONC ((first y).1," ")
    xlist := [temp,:xlist]      
    y := rest y
  xstring := bcwords2liststring xlist    
  for i in 1..n repeat
    temp := STRCONC ((first y).1," ")
    bulist := [temp,:bulist]    
    y := rest y
  bustring := bcwords2liststring bulist    
  for i in 1..n repeat
    temp := STRCONC ((first y).1," ")
    bllist := [temp,:bllist]    
    y := rest y
  blstring := bcwords2liststring bllist    
  f := (first y).1
  prefix := STRCONC("e04jaf(",STRINGIMAGE n,",",STRINGIMAGE ibound,", ")
  prefix := STRCONC(prefix,STRINGIMAGE liw,",",STRINGIMAGE lw,", [")
  prefix := STRCONC(prefix,blstring,"],[",bustring,"],[")
  middle := STRCONC(xstring,"],",STRINGIMAGE ifail,",(")
  linkGen STRCONC(prefix,middle,f,"::Expression(Float))::ASP24(FUNCT1))")


e04mbf() ==
  htInitPage('"E04MBF - Linear programming problem",nil)
  htMakePage '(
    (domainConditions 
        (isDomain EM $EmptyMode)
        (isDomain PI (PositiveInteger))
        (isDomain F (Float)))
    (text . "\windowlink{Manual Page}{manpageXXe04mbf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e04mbf| '|NagOptimisationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "E04MBF is an easy to use routine to solve linear programming ")
    (text . "(LP) problems of the form \center{\htbitmap{e04mbf}} \newline ")
    (text . "where {\it c} is an {\it n} element vector and {\it A} is an ")
    (text . "{\it m} by {\it n} matrix, i.e., there are {\it n} variables ")
    (text . "and {\it m} linear constraints. {\it m} may be zero in which ")
    (text . "case the LP problem is subject only to bounds on the variables. ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Upper bound on number of iterations, {\it itmax}:")
    (text . "\newline\tab{2} ")
    (bcStrings (6 20 itmax PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Type of output messages required, {\it msglvl}: ")
    (radioButtons msglvl
        (" = 1 " " Printing occurs at the solution " mOne)
        (" = 0 " " Printing only if an input parameter is incorrect " mZero)
        (" < 0 " " No printing " mMinus))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of variables, {\it n}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 7 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of general linear constraints, {\it nclin}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 7 nclin PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "First dimension of array {\it a}, {\it nrowa}:")
    (text . "\newline\tab{2} ")
    (bcStrings (6 7 nrowa PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Specifies whether or not a linear objective function is present, {\it linobj}:")
    (radioButtons linobj
        ("" " true - full LP problem is solved" true)
        ("" " false - only a feasible problem is found" false))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Dimension of {\it iwork}, {\it liwork}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 14 liwork F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Dimension of {\it work}, {\it lwork}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 182 lwork F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e04mbfSolve)
  htShowPage()

e04mbfSolve htPage ==
  itmax :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'itmax)
    objValUnwrap htpLabelSpadValue(htPage, 'itmax)
  msg := htpButtonValue(htPage,'msglvl)
  msglvl :=
    msg = 'mMinus => '-1
    msg = 'mZero => '0
    '1
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  nclin :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nclin)
    objValUnwrap htpLabelSpadValue(htPage, 'nclin)
  nrowa :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nrowa)
    objValUnwrap htpLabelSpadValue(htPage, 'nrowa)
  lin := htpButtonValue(htPage,'linobj)
  linobj :=
    lin = 'true => '"true"
    '"false"
  liwork :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'liwork)
    objValUnwrap htpLabelSpadValue(htPage, 'liwork)
  lwork :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'lwork)
    objValUnwrap htpLabelSpadValue(htPage, 'lwork)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  ((nrowa = '7 and n = 7) and nclin = 7) => e04mbfDefaultSolve(htPage,itmax,msglvl,linobj,liwork,lwork,ifail)
  aList := 
    "append"/[fa(i,n) for i in 1..nrowa] where fa(i,n) ==
       labelList := 
         "append"/[fb(i,j) for j in 1..n] where fb(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i,STRINGIMAGE j)
            [['bcStrings,[8, 0, anam, 'F]]]
       prefix := ('"\newline ")
       labelList := [['text,:prefix],:labelList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter lower boundary ")
  middle := STRCONC(middle,'"conditions {\it bl(n + nclin)}: \newline ")
  blList := 
    "append"/[fc(i) for i in 1..(n+nclin)] where fc(i) ==
      blnam := INTERN STRCONC ('"bl",STRINGIMAGE i)
      [['bcStrings,[8, '"0.0", blnam, 'F]]]
  blList := [['text,:middle],:blList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter upper boundary ")
  middle := STRCONC(middle,'"conditions {\it bu(n+nclin)}: \newline ")
  buList := 
    "append"/[fd(i) for i in 1..(n+nclin)] where fd(i) ==
      bunam := INTERN STRCONC ('"bu",STRINGIMAGE i)
      [['bcStrings,[8, '"0.0", bunam, 'F]]]
  buList := [['text,:middle],:buList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter coefficients of the ")
  middle := STRCONC(middle,'"objective function {\it cvec(n)}: \newline ")
  cList := 
    "append"/[fe(i) for i in 1..n] where fe(i) ==
      cnam := INTERN STRCONC ('"c",STRINGIMAGE i)
      [['bcStrings,[8, '"0.0", cnam, 'F]]]
  cList := [['text,:middle],:cList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter initial guess ")
  middle := STRCONC(middle,'"of the solution vector {\it x(n)}: \newline ")
  xList := 
    "append"/[fg(i) for i in 1..n] where fg(i) ==
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      [['bcStrings,[8, '"0.0", xnam, 'F]]]
  xList := [['text,:middle],:xList]
  equationPart := [
     '(domainConditions 
        (isDomain EM $EmptyMode)
          (isDomain F (Float))
            (isDomain I (Integer))),
                :aList,:blList,:buList,:cList,:xList]
  page:= htInitPage('"E04MBF - Linear programming problem",nil)
  htSay '"\menuitemstyle{}\tab{2} "
  htSay '"Enter the elements of the array {\it a(nrowa,n)}: \newline "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'e04mbfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'nclin,nclin)
  htpSetProperty(page,'nrowa,nrowa)
  htpSetProperty(page,'itmax,itmax)
  htpSetProperty(page,'msglvl,msglvl)
  htpSetProperty(page,'linobj,linobj)
  htpSetProperty(page,'liwork,liwork)
  htpSetProperty(page,'lwork,lwork)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e04mbfDefaultSolve(htPage,itmax,msglvl,linobj,liwork,lwork,ifail) ==
  n := '7
  nclin := '7
  nrowa := '7
  page:= htInitPage('"E04MBF - Linear programming problem",nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the elements of array {\it a(nrowa,n)}: \newline ")
    (bcStrings (5 "1" a11 F))
    (bcStrings (5 "1" a12 F))
    (bcStrings (5 "1" a13 F))
    (bcStrings (5 "1" a14 F))
    (bcStrings (5 "1" a15 F))
    (bcStrings (5 "1" a16 F))
    (bcStrings (5 "1" a17 F))
    (text . "\newline ")
    (bcStrings (5 "0.15" a21 F))
    (bcStrings (5 "0.04" a22 F))
    (bcStrings (5 "0.02" a23 F))
    (bcStrings (5 "0.04" a24 F))
    (bcStrings (5 "0.02" a25 F))
    (bcStrings (5 "0.01" a26 F))
    (bcStrings (5 "0.03" a27 F))
    (text . "\newline ")
    (bcStrings (5 "0.03" a31 F))
    (bcStrings (5 "0.05" a32 F))
    (bcStrings (5 "0.08" a33 F))
    (bcStrings (5 "0.02" a34 F))
    (bcStrings (5 "0.06" a35 F))
    (bcStrings (5 "0.01" a36 F))
    (bcStrings (5 "0" a37 F))
    (text . "\newline ")
    (bcStrings (5 "0.02" a41 F))
    (bcStrings (5 "0.04" a42 F))
    (bcStrings (5 "0.01" a43 F))
    (bcStrings (5 "0.02" a44 F))
    (bcStrings (5 "0.02" a45 F))
    (bcStrings (5 "0" a46 F))
    (bcStrings (5 "0" a47 F))
    (text . "\newline ")
    (bcStrings (5 "0.02" a51 F))
    (bcStrings (5 "0.03" a52 F))
    (bcStrings (5 "0" a53 F))
    (bcStrings (5 "0" a54 F))
    (bcStrings (5 "0.01" a55 F))
    (bcStrings (5 "0" a56 F))
    (bcStrings (5 "0" a57 F))
    (text . "\newline ")
    (bcStrings (5 "0.7" a61 F))
    (bcStrings (5 "0.75" a62 F))
    (bcStrings (5 "0.8" a63 F))
    (bcStrings (5 "0.75" a64 F))
    (bcStrings (5 "0.8" a65 F))
    (bcStrings (5 "0.97" a66 F))
    (bcStrings (5 "0" a67 F))
    (text . "\newline ")
    (bcStrings (5 "0.02" a71 F))
    (bcStrings (5 "0.06" a72 F))
    (bcStrings (5 "0.08" a73 F))
    (bcStrings (5 "0.12" a74 F))
    (bcStrings (5 "0.02" a75 F))
    (bcStrings (5 "0.01" a76 F))
    (bcStrings (5 "0.97" a77 F))
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter lower boundary conditions {\it bl(n+nclin)}: \newline ")
    (bcStrings (8 "-0.01" bl1 F))
    (bcStrings (8 "-0.1" bl2 F))
    (bcStrings (8 "-0.01" bl3 F))
    (bcStrings (8 "-0.04" bl4 F))
    (bcStrings (8 "-0.1" bl5 F))
    (bcStrings (8 "-0.01" bl6 F))
    (bcStrings (8 "-0.01" bl7 F))
    (bcStrings (8 "-0.13" bl8 F))
    (bcStrings (8 "-1.0e+21" bl9 F))
    (bcStrings (8 "-1.0e+21" bl10 F))
    (bcStrings (8 "-1.0e+21" bl11 F))
    (bcStrings (8 "-1.0e+21" bl12 F))
    (bcStrings (8 "-0.0992" bl13 F))
    (bcStrings (8 "-0.003" bl14 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter upper boundary conditions {\it bu(n+nclin)}: \newline ")
    (bcStrings (8 "0.01" bu1 F))
    (bcStrings (8 "0.15" bu2 F))
    (bcStrings (8 "0.03" bu3 F))
    (bcStrings (8 "0.02" bu4 F))
    (bcStrings (8 "0.05" bu5 F))
    (bcStrings (8 "1.0e+21" bu6 F))
    (bcStrings (8 "1.0e+21" bu7 F))
    (bcStrings (8 "-0.13" bu8 F))
    (bcStrings (8 "-0.0049" bu9 F))
    (bcStrings (8 "-0.0064" bu10 F))
    (bcStrings (8 "-0.0037" bu11 F))
    (bcStrings (8 "-0.0012" bu12 F))
    (bcStrings (8 "1.0e+21" bu13 F))
    (bcStrings (8 "0.002" bu14 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter coefficients of the objective function, {\it cvec(n)}: ") 
    (text . "\newline ")
    (bcStrings (8 "-0.02" c1 F))
    (bcStrings (8 "-0.2" c2 F))
    (bcStrings (8 "-0.2" c3 F))
    (bcStrings (8 "-0.2" c4 F))
    (bcStrings (8 "-0.2" c5 F))
    (bcStrings (8 "0.04" c6 F))
    (bcStrings (8 "0.04" c7 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter initial guess of the solution vector, {\it x(n)}: ")
    (text . "\newline ")
    (bcStrings (8 "-0.01" x1 F))
    (bcStrings (8 "-0.03" x2 F))
    (bcStrings (8 "0.0" x3 F))
    (bcStrings (8 "-0.01" x4 F))
    (bcStrings (8 "-0.1" x5 F))
    (bcStrings (8 "0.02" x6 F))
    (bcStrings (8 "0.01" x7 F)))
  htMakeDoneButton('"Continue",'e04mbfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'nclin,nclin)
  htpSetProperty(page,'nrowa,nrowa)
  htpSetProperty(page,'itmax,itmax)
  htpSetProperty(page,'msglvl,msglvl)
  htpSetProperty(page,'linobj,linobj)
  htpSetProperty(page,'liwork,liwork)
  htpSetProperty(page,'lwork,lwork)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e04mbfGen htPage ==
  n := htpProperty(htPage, 'n)
  nclin := htpProperty(htPage, 'nclin)
  nrowa := htpProperty(htPage, 'nrowa)
  itmax := htpProperty(htPage, 'itmax)
  msglvl := htpProperty(htPage, 'msglvl)
  linobj := htpProperty(htPage, 'linobj)
  liwork := htpProperty(htPage,'liwork)
  lwork := htpProperty(htPage,'lwork)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..n repeat
    temp := STRCONC ((first y).1," ")
    xlist := [temp,:xlist]      
    y := rest y
  xstring := bcwords2liststring xlist    
  for i in 1..n repeat
    temp := STRCONC ((first y).1," ")
    clist := [temp,:clist]      
    y := rest y
  cstring := bcwords2liststring clist    
  for i in 1..(n+nclin) repeat
    temp := STRCONC ((first y).1," ")
    bulist := [temp,:bulist]    
    y := rest y
  bustring := bcwords2liststring bulist    
  for i in 1..(n+nclin) repeat
    temp := STRCONC ((first y).1," ")
    bllist := [temp,:bllist]    
    y := rest y
  blstring := bcwords2liststring bllist    
  for i in 1..nrowa repeat -- matrix A
    for j in 1..n repeat
      a := STRCONC((first y).1," ")
      arrlist := [a,:arrlist]
      y := rest y
    amatlist := [:amatlist,arrlist]
    arrlist := []
  amatlist := reverse amatlist 
  amatstr := bcwords2liststring [bcwords2liststring x for x in amatlist]  
  nctotl := n + nclin
  prefix := STRCONC("e04mbf(",STRINGIMAGE itmax,",", STRINGIMAGE msglvl,", ")
  prefix := STRCONC(prefix,STRINGIMAGE n,",",STRINGIMAGE nclin,", ")
  prefix := STRCONC(prefix,STRINGIMAGE nctotl,",",STRINGIMAGE nrowa,", ")
  middle := STRCONC(amatstr,",[")
  middle := STRCONC(middle,blstring,"],[",bustring,"],[",cstring)
  middle := STRCONC(middle,"],",linobj,", ",STRINGIMAGE liwork)
  middle := STRCONC(middle,",",STRINGIMAGE lwork,",[")
  middle := STRCONC(middle,xstring,"],",STRINGIMAGE ifail,")")
  linkGen STRCONC(prefix,middle)



e04naf() ==
  htInitPage('"E04NAF - Quadratic programming problem",nil)
  htMakePage '(
    (domainConditions 
        (isDomain EM $EmptyMode)
        (isDomain PI (PositiveInteger))
        (isDomain F (Float)))
    (text . "\windowlink{Manual Page}{manpageXXe04naf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e04naf| '|NagOptimisationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "E04NAF is a comprehensive routine to solve quadratic problems ")
    (text . "(QP) of the form \center{\htbitmap{e04naf}} \newline ")
    (text . "where {\it c} is a constant {\it n} element vector, {\it H} is a")
    (text . " constant {\it n} by {\it n} symmetric matrix, and the matrix ")
    (text . "{\it A} is {\it m} by {\it n}, i.e. there are {\it n} variables ")
    (text . "and {\it m} general linear constraints. {\it m} may be zero in ")
    (text . "which case the LP problem is subject only to bounds on the ")
    (text . "variables. \blankline If {\it H} = 0 a flag can be set so that ")
    (text . "the problem is treated as a linear programming (LP) problem. ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Upper bound on number of iterations, {\it itmax}:")
    (text . "\newline\tab{2} ")
    (bcStrings (6 20 itmax PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Type of output messages required, {\it msglvl}: ")
    (radioButtons msglvl
        (" < 0 " " No printing " mMinus)
        (" = 0 " " Printing only if an input parameter is incorrect or overflow is likely" mZero)
        (" = 1" " Printing occurs at the solution " mOne)
        (" = 5" " One line of output for each constraint addition or deletion, no printout" mFive)
        (" \htbitmap{great=} 10" " As above with printout of the solution" mTen)
        (" \htbitmap{great=} 15" " As above with X, ISTATE and indices of free variables at each iteration" mFifteen)
        (" \htbitmap{great=} 20" " As above with the Lagrange multiplier estimates and the free variables at each iteration" mTwenty)
        (" \htbitmap{great=} 30" " As above with the diagonal elements of the matrix {\it T} associated with the {\it TQ} factorization of the working set, and the diagonal elements of the Cholesky factor {\it R} of the projected Hessian" mThirty)
        (" \htbitmap{great=} 80" " As above with debug printout" mEighty)        
        (" = 99" " As above with arrays {\it cvec} and {\it hess}" mNinetyNine))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of variables, {\it n}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 7 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of general linear constraints, {\it nclin}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 7 nclin PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "First dimension of array {\it a}, {\it nrowa}:")
    (text . "\newline\tab{2} ")
    (bcStrings (6 7 nrowa PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "First dimension of array {\it hess}, {\it nrowh}:")
    (text . "\newline\tab{2} ")
    (bcStrings (6 7 nrowh PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Second dimension of array {\it hess}, {\it ncolh}:")
    (text . "\newline\tab{2} ")
    (bcStrings (6 7 ncolh PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Size above which a bound is regarded as infinite, {\it bigbnd}:")
    (text . "\newline\tab{2} ")
    (bcStrings (10 "1.0e10" bigbnd F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Specifies whether or not an initial estimate of the active constraints is present, {\it cold}:")
    (radioButtons cold
        ("" " true - E04NAF determines the initial working set" cTrue)
        ("" " false - user defined contents of array {\it istate}" cFalse))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Specifies whether or not {\it h} is a zero matrix, {\it lp}:")
    (radioButtons lp
        ("" " false - QP problem " lFalse)
        ("" " true - LP problem, {\it hess} and {\it qphess} are not referenced " lTrue))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Specifies whether or not orthogonal transformations are to be used in computing and updating the working set, {\it orthog}:")
    (radioButtons orthog
        ("" " true " oTrue)
        ("" " false " oFalse))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Dimension of {\it iwork}, {\it liwork}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 14 liwork F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Dimension of {\it work}, {\it lwork}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 238 lwork F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e04nafSolve)
  htShowPage()

e04nafSolve htPage ==
  itmax :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'itmax)
    objValUnwrap htpLabelSpadValue(htPage, 'itmax)
  msg := htpButtonValue(htPage,'msglvl)
  msglvl :=
    msg = 'mMinus => '-1
    msg = 'mZero => '0
    msg = 'mOne => '1
    msg = 'mFive => '5
    msg = 'mTen => '10
    msg = 'mFifteen => '15
    msg = 'mTwenty => '20
    msg = 'mThirty => '30
    msg = 'mEighty => '80
    '99
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  nclin :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nclin)
    objValUnwrap htpLabelSpadValue(htPage, 'nclin)
  nrowa :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nrowa)
    objValUnwrap htpLabelSpadValue(htPage, 'nrowa)
  nrowh :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nrowh)
    objValUnwrap htpLabelSpadValue(htPage, 'nrowh)
  ncolh :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ncolh)
    objValUnwrap htpLabelSpadValue(htPage, 'ncolh)
  bigbnd := htpLabelInputString(htPage,'bigbnd)
  col := htpButtonValue(htPage,'cold)
  cold :=
    col = 'cTrue => '"true"
    '"false"
  linear := htpButtonValue(htPage,'lp)
  lp :=
    linear = 'lTrue => '"true"
    '"false"
  ortho := htpButtonValue(htPage,'orthog)
  orthog :=
    ortho = 'oTrue => '"true"
    '"false"
  liwork :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'liwork)
    objValUnwrap htpLabelSpadValue(htPage, 'liwork)
  lwork :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'lwork)
    objValUnwrap htpLabelSpadValue(htPage, 'lwork)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (((nrowa = '7 and n = '7) and (nrowh = '7 and ncolh ='7)) and nclin = '7) => 
     e04nafDefaultSolve(htPage,itmax,msglvl,bigbnd,cold,lp,orthog,liwork,lwork,ifail)
  aList := 
    "append"/[fa(i,n) for i in 1..nrowa] where fa(i,n) ==
       labelList := 
         "append"/[fb(i,j) for j in 1..n] where fb(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i,STRINGIMAGE j)
            [['bcStrings,[8, 0, anam, 'F]]]
       prefix := ('"\newline ")
       labelList := [['text,:prefix],:labelList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter lower boundary ")
  middle := STRCONC(middle,'"conditions {\it bl(n + nclin)}: \newline ")
  blList := 
    "append"/[fc(i) for i in 1..(n+nclin)] where fc(i) ==
      blnam := INTERN STRCONC ('"bl",STRINGIMAGE i)
      [['bcStrings,[8, '"0.0", blnam, 'F]]]
  blList := [['text,:middle],:blList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter upper boundary ")
  middle := STRCONC(middle,'"conditions {\it bu(n+nclin)}: \newline ")
  buList := 
    "append"/[fd(i) for i in 1..(n+nclin)] where fd(i) ==
      bunam := INTERN STRCONC ('"bu",STRINGIMAGE i)
      [['bcStrings,[8, '"0.0", bunam, 'F]]]
  buList := [['text,:middle],:buList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter coefficients of the ")
  middle := STRCONC(middle,'"objective function {\it cvec(n)}: \newline ")
  cList := 
    "append"/[fe(i) for i in 1..n] where fe(i) ==
      cnam := INTERN STRCONC ('"c",STRINGIMAGE i)
      [['bcStrings,[8, '"0.0", cnam, 'F]]]
  cList := [['text,:middle],:cList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter set of positive ")
  middle := STRCONC(middle,'"tolerances {\it featol(n+nclin)}: \newline ")
  fList := 
    "append"/[ff(i) for i in 1..(n+nclin)] where ff(i) ==
      fnam := INTERN STRCONC ('"f",STRINGIMAGE i)
      [['bcStrings,[9, '"0.1053e-7", fnam, 'F]]]
  fList := [['text,:middle],:fList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter the elements of ")
  middle := STRCONC(middle,'"array {\it hess(nrowh,ncolh)}: \newline ")
  hList := 
    "append"/[fh(i,n) for i in 1..nrowh] where fh(i,n) ==
       labelList := 
         "append"/[fi(i,j) for j in 1..n] where fi(i,j) ==
            hnam := INTERN STRCONC ('"h",STRINGIMAGE i,STRINGIMAGE j)
            [['bcStrings,[8, 0, hnam, 'F]]]
       prefix := ('"\newline ")
       labelList := [['text,:prefix],:labelList]
  hList := [['text,:middle],:hList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter initial guess ")
  middle := STRCONC(middle,'"of the solution vector {\it x(n)}: \newline ")
  xList := 
    "append"/[fg(i) for i in 1..n] where fg(i) ==
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      [['bcStrings,[8, '"0.0", xnam, 'F]]]
  xList := [['text,:middle],:xList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} If {\it cold} = false ")
  middle := STRCONC(middle,'"enter {\it istate(n+nclin)} values: \newline ")
  iList := 
    "append"/[fj(i) for i in 1..(n+nclin)] where fj(i) ==
      inam := INTERN STRCONC ('"i",STRINGIMAGE i)
      [['bcStrings,[8, '"0.0", inam, 'F]]]
  iList := [['text,:middle],:iList]
  equationPart := [
     '(domainConditions 
        (isDomain EM $EmptyMode)
          (isDomain F (Float))
            (isDomain I (Integer))),
                :aList,:blList,:buList,:cList,:fList,:hList,:xList,:iList]
  page:= htInitPage('"E04NAF - Quadratic programming problem",nil)
  htSay '"\menuitemstyle{}\tab{2} "
  htSay '"Enter the elements of the array {\it a(nrowa,n)}: \newline "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'e04nafGen)
  htpSetProperty(page,'itmax,itmax)
  htpSetProperty(page,'msglvl,msglvl)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'nclin,nclin)
  htpSetProperty(page,'nrowa,nrowa)
  htpSetProperty(page,'nrowh,nrowh)
  htpSetProperty(page,'ncolh,ncolh)
  htpSetProperty(page,'bigbnd,bigbnd)
  htpSetProperty(page,'cold,cold)
  htpSetProperty(page,'lp,lp)
  htpSetProperty(page,'orthog,orthog)
  htpSetProperty(page,'liwork,liwork)
  htpSetProperty(page,'lwork,lwork)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e04nafDefaultSolve(htPage,itmax,msglvl,bigbnd,cold,lp,orthog,liwork,lwork,ifail) ==
  n := '7
  nclin := '7
  nrowa := '7
  nrowh := '7
  ncolh := '7
  page:= htInitPage('"E04NAF - Quadratic programming problem",nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the elements of array {\it a(nrowa,n)}: \newline ")
    (bcStrings (5 "1" a11 F))
    (bcStrings (5 "1" a12 F))
    (bcStrings (5 "1" a13 F))
    (bcStrings (5 "1" a14 F))
    (bcStrings (5 "1" a15 F))
    (bcStrings (5 "1" a16 F))
    (bcStrings (5 "1" a17 F))
    (text . "\newline ")
    (bcStrings (5 "0.15" a21 F))
    (bcStrings (5 "0.04" a22 F))
    (bcStrings (5 "0.02" a23 F))
    (bcStrings (5 "0.04" a24 F))
    (bcStrings (5 "0.02" a25 F))
    (bcStrings (5 "0.01" a26 F))
    (bcStrings (5 "0.03" a27 F))
    (text . "\newline ")
    (bcStrings (5 "0.03" a31 F))
    (bcStrings (5 "0.05" a32 F))
    (bcStrings (5 "0.08" a33 F))
    (bcStrings (5 "0.02" a34 F))
    (bcStrings (5 "0.06" a35 F))
    (bcStrings (5 "0.01" a36 F))
    (bcStrings (5 "0" a37 F))
    (text . "\newline ")
    (bcStrings (5 "0.02" a41 F))
    (bcStrings (5 "0.04" a42 F))
    (bcStrings (5 "0.01" a43 F))
    (bcStrings (5 "0.02" a44 F))
    (bcStrings (5 "0.02" a45 F))
    (bcStrings (5 "0" a46 F))
    (bcStrings (5 "0" a47 F))
    (text . "\newline ")
    (bcStrings (5 "0.02" a51 F))
    (bcStrings (5 "0.03" a52 F))
    (bcStrings (5 "0" a53 F))
    (bcStrings (5 "0" a54 F))
    (bcStrings (5 "0.01" a55 F))
    (bcStrings (5 "0" a56 F))
    (bcStrings (5 "0" a57 F))
    (text . "\newline ")
    (bcStrings (5 "0.7" a61 F))
    (bcStrings (5 "0.75" a62 F))
    (bcStrings (5 "0.8" a63 F))
    (bcStrings (5 "0.75" a64 F))
    (bcStrings (5 "0.8" a65 F))
    (bcStrings (5 "0.97" a66 F))
    (bcStrings (5 "0" a67 F))
    (text . "\newline ")
    (bcStrings (5 "0.02" a71 F))
    (bcStrings (5 "0.06" a72 F))
    (bcStrings (5 "0.08" a73 F))
    (bcStrings (5 "0.12" a74 F))
    (bcStrings (5 "0.02" a75 F))
    (bcStrings (5 "0.01" a76 F))
    (bcStrings (5 "0.97" a77 F))
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter lower boundary conditions {\it bl(n+nclin)}: \newline ")
    (bcStrings (8 "-0.01" bl1 F))
    (bcStrings (8 "-0.1" bl2 F))
    (bcStrings (8 "-0.01" bl3 F))
    (bcStrings (8 "-0.04" bl4 F))
    (bcStrings (8 "-0.1" bl5 F))
    (bcStrings (8 "-0.01" bl6 F))
    (bcStrings (8 "-0.01" bl7 F))
    (bcStrings (8 "-0.13" bl8 F))
    (bcStrings (8 "-1.0e+21" bl9 F))
    (bcStrings (8 "-1.0e+21" bl10 F))
    (bcStrings (8 "-1.0e+21" bl11 F))
    (bcStrings (8 "-1.0e+21" bl12 F))
    (bcStrings (8 "-0.0992" bl13 F))
    (bcStrings (8 "-0.003" bl14 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter upper boundary conditions {\it bu(n+nclin)}: \newline ")
    (bcStrings (8 "0.01" bu1 F))
    (bcStrings (8 "0.15" bu2 F))
    (bcStrings (8 "0.03" bu3 F))
    (bcStrings (8 "0.02" bu4 F))
    (bcStrings (8 "0.05" bu5 F))
    (bcStrings (8 "1.0e+21" bu6 F))
    (bcStrings (8 "1.0e+21" bu7 F))
    (bcStrings (8 "-0.13" bu8 F))
    (bcStrings (8 "-0.0049" bu9 F))
    (bcStrings (8 "-0.0064" bu10 F))
    (bcStrings (8 "-0.0037" bu11 F))
    (bcStrings (8 "-0.0012" bu12 F))
    (bcStrings (8 "1.0e+21" bu13 F))
    (bcStrings (8 "0.002" bu14 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter coefficients of the objective function, {\it cvec(n)}: ") 
    (text . "\newline ")
    (bcStrings (8 "-0.02" c1 F))
    (bcStrings (8 "-0.2" c2 F))
    (bcStrings (8 "-0.2" c3 F))
    (bcStrings (8 "-0.2" c4 F))
    (bcStrings (8 "-0.2" c5 F))
    (bcStrings (8 "0.04" c6 F))
    (bcStrings (8 "0.04" c7 F))
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter set of positive tolerances {\it featol(n+nclin)}:\newline ")
    (bcStrings (9 "0.1053e-7" f1 F))
    (bcStrings (9 "0.1053e-7" f2 F))
    (bcStrings (9 "0.1053e-7" f3 F))
    (bcStrings (9 "0.1053e-7" f4 F))
    (bcStrings (9 "0.1053e-7" f5 F))
    (bcStrings (9 "0.1053e-7" f6 F))
    (bcStrings (9 "0.1053e-7" f7 F))
    (bcStrings (9 "0.1053e-7" f8 F))
    (bcStrings (9 "0.1053e-7" f9 F))
    (bcStrings (9 "0.1053e-7" f10 F))
    (bcStrings (9 "0.1053e-7" f11 F))
    (bcStrings (9 "0.1053e-7" f12 F))
    (bcStrings (9 "0.1053e-7" f13 F))
    (bcStrings (9 "0.1053e-7" f14 F))
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the elements of array {\it hess(nrowh,ncolh)}: \newline ")
    (bcStrings (5 "2" h11 F))
    (bcStrings (5 "0" h12 F))
    (bcStrings (5 "0" h13 F))
    (bcStrings (5 "0" h14 F))
    (bcStrings (5 "0" h15 F))
    (bcStrings (5 "0" h16 F))
    (bcStrings (5 "0" h17 F))
    (text . "\newline ")
    (bcStrings (5 "0" h21 F))
    (bcStrings (5 "2" h22 F))
    (bcStrings (5 "0" h23 F))
    (bcStrings (5 "0" h24 F))
    (bcStrings (5 "0" h25 F))
    (bcStrings (5 "0" h26 F))
    (bcStrings (5 "0" h27 F))
    (text . "\newline ")
    (bcStrings (5 "0" h31 F))
    (bcStrings (5 "0" h32 F))
    (bcStrings (5 "2" h33 F))
    (bcStrings (5 "2" h34 F))
    (bcStrings (5 "0" h35 F))
    (bcStrings (5 "0" h36 F))
    (bcStrings (5 "0" h37 F))
    (text . "\newline ")
    (bcStrings (5 "0" h41 F))
    (bcStrings (5 "0" h42 F))
    (bcStrings (5 "2" h43 F))
    (bcStrings (5 "2" h44 F))
    (bcStrings (5 "0" h45 F))
    (bcStrings (5 "0" h46 F))
    (bcStrings (5 "0" h47 F))
    (text . "\newline ")
    (bcStrings (5 "0" h51 F))
    (bcStrings (5 "0" h52 F))
    (bcStrings (5 "0" h53 F))
    (bcStrings (5 "0" h54 F))
    (bcStrings (5 "2" h55 F))
    (bcStrings (5 "0" h56 F))
    (bcStrings (5 "0" h57 F))
    (text . "\newline ")
    (bcStrings (5 "0" h61 F))
    (bcStrings (5 "0" h62 F))
    (bcStrings (5 "0" h63 F))
    (bcStrings (5 "0" h64 F))
    (bcStrings (5 "0" h65 F))
    (bcStrings (5 "-2" h66 F))
    (bcStrings (5 "-2" h67 F))
    (text . "\newline ")
    (bcStrings (5 "0" h71 F))
    (bcStrings (5 "0" h72 F))
    (bcStrings (5 "0" h73 F))
    (bcStrings (5 "0" h74 F))
    (bcStrings (5 "0" h75 F))
    (bcStrings (5 "-2" h76 F))
    (bcStrings (5 "-2" h77 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter initial guess of the solution vector, {\it x(n)}: ")
    (text . "\newline ")
    (bcStrings (8 "-0.01" x1 F))
    (bcStrings (8 "-0.03" x2 F))
    (bcStrings (8 "0.0" x3 F))
    (bcStrings (8 "-0.01" x4 F))
    (bcStrings (8 "-0.1" x5 F))
    (bcStrings (8 "0.02" x6 F))
    (bcStrings (8 "0.01" x7 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "If {\it cold} = false enter {\it istate(n+nclin)} values: ")
    (text . "\newline ")
    (bcStrings (8 "0" i1 F))
    (bcStrings (8 "0" i2 F))
    (bcStrings (8 "0" i3 F))
    (bcStrings (8 "0" i4 F))
    (bcStrings (8 "0" i5 F))
    (bcStrings (8 "0" i6 F))
    (bcStrings (8 "0" i7 F))
    (bcStrings (8 "0" i8 F))
    (bcStrings (8 "0" i9 F))
    (bcStrings (8 "0" i10 F))
    (bcStrings (8 "0" i11 F))
    (bcStrings (8 "0" i12 F))
    (bcStrings (8 "0" i13 F))
    (bcStrings (8 "0" i14 F)))
  htMakeDoneButton('"Continue",'e04nafGen)
  htpSetProperty(page,'itmax,itmax)
  htpSetProperty(page,'msglvl,msglvl)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'nclin,nclin)
  htpSetProperty(page,'nrowa,nrowa)
  htpSetProperty(page,'nrowh,nrowh)
  htpSetProperty(page,'ncolh,ncolh)
  htpSetProperty(page,'bigbnd,bigbnd)
  htpSetProperty(page,'cold,cold)
  htpSetProperty(page,'lp,lp)
  htpSetProperty(page,'orthog,orthog)
  htpSetProperty(page,'liwork,liwork)
  htpSetProperty(page,'lwork,lwork)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e04nafGen htPage ==
  itmax := htpProperty(htPage, 'itmax)
  msglvl := htpProperty(htPage, 'msglvl)
  n := htpProperty(htPage, 'n)
  nclin := htpProperty(htPage, 'nclin)
  nrowa := htpProperty(htPage, 'nrowa)
  nrowh := htpProperty(htPage, 'nrowh)
  ncolh := htpProperty(htPage, 'ncolh)
  bigbnd := htpProperty(htPage, 'bigbnd)
  cold := htpProperty(htPage, 'cold)
  lp := htpProperty(htPage, 'lp)
  orthog := htpProperty(htPage, 'orthog)
  liwork := htpProperty(htPage,'liwork)
  lwork := htpProperty(htPage,'lwork)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..(n+nclin) repeat
    temp := STRCONC ((first y).1," ")
    ilist := [temp,:ilist]      
    y := rest y
  istring := bcwords2liststring ilist    
  for i in 1..n repeat
    temp := STRCONC ((first y).1," ")
    xlist := [temp,:xlist]      
    y := rest y
  xstring := bcwords2liststring xlist    
  for i in 1..nrowh repeat -- matrix H
    for j in 1..ncolh repeat
      h := STRCONC((first y).1," ")
      hlist := [h,:hlist]
      y := rest y
    hmatlist := [:hmatlist,hlist]
    hlist := []
  hmatlist := reverse hmatlist 
  hmatstr := bcwords2liststring [bcwords2liststring x for x in hmatlist]  
  for i in 1..(n+nclin) repeat
    temp := STRCONC ((first y).1," ")
    flist := [temp,:flist]      
    y := rest y
  fstring := bcwords2liststring flist    
  for i in 1..n repeat
    temp := STRCONC ((first y).1," ")
    clist := [temp,:clist]      
    y := rest y
  cstring := bcwords2liststring clist    
  for i in 1..(n+nclin) repeat
    temp := STRCONC ((first y).1," ")
    bulist := [temp,:bulist]    
    y := rest y
  bustring := bcwords2liststring bulist    
  for i in 1..(n+nclin) repeat
    temp := STRCONC ((first y).1," ")
    bllist := [temp,:bllist]    
    y := rest y
  blstring := bcwords2liststring bllist    
  for i in 1..nrowa repeat -- matrix A
    for j in 1..n repeat
      a := STRCONC((first y).1," ")
      arrlist := [a,:arrlist]
      y := rest y
    amatlist := [:amatlist,arrlist]
    arrlist := []
  amatlist := reverse amatlist 
  amatstr := bcwords2liststring [bcwords2liststring x for x in amatlist]  
  nctotl := n + nclin
  prefix := STRCONC("e04naf(",STRINGIMAGE itmax,",", STRINGIMAGE msglvl,", ")
  prefix := STRCONC(prefix,STRINGIMAGE n,",",STRINGIMAGE nclin,", ")
  prefix := STRCONC(prefix,STRINGIMAGE nctotl,",",STRINGIMAGE nrowa,", ")
  prefix := STRCONC(prefix,STRINGIMAGE nrowh,",",STRINGIMAGE ncolh,", ",bigbnd)
  middle := STRCONC(", ",amatstr,",[")
  middle := STRCONC(middle,blstring,"],[",bustring,"],[",cstring)
  middle := STRCONC(middle,"],[",fstring,"],",hmatstr,",",STRINGIMAGE cold,",")
  middle := STRCONC(middle,STRINGIMAGE lp,", ",STRINGIMAGE orthog,", ")
  middle := STRCONC(middle,STRINGIMAGE liwork,",",STRINGIMAGE lwork,",[")
  middle := STRCONC(middle,xstring,"],[",istring,"]::Matrix Integer,")
  middle := STRCONC(middle,STRINGIMAGE ifail)
  end := STRCONC(",((",hmatstr,")::Matrix Expression Float)::ASP20('QPHESS))")
  linkGen STRCONC(prefix,middle,end)

e04ucf() ==
  htInitPage('"E04UCF - Minimum, function of several variables, sequential QP method, nonlinear constraints, using function values and optionally 1st derivatives", nil)
  htMakePage '(
    (domainConditions 
        (isDomain EM $EmptyMode)
        (isDomain PI (PositiveInteger))
        (isDomain F (Float)))
    (text . "\windowlink{Manual Page}{manpageXXe04ucf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e04ucf| '|NagOptimisationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "E04UCF minimizes an arbitrary smooth function subject to ")
    (text . "constraints which may include simple bounds on the variables, ")
    (text . "linear constraints and smooth nonlinear constraints. As many ")
    (text . "first partial derivatives as possible should be supplied by the ")
    (text . "user, unspecified derivatives being estimated by finite ")
    (text . "differences. \newline The routine solves problems of the form")
    (text . "\center{\htbitmap{e04ucf}}\newline where the objective function ")
    (text . "{\it F(x)} is nonlinear, \htbitmap{Al} is an \htbitmap{nl} by n ")
    (text . "constant matrix and {\it c(x)} is an \htbitmap{nn} element ")
    (text . "vector of nonlinear constraint functions. The objective function")
    (text . " and constraint functions are assumed to be smooth (i.e. at ")
    (text . "least twice continuously differentiable), although the method ")
    (text . "will usually work if there are discontinuities away from the ")
    (text . "solution. \blankline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the number of variables, {\it n}: ")
    (text . "\newline ")
    (bcStrings (5 4 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the number of general linear constraints, {\it nclin}: ")
    (text . "\newline ")
    (bcStrings (5 1 nclin PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the number of nonlinear constraints, {\it ncnln}: ")
    (text . "\newline ")
    (bcStrings (5 2 ncnln PI))
    (text . "\blankline ")
    (text . "Change optional parameters:")
    (radioButtons optional
        ("" " No" no)
        ("" " Yes" yes))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Start value:")
    (radioButtons start
        ("" " Cold start" false)
        ("" " Warm start" true))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e04ucfSolve)
  htShowPage()


e04ucfSolve(htPage) ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  nclin :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nclin)
    objValUnwrap htpLabelSpadValue(htPage, 'nclin)
  ncnln :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ncnln)
    objValUnwrap htpLabelSpadValue(htPage, 'ncnln)
  nrowa :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nclin)
    objValUnwrap htpLabelSpadValue(htPage, 'nrowa)
  nrowj :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'ncnln)
    objValUnwrap htpLabelSpadValue(htPage, 'nrowj)
  nrowr :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'nrowr)
  liwork := 3*n+nclin+2*ncnln
  lwork :=
    (ncnln = '0 and nclin = '0) => 20*n
    (ncnln = '0 and nclin > '0) => 2*n*n + 20*n + 11*nclin
    (ncnln > '0 and nclin >= '0) => 2*n*n + n*nclin +2*n*ncnln + 20*n + 11*nclin + 21*ncnln
    '1 
  initial := htpButtonValue(htPage,'start)
  start :=
    initial = 'true => '1
    '0
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  param := htpButtonValue(htPage,'optional)
  optional := 
    param = 'no => '0
    '1
  ((n = '4  and optional = '0 and nclin=1 and ncnln=2) and (start = '0)) => 
    e04ucfDefaultSolve(htPage,nclin,ncnln,nrowa,nrowj,nrowr,liwork,lwork,ifail)
  start = '1 => e04ucfCopOut()
  optional := '1
  aList := 
    "append"/[fa(i,n) for i in 1..nrowa] where fa(i,n) ==
       labelList := 
         "append"/[fb(i,j) for j in 1..n] where fb(i,j) ==
            anam := INTERN STRCONC ('"a",STRINGIMAGE i,STRINGIMAGE j)
            [['bcStrings,[8, 0, anam, 'F]]]
       prefix := ('"\newline ")
       labelList := [['text,:prefix],:labelList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter lower boundary ")
  middle := STRCONC(middle,'"conditions {\it bl(n+nclin+ncnln)}: \newline ")
  blList := 
    "append"/[fc(i) for i in 1..(n+nclin+ncnln)] where fc(i) ==
      blnam := INTERN STRCONC ('"bl",STRINGIMAGE i)
      [['bcStrings,[8, '"-1.E25", blnam, 'F]]]
  blList := [['text,:middle],:blList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter upper boundary ")
  middle := STRCONC(middle,'"conditions {\it bu(n+nclin+ncnln)}: \newline ")
  buList := 
    "append"/[fd(i) for i in 1..(n+nclin+ncnln)] where fd(i) ==
      bunam := INTERN STRCONC ('"bu",STRINGIMAGE i)
      [['bcStrings,[8, '"1.E25", bunam, 'F]]]
  buList := [['text,:middle],:buList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter the nonlinear ")
  middle := STRCONC(middle,'"constraint functions {\it c(ncnln)} ")
  middle := STRCONC(middle,'"in terms of X[1]...X[n]: \newline ")
  cList := 
    "append"/[fe(i) for i in 1..ncnln] where fe(i) ==
      lineEnd := ('"\newline \tab{2} ")
      cnam := INTERN STRCONC ('"c",STRINGIMAGE i)
      [['text,:lineEnd],['bcStrings,[55, '"X[1]", cnam, 'F]]]
  cList := [['text,:middle],:cList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter the objective ")
  middle := STRCONC(middle,'"function, {\it F(x)} ")
  middle := STRCONC(middle,'"in terms of X[1]...X[n]: \newline ")
  funcList := [['bcStrings,[55, '"X[1]", 'f, 'EM]]]
  funcList := [['text,:middle],:funcList]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter initial guess ")
  middle := STRCONC(middle,'"of the solution vector {\it x(n)}: \newline ")
  xList := 
    "append"/[fg(i) for i in 1..n] where fg(i) ==
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      [['bcStrings,[8, '"0.0", xnam, 'F]]]
  xList := [['text,:middle],:xList]
  equationPart := [
     '(domainConditions 
        (isDomain EM $EmptyMode)
          (isDomain F (Float))
            (isDomain I (Integer))),
                :aList,:blList,:buList,:cList,:funcList,:xList,
                  :'(
    (text . "\blankline ")_
    (text . "\newline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Crash tolerance, {\it cra}: ")_
    (text . "\newline ")_
    (bcStrings (20 "0.01" cra F))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Derivative level, {\it der}: ")_
    (text . "\newline ")_
    (bcStrings (5 3 der PI))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Feasibility tolerance, {\it fea}: ")_
    (text . "\newline ")_
    (bcStrings (20 "0.1053671201E-7" fea F))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Function Precision, {\it fun}: ")_
    (text . "\newline ")_
    (bcStrings (20 "0.4373903510E-14" fun F))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2}")_
    (text . "{\it r} is a Hessian matrix :")_
    (radioButtons hess _
        ("" " No" hFalse)_
        ("" " Yes" hTrue))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Infinite bound size, {\it infb}: ")_
    (text . "\newline ")_
    (bcStrings (20 "1.00E+15" infb F))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Infinite step size, {\it infs}: ")_
    (text . "\newline ")_
    (bcStrings (20 "1.00E+15" infs F))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Linear feasibility tolerance, {\it linf}: ")_
    (text . "\newline ")_
    (bcStrings (20 "0.1053671201E-7" linf F))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Linesearch tolerance, {\it lint}: ")_
    (text . "\newline ")_
    (bcStrings (20 "0.9" lint F))_
    (text . "\blankline ")_
    (text . "\newline ")_
    (text . "\menuitemstyle{}\tab{2}")_
    (text . "List parameters:")_
    (radioButtons list _
        ("" " No" false)_
        ("" " Yes" true))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Major iteration limit, {\it maji}: ")_
    (text . "\newline ")_
    (bcStrings (5 30 maji PI))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Major print level, {\it majp}: ")_
    (text . "\newline ")_
    (bcStrings (5 1 majp PI))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Minor iteration limit, {\it mini}: ")_
    (text . "\newline ")_
    (bcStrings (5 81 mini PI))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Minor print level, {\it minp}: ")_
    (text . "\newline ")_
    (bcStrings (5 0 minp PI))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Monitoring channel, {\it mon}. ")_
    (text . "(Ignored in Foundation Library version.) ")_
    (text . "\newline ")_
    (bcStrings (5 "-1" mon F))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Nonlinear feasibiltity tolerance, {\it nonf}: ")_
    (text . "\newline ")_
    (bcStrings (20 "1.05E-08" nonf F))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Optimality tolerance, {\it opt}: ")_
    (text . "\newline ")_
    (bcStrings (20 "3.26E-08" opt F))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Step limit, {\it ste}: ")_
    (text . "\newline ")_
    (bcStrings (5 "2.0" ste F))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Start objective check at variable, {\it stao}: ")_
    (text . "\newline ")_
    (bcStrings (5 1 stao PI))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Start constraint check at variable, {\it stac}: ")_
    (text . "\newline ")_
    (bcStrings (5 1 stac PI))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Stop objective check at variable, {\it stoo}: ")_
    (text . "\newline ")_
    (bcStrings (5 9 stoo PI))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Stop objective check at variable, {\it stoc}: ")_
    (text . "\newline ")_
    (bcStrings (5 9 stoc PI))_
    (text . "\blankline ")_
    (text . "\menuitemstyle{}\tab{2} ")_
    (text . "Verify level, {\it ver}: ")_
    (text . "\newline ")_
    (bcStrings (5 3 ver PI)))]
  page := htInitPage('"E04UCF - Unconstrained minimum, pre-conditioned conjugate gradient algorithm, function of several variables using 1st derivatives",nil)
  htSay '"\menuitemstyle{}\tab{2} "
  htSay '"Enter the elements of the array, {\it A(nrowa,n)}: "
  htSay '"\newline "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'e04ucfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'nclin,nclin)
  htpSetProperty(page,'ncnln,ncnln)
  htpSetProperty(page,'nrowa,nrowa)
  htpSetProperty(page,'nrowj,nrowj)
  htpSetProperty(page,'nrowr,nrowr)
  htpSetProperty(page,'liwork,liwork)
  htpSetProperty(page,'lwork,lwork)
  htpSetProperty(page,'optional,optional)
  htpSetProperty(page,'start,start)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e04ucfDefaultSolve(htPage,nclin,ncnln,nrowa,nrowj,nrowr,liwork,lwork,ifail) ==
  n := '4
  optional := '0
  start := '0
  page := htInitPage('"E04UCF - Unconstrained minimum, pre-conditioned conjugate gradient algorithm, function of several variables using 1st derivatives",nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the elements of the array {\it A(nrowa,n)}: ")
    (text . "\newline ")
    (bcStrings (4 "1.0" a11 F))
    (bcStrings (4 "1.0" a12 F))
    (bcStrings (4 "1.0" a13 F))
    (bcStrings (4 "1.0" a14 F))
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the lower boundary conditions {\it bl(n+nclin+ncnln)}: ")
    (text . "\newline ")
    (bcStrings (8 "1.0" bl1 F))
    (bcStrings (8 "1.0" bl2 F))
    (bcStrings (8 "1.0" bl3 F))
    (bcStrings (8 "1.0" bl4 F))
    (bcStrings (8 "-1.E25" bl5 F))
    (bcStrings (8 "-1.E25" bl6 F))
    (bcStrings (8 "25.0" bl7 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the upper boundary conditions {\it bu(n+nclin+ncnln)}: ")
    (text . "\newline ")
    (bcStrings (8 "5.0" bu1 F))
    (bcStrings (8 "5.0" bu2 F))
    (bcStrings (8 "5.0" bu3 F))
    (bcStrings (8 "5.0" bu4 F))
    (bcStrings (8 "20.0" bu5 F))
    (bcStrings (8 "40.0" bu6 F))
    (bcStrings (8 "1.E25" bu7 F))
    -- no istate or clamda or r as default condition is cold
    -- what about cjac when der = 3 ?
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the nonlinear constraint functions, {\it c(ncnln)} ")
    (text . "in terms of X[1]...X[n]: ")
    (text . "\newline ")        
    (bcStrings (55 "X[1]**2 + X[2]**2 + X[3]**2 + X[4]**2" cx1 EM))
    (text . "\newline ")        
    (bcStrings (55 "X[1]*X[2]*X[3]*X[4]" cx2 EM))
    (text . "\newline ")        
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the objective function, {\it F(x)} ")
    (text . "in terms of X[1]...X[n]: ")
    (text . "\newline ")        
    (bcStrings (55 "X[1]*X[4]*(X[1] + X[2] + X[3]) + X[3]" of  EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter initial guess of the solution vector, {\it x(n)}: \newline")
    (bcStrings (8 "1.0" x1 F))
    (bcStrings (8 "5.0" x2 F))
    (bcStrings (8 "5.0" x3 F))
    (bcStrings (8 "1.0" x4 F)))
  htMakeDoneButton('"Continue",'e04ucfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'nclin,nclin)
  htpSetProperty(page,'ncnln,ncnln)
  htpSetProperty(page,'nrowa,nrowa)
  htpSetProperty(page,'nrowj,nrowj)
  htpSetProperty(page,'nrowr,nrowr)
  htpSetProperty(page,'liwork,liwork)
  htpSetProperty(page,'lwork,lwork)
  htpSetProperty(page,'start,start)
  htpSetProperty(page,'optional,optional)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


e04ucfGen htPage ==
  n := htpProperty(htPage,'n)
  nclin := htpProperty(htPage,'nclin)
  ncnln := htpProperty(htPage,'ncnln)
  nrowa := htpProperty(htPage,'nrowa)
  nrowj := htpProperty(htPage,'nrowj)
  nrowr := htpProperty(htPage,'nrowr)
  liwork := htpProperty(htPage,'liwork)
  lwork := htpProperty(htPage,'lwork)
  optional := htpProperty(htPage,'optional)
  start := htpProperty(htPage,'start)
  ifail := htpProperty(htPage,'ifail)
  sta := 'false -- no warm start in HD
  alist := htpInputAreaAlist htPage
  y := alist
  if (optional = '0) then
    cra := '"0.01"
    der := 3
    fea := '"0.1053671201E-7"
    fun := '"0.4373903510E-14"
    hes := 'true
    infb := '"1.00E+15"
    infs := '"1.00E+15"
    linf := '"0.1053671201E-7"
    lint := '"0.9"
    lis := 'true
    maji := 30
    majp := 1
    mini := 81
    minp := 0
    mon := '"-1"
    nonf := '"1.05E-08"
    opt := '"3.26E-08"
    ste := '"2.0"
    stao := 1
    stac := 1
    stoo := n
    stoc := n
    ver := 3
    for i in 1..n repeat
      temp := STRCONC ((first y).1," ")
      xlist := [temp,:xlist]    
      y := rest y
    xstring := bcwords2liststring xlist
    f := (first y).1
    y := rest y
    for i in 1..ncnln repeat
      temp := STRCONC ((first y).1," ")
      cxlist := [temp,:cxlist]  
      y := rest y
    cxstring := bcwords2liststring cxlist
    for i in 1..(n+nclin+ncnln) repeat
      temp := STRCONC ((first y).1," ")
      bulist := [temp,:bulist]  
      y := rest y
    buu := bcwords2liststring bulist
    for i in 1..(n+nclin+ncnln) repeat
      temp := STRCONC ((first y).1," ")
      bllist := [temp,:bllist]  
      y := rest y
    bll := bcwords2liststring bllist
    for i in 1..nrowa repeat -- matrix A
      for j in 1..n repeat
        a := STRCONC((first y).1," ")
        arrlist := [a,:arrlist]
        y := rest y
      amatlist := [:amatlist,arrlist]
      arrlist := []
    amatlist := reverse amatlist 
    amatstr := bcwords2liststring [bcwords2liststring x for x in amatlist]  
  else
    ver := STRCONC((first y).1," ")
    y := rest y
    stoc := STRCONC((first y).1," ")
    y := rest y
    stoo := STRCONC((first y).1," ")
    y := rest y
    stac := STRCONC((first y).1," ")
    y := rest y
    stao := STRCONC((first y).1," ")
    y := rest y
    ste := STRCONC((first y).1," ")
    y := rest y
    opt := STRCONC((first y).1," ")
    y := rest y
    nonf := STRCONC((first y).1," ")
    y := rest y
    mon := STRCONC((first y).1," ")
    y := rest y
    minp := STRCONC((first y).1," ")
    y := rest y
    mini := STRCONC((first y).1," ")
    y := rest y
    majp := STRCONC((first y).1," ")
    y := rest y
    maji := STRCONC((first y).1," ")
    y := rest y
    nolist := (first y).1
    lis := 
      nolist = '" nil" => '"false"
      '"true"
    y := rest y
    dummy1 := first y
    y := rest y
    lint := STRCONC((first y).1," ")
    y := rest y
    linf := STRCONC((first y).1," ")
    y := rest y
    infs := STRCONC((first y).1," ")
    y := rest y
    infb := STRCONC((first y).1," ")
    y := rest y
    noHess := (first y).1
    hes :=
      noHess = '" nil" => '"false"
      '"true"
    y := rest y
    dummy2 := first y
    y := rest y
    fun := STRCONC((first y).1," ")
    y := rest y
    fea := STRCONC((first y).1," ")
    y := rest y
    der := STRCONC((first y).1," ")
    y := rest y
    cra := STRCONC((first y).1," ")
    y := rest y
    for i in 1..n repeat
      temp := STRCONC ((first y).1," ")
      xlist := [temp,:xlist]    
      y := rest y
    xstring := bcwords2liststring xlist
    f := (first y).1
    y := rest y
    for i in 1..ncnln repeat
      temp := STRCONC ((first y).1," ")
      cxlist := [temp,:cxlist]  
      y := rest y
    cxstring := bcwords2liststring cxlist
    for i in 1..(n+nclin+ncnln) repeat
      temp := STRCONC ((first y).1," ")
      bulist := [temp,:bulist]  
      y := rest y
    buu := bcwords2liststring bulist
    for i in 1..(n+nclin+ncnln) repeat
      temp := STRCONC ((first y).1," ")
      bllist := [temp,:bllist]  
      y := rest y
    bll := bcwords2liststring bllist
    for i in 1..nrowa repeat -- matrix A
      for j in 1..n repeat
        a := STRCONC((first y).1," ")
        arrlist := [a,:arrlist]
        y := rest y
      amatlist := [:amatlist,arrlist]
      arrlist := []
    amatlist := reverse amatlist 
    amatstr := bcwords2liststring [bcwords2liststring x for x in amatlist]  
  ntotl := n + nclin + ncnln
  prefix := STRCONC("e04ucf(",STRINGIMAGE n,", ",STRINGIMAGE nclin,", ")
  prefix := STRCONC(prefix,STRINGIMAGE ncnln,", ",STRINGIMAGE nrowa,", ")
  prefix := STRCONC(prefix,STRINGIMAGE nrowj,", ",STRINGIMAGE nrowr,", ")
  prefix:= STRCONC(prefix,amatstr,",[",bll,"],[",buu,"],",STRINGIMAGE liwork)
  prefix := STRCONC(prefix,", ",STRINGIMAGE lwork,", ",STRINGIMAGE sta,", ")
  prefix := STRCONC(prefix,cra,", ",STRINGIMAGE der,", ",fea,", ")
  prefix := STRCONC(prefix,fun,", ",hes,", ",infb,", ",infs,", ",linf,", ")
  prefix := STRCONC(prefix,lint,", ",lis,", ",STRINGIMAGE maji,", ")
  prefix := STRCONC(prefix,STRINGIMAGE majp,", ",STRINGIMAGE mini,", ")
  prefix := STRCONC(prefix,STRINGIMAGE minp,", ",mon,", ",nonf,", ",opt,", ")
  prefix := STRCONC(prefix,ste,", ",STRINGIMAGE stao,", ",STRINGIMAGE stac)
  prefix := STRCONC(prefix,", ",STRINGIMAGE stoo,", ",STRINGIMAGE stoc,", ")
  middle:= STRCONC(STRINGIMAGE ver,",[[0 for i in 1..",STRINGIMAGE ntotl,"]]")
  middle:=STRCONC(middle,"::Matrix Integer,[[0.0 for i in 1..",STRINGIMAGE n)
  middle:=STRCONC(middle,"] for j in 1..",STRINGIMAGE nrowj,"],[[0.0 for i in 1..")
  middle := STRCONC(middle,STRINGIMAGE ntotl,"]],[[0.0 for i in 1..")
  middle := STRCONC(middle,STRINGIMAGE n,"] for j in 1..",STRINGIMAGE nrowr)
  middle := STRCONC(middle,"],[",xstring,"],",STRINGIMAGE ifail)
  end:=STRCONC(",((",cxstring,")::Vector Expression(Float))::ASP55(CONFUN),")
  end := STRCONC(end,"((",f,")::Expression(Float))::ASP49(OBJFUN))")
  linkGen STRCONC(prefix,middle,end)


e04ucfCopOut() ==
  htInitPage('"E04UCF - Unconstrained minimum, pre-conditioned conjugate gradient algorithm, function of several variables using 1st derivatives",nil)  
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\blankline ")
    (text . "{\center{\em Hyperdoc interface not available for warm start}}")
    (text . "\newline ")
    (text . "{\center{\em Please use the command line.}}"))
  htMakeDoneButton('"Continue",'e04ucf)
  htShowPage()

e04ycf() ==
  htInitPage('"E04YCF - Covariance matrix for non-linear least-squares problem", nil)
  htMakePage '(
    (domainConditions 
        (isDomain EM $EmptyMode)
        (isDomain PI (PositiveInteger))
        (isDomain F (Float)))
    (text . "\windowlink{Manual Page}{manpageXXe04ycf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|e04ycf| '|NagOptimisationPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "E04YCF returns estimates of elements of the variance-covariance ")
    (text . "matrix of the estimated regression coefficients for a nonlinear ")
    (text . "least-squares problem. ")
    (text . "\blankline ")
    (text . "This routine may be used following any of the nonlinear ")
    (text . "least-squares routines E04FDF, E04GCF. It ")
    (text . "requires the parameters {\it fumsq, s} and {\it v} supplied ")
    (text . "by those routines. ")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Elements of {\it c} returned, {\it job}: ")
    (radioButtons job
        (" 0" " The diagonal elements of {\it c} " jZero)
        (" 1" " Elements of column {\it job} of {\it c} " jOne)
        (" -1" " The whole {\it n} by {\it n} symmetric matrix " jMinus))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of observations, {\it m}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 15 m PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of variables, {\it n}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (6 3 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Sum of the squares of the residuals, {\it fsumsq}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (30 "0.0082148773065789729" fsumsq F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "First dimension of array {\it v}, {\it lv}:")
    (text . "\newline\tab{2} ")
    (bcStrings (6 3 lv PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'e04ycfSolve)
  htShowPage()

e04ycfSolve htPage ==
  temp := htpButtonValue(htPage,'job)
  job :=
    temp = 'jMinus => '-1
    temp = 'jOne => '1
    '0
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  fsumsq := htpLabelInputString(htPage, 'fsumsq)
  lv :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'lv)
    objValUnwrap htpLabelSpadValue(htPage, 'lv)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  (n = 3 and lv = 3) => e04ycfDefaultSolve(htPage,job,m,fsumsq,ifail)
  sList := 
    "append"/[fa(i) for i in 1..(n)] where fa(i) ==
      snam := INTERN STRCONC ('"s",STRINGIMAGE i)
      [['bcStrings,[30, '"0.0", snam, 'F]]]
  middle := ('"\blankline \menuitemstyle{}\tab{2} Enter the elements ")
  middle := STRCONC(middle,'"of array {\it v(lv,n)}: \newline ")
  vList := 
    "append"/[fb(i,n) for i in 1..lv] where fb(i,n) ==
       labelList := 
         "append"/[fc(i,j) for j in 1..n] where fc(i,j) ==
            vnam := INTERN STRCONC ('"v",STRINGIMAGE i,STRINGIMAGE j)
            [['bcStrings,[15, 0, vnam, 'F]]]
       prefix := ('"\newline ")
       labelList := [['text,:prefix],:labelList]
  vList := [['text,:middle],:vList]
  equationPart := [
     '(domainConditions 
        (isDomain EM $EmptyMode)
          (isDomain F (Float))
            (isDomain I (Integer))),
                :sList,:vList]
  page:= htInitPage('"E04YCF - Covariance matrix for non-linear least-squares problem", nil)
  htSay '"\menuitemstyle{}\tab{2} "
  htSay '"Enter the elements of the array {\it s(n)}: \newline "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'e04ycfGen)
  htpSetProperty(page,'job,job)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'fsumsq,fsumsq)
  htpSetProperty(page,'lv,lv)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e04ycfDefaultSolve(htPage,job,m,fsumsq,ifail) ==
  n := '3
  lv := '3
  page:= htInitPage('"E04YCF - Covariance matrix for non-linear least-squares problem", nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the elements of array {\it s(n)}: \newline ")
    (bcStrings (30 "4.0965034571419325" s1 F))
    (bcStrings (30 "1.5949579400198182" s2 F))
    (bcStrings (30 "0.061258491120317927" s3 F))
    (text . "\newline ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the elements of array {\it v(lv,n)}: \newline ")
    -- not the correct values yet !
    (bcStrings (8 "0.9354" v11 F))
    (bcStrings (8 "-0.2592" v12 F))
    (bcStrings (8 "-0.2405" v13 F))
    (text . "\newline ")
    (bcStrings (8 "0.3530" v21 F))
    (bcStrings (8 "0.6432" v22 F))
    (bcStrings (8 "0.6795" v23 F))
    (text . "\newline ")
    (bcStrings (8 "-0.0215" v31 F))
    (bcStrings (8 "-0.7205" v32 F))
    (bcStrings (8 "0.6932" v33 F)))
  htMakeDoneButton('"Continue",'e04ycfGen)
  htpSetProperty(page,'job,job)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'fsumsq,fsumsq)
  htpSetProperty(page,'lv,lv)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

e04ycfGen htPage ==
  job := htpProperty(htPage,'job)
  n := htpProperty(htPage, 'n)
  m := htpProperty(htPage, 'm)
  fsumsq := htpProperty(htPage, 'fsumsq)
  lv := htpProperty(htPage, 'lv)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..(lv*n) repeat
    temp := STRCONC ((first y).1," ")
    vlist := [temp,:vlist]      
    y := rest y
  vstring := bcwords2liststring vlist    
  for i in 1..n repeat
    temp := STRCONC ((first y).1," ")
    slist := [temp,:slist]      
    y := rest y
  sstring := bcwords2liststring slist    
  prefix := STRCONC("e04ycf(",STRINGIMAGE job,",", STRINGIMAGE m,", ")
  prefix := STRCONC(prefix,STRINGIMAGE n,",",fsumsq,", [")
  prefix := STRCONC(prefix,sstring,"],", STRINGIMAGE lv,",[",vstring)
  linkGen STRCONC(prefix,"],",STRINGIMAGE ifail,")")




