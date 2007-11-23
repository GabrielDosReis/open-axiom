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

d02bbf() ==
  htInitPage('"D02BBF - ODEs, IVP, Runge-Kutta-Merson method, over a range, intermediate output",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXd02bbf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|d02bbf| '|NagOrdinaryDifferentialEquationsPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "D02BBF integrates a system of {\it n} ordinary differential ")
    (text . "equations, {\htbitmap{yi}}' = {\htbitmap{fi}}(x,y), for ")
    (text . "{\it i} = 1,2,...,{\it n}, over a range with given initial ")
    (text . "conditions using a Runge-Kutta-Merson method; the solution ")
    (text . "may be output at specified points.")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Initial value of {\it x}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 "0.0" x F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "End of integration range {\it xend}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 "8.0" xend F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of differential equations {\it n}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 3 n PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "\newline Tolerance required {\it tol}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (10 "0.0001" tol F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Error control indicator {\it irelab}:")
    (radioButtons irelab
        ("" " 0, mixed" mix)
        ("" " 1, absolute" abs)
        ("" " 2, relative" rel))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'd02bbfSolve)
  htShowPage()

d02bbfSolve htPage ==
  x := htpLabelInputString(htPage,'x)
  xend := htpLabelInputString(htPage,'xend)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  tol := htpLabelInputString(htPage,'tol)
  control := htpButtonValue(htPage,'irelab)
  irelab :=
    control = 'mix => '0
    control = 'abs => '1
    '2
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'minusOne => '-1
    '1
  n = '3 => d02bbfDefaultSolve(htPage,x,xend,tol,irelab,ifail)
  funcList := 
    "append"/[fa(i) for i in 1..n] where fa(i) ==
      prefix := ('"\newline {\em Function ")
      prefix := STRCONC(prefix,STRINGIMAGE i,'":} \space{1}")
      funct := STRCONC ('"Y[",STRINGIMAGE i ,"]")
      nam := INTERN STRCONC ('"n",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[42, funct, nam, 'EM]]]
  middle := ('"\blankline \menuitemstyle{} \tab{2} Enter the initial ")
  middle := STRCONC(middle,'"values of \htbitmap{yi}: \newline \tab{2}")
  yList := 
    "append"/[fb(i) for i in 1..n] where fb(i) ==
        ynam := INTERN STRCONC ('"u",STRINGIMAGE i)
        [['bcStrings,[6, 0, ynam, 'F]]]
  yList :=  [['text,:middle],:yList]
  mid:= ('"\blankline \menuitemstyle{} \tab{2} Intermediate values of {\it x}")
  mid := STRCONC(mid,'" at which \htbitmap{yi} is required: \newline \tab{2}")
  vList := [['bcStrings,[30, "0", 'out, 'EM]]]
  vList :=  [['text,:mid],:vList]
  equationPart := [
     '(domainConditions 
        (isDomain EM $EmptyMode)
          (isDomain S (String))
            (isDomain F (Float))
              (isDomain I (Integer))),
                :funcList,:yList,:vList]
  page :=   htInitPage('"D02BBF - ODEs, IVP, Runge-Kutta-Merson method, over a range, intermediate output",nil)
  htSay '"\menuitemstyle{}\tab{2} "
  htSay '"Enter the functions (i.e. the derivatives) below "
  htSay '"as functions of Y[1]...Y[n]: "
  htSay '"\newline "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'d02bbfGen)
  htpSetProperty(page,'x,x)
  htpSetProperty(page,'xend,xend)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'irelab,irelab)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

d02bbfDefaultSolve(htPage,x,xend,tol,irelab,ifail)   ==
  n := '3
  page :=   htInitPage('"D02BBF - ODEs, IVP, Runge-Kutta-Merson method, over a range, intermediate output",nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the functions (i.e. the derivatives) below ")
    (text . "as functions of Y[1]...Y[n]: ")
    (text . "\newline ")
    (text . "\newline {\em Function 1:} \space{1}")
    (bcStrings (44 "tan(Y[3])" f1 EM))
    (text . "\newline {\em Function 2:} \space{1}")
    (bcStrings (44 "-0.032*tan(Y[3])/Y[2] -0.02*Y[2]/cos(Y[3])" f2 EM))
    (text . "\newline {\em Function 3:} \space{1}")
    (bcStrings (44 "-0.032/(Y[2]**2)" f3 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the initial values of \htbitmap{yi}:")
    (text . "\newline \tab{2}")
    (bcStrings (8 "0.0" y1 EM))
    (bcStrings (8 "0.5" y2 EM))
    (bcStrings (8 "\%pi*0.2" y3 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Intermediate values of {\it x} at which \htbitmap{yi} is required:")
    (text . "\newline \tab{2}")
    (bcStrings (30 "1,2,3,4,5,6,7,8" out EM)))
  htpSetProperty(page,'x,x)
  htpSetProperty(page,'xend,xend)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'irelab,irelab)
  htpSetProperty(page,'ifail,ifail)
  htMakeDoneButton('"Continue",'d02bbfGen)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

d02bbfGen htPage ==
  x := htpProperty(htPage, 'x)
  xend := htpProperty(htPage, 'xend)
  n := htpProperty(htPage, 'n)
  tol := htpProperty(htPage, 'tol)
  irelab := htpProperty(htPage, 'irelab)
  ifail := htpProperty(htPage, 'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  outp := ((first y).1)
  oList := [outp,:oList]
  y := rest y
  ostring := bcwords2liststring oList
  -- This is distictly horrible!  OUTP is a comma-seperated string so we
  -- count up the commas to see how many elements it has.  We return this
  -- quantity plus 1 since the ASP OUTPUT is always called at least once.
  numberOfPoints := 
    ZEROP LENGTH(outp) => 1
    2+COUNT(CHARACTER(44),outp)
  for i in 1..n repeat
    ytemp := STRCONC((first y).1," ")
    yList := [ytemp,:yList]
    y := rest y
  ystring := bcwords2liststring yList
  while y repeat
    f := STRCONC((first y).1," ")
    fList := [f,:fList]
    y := rest y
  fstring := bcwords2liststring fList
  prefix := STRCONC("d02bbf(", xend,", ", STRINGIMAGE numberOfPoints, ", ",STRINGIMAGE n,", ",STRINGIMAGE irelab)
  prefix := STRCONC(prefix,", ",x,", [", ystring,"],",tol)
  prefix := STRCONC(prefix,", ",STRINGIMAGE ifail,",(")
  end := STRCONC(fstring,"::Vector Expression Float)::ASP7('FCN),(",ostring)
  end := STRCONC(end,"::Vector MachineFloat)::ASP8('OUTPUT))")
  linkGen STRCONC(prefix,end)

d02bhf() ==
  htInitPage('"D02BHF - ODEs, IVP, Runge-Kutta-Merson method, until function of solution is zero",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXd02bhf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|d02bhf| '|NagOrdinaryDifferentialEquationsPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "D02BHF integrates a system of {\it n} ordinary differential ")
    (text . "equations, {\htbitmap{yi}}' = {\htbitmap{fi}}(x,y), for ")
    (text . "{\it i} = 1,2,...,{\it n}, over a range with given initial ")
    (text . "conditions using a Runge-Kutta-Merson method until a specified ")
    (text . "function {\em g(x,y)} of the solution is zero. ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Initial value of {\it x}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 "0.0" x F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "End of integration range {\it xend}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 "10.0" xend F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of differential equations {\it n}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 3 n PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "\newline Tolerance required {\it tol}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (10 "0.0001" tol F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Error control indicator {\it irelab}:")
    (radioButtons irelab
        ("" " 0, mixed" mix)
        ("" " 1, absolute" abs)
        ("" " 2, relative" rel))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "\newline Upper bound on size of the interval {\it hmax}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (10 "0.0" hmax F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'd02bhfSolve)
  htShowPage()

d02bhfSolve htPage ==
  x := htpLabelInputString(htPage,'x)
  xend := htpLabelInputString(htPage,'xend)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  tol := htpLabelInputString(htPage,'tol)
  control := htpButtonValue(htPage,'irelab)
  irelab :=
    control = 'mix => '0
    control = 'abs => '1
    '2
  hmax := htpLabelInputString(htPage,'hmax)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '3 => d02bhfDefaultSolve(htPage,x,xend,tol,irelab,hmax,ifail)
  funcList := 
    "append"/[fa(i) for i in 1..n] where fa(i) ==
      prefix := ('"\newline {\em Function ")
      prefix := STRCONC(prefix,STRINGIMAGE i,'":} \space{1}")
      funct := STRCONC ('"Y[",STRINGIMAGE i ,"]")
      nam := INTERN STRCONC ('"n",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[42, funct, nam, 'EM]]]
  middle := ('"\blankline \menuitemstyle{} \tab{2} Enter the initial ")
  middle := STRCONC(middle,'"values of \htbitmap{yi}: \newline ")
  yList := 
    "append"/[fb(i) for i in 1..n] where fb(i) ==
        ynam := INTERN STRCONC ('"u",STRINGIMAGE i)
        [['bcStrings,[6, 0, ynam, 'F]]]
  yList :=  [['text,:middle],:yList]
  mid:= ('"\blankline \menuitemstyle{} \tab{2} Enter the function below ")
  mid := STRCONC(mid,'"{\em g(x,y)}: \newline ")
  vList := [['bcStrings,[30, '"Y[1]", 'g, 'EM]]]
  vList :=  [['text,:mid],:vList]
  equationPart := [
     '(domainConditions 
        (isDomain EM $EmptyMode)
          (isDomain S (String))
            (isDomain F (Float))
              (isDomain I (Integer))),
                :funcList,:yList,:vList]
  page :=   htInitPage('"D02BHF - ODEs, IVP, Runge-Kutta-Merson method, until function of solution is zero",nil)
  htSay '"\menuitemstyle{}\tab{2} "
  htSay '"Enter the functions (i.e. the derivatives) below \htbitmap{fi} "
  htSay '"as functions of Y[1]...Y[n]: "
  htSay '"\newline "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'d02bhfGen)
  htpSetProperty(page,'x,x)
  htpSetProperty(page,'xend,xend)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'irelab,irelab)
  htpSetProperty(page,'hmax,hmax)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

d02bhfDefaultSolve(htPage,x,xend,tol,irelab,hmax,ifail)   ==
  n := '3
  page :=  htInitPage('"D02BHF - ODEs, IVP, Runge-Kutta-Merson method, until function of solution is zero",nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the functions (i.e. the derivatives) below \htbitmap{fi} ")
    (text . "as functions of Y[1]...Y[n]: ")
    (text . "\newline {\em Function 1:} \space{1}")
    (bcStrings (44 "tan(Y[3])" f1 EM))
    (text . "\newline {\em Function 2:} \space{1}")
    (bcStrings (44 "-0.032*tan(Y[3])/Y[2] -0.02*Y[2]/cos(Y[3])" f2 EM))
    (text . "\newline {\em Function 3:} \space{1}")
    (bcStrings (44 "-0.032/(Y[2]**2)" f3 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the initial values of \htbitmap{yi}:")
    (text . "\newline ")
    (bcStrings (8 "0.5" y1 EM))
    (bcStrings (8 "0.5" y2 EM))
    (bcStrings (8 "\%pi*0.2" y3 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the function below {\em g(x,y)}: ")
    (text . "\newline ")
    (bcStrings (30 "Y[1]" g EM)))
  htpSetProperty(page,'x,x)
  htpSetProperty(page,'xend,xend)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'irelab,irelab)
  htpSetProperty(page,'hmax,hmax)
  htpSetProperty(page,'ifail,ifail)
  htMakeDoneButton('"Continue",'d02bhfGen)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

d02bhfGen htPage ==
  x := htpProperty(htPage, 'x)
  xend := htpProperty(htPage, 'xend)
  n := htpProperty(htPage, 'n)
  tol := htpProperty(htPage, 'tol)
  irelab := htpProperty(htPage, 'irelab)
  hmax := htpProperty(htPage, 'hmax)
  ifail := htpProperty(htPage, 'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  g := ((first y).1)
  y := rest y
  for i in 1..n repeat
    ytemp := STRCONC((first y).1," ")
    yList := [ytemp,:yList]
    y := rest y
  ystring := bcwords2liststring yList
  while y repeat
    f := STRCONC((first y).1," ")
    fList := [f,:fList]
    y := rest y
  fstring := bcwords2liststring fList
  prefix := STRCONC("d02bhf(", xend,", ",STRINGIMAGE n,", ",STRINGIMAGE irelab)
  mid := STRCONC(", ",hmax,", ",x,", [", ystring,"],")
  mid := STRCONC(mid,tol,", ",STRINGIMAGE ifail,",(",g)
  mid := STRCONC(mid,"::Expression Float)::ASP9('G),(")
  end := STRCONC(fstring,"::Vector Expression Float)::ASP7('FCN))")
  linkGen STRCONC(prefix,mid,end)

  
d02cjf() ==
  htInitPage('"D02CJF - ODEs, IVP, Adams method, until function of solution is zero, intermediate output",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXd02cjf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|d02cjf| '|NagOrdinaryDifferentialEquationsPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "D02CJF integrates a system of {\it n} ordinary differential ")
    (text . "equations, {\htbitmap{yi}}' = {\htbitmap{fi}}(x,y), for ")
    (text . "{\it i} = 1,2,...,{\it n}, over a range with given initial ")
    (text . "conditions using an Adams method until a specified ")
    (text . "function {\em g(x,y)} of the solution is zero; the solution may ")
    (text . "be output at specified points. \blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Initial value of {\it x}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 "0.0" x F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "End of integration range {\it xend}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 "10.0" xend F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of differential equations {\it n}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 3 n PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "\newline Tolerance required {\it tol}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (10 "0.0001" tol F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Type of error test used {\it relabs}:")
    (radioButtons relabs
        ("" " D, default (mixed)" mix)
        ("" " A, absolute" abs)
        ("" " R, relative" rel))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'd02cjfSolve)
  htShowPage()

d02cjfSolve htPage ==
  x := htpLabelInputString(htPage,'x)
  xend := htpLabelInputString(htPage,'xend)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  tol := htpLabelInputString(htPage,'tol)
  control := htpButtonValue(htPage,'relabs)
  relabs :=
    control = 'mix => '"D"
    control = 'abs => '"A"
    '"R"
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '3 => d02cjfDefaultSolve(htPage,x,xend,tol,relabs,ifail)
  funcList := 
    "append"/[fa(i) for i in 1..n] where fa(i) ==
      prefix := ('"\newline {\em Function ")
      prefix := STRCONC(prefix,STRINGIMAGE i,'":} \space{1}")
      funct := STRCONC ('"Y[",STRINGIMAGE i ,"]")
      nam := INTERN STRCONC ('"n",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[42, funct, nam, 'EM]]]
  middle := ('"\blankline \menuitemstyle{} \tab{2} Enter the initial ")
  middle := STRCONC(middle,'"values of \htbitmap{yi}: \newline ")
  yList := 
    "append"/[fb(i) for i in 1..n] where fb(i) ==
        ynam := INTERN STRCONC ('"u",STRINGIMAGE i)
        [['bcStrings,[6, 0, ynam, 'F]]]
  yList :=  [['text,:middle],:yList]
  mid:= ('"\blankline \menuitemstyle{} \tab{2} Intermediate values of {\it x}")
  mid := STRCONC(mid,'" at which \htbitmap{yi} is required: \newline ")
  vList := [['bcStrings,[30, "2,4", 'out, 'EM]]]
  vList :=  [['text,:mid],:vList]
  midd := ('"\blankline \menuitemstyle{} \tab{2} Enter the function below ")
  midd := STRCONC(midd,'"{\em g(x,y)}: \newline ")
  uList := [['bcStrings,[30, '"Y[1]", 'g, 'EM]]]
  uList :=  [['text,:midd],:uList]
  equationPart := [
     '(domainConditions 
        (isDomain EM $EmptyMode)
          (isDomain S (String))
            (isDomain F (Float))
              (isDomain I (Integer))),
                :funcList,:yList,:vList,:uList]
  page :=  htInitPage('"D02CJF - ODEs, IVP, Adams method, until function of solution is zero, intermediate output",nil)
  htSay '"\menuitemstyle{}\tab{2} "
  htSay '"Enter the functions (i.e. the derivatives) below \htbitmap{fi} "
  htSay '"as functions of Y[1]...Y[n]: "
  htSay '"\newline "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'d02cjfGen)
  htpSetProperty(page,'x,x)
  htpSetProperty(page,'xend,xend)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'relabs,relabs)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

d02cjfDefaultSolve(htPage,x,xend,tol,relabs,ifail)   ==
  n := '3
  page := htInitPage('"D02CJF - ODEs, IVP, Adams method, until function of solution is zero, intermediate output",nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the functions (i.e. the derivatives) below \htbitmap{fi} ")
    (text . "as functions of Y[1]...Y[n]: ")
    (text . "\newline {\em Function 1:} \space{1}")
    (bcStrings (44 "tan(Y[3])" f1 EM))
    (text . "\newline {\em Function 2:} \space{1}")
    (bcStrings (44 "-0.032*tan(Y[3])/Y[2] -0.02*Y[2]/cos(Y[3])" f2 EM))
    (text . "\newline {\em Function 3:} \space{1}")
    (bcStrings (44 "-0.032/(Y[2]**2)" f3 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the initial values of \htbitmap{yi}:")
    (text . "\newline ")
    (bcStrings (8 "0.5" y1 EM))
    (bcStrings (8 "0.5" y2 EM))
    (bcStrings (8 "\%pi*0.2" y3 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} Intermediate")
    (text . " values of {\it x} at which \htbitmap{yi} is required:")
    (text . "\newline ")
    (bcStrings (30 "2,4,6,8" out EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the function below {\em g(x,y)}: ")
    (text . "\newline ")
    (bcStrings (30 "Y[1]" g EM)))
  htpSetProperty(page,'x,x)
  htpSetProperty(page,'xend,xend)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'relabs,relabs)
  htpSetProperty(page,'ifail,ifail)
  htMakeDoneButton('"Continue",'d02cjfGen)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

d02cjfGen htPage ==
  x := htpProperty(htPage, 'x)
  xend := htpProperty(htPage, 'xend)
  n := htpProperty(htPage, 'n)
  tol := htpProperty(htPage, 'tol)
  relabs := htpProperty(htPage, 'relabs)
  ifail := htpProperty(htPage, 'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  g := ((first y).1)
  y := rest y
  outp := ((first y).1)
  oList := [outp,:oList]
  ostring := bcwords2liststring oList
  -- This is distictly horrible!  OUTP is a comma-seperated string so we
  -- count up the commas to see how many elements it has.  We return this
  -- quantity plus 1 since the ASP OUTPUT is always called at least once.
  numberOfPoints := 
    ZEROP LENGTH(outp) => 1
    2+COUNT(CHARACTER(44),outp)
  y := rest y
  for i in 1..n repeat
    ytemp := STRCONC((first y).1," ")
    yList := [ytemp,:yList]
    y := rest y
  ystring := bcwords2liststring yList
  while y repeat
    f := STRCONC((first y).1," ")
    fList := [f,:fList]
    y := rest y
  fstring := bcwords2liststring fList
  prefix := STRCONC("d02cjf(",xend,", ",STRINGIMAGE numberOfPoints ,", ", STRINGIMAGE n,", ",tol,",_"",relabs)
  mid := STRCONC("_", ",x ,", [", ystring,"],",STRINGIMAGE ifail)
  mid := STRCONC(mid,",(",g,"::Expression Float)::ASP9('G),(",fstring)
  end := STRCONC("::Vector Expression Float)::ASP7('FCN),(",ostring)
  end := STRCONC(end,"::Vector MachineFloat)::ASP8('OUTPUT))")
  linkGen STRCONC(prefix,mid,end)
  


d02ejf() ==
  htInitPage('"D02EJF - ODEs, stiff IVP, BDF method, until function of solution is zero, intermediate output",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXd02ejf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|d02ejf| '|NagOrdinaryDifferentialEquationsPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "D02EJF integrates a system of {\em n} ordinary differential ")
    (text . "equations, {\htbitmap{yi}}' = {\htbitmap{fi}}(x,y), for {\it i} ")
    (text . "= 1,,2,...,{\it n}, over a range with given initial conditions")
    (text . " using backward differentiation formulae until a specified ")
    (text . "function {\em g(x,y)} of the solution is zero; the solution may ")
    (text . "be output at specified points. \blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Initial value of {\it x}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 "0.0" x F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "End of integration range {\it xend}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 "10.0" xend F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of differential equations {\it n}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 3 n PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "\newline Tolerance required {\it tol}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (10 "0.0001" tol F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Type of error test used {\it relabs}:")
    (radioButtons relabs
        ("" " D, default (mixed)" mix)
        ("" " A, absolute" abs)
        ("" " R, relative" rel))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'd02ejfSolve)
  htShowPage()

d02ejfSolve htPage ==
  x := htpLabelInputString(htPage,'x)
  xend := htpLabelInputString(htPage,'xend)
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  tol := htpLabelInputString(htPage,'tol)
  control := htpButtonValue(htPage,'relabs)
  relabs :=
    control = 'mix => '"D"
    control = 'abs => '"A"
    '"R"
  iw := (n + 12) * n + 50
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'one => '1
    '-1
  n = '3 => d02ejfDefaultSolve(htPage,x,xend,tol,relabs,iw,ifail)
  funcList := 
    "append"/[fa(i) for i in 1..n] where fa(i) ==
      prefix := ('"\newline {\em Function ")
      prefix := STRCONC(prefix,STRINGIMAGE i,'":} \space{1}")
      funct := STRCONC ('"Y[",STRINGIMAGE i ,"]")
      nam := INTERN STRCONC ('"n",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[42, funct, nam, 'EM]]]
  middle := ('"\blankline \menuitemstyle{} \tab{2} Enter the initial ")
  middle := STRCONC(middle,'"values of \htbitmap{yi}: \newline ")
  yList := 
    "append"/[fb(i) for i in 1..n] where fb(i) ==
        ynam := INTERN STRCONC ('"u",STRINGIMAGE i)
        [['bcStrings,[6, 0, ynam, 'F]]]
  yList :=  [['text,:middle],:yList]
  mid:= ('"\blankline \menuitemstyle{} \tab{2} Intermediate values of {\it x}")
  mid := STRCONC(mid,'" at which \htbitmap{yi} is required: \newline ")
  vList := [['bcStrings,[30, "2,4,6,8", 'out, 'EM]]]
  vList :=  [['text,:mid],:vList]
  midd := ('"\blankline \menuitemstyle{} \tab{2} Enter the function below ")
  midd := STRCONC(midd,'"{\em g(x,y)}: \newline ")
  uList := [['bcStrings,[30, '"Y[1]", 'g, 'EM]]]
  uList :=  [['text,:midd],:uList]
  equationPart := [
     '(domainConditions 
        (isDomain EM $EmptyMode)
          (isDomain S (String))
            (isDomain F (Float))
              (isDomain I (Integer))),
                :funcList,:yList,:vList,:uList]
  page := htInitPage('"D02EJF - ODEs, stiff IVP, BDF method, until function of solution is zero, intermediate output",nil)
  htSay '"\menuitemstyle{}\tab{2} "
  htSay '"Enter the functions (i.e. the derivatives) below \htbitmap{fi} "
  htSay '"as functions of Y[1]...Y[n]: "
  htSay '"\newline "
  htMakePage equationPart
  htSay '"\blankline {\em Note:} PEDERV is automatically generated using the vector "
  htSay '"of derivatives given above. "
  htMakeDoneButton('"Continue",'d02ejfGen)
  htpSetProperty(page,'x,x)
  htpSetProperty(page,'xend,xend)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'relabs,relabs)
  htpSetProperty(page,'iw,iw)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

d02ejfDefaultSolve(htPage,x,xend,tol,relabs,iw,ifail)   ==
  n := '3
  page := htInitPage('"D02EJF - ODEs, stiff IVP, BDF method, until function of solution is zero, intermediate output",nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the functions (i.e. the derivatives) below \htbitmap{fi} ")
    (text . "as functions of Y[1]...Y[n]: ")
    (text . "\newline {\em Function 1:} \space{1}")
    (bcStrings (44 "-0.04*Y[1]+1.0E4*Y[2]*Y[3]" f1 EM))
    (text . "\newline {\em Function 2:} \space{1}")
    (bcStrings (44 "0.04*Y[1]-1.0E4*Y[2]*Y[3]-3.0E7*Y[2]*Y[2]" f2 EM))
    (text . "\newline {\em Function 3:} \space{1}")
    (bcStrings (44 "3.0E7*Y[2]*Y[2]" f3 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the initial values of \htbitmap{yi}:")
    (text . "\newline ")
    (bcStrings (8 "1.0" y1 EM))
    (bcStrings (8 "0.0" y2 EM))
    (bcStrings (8 "0.0" y3 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} Intermediate")
    (text . " values of {\it x} at which \htbitmap{yi} is required:")
    (text . "\newline ")
    (bcStrings (30 "2,4,6,8" out EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the function below {\em g(x,y)}: ")
    (text . "\newline ")
    (bcStrings (30 "Y[1]-0.9" g EM))
    (text . "\blankline ")
    (text . "{\em Note:} PEDERV is automatically generated using the vector ")
    (text . "of derivatives given above. "))
  htpSetProperty(page,'x,x)
  htpSetProperty(page,'xend,xend)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'relabs,relabs)
  htpSetProperty(page,'iw,iw)
  htpSetProperty(page,'ifail,ifail)
  htMakeDoneButton('"Continue",'d02ejfGen)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

d02ejfGen htPage ==
  x := htpProperty(htPage, 'x)
  xend := htpProperty(htPage, 'xend)
  n := htpProperty(htPage, 'n)
  tol := htpProperty(htPage, 'tol)
  relabs := htpProperty(htPage, 'relabs)
  iw := htpProperty(htPage, 'iw)
  ifail := htpProperty(htPage, 'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  g := ((first y).1)
  y := rest y
  outp := ((first y).1)
  oList := [outp,:oList]
  ostring := bcwords2liststring oList
  -- This is distictly horrible!  OUTP is a comma-seperated string so we
  -- count up the commas to see how many elements it has.  We return this
  -- quantity plus 1 since the ASP OUTPUT is always called at least once.
  numberOfPoints := 
    ZEROP LENGTH(outp) => 1
    2+COUNT(CHARACTER(44),outp)
  y := rest y
  for i in 1..n repeat
    ytemp := STRCONC((first y).1," ")
    yList := [ytemp,:yList]
    y := rest y
  ystring := bcwords2liststring yList
  while y repeat
    f := STRCONC((first y).1," ")
    fList := [f,:fList]
    y := rest y
  fstring := bcwords2liststring fList
  prefix := STRCONC("d02ejf(",xend,", ",STRINGIMAGE numberOfPoints,", ", STRINGIMAGE n,",_"",relabs,"_", ")
  mid:=STRCONC(STRINGIMAGE iw,", ",x ,", [", ystring,"], ",tol,", ")
  mid := STRCONC(mid,STRINGIMAGE ifail,",(",g,"::Expression Float)::ASP9(G),(")
  end := STRCONC(fstring,"::Vector Expression Float)::ASP7('FCN),(",fstring)
  end := STRCONC(end,"::Vector Expression Float)::ASP31('PEDERV),(",ostring)
  end := STRCONC(end,"::Vector MachineFloat)::ASP8('OUTPUT))")
  linkGen STRCONC(prefix,mid,end)

d02gaf() ==
  htInitPage('"D02GAF - ODEs, boundary value problem, finite difference technique with deferred correction, simple nonlinear problem", nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXd02gaf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|d02gaf| '|NagOrdinaryDifferentialEquationsPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "D02GAF solves a two-point boundary value problem for a system ")
    (text . "of n ODEs \center{\htbitmap{d02gaf},} for i = 1,2,...,n, on ")
    (text . "the range [a,b] with assigned boundary conditions using a ")
    (text . "deferred correction technique and a Newton iteration; ")
    (text . "the solution is computed on a mesh. ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the number of equations in the system {\it n}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 3 n PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "\newline Left hand boundary point {\it a}: ")
    (text . "\tab{32} \menuitemstyle{}\tab{34} ")
    (text . "Right hand boundary {\it b}:")
    (text . "\newline\tab{2} ")
    (bcStrings (10 "0.0" a F))
    (text . "\tab{34} ")
    (bcStrings (10 "10.0" b F))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "\newline Max number of mesh points {\it mnp}:")
    (text . "\tab{32} \menuitemstyle{}\tab{34} ")
    (text . "Number of points {\it np} ({\it np} = 0 or {\it np} ")
    (text . "\htbitmap{great=} 4): ")
    (text . "\newline\tab{2} ")
    (bcStrings (10 64 mnp PI))
    (text . "\tab{34} ")
    (bcStrings (10 26 np PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Accuracy required {\it tol}:")
    (text . "\newline\tab{2} ")
    (bcStrings (10 "1.0e-3" tol F))
    (text . "\blankline ")
    (text . "\newline \tab{2} ")
    (text . "Ifail is input in three components: ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "{\it a} ")
    (radioButtons afail
        ("" "  0, hard failure" azero)
        ("" "  1, soft failure" aone))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "{\it b} ")
    (radioButtons bfail
        ("" "  1, print error messages" bone)
        ("" "  0, suppress error messages" bzero))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "{\it c} ")
    (radioButtons cfail
        ("" "  1, print warning messages" cone)
        ("" "  0, suppress warning messages" czero)))
  htMakeDoneButton('"Continue", 'd02gafSolve)
  htShowPage()

d02gafSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  a := htpLabelInputString(htPage,'a)
  b := htpLabelInputString(htPage,'b)
  mnp :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'mnp)
    objValUnwrap htpLabelSpadValue(htPage, 'mnp)
  np :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'np)
    objValUnwrap htpLabelSpadValue(htPage, 'np)
  lw := mnp * (3*n*n + 6*n + 2) + 4*n*n + 4*n
  liw := mnp * (2*n + 1) + n*n + 4*n + 2
  tol := htpLabelInputString(htPage,'tol)
  aerror := htpButtonValue(htPage,'afail)
  afail :=
    aerror = 'azero => '0
    '1
  berror := htpButtonValue(htPage,'bfail)
  bfail :=
    berror = 'bone => '1
    '0
  cerror := htpButtonValue(htPage,'cfail)
  cfail :=
    cerror = 'cone => '1
    '0
  ifail := 100*cfail + 10*bfail + afail
  n = '3 => d02gafDefaultSolve(htPage,a,b,mnp,np,lw,liw,tol,ifail)
  funcList := 
    "append"/[fa(i) for i in 1..n] where fa(i) ==
      prefix := ('"\newline {\em Function ")
      prefix := STRCONC(prefix,STRINGIMAGE i,'":} \space{1}")
      funct := STRCONC ('"Y[",STRINGIMAGE i ,"]")
      nam := INTERN STRCONC ('"n",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[42, funct, nam, 'EM]]]
  middle := ('"\blankline \menuitemstyle{} \tab{2} Enter known or estimated ")
  middle := STRCONC(middle,'"values of \htbitmap{yi} at a and b, ")
  middle := STRCONC(middle,"{\it U(n,2)}. [\htbitmap{yi}(a) in the first ")
  middle := STRCONC(middle,"column, \htbitmap{yi}(b) in the second.] ")
  middle := STRCONC(middle,"\newline ")
  uList := 
    "append"/[fb(i) for i in 1..n] where fb(i) ==
       labelList := 
         "append"/[fc(i,j) for j in 1..2] where fc(i,j) ==
            unam := INTERN STRCONC ('"u",STRINGIMAGE i,STRINGIMAGE j)
            [['bcStrings,[6, 0, unam, 'F]]]
       prefix := ('"\newline ")
       labelList := [['text,:prefix],:labelList]
  uList :=  [['text,:middle],:uList]
  mid := ('"\blankline \menuitemstyle{} \tab{2} Enter {\it V(n,2)}. ")
  mid := STRCONC(mid,'"If U(i,j) is known V(i,j) ")
  mid := STRCONC(mid,'"= 0.0, else V(i,j) = 1.0. \newline ")
  vList := 
    "append"/[fd(i) for i in 1..n] where fd(i) ==
       labelList := 
         "append"/[fe(i,j) for j in 1..2] where fe(i,j) ==
            vnam := INTERN STRCONC ('"v",STRINGIMAGE i,STRINGIMAGE j)
            [['bcStrings,[6, 0, vnam, 'F]]]
       prefix := ('"\newline ")
       labelList := [['text,:prefix],:labelList]
  vList :=  [['text,:mid],:vList]
  xList := 
    "append"/[ff(i) for i in 1..mnp] where ff(i) ==
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      [['bcStrings,[8, "0.0", xnam, 'F]]]
  end := ('"\blankline \menuitemstyle{} \tab{2} Enter the initial mesh ")
  end := STRCONC(end,'"{\it X(mnp)}: \newline ")
  xList :=  [['text,:end],:xList]
  equationPart := [
     '(domainConditions 
        (isDomain EM $EmptyMode)
          (isDomain S (String))
            (isDomain F (Float))
              (isDomain I (Integer))),
                :funcList,:uList,:vList,:xList]
  page := htInitPage('"D02GAF - ODEs, boundary value problem, finite difference technique with deferred correction, simple nonlinear problem", nil)
  htSay '"\menuitemstyle{}\tab{2} "
  htSay '"Enter the functions (i.e. the derivatives) below as functions of "
  htSay '"Y[1]...Y[n]: "
  htSay '"\newline "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'d02gafGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'a,a)
  htpSetProperty(page,'b,b)
  htpSetProperty(page,'mnp,mnp)
  htpSetProperty(page,'np,np)
  htpSetProperty(page,'lw,lw)
  htpSetProperty(page,'liw,liw)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

d02gafDefaultSolve(htPage,a,b,mnp,np,lw,liw,tol,ifail) ==
  n := '3
  page := htInitPage('"D02GAF - ODEs, boundary value problem, finite difference technique with deferred correction, simple nonlinear problem", nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the functions (i.e. the derivatives) below ")
    (text . "as functions of Y[1]...Y[n]: ")
    (text . "\newline ")
    (text . "\newline {\em Function 1:} \space{1}")
    (bcStrings (42 "Y[2]" f1 EM))
    (text . "\newline {\em Function 2:} \space{1}")
    (bcStrings (42 "Y[3]" f2 EM))
    (text . "\newline {\em Function 3:} \space{1}")
    (bcStrings (42 "-Y[1]*Y[3]-0.2*(1-Y[2]*Y[2])" f3 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter known or estimated values of \htbitmap{yi} at a and b,")
    (text . " {\it U(n,2)}. ")
    (text . " [\htbitmap{yi}(a) in the first column, \htbitmap{yi}(b) ")
    (text . "in the second.] \newline ")
    (bcStrings (6 "0" u11 F))
    (bcStrings (6 "10" u21 F))
    (text . "\newline ")
    (bcStrings (6 "0" u12 F))
    (bcStrings (6 "1" u22 F))
    (text . "\newline ")
    (bcStrings (6 "0" u13 F))
    (bcStrings (6 "0" u23 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter {\it V(n,2)}. ")
    (text . "If U(i,j) is known V(i,j) = 0.0, else V(i,j) = 1.0: \newline")
    (bcStrings (6 "0.0" v11 F))
    (bcStrings (6 "1.0" v21 F))
    (text . "\newline ")
    (bcStrings (6 "0.0" v12 F))
    (bcStrings (6 "0.0" v22 F))
    (text . "\newline ")
    (bcStrings (6 "1.0" v13 F))
    (bcStrings (6 "1.0" v23 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} ")
    (text . "Enter the initial mesh {\it X(mnp)}: ")
    (text . "\newline ")
    (bcStrings (8 "0.0" x1 F))
    (bcStrings (8 "0.4" x2 F))
    (bcStrings (8 "0.8" x3 F))
    (bcStrings (8 "1.2" x4 F))
    (bcStrings (8 "1.6" x5 F))
    (bcStrings (8 "2.0" x6 F))
    (bcStrings (8 "2.4" x7 F))
    (bcStrings (8 "2.8" x8 F))
    (bcStrings (8 "3.2" x9 F))
    (bcStrings (8 "3.6" x10 F))
    (bcStrings (8 "4.0" x11 F))
    (bcStrings (8 "4.4" x12 F))
    (bcStrings (8 "4.8" x13 F))
    (bcStrings (8 "5.2" x14 F))
    (bcStrings (8 "5.6" x15 F))
    (bcStrings (8 "6.0" x16 F))
    (bcStrings (8 "6.4" x17 F))
    (bcStrings (8 "6.8" x18 F))
    (bcStrings (8 "7.2" x19 F))
    (bcStrings (8 "7.6" x20 F))
    (bcStrings (8 "8.0" x21 F))
    (bcStrings (8 "8.4" x22 F))
    (bcStrings (8 "8.8" x23 F))
    (bcStrings (8 "9.2" x24 F))
    (bcStrings (8 "9.6" x25 F))
    (bcStrings (8 "10.0" x26 F))
    (bcStrings (8 "0.0" x27 F))
    (bcStrings (8 "0.0" x28 F))
    (bcStrings (8 "0.0" x29 F))
    (bcStrings (8 "0.0" x30 F))
    (bcStrings (8 "0.0" x31 F))
    (bcStrings (8 "0.0" x32 F))
    (bcStrings (8 "0.0" x33 F))
    (bcStrings (8 "0.0" x34 F))
    (bcStrings (8 "0.0" x35 F))
    (bcStrings (8 "0.0" x36 F))
    (bcStrings (8 "0.0" x37 F))
    (bcStrings (8 "0.0" x38 F))
    (bcStrings (8 "0.0" x39 F))
    (bcStrings (8 "0.0" x40 F))
    (bcStrings (8 "0.0" x41 F))
    (bcStrings (8 "0.0" x42 F))
    (bcStrings (8 "0.0" x43 F))
    (bcStrings (8 "0.0" x44 F))
    (bcStrings (8 "0.0" x45 F))
    (bcStrings (8 "0.0" x46 F))
    (bcStrings (8 "0.0" x47 F))
    (bcStrings (8 "0.0" x48 F))
    (bcStrings (8 "0.0" x49 F))
    (bcStrings (8 "0.0" x50 F))
    (bcStrings (8 "0.0" x51 F))
    (bcStrings (8 "0.0" x52 F))
    (bcStrings (8 "0.0" x53 F))
    (bcStrings (8 "0.0" x54 F))
    (bcStrings (8 "0.0" x55 F))   
    (bcStrings (8 "0.0" x56 F))   
    (bcStrings (8 "0.0" x57 F))  
    (bcStrings (8 "0.0" x58 F))   
    (bcStrings (8 "0.0" x59 F))   
    (bcStrings (8 "0.0" x60 F))   
    (bcStrings (8 "0.0" x61 F))   
    (bcStrings (8 "0.0" x62 F))   
    (bcStrings (8 "0.0" x63 F))   
    (bcStrings (8 "0.0" x64 F)))
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'a,a)
  htpSetProperty(page,'b,b)
  htpSetProperty(page,'mnp,mnp)
  htpSetProperty(page,'np,np)
  htpSetProperty(page,'lw,lw)
  htpSetProperty(page,'liw,liw)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'ifail,ifail)
  htMakeDoneButton('"Continue",'d02gafGen)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

d02gafGen htPage ==
  n := htpProperty(htPage, 'n)
  a := htpProperty(htPage, 'a)
  b := htpProperty(htPage, 'b)
  mnp := htpProperty(htPage, 'mnp)
  np := htpProperty(htPage, 'np)
  lw := htpProperty(htPage, 'lw)
  liw := htpProperty(htPage, 'liw)
  ifail := htpProperty(htPage,'ifail)
  tol := htpProperty(htPage,'tol)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..mnp repeat
    x := STRCONC((first y).1," ")
    xList := [x,:xList]
    y := rest y
  xstring := bcwords2liststring xList
  for i in 1..n repeat
    for j in 1..2 repeat
      v := STRCONC((first y).1," ")
      rowList := [v,:rowList]
      y := rest y
    vList := [:vList,rowList]
    rowList := []
  for i in 1..n repeat
    for j in 1..2 repeat
      u := STRCONC((first y).1," ")
      rowList := [u,:rowList]
      y := rest y
    uList := [:uList,rowList]
    rowList := []
  vList := reverse vList
  uList := reverse uList
  vstring := bcwords2liststring [bcwords2liststring x for x in vList]
  ustring := bcwords2liststring [bcwords2liststring x for x in uList]
  while y repeat
    f := STRCONC((first y).1," ")
    fList := [f,:fList]
    y := rest y
  fstring := bcwords2liststring fList
  Y:='Y
  prefix := STRCONC("d02gaf(",ustring,", ",vstring,", ",STRINGIMAGE n,", ")
  prefix := STRCONC(prefix,a,", ",b,", ",tol,", ")
  prefix := STRCONC(prefix,STRINGIMAGE mnp,", ",STRINGIMAGE lw,", ")
  prefix := STRCONC(prefix,STRINGIMAGE liw,", [",xstring,"], ",STRINGIMAGE np)
  end:=STRCONC (",",STRINGIMAGE ifail,",(",fstring,"::Vector Expression Float")
  linkGen STRCONC (prefix,end,")::ASP7('FCN))")

d02gbf() ==
  htInitPage('"D02GBF - ODEs, boundary value problem, finite difference technique with deferred correction, general nonlinear problem", nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXd02gbf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|d02gbf| '|NagOrdinaryDifferentialEquationsPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "D02GBF solves a general linear two-point boundary value problem ")
    (text . "for a system of n ODEs {\it y' = F(x)y + g(x)} on the range ")
    (text . "[a,b] with boundary conditions {\it Cy(a) + Dy(b) = \gamma} ")
    (text . "using a deferred correction technique.")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the number of equations in the system {\it n}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 2 n PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "\newline Left hand boundary point {\it a}: ")
    (text . "\tab{32} \menuitemstyle{}\tab{34} ")
    (text . "Right hand boundary {\it b}:")
    (text . "\newline\tab{2} ")
    (bcStrings (10 "0.0" a F))
    (text . "\tab{34} ")
    (bcStrings (10 "1.0" b F))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "\newline Max number of mesh points {\it mnp}:")
    (text . "\tab{32} \menuitemstyle{}\tab{34} ")
    (text . "Number of points {\it np}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (10 70 mnp PI))
    (text . "\tab{34} ")
    (bcStrings (10 0 np PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Accuracy required {\it tol}:")
    (text . "\newline\tab{2} ")
    (bcStrings (10 "1.0e-3" tol F))
    (text . "\blankline ")
    (text . "\newline \tab{2} ")
    (text . "Ifail is input in three components: ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "{\it a} ")
    (radioButtons afail
        ("" "  0, hard failure" azero)
        ("" "  1, soft failure" aone))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "{\it b} ")
    (radioButtons bfail
        ("" "  1, print error messages" bone)
        ("" "  0, suppress error messages" bzero))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "{\it c} ")
    (radioButtons cfail
        ("" "  1, print warning messages" cone)
        ("" "  0, suppress warning messages" czero)))
  htMakeDoneButton('"Continue", 'd02gbfSolve)
  htShowPage()

d02gbfSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  a := htpLabelInputString(htPage,'a)
  b := htpLabelInputString(htPage,'b)
  mnp :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'mnp)
    objValUnwrap htpLabelSpadValue(htPage, 'mnp)
  np :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'np)
    objValUnwrap htpLabelSpadValue(htPage, 'np)
  lw := mnp * (3*n*n + 5*n + 2) + 3*n*n + 5*n
  liw := mnp * (2*n + 1) + n
  tol := htpLabelInputString(htPage,'tol)
  aerror := htpButtonValue(htPage,'afail)
  afail :=
    aerror = 'azero => '0
    '1
  berror := htpButtonValue(htPage,'bfail)
  bfail :=
    berror = 'bone => '1
    '0
  cerror := htpButtonValue(htPage,'cfail)
  cfail :=
    cerror = 'cone => '1
    '0
  ifail := 100*cfail + 10*bfail + afail
  n = '2 => d02gbfDefaultSolve(htPage,a,b,mnp,np,lw,liw,tol,ifail)
  cList := 
    "append"/[fa(i,n) for i in 1..n] where fa(i,n) ==
       labelList := 
         "append"/[fb(i,j) for j in 1..n] where fb(i,j) ==
            cnam := INTERN STRCONC ('"c",STRINGIMAGE i,STRINGIMAGE j)
            [['bcStrings,[6, 0, cnam, 'F]]]
       prefix := ('"\newline ")
       labelList := [['text,:prefix],:labelList]
  middle := ('"\blankline \menuitemstyle{} \tab{2} Enter the matrix {\it D}: ")
  middle := STRCONC(middle,"\newline ")  
  dList := 
    "append"/[fc(i,n) for i in 1..n] where fc(i,n) ==
       labelList := 
         "append"/[fd(i,j) for j in 1..n] where fd(i,j) ==
            dnam := INTERN STRCONC ('"d",STRINGIMAGE i,STRINGIMAGE j)
            [['bcStrings,[6, 0, dnam, 'F]]]
       prefix := ('"\newline ")
       labelList := [['text,:prefix],:labelList]
  dList :=  [['text,:middle],:dList]
  middle := ('"\blankline \menuitemstyle{} \tab{2} Enter the vector \gamma: ")
  middle := STRCONC(middle,"\newline ")  
  gamList := 
    "append"/[fe(i) for i in 1..n] where fe(i) ==
      gamnam := INTERN STRCONC ('"gam",STRINGIMAGE i)
      [['bcStrings,[6, 0, gamnam, 'F]]]
  prefix := ('"\newline ")
  gamList := [['text,:middle],:gamList]
  middle := ('"\blankline \menuitemstyle{} \tab{2} Enter the matrix ")
  middle := STRCONC(middle,"{\it F(x)} from the equation {\it y' =} ")
  middle := STRCONC(middle,"{\it F(x)y + g(x)}: \newline ")  
  fList := 
    "append"/[ff(i,n) for i in 1..n] where ff(i,n) ==
       labelList := 
         "append"/[fg(i,j) for j in 1..n] where fg(i,j) ==
            fnam := INTERN STRCONC ('"f",STRINGIMAGE i,STRINGIMAGE j)
            [['bcStrings,[6, 0, fnam, 'F]]]
       prefix := ('"\newline ")
       labelList := [['text,:prefix],:labelList]
  fList :=  [['text,:middle],:fList]
  mid := ('"\blankline \menuitemstyle{} \tab{2} Enter the vector {\it g(x)}: ")
  mid := STRCONC(mid,'"\newline ")
  gList := 
    "append"/[fh(i) for i in 1..n] where fh(i) ==
      gnam := INTERN STRCONC ('"g",STRINGIMAGE i)
      [['bcStrings,[6, 0, gnam, 'F]]]
  prefix := ('"\newline ")
  gList := [['text,:middle],:gList]
  xList := 
    "append"/[fi(i) for i in 1..mnp] where fi(i) ==
      xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
      [['bcStrings,[8, "0.0", xnam, 'F]]]
  end := ('"\blankline \menuitemstyle{} \tab{2} The initial mesh {\it X(mnp)}")
  end := STRCONC(end,'", (all entries = 0 if np < 4): \newline ")
  xList :=  [['text,:end],:xList]
  equationPart := [
     '(domainConditions 
        (isDomain EM $EmptyMode)
          (isDomain S (String))
            (isDomain F (Float))
              (isDomain I (Integer))),
                :cList,:dList,:gamList,:fList,:gList,:xList]
  page := htInitPage('"D02GBF - ODEs, boundary value problem, finite difference technique with deferred correction, general nonlinear problem", nil)
  htSay '"\menuitemstyle{}\tab{2} "
  htSay '"Enter the matrix {\it C} form the equation {\it Cy(a) + Dy(b)} "
  htSay '"= \gamma \newline "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'d02gbfGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'a,a)
  htpSetProperty(page,'b,b)
  htpSetProperty(page,'mnp,mnp)
  htpSetProperty(page,'np,np)
  htpSetProperty(page,'lw,lw)
  htpSetProperty(page,'liw,liw)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

d02gbfDefaultSolve(htPage,a,b,mnp,np,lw,liw,tol,ifail) ==
  n := '2
  page := htInitPage('"D02GBF - ODEs, boundary value problem, finite difference technique with deferred correction, general nonlinear problem", nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the matrix {\it C} from the equation {\it Cy(a) + Dy(b)} = \gamma:")
    (text . "\newline ")
    (bcStrings (6 "1" c11 F))
    (bcStrings (6 "0" c12 F))
    (text . "\newline ")
    (bcStrings (6 "0" c21 F))
    (bcStrings (6 "0" c22 F))
    (text . "\blankline \menuitemstyle{}\tab{2}")
    (text . "Enter the matrix {\it D}: \newline ")
    (bcStrings (6 "0" d11 F))
    (bcStrings (6 "0" d12 F))
    (text . "\newline ")
    (bcStrings (6 "1" d21 F))
    (bcStrings (6 "0" d22 F))
    (text . "\blankline \menuitemstyle{}\tab{2}")
    (text . "Enter the vector \gamma: \newline ")
    (bcStrings (6 "0" gam1 F))
    (bcStrings (6 "1" gam2 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the matrix {\it F(x)} from the equation {\it y' = F(x)y + g(x)} : ")
    (text . "\newline ")
    (bcStrings (6 "0" f11 F))
    (bcStrings (6 "1" f12 F))
    (text . "\newline ")
    (bcStrings (6 "0" f21 F))
    (bcStrings (6 "-10" f22 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the vector {\it g(x)}: ")
    (text . "\newline ")
    (bcStrings (6 "0" g1 F))
    (bcStrings (6 "0" g2 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2} The initial mesh {\it X(mnp)}, ")
    (text . "(all entries = 0 if np < 4): \newline ")
    (bcStrings (8 "0.0" x1 F))
    (bcStrings (8 "0.0" x2 F))
    (bcStrings (8 "0.0" x3 F))
    (bcStrings (8 "0.0" x4 F))
    (bcStrings (8 "0.0" x5 F))
    (bcStrings (8 "0.0" x6 F))
    (bcStrings (8 "0.0" x7 F))
    (bcStrings (8 "0.0" x8 F))
    (bcStrings (8 "0.0" x9 F))
    (bcStrings (8 "0.0" x10 F))
    (bcStrings (8 "0.0" x11 F))
    (bcStrings (8 "0.0" x12 F))
    (bcStrings (8 "0.0" x13 F))
    (bcStrings (8 "0.0" x14 F))
    (bcStrings (8 "0.0" x15 F))
    (bcStrings (8 "0.0" x16 F))
    (bcStrings (8 "0.0" x17 F))
    (bcStrings (8 "0.0" x18 F))
    (bcStrings (8 "0.0" x19 F))
    (bcStrings (8 "0.0" x20 F))
    (bcStrings (8 "0.0" x21 F))
    (bcStrings (8 "0.0" x22 F))
    (bcStrings (8 "0.0" x23 F))
    (bcStrings (8 "0.0" x24 F))
    (bcStrings (8 "0.0" x25 F))
    (bcStrings (8 "0.0" x26 F))
    (bcStrings (8 "0.0" x27 F))
    (bcStrings (8 "0.0" x28 F))
    (bcStrings (8 "0.0" x29 F))
    (bcStrings (8 "0.0" x30 F))
    (bcStrings (8 "0.0" x31 F))
    (bcStrings (8 "0.0" x32 F))
    (bcStrings (8 "0.0" x33 F))
    (bcStrings (8 "0.0" x34 F))
    (bcStrings (8 "0.0" x35 F))
    (bcStrings (8 "0.0" x36 F))
    (bcStrings (8 "0.0" x37 F))
    (bcStrings (8 "0.0" x38 F))
    (bcStrings (8 "0.0" x39 F))
    (bcStrings (8 "0.0" x40 F))
    (bcStrings (8 "0.0" x41 F))
    (bcStrings (8 "0.0" x42 F))
    (bcStrings (8 "0.0" x43 F))
    (bcStrings (8 "0.0" x44 F))
    (bcStrings (8 "0.0" x45 F))
    (bcStrings (8 "0.0" x46 F))
    (bcStrings (8 "0.0" x47 F))
    (bcStrings (8 "0.0" x48 F))
    (bcStrings (8 "0.0" x49 F))
    (bcStrings (8 "0.0" x50 F))
    (bcStrings (8 "0.0" x51 F))
    (bcStrings (8 "0.0" x52 F))
    (bcStrings (8 "0.0" x53 F))
    (bcStrings (8 "0.0" x54 F))
    (bcStrings (8 "0.0" x55 F))   
    (bcStrings (8 "0.0" x56 F))   
    (bcStrings (8 "0.0" x57 F))  
    (bcStrings (8 "0.0" x58 F))   
    (bcStrings (8 "0.0" x59 F))   
    (bcStrings (8 "0.0" x60 F))   
    (bcStrings (8 "0.0" x61 F))   
    (bcStrings (8 "0.0" x62 F))   
    (bcStrings (8 "0.0" x63 F))   
    (bcStrings (8 "0.0" x64 F))
    (bcStrings (8 "0.0" x65 F))
    (bcStrings (8 "0.0" x66 F))
    (bcStrings (8 "0.0" x67 F))
    (bcStrings (8 "0.0" x68 F))
    (bcStrings (8 "0.0" x69 F))
    (bcStrings (8 "0.0" x70 F)))
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'a,a)
  htpSetProperty(page,'b,b)
  htpSetProperty(page,'mnp,mnp)
  htpSetProperty(page,'np,np)
  htpSetProperty(page,'lw,lw)
  htpSetProperty(page,'liw,liw)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'ifail,ifail)
  htMakeDoneButton('"Continue",'d02gbfGen)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

d02gbfGen htPage ==
  n := htpProperty(htPage, 'n)
  a := htpProperty(htPage, 'a)
  b := htpProperty(htPage, 'b)
  mnp := htpProperty(htPage, 'mnp)
  np := htpProperty(htPage, 'np)
  lw := htpProperty(htPage, 'lw)
  liw := htpProperty(htPage, 'liw)
  ifail := htpProperty(htPage,'ifail)
  tol := htpProperty(htPage,'tol)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..mnp repeat -- matrix
    x := STRCONC((first y).1," ")
    xList := [x,:xList]
    y := rest y
  xstring := bcwords2liststring xList
  for i in 1..n repeat  -- vector g
    g := STRCONC((first y).1," ")
    gList := [g,:gList]
    y := rest y
  gstring := bcwords2liststring gList
  for i in 1..n repeat -- matrix F
    for j in 1..n repeat
      f := STRCONC((first y).1," ")
      flist := [f,:flist]
      y := rest y
    fmatlist := [:fmatlist,flist]
    flist := []
  fmatlist := reverse fmatlist 
  fmatstr := bcwords2liststring [bcwords2liststring x for x in fmatlist]
  for i in 1..n repeat  -- vector gamma
    gam := STRCONC((first y).1," ")
    gamList := [gam,:gamList]
    y := rest y
  gamstr := bcwords2liststring gamList
  for i in 1..n repeat -- matrix D
    for j in 1..n repeat
      d := STRCONC((first y).1," ")
      dlist := [d,:dlist]
      y := rest y
    dmatlist := [:dmatlist,dlist]
    dlist := []
  dmatlist := reverse dmatlist 
  dmatstr := bcwords2liststring [bcwords2liststring x for x in dmatlist]
  for i in 1..n repeat -- matrix C
    for j in 1..n repeat
      c := STRCONC((first y).1," ")
      clist := [c,:clist]
      y := rest y
    cmatlist := [:cmatlist,clist]
    clist := []
  cmatlist := reverse cmatlist 
  cmatstr := bcwords2liststring [bcwords2liststring x for x in cmatlist]
  prefix := STRCONC("d02gbf(",STRINGIMAGE a,", ",STRINGIMAGE b,", ")
  prefix := STRCONC(prefix,STRINGIMAGE n,", ",tol,", ",STRINGIMAGE mnp,", ")
  prefix := STRCONC(prefix,STRINGIMAGE lw,", ",STRINGIMAGE liw,", ")
  prefix := STRCONC(prefix,cmatstr,"::Matrix DoubleFloat,",dmatstr,"::Matrix DoubleFloat,[",gamstr,"]::Matrix DoubleFloat,[",xstring,"]::Matrix DoubleFloat, ")
  mid := STRCONC(STRINGIMAGE np,", ",STRINGIMAGE ifail,", ")
  end := STRCONC("(",fmatstr,"::Matrix(Expression(Float)))::ASP77(FCNF),(",gstring)
  linkGen STRCONC(prefix,mid,end,"::Vector(Expression(Float)))::ASP78(FCNG))")

d02kef() ==
  htInitPage('"D02KEF - 2nd order Sturm-Liouville problem, regular/singular system, finite/infinite range, eigenvalue and eigenfunction, user-specified break-points", nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXd02kef} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|d02kef| '|NagOrdinaryDifferentialEquationsPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "D02KEF finds a specified eigenvalue \htbitmap{lamdab} of a ")
    (text . "regular or second-order Sturm-Liouville system ")
    (text . "{\it(p(x)y')' + q(x; \lambda)y = 0} on a finite or infinite ")
    (text . "range [a,b]; a Pruefer transformation and shooting method ")
    (text . "are used; discontinuities in coefficient functions or their ")
    (text . "derivatives are permitted. ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of points in XPOINT {\it m}:")
    (text . "\newline\tab{2} ")
    (bcStrings (6 5 m PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "\newline Index of the `break-point' {\it match}:")
    (text . "\newline\tab{2} ")
    (bcStrings (6 0 match PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "\newline Index of the required eigenvalue {\it k}:")
    (text . "\newline\tab{2} ")
    (bcStrings (6 11 k PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Accuracy required {\it tol}:")
    (text . "\newline\tab{2} ")
    (bcStrings (10 "0.0001" tol F))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Eigenvalue estimate {\it elam}: ")
    (text . "\tab{32} \menuitemstyle{}\tab{34} ")
    (text . "Scale of the problem {\it delam}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (10 "14" elam F))
    (text . "\tab{34} ")
    (bcStrings (10 "1" delam F))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} \newline ")
    (text . "Max iterations {\it maxit}:")
    (text . "\tab{32} \menuitemstyle{}\tab{34} ")
    (text . "Max COEFFN calls {\it maxfun}:")
    (text . "\newline\tab{2} ")
    (bcStrings (10 0 maxit PI))
    (text . "\tab{34} ")
    (bcStrings (10 0 maxfun PI))
    (text . "\blankline ")
    (text . "\tab{2} \newline {\it Note:} no bound is assumed ") 
    (text . "if maxit = 0 \blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Ifail value:")
    (radioButtons ifail
        ("" " -1, Print error messages" minusOne)
        ("" "  1, Suppress error messages" one)))
  htMakeDoneButton('"Continue", 'd02kefSolve)
  htShowPage()

d02kefSolve htPage ==
  m :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'm)
    objValUnwrap htpLabelSpadValue(htPage, 'm)
  match :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'match)
    objValUnwrap htpLabelSpadValue(htPage, 'match)
  k :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'k)
    objValUnwrap htpLabelSpadValue(htPage, 'k)
  tol := htpLabelInputString(htPage,'tol)
  elam := htpLabelInputString(htPage,'elam)
  delam := htpLabelInputString(htPage,'delam)
  maxit :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'maxit)
    objValUnwrap htpLabelSpadValue(htPage, 'maxit)
  maxfun :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'maxfun)
    objValUnwrap htpLabelSpadValue(htPage, 'maxfun)
  error := htpButtonValue(htPage,'ifail)
  ifail :=
    error = 'minusOne => '-1
    '1
  m = '5 =>d02kefDefaultSolve(htPage,match,k,tol,elam,delam,maxit,maxfun,ifail)
  xpList := 
    "append"/[fa(i) for i in 1..m] where fa(i) ==
      xpnam := INTERN STRCONC ('"xp",STRINGIMAGE i)
      [['bcStrings,[10, "0.0", xpnam, 'EM]]]
  middle:=('"\blankline \menuitemstyle{} \tab{2} Value of {\it p} for COEFFN:")
  middle := STRCONC(middle,"\newline ")
  cList :=  [['text,:middle],['bcStrings,[42, "0.0", 'c1, 'EM]]]
  middle:=('"\blankline \menuitemstyle{} \tab{2} Value of {\it q} for COEFFN:")
  middle := STRCONC(middle,"\newline ")
  c1List := [['text,:middle],['bcStrings,[42, "0.0", 'c2, 'EM]]]
  cList :=  [:cList,:c1List]
  middle:=('"\blankline \menuitemstyle{} \tab{2} Value of {\it dqdl}")
  middle := STRCONC(middle," for COEFFN: \newline ")
  c2List := [['text,:middle],['bcStrings,[42, "0.0", 'c3, 'EM]]]
  cList :=  [:cList,:c2List]
  middle:=('"\blankline \menuitemstyle{} \tab{2} Values of YL(1) & YL(2) ")
  middle := STRCONC(middle,"for BDYVAL: \newline ")
  ylList := 
    "append"/[fb(i) for i in 1..2] where fb(i) ==
      ylnam := INTERN STRCONC ('"yl",STRINGIMAGE i)
      [['bcStrings,[42, "0.0", ylnam, 'EM]]]
  ylList := [['text,:middle],:ylList]
  middle:=('"\blankline \menuitemstyle{} \tab{2} Values of YR(1) & YR(2) ")
  middle := STRCONC(middle,"for BDYVAL: \newline ")
  yrList := 
    "append"/[fc(i) for i in 1..2] where fc(i) ==
      yrnam := INTERN STRCONC ('"yr",STRINGIMAGE i)
      [['bcStrings,[42, "0.0", yrnam, 'EM]]]
  yrList := [['text,:middle],:yrList]
  middle:=('"\blankline \menuitemstyle{} \tab{2} Maximum step size ")
  middle := STRCONC(middle,"{\it hmax(2,m)}: \newline ")
  hList := 
    "append"/[fd(i,m) for i in 1..2] where fd(i,m) ==
       labelList := 
         "append"/[fe(i,j) for j in 1..m] where fe(i,j) ==
            hnam := INTERN STRCONC ('"h",STRINGIMAGE i,STRINGIMAGE j)
            [['bcStrings,[6, "0.0", hnam, 'F]]]
       prefix := ('"\newline ")
       labelList := [['text,:prefix],:labelList]
  hList := [['text,:middle],:hList]
  equationPart := [
     '(domainConditions 
        (isDomain EM $EmptyMode)
          (isDomain S (String))
            (isDomain F (Float))
              (isDomain I (Integer))),
                :xpList,:cList,:ylList,:yrList,:hList]
  page := htInitPage('"D02KEF - 2nd order Sturm-Liouville problem, regular/singular system, finite/infinite range, eigenvalue and eigenfunction, user-specified break-points", nil)
  htSay '"\menuitemstyle{}\tab{2} Enter points where boundary "
  htSay '"conditions are to be imposed {\it xpoint}: \newline "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'d02kefGen)
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'match,match)
  htpSetProperty(page,'k,k)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'elam,elam)
  htpSetProperty(page,'delam,delam)
  htpSetProperty(page,'maxit,maxit)
  htpSetProperty(page,'maxfun,maxfun)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

d02kefDefaultSolve(htPage,match,k,tol,elam,delam,maxit,maxfun,ifail) ==
  m := '5
  page := htInitPage('"D02KEF - 2nd order Sturm-Liouville problem, regular/singular system, finite/infinite range, eigenvalue and eigenfunction, user-specified break-points", nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter points where boundary conditions are to be imposed ")
    (text . "{\it xpoint}: \newline ")
    (bcStrings (10 "0.0" xp1 F))
    (bcStrings (10 "0.1" xp2 F))
    (bcStrings (10 "4**(1/3)" xp3 F))
    (bcStrings (10 "30.0" xp4 F))
    (bcStrings (10 "30.0" xp5 F))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Value of {\it p} for COEFFN: \newline ")
    (bcStrings (42 "1.0" c1 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Value of {\it q} for COEFFN: \newline ")
    (bcStrings (42 "ELAM-X-2.0/(X*X)" c2 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Value of {\it dqdl} for COEFFN: \newline ")
    (bcStrings (42 "1.0" c3 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Values of YL(1) & YL(2) for BDYVAL: \newline ")
    (bcStrings (42 "XL" yl1 EM))
    (bcStrings (42 "2.0" yl2 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Values of YR(1) & YR(2) for BDYVAL: \newline ")
    (bcStrings (42 "1.0" yr1 EM))
    (bcStrings (42 "-sqrt(XR-ELAM)" yr2 EM))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Maximum step size {\it hmax(2,m)}: \newline ")
    (bcStrings (6 "0.0" h11 F))
    (bcStrings (6 "0.0" h12 F))
    (bcStrings (6 "0.0" h13 F))
    (bcStrings (6 "0.0" h14 F))
    (bcStrings (6 "0.0" h15 F))
    (text . "\newline ")
    (bcStrings (6 "0.0" h21 F))
    (bcStrings (6 "0.0" h22 F))
    (bcStrings (6 "0.0" h23 F))
    (bcStrings (6 "0.0" h24 F))
    (bcStrings (6 "0.0" h25 F)))
  htpSetProperty(page,'m,m)
  htpSetProperty(page,'match,match)
  htpSetProperty(page,'k,k)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'elam,elam)
  htpSetProperty(page,'delam,delam)
  htpSetProperty(page,'maxit,maxit)
  htpSetProperty(page,'maxfun,maxfun)
  htpSetProperty(page,'ifail,ifail)
  htMakeDoneButton('"Continue",'d02kefGen)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

d02kefGen htPage ==
  m := htpProperty(htPage, 'm)
  match := htpProperty(htPage, 'match)
  k := htpProperty(htPage, 'k)
  tol := htpProperty(htPage, 'tol)
  elam := htpProperty(htPage, 'elam)
  delam := htpProperty(htPage, 'delam)
  maxit := htpProperty(htPage, 'maxit)
  maxfun := htpProperty(htPage, 'maxfun)
  ifail := htpProperty(htPage,'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..m repeat
    for j in 1..2 repeat
      h := STRCONC((first y).1," ")
      rowList := [h,:rowList]
      y := rest y
    hList := [:hList,rowList]
    rowList := []
  hList := reverse hList
  hstring := bcwords2liststring [bcwords2liststring x for x in hList]
  for i in 1..2 repeat
    for j in 1..2 repeat
      b := STRCONC((first y).1," ")
      rowList := [b,:rowList]
      y := rest y
    bList := [:bList,rowList]
    rowList := []
  bList := reverse bList
  bstring := bcwords2liststring [bcwords2liststring x for x in bList]
  for i in 1..3 repeat
    c := STRCONC((first y).1," ")
    cList := [c,:cList]
    y := rest y
  cstring := bcwords2liststring cList
  while y repeat
    x := STRCONC((first y).1," ")
    xList := [x,:xList]
    y := rest y
  xstring := bcwords2liststring xList
  prefix := STRCONC("d02kef([",xstring,"]::Matrix DoubleFloat, ",STRINGIMAGE m)
  prefix := STRCONC(prefix,", ",STRINGIMAGE k,", ",tol,", ",STRINGIMAGE maxfun)
  prefix := STRCONC(prefix,", ",STRINGIMAGE match,", ",STRINGIMAGE elam,", ")
  prefix:=STRCONC(prefix,STRINGIMAGE delam,", ",hstring,", ",STRINGIMAGE maxit)
  end := STRCONC(", ",STRINGIMAGE ifail,",(",cstring,"::Vector(Expression Float))::ASP10(COEFFN)")
  end := STRCONC(end,", (",bstring,"::Matrix Expression Float)::ASP80('BDYVAL))")
  linkGen STRCONC (prefix,end)

d02raf() ==
  htInitPage('"D02RAF - ODEs, general nonlinear boundary value problem, finite difference technique with deferred correction, continuation facility",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\windowlink{Manual Page}{manpageXXd02raf} for this routine ")
    (text . "\newline ")
    (text . "\lispwindowlink{Browser operation page}{(|oPageFrom| '|d02raf| '|NagOrdinaryDifferentialEquationsPackage|)} for this routine")
    (text . "\newline \horizontalline ")
    (text . "\newline ")
    (text . "D02RAF solves a two-point boundary value problem for a system ")
    (text . "of {\it n} first-order ordinary differential equations ")
    (text . "{\it \htbitmap{yi}'= \htbitmap{fi}(x,y)}, for {\it i} = 1,2,...,")
    (text . "{\it n}, on the range [a,b] with {\it n} nonlinear boundary ")
    (text . "conditions \htbitmap{gi}{\it (y(a),y(b)) = 0} for {\it i} = 1,2,")
    (text . "...,{\it n} using a deferred correction technique and a Newton ")
    (text . "iteration; the solution is computed on a mesh.  A continuation ")
    (text . "facility is provided for which a family of problems is solved ")
    (text . "posed as {\it y' = f(x,y,\epsilon)} subject to the boundary ")
    (text . "conditions {\it g(y(a),y(b),\epsilon) = 0}, where \epsilon ")
    (text . "is the continuation parameter. The choice \epsilon = 0 should ")
    (text . "define an easy problem to solve and \epsilon = 1 the problem ")
    (text . "whose solution is required; a sequence of problems is solved ")
    (text . "with 0 = \htbitmap{ep1} < \htbitmap{ep2} < ... \htbitmap{epp} ")
    (text . "= 1 where {\it p} and the \htbitmap{epi} are chosen by D02RAF. ")
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the number of differential equations {\it n}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 3 n PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "The maximum number of points in the mesh {\it mnp}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 40 mnp PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Number of points in the initial mesh {\it np}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 17 np PI))
    (text . "\blankline ")
    (text . "\newline \menuitemstyle{}\tab{2} ")
    (text . "\newline Number of boundary conditions involving y(a) only ")
    (text . "{\it numbeg}: \newline\tab{2} ")
    (bcStrings (5 2 numbeg PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "\newline Boundary conditions involving both y(a) and ")
    (text . "y(b) {\it nummix}: \newline\tab{2} ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 0 nummix PI))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Absolute error tolerance {\it tol}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (10 "1.0e-4" tol F))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Do you wish to use an intial mesh or default values,{\it init} ")
    (radioButtons init
        ("" " default values" init_zero)
        ("" " initial mesh" init_nonZero))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "First dimension of y, {\it iy}: ")
    (text . "\newline\tab{2} ")
    (bcStrings (5 3 iy PI))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Are JACOBF & JACOBG routines being supplied, {\it ijac}:")
    (radioButtons ijac
        ("" " yes" ijac_nonZero)
        ("" " no" ijac_zero))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Continuation facility {\it deleps}:")
    (text . "\newline\tab{2} ")
    (bcStrings (5 "0.1" deleps F))
    (text . "\newline\tab{2} ")
    (text . "Note: For 0.0 \htbitmap{great=} deleps > 1.0, continuation ")    
    (text . "is not used. ")
    (text . "\blankline ")
    (text . "\newline \tab{2} ")
    (text . "Ifail is input in three components: ")
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "{\it a} ")
    (radioButtons afail
        ("" "  0, hard failure" azero)
        ("" "  1, soft failure" aone))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "{\it b} ")
    (radioButtons bfail
        ("" "  1, print error messages" bone)
        ("" "  0, suppress error messages" bzero))
    (text . "\blankline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "{\it c} ")
    (radioButtons cfail
        ("" "  1, print warning messages" cone)
        ("" "  0, suppress warning messages" czero)))
  htMakeDoneButton('"Continue", 'd02rafSolve)
  htShowPage()

d02rafSolve htPage ==
  n :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'n)
    objValUnwrap htpLabelSpadValue(htPage, 'n)
  mnp :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'mnp)
    objValUnwrap htpLabelSpadValue(htPage, 'mnp)
  np :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'np)
    objValUnwrap htpLabelSpadValue(htPage, 'np)
  numbeg :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'numbeg)
    objValUnwrap htpLabelSpadValue(htPage, 'numbeg)
  nummix :=
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'nummix)
    objValUnwrap htpLabelSpadValue(htPage, 'nummix)
  tol := htpLabelInputString(htPage,'tol)
  mesh := htpButtonValue(htPage,'init)
  init :=
    mesh = 'init_zero => '0 
    '1
  iy := 
    $bcParseOnly => PARSE_-INTEGER htpLabelInputString(htPage, 'iy)
    objValUnwrap htpLabelSpadValue(htPage, 'iy)
  jacob := htpButtonValue(htPage,'ijac)
  ijac :=
    jacob = 'ijac_zero => '0
    '1
  deleps := htpLabelInputString(htPage,'deleps)
  lwork := mnp*(3*n*n + 6*n +2) +4*n*n + 3*n
  liwork :=
    ijac = 0 => mnp*(2*n +1) + n*n + 4*n +2
    mnp*(2*n +1) + n
  aerror := htpButtonValue(htPage,'afail)
  afail :=
    aerror = 'azero => '0
    '1
  berror := htpButtonValue(htPage,'bfail)
  bfail :=
    berror = 'bone => '1
    '0
  cerror := htpButtonValue(htPage,'cfail)
  cfail :=
    cerror = 'cone => '1
    '0
  ifail := 100*cfail + 10*bfail + afail
  (n = '3 and init = '0 and iy = '3 and nummix = '0 and numbeg = '2 and np = '17 and mnp = '40) => d02rafDefaultSolve(htPage,mnp,np,numbeg,nummix,tol,init,iy,ijac,deleps,lwork,liwork,ifail)
  init = '1 => d02rafCopOut()
  funcList := 
    "append"/[fa(i) for i in 1..n] where fa(i) ==
      prefix := ('"\newline {\em Function f")
      prefix := STRCONC(prefix,STRINGIMAGE i,'":} \space{1}")
      funct := STRCONC ('"Y[",STRINGIMAGE i ,"]")
      fnam := INTERN STRCONC ('"f",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[42, funct, fnam, 'EM]]]
  middle := ('"\blankline \menuitemstyle{} \tab{2} Enter the functions ")
  middle := STRCONC(middle,'"\htbitmap{gi} below ")
  middle := STRCONC(middle,'"as functions of YA[i] and YB[i]: \newline ")
  gList := 
    "append"/[fb(i) for i in 1..n] where fb(i) ==
      prefix := ('"\newline {\em Function g")
      prefix := STRCONC(prefix,STRINGIMAGE i,'":} \space{1}")
      fnc := STRCONC ('"YA[",STRINGIMAGE i ,"]")
      gnam := INTERN STRCONC ('"g",STRINGIMAGE i)
      [['text,:prefix],['bcStrings,[42, fnc, gnam, 'EM]]]
  gList :=  [['text,:middle],:gList]
  mid:= ('"\blankline \menuitemstyle{} \tab{2} Enter the array ")
  mid := STRCONC(mid,'"{\it x(mnp)}: \newline ")
  xList := 
    "append"/[fc(i) for i in 1..mnp] where fc(i) ==
        xnam := INTERN STRCONC ('"x",STRINGIMAGE i)
        [['bcStrings,[4, 0, xnam, 'F]]]
  xList :=  [['text,:mid],:xList]
  equationPart := [
     '(domainConditions 
        (isDomain EM $EmptyMode)
          (isDomain S (String))
            (isDomain F (Float))
              (isDomain I (Integer))),
                :funcList,:gList,:xList]
  page := htInitPage('"D02RAF - ODEs, general nonlinear boundary value problem, finite difference technique with deferred correction, continuation facility",nil)
  htSay '"\menuitemstyle{}\tab{2} "
  htSay '"Enter the functions \htbitmap{fi} (i.e. the derivatives) below "
  htSay '"as functions of Y[1]...Y[n]: "
  htSay '"\newline "
  htMakePage equationPart
  htMakeDoneButton('"Continue",'d02rafGen)
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'mnp,mnp)
  htpSetProperty(page,'np,np)
  htpSetProperty(page,'numbeg,numbeg)
  htpSetProperty(page,'nummix,nummix)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'init,init)
  htpSetProperty(page,'iy,iy)
  htpSetProperty(page,'ijac,ijac)
  htpSetProperty(page,'deleps,deleps)
  htpSetProperty(page,'lwork,lwork)
  htpSetProperty(page,'liwork,liwork)
  htpSetProperty(page,'ifail,ifail)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()


d02rafDefaultSolve(htPage,mnp,np,numbeg,nummix,tol,init,iy,ijac,deleps,lwork,liwork,ifail) ==
  n := '3
  page := htInitPage('"D02RAF - ODEs, general nonlinear boundary value problem, finite difference technique with deferred correction, continuation facility",nil)
  htMakePage '(
    (domainConditions 
       (isDomain EM $EmptyMode)
       (isDomain F (Float))
       (isDomain I (Integer)))
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the functions \htbitmap{fi} (i.e. the derivatives) below ")
    (text . "as functions of Y[1]...Y[n]: ")
    (text . "\newline {\em Function f1:} \space{1}")
    (bcStrings (44 "Y[2]" f1 EM))
    (text . "\newline {\em Function f2:} \space{1}")
    (bcStrings (44 "Y[3]" f2 EM))
    (text . "\newline {\em Function f3:} \space{1}")
    (bcStrings (44 "-Y[1]*Y[3] - 2*EPS*(1-Y[2]*Y[2])" f3 EM))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the functions \htbitmap{gi} below ")
    (text . "as functions of YA[i] and YB[i]: ")
    (text . "\newline {\em Function g1:} \space{1}")
    (bcStrings (44 "YA[1]" g1 EM))
    (text . "\newline {\em Function g2:} \space{1}")
    (bcStrings (44 "YA[2]" g2 EM))
    (text . "\newline {\em Function g3:} \space{1}")
    (bcStrings (44 "YB[2] -1" g3 EM))
    (text . "\blankline ")
    (text . "\newline ")
    (text . "\menuitemstyle{}\tab{2}")
    (text . "Enter the array {\it x(mnp)}: \newline ")
    (bcStrings (4 "0.0" x1 F))
    (bcStrings (4 "0.0" x2 F))
    (bcStrings (4 "0.0" x3 F))
    (bcStrings (4 "0.0" x4 F))
    (bcStrings (4 "0.0" x5 F))
    (bcStrings (4 "0.0" x6 F))
    (bcStrings (4 "0.0" x7 F))
    (bcStrings (4 "0.0" x8 F))
    (bcStrings (4 "0.0" x9 F))
    (bcStrings (4 "0.0" x10 F))
    (bcStrings (4 "0.0" x11 F))
    (bcStrings (4 "0.0" x12 F))
    (bcStrings (4 "0.0" x13 F))
    (bcStrings (4 "0.0" x14 F))
    (bcStrings (4 "0.0" x15 F))
    (bcStrings (4 "0.0" x16 F))
    (bcStrings (4 "10.0" x17 F))
    (bcStrings (4 "0.0" x18 F))
    (bcStrings (4 "0.0" x19 F))
    (bcStrings (4 "0.0" x20 F))
    (bcStrings (4 "0.0" x21 F))
    (bcStrings (4 "0.0" x22 F))
    (bcStrings (4 "0.0" x23 F))
    (bcStrings (4 "0.0" x24 F))
    (bcStrings (4 "0.0" x25 F))
    (bcStrings (4 "0.0" x26 F))
    (bcStrings (4 "0.0" x27 F))
    (bcStrings (4 "0.0" x28 F))
    (bcStrings (4 "0.0" x29 F))
    (bcStrings (4 "0.0" x30 F))
    (bcStrings (4 "0.0" x31 F))
    (bcStrings (4 "0.0" x32 F))
    (bcStrings (4 "0.0" x33 F))
    (bcStrings (4 "0.0" x34 F))
    (bcStrings (4 "0.0" x35 F))
    (bcStrings (4 "0.0" x36 F))
    (bcStrings (4 "0.0" x37 F))
    (bcStrings (4 "0.0" x38 F))
    (bcStrings (4 "0.0" x39 F))
    (bcStrings (4 "0.0" x40 F)))
  htpSetProperty(page,'n,n)
  htpSetProperty(page,'mnp,mnp)
  htpSetProperty(page,'np,np)
  htpSetProperty(page,'numbeg,numbeg)
  htpSetProperty(page,'nummix,nummix)
  htpSetProperty(page,'tol,tol)
  htpSetProperty(page,'init,init)
  htpSetProperty(page,'iy,iy)
  htpSetProperty(page,'ijac,ijac)
  htpSetProperty(page,'deleps,deleps)
  htpSetProperty(page,'lwork,lwork)
  htpSetProperty(page,'liwork,liwork)
  htpSetProperty(page,'ifail,ifail)
  htMakeDoneButton('"Continue",'d02rafGen)
  htpSetProperty(page,'inputArea, htpInputAreaAlist htPage)
  htShowPage()

d02rafGen htPage ==
  n := htpProperty(htPage, 'n)
  mnp := htpProperty(htPage, 'mnp)
  np := htpProperty(htPage, 'np)
  numbeg := htpProperty(htPage, 'numbeg)
  nummix := htpProperty(htPage, 'nummix)
  tol := htpProperty(htPage, 'tol)
  init := htpProperty(htPage, 'init)
  iy := htpProperty(htPage, 'iy)
  ijac := htpProperty(htPage, 'ijac)
  deleps := htpProperty(htPage, 'deleps)
  lwork := htpProperty(htPage, 'lwork)
  liwork := htpProperty(htPage, 'liwork)
  ifail := htpProperty(htPage, 'ifail)
  alist := htpInputAreaAlist htPage
  y := alist
  for i in 1..mnp repeat
    xtemp := STRCONC((first y).1," ")
    xList := [xtemp,:xList]
    y := rest y
  xstring := bcwords2liststring xList
  for i in 1..n repeat
    gtemp := STRCONC((first y).1," ")
    gList := [gtemp,:gList]
    y := rest y
  gstring := bcwords2liststring gList
  while y repeat
    f := STRCONC((first y).1," ")
    fList := [f,:fList]
    y := rest y
  fstring := bcwords2liststring fList
  prefix := STRCONC("d02raf(",STRINGIMAGE n,", ",STRINGIMAGE mnp,", ")
  prefix := STRCONC(prefix,STRINGIMAGE numbeg,", ",STRINGIMAGE nummix,", ")
  prefix := STRCONC(prefix,tol,", ",STRINGIMAGE init,", ",STRINGIMAGE iy,", ")
  middle:= STRCONC(STRINGIMAGE ijac,", ",STRINGIMAGE lwork,", ")
  middle := STRCONC(middle,STRINGIMAGE liwork,", ",STRINGIMAGE np,", [")
  middle := STRCONC(middle,xstring,"],[[0.0 for i in 1..", STRINGIMAGE mnp)
  middle := STRCONC(middle,"] for j in 1..",STRINGIMAGE iy,"]")
  middle := STRCONC(middle,":: Matrix DoubleFloat,",STRINGIMAGE deleps,", ")
  middle := STRCONC(middle,STRINGIMAGE ifail,", (",fstring,"::Vector ")
  middle := STRCONC(middle,"Expression Float)::ASP41('FCN,'JACOBF,'JACEPS),(")
  middle := STRCONC(middle,gstring,"::Vector Expression Float)::ASP42('G,'JACOBG,")
  middle := STRCONC(middle,"'JACGEP))")
  linkGen STRCONC(prefix,middle)
  

d02rafCopOut() ==
  htInitPage('"D02RAF - ODEs, general nonlinear boundary value problem, finite difference technique with deferred correction, continuation facility",nil)
  htMakePage '(
    (domainConditions 
       (isDomain PI (PositiveInteger)))
    (text . "\blankline ")
    (text . "{\center{\em Hyperdoc interface not available for initial mesh}}")
    (text . "\newline ")
    (text . "{\center{\em Please use the command line.}}"))
  htMakeDoneButton('"Continue",'d02raf)
  htShowPage()
