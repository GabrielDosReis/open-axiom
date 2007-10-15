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

-- READ THIS NOW!
--
-- The automatic make fails to compile this file properly, leaving a 
-- truncated clisp file in int/interp.  So if you change this file it
-- must be compiled by hand in the interpreter (which works fine).
-- MCD.
--

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



