-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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


import ht_-util
import c_-util
namespace BOOT

++
$newConstructorList := nil

++ true if we should rebuild local databases.
$createLocalLibDb := true



bcFinish(name,arg,:args) == bcGen bcMkFunction(name,arg,args)

bcMkFunction(name,arg,args) ==
  args := [x for x in args | x]
  strconc(name,'"(",arg,strconc/[strconc('",", x) for x in args],'")")

bcString2HyString2 s ==
  string? s and stringChar(s,0) = char "_""  =>
    len := #s
    strconc('"\_"", subString(s, 1, len-2), '"\_"")
  s

bcString2HyString s == s

bcFindString(s,i,n,char) ==  or/[j for j in i..n | s.j = char]

bcGen command ==
  htInitPage('"Basic Command",nil)
  string :=
    #command < 50 => strconc('"{\centerline{\tt ",command,'" }}")
    strconc('"{\tt ",command,'" }")
  htMakePage [
     '(text
        "{Here is the AXIOM command you could have issued to compute this result:}"
            "\vspace{2}\newline "),
      ['text,:string]]
  htMakeDoitButton('"Do It", command)
  htShowPage()

-- bcGen for axiom - nag link
linkGen command ==
  htInitPage('"AXIOM-Nag Link Command",nil)
  string := 
    #command < 50 => strconc('"{\centerline{ ",command,'" }}")
    command
  htMakePage [
     '(text 
        "\centerline{{\em Here is the AXIOM command}}"
          "\centerline{{\em you could have issued to compute this result:}}"
            "\vspace{2}\newline "),
      ['text,:string]]
  htMakeDoitButton('"Do It", command)
  htShowPage()

bcOptional s ==
  s = '"" => '"2"
  s

bcvspace() == bcHt '"\vspace{1}\newline "

bcString2WordList s == fn(s,0,maxIndex s) where
  fn(s,i,n) ==
    i > n => nil
    k := or/[j for j in i..n | s.j ~= char " "]
    not integer? k => nil
    l := bcFindString(s,k + 1,n,char " ")
    not integer? l => [subString(s,k)]
    [subString(s,k,l-k),:fn(s,l + 1,n)]


bcwords2liststring u ==
  null u => nil
  strconc('"[",first u,fn rest u) where
    fn(u) ==
      null u => '"]"
      strconc('", ",first u,fn rest u)

bcVectorGen vec == bcwords2liststring vec

bcError string ==
  sayBrightlyNT '"NOTE: "
  sayBrightly string

bcDrawIt(ind,a,b) == strconc(ind,'"=",a,'"..",b)

bcNotReady htPage ==
  htInitPage('"Basic Command",nil)
  htMakePage '(
     (text .
        "{\centerline{\em This facility will soon be available}}"))
  htShowPage()

htStringPad(n,w) ==
  s := toString n
  ws := #s
  strconc('"\space{",toString (w - ws + 1),'"}",s)

stringList2String x ==
  null x => '"()"
  strconc('"(",first x,strconc/[strconc('",",y) for y in rest x],'")")

htMkName(s,n) == strconc(s,toString n)

