-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2012, Gabriel Dos Reis.
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


-- HyperTex Spad interface

-- Issue a line of HyperTex
issueHT line ==
  sockSendInt($MenuServer, $SendLine)
  sockSendString($MenuServer, line)

testPage() ==
  startHTPage(50)
  issueHT '"\page{TestPage}{Test Page generated from Lisp} "
  issueHT '"\horizontalline\beginscroll\beginitems "
  issueHT '"\item \downlink{Quayle Jokes}{ChickenPage} \space{2} "
  issueHT '"The misadventures of the White House bellboy. "
  issueHT '"\enditems\endscroll\autobuttons "
  endHTPage()

-- create a named top-page
HTLinkToPage name ==
  sockSendInt($MenuServer, $PageStuff)
  sockSendInt($MenuServer, $currentFrameNum)
  sockSendInt($MenuServer, $LinkToPage)
  sockSendString($MenuServer, name)

-- create a pop-up named page ; returns a windowid
HTPopUpNamedPage(name,cols) ==
  sockSendInt($MenuServer, $PageStuff)
  sockSendInt($MenuServer, $currentFrameNum)
  sockSendInt($MenuServer, $PopUpNamedPage)
  sockSendInt($MenuServer, cols)
  sockSendString($MenuServer, name)
  sockGetInt($MenuServer)

-- Update a window with named page
HTReplaceNamedPage(window, name) ==
  sockSendInt($MenuServer, $PageStuff)
  sockSendInt($MenuServer, $currentFrameNum)
  sockSendInt($MenuServer, $ReplaceNamedPage)
  sockSendInt($MenuServer, window)
  sockSendString($MenuServer, name)
   
-- Start a pop-up page ; returns a windowid
HTPopUpPage cols ==
  sockSendInt($MenuServer, $PageStuff)
  sockSendInt($MenuServer, $currentFrameNum)
  sockSendInt($MenuServer, $PopUpPage)
  sockSendInt($MenuServer, cols)
  sockGetInt($MenuServer)

-- Start an update sequence on a window
HTReplacePage w ==
  sockSendInt($MenuServer, $PageStuff)
  sockSendInt($MenuServer, $currentFrameNum)
  sockSendInt($MenuServer, $ReplacePage)
  sockSendInt($MenuServer, w)

-- Start a top-page ; no further Lisp interaction
HTStartPage cols ==
  sockSendInt($MenuServer, $PageStuff)
  sockSendInt($MenuServer, $currentFrameNum)
  sockSendInt($MenuServer, $StartPage)
  sockSendInt($MenuServer, cols)

-- Kill a window from Lisp
HTKillPage w ==
  sockSendInt($MenuServer, $PageStuff)
  sockSendInt($MenuServer, $currentFrameNum)
  sockSendInt($MenuServer, $KillPage)
  sockSendInt($MenuServer, w)
   
HTErrorSignal() ==
  sockSendInt($MenuServer, $SpadError)
