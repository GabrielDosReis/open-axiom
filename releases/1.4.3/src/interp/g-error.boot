-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
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


import diagnostics
import g_-util
namespace BOOT

-- This file contains the error printing code used in BOOT and SPAD.
-- While SPAD only calls "error" (which is then labeled as an algebra
-- error, BOOT calls "userError" and "systemError" when a problem is
-- found.
--
-- The variable $BreakMode is set using the system command )set breakmode
-- and can have one of the values:
--   break    -- always enter a lisp break when an error is signalled
--   nobreak  -- do not enter lisp break mode
--   query    -- ask the user if break mode should be entered

$SystemError == 'SystemError
$UserError == 'UserError
$AlgebraError =='AlgebraError

$ReadingFile := false


-- REDERR is used in BFLOAT LISP, should be a macro
-- REDERR msg == error msg

-- BFLERRMSG func ==
--  errorSupervisor($AlgebraError,strconc(
--    '"BigFloat: invalid argument to ",func))

argumentDataError(argnum, condit, funname) ==
  msg := ['"The test",:bright pred2English condit,'"evaluates to",
    :bright '"false",'"%l",'"   for argument",:bright argnum,_
    '"to the function",:bright funname,'"and this indicates",'"%l",_
    '"   that the argument is not appropriate."]
  errorSupervisor($AlgebraError,msg)

queryUser msg ==
  -- display message and return reply
  sayBrightly msg
  line := readLine $InputStream
  line ~= %nothing => line
  nil

-- errorSupervisor is the old style error message trapper

errorSupervisor(errorType,errorMsg) ==
  errorSupervisor1(errorType,errorMsg,$BreakMode)

needsToSplitMessage msg ==
  member("%b",msg) or member('"%b",msg) => false
  member("%d",msg) or member('"%d",msg) => false
  member("%l",msg) or member('"%l",msg) => false
  true

errorSupervisor1(errorType,errorMsg,$BreakMode) ==
  BUMPERRORCOUNT "semantic"
  errorLabel :=
      errorType = $SystemError  => '"System error"
      errorType = $UserError    => '"Apparent user error"
      errorType = $AlgebraError =>
        '"Error detected within library code"
      string? errorType         => errorType
      '"Error with unknown classification"
  msg :=
    errorMsg is ['mathprint, :.] => errorMsg
    errorMsg isnt [.,:.] => ['"   ", errorMsg]
    needsToSplitMessage errorMsg => rest [:['"%l",'"   ",u] for u in errorMsg]
    ['"   ",:errorMsg]
  sayErrorly(errorLabel, msg)
  handleLispBreakLoop($BreakMode)

handleLispBreakLoop($BreakMode) ==
  finishLine $OutputStream
  $BreakMode = 'break =>
    sayBrightly '" "
    BREAK()
  $BreakMode = 'query =>
    gotIt := nil
    while not gotIt repeat
      gotIt := true
      msgQ := 
       ['"%l",'"   You have three options. Enter:",'"%l",_
        '"    ",:bright '"continue",'"  to continue processing,",'"%l",_
        '"    ",:bright '"top     ",'"  to return to top level, or",'"%l",_
        '"    ",:bright '"break   ",'"  to enter a LISP break loop.",'"%l",_
        '"%l",'"   Please enter your choice now:"]
      x := STRING2ID_-N(queryUser msgQ,1)
      x := selectOptionLC(x,'(top break continue),nil)
      null x =>
        sayBrightly bright '"  That was not one of your choices!"
        gotIt := nil
      x = 'top => returnToTopLevel()
      x = 'break =>
        $BreakMode := 'break
	sayBrightly ['"   Enter",:bright '":C",
	  '"when you are ready to continue processing where you ",'"%l",_
	  '"   interrupted the system, enter",:bright '"(TOP)",_
	  '"when you wish to return",'"%l",'"   to top level.",'"%l",'"%l"]
        BREAK()
      sayBrightly
        '"   Processing will continue where it was interrupted."
      THROW($SpadReaderTag, nil)
  $BreakMode = 'resume or $ReadingFile =>
    returnToReader()
  returnToTopLevel()

TOP() == returnToTopLevel()

returnToTopLevel() ==
  THROW($intTopLevel,'restart)

returnToReader() ==
  not $ReadingFile => returnToTopLevel()
  sayBrightly ['"   Continuing to read the file...", '"%l"]
  THROW($SpadReaderTag, nil)

sayErrorly(errorLabel, msg) ==
  sayBrightly '" "
  if $testingSystem then sayMSG $testingErrorPrefix
  sayBrightly ['"   >> ",errorLabel,'":"]
  m := msg
  msg is ['mathprint, mathexpr] =>
    mathprint mathexpr
  sayBrightly msg

-- systemError is being phased out. Please use keyedSystemError.
systemError(:x) == 
  errorSupervisor($SystemError,IFCAR x)

-- unexpectedSystemError() ==
--  systemError '"Oh, no.  Unexpected internal error."

userError x == 
  errorSupervisor($UserError,x)

error(x) == 
  errorSupervisor($AlgebraError,x)

IdentityError(op) ==
    error(["No identity element for reduce of empty list using operation",op])

throwMessage(:msg) ==
  if $compilingMap then clearCache $mapName
  msg' := mkMessage concatList msg
  sayMSG msg'
  if $printMsgsToFile then sayMSG2File msg'
  spadThrow()


++ Error handler for Lisp systems that support Common Lisp conditions.
++ We don't want users to get dropped into the Lisp debugger.
systemErrorHandler c ==
  $NeedToSignalSessionManager := true
  $BreakMode = "validate" => 
    systemError ERROR_-FORMAT('"~a",[c])
  not $inLispVM and $BreakMode in '(nobreak query resume) =>
    TYPEP(c,'CONTROL_-ERROR) => keyedSystemError('S2GE0020,nil)
    LET(($inLispVM true)(), systemError ERROR_-FORMAT('"~a",[c]))
  $BreakMode = "letPrint2" =>
    $BreakMode := nil
    THROW("letPrint2",nil)

