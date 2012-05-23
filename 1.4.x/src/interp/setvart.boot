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


import sys_-macros
namespace BOOT

--% Table of )set options
$setOptions := '(
-- -------------------- The breakmode Option ---------------------
--
--  Description: execute break processing on error
--
--  The breakmode option may be followed by any one of the 
--  following:
--
--     nobreak
--  -> break 
--     query
--     resume
--     fastlinks
--
--  The current setting is indicated within the list.
  (breakmode
   "execute break processing on error"
   interpreter
   LITERALS
   $BreakMode
   (nobreak break query resume fastlinks)
   nobreak)         -- needed to avoid possible startup looping

-- ---------------------- The expose Option ----------------------
--
--  Description: control interpreter constructor exposure
--
--    The following groups are explicitly exposed in the current 
--    frame (called initial ):
--                                    basic                                   
--                                 categories                                 
-- 
--    The following constructors are explicitly exposed in the 
--    current frame:
--                there are no explicitly exposed constructors                
-- 
--    The following constructors are explicitly hidden in the 
--    current frame:
--                 there are no explicitly hidden constructors                
-- 
--    When )set expose is followed by no arguments, the information
--    you now see is displayed. When followed by the initialize 
--    argument, the exposure group data in the file INTERP.EXPOSED 
--    is read and is then available. The arguments add and drop are 
--    used to add or drop exposure groups or explicit constructors 
--    from the local frame exposure data. Issue
--                   )set expose add    or    )set expose drop 
--    for more information.
  (expose
   "control interpreter constructor exposure"
   interpreter
   FUNCTION
   setExpose
   NIL
   htSetExpose)

--              Current Values of  functions  Variables                  
--
-- Variable     Description                           Current Value
-- -----------------------------------------------------------------
-- cache        number of function results to cache        0 
-- compile      compile, don't just define function bodies off 
-- recurrence   specially compile recurrence relations     on 
  (functions
   "some interpreter function options"
   interpreter
   TREE
   novar

-- ---------------------- The cache Option -----------------------
--
--  Description: number of function results to cache
--
--  )set functions cache is used to tell AXIOM how many  values 
--   computed by interpreter functions should be saved.  This can 
--   save quite a bit of time in recursive functions, though one 
--   must consider that the cached values will take up (perhaps 
--   valuable) room in the workspace.
--
--  The value given  after cache must either be the word all or 
--  a positive integer. This may be followed by any number of 
--  function names whose cache sizes you wish to so set.  If no 
--  functions are given, the default cache  size is set.
--
--  Examples:   )set fun cache all
--              )set fun cache 10 f g Legendre
--
--  In general, functions will cache no returned values.
   ((cache
     "number of function results to cache"
     interpreter
     FUNCTION
     setFunctionsCache
     NIL
     htSetCache)

-- --------------------- The compile Option ----------------------
--
--  Description: compile, don't just define function bodies
--
--  The compile option may be followed by any one of the following:
--
--  -> on
--     off 
--
--  The current setting is indicated within the list.
    (compile
     "compile, don't just define function bodies"
     interpreter
     LITERALS
     $compileDontDefineFunctions
     (on off)
     on)

-- -------------------- The recurrence Option --------------------
--
--  Description: specially compile recurrence relations
--
--  The recurrence option may be followed by any one of the 
--  following:
--
--  -> on 
--     off
--
--  The current setting is indicated within the list.
    (recurrence
     "specially compile recurrence relations"
     interpreter
     LITERALS
     $compileRecurrence
     (on off)
     on)))

--               Current Values of  fortran  Variables                   
--
-- Variable     Description                           Current Value
-- -----------------------------------------------------------------
-- ints2floats  where sensible, coerce integers to reals   on 
-- fortindent   the number of characters indented          6 
-- fortlength   the number of characters on a line         72 
-- typedecs     print type and dimension lines             on 
-- defaulttype  default generic type for FORTRAN object    REAL 
-- precision    precision of generated FORTRAN objects     double 
-- intrinsic    whether to use INTRINSIC FORTRAN functions off 
-- explength    character limit for FORTRAN expressions    1320 
-- segment      split long FORTRAN expressions             on 
-- optlevel     FORTRAN optimisation level                 0 
-- startindex   starting index for FORTRAN arrays          1 
-- calling      options for external FORTRAN calls         ... 
--
-- Variables with current values of ... have further sub-options. 
-- For example, issue )set calling to see what the options are for 
-- calling.
-- For more information, issue )help set .
  (fortran
   "view and set options for FORTRAN output"
   interpreter
   TREE
   novar

-- ------------------- The ints2floats Option --------------------
--
--  Description: where sensible, coerce integers to reals
--
--  The ints2floats option may be followed by any one of the 
--  following:
--
--  -> on 
--     off
--
--  The current setting is indicated within the list.
   ((ints2floats
     "where sensible, coerce integers to reals"
     interpreter
     LITERALS
     $fortInts2Floats
     (on off)
     on)

-- -------------------- The fortindent Option --------------------
--
--  Description: the number of characters indented
--
--  The fortindent option may be followed by an integer in the range 
--  0 to inclusive. The current setting is 6 
    (fortindent
     "the number of characters indented"
     interpreter
     INTEGER
     $fortIndent
     (0 NIL)
     6)

-- -------------------- The fortlength Option --------------------
--
--  Description: the number of characters on a line
--
--  The fortlength option may be followed by an integer in the range 
--  1 to inclusive. The current setting is 72 
    (fortlength
     "the number of characters on a line"
     interpreter
     INTEGER
     $fortLength
     (1 NIL)
     72)

-- --------------------- The typedecs Option ---------------------
--
--  Description: print type and dimension lines
--
--  The typedecs option may be followed by any one of the 
--  following:
--
--  -> on 
--     off
--
--  The current setting is indicated within the list.
    (typedecs
     "print type and dimension lines"
     interpreter
     LITERALS
     $printFortranDecs
     (on off)
     on)

-- ------------------- The defaulttype Option --------------------
--
--  Description: default generic type for FORTRAN object
--
--  The defaulttype option may be followed by any one of the 
--  following:
--
--  -> REAL 
--     INTEGER
--     COMPLEX
--     LOGICAL
--     CHARACTER
--
--  The current setting is indicated within the list.
    (defaulttype
     "default generic type for FORTRAN object"
     interpreter
     LITERALS
     $defaultFortranType
     (REAL INTEGER COMPLEX LOGICAL CHARACTER)
     REAL)

-- -------------------- The precision Option ---------------------
--
--  Description: precision of generated FORTRAN objects
--
--  The precision option may be followed by any one of the 
--  following:
--
--     single
--  -> double 
--
--  The current setting is indicated within the list.
    (precision
    "precision of generated FORTRAN objects"
     interpreter
     LITERALS
     $fortranPrecision
     (single double)
     double)

-- -------------------- The intrinsic Option ---------------------
--
--  Description: whether to use INTRINSIC FORTRAN functions
--
--  The intrinsic option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (intrinsic
     "whether to use INTRINSIC FORTRAN functions"
     interpreter
     LITERALS
     $useIntrinsicFunctions
     (on off)
     off)

-- -------------------- The explength Option ---------------------
--
--  Description: character limit for FORTRAN expressions
--
--  The explength option may be followed by an integer in the range 
--  0 to inclusive. The current setting is 1320 
    (explength
     "character limit for FORTRAN expressions"
     interpreter
     INTEGER
     $maximumFortranExpressionLength
     (0 NIL)
     1320)

-- --------------------- The segment Option ----------------------
--
--  Description: split long FORTRAN expressions
--
--  The segment option may be followed by any one of the following:
--
--  -> on 
--     off
--
--  The current setting is indicated within the list.
    (segment
     "split long FORTRAN expressions"
     interpreter
     LITERALS
     $fortranSegment
     (on off)
     on)

-- --------------------- The optlevel Option ---------------------
--
--  Description: FORTRAN optimisation level
--
--  The optlevel option may be followed by an integer in the range 
--  0 to 2 inclusive. The current setting is 0 
    (optlevel
     "FORTRAN optimisation level"
     interpreter
     INTEGER
     $fortranOptimizationLevel
     (0 2)
     0)

-- -------------------- The startindex Option --------------------
--
--  Description: starting index for FORTRAN arrays
--
--  The startindex option may be followed by an integer in the range 
--  0 to 1 inclusive. The current setting is 1 
    (startindex
     "starting index for FORTRAN arrays"
     interpreter
     INTEGER
     $fortranArrayStartingIndex
     (0 1)
     1)

--               Current Values of  calling  Variables                   
--
-- Variable     Description                           Current Value
-- -----------------------------------------------------------------
-- tempfile     set location of temporary data files       /tmp/ 
-- directory    set location of generated FORTRAN files    ./ 
-- linker       linker arguments (e.g. libraries to search) -lxlf 
    (calling
    "options for external FORTRAN calls"
    interpreter
    TREE
    novar

-- --------------------- The tempfile Option ---------------------
--
--  Description: set location of temporary data files
--
--  )set fortran calling tempfile  is used to tell AXIOM where
--  to place intermediate FORTRAN data files . This must be the 
--  name of a valid existing directory to which you have permission 
--  to write (including the final slash).
--
--  Syntax:
--    )set fortran calling tempfile DIRECTORYNAME
--
--  The current setting is /tmp/ 
    ((tempfile
      "set location of temporary data files"
      interpreter
      FUNCTION
      setFortTmpDir
      (("enter directory name for which you have write-permission"
        DIRECTORY
        $fortranTmpDir
        chkDirectory
        "/tmp/"))
      NIL)

-- -------------------- The directory Option ---------------------
--
--  Description: set location of generated FORTRAN files
--
--  )set fortran calling directory  is used to tell AXIOM where
--  to place generated FORTRAN files. This must be the name 
--  of a valid existing directory to which you have permission 
--  to write (including the final slash).
--
--  Syntax:
--    )set fortran calling directory DIRECTORYNAME
--
--  The current setting is ./ 
     (directory
      "set location of generated FORTRAN files"
      interpreter
      FUNCTION
      setFortDir
      (("enter directory name for which you have write-permission"
	DIRECTORY
	$fortranDirectory
	chkDirectory
	"./"))
      NIL)

-- ---------------------- The linker Option ----------------------
--
--  Description: linker arguments (e.g. libraries to search)
--
--  )set fortran calling linkerargs is used to pass arguments to 
--  the linker when using  mkFort  to create functions which call 
--  Fortran code. For example, it might give a list of libraries 
--  to be searched, and their locations.
--  The string is passed verbatim, so must be the correct syntax for
--  the particular linker being used.
--
--  Example: )set fortran calling linker "-lxlf"
--
--  The current setting is -lxlf 
     (linker
      "linker arguments (e.g. libraries to search)"
      interpreter
      FUNCTION
      setLinkerArgs
      (("enter linker arguments "
	STRING
	$fortranLibraries
	chkDirectory
	"-lxlf"))
      NIL)
     ))
   ))

--               Current Values of  kernel  Variables                    
--
-- Variable     Description                           Current Value
-- -----------------------------------------------------------------
-- warn         warn when re-definition is attempted       off 
-- protect      prevent re-definition of kernel functions  off 
  (kernel
   "library functions built into the kernel for efficiency"
   interpreter
   TREE
   novar

-- ----------------------- The warn Option -----------------------
--
--  Description: warn when re-definition is attempted
--
-- Some AXIOM library functions are compiled into the kernel for 
-- efficiency reasons. To prevent them being re-defined when loaded 
-- from a library they are specially protected. If a user wishes to 
-- know when an attempt is made to re-define such a function, he or 
-- she should issue the command:
--         )set kernel warn on
-- To restore the default behaviour, he or she should issue the 
-- command:
--         )set kernel warn off
   ((warn
     "warn when re-definition is attempted"
     interpreter
     FUNCTION
     protectedSymbolsWarning
     NIL
     htSetKernelWarn)
-- --------------------- The protect Option ----------------------
--
--  Description: prevent re-definition of kernel functions
--
-- Some AXIOM library functions are compiled into the kernel for 
-- efficiency reasons. To prevent them being re-defined when loaded
-- from a library they are specially protected.  If a user wishes 
-- to re-define these functions, he or she should issue the command:
--         )set kernel protect off
-- To restore the default behaviour, he or she should issue the 
-- command:
--         )set kernel protect on
    (protect
     "prevent re-definition of kernel functions"
     interpreter
     FUNCTION
     protectSymbols
     NIL
     htSetKernelProtect)
   ))

--              Current Values of  hyperdoc  Variables                   
--
-- Variable     Description                           Current Value
-- -----------------------------------------------------------------
-- fullscreen   use full screen for this facility          off 
-- mathwidth    screen width for history output            120 
  (hyperdoc
   "options in using HyperDoc"
   interpreter
   TREE
   novar
-- -------------------- The fullscreen Option --------------------
--
--  Description: use full screen for this facility
--
--  The fullscreen option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
   ((fullscreen
     "use full screen for this facility"
     interpreter
     LITERALS
     $fullScreenSysVars
     (on off)
     off)
-- -------------------- The mathwidth Option ---------------------
--
--  Description: screen width for history output
--
--  The mathwidth option may be followed by an integer in the range 
--  0 to inclusive. The current setting is 120 
    (mathwidth
     "screen width for history output"
     interpreter
     INTEGER
     $historyDisplayWidth
     (0 NIL)
     120)
   ))

--                Current Values of  help  Variables                     
--
-- Variable     Description                           Current Value
-- -----------------------------------------------------------------
-- fullscreen   use fullscreen facility, if possible       off 
  (help
   "view and set some help options"
   interpreter
   TREE
   novar
-- -------------------- The fullscreen Option --------------------
--
--  Description: use fullscreen facility, if possible
--
--  The fullscreen option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
   ((fullscreen
    "use fullscreen facility, if possible"
    interpreter
    LITERALS
    $useFullScreenHelp
    (on off)
    off)
   ))

-- --------------------- The history Option ----------------------
--
--  Description: save workspace values in a history file
--
--  The history option may be followed by any one of the 
--  following:
--
--  -> on 
--     off
--
--  The current setting is indicated within the list.
  (history
   "save workspace values in a history file"
   interpreter
   LITERALS
   $HiFiAccess
   (on off)
   on)

--              Current Values of  messages  Variables                   
--
-- Variable     Description                           Current Value
-- -----------------------------------------------------------------
-- autoload     print file auto-load messages              off 
-- bottomup     display bottom up modemap selection        off 
-- coercion     display datatype coercion messages         off 
-- dropmap      display old map defn when replaced         off 
-- expose       warning for unexposed functions            off 
-- file         print msgs also to SPADMSG LISTING         off 
-- frame        display messages about frames              off 
-- highlighting use highlighting in system messages        off 
-- instant      present instantiation summary              off 
-- insteach     present instantiation info                 off 
-- interponly   say when function code is interpreted      on 
-- number       display message number with message        off 
-- prompt       set type of input prompt to display        step 
-- selection    display function selection msgs            off 
-- set          show )set setting after assignment         off 
-- startup      display messages on start-up               off 
-- summary      print statistics after computation         off 
-- testing      print system testing header                off 
-- time         print timings after computation            off 
-- type         print type after computation               on 
-- void         print Void value when it occurs            off 
-- any          print the internal type of objects of domain Any on 
  (messages
   "show messages for various system features"
   interpreter
   TREE
   novar

-- ----------------------- The any Option ------------------------
--
--  Description: print the internal type of objects of domain Any
--
--  The any option may be followed by any one of the following:
--
--  -> on 
--     off
--
--  The current setting is indicated within the list.
   ((any
     "print the internal type of objects of domain Any"
     interpreter
     LITERALS
     $printAnyIfTrue
     (on off)
     on)
-- --------------------- The autoload Option ---------------------
--
--  Description: print file auto-load messages
    (autoload
     "print file auto-load messages"
     interpreter
     FUNCTION
     printLoadMessages
     (on off)
     on)

-- --------------------- The bottomup Option ---------------------
--
--  Description: display bottom up modemap selection
--
--  The bottomup option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (bottomup
     "display bottom up modemap selection"
     development
     LITERALS
     $reportBottomUpFlag
     (on off)
     off)

-- --------------------- The coercion Option ---------------------
--
--  Description: display datatype coercion messages
--
--  The coercion option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (coercion
     "display datatype coercion messages"
     development
     LITERALS
     $reportCoerceIfTrue
     (on off)
     off)

-- --------------------- The dropmap Option ----------------------
--
--  Description: display old map defn when replaced
--
--  The dropmap option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (dropmap
     "display old map defn when replaced"
     interpreter
     LITERALS
     $displayDroppedMap
     (on off)
     off)

-- ---------------------- The expose Option ----------------------
--
--  Description: warning for unexposed functions
--
--  The expose option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (expose
     "warning for unexposed functions"
     interpreter
     LITERALS
     $giveExposureWarning
     (on off)
     off)

-- ----------------------- The file Option -----------------------
--
--  Description: print msgs also to SPADMSG LISTING
--
--  The file option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (file
     "print msgs also to SPADMSG LISTING"
     development
     LITERALS
     $printMsgsToFile
     (on off)
     off)

-- ---------------------- The frame Option -----------------------
--
--  Description: display messages about frames
--
--  The frame option may be followed by any one of the following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (frame
     "display messages about frames"
     interpreter
     LITERALS
     $frameMessages
     (on off)
     off)

-- ------------------- The highlighting Option -------------------
--
--  Description: use highlighting in system messages
--
--  The highlighting option may be followed by any one of the 
--  following:
--
--  -> on
--     off 
--
--  The current setting is indicated within the list.
    (highlighting
     "use highlighting in system messages"
     interpreter
     LITERALS
     $highlightAllowed
     (on off)
     on)

-- --------------------- The instant Option ----------------------
--
--  Description: present instantiation summary
--
--  The instant option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (instant
     "present instantiation summary"
     development
     LITERALS
     $reportInstantiations
     (on off)
     off)

-- --------------------- The insteach Option ---------------------
--
--  Description: present instantiation info
--
--  The insteach option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (insteach
     "present instantiation info"
     development
     LITERALS
     $reportEachInstantiation
     (on off)
     off)

-- -------------------- The interponly Option --------------------
--
--  Description: say when function code is interpreted
--
--  The interponly option may be followed by any one of the 
--  following:
--
--  -> on 
--     off
--
--  The current setting is indicated within the list.
    (interponly
     "say when function code is interpreted"
     interpreter
     LITERALS
     $reportInterpOnly
     (on off)
     on)

-- ---------------------- The number Option ----------------------
--
--  Description: display message number with message
--
--  The number option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (number
     "display message number with message"
     interpreter
     LITERALS
     $displayMsgNumber
     (on off)
     off)

-- ---------------------- The prompt Option ----------------------
--
--  Description: set type of input prompt to display
--
--  The prompt option may be followed by any one of the following:
--
--     none
--     frame
--     plain
--  -> step 
--     verbose
--
--  The current setting is indicated within the list.
    (prompt
     "set type of input prompt to display"
     interpreter
     LITERALS
     $inputPromptType
     (none frame plain step verbose)
     step)

-- -------------------- The selection Option ---------------------
--
--  Description: display function selection msgs
--
--  The selection option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (selection
     "display function selection msgs"
     interpreter
     LITERALS
     $reportBottomUpFlag
     (on off)
     off)

-- ----------------------- The set Option ------------------------
--
--  Description: show )set setting after assignment
--
--  The set option may be followed by any one of the following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (set
     "show )set setting after assignment"
     interpreter
     LITERALS
     $displaySetValue
     (on off)
     off)

-- --------------------- The startup Option ----------------------
--
--  Description: display messages on start-up
--
--  The startup option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (startup
     "display messages on start-up"
     interpreter
     LITERALS
     $displayStartMsgs
     (on off)
     on)

-- --------------------- The summary Option ----------------------
--
--  Description: print statistics after computation
--
--  The summary option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (summary
     "print statistics after computation"
     interpreter
     LITERALS
     $printStatisticsSummaryIfTrue
     (on off)
     off)

-- --------------------- The testing Option ----------------------
--
--  Description: print system testing header
--
--  The testing option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (testing
     "print system testing header"
     development
     LITERALS
     $testingSystem
     (on off)
     off)

-- ----------------------- The time Option -----------------------
--
--  Description: print timings after computation
--
--  The time option may be followed by any one of the following:
--
--     on
--  -> off 
--     long
--
--  The current setting is indicated within the list.
    (time
     "print timings after computation"
     interpreter
     LITERALS
     $printTimeIfTrue
     (on off long)
     off)

-- ----------------------- The type Option -----------------------
--
--  Description: print type after computation
--
--  The type option may be followed by any one of the following:
--
--  -> on 
--     off
--
--  The current setting is indicated within the list.
    (type
     "print type after computation"
     interpreter
     LITERALS
     $printTypeIfTrue
     (on off)
     on)

-- ----------------------- The void Option -----------------------
--
--  Description: print Void value when it occurs
--
--  The void option may be followed by any one of the following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (void
     "print Void value when it occurs"
     interpreter
     LITERALS
     $printVoidIfTrue
     (on off)
     off)
   ))

-- Variable     Description                           Current Value
-- -----------------------------------------------------------------
-- abbreviate  abbreviate type names                    off 
-- algebra     display output in algebraic form         On:CONSOLE 
-- asgard      show output in asgard form               off 
-- characters  choose special output character set      plain 
-- fortran     create output in FORTRAN format          Off:CONSOLE 
-- fraction    how fractions are formatted              vertical 
-- length      line length of output displays           77 
-- scripts     show subscripts,... linearly             off 
-- showeditor  view output of )show in editor           off 
-- tex         create output in TeX style               Off:CONSOLE
-- mathml      create output in MathML style            Off:CONSOLE 
  (output
   "view and set some output options"
   interpreter
   TREE
   novar

-- -------------------- The abbreviate Option --------------------
--
--  Description: abbreviate type names
--
--  The abbreviate option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    ((abbreviate
      "abbreviate type names"
      interpreter
      LITERALS
      $abbreviateTypes
      (on off)
      off)

-- --------------------- The algebra Option ----------------------
--
--  Description: display output in algebraic form
--
--  )set output algebra is used to tell AXIOM to turn algebra-style
--   output printing on and off, and where to place the output.  By
--   default, the destination for the output is the screen but 
--   printing is turned off.
--
-- Syntax:   )set output algebra <arg>
--     where arg can be one of
--   on          turn algebra printing on (default state)
--   off         turn algebra printing off
--   console     send algebra output to screen (default state)
--   fp<.fe>     send algebra output to file with file prefix fp
--               and file extension .fe. If not given, 
--               .fe defaults to .spout.
--
-- If you wish to send the output to a file, you may need to issue
-- this command twice: once with on and once with the file name. 
-- For example, to send algebra output to the file polymer.spout,
-- issue the two commands
--
--   )set output algebra on
--   )set output algebra polymer
--
-- The output is placed in the directory from which you invoked 
-- AXIOM or the one you set with the )cd system command.
-- The current setting is:  On:CONSOLE 
     (algebra
      "display output in algebraic form"
      interpreter
      FUNCTION
      setOutputAlgebra
      (("display output in algebraic form"
        LITERALS
        $algebraFormat
        (off on)
        on)
       (break $algebraFormat)
       ("where algebra printing goes (enter {\em console} or a pathname)?"
        FILENAME
        $algebraOutputFile
        chkOutputFileName
        "console"))
      NIL)

-- -------------------- The Asgard Form Option --------------------
--
--  Description: show output in asgard form
--
--  The abbreviate option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
     (asgard
      "show output in asgard form"
      interpreter
      LITERALS
      $asgardForm
      (on off)
      off)

-- -------------------- The characters Option --------------------
--
--  Description: choose special output character set
--
--  The characters option may be followed by any one of the 
--  following:
--
--     default
--  -> plain 
--
--  The current setting is indicated within the list.  This 
--  option  determines the special characters used for algebraic 
--  output. This is what the current choice of special characters 
--  looks like:
--    ulc is shown as +          urc is shown as +       
--    llc is shown as +          lrc is shown as +       
--    vbar is shown as |         hbar is shown as -      
--    quad is shown as ?         lbrk is shown as [      
--    rbrk is shown as ]         lbrc is shown as {      
--    rbrc is shown as }         ttee is shown as +      
--    btee is shown as +         rtee is shown as +      
--    ltee is shown as +         ctee is shown as +      
--    bslash is shown as \    
     (characters
      "choose special output character set"
      interpreter
      FUNCTION
      setOutputCharacters
      NIL
      htSetOutputCharacters)

-- --------------------- The fortran Option ----------------------
--
--  Description: create output in FORTRAN format
--
--  )set output fortran is used to tell AXIOM to turn FORTRAN-style
--   output printing on and off, and where to place the output.  By
--   default, the destination for the output is the screen but 
--   printing is turned off.
--
-- Also See: )set fortran
--
-- Syntax:   )set output fortran <arg>
--     where arg can be one of
--   on          turn FORTRAN printing on
--   off         turn FORTRAN printing off (default state)
--   console     send FORTRAN output to screen (default state)
--   fp<.fe>     send FORTRAN output to file with file prefix 
--               fp and file extension .fe. If not given, 
--               .fe defaults to .sfort.
--
-- If you wish to send the output to a file, you must issue 
-- this command twice: once with on and once with the file name.
-- For example, to send FORTRAN output to the file polymer.sfort,
--  issue the two commands
--
--   )set output fortran on
--   )set output fortran polymer
--
-- The output is placed in the directory from which you invoked
-- AXIOM or the one you set with the )cd system command.
-- The current setting is:  Off:CONSOLE 
     (fortran
      "create output in FORTRAN format"
      interpreter
      FUNCTION
      setOutputFortran
      (("create output in FORTRAN format"
        LITERALS
        $fortranFormat
        (off on)
        off)
       (break $fortranFormat)
       ("where FORTRAN output goes (enter {\em console} or a a pathname)"
        FILENAME
        $fortranOutputFile
        chkOutputFileName
        "console"))
      NIL)

-- --------------------- The fraction Option ---------------------
--
--  Description: how fractions are formatted
--
--  The fraction option may be followed by any one of the following:
--
--  -> vertical 
--     horizontal
--
--  The current setting is indicated within the list.
     (fraction
      "how fractions are formatted"
      interpreter
      LITERALS
      $fractionDisplayType
      (vertical horizontal)
      vertical)

-- ---------------------- The length Option ----------------------
--
--  Description: line length of output displays
--
--  The length option may be followed by an integer in the range 
--  10 to 245 inclusive. The current setting is 77 
     (length
      "line length of output displays"
      interpreter
      INTEGER
      $LINELENGTH
      (10 245)
      77)

-- --------------------- The scripts Option ----------------------
--
--  Description: show subscripts,... linearly
--
--  The scripts option may be followed by any one of the following:
--
--     yes
--     no
--
--  The current setting is indicated within the list.
     (scripts
      "show subscripts,... linearly"
      interpreter
      LITERALS
      $linearFormatScripts
      (yes no)
      no)

-- -------------------- The showeditor Option --------------------
--
--  Description: view output of )show in editor
--
--  The showeditor option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
     (showeditor
      "view output of )show in editor"
      interpreter
      LITERALS
      $useEditorForShowOutput
      (on off)
      off)

-- ----------------------- The tex Option ------------------------
--
--  Description: create output in TeX style
--
--  )set output tex is used to tell AXIOM to turn TeX-style output
-- printing on and off, and where to place the output.  By default,
-- the destination for the output is the screen but printing is 
-- turned off.
--
-- Syntax:   )set output tex <arg>
--     where arg can be one of
--   on          turn TeX printing on
--   off         turn TeX printing off (default state)
--   console     send TeX output to screen (default state)
--   fp<.fe>     send TeX output to file with file prefix fp
--               and file extension .fe. If not given, 
--               .fe defaults to .stex.
--
-- If you wish to send the output to a file, you must issue 
-- this command twice: once with on and once with the file name. 
-- For example, to send TeX output to the file polymer.stex, 
-- issue the two commands
--
--   )set output tex on
--   )set output tex polymer
--
-- The output is placed in the directory from which you invoked 
-- AXIOM or the one you set with the )cd system command.
-- The current setting is:  Off:CONSOLE 
     (tex
      "create output in TeX style"
      interpreter
      FUNCTION
      setOutputTex
      (("create output in TeX format"
        LITERALS
        $texFormat
        (off on)
        off)
       (break $texFormat)
       ("where TeX output goes (enter {\em console} or a pathname)"
        FILENAME
        $texOutputFile
        chkOutputFileName
        "console"))
      NIL)

-- ----------------------- The mathml Option ------------------------
--
-- Description: create output in MathML style
--
-- )set output mathml is used to tell OpenAxiom to turn MathML-style output
-- printing on and off, and where to place the output. By default,
-- the destination for the output is the screen but printing is
-- turned off.
--
-- Syntax: )set output mathml <arg>
-- where arg can be one of
-- on turn MathML printing on
-- off turn MathML printing off (default state)
-- console send MathML output to screen (default state)
-- fp<.fe> send MathML output to file with file prefix fp
-- and file extension .fe. If not given,
-- .fe defaults to .smml.
--
-- If you wish to send the output to a file, you must issue
-- this command twice: once with on and once with the file name.
-- For example, to send MathML output to the file polymer.smml,
-- issue the two commands
--
-- )set output mathml on
-- )set output mathml polymer
--
-- The output is placed in the directory from which you invoked
-- OpenAxiom or the one you set with the )cd system command.
-- The current setting is: Off:CONSOLE

  (mathml
  "create output in MathML style"
  interpreter
  FUNCTION
  setOutputMathml
  (("create output in MathML format"
  LITERALS
  $mathmlFormat
  (off on)
  off)
  (break $mathmlFormat)
  ("where MathML output goes (enter {\em console} or a pathname)"
  FILENAME
  $mathmlOutputFile
  chkOutputFileName
  "console"))
  NIL)
  ))

-- ----------------------- The quit Option -----------------------
--
--  Description: protected or unprotected quit
--
--  The quit option may be followed by any one of the following:
--
--     protected
--  -> unprotected 
--
--  The current setting is indicated within the list.
  (quit
   "protected or unprotected quit"
   interpreter
   LITERALS
   $quitCommandType
   (protected unprotected)
   unprotected)

--               Current Values of  streams  Variables                   
--
-- Variable     Description                           Current Value
-- -----------------------------------------------------------------
-- calculate    specify number of elements to calculate    10 
-- showall      display all stream elements computed       off 
  (streams
   "set some options for working with streams"
   interpreter
   TREE
   novar

-- -------------------- The calculate Option ---------------------
--
--  Description: specify number of elements to calculate
--
--    )set streams calculate is used to tell AXIOM how many elements
--     of a stream to calculate when a computation uses the stream. 
--     The value given after calculate must either be the word all 
--     or a positive integer.
--
--       The current setting is 10 .
   ((calculate
     "specify number of elements to calculate"
     interpreter
     FUNCTION
     setStreamsCalculate
     (("number of initial stream elements you want calculated"
      INTEGER
      $streamCount
      (0 NIL)
      10))
     NIL)

-- --------------------- The showall Option ----------------------
--
--  Description: display all stream elements computed
--
--  The showall option may be followed by any one of the following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
   (showall
    "display all stream elements computed"
    interpreter
    LITERALS
    $streamsShowAll
    (on off)
    off)
  ))

--               Current Values of  system  Variables                    
--
-- Variable     Description                           Current Value
-- -----------------------------------------------------------------
-- functioncode show gen. LISP for functions when compiled off 
-- optimization show optimized LISP code                   off 
-- prettyprint  prettyprint BOOT func's as they compile    off 
  (system
   "set some system development variables"
   development
   TREE
   novar

-- ------------------- The functioncode Option -------------------
--
--  Description: show gen. LISP for functions when compiled
--
--  The functioncode option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
   ((functioncode
     "show gen. LISP for functions when compiled"
     development
     LITERALS
     $reportCompilation
     (on off)
     off)

-- ------------------- The optimization Option -------------------
--
--  Description: show optimized LISP code
--
--  The optimization option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (optimization
     "show optimized LISP code"
     development
     LITERALS
     $reportOptimization
     (on off)
     off)

-- ------------------- The prettyprint Option --------------------
--
--  Description: prettyprint BOOT func's as they compile
--
--  The prettyprint option may be followed by any one of the 
--  following:
--
--     on
--  -> off 
--
--  The current setting is indicated within the list.
    (prettyprint
     "prettyprint BOOT func's as they compile"
     development
     LITERALS
     $PrettyPrint
     (on off)
     off)
   ))

-- -------------------- The userlevel Option ---------------------
--
--  Description: operation access level of system user
--
--  The userlevel option may be followed by any one of the 
--  following:
--
--     interpreter
--     compiler
--  -> development 
--
--  The current setting is indicated within the list.
  (userlevel
   "operation access level of system user"
   interpreter
   LITERALS
   $UserLevel
   (interpreter compiler development)
   development)
 )

--%
$reportCoerceIfTrue := nil

--%
printLoadMessages u ==
  u in '(%display% %describe%) =>
    ($printLoadMsgs => '"on"; '"off")
  $printLoadMsgs := u is ["on"]

-- The following creates a list of option names in the above table.

$setOptionNames := [x.0 for x in $setOptions]

initializeSetVariables $setOptions
