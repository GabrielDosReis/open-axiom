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



-- Conventions:

--  * when called with argument "%initialize", a function will set the
--    appropriate variables to their default states. 

--  * when called with argument "%display%", a function will return a
--    current state information suitable for sayBrightly 

--  * when called with argument "%describe%", a function will print a
--    description of itself and any conditions it imposes. 

--  * otherwise, a function may interpret its arguments as it sees appropriate.

-- Also by convention each top level function named in the FUNCTION 
-- slot (see the data structure in setvart.boot) has an
-- associated describe function. Thus, for example,
-- setOutputFortran is accompanied by function to describe 
-- its arguments, such as describeSetOutputFortran.


import sys_-macros
import debug
namespace BOOT


$InitialCommandSynonymAlist == [
   ["?"          , :'"what commands"],_
   ["ap"         , :'"what things"],_
   ["apr"        , :'"what things"],_
   ["apropos"    , :'"what things"],_
   ["cache"      , :'"set functions cache"],_
   ["cl"         , :'"clear"],_
   ["cls"        , :'"zsystemdevelopment )cls"],_
   ["cms"        , :'"system"],_
   ["co"         , :'"compiler"],_
   ["d"          , :'"display"],_
   ["dep"        , :'"display dependents"],_
   ["dependents" , :'"display dependents"],_
   ["e"          , :'"edit"],_
   ["expose"     , :'"set expose add constructor"],_
   ["fc"         , :'"zsystemdevelopment )c"],_
   ["fd"         , :'"zsystemdevelopment )d"],_
   ["fdt"        , :'"zsystemdevelopment )dt"],_
   ["fct"        , :'"zsystemdevelopment )ct"],_
   ["fctl"       , :'"zsystemdevelopment )ctl"],_
   ["fe"         , :'"zsystemdevelopment )e"],_
   ["fec"        , :'"zsystemdevelopment )ec"],_
   ["fect"       , :'"zsystemdevelopment )ect"],_
   ["fns"        , :'"exec spadfn"],_
   ["fortran"    , :'"set output fortran"],_
   ["h"          , :'"help"],_
   ["hd", :["CONCAT",'"system ",["systemRootDirectory"],'"lib/hypertex &"]],_
   ["kclam"      , :'"boot clearClams ( )"],_
   ["killcaches" , :'"boot clearConstructorAndLisplibCaches ( )"],_
   ["patch"      , :'"zsystemdevelopment )patch"],_
   ["pause"      , :'"zsystemdevelopment )pause"],_
   ["prompt"     , :'"set message prompt"],_
   ["recurrence" , :'"set functions recurrence"],_
   ["restore"    , :'"history )restore"],_
   ["save"       , :'"history )save"],_
   ["startGraphics",:["CONCAT",'"system ",["systemRootDirectory"],'"lib/viewman &"]],_
   ["stopGraphics", :'"lisp (|sockSendSignal| 2 15)"],_
   ["time"       , :'"set message time"],_
   ["type"       , :'"set message type"],_
   ["unexpose"   , :'"set expose drop constructor"],_
   ["up"         , :'"zsystemdevelopment )update"],_
   ["version"    , :'"lisp *build-version*"],_
   ["w"          , :'"what"],_
   ["wc"         , :'"what categories"],_
   ["wd"         , :'"what domains"],_
   ["who"        , :'"lisp (pprint credits)"],_
   ["wp"         , :'"what packages"],_
   ["ws"         , :'"what synonyms"]_
 ]

$CommandSynonymAlist := 
  copyTree $InitialCommandSynonymAlist

-- The `set' function in this file handles the top level `)set'
-- command line functions.


initializeSetVariables (setTree) ==
  -- this function passes through the table of set variable information
  -- and initializes the variables to their default definitions.
  for setData in setTree repeat
    st := setData.setType
    st = 'FUNCTION =>
      -- here setVar is really the name of a function to call
      if functionp(setData.setVar)
        then FUNCALL( setData.setVar,"%initialize%")
        else sayMSG '"   Function not implemented."
    st = 'INTEGER  =>
      symbolValue(setData.setVar) := setData.setDef
    st = 'STRING  =>
      symbolValue(setData.setVar) := setData.setDef
    st = 'LITERALS =>
      symbolValue(setData.setVar) := translateYesNo2TrueFalse setData.setDef
    st = 'TREE =>
      initializeSetVariables(setData.setLeaf)

resetWorkspaceVariables() ==
  -- this replaces def in DEBUG LISP
  -- this function resets many workspace variables to their default
  -- values. Some things are reset by start and not reset by restart.
  SETQ(_/COUNTLIST                  , nil)
  SETQ($editFile                   , nil)
  SETQ(_/SOURCEFILES                , nil)
  SETQ($sourceFiles                 , nil)
  SETQ(_/PRETTY                     , nil)
  SETQ(_/SPACELIST                  , nil)
  SETQ(_/TIMERLIST                  , nil)
  SETQ($existingFiles               , hashTable 'EQUAL)
  SETQ($functionTable               , nil)
  SETQ($echoLineStack               , nil)
  SETQ($slamFlag                    , nil)
  SETQ($CommandSynonymAlist         , copyTree($InitialCommandSynonymAlist))
  SETQ($UserAbbreviationsAlist      , nil)
  SETQ($msgDatabase                 , nil)
  SETQ($msgDatabaseName             , nil)
  SETQ($IOindex                     , 1  )
  SETQ($e                           , $EmptyEnvironment)
  SETQ($env                         , [[nil]])

  -- many variables set by the following

  initializeSetVariables($setOptions)


translateYesNo2TrueFalse x ==
  x in '(yes on) => true
  x in '(no off)  => false
  x


translateTrueFalse2YesNo x ==
  x = true  => 'on
  x = false => 'off
  x


set l ==  set1(l, $setOptions)


++ turn on/off GCL optimized function call.
useFastLinks flag ==
)if %hasFeature KEYWORD::GCL
  SI::USE_-FAST_-LINKS flag
)else
  flag
)endif


set1(l,setTree) ==
  null l => displaySetVariableSettings(setTree,"")
  $setOptionNames : local := [x.0 for x in setTree]
  arg := selectOption(DOWNCASE first l,$setOptionNames,'optionError)
  setData := [arg,:LASSOC(arg,setTree)]

  -- check is the user is authorized for the set variable
  null satisfiesUserLevel setData.setLevel =>
    sayKeyedMsg("S2IZ0007",[$UserLevel,'"set option"])

  1 = #l => displaySetOptionInformation(arg,setData)
  st := setData.setType

  st = 'FUNCTION =>
    -- allow the user to set the default
    setfunarg :=
      l.1 = 'DEFAULT => "%initialize%"
--    (arg2 := selectOption(l.1,['default],nil)) => "%initialize%"
      KDR l
    if functionp(setData.setVar)
      then FUNCALL( setData.setVar,setfunarg)
      else sayMSG '"   Function not implemented."
    -- if so set, then show option information
    if $displaySetValue then displaySetOptionInformation(arg,setData)
    nil

  st = 'STRING   =>
    arg2 := l.1
    if arg2 = 'DEFAULT
      then symbolValue(setData.setVar) := setData.setDef
      else if arg2 then symbolValue(setData.setVar) := arg2
    -- if so set or not a valid choice, then show option information
    if $displaySetValue or (null arg2) then
      displaySetOptionInformation(arg,setData)
    nil

  st = 'INTEGER  =>
    -- validate the option, allowing the user to set the default
    arg2 :=
      num := l.1
      (integer? num) and (num >= (setData.setLeaf).0) and
        (null (upperlimit := setData.setLeaf.1) or num <= upperlimit) => num
      selectOption(l.1,['default,:setData.setLeaf],nil)
    if arg2 = 'DEFAULT
      then symbolValue(setData.setVar) := setData.setDef
      else if arg2 then symbolValue(setData.setVar) := arg2
    -- if so set or not a valid choice, then show option information
    if $displaySetValue or (null arg2) then
      displaySetOptionInformation(arg,setData)
    null arg2 => sayMessage ['" Your value",:bright object2String l.1,
        '"is not among the valid choices."]
    nil

  st = 'LITERALS =>
    -- validate the option, allowing the user to set the default
    if (arg2 := selectOption(l.1,['default,:setData.setLeaf],nil)) then
      if arg2 = 'DEFAULT
        then symbolValue(setData.setVar) := translateYesNo2TrueFalse setData.setDef
        else
          if arg2 = 'nobreak then
             useFastLinks true
          if arg2 = 'fastlinks then
             useFastLinks false
             arg2 := 'break
          symbolValue(setData.setVar) := translateYesNo2TrueFalse arg2
    -- if so set or not a valid choice, then show option information
    if $displaySetValue or (null arg2) then
      displaySetOptionInformation(arg,setData)
    null arg2 => sayMessage ['" Your value",:bright object2String l.1,
        '"is not among the valid choices."]
    nil

  -- for a sub-tree, we must recurse
  st = 'TREE =>
    set1(KDR l,setData.setLeaf)
    nil
  sayMessage ['"Cannot handle set tree node type",:bright st,"yet"]
  nil


displaySetOptionInformation(arg,setData) ==
  st := setData.setType
  -- if the option is a sub-tree, show the full menu
  st = 'TREE =>
    displaySetVariableSettings(setData.setLeaf,setData.setName)

  -- otherwise we want to show the current setting
  centerAndHighlight (strconc('"The ",object2String arg,'" Option"),
                      $LINELENGTH,specialChar 'hbar)
  sayBrightly ['"%l",:bright '"Description:",setData.setLabel]

  st = 'FUNCTION =>
    TERPRI()
    if functionp(setData.setVar)
      then FUNCALL(setData.setVar,"%describe%")
      else sayMSG '"   Function not implemented."

  st = 'INTEGER  =>
    sayMessage ['" The",:bright arg,'"option",
      '" may be followed by an integer in the range",
       :bright setData.setLeaf.0,'"to",'"%l",
        :bright setData.setLeaf.1,'"inclusive.",
         '" The current setting is",:bright eval setData.setVar]

  st = 'STRING  =>
    sayMessage ['" The",:bright arg,'"option",
      '" is followed by a string enclosed in double quote marks.", '"%l",
         '" The current setting is",:bright ["_"",eval setData.setVar, "_""]]

  st = 'LITERALS =>
    sayMessage ['" The",:bright arg,'"option",
      '" may be followed by any one of the following:"]
    current := translateTrueFalse2YesNo eval setData.setVar
    for name in setData.setLeaf repeat
      if name = current
        then sayBrightly ['" ->",:bright object2String name]
        else sayBrightly ['"    ",object2String name]
    sayMessage '" The current setting is indicated within the list."
    if (setData.setLeaf = '(yes no on off)) or
      (setData.setLeaf = '(yes no on off long)) then
       sayMessage [:bright '"yes",'"and",:bright '"no",
        '"have the same effect as",:bright '"on",'"and",:bright '"off",
          '"respectively."]


displaySetVariableSettings(setTree,label) ==
  if label = "" then label := '")set"
    else label := strconc('" ",object2String label,'" ")
  centerAndHighlight(strconc('"Current Values of ",label,
    '" Variables"),$LINELENGTH," ")
  TERPRI()
  sayBrightly ["Variable     ",
               "Description                                ",
                 "Current Value"]
  SAY fillerSpaces($LINELENGTH,char specialChar 'hbar)
  subtree := nil
  for setData in setTree repeat
    null satisfiesUserLevel setData.setLevel => nil
    setOption := object2String setData.setName
    setOption := strconc(setOption,fillerSpaces(13-#setOption,char " "),
                         setData.setLabel)
    setOption := strconc(setOption,fillerSpaces(55-#setOption,char " "))
    st := setData.setType
    st = 'FUNCTION =>
      opt :=
        functionp(setData.setVar) => FUNCALL( setData.setVar,"%display%")
        '"unimplemented"
      if cons? opt then opt := [:[o,'" "] for o in opt]
      sayBrightly concat(setOption,'"%b",opt,'"%d")
    st = 'STRING   =>
      opt := object2String eval setData.setVar
      sayBrightly [setOption,:bright opt]
    st = 'INTEGER  =>
      opt := object2String eval setData.setVar
      sayBrightly [setOption,:bright opt]
    st = 'LITERALS =>
      opt := object2String translateTrueFalse2YesNo eval setData.setVar
      sayBrightly [setOption,:bright opt]
    st = 'TREE     =>
      sayBrightly [setOption,:bright '"..."]
      subtree := true
      subname := object2String setData.setName
  TERPRI()
  subtree =>
    sayBrightly ['"Variables with current values of",:bright '"...",
      '"have further sub-options. For example,"]
    sayBrightly ['"issue",:bright '")set ",subname,
      '" to see what the options are for",:bright subname,'".",'"%l",
        '"For more information, issue",:bright '")help set",'"."]

-- See the section expose in setvart.boot
-- ---------------------- The expose Option ----------------------

--  Description: control interpreter constructor exposure

--    The following groups are explicitly exposed in the current 
--    frame (called initial ):
--                                    basic                                   
--                                 categories                                 
 
--    The following constructors are explicitly exposed in the 
--    current frame:
--                there are no explicitly exposed constructors                
 
--    The following constructors are explicitly hidden in the 
--    current frame:
--                 there are no explicitly hidden constructors                
 
--    When )set expose is followed by no arguments, the information
--    you now see is displayed. When followed by the initialize 
--    argument, the exposure group data in the file INTERP.EXPOSED 
--    is read and is then available. The arguments add and drop are 
--    used to add or drop exposure groups or explicit constructors 
--    from the local frame exposure data. Issue
--                   )set expose add    or    )set expose drop 
--    for more information.


setExpose arg ==
  arg = "%initialize%" => loadExposureGroupData()
  arg = "%display%" => '"..."

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    --  give msg about exposure groups
    displayExposedGroups()
    --  give msg about explicitly exposed constructors
    sayMSG '" "
    displayExposedConstructors()
    --  give msg about explicitly hidden constructors
    sayMSG '" "
    displayHiddenConstructors()
    -- give some more details
    sayMSG '" "
    sayKeyedMsg("S2IZ0049D",[namestring pathname ["INTERP","EXPOSED"]])

  arg is [fn,:fnargs] and (fn := selectOptionLC(fn,
    '(add drop initialize),nil)) =>
      fn = 'add  =>  setExposeAdd fnargs
      fn = 'drop =>  setExposeDrop fnargs
      fn = 'initialize => setExpose "%initialize%"
      nil
  setExpose nil


setExposeAdd arg ==
  (null arg) =>
    centerAndHighlight ("The add Option",$LINELENGTH,specialChar 'hbar)
    --  give msg about exposure groups
    displayExposedGroups()
    --  give msg about explicitly exposed constructors
    sayMSG '" "
    displayExposedConstructors()
    sayMSG '" "
    sayKeyedMsg("S2IZ0049E",nil)
  arg is [fn,:fnargs] and (fn := selectOptionLC(fn,
    '(group constructor),nil)) =>
      fn = 'group  =>  setExposeAddGroup fnargs
      fn = 'constructor =>  setExposeAddConstr fnargs
      nil
  setExposeAdd nil


setExposeAddGroup arg ==
  (null arg) =>
    centerAndHighlight("The group Option",$LINELENGTH,specialChar 'hbar)
    --  give msg about exposure groups
    displayExposedGroups()
    sayMSG '" "
    sayKeyedMsg("S2IZ0049G",[namestring pathname ["INTERP","EXPOSED"]])
    sayMSG '" "
    sayAsManyPerLineAsPossible [object2String first x for x in
      $globalExposureGroupAlist]
  for x in arg repeat
    if cons? x then x := first x
    x = 'all =>
      $localExposureData.0 :=[first x for x in $globalExposureGroupAlist]
      $localExposureData.1 :=nil
      $localExposureData.2 :=nil
      displayExposedGroups()
      sayMSG '" "
      displayExposedConstructors()
      sayMSG '" "
      displayHiddenConstructors()
      clearClams()
    null GETALIST($globalExposureGroupAlist,x) =>
      sayKeyedMsg("S2IZ0049H",[x])
    member(x,$localExposureData.0) =>
      sayKeyedMsg("S2IZ0049I",[x,$interpreterFrameName])
    $localExposureData.0 := MSORT [x,:$localExposureData.0]
    sayKeyedMsg("S2IZ0049R",[x,$interpreterFrameName])
    clearClams()


setExposeAddConstr arg ==
  (null arg) =>
    centerAndHighlight ("The constructor Option",$LINELENGTH,
      specialChar 'hbar)
    --  give msg about explicitly exposed constructors
    displayExposedConstructors()
  for x in arg repeat
    x := unabbrev x
    if cons? x then x := first x
    -- if the constructor is known, we know what type it is
    null getConstructorKindFromDB x =>
      sayKeyedMsg("S2IZ0049J",[x])
    member(x,$localExposureData.1) =>
      sayKeyedMsg("S2IZ0049K",[x,$interpreterFrameName])
    -- if the constructor is explicitly hidden, then remove that
    if member(x,$localExposureData.2) then
      $localExposureData.2 := remove($localExposureData.2,x)
    $localExposureData.1 := MSORT [x,:$localExposureData.1]
    clearClams()
    sayKeyedMsg("S2IZ0049P",[x,$interpreterFrameName])


setExposeDrop arg ==
  (null arg) =>
    centerAndHighlight ("The drop Option",$LINELENGTH,specialChar 'hbar)
    --  give msg about explicitly hidden constructors
    displayHiddenConstructors()
    sayMSG '" "
    sayKeyedMsg("S2IZ0049F",nil)
  arg is [fn,:fnargs] and (fn := selectOptionLC(fn,
    '(group constructor),nil)) =>
      fn = 'group  =>  setExposeDropGroup fnargs
      fn = 'constructor =>  setExposeDropConstr fnargs
      nil
  setExposeDrop nil


setExposeDropGroup arg ==
  (null arg) =>
    centerAndHighlight ("The group Option",$LINELENGTH,specialChar 'hbar)
    sayKeyedMsg("S2IZ0049L",nil)
    sayMSG '" "
    displayExposedGroups()
  for x in arg repeat
    if cons? x then x := first x
    x = 'all =>
      $localExposureData.0 := nil
      $localExposureData.1 := nil
      $localExposureData.2 := nil
      displayExposedGroups()
      sayMSG '" "
      displayExposedConstructors()
      sayMSG '" "
      displayHiddenConstructors()
      clearClams()
    member(x,$localExposureData.0) =>
      $localExposureData.0 := remove($localExposureData.0,x)
      clearClams()
      sayKeyedMsg("S2IZ0049S",[x,$interpreterFrameName])
    GETALIST($globalExposureGroupAlist,x) =>
      sayKeyedMsg("S2IZ0049I",[x,$interpreterFrameName])
    sayKeyedMsg("S2IZ0049H",[x])


setExposeDropConstr arg ==
  (null arg) =>
    centerAndHighlight ("The constructor Option",$LINELENGTH,
      specialChar 'hbar)
    sayKeyedMsg("S2IZ0049N",nil)
    sayMSG '" "
    displayExposedConstructors()
    sayMSG '" "
    displayHiddenConstructors()
  for x in arg repeat
    x := unabbrev x
    if cons? x then x := first x
    -- if the constructor is known, we know what type it is
    null getConstructorKindFromDB x =>
      sayKeyedMsg("S2IZ0049J",[x])
    member(x,$localExposureData.2) =>
      sayKeyedMsg("S2IZ0049O",[x,$interpreterFrameName])
    if member(x,$localExposureData.1) then
      $localExposureData.1 := remove($localExposureData.1,x)
    $localExposureData.2 := MSORT [x,:$localExposureData.2]
    clearClams()
    sayKeyedMsg("S2IZ0049Q",[x,$interpreterFrameName])

-- See the section calling in servart.boot

--               Current Values of  calling  Variables                   

-- Variable     Description                           Current Value
-- -----------------------------------------------------------------
-- tempfile     set location of temporary data files       /tmp/ 
-- directory    set location of generated FORTRAN files    ./ 
-- linker       linker arguments (e.g. libraries to search) -lxlf 


setFortTmpDir arg ==

  arg = "%initialize%" =>
    $fortranTmpDir := '"/tmp/"

  arg = "%display%" =>
    string? $fortranTmpDir => $fortranTmpDir
    PNAME $fortranTmpDir

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetFortTmpDir()

  -- try to figure out what the argument is

  -- VM code - must be an accessed disk mode [mode]
  not (mode := validateOutputDirectory arg) =>
    sayBrightly ['" Sorry, but your argument(s)",:bright arg,
      '"is(are) not valid.",'"%l"]
    describeSetFortTmpDir()
  $fortranTmpDir := mode


validateOutputDirectory x ==
  AND(PATHNAME_-DIRECTORY(PROBE_-FILE(first(x))), NOT PATHNAME_-NAME  (PROBE_-FILE(first(x)))) =>
    first(x)
  nil


describeSetFortTmpDir() ==
  sayBrightly [
   '"%b",'")set fortran calling tempfile",'"%d",_
   '" is used to tell AXIOM where",'"%l",_
   '" to place intermediate FORTRAN data files . This must be the ",'"%l",_
   '" name of a valid existing directory to which you have permission ",'"%l",_
   '" to write (including the final slash).",'"%l",'"%l",_
   '" Syntax:",'"%l",_
   '"   )set fortran calling tempfile DIRECTORYNAME",'"%l",'"%l",_
   '" The current setting is",'"%b",$fortranTmpDir,'"%d"]


setFortDir arg ==
  arg = "%initialize%" =>
    $fortranDirectory := '"./"

  arg = "%display%" =>
    string? $fortranDirectory => $fortranDirectory
    PNAME $fortranDirectory

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetFortDir()

  -- try to figure out what the argument is

  -- VM code - must be an accessed disk mode [mode]
  not (mode := validateOutputDirectory arg) =>
    sayBrightly ['" Sorry, but your argument(s)",:bright arg,
      '"is(are) not valid.",'"%l"]
    describeSetFortDir()
  $fortranDirectory := mode


describeSetFortDir() ==
  sayBrightly LIST (
   '"%b",'")set fortran calling directory",'"%d",_
   '" is used to tell OpenAxiom where",'"%l",_
   '" to place generated FORTRAN files. This must be the name ",'"%l",_
   '" of a valid existing directory to which you have permission ",'"%l",_
   '" to write (including the final slash).",'"%l",'"%l",_
   '" Syntax:",'"%l",_
   '"   )set fortran calling directory DIRECTORYNAME",'"%l",'"%l",_
   '" The current setting is",'"%b",$fortranDirectory,'"%d")


setLinkerArgs arg ==

  arg = "%initialize%" =>
    $fortranLibraries := '"-lxlf"
  arg = "%display%" => object2String $fortranLibraries
  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetLinkerArgs()
  LISTP(arg) and string?(first arg) => 
    $fortranLibraries := first(arg)
  describeSetLinkerArgs()


describeSetLinkerArgs() ==
  sayBrightly [
   '"%b",'")set fortran calling linkerargs",'"%d",_
   '" is used to pass arguments to the linker",'"%l",_
   '" when using ",'"%b",'"mkFort",'"%d",_
   '" to create functions which call Fortran code.",'"%l",_
   '" For example, it might give a list of libraries to be searched,",'"%l",_
   '" and their locations.",'"%l",_
   '" The string is passed verbatim, so must be the correct syntax for",'"%l",_
   '" the particular linker being used.",'"%l",'"%l",_
   '" Example: )set fortran calling linker _"-lxlf_"",'"%l",'"%l",_
   '" The current setting is",'"%b",$fortranLibraries,'"%d"]

-- See the section functions in setvart.boot
--
--              Current Values of  functions  Variables                  

-- Variable     Description                           Current Value
-- -----------------------------------------------------------------
-- cache        number of function results to cache        0 
-- compile      compile, don't just define function bodies off 
-- recurrence   specially compile recurrence relations     on 


setFunctionsCache arg ==
  $options : local := nil
  arg = "%initialize%" =>
    $cacheCount := 0
    $cacheAlist := nil
  arg = "%display%" =>
    null $cacheAlist => object2String $cacheCount
    '"..."
  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetFunctionsCache()
    TERPRI()
    sayAllCacheCounts()
  n := first arg
  (n ~= 'all) and ((not integer? n) or (n < 0)) =>
    sayMessage ['"Your value of",:bright n,'"is invalid because ..."]
    describeSetFunctionsCache()
    terminateSystemCommand()
  if (rest arg) then $options := [['vars,:rest arg]]
  countCache n


countCache n ==
  $options =>
    $options is [["vars",:l]] =>
      for x in l repeat
        not ident? x => sayKeyedMsg("S2IF0007",[x])
        $cacheAlist:= insertAlist(x,n,$cacheAlist)
        cacheCountName:= makeSymbol strconc(x,'";COUNT")
        symbolValue(cacheCountName) := n
        sayCacheCount(x,n)
    optionError(CAAR $options,nil)
  sayCacheCount(nil,$cacheCount:= n)


describeSetFunctionsCache() ==
  sayBrightly [
    '"%b",'")set functions cache",'"%d",'"is used to tell AXIOM how many",'"%l",_
    '" values computed by interpreter functions should be saved.  This can save ",'"%l", _
    '" quite a bit of time in recursive functions, though one must consider that",'"%l",_
    '" the cached values will take up (perhaps valuable) room in the workspace.",'"%l",'"%l",_
    '" The value given  after",'"%b",'"cache",'"%d",'"must either be the",_
    '" word",'"%b",'"all",'"%d",'"or a positive",'"%l",_
    '" integer.  This may be followed by any number of function names whose cache",'"%l",_
    '" sizes you wish to so set.  If no functions are given, the default cache",'"%l",_
    '" size is set.",'%l,'" Examples:",_
    '"   )set fun cache all         )set fun cache 10 f g Legendre"]


sayAllCacheCounts () ==
  sayCacheCount(nil,$cacheCount)
  $cacheAlist =>
    TERPRI()
--    SAY '" However,"
    for [x,:n] in $cacheAlist | n ~= $cacheCount repeat sayCacheCount(x,n)


sayCacheCount(fn,n) ==
  prefix:=
    fn => ["function",:bright linearFormatName fn]
    n = 0 => ["interpreter functions "]
    ["In general, interpreter functions "]
  n = 0 =>
    fn =>
      sayBrightly ['"   Caching for ",:prefix,
        '"is turned off"]
    sayBrightly '" In general, functions will cache no returned values."
  phrase:=
    n="all" => [:bright "all","values."]
    n=1 => [" only the last value."]
    [" the last",:bright n,"values."]
  sayBrightly ['"   ",:prefix,'"will cache",:phrase]

-- See the section history in setvart.boot
-- --------------------- The history Option ----------------------

--  Description: save workspace values in a history file

--  The history option may be followed by any one of the 
--  following:

--  -> on 
--     off

--  The current setting is indicated within the list.


setHistory arg ==
  -- this is just a front end for the history functions
  arg = "%initialize%" => nil

  current := object2String translateTrueFalse2YesNo $HiFiAccess
  arg = "%display%" => current

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    sayMessage ['" The",:bright '"history",'"option",
      '" may be followed by any one of the following:"]
    for name in '("on" "off" "yes" "no") repeat
      if name = current
        then sayBrightly ['" ->",:bright name]
        else sayBrightly ['"    ",name]
    TERPRI()
    sayBrightly '" The current setting is indicated within the list."
    sayBrightly [:bright '"yes",'"and",:bright '"no",
     '"have the same effect as",:bright '"on",'"and",:bright '"off",
       '"respectively."]
    if $useInternalHistoryTable
      then wh := '"memory"
      else wh := '"a file"
    sayBrightly ['%l,'" When the history facility is active, the data",
      '" is kept in ",wh,'"."]
    sayMessage ['" Issue",:bright '")help history",
      '"for more information."]

  arg is [fn] and
   (fn := DOWNCASE(fn)) in '(y n ye yes no on of off) =>
    $options := [[fn]]
    historySpad2Cmd()
  setHistory nil

-- 
-- See the section kernel in setvart.boot
-- \begin{verbatim}
--               Current Values of  kernel  Variables                    

-- Variable     Description                           Current Value
-- -----------------------------------------------------------------
-- warn         warn when re-definition is attempted       off 
-- protect      prevent re-definition of kernel functions  off 

-- 


describeProtectedSymbolsWarning() ==
 sayBrightly [
  '"Some AXIOM library functions are compiled into the kernel for efficiency",_
  '%l,'"reasons.  To prevent them being re-defined when loaded from a library",_
  '%l,'"they are specially protected.  If a user wishes to know when an attempt",_
  '%l,'"is made to re-define such a function, he or she should issue the command:",_
  '%l,'"        )set kernel warn on",_
  '%l,'"To restore the default behaviour, he or she should issue the command:",_
  '%l,'"        )set kernel warn off"]


protectedSymbolsWarning arg ==
  arg = "%initialize%" => PROTECTED_-SYMBOL_-WARN(false)
  arg = "%display%" =>
    v := PROTECTED_-SYMBOL_-WARN(true)
    PROTECTED_-SYMBOL_-WARN(v)
    v => '"on" 
    '"off"
  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeProtectedSymbolsWarning()
  PROTECTED_-SYMBOL_-WARN translateYesNo2TrueFalse first arg

PROTECTED_-SYMBOL_-WARN flag ==
  -- ??? Nobody seems to know what this function is supposed to do.
  -- ??? It is rumored to have been builtin to CCL.
  flag

describeProtectSymbols() ==
 sayBrightly [
  '"Some AXIOM library functions are compiled into the kernel for efficiency",_
  '%l,'"reasons.  To prevent them being re-defined when loaded from a library",_
  '%l,'"they are specially protected.  If a user wishes to re-define these",_
  '%l,'"functions, he or she should issue the command:",_
  '%l,'"        )set kernel protect off",_
  '%l,'"To restore the default behaviour, he or she should issue the command:",_
  '%l,'"        )set kernel protect on"]


protectSymbols arg ==
  arg = "%initialize%" => PROTECT_-SYMBOLS(true)
  arg = "%display%" =>
    v := PROTECT_-SYMBOLS(true)
    PROTECT_-SYMBOLS(v)
    v => '"on" 
    '"off"
  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeProtectSymbols()
  PROTECT_-SYMBOLS translateYesNo2TrueFalse first arg


PROTECT_-SYMBOLS flag ==
  -- ??? nobody seems to know what this function is supposed to do.
  -- ??? It has been rumored to be native to CCL.
  flag

-- See the subsection output algebra in setvart.boot
-- 
-- --------------------- The algebra Option ----------------------

--  Description: display output in algebraic form

--  )set output algebra is used to tell AXIOM to turn algebra-style
--   output printing on and off, and where to place the output.  By
--   default, the destination for the output is the screen but 
--   printing is turned off.

-- Syntax:   )set output algebra <arg>
--     where arg can be one of
--   on          turn algebra printing on (default state)
--   off         turn algebra printing off
--   console     send algebra output to screen (default state)
--   fp<.fe>     send algebra output to file with file prefix fp
--               and file extension .fe. If not given, 
--               .fe defaults to .spout.

-- If you wish to send the output to a file, you may need to issue
-- this command twice: once with on and once with the file name. 
-- For example, to send algebra output to the file polymer.spout,
-- issue the two commands

--   )set output algebra on
--   )set output algebra polymer

-- The output is placed in the directory from which you invoked 
-- AXIOM or the one you set with the )cd system command.
-- The current setting is:  On:CONSOLE 
-- 


setOutputAlgebra arg ==
  arg = "%initialize%" =>
    $algebraOutputStream :=
      DEFIOSTREAM('((MODE . OUTPUT) (DEVICE . CONSOLE)),255,0)
    $algebraOutputFile := '"CONSOLE"
    $algebraFormat := true

  arg = "%display%" =>
    if $algebraFormat then label := '"On:" else label := '"Off:"
    strconc(label,$algebraOutputFile)

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetOutputAlgebra()

  -- try to figure out what the argument is

  if arg is [fn] then
    arg := 
      fn in '(Y N YE YES NO O ON OF OFF CONSOLE
                y n ye yes no o on of off console) => arg
      [fn,'spout]

  arg is [fn] =>
    UPCASE(fn) in '(Y N YE O OF) =>
      sayKeyedMsg("S2IV0002",'(algebra algebra))
    UPCASE(fn) in '(NO OFF)  => $algebraFormat := nil
    UPCASE(fn) in '(YES ON) => $algebraFormat := true
    UPCASE(fn) = 'CONSOLE =>
      SHUT $algebraOutputStream
      $algebraOutputStream :=
        DEFIOSTREAM('((MODE . OUTPUT) (DEVICE . CONSOLE)),255,0)
      $algebraOutputFile := '"CONSOLE"

  (arg is [fn,ft]) or (arg is [fn,ft,fm]) => -- aha, a file
    if (ptype := pathnameType fn) then
      fn := strconc(pathnameDirectory fn,pathnameName fn)
      ft := ptype
    if null fm then fm := 'A
    filename := $FILEP(fn,ft,fm)
    null filename =>
      sayKeyedMsg("S2IV0003",[fn,ft,fm])
    (testStream := MAKE_-OUTSTREAM(filename,255,0)) =>
      SHUT $algebraOutputStream
      $algebraOutputStream := testStream
      $algebraOutputFile := object2String filename
      -- Inform of redirection only if verbosity is desired.
      $verbose and sayKeyedMsg("S2IV0004",['"Algebra",$algebraOutputFile])
    sayKeyedMsg("S2IV0003",[fn,ft,fm])

  sayKeyedMsg("S2IV0005",nil)
  describeSetOutputAlgebra()


describeSetOutputAlgebra() ==
  sayBrightly ['%b,'")set output algebra",'%d,_
   '"is used to tell AXIOM to turn algebra-style output",'%l,_
   '"printing on and off, and where to place the output.  By default, the",'%l,_
   '"destination for the output is the screen but printing is turned off.",'%l,_
   '%l,_
   '"Syntax:   )set output algebra <arg>",'%l,_
  '"    where arg can be one of",'%l,_
  '"  on          turn algebra printing on (default state)",'%l,_
  '"  off         turn algebra printing off",'%l,_
  '"  console     send algebra output to screen (default state)",'%l,_
  '"  fp<.fe>     send algebra output to file with file prefix fp",'%l,_
  '"              and file extension .fe. If not given, .fe defaults to .spout.",'%l,
  '%l,_
  '"If you wish to send the output to a file, you may need to issue this command",'%l,_
  '"twice: once with",'%b,'"on",'%d,'"and once with the file name. For example, to send",'%l,_
  '"algebra output to the file",'%b,'"polymer.spout,",'%d,'"issue the two commands",'%l,_
  '%l,_
  '"  )set output algebra on",'%l,_
  '"  )set output algebra polymer",'%l,_
  '%l,_
  '"The output is placed in the directory from which you invoked AXIOM or",'%l,_
  '"the one you set with the )cd system command.",'%l,_
  '"The current setting is: ",'%b,setOutputAlgebra "%display%",'%d]


++ In case we are catching the output, we are also interested in 
++ redirecting traffic on the standard output to that capturing stream.
setStandardOutputToAlgebraStream() ==
  $OutputStream := $algebraOutputStream
  

-- See the subsection output characters in setvart.boot
-- 
-- -------------------- The characters Option --------------------

--  Description: choose special output character set


--  The characters option may be followed by any one of the 
--  following:

--     default
--  -> plain 

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
-- 


setOutputCharacters arg ==
  -- this sets the special character set
  arg = "%initialize%" =>
    $specialCharacters := $plainRTspecialCharacters

  current :=
    $specialCharacters = $RTspecialCharacters      => '"default"
    $specialCharacters = $plainRTspecialCharacters => '"plain"
    '"unknown"
  arg = "%display%" => current

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    sayMessage ['" The",:bright '"characters",'"option",
      '" may be followed by any one of the following:"]
    for name in '("default" "plain") repeat
      if name = current
        then sayBrightly ['" ->",:bright name]
        else sayBrightly ['"    ",name]
    TERPRI()
    sayBrightly '" The current setting is indicated within the list.  This option determines "
    sayBrightly '" the special characters used for algebraic output.  This is what the"
    sayBrightly '" current choice of special characters looks like:"
    l := nil
    for [char,:.] in $specialCharacterAlist repeat
      s := strconc('"   ",PNAME char,'" is shown as ",
        PNAME specialChar(char))
      l := [s,:l]
    sayAsManyPerLineAsPossible reverse l

  arg is [fn] and (fn := DOWNCASE(fn)) =>
    fn = 'default => $specialCharacters := $RTspecialCharacters
    fn = 'plain   => $specialCharacters := $plainRTspecialCharacters
    setOutputCharacters nil
  setOutputCharacters nil

-- See the subsection output fortran in setvart.boot
-- 
-- --------------------- The fortran Option ----------------------

--  Description: create output in FORTRAN format

--  )set output fortran is used to tell AXIOM to turn FORTRAN-style
--   output printing on and off, and where to place the output.  By
--   default, the destination for the output is the screen but 
--   printing is turned off.

-- Also See: )set fortran

-- Syntax:   )set output fortran <arg>
--     where arg can be one of
--   on          turn FORTRAN printing on
--   off         turn FORTRAN printing off (default state)
--   console     send FORTRAN output to screen (default state)
--   fp<.fe>     send FORTRAN output to file with file prefix 
--               fp and file extension .fe. If not given, 
--               .fe defaults to .sfort.

-- If you wish to send the output to a file, you must issue 
-- this command twice: once with on and once with the file name.
-- For example, to send FORTRAN output to the file polymer.sfort,
--  issue the two commands

--   )set output fortran on
--   )set output fortran polymer

-- The output is placed in the directory from which you invoked
-- AXIOM or the one you set with the )cd system command.
-- The current setting is:  Off:CONSOLE 
-- 


makeStream(append,filename,i,j) ==
  append => MAKE_-APPENDSTREAM(filename,i,j)
  MAKE_-OUTSTREAM(filename,i,j)


setOutputFortran arg ==
  arg = "%initialize%" =>
    $fortranOutputStream :=
      DEFIOSTREAM('((MODE . OUTPUT) (DEVICE . CONSOLE)),255,0)
    $fortranOutputFile := '"CONSOLE"
    $fortranFormat := nil

  arg = "%display%" =>
    if $fortranFormat then label := '"On:" else label := '"Off:"
    strconc(label,$fortranOutputFile)

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetOutputFortran()

  -- try to figure out what the argument is

  append := nil
  quiet := nil
  while LISTP arg and UPCASE(first arg) in '(APPEND QUIET) repeat
    if UPCASE first(arg) = 'APPEND then append := true
    else if UPCASE first(arg) = 'QUIET then quiet := true
    arg := rest(arg)

  if arg is [fn] and
    fn in '(Y N YE YES NO O ON OF OFF CONSOLE y n ye yes no o on of off console)
      then 'ok
      else arg := [fn,'sfort]

  arg is [fn] =>
    UPCASE(fn) in '(Y N YE O OF) =>
      sayKeyedMsg("S2IV0002",'(FORTRAN fortran))
    UPCASE(fn) in '(NO OFF)  => $fortranFormat := nil
    UPCASE(fn) in '(YES ON)  => $fortranFormat := true
    UPCASE(fn) = 'CONSOLE =>
      SHUT $fortranOutputStream
      $fortranOutputStream :=
        DEFIOSTREAM('((MODE . OUTPUT) (DEVICE . CONSOLE)),255,0)
      $fortranOutputFile := '"CONSOLE"

  (arg is [fn,ft]) or (arg is [fn,ft,fm]) => -- aha, a file
    fn := STRING fn
    if (ptype := pathnameType fn) then
      fn := strconc(pathnameDirectory fn,pathnameName fn)
      ft := ptype
    if null fm then fm := 'A
    filename := $FILEP(fn,ft,fm)
    null filename => sayKeyedMsg("S2IV0003",[fn,ft,fm])
    (testStream := makeStream(append,filename,255,0)) =>
      SHUT $fortranOutputStream
      $fortranOutputStream := testStream
      $fortranOutputFile := object2String filename
      if null quiet then sayKeyedMsg("S2IV0004",['FORTRAN,$fortranOutputFile])
    if null quiet then sayKeyedMsg("S2IV0003",[fn,ft,fm])
  if null quiet then sayKeyedMsg("S2IV0005",nil)
  describeSetOutputFortran()


describeSetOutputFortran() ==
  sayBrightly ['%b,'")set output fortran",'%d,_
   '"is used to tell AXIOM to turn FORTRAN-style output",'%l,_
   '"printing on and off, and where to place the output.  By default, the",'%l,_
   '"destination for the output is the screen but printing is turned off.",'%l,_
   '%l,_
   '"Also See: )set fortran",'%l,
   '%l,_
   '"Syntax:   )set output fortran <arg>",'%l,_
  '"    where arg can be one of",'%l,_
  '"  on          turn FORTRAN printing on",'%l,_
  '"  off         turn FORTRAN printing off (default state)",'%l,_
  '"  console     send FORTRAN output to screen (default state)",'%l,_
  '"  fp<.fe>     send FORTRAN output to file with file prefix fp and file",'%l,_
  '"              extension .fe. If not given, .fe defaults to .sfort.",'%l,
  '%l,_
  '"If you wish to send the output to a file, you must issue this command",'%l,_
  '"twice: once with",'%b,'"on",'%d,'"and once with the file name. For example, to send",'%l,_
  '"FORTRAN output to the file",'%b,'"polymer.sfort,",'%d,'"issue the two commands",'%l,_
  '%l,_
  '"  )set output fortran on",'%l,_
  '"  )set output fortran polymer",'%l,_
  '%l,_
  '"The output is placed in the directory from which you invoked AXIOM or",'%l,_
  '"the one you set with the )cd system command.",'%l,_
  '"The current setting is: ",'%b,setOutputFortran "%display%",'%d]


-- See the section mathml in setvart.boot.pamphlet
--
-- ----------------------- The mathml Option ------------------------
--
-- Description: create output in MathML style
--
-- )set output mathml is used to tell AXIOM to turn MathML-style output
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
-- .fe defaults to .stex.
--
-- If you wish to send the output to a file, you must issue
-- this command twice: once with on and once with the file name.
-- For example, to send MathML output to the file polymer.stex,
-- issue the two commands
-- 
-- )set output mathml on
-- )set output mathml polymer
--
-- The output is placed in the directory from which you invoked
-- OpenAxiom or the one you set with the )cd system command.
-- The current setting is: Off:CONSOLE

setOutputMathml arg ==

  arg = "%initialize%" =>
    $mathmlOutputStream :=
      DEFIOSTREAM('((MODE . OUTPUT) (DEVICE . CONSOLE)),255,0)
    $mathmlOutputFile := '"CONSOLE"
    $mathmlFormat := nil 

  arg = "%display%" =>
    if $mathmlFormat then label := '"On:" else label := '"Off:"
    strconc(label,$mathmlOutputFile)

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetOutputMathml()

  -- try to figure out what the argument is

  if arg is [fn] and
    fn in '(Y N YE YES NO O ON OF OFF CONSOLE y n ye yes no o on of off console)
      then 'ok
      else arg := [fn,'smml]

  arg is [fn] =>
    UPCASE(fn) in '(Y N YE O OF) =>
      sayKeyedMsg("S2IV0002",'(MathML mathml))
    UPCASE(fn) in '(NO OFF) => $mathmlFormat := nil
    UPCASE(fn) in '(YES ON) => $mathmlFormat := true
    UPCASE(fn) = 'CONSOLE =>
      SHUT $mathmlOutputStream
      $mathmlOutputStream :=
        DEFIOSTREAM('((MODE . OUTPUT) (DEVICE . CONSOLE)),255,0)
      $mathmlOutputFile := '"CONSOLE"

  (arg is [fn,ft]) or (arg is [fn,ft,fm]) => -- aha, a file
    if (ptype := pathnameType fn) then
      fn := strconc(pathnameDirectory fn,pathnameName fn)
      ft := ptype
    if null fm then fm := 'A
    filename := $FILEP(fn,ft,fm)
    null filename =>
      sayKeyedMsg("S2IV0003",[fn,ft,fm])
    (testStream := MAKE_-OUTSTREAM(filename,255,0)) =>
      SHUT $mathmlOutputStream
      $mathmlOutputStream := testStream
      $mathmlOutputFile := object2String filename
      sayKeyedMsg("S2IV0004",['"MathML",$mathmlOutputFile])
    sayKeyedMsg("S2IV0003",[fn,ft,fm])

  sayKeyedMsg("S2IV0005",nil)
  describeSetOutputMathml()
 
describeSetOutputMathml() ==
  sayBrightly ['%b,'")set output mathml",'%d,_
    '"is used to tell AXIOM to turn MathML-style output",'%l,_
    '"printing on and off, and where to place the output. By default, the",'%l,_
    '"destination for the output is the screen but printing is turned off.",'%l,_ '
    '%l,_
    '"Syntax: )set output mathml <arg>",'%l,_
   '" where arg can be one of",'%l,_
   '" on turn MathML printing on",'%l,_
   '" off turn MathML printing off (default state)",'%l,_
   '" console send MathML output to screen (default state)",'%l,_
   '" fp<.fe> send MathML output to file with file prefix fp and file",'%l,_
   '" extension .fe. If not given, .fe defaults to .stex.",'%l,
   '%l,_
   '"If you wish to send the output to a file, you must issue this command",'%l,_
   '"twice: once with",'%b,'"on",'%d,'"and once with the file name. For example, to send",'%l,_
   '"MathML output to the file",'%b,'"polymer.smml,",'%d,'"issue the two commands",'%l,_
   '%l,_
   '" )set output mathml on",'%l,_
   '" )set output mathml polymer",'%l,_
   '%l,_
   '"The output is placed in the directory from which you invoked OpenAxiom or",'%l,_
   '"the one you set with the )cd system command.",'%l,_
   '"The current setting is: ",'%b,setOutputMathml "%display%",'%d]


-- See the section tex in setvart.boot
--
-- ----------------------- The tex Option ------------------------

--  Description: create output in TeX style

--  )set output tex is used to tell AXIOM to turn TeX-style output
-- printing on and off, and where to place the output.  By default,
-- the destination for the output is the screen but printing is 
-- turned off.

-- Syntax:   )set output tex <arg>
--     where arg can be one of
--   on          turn TeX printing on
--   off         turn TeX printing off (default state)
--   console     send TeX output to screen (default state)
--   fp<.fe>     send TeX output to file with file prefix fp
--               and file extension .fe. If not given, 
--               .fe defaults to .stex.

-- If you wish to send the output to a file, you must issue 
-- this command twice: once with on and once with the file name. 
-- For example, to send TeX output to the file polymer.stex, 
-- issue the two commands

--   )set output tex on
--   )set output tex polymer

-- The output is placed in the directory from which you invoked 
-- AXIOM or the one you set with the )cd system command.
-- The current setting is:  Off:CONSOLE 
--


setOutputTex arg ==
  arg = "%initialize%" =>
    $texOutputStream :=
      DEFIOSTREAM('((MODE . OUTPUT) (DEVICE . CONSOLE)),255,0)
    $texOutputFile := '"CONSOLE"
    $texFormat := nil

  arg = "%display%" =>
    if $texFormat then label := '"On:" else label := '"Off:"
    strconc(label,$texOutputFile)

  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetOutputTex()

  -- try to figure out what the argument is

  if arg is [fn] and
    fn in '(Y N YE YES NO O ON OF OFF CONSOLE y n ye yes no o on of off console)
      then 'ok
      else arg := [fn,'stex]

  arg is [fn] =>
    UPCASE(fn) in '(Y N YE O OF) =>
      sayKeyedMsg("S2IV0002",'(TeX tex))
    UPCASE(fn) in '(NO OFF)  => $texFormat := nil
    UPCASE(fn) in '(YES ON) => $texFormat := true
    UPCASE(fn) = 'CONSOLE =>
      SHUT $texOutputStream
      $texOutputStream :=
        DEFIOSTREAM('((MODE . OUTPUT) (DEVICE . CONSOLE)),255,0)
      $texOutputFile := '"CONSOLE"

  (arg is [fn,ft]) or (arg is [fn,ft,fm]) => -- aha, a file
    if (ptype := pathnameType fn) then
      fn := strconc(pathnameDirectory fn,pathnameName fn)
      ft := ptype
    if null fm then fm := 'A
    filename := $FILEP(fn,ft,fm)
    null filename =>
      sayKeyedMsg("S2IV0003",[fn,ft,fm])
    (testStream := MAKE_-OUTSTREAM(filename,255,0)) =>
      SHUT $texOutputStream
      $texOutputStream := testStream
      $texOutputFile := object2String filename
      sayKeyedMsg("S2IV0004",['"TeX",$texOutputFile])
    sayKeyedMsg("S2IV0003",[fn,ft,fm])

  sayKeyedMsg("S2IV0005",nil)
  describeSetOutputTex()


describeSetOutputTex() ==
  sayBrightly ['%b,'")set output tex",'%d,_
   '"is used to tell AXIOM to turn TeX-style output",'%l,_
   '"printing on and off, and where to place the output.  By default, the",'%l,_
   '"destination for the output is the screen but printing is turned off.",'%l,_
   '%l,_
   '"Syntax:   )set output tex <arg>",'%l,_
  '"    where arg can be one of",'%l,_
  '"  on          turn TeX printing on",'%l,_
  '"  off         turn TeX printing off (default state)",'%l,_
  '"  console     send TeX output to screen (default state)",'%l,_
  '"  fp<.fe>     send TeX output to file with file prefix fp and file",'%l,_
  '"              extension .fe. If not given, .fe defaults to .stex.",'%l,
  '%l,_
  '"If you wish to send the output to a file, you must issue this command",'%l,_
  '"twice: once with",'%b,'"on",'%d,'"and once with the file name. For example, to send",'%l,_
  '"TeX output to the file",'%b,'"polymer.stex,",'%d,'"issue the two commands",'%l,_
  '%l,_
  '"  )set output tex on",'%l,_
  '"  )set output tex polymer",'%l,_
  '%l,_
  '"The output is placed in the directory from which you invoked AXIOM or",'%l,_
  '"the one you set with the )cd system command.",'%l,_
  '"The current setting is: ",'%b,setOutputTex "%display%",'%d]

-- See the section streams in setvart.boot
--               Current Values of  streams  Variables                   

-- Variable     Description                           Current Value
-- -----------------------------------------------------------------
-- calculate    specify number of elements to calculate    10 
-- showall      display all stream elements computed       off 



setStreamsCalculate arg ==
  arg = "%initialize%" =>
    $streamCount := 10
  arg = "%display%" =>
    object2String $streamCount
  (null arg) or (arg = "%describe%") or (first arg = '_?) =>
    describeSetStreamsCalculate()
  n := first arg
  (n ~= 'all) and ((not integer? n) or (n < 0)) =>
    sayMessage ['"Your value of",:bright n,'"is invalid because ..."]
    describeSetStreamsCalculate()
    terminateSystemCommand()
  $streamCount := n

describeSetStreamsCalculate() == sayKeyedMsg("S2IV0001",[$streamCount])

