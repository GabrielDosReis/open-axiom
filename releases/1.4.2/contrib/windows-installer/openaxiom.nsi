#----------------------------------------------------
# OpenAxiom NSI Install Script
#
# Written By: Dan Martens dan_martens@lycos.com
# Updated By: Bill Page  bill.page1@sympatico.ca
# Updated By: Alfredo Portes alfredo.portes@gmail.com
#----------------------------------------------------

!include "MUI.nsh"
!include "StrFunc.nsh"
!include "WinMessages.nsh"

;-----------------
; Define Variables
;-----------------

!define APPNAME "OpenAxiom"
!define BUILD_VERSION "1.4.1"
!define BUILD_DIRECTORY "lib\open-axiom\i686-pc-mingw32\${BUILD_VERSION}"
!define APPNAMEANDVERSION "${APPNAME}-${BUILD_VERSION}"
!define ICON "openaxiom.ico"
!define BINARIES "bin"
!define GCC "gcc"
!define EXECUTABLE "bin\open-axiom.exe"
!define LICENSE "copyright.txt"
!define OUTFILE "open-axiom-${BUILD_VERSION}-windows-i386.exe"
!define TEXMACS_PLUGINS_DIRECTORY "C:\Program Files\WinTeXmacs\TeXmacs\plugins"

Var OpenAxiom_TEMP
Var STARTMENU_FOLDER

!verbose 3

!ifdef ALL_USERS
  !define WriteEnvStr_RegKey \
    'HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"'
!else
  !define WriteEnvStr_RegKey 'HKCU "Environment"'
!endif

!verbose 4

${StrRep}
Var OPENAXIOMVAR

Function AddToPath
  Exch $0
  Push $1
  Push $2
  Push $3

  # don't add if the path doesn't exist
  IfFileExists $0 "" AddToPath_done

  ReadEnvStr $1 PATH
  Push "$1;"
  Push "$0;"
  Call StrStr
  Pop $2
  StrCmp $2 "" "" AddToPath_done
  Push "$1;"
  Push "$0\;"
  Call StrStr
  Pop $2
  StrCmp $2 "" "" AddToPath_done
  GetFullPathName /SHORT $3 $0
  Push "$1;"
  Push "$3;"
  Call StrStr
  Pop $2
  StrCmp $2 "" "" AddToPath_done
  Push "$1;"
  Push "$3\;"
  Call StrStr
  Pop $2
  StrCmp $2 "" "" AddToPath_done

  Call IsNT
  Pop $1
  StrCmp $1 1 AddToPath_NT
    ; Not on NT
    StrCpy $1 $WINDIR 2
    FileOpen $1 "$1\autoexec.bat" a
    FileSeek $1 -1 END
    FileReadByte $1 $2
    IntCmp $2 26 0 +2 +2 # DOS EOF
      FileSeek $1 -1 END # write over EOF
    FileWrite $1 "$\r$\nSET PATH=$3;%PATH%$\r$\n"
    FileClose $1
    SetRebootFlag true
    Goto AddToPath_done

  AddToPath_NT:
    ReadRegStr $1 HKCU "Environment" "PATH"
    StrCpy $2 $1 1 -1 # copy last char
    StrCmp $2 ";" 0 +2 # if last char == ;
      StrCpy $1 $1 -1 # remove last char
    StrCmp $1 "" AddToPath_NTdoIt
      StrCpy $0 "$0;$1"
    AddToPath_NTdoIt:
      WriteRegExpandStr HKCU "Environment" "PATH" $0
      SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000

  AddToPath_done:
    Pop $3
    Pop $2
    Pop $1
    Pop $0
FunctionEnd

Function un.RemoveFromPath
  Exch $0
  Push $1
  Push $2
  Push $3
  Push $4
  Push $5
  Push $6

  IntFmt $6 "%c" 26 # DOS EOF

  Call un.IsNT
  Pop $1
  StrCmp $1 1 unRemoveFromPath_NT
    ; Not on NT
    StrCpy $1 $WINDIR 2
    FileOpen $1 "$1\autoexec.bat" r
    GetTempFileName $4
    FileOpen $2 $4 w
    GetFullPathName /SHORT $0 $0
    StrCpy $0 "SET PATH=%PATH%;$0"
    Goto unRemoveFromPath_dosLoop

    unRemoveFromPath_dosLoop:
      FileRead $1 $3
      StrCpy $5 $3 1 -1 # read last char
      StrCmp $5 $6 0 +2 # if DOS EOF
        StrCpy $3 $3 -1 # remove DOS EOF so we can compare
      StrCmp $3 "$0$\r$\n" unRemoveFromPath_dosLoopRemoveLine
      StrCmp $3 "$0$\n" unRemoveFromPath_dosLoopRemoveLine
      StrCmp $3 "$0" unRemoveFromPath_dosLoopRemoveLine
      StrCmp $3 "" unRemoveFromPath_dosLoopEnd
      FileWrite $2 $3
      Goto unRemoveFromPath_dosLoop
      unRemoveFromPath_dosLoopRemoveLine:
        SetRebootFlag true
        Goto unRemoveFromPath_dosLoop

    unRemoveFromPath_dosLoopEnd:
      FileClose $2
      FileClose $1
      StrCpy $1 $WINDIR 2
      Delete "$1\autoexec.bat"
      CopyFiles /SILENT $4 "$1\autoexec.bat"
      Delete $4
      Goto unRemoveFromPath_done

  unRemoveFromPath_NT:
    ReadRegStr $1 HKCU "Environment" "PATH"
    StrCpy $5 $1 1 -1 # copy last char
    StrCmp $5 ";" +2 # if last char != ;
      StrCpy $1 "$1;" # append ;
    Push $1
    Push "$0;"
    Call un.StrStr ; Find `$0;` in $1
    Pop $2 ; pos of our dir
    StrCmp $2 "" unRemoveFromPath_done
      ; else, it is in path
      # $0 - path to add
      # $1 - path var
      StrLen $3 "$0;"
      StrLen $4 $2
      StrCpy $5 $1 -$4 # $5 is now the part before the path to remove
      StrCpy $6 $2 "" $3 # $6 is now the part after the path to remove
      StrCpy $3 $5$6

      StrCpy $5 $3 1 -1 # copy last char
      StrCmp $5 ";" 0 +2 # if last char == ;
        StrCpy $3 $3 -1 # remove last char

      WriteRegExpandStr HKCU "Environment" "PATH" $3
      SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000

  unRemoveFromPath_done:
    Pop $6
    Pop $5
    Pop $4
    Pop $3
    Pop $2
    Pop $1
    Pop $0
FunctionEnd

#-----------------------------------------------
# WriteEnvStr - Writes an environment variable
# Note: Win9x systems requires reboot
#
# Example:
#  Push "HOMEDIR"           # name
#  Push "C:\New Home Dir\"  # value
#  Call WriteEnvStr
#-----------------------------------------------
Function WriteEnvStr
  Exch $1 ; $1 has environment variable value
  Exch
  Exch $0 ; $0 has environment variable name
  Push $2
  
  Call IsNT
  Pop $2
  StrCmp $2 1 WriteEnvStr_NT
    ; Not on NT
    StrCpy $2 $WINDIR 2 ; Copy drive of windows (c:)
    FileOpen $2 "$2\autoexec.bat" a
    FileSeek $2 0 END
    FileWrite $2 "$\r$\nSET $0=$1$\r$\n"
    FileClose $2
    SetRebootFlag true
    Goto WriteEnvStr_done

  WriteEnvStr_NT:
      WriteRegExpandStr ${WriteEnvStr_RegKey} $0 $1
      SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} \
        0 "STR:Environment" /TIMEOUT=5000
  
  WriteEnvStr_done:
    Pop $2
    Pop $1
    Pop $0
FunctionEnd

#---------------------------------------------------
# un.DeleteEnvStr - Removes an environment variable
# Note: Win9x systems requires reboot
#
# Example:
#  Push "HOMEDIR"           # name
#  Call un.DeleteEnvStr
#---------------------------------------------------
Function un.DeleteEnvStr
  Exch $0 ; $0 now has the name of the variable
  Push $1
  Push $2
  Push $3
  Push $4
  Push $5
  
  Call un.IsNT
  Pop $1
  StrCmp $1 1 DeleteEnvStr_NT
    ; Not on NT
    StrCpy $1 $WINDIR 2
    FileOpen $1 "$1\autoexec.bat" r
    GetTempFileName $4
    FileOpen $2 $4 w
    StrCpy $0 "SET $0="
    SetRebootFlag true
    
    DeleteEnvStr_dosLoop:
      FileRead $1 $3
      StrLen $5 $0
      StrCpy $5 $3 $5
      StrCmp $5 $0 DeleteEnvStr_dosLoop
      StrCmp $5 "" DeleteEnvStr_dosLoopEnd
      FileWrite $2 $3
      Goto DeleteEnvStr_dosLoop
    
    DeleteEnvStr_dosLoopEnd:
      FileClose $2
      FileClose $1
      StrCpy $1 $WINDIR 2
      Delete "$1\autoexec.bat"
      CopyFiles /SILENT $4 "$1\autoexec.bat"
      Delete $4
      Goto DeleteEnvStr_done

  DeleteEnvStr_NT:
    DeleteRegValue ${WriteEnvStr_RegKey} $0
    SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} \
      0 "STR:Environment" /TIMEOUT=5000
  
  DeleteEnvStr_done:
    Pop $5
    Pop $4
    Pop $3
    Pop $2
    Pop $1
    Pop $0
FunctionEnd

!macro IsNT un
Function ${un}IsNT
  Push $0
  ReadRegStr $0 HKLM "SOFTWARE\Microsoft\Windows NT\CurrentVersion" CurrentVersion
  StrCmp $0 "" 0 IsNT_yes
  ; we are not NT.
  Pop $0
  Push 0
  Return

  IsNT_yes:
    ; NT!!!
    Pop $0
    Push 1
FunctionEnd

!macroend
!insertmacro IsNT ""
!insertmacro IsNT "un."

!macro StrStr un
Function ${un}StrStr
Exch $R1 ; st=haystack,old$R1, $R1=needle
  Exch    ; st=old$R1,haystack
  Exch $R2 ; st=old$R1,old$R2, $R2=haystack
  Push $R3
  Push $R4
  Push $R5
  StrLen $R3 $R1
  StrCpy $R4 0
  ; $R1=needle
  ; $R2=haystack
  ; $R3=len(needle)
  ; $R4=cnt
  ; $R5=tmp
  loop:
    StrCpy $R5 $R2 $R3 $R4
    StrCmp $R5 $R1 done
    StrCmp $R5 "" done
    IntOp $R4 $R4 + 1
    Goto loop
done:
  StrCpy $R1 $R2 "" $R4
  Pop $R5
  Pop $R4
  Pop $R3
  Pop $R2
  Exch $R1
FunctionEnd

!macroend
!insertmacro StrStr ""
!insertmacro StrStr "un."

InstType "Typical"
Name "${APPNAME}"
OutFile ${OUTFILE}

;Default installation folder
InstallDir "$PROGRAMFILES\${APPNAME}"

;Get installation folder from registry if available
InstallDirRegKey HKLM "Software\${APPNAME}" ""

;Vista redirects $SMPROGRAMS to all users without this
;RequestExecutionLevel admin

!define MUI_ABORTWARNING
;!define MUI_FINISHPAGE_RUN "$INSTDIR\${EXECUTABLE}" '--system="$INSTDIR\${BUILD_DIRECTORY}'
;!define MUI_FINISHPAGE_LINK "Please donate to the Axiom Foundation"
;!define MUI_FINISHPAGE_LINK_LOCATION "http://axiom-developer.org/public/donate.html"
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE ${LICENSE}
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
  
;Start Menu Folder Page Configuration
!define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKCU" 
!define MUI_STARTMENUPAGE_REGISTRY_KEY "Software\${APPNAME}"
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"
  
!insertmacro MUI_PAGE_STARTMENU Application $STARTMENU_FOLDER
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH
!insertmacro MUI_UNPAGE_WELCOME
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_UNPAGE_FINISH
!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_LANGUAGE "French"
!insertmacro MUI_LANGUAGE "German"
!insertmacro MUI_LANGUAGE "Russian"
!insertmacro MUI_LANGUAGE "Spanish"
!insertmacro MUI_LANGUAGE "TradChinese"
!insertmacro MUI_RESERVEFILE_LANGDLL

Section "!OpenAxiom Core" Section1

  SectionIn 1 2 RO
        
  ; Set Section properties
  SetOverwrite on

  ; Install for all users for now.
  ; We should ask the user.
  SetShellVarContext all

  SetOutPath "$INSTDIR"
  File /r ${APPNAME}\*.*
  File ${ICON}

  SetOutPath "$INSTDIR\lib\open-axiom\i686-pc-mingw32\${BUILD_VERSION}\bin"
  File /nonfatal /r ${BINARIES}\*.*

  ;SetOutPath "$INSTDIR\lib"
  ;File /r ${GCC}

  ReadEnvStr $0 "USERPROFILE" ;

  ;Store installation folder
  WriteRegStr HKCU "Software\${APPNAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
    
    CreateDirectory "$SMPROGRAMS\$STARTMENU_FOLDER"  
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\${APPNAME}.lnk" "$INSTDIR\bin\open-axiom.exe" '--system="$INSTDIR\${BUILD_DIRECTORY}"' "$INSTDIR\${ICON}"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\${APPNAME} (Interpreter).lnk" "$INSTDIR\bin\open-axiom.exe" '--no-gui --system="$INSTDIR\${BUILD_DIRECTORY}"' "$INSTDIR\${ICON}"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
    CreateShortCut "$DESKTOP\OpenAxiom.lnk" "$INSTDIR\bin\open-axiom.exe" '--system="$INSTDIR\${BUILD_DIRECTORY}"' "$INSTDIR\${ICON}"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\OpenAxiom Website.lnk" "http://www.open-axiom.org" "" "$INSTDIR\${ICON}"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\OpenAxiom Bug Reports.lnk" "http://www.open-axiom.org/bugs.html" "" "$INSTDIR\${ICON}"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Online Axiom Book.lnk" "http://axiom-wiki.newsynthesis.org/uploads/contents.xhtml" "" "$INSTDIR\${ICON}"
  
  !insertmacro MUI_STARTMENU_WRITE_END

SectionEnd

#Section /o "JyperDoc" Section2
#        
#  SetOutPath "$INSTDIR\bin"
#  SetOverwrite on 
#
#  ;Shortcuts
#  CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\JyperDoc.lnk" "$INSTDIR\bin\jyperdoc.bat" "$INSTDIR\${ICON}"
#  CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\JyperDoc Website.lnk" "http://code.google.com/p/jyperdoc/" "" "$INSTDIR\${ICON}"
#
#  SetOutPath "$INSTDIR\${BUILD_DIRECTORY}"
#  SetOverwrite on 
#
#  File /r jyperdoc
#
#SectionEnd

Section -FinishSection

  ${StrRep} $OPENAXIOMVAR "$INSTDIR\${BUILD_DIRECTORY}" "\" "/"
  Push "OPENAXIOM"
  Push "$OPENAXIOMVAR"
  Call WriteEnvStr

  SetOutPath "$0\My Documents" # sets the 'START IN' parameter
  WriteRegStr HKLM "Software\${APPNAME}" "" "$INSTDIR"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "DisplayName" "${APPNAME}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "UninstallString" "$INSTDIR\uninstall.exe"
  WriteUninstaller "$INSTDIR\uninstall.exe"

SectionEnd

;Section /o "TeXMacs Plugin"

;  SetOutPath $INSTDIR
;  SetOverwrite on

;  File /r tm_openaxiom\openaxiom  

;  SetOutPath "$OPENAXIOMVAR\bin"

;  File /r tm_openaxiom\openaxiom\bin\tm_openaxiom.exe

;SectionEnd

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${Section1} "OpenAxiom Core"
  !insertmacro MUI_DESCRIPTION_TEXT ${Section2} "TeXMacs Plugin"
  ;!insertmacro MUI_DESCRIPTION_TEXT ${Section3} "Experimental browser frontend for OpenAxiom"
!insertmacro MUI_FUNCTION_DESCRIPTION_END

Section -AddtoPath
  Push "$INSTDIR\bin"
  Call AddToPath
SectionEnd

;-----------------
; Unistall Section
;-----------------

Section "Uninstall"

  ; Install for all users for now.
  ; We should ask the user.
  SetShellVarContext all
  
  Delete "$INSTDIR\Uninstall.exe"
  RMDir /r $INSTDIR
  !insertmacro MUI_STARTMENU_GETFOLDER Application $OpenAxiom_TEMP
    
  Delete "$SMPROGRAMS\$OpenAxiom_TEMP\Uninstall.lnk"
  Delete "$SMPROGRAMS\$OpenAxiom_TEMP\${APPNAME}.lnk"
  Delete "$SMPROGRAMS\$OpenAxiom_TEMP\${APPNAME} (Interpreter).lnk"
  Delete "$DESKTOP\OpenAxiom.lnk"
  Delete "$SMPROGRAMS\$OpenAxiom_TEMP\OpenAxiom Website.lnk"
  Delete "$SMPROGRAMS\$OpenAxiom_TEMP\OpenAxiom Bug Reports.lnk"
  Delete "$SMPROGRAMS\$OpenAxiom_TEMP\Online Axiom Book.lnk"

  ;Delete "$SMPROGRAMS\$OpenAxiom_TEMP\JyperDoc.lnk"
  ;Delete "$SMPROGRAMS\$OpenAxiom_TEMP\JyperDoc Website.lnk"
  
  ;Delete empty start menu parent directories
  StrCpy $OpenAxiom_TEMP "$SMPROGRAMS\$OpenAxiom_TEMP"
 
  startMenuDeleteLoop:
    ClearErrors
    RMDir $OpenAxiom_TEMP
    GetFullPathName $OpenAxiom_TEMP "$OpenAxiom_TEMP\.."
    
    IfErrors startMenuDeleteLoopDone
  
    StrCmp $OpenAxiom_TEMP $SMPROGRAMS startMenuDeleteLoopDone startMenuDeleteLoop
  startMenuDeleteLoopDone:

  DeleteRegKey /ifempty HKCU "Software\${APPNAME}"
  Push "$INSTDIR\bin"
        
  Call un.RemoveFromPath

  # remove the variable
  Push "OPENAXIOM"
  Call un.DeleteEnvStr

SectionEnd

