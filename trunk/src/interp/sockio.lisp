;; Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
;; All rights reserved.
;; Copyright (C) 2007-2008, Gabriel Dos Reis.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     - Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;     - Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in
;;       the documentation and/or other materials provided with the
;;       distribution.
;;
;;     - Neither the name of The Numerical Algorithms Group Ltd. nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



(import-module "boot-pkg")
(in-package "BOOT")

;; Macros for use in Boot

;; Socket types.  This list must be consistent with the one in com.h

(defconstant SessionManager     1)
(defconstant ViewportServer     2)
(defconstant MenuServer         3)
(defconstant SessionIO          4)
(defconstant MessageServer      5)
(defconstant InterpWindow       6)
(defconstant KillSpad           7)
(defconstant DebugWindow        8)
(defconstant Forker             9)

;; same constants for use in BOOT
(defconstant |$SessionManager|  SessionManager)
(defconstant |$ViewportServer|  ViewportServer)
(defconstant |$MenuServer|      MenuServer)
(defconstant |$SessionIO|       SessionIO)
(defconstant |$MessageServer|   MessageServer)
(defconstant |$InterpWindow|    InterpWindow)
(defconstant |$KillSpad|        KillSpad)
(defconstant |$DebugWindow|     DebugWindow)
(defconstant |$Forker|          Forker)

;; Session Manager action requests

(defconstant CreateFrame        1)
(defconstant SwitchFrames       2)
(defconstant EndOfOutput        3)
(defconstant CallInterp         4)
(defconstant EndSession         5)
(defconstant LispCommand        6)
(defconstant SpadCommand        7)
(defconstant SendXEventToHyperTeX 8)
(defconstant QuietSpadCommand   9)
(defconstant CloseClient        10)
(defconstant QueryClients       11)
(defconstant QuerySpad          12)
(defconstant NonSmanSession     13)
(defconstant KillLispSystem     14)

(defconstant |$CreateFrame|     CreateFrame)
(defconstant |$SwitchFrames|    SwitchFrames)
(defconstant |$EndOfOutput|     EndOfOutput)
(defconstant |$CallInterp|      CallInterp)
(defconstant |$EndSession|      EndSession)
(defconstant |$LispCommand|     LispCommand)
(defconstant |$SpadCommand|     SpadCommand)
(defconstant |$SendXEventToHyperTeX| SendXEventToHyperTeX)
(defconstant |$QuietSpadCommand| QuietSpadCommand)
(defconstant |$CloseClient|     CloseClient)
(defconstant |$QueryClients|    QueryClients)
(defconstant |$QuerySpad|       QuerySpad)
(defconstant |$NonSmanSession|  NonSmanSession)
(defconstant |$KillLispSystem|  KillLispSystem)

;; signal types (from /usr/include/sys/signal.h)
#+(and :Lucid (not :ibm/370))
(progn 
  (defconstant  SIGUSR1 16)     ;; user defined signal 1
  (defconstant  SIGUSR2 17)     ;; user defined signal 2
  )

#+:RIOS
(progn 
  (defconstant  SIGUSR1 30)     ;; user defined signal 1
  (defconstant  SIGUSR2 31)     ;; user defined signal 2
  )

#+:IBMPS2
(progn
  (defconstant  SIGUSR1 30)     ;; user defined signal 1
  (defconstant  SIGUSR2 31)     ;; user defined signal 2
  )
