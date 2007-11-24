;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
;; All rights reserved.
;; Copyright (C) 2007, Gabriel Dos Reis.
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
;;     - Neither the name of The Numerical ALgorithms Group Ltd. nor the
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



(import-module "sys-macros")
(in-package "BOOT")

#+(and :Lucid (not :ibm/370))
(progn
  (system:define-foreign-function :c 'open_server :fixnum)
  (system:define-foreign-function :c 'sock_get_int :fixnum)
  (system:define-foreign-function :c 'sock_send_int :fixnum)
  (system:define-foreign-function :c 'sock_get_string_buf :fixnum)
  (system:define-foreign-function :c 'sock_send_string_len :fixnum)
  (system:define-foreign-function :c 'sock_get_float :single)
  (system:define-foreign-function :c 'sock_send_float :fixnum)
  (system:define-foreign-function :c 'sock_send_wakeup :fixnum)
  (system:define-foreign-function :c 'server_switch :fixnum)
  (system:define-foreign-function :c 'flush_stdout :fixnum)
  (system:define-foreign-function :c 'sock_send_signal :fixnum)
  (system:define-foreign-function :c 'print_line :fixnum)
  (system:define-foreign-function :c 'plus_infininty :single)
  (system:define-foreign-function :c 'minus_infinity :single)
  (system:define-foreign-function :c 'NANQ :single)
)

#+KCL
(progn
  (clines "extern double plus_infinity(), minus_infinity(), NANQ();")
  (clines "extern double sock_get_float();")
;; GCL may pass strings by value.  'sock_get_string_buf' should fill
;; string with data read from connection, therefore needs address of
;; actual string buffer. We use 'sock_get_string_buf_wrapper' to
;; resolve the problem
  (clines "int sock_get_string_buf_wrapper(int i, object x, int j)"
          "{ if (type_of(x)!=t_string) FEwrong_type_argument(sLstring,x);"
          "  if (x->st.st_fillp<j)"
          "    FEerror(\"string too small in sock_get_string_buf_wrapper\",0);"
          "  return sock_get_string_buf(i, x->st.st_self, j); }")
  (defentry open_server (string) (int "open_server"))
  (defentry sock_get_int (int) (int "sock_get_int"))
  (defentry sock_send_int (int int) (int "sock_send_int"))
  (defentry sock_get_string_buf (int object int) 
     (int "sock_get_string_buf_wrapper"))
  (defentry sock_send_string_len (int string int) (int "sock_send_string_len"))
  (defentry sock_get_float (int) (double "sock_get_float"))
  (defentry sock_send_float (int double) (int "sock_send_float"))
  (defentry sock_send_wakeup (int int) (int "sock_send_wakeup"))
  (defentry server_switch () (int "server_switch"))
  (defentry flush_stdout () (int "flush_stdout"))
  (defentry sock_send_signal (int int) (int "sock_send_signal"))
  (defentry print_line (string) (int "print_line"))
  (defentry plus_infinity () (double "plus_infinity"))
  (defentry minus_infinity () (double "minus_infinity"))
  (defentry NANQ () (double "NANQ"))
  )

(defun open-server (name)
#+(and :lucid :ibm/370) -2
#-(and :lucid :ibm/370)
  (open_server name))
(defun sock-get-int (type)
#+(and :lucid :ibm/370) ()
#-(and :lucid :ibm/370)
  (sock_get_int type))
(defun sock-send-int (type val)
#+(and :lucid :ibm/370) ()
#-(and :lucid :ibm/370)
  (sock_send_int type val))
(defun sock-get-string (type buf buf-len)
#+(and :lucid :ibm/370) ()
#-(and :lucid :ibm/370)
  (sock_get_string_buf type buf buf-len))
(defun sock-send-string (type str)
#+(and :lucid :ibm/370) ()
#-(and :lucid :ibm/370)
  (sock_send_string_len type str (length str)))
(defun sock-get-float (type)
#+(and :lucid :ibm/370) ()
#-(and :lucid :ibm/370)
  (sock_get_float type))
(defun sock-send-float (type val)
#+(and :lucid :ibm/370) ()
#-(and :lucid :ibm/370)
  (sock_send_float type val))
(defun sock-send-wakeup (type)
#+(and :lucid :ibm/370) ()
#-(and :lucid :ibm/370)
  (sock_send_wakeup type))
(defun server-switch ()
#+(and :lucid :ibm/370) ()
#-(and :lucid :ibm/370)
  (server_switch))
(defun sock-send-signal (type signal)
#+(and :lucid :ibm/370) ()
#-(and :lucid :ibm/370)
  (sock_send_signal type signal))
(defun print-line (str)
#+(and :lucid :ibm/370) ()
#-(and :lucid :ibm/370)
  (print_line str))
(defun |plusInfinity| () (plus_infinity))
(defun |minusInfinity| () (minus_infinity))

;; Macros for use in Boot

(defun |openServer| (name)
  (open_server name))
(defun |sockGetInt| (type)
  (sock_get_int type))
(defun |sockSendInt| (type val)
  (sock_send_int type val))
(defun |sockGetString| (type buf buf-len)
  (sock_get_string_buf type buf buf-len))
(defun |sockSendString| (type str)
  (sock_send_string_len type str (length str)))
(defun |sockGetFloat| (type)
  (sock_get_float type))
(defun |sockSendFloat| (type val)
  (sock_send_float type val))
(defun |sockSendWakeup| (type)
  (sock_send_wakeup type))
(defun |serverSwitch| ()
  (server_switch))
(defun |sockSendSignal| (type signal)
  (sock_send_signal type signal))
(defun |printLine| (str)
  (print_line str))

;; Socket types.  This list must be consistent with the one in com.h

(defconstant SessionManager     1)
(defconstant ViewportServer     2)
(defconstant MenuServer         3)
(defconstant SessionIO          4)
(defconstant MessageServer      5)
(defconstant InterpWindow       6)
(defconstant KillSpad           7)
(defconstant DebugWindow        8)
(defconstant NAGLinkServer      8)
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
(defconstant |$NAGLinkServer|   NAGLinkServer)
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

(setq |$NaNvalue| (NANQ))
#-:ccl
  (setq |$plusInfinity| (* 1.1  MOST-POSITIVE-LONG-FLOAT))
#+:ccl
  (setq |$plusInfinity| MOST-POSITIVE-LONG-FLOAT)
(setq |$minusInfinity| (- |$plusInfinity|))

