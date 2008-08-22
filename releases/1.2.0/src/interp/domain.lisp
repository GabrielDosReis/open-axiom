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


;; lisp support for creating domain stubs

(in-package "BOOT")
;;(SETQ |$optimizableConstructorNames| nil)

(defstruct domain constructor args
  (dollar (check-dollar-fields constructor args)))

(defstruct (old-compiler-domain (:include domain) (:conc-name oldom-))
  (devaluate (if dollar (|devaluate| dollar)
               (CONS constructor (MAPCAR #'|devaluate| args))))
  (vector nil))  

(defun check-dollar-fields (constructor arglist)
  (if (some #'(lambda (x) (and (domain-p x) (domain-dollar x))) arglist)
      (apply constructor (mapcar #'(lambda (x) (if (domain-p x)
                                                   (or (domain-dollar x) x)
                                                 x)) arglist))
    nil))

(defun |domain?| (x) (domain-p x))

(defun |Mapping| (&rest args)
  (make-old-compiler-domain :constructor '|Mapping| :args args
                            :vector '|Mapping0|))

(defun |Record| (&rest args)
  (make-old-compiler-domain :constructor '|Record| :args args
                            :vector '|Record0|))

(defun |Union| (&rest args)
  (make-old-compiler-domain :constructor '|Union| :args args
                            :vector '|Union0|))

(defun |devaluate| (x &aux tag dom)
  (cond ((REFVECP x)
         (if (> (QVSIZE x) 5)
             (cond ((equal (qvelt x 3) '(|Category|))
                    (qvelt x 0))
;; next line will become obsolete
                   ((|isFunctor| (qvelt x 0)) (qvelt x 0))
                   ((domain-p (qvelt x 0)) (|devaluate| (qvelt x 0)))
                   (t x))
             x))
        ((and (pairp x) (eq (car x) '|:|) (dcq (tag dom) (cdr x)))
         (list (car x) tag (|devaluate| dom)))
; 20030527 note that domain-p does not exist
        ((not (domain-p x)) x)
; 20030527 note that old-compiler-domain-p does not exist
        ((old-compiler-domain-p x) (oldom-devaluate x))
        (t (error "devaluate of new compiler domain"))))

(defun |domainEqual| (x y)
  (cond ((old-compiler-domain-p x)
         (if (old-compiler-domain-p y)
             (equalp (oldom-devaluate x) (oldom-devaluate y))
             nil))
        ((old-compiler-domain-p y) nil)
        (t (error "no new compiler domains yet"))))

(defun |domainSelectDollar| (dom)
  (or (domain-dollar dom) dom))

(defun |domainSetDollar| (dom dollar)
  (setf (domain-dollar dom) dollar)
  (if (old-compiler-domain-p dom)
      (setf (oldom-devaluate dom) (|devaluate| dollar))))

(defun |domainSelectVector| (dom)
  (let ((vec (oldom-vector dom)))
    (cond ((vectorp vec) vec)
          ((null vec) nil)
          ((symbolp vec) ;; case for Records and Unions
           (setq vec (funcall vec (domain-args dom)))
           (setf (elt vec 0) dom)
           (setf (oldom-vector dom) vec))
          ((or (fboundp (car vec))
               (|loadLib| (cdr vec)) t)
           (instantiate (car vec) dom)))))

;;(defun instantiate (innername dom) 
;;  (let ((vec (apply innername (domain-args dom))))
;;    (setelt vec 0 dom)
;;    (setf (oldom-vector dom) vec)
;;    vec))

(defun instantiate (innername dom)
  (let* ((infovec (get (domain-constructor dom) '|infovec|))
         (|$dollarVec| (getrefv (size (car infovec )))))
    (declare (special |$dollarVec|))
    (setf (elt |$dollarVec| 0) dom)
    (setf (elt |$dollarVec| 1)
          (list (symbol-function (|getLookupFun| infovec))
                |$dollarVec|
                (elt infovec 1)))
    (setf (elt |$dollarVec| 2) (elt infovec 2))
    (setf (oldom-vector dom) |$dollarVec|)
    (apply innername (domain-args dom))
    |$dollarVec|))
  
(defun universal-domain-constructor (&rest args-env)
  (let* ((args (fix-domain-args (butlast args-env)))
         (env (car (last args-env))))
    (check-constructor-cache env args)))

(defun fix-domain-args (args)
  (mapcar #'(lambda (x) (if (and (vectorp x) (domain-p (elt x 0)))
                            (elt x 0) x)) args))

(defun universal-nocache-domain-constructor (&rest args-env)
  (let* ((args (butlast args-env))
         (env (car (last args-env))))
    (make-old-compiler-domain :constructor (car env)
                              :args args
                              :vector (cdr env))))

(defun universal-category-defaults-constructor (&rest args-env)
  (let* ((args (butlast args-env))
         (env (car (last args-env))))
    (make-old-compiler-domain :constructor (car env)
                              :args args
                              :dollar (car args)
                              :vector (cdr env))))

(defun cached-constructor (cname)
  (if (or (|isCategoryPackageName| cname)
          (and (boundp '|$mutableDomains|)
               (memq cname |$mutableDomains|)))
      nil
    t))

(defun |makeDomainStub| (con) 
  (|systemDependentMkAutoload| (|constructor?| con) con))

(defun |mkAutoLoad| (fn cname)
  (cond ((or (memq cname |$CategoryNames|)
          (eq (|getConstructorKindFromDB| cname) '|category|))
         (function (lambda (&rest args)
                     (|autoLoad| fn cname)
                     (apply cname args))))
        (t (|systemDependentMkAutoload| fn cname)
           (symbol-function cname))))

(defun |systemDependentMkAutoload| (fn cname)
  (let* ((cnameInner (intern (strconc cname ";")))
         (env (list* cname cnameInner fn))
         (spadfun
          (cond ((|isCategoryPackageName| cname)
                 (cons #'universal-category-defaults-constructor env))
                ((and (boundp '|$mutableDomains|)
                      (memq cname |$mutableDomains|))
                 (cons #'universal-nocache-domain-constructor env))
                (t (cons #'universal-domain-constructor env)))))
    (setf (symbol-function cname) (mkConstructor spadfun))
    (set cname spadfun)))

(defun mkConstructor (spadfun)
   (function (lambda (&rest args)
               (apply (car spadfun) (append args (list (cdr spadfun)))))))

(defun |makeAddDomain| (add-domain dollar)
  (cond ((old-compiler-domain-p add-domain)
         (make-old-compiler-domain :constructor (domain-constructor add-domain)
                            :args (domain-args add-domain)
                            :dollar dollar
                            :vector (cddr (eval (domain-constructor add-domain)))))
        (t (error "no new compiler adds supported yet"))))

(defun check-constructor-cache (env arglist)
  (let ((dollar (check-dollar-fields (car env) arglist)))
    (if dollar (make-old-compiler-domain :constructor (car env)
                                         :args arglist
                                         :dollar dollar
                                         :vector (cdr env))
      (let* ((constructor (car env))
             (devargs (mapcar #'|devaluate| arglist))
             (cacheddom
              (|lassocShiftWithFunction| devargs
               (HGET |$ConstructorCache| constructor)
               #'|domainEqualList|)))
        (if cacheddom (|CDRwithIncrement| cacheddom)
          (cdr (|haddProp| |$ConstructorCache| constructor devargs
                (cons 1 (make-old-compiler-domain :constructor constructor
                                                  :args arglist
                                                  :devaluate
                                                  (cons constructor devargs)
                                                  :vector (cdr env))))))))))




          
         
