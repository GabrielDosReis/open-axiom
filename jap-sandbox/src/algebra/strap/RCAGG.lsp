
(/VERSIONCHECK 2) 

(DEFPARAMETER |RecursiveAggregate;CAT| 'NIL) 

(DEFPARAMETER |RecursiveAggregate;AL| 'NIL) 

(DEFUN |RecursiveAggregate;| (|t#1|)
  (PROG (#0=#:G1396)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                       (COND
                         (|RecursiveAggregate;CAT|)
                         ('T
                          (LETT |RecursiveAggregate;CAT|
                                (|Join| (|HomogeneousAggregate| '|t#1|)
                                        (|mkCategory| '|domain|
                                         '(((|children| ((|List| $) $))
                                            T)
                                           ((|nodes| ((|List| $) $)) T)
                                           ((|leaf?| ((|Boolean|) $))
                                            T)
                                           ((|value| (|t#1| $)) T)
                                           ((|elt| (|t#1| $ "value"))
                                            T)
                                           ((|cyclic?| ((|Boolean|) $))
                                            T)
                                           ((|leaves|
                                             ((|List| |t#1|) $))
                                            T)
                                           ((|distance|
                                             ((|Integer|) $ $))
                                            T)
                                           ((|child?|
                                             ((|Boolean|) $ $))
                                            (|has| |t#1|
                                             (|SetCategory|)))
                                           ((|node?| ((|Boolean|) $ $))
                                            (|has| |t#1|
                                             (|SetCategory|)))
                                           ((|setchildren!|
                                             ($ $ (|List| $)))
                                            (|has| $
                                             (ATTRIBUTE
                                              |shallowlyMutable|)))
                                           ((|setelt|
                                             (|t#1| $ "value" |t#1|))
                                            (|has| $
                                             (ATTRIBUTE
                                              |shallowlyMutable|)))
                                           ((|setvalue!|
                                             (|t#1| $ |t#1|))
                                            (|has| $
                                             (ATTRIBUTE
                                              |shallowlyMutable|))))
                                         NIL
                                         '((|List| $) (|Boolean|)
                                           (|Integer|) (|List| |t#1|))
                                         NIL))
                                . #1=(|RecursiveAggregate|))))) . #1#)
        (|setShellEntry| #0# 0
            (LIST '|RecursiveAggregate| (|devaluate| |t#1|))))))) 

(DEFUN |RecursiveAggregate| (#0=#:G1397)
  (LET (#1=#:G1398)
    (COND
      ((SETQ #1# (|assoc| (|devaluate| #0#) |RecursiveAggregate;AL|))
       (CDR #1#))
      (T (SETQ |RecursiveAggregate;AL|
               (|cons5| (CONS (|devaluate| #0#)
                              (SETQ #1# (|RecursiveAggregate;| #0#)))
                        |RecursiveAggregate;AL|))
         #1#)))) 
