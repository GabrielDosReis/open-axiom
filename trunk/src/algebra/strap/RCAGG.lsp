
(/VERSIONCHECK 2) 

(DEFPARAMETER |RecursiveAggregate;CAT| 'NIL) 

(DEFPARAMETER |RecursiveAggregate;AL| 'NIL) 

(DEFUN |RecursiveAggregate;| (|t#1|)
  (LET ((#0=#:G1397
            (|sublisV| (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                (COND
                  (|RecursiveAggregate;CAT|)
                  ('T
                   (LETT |RecursiveAggregate;CAT|
                         (|Join| (|HomogeneousAggregate| '|t#1|)
                                 (|mkCategory| '|domain|
                                     '(((|children| ((|List| $) $)) T)
                                       ((|nodes| ((|List| $) $)) T)
                                       ((|leaf?| ((|Boolean|) $)) T)
                                       ((|value| (|t#1| $)) T)
                                       ((|elt| (|t#1| $ "value")) T)
                                       ((|cyclic?| ((|Boolean|) $)) T)
                                       ((|leaves| ((|List| |t#1|) $))
                                        T)
                                       ((|distance| ((|Integer|) $ $))
                                        T)
                                       ((|child?| ((|Boolean|) $ $))
                                        (|has| |t#1| (|SetCategory|)))
                                       ((|node?| ((|Boolean|) $ $))
                                        (|has| |t#1| (|SetCategory|)))
                                       ((|setchildren!|
                                         ($ $ (|List| $)))
                                        (|has| $
                                         (ATTRIBUTE |shallowlyMutable|)))
                                       ((|setelt|
                                         (|t#1| $ "value" |t#1|))
                                        (|has| $
                                         (ATTRIBUTE |shallowlyMutable|)))
                                       ((|setvalue!| (|t#1| $ |t#1|))
                                        (|has| $
                                         (ATTRIBUTE |shallowlyMutable|))))
                                     NIL
                                     '((|List| $) (|Boolean|)
                                       (|Integer|) (|List| |t#1|))
                                     NIL))
                         |RecursiveAggregate|))))))
    (|setShellEntry| #0# 0
        (LIST '|RecursiveAggregate| (|devaluate| |t#1|)))
    #0#)) 

(DEFUN |RecursiveAggregate| (#0=#:G1398)
  (LET ((#1=#:G1399 (|assoc| (|devaluate| #0#) |RecursiveAggregate;AL|)))
    (COND
      (#1# (CDR #1#))
      (T (PROGN
           (SETQ #1# (|RecursiveAggregate;| #0#))
           (SETQ |RecursiveAggregate;AL|
                 (|cons5| (CONS (|devaluate| #0#) #1#)
                          |RecursiveAggregate;AL|))
           #1#))))) 
