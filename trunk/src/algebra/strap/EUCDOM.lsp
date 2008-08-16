(|/VERSIONCHECK| 2) 

(DEFPARAMETER |EuclideanDomain;AL| (QUOTE NIL)) 

(DEFUN |EuclideanDomain| NIL 
  (LET (#:G83585) 
    (COND 
      (|EuclideanDomain;AL|)
      (T (SETQ |EuclideanDomain;AL| (|EuclideanDomain;|)))))) 

(DEFUN |EuclideanDomain;| NIL 
  (PROG (#1=#:G83583) 
    (RETURN 
      (PROG1 
        (LETT #1# 
          (|Join| 
            (|PrincipalIdealDomain|)
            (|mkCategory| 
              (QUOTE |domain|)
              (QUOTE (
                ((|sizeLess?| ((|Boolean|) |$| |$|)) T)
                ((|euclideanSize| ((|NonNegativeInteger|) |$|)) T)
                ((|divide| 
                  ((|Record| 
                    (|:| |quotient| |$|)
                    (|:| |remainder| |$|))
                  |$| |$|)) T)
                ((|quo| (|$| |$| |$|)) T)
                ((|rem| (|$| |$| |$|)) T)
                ((|extendedEuclidean| 
                  ((|Record| 
                    (|:| |coef1| |$|)
                    (|:| |coef2| |$|)
                    (|:| |generator| |$|))
                  |$| |$|)) T)
                ((|extendedEuclidean| 
                  ((|Union| 
                      (|Record| (|:| |coef1| |$|) (|:| |coef2| |$|))
                      "failed")
                    |$| |$| |$|)) T)
                ((|multiEuclidean| 
                  ((|Union| 
                      (|List| |$|)
                      "failed") 
                   (|List| |$|) |$|)) T)))
              NIL 
              (QUOTE ((|List| |$|) (|NonNegativeInteger|) (|Boolean|)))
              NIL)) 
            |EuclideanDomain|)
        (SETELT #1# 0 (QUOTE (|EuclideanDomain|))))))) 

(MAKEPROP (QUOTE |EuclideanDomain|) (QUOTE NILADIC) T) 

