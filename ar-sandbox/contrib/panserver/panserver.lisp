(defvar  |NewLine| '#\NewLine)

(defun |WriteLine| (string &optional (outstream *standard-output*))
  (write-line string outstream)
  (finish-output outstream) )

(defun |HistoryTable| ()
  (car (car |$internalHistoryTable|)))

(defun |panOpen| (pathvar)
  (open pathvar :if-does-not-exist nil))