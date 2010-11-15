(defvar  |NewLine| '#\NewLine)

(defun |WriteLine| (string &optional (outstream *standard-output*))
  (write-line string outstream)
  (finish-output outstream) )


(defun |panOpen| (pathvar)
  (open pathvar :if-does-not-exist nil))
