(defun askname ()
    (format t "name: ")
    (finish-output)
    (read-line)
)
(defun askname-returned ()
    (let ((name (askname)))
    (format t "~A~%" name)
    name))

(princ (askname-returned))

