(defun insert-time ()
   (interactive)
   (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))
(defun insert-date ()
   (interactive)
   (insert (format-time-string "%Y-%m-%d")))
