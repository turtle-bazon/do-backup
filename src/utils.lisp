(in-package :ru.bazon.server-tools.do-backup)

(defun string-split (string &key (ws '(#\Space #\Tab)) (empty-strings t))
  (if (eq string nil) '()
      (let* ((ws-position (position-if #'(lambda (char) (member char ws))
				       string))
	     (string-first (subseq string 0 ws-position))
	     (string-rest (cond (ws-position (subseq string (+ ws-position 1)))
				(t nil))))
	(macrolet ((string-split-req ()
		     `(string-split string-rest
				    :ws ws
				    :empty-strings empty-strings)))
	  (if (and (equal string-first "") (not empty-strings))
	      (string-split-req)
	      (cons string-first (string-split-req)))))))

(defun run/string (program arguments)
  (with-output-to-string (stream)
    (run program arguments :output stream)))

(defun run/strings (program arguments)
  (string-split (run/string program arguments)
		:ws '(#\Return #\NewLine)))
