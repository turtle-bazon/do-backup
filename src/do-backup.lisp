;;;; -*- mode: lisp -*-

(in-package :ru.bazon.server-tools.do-backup)

;(setf sb-impl::*default-external-format* :UTF-8)
;
;(defparameter *config-location* "/etc/do-backup.conf")

(defparameter *tmp-directory* "/tmp/")

(defun read-config (file)
  (with-open-file (stream file) (read stream)))

(defun parse-actions (config)
  (get-values :action config))

(defun find-action (action actions)
  (get-value action actions))

(defun action-program (action)
  (first (get-value :program action)))

(defun action-suffix (action)
  (first (get-value :suffix action)))

(defun action-arguments (action)
  (flatten (get-values :argument action)))

(defun get-values (value list &optional &key (test #'equal))
  (remove nil
	  (mapcar
	   #'(lambda (element)
	       (if (funcall test value (first element))
		   (rest element)))
	   list)))

(defun get-value (value list &optional &key (test #'equal))
  (let ((values (get-values value list :test test)))
    (if (= (length values) 1)
	(first values)
	(error "Error number of values ~a in result ~a"
	       (length values)
	       values))))

(defun flatten (list)
  (cond ((null list) list)
	((= 1 (length list)) (if (listp (first list))
				 (first list)
				 list))
	(t (reduce #'(lambda (elt1 elt2)
		       (cond ((and (listp elt1)
				   (listp elt2)) (append elt1 elt2))
			     ((and (listp elt1)
				   (atom  elt2)) (append elt1 (list elt2)))
			     ((and (atom  elt1)
				   (listp elt2)) (cons elt1 elt2))
			     ((and (atom  elt1)
				   (atom  elt2)) (cons elt1 (list elt2)))))
		   list))))

(defun substitute-vars (arguments file files)
  (flatten
   (mapcar #'(lambda (argument)
	       (cond ((eq argument :file) file)
		     ((eq argument :files) files)
		     (t argument)))
	   arguments)))

(defun program-run (program arguments &optional outputstream)
  (multiple-value-bind (status code)
      (if outputstream
	  (run program arguments :output outputstream)
	  (run program arguments))
    (if (not (= code 0))
	(format t "~a, ~a, [~a, ~a]~&" program arguments status code))
    (values status code)))

(defun mail-message (to subject message)
  (let ((process (start "mail" `("-s" ,subject ,to) :input :stream)))
    (with-open-stream (process-input (process-input-stream process))
      (format process-input message))))

(defun call-action (action &optional file files outputstream)
  (let ((program (action-program action))
	(arguments (substitute-vars (action-arguments action) file files)))
    (program-run program arguments outputstream)))

(defun perform-dynamic-files (dynamic-files actions)
  (dolist (dynamic-file dynamic-files)
    (let* ((file (car dynamic-file))
	   (file-options (cdr dynamic-file))
	   (actionname (getf file-options :action))
	   (action (find-action actionname actions))
	   (saveoutput (getf file-options :saveoutput)))
      (if (not (eq action nil))
	  (if saveoutput
	      (with-open-file (stream file
				      :direction :output
				      :if-exists :supersede)
		(call-action action file nil stream))	      
	      (call-action action file))
	  (format t "Null action ~a" actionname)))))

(defun perform-preliminary-backup (backup actions directory)
  (let* ((action-name (car (get-value :action backup)))
	 (action (find-action action-name actions))
	 (backup-file (format nil "~a/backuped~a" directory
			      (action-suffix action))))
    (call-action action backup-file
		 (mapcar #'(lambda (f)
			     (car f))
			 (append
			  (get-values :file backup)
			  (get-values :dynamic-file backup))))
    backup-file))

(defun copy-to-dir (file-copy-from file-copy-to directory)
  (multiple-value-bind (ensured-directory dir-created-p)
      (ensure-directories-exist directory)
    (if dir-created-p
	(format t "Created directory ~a.~%" ensured-directory))
    (if ensured-directory
	(program-run "cp"
		     `(,file-copy-from
		       ,(format nil "~a~a" directory file-copy-to)))
	(format t "Directory ~a can't be accessed.~%" ensured-directory))))

(defun directory-p (filename)
  (probe-file (format nil "~a/." filename)))

(defun normalize-directory (directory backup-length)
  (let ((backup-list
	 (remove-if
	  #'directory-p
	  (mapcar
	   #'(lambda (filename)
	       (format nil "~a/~a" directory filename))
	   (run/strings
	    "ls"
	    `("-1" "-t" "-r" ,directory))))))
    (do ((new-length (length backup-list) (- new-length 1))
	 (new-list backup-list (rest new-list)))
	((<= new-length backup-length) '())
      (if (not (delete-file (first new-list)))
	  (format t "Error while deleting file ~a.~%"
		  (first new-list))))))

(defun perform-copy-to-dirs (file directories)
  (let ((universal-time (get-universal-time)))
    (dolist (directory directories)
      (let ((dir-name (first directory))
	    (backup-template (second directory))
	    (backup-length (third directory)))
	(copy-to-dir file
		     (format nil backup-template universal-time)
		     dir-name)
	(normalize-directory dir-name backup-length)))))

(defun perform-delete-preliminary-backup (directory)
  (program-run "rm"
	       `("-rf" ,directory)))

(defun perform-backup (backup actions)
  (let ((tmp-dir (format nil "/tmp/~a.~a/"
			 (get-universal-time)
			 (random 1000))))
    (multiple-value-bind (directory dir-created-p)
	(ensure-directories-exist tmp-dir)
      (if dir-created-p
	  (let ((dynamic (perform-dynamic-files
			  (get-values :dynamic-file backup) actions))
		(backup-file (perform-preliminary-backup backup
							 actions
							 directory)))
	    (perform-copy-to-dirs
	     backup-file
	     (get-values :output-directory backup))
	    (perform-delete-preliminary-backup directory))
	  (format t "Directory ~a can't be created~%" directory)))))

(defun do-backup (config-location)
  (let ((so (make-array '(0) :element-type 'base-char
			:fill-pointer 0 :adjustable t))
	(se (make-array '(0) :element-type 'base-char
			:fill-pointer 0 :adjustable t)))
    (with-output-to-string (*standard-output* so)
      (with-output-to-string (*error-output* se)
	(let* ((config (read-config config-location))
	       (actions (parse-actions config)))
	  (dolist (backup (get-values :backup config))
	    (perform-backup backup actions)))))
    (if (or (not (= (length so) 0))
	    (not (= (length se) 0)))
	(mail-message "root" "DO-BACKUP reports"
		      (format nil "STD:~& ~a,~& ERR:~& ~a~&" so se)))))
