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
  (reduce #'(lambda (elt1 elt2)
	      (cond ((and (listp elt1) (listp elt2)) (append elt1 elt2))
		    ((and (listp elt1) (atom  elt2)) (append elt1 (list elt2)))
		    ((and (atom  elt1) (listp elt2)) (cons elt1 elt2))
		    ((and (atom  elt1) (atom  elt2)) (cons elt1 (list elt2)))))
	  list))

(defun substitute-vars (arguments file files)
  (flatten
   (mapcar #'(lambda (argument)
	       (cond ((eq argument :file) file)
		     ((eq argument :files) files)
		     (t argument)))
	   arguments)))

(defun call-action (action &optional file files)
  (let ((program (action-program action))
	(arguments (action-arguments action)))
  (break)
    (run program (substitute-vars arguments file files))))

(defun perform-dynamic-files (dynamic-files actions)
  (dolist (dynamic-file dynamic-files)
    (let* ((file (getf dynamic-file :dynamic-file))
	   (actionname (getf dynamic-file :action))
	   (action (find-action actionname actions)))
      (if (not (eq action nil))
	  (call-action action file)
	  (format t "Null action ~a" actionname)))))

(defun perform-preliminary-backup (files backup-file)
  (run "tar"
       `("-cjf"
	 ,backup-file
	 ,@files)))

(defun copy-to-dir (file-copy-from file-copy-to directory)
  (multiple-value-bind (ensured-directory dir-created-p)
      (ensure-directories-exist directory)
    (if dir-created-p
	(format t "Created directory ~a.~%" ensured-directory))
    (if ensured-directory
	(run "cp"
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
	   (run ;;/strings
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
  (run "rm"
       `("-rf" ,directory)))

(defun perform-backup (backup actions)
  (let ((tmp-dir (format nil "/tmp/~a.~a/"
			 (get-universal-time)
			 (random 1000))))
    (multiple-value-bind (directory dir-created-p)
	(ensure-directories-exist tmp-dir)
      (if dir-created-p
	  (let ((backup-file (format nil "~a/backuped.tar.bz2" directory)))
	    (perform-dynamic-files
	     (get-values :dynamic-file backup)
	     actions)
	    (perform-preliminary-backup
	     (flatten (get-values :file backup))
	     backup-file)
	    (perform-copy-to-dirs
	     backup-file
	     (get-by-value :output-directory backup))
	    (perform-delete-preliminary-backup directory))
	  (format t "Directory ~a can't be created~%" directory)))))

(defun do-backup (config-location)
  (let* ((config (read-config config-location))
	 (actions (parse-actions config)))
    (dolist (backup (get-values :backup config))
      (perform-backup backup actions))))
