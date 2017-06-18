;;;; Demo application with some security issues.

;;;; Apart from XSS and SQLI below, it also
;;;;
;;;; - servers password over unencrypted,
;;;; - allows to find existing user names,
;;;; - password is in code & googlable,
;;;; - shows versions and documentation,
;;;; - does not have proper "secure" headers
;;;; etc
(eval-when (:compile-toplevel :load-toplevel)
  (mapcar 'ql:quickload '(cl-who hunchentoot clsql-sqlite3))
  (use-package 'clsql))

(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 8888))


(defun init ()
  "Prepare DB and run the server"
  (setq *default-database-type* :sqlite3
	*default-database* (connect ":memory:"))
  (create-table "demo" '((name (string 24)) (password (string 24))))
  (query "insert into demo VALUES ('pepa', 'heslo')")
  (hunchentoot:start *server*))

(defun cleanup ()
  (drop-table "demo")
  (disconnect)
  (setq *default-database* nil)
  (hunchentoot:stop *server*))

(defun warn+form (text)
  (cl-who:with-html-output-to-string (*standard-output*)
    (:b (princ text))
    (:h2 "Login")
    (:form
     "Jméno "(:input :name "name") :br
     "Heslo" (:input :name "password") :br
     (:input :type :submit))))

(hunchentoot:define-easy-handler (bad-query :uri "/login") (name password)
  (if (null name) (warn+form "")
      (handler-case
	  (let ((res
		 ;; SQL injection
		 (query (format nil "SELECT password from DEMO where name='~a'" name))
		  ;; proper version would be easier, with ((enable-sql-reader-syntax)
		  #+nil (select [password] :from [demo] :where [= name [name]])))
	    (cond
	      ((null res)
	       ;; XSS!
	       (warn+form  (concatenate 'string "No such user " name)))
	      ((equal (caar res) password) "You are logged in!")
	      (t (warn+form "Bad password"))))
	(error () (warn+form "Some error ocurred<br>" )))))

;;; ./sqlmap.py -p name -u "http://localhost:8888/login?name=aaa" --passwords --dbms sqlite3 --technique B --level 3 --risk 3 --dump
