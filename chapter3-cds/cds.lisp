;;;; This a demo of database of cds
;;;; It's an example in chapter-3 of 'Pratical Common Lisp' written by Peter Seibel
;;;; Each record contains a title, an artist, a rate and a ripped flag
;;;; Following methods are supportted:
;;;; (add-cds): add records of cds, after finish inputing a record,
;;;;            user will be asked if another need to be input,
;;;;            end inputing by typing 'n'
;;;; (dump-cd db): output all the records in db
;;;; (save-db loaction): save all the records into a file
;;;; (load-db location): read records from a file
;;;; (select selector-fn): select records in *db* which match selector-fn
;;;; (update selector-fn &key title artist rating ripped): update records in *db* which match selector-fn
;;;; (delete-record selector-fn): delete records in *db* which match selector-fn
;;;; (where &key title artist rating ripped): generate a selector-fn


;; global variable *db* stored all the records of cds
(defvar *db* nil)

;; make a record of a cd
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;; add a record into *db*
(defun add-record (cd)
  (push cd *db*))

;; output all the record in db
(defun dump-cd (db)
  (dolist (cd db)
    (format t "~{~a: ~10t~a~%~}~%" cd)))

;; read a string
(defun prompt-read (prompt)
  (format *query-io* "~a:" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; read a record
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped?")))

;; add records of cds, after finish inputing a record,
;; user will be asked if another need to be input,
;; end inputing by typing 'n' 
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another?")) (return))))

;; save all the records in *db* into a file
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;; read all the records form a file and store them into *db*
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; select records in *db* which match selector-fn
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;; generate a selector-fn 
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and 
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

;; update records in *db* which match selector-fn
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (cd)
             (when (funcall selector-fn cd)
               (if title    (setf (getf cd :title)  title ))
               (if artist   (setf (getf cd :artist) artist))
               (if rating   (setf (getf cd :rating) rating))
               (if ripped-p (setf (getf cd :ripped) ripped)))
             cd) *db*)))

;; delete records in *db* which match selector-fn
(defun delete-record (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))


