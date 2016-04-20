;;;; This is a demo of test frame
;;;; It's an example in chapter-9 of 'Pratical Common Lisp' written by Peter Seibel

;;; original method 
;; old method used to test + with only one output T or NIL
(defun test-+-old1()
  (and
   (= (+ 1 2) 3)
   (= (+ -5 4) -1)
   (= (+ -3 -2) -5)))

;; old method used to test + with detialed test Info   
(defun test-+-old2()
  (format t "~:[Fail~;Pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[Fail~;Pass~] ... ~a~%" (= (+ -5 4) -1) '(= (+ -5 4) -1))
  (format t "~:[Fail~;Pass~] ... ~a~%" (= (+ -3 -2) -5) '(= (+ -3 -2) -5)))

;;; a little abstract
;; report result
(defun report-result-old (result form)
  (format t "~:[Fail~;Pass~] ... ~a~%" result form)
  result)

;; old method used to test + based on test-+-old2 with a little abstract
(defun test-+-old3 ()
  (report-result-old (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result-old (= (+ -5 4) -1) '(= (+ -5 4) -1))
  (report-result-old (= (+ -3 -2) -5) '(= (+ -3 -2) -5)))

;;; more abstract
;; check macro
(defmacro check1 (test-form)
  `(report-result-old ,test-form ',test-form))

;; test + with check-old1 macro
(defun test-+-old4 ()
  (check1 (= (+ 1 2) 3))
  (check1 (= (+ -5 4) -1))
  (check1 (= (+ -3 -2) -5)))

;; check
(defmacro check2 (&body test-form)
  `(progn     
     ,@(loop for f in test-form collect `(report-result-old ,f ',f))))

;; another test func
(defun test-+-old5 ()
  (check2 
    (= (+ 1 2) 3)
    (= (+ -5 4) -1)
    (= (+ -3 -2) -5)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
    ,@body))

;; old combine-results: and without shortcut
(defmacro combine-results-old (&body forms)
  (let ((result (gensym)))
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

;; combine-results: and without shortcut 
(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

;; check
(defmacro check-old2 (&body test-form)
  `(combine-results     
     ,@(loop for f in test-form collect `(report-result-old ,f ',f))))

;; final text-+
(defun test-+-old7 ()
  (check-old2 
    (= (+ 1 2) 3)
    (= (+ -5 4) -1)
    (= (+ -3 -2) -5)))

;; another test func for *
(defun test-*-old1 ()
  (check-old2
    (= (* 1 3) 3)
    (= (* -2 3) -6)))

;; a test func 
(defun test-arithmetic-old ()
  (combine-results
    (test-+-old7)
    (test-*-old1)))

;;; final version

;; a global var for report test branch
(defvar *test-name* nil)

;; define a func for testing
(defmacro deftest (name param &body body)
  `(defun ,name ,param
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

;; final report-result
(defun report-result (result form)
  (format t "~:[Fail~;Pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;; final check
(defmacro check (&body test-form)
  `(combine-results     
     ,@(loop for f in test-form collect `(report-result ,f ',f))))

(deftest test-+ ()
  (combine-results
    (check
      (= (+ 1 2) 3)
      (= (+ -5 4) -1)
      (= (+ -2 -3) -5))))

(deftest test-* ()
  (combine-results
    (check
      (= (* 1 3) 3)
      (= (* -2 3) -6))))

;; a test func 
(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

;;
(deftest test-math ()
  (combine-results
    (test-arithmetic)))







