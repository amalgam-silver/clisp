;;;; This is the final version of the test framework
;;;; It's an example in chapter-9 of 'Pratical Common Lisp' written by Peter Seibel

(defvar *test-name* nil)

(defmacro with-gensyms ((&rest names) &body body)
  "Define a macro with inner var using gensyms"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'"
  (format t  "~:[Fail~;Pass~] ... ~a : ~a~%" result *test-name* form)
  result)

(defmacro combin-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order"
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combin-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro deftest (name param &body body)
  "Define a test function. Within a test function we can call other test function or use 'check' to run individual test cases."
  `(defun ,name ,param
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

;;;examples
(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ -5 4) -1)
    (= (+ -2 -3) -5)))

(deftest test-* ()
  (check
    (= (* 1 3) 3)
    (= (* -2 3) -6)))

(deftest test-arithmetic ()
  (test-+)
  (test-*))

(deftest test-math ()
  (test-arithmetic))







