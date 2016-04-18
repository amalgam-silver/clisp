(dolist (x '(1 2 3)) (print x))

(defun fib-num (num)
  (do ((n 0 (+ 1 n))
       (cur 0 next)
       (next 1 (+ cur next)))
      ((= n num) cur) (print cur)))

(defun seq(num)
  (dotimes (i num)
    (print i)))

(defun primep (num)
  (when (> num 1)
    (loop for fac from 2 to (isqrt num) never (zerop (mod num fac)))))

(defun next-prime (num)
  (loop for n from num when (primep n) return n))

(defmacro do-prime (var-and-range &rest body)
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
      ,@body)))









