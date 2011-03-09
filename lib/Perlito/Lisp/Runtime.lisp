
;; Perlito Lisp runtime
;;
;; Author: Flavio Soibelmann Glock <fglock@gmail.com>
;;
;; Copyright 2009, 2011 by Flavio Soibelmann Glock and others.
;; 
;; This program is free software; you can redistribute it and/or modify it
;; under the same terms as Perl itself.
;; 
;; See <http://www.perl.com/perl/misc/Artistic.html>

(defpackage mp-Main
  (:use common-lisp)
  (:export 
        #:sv-eq #:sv-bool #:sv-substr #:sv-say #:sv-print #:sv-index 
        #:sv-and #:sv-or #:sv-perl #:sv-scalar #:sv-string #:sv-undef
        #:sv-defined #:sv-array-index #:sv-hash-lookup #:sv-add 
        #:sv-true ))
(in-package mp-Main)

(defparameter *mp6-args* ())

(defun init-argv ()
  (progn
    (setf COMMON-LISP-USER::*posix-argv* (cdr COMMON-LISP-USER::*posix-argv*))
    (setf *mp6-args* (make-array 
                                (length COMMON-LISP-USER::*posix-argv*) 
                                :adjustable 1 
                                :fill-pointer t 
                                :initial-contents COMMON-LISP-USER::*posix-argv*))))

;; predeclarations

(if (not (ignore-errors (find-method '(setf sv-bool) () ())))
  (defgeneric (setf sv-bool) (x v)))
(if (not (ignore-errors (find-method '(setf sv-from) () ())))
  (defgeneric (setf sv-from) (x v)))
(if (not (ignore-errors (find-method '(setf sv-to)   () ())))
  (defgeneric (setf sv-to)   (x v)))
(if (not (ignore-errors (find-method '(setf sv-str)  () ())))
  (defgeneric (setf sv-str)  (x v)))
(if (not (ignore-errors (find-method 'sv-string () ())))
  (defgeneric sv-string (x) 
      (:documentation "stringify values")))
(if (not (ignore-errors (find-method 'sv-join () ())))
  (defgeneric sv-join (l &optional delim)
      (:documentation "list join")))
(if (not (ignore-errors (find-method 'sv-array () ())))
  (defgeneric sv-array (self)
      (:documentation "get an array value")))
(if (not (ignore-errors (find-method 'sv-hash () ())))
  (defgeneric sv-hash (self)
      (:documentation "get a hash value")))
(if (not (ignore-errors (find-method 'sv-push () ())))
  (defgeneric sv-push (self x)
      (:documentation "push")))
(if (not (ignore-errors (find-method 'sv-perl_escape_string () ())))
  (defgeneric sv-perl_escape_string (self)
      (:documentation "escape a single quoted perl string value")))
(if (not (ignore-errors (find-method 'sv-javascript_escape_string () ())))
  (defgeneric sv-javascript_escape_string (self)
      (:documentation "escape a single quoted javascript string value")))


;; "undef"

(if (not (ignore-errors (find-class 'mp-Undef)))
  (defclass mp-Undef () ()))
(let (x)
  (setq x (make-instance 'mp-Undef))
  (defun proto-mp-Undef () x)
  (defun sv-undef () x))

;; core functions

(if (not (ignore-errors (find-method 'sv-defined () ())))
  (defgeneric sv-defined (x)
      (:documentation "check if a value is defined")))
(defmethod sv-defined (x) t)
(defmethod sv-defined ((x mp-Undef)) nil)

(defun sv-true ()
  T)

(defun sv-say (l)
  (progn
    (map nil #'(lambda (c) (format t "~a" (sv-string c))) l)
    (format t "~%" )))

(defun sv-print (l)
  (map nil #'(lambda (c) (format t "~a" (sv-string c))) l))

(if (not (ignore-errors (find-method 'sv-substr () ())))
  (defgeneric sv-substr (x s c)
      (:documentation "substring")))
(defmethod sv-substr ((s mp-Undef) start count) "")
(defmethod sv-substr (s start count) 
  (let ((l1 (length s)) (l2 (+ start count)))
  (or (ignore-errors (subseq s start (if (> l2 l1) l1 l2)))
      "")))

(defun sv-index (s substr &optional start) 
  (declare (ignorable start))    ;; TODO
  (let ((l1 (search substr s))) 
    (if l1 l1 -1)))

(defmacro sv-array-index (sv-array sv-ix)
  `(aref 
    (progn
      (loop for i from (length ,sv-array) to ,sv-ix do (vector-push-extend (sv-undef) ,sv-array))
      ,sv-array) 
    ,sv-ix))

(defmacro sv-hash-lookup (key h)
  `(gethash ,key (if (hash-table-p ,h) ,h (sv-hash ,h))))

(if (not (ignore-errors (find-method 'sv-Int () ())))
  (defgeneric sv-Int (x)
      (:documentation "Int()")))
(defmethod sv-Int (x) x)
(defmethod sv-Int ((x string)) (parse-integer x))

(if (not (ignore-errors (find-method 'sv-Num () ())))
  (defgeneric sv-Num (x)
      (:documentation "Num()")))
(defmethod sv-Num (x) x)
(defmethod sv-Num ((x string)) (read-from-string x))

(defmethod sv-string (x) x)
(defmethod sv-string ((x vector)) (sv-join x " "))
(defmethod sv-string ((x number)) (format nil "~a" x))
(defmethod sv-string ((x mp-Undef)) "")

(if (not (ignore-errors (find-method 'sv-eq () ())))
  (defgeneric sv-eq (x y)
      (:documentation "compare string values")))
(defmethod sv-eq (x y)                   (equal (sv-string x) (sv-string y)))
(defmethod sv-eq (x (y string))          (equal (sv-string x) y))
(defmethod sv-eq ((x string) (y string)) (equal x y))
(defmethod sv-eq ((x string) (y number)) (equal x (format nil "~a" y)))
(defmethod sv-eq ((x number) (y string)) (equal (format nil "~a" x) y))

(defmacro create-numeric-op (op-name op-documentation op-symbol)
  `(progn
     (if (not (ignore-errors (find-method ',op-name () ())))
       (defgeneric ,op-name (x y)
           (:documentation ,op-documentation)))
     (defmethod ,op-name (x y)                   (,op-symbol x y))
     (defmethod ,op-name (x (y string))          (,op-symbol x (read-from-string y)))
     (defmethod ,op-name ((x string) (y string)) (,op-symbol (read-from-string x) (read-from-string y)))
     (defmethod ,op-name ((x string) (y number)) (,op-symbol (read-from-string x) y))
     (defmethod ,op-name ((x number) (y string)) (,op-symbol x (read-from-string y)))
     (defmethod ,op-name ((x number) (y number)) (,op-symbol x y))
     (defmethod ,op-name (x (y mp-Undef))        (,op-symbol x 0))
     (defmethod ,op-name ((x mp-Undef) y)        (,op-symbol 0 y))))

(create-numeric-op sv-add                   "add 2 values"               +)
(create-numeric-op sv-sub                   "subtract 2 values"          -)
(create-numeric-op sv-mul                   "multiply 2 values"          *)
(create-numeric-op sv-div                   "divide 2 values"            /)
(create-numeric-op sv-numeric-equal         "compare 2 numeric values" eql)
(create-numeric-op sv-numeric-smaller       "compare 2 numeric values"   <)
(create-numeric-op sv-numeric-bigger        "compare 2 numeric values"   >)
(create-numeric-op sv-numeric-smaller-equal "compare 2 numeric values"  <=)
(create-numeric-op sv-numeric-bigger-equal  "compare 2 numeric values"  >=)

(if (not (ignore-errors (find-method 'sv-bool () ())))
  (defgeneric sv-bool (self)
      (:documentation "get a bool value")))
(defmethod sv-bool (x) x)
(defmethod sv-bool ((x mp-Undef)) nil)
(defmethod sv-bool ((x number)) (not (or (eql x 0) (eql x 0.0))))
(defmethod sv-bool ((x string)) (and (not (equal x "")) (not (equal x "0"))))
(defmethod sv-bool ((x vector)) (not (eql (length x) 0)))

(defmacro sv-and (x y)
 `(and (sv-bool ,x) (sv-bool ,y)))

(defmacro sv-or (x y)
 `(or (sv-bool ,x) (sv-bool ,y)))

(if (not (ignore-errors (find-method 'sv-perl () ())))
  (defgeneric sv-perl (self)
      (:documentation "data dumper")))
(defmethod sv-perl (x)          (format nil "~A" x))
(defmethod sv-perl ((x string)) (format nil "~{~a~}" (list "'" (sv-perl_escape_string x) "'")))
(defmethod sv-perl ((x vector)) (format nil "~{~a~}" (list 
        "[ " 
        (sv-join (map 'vector #'(lambda (c) (sv-perl c)) x))
        " ]" )))
(defmethod sv-perl ((x mp-Undef)) "undef")
(defmethod sv-perl ((x hash-table))
   (format nil "~{~a~}" (list
        "{ "
        (let ((l (make-array 0 :adjustable 1 :fill-pointer t)))
            (maphash #'(lambda (key val) (vector-push-extend (format nil "~A => ~A" (sv-perl key) (sv-perl val)) l)) x)    
            (sv-join l ", " ))
        " }" )))


(if (not (ignore-errors (find-method 'sv-values () ())))
  (defgeneric sv-values (self)
      (:documentation "hash values")))
(defmethod sv-values ((x hash-table))
  (let ((tmp (make-array 0 :adjustable 1 :fill-pointer t)))
    (maphash #'(lambda (key val) 
                  (declare (ignorable key))
                  (sv-push tmp val)) 
             x) 
    tmp ))

(if (not (ignore-errors (find-method 'sv-keys () ())))
  (defgeneric sv-keys (self)
      (:documentation "hash keys")))
(defmethod sv-keys ((x hash-table))
  (let ((tmp (make-array 0 :adjustable 1 :fill-pointer t)))
    (maphash #'(lambda (key val) 
                  (declare (ignorable val))
                  (sv-push tmp key)) 
             x) 
    tmp ))

(defmethod sv-push (a x) 
  (progn 
    (vector-push-extend x a)
    x))

(if (not (ignore-errors (find-method 'sv-unshift () ())))
  (defgeneric sv-unshift (self x)
      (:documentation "unshift")))
(defmethod sv-unshift (a x) 
  (let ((l (length a)))
    (vector-push-extend 0 a)
    (loop for i from 1 to l 
          do (setf (aref a (+ (- l i) 1)) 
                   (aref a (- l i))))
    (setf (aref a 0) x)
    x))

(if (not (ignore-errors (find-method 'sv-shift () ())))
  (defgeneric sv-shift (self)
      (:documentation "shift")))
(defmethod sv-shift (a) 
    (if (eql (length a) 0)
        (sv-Undef)   
        (let (x)
          (setf x (aref a 0))
          (loop for i from 0 to (- (length a) 2) 
              do (setf (aref a i) (aref a (+ i 1))))
          (vector-pop a)
          x)))

(if (not (ignore-errors (find-method 'sv-pop () ())))
  (defgeneric sv-pop (self)
      (:documentation "pop")))
(defmethod sv-pop (a)
    (if (eql (length a) 0)
        (sv-Undef)   
        (vector-pop a)))

(if (not (ignore-errors (find-method 'sv-scalar () ())))
  (defgeneric sv-scalar (self)
      (:documentation "get a scalar value")))
(defmethod sv-scalar (x) x)

;; Grammars

(if (not (ignore-errors (find-class 'mp-Perlito-Grammar)))
  (defclass mp-Perlito-Grammar () ()))
(let (x)
  (setq x (make-instance 'mp-Perlito-Grammar))
  (defun proto-mp-Perlito-Grammar () x))

;; token <space>
(if (not (ignore-errors (find-method 'sv-space () ())))
   (defgeneric sv-space (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-space ((sv-grammar mp-Perlito-Grammar) &optional sv-str sv-pos)
    (if (ignore-errors (or (char= (aref sv-str sv-pos) #\Space) (char= (aref sv-str sv-pos) #\Tab)))
         (let ((m (make-instance 'mp-Perlito-Match))) 
            (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) (+ sv-pos 1))(setf (sv-bool m) 1) m)
         (let ((m (make-instance 'mp-Perlito-Match))) 
            (setf (sv-bool m) nil) m)))

;; token <digit>
(if (not (ignore-errors (find-method 'sv-digit () ())))
  (defgeneric sv-digit (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-digit ((sv-grammar mp-Perlito-Grammar) &optional sv-str sv-pos)
    (if (ignore-errors (digit-char-p (aref sv-str sv-pos)))
         (let ((m (make-instance 'mp-Perlito-Match))) 
            (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) (+ sv-pos 1))(setf (sv-bool m) 1) m)
         (let ((m (make-instance 'mp-Perlito-Match))) 
            (setf (sv-bool m) nil) m)))

;; token <word>
(if (not (ignore-errors (find-method 'sv-word () ())))
  (defgeneric sv-word (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-word ((sv-grammar mp-Perlito-Grammar) &optional sv-str sv-pos)
    (if (ignore-errors (or (alphanumericp (aref sv-str sv-pos)) (char= (aref sv-str sv-pos) #\_)))
         (let ((m (make-instance 'mp-Perlito-Match))) 
            (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) (+ sv-pos 1))(setf (sv-bool m) 1) m)
         (let ((m (make-instance 'mp-Perlito-Match))) 
            (setf (sv-bool m) nil) m)))

;; token <is_newline>
(if (not (ignore-errors (find-method 'sv-is_newline () ())))
   (defgeneric sv-is_newline (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-is_newline ((sv-grammar mp-Perlito-Grammar) &optional sv-str sv-pos)
  (let (from)
    (setq from sv-pos)
    (if (ignore-errors (char= (aref sv-str sv-pos) #\Return))
         (progn (setf sv-pos (+ sv-pos 1))
                (if (ignore-errors (char= (aref sv-str sv-pos) #\Newline)) (setf sv-pos (+ sv-pos 1)))
                (let ((m (make-instance 'mp-Perlito-Match))) 
                    (setf (sv-str m) sv-str)(setf (sv-from m) from)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))
         (if (ignore-errors (char= (aref sv-str sv-pos) #\Newline))
              (progn (setf sv-pos (+ sv-pos 1))
                     (if (ignore-errors (char= (aref sv-str sv-pos) #\Return)) (setf sv-pos (+ sv-pos 1)))
                     (let ((m (make-instance 'mp-Perlito-Match))) 
                         (setf (sv-str m) sv-str)(setf (sv-from m) from)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))
              (let ((m (make-instance 'mp-Perlito-Match))) 
                 (setf (sv-bool m) nil) m)))))

;; token <not_newline>
(if (not (ignore-errors (find-method 'sv-not_newline () ())))
   (defgeneric sv-not_newline (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-not_newline ((sv-grammar mp-Perlito-Grammar) &optional sv-str sv-pos)
    (if (not (ignore-errors (or (char= (aref sv-str sv-pos) #\Return) (char= (aref sv-str sv-pos) #\Newline))))
         (let ((m (make-instance 'mp-Perlito-Match))) 
            (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) (+ sv-pos 1))(setf (sv-bool m) 1) m)
         (let ((m (make-instance 'mp-Perlito-Match))) 
            (setf (sv-bool m) nil) m)))


;; Match objects

(if (not (ignore-errors (find-class 'mp-Perlito-Match)))
  (defclass mp-Perlito-Match () 
    (hash array)))

(defvar sv-MATCH (make-instance 'mp-Perlito-Match))

(defmethod sv-hash ((m mp-Perlito-Match)) 
  (or 
    (ignore-errors (slot-value m 'hash))
    (setf (slot-value m 'hash) (make-hash-table :test 'equal))))

(defmethod sv-array ((m mp-Perlito-Match)) 
  (or 
    (ignore-errors (slot-value m 'array))
    (setf (slot-value m 'array) (make-array 0 :adjustable 1))))
    ;; (setf (slot-value m 'array) (list (sv-undef) (sv-undef) (sv-undef)))))

;; compiler utils

;; function replace-substring pasted from: 
;;   http://web.mit.edu/maxima_v5.13.0/src/maxima-5.13.0/configure.lisp
(defun replace-substring (in-string old new) 
  (let ((result ""))
    (do ((begin 0)
     (end (search old in-string) 
          (search old in-string :start2 begin)))
    ((>= begin (length in-string)) 'done)
      (if end
      (progn (setf result (concatenate 'string result 
                       (subseq in-string begin end)
                       new))
         (setf begin (+ end (length old))))
      (progn (setf result (concatenate 'string result 
                       (subseq in-string begin
                           (length in-string))))
         (setf begin (length in-string)))))
    result))

(if (not (ignore-errors (find-method 'sv-lisp_escape_string () ())))
  (defgeneric sv-lisp_escape_string (self)
      (:documentation "escape a lisp string value")))
(defmethod sv-lisp_escape_string ((s string)) 
    (replace-substring
        (replace-substring s "\\" "\\\\")
                             "\"" "\\\""))
(defun mp-Main-sv-lisp_escape_string (s)
  (sv-lisp_escape_string s))

(defmethod sv-perl_escape_string ((s string)) 
    (replace-substring
        (replace-substring s "\\" "\\\\")
                             "'" "\\\'"))
(defun mp-Main-sv-perl_escape_string (s)
  (sv-perl_escape_string s))

(defmethod sv-javascript_escape_string ((s string)) 
    (replace-substring
      (replace-substring
        (replace-substring s "\\" "\\\\")
                             "\"" "\\\"")
                             "
" "\\n"))
(defun mp-Main-sv-javascript_escape_string (s)
  (sv-javascript_escape_string s))

(if (not (ignore-errors (find-method 'sv-to_lisp_namespace () ())))
  (defgeneric sv-to_lisp_namespace (self)
      (:documentation "escape a lisp namespace string")))
(defmethod sv-to_lisp_namespace ((s string)) 
    (format nil "mp-~a" (replace-substring s "::" "-")))
(defun mp-Main-sv-to_lisp_namespace (s)
  (sv-to_lisp_namespace s))

(if (not (ignore-errors (find-method 'sv-to_go_namespace () ())))
  (defgeneric sv-to_go_namespace (self)
      (:documentation "escape a lisp namespace string")))
(defmethod sv-to_go_namespace ((s string)) 
    (format nil "mp-~a" (replace-substring s "::" "__")))
(defun mp-Main-sv-to_go_namespace (s)
  (sv-to_go_namespace s))

(if (not (ignore-errors (find-method 'sv-to_javascript_namespace () ())))
  (defgeneric sv-to_javascript_namespace (self)
      (:documentation "escape a lisp namespace string")))
(defmethod sv-to_javascript_namespace ((s string)) 
    (format nil "mp-~a" (replace-substring s "::" "$")))
(defun mp-Main-sv-to_javascript_namespace (s)
  (sv-to_javascript_namespace s))

(defmethod sv-join ((l string) &optional (delim "")) 
  (declare (ignorable delim))
  l)
(defmethod sv-join ((v vector) &optional (delim ""))
  (with-output-to-string (s)
    (when v
        (if (> (length v) 0)
          (progn
            (format s "~A" (sv-string (aref v 0)))
            (loop for i from 1 to (- (length v) 1) 
              do (format s "~A~A" delim (aref v i))))
          ""))))

;; IO

(defpackage mp-IO
  (:use common-lisp mp-Main))

(in-package mp-Main)
(defun sv-slurp (sv-filename)
  (format nil "~{~a~%~}"
    (with-open-file (s sv-filename)
      (loop for line = (read-line s nil nil)
            while line
            collect line into lines
            finally (return lines)))))
(defun mp-io-sv-slurp (s)
  (sv-slurp s))

(in-package mp-IO)
  (defun sv-slurp (&optional sv-filename )
    (mp-Main::sv-slurp sv-filename ))
(in-package mp-Main)

