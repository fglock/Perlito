
;; MiniPerl6 Lisp runtime
;;
;; Author: Flavio Soibelmann Glock <fglock@gmail.com>
;;
;; Copyright 2009 by Flavio Soibelmann Glock and others.
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
        #:sv-defined #:sv-array-index #:sv-hash-lookup))
(in-package mp-Main)

(setf COMMON-LISP-USER::*posix-argv* (cdr COMMON-LISP-USER::*posix-argv*))
(defparameter *mp6-args* (make-array 
                            (length COMMON-LISP-USER::*posix-argv*) 
                            :adjustable 1 
                            :fill-pointer t 
                            :initial-contents COMMON-LISP-USER::*posix-argv*))

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

(defun sv-say (l)
  (progn
    (map nil #'(lambda (c) (format t "~a" (sv-string c))) l)
    (format t "~%" nil)))
;;  (format t "~a~%" (sv-join l "")))

(defun sv-print (l)
  (map nil #'(lambda (c) (format t "~a" (sv-string c))) l))
;;  (format t "~a" (sv-join l "")))

(defun sv-substr (s start count) 
  (let ((l1 (length s)) (l2 (+ start count)))
  (or (ignore-errors (subseq s start (if (> l2 l1) l1 l2)))
      "")))

(defun sv-index (s substr &optional start) 
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
(defmethod sv-Num ((x string)) (parse-integer x))

(if (not (ignore-errors (find-method 'sv-string () ())))
  (defgeneric sv-string (x)
      (:documentation "stringify values")))
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

(if (not (ignore-errors (find-method 'sv-eq-int () ())))
  (defgeneric sv-eq-int (x y)
      (:documentation "compare int values")))
;; (defmethod sv-eq (x y) (eql x y))
(defmethod sv-eq-int (x y)                   (eql x y))
(defmethod sv-eq-int (x (y string))          (eql x (parse-integer y)))
(defmethod sv-eq-int ((x string) (y string)) (eql (parse-integer x) (parse-integer y)))
(defmethod sv-eq-int ((x string) (y number)) (eql (parse-integer x) y))
(defmethod sv-eq-int ((x number) (y string)) (eql x (parse-integer y)))
(defmethod sv-eq-int ((x number) (y number)) (eql x y))

(if (not (ignore-errors (find-method 'sv-bool () ())))
  (defgeneric sv-bool (self)
      (:documentation "get a bool value")))
(defmethod sv-bool (x) x)
(defmethod sv-bool ((x mp-Undef)) nil)
(defmethod sv-bool ((x number)) (not (eql x 0)))
(defmethod sv-bool ((x string)) (and (not (equal x "")) (not (equal x "0"))))

(defmacro sv-and (x y)
 `(and (sv-bool ,x) (sv-bool ,y)))

(defmacro sv-or (x y)
 `(or (sv-bool ,x) (sv-bool ,y)))

(if (not (ignore-errors (find-method 'sv-perl () ())))
  (defgeneric sv-perl (self)
      (:documentation "data dumper")))
(defmethod sv-perl (x)          (format nil "~A" x))
(defmethod sv-perl ((x string)) (format nil "~{~a~}" (list "'" (sv-perl_escape_string x) "'")))
(defmethod sv-perl ((x vector)) (format nil "~{~a~}" (list "[ " (sv-join (mapcar #'sv-perl x) ", ") " ]" )))
(defmethod sv-perl ((x mp-Undef)) "undef")
(defmethod sv-perl ((x hash-table))
   (format nil "~{~a~}" (list
        "{ " 
        (let (l) 
            (maphash #'(lambda (key val) (push (format nil "~A => ~A" (sv-perl key) (sv-perl val)) l)) x) 
            (sv-join l ", " ))
        " }" )))

(defmethod sv-values ((x hash-table))
  (let ((tmp (make-array 0 :adjustable 1 :fill-pointer t)))
    (maphash #'(lambda (key val) (push val tmp)) x) 
    tmp ))

(defmethod sv-keys ((x hash-table))
  (let ((tmp (make-array 0 :adjustable 1 :fill-pointer t)))
    (maphash #'(lambda (key val) (push key tmp)) x) 
    tmp ))

(defmethod sv-push (a x) 
  (vector-push-extend x a))

(if (not (ignore-errors (find-method 'sv-scalar () ())))
  (defgeneric sv-scalar (self)
      (:documentation "get a scalar value")))
(defmethod sv-scalar (x) x)

;; Grammars

(if (not (ignore-errors (find-class 'mp-MiniPerl6-Grammar)))
  (defclass mp-MiniPerl6-Grammar () ()))
(let (x)
  (setq x (make-instance 'mp-MiniPerl6-Grammar))
  (defun proto-mp-MiniPerl6-Grammar () x))

;; token <space>
(if (not (ignore-errors (find-method 'sv-space () ())))
   (defgeneric sv-space (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-space ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
    (if (ignore-errors (or (char= (aref sv-str sv-pos) #\Space) (char= (aref sv-str sv-pos) #\Tab)))
         (let ((m (make-instance 'mp-MiniPerl6-Match))) 
            (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) (+ sv-pos 1))(setf (sv-bool m) 1) m)
         (let ((m (make-instance 'mp-MiniPerl6-Match))) 
            (setf (sv-bool m) nil) m)))

;; token <digit>
(if (not (ignore-errors (find-method 'sv-digit () ())))
  (defgeneric sv-digit (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-digit ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
    (if (ignore-errors (digit-char-p (aref sv-str sv-pos)))
         (let ((m (make-instance 'mp-MiniPerl6-Match))) 
            (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) (+ sv-pos 1))(setf (sv-bool m) 1) m)
         (let ((m (make-instance 'mp-MiniPerl6-Match))) 
            (setf (sv-bool m) nil) m)))

;; token <word>
(if (not (ignore-errors (find-method 'sv-word () ())))
  (defgeneric sv-word (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-word ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
    (if (ignore-errors (or (alphanumericp (aref sv-str sv-pos)) (char= (aref sv-str sv-pos) #\_)))
         (let ((m (make-instance 'mp-MiniPerl6-Match))) 
            (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) (+ sv-pos 1))(setf (sv-bool m) 1) m)
         (let ((m (make-instance 'mp-MiniPerl6-Match))) 
            (setf (sv-bool m) nil) m)))

;; token <is_newline>
(if (not (ignore-errors (find-method 'sv-is_newline () ())))
   (defgeneric sv-is_newline (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-is_newline ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
    (if (ignore-errors (char= (aref sv-str sv-pos) #\Return))
         (progn (setf sv-pos (+ sv-pos 1))
                (if (ignore-errors (char= (aref sv-str sv-pos) #\Newline)) (setf sv-pos (+ sv-pos 1)))
                (let ((m (make-instance 'mp-MiniPerl6-Match))) 
                    (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))
    (if (ignore-errors (char= (aref sv-str sv-pos) #\Newline))
         (progn (setf sv-pos (+ sv-pos 1))
                (if (ignore-errors (char= (aref sv-str sv-pos) #\Return)) (setf sv-pos (+ sv-pos 1)))
                (let ((m (make-instance 'mp-MiniPerl6-Match))) 
                    (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))
         (let ((m (make-instance 'mp-MiniPerl6-Match))) 
            (setf (sv-bool m) nil) m))))

;; token <not_newline>
(if (not (ignore-errors (find-method 'sv-not_newline () ())))
   (defgeneric sv-not_newline (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-not_newline ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
    (if (not (ignore-errors (or (char= (aref sv-str sv-pos) #\Return) (char= (aref sv-str sv-pos) #\Newline))))
         (let ((m (make-instance 'mp-MiniPerl6-Match))) 
            (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) (+ sv-pos 1))(setf (sv-bool m) 1) m)
         (let ((m (make-instance 'mp-MiniPerl6-Match))) 
            (setf (sv-bool m) nil) m)))


;; Match objects

(if (not (ignore-errors (find-class 'mp-MiniPerl6-Match)))
  (defclass mp-MiniPerl6-Match () 
    (hash array)))

(defvar sv-MATCH (make-instance 'mp-MiniPerl6-Match))

(defmethod sv-hash ((m mp-MiniPerl6-Match)) 
  (or 
    (ignore-errors (slot-value m 'hash))
    (setf (slot-value m 'hash) (make-hash-table :test 'equal))))

(if (not (ignore-errors (find-method 'sv-array () ())))
  (defgeneric sv-array (self)
      (:documentation "get an array value")))
(defmethod sv-array ((m mp-MiniPerl6-Match)) 
  (or 
    (ignore-errors (slot-value m 'array))
    (setf (slot-value m 'array) (make-array 0 :adjustable 1))))
    ;; (setf (slot-value m 'array) (list (sv-undef) (sv-undef) (sv-undef)))))

;; compiler utils

(if (not (ignore-errors (find-method 'sv-newline () ())))
  (defgeneric sv-newline (class)
      (:documentation "the newline string")))
(defmethod sv-newline (class)
  (format nil "~%"))

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
  (defgeneric sv-hash (self)
      (:documentation "escape a lisp string value")))
(defmethod sv-lisp_escape_string ((s string)) 
    (replace-substring
        (replace-substring s "\\" "\\\\")
                             "\"" "\\\""))

(if (not (ignore-errors (find-method 'sv-perl_escape_string () ())))
  (defgeneric sv-hash (self)
      (:documentation "escape a single quoted perl string value")))
(defmethod sv-perl_escape_string ((s string)) 
    (replace-substring
        (replace-substring s "\\" "\\\\")
                             "'" "\\\'"))

(if (not (ignore-errors (find-method 'sv-to_lisp_namespace () ())))
  (defgeneric sv-hash (self)
      (:documentation "escape a lisp namespace string")))
(defmethod sv-to_lisp_namespace ((s string)) 
    (format nil "mp-~a" (replace-substring s "::" "-")))

(if (not (ignore-errors (find-method 'sv-join () ())))
  (defgeneric sv-join (l &optional delim)
      (:documentation "list join")))
;;(defmethod sv-join (l &optional (delim ""))
;;  (sv-string l))
(defmethod sv-join ((l string) &optional (delim "")) l)
(defmethod sv-join ((v vector) &optional (delim ""))
  (with-output-to-string (s)
    (when v
        (if (> (length v) 0)
          (progn
            (format s "~A" (sv-string (aref v 0)))
            (loop for i from 1 to (- (length v) 1) 
              do (format s "~A~A" delim (aref v i))))
          ""))))

;; Do not edit this file - Generated by MiniPerl6 3.0
(defpackage mp-MiniPerl6-Match
  (:use common-lisp mp-Main))
(defpackage mp-Pair
  (:use common-lisp mp-Main))
(defpackage mp-Main
  (:use common-lisp mp-Main))
;; class MiniPerl6::Match
(if (not (ignore-errors (find-class 'mp-MiniPerl6-Match)))
  (defclass mp-MiniPerl6-Match () ()))

(let (x) 
  (setq x (make-instance 'mp-MiniPerl6-Match))
  (defun proto-mp-MiniPerl6-Match () x))
;; has $.from
(let ((new-slots (list (list :name 'sv-from
  :readers '(sv-from)
  :writers '((setf sv-from))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-MiniPerl6-Match)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-MiniPerl6-Match :direct-slots new-slots))

;; has $.to
(let ((new-slots (list (list :name 'sv-to
  :readers '(sv-to)
  :writers '((setf sv-to))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-MiniPerl6-Match)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-MiniPerl6-Match :direct-slots new-slots))

;; has $.str
(let ((new-slots (list (list :name 'sv-str
  :readers '(sv-str)
  :writers '((setf sv-str))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-MiniPerl6-Match)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-MiniPerl6-Match :direct-slots new-slots))

;; has $.bool
(let ((new-slots (list (list :name 'sv-bool
  :readers '(sv-bool)
  :writers '((setf sv-bool))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-MiniPerl6-Match)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-MiniPerl6-Match :direct-slots new-slots))

;; has $.capture
(let ((new-slots (list (list :name 'sv-capture
  :readers '(sv-capture)
  :writers '((setf sv-capture))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-MiniPerl6-Match)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-MiniPerl6-Match :direct-slots new-slots))

;; method scalar
(if (not (ignore-errors (find-method 'sv-scalar () ())))
  (defgeneric sv-scalar (sv-self)
      (:documentation "a method")))
(defmethod sv-scalar ((sv-self mp-MiniPerl6-Match))
  (block mp6-function
    (progn (if (sv-bool (sv-bool sv-self)) (progn (if (sv-bool (sv-defined (sv-capture sv-self))) (progn (return-from mp6-function (sv-capture sv-self))) nil)(return-from mp6-function (sv-substr (sv-str sv-self) (sv-from sv-self) (-(sv-to sv-self) (sv-from sv-self))))) (progn (return-from mp6-function ""))))))

;; method string
(if (not (ignore-errors (find-method 'sv-string () ())))
  (defgeneric sv-string (sv-self)
      (:documentation "a method")))
(defmethod sv-string ((sv-self mp-MiniPerl6-Match))
  (block mp6-function
    (progn (if (sv-bool (sv-bool sv-self)) (progn (if (sv-bool (sv-defined (sv-capture sv-self))) (progn (return-from mp6-function (sv-capture sv-self))) nil)(return-from mp6-function (sv-substr (sv-str sv-self) (sv-from sv-self) (-(sv-to sv-self) (sv-from sv-self))))) (progn (return-from mp6-function ""))))))

(defmethod sv-perl ((self mp-MiniPerl6-Match))
  (mp-Main::sv-lisp_dump_object "::MiniPerl6::Match" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "from") (setf (sv-value m) (sv-from self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "to") (setf (sv-value m) (sv-to self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "str") (setf (sv-value m) (sv-str self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "bool") (setf (sv-value m) (sv-bool self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "capture") (setf (sv-value m) (sv-capture self)) m) )))




;; class Pair
(if (not (ignore-errors (find-class 'mp-Pair)))
  (defclass mp-Pair () ()))

(let (x) 
  (setq x (make-instance 'mp-Pair))
  (defun proto-mp-Pair () x))
;; has $.key
(let ((new-slots (list (list :name 'sv-key
  :readers '(sv-key)
  :writers '((setf sv-key))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Pair)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Pair :direct-slots new-slots))

;; has $.value
(let ((new-slots (list (list :name 'sv-value
  :readers '(sv-value)
  :writers '((setf sv-value))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Pair)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Pair :direct-slots new-slots))

;; method perl
(if (not (ignore-errors (find-method 'sv-perl () ())))
  (defgeneric sv-perl (sv-self)
      (:documentation "a method")))
(defmethod sv-perl ((sv-self mp-Pair))
  (block mp6-function
    (progn (return-from mp6-function (concatenate 'string (sv-string (sv-key sv-self)) (sv-string (concatenate 'string (sv-string " => ") (sv-string (sv-perl (sv-value sv-self) )))))))))




;; class Main
(if (not (ignore-errors (find-class 'mp-Main)))
  (defclass mp-Main () ()))

(let (x) 
  (setq x (make-instance 'mp-Main))
  (defun proto-mp-Main () x))
(in-package mp-Main)
  (defun sv-to_lisp_identifier (&optional sv-ident )
  (block mp6-function (progn (return-from mp6-function (concatenate 'string (sv-string "sv-") (sv-string sv-ident))))))

(in-package mp-Main)
(in-package mp-Main)
  (defun sv-lisp_dump_object (&optional sv-class_name sv-data )
  (block mp6-function (progn (return-from mp6-function (concatenate 'string (sv-string sv-class_name) (sv-string (concatenate 'string (sv-string "( ") (sv-string (concatenate 'string (sv-string (sv-join (let ((tmp (make-array 0 :adjustable 1 :fill-pointer t))) (map nil #'(lambda (c) (push (sv-perl  c) tmp)) sv-data) tmp ) ", ")) (sv-string " )"))))))))))

(in-package mp-Main)
(defmethod sv-perl ((self mp-Main))
  (mp-Main::sv-lisp_dump_object "::Main" (list )))




;; Do not edit this file - Generated by MiniPerl6 3.0
(defpackage mp-MiniPerl6-Lisp-LexicalBlock
  (:use common-lisp mp-Main))
(defpackage mp-CompUnit
  (:use common-lisp mp-Main))
(defpackage mp-Val-Int
  (:use common-lisp mp-Main))
(defpackage mp-Val-Bit
  (:use common-lisp mp-Main))
(defpackage mp-Val-Num
  (:use common-lisp mp-Main))
(defpackage mp-Val-Buf
  (:use common-lisp mp-Main))
(defpackage mp-Val-Undef
  (:use common-lisp mp-Main))
(defpackage mp-Val-Object
  (:use common-lisp mp-Main))
(defpackage mp-Lit-Seq
  (:use common-lisp mp-Main))
(defpackage mp-Lit-Array
  (:use common-lisp mp-Main))
(defpackage mp-Lit-Hash
  (:use common-lisp mp-Main))
(defpackage mp-Lit-Code
  (:use common-lisp mp-Main))
(defpackage mp-Lit-Object
  (:use common-lisp mp-Main))
(defpackage mp-Index
  (:use common-lisp mp-Main))
(defpackage mp-Lookup
  (:use common-lisp mp-Main))
(defpackage mp-Var
  (:use common-lisp mp-Main))
(defpackage mp-Bind
  (:use common-lisp mp-Main))
(defpackage mp-Proto
  (:use common-lisp mp-Main))
(defpackage mp-Call
  (:use common-lisp mp-Main))
(defpackage mp-Apply
  (:use common-lisp mp-Main))
(defpackage mp-Return
  (:use common-lisp mp-Main))
(defpackage mp-If
  (:use common-lisp mp-Main))
(defpackage mp-For
  (:use common-lisp mp-Main))
(defpackage mp-Decl
  (:use common-lisp mp-Main))
(defpackage mp-Sig
  (:use common-lisp mp-Main))
(defpackage mp-Method
  (:use common-lisp mp-Main))
(defpackage mp-Sub
  (:use common-lisp mp-Main))
(defpackage mp-Do
  (:use common-lisp mp-Main))
(defpackage mp-Use
  (:use common-lisp mp-Main))
;; class MiniPerl6::Lisp::LexicalBlock
(if (not (ignore-errors (find-class 'mp-MiniPerl6-Lisp-LexicalBlock)))
  (defclass mp-MiniPerl6-Lisp-LexicalBlock () ()))

(let (x) 
  (setq x (make-instance 'mp-MiniPerl6-Lisp-LexicalBlock))
  (defun proto-mp-MiniPerl6-Lisp-LexicalBlock () x))
;; has $.block
(let ((new-slots (list (list :name 'sv-block
  :readers '(sv-block)
  :writers '((setf sv-block))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-MiniPerl6-Lisp-LexicalBlock)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-MiniPerl6-Lisp-LexicalBlock :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-MiniPerl6-Lisp-LexicalBlock))
  (block mp6-function
    (let ((sv-str (sv-undef))(sv-has_my_decl (sv-undef))(sv-my_decl (sv-undef))(sv-decl_seen (make-hash-table :test 'equal))) (if (sv-bool (not (sv-bool (sv-block sv-self)))) (progn (return-from mp6-function "nil")) nil)(setf sv-str "")(setf sv-has_my_decl 0)(setf sv-my_decl "")(loop for sv-decl across (sv-block sv-self) do (progn (if (sv-bool (sv-and (typep sv-decl 'mp-Decl) (sv-eq (sv-decl sv-decl ) "my"))) (let ((sv-var_name (sv-undef))) (setf sv-var_name (sv-emit_lisp (sv-var sv-decl ) ))(if (sv-bool (not (sv-bool (mp-Main::sv-hash-lookup sv-var_name sv-decl_seen)))) (progn (setf sv-has_my_decl 1)(setf sv-my_decl (concatenate 'string (sv-string sv-my_decl) (sv-string (mp-Decl::sv-emit_lisp_initializer (sv-var sv-decl )))))(setf (mp-Main::sv-hash-lookup sv-var_name sv-decl_seen) 1)) nil)) nil)(if (sv-bool (sv-and (typep sv-decl 'mp-Bind) (sv-and (typep (sv-parameters sv-decl ) 'mp-Decl) (sv-eq (sv-decl (sv-parameters sv-decl ) ) "my")))) (let ((sv-var_name (sv-undef))) (setf sv-var_name (sv-emit_lisp (sv-var (sv-parameters sv-decl ) ) ))(if (sv-bool (not (sv-bool (mp-Main::sv-hash-lookup sv-var_name sv-decl_seen)))) (progn (setf sv-has_my_decl 1)(setf sv-my_decl (concatenate 'string (sv-string sv-my_decl) (sv-string (mp-Decl::sv-emit_lisp_initializer (sv-var (sv-parameters sv-decl ) )))))(setf (mp-Main::sv-hash-lookup sv-var_name sv-decl_seen) 1)) nil)) nil)))(if (sv-bool sv-has_my_decl) (progn (setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string "(let (") (sv-string (concatenate 'string (sv-string sv-my_decl) (sv-string ") ")))))))) (progn (setf sv-str (concatenate 'string (sv-string sv-str) (sv-string "(progn ")))))(loop for sv-decl across (sv-block sv-self) do (progn (if (sv-bool (not (sv-bool (sv-and (typep sv-decl 'mp-Decl) (sv-eq (sv-decl sv-decl ) "my"))))) (progn (setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (sv-emit_lisp sv-decl ))))) nil)))(return-from mp6-function (concatenate 'string (sv-string sv-str) (sv-string ")"))))))

(defmethod sv-perl ((self mp-MiniPerl6-Lisp-LexicalBlock))
  (mp-Main::sv-lisp_dump_object "::MiniPerl6::Lisp::LexicalBlock" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "block") (setf (sv-value m) (sv-block self)) m) )))




;; class CompUnit
(if (not (ignore-errors (find-class 'mp-CompUnit)))
  (defclass mp-CompUnit () ()))

(let (x) 
  (setq x (make-instance 'mp-CompUnit))
  (defun proto-mp-CompUnit () x))
;; has $.name
(let ((new-slots (list (list :name 'sv-name
  :readers '(sv-name)
  :writers '((setf sv-name))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-CompUnit)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-CompUnit :direct-slots new-slots))

;; has $.attributes
(let ((new-slots (list (list :name 'sv-attributes
  :readers '(sv-attributes)
  :writers '((setf sv-attributes))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-CompUnit)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-CompUnit :direct-slots new-slots))

;; has $.methods
(let ((new-slots (list (list :name 'sv-methods
  :readers '(sv-methods)
  :writers '((setf sv-methods))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-CompUnit)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-CompUnit :direct-slots new-slots))

;; has $.body
(let ((new-slots (list (list :name 'sv-body
  :readers '(sv-body)
  :writers '((setf sv-body))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-CompUnit)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-CompUnit :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-CompUnit))
  (block mp6-function
    (let ((sv-class_name (sv-undef))(sv-str (sv-undef))(sv-has_my_decl (sv-undef))(sv-my_decl (sv-undef))(sv-decl_seen (make-hash-table :test 'equal))(sv-dumper (sv-undef))) (setf sv-class_name (mp-Main::sv-to_lisp_namespace (sv-name sv-self)))(setf sv-str (concatenate 'string (sv-string ";; class ") (sv-string (concatenate 'string (sv-string (sv-name sv-self)) (sv-string (sv-newline (proto-mp-Main) ))))))(setf sv-has_my_decl 0)(setf sv-my_decl "")(loop for sv-decl across (sv-body sv-self) do (progn (if (sv-bool (sv-and (typep sv-decl 'mp-Decl) (sv-eq (sv-decl sv-decl ) "my"))) (let ((sv-var_name (sv-undef))) (setf sv-var_name (sv-emit_lisp (sv-var sv-decl ) ))(if (sv-bool (not (sv-bool (mp-Main::sv-hash-lookup sv-var_name sv-decl_seen)))) (progn (setf sv-has_my_decl 1)(setf sv-my_decl (concatenate 'string (sv-string sv-my_decl) (sv-string (mp-Decl::sv-emit_lisp_initializer (sv-var sv-decl )))))(setf (mp-Main::sv-hash-lookup sv-var_name sv-decl_seen) 1)) nil)) nil)(if (sv-bool (sv-and (typep sv-decl 'mp-Bind) (sv-and (typep (sv-parameters sv-decl ) 'mp-Decl) (sv-eq (sv-decl (sv-parameters sv-decl ) ) "my")))) (let ((sv-var_name (sv-undef))) (setf sv-var_name (sv-emit_lisp (sv-var (sv-parameters sv-decl ) ) ))(if (sv-bool (not (sv-bool (mp-Main::sv-hash-lookup sv-var_name sv-decl_seen)))) (progn (setf sv-has_my_decl 1)(setf sv-my_decl (concatenate 'string (sv-string sv-my_decl) (sv-string (mp-Decl::sv-emit_lisp_initializer (sv-var (sv-parameters sv-decl ) )))))(setf (mp-Main::sv-hash-lookup sv-var_name sv-decl_seen) 1)) nil)) nil)))(if (sv-bool sv-has_my_decl) (progn (setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string "(let (") (sv-string (concatenate 'string (sv-string sv-my_decl) (sv-string (concatenate 'string (sv-string ")") (sv-string (sv-newline (proto-mp-Main) ))))))))))) nil)(setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string "(if (not (ignore-errors (find-class '") (sv-string (concatenate 'string (sv-string sv-class_name) (sv-string (concatenate 'string (sv-string ")))
  (defclass ") (sv-string (concatenate 'string (sv-string sv-class_name) (sv-string (concatenate 'string (sv-string " () ()))

(let (x) 
  (setq x (make-instance '") (sv-string (concatenate 'string (sv-string sv-class_name) (sv-string (concatenate 'string (sv-string "))
  (defun proto-") (sv-string (concatenate 'string (sv-string sv-class_name) (sv-string " () x))
")))))))))))))))))))(setf sv-dumper "")(loop for sv-decl across (sv-body sv-self) do (progn (if (sv-bool (sv-and (typep sv-decl 'mp-Decl) (sv-eq (sv-decl sv-decl ) "has"))) (let ((sv-accessor_name (sv-undef))) (setf sv-accessor_name (sv-name (sv-var sv-decl ) ))(setf sv-dumper (concatenate 'string (sv-string sv-dumper) (sv-string (concatenate 'string (sv-string "(let ((m (make-instance 'mp-Pair))) ") (sv-string (concatenate 'string (sv-string "(setf (sv-key m) \"") (sv-string (concatenate 'string (sv-string (mp-Main::sv-lisp_escape_string sv-accessor_name)) (sv-string (concatenate 'string (sv-string "\") ") (sv-string (concatenate 'string (sv-string "(setf (sv-value m) (") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_identifier sv-accessor_name)) (sv-string " self)) m) ")))))))))))))))(setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string ";; has $.") (sv-string (concatenate 'string (sv-string sv-accessor_name) (sv-string (concatenate 'string (sv-string "
(let ((new-slots (list (list :name '") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_identifier sv-accessor_name)) (sv-string (concatenate 'string (sv-string "
  :readers '(") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_identifier sv-accessor_name)) (sv-string (concatenate 'string (sv-string ")
  :writers '((setf ") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_identifier sv-accessor_name)) (sv-string (concatenate 'string (sv-string "))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class '") (sv-string (concatenate 'string (sv-string sv-class_name) (sv-string (concatenate 'string (sv-string ")))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class '") (sv-string (concatenate 'string (sv-string sv-class_name) (sv-string " :direct-slots new-slots))

")))))))))))))))))))))))))))) nil)(if (sv-bool (typep sv-decl 'mp-Method)) (let ((sv-sig (sv-undef))(sv-invocant (sv-undef))(sv-pos (sv-undef))(sv-str_specific (sv-undef))(sv-str_generic (sv-undef))(sv-str_optionals (sv-undef))(sv-block (sv-undef))) (setf sv-sig (sv-sig sv-decl ))(setf sv-invocant (sv-invocant sv-sig ))(setf sv-pos (sv-positional sv-sig ))(setf sv-str_specific (concatenate 'string (sv-string "(") (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-invocant )) (sv-string (concatenate 'string (sv-string " ") (sv-string (concatenate 'string (sv-string sv-class_name) (sv-string ")")))))))))(setf sv-str_generic (sv-emit_lisp sv-invocant ))(setf sv-str_optionals "")(loop for sv-field across sv-pos do (progn (setf sv-str_optionals (concatenate 'string (sv-string sv-str_optionals) (sv-string (concatenate 'string (sv-string " ") (sv-string (sv-emit_lisp sv-field ))))))))(if (sv-bool sv-str_optionals) (progn (setf sv-str_specific (concatenate 'string (sv-string sv-str_specific) (sv-string (concatenate 'string (sv-string " &optional") (sv-string sv-str_optionals)))))(setf sv-str_generic (concatenate 'string (sv-string sv-str_generic) (sv-string (concatenate 'string (sv-string " &optional") (sv-string sv-str_optionals)))))) nil)(setf sv-block (let ((m (make-instance 'mp-MiniPerl6-Lisp-LexicalBlock))) (setf (sv-block m) (sv-block sv-decl )) m))(setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string ";; method ") (sv-string (concatenate 'string (sv-string (sv-name sv-decl )) (sv-string (concatenate 'string (sv-string "
(if (not (ignore-errors (find-method '") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_identifier (sv-name sv-decl ))) (sv-string (concatenate 'string (sv-string " () ())))
  (defgeneric ") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_identifier (sv-name sv-decl ))) (sv-string (concatenate 'string (sv-string " (") (sv-string (concatenate 'string (sv-string sv-str_generic) (sv-string (concatenate 'string (sv-string ")") (sv-string (sv-newline (proto-mp-Main) ))))))))))))))))))))))(setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string "      (:documentation ") (sv-string (concatenate 'string (sv-string "\"") (sv-string (concatenate 'string (sv-string "a method") (sv-string (concatenate 'string (sv-string "\"") (sv-string (concatenate 'string (sv-string ")))
(defmethod ") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_identifier (sv-name sv-decl ))) (sv-string (concatenate 'string (sv-string " (") (sv-string (concatenate 'string (sv-string sv-str_specific) (sv-string (concatenate 'string (sv-string ")
  (block mp6-function
    ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-block )) (sv-string "))

")))))))))))))))))))))))) nil)(if (sv-bool (typep sv-decl 'mp-Sub)) (progn (setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string "(in-package ") (sv-string (concatenate 'string (sv-string sv-class_name) (sv-string (concatenate 'string (sv-string ")") (sv-string (concatenate 'string (sv-string (sv-newline (proto-mp-Main) )) (sv-string (concatenate 'string (sv-string "  ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-decl )) (sv-string (concatenate 'string (sv-string (sv-newline (proto-mp-Main) )) (sv-string (concatenate 'string (sv-string "(in-package mp-Main)") (sv-string (sv-newline (proto-mp-Main) ))))))))))))))))))))) nil)))(if (sv-bool (not (sv-eq (sv-name sv-self) "Pair"))) (progn (setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string "(defmethod sv-perl ((self ") (sv-string (concatenate 'string (sv-string sv-class_name) (sv-string (concatenate 'string (sv-string "))") (sv-string (concatenate 'string (sv-string (sv-newline (proto-mp-Main) )) (sv-string (concatenate 'string (sv-string "  (mp-Main::sv-lisp_dump_object \"::") (sv-string (concatenate 'string (sv-string (mp-Main::sv-lisp_escape_string (sv-name sv-self))) (sv-string (concatenate 'string (sv-string "\"") (sv-string (concatenate 'string (sv-string " (list ") (sv-string (concatenate 'string (sv-string sv-dumper) (sv-string (concatenate 'string (sv-string ")))") (sv-string (concatenate 'string (sv-string (sv-newline (proto-mp-Main) )) (sv-string (sv-newline (proto-mp-Main) ))))))))))))))))))))))))))) nil)(loop for sv-decl across (sv-body sv-self) do (progn (if (sv-bool (sv-and (not (sv-bool (sv-and (typep sv-decl 'mp-Decl) (sv-or (sv-eq (sv-decl sv-decl ) "has") (sv-eq (sv-decl sv-decl ) "my"))))) (sv-and (not (sv-bool (typep sv-decl 'mp-Method))) (not (sv-bool (typep sv-decl 'mp-Sub)))))) (progn (setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-decl )) (sv-string (sv-newline (proto-mp-Main) ))))))) nil)))(if (sv-bool sv-has_my_decl) (progn (setf sv-str (concatenate 'string (sv-string sv-str) (sv-string ")")))) nil)(setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string (sv-newline (proto-mp-Main) )) (sv-string (sv-newline (proto-mp-Main) )))))))))

(in-package mp-CompUnit)
  (defun sv-emit_lisp_program (&optional sv-comp_units )
  (block mp6-function (let ((sv-str (sv-undef))) (setf sv-str "")(loop for sv-comp_unit across sv-comp_units do (progn (setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string "(defpackage ") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_namespace (sv-name sv-comp_unit ))) (sv-string (concatenate 'string (sv-string "
") (sv-string (concatenate 'string (sv-string "  (:use common-lisp mp-Main))") (sv-string "
")))))))))))))(loop for sv-comp_unit across sv-comp_units do (progn (setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-comp_unit )) (sv-string "
")))))))(return-from mp6-function sv-str))))

(in-package mp-Main)
(defmethod sv-perl ((self mp-CompUnit))
  (mp-Main::sv-lisp_dump_object "::CompUnit" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "name") (setf (sv-value m) (sv-name self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "attributes") (setf (sv-value m) (sv-attributes self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "methods") (setf (sv-value m) (sv-methods self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "body") (setf (sv-value m) (sv-body self)) m) )))




;; class Val::Int
(if (not (ignore-errors (find-class 'mp-Val-Int)))
  (defclass mp-Val-Int () ()))

(let (x) 
  (setq x (make-instance 'mp-Val-Int))
  (defun proto-mp-Val-Int () x))
;; has $.int
(let ((new-slots (list (list :name 'sv-int
  :readers '(sv-int)
  :writers '((setf sv-int))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Val-Int)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Val-Int :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Val-Int))
  (block mp6-function
    (progn (sv-int sv-self))))

(defmethod sv-perl ((self mp-Val-Int))
  (mp-Main::sv-lisp_dump_object "::Val::Int" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "int") (setf (sv-value m) (sv-int self)) m) )))




;; class Val::Bit
(if (not (ignore-errors (find-class 'mp-Val-Bit)))
  (defclass mp-Val-Bit () ()))

(let (x) 
  (setq x (make-instance 'mp-Val-Bit))
  (defun proto-mp-Val-Bit () x))
;; has $.bit
(let ((new-slots (list (list :name 'sv-bit
  :readers '(sv-bit)
  :writers '((setf sv-bit))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Val-Bit)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Val-Bit :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Val-Bit))
  (block mp6-function
    (progn (sv-bit sv-self))))

(defmethod sv-perl ((self mp-Val-Bit))
  (mp-Main::sv-lisp_dump_object "::Val::Bit" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "bit") (setf (sv-value m) (sv-bit self)) m) )))




;; class Val::Num
(if (not (ignore-errors (find-class 'mp-Val-Num)))
  (defclass mp-Val-Num () ()))

(let (x) 
  (setq x (make-instance 'mp-Val-Num))
  (defun proto-mp-Val-Num () x))
;; has $.num
(let ((new-slots (list (list :name 'sv-num
  :readers '(sv-num)
  :writers '((setf sv-num))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Val-Num)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Val-Num :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Val-Num))
  (block mp6-function
    (progn (sv-num sv-self))))

(defmethod sv-perl ((self mp-Val-Num))
  (mp-Main::sv-lisp_dump_object "::Val::Num" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "num") (setf (sv-value m) (sv-num self)) m) )))




;; class Val::Buf
(if (not (ignore-errors (find-class 'mp-Val-Buf)))
  (defclass mp-Val-Buf () ()))

(let (x) 
  (setq x (make-instance 'mp-Val-Buf))
  (defun proto-mp-Val-Buf () x))
;; has $.buf
(let ((new-slots (list (list :name 'sv-buf
  :readers '(sv-buf)
  :writers '((setf sv-buf))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Val-Buf)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Val-Buf :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Val-Buf))
  (block mp6-function
    (progn (concatenate 'string (sv-string "\"") (sv-string (concatenate 'string (sv-string (mp-Main::sv-lisp_escape_string (sv-buf sv-self))) (sv-string "\"")))))))

(defmethod sv-perl ((self mp-Val-Buf))
  (mp-Main::sv-lisp_dump_object "::Val::Buf" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "buf") (setf (sv-value m) (sv-buf self)) m) )))




;; class Val::Undef
(if (not (ignore-errors (find-class 'mp-Val-Undef)))
  (defclass mp-Val-Undef () ()))

(let (x) 
  (setq x (make-instance 'mp-Val-Undef))
  (defun proto-mp-Val-Undef () x))
;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Val-Undef))
  (block mp6-function
    (progn "(sv-undef)")))

(defmethod sv-perl ((self mp-Val-Undef))
  (mp-Main::sv-lisp_dump_object "::Val::Undef" (list )))




;; class Val::Object
(if (not (ignore-errors (find-class 'mp-Val-Object)))
  (defclass mp-Val-Object () ()))

(let (x) 
  (setq x (make-instance 'mp-Val-Object))
  (defun proto-mp-Val-Object () x))
;; has $.class
(let ((new-slots (list (list :name 'sv-class
  :readers '(sv-class)
  :writers '((setf sv-class))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Val-Object)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Val-Object :direct-slots new-slots))

;; has $.fields
(let ((new-slots (list (list :name 'sv-fields
  :readers '(sv-fields)
  :writers '((setf sv-fields))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Val-Object)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Val-Object :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Val-Object))
  (block mp6-function
    (progn (concatenate 'string (sv-string "bless(") (sv-string (concatenate 'string (sv-string (sv-perl (sv-fields sv-self) )) (sv-string (concatenate 'string (sv-string ", ") (sv-string (concatenate 'string (sv-string (sv-perl (sv-class sv-self) )) (sv-string ")")))))))))))

(defmethod sv-perl ((self mp-Val-Object))
  (mp-Main::sv-lisp_dump_object "::Val::Object" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "class") (setf (sv-value m) (sv-class self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "fields") (setf (sv-value m) (sv-fields self)) m) )))




;; class Lit::Seq
(if (not (ignore-errors (find-class 'mp-Lit-Seq)))
  (defclass mp-Lit-Seq () ()))

(let (x) 
  (setq x (make-instance 'mp-Lit-Seq))
  (defun proto-mp-Lit-Seq () x))
;; has $.seq
(let ((new-slots (list (list :name 'sv-seq
  :readers '(sv-seq)
  :writers '((setf sv-seq))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Lit-Seq)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Lit-Seq :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Lit-Seq))
  (block mp6-function
    (progn (concatenate 'string (sv-string "(") (sv-string (concatenate 'string (sv-string (sv-join (let ((tmp (make-array 0 :adjustable 1 :fill-pointer t))) (map nil #'(lambda (c) (push (sv-emit_lisp  c) tmp)) (sv-seq sv-self)) tmp ) " ")) (sv-string ")")))))))

(defmethod sv-perl ((self mp-Lit-Seq))
  (mp-Main::sv-lisp_dump_object "::Lit::Seq" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "seq") (setf (sv-value m) (sv-seq self)) m) )))




;; class Lit::Array
(if (not (ignore-errors (find-class 'mp-Lit-Array)))
  (defclass mp-Lit-Array () ()))

(let (x) 
  (setq x (make-instance 'mp-Lit-Array))
  (defun proto-mp-Lit-Array () x))
;; has $.array1
(let ((new-slots (list (list :name 'sv-array1
  :readers '(sv-array1)
  :writers '((setf sv-array1))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Lit-Array)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Lit-Array :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Lit-Array))
  (block mp6-function
    (progn (if (sv-bool (sv-array1 sv-self)) (let ((sv-str (sv-undef))) (setf sv-str "")(loop for sv-elem across (sv-array1 sv-self) do (progn (if (sv-bool (sv-or (sv-and (typep sv-elem 'mp-Var) (sv-eq (sv-sigil sv-elem ) "@")) (sv-and (typep sv-elem 'mp-Apply) (sv-eq (sv-code sv-elem ) "prefix:<@>")))) (progn (setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string " ") (sv-string (sv-emit_lisp sv-elem ))))))) (progn (setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string " (list ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-elem )) (sv-string ")")))))))))))(concatenate 'string (sv-string (return-from mp6-function (sv-undef))) (sv-string (concatenate 'string (sv-string "(let ((_tmp_ (concatenate 'list ") (sv-string (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string "))) ") (sv-string "(make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_))"))))))))) (progn (return-from mp6-function "(make-array 0 :adjustable 1)"))))))

(defmethod sv-perl ((self mp-Lit-Array))
  (mp-Main::sv-lisp_dump_object "::Lit::Array" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "array1") (setf (sv-value m) (sv-array1 self)) m) )))




;; class Lit::Hash
(if (not (ignore-errors (find-class 'mp-Lit-Hash)))
  (defclass mp-Lit-Hash () ()))

(let (x) 
  (setq x (make-instance 'mp-Lit-Hash))
  (defun proto-mp-Lit-Hash () x))
;; has $.hash1
(let ((new-slots (list (list :name 'sv-hash1
  :readers '(sv-hash1)
  :writers '((setf sv-hash1))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Lit-Hash)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Lit-Hash :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Lit-Hash))
  (block mp6-function
    (progn (if (sv-bool (sv-hash1 sv-self)) (let ((sv-fields (sv-undef))(sv-str (sv-undef))) (setf sv-fields (sv-hash1 sv-self))(setf sv-str "")(loop for sv-field across sv-fields do (progn (setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string "(setf (mp-Main::sv-hash-lookup ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (mp-Main::sv-array-index sv-field 0) )) (sv-string (concatenate 'string (sv-string " h) ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (mp-Main::sv-array-index sv-field 1) )) (sv-string ")")))))))))))))(return-from mp6-function (concatenate 'string (sv-string "(let ((h (make-hash-table :test 'equal))) ") (sv-string (concatenate 'string (sv-string sv-str) (sv-string " h)")))))) (progn (return-from mp6-function "(make-hash-table :test 'equal)"))))))

(defmethod sv-perl ((self mp-Lit-Hash))
  (mp-Main::sv-lisp_dump_object "::Lit::Hash" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "hash1") (setf (sv-value m) (sv-hash1 self)) m) )))




;; class Lit::Code
(if (not (ignore-errors (find-class 'mp-Lit-Code)))
  (defclass mp-Lit-Code () ()))

(let (x) 
  (setq x (make-instance 'mp-Lit-Code))
  (defun proto-mp-Lit-Code () x))
(defmethod sv-perl ((self mp-Lit-Code))
  (mp-Main::sv-lisp_dump_object "::Lit::Code" (list )))




;; class Lit::Object
(if (not (ignore-errors (find-class 'mp-Lit-Object)))
  (defclass mp-Lit-Object () ()))

(let (x) 
  (setq x (make-instance 'mp-Lit-Object))
  (defun proto-mp-Lit-Object () x))
;; has $.class
(let ((new-slots (list (list :name 'sv-class
  :readers '(sv-class)
  :writers '((setf sv-class))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Lit-Object)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Lit-Object :direct-slots new-slots))

;; has $.fields
(let ((new-slots (list (list :name 'sv-fields
  :readers '(sv-fields)
  :writers '((setf sv-fields))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Lit-Object)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Lit-Object :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Lit-Object))
  (block mp6-function
    (progn (if (sv-bool (sv-fields sv-self)) (let ((sv-fields (sv-undef))(sv-str (sv-undef))) (setf sv-fields (sv-fields sv-self))(setf sv-str "")(loop for sv-field across sv-fields do (progn (setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string "(setf (") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_identifier (sv-buf (mp-Main::sv-array-index sv-field 0) ))) (sv-string (concatenate 'string (sv-string " m) ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (mp-Main::sv-array-index sv-field 1) )) (sv-string ")")))))))))))))(concatenate 'string (sv-string "(let ((m (make-instance '") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_namespace (sv-class sv-self))) (sv-string (concatenate 'string (sv-string "))) ") (sv-string (concatenate 'string (sv-string sv-str) (sv-string " m)"))))))))) (progn (return-from mp6-function (concatenate 'string (sv-string "(make-instance '") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_namespace (sv-class sv-self))) (sv-string ")"))))))))))

(defmethod sv-perl ((self mp-Lit-Object))
  (mp-Main::sv-lisp_dump_object "::Lit::Object" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "class") (setf (sv-value m) (sv-class self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "fields") (setf (sv-value m) (sv-fields self)) m) )))




;; class Index
(if (not (ignore-errors (find-class 'mp-Index)))
  (defclass mp-Index () ()))

(let (x) 
  (setq x (make-instance 'mp-Index))
  (defun proto-mp-Index () x))
;; has $.obj
(let ((new-slots (list (list :name 'sv-obj
  :readers '(sv-obj)
  :writers '((setf sv-obj))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Index)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Index :direct-slots new-slots))

;; has $.index_exp
(let ((new-slots (list (list :name 'sv-index_exp
  :readers '(sv-index_exp)
  :writers '((setf sv-index_exp))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Index)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Index :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Index))
  (block mp6-function
    (progn (return-from mp6-function (concatenate 'string (sv-string "(mp-Main::sv-array-index ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (sv-obj sv-self) )) (sv-string (concatenate 'string (sv-string " ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (sv-index_exp sv-self) )) (sv-string ")"))))))))))))

(defmethod sv-perl ((self mp-Index))
  (mp-Main::sv-lisp_dump_object "::Index" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "obj") (setf (sv-value m) (sv-obj self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "index_exp") (setf (sv-value m) (sv-index_exp self)) m) )))




;; class Lookup
(if (not (ignore-errors (find-class 'mp-Lookup)))
  (defclass mp-Lookup () ()))

(let (x) 
  (setq x (make-instance 'mp-Lookup))
  (defun proto-mp-Lookup () x))
;; has $.obj
(let ((new-slots (list (list :name 'sv-obj
  :readers '(sv-obj)
  :writers '((setf sv-obj))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Lookup)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Lookup :direct-slots new-slots))

;; has $.index_exp
(let ((new-slots (list (list :name 'sv-index_exp
  :readers '(sv-index_exp)
  :writers '((setf sv-index_exp))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Lookup)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Lookup :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Lookup))
  (block mp6-function
    (progn (return-from mp6-function (concatenate 'string (sv-string "(mp-Main::sv-hash-lookup ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (sv-index_exp sv-self) )) (sv-string (concatenate 'string (sv-string " ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (sv-obj sv-self) )) (sv-string ")"))))))))))))

(defmethod sv-perl ((self mp-Lookup))
  (mp-Main::sv-lisp_dump_object "::Lookup" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "obj") (setf (sv-value m) (sv-obj self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "index_exp") (setf (sv-value m) (sv-index_exp self)) m) )))




;; class Var
(if (not (ignore-errors (find-class 'mp-Var)))
  (defclass mp-Var () ()))

(let (x) 
  (setq x (make-instance 'mp-Var))
  (defun proto-mp-Var () x))
;; has $.sigil
(let ((new-slots (list (list :name 'sv-sigil
  :readers '(sv-sigil)
  :writers '((setf sv-sigil))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Var)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Var :direct-slots new-slots))

;; has $.twigil
(let ((new-slots (list (list :name 'sv-twigil
  :readers '(sv-twigil)
  :writers '((setf sv-twigil))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Var)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Var :direct-slots new-slots))

;; has $.namespace
(let ((new-slots (list (list :name 'sv-namespace
  :readers '(sv-namespace)
  :writers '((setf sv-namespace))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Var)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Var :direct-slots new-slots))

;; has $.name
(let ((new-slots (list (list :name 'sv-name
  :readers '(sv-name)
  :writers '((setf sv-name))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Var)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Var :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Var))
  (block mp6-function
    (let ((sv-ns (sv-undef))) (setf sv-ns "")(if (sv-bool (sv-namespace sv-self)) (progn (setf sv-ns (concatenate 'string (sv-string (mp-Main::sv-to_lisp_namespace (sv-namespace sv-self))) (sv-string "::")))) (progn (if (sv-bool (sv-and (sv-eq (sv-sigil sv-self) "@") (sv-and (sv-eq (sv-twigil sv-self) "*") (sv-eq (sv-name sv-self) "ARGS")))) (progn (return-from mp6-function "COMMON-LISP-USER::*posix-argv*")) nil)))(if (sv-bool (sv-eq (sv-twigil sv-self) ".")) (concatenate 'string (sv-string "(") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_identifier (sv-name sv-self))) (sv-string " sv-self)")))) (if (sv-bool (sv-eq (sv-name sv-self) "/")) (mp-Main::sv-to_lisp_identifier "MATCH") (concatenate 'string (sv-string sv-ns) (sv-string (mp-Main::sv-to_lisp_identifier (sv-name sv-self)))))))))

(defmethod sv-perl ((self mp-Var))
  (mp-Main::sv-lisp_dump_object "::Var" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "sigil") (setf (sv-value m) (sv-sigil self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "twigil") (setf (sv-value m) (sv-twigil self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "namespace") (setf (sv-value m) (sv-namespace self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "name") (setf (sv-value m) (sv-name self)) m) )))




;; class Bind
(if (not (ignore-errors (find-class 'mp-Bind)))
  (defclass mp-Bind () ()))

(let (x) 
  (setq x (make-instance 'mp-Bind))
  (defun proto-mp-Bind () x))
;; has $.parameters
(let ((new-slots (list (list :name 'sv-parameters
  :readers '(sv-parameters)
  :writers '((setf sv-parameters))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Bind)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Bind :direct-slots new-slots))

;; has $.arguments
(let ((new-slots (list (list :name 'sv-arguments
  :readers '(sv-arguments)
  :writers '((setf sv-arguments))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Bind)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Bind :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Bind))
  (block mp6-function
    (progn (if (sv-bool (typep (sv-parameters sv-self) 'mp-Lit-Object)) (let ((sv-class (sv-undef))(sv-a (sv-undef))(sv-b (sv-undef))(sv-str (sv-undef))(sv-i (sv-undef))(sv-arg (sv-undef))) (setf sv-class (sv-class (sv-parameters sv-self) ))(setf sv-a (sv-fields (sv-parameters sv-self) ))(setf sv-b (sv-arguments sv-self))(setf sv-str "do { ")(setf sv-i 0)(loop for sv-var across sv-a do (let ((sv-bind (sv-undef))) (setf sv-bind (let ((m (make-instance 'mp-Bind))) (setf (sv-parameters m) (mp-Main::sv-array-index sv-var 1))(setf (sv-arguments m) (let ((m (make-instance 'mp-Call))) (setf (sv-invocant m) sv-b)(setf (sv-method m) (sv-buf (mp-Main::sv-array-index sv-var 0) ))(setf (sv-arguments m) (make-array 0 :adjustable 1))(setf (sv-hyper m) 0) m)) m))(setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string " ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-bind )) (sv-string " ")))))))(setf sv-i (+ sv-i 1))))(return-from mp6-function (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string (sv-emit_lisp (sv-parameters sv-self) )) (sv-string " }")))))) nil)(if (sv-bool (sv-and (typep (sv-parameters sv-self) 'mp-Decl) (sv-eq (sv-decl (sv-parameters sv-self) ) "my"))) (progn (return-from mp6-function (concatenate 'string (sv-string "(setf ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (sv-var (sv-parameters sv-self) ) )) (sv-string (concatenate 'string (sv-string " ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (sv-arguments sv-self) )) (sv-string ")")))))))))) nil)(concatenate 'string (sv-string "(setf ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (sv-parameters sv-self) )) (sv-string (concatenate 'string (sv-string " ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (sv-arguments sv-self) )) (sv-string ")")))))))))))

(defmethod sv-perl ((self mp-Bind))
  (mp-Main::sv-lisp_dump_object "::Bind" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "parameters") (setf (sv-value m) (sv-parameters self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "arguments") (setf (sv-value m) (sv-arguments self)) m) )))




;; class Proto
(if (not (ignore-errors (find-class 'mp-Proto)))
  (defclass mp-Proto () ()))

(let (x) 
  (setq x (make-instance 'mp-Proto))
  (defun proto-mp-Proto () x))
;; has $.name
(let ((new-slots (list (list :name 'sv-name
  :readers '(sv-name)
  :writers '((setf sv-name))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Proto)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Proto :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Proto))
  (block mp6-function
    (progn (concatenate 'string (sv-string "(proto-") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_namespace (sv-name sv-self))) (sv-string ")")))))))

(defmethod sv-perl ((self mp-Proto))
  (mp-Main::sv-lisp_dump_object "::Proto" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "name") (setf (sv-value m) (sv-name self)) m) )))




;; class Call
(if (not (ignore-errors (find-class 'mp-Call)))
  (defclass mp-Call () ()))

(let (x) 
  (setq x (make-instance 'mp-Call))
  (defun proto-mp-Call () x))
;; has $.invocant
(let ((new-slots (list (list :name 'sv-invocant
  :readers '(sv-invocant)
  :writers '((setf sv-invocant))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Call)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Call :direct-slots new-slots))

;; has $.hyper
(let ((new-slots (list (list :name 'sv-hyper
  :readers '(sv-hyper)
  :writers '((setf sv-hyper))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Call)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Call :direct-slots new-slots))

;; has $.method
(let ((new-slots (list (list :name 'sv-method
  :readers '(sv-method)
  :writers '((setf sv-method))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Call)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Call :direct-slots new-slots))

;; has $.arguments
(let ((new-slots (list (list :name 'sv-arguments
  :readers '(sv-arguments)
  :writers '((setf sv-arguments))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Call)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Call :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Call))
  (block mp6-function
    (let ((sv-arguments (sv-undef))(sv-invocant (sv-undef))(sv-meth (sv-undef))) (setf sv-arguments (sv-join (let ((tmp (make-array 0 :adjustable 1 :fill-pointer t))) (map nil #'(lambda (c) (push (sv-emit_lisp  c) tmp)) (sv-arguments sv-self)) tmp ) " "))(setf sv-invocant (sv-emit_lisp (sv-invocant sv-self) ))(if (sv-bool (sv-eq sv-invocant "self")) (progn (setf sv-invocant "sv-self")) nil)(if (sv-bool (sv-eq (sv-method sv-self) "isa")) (progn (if (sv-bool (sv-eq (sv-buf (mp-Main::sv-array-index (sv-arguments sv-self) 0) ) "Str")) (progn (return-from mp6-function (concatenate 'string (sv-string "(typep ") (sv-string (concatenate 'string (sv-string sv-invocant) (sv-string " 'string)")))))) nil)(return-from mp6-function (concatenate 'string (sv-string "(typep ") (sv-string (concatenate 'string (sv-string sv-invocant) (sv-string (concatenate 'string (sv-string " '") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_namespace (sv-buf (mp-Main::sv-array-index (sv-arguments sv-self) 0) ))) (sv-string ")")))))))))) nil)(if (sv-bool (sv-eq (sv-method sv-self) "chars")) (progn (if (sv-bool (sv-hyper sv-self)) (progn (progn (write-line (format nil "~{~a~}" (list "not implemented")) *error-output*) (sb-ext:quit))) (progn (return-from mp6-function (concatenate 'string (sv-string "(length ") (sv-string (concatenate 'string (sv-string sv-invocant) (sv-string ")")))))))) nil)(if (sv-bool (sv-or (sv-eq (sv-method sv-self) "yaml") (sv-eq (sv-method sv-self) "say"))) (progn (if (sv-bool (sv-hyper sv-self)) (progn (return-from mp6-function (concatenate 'string (sv-string "[ map { ") (sv-string (concatenate 'string (sv-string (sv-method sv-self)) (sv-string (concatenate 'string (sv-string "( $_, ") (sv-string (concatenate 'string (sv-string ", ") (sv-string (concatenate 'string (sv-string sv-arguments) (sv-string (concatenate 'string (sv-string ")") (sv-string (concatenate 'string (sv-string " } @{ ") (sv-string (concatenate 'string (sv-string sv-invocant) (sv-string " } ]")))))))))))))))))) (progn (return-from mp6-function (concatenate 'string (sv-string "(") (sv-string (concatenate 'string (sv-string (sv-method sv-self)) (sv-string (concatenate 'string (sv-string " ") (sv-string (concatenate 'string (sv-string sv-invocant) (sv-string (concatenate 'string (sv-string " ") (sv-string (concatenate 'string (sv-string sv-arguments) (sv-string ")")))))))))))))))) nil)(setf sv-meth (concatenate 'string (sv-string (mp-Main::sv-to_lisp_identifier (sv-method sv-self))) (sv-string " ")))(if (sv-bool (sv-eq (sv-method sv-self) "postcircumfix:<( )>")) (progn (return-from mp6-function (concatenate 'string (sv-string "(funcall ") (sv-string (concatenate 'string (sv-string sv-invocant) (sv-string (concatenate 'string (sv-string " ") (sv-string (concatenate 'string (sv-string sv-arguments) (sv-string ")")))))))))) nil)(if (sv-bool (sv-hyper sv-self)) (progn (return-from mp6-function (concatenate 'string (sv-string "(let ((tmp (make-array 0 :adjustable 1 :fill-pointer t))) ") (sv-string (concatenate 'string (sv-string "(map nil #'(lambda (c) (push (") (sv-string (concatenate 'string (sv-string sv-meth) (sv-string (concatenate 'string (sv-string " c) tmp)) ") (sv-string (concatenate 'string (sv-string sv-invocant) (sv-string (concatenate 'string (sv-string ") ") (sv-string "tmp )")))))))))))))) (progn (return-from mp6-function (concatenate 'string (sv-string "(") (sv-string (concatenate 'string (sv-string sv-meth) (sv-string (concatenate 'string (sv-string sv-invocant) (sv-string (concatenate 'string (sv-string " ") (sv-string (concatenate 'string (sv-string sv-arguments) (sv-string ")"))))))))))))))))

(defmethod sv-perl ((self mp-Call))
  (mp-Main::sv-lisp_dump_object "::Call" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "invocant") (setf (sv-value m) (sv-invocant self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "hyper") (setf (sv-value m) (sv-hyper self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "method") (setf (sv-value m) (sv-method self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "arguments") (setf (sv-value m) (sv-arguments self)) m) )))




;; class Apply
(if (not (ignore-errors (find-class 'mp-Apply)))
  (defclass mp-Apply () ()))

(let (x) 
  (setq x (make-instance 'mp-Apply))
  (defun proto-mp-Apply () x))
;; has $.code
(let ((new-slots (list (list :name 'sv-code
  :readers '(sv-code)
  :writers '((setf sv-code))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Apply)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Apply :direct-slots new-slots))

;; has $.arguments
(let ((new-slots (list (list :name 'sv-arguments
  :readers '(sv-arguments)
  :writers '((setf sv-arguments))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Apply)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Apply :direct-slots new-slots))

;; has $.namespace
(let ((new-slots (list (list :name 'sv-namespace
  :readers '(sv-namespace)
  :writers '((setf sv-namespace))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Apply)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Apply :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Apply))
  (block mp6-function
    (let ((sv-ns (sv-undef))(sv-code (sv-undef))(sv-args (sv-undef))) (setf sv-ns "")(if (sv-bool (sv-namespace sv-self)) (progn (setf sv-ns (concatenate 'string (sv-string (mp-Main::sv-to_lisp_namespace (sv-namespace sv-self))) (sv-string "::")))) nil)(setf sv-code (concatenate 'string (sv-string sv-ns) (sv-string (sv-code sv-self))))(if (sv-bool (sv-eq sv-code "infix:<~>")) (progn (return-from mp6-function (concatenate 'string (sv-string "(concatenate 'string (sv-string ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (mp-Main::sv-array-index (sv-arguments sv-self) 0) )) (sv-string (concatenate 'string (sv-string ") (sv-string ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (mp-Main::sv-array-index (sv-arguments sv-self) 1) )) (sv-string "))")))))))))) nil)(if (sv-bool (sv-eq sv-code "ternary:<?? !!>")) (progn (return-from mp6-function (concatenate 'string (sv-string "(if (sv-bool ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (mp-Main::sv-array-index (sv-arguments sv-self) 0) )) (sv-string (concatenate 'string (sv-string ") ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (mp-Main::sv-array-index (sv-arguments sv-self) 1) )) (sv-string (concatenate 'string (sv-string " ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (mp-Main::sv-array-index (sv-arguments sv-self) 2) )) (sv-string ")")))))))))))))) nil)(setf sv-args "")(if (sv-bool (sv-arguments sv-self)) (progn (setf sv-args (sv-join (let ((tmp (make-array 0 :adjustable 1 :fill-pointer t))) (map nil #'(lambda (c) (push (sv-emit_lisp  c) tmp)) (sv-arguments sv-self)) tmp ) " "))) nil)(if (sv-bool (sv-eq sv-code "self")) (progn (return-from mp6-function "sv-self")) nil)(if (sv-bool (sv-eq sv-code "false")) (progn (return-from mp6-function "nil")) nil)(if (sv-bool (sv-eq sv-code "make")) (progn (return-from mp6-function (concatenate 'string (sv-string "(setf (sv-capture sv-MATCH) ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string ")")))))) nil)(if (sv-bool (sv-eq sv-code "substr")) (progn (return-from mp6-function (concatenate 'string (sv-string "(sv-substr ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string ")")))))) nil)(if (sv-bool (sv-eq sv-code "say")) (progn (return-from mp6-function (concatenate 'string (sv-string "(mp-Main::sv-say (list ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string "))")))))) nil)(if (sv-bool (sv-eq sv-code "print")) (progn (return-from mp6-function (concatenate 'string (sv-string "(mp-Main::sv-print (list ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string "))")))))) nil)(if (sv-bool (sv-eq sv-code "warn")) (progn (return-from mp6-function (concatenate 'string (sv-string "(write-line (format nil \"~{~a~}\" (list ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string ")) *error-output*)")))))) nil)(if (sv-bool (sv-eq sv-code "die")) (progn (return-from mp6-function (concatenate 'string (sv-string "(progn (write-line (format nil \"~{~a~}\" (list ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string ")) *error-output*) (sb-ext:quit))")))))) nil)(if (sv-bool (sv-eq sv-code "array")) (progn (return-from mp6-function sv-args)) nil)(if (sv-bool (sv-eq sv-code "exists")) (let ((sv-arg (sv-undef))) (setf sv-arg (mp-Main::sv-array-index (sv-arguments sv-self) 0))(if (sv-bool (typep sv-arg 'mp-Lookup)) (progn (return-from mp6-function (concatenate 'string (sv-string "(nth-value 1 ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-arg )) (sv-string ")")))))) nil)) nil)(if (sv-bool (sv-eq sv-code "prefix:<~>")) (progn (return-from mp6-function (concatenate 'string (sv-string "(sv-string ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string ")")))))) nil)(if (sv-bool (sv-eq sv-code "prefix:<!>")) (progn (return-from mp6-function (concatenate 'string (sv-string "(not (sv-bool ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string "))")))))) nil)(if (sv-bool (sv-eq sv-code "prefix:<?>")) (progn (return-from mp6-function (concatenate 'string (sv-string "(sv-bool ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string ")")))))) nil)(if (sv-bool (sv-eq sv-code "prefix:<$>")) (progn (return-from mp6-function (concatenate 'string (sv-string "(sv-scalar ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string ")")))))) nil)(if (sv-bool (sv-eq sv-code "prefix:<@>")) (progn (return-from mp6-function sv-args)) nil)(if (sv-bool (sv-eq sv-code "prefix:<%>")) (progn (return-from mp6-function sv-args)) nil)(if (sv-bool (sv-eq sv-code "infix:<+>")) (progn (return-from mp6-function (concatenate 'string (sv-string "(+ ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string ")")))))) nil)(if (sv-bool (sv-eq sv-code "infix:<->")) (progn (return-from mp6-function (concatenate 'string (sv-string "(-") (sv-string (concatenate 'string (sv-string sv-args) (sv-string ")")))))) nil)(if (sv-bool (sv-eq sv-code "infix:<>>")) (progn (return-from mp6-function (concatenate 'string (sv-string "(> ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string ")")))))) nil)(if (sv-bool (sv-eq sv-code "infix:<&&>")) (progn (return-from mp6-function (concatenate 'string (sv-string "(sv-and ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string ")")))))) nil)(if (sv-bool (sv-eq sv-code "infix:<||>")) (progn (return-from mp6-function (concatenate 'string (sv-string "(sv-or ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string ")")))))) nil)(if (sv-bool (sv-eq sv-code "infix:<eq>")) (progn (return-from mp6-function (concatenate 'string (sv-string "(sv-eq ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string ")")))))) nil)(if (sv-bool (sv-eq sv-code "infix:<ne>")) (progn (return-from mp6-function (concatenate 'string (sv-string "(not (sv-eq ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string "))")))))) nil)(if (sv-bool (sv-eq sv-code "infix:<==>")) (progn (return-from mp6-function (concatenate 'string (sv-string "(sv-eq-int ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string ")")))))) nil)(if (sv-bool (sv-eq sv-code "infix:<!=>")) (progn (return-from mp6-function (concatenate 'string (sv-string "(not (sv-eq-int ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string "))")))))) nil)(return-from mp6-function (concatenate 'string (sv-string "(") (sv-string (concatenate 'string (sv-string sv-ns) (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_identifier (sv-code sv-self))) (sv-string (concatenate 'string (sv-string " ") (sv-string (concatenate 'string (sv-string sv-args) (sv-string ")"))))))))))))))

(defmethod sv-perl ((self mp-Apply))
  (mp-Main::sv-lisp_dump_object "::Apply" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "code") (setf (sv-value m) (sv-code self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "arguments") (setf (sv-value m) (sv-arguments self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "namespace") (setf (sv-value m) (sv-namespace self)) m) )))




;; class Return
(if (not (ignore-errors (find-class 'mp-Return)))
  (defclass mp-Return () ()))

(let (x) 
  (setq x (make-instance 'mp-Return))
  (defun proto-mp-Return () x))
;; has $.result
(let ((new-slots (list (list :name 'sv-result
  :readers '(sv-result)
  :writers '((setf sv-result))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Return)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Return :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Return))
  (block mp6-function
    (progn (return-from mp6-function (concatenate 'string (sv-string "(return-from mp6-function ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (sv-result sv-self) )) (sv-string ")"))))))))

(defmethod sv-perl ((self mp-Return))
  (mp-Main::sv-lisp_dump_object "::Return" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "result") (setf (sv-value m) (sv-result self)) m) )))




;; class If
(if (not (ignore-errors (find-class 'mp-If)))
  (defclass mp-If () ()))

(let (x) 
  (setq x (make-instance 'mp-If))
  (defun proto-mp-If () x))
;; has $.cond
(let ((new-slots (list (list :name 'sv-cond
  :readers '(sv-cond)
  :writers '((setf sv-cond))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-If)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-If :direct-slots new-slots))

;; has $.body
(let ((new-slots (list (list :name 'sv-body
  :readers '(sv-body)
  :writers '((setf sv-body))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-If)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-If :direct-slots new-slots))

;; has $.otherwise
(let ((new-slots (list (list :name 'sv-otherwise
  :readers '(sv-otherwise)
  :writers '((setf sv-otherwise))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-If)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-If :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-If))
  (block mp6-function
    (let ((sv-block1 (sv-undef))(sv-block2 (sv-undef))) (setf sv-block1 (let ((m (make-instance 'mp-MiniPerl6-Lisp-LexicalBlock))) (setf (sv-block m) (sv-body sv-self)) m))(setf sv-block2 (let ((m (make-instance 'mp-MiniPerl6-Lisp-LexicalBlock))) (setf (sv-block m) (sv-otherwise sv-self)) m))(concatenate 'string (sv-string "(if (sv-bool ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (sv-cond sv-self) )) (sv-string (concatenate 'string (sv-string ") ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-block1 )) (sv-string (concatenate 'string (sv-string " ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-block2 )) (sv-string ")")))))))))))))))

(defmethod sv-perl ((self mp-If))
  (mp-Main::sv-lisp_dump_object "::If" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "cond") (setf (sv-value m) (sv-cond self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "body") (setf (sv-value m) (sv-body self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "otherwise") (setf (sv-value m) (sv-otherwise self)) m) )))




;; class For
(if (not (ignore-errors (find-class 'mp-For)))
  (defclass mp-For () ()))

(let (x) 
  (setq x (make-instance 'mp-For))
  (defun proto-mp-For () x))
;; has $.cond
(let ((new-slots (list (list :name 'sv-cond
  :readers '(sv-cond)
  :writers '((setf sv-cond))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-For)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-For :direct-slots new-slots))

;; has $.body
(let ((new-slots (list (list :name 'sv-body
  :readers '(sv-body)
  :writers '((setf sv-body))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-For)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-For :direct-slots new-slots))

;; has $.topic
(let ((new-slots (list (list :name 'sv-topic
  :readers '(sv-topic)
  :writers '((setf sv-topic))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-For)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-For :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-For))
  (block mp6-function
    (let ((sv-cond (sv-undef))(sv-block (sv-undef))) (setf sv-cond (sv-cond sv-self))(setf sv-block (let ((m (make-instance 'mp-MiniPerl6-Lisp-LexicalBlock))) (setf (sv-block m) (sv-body sv-self)) m))(if (sv-bool (sv-and (typep sv-cond 'mp-Var) (sv-eq (sv-sigil sv-cond ) "@"))) (progn (setf sv-cond (let ((m (make-instance 'mp-Apply))) (setf (sv-code m) "prefix:<@>")(setf (sv-arguments m) (let ((_tmp_ (concatenate 'list  (list sv-cond)))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_))) m))) nil)(concatenate 'string (sv-string "(loop for ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp (sv-topic sv-self) )) (sv-string (concatenate 'string (sv-string " across ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-cond )) (sv-string (concatenate 'string (sv-string " do ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-block )) (sv-string ")")))))))))))))))

(defmethod sv-perl ((self mp-For))
  (mp-Main::sv-lisp_dump_object "::For" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "cond") (setf (sv-value m) (sv-cond self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "body") (setf (sv-value m) (sv-body self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "topic") (setf (sv-value m) (sv-topic self)) m) )))




;; class Decl
(if (not (ignore-errors (find-class 'mp-Decl)))
  (defclass mp-Decl () ()))

(let (x) 
  (setq x (make-instance 'mp-Decl))
  (defun proto-mp-Decl () x))
;; has $.decl
(let ((new-slots (list (list :name 'sv-decl
  :readers '(sv-decl)
  :writers '((setf sv-decl))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Decl)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Decl :direct-slots new-slots))

;; has $.type
(let ((new-slots (list (list :name 'sv-type
  :readers '(sv-type)
  :writers '((setf sv-type))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Decl)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Decl :direct-slots new-slots))

;; has $.var
(let ((new-slots (list (list :name 'sv-var
  :readers '(sv-var)
  :writers '((setf sv-var))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Decl)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Decl :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Decl))
  (block mp6-function
    (let ((sv-decl (sv-undef))(sv-name (sv-undef))) (setf sv-decl (sv-decl sv-self))(setf sv-name (sv-name (sv-var sv-self) ))(if (sv-bool (sv-eq sv-decl "has")) (concatenate 'string (sv-string "sub ") (sv-string (concatenate 'string (sv-string sv-name) (sv-string (concatenate 'string (sv-string " { ") (sv-string (concatenate 'string (sv-string "@_ == 1 ") (sv-string (concatenate 'string (sv-string "? ( $_[0]->{") (sv-string (concatenate 'string (sv-string sv-name) (sv-string (concatenate 'string (sv-string "} ) ") (sv-string (concatenate 'string (sv-string ": ( $_[0]->{") (sv-string (concatenate 'string (sv-string sv-name) (sv-string (concatenate 'string (sv-string "} = $_[1] ) ") (sv-string "}")))))))))))))))))))) (concatenate 'string (sv-string (sv-decl sv-self)) (sv-string (concatenate 'string (sv-string " ") (sv-string (concatenate 'string (sv-string (sv-type sv-self)) (sv-string (concatenate 'string (sv-string " ") (sv-string (sv-emit_lisp (sv-var sv-self) )))))))))))))

(in-package mp-Decl)
  (defun sv-emit_lisp_initializer (&optional sv-decl )
  (block mp6-function (progn (if (sv-bool (sv-eq (sv-sigil sv-decl ) "%")) (progn (return-from mp6-function (concatenate 'string (sv-string "(") (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-decl )) (sv-string " (make-hash-table :test 'equal))")))))) (progn (if (sv-bool (sv-eq (sv-sigil sv-decl ) "@")) (progn (return-from mp6-function (concatenate 'string (sv-string "(") (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-decl )) (sv-string " (make-array 0 :fill-pointer t :adjustable t))")))))) (progn (return-from mp6-function (concatenate 'string (sv-string "(") (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-decl )) (sv-string " (sv-undef))"))))))))))))

(in-package mp-Main)
(defmethod sv-perl ((self mp-Decl))
  (mp-Main::sv-lisp_dump_object "::Decl" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "decl") (setf (sv-value m) (sv-decl self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "type") (setf (sv-value m) (sv-type self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "var") (setf (sv-value m) (sv-var self)) m) )))




;; class Sig
(if (not (ignore-errors (find-class 'mp-Sig)))
  (defclass mp-Sig () ()))

(let (x) 
  (setq x (make-instance 'mp-Sig))
  (defun proto-mp-Sig () x))
;; has $.invocant
(let ((new-slots (list (list :name 'sv-invocant
  :readers '(sv-invocant)
  :writers '((setf sv-invocant))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Sig)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Sig :direct-slots new-slots))

;; has $.positional
(let ((new-slots (list (list :name 'sv-positional
  :readers '(sv-positional)
  :writers '((setf sv-positional))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Sig)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Sig :direct-slots new-slots))

;; has $.named
(let ((new-slots (list (list :name 'sv-named
  :readers '(sv-named)
  :writers '((setf sv-named))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Sig)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Sig :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Sig))
  (block mp6-function
    (progn " print 'Signature - TODO'; die 'Signature - TODO'; ")))

(defmethod sv-perl ((self mp-Sig))
  (mp-Main::sv-lisp_dump_object "::Sig" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "invocant") (setf (sv-value m) (sv-invocant self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "positional") (setf (sv-value m) (sv-positional self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "named") (setf (sv-value m) (sv-named self)) m) )))




;; class Method
(if (not (ignore-errors (find-class 'mp-Method)))
  (defclass mp-Method () ()))

(let (x) 
  (setq x (make-instance 'mp-Method))
  (defun proto-mp-Method () x))
;; has $.name
(let ((new-slots (list (list :name 'sv-name
  :readers '(sv-name)
  :writers '((setf sv-name))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Method)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Method :direct-slots new-slots))

;; has $.sig
(let ((new-slots (list (list :name 'sv-sig
  :readers '(sv-sig)
  :writers '((setf sv-sig))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Method)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Method :direct-slots new-slots))

;; has $.block
(let ((new-slots (list (list :name 'sv-block
  :readers '(sv-block)
  :writers '((setf sv-block))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Method)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Method :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Method))
  (block mp6-function
    nil))

(defmethod sv-perl ((self mp-Method))
  (mp-Main::sv-lisp_dump_object "::Method" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "name") (setf (sv-value m) (sv-name self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "sig") (setf (sv-value m) (sv-sig self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "block") (setf (sv-value m) (sv-block self)) m) )))




;; class Sub
(if (not (ignore-errors (find-class 'mp-Sub)))
  (defclass mp-Sub () ()))

(let (x) 
  (setq x (make-instance 'mp-Sub))
  (defun proto-mp-Sub () x))
;; has $.name
(let ((new-slots (list (list :name 'sv-name
  :readers '(sv-name)
  :writers '((setf sv-name))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Sub)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Sub :direct-slots new-slots))

;; has $.sig
(let ((new-slots (list (list :name 'sv-sig
  :readers '(sv-sig)
  :writers '((setf sv-sig))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Sub)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Sub :direct-slots new-slots))

;; has $.block
(let ((new-slots (list (list :name 'sv-block
  :readers '(sv-block)
  :writers '((setf sv-block))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Sub)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Sub :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Sub))
  (block mp6-function
    (let ((sv-sig (sv-undef))(sv-pos (sv-undef))(sv-block (sv-undef))(sv-str (sv-undef))) (setf sv-sig (sv-sig sv-self))(setf sv-pos (sv-positional sv-sig ))(setf sv-block (let ((m (make-instance 'mp-MiniPerl6-Lisp-LexicalBlock))) (setf (sv-block m) (sv-block sv-self)) m))(if (sv-bool sv-pos) (progn (loop for sv-field across sv-pos do (progn (setf sv-str (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-field )) (sv-string " ")))))))) nil)(if (sv-bool sv-str) (progn (setf sv-str (concatenate 'string (sv-string "&optional ") (sv-string sv-str)))) nil)(if (sv-bool (sv-name sv-self)) (progn (concatenate 'string (sv-string "(defun ") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_identifier (sv-name sv-self))) (sv-string (concatenate 'string (sv-string " (") (sv-string (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string ")") (sv-string (concatenate 'string (sv-string (sv-newline (proto-mp-Main) )) (sv-string (concatenate 'string (sv-string "  (block mp6-function ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-block )) (sv-string (concatenate 'string (sv-string "))") (sv-string (sv-newline (proto-mp-Main) )))))))))))))))))))) (progn (concatenate 'string (sv-string "(lambda ") (sv-string (concatenate 'string (sv-string (sv-name sv-self)) (sv-string (concatenate 'string (sv-string " (") (sv-string (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string ")") (sv-string (concatenate 'string (sv-string (sv-newline (proto-mp-Main) )) (sv-string (concatenate 'string (sv-string "  (block mp6-function ") (sv-string (concatenate 'string (sv-string (sv-emit_lisp sv-block )) (sv-string (concatenate 'string (sv-string "))") (sv-string (sv-newline (proto-mp-Main) ))))))))))))))))))))))))

(defmethod sv-perl ((self mp-Sub))
  (mp-Main::sv-lisp_dump_object "::Sub" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "name") (setf (sv-value m) (sv-name self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "sig") (setf (sv-value m) (sv-sig self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "block") (setf (sv-value m) (sv-block self)) m) )))




;; class Do
(if (not (ignore-errors (find-class 'mp-Do)))
  (defclass mp-Do () ()))

(let (x) 
  (setq x (make-instance 'mp-Do))
  (defun proto-mp-Do () x))
;; has $.block
(let ((new-slots (list (list :name 'sv-block
  :readers '(sv-block)
  :writers '((setf sv-block))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Do)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Do :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Do))
  (block mp6-function
    (let ((sv-block (sv-undef))) (setf sv-block (let ((m (make-instance 'mp-MiniPerl6-Lisp-LexicalBlock))) (setf (sv-block m) (sv-block sv-self)) m))(return-from mp6-function (sv-emit_lisp sv-block )))))

(defmethod sv-perl ((self mp-Do))
  (mp-Main::sv-lisp_dump_object "::Do" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "block") (setf (sv-value m) (sv-block self)) m) )))




;; class Use
(if (not (ignore-errors (find-class 'mp-Use)))
  (defclass mp-Use () ()))

(let (x) 
  (setq x (make-instance 'mp-Use))
  (defun proto-mp-Use () x))
;; has $.mod
(let ((new-slots (list (list :name 'sv-mod
  :readers '(sv-mod)
  :writers '((setf sv-mod))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Use)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Use :direct-slots new-slots))

;; method emit_lisp
(if (not (ignore-errors (find-method 'sv-emit_lisp () ())))
  (defgeneric sv-emit_lisp (sv-self)
      (:documentation "a method")))
(defmethod sv-emit_lisp ((sv-self mp-Use))
  (block mp6-function
    (progn (concatenate 'string (sv-string (sv-newline (proto-mp-Main) )) (sv-string (concatenate 'string (sv-string ";; use ") (sv-string (concatenate 'string (sv-string (mp-Main::sv-to_lisp_namespace (sv-mod sv-self))) (sv-string (sv-newline (proto-mp-Main) ))))))))))

(defmethod sv-perl ((self mp-Use))
  (mp-Main::sv-lisp_dump_object "::Use" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "mod") (setf (sv-value m) (sv-mod self)) m) )))




;; Do not edit this file - Generated by MiniPerl6 3.0
(defpackage mp-MiniPerl6-Grammar
  (:use common-lisp mp-Main))
(defpackage mp-MiniPerl6-Grammar
  (:use common-lisp mp-Main))
(defpackage mp-MiniPerl6-Grammar
  (:use common-lisp mp-Main))
(defpackage mp-MiniPerl6-Grammar
  (:use common-lisp mp-Main))
(defpackage mp-MiniPerl6-Grammar
  (:use common-lisp mp-Main))
(defpackage mp-MiniPerl6-Grammar
  (:use common-lisp mp-Main))
;; class MiniPerl6::Grammar
(let ((sv-Class_name (sv-undef)))
(if (not (ignore-errors (find-class 'mp-MiniPerl6-Grammar)))
  (defclass mp-MiniPerl6-Grammar () ()))

(let (x) 
  (setq x (make-instance 'mp-MiniPerl6-Grammar))
  (defun proto-mp-MiniPerl6-Grammar () x))
(in-package mp-MiniPerl6-Grammar)
  (defun sv-get_class_name ()
  (block mp6-function (progn sv-Class_name)))

(in-package mp-Main)
;; method ident_digit
(if (not (ignore-errors (find-method 'sv-ident_digit () ())))
  (defgeneric sv-ident_digit (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-ident_digit ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-word sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "_" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)(let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-digit sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))))) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ident_digit sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))))))sv-MATCH)))

;; method ident
(if (not (ignore-errors (find-method 'sv-ident () ())))
  (defgeneric sv-ident (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-ident ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-word sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "_" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)))) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ident_digit sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))))))sv-MATCH)))

;; method full_ident
(if (not (ignore-errors (find-method 'sv-full_ident () ())))
  (defgeneric sv-full_ident (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-full_ident ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "::" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-full_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1)))))))sv-MATCH)))

;; method namespace_before_ident
(if (not (ignore-errors (find-method 'sv-namespace_before_ident () ())))
  (defgeneric sv-namespace_before_ident (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-namespace_before_ident ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-tmp (sv-undef))) (setf sv-tmp sv-MATCH)(setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) (sv-to sv-tmp ))(setf (sv-to m) (sv-to sv-tmp ))(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (if (sv-bool (sv-eq "::" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil))))(setf (sv-bool sv-tmp ) (sv-bool sv-MATCH))(setf sv-MATCH sv-tmp)(sv-bool sv-MATCH)) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "::" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-namespace_before_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))))))))sv-MATCH)))

;; method optional_namespace_before_ident
(if (not (ignore-errors (find-method 'sv-optional_namespace_before_ident () ())))
  (defgeneric sv-optional_namespace_before_ident (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-optional_namespace_before_ident ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-namespace_before_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "namespace_before_ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq "::" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-string (mp-Main::sv-hash-lookup "namespace_before_ident" sv-MATCH)))) 1)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and 1 (sv-or (progn (setf (sv-capture sv-MATCH) "")) 1))))))sv-MATCH)))

;; method to_line_end
(if (not (ignore-errors (find-method 'sv-to_line_end () ())))
  (defgeneric sv-to_line_end (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-to_line_end ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-not_newline sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-to_line_end sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))))sv-MATCH)))

;; method pod_begin
(if (not (ignore-errors (find-method 'sv-pod_begin () ())))
  (defgeneric sv-pod_begin (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-pod_begin ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-is_newline sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "=end" (sv-substr sv-str (sv-to sv-MATCH ) 4))) (+ 1 (setf (sv-to sv-MATCH ) (+ 4 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-to_line_end sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (not (sv-eq "" (sv-substr sv-str (sv-to sv-MATCH ) 1)))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-to_line_end sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-pod_begin sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))))))))sv-MATCH)))

;; method pod_other
(if (not (ignore-errors (find-method 'sv-pod_other () ())))
  (defgeneric sv-pod_other (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-pod_other ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-is_newline sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "=cut" (sv-substr sv-str (sv-to sv-MATCH ) 4))) (+ 1 (setf (sv-to sv-MATCH ) (+ 4 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-to_line_end sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (not (sv-eq "" (sv-substr sv-str (sv-to sv-MATCH ) 1)))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-to_line_end sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-pod_other sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))))))))sv-MATCH)))

;; method ws
(if (not (ignore-errors (find-method 'sv-ws () ())))
  (defgeneric sv-ws (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-ws ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "#" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-to_line_end sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-is_newline sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "=begin" (sv-substr sv-str (sv-to sv-MATCH ) 6))) (+ 1 (setf (sv-to sv-MATCH ) (+ 6 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-pod_begin sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "=kwid" (sv-substr sv-str (sv-to sv-MATCH ) 5))) (+ 1 (setf (sv-to sv-MATCH ) (+ 5 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-pod_other sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "=pod" (sv-substr sv-str (sv-to sv-MATCH ) 4))) (+ 1 (setf (sv-to sv-MATCH ) (+ 4 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-pod_other sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "=for" (sv-substr sv-str (sv-to sv-MATCH ) 4))) (+ 1 (setf (sv-to sv-MATCH ) (+ 4 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-pod_other sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "=head1" (sv-substr sv-str (sv-to sv-MATCH ) 6))) (+ 1 (setf (sv-to sv-MATCH ) (+ 6 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-pod_other sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-space sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1)))))))sv-MATCH)))

;; method opt_ws
(if (not (ignore-errors (find-method 'sv-opt_ws () ())))
  (defgeneric sv-opt_ws (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-opt_ws ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))))sv-MATCH)))

;; method opt_ws2
(if (not (ignore-errors (find-method 'sv-opt_ws2 () ())))
  (defgeneric sv-opt_ws2 (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-opt_ws2 ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))))sv-MATCH)))

;; method opt_ws3
(if (not (ignore-errors (find-method 'sv-opt_ws3 () ())))
  (defgeneric sv-opt_ws3 (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-opt_ws3 ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))))sv-MATCH)))

;; method parse
(if (not (ignore-errors (find-method 'sv-parse () ())))
  (defgeneric sv-parse (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-parse ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-comp_unit sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "comp_unit" sv-MATCH) sv-m2)1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-parse sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "parse" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "comp_unit" sv-MATCH))) (sv-scalar (mp-Main::sv-hash-lookup "parse" sv-MATCH))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_)))) 1))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "comp_unit" sv-MATCH)))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_)))) 1)))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (make-array 0 :adjustable 1))) 1)))))sv-MATCH)))

;; method comp_unit
(if (not (ignore-errors (find-method 'sv-comp_unit () ())))
  (defgeneric sv-comp_unit (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-comp_unit ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq ";" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))) (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "use" (sv-substr sv-str (sv-to sv-MATCH ) 3))) (+ 1 (setf (sv-to sv-MATCH ) (+ 3 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "v6-" (sv-substr sv-str (sv-to sv-MATCH ) 3))) (+ 1 (setf (sv-to sv-MATCH ) (+ 3 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq ";" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))))))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "use" (sv-substr sv-str (sv-to sv-MATCH ) 3))) (+ 1 (setf (sv-to sv-MATCH ) (+ 3 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "v6" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq ";" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1)))) (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq "class" (sv-substr sv-str (sv-to sv-MATCH ) 5))) (+ 1 (setf (sv-to sv-MATCH ) (+ 5 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "grammar" (sv-substr sv-str (sv-to sv-MATCH ) 7))) (+ 1 (setf (sv-to sv-MATCH ) (+ 7 (sv-to sv-MATCH )))) nil)))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-full_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "full_ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (sv-or (progn (setf sv-Class_name (sv-string (mp-Main::sv-hash-lookup "full_ident" sv-MATCH)))) 1) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_stmts sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq ";" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-CompUnit))) (setf (sv-name m) (sv-scalar (mp-Main::sv-hash-lookup "full_ident" sv-MATCH)))(setf (sv-attributes m) (make-hash-table :test 'equal))(setf (sv-methods m) (make-hash-table :test 'equal))(setf (sv-body m) (sv-scalar (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH))) m))) 1)))))))))))))))))))sv-MATCH)))

;; method infix_op
(if (not (ignore-errors (find-method 'sv-infix_op () ())))
  (defgeneric sv-infix_op (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-infix_op ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq "+" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "-" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "*" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "/" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (if (sv-bool (sv-eq "q" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "n" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "==" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "!=" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "&&" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "||" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "~~" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "~" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "x" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)))))))))))))))))sv-MATCH)))

;; method hyper_op
(if (not (ignore-errors (find-method 'sv-hyper_op () ())))
  (defgeneric sv-hyper_op (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-hyper_op ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq ">>" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))))sv-MATCH)))

;; method prefix_op
(if (not (ignore-errors (find-method 'sv-prefix_op () ())))
  (defgeneric sv-prefix_op (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-prefix_op ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq "$" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "@" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "%" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "?" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "!" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "++" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "--" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "+" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "-" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "~" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)))))))))))) (let ((sv-tmp (sv-undef))) (setf sv-tmp sv-MATCH)(setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) (sv-to sv-tmp ))(setf (sv-to m) (sv-to sv-tmp ))(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq "(" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "$" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)))))(setf (sv-bool sv-tmp ) (sv-bool sv-MATCH))(setf sv-MATCH sv-tmp)(sv-bool sv-MATCH))))))sv-MATCH)))

;; method declarator
(if (not (ignore-errors (find-method 'sv-declarator () ())))
  (defgeneric sv-declarator (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-declarator ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq "my" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "state" (sv-substr sv-str (sv-to sv-MATCH ) 5))) (+ 1 (setf (sv-to sv-MATCH ) (+ 5 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "has" (sv-substr sv-str (sv-to sv-MATCH ) 3))) (+ 1 (setf (sv-to sv-MATCH ) (+ 3 (sv-to sv-MATCH )))) nil))))))sv-MATCH)))

;; method exp2
(if (not (ignore-errors (find-method 'sv-exp2 () ())))
  (defgeneric sv-exp2 (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-exp2 ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH)))) 1)))))sv-MATCH)))

;; method exp_stmts2
(if (not (ignore-errors (find-method 'sv-exp_stmts2 () ())))
  (defgeneric sv-exp_stmts2 (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-exp_stmts2 ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_stmts sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH)))) 1)))))sv-MATCH)))

(defmethod sv-perl ((self mp-MiniPerl6-Grammar))
  (mp-Main::sv-lisp_dump_object "::MiniPerl6::Grammar" (list )))


;; use mp-MiniPerl6-Grammar-Regex


;; use mp-MiniPerl6-Grammar-Mapping


;; use mp-MiniPerl6-Grammar-Control

)


;; class MiniPerl6::Grammar
(if (not (ignore-errors (find-class 'mp-MiniPerl6-Grammar)))
  (defclass mp-MiniPerl6-Grammar () ()))

(let (x) 
  (setq x (make-instance 'mp-MiniPerl6-Grammar))
  (defun proto-mp-MiniPerl6-Grammar () x))
;; method exp
(if (not (ignore-errors (find-method 'sv-exp () ())))
  (defgeneric sv-exp (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-exp ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-term_meth sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "term_meth" sv-MATCH) sv-m2)1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "??" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "!!" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp2 sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp2" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Apply))) (setf (sv-namespace m) "")(setf (sv-code m) "ternary:<?? !!>")(setf (sv-arguments m) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "term_meth" sv-MATCH))) (list (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH))) (list (sv-scalar (mp-Main::sv-hash-lookup "exp2" sv-MATCH)))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_))) m))) 1)))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (mp-Main::sv-say (list "*** Syntax error in ternary operation"))) 1))))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-infix_op sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "infix_op" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Apply))) (setf (sv-namespace m) "")(setf (sv-code m) (concatenate 'string (sv-string "infix:<") (sv-string (concatenate 'string (sv-string (mp-Main::sv-hash-lookup "infix_op" sv-MATCH)) (sv-string ">")))))(setf (sv-arguments m) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "term_meth" sv-MATCH))) (list (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH)))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_))) m))) 1)))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq ":=" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Bind))) (setf (sv-parameters m) (sv-scalar (mp-Main::sv-hash-lookup "term_meth" sv-MATCH)))(setf (sv-arguments m) (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH))) m))) 1)))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "=" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (progn (write-line (format nil "~{~a~}" (list "*** Error in assignment operation: infix<=> not implemented; use infix<:=> instead")) *error-output*) (sb-ext:quit))) 1)))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "term_meth" sv-MATCH)))) 1)))))))))))sv-MATCH)))

;; method opt_ident
(if (not (ignore-errors (find-method 'sv-opt_ident () ())))
  (defgeneric sv-opt_ident (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-opt_ident ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "ident" sv-MATCH)))) 1))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and 1 (sv-or (progn (setf (sv-capture sv-MATCH) "postcircumfix:<( )>")) 1))))))sv-MATCH)))

;; method term_meth
(if (not (ignore-errors (find-method 'sv-term_meth () ())))
  (defgeneric sv-term_meth (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-term_meth ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-full_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "full_ident" sv-MATCH) sv-m2)1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq ".new(" (sv-substr sv-str (sv-to sv-MATCH ) 5))) (+ 1 (setf (sv-to sv-MATCH ) (+ 5 (sv-to sv-MATCH )))) nil) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_mapping sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_mapping" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq ")" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Lit-Object))) (setf (sv-class m) (sv-scalar (mp-Main::sv-hash-lookup "full_ident" sv-MATCH)))(setf (sv-fields m) (sv-scalar (mp-Main::sv-hash-lookup "exp_mapping" sv-MATCH))) m))) 1)))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (mp-Main::sv-say (list "*** Syntax Error parsing Constructor"))(progn (write-line (format nil "~{~a~}" (list )) *error-output*) (sb-ext:quit))) 1)))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "." (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-hyper_op sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "hyper_op" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "ident" sv-MATCH) sv-m2)1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "(" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_seq sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq ")" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Call))) (setf (sv-invocant m) (let ((m (make-instance 'mp-Proto))) (setf (sv-name m) (sv-string (mp-Main::sv-hash-lookup "full_ident" sv-MATCH))) m))(setf (sv-method m) (sv-scalar (mp-Main::sv-hash-lookup "ident" sv-MATCH)))(setf (sv-arguments m) (sv-scalar (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH)))(setf (sv-hyper m) (sv-scalar (mp-Main::sv-hash-lookup "hyper_op" sv-MATCH))) m))) 1))))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq ":" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_seq sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Call))) (setf (sv-invocant m) (let ((m (make-instance 'mp-Proto))) (setf (sv-name m) (sv-string (mp-Main::sv-hash-lookup "full_ident" sv-MATCH))) m))(setf (sv-method m) (sv-scalar (mp-Main::sv-hash-lookup "ident" sv-MATCH)))(setf (sv-arguments m) (sv-scalar (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH)))(setf (sv-hyper m) (sv-scalar (mp-Main::sv-hash-lookup "hyper_op" sv-MATCH))) m))) 1)))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Call))) (setf (sv-invocant m) (let ((m (make-instance 'mp-Proto))) (setf (sv-name m) (sv-string (mp-Main::sv-hash-lookup "full_ident" sv-MATCH))) m))(setf (sv-method m) (sv-scalar (mp-Main::sv-hash-lookup "ident" sv-MATCH)))(setf (sv-arguments m) (make-array 0 :adjustable 1))(setf (sv-hyper m) (sv-scalar (mp-Main::sv-hash-lookup "hyper_op" sv-MATCH))) m))) 1))))))))))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_term sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_term" sv-MATCH) sv-m2)1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "." (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-hyper_op sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "hyper_op" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "opt_ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "(" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_seq sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (if (sv-bool (sv-eq ")" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq ":" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_seq sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH) sv-m2)1) (progn nil))) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Call))) (setf (sv-invocant m) (sv-scalar (mp-Main::sv-hash-lookup "exp_term" sv-MATCH)))(setf (sv-method m) (sv-scalar (mp-Main::sv-hash-lookup "opt_ident" sv-MATCH)))(setf (sv-arguments m) (make-array 0 :adjustable 1))(setf (sv-hyper m) (sv-scalar (mp-Main::sv-hash-lookup "hyper_op" sv-MATCH))) m))) 1))))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Call))) (setf (sv-invocant m) (sv-scalar (mp-Main::sv-hash-lookup "exp_term" sv-MATCH)))(setf (sv-method m) (sv-scalar (mp-Main::sv-hash-lookup "opt_ident" sv-MATCH)))(setf (sv-arguments m) (sv-scalar (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH)))(setf (sv-hyper m) (sv-scalar (mp-Main::sv-hash-lookup "hyper_op" sv-MATCH))) m))) 1)))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "[" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "]" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Index))) (setf (sv-obj m) (sv-scalar (mp-Main::sv-hash-lookup "exp_term" sv-MATCH)))(setf (sv-index_exp m) (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH))) m))) 1))))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Lookup))) (setf (sv-obj m) (sv-scalar (mp-Main::sv-hash-lookup "exp_term" sv-MATCH)))(setf (sv-index_exp m) (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH))) m))) 1))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "exp_term" sv-MATCH)))) 1)))))))))))sv-MATCH)))

;; method sub_or_method_name
(if (not (ignore-errors (find-method 'sv-sub_or_method_name () ())))
  (defgeneric sv-sub_or_method_name (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-sub_or_method_name ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-full_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "full_ident" sv-MATCH) sv-m2)1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "." (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "ident" sv-MATCH) sv-m2)1) (progn nil))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1)))))))sv-MATCH)))

;; method opt_type
(if (not (ignore-errors (find-method 'sv-opt_type () ())))
  (defgeneric sv-opt_type (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-opt_type ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq "::" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-full_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "full_ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "full_ident" sv-MATCH)))) 1)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and 1 (sv-or (progn (setf (sv-capture sv-MATCH) "")) 1))))))sv-MATCH)))

;; method exp_term
(if (not (ignore-errors (find-method 'sv-exp_term () ())))
  (defgeneric sv-exp_term (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-exp_term ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-var_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "var_ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "var_ident" sv-MATCH)))) 1))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-prefix_op sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "prefix_op" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Apply))) (setf (sv-namespace m) "")(setf (sv-code m) (concatenate 'string (sv-string "prefix:<") (sv-string (concatenate 'string (sv-string (mp-Main::sv-hash-lookup "prefix_op" sv-MATCH)) (sv-string ">")))))(setf (sv-arguments m) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH)))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_))) m))) 1)))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "(" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq ")" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH)))) 1))))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_mapping sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_mapping" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Lit-Hash))) (setf (sv-hash1 m) (sv-scalar (mp-Main::sv-hash-lookup "exp_mapping" sv-MATCH))) m))) 1))))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "[" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_seq sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "]" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Lit-Array))) (setf (sv-array1 m) (sv-scalar (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH))) m))) 1))))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "$" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "<" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-sub_or_method_name sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "sub_or_method_name" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Lookup))) (setf (sv-obj m) (let ((m (make-instance 'mp-Var))) (setf (sv-sigil m) "$")(setf (sv-twigil m) "")(setf (sv-name m) "/") m))(setf (sv-index_exp m) (let ((m (make-instance 'mp-Val-Buf))) (setf (sv-buf m) (sv-scalar (mp-Main::sv-hash-lookup "sub_or_method_name" sv-MATCH))) m)) m))) 1)))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "d" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "o" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_stmts sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Do))) (setf (sv-block m) (sv-scalar (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH))) m))) 1)))))))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-declarator sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "declarator" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_type sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "opt_type" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-var_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "var_ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Decl))) (setf (sv-decl m) (sv-scalar (mp-Main::sv-hash-lookup "declarator" sv-MATCH)))(setf (sv-type m) (sv-scalar (mp-Main::sv-hash-lookup "opt_type" sv-MATCH)))(setf (sv-var m) (sv-scalar (mp-Main::sv-hash-lookup "var_ident" sv-MATCH))) m))) 1))))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "u" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "s" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-full_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "full_ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "-" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "ident" sv-MATCH) sv-m2)1) (progn nil))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Use))) (setf (sv-mod m) (sv-scalar (mp-Main::sv-hash-lookup "full_ident" sv-MATCH))) m))) 1)))))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-val sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "val" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "val" sv-MATCH)))) 1))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-lit sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "lit" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "lit" sv-MATCH)))) 1))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-token sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "token" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "token" sv-MATCH)))) 1))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-method_def sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "method_def" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "method_def" sv-MATCH)))) 1))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-sub_def sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "sub_def" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "sub_def" sv-MATCH)))) 1))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-control sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "control" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "control" sv-MATCH)))) 1))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-apply sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "apply" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "apply" sv-MATCH)))) 1))))))))))))))))))))sv-MATCH)))

(defmethod sv-perl ((self mp-MiniPerl6-Grammar))
  (mp-Main::sv-lisp_dump_object "::MiniPerl6::Grammar" (list )))




;; class MiniPerl6::Grammar
(if (not (ignore-errors (find-class 'mp-MiniPerl6-Grammar)))
  (defclass mp-MiniPerl6-Grammar () ()))

(let (x) 
  (setq x (make-instance 'mp-MiniPerl6-Grammar))
  (defun proto-mp-MiniPerl6-Grammar () x))
;; method var_sigil
(if (not (ignore-errors (find-method 'sv-var_sigil () ())))
  (defgeneric sv-var_sigil (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-var_sigil ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq "$" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "%" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "@" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "&" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)))))))sv-MATCH)))

;; method var_twigil
(if (not (ignore-errors (find-method 'sv-var_twigil () ())))
  (defgeneric sv-var_twigil (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-var_twigil ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq "." (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "!" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "^" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "*" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))))sv-MATCH)))

;; method var_name
(if (not (ignore-errors (find-method 'sv-var_name () ())))
  (defgeneric sv-var_name (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-var_name ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-full_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "full_ident" sv-MATCH) sv-m2)1) (progn nil)))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "/" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)(let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-digit sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "digit" sv-MATCH) sv-m2)1) (progn nil))))))))sv-MATCH)))

;; method var_ident
(if (not (ignore-errors (find-method 'sv-var_ident () ())))
  (defgeneric sv-var_ident (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-var_ident ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-var_sigil sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "var_sigil" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-var_twigil sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "var_twigil" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-optional_namespace_before_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "optional_namespace_before_ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-var_name sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "var_name" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Var))) (setf (sv-sigil m) (sv-string (mp-Main::sv-hash-lookup "var_sigil" sv-MATCH)))(setf (sv-twigil m) (sv-string (mp-Main::sv-hash-lookup "var_twigil" sv-MATCH)))(setf (sv-namespace m) (sv-scalar (mp-Main::sv-hash-lookup "optional_namespace_before_ident" sv-MATCH)))(setf (sv-name m) (sv-string (mp-Main::sv-hash-lookup "var_name" sv-MATCH))) m))) 1))))))))sv-MATCH)))

;; method val
(if (not (ignore-errors (find-method 'sv-val () ())))
  (defgeneric sv-val (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-val ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-val_undef sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "val_undef" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "val_undef" sv-MATCH)))) 1))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-val_int sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "val_int" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "val_int" sv-MATCH)))) 1))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-val_bit sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "val_bit" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "val_bit" sv-MATCH)))) 1))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-val_num sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "val_num" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "val_num" sv-MATCH)))) 1))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-val_buf sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "val_buf" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "val_buf" sv-MATCH)))) 1)))))))))sv-MATCH)))

;; method val_bit
(if (not (ignore-errors (find-method 'sv-val_bit () ())))
  (defgeneric sv-val_bit (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-val_bit ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "T" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "r" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "u" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Val-Bit))) (setf (sv-bit m) 1) m))) 1)))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "F" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "a" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "l" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "s" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Val-Bit))) (setf (sv-bit m) 0) m))) 1))))))))))sv-MATCH)))

(defmethod sv-perl ((self mp-MiniPerl6-Grammar))
  (mp-Main::sv-lisp_dump_object "::MiniPerl6::Grammar" (list )))




;; class MiniPerl6::Grammar
(if (not (ignore-errors (find-class 'mp-MiniPerl6-Grammar)))
  (defclass mp-MiniPerl6-Grammar () ()))

(let (x) 
  (setq x (make-instance 'mp-MiniPerl6-Grammar))
  (defun proto-mp-MiniPerl6-Grammar () x))
;; method val_undef
(if (not (ignore-errors (find-method 'sv-val_undef () ())))
  (defgeneric sv-val_undef (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-val_undef ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "u" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "n" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "d" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "f" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-tmp (sv-undef))) (setf sv-tmp sv-MATCH)(setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) (sv-to sv-tmp ))(setf (sv-to m) (sv-to sv-tmp ))(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (if (sv-bool (sv-eq "w" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))))(setf (sv-bool sv-tmp ) (not (sv-bool sv-MATCH)))(setf sv-MATCH sv-tmp)(sv-bool sv-MATCH)) (sv-or (progn (setf (sv-capture sv-MATCH) (make-instance 'mp-Val-Undef))) 1))))))))))sv-MATCH)))

;; method val_num
(if (not (ignore-errors (find-method 'sv-val_num () ())))
  (defgeneric sv-val_num (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-val_num ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "X" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "X" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "X" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) "TODO: val_num")) 1)))))))sv-MATCH)))

;; method char_any
(if (not (ignore-errors (find-method 'sv-char_any () ())))
  (defgeneric sv-char_any (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-char_any ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (if (sv-bool (not (sv-eq "" (sv-substr sv-str (sv-to sv-MATCH ) 1)))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))))sv-MATCH)))

;; method single_quoted_unescape
(if (not (ignore-errors (find-method 'sv-single_quoted_unescape () ())))
  (defgeneric sv-single_quoted_unescape (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-single_quoted_unescape ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "\\" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "'" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-single_quoted_unescape sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "single_quoted_unescape" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (concatenate 'string (sv-string "'") (sv-string (mp-Main::sv-hash-lookup "single_quoted_unescape" sv-MATCH))))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "\\" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "\"" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-single_quoted_unescape sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "single_quoted_unescape" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (concatenate 'string (sv-string "\"") (sv-string (mp-Main::sv-hash-lookup "single_quoted_unescape" sv-MATCH))))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "\\" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "\\" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-single_quoted_unescape sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "single_quoted_unescape" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (concatenate 'string (sv-string "\\") (sv-string (mp-Main::sv-hash-lookup "single_quoted_unescape" sv-MATCH))))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-tmp (sv-undef))) (setf sv-tmp sv-MATCH)(setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) (sv-to sv-tmp ))(setf (sv-to m) (sv-to sv-tmp ))(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (if (sv-bool (sv-eq "'" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))))(setf (sv-bool sv-tmp ) (not (sv-bool sv-MATCH)))(setf sv-MATCH sv-tmp)(sv-bool sv-MATCH)) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-char_any sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "char_any" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-single_quoted_unescape sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "single_quoted_unescape" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (concatenate 'string (sv-string (mp-Main::sv-hash-lookup "char_any" sv-MATCH)) (sv-string (mp-Main::sv-hash-lookup "single_quoted_unescape" sv-MATCH))))) 1))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1)))))))sv-MATCH)))

;; method double_quoted_unescape
(if (not (ignore-errors (find-method 'sv-double_quoted_unescape () ())))
  (defgeneric sv-double_quoted_unescape (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-double_quoted_unescape ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "\\" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "'" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-double_quoted_unescape sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "double_quoted_unescape" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (concatenate 'string (sv-string "'") (sv-string (mp-Main::sv-hash-lookup "double_quoted_unescape" sv-MATCH))))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "\\" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "\"" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-double_quoted_unescape sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "double_quoted_unescape" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (concatenate 'string (sv-string "\"") (sv-string (mp-Main::sv-hash-lookup "double_quoted_unescape" sv-MATCH))))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "\\" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "\\" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-double_quoted_unescape sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "double_quoted_unescape" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (concatenate 'string (sv-string "\\") (sv-string (mp-Main::sv-hash-lookup "double_quoted_unescape" sv-MATCH))))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "\\" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "n" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-double_quoted_unescape sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "double_quoted_unescape" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (concatenate 'string (sv-string (sv-newline (proto-mp-Main) )) (sv-string (mp-Main::sv-hash-lookup "double_quoted_unescape" sv-MATCH))))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-tmp (sv-undef))) (setf sv-tmp sv-MATCH)(setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) (sv-to sv-tmp ))(setf (sv-to m) (sv-to sv-tmp ))(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (if (sv-bool (sv-eq "\"" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))))(setf (sv-bool sv-tmp ) (not (sv-bool sv-MATCH)))(setf sv-MATCH sv-tmp)(sv-bool sv-MATCH)) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-char_any sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "char_any" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-double_quoted_unescape sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "double_quoted_unescape" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (concatenate 'string (sv-string (mp-Main::sv-hash-lookup "char_any" sv-MATCH)) (sv-string (mp-Main::sv-hash-lookup "double_quoted_unescape" sv-MATCH))))) 1))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))))))))sv-MATCH)))

;; method val_buf
(if (not (ignore-errors (find-method 'sv-val_buf () ())))
  (defgeneric sv-val_buf (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-val_buf ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "\"" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-double_quoted_unescape sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "double_quoted_unescape" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq "\"" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Val-Buf))) (setf (sv-buf m) (sv-scalar (mp-Main::sv-hash-lookup "double_quoted_unescape" sv-MATCH))) m))) 1))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "'" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-single_quoted_unescape sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "single_quoted_unescape" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq "'" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Val-Buf))) (setf (sv-buf m) (sv-scalar (mp-Main::sv-hash-lookup "single_quoted_unescape" sv-MATCH))) m))) 1))))))))sv-MATCH)))

;; method digits
(if (not (ignore-errors (find-method 'sv-digits () ())))
  (defgeneric sv-digits (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-digits ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-digit sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-digits sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "digits" sv-MATCH) sv-m2)1) (progn nil)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1)))))))sv-MATCH)))

;; method val_int
(if (not (ignore-errors (find-method 'sv-val_int () ())))
  (defgeneric sv-val_int (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-val_int ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-digits sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "digits" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Val-Int))) (setf (sv-int m) (sv-string sv-MATCH)) m))) 1)))))sv-MATCH)))

;; method exp_stmts
(if (not (ignore-errors (find-method 'sv-exp_stmts () ())))
  (defgeneric sv-exp_stmts (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-exp_stmts ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq ";" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_stmts sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq ";" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH))) (sv-scalar (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_)))) 1)))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq ";" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH)))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_)))) 1)))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (make-array 0 :adjustable 1))) 1)))))sv-MATCH)))

;; method exp_seq
(if (not (ignore-errors (find-method 'sv-exp_seq () ())))
  (defgeneric sv-exp_seq (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-exp_seq ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "," (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_seq sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "," (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH))) (sv-scalar (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_)))) 1)))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "," (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH)))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_)))) 1)))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (make-array 0 :adjustable 1))) 1)))))sv-MATCH)))

(defmethod sv-perl ((self mp-MiniPerl6-Grammar))
  (mp-Main::sv-lisp_dump_object "::MiniPerl6::Grammar" (list )))




;; class MiniPerl6::Grammar
(if (not (ignore-errors (find-class 'mp-MiniPerl6-Grammar)))
  (defclass mp-MiniPerl6-Grammar () ()))

(let (x) 
  (setq x (make-instance 'mp-MiniPerl6-Grammar))
  (defun proto-mp-MiniPerl6-Grammar () x))
;; method lit
(if (not (ignore-errors (find-method 'sv-lit () ())))
  (defgeneric sv-lit (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-lit ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-lit_object sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "lit_object" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "lit_object" sv-MATCH)))) 1)))))sv-MATCH)))

;; method lit_seq
(if (not (ignore-errors (find-method 'sv-lit_seq () ())))
  (defgeneric sv-lit_seq (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-lit_seq ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "X" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "X" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "X" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) "TODO: lit_seq")) 1)))))))sv-MATCH)))

;; method lit_array
(if (not (ignore-errors (find-method 'sv-lit_array () ())))
  (defgeneric sv-lit_array (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-lit_array ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "X" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "X" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "X" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) "TODO: lit_array")) 1)))))))sv-MATCH)))

;; method lit_hash
(if (not (ignore-errors (find-method 'sv-lit_hash () ())))
  (defgeneric sv-lit_hash (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-lit_hash ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "X" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "X" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "X" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) "TODO: lit_hash")) 1)))))))sv-MATCH)))

;; method lit_code
(if (not (ignore-errors (find-method 'sv-lit_code () ())))
  (defgeneric sv-lit_code (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-lit_code ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "X" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "X" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "X" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) "TODO - Lit::Code")) 1)))))))sv-MATCH)))

;; method lit_object
(if (not (ignore-errors (find-method 'sv-lit_object () ())))
  (defgeneric sv-lit_object (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-lit_object ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "::" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-full_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "full_ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq "(" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_mapping sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_mapping" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq ")" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Lit-Object))) (setf (sv-class m) (sv-scalar (mp-Main::sv-hash-lookup "full_ident" sv-MATCH)))(setf (sv-fields m) (sv-scalar (mp-Main::sv-hash-lookup "exp_mapping" sv-MATCH))) m))) 1)))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (mp-Main::sv-say (list "*** Syntax Error parsing Constructor"))(progn (write-line (format nil "~{~a~}" (list )) *error-output*) (sb-ext:quit))) 1))))))))))sv-MATCH)))

;; method bind
(if (not (ignore-errors (find-method 'sv-bind () ())))
  (defgeneric sv-bind (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-bind ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq ":=" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp2 sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp2" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Bind))) (setf (sv-parameters m) (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH)))(setf (sv-arguments m) (sv-scalar (mp-Main::sv-hash-lookup "exp2" sv-MATCH))) m))) 1)))))))))sv-MATCH)))

;; method call
(if (not (ignore-errors (find-method 'sv-call () ())))
  (defgeneric sv-call (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-call ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq "." (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq "(" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_seq sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq ")" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Call))) (setf (sv-invocant m) (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH)))(setf (sv-method m) (sv-scalar (mp-Main::sv-hash-lookup "ident" sv-MATCH)))(setf (sv-arguments m) (sv-scalar (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH)))(setf (sv-hyper m) "") m))) 1))))))))))))sv-MATCH)))

;; method apply
(if (not (ignore-errors (find-method 'sv-apply () ())))
  (defgeneric sv-apply (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-apply ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-optional_namespace_before_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "optional_namespace_before_ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-full_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "full_ident" sv-MATCH) sv-m2)1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "(" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_seq sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (if (sv-bool (sv-eq ")" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_seq sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH) sv-m2)1) (progn nil))) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))))))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Apply))) (setf (sv-namespace m) (sv-scalar (mp-Main::sv-hash-lookup "optional_namespace_before_ident" sv-MATCH)))(setf (sv-code m) (sv-scalar (mp-Main::sv-hash-lookup "full_ident" sv-MATCH)))(setf (sv-arguments m) (sv-scalar (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH))) m))) 1))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Apply))) (setf (sv-namespace m) (sv-scalar (mp-Main::sv-hash-lookup "optional_namespace_before_ident" sv-MATCH)))(setf (sv-code m) (sv-scalar (mp-Main::sv-hash-lookup "full_ident" sv-MATCH)))(setf (sv-arguments m) (make-array 0 :adjustable 1)) m))) 1)))))))))sv-MATCH)))

;; method opt_name
(if (not (ignore-errors (find-method 'sv-opt_name () ())))
  (defgeneric sv-opt_name (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-opt_name ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "ident" sv-MATCH) sv-m2)1) (progn nil)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))))sv-MATCH)))

;; method var_invocant
(if (not (ignore-errors (find-method 'sv-var_invocant () ())))
  (defgeneric sv-var_invocant (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-var_invocant ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-var_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "var_ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ":" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "var_ident" sv-MATCH)))) 1)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Var))) (setf (sv-sigil m) "$")(setf (sv-twigil m) "")(setf (sv-name m) "self") m))) 1)))))sv-MATCH)))

;; method args_sig
(if (not (ignore-errors (find-method 'sv-args_sig () ())))
  (defgeneric sv-args_sig (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-args_sig ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-var_invocant sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "var_invocant" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_seq sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Sig))) (setf (sv-invocant m) (sv-scalar (mp-Main::sv-hash-lookup "var_invocant" sv-MATCH)))(setf (sv-positional m) (sv-scalar (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH)))(setf (sv-named m) (make-hash-table :test 'equal)) m))) 1)))))))sv-MATCH)))

;; method method_sig
(if (not (ignore-errors (find-method 'sv-method_sig () ())))
  (defgeneric sv-method_sig (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-method_sig ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "(" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-args_sig sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "args_sig" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq ")" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "args_sig" sv-MATCH)))) 1)))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Sig))) (setf (sv-invocant m) (let ((m (make-instance 'mp-Var))) (setf (sv-sigil m) "$")(setf (sv-twigil m) "")(setf (sv-name m) "self") m))(setf (sv-positional m) (make-array 0 :adjustable 1))(setf (sv-named m) (make-hash-table :test 'equal)) m))) 1)))))sv-MATCH)))

;; method method_def
(if (not (ignore-errors (find-method 'sv-method_def () ())))
  (defgeneric sv-method_def (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-method_def ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "m" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "t" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "h" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "o" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "d" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_name sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "opt_name" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-method_sig sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "method_sig" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_stmts sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (mp-Main::sv-say (list "*** Syntax Error in method '" (sv-get_class_name ) "." (sv-scalar (mp-Main::sv-hash-lookup "name" sv-MATCH)) "' near pos=" (sv-to sv-MATCH )))(progn (write-line (format nil "~{~a~}" (list "error in Block")) *error-output*) (sb-ext:quit))) 1)))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Method))) (setf (sv-name m) (sv-scalar (mp-Main::sv-hash-lookup "opt_name" sv-MATCH)))(setf (sv-sig m) (sv-scalar (mp-Main::sv-hash-lookup "method_sig" sv-MATCH)))(setf (sv-block m) (sv-scalar (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH))) m))) 1))))))))))))))))))))sv-MATCH)))

;; method sub_def
(if (not (ignore-errors (find-method 'sv-sub_def () ())))
  (defgeneric sv-sub_def (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-sub_def ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "s" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "u" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "b" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_name sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "opt_name" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-method_sig sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "method_sig" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_stmts sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (mp-Main::sv-say (list "*** Syntax Error in sub '" (sv-scalar (mp-Main::sv-hash-lookup "name" sv-MATCH)) "'"))(progn (write-line (format nil "~{~a~}" (list "error in Block")) *error-output*) (sb-ext:quit))) 1)))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Sub))) (setf (sv-name m) (sv-scalar (mp-Main::sv-hash-lookup "opt_name" sv-MATCH)))(setf (sv-sig m) (sv-scalar (mp-Main::sv-hash-lookup "method_sig" sv-MATCH)))(setf (sv-block m) (sv-scalar (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH))) m))) 1)))))))))))))))))sv-MATCH)))

(defmethod sv-perl ((self mp-MiniPerl6-Grammar))
  (mp-Main::sv-lisp_dump_object "::MiniPerl6::Grammar" (list )))




;; class MiniPerl6::Grammar
(if (not (ignore-errors (find-class 'mp-MiniPerl6-Grammar)))
  (defclass mp-MiniPerl6-Grammar () ()))

(let (x) 
  (setq x (make-instance 'mp-MiniPerl6-Grammar))
  (defun proto-mp-MiniPerl6-Grammar () x))
;; method token
(if (not (ignore-errors (find-method 'sv-token () ())))
  (defgeneric sv-token (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-token ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "t" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "o" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "k" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "n" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_name sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "opt_name" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-rule (proto-mp-MiniPerl6-Grammar-Regex) sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "MiniPerl6::Grammar::Regex.rule" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (let ((sv-source (sv-undef))(sv-ast (sv-undef))) (setf sv-source (concatenate 'string (sv-string "method ") (sv-string (concatenate 'string (sv-string (mp-Main::sv-hash-lookup "opt_name" sv-MATCH)) (sv-string (concatenate 'string (sv-string " ( $grammar: $str, $pos ) { ") (sv-string (concatenate 'string (sv-string "my $MATCH; $MATCH := MiniPerl6::Match.new( 'str' => $str, 'from' => $pos, 'to' => $pos, 'bool' => 1 ); ") (sv-string (concatenate 'string (sv-string "$MATCH.bool := ( ") (sv-string (concatenate 'string (sv-string (sv-emit (sv-scalar (mp-Main::sv-hash-lookup "MiniPerl6::Grammar::Regex.rule" sv-MATCH)) )) (sv-string (concatenate 'string (sv-string "); ") (sv-string "$MATCH }")))))))))))))))(setf sv-ast (sv-exp_term (proto-mp-MiniPerl6-Grammar) sv-source 0))(setf (sv-capture sv-MATCH) (sv-scalar sv-ast))) 1)))))))))))))))sv-MATCH)))

(defmethod sv-perl ((self mp-MiniPerl6-Grammar))
  (mp-Main::sv-lisp_dump_object "::MiniPerl6::Grammar" (list )))




;; Do not edit this file - Generated by MiniPerl6 3.0
(defpackage mp-MiniPerl6-Grammar
  (:use common-lisp mp-Main))
;; class MiniPerl6::Grammar
(if (not (ignore-errors (find-class 'mp-MiniPerl6-Grammar)))
  (defclass mp-MiniPerl6-Grammar () ()))

(let (x) 
  (setq x (make-instance 'mp-MiniPerl6-Grammar))
  (defun proto-mp-MiniPerl6-Grammar () x))
;; method control
(if (not (ignore-errors (find-method 'sv-control () ())))
  (defgeneric sv-control (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-control ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ctrl_return sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "ctrl_return" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "ctrl_return" sv-MATCH)))) 1))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ctrl_leave sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "ctrl_leave" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "ctrl_leave" sv-MATCH)))) 1))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-if sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "if" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "if" sv-MATCH)))) 1))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-when sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "when" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "when" sv-MATCH)))) 1))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-for sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "for" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "for" sv-MATCH)))) 1))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-while sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "while" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "while" sv-MATCH)))) 1))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-apply sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "apply" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "apply" sv-MATCH)))) 1)))))))))))sv-MATCH)))

;; method if
(if (not (ignore-errors (find-method 'sv-if () ())))
  (defgeneric sv-if (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-if ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "i" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "f" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_stmts sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "l" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "s" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_stmts2 sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_stmts2" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-If))) (setf (sv-cond m) (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH)))(setf (sv-body m) (sv-scalar (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH)))(setf (sv-otherwise m) (sv-scalar (mp-Main::sv-hash-lookup "exp_stmts2" sv-MATCH))) m))) 1))))))))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-If))) (setf (sv-cond m) (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH)))(setf (sv-body m) (sv-scalar (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH)))(setf (sv-otherwise m) (make-array 0 :adjustable 1)) m))) 1)))))))))))))))))sv-MATCH)))

;; method when
(if (not (ignore-errors (find-method 'sv-when () ())))
  (defgeneric sv-when (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-when ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "w" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "h" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "n" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_seq sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_stmts sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-When))) (setf (sv-parameters m) (sv-scalar (mp-Main::sv-hash-lookup "exp_seq" sv-MATCH)))(setf (sv-body m) (sv-scalar (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH))) m))) 1))))))))))))))))sv-MATCH)))

;; method for
(if (not (ignore-errors (find-method 'sv-for () ())))
  (defgeneric sv-for (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-for ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "f" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "o" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "r" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "->" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-var_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "var_ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_stmts sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-For))) (setf (sv-cond m) (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH)))(setf (sv-topic m) (sv-scalar (mp-Main::sv-hash-lookup "var_ident" sv-MATCH)))(setf (sv-body m) (sv-scalar (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH))) m))) 1)))))))))))))))))))sv-MATCH)))

;; method while
(if (not (ignore-errors (find-method 'sv-while () ())))
  (defgeneric sv-while (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-while ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "w" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "h" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "i" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "l" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_stmts sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-While))) (setf (sv-cond m) (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH)))(setf (sv-body m) (sv-scalar (mp-Main::sv-hash-lookup "exp_stmts" sv-MATCH))) m))) 1)))))))))))))))))sv-MATCH)))

;; method ctrl_leave
(if (not (ignore-errors (find-method 'sv-ctrl_leave () ())))
  (defgeneric sv-ctrl_leave (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-ctrl_leave ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (if (sv-bool (sv-eq "l" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "a" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "v" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (make-instance 'mp-Leave))) 1)))))))))sv-MATCH)))

;; method ctrl_return
(if (not (ignore-errors (find-method 'sv-ctrl_return () ())))
  (defgeneric sv-ctrl_return (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-ctrl_return ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "r" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "t" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "u" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "r" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "n" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-tmp (sv-undef))) (setf sv-tmp sv-MATCH)(setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) (sv-to sv-tmp ))(setf (sv-to m) (sv-to sv-tmp ))(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (if (sv-bool (sv-eq "(" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))))(setf (sv-bool sv-tmp ) (sv-bool sv-MATCH))(setf sv-MATCH sv-tmp)(sv-bool sv-MATCH))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Return))) (setf (sv-result m) (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH))) m))) 1)))))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "r" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "e" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "t" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "u" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "r" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "n" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Return))) (setf (sv-result m) (make-instance 'mp-Val-Undef)) m))) 1)))))))))))sv-MATCH)))

(defmethod sv-perl ((self mp-MiniPerl6-Grammar))
  (mp-Main::sv-lisp_dump_object "::MiniPerl6::Grammar" (list )))




;; Do not edit this file - Generated by MiniPerl6 3.0
(defpackage mp-MiniPerl6-Grammar
  (:use common-lisp mp-Main))
;; class MiniPerl6::Grammar
(if (not (ignore-errors (find-class 'mp-MiniPerl6-Grammar)))
  (defclass mp-MiniPerl6-Grammar () ()))

(let (x) 
  (setq x (make-instance 'mp-MiniPerl6-Grammar))
  (defun proto-mp-MiniPerl6-Grammar () x))
;; method pair_key
(if (not (ignore-errors (find-method 'sv-pair_key () ())))
  (defgeneric sv-pair_key (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-pair_key ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-tmp (sv-undef))) (setf sv-tmp sv-MATCH)(setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) (sv-to sv-tmp ))(setf (sv-to m) (sv-to sv-tmp ))(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq "=>" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)(let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))))))(setf (sv-bool sv-tmp ) (sv-bool sv-MATCH))(setf sv-MATCH sv-tmp)(sv-bool sv-MATCH)) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Val-Buf))) (setf (sv-buf m) (sv-string (mp-Main::sv-hash-lookup "ident" sv-MATCH))) m))) 1)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH)))) 1))))))sv-MATCH)))

;; method pair
(if (not (ignore-errors (find-method 'sv-pair () ())))
  (defgeneric sv-pair (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-pair ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-pair_key sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "pair_key" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "=>" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "pair_key" sv-MATCH))) (list (sv-scalar (mp-Main::sv-hash-lookup "exp" sv-MATCH)))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_)))) 1))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq ":" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-var_sigil sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "var_sigil" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((_tmp_ (concatenate 'list  (list (let ((m (make-instance 'mp-Val-Buf))) (setf (sv-buf m) (sv-string (mp-Main::sv-hash-lookup "ident" sv-MATCH))) m)) (list (let ((m (make-instance 'mp-Var))) (setf (sv-sigil m) (sv-string (sv-scalar (mp-Main::sv-hash-lookup "var_sigil" sv-MATCH))))(setf (sv-twigil m) "")(setf (sv-name m) (sv-scalar (mp-Main::sv-hash-lookup "ident" sv-MATCH))) m))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_)))) 1))))))))sv-MATCH)))

;; method exp_mapping
(if (not (ignore-errors (find-method 'sv-exp_mapping () ())))
  (defgeneric sv-exp_mapping (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-exp_mapping ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-pair sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "pair" sv-MATCH) sv-m2)1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "," (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-exp_mapping sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "exp_mapping" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "pair" sv-MATCH))) (sv-scalar (mp-Main::sv-hash-lookup "exp_mapping" sv-MATCH))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_)))) 1)))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "," (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "pair" sv-MATCH)))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_)))) 1)))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (make-array 0 :adjustable 1))) 1)))))sv-MATCH)))

(defmethod sv-perl ((self mp-MiniPerl6-Grammar))
  (mp-Main::sv-lisp_dump_object "::MiniPerl6::Grammar" (list )))




;; Do not edit this file - Generated by MiniPerl6 3.0
(defpackage mp-MiniPerl6-Grammar-Regex
  (:use common-lisp mp-Main))
;; class MiniPerl6::Grammar::Regex
(let ((sv-rule_terms (make-hash-table :test 'equal)))
(if (not (ignore-errors (find-class 'mp-MiniPerl6-Grammar-Regex)))
  (defclass mp-MiniPerl6-Grammar-Regex () ()))

(let (x) 
  (setq x (make-instance 'mp-MiniPerl6-Grammar-Regex))
  (defun proto-mp-MiniPerl6-Grammar-Regex () x))
;; method ws
(if (not (ignore-errors (find-method 'sv-ws () ())))
  (defgeneric sv-ws (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-ws ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws (proto-mp-MiniPerl6-Grammar) sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))))))sv-MATCH)))

;; method rule_ident
(if (not (ignore-errors (find-method 'sv-rule_ident () ())))
  (defgeneric sv-rule_ident (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-rule_ident ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-full_ident (proto-mp-MiniPerl6-Grammar) sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-digit sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "digit" sv-MATCH) sv-m2)1) (progn nil)))))))sv-MATCH)))

;; method any
(if (not (ignore-errors (find-method 'sv-any () ())))
  (defgeneric sv-any (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-any ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (if (sv-bool (not (sv-eq "" (sv-substr sv-str (sv-to sv-MATCH ) 1)))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))))sv-MATCH)))

;; method literal
(if (not (ignore-errors (find-method 'sv-literal () ())))
  (defgeneric sv-literal (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-literal ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "\\" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (not (sv-eq "" (sv-substr sv-str (sv-to sv-MATCH ) 1)))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-literal sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "literal" sv-MATCH) sv-m2)1) (progn nil)))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-tmp (sv-undef))) (setf sv-tmp sv-MATCH)(setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) (sv-to sv-tmp ))(setf (sv-to m) (sv-to sv-tmp ))(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (if (sv-bool (sv-eq "'" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))))(setf (sv-bool sv-tmp ) (not (sv-bool sv-MATCH)))(setf sv-MATCH sv-tmp)(sv-bool sv-MATCH)) (sv-and (if (sv-bool (not (sv-eq "" (sv-substr sv-str (sv-to sv-MATCH ) 1)))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-literal sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "literal" sv-MATCH) sv-m2)1) (progn nil)))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1)))))sv-MATCH)))

;; method metasyntax_exp
(if (not (ignore-errors (find-method 'sv-metasyntax_exp () ())))
  (defgeneric sv-metasyntax_exp (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-metasyntax_exp ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "\\" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (if (sv-bool (not (sv-eq "" (sv-substr sv-str (sv-to sv-MATCH ) 1)))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "'" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-literal sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (if (sv-bool (sv-eq "'" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-string_code sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "<" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-metasyntax_exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-tmp (sv-undef))) (setf sv-tmp sv-MATCH)(setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) (sv-to sv-tmp ))(setf (sv-to m) (sv-to sv-tmp ))(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))))(setf (sv-bool sv-tmp ) (not (sv-bool sv-MATCH)))(setf sv-MATCH sv-tmp)(sv-bool sv-MATCH)) (if (sv-bool (not (sv-eq "" (sv-substr sv-str (sv-to sv-MATCH ) 1)))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)))))))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-metasyntax_exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "metasyntax_exp" sv-MATCH) sv-m2)1) (progn nil)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1)))))))sv-MATCH)))

;; method char_range
(if (not (ignore-errors (find-method 'sv-char_range () ())))
  (defgeneric sv-char_range (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-char_range ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "\\" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (if (sv-bool (not (sv-eq "" (sv-substr sv-str (sv-to sv-MATCH ) 1)))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-tmp (sv-undef))) (setf sv-tmp sv-MATCH)(setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) (sv-to sv-tmp ))(setf (sv-to m) (sv-to sv-tmp ))(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (if (sv-bool (sv-eq "]" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))))(setf (sv-bool sv-tmp ) (not (sv-bool sv-MATCH)))(setf sv-MATCH sv-tmp)(sv-bool sv-MATCH)) (if (sv-bool (not (sv-eq "" (sv-substr sv-str (sv-to sv-MATCH ) 1)))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-char_range sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "char_range" sv-MATCH) sv-m2)1) (progn nil)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1)))))))sv-MATCH)))

;; method char_class
(if (not (ignore-errors (find-method 'sv-char_class () ())))
  (defgeneric sv-char_class (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-char_class ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-rule_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "[" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-char_range sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (if (sv-bool (sv-eq "]" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)))))))sv-MATCH)))

;; method string_code
(if (not (ignore-errors (find-method 'sv-string_code () ())))
  (defgeneric sv-string_code (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-string_code ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "\\" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (if (sv-bool (not (sv-eq "" (sv-substr sv-str (sv-to sv-MATCH ) 1)))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "'" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-literal sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (if (sv-bool (sv-eq "'" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-string_code sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-tmp (sv-undef))) (setf sv-tmp sv-MATCH)(setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) (sv-to sv-tmp ))(setf (sv-to m) (sv-to sv-tmp ))(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))))(setf (sv-bool sv-tmp ) (not (sv-bool sv-MATCH)))(setf sv-MATCH sv-tmp)(sv-bool sv-MATCH)) (if (sv-bool (not (sv-eq "" (sv-substr sv-str (sv-to sv-MATCH ) 1)))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))))))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-string_code sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "string_code" sv-MATCH) sv-m2)1) (progn nil)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1)))))))sv-MATCH)))

;; method parsed_code
(if (not (ignore-errors (find-method 'sv-parsed_code () ())))
  (defgeneric sv-parsed_code (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-parsed_code ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-string_code sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-string sv-MATCH))) 1)))))sv-MATCH)))

;; method named_capture_body
(if (not (ignore-errors (find-method 'sv-named_capture_body () ())))
  (defgeneric sv-named_capture_body (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-named_capture_body ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "(" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-rule sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "rule" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ")" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((h (make-hash-table :test 'equal))) (setf (mp-Main::sv-hash-lookup "capturing_group" h) (sv-scalar (mp-Main::sv-hash-lookup "rule" sv-MATCH))) h))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "[" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-rule sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "rule" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq "]" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "rule" sv-MATCH)))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "<" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-metasyntax_exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "metasyntax_exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-Subrule))) (setf (sv-metasyntax m) (sv-scalar (mp-Main::sv-hash-lookup "metasyntax_exp" sv-MATCH))) m))) 1))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (progn (write-line (format nil "~{~a~}" (list "invalid alias syntax")) *error-output*) (sb-ext:quit))) 1)))))))sv-MATCH)))

;; method variables
(if (not (ignore-errors (find-method 'sv-variables () ())))
  (defgeneric sv-variables (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-variables ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "$<" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-rule_ident sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "rule_ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (concatenate 'string (sv-string "$/{") (sv-string (concatenate 'string (sv-string "'") (sv-string (concatenate 'string (sv-string (mp-Main::sv-hash-lookup "rule_ident" sv-MATCH)) (sv-string (concatenate 'string (sv-string "'") (sv-string "}")))))))))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-var_sigil (proto-mp-MiniPerl6-Grammar) sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "MiniPerl6::Grammar.var_sigil" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-digits (proto-mp-MiniPerl6-Grammar) sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "MiniPerl6::Grammar.digits" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (concatenate 'string (sv-string (mp-Main::sv-hash-lookup "MiniPerl6::Grammar.var_sigil" sv-MATCH)) (sv-string (concatenate 'string (sv-string "/[") (sv-string (concatenate 'string (sv-string (mp-Main::sv-hash-lookup "MiniPerl6::Grammar.digits" sv-MATCH)) (sv-string "]")))))))) 1)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-var_sigil (proto-mp-MiniPerl6-Grammar) sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "MiniPerl6::Grammar.var_sigil" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-var_twigil (proto-mp-MiniPerl6-Grammar) sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "MiniPerl6::Grammar.var_twigil" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-full_ident (proto-mp-MiniPerl6-Grammar) sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "MiniPerl6::Grammar.full_ident" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-Var))) (setf (sv-sigil m) (sv-string (mp-Main::sv-hash-lookup "MiniPerl6::Grammar.var_sigil" sv-MATCH)))(setf (sv-twigil m) (sv-string (mp-Main::sv-hash-lookup "MiniPerl6::Grammar.var_twigil" sv-MATCH)))(setf (sv-name m) (sv-string (mp-Main::sv-hash-lookup "MiniPerl6::Grammar.full_ident" sv-MATCH))) m))) 1)))))))))sv-MATCH)))

;; method rule_terms
(if (not (ignore-errors (find-method 'sv-rule_terms () ())))
  (defgeneric sv-rule_terms (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-rule_terms ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "(" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-rule sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "rule" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ")" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-Capture))) (setf (sv-rule_exp m) (sv-scalar (mp-Main::sv-hash-lookup "rule" sv-MATCH))) m))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "<(" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-rule sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "rule" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ")>" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-CaptureResult))) (setf (sv-rule_exp m) (sv-scalar (mp-Main::sv-hash-lookup "rule" sv-MATCH))) m))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "<after" (sv-substr sv-str (sv-to sv-MATCH ) 6))) (+ 1 (setf (sv-to sv-MATCH ) (+ 6 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-rule sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "rule" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-After))) (setf (sv-rule_exp m) (sv-scalar (mp-Main::sv-hash-lookup "rule" sv-MATCH))) m))) 1)))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "<before" (sv-substr sv-str (sv-to sv-MATCH ) 7))) (+ 1 (setf (sv-to sv-MATCH ) (+ 7 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-rule sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "rule" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-Before))) (setf (sv-rule_exp m) (sv-scalar (mp-Main::sv-hash-lookup "rule" sv-MATCH))) m))) 1)))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "<!before" (sv-substr sv-str (sv-to sv-MATCH ) 8))) (+ 1 (setf (sv-to sv-MATCH ) (+ 8 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-rule sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "rule" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-NotBefore))) (setf (sv-rule_exp m) (sv-scalar (mp-Main::sv-hash-lookup "rule" sv-MATCH))) m))) 1)))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "<!" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-metasyntax_exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "metasyntax_exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((h (make-hash-table :test 'equal))) (setf (mp-Main::sv-hash-lookup "negate" h) (let ((h (make-hash-table :test 'equal))) (setf (mp-Main::sv-hash-lookup "metasyntax" h) (sv-scalar (mp-Main::sv-hash-lookup "metasyntax_exp" sv-MATCH))) h)) h))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "<+" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-char_class sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "char_class" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-CharClass))) (setf (sv-chars m) (sv-string (mp-Main::sv-hash-lookup "char_class" sv-MATCH))) m))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "<-" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-char_class sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "char_class" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-NegateCharClass))) (setf (sv-chars m) (sv-string (mp-Main::sv-hash-lookup "char_class" sv-MATCH))) m))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "'" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-literal sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "literal" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq "'" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-Constant))) (setf (sv-constant m) (sv-scalar (mp-Main::sv-hash-lookup "literal" sv-MATCH))) m))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "<" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq "'" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-literal sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "literal" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq "'" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-Constant))) (setf (sv-constant m) (sv-scalar (mp-Main::sv-hash-lookup "literal" sv-MATCH))) m))) 1))))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "<" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-variables sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "variables" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-InterpolateVar))) (setf (sv-var m) (sv-scalar (mp-Main::sv-hash-lookup "variables" sv-MATCH))) m))) 1)))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "?" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-metasyntax_exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "metasyntax_exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-SubruleNoCapture))) (setf (sv-metasyntax m) (sv-scalar (mp-Main::sv-hash-lookup "metasyntax_exp" sv-MATCH))) m))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "." (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-metasyntax_exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "metasyntax_exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-SubruleNoCapture))) (setf (sv-metasyntax m) (sv-scalar (mp-Main::sv-hash-lookup "metasyntax_exp" sv-MATCH))) m))) 1))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-metasyntax_exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "metasyntax_exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-Subrule))) (setf (sv-metasyntax m) (sv-scalar (mp-Main::sv-hash-lookup "metasyntax_exp" sv-MATCH))) m))) 1)))))))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-parsed_code sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "parsed_code" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-Block))) (setf (sv-closure m) (sv-scalar (mp-Main::sv-hash-lookup "parsed_code" sv-MATCH))) m))) 1))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "\\" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-any sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "any" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-SpecialChar))) (setf (sv-char m) (sv-scalar (mp-Main::sv-hash-lookup "any" sv-MATCH))) m))) 1)))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "." (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (make-instance 'mp-Rul-Dot))) 1))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (if (sv-bool (sv-eq "[" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-rule sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "rule" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq "]" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "rule" sv-MATCH)))) 1)))))))))))))))))))))sv-MATCH)))

;; method rule_term
(if (not (ignore-errors (find-method 'sv-rule_term () ())))
  (defgeneric sv-rule_term (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-rule_term ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-variables sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "variables" sv-MATCH) sv-m2)1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq ":=" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-named_capture_body sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "named_capture_body" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-NamedCapture))) (setf (sv-rule_exp m) (sv-scalar (mp-Main::sv-hash-lookup "named_capture_body" sv-MATCH)))(setf (sv-capture_ident m) (sv-scalar (mp-Main::sv-hash-lookup "variables" sv-MATCH))) m))) 1)))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "variables" sv-MATCH)))) 1)))))) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-rule_terms sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "rule_terms" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "rule_terms" sv-MATCH)))) 1))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-and (let ((sv-tmp (sv-undef))) (setf sv-tmp sv-MATCH)(setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) (sv-to sv-tmp ))(setf (sv-to m) (sv-to sv-tmp ))(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq "]" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq ")" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq ">" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq ":" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "?" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "+" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "*" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "|" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "&" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "/" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))))))))))))))(setf (sv-bool sv-tmp ) (not (sv-bool sv-MATCH)))(setf sv-MATCH sv-tmp)(sv-bool sv-MATCH)) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-any sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "any" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-Constant))) (setf (sv-constant m) (sv-scalar (mp-Main::sv-hash-lookup "any" sv-MATCH))) m))) 1))))))))sv-MATCH)))

;; method quant_exp
(if (not (ignore-errors (find-method 'sv-quant_exp () ())))
  (defgeneric sv-quant_exp (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-quant_exp ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "**" (sv-substr sv-str (sv-to sv-MATCH ) 2))) (+ 1 (setf (sv-to sv-MATCH ) (+ 2 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws (proto-mp-MiniPerl6-Grammar) sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (if (sv-bool (sv-eq "{" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-parsed_code sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "parsed_code" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (if (sv-bool (sv-eq "}" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((h (make-hash-table :test 'equal))) (setf (mp-Main::sv-hash-lookup "closure" h) (sv-scalar (mp-Main::sv-hash-lookup "parsed_code" sv-MATCH))) h))) 1))))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq "?" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "*" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "+" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)))))))))sv-MATCH)))

;; method greedy_exp
(if (not (ignore-errors (find-method 'sv-greedy_exp () ())))
  (defgeneric sv-greedy_exp (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-greedy_exp ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (if (sv-bool (sv-eq "?" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (sv-or (progn (setf (sv-to sv-MATCH ) sv-pos1)(if (sv-bool (sv-eq "+" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil)) (progn (setf (sv-to sv-MATCH ) sv-pos1)1)))))sv-MATCH)))

;; method quantifier
(if (not (ignore-errors (find-method 'sv-quantifier () ())))
  (defgeneric sv-quantifier (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-quantifier ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws (proto-mp-MiniPerl6-Grammar) sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-rule_term sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "rule_term" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws2 (proto-mp-MiniPerl6-Grammar) sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-quant_exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "quant_exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-greedy_exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "greedy_exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-opt_ws3 (proto-mp-MiniPerl6-Grammar) sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-Quantifier))) (setf (sv-term m) (sv-scalar (mp-Main::sv-hash-lookup "rule_term" sv-MATCH)))(setf (sv-quant m) (sv-scalar (mp-Main::sv-hash-lookup "quant_exp" sv-MATCH)))(setf (sv-greedy m) (sv-scalar (mp-Main::sv-hash-lookup "greedy_exp" sv-MATCH)))(setf (sv-ws1 m) (sv-scalar (mp-Main::sv-hash-lookup "MiniPerl6::Grammar.opt_ws" sv-MATCH)))(setf (sv-ws2 m) (sv-scalar (mp-Main::sv-hash-lookup "MiniPerl6::Grammar.opt_ws2" sv-MATCH)))(setf (sv-ws3 m) (sv-scalar (mp-Main::sv-hash-lookup "MiniPerl6::Grammar.opt_ws3" sv-MATCH))) m))) 1))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (sv-scalar (mp-Main::sv-hash-lookup "rule_term" sv-MATCH)))) 1))))))))))sv-MATCH)))

;; method concat_list
(if (not (ignore-errors (find-method 'sv-concat_list () ())))
  (defgeneric sv-concat_list (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-concat_list ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-quantifier sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "quantifier" sv-MATCH) sv-m2)1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-concat_list sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "concat_list" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "quantifier" sv-MATCH))) (sv-scalar (mp-Main::sv-hash-lookup "concat_list" sv-MATCH))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_)))) 1))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "quantifier" sv-MATCH)))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_)))) 1)))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (make-array 0 :adjustable 1))) 1)))))sv-MATCH)))

;; method concat_exp
(if (not (ignore-errors (find-method 'sv-concat_exp () ())))
  (defgeneric sv-concat_exp (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-concat_exp ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-concat_list sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "concat_list" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-Concat))) (setf (sv-concat m) (sv-scalar (mp-Main::sv-hash-lookup "concat_list" sv-MATCH))) m))) 1)))))sv-MATCH)))

;; method or_list_exp
(if (not (ignore-errors (find-method 'sv-or_list_exp () ())))
  (defgeneric sv-or_list_exp (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-or_list_exp ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-concat_exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "concat_exp" sv-MATCH) sv-m2)1) (progn nil))) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (if (sv-bool (sv-eq "|" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-or_list_exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "or_list_exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "concat_exp" sv-MATCH))) (sv-scalar (mp-Main::sv-hash-lookup "or_list_exp" sv-MATCH))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_)))) 1)))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (let ((_tmp_ (concatenate 'list  (list (sv-scalar (mp-Main::sv-hash-lookup "concat_exp" sv-MATCH)))))) (make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_)))) 1)))))) (progn (setf (sv-to sv-MATCH ) sv-pos1)(sv-or (progn (setf (sv-capture sv-MATCH) (make-array 0 :adjustable 1))) 1)))))sv-MATCH)))

;; method rule
(if (not (ignore-errors (find-method 'sv-rule () ())))
  (defgeneric sv-rule (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-rule ((sv-grammar mp-MiniPerl6-Grammar-Regex) &optional sv-str sv-pos)
  (block mp6-function
    (let ((sv-MATCH (sv-undef))) (setf sv-MATCH (let ((m (make-instance 'mp-MiniPerl6-Match))) (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) sv-pos)(setf (sv-bool m) 1) m))(setf (sv-bool sv-MATCH ) (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(progn (sv-and (let ((sv-pos1 (sv-undef))) (setf sv-pos1 (sv-to sv-MATCH ))(sv-or (progn (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-ws sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))1) (progn nil))) (if (sv-bool (sv-eq "|" (sv-substr sv-str (sv-to sv-MATCH ) 1))) (+ 1 (setf (sv-to sv-MATCH ) (+ 1 (sv-to sv-MATCH )))) nil))) (progn (setf (sv-to sv-MATCH ) sv-pos1)1))) (sv-and (let ((sv-m2 (sv-undef))) (setf sv-m2 (sv-or_list_exp sv-grammar sv-str (sv-to sv-MATCH )))(if (sv-bool sv-m2) (progn (setf (sv-to sv-MATCH ) (sv-to sv-m2 ))(setf (mp-Main::sv-hash-lookup "or_list_exp" sv-MATCH) sv-m2)1) (progn nil))) (sv-or (progn (setf (sv-capture sv-MATCH) (let ((m (make-instance 'mp-Rul-Or))) (setf (sv-or_list m) (sv-scalar (mp-Main::sv-hash-lookup "or_list_exp" sv-MATCH))) m))) 1))))))sv-MATCH)))

(defmethod sv-perl ((self mp-MiniPerl6-Grammar-Regex))
  (mp-Main::sv-lisp_dump_object "::MiniPerl6::Grammar::Regex" (list )))

)


;; Do not edit this file - Generated by MiniPerl6 3.0
(defpackage mp-Rul
  (:use common-lisp mp-Main))
(defpackage mp-Rul-Quantifier
  (:use common-lisp mp-Main))
(defpackage mp-Rul-Or
  (:use common-lisp mp-Main))
(defpackage mp-Rul-Concat
  (:use common-lisp mp-Main))
(defpackage mp-Rul-Subrule
  (:use common-lisp mp-Main))
(defpackage mp-Rul-SubruleNoCapture
  (:use common-lisp mp-Main))
(defpackage mp-Rul-Var
  (:use common-lisp mp-Main))
(defpackage mp-Rul-Constant
  (:use common-lisp mp-Main))
(defpackage mp-Rul-Dot
  (:use common-lisp mp-Main))
(defpackage mp-Rul-SpecialChar
  (:use common-lisp mp-Main))
(defpackage mp-Rul-Block
  (:use common-lisp mp-Main))
(defpackage mp-Rul-InterpolateVar
  (:use common-lisp mp-Main))
(defpackage mp-Rul-NamedCapture
  (:use common-lisp mp-Main))
(defpackage mp-Rul-Before
  (:use common-lisp mp-Main))
(defpackage mp-Rul-NotBefore
  (:use common-lisp mp-Main))
(defpackage mp-Rul-NegateCharClass
  (:use common-lisp mp-Main))
(defpackage mp-Rul-CharClass
  (:use common-lisp mp-Main))
(defpackage mp-Rul-Capture
  (:use common-lisp mp-Main))
(defpackage mp-Rul-CaptureResult
  (:use common-lisp mp-Main))
(defpackage mp-Rul-After
  (:use common-lisp mp-Main))
;; class Rul
(if (not (ignore-errors (find-class 'mp-Rul)))
  (defclass mp-Rul () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul))
  (defun proto-mp-Rul () x))
(in-package mp-Rul)
  (defun sv-constant (&optional sv-str )
  (block mp6-function (let ((sv-len (sv-undef))) (setf sv-len (length sv-str))(if (sv-bool (sv-eq sv-str "\\")) (progn (setf sv-str "\\\\")) nil)(if (sv-bool (sv-eq sv-str "'")) (progn (setf sv-str "\\'")) nil)(if (sv-bool sv-len) (progn (concatenate 'string (sv-string "( ( '") (sv-string (concatenate 'string (sv-string sv-str) (sv-string (concatenate 'string (sv-string "' eq substr( $str, $MATCH.to, ") (sv-string (concatenate 'string (sv-string sv-len) (sv-string (concatenate 'string (sv-string ")) ") (sv-string (concatenate 'string (sv-string "  ?? (1 + ( $MATCH.to := ") (sv-string (concatenate 'string (sv-string sv-len) (sv-string (concatenate 'string (sv-string " + $MATCH.to ))") (sv-string (concatenate 'string (sv-string "  !! false ") (sv-string ")"))))))))))))))))))) (progn (return-from mp6-function "1"))))))

(in-package mp-Main)
(defmethod sv-perl ((self mp-Rul))
  (mp-Main::sv-lisp_dump_object "::Rul" (list )))




;; class Rul::Quantifier
(if (not (ignore-errors (find-class 'mp-Rul-Quantifier)))
  (defclass mp-Rul-Quantifier () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-Quantifier))
  (defun proto-mp-Rul-Quantifier () x))
;; has $.term
(let ((new-slots (list (list :name 'sv-term
  :readers '(sv-term)
  :writers '((setf sv-term))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Quantifier)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Quantifier :direct-slots new-slots))

;; has $.quant
(let ((new-slots (list (list :name 'sv-quant
  :readers '(sv-quant)
  :writers '((setf sv-quant))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Quantifier)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Quantifier :direct-slots new-slots))

;; has $.greedy
(let ((new-slots (list (list :name 'sv-greedy
  :readers '(sv-greedy)
  :writers '((setf sv-greedy))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Quantifier)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Quantifier :direct-slots new-slots))

;; has $.ws1
(let ((new-slots (list (list :name 'sv-ws1
  :readers '(sv-ws1)
  :writers '((setf sv-ws1))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Quantifier)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Quantifier :direct-slots new-slots))

;; has $.ws2
(let ((new-slots (list (list :name 'sv-ws2
  :readers '(sv-ws2)
  :writers '((setf sv-ws2))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Quantifier)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Quantifier :direct-slots new-slots))

;; has $.ws3
(let ((new-slots (list (list :name 'sv-ws3
  :readers '(sv-ws3)
  :writers '((setf sv-ws3))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Quantifier)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Quantifier :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-Quantifier))
  (block mp6-function
    (progn (sv-emit (sv-term sv-self) ))))

(defmethod sv-perl ((self mp-Rul-Quantifier))
  (mp-Main::sv-lisp_dump_object "::Rul::Quantifier" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "term") (setf (sv-value m) (sv-term self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "quant") (setf (sv-value m) (sv-quant self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "greedy") (setf (sv-value m) (sv-greedy self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "ws1") (setf (sv-value m) (sv-ws1 self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "ws2") (setf (sv-value m) (sv-ws2 self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "ws3") (setf (sv-value m) (sv-ws3 self)) m) )))




;; class Rul::Or
(if (not (ignore-errors (find-class 'mp-Rul-Or)))
  (defclass mp-Rul-Or () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-Or))
  (defun proto-mp-Rul-Or () x))
;; has $.or_list
(let ((new-slots (list (list :name 'sv-or_list
  :readers '(sv-or_list)
  :writers '((setf sv-or_list))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Or)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Or :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-Or))
  (block mp6-function
    (progn (concatenate 'string (sv-string "do { ") (sv-string (concatenate 'string (sv-string "my $pos1 := $MATCH.to; do{ ") (sv-string (concatenate 'string (sv-string (sv-join (let ((tmp (make-array 0 :adjustable 1 :fill-pointer t))) (map nil #'(lambda (c) (push (sv-emit  c) tmp)) (sv-or_list sv-self)) tmp ) "} || do { $MATCH.to := $pos1; ")) (sv-string "} }")))))))))

(defmethod sv-perl ((self mp-Rul-Or))
  (mp-Main::sv-lisp_dump_object "::Rul::Or" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "or_list") (setf (sv-value m) (sv-or_list self)) m) )))




;; class Rul::Concat
(if (not (ignore-errors (find-class 'mp-Rul-Concat)))
  (defclass mp-Rul-Concat () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-Concat))
  (defun proto-mp-Rul-Concat () x))
;; has $.concat
(let ((new-slots (list (list :name 'sv-concat
  :readers '(sv-concat)
  :writers '((setf sv-concat))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Concat)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Concat :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-Concat))
  (block mp6-function
    (progn (concatenate 'string (sv-string "(") (sv-string (concatenate 'string (sv-string (sv-join (let ((tmp (make-array 0 :adjustable 1 :fill-pointer t))) (map nil #'(lambda (c) (push (sv-emit  c) tmp)) (sv-concat sv-self)) tmp ) " && ")) (sv-string ")")))))))

(defmethod sv-perl ((self mp-Rul-Concat))
  (mp-Main::sv-lisp_dump_object "::Rul::Concat" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "concat") (setf (sv-value m) (sv-concat self)) m) )))




;; class Rul::Subrule
(if (not (ignore-errors (find-class 'mp-Rul-Subrule)))
  (defclass mp-Rul-Subrule () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-Subrule))
  (defun proto-mp-Rul-Subrule () x))
;; has $.metasyntax
(let ((new-slots (list (list :name 'sv-metasyntax
  :readers '(sv-metasyntax)
  :writers '((setf sv-metasyntax))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Subrule)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Subrule :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-Subrule))
  (block mp6-function
    (let ((sv-meth (sv-undef))) (setf sv-meth (if (sv-bool (+ 1 (sv-index (sv-metasyntax sv-self) "."))) (sv-metasyntax sv-self) (concatenate 'string (sv-string "$grammar.") (sv-string (sv-metasyntax sv-self)))))(concatenate 'string (sv-string "do { ") (sv-string (concatenate 'string (sv-string "my $m2 := ") (sv-string (concatenate 'string (sv-string sv-meth) (sv-string (concatenate 'string (sv-string "($str, $MATCH.to); ") (sv-string (concatenate 'string (sv-string "if $m2 { $MATCH.to := $m2.to; $MATCH{'") (sv-string (concatenate 'string (sv-string (sv-metasyntax sv-self)) (sv-string (concatenate 'string (sv-string "'} := $m2; 1 } else { false } ") (sv-string "}")))))))))))))))))

(defmethod sv-perl ((self mp-Rul-Subrule))
  (mp-Main::sv-lisp_dump_object "::Rul::Subrule" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "metasyntax") (setf (sv-value m) (sv-metasyntax self)) m) )))




;; class Rul::SubruleNoCapture
(if (not (ignore-errors (find-class 'mp-Rul-SubruleNoCapture)))
  (defclass mp-Rul-SubruleNoCapture () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-SubruleNoCapture))
  (defun proto-mp-Rul-SubruleNoCapture () x))
;; has $.metasyntax
(let ((new-slots (list (list :name 'sv-metasyntax
  :readers '(sv-metasyntax)
  :writers '((setf sv-metasyntax))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-SubruleNoCapture)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-SubruleNoCapture :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-SubruleNoCapture))
  (block mp6-function
    (let ((sv-meth (sv-undef))) (setf sv-meth (if (sv-bool (+ 1 (sv-index (sv-metasyntax sv-self) "."))) (sv-metasyntax sv-self) (concatenate 'string (sv-string "$grammar.") (sv-string (sv-metasyntax sv-self)))))(concatenate 'string (sv-string "do { ") (sv-string (concatenate 'string (sv-string "my $m2 := ") (sv-string (concatenate 'string (sv-string sv-meth) (sv-string (concatenate 'string (sv-string "($str, $MATCH.to); ") (sv-string (concatenate 'string (sv-string "if $m2 { $MATCH.to := $m2.to; 1 } else { false } ") (sv-string "}")))))))))))))

(defmethod sv-perl ((self mp-Rul-SubruleNoCapture))
  (mp-Main::sv-lisp_dump_object "::Rul::SubruleNoCapture" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "metasyntax") (setf (sv-value m) (sv-metasyntax self)) m) )))




;; class Rul::Var
(if (not (ignore-errors (find-class 'mp-Rul-Var)))
  (defclass mp-Rul-Var () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-Var))
  (defun proto-mp-Rul-Var () x))
;; has $.sigil
(let ((new-slots (list (list :name 'sv-sigil
  :readers '(sv-sigil)
  :writers '((setf sv-sigil))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Var)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Var :direct-slots new-slots))

;; has $.twigil
(let ((new-slots (list (list :name 'sv-twigil
  :readers '(sv-twigil)
  :writers '((setf sv-twigil))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Var)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Var :direct-slots new-slots))

;; has $.name
(let ((new-slots (list (list :name 'sv-name
  :readers '(sv-name)
  :writers '((setf sv-name))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Var)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Var :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-Var))
  (block mp6-function
    (let ((sv-table (sv-undef))) (setf sv-table (let ((h (make-hash-table :test 'equal))) (setf (mp-Main::sv-hash-lookup "$" h) "$")(setf (mp-Main::sv-hash-lookup "@" h) "$List_")(setf (mp-Main::sv-hash-lookup "%" h) "$Hash_")(setf (mp-Main::sv-hash-lookup "&" h) "$Code_") h))(concatenate 'string (sv-string (mp-Main::sv-hash-lookup (sv-sigil sv-self) sv-table)) (sv-string (sv-name sv-self))))))

(defmethod sv-perl ((self mp-Rul-Var))
  (mp-Main::sv-lisp_dump_object "::Rul::Var" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "sigil") (setf (sv-value m) (sv-sigil self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "twigil") (setf (sv-value m) (sv-twigil self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "name") (setf (sv-value m) (sv-name self)) m) )))




;; class Rul::Constant
(if (not (ignore-errors (find-class 'mp-Rul-Constant)))
  (defclass mp-Rul-Constant () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-Constant))
  (defun proto-mp-Rul-Constant () x))
;; has $.constant
(let ((new-slots (list (list :name 'sv-constant
  :readers '(sv-constant)
  :writers '((setf sv-constant))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Constant)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Constant :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-Constant))
  (block mp6-function
    (let ((sv-str (sv-undef))) (setf sv-str (sv-constant sv-self))(mp-Rul::sv-constant sv-str))))

(defmethod sv-perl ((self mp-Rul-Constant))
  (mp-Main::sv-lisp_dump_object "::Rul::Constant" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "constant") (setf (sv-value m) (sv-constant self)) m) )))




;; class Rul::Dot
(if (not (ignore-errors (find-class 'mp-Rul-Dot)))
  (defclass mp-Rul-Dot () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-Dot))
  (defun proto-mp-Rul-Dot () x))
;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-Dot))
  (block mp6-function
    (progn (concatenate 'string (sv-string "( ('' ne substr( $str, $MATCH.to, 1 )) ") (sv-string (concatenate 'string (sv-string "  ?? (1 + ($MATCH.to := 1 + $MATCH.to ))") (sv-string (concatenate 'string (sv-string "  !! false ") (sv-string ")")))))))))

(defmethod sv-perl ((self mp-Rul-Dot))
  (mp-Main::sv-lisp_dump_object "::Rul::Dot" (list )))




;; class Rul::SpecialChar
(if (not (ignore-errors (find-class 'mp-Rul-SpecialChar)))
  (defclass mp-Rul-SpecialChar () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-SpecialChar))
  (defun proto-mp-Rul-SpecialChar () x))
;; has $.char
(let ((new-slots (list (list :name 'sv-char
  :readers '(sv-char)
  :writers '((setf sv-char))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-SpecialChar)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-SpecialChar :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-SpecialChar))
  (block mp6-function
    (let ((sv-char (sv-undef))) (setf sv-char (sv-char sv-self))(if (sv-bool (sv-eq sv-char "n")) (let ((sv-rul (sv-undef))) (setf sv-rul (let ((m (make-instance 'mp-Rul-SubruleNoCapture))) (setf (sv-metasyntax m) "is_newline") m))(setf sv-rul (sv-emit sv-rul ))(return-from mp6-function sv-rul)) nil)(if (sv-bool (sv-eq sv-char "N")) (let ((sv-rul (sv-undef))) (setf sv-rul (let ((m (make-instance 'mp-Rul-SubruleNoCapture))) (setf (sv-metasyntax m) "not_newline") m))(setf sv-rul (sv-emit sv-rul ))(return-from mp6-function sv-rul)) nil)(if (sv-bool (sv-eq sv-char "d")) (let ((sv-rul (sv-undef))) (setf sv-rul (let ((m (make-instance 'mp-Rul-SubruleNoCapture))) (setf (sv-metasyntax m) "digit") m))(setf sv-rul (sv-emit sv-rul ))(return-from mp6-function sv-rul)) nil)(if (sv-bool (sv-eq sv-char "s")) (let ((sv-rul (sv-undef))) (setf sv-rul (let ((m (make-instance 'mp-Rul-SubruleNoCapture))) (setf (sv-metasyntax m) "space") m))(setf sv-rul (sv-emit sv-rul ))(return-from mp6-function sv-rul)) nil)(return-from mp6-function (mp-Rul::sv-constant sv-char)))))

(defmethod sv-perl ((self mp-Rul-SpecialChar))
  (mp-Main::sv-lisp_dump_object "::Rul::SpecialChar" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "char") (setf (sv-value m) (sv-char self)) m) )))




;; class Rul::Block
(if (not (ignore-errors (find-class 'mp-Rul-Block)))
  (defclass mp-Rul-Block () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-Block))
  (defun proto-mp-Rul-Block () x))
;; has $.closure
(let ((new-slots (list (list :name 'sv-closure
  :readers '(sv-closure)
  :writers '((setf sv-closure))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Block)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Block :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-Block))
  (block mp6-function
    (progn (concatenate 'string (sv-string "(do { ") (sv-string (concatenate 'string (sv-string (sv-closure sv-self)) (sv-string " } || 1)")))))))

(defmethod sv-perl ((self mp-Rul-Block))
  (mp-Main::sv-lisp_dump_object "::Rul::Block" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "closure") (setf (sv-value m) (sv-closure self)) m) )))




;; class Rul::InterpolateVar
(if (not (ignore-errors (find-class 'mp-Rul-InterpolateVar)))
  (defclass mp-Rul-InterpolateVar () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-InterpolateVar))
  (defun proto-mp-Rul-InterpolateVar () x))
;; has $.var
(let ((new-slots (list (list :name 'sv-var
  :readers '(sv-var)
  :writers '((setf sv-var))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-InterpolateVar)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-InterpolateVar :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-InterpolateVar))
  (block mp6-function
    (progn (mp-Main::sv-say (list (concatenate 'string (sv-string "# TODO: interpolate var ") (sv-string (concatenate 'string (sv-string (sv-emit (sv-var sv-self) )) (sv-string ""))))))(progn (write-line (format nil "~{~a~}" (list )) *error-output*) (sb-ext:quit)))))

(defmethod sv-perl ((self mp-Rul-InterpolateVar))
  (mp-Main::sv-lisp_dump_object "::Rul::InterpolateVar" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "var") (setf (sv-value m) (sv-var self)) m) )))




;; class Rul::NamedCapture
(if (not (ignore-errors (find-class 'mp-Rul-NamedCapture)))
  (defclass mp-Rul-NamedCapture () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-NamedCapture))
  (defun proto-mp-Rul-NamedCapture () x))
;; has $.rule_exp
(let ((new-slots (list (list :name 'sv-rule_exp
  :readers '(sv-rule_exp)
  :writers '((setf sv-rule_exp))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-NamedCapture)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-NamedCapture :direct-slots new-slots))

;; has $.capture_ident
(let ((new-slots (list (list :name 'sv-capture_ident
  :readers '(sv-capture_ident)
  :writers '((setf sv-capture_ident))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-NamedCapture)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-NamedCapture :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-NamedCapture))
  (block mp6-function
    (progn (mp-Main::sv-say (list (concatenate 'string (sv-string "# TODO: named capture ") (sv-string (concatenate 'string (sv-string (sv-capture_ident sv-self)) (sv-string (concatenate 'string (sv-string " := ") (sv-string (concatenate 'string (sv-string (sv-emit (sv-rule_exp sv-self) )) (sv-string ""))))))))))(progn (write-line (format nil "~{~a~}" (list )) *error-output*) (sb-ext:quit)))))

(defmethod sv-perl ((self mp-Rul-NamedCapture))
  (mp-Main::sv-lisp_dump_object "::Rul::NamedCapture" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "rule_exp") (setf (sv-value m) (sv-rule_exp self)) m) (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "capture_ident") (setf (sv-value m) (sv-capture_ident self)) m) )))




;; class Rul::Before
(if (not (ignore-errors (find-class 'mp-Rul-Before)))
  (defclass mp-Rul-Before () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-Before))
  (defun proto-mp-Rul-Before () x))
;; has $.rule_exp
(let ((new-slots (list (list :name 'sv-rule_exp
  :readers '(sv-rule_exp)
  :writers '((setf sv-rule_exp))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Before)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Before :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-Before))
  (block mp6-function
    (progn (concatenate 'string (sv-string "do { ") (sv-string (concatenate 'string (sv-string "my $tmp := $MATCH; ") (sv-string (concatenate 'string (sv-string "$MATCH := MiniPerl6::Match.new( 'str' => $str, 'from' => $tmp.to, 'to' => $tmp.to, 'bool' => 1  ); ") (sv-string (concatenate 'string (sv-string "$MATCH.bool := ") (sv-string (concatenate 'string (sv-string (sv-emit (sv-rule_exp sv-self) )) (sv-string (concatenate 'string (sv-string "; ") (sv-string (concatenate 'string (sv-string "$tmp.bool := ?$MATCH; ") (sv-string (concatenate 'string (sv-string "$MATCH := $tmp; ") (sv-string (concatenate 'string (sv-string "?$MATCH; ") (sv-string "}")))))))))))))))))))))

(defmethod sv-perl ((self mp-Rul-Before))
  (mp-Main::sv-lisp_dump_object "::Rul::Before" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "rule_exp") (setf (sv-value m) (sv-rule_exp self)) m) )))




;; class Rul::NotBefore
(if (not (ignore-errors (find-class 'mp-Rul-NotBefore)))
  (defclass mp-Rul-NotBefore () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-NotBefore))
  (defun proto-mp-Rul-NotBefore () x))
;; has $.rule_exp
(let ((new-slots (list (list :name 'sv-rule_exp
  :readers '(sv-rule_exp)
  :writers '((setf sv-rule_exp))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-NotBefore)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-NotBefore :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-NotBefore))
  (block mp6-function
    (progn (concatenate 'string (sv-string "do { ") (sv-string (concatenate 'string (sv-string "my $tmp := $MATCH; ") (sv-string (concatenate 'string (sv-string "$MATCH := MiniPerl6::Match.new( 'str' => $str, 'from' => $tmp.to, 'to' => $tmp.to, 'bool' => 1  ); ") (sv-string (concatenate 'string (sv-string "$MATCH.bool := ") (sv-string (concatenate 'string (sv-string (sv-emit (sv-rule_exp sv-self) )) (sv-string (concatenate 'string (sv-string "; ") (sv-string (concatenate 'string (sv-string "$tmp.bool := !$MATCH; ") (sv-string (concatenate 'string (sv-string "$MATCH := $tmp; ") (sv-string (concatenate 'string (sv-string "?$MATCH; ") (sv-string "}")))))))))))))))))))))

(defmethod sv-perl ((self mp-Rul-NotBefore))
  (mp-Main::sv-lisp_dump_object "::Rul::NotBefore" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "rule_exp") (setf (sv-value m) (sv-rule_exp self)) m) )))




;; class Rul::NegateCharClass
(if (not (ignore-errors (find-class 'mp-Rul-NegateCharClass)))
  (defclass mp-Rul-NegateCharClass () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-NegateCharClass))
  (defun proto-mp-Rul-NegateCharClass () x))
;; has $.chars
(let ((new-slots (list (list :name 'sv-chars
  :readers '(sv-chars)
  :writers '((setf sv-chars))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-NegateCharClass)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-NegateCharClass :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-NegateCharClass))
  (block mp6-function
    (progn (mp-Main::sv-say (list "TODO NegateCharClass"))(progn (write-line (format nil "~{~a~}" (list )) *error-output*) (sb-ext:quit)))))

(defmethod sv-perl ((self mp-Rul-NegateCharClass))
  (mp-Main::sv-lisp_dump_object "::Rul::NegateCharClass" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "chars") (setf (sv-value m) (sv-chars self)) m) )))




;; class Rul::CharClass
(if (not (ignore-errors (find-class 'mp-Rul-CharClass)))
  (defclass mp-Rul-CharClass () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-CharClass))
  (defun proto-mp-Rul-CharClass () x))
;; has $.chars
(let ((new-slots (list (list :name 'sv-chars
  :readers '(sv-chars)
  :writers '((setf sv-chars))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-CharClass)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-CharClass :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-CharClass))
  (block mp6-function
    (progn (mp-Main::sv-say (list "TODO CharClass"))(progn (write-line (format nil "~{~a~}" (list )) *error-output*) (sb-ext:quit)))))

(defmethod sv-perl ((self mp-Rul-CharClass))
  (mp-Main::sv-lisp_dump_object "::Rul::CharClass" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "chars") (setf (sv-value m) (sv-chars self)) m) )))




;; class Rul::Capture
(if (not (ignore-errors (find-class 'mp-Rul-Capture)))
  (defclass mp-Rul-Capture () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-Capture))
  (defun proto-mp-Rul-Capture () x))
;; has $.rule_exp
(let ((new-slots (list (list :name 'sv-rule_exp
  :readers '(sv-rule_exp)
  :writers '((setf sv-rule_exp))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-Capture)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-Capture :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-Capture))
  (block mp6-function
    (progn (mp-Main::sv-say (list "TODO RulCapture"))(progn (write-line (format nil "~{~a~}" (list )) *error-output*) (sb-ext:quit)))))

(defmethod sv-perl ((self mp-Rul-Capture))
  (mp-Main::sv-lisp_dump_object "::Rul::Capture" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "rule_exp") (setf (sv-value m) (sv-rule_exp self)) m) )))




;; class Rul::CaptureResult
(if (not (ignore-errors (find-class 'mp-Rul-CaptureResult)))
  (defclass mp-Rul-CaptureResult () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-CaptureResult))
  (defun proto-mp-Rul-CaptureResult () x))
;; has $.rule_exp
(let ((new-slots (list (list :name 'sv-rule_exp
  :readers '(sv-rule_exp)
  :writers '((setf sv-rule_exp))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-CaptureResult)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-CaptureResult :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-CaptureResult))
  (block mp6-function
    (progn (mp-Main::sv-say (list "TODO Rul::CaptureResult"))(progn (write-line (format nil "~{~a~}" (list )) *error-output*) (sb-ext:quit)))))

(defmethod sv-perl ((self mp-Rul-CaptureResult))
  (mp-Main::sv-lisp_dump_object "::Rul::CaptureResult" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "rule_exp") (setf (sv-value m) (sv-rule_exp self)) m) )))




;; class Rul::After
(if (not (ignore-errors (find-class 'mp-Rul-After)))
  (defclass mp-Rul-After () ()))

(let (x) 
  (setq x (make-instance 'mp-Rul-After))
  (defun proto-mp-Rul-After () x))
;; has $.rule_exp
(let ((new-slots (list (list :name 'sv-rule_exp
  :readers '(sv-rule_exp)
  :writers '((setf sv-rule_exp))
  :initform '(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class 'mp-Rul-After)))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class 'mp-Rul-After :direct-slots new-slots))

;; method emit
(if (not (ignore-errors (find-method 'sv-emit () ())))
  (defgeneric sv-emit (sv-self)
      (:documentation "a method")))
(defmethod sv-emit ((sv-self mp-Rul-After))
  (block mp6-function
    (progn (mp-Main::sv-say (list "TODO Rul::After"))(progn (write-line (format nil "~{~a~}" (list )) *error-output*) (sb-ext:quit)))))

(defmethod sv-perl ((self mp-Rul-After))
  (mp-Main::sv-lisp_dump_object "::Rul::After" (list (let ((m (make-instance 'mp-Pair))) (setf (sv-key m) "rule_exp") (setf (sv-value m) (sv-rule_exp self)) m) )))





(defun slurp (filename) 
  (format nil "~{~a~%~}" 
    (with-open-file (s filename)
      (loop for line = (read-line s nil nil)
            while line
            collect line into lines
            finally (return lines)))))

(let (source (pos 0) p)
    (if (sv-eq (elt *mp6-args* 1) "-e")
        (setf source (elt *mp6-args* 2))
        (setf source (slurp (elt *mp6-args* 1))))
    ;; (format t "~a" source)
    (sv-say (list ";; Do not edit this file - Generated by MiniPerl6 3.0" ))
    (loop while (< pos (length source)) 
          do (progn
             (setf p (sv-comp_unit (proto-mp-MiniPerl6-Grammar) source pos))
             ;; (format t "~a~%" (sv-perl p))
             (format t "~a~%" (sv-emit_lisp (sv-capture p)))
             ;; (sv-say (list ";; at source pos: " (sv-to p) " source end: " (length source)))
             (setf pos (sv-to p)))))


