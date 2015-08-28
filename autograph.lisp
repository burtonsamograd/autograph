(require :cl-ppcre)
(require :anaphora)
(use-package :anaphora)

;; Encode a js symbol, graciously stolen from from Parenscript
(let ((cache (make-hash-table :test 'equal)))
  (defun encode-js-identifier (identifier)
    "Given a string, produces to a valid JavaScript identifier by
following transformation heuristics case conversion. For example,
paren-script becomes parenScript, *some-global* becomes SOMEGLOBAL."
    (or (gethash identifier cache)
        (setf (gethash identifier cache)
              (cond ((some (lambda (c) (find c "-+!?#%/=:<>^")) identifier)
                     (let ((lowercase t)
                           (all-uppercase nil))
                       (when (and (not (string= identifier "[]")) ;; HACK
                                  (find-if (lambda (x) (find x '(#\. #\[ #\]))) identifier))
                         (warn "Symbol ~A contains one of '.[]' - this compound naming convention is no longer supported by Parenscript!"
                               identifier))
                       (acond ((nth-value 1 (cl-ppcre:scan-to-strings "[\\*|\\+](.+)[\\*|\\+](.*)"
                                                                      identifier :sharedp t))
                               (setf all-uppercase t
                                     identifier (concatenate 'string (aref it 0) (aref it 1))))
                              ((and (> (length identifier) 1)
                                    (or (eql (char identifier 0) #\+)
                                        (eql (char identifier 0) #\*)))
                               (setf lowercase nil
                                     identifier (subseq identifier 1))))
                       (with-output-to-string (acc)
                         (loop for c across identifier
                            do (acond ((eql c #\-)
                                       (setf lowercase (not lowercase)))
                                      ((position c "!?#%+/=:<>^")
                                       (write-sequence (aref #("bang" "what" "hash" "percent"
                                                               "plus" "slash" "equals" "colon"
                                                               "lessthan" "greaterthan" "caret")
                                                             it)
                                                       acc))
                                      (t (write-char (cond ((and lowercase
                                                                 (not all-uppercase)) 
                                                            (char-downcase c))
                                                           (t (char-upcase c)))
                                                     acc)
                                         (setf lowercase t)))))))
                    ((every #'upper-case-p (remove-if-not #'alpha-char-p identifier)) 
                     (string-downcase identifier))
                    ((every #'lower-case-p (remove-if-not #'alpha-char-p identifier)) 
                     (string-upcase identifier))
                    (t identifier))))))

(defparameter *include-paths* ())

(defmacro include (file)
  ;;(format *error-output* "~A~%" *include-paths*)
  (let (code)
      (catch 'found
        (dolist (include-path *include-paths*)
          (let ((path (concatenate 'string (directory-namestring include-path) file)))
            ;;(format *error-output* "Searching: ~A~%" path)
            (when (probe-file path)
              (with-open-file (f path)
                ;;(format *error-output* "Found: ~A~%" path)
                (do
                 ((form (read f nil) (read f nil)))
                 ((not form))
                  (push form code)))
              (throw 'found (cons 'progn (nreverse code))))))
          (format *error-output* "autograph: Cannot find load file: ~A~%" file))
      ))

(defmacro @ (class &optional thing)
    (concatenate 'string
     "."
     (encode-js-identifier (symbol-name class))
     (when  thing
       (concatenate 'string ":"
                    (encode-js-identifier (symbol-name thing))))))

(defmacro % (class &optional thing)
    (concatenate 'string
     "#"
     (encode-js-identifier (symbol-name class))
     (when  thing
       (concatenate 'string ":"
                    (encode-js-identifier (symbol-name thing))))))

(defun string-lowercase (s)
  (map 'string #'char-downcase s))

(defun expand-rules (rules)
  (flet ((format-rule-values (values)
           (with-output-to-string (s)
               (dolist (value values)
                 (if (stringp value)
                     (format s "~S" value)
                     (if (or (listp value) (boundp value))
                         (format s "~A " (let ((value (eval value)))
                                        (if (symbolp value)
                                            (string-lowercase (symbol-name value))
                                            value)))
                         (format s "~A " (string-lowercase (symbol-name value)))))))))
    
    (with-output-to-string (s)
      (dolist (rule rules)
        (destructuring-bind (first &rest rest) rule
          (format s "~A: ~A;~%"
                  (if (boundp first)
                      (string-lowercase (symbol-name (eval first)))
                      (if (symbolp first)
                          (string-lowercase (symbol-name first))
                          first))
                  (format-rule-values rest)))))))
     
(defmacro css (selector &body rules)
  (if (stringp selector)
      (format t "~A {~% ~A }~%" selector (expand-rules rules))
      (if (listp selector)
          (format t "~A {~% ~A }~%" (eval selector) (expand-rules rules))
          (format t "~A {~% ~A }~%" (string-lowercase (symbol-name selector))
                  (expand-rules rules)))
          ))

(defvar *vars* '())
(defvar *funs* '())

(defun autograph (f)
  (do ((form (read f nil) (read f nil)))
      ((not form))
    (format t "/* ~A */~%" form (car form))
    (case (car form)
      ((defvar defun)
       (case (car form)
         (defvar (push (second form) *vars*))
         (defun (push (second form) *funs*)))
       (eval form))
      (t
         (if (and (find (car form) *funs*) (symbol-function (car form)))
             (eval `(css ,@(eval form)))
             (eval `(css ,@form)))))))

(defmacro while (test &body body)
  `(loop
      (when (not ,test)
        (return))
      ,@body))

(defun main (argv)
  (push (probe-file ".") *include-paths*)
  (if (cdr argv)
      (progn
        (pop argv)
        (while argv
          (let ((arg (pop argv)))
            (cond 
              ((string= arg "-I")
               (let ((dir (pop argv)))
                 (push (probe-file dir) *include-paths*)))
              ((string= arg "--eval")
               (let ((code (pop argv)))
                 (format t "/* --eval ~A~% */" (read-from-string code))
                 (in-package :ps)
                 (eval (read-from-string code))))
              (t
               (let ((probe-results (probe-file arg)))
                 (when probe-results
                   ;; Add current file directory to include paths so they can relative include properly
                   (push (directory-namestring probe-results) *include-paths*)
                   
                   (setf *include-paths* (reverse *include-paths*))
                   (with-open-file (f arg)
                     (handler-bind
                         ((error
                           (lambda (e) 
                             (format *error-output* "~A~%" e)
                             (sb-ext:exit :code 1))))
                       (autograph f))))))))))
      (format *error-output* "Usage: autograph style.autograph > style.css~%")))
