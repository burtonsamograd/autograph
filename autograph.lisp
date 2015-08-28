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
              (cond ((some (lambda (c) (find c "-*+!?#@%/=:<>^")) identifier)
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
                                      ((position c "!?#@%+*/=:<>^")
                                       (write-sequence (aref #("bang" "what" "hash" "at" "percent"
                                                               "plus" "star" "slash" "equals" "colon"
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
