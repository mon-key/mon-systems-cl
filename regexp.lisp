;;; :FILE mon-systems/regexp.lisp
;;; ==============================


(in-package #:mon)
;; *package*

;; :COURTESY cl-etsy/base.lisp :WAS `underscore-to-dash'
(defun string-underscore-to-dash (string)
  ;; :WAS (intern (nstring-upcase (cl-ppcre:regex-replace-all "_" string "-"))))
  (declare (type string string))
  (or (and (string-empty-p string)
           (values string nil))
      (cl-ppcre:regex-replace-all "_" string "-")))

;;; :COURTESY bknr-datastore-20100901-git/src/utils/utils.lisp `find-matching-strings'
(defun string-find-matching (regexp strings &key case-sensitive)
  (let ((scanner (cl-ppcre:create-scanner regexp :case-insensitive-mode (not case-sensitive))))
    (remove-if-not #'(lambda (str)
                       (cl-ppcre:scan scanner str)) strings)))


;;; ==============================
;;; :REGEXP-DOCUMENTATION
;;; ==============================

(fundoc 'string-underscore-to-dash
"Replace all occurences of #\\_ with #\\- in STRING.~%~@
Return value is as if by `cl:values':~%
 - First value is STRING with any modifications.
 - Second value is a boolean, T when modifications to STRING were made else NIL.~%~@
Replacement as if by `cl-ppcre:regex-replace-all'.~%~@
:EXAMPLE~%
 \(string-underscore-to-dash \"i_am_an_ugly_string\"\)~%
 \(string-underscore-to-dash \(string-underscore-to-dash \"i_am_an_ugly_string\"\)\)~%
 \(string-underscore-to-dash \"\"\)~%~@
:NOTE Included mostly to remind us to use the :PACKAGE cl-ppcre.~%~@
:SEE-ALSO `dbc:field-name-underscore-to-dash'.~%►►►")

(fundoc 'string-find-matching
"Find matches for REGEXP in STRINGS.~%~@
When keyword CASE-SENSITIVE is non-nil do so as if the keyword arg
CASE-INSENSITIVE-MODE to `cl-ppcre:create-scanner' is non-nil.~%~@
:EXAMPLE~%
 \(string-find-matching \"a.[ab]\" '\(\"a\" \"a a\" \"a b\" \"b a\" \"c d a\"\)\)~%
 \(string-find-matching \"a.[ab]\\\(q.*\\\)\" '\(\"a bqbib\"\)\)~%~@
:SEE-ALSO `cl-ppcre:scan', `cl:find', `cl:find-if', `cl:position',
`cl:search'.~%►►►")

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
