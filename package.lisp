;;-*- Mode: LISP; Syntax: COMMON-LISP; Encoding: utf-8; Base: 10; -*-
;;; :FILE mon-systems/package.lisp
;;; ==============================

;; (in-package #:mon) ;; for Slime
;; *package*


(defpackage #:mon (:use #:common-lisp) ;; #+sbcl #:sb-int
	    ;; (:nicknames #:mon-system)
	    ;; (:import-from #:arnesi #:parse-float)  ;; #:sb-impl #:last-cons-of
            (:import-from #:alexandria 
                #:once-only
                #-sbcl #:symbolicate 
                #-sbcl #:keywordicate
                #-sbcl #:make-gensym-list
                #-sbcl #:featurep  
                ;; We used to do this but now it is defined by calling out to sb-impl::featurep instead
                ;; #+sbcl #:sb-impl::featurep 
                ;; #:compose 
                ;; #:multiple-value-compose
                )

 ;; sbcl/src/code/early-extensions.lisp
  #+sbcl 
  (:import-from #:sb-int
                ;;
              ;; <MACROS>
		;;
                #:acond
                #:make-gensym-list
                #:awhen
                #:collect
		#:acond
		#:collect
		#:dohash
                ;; #:sb-int:once-only ;; No, use alexandria's instead.
		;;
              ;; <FUNCTIONS>
		;;
                #:symbolicate 
                #:keywordicate
                )
  
  #+sbcl 
  (:import-from #:sb-introspect
		#:function-type
		#:deftype-lambda-list)

  #+sbcl 
  (:import-from #:sb-ext
		;; native-namestring
		;; native-pathname
		;; parse-native-namestring
		;; delete-directory
                ;; sb-ext:posix-environ
                ;; sb-ext:posix-getenv
		#:run-program
		;; sb-ext:process-status
		;; sb-ext:process-plist
		;; sb-ext:process-pid
		;; sb-ext:process-p
		)
  ;;
  ;; #+sbcl (:import-from #:sb-posix #:rmdir)
  ;;
  ;; (:shadowing-import-from  {...} )
  ;;
  (:export 
   ;;
 ;; specials.lisp
   ;;
   #:doc-set
   #:typedoc
   #:vardoc
   #:fundoc
   #:defconst*
   #:defconst
   ;; #:%reevaluate-constant ;; don't export
   ;; #:define-constant      ;; don't export
   ;;
   #:*search-path*
   #:*user-name*
   #:*default-class-documentation-table*
   #:*documentation-types*
   #:*standard-test-functions*
   #:*error-table*
   #:*time-zones*
   #:*week-days*
   #:*month-names*
   #:*roman-numeral-map*
   #:*whitespace-chars*
   #:*hexadecimal-chars*
   #:*vowel-chars*
   #:*length-unit*
   ;;
 ;; macros.lisp
   ;;
   ;;
   #:type-not         ;; #:sb-int:not/type
   #:type-and         ;; #:sb-int:and/type
   #:and-so
   #:dequote
   ;;
   #:defsubst
   #:defalias
   #:bound-and-true-p
   ;;
   #:eval-when-all
   #:eval-when-compile
   #:w-fast-declared
   #:w-debug-declared
   ;;
   #:with-gensyms          ;; sb-int:with-unique-names
   #:retaining-value
   #:multiple-value-setf
   #:once-only         ;; #+sbcl  sb-int:once-only
   #:ref-bind
   #:it             ;; `it' won't be accessible otherwise.
   #:ref-it-if
   #:if-not
   ;;
   #:unitl
   #:while
   #:for
   #:dosublists 
   #:dosequence
   #:doenumerated
   #:dorange
   #:do-until
   #:do-while
   ;;
   #:popn
   #:list-sift
   #:sortf        ;; :NOTE a `define-modify-macro' 
   #:stable-sortf ;; :NOTE a `define-modify-macro'
   ;;
   #:assoc-create
   ;;
   #:to-percent
   #:number-to-double-float
   ;;
   #:string-get
   #:string-set
   #:string-append-into
   #:string-case
   #:string-case-fast  ;; string-case:string-case
   #:output-stream-normalize
   ;;
   #:aset  
   #:mk-array
   #:vector-remove-element
   #:vector-insert-element
   #:vector-map
   ;;
   #:byte-octets-for-integer
   ;;
   #:multiple-value-nth-p
   #:multiple-value-nil-nil-p
   #:multiple-value-t-t-p
   #:multiple-value-t-nil-p
   #:multiple-value-nil-t-p
   #:multiple-value-nil-nil-p
   ;;
   #:condition-case
   ;;
 ;; file-io.lisp
   ;; 
   #:with-opened-file
   #:with-temp-file        ;; alexandria:with-output-to-file
   #:with-each-stream-line
   #:with-output-to-string-or-stream
   #:with-input-file
   #:with-new-file
   #:with-new-file-renaming-old
   #:with-file-overwritten ;; flexi-streams make-flexi-stream'
   #:write-file
   #:read-file-to-string
   #:read-file-forms
   ;;
   ;; following three functions require:
   ;; flex:with-output-to-sequence, chipz:decompress 
   ;; salza2:with-compressor salza2:compress-octet-vector salza2:make-stream-output-callback
   ;; [ sb-ext:octets-to-string | flex:octets-to-string ]
   ;; [ sb-ext:string-to-octets | flex:string-to-octets ]
   #:read-file-gunzip-to-string     
   #:write-string-to-file-gzip      
   #:read-file-gzip-to-gunzip-file
   
 ;; environ.lisp
   #:username-for-system-var-p      ;; sb-posix:getpwnam sb-posix:passwd-name
   #:username-for-system-var-bind   
   #:lisp-implementation-description
   ;; #:
   ;; 
 ;; introspect.lisp
   ;;
   #:intern-soft
   #:make-keyword-sanely
   #:keyword-prune
   #:fset   
   #:where-is
   #:where-is-local          ;; sb-int:sane-package
   #:find-package*
   #:package-external-symbols
   #:do-all-symbols-to-stream ;; alexandria:ensure-list
   #:symbol-external-p
   #:print-symbol-name-qualified
   #:symbol-string-name-check
   #:symbol-string-name-chk-whitespace
   #:read-symbol-name-preserving-case-if
   #:read-symbol-name-preserving-case
   #+sbcl #:symbol-hash      ;; sb-impl::symbol-hash ;; sbcl/src/code/symbol.lisp
   #+sbcl #:function-arglist ;; sb-introspect:function-lambda-list
   ;;
 ;; class-utils.lisp
   ;;
   #:slot-value-or
   #:class-name-of
   #:class-instance-p
   #:class-subclasses ;; sb-mop:class-direct-subclasses
   #:class-name-of
   #:find-class-name-as-string
   #:slot-definition-and-name ;; sb-mop:class-finalized-p, sb-mop:slot-definition-name
   #:class-bound-slot-names
   #+sbcl #:copy-instance-of-class-shallowly ;; sb-mop:class-slots sb-mop:slot-value-using-class sb-mop:slot-boundp-using-class
   ;; Following use:  sb-pcl::slot-definition-allocation, sb-pcl::initargs, sb-pcl::name
   #+sbcl #:class-slot-list-direct
   #+sbcl #:class-slot-list
   #+sbcl #:class-slot-initargs
   #+sbcl #:structure-slots
   ;;
 ;; class-doc-.lisp
   ;;
   #:documented-class-doc                          ;; GENERIC
   #:documented-class-slot-doc                     ;; GENERIC   
   #:make-documented-class      
   #:classdoc
   #:documented-class-verify-init
   ;; #:%verify-hash-table-for-documented-class    ;; INTERNAL 
   #:documented-class-with-docs                    ;; CLASS
   ;;
 ;; types.lisp
   ;;
   #:proper-list     ;; alexandria::proper-list
   #:proper-plist
   #:proper-plist-not-null
   #:circular-list
   #:dotted-list
   #:not-null
   #:not-t
   #:not-boolean
   #:boolean-integer
   #:boolean-or-boolean-integer
   #:not-boolean-integer
   #:string-or-symbol-not-boolean
   #:string-or-symbol-not-empty-nor-boolean
   #:standard-test-function
   #:pathname-designator
   #:filename-designator
   #:stream-or-boolean
   #:stream-or-boolean-or-string-with-fill-pointer
   #:symbol-not-null
   #:string-or-symbol
   #:string-with-fill-pointer
   #:symbol-not-a-constant
   #:symbol-not-null-or-string-not-empty
   #:proper-list-not-null
   #:string-or-null
   #:string-not-null
   #:string-empty
   #:string-not-empty
   #:string-null-or-empty
   #:simple-string-not-null
   #:string-not-null-or-empty
   #:simple-string-empty
   #:simple-string-not-empty
   #:simple-string-null-or-empty
   #:simple-string-not-null-or-empty
   #:simple-string-or-null
   #:simple-ascii-string
   #:simple-latin-1-string
   #:string-all-whitespace
   #:standard-char-or-null
   #:digit-char-0-or-1
   #:string-all-digit-char-0-or-1
   #:each-a-string
   #:each-a-simple-string
   #:each-a-string-of-length-1
   #:type-every          ;; #:sb-int:every/type
   #:type-any            ;; #:sb-impl::any/type
   #:array-index
   #:array-length
   #:index
   #:index-plus-1
   #:index-or-minus-1
   #:fixnum-0-or-over
   #:fixnum-exclusive 
   #:unsigned-byte-128
   #:unsigned-byte-96
   #:unsigned-byte-64
   #:unsigned-byte-56
   #:unsigned-byte-48
   #:unsigned-byte-32
   #:unsigned-byte-29
   #:unsigned-byte-24
   #:unsigned-byte-16
   #:unsigned-byte-8
   #:octet
   #:nibble
   #:each-a-character
   #:whitespace-char
   #:hexadecimal-char
   #:char-code-integer
   #:string-or-char
   #:string-symbol-or-char
   #:string-or-char-or-code-point-integer
   #:string-symbol-or-char-or-code-point-integer
   #:each-a-char-code-integer
   #:each-a-character-or-char-code-integer
   #:bit-vector-octet
   #:bool-vector
   #:byte-array
   #:code-point
   #+sbcl #:closure-obj  ;; sbcl/src/code/kernel.lisp  #:sb-impl::closurep
 ;; :TYPE-PREDICATES
   #:define-list-of-predicate
   #:make-list-of-predicate-name
   #:type-equal
   #:list-circular-p
   #:list-dotted-p
   #:list-proper-p
   #:plist-proper-p
   #:plist-proper-not-null-p
   #:list-proper-not-null-p
   #:sequence-zerop        ;; :CALLED-BY `mon:freqs' :FILE seqs.lisp
   #:sequence-type
   #:sequencep
   #:symbol-not-a-constant
   #:symbol-not-null-or-string-not-empty-p
   #:each-a-string-p
   #:each-a-simple-string-p
   #:each-a-string-of-length-1-p
   #:string-all-whitespace-p
   #:string-all-hex-char-p
   #:string-contains-whitespace-p
   #:string-no-whitespace-p
   #:string-or-null-p
   #:vector-with-fill-pointer-p
   #:string-with-fill-pointer-p
   #:simple-string-or-null-p
   #:simple-string-null-or-empty-p
   #:string-null-or-empty-p
   #:string-empty-p
   #:string-not-empty-p
   #:simple-string-empty-p
   #:string-not-null-or-empty-p
   #:string-not-null-p
   #:simple-string-not-null-p
   #:simple-string-not-null-or-empty-p
   #:string-all-digit-char-0-or-1-p
   #:bignump
   #:fixnump
   #:base-char-p
   #:digit-char-0-or-1-p
   #:each-a-character-p
   #:char-code-integer-p
   #:each-a-char-code-integer-p
   #:each-a-character-or-char-code-integer-p
   #:booleanp
   #:not-boolean-p
   #:not-boolean-integer-p
   #:boolean-integer-p
   #:type-specifier-p
   #:standard-test-function-p
   #:declared-special-p             ;; sb-walker:var-globally-special-p
   #:featurep                       ;; sb-int:featurep      ;;#-sbcl alexandria:eswitch
   #+sbcl #:closure-p               ;; sb-impl::closurep    ;; sbcl/src/code/kernel.lisp 
   #+sbcl #:variable-special-p      ;; sb-walker:var-globally-special-p
   #+sbcl #:type-specifier-valid-p  ;; sb-ext:valid-type-specifier-p
   #+sbcl #:type-expand-all         ;; sb-ext:typexpand-all
   #+sbcl #:type-expand             ;; sb-ext:typexpand 
   #+sbcl #:type-expand-1           ;; sb-ext:typexpand-1
   #+sbcl #:singleton-p             ;; sb-int::singleton-p
   ;;
 ;; conditions.lisp
   ;;
   #:error-mon
   #:error-sym         ;; reader error-mon
   #:error-sym-type    ;; reader error-mon
   #:error-spec        ;; reader error-mon
   #:error-args        ;; reader error-mon
   ;;
   #:simple-error-mon
   #:error-got         ;; reader simple-error-mon
   #:error-type-of     ;; reader simple-error-mon
   ;;
   #:case-error   
   #:case-spec         ;; reader case-error
   #:case-args         ;; reader case-error
   ;;
   #:proper-list-error
   #:circular-list-error
   #:package-error-not
   #:proper-spec       ;; reader proper-list-error
   #:proper-args       ;; reader proper-list-error
   ;;
   #:slot-non-existent-error
   #:w-object-locus          ;; reader slot-non-existent-error
   #:value-for-not-slot      ;; reader slot-non-existent-error
   ;;
   #:plist-error
   #:plist-error-locus    ;; reader plist-error
   #:plist-not-null-error
   ;;
   #:symbol-not-null-or-string-not-empty-error
   #:symbol-nor-string-error-locus ;; reader symbol-not-null-or-string-not-empty-error
   ;;
   #:open-stream-output-stream-error
   #:stream-error-locus   ;; reader open-stream-output-stream-error
   ;;
   #:format-error-symbol-type
   #:ensure-one-of
   #:ensure-signal-or-only
   #:signal-error-or-condition
   #:simple-error-mon-report
   #:package-error-not-report
   #:symbol-not-null-or-string-not-empty-error-report
   #:proper-list-error-report
   #:plist-error-report
   #:open-stream-output-stream-error-report
   #:symbol-not-null-or-error
   #:string-empty-error
   #:format-error
   ;;
   ;; #:eval-code
   ;;
 ;; alist.lisp
   ;;
   #:assq                ;; sb-int:assq
   #:alist-to-plist
   #:nalist-to-plist
   #:alist-sort-keys
   #:alist-eql
   ;;
 ;; plist.lisp
   ;;
   #:put
   #:putf
   #:nplist-to-alist
   #:plist-to-alist
   #:plist-eql
   #:alist-eql
   #:plist-put
   #:plist-get
   #:plist-remove
   #:plist-removef  ;; `define-modify-macro'
   #:plist-delete
   #:plist-deletef  ;; `define-modify-macro'
   #:plist-keys
   #:plist-vaules
   ;;
 ;; hash.lisp
   ;;
   ;; :HACHE-CACHES    sbcl/src/code/early-extensions.lisp
   ;; defines hash-table caches ?memoizations? requires `symbolicate'
   ;; #:*profile-hash-cache*
   ;; #:define-hash-cache
   ;; #:define-cached-synonym
   ;; #:defun-cached
   #:prime-plusp
   #:prime-or-next-greatest
   #:hash-merge
   #:hash-invert-key-val
   #:hash-to-alist
   ;; #:alist-to-hash
   #:hash-from-alist
   #:hash-get-keys
   #:hash-map-sorted
   #:hash-mapcar
   #:hash-pop
   #:hash-car
   #:hash-print
   #:hash-print-key-value-pairs
   #:hash-pprint
   #:hash-or-symbol-p
   ;; :NOTE `hash-resize' requires the following:
   ;; sb-thread::with-recursive-system-spinlock, sb-impl::hash-table-spinlock
   ;; sb-impl::hash-table-next-vector, sb-impl::rehash-size, sb-impl::hash-table-rehash-size
   #+sbcl #:hash-resize 
   ;;
 ;; seqs.lisp
   ;;
   #:car-less-than-car
   #:car-greater-than-car
   #:list-length-n-p
   ;;
   #:car-safe
   #:cdr-safe
   #:car-eq
   #:adjoinq
   #:delq               ;; sb-int:delq
   #:delq-one
   #:remq
   #:delete-all-elts-eq
   #:positions
   #:position-eq        ;; sb-int::posq  
   #:union-eq-keep-first
   #:memq
   #:not-eq             ;; sb-int:neq
   #:last-elt
   #:list-elt
   #:list-last          ;; :ALIAS-OF list-elt
   #:list-quote-elts
   #:list-from-singleton
   #:list-get-singleton
   #:list-dotted-p-destructure
   #+sbcl #:last-cons    ;; sb-impl::last-cons-of
   #+sbcl #:nth-sane     ;; sb-int::nth-but-with-sane-arg-order
   ;;
   #:nth-remove
   #:delete-w-count
   #:delete-dups
   #:nsublist
   #:list-take
   #:list-drop
   #:add-to-list
   #:add-to-nth-list
   #:setcar
   #:setcdr
   ;;
   #:copy-sequence
   #:list-subsets
   #:list-to-array
   #:flatten
   #:list-transpose
   #:interleave
   #:subseq-count
   #:split-seq
   #:list-split-odd-even
   #:list-to-intervals
   #:freqs
   #:group-by-w-hash
   #:group-by-w-list
   #:list-group-by
   #:mapcar-sharing
   #:%sharing-cons
   ;;
 ;; numbers.lisp
   ;;
   #:length-unit-get
   #:length-unit-convert
   #:%
   #:coerce-int-float
   #:number-sequence
   #:number-sequence-loop
   #:number-to-string
   #:string-to-number
   #:parse-float
   #:parse-integer-list
   #:value-in-range-p
   #:random-number-pairs
   #:average-number-seq
   #:number-power-of-two-ceiling
   ;; 
 ;; char-numeric.lisp
   ;;
   ;; #:%char-numeric=  ;; INTERNAL!
   ;;
 ;; chars.lisp
   ;;
   #:char-to-string
   #:whitespace-char-p
   #:hexadecimal-char-p
   #:char-position
   #:chars-not-in-string-p
   #:max-char
   #:ascii-string-p
   #:ascii-simple-string-p
   #:ascii-char-p
   #:ascii-downcase
   #:ascii-equal
   #:ascii-control-p
   #:latin-1-char-p
   #:latin-1-string-p
   #:latin-1-simple-string-p
   #:char-numeric=
   #:char-list-to-string
   #:char-code-integer-to-string
   #:char-code-integer-to-char
   #:char-invert-case-maybe
   #:char-for-readtable-case
   #+sbcl #:char-length         ;; sb-impl::char-len-as-utf8
   ;;
   ;; #+sbcl
   ;; +unicode-replacement-character+     ;; sb-impl::+unicode-replacement-character+
   ;; with-default-decoding-replacement   ;; sb-impl::with-default-decoding-replacement
   ;; with-standard-replacement-character ;; sb-impl::with-standard-replacement-character
   ;;
 ;; strings.lisp
   ;;
   #:string-split-multi         ;; mon::split-seq seqs.lisp 
   #:string-split-on-chars      ;; mon::*whitespace-chars* chars.lisp
   #:string-delimited-to-list
   #:string-split-on-column
   #:string-split-spaces
   #:string-split-newline
   #:string-trim-whitespace
   #:string-from-delimited-list
   #:string-invert-case
   #:string-for-readtable-case
   #:string-explode
   #:string-implode
   #:symbol-name-explode
   #:string-elt
   #:string-length
   #:string-begins
   #:string-coerce-from
   #:string-replace-all
   #:string-cat
   #:string-convert-to-readable
   #:string-to-char
   #:string-upto-char
   #:string-first-char
   #:string-or-symbol-first-char
   #:string-is-nil-like
   #:string-substitute
   #:string-starts-with
   #:string-longest-common-prefix
   #:string-last-word
   #:string-convert-tabs
   #:make-string*
   ;; #:string-simple-string
   #:flatten-list-to-string
   #:concat
   #:substring
   #:string-insert-char
   #:string-insert-char-3b
   #:string-to-number
   #:mapconcat
   #:string-map
   #:string-join-strings
   #:string-no-upper-p
   #:capitalize 
   #:downcase
   #:upcase
   #:capitalize-loosely
   #:downcase-loosely
   #:upcase-loosely
   #:string-symbol-or-char-if
   #:string-or-char-or-code-point-integer-if
   #:string-symbol-or-char-or-code-point-integer-if
   #:string-lines-to-array
   #+sbcl #:string-remove-backslashes ;; sb-impl::remove-backslashes
   ;;
 ;; file-dir.lisp
   ;;
   #:substitute-in-file-name    ;; sb-ext:posix-getenv 
   #:pathname-strip-filespec
   #:copy-file 
   #:directorize-namestring
   #:expand-file-name
   #:directory-file-name
   #:file-truename
   #:file-directory-p
   #:file-name-directory
   #:directory-parent
   #:to-directory  ;; delete once expand-file-name is finished.
   #:pathname-directory-append
   #:unix-dot-directory-p
   #:pathname-components
   #:pathname-directory-merged
   #:file-newer-than-file-p
   #:find-file-search-path
   #+sbcl #:remove-directory  ;; sb-posix:rmdir
   #+sbcl #:probe-directory   ;; sb-impl::native-file-kind
   #+sbcl #:logical-hosts
   #:rename-file*
   #:replace-file
   #:delete-file-if-exists
   #:ensure-file-exists
   ;; These require cl-fad
   #:directory-files
   #:pathname-directory-pathname         ;; cl-fad:pathname-as-directory
   #:make-pathname-directory-wildcard    ;; cl-fad::directory-wildcard
   #:make-pathname-directory-w-type-wild ;; cl-fad:directory-exists-p
   ;; :FINISH-ME
   ;; #:make-pathname-directory-w-name-wild ;; cl-fad::directory-wildcard 
   ;;
   #:pathname-absolute-p
   #+asdf #:default-directory   ;; asdf:truenamize, asdf:pathname-directory-pathname
   #+asdf #:namestring-system
   #+asdf #:pathname-system
   #+asdf #:pathname-directory-system
   #+asdf #:pathname-directory-system-ensure
   ;; sb-ext:native-namestring
   ;; sb-ext:native-pathname
   ;; sb-ext:parse-native-namestring
   ;;
 ;;
   ;; bit-twiddle.lisp
   ;;
   #:bit-from-boolean
   #:boolean-to-bit
   #:byte-swap
   #:byte-request-integer
   ;;
   #:octet-to-bit-vector
   #:octet-logbitp-1-or-0
   #:octet-bit-vector-zeroed
   #:octet-bit-vector-initialize
   #:octet-set-bit-vector-index
   #:octet-set-bit-vector-index-xor
   #:octet-set-bit-vector-index-xor-if
   #:make-array-of-octet-bit-vectors
   #:bytes-to-int
   #:string-to-sha1-byte-array  ;; ironclad:make-digest, ironclad:update-digest, sb-ext:string-to-octets
   #:number-to-byte-array
   ;;
 ;; arrays.lisp
   ;;
   #:array-get-undisplaced
   #:make-string-adjustable
   #:make-bool-vector
   #:bool-vector-p
   #:string-to-bit-vector
   #:bit-vector-to-string
   #:string-ascii-to-byte-array
   #:byte-array-to-hex-string
   #+sbcl #:byte-array-to-string    ;; sb-ext:octets-to-string
   #+sbcl #:string-to-byte-array    ;; sb-ext:string-to-octets
   #:bit-vector-copy
   #:bit-vector-replace
   #+sbcl #:bit-vector-set
   #+sbcl #:bit-vector-clear
   #:vector-remove-elts
   #:vector-copy-extend
   #:shuffle-vector
   #:nshuffle-vector
   #:vector-grow
   #+sbcl  #:vector-shrink   ;; sb-kernel::shrink-vector
   ;; #+sbcl  #:dovector     ;; sb-int::dovector
   ;;
   ;; #:ignore
   ;;
 ;; io.lisp
   ;;
   #:toggle-print-case
   #:eof-p
   #:query-string
   #:press-enter-to-continue
   #:print-spaces
   #:princ-16
   #:indent-by
   #:read-string-with-escapes
   #:read-next-term-char
   #:read-new-random-state-seed
   #:stream-file-descriptor
   #:open-stream-output-stream-p
   ;;
 ;; regexp.lisp 
   ;; 
   #:string-underscore-to-dash   ;; cl-ppcre:regex-replace-all
   #:string-find-matching        ;; cl-ppcre:create-scanner, cl-ppcre:scan 
   ;;
   ;;
 ;; chronos.lisp
   ;; 
   #+sbcl #:current-time          ;; sb-ext:get-time-of-day
   #:time-string-right-now
   #:time-string-yyyy-mm-dd
   #:time-zone-to-string
   #:timestamp
   #:timestamp-for-file
   ;;
 ;; compose.lisp
   ;; :NOTE Don't export the `%compose' macro its only needed by `class-slot-list' and
   ;; `class-slot-initargs' in :FILE class-utils.lisp Use alexandria's `compose'
   ;; `multiple-value-compose' when possible they're nicer
   ;; #:%compose 
   ;;
   #:compose-fun
   #:compose-all
   ;;
 ;; format.lisp
   #:*format-delimited-english-list-templates*
   #:format-delimited-english-list
   #:format-emit-tab
   #:format-list-items-by-n
   ;;
 ;; deprecated.lisp
   ;;
   ;; #:bit-convertable-p
   ;; #:bit-convertable
   ;; #:bits-to-number
   ;; #:number-to-bits
   )) 


;; (in-package #:mon)

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
