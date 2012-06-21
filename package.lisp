;;-*- Mode: LISP; Syntax: COMMON-LISP; Encoding: utf-8; Base: 10; -*-
;;; :FILE mon-systems/package.lisp
;;; ==============================

;; (in-package #:mon) ;; for Slime
;; *package*


(defpackage #:mon (:use #:common-lisp) ;; #+sbcl #:sb-int
            (:import-from #:alexandria 
                          #:once-only
                          #-sbcl #:symbolicate 
                          #-sbcl #:keywordicate
                          #-sbcl #:make-gensym-list
                          #-sbcl #:featurep)

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
                #:keywordicate)
  
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
		;; #:run-program
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
   ;; #:*sldb-bitvector-length-no-miser-me*
   ;; #:*sldb-string-length-no-miser-me*
   ;; #:*sldb-printer-bindings-no-miser-me*
   ;; 
   #:doc-set
   #:typedoc
   #:vardoc
   #:fundoc
   #:method-doc
   #:generic-doc
   #:defconst*
   #:defconst
   ;; #:%reevaluate-constant ;; don't export
   ;; #:define-constant      ;; don't export
   ;;
   #:*search-path*
   #:*default-pathname-directory-name-ignorables*
   #:*default-pathname-filename-ignorables*
   #:*user-name*
   #:*default-class-documentation-table*
   #:*documentation-types*
   #:*standard-test-functions*
   #:*error-table*
   #:*time-zones*
   #:*iso-8601-format*
   #:*rfc3339-format*
   #:*timestamp-for-file-header-format*
   #:*timestamp-for-file-format*
   #:*timestamp-for-file-gmt-no-colon-offset-format*
   #:*timestamp-yyyy-mm-dd-format*
   #:*week-days*
   #:*month-names*
   #:*roman-numeral-map*
   #:*whitespace-chars*
   #:*hexadecimal-chars*
   #:*vowel-chars*
   #:*length-unit*
   #:*keyword-hash-inverted*
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
   #:until
   #:while
   #:for
   #:map-do-list
   #:dosublists 
   #:dosequence
   #:doenumerated
   #:dorange
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
   #:bytes-round-to-words
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
   #:read-file-list-from-fprint0-file
   #:read-file-forms
   ;; 
   #:with-pipe-stream 
   #:make-pipe-stream              ;; sb-sys:make-fd-stream, ccl::make-fd-stream, ccl::external-format-character-encoding
   #:pipe-stream-close-read-side
   #:pipe-stream-close-write-side
   ;;
   ;; Following three functions require:
   ;; flex:with-output-to-sequence, chipz:decompress 
   ;; salza2:with-compressor salza2:compress-octet-vector salza2:make-stream-output-callback
   ;; [ sb-ext:octets-to-string | flex:octets-to-string ]
   ;; [ sb-ext:string-to-octets | flex:string-to-octets ]
   #:read-file-gunzip-to-string     
   #:write-string-to-file-gzip      
   #:read-file-gzip-to-gunzip-file
   #:gzip-files-and-delete-source
   ;;
 ;; environ.lisp
   #:executable-find                ;; sb-ext:run-program
   #:setenv                         ;; sb-posix:putenv 
   #:getenv-path-pathnames
   #:posix-working-directory        ;; sb-posix:getcwd, si:getcwd, ccl::current-directory-name, ext:cd
   #:set-posix-working-directory    ;; sb-posix:chdir, si:chdir, ccl::%chdir, ext:cd, 
   #+sbcl #:username-for-system-var-bind   
   #+sbcl #:lisp-implementation-description
   #+sbcl #:username-for-system-var-p      ;; sb-posix:getpwnam sb-posix:passwd-name
   #+sbcl #:syslog-action                  ;; sb-posix:syslog, sb-posix:openlog, sb-posix:closelog
   #+sbcl #:logical-hosts
   ;; 
 ;; introspect.lisp
   ;;
   #:intern-soft
   #:make-keyword-sanely
   #:keyword-prune
   #:keyword-property-to-function
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
   #:find-class-slot-instance
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
   #:proper-sequence
   #:not-null
   #:null-or-nil
   #:not-t
   #:not-boolean
   ;; #:bool
   #:boolean-integer
   #:boolean-or-boolean-integer
   #:not-boolean-integer
   #:string-or-symbol-not-boolean
   #:string-or-symbol-not-empty-nor-boolean
   #:standard-test-function
   #:logical-pathname-designator
   #:pathname-designator
   #:filename-designator
   #:pathname-or-namestring
   #:stream-or-boolean
   #:stream-or-boolean-or-string-with-fill-pointer
   #:symbol-not-null
   #:string-or-symbol
   #:hash-table-or-symbol
   #:hash-table-or-symbol-with-hash
   #:string-with-fill-pointer
   #:symbol-not-a-constant
   #:symbol-not-null-or-string-not-empty
   #:proper-list-not-null
   #:string-or-null
   #:string-not-null
   #:string-empty
   #:string-all-whitespace-safely
   #:string-null-empty-or-all-whitespace
   #:string-not-null-empty-or-all-whitespace
   #:string-not-empty
   #:string-null-or-empty
   #:string-all-whitespace
   #:string-of-length-1
   #:string-not-null-or-empty
   #:simple-string-not-null
   #:simple-string-empty
   #:simple-string-not-empty
   #:simple-string-null-or-empty
   #:simple-string-not-null-or-empty
   #:simple-string-of-length-1   
   #:simple-string-or-null
   #:simple-ascii-string
   #:simple-latin-1-string
   #:standard-char-or-null
   #:digit-char-0-or-1
   #:string-all-digit-char-0-or-1
   #:each-a-string-or-null
   #:each-a-string
   #:each-a-simple-string
   #:each-a-string-of-length-1
   #:each-a-simple-string-of-length-1
   #:each-a-sequence
   #:each-a-sequence-proper
   #:each-a-sequence-proper-or-character
   #:type-every          ;; #:sb-int:every/type
   #:type-any            ;; #:sb-impl::any/type
   #:array-index
   #:array-length
   #:index
   #:index-from-1
   #:index-plus-1
   #:index-or-minus-1
   #:fixnum-bit-width
   #:bignum-bit-width
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
   #:signed-byte-64
   #:signed-byte-32
   #:signed-byte-16
   #:signed-byte-8
   #:uint64
   #:uint32
   #:uint16
   #:uint8
   #:int64
   #:int32
   #:int16
   #:int8
   ;; #:i64
   ;; #:i32
   ;; #:i16
   #:unsigned-byte-128-integer-length
   #:unsigned-byte-96-integer-length
   #:unsigned-byte-64-integer-length
   #:unsigned-byte-56-integer-length
   #:unsigned-byte-48-integer-length
   #:unsigned-byte-32-integer-length
   #:unsigned-byte-29-integer-length
   #:unsigned-byte-24-integer-length
   #:unsigned-byte-16-integer-length
   #:unsigned-byte-8-integer-length 
   #:each-a-character
   #:whitespace-char
   #:char-not-whitespace-char
   #:hexadecimal-char
   #:char-code-integer
   #:char-or-char-code-integer
   #:char-or-char-code-integer-or-string-1
   #:char-or-char-code-integer-or-simple-string-1
   ;; #:character-or-char-code-integer                   ; :ALIAS (don't export)
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
   #:sequence-proper-p
   #:plist-proper-p
   #:plist-proper-not-null-p
   #:list-proper-not-null-p
   #:sequence-zerop        ;; :CALLED-BY `mon:freqs' :FILE seqs.lisp
   #:sequence-type
   #:sequencep
   #:symbol-not-a-constant
   #:symbol-not-null-or-string-not-empty-p
   #:each-a-string-or-vector-in-vector
   #:each-a-string-or-vector-in-list
   #:each-a-string-p
   #:each-a-simple-string-p
   #:string-of-length-1-p
   #:simple-string-of-length-1-p
   #:each-a-string-of-length-1-p
   #:each-a-simple-string-of-length-1-p
   #:each-a-sequence-p
   #:each-a-sequence-proper-p
   #:each-a-sequence-proper-or-character-p
   #:each-a-string-or-null-p
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
   #:string-null-empty-or-all-whitespace-p
   #:string-not-empty-or-all-whitespace-p
   #:string-not-null-empty-or-all-whitespace-p
   #:string-empty-p
   #:string-not-empty-p
   #:simple-string-empty-p
   #:string-not-null-or-empty-p
   #:string-not-null-p
   #:simple-string-not-null-p
   #:simple-string-not-null-or-empty-p
   #:string-all-digit-char-0-or-1-p
   #:fixnump
   #:bignump
   #:fixnum-0-or-over-p
   #:bignum-0-or-over-p
   #:fixnum-bit-width-p
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
   #:logical-pathname-p
   #:filename-designator-p
   #:file-stream-designator-p
   #:pathname-designator-p
   #:pathname-or-namestring-p
   #:pathname-empty-p
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
   #:error-sym         ;; reader-for error-mon
   #:error-sym-type    ;; reader-for error-mon
   #:error-spec        ;; reader-for error-mon
   #:error-args        ;; reader-for error-mon
   ;;
   #:simple-error-mon
   #:error-got         ;; reader-for simple-error-mon
   #:error-type-of     ;; reader-for simple-error-mon
   ;;
   #:case-error   
   #:case-spec         ;; reader-for case-error
   #:case-args         ;; reader-for case-error
   ;;
   #:proper-list-error
   #:circular-list-error
   #:package-error-not
   #:proper-spec       ;; reader-for proper-list-error
   #:proper-args       ;; reader-for proper-list-error
   ;;
   #:slot-non-existent-error
   #:w-object-locus          ;; reader-for slot-non-existent-error
   #:value-for-not-slot      ;; reader-for slot-non-existent-error
   ;;
   #:plist-error
   #:plist-error-locus    ;; reader-for plist-error
   #:plist-not-null-error
   ;;
   #:symbol-not-null-or-string-not-empty-error
   #:symbol-nor-string-error-locus ;; reader-for symbol-not-null-or-string-not-empty-error
   ;;
   #:open-stream-output-stream-error
   #:stream-error-locus   ;; reader-for open-stream-output-stream-error
   ;;
   #:file-error-wild-pathname
   #:pathname-wild-arg-error-locus   ;; reader-for file-error-wild-pathname  
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
   #:file-error-wild-pathname-report
   ;;
   ;; #:format-error
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
   #:plist-hide
   ;;
 ;; hash.lisp
   ;;
   ;; :HACHE-CACHES    sbcl/src/code/early-extensions.lisp
   ;; defines hash-table caches ?memoizations? requires `symbolicate'
   ;; #:*profile-hash-cache*
   ;; #:define-hash-cache
   ;; #:define-cached-synonym
   ;; #:defun-cached
   #:make-hash-table-keyed-by-string
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
   #:hash-found-p
   ;; :NOTE `hash-resize' requires the following:
   ;; sb-thread::with-recursive-system-spinlock, sb-impl::hash-table-spinlock
   ;; sb-impl::hash-table-next-vector, sb-impl::rehash-size, sb-impl::hash-table-rehash-size
   ;; #+sbcl #:hash-resize 
   ;;
 ;; seqs.lisp
   ;;
   #:car-less-than-car
   #:car-greater-than-car
   #:list-length-n-p
   #:list-lengths
   #:list-circular-lengths
   #:list-dotted-length
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
   #:list-n-tuples
   #:list-slice
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
   #:disjoint-sets
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
   #:number-average-seq
   #:number-average-seq-simple
   #:number-power-of-two-ceiling
   #:bits-set-p
   ;; 
 ;; char-numeric.lisp
   ;;
   ;; #:%char-numeric=  ;; INTERNAL!
   ;;
 ;; chars.lisp
   ;;
   #:char-ascii-table
   #:char-to-string
   #:whitespace-char-p
   #:char-not-whitespace-char-p
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
   #:char-or-char-code-integer-or-string-1-ensure-char
   #:char-invert-case-maybe
   #:char-for-readtable-case
   #+sbcl #:char-length         ;; sb-impl::char-len-as-utf8
   #:string-escape-as-unicode-octo-chars 
   #:char-escape-as-unicode-octochar-string
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
   #:string-reduce
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
   #:string-seqs-convert-chars-if
   #:substring
   #:string-insert-char
   #:string-insert-char-3b
   #:nstring-insert-char
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
   #:char-or-char-code-integer-or-string-1-p
   #:char-or-char-code-integer-or-simple-string-1-p
   #:string-lines-to-array
   #:string-subdivide
   #:string-call-with-substrings
   #:string-percent-encode
   #+sbcl #:string-remove-backslashes ;; sb-impl::remove-backslashes
   ;;
 ;; file-dir.lisp
   ;;
   #:substitute-in-file-name    ;; sb-ext:posix-getenv 
   #:pathname-strip-filespec
   #:copy-file 
   #:directorize-namestring
   #:directory-file-name
   #:file-truename
   #:file-directory-p
   #:file-name-directory
   #:file-write-date-timestring
   #+sbcl #:set-file-write-date
   #+sbcl #:set-file-write-date-using-file
   #:timestamp-for-file-with
   #:namestring-directory
   #:namestring-file
   #:directory-parent
   #:directory-unfiltered-p
   #:pathname-directory-append
   #:unix-dot-directory-p
   #:pathname-components
   #:pathname-components-funcallable-pairs
   #:pathname-directory-merged
   #:file-newer-than-file-p
   #:find-file-search-path            ;; osicat-sys:native-namestring
   #:pathname-not-wild-empty-or-dotted-p
   #:pathname-or-namestring-empty-p
   #:pathname-or-namestring-not-empty-dotted-or-wild-p
   #:pathname-native-file-kind        ;; sb-ext:native-namestring/osicat-sys:native-namestring  sb-impl::native-file-kind/osicat:file-kind
   #:remove-directory                 ;; sb-posix:rmdir/osicat-posix:rmdir
   #:probe-directory                  ;; sb-impl::native-file-kind/osicat:file-kind
   #+sbcl #:directory-pathname-ensure ;; sb-ext:parse-native-namestring sb-ext:native-namestring
   #:rename-file*
   #:replace-file
   #:delete-file-if-exists
   #:ensure-file-exists
   #:pathname-file-if
   #:pathname-file-list-if
   #:make-pathname-user-homedir          ;; sb-ext:native-namestring/osicat-sys:native-namestring
   #:pathname-as-directory               ;; osicat:pathname-as-directory/cl-fad:pathname-as-directory
   #:pathname-as-file                    ;; osicat:pathname-as-file/cl-fad:pathname-as-file
   ;; These require cl-fad/osicat
   #:directory-files                     ;; osicat:list-directory osicat:file-exists-p/cl-fad:directory-exists-p 
   #:make-pathname-directory-w-type-wild ;; osicat:file-exists-p/cl-fad:directory-exists-p
   #:pathname-directory-pathname         ;; cl-fad:pathname-as-directory
   #:make-pathname-directory-wildcard

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
   #:bit-format
   #:number-to-bit-list
   #:number-to-bit-vector
   #:bit-vector-to-integer
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
   #:read-skip-line
   #:read-skip-bytes
   #:read-integer-as-text
   ;;
 ;; regexp.lisp 
   ;; 
   #:string-underscore-to-dash   ;; cl-ppcre:regex-replace-all
   #:string-find-matching        ;; cl-ppcre:create-scanner, cl-ppcre:scan 
   #:string-whitespace-to-char       ;; cl-ppcre:create-scanner, regex-replace-all
   #:string-whitespace-to-dash
   #:string-whitespace-to-underscore
   #:string-escape-for-regex
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
   #:time-string-get-universal-time
   #:get-universal-time-string ;; alias of `time-string-get-universal-time'
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
   ;;
 ;; deprecated.lisp
   ;;
   ;; #:bit-convertable-p
   ;; #:bit-convertable
   ;; #:bits-to-number
   ;; #:number-to-bits
   ;;
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
