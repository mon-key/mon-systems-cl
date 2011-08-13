#!/bin/sh
#
#
###  ==============================
# :FILE-CREATED <Timestamp: #{2011-04-20T12:58:07-04:00Z}#{11163} - by MON>
# :FILE mon-systems/make-mon-systems-etags.sh
#
# quickly find all the lisp files here:
# shell> find . -name "*.lisp" -print
#
#  Add the current directory to the tail of Emacs' `tags-table-list'
# (add-to-list 'tags-table-list default-directory t)
#
# Make sure to tell customize or it'll wind up bitching:
# (custom-note-var-changed 'tags-table-list)
# 
# Currently not tagging these:
#
# ./deprecated.lisp
# ./emacs-compat.lisp
#
# I've always wondered why authors of CL DSLs often name write their macros
# def-<FOO> (as opposed to some less lispy prefix or no prefix at all). 
# TIL that apparently etags --language=lisp finds pretty much anything "def-" or
# "(pkg:def*" at BOL. This Includes stuff like "(sb-rt:deftest".
#
###  ==============================
etags ./alist.lisp \
./arrays.lisp \
./bit-twiddle.lisp \
./buffer.lisp \
./char-numeric.lisp \
./chars.lisp \
./chronos.lisp \
./class-doc.lisp \
./class-utils.lisp \
./completion.lisp \
./compose.lisp \
./conditions.lisp \
./environ.lisp \
./docs.lisp \
./file-dir.lisp \
./file-io.lisp \
./format.lisp \
./hash.lisp \
./image-rotate.lisp \
./introspect.lisp \
./io.lisp \
./loadtime-bind.lisp \
./macros.lisp \
./numbers.lisp \
./package.lisp \
./plist.lisp \
./regexp.lisp \
./seqs.lisp \
./specials.lisp \
./strings.lisp \
./types.lisp \
./tests/test.lisp \
./tests/timings.lisp \
./tests/testing.lisp \
./tests/usec-tests.lisp \
./tests/timing.lisp \
./tests/package.lisp \
--language=lisp

###  ==============================
### EOF
