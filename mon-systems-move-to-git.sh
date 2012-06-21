#!/bin/sh
#
#
### ==============================
### :FILE-CREATED <Timestamp: #{2011-04-20T19:24:08-04:00Z}#{11163} - by MON KEY>
### :FILE mon-systems/mon-systems-move-to-git.sh
### ==============================
### :NOTE This script won't work unless $DEVHOME is in your environment 
### elisp> (getenv "DEVHOME")
### shell> $DEVHOME
###
### $DEVHOME
### No doubt there are better ways of doing this with a shell script, but... 
### I F*CKING HATE SHELL SCRIPTING!!!
### ==============================


### CL_MON_CODE=$DEVHOME/some/path/to/mon

MON_SYSTEM_SRC=$CL_MON_CODE/mon-systems
MON_SYSTEM_GIT=$CL_MON_CODE/mon-systems-GIT

# TESTING
#NOT_GIT_DIR=$MON_SYSTEM_GIT/non-existent-dir 

# mon-systems/mon-systems-move-to-git.sh

MON_SYSTEM_FILES="mon.asd
mon-test.asd
alist.lisp
arrays.lisp
bit-twiddle.lisp
char-numeric.lisp
chars.lisp
chronos.lisp
class-doc.lisp
class-utils.lisp
compose.lisp
conditions.lisp
environ.lisp
file-dir.lisp
file-io.lisp
format.lisp
hash.lisp
image-rotate.lisp
introspect.lisp
io.lisp
loadtime-bind.lisp
macros.lisp
numbers.lisp
package.lisp
plist.lisp
regexp.lisp
seqs.lisp
specials.lisp
strings.lisp
types.lisp
LICENSE.txt
make-mon-systems-etags.sh
mon-systems-move-to-git.sh"

# $MON_SYSTEM_SRC/tests
MON_SYSTEM_TEST_FILES="defalias-test
package.lisp
testing.lisp
test.lisp
timing.lisp
timings.lisp
usec-tests.lisp"

ensure_abort_dirs () 
{
    for j in `echo "$MON_SYSTEM_SRC $MON_SYSTEM_SRC/tests $MON_SYSTEM_GIT"`;do
        if [ ! -d "$j" ]
            then 
            echo "A required directory was non-existent: $j";
            printf "\tdeclining to proceed further\n";
            exit 1;
        fi
  done; 
}

ensure_tests_dir ()
{
    if [ ! -d "$MON_SYSTEM_GIT/tests" ]
        then
        mkdir -p "$MON_SYSTEM_GIT/tests";
        echo "Created previously non-existent directory: $MON_SYSTEM_GIT/tests"
        echo
    fi
}


ensure_readme ()
{
 if [ ! -e $MON_SYSTEM_GIT/README ]
  then
     echo "Creating empty README file: $MON_SYSTEM_GIT/README"
     echo 
     touch $MON_SYSTEM_GIT/README
  fi
}


ensure_loadtime_bind ()
{
 if [ ! -e $MON_SYSTEM_GIT/loadtime-bind ]
  then
     echo "Creating template file: $MON_SYSTEM_GIT/loadtime-bind"
     printf "\n(\"<USERNAME>\" . \"<MONIKER>\")\n\n" > $MON_SYSTEM_GIT/loadtime-bind
  fi
}

copy_mon_files ()
{ 
 for f in $MON_SYSTEM_FILES; do 
     if [ ! -e "$MON_SYSTEM_SRC/$f" ]                     # Check if file exists.
     then
        echo ":FILE $MON_SYSTEM_SRC/$f does not exist";  
        echo
        else                         # On to next.
         cp "$MON_SYSTEM_SRC/$f" "$MON_SYSTEM_GIT/$f";
         echo "Copied :FILE $f"; 
         echo "From   :SOURCE $MON_SYSTEM_SRC/ to :DEST $MON_SYSTEM_GIT/";
         echo
     fi
 done;
}

# 

copy_mon_test_files ()
{ 
 for f in $MON_SYSTEM_TEST_FILES; do 
     if [ ! -e "$MON_SYSTEM_SRC/tests/$f" ]                     # Check if file exists.
     then
        echo ":FILE $MON_SYSTEM_SRC/tests/$f does not exist";  
        echo
     else                         # On to next.
         # cp `echo $MON_SYSTEM_SRC/tests/$f $MON_SYSTEM_GIT/tests/$f`;
	 cp $MON_SYSTEM_SRC/tests/$f $MON_SYSTEM_GIT/tests/$f;
         echo "Copied :FILE $f"; 
         echo "From   :SOURCE $MON_SYSTEM_SRC/tests to :DEST $MON_SYSTEM_GIT/tests";
         echo
     fi
 done;
}

etags_src ()
{ 
    cd $MON_SYSTEM_SRC
    if [ -e "$MON_SYSTEM_SRC/make-mon-systems-etags.sh" ] 
    then
	./make-mon-systems-etags.sh
	echo "executing make-mon-systems-etags.sh"
     fi
}

etags_after_copy ()
{
 cd $MON_SYSTEM_GIT
 find . -name '*.lisp' -print | xargs etags -o ./TAGS --language=lisp
 echo "etags created :FILE $MON_SYSTEM_GIT/TAGS"
 echo
}

cd $MON_SYSTEM_SRC

ensure_abort_dirs
ensure_tests_dir
copy_mon_files
copy_mon_test_files
ensure_readme
ensure_loadtime_bind
etags_src
etags_after_copy

exit 0

### ==============================
### EOF

