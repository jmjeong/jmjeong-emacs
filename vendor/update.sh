#!/bin/sh

# bbdb
cvs -d :pserver:anonymous@bbdb.cvs.sourceforge.net:/cvsroot/bbdb checkout bbdb 
mv bbdb bbdb-cvs
mkdir bbdb
# compile bbdb-cvs
( cd bbdb-cvs && autoconf && ./configure && make autoloads && make )
cp bbdb-cvs/lisp/*.el bbdb-cvs/lisp/*.elc bbdb
cp bbdb-cvs/texinfo/bbdb.info info
rm -rf bbdb-cvs

# emacs-w3m
cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m
# compile emacs-w3m
( cd emacs-w3m && autoconf && ./configure && make && rm w3m-ems.elc )

# yasnippet 
svn checkout http://yasnippet.googlecode.com/svn/trunk/ yasnippet