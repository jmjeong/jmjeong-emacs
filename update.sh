#!/bin/sh

cvs -d :pserver:anonymous@bbdb.cvs.sourceforge.net:/cvsroot/bbdb checkout bbdb
# emacs-w3m
cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m
# howm
if [ ! -d howm-1.3.7 ] 
then 
wget http://howm.sourceforge.jp/a/howm-1.3.7.tar.gz
tar zxvf howm-1.3.7.tar.gz
rm howm-1.3.7.tar.gz
fi
# yasnippet
#svn checkout http://yasnippet.googlecode.com/svn/trunk/ yasnippet
if [ ! -d yasnippet-0.5.10 ]
then
wget http://yasnippet.googlecode.com/files/yasnippet-0.5.10.tar.bz2
bunzip2 yasnippet-0.5.10.tar.bz2
tar xvf yasnippet-0.5.10.tar
rm yasnippet-0.5.10.tar
fi

# compile howm
( cd howm-1.3.7 && ./configure && make )
# compile emacs-w3m
( cd emacs-w3m && autoconf && ./configure && make && rm w3m-ems.elc )
# compile bbdb
( cd bbdb && autoconf && ./configure && make autoloads && make )

# byte-compile files
# /usr/bin/emacs --no-init-file --batch -f batch-byte-compile *.el
