all:
	git submodule init
	git submodule update
	sh update.sh
	exit 0
