package:
	cd src && make jlc distclean
	tar -cvz --exclude-vcs --exclude .gitkeep --exclude *.tar.gz -f partA-1.tar.gz doc lib src