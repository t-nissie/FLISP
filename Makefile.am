# Makefile.am for fftw_mpi
# Author: Takeshi NISHIMATSU
##
EXTRA_DIST = README.md README.ja index.ja.html style.css src
index.ja.html: README.ja
	ulmul2html5 -s ulmul2html5.css -n 'Takeshi Nishimatsu' --language=ja $< > $@
clean:
	rm index.ja.html
