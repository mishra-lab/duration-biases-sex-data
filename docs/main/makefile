all:
	make tex && make bib && make tex && make tex
tex:
	pdflatex main
bib:
	biber main
wc:
	python3 ../wc/wc.py summary abstract intro+methods+meth.yss+meth.parts+results+discuss
cls:
	cp ~/dev/tex/pkg/ms/ms/ms.cls .
clean:
	texclean .
figs:
	cd fig && make all
