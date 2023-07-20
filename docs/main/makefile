all:
	make tex && make bib && make tex && make tex
tex:
	pdflatex main
bib:
	biber main
cls:
	cp ~/dev/tex/pkg/ms/ms/ms.cls .
words:
	python3 wc/wc.py
clean:
	texclean .
