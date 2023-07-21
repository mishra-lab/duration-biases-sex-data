all:
	make tex && make bib && make tex && make tex
tex:
	pdflatex main
bib:
	biber main
wc:
	python3 words/words.py
cls:
	cp ~/dev/tex/pkg/ms/ms/ms.cls .
clean:
	texclean .
