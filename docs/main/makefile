all:
	make tex && make bib && make tex && make tex
tex:
	pdflatex main
bib:
	biber main
words:
	python3 wc/wc.py
clean:
	texclean .
