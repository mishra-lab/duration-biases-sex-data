all:
	make tex && make bib && make tex && make tex
tex:
	pdflatex main
bib:
	biber main
wc:
	detex intro methods results discuss | wc
clean:
	texclean .
