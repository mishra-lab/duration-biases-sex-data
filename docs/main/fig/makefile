all:
	for fig in diag model; do \
		cat ../fig.$$fig.tex > fig.tex ;\
		pdflatex main ;\
		mv main.pdf fig.$$fig.pdf ;\
		rm fig.tex ;\
	done
