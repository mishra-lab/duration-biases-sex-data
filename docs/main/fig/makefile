all:
	for fig in diag model yss parts; do \
		cat ../fig.$$fig.tex > fig.tex ;\
		pdflatex main ;\
		mv main.pdf fig.$$fig.pdf ;\
		rm fig.tex ;\
	done
