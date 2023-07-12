figs =
figs += diag.yss.censor
figs += diag.yss.gaps
figs += diag.partner.cases
figs += diag.partner.recall
figs += model.yss
figs += model.part
all:
	for fig in $(figs); do \
	  ./maketikz.sh $$fig ;\
	done
