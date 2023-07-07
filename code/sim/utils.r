suppressPackageStartupMessages({
  library('reshape2')
  library('ggplot2')
  library('viridis')
})
options(width=180)

mci.named = function(x,pre=''){
  mci = setNames(c(mean(x),quantile(x,c(.025,.975))),paste0(pre,c('m','lo','hi')))
}

ci.named = function(x,pre=''){
  ci = setNames(quantile(x,c(.025,.975)),paste0(pre,c('lo','hi')))
}

par.lapply = function(...,cores=7){
  parallel::mclapply(...,mc.cores=cores,mc.set.seed=FALSE)
}

# bab: beta approximation of binomial
rbab = function(n,size,prob){ rbeta(n,size*prob,size*(1-prob)) }
qbab = function(p,size,prob){ qbeta(p,size*prob,size*(1-prob)) }
dbab = function(x,size,prob){ dbeta(x,size*prob,size*(1-prob)) }

est.n = function(X,p='p.adj',p.95=c('p.025','p.975')){
  # effective n for each row from p.95 (bab)
  n = sapply(1:nrow(X),function(i){
    pi = X[i,p]
    pi.95 = X[i,p.95]
    e.fun = function(x){
      e = sum((pi.95 - qbab(c(.025,.975), 10^x, pi))^2) }
    ni = 10^optimize(e.fun,c(0,4))$minimum
  })
}

fig.save = function(name,...){
  fname = paste0(file.path('..','out','fig','sim',name),'.pdf')
  print(fname)
  ggsave(fname,...)
}

plot.clean = function(g,case,dir=1,...){
  clr.args = list(
    fit      = list(discrete=TRUE,option='viridis',direction=+1,begin=.5,end=1,name='Adjustment'),
    yss      = list(discrete=TRUE,option='viridis',direction=+1,begin=0, end=1, name='Adjustment'),
    dur      = list(discrete=TRUE,option='inferno',direction=-1,begin=.1,end=.9,name='Partnership\nDuration'),
    dur.hide = list(discrete=TRUE,option='inferno',direction=-1,begin=.3,end=.7,guide='none')
  )[[case]]
  g = g + theme_light() +
    do.call(scale_color_viridis,clr.args) +
    do.call(scale_fill_viridis,clr.args) +
    theme(...,
      strip.background=element_rect(fill='gray85'),
      strip.text.x=element_text(color='black'),
      strip.text.y=element_text(color='black'))
}