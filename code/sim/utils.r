suppressPackageStartupMessages({
  library('reshape2')
  library('ggplot2')
  library('viridis')
  library('rjags')
})
options(width=180)

rename = setNames

colors = list('yss'='#21908C','swo'='#F3771A','swr'='#BB3754','npp'='#6B186E')

mci.named = function(x,pre='',rnd=Inf){
  mci = c(mean(x),quantile(x,c(.025,.975)))
  mci = rename(round(mci,rnd),paste0(pre,c('m','lo','hi')))
}

load.data = function(name){
  fname = file.path('data',paste0(name,'.csv'))
  X = read.csv(fname,strip.white=TRUE)
  X$n = est.n(X)
  return(X)
}

est.n = function(X,p='p.adj',p.95=c('p.025','p.975')){
  n = sapply(1:nrow(X),function(i){
    pi = X[i,p]
    pi.95 = X[i,p.95]
    e.fun = function(x){
      e = sum((pi.95 - qbeta(c(.025,.975),(10^x)*pi,(10^x)*(1-pi)))^2) }
    ni = 10^optimize(e.fun,c(0,4))$minimum
  })
}

par.lapply = function(...,cores=7){
  parallel::mclapply(...,mc.cores=cores,mc.set.seed=FALSE)
}

melt.samples = function(samples){
  X.s = do.call(rbind,lapply(samples,function(s){ melt(data.frame(s),id=NULL) }))
}

run.jags = function(name,data,vars,...){
  file = file.path('sim',paste0(name,'.jags'))
  model = jags.model(
    file = file,
    data = data,
    quiet = TRUE,
    ...)
  X.s = melt.samples(coda.samples(
    variable.names = c(vars,'p.sim'),
    n.iter = 1e3,
    model = model))
}

p.sim.pop = function(X.s,keep=TRUE){
  b = grepl('^p\\.sim\\.\\d\\.$',X.s$variable)
  X.s = X.s[b==keep,]
}

fig.save = function(name,...){
  fname = paste0(file.path('..','out','fig','sim',name),'.pdf')
  print(fname)
  ggsave(fname,...)
}

plot.cmap = function(g,case,...){
  clr.args = list(
    yss  = list(option='viridis',direction=+1,begin=0, end=1,name='Adjustment'),
    grid = list(option='inferno',direction=-1,begin=.1,end=.9,name='Partnership\nDuration'),
    part = list(option='inferno',direction=-1,begin=.3,end=.7,guide='none')
  )[[case]]
  clr.args = c(clr.args,list(discrete=TRUE,drop=FALSE))
  g = g + do.call(scale_color_viridis,clr.args) + do.call(scale_fill_viridis,clr.args)
}

plot.clean = function(g,...){
  g = g + theme_light() + theme(...,
    strip.background=element_rect(fill='gray85'),
    strip.text.x=element_text(color='black'),
    strip.text.y=element_text(color='black'))
}
