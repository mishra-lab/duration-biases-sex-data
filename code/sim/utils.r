suppressPackageStartupMessages({
  library('reshape2')
  library('ggplot2')
  library('viridis')
  library('GGally')
  library('rjags')
})
options(width=180)

N = list(i=100,adapt=1e3,iter=1e5,seed=0)
fresh = TRUE

rename = setNames

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

run.jags = function(name,data,vars,inits=NULL){
  set.seed(N$seed)
  file = file.path('sim',paste0(name,'.jags'))
  model = jags.model(
    file = file,
    data = data,
    inits = inits,
    n.adapt = N$adapt,
    quiet = TRUE)
  X.s = melt.samples(coda.samples(
    model = model,
    variable.names = c('rho.z',vars),
    n.iter = N$iter))
}

melt.samples = function(samples){
  X.s = do.call(rbind,lapply(samples,function(s){
    cbind(melt(data.frame(s),id=NULL),i=1:nrow(s)) }))
}

rho.split = function(X.s,keep='other'){
  X.s = rename(split(X.s,list(grepl('^rho\\.z\\.\\d\\.$',X.s$variable))),c('var','rho'))
}

run.fresh = function(name,run){
  uid = paste0(names(N),N,collapse='_')
  fname = file.path('data','.rdata',paste0('Xs_',name,'_',uid,'.rdata'))
  if (fresh){ X.s = run; saveRDS(X.s,fname) } else { X.s = readRDS(fname) }
  return(X.s)
}

fig.name = function(name){
  fname = file.path('..','out','fig','sim',paste0(name,'.pdf'))
}

fig.save = function(name,...){
  fname = fig.name(name)
  print(fname)
  ggsave(fname,...)
}

plot.cmap = function(g,case,...){
  clr.args = list(
    yss      = list(option='viridis',direction=+1,begin=0, end=1,name='Adjustment'),
    yss.fit  = list(option='viridis',direction=+1,begin=.4,end=1,name='Adjustment'),
    grid     = list(option='inferno',direction=-1,begin=.1,end=.9,name='Partnership\nDuration'),
    part.fit = list(option='inferno',direction=-1,begin=.3,end=.7,name='Assumption')
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
