source('sim/utils.r')

s = 100000

# probability of rate given RDS-adjusted proportions
l.fun = function(rate){
  d = diff(pexp(c(0,yss$yss),rate)) # stratum sizes
  l = prod(dbab(d,yss$n,yss$p.adj)) # likelihood
}

do.sim = function(yss){
  # fitting RDS-adjusted proportions
  rate.opt = optimize(l.fun,c(.01,1),maximum=TRUE)$maximum # MLE
  rate.u = rate.opt * 10^runif(s,-1,+1) # random sample (log)
  l.u    = sapply(rate.u,l.fun)
  rate.s = sample(rate.u,rep=TRUE,prob=l.u) # posterior sample
  # other sdjustments
  X.s = data.frame('Median'=rep(4,s),'Mean'=4/log(2))
  X.s$'+ Sampling'    = 1/rate.s
  X.s$'+ Censoring'   = X.s$'+ Sampling' * runif(s,1.5,2)
  X.s$'+ Measurement' = X.s$'+ Censoring' * (.55 + .45 * runif(s,0,1/3))
  return(X.s)
}

do.plot.fit = function(Xt,yss){
  g = ggplot(Xt[Xt$adj=='+ Sampling',],aes(x=t)) +
    geom_ribbon(aes(ymin=cdf.lo,ymax=cdf.hi,fill=adj),alpha=.2) +
    geom_line(aes(y=cdf.m,color=adj,lty=adj)) +
    geom_point(data=yss[1:3,],aes(x=yss,y=cumsum(p.adj)),shape=1,size=2) +
    geom_linerange(data=yss[1:3,],aes(x=yss,
      ymin=qbab(.025,n,cumsum(p.adj)),
      ymax=qbab(.975,n,cumsum(p.adj)))) +
    scale_linetype_manual(values='41',name='Adjustment') +
    labs(x='Years selling sex',y='Cumulative proportion')
  g = plot.clean(g,case='fit')
}

do.plot.adj = function(Xt,X){
  g = ggplot(Xt,aes(x=t)) +
    geom_ribbon(aes(ymin=cdf.lo,ymax=cdf.hi,fill=adj),alpha=.2) +
    geom_line(aes(y=cdf.m,color=adj,lty=adj)) +
    geom_point(data=X,aes(x=m,y=1-exp(-1),color=adj),shape=1,size=1.5,show.legend=FALSE) +
    scale_linetype_manual(values=c('11','21','41','81','solid'),name='Adjustment') +
    labs(x='Years selling sex',y='Cumulative proportion')
  g = plot.clean(g,case='yss')
}

# main
yss = read.csv('data/yss.csv',strip.white=TRUE)
yss$n = est.n(yss)
X.s = do.sim(yss)
X = data.frame(t(sapply(X.s,mci.named)))
print(round(X,2)) # TABLE
X$adj = factor(rownames(X),levels=rownames(X))
Xt = merge(X,list(t=seq(0,20,.1)))
Xt$cdf.m  = 1 - exp( - Xt$t / Xt$m)
Xt$cdf.lo = 1 - exp( - Xt$t / Xt$lo)
Xt$cdf.hi = 1 - exp( - Xt$t / Xt$hi)
g = do.plot.adj(Xt,X);   fig.save('yss.adj',w=5,h=3)
g = do.plot.fit(Xt,yss); fig.save('yss.fit',w=5,h=3)
