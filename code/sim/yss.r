source('sim/utils.r')

# constants
s = 10000
q3 = c('q.025'=.025,'q.500'=.5,'q.975'=.975)

# effective n for each stratum from CI
yss.n = function(yss){
  n = sapply(1:nrow(yss),function(i){
    p = yss[i,'p.adj']
    p3 = yss[i,c('p.025','p.adj','p.975')]
    e.fun = function(x){
      n = round(10^x)
      e = sum((p3-qbinom(q3,n,p)/n)^2) }
    n = round(10^optimize(e.fun,c(0,3))$minimum)
  })
}

# probability of rate given RDS-adjusted proportions
l.fun = function(rate){
  d = diff(pexp(c(0,yss$yss),rate)) # stratum sizes
  l = prod(dbinom(round(d*yss$n),yss$n,yss$p.adj)) # likelihood
}

do.sim = function(yss){
  # fitting RDS-adjusted proportions
  rate.opt = optimize(l.fun,c(.01,1),maximum=TRUE)$maximum # MLE
  rate.u = rate.opt * 10^runif(s,-1,+1) # random sample (log)
  l.u    = sapply(rate.u,l.fun)
  rate.s = sample(rate.u,rep=TRUE,prob=l.u) # posterior sample
  # other sdjustments
  X.s = data.frame('None'=rep(4/log(2),s))
  X.s$'Sampling'      = 1/rate.s
  X.s$'+ Measurement' = X.s$'Sampling' * (.55 + .54 * runif(s,.2,.4))
  X.s$'+ Censoring'   = X.s$'+ Measurement' * runif(s,1.5,2)
  return(X.s)
}

do.plot = function(Xt,X){
  g = ggplot(Xt,aes(x=t,fill=adj,lty=adj)) +
    geom_ribbon(aes(ymin=cdf.025,ymax=cdf.975),alpha=.2) +
    geom_line(aes(y=cdf.500,color=adj)) +
    geom_point(data=X,aes(x=q.500,y=1-exp(-1),color=adj),size=1,show.legend=FALSE) +
    scale_linetype_manual(values=c('11','31','61','solid')) +
    labs(x='Years selling sex',y='Cumulative proportion') +
    labs(color='Adjustment',fill='Adjustment',lty='Adjustment')
  g = plot.clean(g)
}

# main
yss = read.csv('sim/yss.csv',strip.white=TRUE)
yss$n = yss.n(yss)
X.s = do.sim(yss)
X = data.frame(t(sapply(X.s,quantile,q3)))
colnames(X) = names(q3)
X$adj = factor(rownames(X),levels=rownames(X))
Xt = merge(X,list(t=seq(0,20,.1)))
Xt$cdf.025 = 1 - exp( - Xt$t / Xt$q.025)
Xt$cdf.500 = 1 - exp( - Xt$t / Xt$q.500)
Xt$cdf.975 = 1 - exp( - Xt$t / Xt$q.975)
g = do.plot(Xt,X)
ggsave('Rplots.pdf',w=5,h=3)