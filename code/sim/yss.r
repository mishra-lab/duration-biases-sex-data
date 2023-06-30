source('sim/utils.r')

# constants
s = 100000

# effective n for each stratum from CI
yss.n = function(yss){
  n = sapply(1:nrow(yss),function(i){
    p = yss[i,'p.adj']
    p.95 = yss[i,c('p.025','p.975')]
    e.fun = function(x){
      n = round(10^x)
      e = sum((p.95-qbinom(c(.025,.975),n,p)/n)^2) }
    n = round(10^optimize(e.fun,c(0,3))$minimum)
  })
}

# probability of rate given RDS-adjusted proportions
l.fun = function(rate){
  d = diff(pexp(c(0,yss$yss),rate)) # stratum sizes
  l = prod(dbinom(round(d*yss$n),yss$n,yss$p.adj)) # likelihood
}

z.fun = function(x){
  ci = unname(quantile(x,c(.025,.975)))
  z = c(m=mean(x),lo=ci[1],hi=ci[2])
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
    geom_ribbon(aes(ymin=cdf.lo,ymax=cdf.hi),alpha=.2) +
    geom_line(aes(y=cdf.m,color=adj)) +
    geom_point(data=X,aes(x=m,y=1-exp(-1),color=adj),size=1,show.legend=FALSE) +
    scale_linetype_manual(values=c('11','31','61','solid')) +
    labs(x='Years selling sex',y='Cumulative proportion') +
    labs(color='Adjustment',fill='Adjustment',lty='Adjustment')
  g = plot.clean(g,case='yss')
}

# main
yss = read.csv('sim/yss.csv',strip.white=TRUE)
yss$n = yss.n(yss)
X.s = do.sim(yss)
X = data.frame(t(sapply(X.s,z.fun)))
print(round(X,2)) # data for table
X$adj = factor(rownames(X),levels=rownames(X))
Xt = merge(X,list(t=seq(0,20,.1)))
Xt$cdf.m  = 1 - exp( - Xt$t / Xt$m)
Xt$cdf.lo = 1 - exp( - Xt$t / Xt$lo)
Xt$cdf.hi = 1 - exp( - Xt$t / Xt$hi)
g = do.plot(Xt,X); fig.save('yss.adj',w=5,h=3)
