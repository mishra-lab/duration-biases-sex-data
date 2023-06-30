source('sim/utils.r')

vars = c('Q: Rate of\nPartnership Change',
         'K: Number of\nCurrent Partners')

load.fsw = function(){
  # TODO: (?) make csv
  types = c('New Clients','Regular Clients','Non-Paying')
  X = data.frame(recall = 1, n = 328,
    type = factor(types,levels=types),
    x   = c(1.77, 4.69, 0.74),
    dur = c(1/30, 4, 36))
  X$Q = X$x / (X$dur + X$recall)
  X$K = X$Q * X$dur
  return(X)
}

x.sim = function(Q,dur,recall,n,...){
  dt = 2*(dur+recall)
  t0 = runif(n*Q*dt*2,-dt,+dt)
  tf = t0 + dur
  x = sum(tf>0 & t0<recall) / n
}

do.sim = function(X,s=10000){
  X = do.call(rbind,par.lapply(1:nrow(X),function(i){
    Q = X$Q[i]; dur = X$dur[i]; recall = X$recall[i];
    x.s = sapply(1:s,function(.){ do.call(x.sim,X[i,]) })
    Q.mci = c(Q, ci.named(x.s) / (dur+recall)) # unbiased Q (theo, lo, hi)
    adj = c(1, dur, (dur+recall)/recall, (dur+recall)) # uQ, uK, bQ, bK
    Xii = X[rep(i,4),] # var (2) x bias (2)
    Xii$true   = Q.mci[1] * adj
    Xii$sim.lo = Q.mci[2] * adj
    Xii$sim.hi = Q.mci[3] * adj
    return(Xii)
  }))
  # grouping vars
  X$bias = rep(c('Unbiased','Biased'),each=2)
  X$var = factor(vars,levels=vars)
  return(X)
}

main.sens = function(){
  X.sens = do.sim(expand.grid(Q = 1,
      n = c(10,100,1000),
      recall = round(10^seq(-1,+1,.2),1),
      dur = c(.1,.3,1,3,10)))
  g = ggplot(X.sens,aes(x=recall,lty=bias)) +
    facet_grid('n~var') +
    geom_line(aes(y=true,color=factor(dur))) +
    geom_ribbon(aes(ymin=sim.lo,ymax=sim.hi,fill=factor(dur)),alpha=.2) +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10') +
    labs(x='Recall Period',y='Variable Value',lty='')
  g = plot.clean(g,case='dur')
  fig.save('partners.sens',w=6,h=7)
}

main.fsw = function(){
  X.fsw = do.sim(load.fsw())
  print(round(X.fsw[order(X.fsw$type,X.fsw$var),c('true','sim.lo','sim.hi')],2))
  g = ggplot(X.fsw,aes(x=bias,color=type)) +
    facet_grid('type~var',scales='free') +
    geom_point(aes(y=true),shape=1,size=1.5) +
    geom_linerange(aes(ymin=sim.lo,ymax=sim.hi)) +
    labs(y='Variable Value',x='',shape='') +
    expand_limits(y=c(0,1))
  g = plot.clean(g,case='dur.hide')
  fig.save('partners.fsw',w=4,h=5)
}

# main.sens()
# main.fsw()
