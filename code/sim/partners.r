source('sim/utils.r')

s = 10000

var.labs = c('Q: Rate of\nPartnership Change',
             'K: Number of\nCurrent Partners')

load.fsw = function(){
  # load proportions reporting x -> infer pop mean (with CI)
  types = c('New Clients','Regular Clients','Non-Paying')
  meta = data.frame(
    type = factor(types,levels=types),
    dur = c(1/30, 4, 36), recall = 1, n = 328)
  X.p = read.csv('data/partners.csv',strip.white=TRUE)
  X.p$n = est.n(X.p)
  # print(aggregate(x*p.adj~type,X.p,sum)) # DEBUG ~= mean(x)
  X = do.call(rbind,par.lapply(meta$type,function(type){
    y = X.p[X.p$type==type,]
    n1 = y$n * y$p.adj
    n2 = y$n * (1-y$p.adj)
    x.s = sapply(1:s,function(si){
      p.si = rbeta(nrow(y),n1,n2)
      x.si = sum(y$x*p.si/sum(p.si)) })
    X.y = data.frame(type=type,s=1:s,x=x.s)
  }))
  X.p = merge(meta,X.p,all=TRUE)
  X = merge(meta,X,all=TRUE)
  X$Q = X$x / (X$dur + X$recall)
  X$K = X$Q * X$dur
  plot.fit(X.p,X)
  return(X)
}

plot.fit = function(X.p,X){
  g = ggplot(X.p,aes(x=x,color=type,fill=type)) +
    facet_grid('type~.') +
    geom_density(data=X,aes(y=after_stat(scaled)/2),color=NA,alpha=.5) +
    geom_point(aes(y=p.adj),shape=1) +
    geom_linerange(aes(ymin=p.025,ymax=p.975)) +
    labs(x='x: Reported Partners',y='Proportion')
  g = plot.clean(g,'dur.hide')
  fig.save('partners.fit',w=6,h=5)
}

x.sim = function(Q,dur,recall,n,...){
  dt = 2*(dur+recall)
  t0 = runif(n*Q*dt*2,-dt,+dt)
  tf = t0 + dur
  x = sum(tf>0 & t0<recall) / n
}

do.sim = function(X){
  X.g = split(X,X[c('n','recall','dur')])
  X = do.call(rbind,par.lapply(X.g,function(X.gi){
    Q = X.gi$Q; dur = X.gi$dur; recall = X.gi$recall;
    Q.s = sapply(1:s,function(si){ do.call(x.sim,X.gi[si,]) }) / (dur+recall)
    adj = list('uQ'=1, 'uK'=dur, 'bQ'=(dur+recall)/recall, 'bK'=(dur+recall))
    Xgg = X.gi[rep(1,4),]
    Xgg$true   = sapply(adj,function(a){ mean(a*Q) })
    Xgg$sim.lo = sapply(adj,function(a){ quantile(a*Q.s,.025) })
    Xgg$sim.hi = sapply(adj,function(a){ quantile(a*Q.s,.975) })
    return(Xgg)
  }))
  # grouping vars
  X$bias = rep(c('Unbiased','Biased'),each=2)
  X$var = factor(var.labs,levels=var.labs)
  rownames(X) = NULL
  return(X)
}

main.sens = function(){
  X.sens = do.sim(expand.grid(Q = 1,
      s = 1:s,
      n = c(10,100,1000),
      recall = round(10^seq(-1,+1,.2),1),
      dur = c(.1,.3,1,3,10)))
  i = X.sens$bias=='Biased'; X.sens$sim.lo[i] = NA; X.sens$sim.hi[i] = NA
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
  # print(round(X.fsw[order(X.fsw$type,X.fsw$var),c('true','sim.lo','sim.hi')],2)) # TODO
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
