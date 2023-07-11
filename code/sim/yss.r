source('sim/utils.r')

adjs = list(
  'samp' = list(adj.cens=0,adj.stop=0),
  'cens' = list(adj.cens=1,adj.stop=0),
  'all'  = list(adj.cens=1,adj.stop=1))

adj.labs = list(
  'Median' = 'med',
  'Mean' = 'mean',
  '+ Sampling' = 'samp',
  '+ Censoring' = 'cens',
  '+ Measurement' = 'all')

sim.fsw = function(X,N=328,adj='all'){
  data = list(
    D.z = c(-1,X$yss),
    p.z = X$p.adj,
    n.z = X$n,
    N.z = nrow(X),
    adj.cens = adjs[[adj]]$adj.cens,
    adj.stop = adjs[[adj]]$adj.stop,
    p.stop = .45,
    D.max = 35,
    N.i = N)
  X.s = run.jags('yss',data,'D',inits=list(D=1))
  if (adj=='all'){
    g = plot.p.sim(X.s,X,color=colors['yss'])
    fig.save('yss.fit',h=2.5,w=3) }
  X.s = cbind(p.sim.pop(X.s,FALSE),adj=adj)
}

do.plot.adj = function(Xt){
  Xm = aggregate(value~adj,Xt,mean)
  g = ggplot(Xt,aes(x=t)) +
    geom_ribbon(aes(ymin=cdf.lo,ymax=cdf.hi,fill=adj),alpha=.2) +
    geom_line(aes(y=cdf.m,color=adj,lty=adj)) +
    geom_point(data=Xm,aes(x=m,y=1-exp(-1),color=adj),shape=1,show.legend=FALSE) +
    scale_linetype_manual(values=c('11','21','41','81','solid'),name='Adjustment') +
    labs(x='Years selling sex',y='Cumulative proportion')
  g = plot.clean(plot.cmap(g,'yss'))
  fig.save('yss.adj',w=5,h=3)
}

main.fsw = function(N=328){
  X = load.data('yss')
  X.s = do.call(rbind,par.lapply(names(adjs),function(adj){
    sim.fsw(X,N=N,adj=adj) }))
  # table data
  print(aggregate(value~variable+adj,X.s,mci.named,rnd=2))
  # clean + plot
  X.u = data.frame(variable=c('D','D'),adj=c('med','mean'),value=c(4,4/log(2)))
  Xt = merge(aggregate(value~variable+adj,rbind(X.u,X.s),mci.named),list(t=seq(0,20,.1)))
  Xt$adj = factor(Xt$adj,levels=adj.labs,labels=names(adj.labs))
  Xt = cbind(Xt,data.frame(cdf=1-exp(-Xt$t/Xt$value)))
  do.plot.adj(Xt)
}

main.fsw()
