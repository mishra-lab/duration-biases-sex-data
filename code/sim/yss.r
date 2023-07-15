source('sim/utils.r')

adjs = list(
  'samp' = list(adj.cens=0,adj.stop=0),
  'cens' = list(adj.cens=1,adj.stop=0),
  'all'  = list(adj.cens=1,adj.stop=1))

adj.labs = list(
  'med'  = 'Median',
  'mean' = 'Mean',
  'samp' = '+ Sampling',
  'cens' = '+ Censoring',
  'all'  = '+ Measurement')

vars = c('D','S','G')

plot.fit = function(X,X.s){
  X.s$lab = factor(X.s$variable,labels=X$lab)
  g = ggplot(X,aes(x=lab)) +
    geom_violin(data=X.s,aes(y=value,fill=adj),lty=0,alpha=.4,position='identity') +
    geom_point(aes(y=p.adj),shape=1) +
    geom_errorbar(aes(ymin=p.025,ymax=p.975),lwd=.5,width=.2) +
    labs(x='Value',y='Proportion')
  g = plot.clean(plot.cmap(g,'yss.fit'))
  fig.save('yss.fit',h=2.5,w=3+.3*nrow(X))
}

plot.samples = function(X.s){
  pdf(fig.name('yss.samples'),h=8,w=8)
  g = ggpairs(dcast(X.s,adj+i~variable,value='value'),
    aes(color=adj,fill=adj),columns=c('i',vars),
    upper = list(continuous=wrap('density',alpha=0,contour_var='ndensity',bins=7)),
    diag  = list(continuous=wrap('densityDiag',alpha=.2)),
    lower = list(continuous=wrap('points',alpha=.2,size=.1)))
  g = plot.clean(plot.cmap(g,'yss.fit'))
  print(g); dev.off()
}

plot.cdf = function(D.s,t){
  D.t = merge(aggregate(value~variable+adj,D.s,mci.named),list(t=t))
  D.t = cbind(D.t,data.frame(cdf=1-exp(-D.t$t/D.t$value)))
  D.m = aggregate(value~adj,D.t,mean)
  g = ggplot(D.t,aes(x=t)) +
    geom_ribbon(aes(ymin=cdf.lo,ymax=cdf.hi,fill=adj),alpha=.2) +
    geom_line(aes(y=cdf.m,color=adj,lty=adj)) +
    geom_point(data=D.m,aes(x=m,y=1-exp(-1),color=adj),shape=1,show.legend=FALSE) +
    scale_linetype_manual(values=c('11','21','41','81','solid'),name='Adjustment') +
    labs(x='Years selling sex',y='Cumulative proportion')
  g = plot.clean(plot.cmap(g,'yss'))
  fig.save('yss.adj',w=5,h=3)
}

sim.fsw = function(X,adj='all'){
  data = list(
    d.z = c(-1,X$yss),
    p.z = X$p.adj,
    n.z = X$n,
    N.z = nrow(X),
    N.i = N$i,
    adj.cens = adjs[[adj]]$adj.cens,
    adj.stop = adjs[[adj]]$adj.stop,
    p.stop = .45,
    d.max = 35,
    eps = 1e-6)
  X.s = run.jags('yss',data,vars,list(D=1,G=1))
  X.s = cbind(X.s,adj=adj.labs[[adj]])
}

main.fsw = function(){
  X = load.data('yss')
  X.s = do.call(rbind,par.lapply(names(adjs),function(adj){ sim.fsw(X,adj) }))
  plot.samples(rho.split(X.s)$var)
  plot.fit(X,rho.split(X.s)$rho)
  D.s = X.s[X.s$variable=='D',]
  print(aggregate(value~variable+adj,D.s,mci.named,rnd=2))
  D.u = data.frame(variable='D',value=c(4,4/log(2)),i=1,adj=c(adj.labs$med,adj.labs$mean))
  plot.cdf(rbind(D.u,D.s),t=seq(0,20,.1))
}

set.seed(0)
main.fsw()
