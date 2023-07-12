source('sim/utils.r')

durs   = c('swo'=1/30,'swr'=4,'npp'=36)
types  = c('New Clients'='swo','Regular Clients'='swr','Non-Paying'='npp')
vars   =  c('Q: Rate of\nPartnership Change'='Q',
            'K: Number of\nCurrent Partners'='K',
            'x: Number of\nReported Partners'='x')

sim.fsw = function(X,N=328,bias='none'){
  recall.eff = function(recall,dur){ list(
    'Long'=dur,
    'Short'=recall,
    'None'=recall+dur
  )[[bias]] }
  X.s = do.call(rbind,par.lapply(types,function(type){
    X.i = X[X$type==type,]
    data = list(
      x.z = c(-1,X.i$x),
      p.z = X.i$p.adj,
      n.z = X.i$n,
      N.z = nrow(X.i),
      dur = durs[type],
      recall.eff = recall.eff(1,durs[type]),
      N.i = N)
    X.s.i = run.jags('partners',data,vars,inits=list(Q=1))
    X.s.i = cbind(X.s.i,type=type,bias=bias)
  }))
  if (bias=='None'){
    X.p = p.sim.pop(X.s)
    X$lab = mapply(function(x,f){ s = strrep(' ',as.numeric(f)); paste0(s,x,s) },X$lab,X$type)
    X$type = factor(X$type,levels=types,labels=names(types))
    X.p$lab = factor(interaction(X.p$variable,X.p$type,drop=TRUE),labels=X$lab)
    X.p$type = factor(X.p$type,levels=types,labels=names(types))
    g = ggplot(X,aes(x=lab,color=type,fill=type)) +
      facet_grid('~ type',scales='free',space='free') +
      geom_violin(data=X.p,aes(y=value),lty=0,alpha=.4) +
      geom_point(aes(y=p.adj),shape=1) +
      geom_errorbar(aes(ymin=p.025,ymax=p.975),size=.5,width=.2) +
      labs(x='Value',y='Proportion')
    g = plot.clean(plot.cmap(g,'part'))
    fig.save(paste0('partners.fit'),h=2.5,w=1+.3*nrow(X)) }
  X.s = p.sim.pop(X.s,FALSE)
}

main.fsw = function(N=328){
  X = load.data('partners')
  X.s = do.call(rbind,par.lapply(c('None','Short','Long'),function(bias){
    sim.fsw(X,N=N,bias=bias) }))
  q()
  # clean
  omit = (X.s$variable=='Q' & X.s$bias=='Long')  |
         (X.s$variable=='K' & X.s$bias=='Short') |
         !(X.s$variable %in% vars)
  X.s = X.s[!omit,]
  # table data
  X.mci = aggregate(value~variable+type+bias,X.s,mci.named,rnd=2)
  print(X.mci[order(X.mci$variable,X.mci$type),])
  # clean + plot
  X.s$variable = factor(X.s$variable,labels=names(vars))
  X.s$type = factor(X.s$type,labels=names(types))
  g = ggplot(X.s,aes(x=bias,y=value)) +
    facet_grid('type ~ variable',scales='free',space='free_x') +
    scale_y_continuous(trans='log10') +
    geom_violin(aes(fill=type),color=NA,alpha=.6) +
    geom_point(aes(color=type),stat='summary',fun=median,shape=1) +
    labs(x='Bias',y='Variable Value')
  g = plot.clean(plot.cmap(g,'part'),legend.position='top')
  fig.save('partners.fsw',w=6,h=6)
}

main.grid = function(Q=1){
  X = expand.grid(rec = 10^seq(-1,+1,.2), dur = c(.1,.3,1,3,10))
  X.b = rbind( # 3 bias cases
    cbind(X,bias='None',  Q = Q, K = Q*X$dur),
    cbind(X,bias='Short', Q = Q*(X$rec+X$dur)/X$rec, K = NA),
    cbind(X,bias='Long',  Q = NA, K = Q*(X$rec+X$dur)))
  # clean + plot
  X.b.long = melt(X.b,id.vars=c(names(X),'bias'))
  X.b.long$variable = factor(X.b.long$variable,labels=names(vars[1:2]))
  g = ggplot(X.b.long,aes(x=rec,lty=bias,color=factor(dur))) +
    facet_grid('~ variable') +
    geom_line(aes(y=value)) +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10') +
    labs(x='Recall Period',y='Variable Value',lty='Assumption')
  g = plot.clean(plot.cmap(g,'grid'))
  fig.save('partners.grid',w=6,h=3)
}

main.fsw()
main.grid()
