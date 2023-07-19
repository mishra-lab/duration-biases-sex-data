source('sim/utils.r')

biases = c('Short','None','Long')
durs   = c('swo'=1/30,'swr'=4,'npp'=36)
types  = c('New Clients'='swo','Regular Clients'='swr','Non-Paying'='npp')
vars   =  c('Q: Rate of\nPartnership Change'='Q',
            'K: Number of\nCurrent Partners'='K')

plot.fit = function(X,X.s){
  X$lab = mapply(function(x,f){ s = strrep(' ',as.numeric(f)); paste0(s,x,s) },X$lab,X$type)
  X$type = factor(X$type,levels=types,labels=names(types))
  X.s$lab = factor(interaction(X.s$variable,X.s$type,drop=TRUE),labels=X$lab)
  X.s$type = factor(X.s$type,levels=types,labels=names(types))
  g = ggplot(X,aes(x=lab)) +
    facet_grid('~ type',scales='free',space='free') +
    geom_violin(data=X.s,aes(y=value,fill=bias),lty=0,alpha=.4,adjust=3,position='identity') +
    geom_point(aes(y=p.adj),shape=1) +
    geom_errorbar(aes(ymin=p.025,ymax=p.975),lwd=.5,width=.2) +
    labs(x='Value',y='Proportion')
  g = plot.clean(plot.cmap(g,'part.fit'),legend.position='top')
  fig.save(paste0('parts.fit'),h=3.5,w=2+.3*nrow(X))
}

plot.fsw = function(X.s){
  X.s$variable = factor(X.s$variable,levels=vars,labels=names(vars))
  X.s$type = factor(X.s$type,labels=names(types))
  g = ggplot(X.s,aes(x=bias,y=value)) +
    facet_grid('type ~ variable',scales='free',space='free_x') +
    scale_y_continuous(trans='log10') +
    geom_violin(aes(fill=bias),color=NA,alpha=.6,adjust=3) +
    geom_point(aes(color=bias),stat='summary',fun=median,shape=1) +
    labs(x='Assumption',y='Value')
  g = plot.clean(plot.cmap(g,'part.fit'),legend.position='top')
  fig.save('parts.fsw',w=4,h=6)
}

sim.fsw = function(X,bias='none'){
  recall.eff = function(recall,dur){ list(
    'Long'  = dur,
    'Short' = recall,
    'None'  = recall+dur
  )[[bias]] }
  X.s = do.call(rbind,par.lapply(types,function(type){
    X.i = X[X$type==type,]
    data = list(
      x.z = c(-1,X.i$x),
      p.z = X.i$p.adj,
      n.z = X.i$n,
      N.z = nrow(X.i),
      N.i = N$i,
      dur = durs[type],
      recall.eff = recall.eff(1,durs[type]),
      eps = 1e-6)
    X.s.i = run.jags('parts',data,vars,list(Q.shape=1,Q.rate=1))
    X.s.i = cbind(X.s.i,type=type,bias=bias)
  }))
}

main.fsw = function(){
  X = load.data('parts')
  X.s = run.fresh('parts',
    do.call(rbind,par.lapply(biases,function(bias){ sim.fsw(X,bias=bias) })))
  plot.fit(X,rho.split(X.s)$rho)
  X.s = X.s[!(X.s$variable=='Q' & X.s$bias=='Long')  &
            !(X.s$variable=='K' & X.s$bias=='Short') &
             (X.s$variable %in% vars),]
  X.mci = aggregate(value~variable+type+bias,X.s,mci.named,rnd=2)
  print(X.mci[order(X.mci$variable,X.mci$type),])
  plot.fsw(X.s)
}

plot.grid = function(X.b){
  X.b.long = melt(X.b,id.vars=c('rec','dur','bias'))
  X.b.long$variable = factor(X.b.long$variable,labels=names(vars[1:2]))
  g = ggplot(X.b.long,aes(x=rec,lty=bias,color=factor(dur))) +
    facet_grid('~ variable') +
    geom_line(aes(y=value)) +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10') +
    scale_linetype_manual(values=c('solid','21','42')) +
    labs(x='Recall Period',y='Variable Value',lty='Assumption')
  g = plot.clean(plot.cmap(g,'grid'))
  fig.save('parts.grid',w=6,h=3)
}

main.grid = function(Q=1){
  X = expand.grid(rec = 10^seq(-1,+1,.2), dur = c(.1,.3,1,3,10))
  X.b = rbind( # 3 bias cases
    cbind(X,bias='None',  Q = Q, K = Q*X$dur),
    cbind(X,bias='Short', Q = Q*(X$rec+X$dur)/X$rec, K = NA),
    cbind(X,bias='Long',  Q = NA, K = Q*(X$rec+X$dur)))
  plot.grid(X.b)
}

main.fsw()
# main.grid()
