# aim: plot biased & unbiased: np & rp, for different dur, recall, n
#      to show biases arising if dur is not considered
# notation:
# np     = number of current partners
# rp     = rate of partnership change
# dur    = partnership duration
# recall = period
# n      = number of samples in the simulation (survey n)
# s      = number of simulations (number of surveys) -> 95%~CI

library('reshape2')
library('ggplot2')
library('viridis')

fig.save = function(name,...){
  fname = paste0(file.path('..','out','fig','sim',name),'.pdf')
  print(fname)
  ggsave(fname,...)
}

x.sim = function(rp,dur,recall,n,...){
  # given the input args, simulate reported partners
  dt = 2*(dur+recall)
  t0 = runif(n*rp*dt*2,-dt,+dt)
  tf = t0 + dur
  x = sum(tf>0 & t0<recall) / n }

do.sim = function(X,s=1000){
  # for each X row: run x.sim -> 95%~CI + apply un/biased conversions; manual melt -> value
  X = do.call(rbind,parallel::mclapply(1:nrow(X),mc.cores=7,function(i){
    rp = X$rp[i]; dur = X$dur[i]; recall = X$recall[i]
    x.s = sapply(1:s,function(z){ do.call(x.sim,X[i,]) })
    rp.qs = c(rp, quantile(x.s,c(.025,.975)) / (dur+recall)) # unbiased rp (theo, lo, hi)
    Xii = X[rep(i,12),] # var (2) x bias (2) x est (3)
    # unbiased np, biased np, unbiased rp, biased rp (3 each)
    Xii$value = c(rp.qs*dur, rp.qs*(dur+recall), rp.qs, rp.qs*(dur+recall)/recall)
    return(Xii)
  }))
  # grouping vars
  X$value = pmin(pmax(X$value,.03),1000) # HACK
  X$est  = c('theo','sim.lo','sim.hi')
  X$bias = rep(c('Unbiased','Biased'),each=3)
  X$var = rep(c('Number of Current Partners','Rate of Partnership Change'),each=6)
  return(X)
}

do.plot = function(X,X.ci){
  g = ggplot(X,aes(x=recall,lty=bias)) +
    facet_grid('n~var') +
    geom_line(d=X[X$est=='theo',],aes(y=value,color=factor(dur))) +
    geom_ribbon(data=X.ci,aes(ymin=sim.lo,ymax=sim.hi,fill=factor(dur)),alpha=.1) +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10') +
    scale_color_viridis(discrete=TRUE,option='inferno',direction=-1,begin=.1,end=.9) +
    scale_fill_viridis(discrete=TRUE,option='inferno',direction=-1,begin=.1,end=.9) +
    labs(x='Recall Period',y='Variable Value') +
    labs(color='Partnership\nDuration',fill='Partnership\nDuration',lty='') +
    theme_light() + theme(
      strip.background=element_rect(fill='gray85'),
      strip.text.x=element_text(color='black'),
      strip.text.y=element_text(color='black'))
}

# main
X = expand.grid(
  n = c(10,100,1000),
  recall = round(10^seq(-1,+1,.2),1),
  dur = c(.1,.3,1,3,10),
  rp = 1)
X = do.sim(X)
X0 = X; X0$n = ''
X.ci = dcast(X,n+recall+dur+rp+var+bias~est)
do.plot(X,X.ci[X.ci$bias=='Biased',]);   fig.save('partners.cib',w=6,h=7)
do.plot(X,X.ci[X.ci$bias=='Unbiased',]); fig.save('partners.ciu',w=6,h=7)
do.plot(X0,X.ci[0,]);                    fig.save('partners.m',w=6,h=3)
