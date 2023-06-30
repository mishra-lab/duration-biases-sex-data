suppressPackageStartupMessages({
  library('reshape2')
  library('ggplot2')
  library('viridis')
})
options(width=180)

mci.named = function(x,pre=''){
  mci = c('m'=mean(x),ci.named(x,pre=pre))
}

ci.named = function(x,pre=''){
  ci = setNames(quantile(x,c(.025,.975)),paste0(pre,c('lo','hi')))
}

par.lapply = function(...,cores=7){
  parallel::mclapply(...,mc.cores=cores,mc.set.seed=FALSE)
}

fig.save = function(name,...){
  fname = paste0(file.path('..','out','fig','sim',name),'.pdf')
  print(fname)
  ggsave(fname,...)
}

plot.clean = function(g,case,dir=1,...){
  clr.args = list(
    yss      = list(discrete=TRUE,option='viridis',direction=+1,begin=0, end=1, name='Adjustment'),
    dur      = list(discrete=TRUE,option='inferno',direction=-1,begin=.1,end=.9,name='Partnership\nDuration'),
    dur.hide = list(discrete=TRUE,option='inferno',direction=-1,begin=.2,end=.8,guide='none')
  )[[case]]
  g = g + theme_light() +
    do.call(scale_color_viridis,clr.args) +
    do.call(scale_fill_viridis,clr.args) +
    theme(...,
      strip.background=element_rect(fill='gray85'),
      strip.text.x=element_text(color='black'),
      strip.text.y=element_text(color='black'))
}