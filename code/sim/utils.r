suppressPackageStartupMessages({
  library('reshape2')
  library('ggplot2')
  library('viridis')
})

fig.save = function(name,...){
  fname = paste0(file.path('..','out','fig','sim',name),'.pdf')
  print(fname)
  ggsave(fname,...)
}

plot.clean = function(g,case,dir=1,...){
  clr.args = list(
    yss      = list(discrete=TRUE,option='viridis',direction=+1,begin=0,end=1),
    partners = list(discrete=TRUE,option='inferno',direction=-1,begin=.1,end=.9)
  )[[case]]
  g = g + theme_light() +
    do.call(scale_color_viridis,clr.args) +
    do.call(scale_fill_viridis,clr.args) +
    theme(...,
      strip.background=element_rect(fill='gray85'),
      strip.text.x=element_text(color='black'),
      strip.text.y=element_text(color='black'))
}