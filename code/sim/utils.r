library('reshape2')
library('ggplot2')
library('viridis')

fig.save = function(name,...){
  fname = paste0(file.path('..','out','fig','sim',name),'.pdf')
  print(fname)
  ggsave(fname,...)
}

plot.clean = function(g,dir=1,...){
  g = g + theme_light() +
    scale_color_viridis(discrete=TRUE,option='inferno',direction=dir,begin=.1,end=.9) +
    scale_fill_viridis(discrete=TRUE,option='inferno',direction=dir,begin=.1,end=.9) +
    theme(...,
      strip.background=element_rect(fill='gray85'),
      strip.text.x=element_text(color='black'),
      strip.text.y=element_text(color='black'))
}