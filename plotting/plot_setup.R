library(ggplot2)

dir.create("fig", showWarnings = FALSE, recursive = TRUE, mode = "0777")

theme_set(theme_bw(10))
theme_update(
  # plot.title = element_text(vjust = 0), #size = rel(1), # default vjust = 1
#  axis.title = element_text(size = rel(1.2)), # rel(0.8) # default is not set, i.e. fixed 12 pt?
  #axis.title.y = element_text(vjust = 1), # default is vjust = 1
  # axis.title.x = element_text(vjust = 0), # default is vjust = 1
#  axis.text = element_text(size = rel(1.2)), # default is rel(0.8)
  #legend.position = "bottom",
  legend.position = "right",
  #legend.title = element_text(size = rel(0.8)),
  #legend.title = element_blank()
)

#theme(legend.position="bottom")+
#  theme(plot.title = element_text(size = rel(1), vjust = 0),
#        axis.title = element_text(size = rel(0.8)),
#        axis.title.y = element_text( vjust=2 ),
#        axis.title.x = element_text( vjust=-0.5 ),
#        legend.title = element_text(size=rel(0.8))) +
#  theme(legend.title = element_blank())+

print_plot <- function(plot, name, width=3.5, height=2.5){
  tex_name <- sprintf("fig/%s.tex",name)
  png_name <- sprintf("fig/%s.png",name)
  tex_width <- width
  tex_height <- height
  png_width <- tex_width*4
  png_height <- tex_height*4
  
  tikz(file=tex_name, width=tex_width, height=tex_height,sanitize = TRUE)
  print(plot)
  dev.off()
  
  png(file=png_name, width=png_width, height=png_height, units="cm", res=150)
  print(plot)
  dev.off()
}
