library(survival)
library(psc)
library(Cairo)
load("flexAtBev.R")
# make CFM
cfm <- pscCFM(fpm_ab)
plot1 <- plot(cfm$datavis[[1]])+theme(plot.title = element_text(colour = "pink"))
plot1+theme(plot.title = element_text(colour = "pink"))
# plot2 <- plot(cfm$datavis[[2]])
# plot3 <- plot(cfm$datavis[[3]])
# plot4 <- plot(cfm$datavis[[4]])
# plot5 <- plot(cfm$datavis[[5]])
# plot6 <- plot(cfm$datavis[[6]])
# plot7 <- plot(cfm$datavis[[7]])
# 
plots <- list()
for(p in 1:length(cfm$datavis)){
  plots[[p]] <- plot(cfm$datavis[[p]])+
    theme(plot.title = element_text(colour = "white")
          # axis.text.x = element_text(colour = "white"),
          # axis.text.y = element_text(colour = "white"),
          # axis.line.x = element_line(colour = "white"),
          # axis.line.y = element_line(colour = "white")
    )
}
dev.off()
# 
# CairoPNG("Output/Images/at_bev_ka.png", width = 400, height = 700,
#     bg = "transparent")
# plot_grid(plot1,
#           plot2,
#           plot3,
#           plot4, 
#           plot5,plot6,plot7,ncol=2)
# dev.off()



CairoPNG("Output/Images/at_bev_ka.png", width = 400, height = 700,
         bg = "transparent")

ggarrange(plotlist = plots, ncol = 2, nrow = 4)
dev.off()

summary <- cfm$datasumm$summ_Table
summary
