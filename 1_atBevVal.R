library(survival)
library(survminer)
library(psc)
library(cowplot)
library(Cairo)
library(ggpubr) #!!!

flexAtBev.R  <- load("flexAtBev.R")
ABdata.R <- load("Data/ABdata.R")

# validation discrim

sobj <- fpm_ab$data$m[,1]
cov <- model.matrix(fpm_ab)
cov1 <- model.matrix(fpm_ab)

gam <- fpm_ab$coefficients[grep("gamma", names(fpm_ab$coefficients))]
coef <- fpm_ab$coefficients[-grep("gamma", names(fpm_ab$coefficients))]


lp <- (coef%*%t(cov))

lp_q <- quantile(lp, c(0.05,0.5,0.85))
lp_rg <- cut(lp, c(-Inf, lp_q, Inf), labels = c("Risk group 1",
                                                "Risk group 2",
                                                "Risk group 3",
                                                "Risk group 4"))      


fit <- survfit(sobj ~ lp_rg)
cm <- coxph(sobj ~ lp_rg)
# hrs:
summary(cm)

# concordance:
concordance <- cm$concordance[6]

#somers d:
som <- 2*(concordance-0.5)

# slope 
cm_sl <- coxph(sobj~t(lp))

## discrimination
CairoPNG("Output/Images/atBev_discrim.png", width = 600,
    height = 600, bg = "transparent")

ggsurvplot(fit, data = as.data.frame(cov),
           palette = c("pink2","purple","cyan","dodgerblue3"),
           xlim = c(0,30),
           legend = c(0.8,0.8),
           legend.title = element_blank(),
           legend.labs = c("Risk Group 1", "Risk Group 2", "Risk Group 3","Risk Group 4"),
           xlab = "Time (months)")$plot +
  geom_hline(yintercept = seq(0,1, by = 0.1), lty = 2, colour = "grey")+
  geom_vline(xintercept = seq(0,30, by = 5), lty = 2, colour = "grey") +
  theme(plot.background = element_rect(fill="transparent", colour = NA),
        panel.background = element_rect(fill="transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        axis.title.x = element_text(face = "italic", colour = "white"),
        axis.title.y = element_text(face = "italic", colour = "white"),
        legend.text = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        axis.line.x = element_line(colour="white"),
        axis.line.y = element_line(colour="white"))
dev.off()


## make survival estimattions???
lb_K <- fpm_ab$knots[1]
up_K <- fpm_ab$knots[length(fpm_ab$knots)]
#3 months?????
# #1. derive the log cumulative baseline hazard (spline fctn) at time t:
# s(log(3))=(log(3)-)


summary(fpm_ab, t = c(3,6,12,24), type = "survival")

