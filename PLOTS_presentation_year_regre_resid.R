# --- set working directory
rm(list = ls())
setwd('C:/Users/lomo0003/SLU/YaraNSensor/')

require(gridExtra)
require(ggplot2)
require(tidyverse)

f_plsr_all_cv     <- read.table("./Data_analysis/results_plot_presentation/data_15_jan_graficos_apresentacao.csv", sep = ";", header = T)
f_plsr_calval     <- read.table("./Data_analysis/results_plot_presentation/PLS_for_plot_firstderivative_SNV.txt", sep = "\t", header = T)
f_plsr_50         <- read.table("./Data_analysis/results_plot_presentation/data_15_jan_graficos_apresentacao_less50.csv", sep = ";", header = T)  
f_mbl             <- read.table("./Data_analysis/results_plot_presentation/MBL_for_plot_firstderivative_SNV.txt", sep = "\t", header = T)

# --- model 1
ref  <- f_plsr_all_cv$pctg_clover_planilha
pred <- f_plsr_all_cv$PLS_all_SNV_1st

fit1              <- lm(ref ~ pred , data = f_plsr_all_cv)
## 


p1                <- ggplot(f_plsr_all_cv, aes(x = pred, y = ref, color = as.factor(year) )) + geom_point(size = 5, shape = 16,alpha =.5) +
  xlim(0,100) + ylim(0,100) + theme_bw() + 
  labs(x = "Estimated clover content (%)", y = "Reference clover content (%)") +
  annotate(label = sprintf("R² = %.2f", summary(fit1)$r.squared),
           geom = "text", x = 30, y = 80, size = 5) +
  annotate(label = sprintf("RMSECV = %.2f", 7.7 ),
           geom = "text", x = 30, y = 70, size = 5) +
  #geom_smooth(method = lm, se=F) + 
  geom_abline(slope = 1, intercept = 0, linetype="dashed", color = "red") +
  ggtitle("PLSR (SNV and 1st derivative) \n LOOCV (574 samples)") +
  theme(legend.title = element_blank(), legend.position = c(.80,.25))
  
p1

# --- model 2


gg <- f_plsr_calval %>%  filter(trat == "cal")

fit2              <- lm(ref_klover ~ est_klover , data = gg)
s2                <- 0.73
s22               <- 0.73


p2                <- ggplot(f_plsr_calval, aes(x = est_klover, y = ref_klover,  color=as.factor(year), shape=trat)) + 
  geom_point(size = 5, alpha = .5) +
  xlim(0,100) + ylim(0,100) + theme_bw() + 
  labs(x = "Estimated clover content (%)", y = "Reference clover content (%)") +
  
  scale_shape_manual(values = c(17,16))+
 
  annotate(label = sprintf("R² cal = %.2f", s2),
           geom = "text", x = 30, y = 80, size = 5) +
  annotate(label = sprintf("RMSECV = %.2f", 7.9 ),
           geom = "text", x = 30, y = 70, size = 5) +
  
  annotate(label = sprintf("R² pred = %.2f", s2),
           geom = "text", x = 75, y = 50, size = 5) +
  annotate(label = sprintf("RMSEP = %.2f", 7.9 ),
           geom = "text", x = 75, y = 40, size = 5) +
  
  theme(legend.position = 'none', legend.title = element_blank())+
  
  geom_abline(slope = 1, intercept = 0, linetype="dashed", color = "red") +
  geom_abline(slope = 0.9312, intercept = 1.8993, linetype="solid", color = "blue") +
  ggtitle("PLSR cal and val (SNV 1st derivative) \n prediction (192 samples)") 
p2



# --- model 3
fit3          <- lm(PLS_less50_SNV_1st ~ pctg_clover_planilha , data = f_plsr_50)


p3            <- ggplot(f_plsr_50, aes(x = PLS_less50_SNV_1st, y =pctg_clover_planilha,color=as.factor(year) )) + 
  geom_point(size = 5, shape = 16, alpha=.5) +
  xlim(0,100) + ylim(0,100) + theme_bw() + 
  labs(x = "Estimated clover content (%)", y = "Reference clover content (%)") +
  annotate(label = sprintf("R² = %.2f", summary(fit3)$r.squared),
           geom = "text", x = 30, y = 80, size = 5) +
  annotate(label = sprintf("RMSECV = %.2f", 7.5 ),
           geom = "text", x = 30, y = 70, size = 5) +
  #geom_smooth(method = lm, se=F) +
  geom_abline(slope = 1, intercept = 0, linetype="dashed", color = "red") +
  ggtitle("PLSR (SNV and 1st derivative) - LOOCV \n (558 samples - lower than 50% clover content)") +
  theme(legend.title = element_blank(),legend.position = 'none' )
p3

# --- model 4

s4 <- 0.73
s44 <- 0.78
fit4              <- lm(y_pred ~ y_pred_mbl , data = f_mbl)


p4                <- ggplot(f_mbl, aes(x = y_pred_mbl, y = y_pred,color = as.factor(year) ) )+ 
  geom_point(size = 5, shape = 16, alpha= .5) +
  xlim(0,100) + ylim(0,100) + theme_bw() + 
  labs(x = "Estimated clover content (%)", y = "Reference clover content (%)") +
  
  annotate(label = sprintf("R² cal = %.2f", s4),
           geom = "text", x = 30, y = 80, size = 5) +
  annotate(label = sprintf("RMSECV = %.2f", 7.4 ),
           geom = "text", x = 30, y = 70, size = 5) +
  
  annotate(label = sprintf("R² pred = %.2f", s44),
           geom = "text", x = 75, y = 50, size = 5) +
  annotate(label = sprintf("RMSEP = %.2f", 7.1 ),
           geom = "text", x = 75, y = 40, size = 5) +
  #geom_smooth(method = lm, se=F) +
  geom_abline(slope = 1, intercept = 0, linetype="dashed", color = "red") +
  ggtitle("MBL (SNV and 1st derivative) - Cal and Prediction \n (192 samples - prediction)") +
  theme(legend.position = 'none')
p4







# --- plotting the residuals
res1              <- f_plsr_all_cv$PLS_all_SNV_1st - f_plsr_all_cv$pctg_clover_planilha
x1                <- f_plsr_all_cv$pctg_clover_planilha
r1                <- data.frame(x = x1, y = res1, id = f_plsr_all_cv$year )

p11               <- ggplot(data = r1, aes(x = x1, y = res1, color = as.factor(id))) +
  geom_point(size = 4, alpha = .5, shape = 16) + 
  ylim(-35,35) + 
  geom_hline(yintercept = c(sd(res1), 0, -sd(res1)), linetype = c("dashed", "solid", "dashed"),
             colour = c("red", "black", "red"), size = 1.25) + labs(x = "Observed (% clover content)", y = "Est - Obs (% clover content)") +
  ggtitle("PLSR (residuals) - LOOCV") + 
  theme_bw()+
  theme(legend.position = 'none')
p11





res2              <- f_plsr_calval$est_klover - f_plsr_calval$ref_klover
x2                <- f_plsr_calval$ref_klover
r2                <- data.frame(x = x2, y = res2,id = f_plsr_calval$year, trat = f_plsr_calval$trat )

p12               <- ggplot(data = r2, aes(x = x2, y = res2,color= as.factor(id), shape = trat )) +
  geom_point(size = 4, alpha = .5) + 
  ylim(-35,35) +
  geom_hline(yintercept = c(sd(res2), 0, -sd(res2)), linetype = c("dashed", "solid", "dashed"),
             colour = c("red", "black", "red"), size = 1.25) +
  labs(x = "Observed (% clover content)", y = "Est - Obs (% clover content)") +
  ggtitle("PLSR Cal and Prediction (residuals)") + 
  theme_bw() +
  theme(legend.position = 'none')
p12



res3              <- f_plsr_50$PLS_less50_SNV_1st - f_plsr_50$pctg_clover_planilha
x3                <- f_plsr_50$pctg_clover_planilha
r3                <- data.frame(x = x3, y = res3, id=f_plsr_50$year)
p13               <- ggplot(data = r3, aes(x = x3, y = res3, color= as.factor(id))) +
  geom_point(size = 4, alpha = .5) + 
  ylim(-35,35) +
  geom_hline(yintercept = c(sd(res3), 0, -sd(res3)), linetype = c("dashed", "solid", "dashed"),
             colour = c("red", "black", "red"), size = 1.25) +
  labs(x = "Observed (% clover content)", y = "Est - Obs (% clover content)") +
  ggtitle("PLSR Cal and Prediction - 50% clover (residuals)") +
  theme_bw()+
  theme(legend.position = 'none')
p13



res4              <- f_mbl$y_pred_mbl - f_mbl$y_pred
x4                <- f_mbl$y_pred
r4                <- data.frame(x = x4, y = res4,id=f_mbl$year)

p14               <- ggplot(data = r4, aes(x = x4, y = res4, color=as.factor(id) )) +
  geom_point(size = 4, alpha = .5) + 
  ylim(-35,35) +
  geom_hline(yintercept = c(sd(res4), 0, -sd(res4)), linetype = c("dashed", "solid", "dashed"),
             colour = c("red", "black", "red"), size = 1.25) +
  labs(x = "Observed (% clover content)", y = "Est - Obs (% clover content)") +
  ggtitle("MBL Prediction (residuals)") +
  theme_bw()+theme(legend.position = 'none')
  
p14




p_final <- grid.arrange(p1,p2,p3,p4,p11,p12,p13,p14, ncol=4)


ggsave(filename = "Regression_residuals_17_jan_year.png", 
       plot = p_final, 
       width = 17,
       height = 10, dpi = 300, family= "serif")





