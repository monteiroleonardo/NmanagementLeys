# --- setting working directory
rm(list = ls())

set.seed(2085)
require(caret)
require(beepr)
require(randomForest)
require(pls)
require(tidyverse)


setwd("C:/Users/lomo0003/SLU/YaraNSensor/")

# --- choosing the scenario 
sce <- "firstderivative_SNV"

# --- loading the database
db <- read.table(paste0("./Data_analysis/Inputs/pretreatments/", sce, ".txt"), header = T)

# --- splitting cal and pred

a <- sample(1:nrow(db),2/3 * nrow(db), replace = F)

# --- cal

cal <- db[a,]

head_cal <- cal[,1:9]
x_cal <- cal[,29:ncol(cal)]
y_cal <- cal$pctg_clover_planilha

# --- pred

pred <- db[-a,]

head_pred <- pred[,1:9]
x_pred <- pred[,29:ncol(pred)]
y_pred <- pred$pctg_clover_planilha


# --- PLS

ncomp <- 30  
pls <- plsr(sqrt(y_cal) ~ as.matrix(x_cal), ncomp = ncomp, method="oscorespls",validation="LOO") # MODEL 'black box'

(ident_best_comp  <- as.numeric(which.min(((RMSEP(pls)$val[2,1,]))[2:length(RMSEP(pls)$val[2,1,])]))) # identify the best component (low RMSECV)

# selecting the 'model output' from the best component
yc <- (pls$validation$pred[,1,ident_best_comp])^2

# MODEL parameters
y_cv <- (yc)# model output
y_ref <- y_cal # lab value in percentage

lm.fit <- lm(y_cv~y_ref)
(r2_cal<- caret::R2(y_cv, y_ref))
(rmsecv_cal<- caret::RMSE(y_cv, y_ref))
(Bias_cal <- 1/length(y_cv)*sum(y_cv-y_ref))

# prediction

pred <- predict(pls, as.matrix(x_pred))

y_pred_pred <- pred[,1,(ident_best_comp)]^2
y_ref_pred <- y_pred

# --- statistical index prediction
(r2_pred <- caret::R2(y_pred_pred, y_ref_pred))
(rmse_pls_pred <- caret::RMSE(y_pred_pred, y_ref_pred))
(Bias_pred <- 1/length(y_pred_pred )*sum(y_pred_pred - y_ref_pred))

# --- organizing the data

trat <- c(rep('cal',382),rep('val',192))
   
final_1 <- rbind(head_cal,head_pred)
final_2 <- c(y_cal, y_pred)
final_3 <- c(y_cv, y_pred_pred)

final <- cbind(final_1, final_2, final_3, trat)

# Saving the model parameters 
write.table(final, paste0('./Data_analysis/Outputs/PLS/PLS_for_plot_', sce, '.txt'), row.names = F, sep = '\t') 

###############################################################################################################################





rm(list = ls())
setwd("C:/Users/lomo0003/SLU/YaraNSensor/")
require(resemble)

# --- choosing the scenario 
sce <- "firstderivative_SNV"

# --- loading the database
db <- read.table(paste0("./Data_analysis/Inputs/pretreatments/", sce, ".txt"), header = T)

# --- splitting cal and pred

a <- sample(1:nrow(db),2/3 * nrow(db), replace = F)

# --- cal

cal <- db[a,]

head_cal <- cal[,1:9]
x_cal <- cal[,29:ncol(cal)]
y_cal <- cal$pctg_clover_planilha

# --- pred

pred <- db[-a,]

head_pred <- pred[,1:9]
x_pred <- pred[,29:ncol(pred)]
y_pred <- pred$pctg_clover_planilha


# Selecting the variable ###########################################################

##cal
Xr <- x_cal
Yr <- y_cal
Xu <- x_pred
Yu <- y_pred

ctrl1 <- mbl_control(return_dissimilarity = F, validation_type = "NNv", number = 30)

sbl.u.1 <- mbl(Yr = Yr, Xr = Xr, Yu = Yu, Xu = Xu,
               control  = ctrl1,
               method = local_fit_pls(30),
               diss_method = "pls", 
               diss_usage = "predictors",
               k = seq(50, 300, 10))
sbl.u.1

# --- selecting the best k
# --- cal
best_k_cal1 <- as.data.frame(sbl.u.1$validation_results$nearest_neighbor_validation)
best_k_cal <- best_k_cal1[which.min(best_k_cal1$rmse),]

# --- pred
best_k_pred1 <- as.data.frame(sbl.u.1$validation_results$Yu_prediction_statistics)
best_k_pred <- best_k_pred1[which.min(best_k_pred1$rmse),]

y_pred_mbl <- sbl.u.1$results$k_190$pred


final_mbl <- cbind(head_pred,y_pred, y_pred_mbl)

# Saving the model parameters 
write.table(final_mbl, paste0('./Data_analysis/Outputs/PLS/MBL_for_plot_', sce, '.txt'), row.names = F, sep = '\t') 


