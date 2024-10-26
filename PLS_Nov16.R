# --- setting working directory
rm(list = ls())
setwd("C:/Users/lomo0003/SLU/YaraNSensor/")
require(tidyverse)
# --- choosing the scenario 
sce <- "SNV_firstderivative"

# --- loading the database
db <- read.table(paste0("./Data_analysis/Inputs/pretreatments/", sce, ".txt"), header = T);head(db)
db_50 <- db %>% filter(pctg_clover_planilha < 50)

x <- db[, 29:ncol(db_50)]
y <- db$pctg_clover_planilha

# =====================================
# ---- Partial least square regression (PLSR)
# =====================================
set.seed(2085)
require(pls)

# ----------------------------------------------------------------
# --- using leave-one-out and whole dataset - NO sqrt
# ----------------------------------------------------------------
model_pls <- plsr(y~as.matrix(x), ncomp = 30, method = "oscorespls", validation = "LOO")

# identify the best component (low RMSECV)  
(ident_best_comp <- as.numeric(which.min(((RMSEP(model_pls)$val[2,1,]))[2:length(RMSEP(model_pls)$val[2,1,])]))) 

# --- select the "model output" from best comp 
db1 <- model_pls$validation$pred[,1,ident_best_comp]

df_pls <- data.frame(est = db1, obs = y)
#plot(db1, y, ylim = c(0,100), xlim = c(0,100))

# --- statistical index
(r2_pls <- caret::R2(df_pls$est, df_pls$obs))
(rmse_pls <- caret::RMSE(df_pls$est, df_pls$obs))

# --- saving the outputs
write.csv(df_pls, paste0("./Data_analysis/Outputs/PLS/pls_allsamples_sqrt_LOOCV_SMALLER50", sce, ".csv"), row.names = F)

plot(df_pls$est, df_pls$obs, xlim = c(0,100), ylim = c(0,100))
lines(x = c(-1000, 1000), y = c(-1000, 1000))

rr = df_pls$est - df_pls$obs
plot(x = df_pls$obs, rr)
abline(h = 0)

# ====================================================================================================

# ----------------------------------------------------------------
# --- using leave-one-out and whole dataset - USING sqrt
# ----------------------------------------------------------------
ysqrt <- sqrt(db$pctg_clover_planilha)

model_pls_SQRT <- plsr(ysqrt~as.matrix(x), ncomp <- 30, method = "oscorespls", validation = "LOO")

# --- identify the best component (low RMSECV)  
(ident_best_comp  <- as.numeric(which.min(((RMSEP(model_pls_SQRT)$val[2,1,]))[2:length(RMSEP(model_pls_SQRT)$val[2,1,])]))) 

# --- select the "model output" from best comp 
db2 <- (model_pls_SQRT$validation$pred[,1,ident_best_comp])^2
df_pls_sqrt <- data.frame(obs = ysqrt^2, est = db2)

plot(df_pls_sqrt$obs, df_pls_sqrt$est, ylim = c(0,100), xlim = c(0,100))

# --- statistical index
(r2_pls_SQRT <- caret::R2(df_pls_sqrt$obs, df_pls_sqrt$est))
(rmse_pls_SQRT <- caret::RMSE(df_pls_sqrt$obs, df_pls_sqrt$est))

# --- deviation
desvios <- y - db2
plot(desvios, ylim = c(-50, 50))
abline(h = 0)
abline(h = c(-20, 20), lty = 2)

# --- saving the outputs
write.csv(df_pls_sqrt, paste0("./Data_analysis/Outputs/PLS/pls_allsamples_sqrt_LOOCV_", sce, ".csv"),
          row.names = F)

# ====================================================================================================

# ----------------------------------------------------------------
# --- using leave-one-out and cal and val dataset - Not using SQRT
# ----------------------------------------------------------------

  # --- using 2/3 calibration and 1/3 validation (100 times)
mat_result <- matrix(NA, ncol = 4, nrow = 100)
colnames(mat_result) <- c("R2_cal", "RMSE_cal", "R2_pred", "RMSE_pred")

for(i in 1:100){
  (cal_samples <- sample(nrow(x), 2/3* nrow(x), replace = F))
  
  y_cal <- y[cal_samples]
  x_cal <- x[cal_samples,]
  y_pred <- y[-cal_samples]
  x_pred <- x[-cal_samples,]

  model_pls_cal <- plsr(y_cal~as.matrix(x_cal), ncomp = 30, method = "oscorespls", validation = "LOO")

# --- identify the best component (low RMSECV)  
  (ident_best_comp  <- as.numeric(which.min(((RMSEP(model_pls_cal)$val[2,1,]))[2:length(RMSEP(model_pls_cal)$val[2,1,])]))) 

# --- select the "model output" from best comp 
  db3 <- (model_pls_cal$validation$pred[,1,ident_best_comp])

# --- statistical index
  (r2_pls_cal <- caret::R2(db3, y_cal))
  (rmse_pls_cal <- caret::RMSE(db3, y_cal))

# --- prediction 
  model_pls_pred <- predict(model_pls_cal, as.matrix(x_pred))
  y_pred_bestcomp <- as.data.frame(model_pls_pred)[,(ident_best_comp)]

# --- statistical index
  r2_pls_pred <- caret::R2(y_pred_bestcomp, y_pred)
  rmse_pls_pred <- caret::RMSE(y_pred_bestcomp, y_pred)
  mat_result[i,1:4] <- c(r2_pls_cal, rmse_pls_cal, r2_pls_pred, rmse_pls_pred)
  print (i)
}

mat_result
colMeans(mat_result)

# --- saving the outputs
write.csv(mat_result, paste0("./Data_analysis/Outputs/PLS/pls_CalVal_NoSqrt_", sce, ".csv"), row.names = F)

# ----------------------------------------------------------------
# --- using leave-one-out and cal and val dataset - USING SQRT
# ----------------------------------------------------------------

# --- using 2/3 calibration and 1/3 validation (100 times)
mat_result_SQRT <- matrix(NA, ncol = 4, nrow = 100)
colnames(mat_result_SQRT) <- c("R2_cal", "RMSE_cal", "R2_pred", "RMSE_pred")

for(j in 1:100){
  (cal_samples <- sample(nrow(x), 2/3* nrow(x), replace = F))
  y_cal <- y[cal_samples]
  x_cal <- x[cal_samples,]
  y_pred <- y[-cal_samples]
  x_pred <- x[-cal_samples,]
  
  model_pls_cal <- plsr(y_cal^.5~as.matrix(x_cal), ncomp = 30, method = "oscorespls", validation = "LOO")
  
  # --- identify the best component (low RMSECV)  
  (ident_best_comp  <- as.numeric(which.min(((RMSEP(model_pls_cal)$val[2,1,]))[2:length(RMSEP(model_pls_cal)$val[2,1,])]))) 
  
  # --- select the "model output" from best comp 
  db4 <- (model_pls_cal$validation$pred[,1,ident_best_comp])^2
  #plot(db3, y_cal, ylim = c(0,100), xlim = c(0,100))
  
  # --- statistical index
  (r2_pls_cal <- caret::R2(db4, y_cal))
  (rmse_pls_cal <- caret::RMSE(db4, y_cal))
  
  # --- prediction 
  model_pls_pred <- predict(model_pls_cal, as.matrix(x_pred))
  y_pred_bestcomp <- as.data.frame(model_pls_pred)[,(ident_best_comp)]
  
  # --- statistical index
  r2_pls_pred <- caret::R2(y_pred_bestcomp, y_pred)
  rmse_pls_pred <- caret::RMSE(y_pred_bestcomp, y_pred)
  mat_result_SQRT[j,1:4] <- c(r2_pls_cal, rmse_pls_cal, r2_pls_pred, rmse_pls_pred)
  print (j)
}

mat_result_SQRT
colMeans(mat_result_SQRT)
# --- saving the outputs
write.csv(mat_result_SQRT, paste0("./Data_analysis/Outputs/PLS/pls_CalVal_sqrt_", sce, ".csv"), row.names = F)


# =============================================================
# PLS removing spectra with zero clover
# =============================================================

# --- loading the database
db <- read.table(paste0("./Data_analysis/Inputs/pretreatments/", sce, ".txt"), header = T);head(db)
db_zero <- db[db$pctg_clover_planilha == 0,c(29:ncol(db))]
db_zero_mean <- colMeans(db_zero)
db_no_zero <- db[db$pctg_clover_planilha != 0,]

# matrix removing the avg spectra ---
db_no_zero1 <- db_no_zero[, 29:ncol(db_no_zero)]

for(i in 1:nrow(db_no_zero1)){
  db_no_zero1[i,] <- db_no_zero[i,29:ncol(db_no_zero)] - db_zero_mean
}

# pls 
x_db <- db_no_zero1
y_db <- db_no_zero$pctg_clover_planilha

# ----------------------------------------------------------------
# --- using leave-one-out and whole dataset - NO sqrt
# ----------------------------------------------------------------
model_pls <- plsr(y_db~as.matrix(x_db), ncomp <- 30, method = "oscorespls", validation = "LOO")

# identify the best component (low RMSECV)  
(ident_best_comp  <- as.numeric(which.min(((RMSEP(model_pls)$val[2,1,]))[2:length(RMSEP(model_pls)$val[2,1,])]))) 

# --- select the "model output" from best comp 
db1 <- model_pls$validation$pred[,1,ident_best_comp]
#plot(db1, y, ylim = c(0,100), xlim = c(0,100))

# --- statistical index
(r2_pls <- caret::R2(db1, y_db))
(rmse_pls <- caret::RMSE(db1, y_db))

df_nozeros <- data.frame(est = db1, obs = y_db)

# --- saving the outputs
write.csv(df_nozeros, paste0("./Data_analysis/Outputs/PLS/pls_WholeDataset_NoSQRT_", sce, ".csv"), row.names = F)


















































