# --- setting working directory
rm(list = ls())
setwd("C:/Users/lomo0003/SLU/YaraNSensor/")

# --- choosing the scenario 
sce <- "SNV_firstderivative"

# --- loading the database
db <- read.table(paste0("./Data_analysis/Inputs/pretreatments/", sce, ".txt"), header = T);head(db)
x <- db[, 29:ncol(db)]
y <- db$pctg_clover_planilha
df <- data.frame(x,y)

# =====================================
# ---- Support Vector Machine regression (SVM)
# =====================================
set.seed(2085)
require(caret)
require(beepr)

# ----------------------------------------------------------------
# --- using leave-one-out and whole dataset - NO SQRT
# ----------------------------------------------------------------
control <- trainControl(method='LOOCV')

#Metric compare model is RMSE
svm_default <- train(y~., 
                    data=df, 
                    method='svmLinear', 
                    preProcess = c("center","scale"),
                    trControl=control,
                    tuneGrid = expand.grid(C = c(1.5)))


best_C <- svm_default$bestTune

out <- svm_default$pred

df_out <- data.frame(out[,1:2])
(r2_svm <- caret::R2(df_out$pred, df_out$obs))
(rmse_svm <- caret::RMSE(df_out$pred, df_out$obs))

#plot(out$pred, out$obs, xlim = c(0,100), ylim = c(0,100))

write.table(df_out, paste0("./Data_analysis/Outputs/SVM/svm_allsamples_NoSqrt_LOOCV_", 
                        sce, ".csv"), row.names = F)

# ----------------------------------------------------------------
# --- using leave-one-out and whole dataset - Sqrt
# ----------------------------------------------------------------
control <- trainControl(method='LOOCV')

#Metric compare model is RMSE
svm_default <- train(sqrt(y)~., 
                     data=df, 
                     method='svmLinear', 
                     preProcess = c("center","scale"),
                     trControl=control,
                     tuneGrid = expand.grid(C = 1.5))

yest = (svm_default$pred$pred)^2

df_out <- data.frame(est = yest, obs = y)
(r2_svm <- caret::R2(df_out$est, df_out$obs))
(rmse_svm <- caret::RMSE(df_out$est, df_out$obs))

plot(df_out$est, df_out$obs, xlim = c(0,100), ylim = c(0,100))

write.table(df_out, paste0("./Data_analysis/Outputs/SVM/svm_allsamples_Sqrt_LOOCV_", 
                           sce, ".csv"), row.names = F)


# ----------------------------------------------------------------
# --- using leave-one-out and cal and val - NoSqrt
# ----------------------------------------------------------------
mat_out <- matrix(NA, ncol = 4, nrow = 100)

for(i in 1:100){
  (cal_samples <- sample(nrow(x), 2/3* nrow(x), replace = F))
  y_cal <- y[cal_samples]
  x_cal <- x[cal_samples,]
  y_pred <- y[-cal_samples]
  x_pred <- x[-cal_samples,]
  
control <- trainControl(method='LOOCV')
#Metric compare model is RMSE
svm_default <- train(y_cal~., 
                     data=data.frame(y_cal, x_cal), 
                     method='svmLinear', 
                     preProcess = c("center","scale"),
                     trControl=control,
                     tuneGrid = expand.grid(C = 1.5))

  out_cal <- svm_default$pred
  
  # statistical indices cal ---
  r2_cal <- caret::R2(out_cal$pred,out_cal$obs)
  rmse_cal <- caret::RMSE(out_cal$pred,out_cal$obs)
  
  # validation ---
  svm_pred <- predict(svm_default, x_pred)
  out_pred <- data.frame(svm_pred, y_pred)
  
  #statistical indices val ---
  r2_val <- caret::R2(svm_pred, y_pred)
  rmse_val <- caret::RMSE(svm_pred, y_pred)
  
  # ---
  mat_out[i, 1:4] <- c(r2_cal, rmse_cal, r2_val, rmse_val)
  print (i)
}

#mat_out
colnames(mat_out) <- c("R2_cal", "RMSE_cal", "R2_Val", "RMSE_Val")
colMeans(mat_out)
write.table(mat_out, paste0("./Data_analysis/Outputs/SVM/svm_CalVal_NoSqrt_LOOCV_", sce, ".csv"), row.names = F)











