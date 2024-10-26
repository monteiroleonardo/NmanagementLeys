# --- setting working directory
rm(list = ls())
setwd("C:/Users/lomo0003/SLU/YaraNSensor/")

# --- choosing the scenario 
sce <- "no_treatment"

# --- loading the database
db <- read.table(paste0("./Data_analysis/Inputs/pretreatments/", sce, ".txt"), header = T);head(db)
x <- db[, 29:ncol(db)]
y <- db$pctg_clover_planilha
df <- data.frame(x,y)

# =====================================
# ---- RANDOM FOREST (RF)
# =====================================
set.seed(2085)
require(caret)
require(beepr)
require(randomForest)

# ----------------------------------------------------------------
# --- using leave-one-out and whole dataset
# ----------------------------------------------------------------
var_pred <- NULL
var_pred_scl <- NULL

for(i in 1:nrow(x)){
  #i <- 1
  x1 <- x[-i,]
  y1 <- y[-i]
  df1 <- data.frame(ysc, x1)
  rf_model <- randomForest(ysc~., data = df1, mtry = 3, ntree = 2000)
  rf_pred <- predict(rf_model, x[i,])
  var_pred_scl <- c(var_pred_scl,rf_pred)  
  print (i)
}

# ----------------------------------------------------------------
# --- using leave-one-out and 2/3 cal 1/3 val dataset
# ----------------------------------------------------------------

# --- using 2/3 calibration and 1/3 validation (100 times)
for(i in 1:100){
  (cal_samples <- sample(nrow(x), 2/3* nrow(x), replace = F))
  y_cal <- y[cal_samples]
  x_cal <- x[cal_samples,]
  y_pred <- y[-cal_samples]
  x_pred <- x[-cal_samples,]
  
  x_cal_sc <- scale(x_cal, scale = T)
  y_cal_sc <- scale(y_cal, scale = T)
  
  model <- randomForest(y_cal~., data = data.frame(y_cal, x_cal_sc), ntree = 2000, mtry = 3)
  print (model)
  
require(e1071)  

tc         = tune.control(cross=5)
tuneResult = tune(svm, x_cal, sqrt(y_cal), ranges = list(epsilon = seq(0.1,1,0.1), cost = 2^(seq(0.5,8,0.5))), tunecontrol=tc)

predSVR    = predict(tuneResult$best.model, x_pred)^2
  
(rmseSVR    = caret::RMSE(predSVR, y_pred))
  
plot(predSVR, y_pred)  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  control <- trainControl(method='repeatedcv', repeats = 5, allowParallel = T)
  #Metric compare model is RMSE
  metric <- "RMSE"
  set.seed(2085)
  tunegrid <- expand.grid(.mtry=ncol(x))
  rf_default <- train(y_cal~., 
                      data=data.frame(y_cal, x_cal), 
                      method='rf', 
                      metric=metric, 
                      tuneGrid=tunegrid, 
                      trControl=control, ntree = 1000)
  out <- rf_default$pred
  write.table(out, paste0("./RF_splitdataset/rf_LOOCV_mtry61_", i, "_", sce, ".csv"), row.names = F)
  print (i)
}

mat_result
# --- saving the outputs
write.csv(mat_result, paste0("./Data_analysis/Outputs/MBL_CalVal_", sce, ".csv"), row.names = F)



































