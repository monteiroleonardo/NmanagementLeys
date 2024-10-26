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

#sce <- "firstderivative_SNV"
#sce <- "firstderivative"
sce <- "no_treatment"


# --- loading the database
db <- read.table(paste0("./Data_analysis/Inputs/pretreatments/", sce, ".txt"), header = T);head(db)

# --- Filter the years
db_1718 <- db %>% filter(year != 2019)  
db_19 <- db %>% filter(year == 2019)  


### --- 2017 and 2018 as a calibration and 2019 as a prediction
db_1718_y <- db_1718$pctg_clover_planilha
db_1718_x <- db_1718[, 29:ncol(db_1718)]

db_19_y <- db_19$pctg_clover_planilha
db_19_x <- db_19[, 29:ncol(db_19)]


ncomp <- 30  
pls <- plsr(sqrt(db_1718_y) ~ as.matrix(db_1718_x), ncomp = ncomp, method="oscorespls",validation="LOO") # MODEL 'black box'

(ident_best_comp  <- as.numeric(which.min(((RMSEP(pls)$val[2,1,]))[2:length(RMSEP(pls)$val[2,1,])]))) # identify the best component (low RMSECV)

# selecting the 'model output' from the best component
yc <- (pls$validation$pred[,1,ident_best_comp])^2

# MODEL parameters
y_cv <- (yc)# model output
y_ref <- db_1718_y # lab value in percentage

lm.fit <- lm(y_cv~y_ref)
(r2_cal<- caret::R2(y_cv, y_ref))
(rmsecv_cal<- caret::RMSE(y_cv, y_ref))
(Bias_cal <- 1/length(y_cv)*sum(y_cv-y_ref))

# -- saving x and y values validation

info <- db_1718[,1:28]
out_validation <- cbind(info,y_ref,y_cv)

write.table(out_validation, paste0('./Data_analysis/Outputs/PLS/Feb_23_PLS_2019_xy_years_Cal_', sce, '.csv'), 
            row.names = F, sep = ';') 


# prediction

pred <- predict(pls, as.matrix(db_19_x))

y_pred <- pred[,1,(ident_best_comp)]^2
y_ref_pred <- db_19_y

# --- statistical index prediction
(r2_pred <- caret::R2(y_pred, y_ref_pred))
(rmse_pls_pred <- caret::RMSE(y_pred, y_ref_pred))
(Bias_pred <- 1/length(y_pred )*sum(y_pred - y_ref_pred))


final <- cbind(r2_cal,rmsecv_cal, Bias_cal,r2_pred,rmse_pls_pred,Bias_pred)

# Saving the model parameters 
write.table(final, paste0('./Data_analysis/Outputs/PLS/PLS_results_years_', sce, '.txt'), row.names = F, sep = '\t') 

# -- saving x and y values prediction

info <- db_19_x[,1:28]
out_prediction <- cbind(info,y_ref_pred,y_pred)

write.table(out_prediction, paste0('./Data_analysis/Outputs/PLS/Feb_23_PLS_2019_xy_years_Pred_', sce, '.csv'), 
            row.names = F, sep = ';') 



##################################################################################################################################
      
      
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
      #sce <- "firstderivative"
      #sce <- "no_treatment"
      
      # --- loading the database
      db <- read.table(paste0("./Data_analysis/Inputs/pretreatments/", sce, ".txt"), header = T)
      
      # --- Filter the years
      db_1719 <- db %>% filter(year != 2018)  
      db_18 <- db %>% filter(year == 2018)  
      
      
      ### --- 2017 and 2019 as a calibration and 2018 as a prediction
      db_1719_y <- db_1719$pctg_clover_planilha
      db_1719_x <- db_1719[, 29:ncol(db_1719)]
      
      db_18_y <- db_18$pctg_clover_planilha
      db_18_x <- db_18[, 29:ncol(db_18)]
      
      
      ncomp <- 30  
      pls <- plsr(sqrt(db_1719_y) ~ as.matrix(db_1719_x), ncomp = ncomp, method="oscorespls",validation="LOO") # MODEL 'black box'
      
      (ident_best_comp  <- as.numeric(which.min(((RMSEP(pls)$val[2,1,]))[2:length(RMSEP(pls)$val[2,1,])]))) # identify the best component (low RMSECV)
      
      # selecting the 'model output' from the best component
      yc <- (pls$validation$pred[,1,ident_best_comp])^2
      
      # MODEL parameters
      y_cv <- (yc)# model output
      y_ref <- db_1719_y # lab value in percentage
      
      lm.fit <- lm(y_cv~y_ref)
      (r2_cal<- caret::R2(y_cv, y_ref))
      (rmsecv_cal<- caret::RMSE(y_cv, y_ref))
      (Bias_cal <- 1/length(y_cv)*sum(y_cv-y_ref))
      
      # -- saving x and y values validation
      
      info <- db_1719[,1:28]
      out_validation <- cbind(info,y_ref,y_cv)
      
      write.table(out_validation, paste0('./Data_analysis/Outputs/PLS/Feb_23_PLS_2018_xy_years_Cal_', sce, '.csv'), 
                  row.names = F, sep = ';') 
      
      # prediction
      
      pred <- predict(pls, as.matrix(db_18_x))
      
      y_pred <- pred[,1,(ident_best_comp)]^2
      y_ref_pred <- db_18_y
      
      # --- statistical index prediction
      (r2_pred <- caret::R2(y_pred, y_ref_pred))
      (rmse_pls_pred <- caret::RMSE(y_pred, y_ref_pred))
      (Bias_pred <- 1/length(y_pred )*sum(y_pred - y_ref_pred))
      
      
      final <- cbind(r2_cal,rmsecv_cal, Bias_cal,r2_pred,rmse_pls_pred,Bias_pred)
      
      # Saving the model parameters 
      write.table(final, paste0('./Data_analysis/Outputs/PLS/1719_cal_PLS_results_years_', sce, '.txt'), row.names = F, sep = '\t') 
      
      # -- saving x and y values prediction
      
      info <- db_18_x[,1:28]
      out_prediction <- cbind(info,y_ref_pred,y_pred)
      
      write.table(out_prediction, paste0('./Data_analysis/Outputs/PLS/Feb_23_PLS_2018_xy_years_Pred_', sce, '.csv'), 
                  row.names = F, sep = ';') 
      
      
      
      
##################################################################################################################################
##################################################################################################################################      
      
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
      #sce <- "firstderivative_SNV"
      #sce <- "firstderivative"
      sce <- "no_treatment"
      
      # --- loading the database
      db <- read.table(paste0("./Data_analysis/Inputs/pretreatments/", sce, ".txt"), header = T)
      
      # --- Filter the years
      db_1819 <- db %>% filter(year != 2017)  
      db_17 <- db %>% filter(year == 2017)  
      
      
      ### --- 2018 and 2019 as a calibration and 2017 as a prediction
      db_1819_y <- db_1819$pctg_clover_planilha
      db_1819_x <- db_1819[, 29:ncol(db_1819)]
      
      db_17_y <- db_17$pctg_clover_planilha
      db_17_x <- db_17[, 29:ncol(db_17)]
      
      
      ncomp <- 30  
      pls <- plsr(sqrt(db_1819_y) ~ as.matrix(db_1819_x), ncomp = ncomp, method="oscorespls",validation="LOO") # MODEL 'black box'
      
      (ident_best_comp  <- as.numeric(which.min(((RMSEP(pls)$val[2,1,]))[2:length(RMSEP(pls)$val[2,1,])]))) # identify the best component (low RMSECV)
      
      # selecting the 'model output' from the best component
      yc <- (pls$validation$pred[,1,ident_best_comp])^2
      
      # MODEL parameters
      y_cv <- (yc)# model output
      y_ref <- db_1819_y # lab value in percentage
      
      lm.fit <- lm(y_cv~y_ref)
      (r2_cal<- caret::R2(y_cv, y_ref))
      (rmsecv_cal<- caret::RMSE(y_cv, y_ref))
      (Bias_cal <- 1/length(y_cv)*sum(y_cv-y_ref))
      
      # -- saving x and y values validation
      
      info <- db_1819[,1:28]
      out_validation <- cbind(info,y_ref,y_cv)
      
      write.table(out_validation, paste0('./Data_analysis/Outputs/PLS/Feb_23_PLS_2017_xy_years_Cal_', sce, '.csv'), 
                  row.names = F, sep = ';') 
      
      # prediction
      
      pred <- predict(pls, as.matrix(db_17_x))
      
      y_pred <- pred[,1,(ident_best_comp)]^2
      y_ref_pred <- db_17_y
      
      # --- statistical index prediction
      (r2_pred <- caret::R2(y_pred, y_ref_pred))
      (rmse_pls_pred <- caret::RMSE(y_pred, y_ref_pred))
      (Bias_pred <- 1/length(y_pred )*sum(y_pred - y_ref_pred))
      
      
      final <- cbind(r2_cal,rmsecv_cal, Bias_cal,r2_pred,rmse_pls_pred,Bias_pred)
      
      # Saving the model parameters 
      write.table(final, paste0('./Data_analysis/Outputs/PLS/1819_cal_PLS_results_years_', sce, '.txt'), row.names = F, sep = '\t') 
      
      
      # -- saving x and y values prediction
      
      info <- db_17_x[,1:28]
      out_prediction <- cbind(info,y_ref_pred,y_pred)
      
      write.table(out_prediction, paste0('./Data_analysis/Outputs/PLS/Feb_23_PLS_2017_xy_years_Pred_', sce, '.csv'), 
                  row.names = F, sep = ';') 


      
#####################################################################################################################
# --- year fold      
      
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
#sce <- "firstderivative"
#sce <- "no_treatment"

# --- loading the database
db <- read.table(paste0("./Data_analysis/Inputs/pretreatments/", sce, ".txt"), header = T);head(db)

# --- Matrix 

db_spc <- db[,29:ncol(db)]

# --- Cross-validation -- Place

seg_cv <- lapply(unique(db$year),
     function(x, db) which(db$year == x),
     db = db)
seg_cv

# --- PLSR

ncomp <- 30  
pls <- plsr(sqrt(db$pctg_clover_planilha) ~ as.matrix(db_spc), ncomp = ncomp, method="oscorespls",
validation = "CV", segments = seg_cv) # MODEL 'black box'

(ident_best_comp  <- as.numeric(which.min(((RMSEP(pls)$val[2,1,]))[2:length(RMSEP(pls)$val[2,1,])]))) # identify the best component (low RMSECV)

# selecting the 'model output' from the best component
yc <- (pls$validation$pred[,1,ident_best_comp])^2

# MODEL parameters
y_cv <- (yc)# model output
y_ref <-  db$pctg_clover_planilha# lab value in percentage

lm.fit <- lm(y_cv~y_ref)
(r2_cal<- caret::R2(y_cv, y_ref))
(rmsecv_cal<- caret::RMSE(y_cv, y_ref))
(Bias_cal <- 1/length(y_cv)*sum(y_cv-y_ref))

# -- saving x and y values prediction

info <- db[,1:28]
out_prediction <- cbind(info,y_ref,y_cv)

write.table(out_prediction, paste0('./Data_analysis/Outputs/PLS/Feb_23_PLS_xy_years_FOLD_', sce, '.csv'), 
            row.names = F, sep = ';') 





