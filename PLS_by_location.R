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

seg_cv <- lapply(unique(db$Place),
                 function(x, db) which(db$Place == x),
                 db = db)
seg_cv

table(db$Local)

for(i in 1:length(seg_cv)){
   
      A1 <- db[-seg_cv[[i]],]
      spc_A1 <- A1[, 29:ncol(A1)]
      y_ref <- A1$pctg_clover_planilha
      
      # --- PLSR

ncomp <- 30  
pls <- plsr(sqrt(y_ref) ~ as.matrix(spc_A1), ncomp = ncomp, method="oscorespls",
            validation = "LOO")# segments = seg_cv) # MODEL 'black box'

(ident_best_comp  <- as.numeric(which.min(((RMSEP(pls)$val[2,1,]))[2:length(RMSEP(pls)$val[2,1,])]))) # identify the best component (low RMSECV)

# selecting the 'model output' from the best component
yc <- (pls$validation$pred[,1,ident_best_comp])^2

# MODEL parameters
y_cv <- (yc)# model output

lm.fit <- lm(y_cv~y_ref)
(r2_cal<- caret::R2(y_cv, y_ref))
(rmsecv_cal<- caret::RMSE(y_cv, y_ref))
(Bias_cal <- 1/length(y_cv)*sum(y_cv-y_ref))


# -- saving x and y values prediction

info <- A1[,1:28]
out_validation <- cbind(info,y_ref,y_cv)

write.table(out_validation, paste0('./Data_analysis/Outputs/PLS/Feb_23_PLS_xy_place_Cal_', sce,'_',i, '.csv'), 
            row.names = F, sep = ';') 


# prediction

A2 <- db[seg_cv[[i]],]
spc_A2 <- A2[, 29:ncol(A2)]
y_ref_pred <- A2$pctg_clover_planilha


pred <- predict(pls, as.matrix(spc_A2))

y_pred <- pred[,1,(ident_best_comp)]^2


# --- statistical index prediction
(r2_pred <- caret::R2(y_pred, y_ref_pred))
(rmse_pls_pred <- caret::RMSE(y_pred, y_ref_pred))
(Bias_pred <- 1/length(y_pred )*sum(y_pred - y_ref_pred))


final <- cbind(r2_cal,rmsecv_cal, Bias_cal,r2_pred,rmse_pls_pred,Bias_pred,i)

# Saving the model parameters 
write.table(final, paste0('./Data_analysis/Outputs/PLS/Results_cal_PLS_results_place_', sce,'_',i, '.txt'), row.names = F, sep = '\t') 


# -- saving x and y values prediction

info <- A2[,1:28]
out_prediction <- cbind(info,y_ref_pred,y_pred)

write.table(out_prediction, paste0('./Data_analysis/Outputs/PLS/Feb_23_PLS_xy_place_Pred_', sce,'_',i, '.csv'), 
            row.names = F, sep = ';') 

print(i)

}

######################################################################################################################

#####################################################################################################################
# --- Place fold      

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

seg_cv <- lapply(unique(db$Place),
                 function(x, db) which(db$Place == x),
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

write.table(out_prediction, paste0('./Data_analysis/Outputs/PLS/Feb_23_PLS_xy_place_FOLD_', sce, '.csv'), 
            row.names = F, sep = ';') 
















