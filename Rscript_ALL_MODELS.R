rm(list = ls())

setwd('D:/SLU/Yara/data_analysis_Oct13/')


d <- read.table("no_treatment.txt", header=T, sep="\t")
dim(d)
set.seed(2805)

cal_samples <- sample(1:nrow(d), 2/3*nrow(d), replace  = F)

d_cal = d [cal_samples,]
d_val = d[-cal_samples,]

head(d)

spc_cal <- as.matrix(d_cal[,29:ncol(d_cal)])
x_cal <- (d_cal$pctg_clover_planilha)

spc_val <- as.matrix(d_val[,29:ncol(d_val)])
x_val <- (d_val$pctg_clover_planilha)


# Selecting the variable ###########################################################

##cal
Xr <- spc_cal

Yr <- x_cal

Xu = spc_val

Yu = x_val

library(resemble)

ctrl1 <- mbl_control(return_dissimilarity = F, validation_type = "NNv", number = 30)
  


sbl.u.1 <- mbl(Yr = Yr, Xr = Xr, Yu = Yu, Xu = Xu,
               control  = ctrl1,
               method = local_fit_pls(30),
               diss_method = "pls", 
               diss_usage = "predictors",
               k = seq(50, 300, 10) ,
               
)
sbl.u.1

y_val_mbl = sbl.u.1$results$k_150$pred

plot(x = x_val, y = y_val_mbl, ylim = c(0,100), xlim = c(0,100))
abline(lm(x_val~y_val_mbl))
lines(x = c(-1000, 1000), y = c(-1000, 1000), lty = 2)

########################################################################

# --- PLS with same cal and val samples
require(pls)

ncomp <- 30

pls <- plsr(x_cal ~ spc_cal, ncomp = ncomp, method = "oscorespls", validation = "LOO")


(ident_best_comp  <- as.numeric(which.min(((RMSEP(pls)$val[2,1,]))[2:length(RMSEP(pls)$val[2,1,])]))) # identify the best component (low RMSECV)

# selecting the 'model output' from the best component
yc <- pls$validation$pred[,1,ident_best_comp]

x = x_cal

lm.fit <- lm(yc ~ x)
(R_2 <- summary(lm.fit)$r.squared)
(RMSECV <- sqrt(1/length(yc) * sum((yc - x)^2)))

pred = predict(pls, newdata = spc_val)
df_pred = as.data.frame(pred)
yv = df_pred[,(ident_best_comp)]

lm.fit_val <- lm(yv ~ x_val)
(R_2_val <- summary(lm.fit_val)$r.squared)
(RMSECV_val <- sqrt(1/length(yv) * sum((yv - x_val)^2)))

## PLS with SQRT 

x_cal_sqrt = sqrt(x_cal)

pls <- plsr(x_cal_sqrt ~ spc_cal, ncomp = ncomp, method = "oscorespls", validation = "LOO")


(ident_best_comp  <- as.numeric(which.min(((RMSEP(pls)$val[2,1,]))[2:length(RMSEP(pls)$val[2,1,])]))) # identify the best component (low RMSECV)

# selecting the 'model output' from the best component
yc_sqrt <- (pls$validation$pred[,1,ident_best_comp])^2

x = x_cal

lm.fit <- lm(yc_sqrt ~ x)
(R_2 <- summary(lm.fit)$r.squared)
(RMSECV <- sqrt(1/length(yc_sqrt) * sum((yc_sqrt - x)^2)))

pred = predict(pls, newdata = spc_val)
df_pred = as.data.frame(pred)
yv_sqrt = df_pred[,(ident_best_comp)]^2

lm.fit_val <- lm(yv_sqrt ~ x_val)
(R_2_val <- summary(lm.fit_val)$r.squared)
(RMSECV_val <- sqrt(1/length(yv_sqrt) * sum((yv_sqrt - x_val)^2)))




# -----------------------------
# --- random forest
# -----------------------------

require(caret)
control <- trainControl(method='LOOCV')

#Metric compare model is Accuracy
metric <- "RMSE"
set.seed(2805)
ls()

#Number randomely variable selected is mtry
mtry <- sqrt(ncol(spc_cal))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(x_cal~., 
                    data=cbind(x_cal, spc_cal), 
                    method='rf', 
                    metric=metric, 
                    tuneGrid=tunegrid, 
                    trControl=control, ntrees = 2000)

print(rf_default)

rf_predict = as.numeric(predict(rf_default, spc_val))

lm.fit_val <- lm(rf_predict ~ x_val)
(R_2_val <- summary(lm.fit_val)$r.squared)
(RMSECV_val <- sqrt(1/length(rf_predict) * sum((rf_predict - x_val)^2)))



# plotting RF results ---
yest_cal = rf_default$pred$pred
yobs = rf_default$pred$obs

plot(yobs, yest_cal, ylim = c(0,100), xlim = c(0,100))
abline(lm(yest_cal~yobs))
lines(x = c(-1000, 1000), y = c(-1000, 1000), lty = 2)

print (rf_default)

# ---------------------------------
# --- gradient boosted machines 
# ---------------------------------
require(gbm)

set.seed(2011)

# train GBM model
gbm.fit <- gbm(
  formula = x_cal ~ .,
  distribution = "gaussian",
  data = data.frame(x_cal, spc_cal),
  n.trees = 10000,
  interaction.depth = 2,
  shrinkage = 0.001,
  cv.folds = length(x_cal),
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

(min_MSE <- which.min(gbm.fit$cv.error))
sqrt(gbm.fit$cv.error[min_MSE])

yest_cv = gbm.fit$cv.fitted

lm.fit_cv <- lm(yest_cv ~ x_cal)
(R_2_cv <- summary(lm.fit_cv)$r.squared)
(RMSECV_cv <- sqrt(1/length(yest_cv) * sum((yest_cv - x_cal)^2)))

ypred = predict(gbm.fit, as.data.frame(spc_val))
lm.fit_pred <- lm(ypred ~ x_val)
(R_2_pred <- summary(lm.fit_pred)$r.squared)
(RMSECV_pred <- sqrt(1/length(ypred) * sum((ypred - x_val)^2)))


# calculating gbm with all samples (d file)

require(gbm)

set.seed(2011)

# train GBM model
gbm.fit_all <- gbm(
  formula = d$pctg_clover_planilha ~ .,
  distribution = "gaussian",
  data = d[, 29:ncol(d)],
  n.trees = 10000,
  interaction.depth = 2,
  shrinkage = 0.001,
  cv.folds = length(d$pctg_clover_cal),
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

(min_MSE <- which.min(gbm.fit_all$cv.error))
sqrt(gbm.fit_all$cv.error[min_MSE])

yest_cv = gbm.fit_all$cv.fitted

lm.fit_cv <- lm(yest_cv ~ d$pctg_clover_planilha)
(R_2_cv <- summary(lm.fit_cv)$r.squared)
(RMSECV_cv <- sqrt(1/length(yest_cv) * sum((yest_cv - d$pctg_clover_planilha)^2)))



# ---------------------------------
# --- Support vector machine 
# ---------------------------------


require(caret)
control <- trainControl(method='LOOCV')

#Metric compare model is Accuracy
metric <- "RMSE"
set.seed(2805)

svm_default <- train(x_cal~., 
                    data=data.frame(cbind(x_cal, spc_cal)), 
                    method='svmLinear', 
                    metric=metric, 
                    trControl=control)
print(svm_default)

svm_predict = as.numeric(predict(svm_default, spc_val))

lm.fit_val <- lm(svm_predict ~ x_val)
(R_2_val <- summary(lm.fit_val)$r.squared)
(RMSECV_val <- sqrt(1/length(svm_predict) * sum((svm_predict - x_val)^2)))



# plotting RF results ---
yest_cal = rf_default$pred$pred
yobs = rf_default$pred$obs

plot(yobs, yest_cal, ylim = c(0,100), xlim = c(0,100))
abline(lm(yest_cal~yobs))
lines(x = c(-1000, 1000), y = c(-1000, 1000), lty = 2)


### SVM ALL samples

x_all = d$pctg_clover_planilha
spc_all = d[,29:ncol(d)]

svm_default <- train(x_all~., 
                     data=data.frame(cbind(x_all, spc_all )), 
                     method='svmPoly', 
                     metric=metric, 
                     trControl=control)

print(svm_default)


















