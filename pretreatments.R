rm(list=ls())

library(prospectr)
library(pls)


d <- read.table("Matrix_YARA_2.csv", header=T, sep=",")

spc <- d[,29: ncol(d)]; dim(spc)
dta <- d[,1:28]


######### comb = combination = no pretreament ##################################

comb <- spc
comb <- cbind(dta,spc)
write.table (comb,"no_treatment.txt", sep="\t",row.names=F)

############### Mean center  ##################################################

mcm <- scale(spc, scale= F)
mc <- cbind(dta,mcm)
write.table (mc,"mean_center.txt", sep="\t",row.names=F)

############## 1st derivative  #################################################

first.derim <- as.matrix(savitzkyGolay(spc, p = 2, w = 3, m = 1))
first.deri <- cbind(dta,first.derim)
write.table (first.deri,"firstderivative.txt", sep="\t",row.names=F)

############ 2nd derivative ####################################################

second.derim <- as.matrix(savitzkyGolay(spc, p = 2, w = 3, m = 2))
second.deri <- cbind(dta,second.derim)
write.table(second.deri, 'secondderivative.txt', row.names = F, sep = '\t')

################ MSC = multiplicative scatter correction #######################


datamscm <- msc(as.matrix(spc))
datamsc <- cbind(dta,datamscm)
write.table(datamsc, 'MSC.txt', row.names = F, sep = '\t')

############### SNV = standard normalized variate ##############################

datasnvm <- as.matrix(standardNormalVariate(spc))
datasnv <- cbind(dta,datasnvm)
write.table(datasnv, 'SNV.txt', row.names = F, sep = '\t')

############ comb 7 = SNV + 1st derivative #####################################

comb7m <- as.matrix(savitzkyGolay(datasnvm, p = 2, w = 3, m = 1))
comb7 <- cbind(dta,comb7m)
write.table(comb7, 'SNV_firstderivative.txt', row.names = F, sep = '\t')

############ comb 8 = 1st derivative + SNV #####################################

comb8m <- as.matrix(standardNormalVariate(first.derim))
comb8 <- cbind(dta,comb8m)
write.table(comb8, 'firstderivative_SNV.txt', row.names = F, sep = '\t')

######## comb 9 = Mean center + 1st derivative #################################

comb9m <- as.matrix(savitzkyGolay(mcm, p = 2, w = 3, m = 1))
comb9 <- cbind(dta,comb9m)
write.table(comb9, 'Mean_center_firstderivative.txt', row.names = F, sep = '\t')

####### comb 10 = 1st derivative + Mean center #################################

comb10m <- as.matrix(scale(first.derim, scale = F))
comb10 <- cbind(dta,comb10m)
write.table(comb10, 'firstderivative_mean_center.txt', row.names = F, sep = '\t')

##### comb 11 = MSC + 1st derivative ##########################################

comb11m <- as.matrix(savitzkyGolay(datamscm, p = 2, w = 3, m = 1))
comb11 <- cbind(dta,comb11m)
write.table(comb11, 'MSC_firstderivative.txt', row.names = F, sep = '\t')

###### comb 12 = 1st derivative  + MSC ########################################

comb12m <- as.matrix(msc(first.derim))
comb12 <- cbind(dta,comb12m)
write.table(comb12, 'firstderivative_MSC.txt', row.names = F, sep = '\t')

######## comb 13 = SNV + 2nd derivative ####################################### 

comb13m <- as.matrix(savitzkyGolay(datasnvm, p = 2, w = 3, m = 2))
comb13 <- cbind(dta,comb13m)
write.table(comb13, 'SNV_secondderivative.txt', row.names = F, sep = '\t')

######## comb 14 = 2nd derivative + SNV ######################################## 

comb14m <- as.matrix(standardNormalVariate(second.derim))
comb14 <- cbind(dta,comb14m)
write.table(comb14, 'secondderivative_SNV.txt', row.names = F, sep = '\t')

###### comb 15 = MC + 2nd derivative ########################################### 

comb15m <- as.matrix(savitzkyGolay(mcm, p = 2, w = 3, m = 2))
comb15 <- cbind(dta,comb15m)
write.table(comb15, 'Mean_center_secondderivative.txt', row.names = F, sep = '\t')

###### comb 16 = 2nd derivative + MC ########################################### 

comb16m <- as.matrix(scale(second.derim, scale= F))
comb16 <- cbind(dta,comb16m)
write.table(comb16, 'secondderivative_mean_center.txt', row.names = F, sep = '\t')

########### comb 17 = MSC + 2nd derivative ##################################### 

comb17m <- as.matrix(savitzkyGolay(datamscm, p = 2, w = 3, m = 2))
comb17 <- cbind(dta,comb17m)
write.table(comb17, 'MSC_secondderivative.txt', row.names = F, sep = '\t')

########## comb 18 = 2nd derivative + MSC  ##################################### 

comb18m <- as.matrix(msc(second.derim))
comb18 <- cbind(dta,comb18m)
write.table(comb18, 'secondderivative_MSC.txt', row.names = F, sep = '\t')

########### comb 19 = MC + SNV ################################################## 

comb19m <- as.matrix(standardNormalVariate(mcm))
comb19 <- cbind(dta,comb19m)
write.table(comb19, 'SNV_mean_center.txt', row.names = F, sep = '\t')

######## comb 20 = SNV  + MC ###################################################### 

comb20m <- as.matrix(scale(datasnvm, scale= F))
comb20 <- cbind(dta,comb20m)
write.table(comb20, 'Mean_center_SNV.txt', row.names = F, sep = '\t')

######### comb 21 = MSC + MC ##################################################### 

comb21m <- as.matrix(scale(datamscm, scale =F))
comb21 <- cbind(dta,comb21m)
write.table(comb21, 'MSC_mean_center.txt', row.names = F, sep = '\t')

######### comb 22 = 2nd derivative + 1st derivative ############################## 

comb22m <- as.matrix(savitzkyGolay (second.derim, m = 1, w =3 ,p = 2))
comb22 <- cbind(dta,comb22m)
write.table(comb22, 'firstderivative_secondderivative.txt', row.names = F, sep = '\t')

######### comb 23 <- 1st derivative + 2nd derivative ############################# 

comb23m <- as.matrix(savitzkyGolay (first.derim, m = 2, w =3 ,p = 2))
comb23 <- cbind(dta,comb23m)
write.table(comb23, 'secondderivative_firstderivative.txt', row.names = F, sep = '\t')

######### comb 24 = SNV + 2nd derivative + 1st derivative ######################### 

comb24m <- as.matrix(savitzkyGolay (comb13m, m = 1, w =3 ,p = 2))
comb24 <- cbind(dta,comb24m)
write.table(comb24, 'firstderivative_secondderivative_SNV.txt', row.names = F, sep = '\t')

######### comb 25 = SNV + 1st derivative + 2nd derivative ######################### 

comb25m <- as.matrix(savitzkyGolay(comb7m, m = 2, w =3 ,p = 2))
comb25 <- cbind(dta,comb25m)
write.table(comb25, 'secondderivative_firstderivative_SNV.txt', row.names = F, sep = '\t')

######### comb 26 = 2nd derivative + 1st derivative + SNV  ######################### 

comb26m <- as.matrix(standardNormalVariate(comb22m))
comb26 <- cbind(dta,comb26m)
write.table(comb26, 'SNV_firstderivative_secondderivative.txt', row.names = F, sep = '\t')

######### comb 27 = 1st derivative + 2nd derivative + SNV ########################## 

comb27m <- as.matrix(standardNormalVariate(comb23m))
comb27 <- cbind(dta,comb27m)
write.table(comb27, 'SNV_secondderivative_firstderivative.txt', row.names = F, sep = '\t')



