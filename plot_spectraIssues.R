rm(list = ls())

setwd('c:/Users/lomo0003/SLU/YaraNSensor/Data_analysis/')

d <- read.table("./Inputs/Matrix_YARA_comproblemas.csv", header=T, sep=",")
dim(d)

d1[1:5, 1:10]

d1 <- d[d$CUT_YEAR == "klipp_2017" & d$Place == 3,]

sp <- d1[, 29:ncol(d1)]


tiff(filename = "spectraIssues.tif", width = 1600, height = 1400,
    res = 300, family = "serif")

matplot(t(sp), ty = "l", lty=1, ylim = c(0,1), col = "gray50",
        xaxt = "n", yaxt = "n", 
        xlab = "Wavelength (nm)", ylab = "Reflectance (%)",
        lwd = 2.5, main = "Spectra issues 2017 - Place 3 ")
axis(1, at = seq(1,61, 5), labels = seq(400, 1000, 50))
axis(2, at = seq(0,1,.2), labels = seq(0,100, 20), las = 2)


dev.off()
dim(sp)
