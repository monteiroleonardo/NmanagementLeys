rm(list = ls())

setwd('c:/Users/lomo0003/SLU/YaraNSensor/Data_analysis/')

d <- read.table("./Inputs/Matrix_YARA_2.csv", header=T, sep=",")
dim(d)


sp <- d[, 29:ncol(d)]

# --- choosing highest and smallest clover content

bc_clover <- d$pctg_clover_planilha
max_clover <- which.max(bc_clover)
min_clover <- which.min(bc_clover)
mean_spc <- colMeans(sp)
range(bc_clover)
mean(bc_clover)

spc_id <- sp[c(max_clover, min_clover),]
spc_plot <- rbind(spc_id, mean_spc)

tiff(filename = "max_min_avgPlot_Dec15.tif", 
     width = 1600, height = 1400, 
    res = 300, family = "serif")

matplot(t(spc_plot), ty = "l", lty=1,col = c("red", "forestgreen", "gray50"), ylim = c(0,1),
        xaxt = "n", yaxt = "n", 
        xlab = "Wavelength (nm)", ylab = "Reflectance (%)",
        lwd = 2.5, main = "Spectra characteristics of clover content")
axis(1, at = seq(1,61, 5), labels = seq(400, 1000, 50))
axis(2, at = seq(0,1,.2), labels = seq(0,100, 20), las = 2)

legend("topleft", legend = c("Max (92.8%)", "Min (0.0%)", "Mean (12.7%)"), 
       col = c("red", "forestgreen", "gray50"), lty = 1, bty = 'n', lwd = 2)

dev.off()
dim(sp)
