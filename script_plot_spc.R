rm(list = ls())

setwd('c:/Users/lomo0003/SLU/YaraNSensor/Data_analysis/')


d <- read.table("./Inputs/Matrix_YARA_2.csv", header=T, sep=",")
dim(d)
### all klover %
classes<-ifelse(d$pctg_clover_planilha == 0, 0, ifelse(
  d$pctg_clover_planilha <= 10, 1, ifelse(
  d$pctg_clover_planilha <= 20, 2, ifelse(
  d$pctg_clover_planilha <= 30, 3, ifelse(
  d$pctg_clover_planilha <= 40, 4, ifelse(
  d$pctg_clover_planilha <= 50, 5, ifelse(
  d$pctg_clover_planilha <= 60, 6, ifelse(
  d$pctg_clover_planilha <= 70, 7, ifelse(
  d$pctg_clover_planilha <= 80, 8, ifelse(
  d$pctg_clover_planilha <= 90,9,10))))))))))
d$classes <- classes

lista <- list(
a0 <- (d[d$classes == 0,29:89]),
a1 <- d[d$classes == 1,29:89],
a2 <- d[d$classes == 2,29:89],
a3 <- d[d$classes == 3,29:89],
a4 <- d[d$classes == 4,29:89],
a5 <- d[d$classes == 5,29:89],
a6 <- d[d$classes == 6,29:89],
a7 <- d[d$classes == 7,29:89],
a8 <- d[d$classes == 8,29:89],
a9 <- d[d$classes == 9,29:89],
a10 <- d[d$classes == 10,29:89])

require(colorRamps)
require(ggplot2)
require(RColorBrewer)

matplot(t(lista[[2]]), ty = "l", lty=1,col = rainbow(11)[2])

for (i in 2:11) {
 
  matplot(t(lista[[i]]), ty = "l",lty=1, col = rainbow(11)[i], add = T)
   
 }

dev.off()

## colMeans by klover %

lista1 <- list(
  a0 <- colMeans(d[d$classes == 0,29:89]),
  a1 <- colMeans(d[d$classes == 1,29:89]),
  a2 <- colMeans(d[d$classes == 2,29:89]),
  a3 <- colMeans(d[d$classes == 3,29:89]),
  a4 <- colMeans(d[d$classes == 4,29:89]),
  a5 <- colMeans(d[d$classes == 5,29:89]),
  a6 <- colMeans(d[d$classes == 6,29:89]),
  a7 <- colMeans(d[d$classes == 7,29:89]),
  a8 <- colMeans(d[d$classes == 8,29:89]),
  a9 <- colMeans(d[d$classes == 9,29:89]),
  a10 <- colMeans(d[d$classes == 10,29:89]))

col_pal <- colorRampPalette(c("darkred", "yellow", "lightgreen", "darkgreen"))
colour_pal <- col_pal(11)

# cores <- c('red', 'blue','green',
#            'purple','yellow', 'black',
#            'orange', 'magenta', 'brown',
#            'turquoise4', 'grey')

tiff("./Outputs/spectra/Average_PctgClover.tif", width = 2000, height = 1500, res = 300, , family = "serif")
matplot(lista1[[1]], ty = "l", lty=1,col = colour_pal[1], ylim = c(0,1),
        xaxt = "n", yaxt = "n", 
        xlab = "Wavelength (nm)", ylab = "Reflectance (%)",
        lwd = 1.5, main = "Mean percentual of clover content")
axis(1, at = seq(1,61, 5), labels = seq(400, 1000, 50))
axis(2, at = seq(0,1,.2), labels = seq(0,100, 20), las = 2)


for (i in 2:11) {
  
  matplot((lista1[[i]]), ty = "l",lty=1, col = colour_pal[i], add = T, lwd = 1.5)
  
}
legend("topleft", legend = c("< 10%", "10-20%", "21-30%", "31-40%",
                             "41-50%", "51-60%", "61-70%", "71-80%",
                             "81-90%", "> 90%"), col = colour_pal,
       lty = 1,lwd = 3, bty = "n",
       cex = .8, ncol = 1)
dev.off()


## by N 


table(d$kg_N_ha)

classe2 <- ifelse(d$kg_N_ha == '0', 0, ifelse(
  d$kg_N_ha == '20', 1, ifelse(
    d$kg_N_ha == '30', 2, ifelse(
      d$kg_N_ha == '40', 3, ifelse(
        d$kg_N_ha == '40+40', 4, ifelse(
          d$kg_N_ha == '60', 5, ifelse(
            d$kg_N_ha == '80', 6, ifelse(
              d$kg_N_ha == '90', 7, ifelse(
                d$kg_N_ha == '120',8, 9)))))))))
classe2[is.na(classe2)] <- 9

d$classes2 <- classe2

lista2 <- list(
  a0 <- (d[d$classes2 == 0,29:89]),
  a1 <- d[d$classes2 == 1,29:89],
  a2 <- d[d$classes2 == 2,29:89],
  a3 <- d[d$classes2 == 3,29:89],
  a4 <- d[d$classes2 == 4,29:89],
  a5 <- d[d$classes2 == 5,29:89],
  a6 <- d[d$classes2 == 6,29:89],
  a7 <- d[d$classes2 == 7,29:89],
  a8 <- d[d$classes2 == 8,29:89],
  a9 <- d[d$classes2 == 9,29:89])


matplot(t(lista2[[1]]), ty = "l", lty=1, col = rainbow(10)[1])

for (i in 2:10) {
  
  matplot(t(lista2[[i]]), ty = "l",lty=1, col = rainbow(10)[i], add = T)
  
}

dev.off()


## colmeans by N

lista3 <- list(
  a0 <- colMeans(d[d$classes2 == 0,29:89]),
  a1 <- colMeans(d[d$classes2 == 1,29:89]),
  a2 <- colMeans(d[d$classes2 == 2,29:89]),
  a3 <- colMeans(d[d$classes2 == 3,29:89]),
  a4 <- colMeans(d[d$classes2 == 4,29:89]),
  a5 <- colMeans(d[d$classes2 == 5,29:89]),
  a6 <- colMeans(d[d$classes2 == 6,29:89]),
  a7 <- colMeans(d[d$classes2 == 7,29:89]),
  a8 <- colMeans(d[d$classes2 == 8,29:89]),
  a9 <- colMeans(d[d$classes2 == 9,29:89]))


col_pal = colorRampPalette(c("red", "yellow", "lightblue", "navy"))
colour_pal = col_pal(10)

tiff("./Outputs/spectra/Average_NitrogenRates.tif", width = 2000, height = 1500, res = 300, , family = "serif")
matplot((lista3[[1]]), ty = "l", lty=1,col = colour_pal[1], ylim = c(0,1),
        xaxt = "n", yaxt = "n", 
        xlab = "Wavelength (nm)", ylab = "Reflectance (%)", lwd = 2,
        main = expression(Nitrogen~rates~(kg~ha^-1)))
axis(1, seq(1,61,5), labels = seq(400,1000, 50))
axis(2, seq(0,1,.2), seq(0,100,20), las = 2)

for (i in 2:10) {
  matplot((lista3[[i]]), ty = "l",lty=1, col = colour_pal[i], add = T,
          lwd = 2)
}

legend("topleft", legend = c("0", "20",
                             "30", "40",
                             "40+40", "60","80", "90",
                             "120"),
       col = colour_pal, lty = 1, lwd = 3, bty = "n", cex = .8)

dev.off()



