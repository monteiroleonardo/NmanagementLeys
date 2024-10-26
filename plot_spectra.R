rm(list=ls())
require(dplyr)
require(tidyverse)

setwd('C:/Users/lomo0003/SLU/YaraNSensor/Data_analysis/')

d <- read.table("./Inputs/Matrix_YARA_2.csv", header=T, sep=",")
head(d)

d1 <- d %>% select(Place,Column,CUT_YEAR, contains("Average.")) %>%
  mutate(Place_Column = paste0(Place, "_",Column)) %>%
  arrange(Place_Column)
d1
head(d1)
x <- d1$Place_Column
x1 <- unique(x)

table(x)

seg_cv <- lapply(unique(d1$Place_Column), 
                 function(x, df) which(d1$Place_Column == x), 
                 df = d1)
seg_cv

for(i in 1:length(unique(x))){
  #i <- 1
  ly <- unlist(seg_cv[i][1])
  mat1 <- as.matrix(d1[ly,])
  mat2 <- mat1[,8:68]

  col <- c ("blue","orange","red2","skyblue","deeppink1", "green","plum3","darkred","purple","pink","green",
            "seagreen","gray50","slateblue3", "magenta",
            "orangered3","greenyellow","cyan1","aquamarine","turquoise3","yellow2")
  
 file_name <- paste(unique(x)[i])
 x_vals = seq(400,1000,10)
 y_vals = mat2
 
  tiff(paste0('./Outputs/test_',file_name, '.tiff'),height = 1200, width = 1800, compression = 'lzw', res = 300)   
  par(mfrow= c(1,1), family = 'serif',mar= c(8, 8, 4, 8), cex = .75)
  par(las=1)
  
  ymax <- as.numeric(max(mat2))
  ymin <- as.numeric(min(mat2))
  
  y <- as.numeric(mat2[1,])
  
  plot(y, ty = 'l', lty=1,lwd=1, col=col[1], 
       ylim = c(0, 1),
       axes = F, ylab = '', xlab = '', 
       main = paste0("Place ", unique(d1$Place[ly])," Column ", unique(d1$Column[ly])),
       xaxt = "n", yaxt = "n")
  
  axis(1, at = seq(1,61,5),labels = seq(400, 1000, 50), las = 1)
  axis(2, seq(0, 1, 0.2), las = 1, labels =seq(0,100, 20))
  
  mtext(side = 1,(bquote('Wavelength [nm]')),padj = 5 , col="black", cex=1)
  mtext(side = 2, "Reflectance (%)",padj = -5 , las=3, col="black", cex=1 )
  
  if(nrow(mat2)>1){
    for(w in 2:nrow(mat2)){
      z = as.numeric(mat2[w, ])
      lines(y = z, x = 1:61, col = col[w])
    }
  }
  box()    
  legend("topleft", inset=.05, 
         cex=1,
         bty = "n",            
         lwd=1, 
         legend=d1$CUT_YEAR[ly],
         col = col)
  dev.off()
  
  print(i)
}

