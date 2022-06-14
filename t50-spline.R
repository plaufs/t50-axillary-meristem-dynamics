
##################### Allows to calculte t50 (time for half development) from kinetics
library(graphics)
library(splines)
library(splines2)
library(ggplot2)
setwd("~/Bureau")
dat<-read.csv("rosette_CUC_NGAL.csv")
# data should be arranged in columns. The first column contoins the days, and one column for each gentoype
# the first line contains the genotype names

# create the file to save the results
nrow <- nrow(dat)
ncol <- ncol(dat)
genotypes = colnames(dat,2)
resultat <- matrix (nrow = (ncol-1), ncol=4)
colnames(resultat) <- c("Genotype",  "deg liberte", "t50", "maxrosette")
Days <- dat[,1]

# Analyse data from column nimber i
# run from line 30 to 66 for each genotype

  # enter value for i, the column number of the data to analyse
  # enter value for deg_lig, the liberty degree of the splines. tipically between 10 and 15
  # enter value max rosette, the max value reach for the % of developped meristem. 
  # Should be 100 if all the meristems are developped or less if some are missing at the end of the kinetic
  # _____________________________________________________________________________________


  i <- 2    # enter data
  deg_lib <-10  # enter data
  maxrosette <- 100  # enter data
  
  # read data
  Genotype = genotypes[i]
  y <- dat[,i]
  
  # interpolation
  sp <- smooth.spline (Days, y, df=deg_lib)
  
  # calculate t50, the time at which half of the meristems are developped
  min=0
  max=20
  repeat{
  milieu=(min+max)/2
  ymilieu = predict(sp, milieu)$y
  max=ifelse(ymilieu>(maxrosette/2), milieu, max)
  min=ifelse(ymilieu<maxrosette/2, milieu, min)
  if (round (ymilieu, 10)==maxrosette/2) {break}}
  t50=round(milieu, 2)
  
  # write results in file
  resultat[i-1,] <- c(Genotype, deg_lib, t50, maxrosette)
  
  # draw graph to check quality of interpolation 
  plot (y ~ Days, main=paste(Genotype), las=1, ylab="% developped meristems", ylim=c(0, 100), col="blue" )
  lines(sp, lty = 2, col = "blue")
  segments(-0.5,(maxrosette/2),milieu,(maxrosette/2), col= "red", lty=3)
  segments(milieu, 0, milieu, (maxrosette/2), col= "red", lty=3)
  text(milieu, 20, labels=paste("t50 =",t50, " days"), col="red", pos=4, cex=0.9)
  text(20, 50, labels=paste(" deg_lib=",deg_lib), col="black", pos=2, cex=0.7)
  
  # save graph and data
  dev.print(device = png, file = Genotype, width = 600)
  write.csv(resultat, file="resultat.csv")
  
  # _____________________________________________________________________________________
  
# END
