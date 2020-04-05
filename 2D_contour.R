##1
library(RColorBrewer)
library(MASS)
library(fields)
data1<-read.table("pron_tg7_a6.dat")
data2<-read.table("prob_tg7_c6.dat")
data3<-read.table("prob_tg7_g8.dat")
data4<-read.table("prob_tg7_t8.dat")
X1<-data.frame(data1$V1,data1$V2)
X2<-data.frame(data2$V1,data2$V2)
X3<-data.frame(data3$V1,data3$V2)
X4<-data.frame(data4$V1,data4$V2)
k<-5
my.cols <- rev(brewer.pal(k, "Spectral"))
fudgeit <- function(){
  xm <- get('xm', envir = parent.frame(1))
  ym <- get('ym', envir = parent.frame(1))
  z  <- get('dens', envir = parent.frame(1))
  colramp <- get('colramp', parent.frame(1))
  fields::image.plot(xm,ym,z, col = colramp(256), legend.only = T, add =F)
}
par(mar = c(5,4,4,5) + .1)
par(mfrow=c(2,2))
smoothScatter(X1, nrpoints=0, colramp=colorRampPalette(my.cols), pch=19, cex=.8, postPlotHook = fudgeit, xlab="Pseudodihedral angle", ylab="energy (kcal/mol)",main="thy7-a6")  #1
smoothScatter(X2, nrpoints=0, colramp=colorRampPalette(my.cols), pch=19, cex=.8, postPlotHook = fudgeit, xlab="Pseudodihedral angle", ylab="energy (kcal/mol)",main="thy7-c6")  #1
smoothScatter(X3, nrpoints=0, colramp=colorRampPalette(my.cols), pch=19, cex=.8, postPlotHook = fudgeit, xlab="Pseudodihedral angle", ylab="energy (kcal/mol)",main="thy7-g8")  #1
smoothScatter(X4, nrpoints=0, colramp=colorRampPalette(my.cols), pch=19, cex=.8, postPlotHook = fudgeit, xlab="Pseudodihedral angle", ylab="energy (kcal/mol)",main="thy7-t8")  #1
##2

df=data.frame(data1$V1,data1$V2)
m<-ggplot(data=data1,aes(data1$V2,data1$V2))
m + stat_density_2d(aes(fill = stat(level)), geom = "polygon") #2


library(MASS)
#files <- list.files(path="/media/tanashree/My Passport/Urea_BASE/stacking intercations/stack", pattern=".dat")
files<-Sys.glob("*.dat") 
for (i in 1:72)
{
sample<-read.table(files[i])

b<-sample$V1

a<-sample$V2

density<-kde2d(b,a)

pdf(paste(files[i],".eps    ",sep=""))

filled.contour(density,color.palette=colorRampPalette(c('cadetblue4','white','red')),xlab="Distance",ylab="Angle1",main="base_conc")  #3

dev.off()
}


