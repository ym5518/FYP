setwd("E:/prac/day 6/ShapeAnalysis")

library(Momocs)
shapeorg <- shapeR("E:/prac/day 6/ShapeAnalysis", "summary1.csv")

#need to be csv, check variables and columns, important...
#shapeorg@master.list.org$pop<-as.factor(shapeorg@master.list.org$pop)
#shapeorg@master.list.org$stock<-as.factor(shapeorg@master.list.org$stock)
#shapeorg@master.list.org$division<-as.factor(shapeorg@master.list.org$division)

shapeorg <- detect.outline(shapeorg, threshold=0.2,write.outline.w.org = FALSE)

#shaperemove <- remove.outline(shapeout,"ALL","image001.png")
#before this step, consider remove some bad pics/smooth outline!
#shapenew<-remove.outline(shapenew,"ALL","image381")


shapecoe <- generateShapeCoefficients(shapeorg) 

shapecoef <- enrich.master.list(shapecoe, folder_name = "folder", pic_name = "picname", 
                             calibration = "cal", include.wavelet = TRUE, include.fourier = TRUE, 
                             n.wavelet.levels = 6, n.fourier.freq = 32)



#summary(is.na(shapecoef@master.list))
#shapeout@outline.list

#shape@master.list$stock
#class(shapecoef1@master.list$stockfac)
#class(shape@fourier.coef.raw)

#shapecoef1@master.list$pop<-as.factor(shapecoef1@master.list$pop)

shapecoefstd <- stdCoefs(shapecoef, classes = "specified", "length_cm")
#coefstd <- stdCoefs(shape, classes = "pop", "length_cm", bonferroni = T)



#get to momocs
fcoe<-getStdFourier(shapecoefstd)
write.table(fcoe, 'E:/prac/day 6/ShapeAnalysis/Fcoe.txt')
wcoe<-getStdWavelet(shapecoefstd)
write.table(wcoe, 'E:/prac/day 6/ShapeAnalysis/Wcoe.txt')

dimen<-getMeasurements(shapecoef)
write.table(dimen, 'E:/prac/day 6/ShapeAnalysis/dimen.txt')

library(Momocs)

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

#fcoe<-read.table("E:/prac/day 6/ShapeAnalysis/Fcoe.txt", header=T)

#import to make first column as row name
Wcoe <- read.csv("E:/prac/day 6/ShapeAnalysis/Wcoe.txt", row.names=1, sep="")
tryW<-OutCoe(coe=Wcoe,fac=grouping, method="efourier", norm=TRUE)

boxplot(tryW)
scree_plot(tryWp)

class(tryF)
grouping <- read.table("E:/prac/day 6/ShapeAnalysis/summaryW.txt", header = T, stringsAsFactors = F, na.strings = " ") # loading explanatory variables
grouping$picname <- as.character(grouping$picname)
#grouping <- grouping[-165,]

tryWp<-PCA(tryW)
PCcontrib(tryWp)

tryWp$x

dev.off()
plot.new()

tryFp$fac$pop

plot.new()
pop.fac <- fac_dispatcher(tryFp, "pop")
pop.fac <- ordered(pop.fac, levels = c( "BS","NS","SC"))
tryplot<-plot_PCA(tryFp, f=pop.fac, morphospace=F, axes = c(1, 2), chull=T)

stock.fac<-fac_dispatcher(tryFp, "stock")
stock.fac <- ordered(stock.fac, levels = c( "NNS_pure","NNS_mix","CNS_pure","CNS_mix","SNS_mix","BS_pure","SC_pure"))
tryplot<-plot_PCA(tryFp, f=stock.fac, morphospace=F, axes = c(1, 2), chull=T)

pop.fac

summary(tryFp)

tryF$fac

ldaF <- LDA(tryFp, pop.fac, retain=0.99)
ldaF
# 8 PC retained
# warning: variables are collinear
plot_LDA(ldaF)

#clustering
# KMEDOIDS as k function
k_range <- 2:12
widths <- sapply(k_range, function(k) KMEDOIDS(tryFp, k=k)$silinfo$avg.width)
plot(k_range, widths, type="b")

#kmeans, looked ok, can add legend
KMEANS(tryFp, 3, nax = 1:2, pch = 20, cex = 0.5)

tryFk <- tryFp %>% KMEDOIDS(3)
# confusion matrix, quite useful
table(tryFk$fac$pop, tryFk$clustering)


##################filtered baseline#######################################
baseF<-filter(tryF, stock %in%c("NNS_pure","CNS_pure")|specified%in%c("BS_NO","West2"))
baseF
s1 <- filter(tryF, picname %in% c("image063")|stock %in%c("NNS_pure","CNS_pure")|specified%in%c("BS_NO","West2"))

baseFp<-PCA(baseF)

s1p$fac
pop.base.fac <- fac_dispatcher(bigNp, "pop")
pop.base.fac <- ordered(pop.base.fac, levels = c("BS","NS","SC"))
stock.base.fac<-fac_dispatcher(s1p, "stock")
stock.base.fac
stock.base.fac <- ordered(stock.base.fac, levels = c( "BS_pure","CNS_pure","SC_pure","NNS_pure","NNS_mix"))
cont.base.fac<-fac_dispatcher(contWp, "specified")
cont.base.fac <- ordered(cont.base.fac, levels = c( "BS_NO","BS_IC","West1"))

tryplot<-plot_PCA(s1p, f=stock.base.fac, morphospace=F, axes = c(1, 2), chull=T, points = TRUE,labelpoints=T)


lda.baseW <- LDA(s1p, stock.base.fac, retain=0.99,labelpoints=T)
lda.baseW
plot_LDA(lda.baseW,labelgroups=T)


k_range <- 2:12
widths <- sapply(k_range, function(k) KMEDOIDS(baseFp, k=k)$silinfo$avg.width)
plot(k_range, widths, type="b")

#kmeans, looked ok, can add legend
KMEANS(baseFp, 3, nax = 1:2, pch = 20, cex = 0.5)

baseFk <- baseFp %>% KMEDOIDS(3)
# confusion matrix, quite useful
table(baseFk$fac$stock, baseFk$clustering)
baseFk$clustering

basecl <- read.delim("E:/prac/basecl.txt")
table(basecl$gender,basecl$cl_stock)


boxplot(basecl$weight~ basecl$cl_stock)
kruskal.test(weight ~ cl_stock, data =basecl)

CLUST(tryF, k=4)

###############filter SNS#################

snsF<-filter(tryF, stock %in% c("SNS_mix")| baseline %in% c("yes"))
snsFp<-PCA(snsF)

sns.stock.fac<-fac_dispatcher(snsFp, "stock")
sns.stock.fac <- ordered(sns.stock.fac, levels = c( "BS_pure","CNS_pure","NNS_pure", "SC_pure", "SNS_mix"))

tryplot<-plot_PCA(snsFp, f=sns.stock.fac, morphospace=F, axes = c(1, 2), chull=T)

ldaF <- LDA(snsFp, sns.stock.fac, retain=0.99)
ldaF
# 8 PC retained
# warning: variables are collinear
plot_LDA(ldaF)

#############################wavelet###########################


#############################################################################end of momocs#################


plotWavelet(shapecoefstd,level=5,class.name= "pop",useStdcoef=TRUE)
plotFourier(shapecoefstd,class.name= "pop",useStdcoef=TRUE)

#useful here
par(mar = c(0.1,0.1,0.1,0.1))
plot.new()
plotWaveletShape(shapecoef, "stock",show.angle = FALSE,lwd=3,lty=1,legend=F) 
plotFourierShape(shapecoef, "stock",show.angle = FALSE,lwd=3,lty=1,legend=F)

est.list <- estimate.outline.reconstruction(shapecoefstd)

outline.reconstruction.plot(est.list,panel.first = grid())

# est.list = estimate.outline.reconstruction(Stock_Data)

library(vegan)

capp <- capscale(getStdFourier(shapecoefstd) ~ getMasterlist(shapecoefstd)$stock)
anova(capp)

eig<-eigenvals(capp, constrained =TRUE)
eig.ratio = eig/sum(eig)
plot.new()
cluster.plot(scores(capp)$sites[,1:2],getMasterlist(shape)$pop,
             
             xlim = range(scores(capp)$sites[,1]),
             
             ylim = range(scores(capp)$sites[,2]),
             
             xlab = paste("CAP1 (",round(eig.ratio[1]*100,1),"%)",sep = ""),
             
             ylab = paste("CAP2 (",round(eig.ratio[2]*100,1),"%)",sep = ""), plotCI = TRUE,conf.level = 0.95,las = 1)

# failed for otooo/capp but dont know why, but can see CAP scores directly by command below (eig is like the amount of variaty explained)

scores(capp)$sites[,1:2]

#classification

#shape <- setFilter(shape, getMasterlist(shape, useFilter = FALSE)$pop %in% c("SC","NO"))
pop <- factor(getMasterlist(shapecoefstd)$stock)

#Estimation of the classifiers success rate based on the Linear Discriminant Analysis can be done with bootstrap or cross-validation using the errorest function in the ipred package [11]. Here we show an example of how to run a cross-validation estimation using the cv estimator:

library(ipred)
library(shapeR)
library(vegan)
library(MASS)


mypredict.lda <- function(object, newdata)
  
  predict(object, newdata = newdata)$class

stdw <- getStdWavelet(shape)

pop <- factor(getMasterlist(shape)$stock)

dd <- data.frame(stdw = stdw,pop = pop)

errorest(pop ~., data = dd, model=lda, estimator = "cv", predict = mypredict.lda,est.para = control.errorest(nboot = 1000))

