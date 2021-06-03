library(Momocs)

setwd("E:/prac/summary")

source("./ex_oto.R")
summary1 <- extract_otolith_shape(adjust=-0.1)

grouping.fac <- read.table("./summary1.txt", header = T, stringsAsFactors = F, na.strings = " ") # loading explanatory variables
grouping.fac$picname <- as.character(grouping.fac$picname)
#grouping.fac <- grouping.fac[-165,]

fd.ls4 <- list.files("./Images", full.names = T)
pop.jpgs4 <- import_jpg(jpg.paths = fd.ls4,
                      auto.notcentered = TRUE,
                      fun.notcentered = NULL,
                      threshold = 0.5)

outs4 <- Out(pop.jpgs4, fac = grouping.fac )
outs4


panel(outs4)



#norm=FALSE might be important here, because when TRUE encountered error (flipped - see manual!!)

ldk <- def_ldk(outs4, 2, close = TRUE, points = TRUE)
ldk2<-def_ldk(outs4, 2, close = TRUE, points = TRUE)
out_ldk <- add_ldk(ldk2, 1)

stack(out_ldk, xy.axis = F, title = NULL)
out_cent <- coo_center(out_ldk)
out_norm <- coo_scale(out_cent)
stack(out_norm, xy.axis = F, title = NULL)
out_align <- fgProcrustes(out_norm)
stack(out_align, xy.axis = F, title = NULL)

calibrate_harmonicpower_efourier(out_norm, nb.h = 40)

stack(out_align)
otofs4<-efourier(out_align, 32,norm=FALSE)
class(otofs4) #outcoe, coe
otofs4



sel<-otofs4$coe
write.table(sel, 'E:/prac/summary/M.txt')

summary(otofs4)

otops4<-PCA(otofs4)
otops4
summary(otops4)
plot_PCA(otops4, labelpoints=TRUE, morphospace=TRUE)

f<-manova(otofs4~otofs4$stock)

m <- manova(otops4$x[,1:5] ~  otops4$division * otops4$fac$length_cm)
summary(m)
summary.aov(m)


#F meaning, significant meaning?
MANOVA_PW(otops3,fac=pop.pcas3)

#trying to do non-assumption clustering/classification



CLUST(otops4, k=5, legend=T, type = "horizontal",
      dist_method = "euclidean",
      hclust_method = "complete",
      retain = 0.99,
      labels,
      lwd = 1/4,
      cex = 1/2,
      palette=pal_manual(c("9", "10", "11","12")))

KMEDOIDS(otops3, k=2, metric = "euclidean")
#removed these collinear columns:PC51
#this is showing where each image is assigned to

otoks4 <- otofs4 %>% KMEDOIDS(4)
# confusion matrix, quite useful
table(otoks4$fac$division, otoks4$clustering)


otoks32 <- otofs3 %>% PCA() %>% KMEDOIDS(4, retain=0.9)
plot(otoks32)
#cluster here in the first plot, but dk how to change color; second graph, the rate is rather like level of separation/confidence

# confusion matrix; same as last one
with(otoks3, table(fac$stock, clustering))
# silhouette plot
otoks3 %>% plot_silhouette()
#table produced is more straightforward, plots are a bit vague and no legend

abc<-MDS(otofs2, method = "euclidean", k = 4)
abc
#what's this doing?

z<- otofs1%>% MDS (k=4)
#NMDS always fails session, stop!!
plot.new()
plot_MDS(z,
         axes = c(1, 2),
         points = TRUE,
         points_transp = 1/4,
         chull = TRUE,
         chullfilled = FALSE,
         labelgroups = TRUE,
         legend = TRUE,
         title = "qq",
         box = TRUE,
         axesnames = TRUE,
         palette=pal_manual(c("9", "10", "11","12")))
#not good

# KMEDOIDS as k function
k_range <- 2:12
widths <- sapply(k_range, function(k) KMEDOIDS(otops4, k=k)$silinfo$avg.width)
plot(k_range, widths, type="b")

#kmeans, looked ok, can add legend
KMEANS(otops4, 3, nax = 1:2, pch = 20, cex = 0.5)


# Variability captured by each of the PCs (simplified script)[?harmonics instead]
boxplot(otofs4)
# Scree-plot: percentage variation explained by each PC
scree_plot(otops4,1:5)



# think about the figures I need, and make them pretty
pop.pcas4 <- fac_dispatcher(otops4, "stock")
pop.pcas4 <- ordered(pop.pcas4, levels = c("CNS_NS", "West", "CNS_BW","CNS_SWI","CNS_SWB","DOWNS_S","NNS_NS","NSS_S","CNS_S","BSSS_IC","BSSS_NO","West2"))
panel(outs4, fac = pop.pcas4, names= T, palette=pal_manual(c("14", "10", "11","12","13")))

pop.pcas5 <- fac_dispatcher(otops4, "division")
pop.pcas5 <- ordered(pop.pcas5, levels = c("2a", "3d", "4a","4b","4c","6a","7d"))
panel(outs4, fac = pop.pcas5, names= T, palette=pal_manual(c("14", "10", "11","12","13")))
# create my_pal function to use my palette
my.col <- c(
  'NSAS'     = "#1",
  'West'     = "#4",
  'BW'    = "#6",
  'SWI'      = "#19",
  'SWB'     = "#2",
  'EEC_08'      = "#21")

my_col <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (my.col)
  
  my.col[cols]
}

my_col()

my.palette <- list(
  `main`  = my_col("NSAS", "West", "BW","SWI","SWB","EEC_08")
)

my_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- my.palette[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

plot.new()
plot_PCA(tryp)

y2<-plot_PCA(tryFp, f=pop.try, axes = c(1, 2),
            points = TRUE,
            points_transp = 1/4,
            morphospace = T,
            morphospace_position = "range",
            chull = TRUE,
            chullfilled = F,
            labelpoints = F,
            labelgroups = T,
            legend = T,
            title = "hh",
            center_origin = T,
            zoom = 1.15,
            eigen = TRUE,
            box = T,
            axesnames = T,
            axesvar = T)

x <- plot_PCA(
  otops1,
  f = pop.pcas1,
  palette = my_pal("main"),
  axes = c(1, 2),
  points = TRUE,
  points_transp = 1/4,
  morphospace = T,
  morphospace_position = "range",
  chull = TRUE,
  chullfilled = F,
  labelpoints = T,
  labelgroups = T,
  legend = T,
  title = "hh",
  center_origin = TRUE,
  zoom = 1.15,
  eigen = TRUE,
  box = T,
  axesnames = T,
  axesvar = T
) 

oldas4 <- LDA(otofs4, pop.pcas5, retain=0.99)
oldas4
# 8 PC retained
# warning: variables are collinear
plot_LDA(oldas4)
plot_CV(oldas3)
plot_CV2(oldas3)

# run these again with eec dropped off

drop<-filter(otofs4, stock %in% c("West2", "BSSS_IC","BSSS_NO","DOWNS_S","NNS_S","CNS_S","West"))
dropp<-PCA(drop)
plot(dropp)

pop.pcas42 <- fac_dispatcher(dropp, "stock")
pop.pcas42 <- ordered(pop.pcas42, levels = c("West2", "BSSS_IC","BSSS_NO","DOWNS_S","NNS_S","CNS_S","West"))


y<-plot_PCA(dropp, f=pop.pcas42, axes = c(1, 2),
            points = TRUE,
            points_transp = 1/4,
            morphospace = T,
            morphospace_position = "range",
            chull = TRUE,
            chullfilled = F,
            labelpoints = F,
            labelgroups = T,
            legend = T,
            title = "hh",
            center_origin = T,
            zoom = 1.15,
            eigen = TRUE,
            box = T,
            axesnames = T,
            axesvar = T)

droplda <- LDA(dropp, pop.pcas32, retain=0.99)
droplda
# 8 PC retained
# warning: variables are collinear
plot_LDA(droplda)
plot_CV(droplda)
plot_CV2(droplda)

# KMEDOIDS as k function
k_range <- 2:12
widths <- sapply(k_range, function(k) KMEDOIDS(dropp, k=k)$silinfo$avg.width)
plot(k_range, widths, type="b")

#kmeans, looked ok, can add legend
KMEANS(dropp, 4, nax = 1:2, pch = 20, cex = 0.5)



dropk <- drop %>% KMEDOIDS(4)
# confusion matrix, quite useful
table(dropk$fac$stock, dropk$clustering)


KMEDOIDS(dropp, k=4, metric = "euclidean")
#choose 4 points as center, their PC value