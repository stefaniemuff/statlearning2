library(tidyverse)
library(ISLR)
library(ggplot2)
library(GGally)
library(patchwork)

library(factoextra)
library(FactoMineR)

PCbiplot <- function(PC, x="PC1", y="PC2", colors=c('black', 'black', 'red', 'red')) {
  # PC being a prcomp object
  data <- data.frame(obsnames=row.names(PC$x), PC$x)
  plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames), color=colors[1])
  plot <- plot + geom_hline(yintercept = 0, size=.2) + 
    geom_vline(xintercept  = 0, size=.2, color=colors[2])
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color=colors[3])
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color=colors[4])
  plot
}




# scaling in PCA ----------------------------------------------------------


states = row.names(USArrests)
USArrests[1:3,]

apply(USArrests,2,mean)
apply(USArrests,2,var)

# perform PCA
p1 =  prcomp(USArrests, scale = FALSE, center = FALSE)
p2 =  prcomp(USArrests, scale = FALSE, center = TRUE)
p3 =  prcomp(USArrests, scale = TRUE, center = TRUE)



PCbiplot(p1) +
  PCbiplot(p2) + plot_layout(ncol = 1)
  PCbiplot(p3)  + plot_layout(ncol = 2)











data("decathlon2")
decathlon2.active <- decathlon2[1:23, 1:10]
names(decathlon2.active) <- c("100m","long_jump","shot_put","high_jump","400m","110.hurdle","discus","pole_vault","javeline","1500m")


dat0 = decathlon2.active
dat1 = dat0 %>% mutate(across(1:10,\(x) scale(x,center =T, scale = F)))
dat2 = dat0 %>% mutate(across(1:10,\(x) scale(x,center =T, scale = T)))

pca0 = prcomp(dat0, scale = F)
pca1 = prcomp(dat1, scale = F)
pca2 = prcomp(dat2, scale = F)

pca0$rotation[,1]
pca1$rotation[,1]
pca2$rotation[,1]


biplot(pca0)
biplot(pca1)
biplot(pca2)


dat0 %>% arrange(`1500m`)
dat0 %>% ggplot() + geom_point(aes(`100m`, long_jump))
pca2$rotation[,1]



# ex 11, Ch10 -------------------------------------------------------------



df = read.csv('Ch10Ex11.csv', header=F)

df_std = apply(df,2,scale)

pca = prcomp(df,  scale. = TRUE)

biplot(pca)
sum = summary(pca)
plot(sum$importance[3,], ylim = c(0,1))
plot(sum$importance[2,])

cor_mat = cor(t(df))
cor_mat2 = cor(t(df_std))

apply(df,1,sd)

ggcorrplot::ggcorrplot(cor_mat)
ggcorrplot::ggcorrplot(cor_mat2)


dd = as.dist(1-cor(t(df)))
dd = dist(df)
dd = as.dist(1-cor(t(apply(df,2,scale))))
hc.complete = hclust(dd, method ='complete')
hc.average  = hclust(dd, method ='average')
hc.single   = hclust(dd, method ='single')



hc.complete <- hclust(as.dist(1 - cor(df)), method='complete')
hc.average <- hclust(as.dist(1 - cor(df)), method='average')
hc.single   = hclust(as.dist(1 - cor(df)), method='single')



plot(hc.complete, main='complete linkage w/ correlation-based distance', xlab='', sub='')
plot(hc.average, main='avreage linkage w/ correlation-based distance', xlab='', sub='')
plot(hc.single, main='single linkage w/ correlation-based distance', xlab='', sub='')

g1 = cutree(hc.complete, 2) # identify the decreased group correctly 
g2 = cutree(hc.average, 2) # identify the decreased group correctly
g3 = cutree(hc.single, 2) # fail to identify 2 groups

Km = kmeans(df,2, nstart = 25)



cbind(true = rep(c(1,2),each = 20),
      complete = g1,
      average = g2,
      single = g3,
      kmean = Km$cluster)

