library(tidyverse)
library(ISLR)
library(ggplot2)
library(GGally)
library(patchwork)

library(factoextra)
library(FactoMineR)
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



df = read.csv('10Unsuper//Ch10Ex11.csv', header=F)
df = t(df)
df_std = apply(df,2,scale)

pca = prcomp(df,  scale. = TRUE)

biplot(pca)
sum = summary(pca)


cor_mat = cor(t(df))
cor_mat2 = cor(t(df_std))

apply(df,1,sd)

ggcorrplot::ggcorrplot(cor_mat)
ggcorrplot::ggcorrplot(cor_mat2)


dd          = as.dist(1-cor(t(df)))
dd = dist(df)
dd = as.dist(1-cor(t(apply(df,2,scale))))
hc.complete = hclust(dd, method ='complete')
hc.average  = hclust(dd, method ='average')
hc.single   = hclust(dd, method ='single')

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
