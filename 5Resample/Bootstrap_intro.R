rm(list=ls())
library(tidyverse)
library(ggplot2)
library(gridExtra)

data = tibble(time = c(94,197,16,38,99,141,23,52,104,146, 10,51,30,40,27,46),
          group = c(rep("treatment",7),rep("control",9)))



treat = data %>% dplyr::filter(group=="treatment") %>% pull(time)
contr = data %>% dplyr::filter(group=="control") %>% pull(time)


## compute the means
m1 =  mean(treat)
m2 =  mean(contr)
diff_mean = m1-m2


## boxplot of the data
data %>% ggplot() + geom_boxplot(aes(group,time)) + 
  geom_hline(yintercept = c(m1,m2), color  = c("red", "blue"))

## is this significant??

## biased version of the variance (divided by n instead of n-1 as in the var() function)
my_var = function(x)
  return(sum((x-mean(x))^2)/length(x))

var_m1 = my_var(treat)/length(treat)
var_m2 = my_var(contr)/length(contr)
var_diff_mean = var_m1 + var_m2
sd_diff_mean = sqrt(var_diff_mean)

CI_diff_mean = c(diff_mean-1.97*sd_diff_mean, diff_mean+1.97*sd_diff_mean)


ggplot(data = data.frame(x = c(-20, 160)), aes(x)) +
  stat_function(fun = dnorm, n = 201, args = list(mean = m1, sd = sqrt(var_m1)),
                color  = "red") +
  stat_function(fun = dnorm, n = 201, args = list(mean = m2, sd = sqrt(var_m2)),
                color = "blue") +
  ylab("") +
  scale_y_continuous(breaks = NULL)


ggplot(data = data.frame(x = c(-80, 160)), aes(x)) +
  stat_function(fun = dnorm, n = 201, args = list(mean = diff_mean, sd = sd_diff_mean),
                color  = "red") +
  ylab("") +geom_vline(xintercept = CI_diff_mean)

## compute the medians

median1 = median(treat)
median2 = median(contr)
diff_median = median1-median2









##  is this significant??
## we need to know the sd of the median estimate and here we 
## have no analytical formula!


ggplot(data, aes(time, group=group, color =  group)) + 
  stat_ecdf(geom = "step") + facet_grid(group~.)


B = 10
est =  matrix(NA, B, 4)
PLOT = TRUE
for(i in 1:B)
{
  treat_star = sample(treat, 7, replace=T)
  contr_star = sample(contr, 9, replace=T)
  cat("sample nr", i, "\n")
  cat("resampled tratment:", treat_star, "\n")
  cat("resampled control:", contr_star, "\n")
  cat("\n")
  if(PLOT)
  {
    p1 = data %>% mutate(time = c(treat_star,contr_star)) %>% 
      ggplot(aes(time, group=group, color =  group)) + stat_ecdf(geom = "step") + facet_grid(.~group) 
    p2 = data %>% mutate(time = c(treat_star,contr_star)) %>% 
      ggplot(aes(group, time)) + geom_boxplot()
    grid.arrange(p1,p2)
    readline("Press to continue")
  }
  est[i,1] = mean(treat_star)
  est[i,2] = mean(contr_star)
  est[i,3] = median(treat_star)
  est[i,4] = median(contr_star)
  
}


est  = data.frame(est) %>% mutate(i = 1:B)
colnames(est) = c("mean_treat", "mean_contr", "median_treat", "median_contr", "i")

dummy = data.frame(a = c("mean","mean","median","median"),
                   b = c("treat", "contr","treat","contr"),
                   val = c(m1,m2,median1, median2))

est %>% pivot_longer(-i) %>%
  separate(name, into = c("a","b")) %>%
  ggplot(aes(i, value,group = b, color = b)) + geom_line() +
  facet_grid(a~.) +
  geom_hline(data = dummy, aes(yintercept = val, color = b ),
             linetype = "dashed")

v1 = sqrt(my_var(est$mean_treat)+my_var(est$mean_contr))
v2 =sqrt(my_var(est$median_treat)+my_var(est$median_contr))

sqrt(v1^2)
sd_diff_mean


### what if we increas B?
B = c(50, 100, 250, 500,1000, 2000, 4000, 6000, 8000, 10000, 12000, 15000)
sd_mean = sd_median = numeric(length(B))
mean_resample = median_resample = list()
for(b in 1:length(B))
{
    treat_star = sample(treat, 7*B[b], replace=T) %>% matrix(B[b],7)
    contr_star = sample(contr, 9*B[b], replace=T) %>% matrix(B[b],9)
    sd_mean[b] = sqrt(my_var(apply(treat_star,1, mean)) + 
                        my_var(apply(contr_star,1, mean)))
    
    mean_resample[[b]] = apply(treat_star,1,mean) - apply(contr_star,1,mean)
    
    treat_star = sample(treat, 7*B[b], replace=T) %>% matrix(B[b],7)
    contr_star = sample(contr, 9*B[b], replace=T) %>% matrix(B[b],9)
    
    
    sd_median[b] = sqrt(my_var(apply(treat_star,1, median)) + 
                          my_var(apply(contr_star,1, median)))
    median_resample[[b]] = apply(treat_star,1,median) - apply(contr_star,1,median)
    
    
    

}

dummy = data.frame(name =   c("sd_mean","sd_median" ), 
                   val = c(sd_diff_mean, NA))

data.frame(B= B, sd_mean = sd_mean, sd_median = sd_median) %>%
  pivot_longer(-B) %>%
  ggplot(aes(B, value)) + geom_point() + 
  geom_line() +
  geom_hline(data = dummy, aes(yintercept = val), linetype = "dashed") +
  facet_grid(name~., scales = "free")




p1 = ggplot() +
  geom_histogram(data = data.frame(x = mean_resample[[1]]), aes(x, y=..density..)) +
  stat_function(data = data.frame(x = c(-50, 160)), aes(x), 
                fun = dnorm, n = 201, 
                args = list(mean = diff_mean, sd = sd_diff_mean),
                color  = "red") + ggtitle(paste(B[1],"bootstrap replicates")) +
  xlim(c(-50,150))
p2 = ggplot() +
  geom_histogram(data = data.frame(x = mean_resample[[5]]), aes(x, y=..density..)) +
  stat_function(data = data.frame(x = c(-50, 160)), aes(x), 
                fun = dnorm, n = 201, 
                args = list(mean = diff_mean, sd = sd_diff_mean),
                color  = "red")+ ggtitle(paste(B[5],"bootstrap replicates")) +
  xlim(c(-50,150))


p3 = ggplot() +
  geom_histogram(data = data.frame(x = mean_resample[[12]]), aes(x, y=..density..)) +
  stat_function(data = data.frame(x = c(-50, 160)), aes(x), 
                fun = dnorm, n = 201, 
                args = list(mean = diff_mean, sd = sd_diff_mean),
                color  = "red")+ ggtitle(paste(B[12],"bootstrap replicates")) +
  xlim(c(-50,150))


p4 = ggplot() +
  geom_histogram(data = data.frame(x = median_resample[[1]]), aes(x, y=..density..)) + 
  ggtitle(paste(B[1],"bootstrap replicates")) +
  xlim(c(-50,150))
p5 = ggplot() +
  geom_histogram(data = data.frame(x = median_resample[[5]]), aes(x, y=..density..)) +
  ggtitle(paste(B[5],"bootstrap replicates"))+
  xlim(c(-50,150))
p6 = ggplot() +
  geom_histogram(data = data.frame(x = median_resample[[12]]), aes(x, y=..density..)) +
  ggtitle(paste(B[12],"bootstrap replicates"))+
  xlim(c(-50,150))



library(patchwork)
p1 + p4 + p2 + p5 + p3  + p6 + plot_layout(ncol = 2)


CI_median_diff = c(diff_median-1.97*sd_median[length(B)],diff_median+1.97*sd_median[length(B)])




B = c(50, 100, 250, 500,1000, 2000, 4000, 6000, 8000, 10000, 12000, 15000)
sd_mean = sd_median = numeric(length(B))
mean_resample = median_resample = list()
for(b in 1:length(B))
{
  treat_star = rnorm(7*B[b], mean(treat), sd(treat)) %>% matrix(B[b],7)
  contr_star = rnorm( 9*B[b], mean(contr) , sd(contr)) %>% matrix(B[b],9)
  sd_mean[b] = sqrt(my_var(apply(treat_star,1, mean)) + 
                      my_var(apply(contr_star,1, mean)))
  
  mean_resample[[b]] = apply(treat_star,1,mean) - apply(contr_star,1,mean)
  
 
  sd_median[b] = sqrt(my_var(apply(treat_star,1, median)) + 
                        my_var(apply(contr_star,1, median)))
  median_resample[[b]] = apply(treat_star,1,median) - apply(contr_star,1,median)
  
  
  
  
}

dummy = data.frame(name =   c("sd_mean","sd_median" ), 
                   val = c(sd_diff_mean, NA))

data.frame(B= B, sd_mean = sd_mean, sd_median = sd_median) %>%
  pivot_longer(-B) %>%
  ggplot(aes(B, value)) + geom_point() + 
  geom_line() +
  geom_hline(data = dummy, aes(yintercept = val), linetype = "dashed") +
  facet_grid(name~., scales = "free")

