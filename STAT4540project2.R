setwd("~/Downloads")
pendigits <- read.csv("pendigits_subset.csv")

#Question 1 
#i.
x <- as.matrix(pendigits[ , 1:16])
dim(x)
#ii.
pcout <- prcomp(x, scale=TRUE)
names(pcout)
#iii. 
pcout$center
pcout$scale
#iv.
pcout$rotation
#vii.
pcout$sdev
pcout$x
#viii.
pcvar <- pcout$sdev^2
pve <- pcvar/sum(pcvar)
pve

#Question 2
#i.
hc.comp <- hclust(dist(x), method="complete")
hc.avg <- hclust(dist(x), method ="average")
hc.sgl <- hclust(dist(x), method ="single")
#ii.
clust.comp = cutree(hc.comp, 2)
clust.avg = cutree(hc.avg, 2)
clust.sgl = cutree(hc.sgl, 2)
#iii.
comp.summ = cbind(comp = clust.comp, true = pendigits$class)
avg.summ = cbind(avg = clust.avg, true = pendigits$class)
sgl.summ = cbind(sgl = clust.sgl, true = pendigits$class)

comp.summ.1 = comp.summ[comp.summ[ , 1] ==  1, ]
comp.summ.2 = comp.summ[comp.summ[ , 1] ==  2, ]
table(comp.summ.1[ , 2])
253/(131 +253 +364 +332)
#result [1] 0.2342593
table(comp.summ.2[ , 2])
111/(232 +111 +4)
#result [1] 0.3198847

avg.summ.1 = avg.summ[avg.summ[ , 1] ==  1, ]
avg.summ.2 = avg.summ[avg.summ[ , 1] ==  2, ]
table(avg.summ.1[ , 2])
364/(364+364+336)
#result [1] 0.3421053
table(avg.summ.2[ , 2])
0/363

sgl.summ.1 = sgl.summ[sgl.summ[ , 1] ==  1, ]
sgl.summ.2 = sgl.summ[sgl.summ[ , 1] ==  2, ]
table(sgl.summ.1[ , 2])
364/(363 +364 +364 +335 )
#result [1] 0.2552595
table(sgl.summ.2[ , 2])

#iv.ordered low to high 

#v.
hc.comp2 <- hclust(dist(pcout$x[ , 1:2]), method="complete")
hc.avg2 <- hclust(dist(pcout$x[ , 1:2]), method ="average")
hc.sgl2 <- hclust(dist(pcout$x[ , 1:2]), method ="single")
clust.comp2 = cutree(hc.comp2, 2)
clust.avg2 = cutree(hc.avg2, 2)
clust.sgl2 = cutree(hc.sgl2, 2)
comp.summ2 = cbind(comp = clust.comp2, true = pendigits$class)
avg.summ2 = cbind(avg = clust.avg2, true = pendigits$class)
sgl.summ2 = cbind(sgl = clust.sgl2, true = pendigits$class)
comp.summ.1.2 = comp.summ2[comp.summ2[ , 1] ==  1, ]
table(comp.summ.1.2[ , 2])
228/(228+849)
#result [1] 0.2116992
comp.summ.2.2 = comp.summ2[comp.summ2[ , 1] ==  2, ]
table(comp.summ.2.2[ , 2])
136/(136+214)
#result [1] 0.3885714
avg.summ.1.2 = avg.summ2[avg.summ2[ , 1] ==  1, ]
avg.summ.2.2 = avg.summ2[avg.summ2[ , 1] ==  2, ]
table(avg.summ.1.2[ , 2])
364/(364+712)
#result [1] 0.33829
table(avg.summ.2.2[ , 2])
0/351
sgl.summ.1.2 = sgl.summ2[sgl.summ2[ , 1] ==  1, ]
sgl.summ.2.2 = sgl.summ2[sgl.summ2[ , 1] ==  2, ]
table(sgl.summ.1.2[ , 2])
table(sgl.summ.2.2[ , 2])


#Question 3
#i. described optimization problem
#ii.
km.out <- kmeans (x, 2, nstart =10)
km.out
km <- km.out$cluster
#iii.
km.summ = cbind(comp = km, true = pendigits$class)
km.summ.1 = km.summ[km.summ[ , 1] ==  1, ]
km.summ.2 = km.summ[km.summ[ , 1] ==  2, ]
table(km.summ.1[ , 2])
0/340
#[1] 0
table(km.summ.2[ , 2])
364/(23+ 364+ 364+ 336)
#[1] 0.3348666

#iv.
km2 <- pcout$x[ , 1:2]
km.summ.2 = cbind(comp = km2, true = pendigits$class)
km.summ.1.2 = km.summ.2[km.summ.2[ , 1] ==  1, ]
km.summ.2.2 = km.summ.2[km.summ.2[ , 1] ==  2, ]
table(km.summ.1[ , 2])
table(km.summ.2[ , 2])


#Question 4 
pendigits$class = as.numeric(pendigits$class == 1)
set.seed(1)
#training data
idx <- sample(1:nrow(pendigits), 0.80 * nrow(pendigits))
#test data
idx0 <- setdiff(1:nrow(pendigits), idx)
#
train = pendigits[idx, ]
test = pendigits[idx0, ]
#i. ridge classification 
train.mat <- model.matrix(class~ ., data=train)
test.mat <- model.matrix(class~ ., data=test)
library(glmnet)
grid <- 10^seq(4, -2, length=100)
fit.ridge <- glmnet(train.mat, train$class, alpha=0, lambda=grid, family = "binomial")
cv.ridge = cv.glmnet(train.mat, train$class, alpha=0, lambda=grid, family = "binomial")
bestlam.ridge = cv.ridge$lambda.min
bestlam.ridge
pred.ridge <- predict(fit.ridge, s=bestlam.ridge, newx=test.mat, type = "response")
pred.yhat = as.numeric(pred.ridge > 0.5)
table(test$class, pred.yhat)


#ii. lasso classification 
fit.lasso <- glmnet(train.mat, train$class, alpha=1, lambda=grid, thresh=1e-12, family = "binomial")
cv.lasso <- cv.glmnet(train.mat, train$class, alpha=1, lambda=grid, thresh=1e-12, family = "binomial")
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso
#[1] 0.01
pred.lasso <- predict(fit.lasso, s=bestlam.lasso, newx=test.mat, type = "response")
pred.yhat = as.numeric(pred.lasso > 0.5)
table(test$class, pred.yhat)


