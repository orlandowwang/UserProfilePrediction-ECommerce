############blend the model###################################################################################
###############把所以训练误差跳到57%左右，加大colsample
###############bind the time feature
rm(list = ls())
gc()
memory.limit(8000000000000)
setwd("E:/阿里巴巴数据挖掘竞赛/ww")
library(xgboost)
load("train.class.RData")
###########model 1 ,cat_model#################################################################################
load("train.stat.cat.time.feature.label.RData")
train.class <- as.numeric(as.character(train.class))

ind <- list()                                      
for(i in 1:12)
{
  ind[[i]] <- which(train.class == i)
}

indlt <- sample(x = ind[[2]],size = 0.18*length(ind[[2]]))
indadd <- c( sample(x = ind[[1]],size = 0.04*length(ind[[1]])),
             sample(x = ind[[3]],size = 0.15*length(ind[[3]])),
             sample(x = ind[[4]],size = 0.55*length(ind[[4]])),
             sample(x = ind[[5]],size = 0.62*length(ind[[5]])),
             sample(x = ind[[6]],replace = TRUE,size = 3.5*length(ind[[6]])),
             sample(x = ind[[7]],size = 0.05*length(ind[[5]])),
             sample(x = ind[[8]],size = 0.05*length(ind[[8]])), 
             sample(x = ind[[9]],size = 0.75*length(ind[[9]])),
             sample(x = ind[[10]],replace = TRUE,size = 1.8*length(ind[[10]])),
             sample(x = ind[[11]],replace = TRUE,size = 1.8*length(ind[[11]])),
             sample(x = ind[[12]],replace = TRUE,size = 4.5*length(ind[[12]])))

newData <- rbind(train.data[-indlt,],train.data[indadd,])

balance.train <- as.matrix(newData[,-1875])
train.class <- as.numeric(as.character(newData[,1875])) - 1

cat.model <- xgboost(data = balance.train, label = train.class, #改成xgb.cv即转换成交叉验证形式
                     max.depth = 3, nround = 300, eta = 0.2,
                     subsample = 0.3 ,    #subsample可以适当调小一点
                     colsample_bytree = 0.65,
                     gamma = 2,
                     objective = "multi:softmax",num_class = 12,
                     verbose = 1,eval_metric ="merror",nfold =10)
#save(cat.model,file = "cat.model_all_0.0507.RData")

#预测结果
load("test2.stat.cat.time.feature.RData")
test.p1 <- predict(cat.model,test2.stat.cat.time.feature)   #注意预测要用xgboost的结果，而非xgb.cv的结果
test.p1 <- test.p1 + 1

#对结果好坏进行预估
load("train.class.RData")
train.ratio <- as.vector(table(train.class))/length(train.class)
test.ratio <- as.vector(table(test.p1))/length(test.p1)
plot(x = c(1:12),y = train.ratio,type = "l",col = "red",main = "cat.feature")
lines(x = c(1:12), y = test.ratio,type = "l")   
distance <- sum(abs(train.ratio-test.ratio))  
distance
save(test.p1,file = "test.p1_all_0.0507.RData")



##########################model 2,seller model############################################
rm(list = ls())
gc()
memory.limit(8000000000000)
setwd("E:/阿里巴巴数据挖掘竞赛/ww")
library(xgboost)

load("train.stat.seller.time.feature.label.RData")     #共5200维，第5200维是标签
load("train.class.RData")
train.class <- as.numeric(as.character(train.class))

ind <- list()                                      
for(i in 1:12)
{
  ind[[i]] <- which(train.class == i)
}

indlt <- sample(x = ind[[2]],size = 0.22*length(ind[[2]]))
indadd <- c( sample(x = ind[[1]],size = 0.05*length(ind[[1]])),
             sample(x = ind[[3]],size = 0.06*length(ind[[3]])),#0.05
             sample(x = ind[[4]],size = 0.6*length(ind[[4]])),#0.5，0.55
             sample(x = ind[[5]],size = 0.6*length(ind[[5]])),
             sample(x = ind[[6]],replace = TRUE,size = 3.5*length(ind[[6]])),
             sample(x = ind[[7]],size = 0.05*length(ind[[7]])),
             sample(x = ind[[8]],size = 0.05*length(ind[[8]])),
             sample(x = ind[[9]],size = 0.7*length(ind[[9]])),
             sample(x = ind[[10]],replace = TRUE,size = 1.7*length(ind[[10]])),#1.35,1.45
             sample(x = ind[[11]],replace = TRUE,size = 1.65*length(ind[[11]])),#1.5
             sample(x = ind[[12]],replace = TRUE,size = 4.5*length(ind[[12]])))

newData <- rbind(train.data[-indlt,],train.data[indadd,])


balance.train <- as.matrix(newData[,-5200]) #去掉标签
train.class <- as.numeric(as.character(newData[,5200])) - 1 



seller.model <- xgboost(data = balance.train, label = train.class, #改成xgb.cv即转换成交叉验证形式
                        max.depth = 2, nround = 500, eta = 0.2,
                        subsample = 0.3 ,   
                        colsample_bytree = 0.65,
                        gamma = 4,
                        objective = "multi:softmax",num_class = 12,
                        verbose = 1,eval_metric ="merror",nfold =10)  
#save(seller.model,file = "seller.model_all.RData")

#测试结果
load("test2.stat.seller.time.feature.RData")

test.p2 <- predict(seller.model,test2.stat.seller.time.feature) #注意预测要用xgboost的结果，而非xgb.cv的结果
test.p2 <- test.p2 + 1
save(test.p2,file = "test.p2_all0.0513.RData")

#对结果好坏进行预估
load("train.class.RData")
train.ratio <- as.vector(table(train.class))/length(train.class)
test.ratio <- as.vector(table(test.p2))/length(test.p2)
plot(x = c(1:12),y = train.ratio,type = "l",col = "red",main = "seller.feature")
lines(x = c(1:12), y = test.ratio,type = "l")
distance <- sum(abs(train.ratio-test.ratio))   #0.039
distance




################# model 3 brand model ###############################################
rm(list = ls())
gc()
memory.limit(8000000000000)
setwd("E:/阿里巴巴数据挖掘竞赛/ww")
library(xgboost)
load("train.stat.brand.time.feature.label.RData")   #load进来的是sts和cat包含label的矩阵，名称为train.data
load("train.class.RData")
train.class <- as.numeric(as.character(train.class))

ind <- list()                                      
for(i in 1:12)
{
  ind[[i]] <- which(train.class == i)
}


#前18维是统计特征，后8681维是seller特征，最后一维是label，共8692维
indlt <- sample(x = ind[[2]],size = 0.215*length(ind[[2]]))
indadd <- c( sample(x = ind[[1]],size = 0.048*length(ind[[1]])),
             sample(x = ind[[3]],size = 0.085*length(ind[[3]])),#0.05,0.06
             sample(x = ind[[4]],size = 0.7*length(ind[[4]])),#0.4,0.6
             sample(x = ind[[5]],size = 0.65*length(ind[[5]])),#0.6
             sample(x = ind[[6]],replace = TRUE,size = 3.5*length(ind[[6]])),
             sample(x = ind[[7]],size = 0.063*length(ind[[7]])),#0.05,0.055
             sample(x = ind[[8]],size = 0.053*length(ind[[8]])),#0.05
             sample(x = ind[[9]],size = 0.75*length(ind[[9]])),#0.7
             sample(x = ind[[10]],replace = TRUE,size = 1.94*length(ind[[10]])),#1.3,1.65,1.7
             sample(x = ind[[11]],replace = TRUE,size = 1.85*length(ind[[11]])),#1.5,1.55
             sample(x = ind[[12]],replace = TRUE,size = 5.3*length(ind[[12]])))#4.5,5
newData <- rbind(train.data[-indlt,],train.data[indadd,])


balance.train <- as.matrix(newData[,-8682])

train.class <- as.numeric(as.character(newData[,8682])) - 1



brand.model <- xgboost(data = balance.train, label = train.class, #改成xgb.cv即转换成交叉验证形式
                       max.depth = 3, nround = 1111, eta = 0.07,
                       subsample = 0.5 ,    
                       colsample_bytree = 0.15,
                       gamma = 6,
                       max_delta_step = 1,
                       objective = "multi:softmax",num_class = 12,
                       verbose = 1,eval_metric ="merror",nfold =10)  
#save(brand.model,file = "brand.model_all_0.0618.RData")

#测试结果
load("test2.stat.brand.time.feature.RData")
#前18维是统计特征，共8691维
test.p3 <- predict(brand.model,test2.stat.brand.time.feature)   #注意预测要用xgboost的结果，而非xgb.cv的结果
test.p3 <- test.p3 + 1
save(test.p3,file = "test.p3_all_0.0618.RData")
#对结果好坏进行预估
load("train.class.RData")
train.ratio <- as.vector(table(train.class))/length(train.class)
test.ratio <- as.vector(table(test.p3))/length(test.p3)
plot(x = c(1:12),y = train.ratio,type = "l",col = "red",main = "brand.feature")
lines(x = c(1:12), y = test.ratio,type = "l")
distance <- sum(abs(train.ratio-test.ratio)) 
distance




##########################混合三个模型，投票#############################################3
rm(list = ls())
gc()
load("test.p1_all_0.0507.RData")
load("test.p2_all_0.0513.RData")
load("test.p3_all_0.0618.RData")

a = cbind(test.p1,test.p2,test.p3)
head(a)
test.p = 0
for(i in 1:37966) {
  test.p[i] = as.numeric(names(sort(table(a[i,]),decreasing = T))[1])
}


############output the result
load("test2_id.RData")
id <- as.numeric(as.character(test2.stat.feature$user_id))
test.p <- as.numeric(as.character(test.p))
result <- data.frame(id,test.p)
rownames(result) <- NULL
colnames(result) <-NULL
write.csv(result,file = "result_finalfinal.csv",row.names = FALSE)


#load("train.class.RData")
#a = table(train.class)
#barplot(a, main = "12类人的分布",ylim = c(0,30000) )
