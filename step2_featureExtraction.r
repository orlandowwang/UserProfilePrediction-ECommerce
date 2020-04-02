#############该程序完成特征提取工作
rm(list = ls())
gc()
setwd("E:/阿里巴巴数据挖掘竞赛/ww")
memory.limit(8000000000000)
#############提取训练集的统计特征
load("train.user.RData")          #train.user中包含这训练集的user信息
load("train.RData")               #train是log_train和info_train进行merge操作后的结果
train.user = user3
############train.stat.feature中的user_id是int类型，方便与info_train中的user_class合并
train.stat.feature <- FeatureExtraction(train,train.user)       #求得训练集特征
load("info_train.RData")
train.stat.feature <- merge(x = train.stat.feature,y = info_train,by.x = "user_id",by.y = "user_id")
train.stat.feature$user_class <- factor(train.stat.feature$user_class,levels = c(1:12))
save(train.stat.feature,file = "train.stat.feature.RData") 
load("train.stat.feature.RData")
train.class <- train.stat.feature[,20]          ######求得按user_id大小排序的训练集的标签
save(train.class,file = "train.class.RData")
############提取测试集的统计特征
load("log_test2.RData")
#######1111111111111111111111111111111111111111111111111
load("log_test2.user.RData")
test2.stat.feature <- FeatureExtraction(log_test2,log_test2.user)
save(test2.stat.feature,file = "test2.stat.feature.RData")


############提取训练集、测试集的类别特征、品牌特征、卖家特征
############当更换测试集时，本段函数要重新运行，各种数量要重新计算
cat.num <- max(unique(c(train$cat_id,log_test2$cat_id))) #1670
train.cat.feature <- CatVecExtraction(train.user,cat.num)
save(train.cat.feature,file = "train.cat.feature.RData")
test2.cat.feature <- CatVecExtraction(log_test2.user,cat.num)
save(test2.cat.feature,file = "test2.cat.feature.RData")

seller.num <- max(unique(c(train$seller_id,log_test2$seller_id)))#4995
train.seller.feature <- SellerVecExtraction(train.user,seller.num)
save(train.seller.feature,file = "train.seller.feature.RData")
test2.seller.feature <- SellerVecExtraction(log_test2.user,seller.num)
save(test2.seller.feature,file = "test2.seller.feature.RData")

brand.num <- max(unique(c(train$brand_id,log_test2$brand_id))) #8477
train.brand.feature <- BrandVecExtraction(train.user,brand.num)
save(train.brand.feature,file = "train.brand.feature.RData")
test2.brand.feature <- BrandVecExtraction(log_test2.user,brand.num)
save(test2.brand.feature,file = "test2.brand.feature.RData")

time.num <- max(unique(c(train$time_stamp,log_test2$time_stamp))) #186
train.time.feature <- TimeVecExtraction(train.user,time.num)
save(train.time.feature,file = "train.time.feature.RData")
test2.time.feature <- TimeVecExtraction(log_test2.user,time.num)
save(test2.time.feature,file = "test2.time.feature.RData")
