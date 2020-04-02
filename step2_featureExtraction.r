#############�ó������������ȡ����
rm(list = ls())
gc()
setwd("E:/����Ͱ������ھ���/ww")
memory.limit(8000000000000)
#############��ȡѵ������ͳ������
load("train.user.RData")          #train.user�а�����ѵ������user��Ϣ
load("train.RData")               #train��log_train��info_train����merge������Ľ��
train.user = user3
############train.stat.feature�е�user_id��int���ͣ�������info_train�е�user_class�ϲ�
train.stat.feature <- FeatureExtraction(train,train.user)       #���ѵ��������
load("info_train.RData")
train.stat.feature <- merge(x = train.stat.feature,y = info_train,by.x = "user_id",by.y = "user_id")
train.stat.feature$user_class <- factor(train.stat.feature$user_class,levels = c(1:12))
save(train.stat.feature,file = "train.stat.feature.RData") 
load("train.stat.feature.RData")
train.class <- train.stat.feature[,20]          ######��ð�user_id��С�����ѵ�����ı�ǩ
save(train.class,file = "train.class.RData")
############��ȡ���Լ���ͳ������
load("log_test2.RData")
#######1111111111111111111111111111111111111111111111111
load("log_test2.user.RData")
test2.stat.feature <- FeatureExtraction(log_test2,log_test2.user)
save(test2.stat.feature,file = "test2.stat.feature.RData")


############��ȡѵ���������Լ������������Ʒ����������������
############���������Լ�ʱ�����κ���Ҫ�������У���������Ҫ���¼���
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
