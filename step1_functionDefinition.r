######################################################################################
######����һ������user_id�ϲ���¼����ȡuser
###########����log_data�ǽ��׵���־��Ϣ
###########ע��train.user�Ǵ�train.RData����ȡ�ģ�train.RData����log_train,info_train�ϲ����ɵ�
###########�������Է�ֹ����Ӧ������
UserExtraction <- function(log_data)
{
  log_data.user.num <- length(unique(log_data$user_id))   #����û���
  log_data.id <- sort(unique(log_data$user_id))           #���������user_id
  log_data.user <- list()                                 #��user��Ϣ������б���
  system.time(
    for(i in 1:log_data.user.num)
    {
      log_data.user[[i]] <- subset(log_data,user_id == log_data.id[i]) 
      cat("���ǵ�" ,i,"��","ѭ��\n")
    }
  )
  return(log_data.user)
}
######################################################################################




######################################################################################
######����������ȡlog_data���ͳ������
######���������ȡ����������ѵ���Ͳ���ǰ�������ݵ�Ԥ����
######�ú���Ҫ��UserExtraction����֮��ſ���ʹ��
######����Data�ǽ�����־��Ϣ��user����ȡ�����û���Ϣ

#0�����1�ӹ��ﳵ��2����3�ղ�
FeatureExtraction <- function(Data,user)
{
  user.num <- length(unique(Data$user_id))        #����û���
  feature <- matrix(nrow = user.num,ncol =1)      #��ʼ����������
  feature[,1] <- sort(unique(Data$user_id))       #��user_id����˳�������

  activity <- 0
  activity.days <-0
  buy.num <-0
  co2buy.rate <- 0
  cart2buy.rate <- 0
  buy.rate <- 0
  
  buy.period <- 0
  collect.period <- 0
  cart.period <- 0
  
  person.brand.num <- 0
  person.seller.num <- 0
  person.cat.num <- 0
  
  collect.num <- 0
  cart.num <- 0         
  collect.days <- 0
  cart.days <- 0
  item.num <- 0
  buy.days <- 0 
  
  #0�����1�ӹ��ﳵ��2����3�ղ�
  for(i in 1:user.num)
  {
    activity[i] <- dim(user[[i]])[1]                        #������û��Ļ�Ծ����
    activity.days[i] <- length(unique(user[[i]]$time_stamp))   #�������Ծ������
    buy.num[i] <- sum(user[[i]]$action_type == 2)           #�������
    if(buy.num[i]!=0)
    {
      co2buy.rate[i] <- sum(user[[i]]$action_type == 3)/buy.num[i]   #�ղ��������ת����
      cart2buy.rate[i] <- sum(user[[i]]$action_type == 1)/buy.num[i] #���빺�ﳵ�������ת����
      buy.rate[i] <- activity[i]/buy.num[i]        #��Ծ�������ת����
    }else
    {
      co2buy.rate[i] <- 0   #�ղ��������ת����
      cart2buy.rate[i] <- 0 #���빺�ﳵ�������ת����
      buy.rate[i] <- 0        #��Ծ�������ת����
    }
    
    user.sort <- user[[i]][order(user[[i]]$time_stamp),] #ȡ������ʱ����������Ϣ
   
    #0�����1�ӹ��ﳵ��2����3�ղ� 
    buy.time <-  user.sort[which(user.sort$action_type == 2),]$time_stamp #���������
    buy.period[i]<- mean(diff( buy.time))
    
    collect.time <- user.sort[which(user.sort$action_type == 3),]$time_stamp #����ղؼ��
    collect.period[i] <- mean(diff( collect.time))
    
    cart.time <- user.sort[which(user.sort$action_type == 1),]$time_stamp   #������ﳵ���
    cart.period[i] <- mean(diff( cart.time))
    
    person.brand.num[i] <- length(unique(user.sort$brand_id))
    person.seller.num[i] <-  length(unique(user.sort$seller_id))
    person.cat.num[i] <-  length(unique(user.sort$cat_id))
    
    #0�����1�ӹ��ﳵ��2����3�ղ�
    collect.num[i] <- sum(user[[i]]$action_type == 3)
    cart.num[i] <- sum(user[[i]]$action_type == 1)         
    collect.days[i] <- length(unique(user[[i]][user[[i]]$action_type == 3,]$time_stamp))
    cart.days[i] <- length(unique(user[[i]][user[[i]]$action_type == 1,]$time_stamp))
    item.num[i] <- length(unique(user.sort$item_id))
    buy.days[i] <- length(unique(user[[i]][user[[i]]$action_type == 2,]$time_stamp))
    cat("���ǵ�",i,"��ѭ��\n")
  }
  
  buy.period[which(is.na(buy.period))] <- 200
  collect.period[which(is.na(collect.period))] <- 200
  cart.period[which(is.na(cart.period))] <- 200
  
  feature <-data.frame(feature,activity,activity.days,
                       buy.num,co2buy.rate,cart2buy.rate,buy.rate,
                       buy.period,collect.period,cart.period,
                       person.brand.num,person.seller.num,person.cat.num,
                       collect.num,
                       cart.num,         
                       collect.days,
                       cart.days,
                       item.num,
                       buy.days)

  names(feature) <-c("user_id","activity","activity.days",
                     "buy.num","co2buy.rate","cart2buy.rate","buy.rate",
                     "buy.period","collect.period","cart.period",
                     "person.brand.num","person.seller.num","person.cat.num",
                     "collect.num",
                     "cart.num",         
                     "collect.days",
                     "cart.days",
                     "item.num",
                     "buy.days")                    

  
  return(feature)
  
}
####################################################################################







####################################################################################
######����������ȡ��־��Ϣ�е�������������
######�ú���������Ҫ�ṩ����ȡ����data��user���Լ��������cat_num
######cat.num <- max(unique(c(train$cat_id,data$cat_id)))

CatVecExtraction <- function(user,cat.num)
{
  user.num <- length(user)
  cat_vec <- matrix(data = 0,ncol = cat.num,nrow = user.num)
  for(i in 1:user.num)
  {
    cat.t <- table(user[[i]]$cat_id)
    idx <- as.numeric(names(cat.t))
    cat.num <- as.vector(cat.t)
    cat_vec[i,idx] <- cat.num
    cat("���ǵ�",i,"��ѭ��\n")
  }
  return(cat_vec)
}
####################################################################################






####################################################################################
######�����ģ���ȡ��־��Ϣ�е�������������
######�ú���������Ҫ�ṩ����ȡ����data��user���Լ����Ҹ���seller_num
######seller.num <- max(unique(c(train$seller_id,data$seller_id)))

SellerVecExtraction <- function(user,seller.num)
{
  user.num <- length(user)
  seller_vec <- matrix(data = 0,ncol = seller.num,nrow = user.num)
  for(i in 1:user.num)
  {
    seller.t <- table(user[[i]]$seller_id)
    idx <- as.numeric(names(seller.t))
    seller.num <- as.vector(seller.t)
    seller_vec[i,idx] <- seller.num
    cat("���ǵ�",i,"��ѭ��\n")
  }
  return(seller_vec)
}
####################################################################################






####################################################################################
######�����壺��ȡ��־��Ϣ�е�Ʒ����������
######�ú���������Ҫ�ṩ����ȡ����data��user���Լ�Ʒ�Ƹ���brand_num
######brand.num <- max(unique(c(train$brand_id,data$brand_id)))

BrandVecExtraction <- function(user,brand.num)
{
  user.num <- length(user)
  brand_vec <- matrix(data = 0,ncol = brand.num,nrow = user.num)
  for(i in 1:user.num)
  {
    brand.t <- table(user[[i]]$brand_id)
    idx <- as.numeric(names(brand.t))
    brand.num <- as.vector(brand.t)
    brand_vec[i,idx] <- brand.num
    cat("���ǵ�",i,"��ѭ��\n")
  }
  return(brand_vec)
}
####################################################################################





####################################################################################
######����������ȡ��־��Ϣ�е�time_stamp����
######�ú���������Ҫ�ṩ����ȡ����data��user���Լ�ʱ�����time_num
######time.num <- max(unique(c(train$time_stamp,data$time_stamp)))

TimeVecExtraction <- function(user,time.num)
{
  user.num <- length(user)
  time_vec <- matrix(data = 0,ncol = time.num,nrow = user.num)
  for(i in 1:user.num)
  {
    time.t <- table(user[[i]]$time_stamp)
    idx <- as.numeric(names(time.t))
    time.num <- as.vector(time.t)
    time_vec[i,idx] <- time.num
    cat("���ǵ�",i,"��ѭ��\n")
  }
  return(time_vec)
}
####################################################################################
