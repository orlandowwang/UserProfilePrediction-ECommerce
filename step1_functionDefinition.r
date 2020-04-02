######################################################################################
######函数一：按照user_id合并记录，提取user
###########参数log_data是交易的日志信息
###########注意train.user是从train.RData中提取的，train.RData是由log_train,info_train合并生成的
###########这样可以防止不对应的问题
UserExtraction <- function(log_data)
{
  log_data.user.num <- length(unique(log_data$user_id))   #求出用户数
  log_data.id <- sort(unique(log_data$user_id))           #求出排序后的user_id
  log_data.user <- list()                                 #将user信息存放至列表里
  system.time(
    for(i in 1:log_data.user.num)
    {
      log_data.user[[i]] <- subset(log_data,user_id == log_data.id[i]) 
      cat("这是第" ,i,"次","循环\n")
    }
  )
  return(log_data.user)
}
######################################################################################




######################################################################################
######函数二：提取log_data里的统计特征
######这个特征提取函数用于在训练和测试前进行数据的预处理
######该函数要在UserExtraction函数之后才可以使用
######参数Data是交易日志信息，user是提取出的用户信息

#0点击，1加购物车，2购买，3收藏
FeatureExtraction <- function(Data,user)
{
  user.num <- length(unique(Data$user_id))        #求出用户数
  feature <- matrix(nrow = user.num,ncol =1)      #初始化特征矩阵
  feature[,1] <- sort(unique(Data$user_id))       #将user_id按照顺序提出来

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
  
  #0点击，1加购物车，2购买，3收藏
  for(i in 1:user.num)
  {
    activity[i] <- dim(user[[i]])[1]                        #计算出用户的活跃次数
    activity.days[i] <- length(unique(user[[i]]$time_stamp))   #计算出活跃的天数
    buy.num[i] <- sum(user[[i]]$action_type == 2)           #购买次数
    if(buy.num[i]!=0)
    {
      co2buy.rate[i] <- sum(user[[i]]$action_type == 3)/buy.num[i]   #收藏至购买的转化率
      cart2buy.rate[i] <- sum(user[[i]]$action_type == 1)/buy.num[i] #加入购物车至购买的转化率
      buy.rate[i] <- activity[i]/buy.num[i]        #活跃至购买的转化率
    }else
    {
      co2buy.rate[i] <- 0   #收藏至购买的转化率
      cart2buy.rate[i] <- 0 #加入购物车至购买的转化率
      buy.rate[i] <- 0        #活跃至购买的转化率
    }
    
    user.sort <- user[[i]][order(user[[i]]$time_stamp),] #取出按照时间排序后的信息
   
    #0点击，1加购物车，2购买，3收藏 
    buy.time <-  user.sort[which(user.sort$action_type == 2),]$time_stamp #求出购买间隔
    buy.period[i]<- mean(diff( buy.time))
    
    collect.time <- user.sort[which(user.sort$action_type == 3),]$time_stamp #求出收藏间隔
    collect.period[i] <- mean(diff( collect.time))
    
    cart.time <- user.sort[which(user.sort$action_type == 1),]$time_stamp   #求出购物车间隔
    cart.period[i] <- mean(diff( cart.time))
    
    person.brand.num[i] <- length(unique(user.sort$brand_id))
    person.seller.num[i] <-  length(unique(user.sort$seller_id))
    person.cat.num[i] <-  length(unique(user.sort$cat_id))
    
    #0点击，1加购物车，2购买，3收藏
    collect.num[i] <- sum(user[[i]]$action_type == 3)
    cart.num[i] <- sum(user[[i]]$action_type == 1)         
    collect.days[i] <- length(unique(user[[i]][user[[i]]$action_type == 3,]$time_stamp))
    cart.days[i] <- length(unique(user[[i]][user[[i]]$action_type == 1,]$time_stamp))
    item.num[i] <- length(unique(user.sort$item_id))
    buy.days[i] <- length(unique(user[[i]][user[[i]]$action_type == 2,]$time_stamp))
    cat("这是第",i,"次循环\n")
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
######函数三：提取日志信息中的种类向量特征
######该函数参数需要提供待提取特征data的user，以及种类个数cat_num
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
    cat("这是第",i,"次循环\n")
  }
  return(cat_vec)
}
####################################################################################






####################################################################################
######函数四：提取日志信息中的卖家向量特征
######该函数参数需要提供待提取特征data的user，以及卖家个数seller_num
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
    cat("这是第",i,"次循环\n")
  }
  return(seller_vec)
}
####################################################################################






####################################################################################
######函数五：提取日志信息中的品牌向量特征
######该函数参数需要提供待提取特征data的user，以及品牌个数brand_num
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
    cat("这是第",i,"次循环\n")
  }
  return(brand_vec)
}
####################################################################################





####################################################################################
######函数六：提取日志信息中的time_stamp特征
######该函数参数需要提供待提取特征data的user，以及时间个数time_num
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
    cat("这是第",i,"次循环\n")
  }
  return(time_vec)
}
####################################################################################
