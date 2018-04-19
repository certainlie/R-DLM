library(ggplot2)
library(zoo)
library(xts)
library(xlsxjars)
library(rJava)
library(doParallel)
library(TSPred)

detectCores()
registerDoParallel(cores=3)

basepath = "C:\\Users\\srawat\\Documents\\Milword Brown Analytics Training\\MB Shared Drive\\New Files\\Modeling\\"
data=read.xlsx(paste0(basepath,"model1-subset.xlsx"),"1")

# scaling
colnames(data)=c('Date','Sales','TV_worb','TV_cartoon','TV_flanker','FB_likes','Twitter','Youtube')
logsales=log(data$Sales)
logfb=log(data$FB_likes)
logtwitter=log(data$Twitter)
logyoutube=log(data$Youtube)
df=cbind('Date'=as.Date(data$Date),logsales,'TV_worb'=data$TV_worb,'TV_cartoon'=data$TV_cartoon,'TV_flanker'=data$TV_flanker,logfb,logtwitter,logyoutube)
df=data.frame(df)
df$Date=as.Date(df$Date)
head(df)

# remove infinite values
df2=df[,names(df)!='Date']
df2=do.call(data.frame,lapply(df2,function(x) replace(x,is.infinite(x),0)))
df2[is.na(df2)]=0 # replace null values with 0
df2['Date']=df['Date']
df2=df2[,c(8,1:7)]
df=df2
rm(df2)


# Helper Functions
':=' = function(lhs, rhs) {
  frame = parent.frame()
  lhs = as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs = lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs = list(rhs)
  if (length(lhs) > length(rhs))
    rhs = c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}

# Split function to split data set into training and test sub sets:
f.split = function(data, probtrain, probtest) {
  # label training and test data sets
  index = sample(x=c(0,1), size=nrow(data), prob=c(probtrain,probtest), replace=TRUE)
  # split data set into training and test
  train = data[index==0,]
  test = data[index==1,]  
  return(list(train, test))
}

## GLM
model=glm(logsales~.,data=df[,names(df)!='Date'])
summary(model)

# validate
set.seed(1234)
r=vector()
repeats=100
result=foreach(icount(repeats),.combine=rbind) %dopar% {
  c(df.train,df.test):= f.split(df,probtrain=0.8,probtest=0.2)
  df.train=data.frame(df.train)
  df.test=data.frame(df.test)
  df.train=df.train[-1]
  df.test=df.test[-1]
  model=glm(logsales~.,data=df.train)
  train.R2=1-(sum((df.train$logsales-predict(model))^2)/sum((df.train$logsales-mean(df.train$logsales))^2))
  test.R2=1-(sum((df.test$logsales-predict(model,newdata=df.test))^2)/sum((df.test$logsales-mean(df.test$logsales))^2))
  n=length(df.test$logsales)
  return(c(model$coefficients,
           train.R2=train.R2,
           test.R2=test.R2,
           AIC=AIC(model),
           BIC=BIC(model),
           RMSE=sqrt(mean((df.test$logsales-predict(model,newdata=df.test))^2)),
           MAPEE=(100/n) * sum(abs((exp(df.test$logsales) - exp(predict(model,newdata=df.test)))/exp(df.test$logsales)))))
  }

result=data.frame(result)

# Parallel to Sequential to free up the Cores
registerDoSEQ()
# check
getDoParWorkers()


## Plot coefficients from TV
par(mfrow=c(3,1))
plot(type='l',result$TV_worb,main='TV_worb')
plot(type='l',result$TV_cartoon,main='TV_cartoon')
plot(type='l',result$TV_flanker,main='TV_flanker')


# forecast vs sales
c(train,test):=f.split(df,probtrain=0.7,probtest=0.3)
train=data.frame(train)
test=data.frame(test)
model=glm(logsales~.,data=train[,names(train)!='Date'])
summary(model)
par(mfrow=c(2,1))
plot(type='l',test$logsales)
plot(type='l',predict(model,newdata=test))











































