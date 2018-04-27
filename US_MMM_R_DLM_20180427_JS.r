library(sas7bdat)
library(rJava)
library(xlsxjars)
library(dlm)
library(ggplot2)
library(scales)
library(zoo)
library(xts)
library(lubridate)
library(xlsx)
library(dplyr)
library(reshape)
library(data.table)
library(doSNOW)

basepath = "C:/Users/jsohn/Documents/GitRepo/R-DLM/"
data=read.xlsx(paste0(basepath, 'model2-data.xlsx'), 'orig_vars2', startRow=4)
data_orig <- data
data <- data_orig
colnames(data)=c('date', 'Sales', 'Price', 'MonsterLaunch', 'Promo_Display', 'Promo_Feature_Monster', 'ACV', 'TPR', 
                 'TV_Total', 'WebTV_Total', 'Display_Total','fb_posts','twitter_int','youtube_views', 'DotCom'
                 , 'Sampling', 'PaidSocial_Total', 'Search_total', 'RBLaunch', 'CSI_Monster', 'Unemployment', 'seas', 'Intercept')
cnames <- colnames(data)

data <- data[4:nrow(data),]

# remove Youtube
# data <- data %>% dplyr::select(-youtube_views)



## Only log of sales with actual and mean of sales
# data=read.xlsx('C:\\Users\\srawat\\Documents\\Milword Brown Analytics Training\\MB Shared Drive\\New Files\\Modeling\\model1-subset.xlsx','2')
# colnames(data)=c('date','Sales','Sales_dep','TV_worb','TV_cartoon','TV_flanker','FB_likes','Twitter','Youtube','WTD','Monster','Price')
# # data['actual_sales']=data$Sales
# #data$Sales=log(data$Sales)
# data['mean_sales']=rep(mean(data$Sales),nrow(data))
# # data$Quarter=NULL
# data[is.na(data)]=0

salesdata <- as.data.frame(data$Sales)
# taking the index of all the columns
# data <- data %>% select(-date) %>% lapply()
data['mean_sales']=rep(mean(data$Sales),nrow(data))
data$Sales_dep <- data$Sales/data$mean_sales
data$Price=data$Price/mean(data$Price)
data$MonsterLaunch=data$MonsterLaunch/mean(data$MonsterLaunch)
data$Promo_Display=data$Promo_Display/mean(data$Promo_Display)
data$Promo_Feature_Monster=data$Promo_Feature_Monster/mean(data$Promo_Feature_Monster)
data$ACV=data$ACV/mean(data$ACV)
data$TPR=data$TPR/mean(data$TPR)
data$TV_Total=data$TV_Total/mean(data$TV_Total)
data$WebTV_Total=data$WebTV_Total/mean(data$WebTV_Total)
data$Display_Total=data$Display_Total/mean(data$Display_Total)
data$fb_posts=data$fb_posts/mean(data$fb_posts)
data$twitter_int=data$twitter_int/mean(data$twitter_int)
data$youtube_views=data$youtube_views/mean(data$youtube_views)
data$DotCom=data$DotCom/mean(data$DotCom)
data$PaidSocial_Total=data$PaidSocial_Total/mean(data$PaidSocial_Total)
data$Search_total=data$Search_total/mean(data$Search_total)
data$RBLaunch=data$RBLaunch/mean(data$RBLaunch)
data$CSI_Monster=data$CSI_Monster/mean(data$CSI_Monster)
data$Unemployment=data$Unemployment/mean(data$Unemployment)

spec=rep("localhost",3)
date.var="date" 
start.date.m="2014-01-26"
end.date.m="2017-12-31" 
start.date.c="2014-01-26"
end.date.c="2017-12-31" 
is.output=F
is.graph=T
varlist=c('Price', 'MonsterLaunch', 'Promo_Display', 'Promo_Feature_Monster', 'ACV', 'TPR', 
          'TV_Total', 'WebTV_Total', 'Display_Total','fb_posts','twitter_int','youtube_views', 'DotCom'
          , 'Sampling', 'PaidSocial_Total', 'Search_total', 'RBLaunch', 'CSI_Monster', 'Unemployment', 'seas', 'Intercept')
# varlist = colnames(data)[-c(1,2)]

# Put Product launch dummy variables when applicable.
# dum=c()
# dum_matrix=NA
dum=c('Seas')
dum_matrix=cbind(c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),rep(1,28))

num_ng_coef=2 # no. of negative coef variables
dep='Sales_dep' # dependent sales
actual="Sales" # actual sales
mean="mean_sales"# mean sales

# Parameters
CI.level=0.95 # confidence interval
iter=10 
level_type=1 # For Order -- dlmModPoly
var_coef="est" 
var_level='est'
var_slope='est'
per_sea=52  # for dlmModTrig
ord_sea=1 # for dlmModTrig
var_sea='est'
optim.method="L-BFGS-B" 
optim.lb=-Inf 
optim.ub=Inf 

data[is.na(data)]=0 # NAs to 0
# setnames(data,date.var,"date")
data=data[data$date>=start.date.m&data$date<=end.date.m,]
# Vectors for Data
data.dma=vector('list',1)
y=vector('list',1)
ymean=vector("list",1)
yreal=vector("list",1)
x=vector('list',1)

data.dma[[1]]=data
y[[1]]=data.dma[[1]][,dep]
ymean[[1]]=data.dma[[1]][,mean]
yreal[[1]]=data.dma[[1]][,actual]
if(length(varlist)!=0) x[[1]]=data.frame(data.table(data.dma[[1]])[,varlist,with=F])
date=data.dma[[1]]$date
coef.check.pre=matrix(1,nc=1,nr=length(varlist),dimnames=list(varlist,1))

# Trend and Seasonality: dlmModPoly (Trend) and dlmModTrig(Seasonality)

p=NA
if (level_type==2){
  if ((var_level=="est") & (var_slope!="est")){
    poly=dlmModPoly(level_type,dV=p,dW=c(exp(p),var_slope))
    no.par=2
  }else if ((var_level!="est") & (var_slope=="est")){
    poly=dlmModPoly(level_type,dV=p,dW=c(var_level,exp(p)))
    no.par=2
  }else if (var_level=="est" & var_slope=="est"){
    poly=dlmModPoly(level_type,dV=p,dW=c(exp(p),exp(p)))
    no.par=3
  }else {
    poly=dlmModPoly(level_type,dV=p,dW=c(var_level,var_slope))
    no.par=1
  }
  
}else if (level_type==1){
  if (var_level=="est"){
    poly=dlmModPoly(level_type,dV=p,dW=c(exp(p)))
    no.par=2
  }else{
    poly=dlmModPoly(level_type,dV=p,dW=c(var_level))
    no.par=1
  }
  
}

if (var_sea=="est"){
  sea=dlmModTrig(s=per_sea,q=ord_sea,dV=0,dW=p)
  no.par=no.par+1
}else{
  sea=dlmModTrig(s=per_sea,q=ord_sea,dV=0,dW=var_sea)
}
poly1=poly
sea1=sea

if(length(varlist)==0) iter=1 # varlist check

# DLM
for (loop in 1:iter){
  print(loop)
  if(length(varlist)!=0) coef.table=matrix(0,nr=length(varlist),nc=1,dimnames=list(varlist,1))  
  
  comb <- function(x, ...) {
    lapply(seq_along(x),
           function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
  }
  cl=makeCluster(spec,type="SOCK",outfile="")
  registerDoSNOW(cl)
  # cl=makeCluster(3)
  # registerDoParallel(core=cl)
  # #registerDoSEQ()
  result=foreach(i = 1,.combine='comb', .multicombine=T,.packages=c("dlm"),
                 .init=list(list(),list(),list(),list(),list(),list(),list())) %dopar% { 
                   poly=poly1
                   
                   sea=sea1
                   build=function(p){
                     
                     poly$V[is.na(poly$V)]=exp(p[1])
                     n=1
                     no=sum(is.na(poly$W))
                     if(no!=0){
                       poly$W[is.na(poly$W)]=exp(p[(n+1):(n+no)])
                       n=n+no
                     }
                     
                     if(var_sea=="est") {
                       sea$W[is.na(sea$W)]=exp(p[n+1])
                       n=n+1
                     }
                     if (length(varlist) !=0){
                       if(var_coef=="est"){
                         reg=dlmModReg(x[[i]],addInt=F,dV=exp(p[1+n]),dW = exp( p[ 2+n : (length( x[[i]] ) + n + 1) ] ))
                       }else{
                         reg=dlmModReg(x[[i]],addInt=F,dV=0,dW = rep(var_coef, ncol(x[[i]])))
                       }
                       mod=reg+poly+sea
                     }else{
                       mod=poly+sea
                     }
                     return(mod)
                     
                   }
                   
                   mle=dlmMLE(y[[i]],rep(0,(length(x[[i]])+no.par+1)),build,method=optim.method,lower=optim.lb,upper=optim.ub)
                   mod=as.dlm(build(mle$par))
                   f=dlmFilter(y[[i]],mod)
                   s=dlmSmooth(f)
                   cov=dlmSvd2var(s$U.S,s$D.S)[[1]]
                   if(length(varlist)!=0){
                     coef=t(tail(dropFirst(s$s[,1:sum(coef.check.pre[,i])]),n=1))[,1]
                     coef.table[coef.check.pre[,i]==1,i]=coef
                     coef.table1=coef.table[,i]
                   }
                   
                   if(length(varlist)!=0) list(mle,mod,f,s,cov,coef,coef.table1) else list(mle,mod,f,s,cov,NULL,NULL)
                 }
  mle=result[[1]]
  mod=result[[2]]
  f=result[[3]]
  s=result[[4]]
  cov=result[[5]]
  if(length(varlist)!=0) {
    coef=result[[6]]
    coef.table=do.call(cbind,result[[7]])
  }
  stopCluster(cl)
  
  
  # Coefficient Sign Check
  # if(length(varlist)!=0){
  #   coef.check=matrix(0,nr=nrow(coef.table),nc=ncol(coef.table),dimnames=list(varlist,1))
  #   for (i in 1:nrow(coef.table)){
  #     if (i <=num_ng_coef){
  #       coef.check[i,coef.table[i,]<0]=1
  #     }else{
  #       coef.check[i,coef.table[i,]>0]=1
  #     }
  #   }
  #   if (length(dum)!=0){
  #     for (j in 1:length(dum)){
  #       coef.check[dum[j],]=dum_matrix[,j]
  #     }
  #   }
  #   
  #   coef.change=coef.check-coef.check.pre
  #   if (sum(coef.change)==0) {
  #     print("Loop Complete.") 
  #     break 
  #   }
  #   
  #   # Reassign Independent variables
  #   x[[1]]=as.data.frame(x[[1]][,varlist[coef.check[,1]==1]])
  #   
  #   coef.check.pre=coef.check
  # }
  
} #Loop Ends.

if(length(varlist)!=0) colnames(coef.table)=1

# Coefficient Table
if(length(varlist)!=0){
  var.in=rep(0,length(varlist))
  for (i in 1:length(varlist)){
    if (sum(coef.table[i,])!=0) var.in[i]=1
  }
  coef.table.output=data.frame(var=varlist,status=var.in,coef.table)
} 


# Decomposition
level=vector("list",1)
slope=vector("list",1)
trend=vector("list",1)
sea=vector("list",1)
reg=vector("list",1)
predict=vector("list",1)
decomp=vector("list",1)
res=vector("list",1)

if (level_type==2){
  
  if(length(varlist)!=0){
    level[[1]]=dropFirst(s[[1]]$s[,ncol(x[[1]])+1])*ymean[[1]]
    slope[[1]]=dropFirst(s[[1]]$s[,ncol(x[[1]])+2])*ymean[[1]]
    trend[[1]]=level[[1]]+slope[[1]]
    sea[[1]]=apply(dropFirst(s[[1]]$s[,(ncol(x[[1]])+3):ncol(s[[1]]$s)]),1,sum)*ymean[[1]]
    reg[[1]]=x[[1]]*dropFirst(s[[1]]$s[,1:sum(coef.check.pre[,1])])*ymean[[1]]
    predict[[1]]=(apply(reg[[1]],1,sum)+trend[[1]]+sea[[1]])
    res[[1]]=yreal[[1]]-predict[[1]]
    decomp[[1]]=data.frame(date=date,market=rep(1,length(date)),actual=yreal[[1]],
                           predict=predict[[1]],upper=predict[[1]]+qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[1]]^2)/(length(yreal[[1]])-nrow(W(mod[[1]])))),
                           lower=predict[[1]]-qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[1]]^2)/(length(yreal[[1]])-nrow(W(mod[[1]])))),residual=res[[1]],
                           level=trend[[1]],season=sea[[1]],reg=apply(reg[[1]],1,sum),reg[[1]])
  }else{
    level[[1]]=dropFirst(s[[1]]$s[,1])*ymean[[1]]
    slope[[1]]=dropFirst(s[[1]]$s[,2])*ymean[[1]]
    trend[[1]]=level[[1]]+slope[[1]]
    sea[[1]]=apply(dropFirst(s[[1]]$s[,3:ncol(s[[1]]$s)]),1,sum)*ymean[[1]]
    predict[[1]]=trend[[1]]+sea[[1]]
    res[[1]]=yreal[[1]]-predict[[1]]
    decomp[[1]]=data.frame(date=date,market=rep(1,length(date)),actual=yreal[[1]],
                           predict=predict[[1]],upper=predict[[1]]+qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[1]]^2)/(length(yreal[[1]])-nrow(W(mod[[1]])))),
                           lower=predict[[1]]-qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[1]]^2)/(length(yreal[[1]])-nrow(W(mod[[1]])))),residual=res[[1]],
                           level=trend[[1]],season=sea[[1]])
  }
  
  
}else if (level_type==1){
  
  if(length(varlist)!=0){
    level[[1]]=dropFirst(s[[1]]$s[,ncol(x[[1]])+1])*ymean[[1]]
    trend[[1]]=level[[1]]
    sea[[1]]=apply(dropFirst(s[[1]]$s[,(ncol(x[[1]])+2):ncol(s[[1]]$s)]),1,sum)*ymean[[1]]
    reg[[1]]=x[[1]]*dropFirst(s[[1]]$s[,1:sum(coef.check.pre[,1])])*ymean[[1]]
    predict[[1]]=(apply(reg[[1]],1,sum)+trend[[1]]+sea[[1]])
    res[[1]]=yreal[[1]]-predict[[1]]
    decomp[[1]]=data.frame(date=date,market=rep(1,length(date)),actual=yreal[[1]],
                           predict=predict[[1]],upper=predict[[1]]+qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[1]]^2)/(length(yreal[[1]])-nrow(W(mod[[1]])))),
                           lower=predict[[1]]-qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[1]]^2)/(length(yreal[[1]])-nrow(W(mod[[1]])))),residual=res[[1]],
                           level=trend[[1]],season=sea[[1]],reg=apply(reg[[1]],1,sum),reg[[1]])
  }else{
    level[[1]]=dropFirst(s[[1]]$s[,1])*ymean[[1]]
    trend[[1]]=level[[1]]
    sea[[1]]=apply(dropFirst(s[[1]]$s[,2:ncol(s[[1]]$s)]),1,sum)*ymean[[1]]
    predict[[1]]=trend[[1]]+sea[[1]]
    res[[1]]=yreal[[1]]-predict[[1]]
    decomp[[1]]=data.frame(date=date,market=rep(1,length(date)),actual=yreal[[1]],
                           predict=predict[[1]],upper=predict[[1]]+qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[1]]^2)/(length(yreal[[1]])-nrow(W(mod[[1]])))),
                           lower=predict[[1]]-qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[1]]^2)/(length(yreal[[1]])-nrow(W(mod[[1]])))),residual=res[[1]],
                           level=trend[[1]],season=sea[[1]])
  }
}


# Row Binding Function
rbind.ordered=function(x,y,fill){ 

  
  diffCol = setdiff(colnames(x),colnames(y))
  if (length(diffCol)>0){
    cols=colnames(y)
    temp=matrix(fill,nr=nrow(y),nc=length(diffCol))
    y=cbind(y,temp)
    colnames(y)=c(cols,diffCol)
  }
  
  diffCol = setdiff(colnames(y),colnames(x))
  if (length(diffCol)>0){
    cols=colnames(x)
    temp=matrix(fill,nr=nrow(x),nc=length(diffCol))
    x=cbind(x,temp)
    colnames(x)=c(cols,diffCol)
  }
  return(rbind(x, y[, colnames(x)]))
}

if(length(varlist)!=0){
  decomp.na=matrix(nr=0,nc=9+length(varlist),dimnames=
                     list(NULL,c("date","actual","predict","upper","lower","residual","level","season","reg",varlist)))
}else{
  decomp.na=matrix(nr=0,nc=9,dimnames=
                     list(NULL,c("date","actual","predict","upper","lower","residual","level","season","reg")))
}


decomp.na=rbind.ordered(decomp.na,decomp[[1]],fill=0)
decomp_temp=melt(decomp.na,id=c("date"))
decomp.agg=dcast(decomp_temp,date~variable,sum)

# Evaluation Metrics
res=decomp.agg$residual
MAPE=mean(abs(res)/decomp.agg$actual)*100
temp=rep(0,(length(res)-1))
for (i in 1:(length(res)-1)){
  temp[i]=res[i+1]-res[i]
}
dw=sum(temp^2)/sum(res^2)
r2=1-sum(res^2)/sum((decomp.agg$actual-mean(decomp.agg$actual))^2)
metrics=data.frame(MAPE=MAPE,DW=dw,Rsquare=r2,level_type=level_type,var_coef=var_coef,
                    var_level=var_level,var_slope=var_slope,ord_sea=ord_sea,var_sea=var_sea,CI.level=CI.level,
                    no.unknown.par=no.par,optim.method=optim.method,
                    optim.ub=optim.ub,optim.lb=optim.lb)

# Contributions
decomp=decomp.agg
decomp=decomp[decomp$date>=start.date.c&decomp$date<=end.date.c,]
decomp_name=names(decomp)
contrib=rep(0,ncol(decomp))
for (i in 2:ncol(decomp)){
  contrib[i]=sum(decomp[,i])/sum(decomp$actual)
}
contrib_data=data.frame(var=decomp_name,contrbution=contrib)
contrib_data=contrib_data[-1,]

# Plotting

plot_level_range=c(0,max(decomp.agg$actual)*1.3)
opar=par(no.readonly=TRUE)
par(mfrow=c(3,1))
plot(decomp.agg$date,decomp.agg$reg,type="l",lwd=2,main="Regresion")
plot(decomp.agg$date,decomp.agg$season,type="l",lwd=2,main="Seasonality")
plot(decomp.agg$date,decomp.agg$level,type="l",lwd=2,main="Level")
par(mfrow=c(2,2))
hist(decomp.agg$residual,freq=FALSE,col="light gray",main="Histogram of Residuals",xlab="Residual")
lines(density(decomp.agg$residual),col="red",lwd=2)
qqnorm(decomp.agg$residual,main="Q-Q Plot of Residuals")
qqline(decomp.agg$residual)
plot(x=decomp.agg$predict, y=scale(decomp.agg$residual), main="Residual Plot",
     xlab="Predict", ylab="Standardized Residual", pch=16)
abline(h=2,lty=2,col="blue");abline(h=-2,lty=2,col="blue")
acf(decomp.agg$residual)
par(mfrow=c(3,1))
dlevel_reg=decomp.agg$reg+decomp.agg$level
plot(decomp.agg$date,decomp.agg$actual,type="l",lwd=2,main="Level+Season+Reg")
lines(decomp.agg$date,decomp.agg$predict,col="red",lty=1,lwd=2)
lines(decomp.agg$date,decomp.agg$upper,col="blue",lty=3,lwd=1)
lines(decomp.agg$date,decomp.agg$lower,col="blue",lty=3,lwd=1)
plot(decomp.agg$date,decomp.agg$actual,type="l",lwd=2,main="Level+Reg")
lines(decomp.agg$date,dlevel_reg,col="green",lty=1,lwd=2)
plot(decomp.agg$date,decomp.agg$actual,type="l",main="Level",lwd=2,ylim=plot_level_range)
lines(decomp.agg$date,decomp.agg$level,col="dark blue",lty=1,lwd=2)
par(opar)








