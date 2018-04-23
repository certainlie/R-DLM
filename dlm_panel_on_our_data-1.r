#' Dynamic Linear Model Function on panel data 
#'
#' A function to run DLM and post-modelling calculation on panel data for Marketing-Mix modelling
#'
#' @param spec a character vector for parallel computation; for example, rep("localhost",3) means open 3 local R sessions.
#' @param data a data.table class object as modelling dataset.
#' @param model.name a character for the name of your model, used in output file names.
#' @param contrb.date a character for the time period of your contribution calculation time window, used in output file names.
#' @param group a character for cross section variable name name in modelling dataset.
#' @param date.var a character for date variable name in modelling dataset.
#' @param start.date.m a character for the start date of modelling; format is YYYY-MM-DD.
#' @param end.date.m a character for the end date of modelling; format is YYYY-MM-DD.
#' @param start.date.c a character for the start date of post-modelling contribution calculation; format is YYYY-MM-DD.
#' @param end.date.m c character for the end date of post-modelling contribution calculation; format is YYYY-MM-DD.
#' @param is.output TRUE output decomp tables.
#' @param is.graph TRUE graph AVP chart.
#' @param clv a numeric for value calcualtion of dependent, such as NPV or CLV. Default value is 1.
#' @param varlist a character vector for the independent variable names, if no independent, then put c(). 
#'    Always put variables required NEGATIVE coefficient at the beginning, then POSITIVE coefficient variables, and variables NOT required the sign of coefficient in the end.
#'    The order would be used in a build-in coefficient sign check feature. If no sign check needed, then the order doesn't matter.
#' @param num_ng_coef a numeric for the number of variables required negative cofficient.
#' @param iter a numeric for the number of iteration of coefficient sign check. Default value is 100. If no sign check needed, please put 1.
#' @param dum a character vector for variables in varlist which is not required sign(the last several variables from the above vector),if none then put c().
#' @param dum_matrix a numeric matrix for the variables in the dum vector, which specify the cross section that each variable would have impact on. If dum is c(), then put NA.
#'    Row is cross section and column is variables in dum vector. Only put 1 or 0 to specify whether a variable has impact or not.
#' @param dep a character for dependent variable name.
#' @param actual a character for actual depdendent before indexed.
#' @param mean a character for weight used to index actual dependent.
#' @param CI.level a numeric for confidence interval level in output; range is from 0 to 1. For example, 0.95 means 95 percent CI level.
#' @param level_type a numeric for the type of level component in DLM; 1 represents level type; 2 represents level+slope type.
#' @param var_coef a numeric for the variance of independent coefficient; 0 reprensents constant coef, non-zero value represents time-varying coef. For example, 1^2 means the standard deviation of each coef is 1 and time-varying;
#'    "est" means estimated by model itself.
#' @param var_level a numeric for the variance of level component; 0 reprensents constant coef, non-zero value represents time-varying coef. For example, 1^2 means the standard deviation of each coef is 1 and time-varying;
#'    "est" means estimated by model itself.
#' @param var_slope a numeric for the variance of slope component; 0 reprensents constant coef, non-zero value represents time-varying coef. For example, 1^2 means the standard deviation of each coef is 1 and time-varying;
#'    "est" means estimated by model itself.
#' @param var_sea a numeric for the variance of seasonality component; 0 reprensents constant coef, non-zero value represents time-varying coef. For example, 1^2 means the standard deviation of each coef is 1 and time-varying;
#'    "est" means estimated by model itself.
#' @param per_sea a numeric for the period of seasonality variable. For example, in weekly data, 52 means the period is 52 weeks.
#' @param ord_sea a numeric for the number of harmonics of seasonality component which is a Fourier series.
#' @param optim.method a charater for the optimization method used in MLE. Default is "L-BFGS-B". Any other method in stats::optim is available.
#' @param optim.lb a numeric for the lower bond of optimization. Defualt is -Inf.
#' @param optim.ub a numeric for the upper bond of optimization. Defualt is Inf.   
#'    
#'    
#' @return a list of modeling result. It contains the following components:
#'    stat: Diagnostic Statistics
#'    coef: a coefficient table if possible
#'
#' @export
# load('C:\\Users\\srawat\\Documents\\Milword Brown Analytics Training\\MB Shared Drive\\New Files\\Modeling\\sample_data.rda')



### log of everything + actual + mean
# data=read.xlsx('C:\\Users\\srawat\\Documents\\Milword Brown Analytics Training\\MB Shared Drive\\New Files\\Modeling\\model1-subset.xlsx','1')
# colnames(data)=c('Date','Sales','TV_worb','TV_cartoon','TV_flanker','FB_likes','Twitter','Youtube','Quarter')
# data$Quarter=NULL
# mean_sales=rep(mean(data$Sales),nrow(data))
# actual_sales=data$Sales
# df2=data[,names(data)!='Date']
# df2=log(df2)
# df2=do.call(data.frame,lapply(df2,function(x) replace(x,is.infinite(x),0)))
# df2[is.na(df2)]=0 # replace null values with 0
# df2['Date']=data['Date']
# df2['mean_sales']=mean_sales
# df2['actual_sales']=actual_sales
# #df2=df2[,c(8,1:10)]
# data=df2
# rm(df2)


## Only log of sales with actual and mean of sales
data=read.xlsx('C:\\Users\\srawat\\Documents\\Milword Brown Analytics Training\\MB Shared Drive\\New Files\\Modeling\\model1-subset.xlsx','1')
colnames(data)=c('date','Sales','TV_worb','TV_cartoon','TV_flanker','FB_likes','Twitter','Youtube','Quarter')
data['actual_sales']=data$Sales
data$Sales=log(data$Sales)
data['mean_sales']=rep(mean(data$actual_sales),nrow(data))
data$Quarter=NULL
data[is.na(data)]=0







DLM=function(
  spec,
  data,
  model.name,
  contrb.date,
  #group,
  date.var,
  start.date.m,
  end.date.m,
  start.date.c,
  end.date.c,
  is.output=F,
  is.graph=T,
  clv=1,
  varlist,
  dum,
  dum_matrix,
  num_ng_coef,
  dep,
  actual,
  mean,
  CI.level=0.95,
  iter=100,
  level_type,
  var_coef,
  var_level,
  var_slope,
  per_sea,
  ord_sea,
  var_sea,
  optim.method="L-BFGS-B",
  optim.lb=-Inf,
  optim.ub=Inf
){
  #library(dlm);library(reshape2);library(data.table);library(doSNOW)
  ############################################################################
  # 1, Setup
  ############################################################################
  spec=rep("localhost",3) # for parallel computation, only change the number which stands for # of threads
  #model.name="DDA_EX"  # Name of model, for output files' name
  # contrb.date="2012" # The time period to compute contribution, for output files' name
  # group="market" # Cross section column name in dataset
  date.var="date" # date column name in dataset
  start.date.m="2014-01-26" # Start date for modelling
  end.date.m="2017-12-31" # End date for modelling
  # #Compute contribution
  start.date.c="2014-01-26" # Starting date for computing contribution
  end.date.c="2017-12-31" # Ending date for computing contribution
  is.output=F
  is.graph=T
  clv=1 # CLV
  # # Vector of all the independent variable names, if no independent, then put c()
  # # Always put variables required NEGATIVE coef at the beginning, and variables NOT required the sign of coef in the end
  varlist=c('TV_worb','TV_cartoon','TV_flanker','FB_likes','Twitter','Youtube')
  # 
  # # Vector of variables above without coef sign constraint ( the last several variables from the above vector),if nothing then put c()
  dum=c()
  # 
  # # For the variables in the dum vector above, specify the DMA's they would have impact on.If nothing then put NA
  dum_matrix=NA
  # 
  num_ng_coef=0 # no. of negative coef variables
  dep='Sales' # dependent var name
  actual="actual_sales" # actual var name
  mean="mean_sales"# mean of actual var name
  # 
  # # DLM setup
  CI.level=0.95 # CI level
  # 
  iter=10 # no. of iteration for model selection
  # 
  level_type=2 # Moving base type: 1 represents level type; 2 represents level+slope type
  # 
  var_coef=0 # Variance of covariate coef: 0 reprensents constant coef, non-zero value represents time-varying coef. For instance, 1^2 means the standard deviation of each coef is 1 and they are time-varying coef's.
  # # "est" means estimated by model
  # 
  var_level='est'  # Variance of the level part of moving base. 0 reprensents constant level, non-zero value represents time-varying level.
  # # "est" means estimated by model
  # 
  var_slope='est' # Variance of the slope part of moving base. 0 reprensents constant slope, non-zero value represents time-varying slope.
  # # "est" means estimated by model
  # 
  per_sea=52 # Period of seasonality variable. For instance, I'm using weekly data, so my season period is 52 weeks which is one year.
  # 
  ord_sea=1# No. of harmonics of seasonality variable. The season var is a Fourier series so that no. of harmonic need to be specified.
  # 
  var_sea='est' # Variance of the seasonality. 0 reprensents constant seasonality, non-zero value represents time-varying seasonality, which means for each period, seasonality is different.
  # # "est" means estimated by model
  # 
  optim.method="L-BFGS-B" # Optimization method for variance of error estimation. Usually don't need to change.
  # 
  optim.lb=-Inf # Lower bound of variance of error for optimization. Usually don't need to change.
  # 
  optim.ub=Inf # Upper bound of variance of error for optimization. Usually don't need to change.
  
  ############################################################################
  # 2, Code part ( CORE PART, DON'T CHANGE ANYTHING WITHIN THIS PART)
  ############################################################################
  # Split data by market
  start=Sys.time()
  data[is.na(data)]=0
  #market=unique(data[[group]])
  setnames(data,date.var,"date")
  data=data[data$date>=start.date.m&data$date<=end.date.m,]
  data.dma=vector('list',1)
  y=vector('list',1)
  ymean=vector("list",1)
  yreal=vector("list",1)
  x=vector('list',1)
  
  # for (i in 1:length(market)){ #i=1
  #   data.dma[[i]]=data[data[group]==market[i],]
  #   y[[i]]=data.dma[[i]][,dep]
  #   ymean[[i]]=data.dma[[i]][,mean]
  #   yreal[[i]]=data.dma[[i]][,actual]
  #   if(length(varlist)!=0) x[[i]]=data.frame(data.table(data.dma[[i]])[,varlist,with=F])
  # }
  i=1
  data.dma[[1]]=data
  y[[1]]=data.dma[[1]][,dep]
  ymean[[1]]=data.dma[[1]][,mean]
  yreal[[1]]=data.dma[[1]][,actual]
  if(length(varlist)!=0) x[[1]]=data.frame(data.table(data.dma[[1]])[,varlist,with=F])
  date=data.dma[[1]]$date
  coef.check.pre=matrix(1,nc=1,nr=length(varlist),dimnames=list(varlist,1))
  
  # build the trend and season part
  
  p=NA
  if (level_type==2){
    if ((var_level=="est") & (var_slope!="est")){
      ploy=dlmModPoly(level_type,dV=p,dW=c(exp(p),var_slope))
      no.par=2
    }else if ((var_level!="est") & (var_slope=="est")){
      ploy=dlmModPoly(level_type,dV=p,dW=c(var_level,exp(p)))
      no.par=2
    }else if (var_level=="est" & var_slope=="est"){
      ploy=dlmModPoly(level_type,dV=p,dW=c(exp(p),exp(p)))
      no.par=3
    }else {
      ploy=dlmModPoly(level_type,dV=p,dW=c(var_level,var_slope))
      no.par=1
    }
    
  }else if (level_type==1){
    if (var_level=="est"){
      ploy=dlmModPoly(level_type,dV=p,dW=c(exp(p)))
      no.par=2
    }else{
      ploy=dlmModPoly(level_type,dV=p,dW=c(var_level))
      no.par=1
    }
    
  }else {stop("The order of lever is greater than 2.")}
  
  if (var_sea=="est"){
    sea=dlmModTrig(s=per_sea,q=ord_sea,dV=0,dW=p)
    no.par=no.par+1
  }else{
    sea=dlmModTrig(s=per_sea,q=ord_sea,dV=0,dW=var_sea)
  }
  ploy1=ploy
  sea1=sea
  
  if(length(varlist)==0) iter=1
  
  # DLM loop starts
  for (loop in 1:iter){ 
    loop=1
    print(paste("********** Round",loop,", Time: ",format(Sys.time(), "%H:%M:%S"),"**********",sep=" "))
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
                     # print(paste("modelling on :",market[i],", Market #:",i,
                     #             ", Time: ",format(Sys.time(), "%H:%M:%S"),sep=""))
                     # ploy=ploy1
                    
                     sea=sea1
                     build=function(p){
                       
                       ploy$V[is.na(ploy$V)]=exp(p[1])
                       n=1
                       no=sum(is.na(ploy$W))
                       if(no!=0){
                         ploy$W[is.na(ploy$W)]=exp(p[(n+1):(n+no)])
                         n=n+no
                       }
                       
                       if(var_sea=="est") {
                         sea$W[is.na(sea$W)]=exp(p[n+1])
                         n=n+1
                       }
                       if (length(varlist) !=0){
                         if(var_coef=="est"){
                           reg=dlmModReg(x[[i]],addInt=F,dV=0,dW = rep(p[n+1], ncol(x[[i]])))
                         }else{
                           reg=dlmModReg(x[[i]],addInt=F,dV=0,dW = rep(var_coef, ncol(x[[i]])))
                         }
                         mod=reg+ploy+sea
                       }else{
                         mod=ploy+sea
                       }
                       return(mod)
                       
                     }
                     
                     mle=dlmMLE(y[[i]],rep(0,5),build,
                                method=optim.method,lower=optim.lb,upper=optim.ub)
                     mod=as.dlm(build(mle$par))
                     ##################
                     #mod[[i]]$JFF[1,][mod[[i]]$JFF[1,]!=0]=0
                     ###################
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
    
    
    # Model Selection
    if(length(varlist)!=0){
      coef.check=matrix(0,nr=nrow(coef.table),nc=ncol(coef.table),dimnames=list(varlist,1))
      for (i in 1:nrow(coef.table)){#i=1
        if (i <=num_ng_coef){
          coef.check[i,coef.table[i,]<0]=1
          #coef.check[i,coef.table[i,]>0]=0
        }else{
          #coef.check[i,coef.table[i,]<0]=0
          coef.check[i,coef.table[i,]>0]=1
        }
      }
      if (length(dum)!=0){
        for (j in 1:length(dum)){#i=1
          coef.check[dum[j],]=dum_matrix[,j]
        }
      }
      
      coef.change=coef.check-coef.check.pre
      if (sum(coef.change)==0) {
        print("modelling is done.") 
        break 
      }
      
      # Recreate the regressor matrix
      for (j in 1:1){#j=1
        x[[j]]=as.data.frame(x[[j]][,varlist[coef.check[,j]==1]])
      }
      coef.check.pre=coef.check
    }
    
  } #DLM loop
  
  if(length(varlist)!=0) colnames(coef.table)=1
  
  # coef table for output
  if(length(varlist)!=0){
    var.in=rep(0,length(varlist))
    for (i in 1:length(varlist)){
      if (sum(coef.table[i,])!=0) var.in[i]=1
    }
    coef.table.output=data.frame(var=varlist,status=var.in,coef.table)
  } 
  
  
  # compute decomp
  level=vector("list",1)
  slope=vector("list",1)
  trend=vector("list",1)
  sea=vector("list",1)
  reg=vector("list",1)
  predict=vector("list",1)
  decomp=vector("list",1)
  res=vector("list",1)
  
  for (i in 1:1){ #i=1
    if (level_type==2){
      
      if(length(varlist)!=0){
        level[[i]]=dropFirst(s[[i]]$s[,ncol(x[[i]])+1])*ymean[[i]]
        slope[[i]]=dropFirst(s[[i]]$s[,ncol(x[[i]])+2])*ymean[[i]]
        trend[[i]]=level[[i]]+slope[[i]]
        sea[[i]]=apply(dropFirst(s[[i]]$s[,(ncol(x[[i]])+3):ncol(s[[i]]$s)]),1,sum)*ymean[[i]]
        reg[[i]]=x[[i]]*dropFirst(s[[i]]$s[,1:sum(coef.check.pre[,i])])*ymean[[i]]
        predict[[i]]=(apply(reg[[i]],1,sum)+trend[[i]]+sea[[i]])
        res[[i]]=yreal[[i]]-predict[[i]]
        decomp[[i]]=data.frame(date=date,market=rep(1,length(date)),actual=yreal[[i]],
                               predict=predict[[i]],upper=predict[[i]]+qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[i]]^2)/(length(yreal[[i]])-nrow(W(mod[[i]])))),
                               lower=predict[[i]]-qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[i]]^2)/(length(yreal[[i]])-nrow(W(mod[[i]])))),residual=res[[i]],
                               level=trend[[i]],season=sea[[i]],reg=apply(reg[[i]],1,sum),reg[[i]])
      }else{
        level[[i]]=dropFirst(s[[i]]$s[,1])*ymean[[i]]
        slope[[i]]=dropFirst(s[[i]]$s[,2])*ymean[[i]]
        trend[[i]]=level[[i]]+slope[[i]]
        sea[[i]]=apply(dropFirst(s[[i]]$s[,3:ncol(s[[i]]$s)]),1,sum)*ymean[[i]]
        predict[[i]]=trend[[i]]+sea[[i]]
        res[[i]]=yreal[[i]]-predict[[i]]
        decomp[[i]]=data.frame(date=date,market=rep(1,length(date)),actual=yreal[[i]],
                               predict=predict[[i]],upper=predict[[i]]+qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[i]]^2)/(length(yreal[[i]])-nrow(W(mod[[i]])))),
                               lower=predict[[i]]-qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[i]]^2)/(length(yreal[[i]])-nrow(W(mod[[i]])))),residual=res[[i]],
                               level=trend[[i]],season=sea[[i]])
      }
      
      
    }else if (level_type==1){
      
      if(length(varlist)!=0){
        level[[i]]=dropFirst(s[[i]]$s[,ncol(x[[i]])+1])*ymean[[i]]
        trend[[i]]=level[[i]]
        sea[[i]]=apply(dropFirst(s[[i]]$s[,(ncol(x[[i]])+2):ncol(s[[i]]$s)]),1,sum)*ymean[[i]]
        reg[[i]]=x[[i]]*dropFirst(s[[i]]$s[,1:sum(coef.check.pre[,i])])*ymean[[i]]
        predict[[i]]=(apply(reg[[i]],1,sum)+trend[[i]]+sea[[i]])
        res[[i]]=yreal[[i]]-predict[[i]]
        decomp[[i]]=data.frame(date=date,market=rep(1,length(date)),actual=yreal[[i]],
                               predict=predict[[i]],upper=predict[[i]]+qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[i]]^2)/(length(yreal[[i]])-nrow(W(mod[[i]])))),
                               lower=predict[[i]]-qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[i]]^2)/(length(yreal[[i]])-nrow(W(mod[[i]])))),residual=res[[i]],
                               level=trend[[i]],season=sea[[i]],reg=apply(reg[[i]],1,sum),reg[[i]])
      }else{
        level[[i]]=dropFirst(s[[i]]$s[,1])*ymean[[i]]
        trend[[i]]=level[[i]]
        sea[[i]]=apply(dropFirst(s[[i]]$s[,2:ncol(s[[i]]$s)]),1,sum)*ymean[[i]]
        predict[[i]]=trend[[i]]+sea[[i]]
        res[[i]]=yreal[[i]]-predict[[i]]
        decomp[[i]]=data.frame(date=date,market=rep(1,length(date)),actual=yreal[[i]],
                               predict=predict[[i]],upper=predict[[i]]+qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[i]]^2)/(length(yreal[[i]])-nrow(W(mod[[i]])))),
                               lower=predict[[i]]-qnorm(CI.level+(1-CI.level)/2)*sqrt(sum(res[[i]]^2)/(length(yreal[[i]])-nrow(W(mod[[i]])))),residual=res[[i]],
                               level=trend[[i]],season=sea[[i]])
      }
    }
  }
  
  rbind.ordered=function(x,y,fill){ 
    # x: dataset1; y: dataset2; fill: character or number for missing value
    
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
    decomp.na=matrix(nr=0,nc=10+length(varlist),dimnames=
                       list(NULL,c("date","market","actual","predict","upper","lower","residual","level","season","reg",varlist)))
  }else{
    decomp.na=matrix(nr=0,nc=10,dimnames=
                       list(NULL,c("date","market","actual","predict","upper","lower","residual","level","season","reg")))
  }
  
  for (i in 1:1){
    decomp.na=rbind.ordered(decomp.na,decomp[[i]],fill=0)
  }
  decomp_temp=melt(decomp.na,id=c("market","date"))
  decomp.agg=dcast(decomp_temp,date~variable,sum)
  
  # diagnose
  res=decomp.agg$residual
  MAPE=mean(abs(res)/decomp.agg$actual)*100
  temp=rep(0,(length(res)-1))
  for (i in 1:(length(res)-1)){
    temp[i]=res[i+1]-res[i]
  }
  dw=sum(temp^2)/sum(res^2)
  r2=1-sum(res^2)/sum((decomp.agg$actual-mean(decomp.agg$actual))^2)
  diagnose=data.frame(MAPE=MAPE,DW=dw,Rsquare=r2,level_type=level_type,var_coef=var_coef,
                      var_level=var_level,var_slope=var_slope,ord_sea=ord_sea,var_sea=var_sea,CI.level=CI.level,
                      no.unknown.par=no.par,optim.method=optim.method,
                      optim.ub=optim.ub,optim.lb=optim.lb)
  
  # compute contribution
  decomp=decomp.agg
  decomp=decomp[decomp$date>=start.date.c&decomp$date<=end.date.c,]
  decomp_name=names(decomp)
  contb=rep(0,ncol(decomp))
  account=rep(0,ncol(decomp))
  npv=rep(0,ncol(decomp))
  for (i in 2:ncol(decomp)){
    account[i]=sum(decomp[,i])
    npv[i]=sum(decomp[,i])*clv
    contb[i]=sum(decomp[,i])/sum(decomp$actual)
  }
  contb_data=data.frame(var=decomp_name,contrbution=contb,account=account,npv=npv)
  contb_data=contb_data[-1,]
  
  # compute forecast
  #for (i in 1:length(market)){
  #  forecast[[i]]=dlmForecast(
  #    f[[i]],nAhead=nAhead)$a[,sum(coef.check.pre[,i])+1]*ymean[[i]]
  #}
  #forecast.all=do.call(cbind,forecast)
  ############################################################################
  # 3, Graph output
  ############################################################################
  if (is.graph){
    plot_level_range=c(0,max(decomp.agg$actual)*1.3)
    opar=par(no.readonly=TRUE)
    #dev.new()
    par(mfrow=c(3,1))
    plot(decomp.agg$date,decomp.agg$reg,type="l",lwd=2,main="Regresion")
    plot(decomp.agg$date,decomp.agg$season,type="l",lwd=2,main="Seasonality")
    plot(decomp.agg$date,decomp.agg$level,type="l",lwd=2,main="Level")
    #dev.new()
    par(mfrow=c(2,2))
    hist(decomp.agg$residual,freq=FALSE,col="light gray",main="Histogram of Residuals",xlab="Residual")
    lines(density(decomp.agg$residual),col="red",lwd=2)
    qqnorm(decomp.agg$residual,main="Q-Q Plot of Residuals")
    qqline(decomp.agg$residual)
    plot(x=decomp.agg$predict, y=scale(decomp.agg$residual), main="Residual Plot",
         xlab="Predict", ylab="Standardized Residual", pch=16)
    abline(h=2,lty=2,col="blue");abline(h=-2,lty=2,col="blue")
    acf(decomp.agg$residual)
    #dev.new()
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
    end=Sys.time()-start
    print(paste("modelling time: ",round(end[[1]],digit=2),attr(end,"units"),sep=""))
  }
  ############################################################################
  # 4, Result output
  ############################################################################
  if (is.output){
    write.csv(decomp.na,paste(model.name,"_DLM_decomp_DMA.csv",sep=""),row.names=F) # DMA level decomp
    write.csv(decomp.agg,paste(model.name,"_DLM_decomp_NAT.csv",sep=""),row.names=F) # National level decomp
    write.csv(contb_data,paste(model.name,"_DLM_con",contrb.date,".csv",sep=""),row.names=F) # Contribution of certain period
    if (length(varlist!=0)) write.csv(coef.table.output,paste(model.name,"_DLM_var.csv",sep=""),row.names=F) # Covariates coming in the model
    write.csv(diagnose,paste(model.name,"_DLM_dx.csv",sep=""),row.names=F) # Residual diagnose 
    #save.image(paste(model.name,"_DLM.RData",sep=""))
  }
  if (length(varlist!=0)) return(list(stat=diagnose,coef=coef.table.output,decomp=decomp.na)) else
    return(list(stat=diagnose,decomp=decomp.na))
}