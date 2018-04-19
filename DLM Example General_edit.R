# DLM_1.R

library(sas7bdat)
library(rJava)
library(xlsxjars)
library(dlm)
library(ggplot2)
library(xlsx)

#Generating Random Data.  For Modeling you'll read the data you have set up.  That data needs to be formatted a bit more for R.
m <- 1
n <- 100
p <- 2
y <- matrix(rnorm(n * m),n,m); y
x <- matrix(rnorm(n * p),n,p); x

print("OLS Regression\n",sep="")
outLM <- lm(y ~ x)
outLM$coef
acf(outLM$res)
qqnorm(outLM$res)



data_orig <- read.xlsx("C:/Users/jsohn/Documents/Projects/MMM/US/model1-subset.xlsx", sheetName = "startwk4")
data <- data_orig
data[is.na(data)] <- 0
rownames(data) <- data$Date
data$Date <- NULL
data <- log(data)
data = do.call(data.frame, lapply(data, function(x) replace(x, is.infinite(x), 0)))
# colnames(data)=c('Sales','TV_worb','TV_cartoon','TV_flanker','youtube_views','fb_likes','fb_comments','fb_shares','twitter_int','cat_sales','monster_sales','rockstar_sales')
colnames(data)=c('Sales','TV_worb','TV_cartoon','TV_flanker','youtube_views','fb_likes','twitter_int','cat_sales','monster_sales','rockstar_sales')
# colnames(data)=c('Sales','TV_worb','TV_cartoon')


y_df = as.data.frame(data$Sales)
x_df = data[, names(data)!='Sales']

y <- as.matrix(y_df)
x <- as.matrix(x_df)

m <- ncol(y)
n <- nrow(x)
p <- ncol(x)

# Sets up a 'dlm' object for the Regression model. NO NEED TO ADJUST.
FF <- diag(1) %x% matrix(c(1,rep(0, p)), nrow = 1)
JFF <- matrix(0,m,m * (p + 1))
for (j in 1 : m){
  JFF[j,(p + 1) * (j - 1) + 1 + 1 : p] <- (j - 1) * p + 1 : p
}

# Measurement Equation (FOR REFERENCE):
# Y(t) = Beta0(t) + Beta1(t) x X1(t) + Beta2(t) x X2(t) + v1, v1 ~ N(0,sigma_v1^2)

# State Equation (FOR REFERENCE):
# Beta0(t) = 1 x Beta0(t-1) + 0 x Beta1(t-1) + 0 x Beta2(t-1) + w0, w0 ~ N(0,sigma_w0_0^2)
# Beta1(t) = 0 x Beta0(t-1) + 1 x Beta1(t-1) + 0 x Beta2(t-1) + w1, w1 ~ N(0,sigma_w1_0^2)
# Beta2(t) = 0 x Beta0(t-1) + 0 x Beta1(t-1) + 1 x Beta2(t-1) + w2, w2 ~ N(0,sigma_w2_0^2)

#ACTUAL REGRESSION PORTION OF DLM.  X are the independents.

modReg <- dlm(FF = FF, GG = diag(m * (p + 1)),JFF = JFF,X = x,V = diag(m),W = diag(m * (p + 1)),m0 = rep(0, m * (p + 1)), 
  C0 = 1e8 * diag(m * (p + 1)))

#THE BELOW INTEGRATES REGRESSION AND SEASONALITY. Can use dlmModTrig(s, q, om, tau, dV = 1, dW = 0, m0, C0) for seasonality instead of dlmModSeas(4,dV=0)
#dlmModTrig approximates a Sine wave (Fourier) which will fit well with your sales data.
#Y is the dependent variable here.

mod <- modReg + dlmModSeas(4,dV=0)
outF <- dlmFilter(y,mod)
outF$mod
outF$m[2:length(y),]

# u = |u1 u2 u3 u4|
# V = |u1| 
# W = |u2 0  0 |
#     |0  u3 0 |
#     |0  0  u4|

buildReg <- function(u) {
  # print(exp(u[1]) * diag(m))
  dlm(FF = FF, GG = diag(m * (p + 1)),JFF = JFF,X = x,V = exp(u[1]) * diag(m),W = diag(exp(u[2:11])),m0 = rep(0, m * (p + 1)), 
  C0 = 1e8 * diag(m * (p + 1))) + dlmModSeas(4,dV=0)
}
outMLE <- dlmMLE(y,parm=rep(0,11),buildReg)
outMLE$value
mod <- buildReg(outMLE$par)
outF <- dlmFilter(y,mod)
outF$mod
round(outF$m,4)

#CHARTS.

plot(y,type='o',col="seagreen")
sdev <- residuals(outF)$sd
lwr <- outF$f + qnorm(0.45) * sdev
upr <- outF$f - qnorm(0.45) * sdev
df <- data.frame(cbind(matrix(c(1:n)),outF$m[2:nrow(outF$m),],outF$f,lwr,upr))
# names(df) <- c("Time","Beta1","Beta2","Beta3","S1","S2","S3","y_hat","lwr","upr")
names(df) <- c("Time",'Sales','TV_worb','TV_cartoon','TV_flanker','youtube_views','fb_likes','twitter_int','cat_sales','monster_sales','rockstar_sales',"S1","S2","S3","y_hat","lwr","upr")
ggplot(df, aes(Time, y = value, color = Parameter)) + 
  geom_line(aes(y=TV_worb, col="TV_worb")) + 
  geom_line(aes(y=TV_cartoon, col="TV_cartoon")) +
  geom_line(aes(y=TV_flanker, col="TV_flanker")) +
  geom_line(aes(y=youtube_views, col="youtube_views")) +
  geom_line(aes(y=fb_likes, col="fb_likes")) +
  geom_line(aes(y=twitter_int, col="twitter_int")) +
  geom_line(aes(y=cat_sales, col="cat_sales")) +
  geom_line(aes(y=monster_sales, col="monster_sales")) +
  geom_line(aes(y=rockstar_sales, col="rockstar_sales")) +
  ggtitle("Parameters by Time\n") + xlab("Time") + ylab("Time-Varying Parameters")
ggplot(df,aes(Time, y=value, color=Quarter)) + 
  geom_line(aes(y=S1, col="S1")) + 
  geom_line(aes(y=S2, col="S2")) +
  geom_line(aes(y=S3, col="S3")) +
  ggtitle("Seasonality by Time\n") + xlab("Time") + ylab("Time-Varying Parameters")
ggplot(df,aes(Time, y=value, color = Series)) + 
  geom_line(aes(y=y, col="y")) + 
  geom_line(aes(y=y_hat, col="y_hat")) +
  ggtitle("Observed Versus Predicted y\n") + xlab("Time") + ylab("Observed & Predicted y")
ggplot(df,aes(Time, y=value, color = Series)) + 
  geom_line(aes(y=y, col="y")) + 
  geom_line(aes(y=y_hat, col="y_hat")) +
  geom_line(aes(y=lwr, col="lwr")) +
  geom_line(aes(y=upr, col="upr")) +
  coord_cartesian(xlim = c(15, 100)) +
  ggtitle("Observed Versus 1-Step-Ahead Forecast y\n") + xlab("Time") + ylab("Observed y & 1-Step-Ahead Forecast")

print("MAD\n",sep="")
round(mean(abs(outF$f - y)),2)
print("MSE\n",sep="")
round(mean((outF$f - y)^2),2)
print("MAPE\n",sep="")
round(mean(abs(outF$f - y) / y),2)

print("90% CI of Beta0\n",sep="")
v <- unlist(dlmSvd2var(outF$U.C, outF$D.C))
v <- v[37:length(v)]
v1 <- v[c(0:99*36)+1]
pl <- dropFirst(outF$m[,1]) + qnorm(0.05, sd=sqrt(v1))
pu <- dropFirst(outF$m[,1]) + qnorm(0.95, sd=sqrt(v1))
plot(y, type="o", col="seagreen")
lines(dropFirst(outF$m[,1]),type="o",pch=20,col="brown")
lines(pl, lty=2, col="brown")
lines(pu, lty=2, col="brown")

outS <- dlmSmooth(outF)
round(outS$s,4)

# rm(list=ls())
