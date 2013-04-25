## [[file:~/projects/hft/hft.org::*sp500%20data][sp500\ data:1]]

rm(list = ls())
require(xts)
require(quantmod)
options(warn=-1)
Sys.setenv(TZ='GMT')
getSymbols("^GSPC",src='yahoo', from='1927-01-01')
sp500.price=na.omit(Cl(GSPC))
sp500=log(sp500.price/lag(sp500.price,1))
sp500=sp500[-1]
colnames(sp500)="sp500"

## sp500\ data:1 ends here

## [[file:~/projects/hft/hft.org::*wmom][wmom:1]]

wmom = function(rets, weights) {
  mean=as.double(filter(rets,weights$mean,sides=1))
  mean[1:length(weights$mean)]=cumsum(rets[1:length(weights$mean)]*as.matrix(weights$mean))
  xvol=(rets-mean)^2
  vol=as.double(filter(xvol,weights$vol,sides=1))^0.5
  vol[1:length(weights$vol)]=cumsum(xvol[1:length(weights$vol)]*as.matrix(weights$vol))^0.5
  wmom=data.frame(mean=mean, vol=vol)
}

## wmom:1 ends here

## [[file:~/projects/hft/hft.org::*moments%20regression][moments\ regression:1]]

l=251
weights=data.frame(mean=rep(1/l,l),vol=rep(1/l,l))
m=wmom(sp500,weights)
data=data.frame(sp500[-1],sp500[-1]^2,
  m$mean[-length(m$mean)],m$vol[-length(m$vol)]) #1 day delay in signal
colnames(data)=c("sp500","sp500.vol","hmean","hvol")
fit.mean = lm(sp500 ~ hmean + hvol,data=data)
summary(fit.mean)
fit.vol = lm(sp500.vol ~ hmean + hvol,data=data)
summary(fit.vol)

## moments\ regression:1 ends here

## [[file:~/projects/hft/hft.org::*ma(20)%20price%20performance][ma\(20\)\ price\ performance:1]]

wmean = function(rets, weights) {
  wm=as.double(filter(rets,weights,sides=1))
  wm[1:length(weights)]=cumsum(rets[1:length(weights)]*as.matrix(weights))
  wmean=wm
}

## ma\(20\)\ price\ performance:1 ends here

## [[file:~/projects/hft/hft.org::*ma(20)%20price%20performance][ma\(20\)\ price\ performance:1]]

sharpe = function(rets) {
  m=sum(log(1+rets))/length(rets)*251
  v=sd(log(1+rets))*(251^0.5)
  sharpe=m/v
}

## ma\(20\)\ price\ performance:1 ends here

## [[file:~/projects/hft/hft.org::*ma(20)%20price%20performance][ma\(20\)\ price\ performance:1]]

weights.ma20=seq(0.95,0.05,-0.05)
m=wmean(sp500,weights.ma20)
sig=as.double(m>0)
sig=sig[-length(sig)]
sharpe.long = sharpe(sp500[-1])
sharpe.ma20 = sharpe(sp500[-1]*sig)
c(sharpe.long,sharpe.ma20)

## ma\(20\)\ price\ performance:1 ends here

## [[file:~/projects/hft/hft.org::*ma(20)%20price%20performance][ma\(20\)\ price\ performance:1]]

png(filename="assets/ma20.delay.png")
weights.ma20=seq(0.95,0.05,-0.05)
m=wmean(sp500,weights.ma20)
sig=as.double(m>0)
sharpe.ma20.delay = rep(0,20)
sharpe.long.delay = rep(0,20)

for (x1 in 0:20) {
  sharpe.long.delay[x1+1] = sharpe(sp500[(x1+1):length(sp500)])
  sharpe.ma20.delay[x1+1] = sharpe(sp500[(x1+1):length(sp500)]*sig[1:(length(sp500)-x1)])
}
require(reshape)
data = melt(data.frame(sharpe.long.delay, sharpe.ma20.delay, delay=0:20),id="delay")
require(ggplot2)
  ggplot(data=data,
         aes(x=delay, y=value,color=variable)) +
    geom_line()

dev.off()

## ma\(20\)\ price\ performance:1 ends here

## [[file:~/projects/hft/hft.org::*turnover%20adjustment][turnover\ adjustment:1]]

sharpe.costs = function(rets,turnover,cost) {
  m=sum(log(1+rets))/length(rets)*251-turnover*cost
  v=sd(log(1+rets))*(251^0.5)
  sharpe.costs=m/v
}

## turnover\ adjustment:1 ends here

## [[file:~/projects/hft/hft.org::*turnover%20adjustment][turnover\ adjustment:1]]

turnover = sum(abs(diff(sig)))/length(sig)*251
x1=1
scost = sharpe.costs(sp500[(x1+1):length(sp500)]*sig[1:(length(sp500)-x1)],turnover, 0.001)

## turnover\ adjustment:1 ends here

## [[file:~/projects/hft/hft.org::*auto-conditionality%20estimates][auto-conditionality\ estimates:1]]

l=251
weights=data.frame(mean=rep(1/l,l),vol=rep(1/l,l))
m=wmom(sp500,weights)
data=data.frame(sp500[-1],sp500[-1]^2,
  m$mean[-length(m$mean)],m$vol[-length(m$vol)]) #1 day delay in signal
colnames(data)=c("sp500","sp500.vol","hmean","hvol")
fit.mean = lm(sp500 ~ 0 + hmean + hvol,data=data)
summary(fit.mean)
fit.vol = lm((sp500.vol-0.005^2) ~ 0 + hmean + hvol,data=data)
summary(fit.vol)

## auto-conditionality\ estimates:1 ends here

## [[file:~/projects/hft/hft.org::*auto-conditionality%20estimates][auto-conditionality\ estimates:1]]

mean.est = 0.0003 + 0.12 * data$hmean + 0.011 * data$hvol
vol.est = pmax(0.005^2 + -0.075 * data$hmean + 0.027 * data$hvol)^0.5

## auto-conditionality\ estimates:1 ends here

## [[file:~/projects/hft/hft.org::*model][model:1]]

png(filename="assets/sig.example.png")
price=c(0,cumsum(log(1+sp500)))
weights.av19 = rep(0.05,19)
weights.av20 = rep(0.05,20)
price.ma=as.double(filter(price,weights.av20,sides=1))
price.ma[1:length(weights.av20)]=cumsum(price[1:length(weights.av20)])/1:length(weights.av20)
gap=price-price.ma
st.mean.forecast = 0
return.forecast = st.mean.forecast-c(0,wmean(log(1+sp500),weights.av19))
price.ma.delta = c(0,price.ma[1:(length(price)-1)] * return.forecast[1:(length(price)-1)])
price.ma.forecast = c(0,price.ma[1:(length(price)-1)]) - price.ma.delta
gap.forecast.norm = (c(0,gap[1:(length(price)-1)])+price.ma.delta)/c(0.005,vol.est[1:(length(price)-1)])
sig.forecast = pnorm(gap.forecast.norm)
sig = as.double(gap>0)

df = data.frame(rets=c(0,sp500),price,price.ma,price.ma.forecast,sig,sig.forecast,
                 time=0:(length(price)-1))
colnames(df) = c("rets","price","price.ma","forecast.ma","sig","sig.forecast","time")

require(ggplot2)
require(reshape2)    

df.melt = melt(df[1:100,],measure.vars=c("price","price.ma","sig","sig.forecast"))

df.melt$type = c("sig","price")[as.double(is.element(df.melt$variable,as.factor(c("price","price.ma"))))+1]

ggplot(df.melt, aes(x = time, y = value)) +
  geom_line(aes(color = variable)) +
  facet_grid(type ~ ., scales = "free_y")
dev.off()

## model:1 ends here

## [[file:~/projects/hft/hft.org::*signal%20forecast%20and%20returns][signal\ forecast\ and\ returns:1]]

png(filename="assets/sig.returns.png")
require(bigvis)
require(grid)
tweak <- list(
  theme(
    legend.position = "bottom",
    plot.margin = unit(c(0, 0.5, 0, 0.5), "lines"),
    legend.key.width = unit(1, "inches"),
    text = element_text(size = 18)
  ),
  labs(x = NULL, y = NULL, fill = NULL),
  scale_fill_gradient(low="#e5e5e5", high = "#444548")
)

ret.sum = condense(bin(df$sig.forecast,0.02),bin(df$rets,0.002))
rs = peel(ret.sum,.95)
autoplot(rs) + tweak
dev.off()

## signal\ forecast\ and\ returns:1 ends here

## [[file:~/projects/hft/hft.org::*mean%20and%20volatility%20variations][mean\ and\ volatility\ variations:1]]

png(filename="assets/sig.analysis.png")
require(bigvis)
tweak <- list(
  scale_x_continuous("Signal Probabilistic Forecast", breaks = seq(0,1,0.2)),
  ylab(NULL),
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    text = element_text(size = 18),
    panel.margin = unit(0.25, "cm")
  )
)
ret.sum = condense(bin(df$sig.forecast,0.02),z=df$rets, summary = "sd")

d = data.frame(
  ret = rep(ret.sum$df.sig.forecast,3),
  summary = rep(c("count","mean","sd"), each = nrow(ret.sum)),
  value = c(ret.sum$.count, ret.sum$.mean*251, ret.sum$.sd*(251^.5))
)

smoothes <- list(
  count = smooth(ret.sum, 0.2, var = ".count", type = "robust"),
  mean = smooth(ret.sum, 0.2, var = ".mean", type = "robust"),
  sd = smooth(ret.sum, 0.2, var = ".sd", type = "robust"))
smoothes$mean$.mean = smoothes$mean$.mean*251
smoothes$sd$.sd = smoothes$sd$.sd*(251^0.5)

smoothes <- Map(function(x, n) {
  names(x)[2] <- "value"
  x$summary <- n
  x
}, smoothes, names(smoothes))
smooth <- do.call(rbind, smoothes)
smooth$summary <- factor(smooth$summary, levels = c("count", "mean", "sd"))
levels(smooth$summary)[1] <- "count"

qplot(ret, value, data = d, geom = "line") +
  facet_grid(summary ~ ., scales = "free") +
  tweak +
  geom_line(data = smooth, aes(x=df.sig.forecast, y=value),colour="#000099") 
  

dev.off()

## mean\ and\ volatility\ variations:1 ends here
