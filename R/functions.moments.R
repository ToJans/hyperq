## [[file:~/projects/hyperq/hyperq.org::*startup][startup:1]]

momentum.startup = function() {
  rm(list = ls())
  p=NULL
  require(xts)
  require(moments)
  require(nloptr)
  require(quantmod)
  require(ascii)
  require(plyr)
  require(ggplot2)
  options(warn=-1)
}

## startup:1 ends here

## [[file:~/projects/hyperq/hyperq.org::*return.to.stats.raw][return\.to\.stats\.raw:1]]

return.to.stats.raw = function(r) {
  r=na.omit(r)
  mean=apply(r,2,sum)/dim(r)[1]
  std=apply(r,2,sd)
  sharpe=mean/std
  skewness=apply(r,2,skewness)
  kurtosis=apply(r,2,kurtosis)
  
  return.to.stats.raw = data.frame(
    mean=mean,
    std=std,
    sharpe=sharpe,
    skewness=skewness,
    kurtosis=kurtosis)
}

## return\.to\.stats\.raw:1 ends here

## [[file:~/projects/hyperq/hyperq.org::*return.to.stats][return\.to\.stats:1]]

return.to.stats = function(xts.r) {
  r=na.omit(xts.r)
  n=dim(r)[1]
  years=as.numeric(difftime(index(r[n]),index(r[1]), units="days")/365.25)
  days.per.year=n/years
  mean=apply(r,2,sum)/years
  std=apply(r,2,sd)*sqrt(days.per.year)
  sharpe=mean/std
  skewness=apply(r,2,skewness)
  kurtosis=apply(r,2,kurtosis)
    
  return.to.stats = data.frame(
    mean=mean,
    std=std,
    sharpe=sharpe,
    skewness=skewness,
    kurtosis=kurtosis)
}

## return\.to\.stats:1 ends here

## [[file:~/projects/hyperq/hyperq.org::*weighted.historicals][weighted\.historicals:1]]

weighted.historicals = function(rets, hist.weights) {
  hmean=as.double(filter(rets,hist.weights$mean,sides=1))
  hmean[1:length(hist.weights$mean)]=cumsum(rets[1:length(hist.weights$mean)]*t(as.matrix(hist.weights$mean)))
  xvol=(rets-hmean)^2
  hvol=as.double(filter(xvol,hist.weights$vol,sides=1))^0.5
  hvol[1:length(hist.weights$vol)]=cumsum(xvol[length(hist.weights$vol)]*t(as.matrix(hist.weights$vol)))^0.5
  weighted.historicals=data.frame(hmean=hmean, hvol=hvol)
}

## weighted\.historicals:1 ends here

## [[file:~/projects/hyperq/hyperq.org::*quantile.ts][quantile\.ts:1]]

quantile.ts = function(ts,q) {
  n=length(ts)
  nq=length(q)
  quantile.ts=
  apply(matrix(ts,n,nq)>t(matrix(q,nq,n)),1,sum)
}

## quantile\.ts:1 ends here

## [[file:~/projects/hyperq/hyperq.org::*bucket.stats.posandr][bucket\.stats\.posandr:1]]

bucket.stats.posandr = function(pos, r, qts1, qts2, num.quantiles, delay.signal) {
  br=matrix(0,num.quantiles,num.quantiles);bcount=br;bpos=br;c=br;babsr=br;bsqr=br;bstd=br;
  for (i in 1:num.quantiles){
    for (j in 1:num.quantiles){
      sb=(qts1==(i-1))&(qts2==(j-1))
      sb=c(as.logical(rep(0,delay.signal)), sb[-(length(sb):(length(sb)-delay.signal))])
      bcount[i,j]=sum(sb)
      if (bcount[i,j]>0){
        br[i,j]=mean(r[sb])
        bpos[i,j]=mean(pos[sb])
        babsr[i,j]=mean(abs(r[sb]))
        bsqr[i,j]=mean(r[sb]^2)
        bstd[i,j]=apply(r[sb],2,sd)
      }
    }
  }
  bucket.stats=list(
    bcount=bcount,
    br=br,
    bpos=bpos,
    babsr=babsr,
    bsqr=bsqr,
    bstd=bstd)
}

## bucket\.stats\.posandr:1 ends here

## [[file:~/projects/hyperq/hyperq.org::*bucket.stats][bucket\.stats:1]]

bucket.stats = function(r, qts1, qts2, num.quantiles, delay.signal) {
  br=matrix(0,num.quantiles,num.quantiles);bcount=br;c=br;babsr=br;bsqr=br;bstd=br;
  for (i in 1:num.quantiles){
    for (j in 1:num.quantiles){
      sb=(qts1==(i-1))&(qts2==(j-1))
      sb=c(as.logical(rep(0,delay.signal)), sb[-(length(sb):(length(sb)-delay.signal))])
      bcount[i,j]=sum(sb)
      if (bcount[i,j]>0){
        br[i,j]=mean(r[sb])
        babsr[i,j]=mean(abs(r[sb]))
        bsqr[i,j]=mean(r[sb]^2)
        bstd[i,j]=sd(r[sb])
      }
    }
  }
  bucket.stats=list(
    bcount=bcount,
    br=br,
    babsr=babsr,
    bsqr=bsqr,
    bstd=bstd)
}

## bucket\.stats:1 ends here

## [[file:~/projects/hyperq/hyperq.org::*print.qstat][print\.qstat:1]]

print.qstat = function(stat,count) {
  row.count = apply(count,1,sum)
  col.count = apply(count,2,sum)
  all.count = sum(row.count)
  row.stat = apply(stat*count,1,sum)/row.count
  col.stat = apply(stat*count,2,sum)/col.count
  all.stat = sum(row.stat*row.count)/all.count
  out=rbind(cbind(stat,row.stat), c(col.stat,all.stat))
  colnames(out)=c("low", "low-mid", "mid", "high-mid", "high", "all")
  rownames(out)=c("low", "low-mid", "mid", "high-mid", "high", "all")
  print.qstat=out }

## print\.qstat:1 ends here

## [[file:~/projects/hyperq/hyperq.org::*print.qstat.count][print\.qstat\.count:1]]

print.qstat.count = function(count) {
  row.stat = apply(count,1,sum)
  col.stat = apply(count,2,sum)
  all.stat = sum(row.stat)
  out=rbind(cbind(count,row.stat), c(col.stat,all.stat))
  colnames(out)=c("low", "low-mid", "mid", "high-mid", "high", "all")
  rownames(out)=c("low", "low-mid", "mid", "high-mid", "high", "all")
  print.qstat=out }

## print\.qstat\.count:1 ends here
