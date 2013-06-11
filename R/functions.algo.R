
## [[file:~/projects/hyperq/hyperq.org::*weighting%20function][weighting\ function:1]]

weighted.curve = function(maxn,slope,weight,delay) {
  c = rep(0,maxn)
  for (x1 in 1:length(slope)) {
    if (slope[x1] == 0) {
      c = c + weight[x1] * 
        c(rep(0,delay[x1]),
          rep(1/maxn,maxn-delay[x1]))
    } else {
      c = c + weight[x1] * 
        c(rep(0,delay[x1]),
          exp((-1:(delay[x1]-maxn))*slope[x1])*(exp(slope[x1])-1))
    }
  }
  weighted.curve = c / sum(c)
}

## weighting\ function:1 ends here
