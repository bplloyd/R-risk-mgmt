outlookPerformance = function(subs, ufts, inds, snp, reportDate)
{
  subs.o = organizeSubs(subs[paste0("/",reportDate),])
  ufts = ufts[paste0("/",reportDate),]
  inds = inds[paste0("/",reportDate),]
  snp = snp[paste0("/",reportDate),]
  hfrx = getHFRX(inds)
  
  monthEnds = function(x){
    return(index(x[endpoints(x, on = "months"),]))
  }
  returnAndVol = function(R)
  {
    R.m = monthEnds(R)
    q.start = R.m[length(R.m)-3]+1
    h.start = R.m[length(R.m)-6]+1
    y.start = R.m[length(R.m)-12]+1 
    R.q = R[paste0(q.start,"/"),]
    R.h = R[paste0(h.start,"/"),]
    R.y = R[paste0(y.start,"/"),]
    
    q = data.frame(Return = apply(R.q, 2, FUN = Return.cumulative) , Vol = apply(R.q, 2, FUN = StdDev.annualized))
    colnames(q) = paste("Qtr",colnames(q))
    h = data.frame(Return = apply(R.h, 2, FUN = Return.cumulative) , Vol = apply(R.h, 2, FUN = StdDev.annualized))
    colnames(h) = paste("6m",colnames(h))
    y = data.frame(Return = apply(R.y, 2, FUN = Return.cumulative) , Vol = apply(R.y, 2, FUN = StdDev.annualized))
    colnames(y) = paste("1y",colnames(y))
    
    return(cbind(q, h, y))
  }
  
  CorBetaAlpha = function(R, snp)
  {
    R.m = monthEnds(R)
    q.start = R.m[length(R.m)-3]+1
    h.start = R.m[length(R.m)-6]+1
    y.start = R.m[length(R.m)-12]+1 
    R.q = R[paste0(q.start,"/"),]
    R.h = R[paste0(h.start,"/"),]
    R.y = R[paste0(y.start,"/"),]
    
    snp.m = monthEnds(snp)
    q.start.snp = snp.m[length(snp.m)-3]+1
    h.start.snp = snp.m[length(snp.m)-6]+1
    y.start.snp = snp.m[length(snp.m)-12]+1 
    snp.q = snp[paste0(q.start.snp,"/"),]
    snp.h = snp[paste0(h.start.snp,"/"),]
    snp.y = snp[paste0(y.start.snp,"/"),]
   
    
    q = data.frame(Cor = apply(R.q, 2, FUN = function(x)return(cor(x, snp.q[index(x),], use = "p"))) , Beta = apply(R.q, 2, FUN = function(x)return(coef(lm(x ~ snp.q[index(x),]))[2])), Alpha = ((1+apply(R.q, 2, FUN = function(x)return(coef(lm(x ~ snp.q[index(x),]))[1])))^252 - 1)) 
    colnames(q) = paste("Qtr",colnames(q))
    h = data.frame(Cor = apply(R.h, 2, FUN = function(x)return(cor(x, snp.h[index(x),], use = "p"))) , Beta = apply(R.h, 2, FUN = function(x)return(coef(lm(x ~ snp.h[index(x),]))[2])), Alpha = ((1+apply(R.h, 2, FUN = function(x)return(coef(lm(x ~ snp.h[index(x),]))[1])))^252 - 1)) 
    colnames(h) = paste("6m",colnames(h))
    y = data.frame(Cor = apply(R.y, 2, FUN = function(x)return(cor(x, snp.y[index(x),], use = "p"))) , Beta = apply(R.y, 2, FUN = function(x)return(coef(lm(x ~ snp.y[index(x),]))[2])), Alpha = ((1+apply(R.y, 2, FUN = function(x)return(coef(lm(x ~ snp.y[index(x),]))[1])))^252 - 1)) 
    colnames(y) = paste("1y",colnames(y))
    
    return(cbind(q, h, y))
  }
  subs.perf = lapply(subs.o, FUN = function(x)return(returnAndVol(x)))
  subs.capm = lapply(subs.o, FUN = function(x)return(CorBetaAlpha(x, snp)))
  
  ufts.perf = returnAndVol(ufts)
  ufts.capm = CorBetaAlpha(ufts, snp)
  
  snp.perf  = returnAndVol(snp)
  
  hfrx.perf = returnAndVol(hfrx)
  hfrx.capm = CorBetaAlpha(hfrx, snp)
  

  
}