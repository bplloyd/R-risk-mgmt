myStats = function(R, bm)
{
  require(xts)
  require(PerformanceAnalytics)
  
  t.ann = table.AnnualizedReturns(R)
  t.stats = table.Stats(R)
  t.capm = table.CAPM(Ra=R, Rb=R[,bm])
  colnames(t.capm)=names(R)
  
  t.cor = t(table.Correlation(Ra = R, Rb=R[,bm]))
  colnames(t.cor)=names(R)
  
  t.dr = table.DownsideRisk(R)
  t.capture = t(table.CaptureRatios(Ra = R, Rb = R[,bm]))
  t.ddratio = table.DrawdownsRatio(R)
 
  t.hm = table.HigherMoments(Ra = R, Rb = R[,bm])
  names(t.hm) = names(R)
  
  t.IR = table.InformationRatio(R = R, Rb = R[,bm])
  t.sr = table.SpecificRisk(Ra = R, Rb = R[,bm])
  
  return(rbind(t.ann, t.stats, t.capm, t.cor, t.dr, t.capture, t.ddratio, t.hm, t.IR, t.sr))
  

}