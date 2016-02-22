# RETURNS LIST OF ALL DAILY VAR EXCEPTIONS FOR AUTOMATED VAR REPORT

allDailyExceptions = function(subs=NULL, ufts=NULL, pofs=NULL, reportDate=NULL)
{
  if(is.null(subs))
      subs = loadSubAdvisors()
  if(is.null(ufts))
    subs = loadUFTs()
  if(is.null(pofs))
    subs = loadPOFs()
  if(is.null(reportDate))
      reportDate = end(subs)
  
  subs.o = organizeSubs(subs[paste("/",reportDate,sep = "")])
  subs.exceptions = lapply(subs.o, dailyExceptions)
  pofs.exceptions = dailyExceptions(pofs[paste("/",reportDate,sep = "")])
  ufts.exceptions = dailyExceptions(ufts[paste("/",reportDate,sep = "")])
  mstrat = pofs.exceptions[which(row.names(pofs.exceptions) %in% c("ALPHX", "ALPIX", "HHSIX")),]
  lse = rbind(pofs.exceptions[which(row.names(pofs.exceptions)=="HLSIX"),], ufts.exceptions[which(row.names(ufts.exceptions)=="LSE"),], subs.exceptions$LSE)
  lsd = rbind(pofs.exceptions[which(row.names(pofs.exceptions)=="HFINX"),], ufts.exceptions[which(row.names(ufts.exceptions)=="LSD"),], subs.exceptions$LSD)
  ed = rbind(ufts.exceptions[which(row.names(ufts.exceptions)=="ED"),], subs.exceptions$ED)
  mn = rbind(ufts.exceptions[which(row.names(ufts.exceptions)=="MN"),], subs.exceptions$MN)
  mf = rbind(pofs.exceptions[which(row.names(pofs.exceptions)=="HMFIX"),], ufts.exceptions[which(row.names(ufts.exceptions)=="MF"),], subs.exceptions$MF)
  exceptions = list(MULTISTRAT = mstrat, LSE=lse, LSD = lsd, ED = ed, MN = mn, MF = mf)
  names(exceptions) = c("MultiStrat", "Long/Short Equity", "Long/Short Debt", "Event Driven", "Market Neutral", "Managed Futures")
  
  attr(exceptions, "subheadings") = paste0("Daily 99% VaR Exceptions for ", names(exceptions))
  return(exceptions)
}