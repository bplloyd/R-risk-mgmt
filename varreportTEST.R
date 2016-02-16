testDate = '/20160208'
library(xtable)
subs.o = organizeSubs(subs[paste(testDate)])
subs.exceptions = lapply(subs.o, dailyExceptions)
pofs.exceptions = dailyExceptions(pofs[paste(testDate)])
ufts.exceptions = dailyExceptions(ufts[paste(testDate)])
mstrat = pofs.exceptions[which(row.names(pofs.exceptions) %in% c("ALPHX", "ALPIX", "HHSIX")),]
lse = rbind(pofs.exceptions[which(row.names(pofs.exceptions)=="HLSIX"),], ufts.exceptions[which(row.names(ufts.exceptions)=="LSE"),], subs.exceptions$LSE)
lsd = rbind(pofs.exceptions[which(row.names(pofs.exceptions)=="HFINX"),], ufts.exceptions[which(row.names(ufts.exceptions)=="LSD"),], subs.exceptions$LSD)
ed = rbind(ufts.exceptions[which(row.names(ufts.exceptions)=="ED"),], subs.exceptions$ED)
mn = rbind(ufts.exceptions[which(row.names(ufts.exceptions)=="MN"),], subs.exceptions$MN)
mf = rbind(pofs.exceptions[which(row.names(pofs.exceptions)=="HMFIX"),], ufts.exceptions[which(row.names(ufts.exceptions)=="MF"),], subs.exceptions$MF)
exceptions = list(MULTISTRAT = mstrat, LSE=lse, LSD = lsd, ED = ed, MN = mn, MF = mf)
names(exceptions) = c("MultiStrat", "Long/Short Equity", "Long/Short Debt", "Event Driven", "Market Neutral", "Managed Futures")

attr(exceptions, "subheadings") = paste0("Daily 99% VaR Exceptions for ", names(exceptions))
exceptions.xList = xtableList(exceptions)
print(exceptions)
print.xtableList(exceptions.xList, floating = F)
#pofs.exceptions
#ufts.exceptions
#subs.exceptions$LSE
#subs.exceptions$LSD
#subs.exceptions$ED
#subs.exceptions$MN
#subs.exceptions$MF

# mstrat
# lse
# lsd
# ed
# mn
# mf
exceptions

