rollingWindows(uft, period = "12m", by = "1m")->wins
myList = vector("list",length(wins$from))

names(myList) = paste0("periodEnding_", seq_along(wins$to))

for(i in 1:length(wins$from)){
  myList[[i]] = VaR(R = na.omit(uft[paste(wins$from[i],"/", wins$to[i], sep = "")]), method = "gaussian", p = 0.99, portfolio_method = "component", weights = w)
}

alp.rollVaR <- myList[[1]]$contribution %>% t() %>% as.data.frame()
for(i in 2:length(wins$from)){
    alp.rollVaR = alp.rollVaR %>% rbind(myList[[i]]$contribution %>% t() %>% as.data.frame())
}
write.csv(alp.rollVaR, file = "alpRollVaR.csv")

