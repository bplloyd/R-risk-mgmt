
assets = na.omit(pofs$ALPHX)
# assets = assets[-1,]
# assets = assets[-nrow(assets),]

# facts = facts[-nrow(facts),]
facts = na.omit(facts)
width = 252
rollTsfm2(assets = assets, factors = facts, window = width) -> assets.lm
#rollTsfm2(assets = assets, factors = facts, window = width, variable.selection = "stepwise") -> assets.lm_sw
(sapply(assets.lm, function(m)return(anova(m$asset.fit[[1]])[1:ncol(facts),5])) %>% t %>% as.xts) -> assets.lm_sig
index(assets.lm_sig) = as.Date(index(assets.lm_sig))
names(assets.lm_sig) = names(facts)


(sapply(assets.lm, function(m)return(coef(m$asset.fit[[1]]))) %>% t %>% as.xts) -> assets.lm_coef
index(assets.lm_coef) = as.Date(index(assets.lm_coef))

(sapply(assets.lm, function(m)return(m$r2))) -> assets.lm_r2
xts(assets.lm_r2, order.by = index(assets[width:nrow(assets)])) -> assets.lm_r2
sapply(names(assets.lm)[-length(assets.lm)], function(d)
                            {
                              r = which(index(facts)==d)+1;
                              #return(facts[r,])
                              if(sum(is.na(facts[r,]))==0)
                                return(predict(assets.lm[[d]], newdata = facts[r,]))
                            })



chart.TimeSeries(assets.lm_coef, legend.loc = "bottomleft", main = paste0("Rolling ", width, "d Betas"))
assets.lm_r2 %>% chart.TimeSeries(., ylim = c(0,1), main = paste0("Rolling ", width, "d R2"))







