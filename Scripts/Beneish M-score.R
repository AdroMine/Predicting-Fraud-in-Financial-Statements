# beneish M Score

newData <- read.csv("model Data/newVariablesFull.csv")

# create some new variables required for Beneish M-score

# days receivables
newData$days_recv <- newData$trade_receivables/newData$sales

# asset quality (Total Assets less Current and Fixed assets scaled by Total Assets)
newData$asset_quality <- (newData$ta - newData$current_assets - newData$fixed_assets)/newData$ta

# depreciation rate
newData$depr_rate <- newData$depr_amort/(newData$depr_amort+newData$fixed_assets)

# SG&A expense (Selling, General and Administrative expenses)
newData$sga <- rowSums(newData[,c(42,50,52:55)])

# SG&A to sales
newData$sga_ratio <- newData$sga/newData$sales

# leverage
newData$leverage <- (newData$long_term_borrowings + newData$current_liabilities)/newData$ta


# Create yearly variables
nds <- split(newData,newData$year)

# function that takes current and previous year data, and calculates 
# M-score for current year, and returns data with the score
beneishM <- function(current,previous){
     # DSRI days receivalbles / prior days receivables
     # already calculated
     
     # GMI gross margin index Prior gross margin / gross margin
     current$gmi <- previous$gross_margin/current$gross_margin
     
     
     # AQI Asset quality index Asset quality / prior asset quality
     # (total assets - (current assets + PP&E))/ total assets
     current$aqi <- current$asset_quality/previous$asset_quality
     
     # SGI sales growth index
     # sales/ prior sales
     current$sgi <- current$sales/previous$sales
     
     # DEPI depr index   prior depr rate / depr rate
     # depr rate = depr/(depr + PP&E)
     current$depi <- previous$depr_rate/current$depr_rate
     
     
     # SGAI sg&a index  sg&a ratio / prior ratio
     # sg&a ratio = SG&A expense / sales
     current$sgai <- current$sga_ratio/previous$sga_ratio
     
     # TATA total accruals to total assets
     # already have
     current$tata <- current$total_accruals_ta
     
     # LVGI leverage index  Leverage/ prior leverage
     # leverage = (long-term debt + current liab) / total assets
     current$lvgi <- current$leverage/previous$leverage
     
     # sanity checking to remove NA and infinite values with 1 (not 0)
     for(i in c(107,112:118)){
          current[,i][is.na(current[,i]) | is.infinite(current[,i])] <- 1
     }

     wts <- c(0.920,0.528,0.404,0.892,0.115,-0.172,4.679,-0.327)
     
     # there should be a better method to do this, but I don't have time
     for(i in 1:nrow(current)){
          current$m_score[i] <- sum(current[i,c(107,112:118)]*wts)-4.840
     }
     current$mfraud <- (current$m_score>-2.2)
     current
}

# compute M-score for 2012-2015
for(i in 5:2){
     nds[[i]] <- beneishM(nds[[i]],nds[[i-1]])
}

# compare M-score with Auditor's opinion
lapply(nds[-1],function(x) table(x$fraud,x$mfraud,dnn=c("Auditor","M-score")))

# combine the yearly data for 2012-2015
require(dplyr)
combinedData <- rbind_all(nds[-1])
write.csv(combinedData,"model Data/Combined Data with yearly variables and M-score.csv",row.names = FALSE)

# take the latest data
latestData <- nds[[5]]
df <- combinedData[,c(1:3,79:82,88:99,119,120,78)]
write.csv(df,"model Data/newVarMScore.csv",row.names = FALSE)
# Done, now go to Benford's Law