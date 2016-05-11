clean <- function(df){
     apply(df,2,function(x) all(x==0))
     # remove all variables with 0 figures
	 df2 <- df[,!apply(df,2,function(x) all(x==0))]
     # remove banking and financial companies
	 df3 <- df2[-grep("bank|financ",df2$Category),] 
     df3 <- droplevels(df3) # droplevels
     df4 <- df3[,!apply(df3,2,function(x) all(is.na(x)))] # remove all NA columns
     df4 <- df4[,-(1:2)] # remove rowname and 1-5 iterator
     df4 <- df4[,!apply(df4,2,function(x) all(x==0))] # remove only 0 columns again
     df5 <- df4[,!apply(df4,2,function(x) sum(is.na(x))>10000)] # remove mostly na
     df5[is.na(df5)] <- 0
     df5
}

# clean balance sheet
df <- read.csv("BalanceSheetsCombined.csv",check.names = FALSE)
df <- clean(df)
write.csv(x = df5,file = "cleanBalanceSheets.csv",row.names = FALSE)

# clean income statement
df <- read.csv("IncomeStatementCombined.csv",check.names = FALSE)
df <- clean(df)
write.csv(df5,file = "cleanIncomeStatements.csv",row.names = FALSE)

# clean cash flow statement
df <- read.csv("CashflowStatementsCombined.csv",check.names = FALSE)
write.csv(df2,file = "cleanCashFlowStatements.csv",row.names = FALSE)

# reorder using reorder tables.R
# reordering hopefully done.

# Merge Balance Sheet and Profit and Loss statement
financials <- merge(bls,pls,by = 1:3,all = FALSE,sort = FALSE)
financials <- merge(financials,cfs,by = 1:3,all = FALSE,sort = FALSE)
order_financials <- c(1:71,77,75,74,78,76,72,73)
financials <- financials[,order_financials]
financials <- financials[,-72] # remove duplicate in profit/loss and cash flow statment
write.csv(financials,"financialsCombined/combinedCleanedOrderedFinancials.csv",row.names = FALSE)

# now create variable for fraudulent or not
financials$fraud <- "no"

require(data.table)
years <- 11:15
nam <- names(culprits)
# now mark companies with fraud as having committed fraud
for(i in 1:5) {
     frd <- culprits[[i]]
     financials$fraud[financials$year==years[i] & (financials$company %in% frd)] <- "yes"
}
write.csv(financials,"model Data/fullData.csv",row.names = FALSE)