# combine individual downloaded files into one 
require(data.table)
setwd("Dissertation/Financials/")
balSh <- list.files(pattern = "-bs.csv$")
temp <- lapply(balSh,fread,sep = ",")
balSheets <- rbindlist(temp,fill = TRUE)
write.csv(balSheets,file = "../BalanceSheetsCombined.csv")

inSt <- list.files(pattern = "-is.csv$")
cfSt <- list.files(pattern = "-cf.csv$")

temp <- lapply(inSt, fread,sep = ",")
incomeStatement <- rbindlist(temp,fill = TRUE)
write.csv(incomeStatement,"../IncomeStatementCombined.csv")

temp <- lapply(cfSt,fread,sep = ",")
cashFlow <- rbindlist(temp,fill = TRUE)
write.csv(cashFlow,"../CashflowStatementsCombined.csv")
