# generating new variables according to the research papers

fullData <- read.csv("model Data/fullData.csv",check.names = FALSE)
newData <- fullData

# remove where revenues were 0
newData <- newData[!newData$revenueNet==0,]
# remove those with 0 equity
newData <- newData[-which(rowSums(newData[,4:5])==0),]

# create accounts receivable and accounts recv to sales
newData$ac_recv <- newData$trade_receivables
newData$ac_recv_to_Sales <- newData$trade_receivables/newData$revenueNet

# total assets and total liabilities
newData$ta <- rowSums(newData[,c(18:25,27:32)])
newData$tl <- rowSums(newData[,c(7:10,12:15)])

# equity and debt
newData$equity <- rowSums(newData[,4:5])
newData$debt <- newData$tl - newData$equity

# EBIT
newData$ebit <- rowSums(newData[,c(57,53,64)])

# current assets and current liabilities
newData$current_assets <- rowSums(newData[,27:32])
newData$current_liabilities <- rowSums(newData[,12:15])

# Altman's Z-score
newData$az_score <- 3.3*newData$ebit/newData$ta + 0.999*newData$revenueNet/newData$ta + 0.6*newData$equity/newData$tl + 1.2*(newData$current_assets - newData$current_liabilities)/newData$ta + 1.4*newData$total_reserves_and_surplus/newData$ta

# debt to equity ratio
newData$debt_equity <- newData$debt/newData$equity

# fixed assets to total assets
newData$fixedAsset_ta <- newData$fixed_assets/newData$ta

# gross margin
newData$gross_margin <- (newData$revenueNet - newData$cogs)/newData$revenueNet

# Return on equity
newData$roe <- newData$NetIncome/newData$equity

# Inventory to sales
newData$inv_to_sales <- newData$inventories/newData$revenueNet

# sales
newData$sales <- newData$revenueNet

# PP&E to sales (Fixed assets to total assets)
newData$ppe_ta <- newData$fixed_assets/newData$ta

# sales to total assets
newData$sales_ta <- newData$sales/newData$ta

# times interest earned
newData$int_earned <- newData$ebit/newData$financeCosts

# total accruals (income - cash flow from operations) to total assets
newData$total_accruals_ta <- (newData[,57] - newData[,72])/newData$ta

# total debt by total assets
newData$td_ta <- newData$debt/newData$ta

# remove observations with missing values
newData <- newData[complete.cases(newData),]

# remove infinites
newData <- newData[-which(is.infinite(newData$int_earned)),]

# remove where total liabilities are 0
newData <- newData[-which(newData$tl==0),]

# keep only those that have full five year data
d5yr <- tapply(newData$year,newData$company,length)
newData <- newData[newData$company %in% names(d5yr[d5yr==5]),]


# Create yearly variables
nds <- split(newData,newData$year)

# function that takes two years data, and creates new variables for the 
# current year data and returns new current year data frame with new
# variables
# use this function in Beneish M-score file
yearlyVars <- function(current,previous){
     current$chg_inv_to_sales <- current$inv_to_sales - previous$inv_to_sales
     current$dsri <- current$ac_recv_to_Sales - previous$ac_recv_to_Sales
     # demand for financing involves four years of data so leaving it
     # declining cash sales dummy involves 3 years data
     # four year geometric sales in growth naturally requires 4 years data
     
     # positive accruals dummy
     pos_accC <- current$netIncome_beforeExceptional_tax - current$net_cashflow_from_operating_activities
     pos_accP <- previous$netIncome_beforeExceptional_tax - previous$net_cashflow_from_operating_activities
     current$pos_acc <- ifelse(pos_accC - pos_accP>0,1,0)
     current$prior_roa_ta_current_year <- (previous$NetIncome/previous$ta)/current$ta
     current$ac_recv_inc <- ifelse(current$ac_recv/previous$ac_recv>1.1,1,0)
     current$ac_recv_inc[is.na(current$ac_recv_inc)] <- 0
     current$gross_marginInc <- ifelse(current$gross_margin/previous$gross_margin>1.1,1,0)
     current
}

# for the data from 2012-2015, create the new variables
for(i in 5:2){
     nds[[i]] <- yearlyVars(nds[[i]],nds[[i-1]])
}

newData <- rbind_all(nds)
# save new data though NAs for new yearly variables for 2011
write.csv(newData,"model Data/newVariablesFull.csv",row.names = FALSE)

# Now go to Beneish M-Score to generate further variables