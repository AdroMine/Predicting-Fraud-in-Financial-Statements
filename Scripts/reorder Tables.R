# script to reorder table columns, rename them and then remove some extra

# balance sheet items reordering scheme
ord49 <- c(1,2,3,13,38,26,37,39,44,11,45,19,36,47,40,23,28,34,32,30,43,5,15,20,42,18,46,35,8,17,41,6,27,25,33,31,7,# can remove after this really
           31,36,48,14,12,16,24,4,21,22,9,10)
# reorder balance sheet
df <- df[,ord49] # balance sheets with 49 variables NA > 10000 removed not 5000
write.csv(df,"orderedBalanceSheets.csv",row.names = FALSE)

# profit/loss statement re-order
pfo <- c(1,2,3,21,13,22,36,25,15,26,6,37,32,5,11,12,9,14,24,17,28,18,7,39,8,23,27,16,20,19,4,10,29,30,35,31,34,38,33)
npl <- pl[,pfo] # reorder income statement
write.csv(npl,"orderedIncomeStatements.csv",row.names = FALSE)

# change names of income statement
pls_names <- c("category", "company", "year", "revenueGross", "less_levies", "revenueNet", "other_operating_revenues", "total_oper_revenue", "other_income", "total_revenue", "cogs", "purchaseStock", "operating_direct_expenses", "changeInventory", "employeeBenefits", "financeCosts", "depr_amort", "other_expenses", "total_expenses", "netIncome_beforeExceptional_tax", "exceptionalItems", "EBT", "current_tax", "less_mat", "deferred_tax", "tax_earlier_years", "total_tax", "EAT_before_Extraordinary", "earnings_continuing_operations", "NetIncome", "EPS_basic", "EPS_diluted", "import_raw_material", "indigenous_raw_material", "import_stores_spares", "indigenous_stores_spares", "dividendEquity", "taxDividend", "dividentRate%")
rem <- c(33:36,39) # remove some variables
pls <- pls[,-rem]

# change balance sheet items name
bls_names <- c("category", "company", "year", "equity_share_capital", "total_share_capital", "reserves_and_surplus", "total_reserves_and_surplus", "total_shareholders_funds", "long_term_borrowings", "deferred_tax_liabilities_net", "other_long_term_liabilities", "long_term_provisions", "total_non-current_liabilities", "short_term_borrowings", "trade_payables", "other_current_liabilities", "short_term_provisions", "total_current_liabilities", "total_capital_and_liabilities", "tangible_assets", "intangible_assets", "capital_work-in-progress", "fixed_assets", "non-current_investments", "deferred_tax_assets_net", "long_term_loans_and_advances", "other_non-current_assets", "total_non-current_assets", "current_investments", "inventories", "trade_receivables", "cash_and_cash_equivalents", "short_term_loans_and_advances", "other_current_assets", "total_current_assets", "total_assets", "contingent_liabilities", "total_assets.1", "total_non-current_liabilities.1", "capital_goods", "expenditure_in_foreign_currency", "dividend_remittance_in_foreign_currency", "fob_value_of_goods", "other_earnings", "bonus_equity_share_capital", "non-current_investments_quoted_market_value", "non-current_investments_unquoted_book_value", "current_investments_quoted_market_value", "current_investments_unquoted_book_value")
names(bls) <- bls_names
rem <- c(4,6,38,39,43,46:49) # variables to be removed
bls <- bls[,-rem] # remove variables

cfs_names <- c("category", "company", "year", "cash_and_equivalents_year_begin", "cash_and_equivalents_year_end", "net_cash_used_in_investing_activities", "net_cashflow_from_operating_activities", "net_inc/dec_in_cash_and_cash_equivalents", "net_profit/loss_before_extraordinary_items_and_tax", "net_cash_used_from_financing_activities")
names(cfs) <- cfs_names

