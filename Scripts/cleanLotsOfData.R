dfog <- read.csv("financials lots wo banking.csv",check.names = FALSE,skip = 1,stringsAsFactors = FALSE,blank.lines.skip = FALSE)

df <- dfog[,!names(dfog)==""]

df <- df[-grep("bank|financ|invest",ignore.case = TRUE,df$Name),]

price <- df[,c(1:22)]
df <- df[,-(3:22)]
quarterly <- df[,c(1,2,57:84)]
df <- df[,-(57:84)]

names(df)[23] <- "Total Liabilities"
names(df)[37] <- "Total Assets"

df <- df[,!apply(df,2,function(x) all(is.na(x)))]
