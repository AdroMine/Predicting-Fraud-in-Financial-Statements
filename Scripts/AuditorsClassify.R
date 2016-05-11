# auditor's reports take out ones with adverse impact suggested by auditors
require(quanteda)
setwd("D:/RData/Dissertation/")

# read different year reports
years <- 11:15
loc <- paste0("audit",years,"/*.txt") # locations of different reports

auditReports <- lapply(loc,function(x) corpus(textfile(x))) # read the reports
auditYears <- c("y11","y12","y13","y14","y15") # names for list elements
names(auditReports) <- auditYears


# function to which a corpus is sent, it selects the reports which had
# some doubtful comments or opinion by the auditors, and returns their names
selectAdverse <- function(corp){
     culpritsAll <- NULL
     
     # the ones with adverse remarks about something in the statements
     # if the firm had done like this, the adverse impact would be like so and so
     adv <- kwic(corp,"adverse")
     res <- !grepl("\\<(no|not)\\>",adv[,3],ignore.case = TRUE)
     if(any(res)){
          adv <- adv[res,]
          culprits1 <- as.character(unique(adv[,1]))
          culpritsAll <- c(culpritsAll,culprits1)
     }
     
     # auditors outright state something is not in conformity with the principles of accounting
     noconf <- kwic(corp,"not in conformity")
     if(!any(is.na(noconf))){
          culprits2 <- as.character(unique(noconf[,1]))
          culpritsAll <- c(culpritsAll,culprits2)
     }
     
     # auditors state somethinig not in true and fair view
     nofair <- kwic(corp,"true and fair view",window = 6)
     res <- grepl("\\<(no|not)\\>",nofair[,3],ignore.case = TRUE)
     if(any(res)){
          nofair <- nofair[res,]
          culprits3 <- as.character(unique(nofair[,1]))
          culpritsAll <- c(culpritsAll,culprits3)
     }
     unique(culpritsAll)
}

# find culprits for all years
culprits <- lapply(auditReports,selectAdverse)

# now remove .txt from their names
culprits <- lapply(culprits,function(x) gsub("\\.txt","",x))
writeLines(culprits,"fraudFirms.txt")

# save all culprits to separate lists by years
lapply(names(culprits),function(x) writeLines(culprits[[x]],paste0("Fraud_",x,".txt")))

