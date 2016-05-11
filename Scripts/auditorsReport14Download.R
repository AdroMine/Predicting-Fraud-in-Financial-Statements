require(XML)
# require(dplyr)
# require(reshape2)
require(RSelenium)
# require(rvest)
startServer()
dr <- remoteDriver(browserName = "phantomjs")

# firefox hide all images permissions.default.image


while(inherits(try(dr$open()),'try-error'))

setwd("D:/RData/Dissertation/")
allLinks <- readLines("StockLinks/allLinks.txt")
iter <- 2868
total <- length(allLinks)
count <- 1235
ar <- "http://www.moneycontrol.com/annual-report/"
for(i in iter:4000){
     link <- allLinks[i]
     i <- i+1
     # verbose messages #
     cat(paste0("\n Iteration:",iter," ; ",
                round(iter/total*100),
                "% completed ..."))
     iter <- iter+1
     
     # get name of company and ticker symbol
     str <- unlist(strsplit(link,"/",fixed = TRUE))
     category <- str[6]
     str <- str[7:8]
     
     # generate auditor's report url
     l <- paste0(ar,str[1],"/auditors-report/",str[2],"#",str[2])
     
     token <- TRUE
     while(token){
          res <- try({
               doc <- htmlParse(l)
               we <- getNodeSet(doc,"//td[@class='headcol11blue']")
               year <- xmlValue(we[[2]])   
          })
          token <- inherits(res,'try-error')
     }
     
     if(!grepl("\\d{2}$",year)){
          # no auditor's report, so skip?
          next()
     }
     year <- as.integer(substr(year,nchar(year)-1,nchar(year)))
     if(!year==15){
          # old data skip it
          next()
     }
     
     # this is the data we copied. Now copy older audit report.
     while(inherits(try(dr$navigate(l)),'try-error')){
          cat("\nCan't load page :( \n")
     }
     Sys.sleep(3)
     
     token2 <- TRUE
     while(token2){
          res <- try({
               we3 <- dr$findElement(using = 'xpath',"//a[@class='prevnext']")
               tex <- we3$getElementText()[[1]]
          })
          token2 <- inherits(res,'try-error')
     }
     if(!grepl("\\d{2}$",tex))
          next()
     we3$clickElement() 
     
     token3 <- TRUE
     while(token3){
          res <- try({
               we2 <- dr$findElement(using = 'xpath',"//pre")
               report <- we2$getElementText()   
          })
          token3 <- inherits(res,'try-error')
     }
     
     # write Auditor's report to file
     writeLines(report[[1]],paste0("AuditorsReport14/",str[2],".txt"))
     count <- count + 1
     cat(paste("Written records:",count))
}
