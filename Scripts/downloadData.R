# download auditor's report and financials
# total records 7851, written 3487
getAllData <- function(){
     
     require(XML)
     require(dplyr)
     require(reshape2)
     require(RSelenium)
     startServer()
     dr <- remoteDriver()
     dr$open()
     setwd("D:/RData/Dissertation/")
     allLinks <- readLines("StockLinks/allLinks.txt")
     iter <- 358
     total <- length(allLinks)
     count <- 161
     # fullBL <- data.frame()
     # fullIS <- data.frame()
     # fullCF <- data.frame()
     
     financials <- function(doc,category,str){
          
          tableSelector <- "//table[@class='table4']"
          node <- getNodeSet(doc,tableSelector)
          n <- length(node)
          node <- node[[n]]
          classes <- c("character",rep("numeric",5)) 
          # balance sheet item, then 5 year data
          
          tab <- readHTMLTable(node,colClasses = classes)
          if(all(is.na(tab[,-1]))){
               return(-1)
          }
          
          
          # replace na's with 0
          tab[is.na(tab)] <- 0
          
          n <- nrow(tab)
          # remove first 3 empty rows, and last 2
          tab <- tab[4:(n-2),]
          
          # put column names
          temp <- grep("\\d{2}$",names(tab),value = TRUE)
          temp <- substr(temp,nchar(temp)-1,n)
          names(tab) <- c("variable",temp)

          # remove empty rows
          tab <- tab[!tab$variable=="",]
          tab$Company <- str[2]
          tab$Category <- category
          
          # first melt data
          tabm <- melt(tab,id.vars = c("Category","Company","variable"),
                       variable.name = "year",value.name = "value")
          # then cast it
          tabc <- dcast(tabm,Category+Company+year~variable,
                        value.var = "value",fun.aggregate = sum)
          tabc
     }
     
     # setwd("AuditorsReport/")
     
     for(i in iter:total){
          link <- allLinks[i]
          i <- i+1
          # verbose messages #
          cat(paste0("\n Iteration:",iter," ; ",round(iter/total*100),"% completed ..."))
          iter <- iter+1
          
          
          # get name of company and ticker symbol
          str <- unlist(strsplit(link,"/",fixed = TRUE))
          category <- str[6]
          str <- str[7:8]
          # generate auditor's report url
          l <- paste0("http://www.moneycontrol.com/annual-report/",
                      str[1],"/auditors-report/",str[2],"#",str[2])
         token <- TRUE
         while(token){
              res <-  try({
                   
                   
                   # go to auditor's page
                   doc <- htmlParse(l)
                   we <- getNodeSet(doc,"//td[@class='headcol11blue']")
                   
                   # get last audit year value
                   year <- xmlValue(we[[2]])
                   if(!grepl("\\d{2}$",year)){
                        # no auditor's report, so skip?
                        next()
                   }
                   
                   # convert year to integer to check latest or not
                   year <- as.integer(substr(year,nchar(year)-1,nchar(year)))
                   if(!year==15){
                        # old data skip it
                        next()
                   }
                   # to go to 2014 auditor's report
                   res <- try(dr$navigate(l))
                   Sys.sleep(5)
                  
                   we <- dr$findElement(using = 'xpath',"//a[@class='prevnext']")
                   tex <- we$getElementText()[[1]]
                   if(!grepl("\\d{2}$",tex))
                        next()
                   we$clickElement()
                   we2 <- dr$findElement(using = 'xpath',"//pre")
                   report <- we2$getElementText()
              })
              token <- inherits(res,'try-error')
         }
         
          # get auditor's report
          # we2 <- getNodeSet(doc,"//pre")
          # report <- xmlValue(we2[[1]])
          
          # write Auditor's report to file
          writeLines(report[[1]],paste0("AuditorsReport14/",str[2],".txt"))
          count <- count + 1
          cat(paste("Written records:",count))
          next() # cause I have already got the financials
          
          
          ###--------------------###
          ###--BALANCE-SHEET-----###
          ###--------------------###
          
          # now balance sheet, wow this is taking too much time
          l <- paste0("http://www.moneycontrol.com/financials/",
                      str[1],"/balance-sheetVI/",str[2])
          
          # go to balance sheet page
          doc <- htmlParse(l)
          tabc <- financials(doc,category,str)
          if(tabc==-1){
               file.remove(paste("AuditorsReport14/",str[2],".txt"))
               next()
          }
          # save it
          write.csv(tabc,file = paste0("Financials14/",str[2],"-bs.csv"))
          # fullBL <- bind_rows(fullBL,tabc)
          
          
          ###--------------------###
          ###--INCOME-STATEMENT--###
          ###--------------------###
          
          l <- paste0("http://www.moneycontrol.com/financials/",
                      str[1],"/profit-lossVI/",str[2])
          # dr$navigate(l)
          
          # go to income page
          doc <- htmlParse(l)
          tabc <- financials(doc,category,str)
          if(tabc==-1){
               file.remove(paste("AuditorsReport14/",str[2],".txt"))
               next()
          }
          write.csv(tabc,file = paste0("Financials14/",str[2],"-is.csv"))
          # fullIS <- bind_rows(fullIS,tabc)
          
          ###--------------------###
          ###------CASH-FLOW-----###
          ###--------------------###
          
          l <- paste0("http://www.moneycontrol.com/financials/",
                      str[1],"/cash-flowVI/",str[2])
          
          # Go to cash flow page
          doc <- htmlParse(l)
          tabc <- financials(doc,category,str)
          if(tabc==-1){
               next()
          }
          write.csv(tabc,file = paste0("Financials14/",str[2],"-cf.csv"))
          # fullCF <- bind_rows(fullCF,tabc)
          count <- count + 1
          cat(paste("Written records:",count))
     }
     
     # dr$closeall()
     
     # list(fullBL,fullIS,fullCF)
}




