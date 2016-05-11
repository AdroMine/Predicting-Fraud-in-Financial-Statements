# Retrieve Links
require(Rselenium)
startServer()
dr <- remoteDriver()
dr$open()

# add LETTERS A-Z at the end of this to get different pages
linkbase <- "http://www.moneycontrol.com/india/stockmarket/pricechartquote/"
# total number of companies in their database
count <- 0

# for each letter, open page, and read all URLs and write them to file
for(letter in LETTERS){
     print(paste("Letter:",letter,"...")) # verbose message
     dr$navigate(paste0(linkbase,letter)) # go to page
     we <- dr$findElements(using = 'xpath',"//a[@class='bl_12']") # find urls
     # retrieve urls
     urls <- sapply(we,function(x) as.character(x$getElementAttribute("href"))) 
     # increase count
     count <- count + length(urls)
     writeLines(urls,paste0("StockLinks/",letter,".txt")) # write to file
     print(paste("Count = ",count)) # verbose message
}
# don't forget to close connection. Be a good boy!
dr$close()

# put all links together could have done it earlier too
allLinks <- character()
setwd("StockLinks/")
# read each file, and combine links
for(file in list.files()){
     allLinks <- c(allLinks,readLines(file))
}
# many links were blank, remove them (30 removed)
allLinks <- allLinks[-grep("^$",allLinks)]
writeLines(allLinks,"allLinks.txt") # write all links to one file
setwd("..")
