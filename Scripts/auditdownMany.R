require(XML)
require(RSelenium)
# startServer()
pJS <- phantom()

# library(RBMproxy)
# startProxy()
# proxy <- bmProxy()
# proxy$closeAllPorts()
# proxy$start(9091)
# proxy$startHAR()
# eCap <- list(phantomjs.page.settings.loadImages = FALSE)
# proxy$getHAR()


dr <- remoteDriver(browserName = "firefox")
# dr <- remoteDriver()
# firefox hide all images permissions.default.image

# download auditor's report if on page and return report (as a list)
auditDown <- function(){
    token <- TRUE
    while(token){
        res <- try({
            we2 <- dr$findElement(using = 'xpath',"//pre")
            report <- we2$getElementText()
            cat(" where's the text!")
        })
        token <- inherits(res,'try-error')
    }
    report
}

# go to previous audit year if possible returns audit year, -1 if failure
auditPrev <- function(){
    token <- TRUE
    while(token){
        res <- try({
            we3 <- dr$findElements(using = 'xpath',"//a[@class='prevnext']")[[1]]
            tex <- we3$getElementText()[[1]]
            cat("  Trying to find button to click ...")
        })
        if(!grepl("\\d{2}$",tex)){ # no previous audit available
            tex=-1
            break
        }
        tex <- as.integer(substr(tex,nchar(tex)-1,nchar(tex)))
        token <- inherits(res,'try-error')
    }
    list(tex,we3)
}


# open connection fails for the first time usually so in loop
while(inherits(try(dr$open()),'try-error')){
    cat("\nCan't open the browser :( \n")
}


allLinks <- readLines("links14.txt")
iter <- 2484
total <- length(allLinks)
count <- 0
countn <- list(a=0,b=0,c=0)
ar <- "http://www.moneycontrol.com/annual-report/"

for(i in iter:total){
    link <- allLinks[i]
    i <- i+1
    # verbose messages #
    cat(paste0("\n Iteration:",iter," ; ",
               round(iter/total*100,3),
               "% completed ..."))
    iter <- iter+1
    
    # get name of company and ticker symbol
    str <- unlist(strsplit(link,"/",fixed = TRUE))
    category <- str[6]
    str <- str[7:8]
    
    # generate auditor's report url
    l <- paste0(ar,str[1],"/auditors-report/",str[2],"#",str[2])
    
    
    
    # this is the data we copied. Now copy older audit report.
    trial <- 0
    while(inherits(try(dr$navigate(l)),'try-error')){
        cat("\n Can't load page :( \n")
        trial <- trial+1
        if(trial==10)
            break()
    }
    # already at page which had 15 and 14 audit reports
    # Sys.sleep(2)
    
    # we are at 15, go to 14 first of all, we know it exists so no sanity checking required
    trial <- 0
    while(inherits(try(dr$findElement(using = 'xpath',"//a[@class='prevnext']")$clickElement()),'try-error')){
        trial <- trial+1
        if(trial==10)
            break()
    }
    
    Sys.sleep(2)
        
    # now at 14, want to download 13,12 and 11
    lastRecord <- 100
    for(j in 1:3){
        we3 <- list() # just blank element for now
        # cat(paste("\n      Trying to go back:",j))
        prev <- auditPrev()
        tex <- prev[[1]]
        we3 <- prev[[2]]
        if(tex==-1 | tex>lastRecord | tex<11 | tex==15 | tex==14) # no further audit, or only can go forward then skip
            break()
        # data exists, so go to it.
        we3$clickElement()
        cat("\n      Gone back, now try downloading...")
        Sys.sleep(2)
        report <- auditDown() # download audit
        cat("      Downloaded successfully...")
        # now to write it in correct folder
        writeLines(report[[1]],paste0("audit",tex,"/",str[2],".txt"))
        count <- count+1
        countn[[j]] <- countn[[j]]+1
        cat(paste("        Written records:",count,"for year",tex))
        lastRecord <- tex
    }
    cat(paste("\t[A:",countn[[1]],"][B:",countn[[2]],"][C:",countn[[3]],"]"))
}
