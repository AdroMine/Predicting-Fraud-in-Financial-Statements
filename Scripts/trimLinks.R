lab <- logical(length = length(allLinks))
files <- list.files("AuditorsReport14/")
files <- gsub("\\.txt","",files)
allLinks <- readLines("allLinks.txt")
for(i in seq_along(allLinks)){
    link <- allLinks[i]
    str <- unlist(strsplit(link,"/",fixed = TRUE))[8]
    if(str %in% files)
        lab[i] <- TRUE
}
# used it to remove files from allLinks.txt that were not used to download data.