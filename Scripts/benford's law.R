# Benford's Law

bfData <- read.csv("model Data/fullData.csv")

bdist <- log10((1+1/(1:9)))
cbdist <- cumsum(bdist)

bford <- function(comp_name,srcData){
     dat <- subset(srcData,company == comp_name)
     dat <- dat[,-c(1:3,78)]
     dat <- abs(dat*100000)
     act_dist <- unlist(apply(dat,2,function(x) as.integer(substr(as.character(x),1,1))))
     act_dist <- factor(act_dist,levels = 0:9)
     counts <- table(act_dist)[-1]
     tcount <- sum(counts)
     ad <- counts/tcount
     cad <- cumsum(ad)
     dist_diff <- cad - cbdist
     ks <- max(dist_diff)
     cutoff <- 1.36/sqrt(tcount)
     return(ks>cutoff)
}


# compute frauds according to benford's law
frauds <- unlist(lapply(unique(bfData$company),bford,bfData))

fraudlist <- data.frame(company = unique(bfData$company),benford = frauds)

bfData <- merge(bfData,fraudlist,"company",all = FALSE)
table(bfData$fraud,bfData$benford,dnn = c("Auditor","Benford"))
write.csv(bfData,"model Data/Benford.csv",row.names = FALSE)


# Benford 2 digit approach with package
require(BenfordTests)
df <- read.csv("model Data/Benford.csv")
temp <- df[,4:77]

fraudp1 <- apply(temp,1,function(x) chisq.benftest(as.numeric(x),digits = 1)$p.value)
fraudp2 <- apply(temp,1,function(x) chisq.benftest(as.numeric(x),digits = 2)$p.value)

fraudpp2 <- apply(temp,1,function(x) jointdigit.benftest(as.numeric(x),digits = 2)$p.value)



df$bfp1 <- fraudp1
df$bfp2 <- fraudp2
df$bfc <- fraudpp2
nds <- read.csv("model Data/newVarMScore.csv")
tp <- df[,c(1,3,79:82)]


combo <- merge(nds,tp,by = c("year","company"),all = FALSE)
write.csv(combo,"model Data/BenfordReduced.csv")
