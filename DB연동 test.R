#########################################################################################################################
### Project  : NIPA - Revision Log
### Script   : Prediction()_new.R
### Contents : Revision Log Recommendation
#########################################################################################################################

#########################################################################################################################
### Setting up environment
#########################################################################################################################

# Load library  
pkgs <- c("tidyr", "arules", "caret")
install.packages(pkgs)
sapply(pkgs, require, character.only = T)

# Parameters
p.names <- c("SHIP_NO", "DWG_TYPE", "DWG_BLOCK", "DWG_PROC", "DWG_STAGE", "RSN_CD")
p.spt   <- 0.01
p.cfd   <- 0.01

# Load data from DB

#####################################################################################
### Project: MongoDB connection
### Content: MongoDB connection using URI format and save the data
### Script : MongoDB.R
#####################################################################################

#####################################################################################
### Set up the environment
#####################################################################################

# Load the package
install.packages("mongolite")
library("mongolite")

#####################################################################################
### Make the URI
#####################################################################################

username <- "skku"
password <- "skku%40AI%21205"
hostname <- "localhost"
portname <- "29835"
option   <- "?serverSelectionTimeoutMS=5000&connectTimeoutMS=10000&authSource=AI&authMechanism=SCRAM-SHA-1"
uri      <- paste0("mongodb://", username, ":", password, "@", hostname, ":", portname,
                   "/", option, sep="")


#####################################################################################
### Connect to DB through URI format
#####################################################################################

# Connect the database through URI format
db <- mongo(collection="RevInfo", # name of table
            db="AI",              # name of database
            url=uri)

#####################################################################################
### Save the data into csv file
#####################################################################################

# Save the data into data frame
df.raw <- db$find()

# Check the data frame
head(df.raw)
str(df.raw)
colSums(is.na(df.raw))

# Save the dataframe into csv in working directory
write.csv(df.raw, "./RevInfo.csv", row.names=FALSE)

df.arm         <- df[ ,p.names]  #DB에서 가져온 데이터를 p.names의 컬럼만 가져옴
df.arm$SHIP_NO <- as.factor(substr(df.arm$SHIP_NO, 1, 1))


#########################################################################################################################
### Analysis
#########################################################################################################################

n.rules <- nrow(df.arm) 
df.eff  <- as(df.arm[sample(1:nrow(df.arm), n.rules),], "transactions")

########################
#### Generate Rules ####
########################

RSN_CD    <- grep("^RSN_CD=", itemLabels(df.eff), value = T)


res.rules <- apriori(data = df.eff, parameter = list(support = 0.00001, confidence = 0.01, minlen = 5), 
                     appearance = list(rhs = RSN_CD), control = list(verbose = F))

#res.rules1 <- sort(res.rules, by = c('confidence','support'))

#### Convert rules into DF ####
df.lhs   <- as(inspect(lhs(res.rules)), "data.frame")
df.rhs  <- as(inspect(rhs(res.rules)), "data.frame")


df.rules <- matrix(NA, dim(df.rhs)[1], length(p.names), dimnames = list(NULL, p.names))
for(i in p.names){
  print(i)
  if(i == "RSN_CD"){
    df.rules[, i] <- apply(df.rhs, 1, function(x) substr(sub(".*=", "", x), 1, 3))
  }else{
    id.sn  <- which(apply(df.lhs, 1, function(x) grepl(i, x)))
    n.temp <- rep(NA, length(id.sn))
    
    for(j in id.sn){
      temp.str  <- strsplit(as.character(df.lhs[j,]), ",")
      id.str    <- unlist(lapply(temp.str, function(x) grep(i, x)))
      n.temp[j] <- substr(sub(".*=", "", unlist(temp.str)[id.str]), 1, ifelse(i == "DWG_BLOCK", 2, 1))
    }
    
    df.rules[, i] <- n.temp  
  }
} # df.rules generation process
df.rules <- apply(df.rules,2, function(x) gsub('[{}]', '', x)) 



###############################################
#### Data Coverage (Building a classifier) ####
###############################################


p <-df.arm
qq <- (df.rules)
qq <- cbind(qq, (quality(res.rules)))
qq[11] <- apply(qq, 1, function(x) sqrt(as.numeric(x[7])*as.numeric(x[8]))) ## conf와 supp의 geometric average값 계산
qq[12] <- apply(qq, 1, function(x) sum('NA' != x[1:5], na.rm= T))
qq <- qq[order(qq$V11, decreasing = T),] #내림차순으로 rank 
qqq <- qq[1:6]
q <- df.rules

p1 <- as.matrix(p)
qqq1 <- as.matrix(qqq)

df.classifier1 <- data.frame()


### building a classifier ###
pb <- progress::progress_bar$new(total = nrow(df.rules))

system.time(for (i in 1:nrow(df.rules)){
  count   <- apply(p1, 1, function(x){
    sum(x != qqq1[i, ], na.rm = T)
  })
  if (min(count) == 0){
    p1    <- p1[-(which(count == 0)), ]
    df.classifier1   <- rbind(df.classifier1, as.data.frame(qq[i,]))
  }
  pb$tick()

})



#### Classifier 기반 추천 ####
zz <- df.classifier1[, -(6:12)]
prediction.RSN_CD <- function(SHIP_NO = NULL, DWG_TYPE = NULL, DWG_BLOCK =NULL, DWG_PROC = NULL, DWG_STGE = NULL, classifier = zz, RSN_DESC = 'both'){
  
  y    <- c(SHIP_NO, DWG_TYPE, DWG_BLOCK, DWG_PROC, DWG_STGE)
  df.less_match <- NULL
  df.max_match <- NULL
  
  count      <- apply(zz, 1, function(x) sum(x == y, na.rm = T))
  
  if( sum(count) == 0){
    print('No match')
  }
  count.max <- max(count)
  df.rules.count <- cbind(df.classifier1, count)
  df.rules.count <- df.rules.count[,c(1,2,3,4,5,6,13,7,8,9,10,11,12)]
  
  df.max_match   <- subset(df.rules.count, as.data.frame(df.rules.count)[7] == count.max) # test case와 최대매칭 되는거 찾기
  
  df.max_match.sub2 <- data.frame()
  
  #RSN_CD별로 grouping?, RSN_CD그룹중 가장 순위가 높은 애들만 모아 두기 
  for (i in 1:nrow(unique(df.max_match[6]))){
    df.max_match.sub <- data.frame()
    for (j in 1:nrow(df.max_match)){
      if(df.max_match[j,6] == unique(df.max_match[6])[i, ]){
        df.max_match.sub <- rbind(df.max_match.sub, df.max_match[j,])
      }
    }
    df.max_match.sub2 <- rbind(df.max_match.sub2, subset(df.max_match.sub, df.max_match.sub[12] == max(df.max_match.sub[12]))[1,])
  }
  
  
  if (nrow(df.max_match.sub2) >= 3) { #여기도 3대신1로 바꿈  
    df.max_match <- df.max_match.sub2[order(-df.max_match.sub2[12]),][1:3,] #1개 추천 (원래 3) # V12(supp와 conf의 조합)이 가장 큰걸 추천 
  } else {
    df.max_match <- df.max_match.sub2
  }
  
  n.max <- 3 - nrow(df.max_match)
  
  df.less_match.sub2 <- data.frame()
  
  # Second most mathces
  if(nrow(df.max_match) == 1 & count.max != 0){ 
    df.less_match <- subset(df.rules.count, as.data.frame(df.rules.count)[7] == (count.max - 1))
    if (nrow(df.less_match) != 0){
      df.less_match <- subset(df.less_match, df.less_match[6] != (as.character(df.max_match[1,6])))
    }
    if (nrow(unique(df.less_match[6])) < 2){
      df.less_match <- rbind(df.less_match, subset(df.rules.count, as.data.frame(df.rules.count)[7] == (count.max - 2)))
      if (nrow(df.less_match) != 0){
        df.less_match <- subset(df.less_match, df.less_match[6] != (as.character(df.max_match[1,6])))
      }
    }
    df.less_match[12] <- apply(df.less_match, 1, function(x) sqrt(as.numeric(x[8])*as.numeric(x[9])))
    
    if (nrow(df.less_match) != 0 ){
      for (i in 1:nrow(unique(df.less_match[6]))){
        df.less_match.sub <- data.frame()
        for (j in 1:nrow(df.less_match)){
          if(df.less_match[j,6] == unique(df.less_match[6])[i,]){
            df.less_match.sub <- rbind(df.less_match.sub, df.less_match[j,])
          }
        }
        df.less_match.sub2 <- rbind(df.less_match.sub2, subset(df.less_match.sub, df.less_match.sub[12] == max(df.less_match.sub[12]))[1,])
      }
    }
    if (nrow(df.less_match.sub2) !=0 ){
      df.less_match     <- df.less_match.sub2[order(-df.less_match.sub2[12]),][1:n.max,]
    }
  } else if(nrow(df.max_match) == 2 & count.max != 1){
    df.less_match <- subset(df.rules.count, as.data.frame(df.rules.count)[7] == (count.max - 1))
    if (nrow(df.less_match) != 0){
      df.less_match <- subset(df.less_match, df.less_match[6] != (as.character(df.max_match[1,6])))
    }
    if (nrow(df.less_match) != 0){
      df.less_match <- subset(df.less_match, df.less_match[6] != (as.character(df.max_match[2,6])))
    }
    if (nrow(unique(df.less_match[6])) < 2){
      df.less_match <- rbind(df.less_match, subset(df.rules.count, as.data.frame(df.rules.count)[7] == (count.max - 2)))
      if (nrow(df.less_match) != 0){
        df.less_match <- subset(df.less_match, df.less_match[6] != (as.character(df.max_match[1,6])))
        df.less_match <- subset(df.less_match, df.less_match[6] != (as.character(df.max_match[2,6])))
      }
    }
    df.less_match[12] <- apply(df.less_match, 1, function(x) sqrt(as.numeric(x[8])*as.numeric(x[9]))) 
    if (nrow(df.less_match) != 0 ){
      for (i in 1:nrow(unique(df.less_match[6]))){ #df.less_match에서 개정코드 별로 또 나눠서 탑티어만 가져오기
        df.less_match.sub <- data.frame()
        for (j in 1:nrow(df.less_match)){
          if(df.less_match[j,6] == unique(df.less_match[6])[i,]){
            df.less_match.sub <- rbind(df.less_match.sub, df.less_match[j,])
          }
        }
        df.less_match.sub2 <- rbind(df.less_match.sub2, subset(df.less_match.sub, df.less_match.sub[12] == max(df.less_match.sub[12]))[1,])
      }
      if (nrow(df.less_match.sub2) !=0 ){
        df.less_match     <- df.less_match.sub2[order(-df.less_match.sub2[12]),][1:n.max,]
      }
    }
  }
  
  df.recomm  <- rbind(df.max_match, df.less_match)
  
  df.recomm  <- subset(df.recomm, !is.na(df.recomm$support))
  
  recomm.RSN <- df.recomm[order(-df.recomm[12]),][1:3,6] # 1:3 을 [6] 으로 바꿈
  
  ##################
  ### 개정 사유 ####
  ##################
    result  <- vector('list', 1)
    result1 <- vector('list', 1)
    result2 <- vector('list', 1)
    for (i in 1:nrow(df.recomm[6])){
    y <- df.recomm[i,6]
    DESC.names   <- c('REV_RSN_DESC','CNFM_MH')
    DESC.i       <- df.raw[df.raw[5] == as.character(y), DESC.names]
    if (RSN_DESC == 'freq'){
    DESC.top3    <- sort(table(DESC.i[1]), decreasing = T)[1:3]# Top3 based on freq
    DESC.top3    <- as.data.frame(DESC.top3) 
    colnames(DESC.top3) <- c('REV_RSN_DESC','Freq')
    }
    else if(RSN_DESC == 'HM'){
    DESC.top3 <- DESC.i[order(DESC.i[2], decreasing = T),][1:3,] #Top3 based on HM
    }
    else if(RSN_DESC == 'both'){
      DESC.top3    <- sort(table(DESC.i[1]), decreasing = T)[1:3]# Top3 based on freq
      DESC.top3.freq    <- as.data.frame(DESC.top3) 
      colnames(DESC.top3.freq) <- c('REV_RSN_DESC','Freq')
      DESC.top3.HM <- DESC.i[order(DESC.i[2], decreasing = T),][1:3,]
    }
    if (RSN_DESC == 'both'){
    result1[[i]] <- list(as.character(df.recomm[i,6]), DESC.top3.freq)
    result2[[i]] <- list(as.character(df.recomm[i,6]), DESC.top3.HM)
    } else {
      result[[i]] <- list(as.character(df.recomm[i,6]), DESC.top3)
    }
    }
    
  result  <- as.data.frame(result)
  result1 <- as.data.frame(result1)
  result2 <- as.data.frame(result2)
  
  if (RSN_DESC == 'both'){
  for (j in 1:2){
  if (j == 1){
    colname <- c()
  for ( i in 1:nrow(df.recomm)){
    a <- as.character(df.recomm[i,6])
    b <- paste0('REV_RSN_DESC:',a)
    c <- 'freq'
    colname <- c(colname,a,b,c)
  }
    colnames(result1) <- colname
  } 
  if ( j == 2){
    colname <- c()
    for ( i in 1:nrow(df.recomm)){
      a <- as.character(df.recomm[i,6])
      b <- paste0('REV_RSN_DESC:',a)
      c <- 'HM'
      colname <- c(colname,a,b,c)
    }
    colnames(result2) <- colname
    result.list <- list(df.recomm, result1, result2)
  } 
    }
  } else {
    colname <- c()
    for ( i in 1:nrow(df.recomm)){
      a <- as.character(df.recomm[i,6])
      b <- paste0('REV_RSN_DESC:',a)
      c <- RSN_DESC
      colname <- c(colname,a,b,c)
    }
    colnames(result) <- colname
    result.list <- list(df.recomm, result)
  }
  return(result.list)
}

aa <- prediction.RSN_CD(4,'W','A4','T','B',RSN_DESC = 'HM') #e.g
