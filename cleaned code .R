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
sapply(pkgs, require, character.only = T)

# Parameters
p.names <- c("SHIP_NO", "DWG_TYPE", "DWG_BLOCK", "DWG_PROC", "DWG_STAGE", "RSN_CD")
p.spt   <- 0.01
p.cfd   <- 0.01

# Load data
df.raw         <- read.csv(url('http://bit.ly/2FOA3Q6'), header = T, sep = ',')
df.arm         <- df.raw[, p.names]
df.arm$SHIP_NO <- as.factor(substr(df.arm$SHIP_NO, 1, 1))


#########################################################################################################################
### Analysis
#########################################################################################################################

# DF for Rule extraction
df.arm.RSN_CD <- unique(df.arm$RSN_CD)

df.arm.train  <- data.frame()
df.arm.test   <- data.frame()
# Divide into Training and Test (Considering RSN_CD distribution)
for (code in df.arm.RSN_CD){
  aa  <- subset(df.arm, code == df.arm$RSN_CD)
  aa  <- aa[sample(nrow(aa)), ]
  row <- nrow(aa)*0.7
  row_1 <- as.integer(row) + 1
  
  df.arm.train <-  rbind(df.arm.train, aa[1:as.integer(row), ])
  df.arm.test  <-  rbind(df.arm.test, aa[(row_1):nrow(aa), ])
}

n.rules <- nrow(df.arm.train) # or n

df.eff  <- as(df.arm.train[sample(1:nrow(df.arm.train), n.rules),], "transactions")

# Generate Rules
RSN_CD    <- grep("^RSN_CD=", itemLabels(df.eff), value = T)
res.rules <- apriori(data = df.eff, parameter = list(support = p.spt, confidence = p.cfd), 
                     appearance = list(rhs = RSN_CD), control = list(verbose = F))

# Convert rules into DF
df.lhs   <- as(inspect(lhs(res.rules)), "data.frame")
df.rhs   <- as(inspect(rhs(res.rules)), "data.frame")
df.rules <- matrix(NA, dim(df.rhs)[1], length(p.names), dimnames = list(NULL, p.names))

for(i in p.names){
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
}

# Predict (based on counts as of now)
key   <- df.arm[sample(1:nrow(df.arm), 1),]

count <- apply(data.frame(df.rules)[,-6], 1, function(x) sum(x == key[,-6], na.rm = T))
# df.rules + quality
cm1 <- character()
cm2 <- character()

# Recommendation process
system.time(df.arm.test[7] <- apply(df.arm.test, 1, function(x){
  y          <- x
  count      <- apply(data.frame(df.rules)[,-6], 1, function(x) sum(x == y, na.rm = T))
  top5       <- cbind(df.rules, quality(res.rules))[order(-count),][1:5,]
  df.recomm  <- top5[order(-top5[8]),][1:3,] #3
  recomm.RSN <- top5[order(-top5[8]),][1:3,6] #3
  x[7]       <- unlist(as.list(x[6])) %in% unlist(as.list(recomm.RSN))
  x[8]       <- paste(recomm.RSN, collapse = ',')
}))



# Find out T/F
df.arm.test[8] <- apply(df.arm.test, 1, function(x) x[6] %in% unlist(strsplit(x[7],',')) )

# Accuracy calculation
top <- length(which(df.arm.test[8] == TRUE))
bottom <- nrow(df.arm.test)
acc <- top / bottom
print(acc)

#########################################################################################################################
### Increasing Accuracy 
#########################################################################################################################

#Read 'Order NO. of con'
df.raw_1 <- read.csv(url('http://bit.ly/2OHvXfw'), header = T, sep =',')
df.raw_1 <- df.raw_1[,2:8]
df.raw_1 <- df.raw_1[df.raw_1$No.of.diff.RSN_CD > 10,] #몇개 이상부터 consider해야하는지에 대한 scientific한 기준?

# Generate rules for each combination in training set
system.time(for(i in 1:nrow(df.raw_1)){
  df.i <- apply(df.arm.train[,1:6], 1, function(x){
    if(sum(x[1:5] == df.raw_1[i,1:5]) == 5){
      return(x)
    }
  })
  
  df.i <- df.i[df.i != 'NULL']
  df.i <- data.frame(matrix(unlist(df.i), nrow=length(df.i), byrow=T))
  
  colnames(df.i) <- p.names
 
  df.eff_i    <- as(df.i[sample(1:nrow(df.i), nrow(df.i)),], "transactions")
  
  RSN_CD_i    <- grep("^RSN_CD=", itemLabels(df.eff_i), value = T)
  # Generate Rules
  res.rules_i <- apriori(data = df.eff_i, parameter = list(support = 0.1, confidence = 0.1, minlen =6), 
                       appearance = list(rhs = RSN_CD_i), control = list(verbose = F))
  
  # Convert rules into DF
  df.lhs_i   <- as(inspect(lhs(res.rules_i)), "data.frame")
  df.rhs_i   <- as(inspect(rhs(res.rules_i)), "data.frame")
  res.rules  <- c(res.rules, res.rules_i)
  df.rules_i <- matrix(NA, dim(df.rhs_i)[1], length(p.names), dimnames = list(NULL, p.names))
  
  for(j in p.names){
    if(j == "RSN_CD"){
      df.rules_i[, j] <- apply(df.rhs_i, 1, function(x) substr(sub(".*=", "", x), 1, 3))
    }else{
      id.sn  <- which(apply(df.lhs_i, 1, function(x) grepl(j, x)))
      n.temp <- rep(NA, length(id.sn))
      for(k in id.sn){
        temp.str  <- strsplit(as.character(df.lhs_i[k,]), ",")
        id.str    <- unlist(lapply(temp.str, function(x) grep(j, x)))
        n.temp[k] <- substr(sub(".*=", "", unlist(temp.str)[id.str]), 1, ifelse(j == "DWG_BLOCK", 2, 1))
      }
      df.rules_i[, j] <- n.temp  
    }
  }
  df.rules <- rbind(df.rules, df.rules_i)
})



# Confusion matrix generation
cm1 <- apply(df.arm.test[7],1, function(x) return(x)) # Recommended RSN_CD
cm2 <- apply(df.arm.test[6],1, function(x) return(x)) # Actual RSN_CD
cm <- table(cm2,cm1)

##--###

system.time(for (i in 1:nrow(df.arm.test)){
  print(df.arm.test[i,])
  count <- apply(data.frame(df.rules)[,-6], 1, function(x) sum(x == df.arm.test[i,], na.rm = T))
  top5  <- cbind(df.rules, quality(res.rules))[order(-count),][1:5,]
  df.recomm  <- top5[order(-top5[8]),][1:3,]
  recomm.RSN <- top5[order(-top5[8]),][1:3,6]
  
  df.arm.test[i,7]     <- unlist(as.list(df.arm.test[i,6])) %in% unlist(as.list(recomm.RSN)) # 이게 좀 차지 하지 안ㄹ으ㅡㄹ까
  print('끝')
 })

top <- length(which(df.arm.test[7] == TRUE))
bottom <- nrow(df.arm.test)
acc <- top / bottom
print(acc)
cm <- confusionMatrix(cm1,cm2)


cbind(df.rules, quality(res.rules))[order(),][1:5,]

for (i in recomm.RSN){
  cm1 <- c(cm1,i)
}
for ( i in 1:nrow(df.arm.train)){
  if (sum(a == df.arm.train[i,1:5]) == 5){
   print(df.arm.train[i,]) 
  }
}
xx <- apply(df.arm.train[,1:6], 1, function(x){
  if(sum(x[1:5] == a) == 5){
    return(x)
  }
})
