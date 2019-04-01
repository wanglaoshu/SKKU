data <- read.csv(url('http://bit.ly/2I4TPs0'), header=TRUE, sep=',')
pkgs <- c("tidyr", "arules",'caret')
install.packages(pkgs)
sapply(pkgs, require, character.only = T)
install.packages('tidyr')
#Randomly shuffle my data
#set.seed(1)
#data <- data[sample(nrow(data)), ]


### Prediction () ##################################################################################################

prediction <- function(data = NULL, Support = 0.001, Confidence = 0.4, a = NULL, b = NULL, c = NULL, d = NULL, e = NULL, K = NULL, result = 1, examination = FALSE){
  
  # Data processing before generating rules 
  data <- data[,c("SHIP_NO",'RSN_CD','DWG_TYPE','DWG_BLOCK','DWG_PROC','DWG_STAGE')] #Save the required columns only
  data$SHIP_NO <- substr(data$SHIP_NO,1,1) 
  data$SHIP_NO <- sapply(data$SHIP_NO, as.factor)
  data <- subset(data, data$RSN_CD != '3_1')
  #### Order by RSN_CD ####
  data <- data[order(data$RSN_CD),]
  RSN_code <- unique(data$RSN_CD)
  
  #Create 10 equally sized fold 
  #folds <- cut(seq(1,nrow(data)), breaks = K, labels = FALSE)
  
  #kfold_recommendation <- data.frame()
  #kfold_recommendation_rule <- data.frame()
  acc_p <- list() # list to 
  if (is.null(K)){
    K = 1
  }
  cm1 <- character()
  cm2 <- character()
  # Rule generation 
  for (i in 1:K){ # for loop for K-fold CV. Only run once if K is not given by the user. 
    #Create lists, data frames
    data_train <- data.frame()
    data_test <- data.frame()
    
    # Divide the data into 7:3 when K is 1. Otherwise, do the K-fold CV
    if (K == 1){
      for (code in RSN_code){
        aa <- subset(data, code == data$RSN_CD)
        aa <- aa[sample(nrow(aa)), ]
        row <- nrow(aa)*0.7
        row_1 <- as.integer(row) + 1
        data_train <- rbind(data_train, aa[1:as.integer(row), ])
        data_test <- rbind(data_test, aa[(row_1):nrow(aa), ])
      }
      data_1 <- data_test
      data_train <- as(data_train, 'transactions')
      data_test <- as(data_test, 'transactions')
    } else {
      for (code in RSN_code){
        aa <- subset(data, code == data$RSN_CD)
        aa <- aa[sample(nrow(aa)), ]
        folds <- cut(seq(1,nrow(aa)), breaks = K, labels = FALSE)
        test_index <- which(folds == i, arr.ind = TRUE)
        data_test <- rbind(data_test, aa[test_index, ])
        data_train <- rbind(data_train, aa[-test_index, ])
      }
      data_1 <- data_test
      data_train <- as(data_train, 'transactions')
      data_test <- as(data_test, 'transactions')
    }
    #print(nrow(data_train))
    #print(nrow(data_test))
    #print(nrow(siba))
    #print(nrow(suba))
    #print('흠')
    RSN <- grep("^RSN_CD=",itemLabels(data_train), value = TRUE)
    ################
    # Generate rules (apriori)
    ################
    rules <- apriori(data = data_train, parameter = list(support = 0.00000001, confidence = 0.01), 
                     appearance = list(rhs = RSN), control = list(verbose = FALSE))
    quality(rules) <- cbind(quality(rules), CF = interestMeasure(rules, measure = "certainty", trans = data))
    quality(rules) <- cbind(quality(rules), CV = interestMeasure(rules, measure = "conviction", trans = data))
    
    rules_df <- as(rules, 'data.frame') # Save the rules as data.frame
    rules_df <- separate(data = rules_df, col = rules, into = c("Input", 'Output'), sep = '=>')
    x <- rules_df
    
    # Put the inputs into a list

    #exam_pred <- data.frame(matrix(ncol = 1, nrow = nrow(data_2)))
    
    ### Data-train이 잘 만들어졌다는 전제 하에...###
    ################################################
    data_1df <- data_1
    data_1df <- data_1df[,c(1,3,4,5,6,2)]
    data_1 <- as(data_test, 'data.frame')
    data_1 <- apply(data_1[1], 1, function(x){
      first_proc <- gsub('[{}]', '', x)
      first_proc <- gsub(' ', '', first_proc)
      second_proc <- strsplit(first_proc, ',')
    })
    
    y <- x #Rule set
    #Rules####
    s <- apply(y[1], 1, function(x){
      first_proc <- gsub('[{}]', '', x)
      first_proc <- gsub(' ', '', first_proc)
      second_proc <- strsplit(first_proc, ',')
    }) 
    
    system.time(for (i in 1:length(data_1)){
      ui <- unlist(data_1[i]) # training set에서의 i번쨰 조합
      
      # ruleset에서의 ui가 얼마나 매칭되는지
      system.time(t <- lapply(s, function(x){
        a <- unlist(x)
        return(length(which(a %in% ui)))
      }))
      
      t <- unlist(t)
      no_match <- as.data.frame(t)
      z <- cbind(y, no_match)
      z <- z[order(z[9], decreasing = TRUE),]
      z <- subset(z, z[9] == max(z[9]))
      z <- z[order(z[4], decreasing = TRUE),]
      recommendation <- z[1:3,] #한개 추천 (추후에 1 대신 파라미터로)
      recommendation <- recommendation[2]
      recommendation <- as(recommendation,'list')
      recommendation <- unlist(recommendation)
      
      first_proc <- gsub('[{}]', '', recommendation)
      first_proc <- gsub(' ', '', first_proc)
      recommendation <- gsub('RSN_CD=', '', first_proc)
      data_1df[i,7] <- data_1df[i,6] %in% recommendation
      cm1 <- c(cm1, recommendation)
      cm2 <- c(cm2, as.character(data_1df[i,6]))
      #### 퓨전 만들어보장 ######
    })
    
    
    ###### Accuracy calc #########
    ##############################
    top <- length(which(data_1df[7] == TRUE))
    bottom <- nrow(data_1df)
    acc <- top / bottom
    print(acc)
    cm <- confusionMatrix(cm1,cm2)
    # print(cm))
    #일단 매칭해서 데이터프레임으로 결합까지는 함
  }
}

#bb <- prediction(data = data, Support = 0.0000001, Confidence = 0.01,'SHIP_NO=2','DWG_BLOCK=S1',result = 2, examination = TRUE)



    
##### 연습용 ############################    
    y[9] <- x[]
    data_1 <- lapply(data_1, unlist)
    input <- data_2[k]
    input <- as(input,'data.frame')
    first_proc <- gsub('[{}]', '', input[,1])
    first_proc <- gsub(' ', '', first_proc)
    second_proc <- strsplit(first_proc, ',')
    input <- unlist(second_proc)
    data_2 <- data.frame(matrix(ncol = 2, nrow = nrow(data_1)))
    

    
    for ( i in 1:nrow(ut_df)){
      ut_df[i,1] <- gsub('[{}]', '', ut_df[i,1])
      ut_df[i,1] <- gsub(' ', '', ut_df[i,1])
      ut_df[i,1] <- strsplit(ut_df[i,1], ',')
    }
    
    ut_df[1] <- gsub('[{}]', '', ut_df[1])
    first_proc <- gsub(' ', '', first_proc)
    second_proc <- strsplit(first_proc, ',')
    
    ut_df_l <- sapply(ut_df[1], function(x){
      first_proc <- gsub('[{}]', '', x)
    })
    
    idk <- data.frame(matrix(ncol=3))
    for ( i in 1:nrow(ut_df_u)){
      a <- subset(ut_df, ut_df[1] == ut_df_u[i,1])
      b <- as(a[2],'list')
      b <- unlist(b)
      b <- paste(b, collapse =',')
      idk[i,1] <- ut_df_u[i,1] 
      idk[i,2] <- nrow(a)
      idk[i,3] <- b
      }  
    count <- apply()
    
    data_t <- data[,c(1,3,4,5,6,2)]
    data_t <- as(data_t, 'transactions')
    data_t_df <- as(data_t, 'data.frame')
    data_t_df_l <- sapply(data_t_df[1], function(x){
      first_proc <- gsub('[{}]', '', x)
    })
    data_t_df <- as.data.frame(data_t_df_l)
    data_t_df <- separate(data = data_t_df, col = items, into = c("Ant", 'Con'), sep = ',RSN_CD=')
    b <- subset()
    table[2]
    
    c <- apply(b, 1, function(x){
      paste()
    })
    CON <- grep("^Con=",itemLabels(c_t), value = TRUE)
    rr <- apriori(data = c_t, parameter = list(support = 0.0001, confidence = 0.1), appearance = list(rhs = CON))
    
    system.time(for (i in 1:length(data_1)){
      ui <- unlist(data_1[i])})
    
    xxx <- sapply(xxx, function(x){
      a <- gsub('[{}]','',x)
    })
    xxx[6] <- sapply(xxx[6], function(x){
      a <- gsub('RSN_CD=','',x)
    })
    
    colnames(xxx) <- c('SHIP_NO','DWG_TYPE','DWG_BLOCK','DWG_PROC',"DWG_STAGE",'RSN_CD')
    
    library(stringr)
    install.packages(splitstackshape)
    library(splitstackshape)
    xxx <- separate(xxz, col = newcol, into = c('1','2','3','4','5','6'), sep = ',')
    
    
  
    xxx <- cbind(xx[1],xx[2])
    xxx <- unite(xxx, newcol, c(Input, Output), sep = ',')
    xxx <- sapply(xxx, function(x){
      a <- gsub('[{}]','',x)
    })
    xxz <- as.data.frame(xxx)
    xxx <- separate(xxz, col = newcol, into = c('1','2','3','4','5','6'), sep = ',')
    
    yyy <- data.frame()
    for ( i in 1:6){
      yyy[1,i] <- a[i]
    }
    apply(xxx, 1, function(x) sum(x == ui_df))
  
