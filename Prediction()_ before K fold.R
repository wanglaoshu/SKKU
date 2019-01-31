
library(tidyr)
library(arules)
data <- read.csv("/Users/heeyoungpark/Desktop/연구실 stuff/final_dataset_csv.csv", header=TRUE, sep=',')

prediction <- function(data = NULL, Support = NULL, Confidence = NULL, a = NULL, b = NULL, c = NULL, d = NULL, e = NULL){
  #Create lists, data frames
  
  list_input <- list()
  list_r <- list()
  list_i <- list()
  list_conf <- list()
  list_supp <- list()
  list_lift <- list()
  list_c <- list()
  list_s <- list()
  df_con <- data.frame()
  
  
  # Data processing before generating rules 
  print('오잉')
  data <- data[,c("SHIP_NO",'RSN_CD','DWG_TYPE','DWG_BLOCK','DWG_PROC','DWG_STAGE')]
  data$SHIP_NO <- substr(data$SHIP_NO,1,1)
  data$SHIP_NO <- sapply(data$SHIP_NO, as.factor)
  data <- as(data,'transactions')
  RSN <- grep("^RSN_CD=",itemLabels(data), value = TRUE)
  
  #Randomly shuffle my data
  train_pct <- 0.7     # Training amount
  data_test <- data[as.integer(nrow(data)*train_pct):nrow(data),]
  
  # Rule generation 
  
  rules <- apriori(data = data[1:nrow(data)*train_pct,], parameter = list(support = Support, confidence = Confidence), 
                   appearance = list(rhs = RSN))
  rules_df <- as(rules, 'data.frame')
  rules_df <- separate(data = rules_df, col = rules, into = c("Input", 'Output'), sep = '=>')
  x <- rules_df
  
  # Put the inputs into a list
  
  input <- c(a, b, c, d, e)
  print(input)
  for (p in 1:length(input)){
    if(is.null(input[p]) == FALSE){
      list_input[p] <- input[p]
    }
  }
  i <- length(list_input)
  
  # Do the matching
  
  for (j in 1:nrow(x)){
    if(i == 1){
      if(grepl(list_input[1], x[j,1]) == TRUE){
        list_c <- c(list_c, x[j,2])
        list_i <- c(list_i, x[j,1])
        list_conf <- c(list_conf, x[j, 4])
        list_supp <- c(list_supp, x[j, 3])
        list_lift <- c(list_lift, x[j, 5])
      }
    } else if(i == 2){
      if(grepl(list_input[1], x[j,1]) & grepl(list_input[2], x[j,1]) 
         | grepl(list_input[1], x[j,1]) | grepl(list_input[2], x[j,1]) == TRUE){
        list_c <- c(list_c, x[j, 2])
        list_i <- c(list_i, x[j, 1])
        list_conf <- c(list_conf, x[j, 4])
        list_supp <- c(list_supp, x[j, 3])
        list_lift <- c(list_lift, x[j, 5])
      }
    } else if(i == 3){
      if(grepl(list_input[1], j) & grepl(list_input[2], x[j,1]) & grepl(list_input[3], x[j,1]) 
         | grepl(list_input[1], x[j,1]) & grepl(list_input[2], x[j,1]) 
         | grepl(list_input[1], x[j,1]) & grepl(list_input[3], x[j,1]) 
         | grepl(list_input[2], x[j,1]) & grepl(list_input[3], x[j,1]) 
         | grepl(list_input[1], x[j,1]) | grepl(list_input[2], x[j,1]) | grepl(list_input[3], x[j,1]) == TRUE){
        list_c <- c(list_c, x[j, 2])
        list_i <- c(list_i, x[j, 1])
        list_conf <- c(list_conf, x[j, 4])
        list_supp <- c(list_supp, x[j, 3])
        list_lift <- c(list_lift, x[j, 5])
      }
    } else if(i == 4){
      if(grepl(list_input[1], x[j,1]) & grepl(list_input[2], x[j,1]) & grepl(list_input[3], x[j,1]) & grepl(list_input[4], x[j,1])
         | grepl(list_input[1], x[j,1]) & grepl(list_input[2], x[j,1]) 
         | grepl(list_input[1], x[j,1]) & grepl(list_input[3], x[j,1]) 
         | grepl(list_input[1], x[j,1]) & grepl(list_input[4], x[j,1]) 
         | grepl(list_input[2], x[j,1]) & grepl(list_input[3], x[j,1]) 
         | grepl(list_input[2], x[j,1]) & grepl(list_input[4], x[j,1])
         | grepl(list_input[3], x[j,1]) & grepl(list_input[4], x[j,1]) 
         | grepl(list_input[1], x[j,1]) & grepl(list_input[2], x[j,1]) & grepl(list_input[3], x[j,1])
         | grepl(list_input[1], x[j,1]) & grepl(list_input[2], x[j,1]) & grepl(list_input[4], x[j,1]) 
         | grepl(list_input[1], x[j,1]) & grepl(list_input[3], x[j,1]) & grepl(list_input[4], x[j,1]) 
         | grepl(list_input[2], x[j,1]) & grepl(list_input[3], x[j,1]) & grepl(list_input[4], x[j,1]) 
         | grepl(list_input[1], x[j,1]) | grepl(list_input[2], x[j,1]) | grepl(list_input[3], x[j,1]) | grepl(list_input[4], x[j,1]) == TRUE){
        list_c <- c(list_c, x[j, 2])
        list_i <- c(list_i, x[j, 1])
        list_conf <- c(list_conf, x[j, 4])
        list_supp <- c(list_supp, x[j, 3])
        list_lift <- c(list_lift, x[j, 5])
      }
    } else if(i == 5){
      if(grepl(list_input[1], x[j,1]) & grepl(list_input[2], x[j,1]) & grepl(list_input[3], x[j,1]) & grepl(list_input[4], x[j,1]) & grepl(list_input[5], x[j,1])
         | grepl(list_input[1], x[j,1]) & grepl(list_input[2], x[j,1]) 
         | grepl(list_input[1], x[j,1]) & grepl(list_input[3], x[j,1]) 
         | grepl(list_input[1], x[j,1]) & grepl(list_input[4], x[j,1])
         | grepl(list_input[1], x[j,1]) & grepl(list_input[5], x[j,1]) 
         | grepl(list_input[2], x[j,1]) & grepl(list_input[3], x[j,1]) 
         | grepl(list_input[2], x[j,1]) & grepl(list_input[4], x[j,1])
         | grepl(list_input[2], x[j,1]) & grepl(list_input[5], x[j,1]) 
         | grepl(list_input[3], x[j,1]) & grepl(list_input[4], x[j,1]) 
         | grepl(list_input[3], x[j,1]) & grepl(list_input[5], x[j,1])
         | grepl(list_input[4], x[j,1]) & grepl(list_input[5], x[j,1]) 
         | grepl(list_input[1], x[j,1]) & grepl(list_input[2], x[j,1]) & grepl(list_input[3], x[j,1])
         | grepl(list_input[1], x[j,1]) & grepl(list_input[2], x[j,1]) & grepl(list_input[4], x[j,1]) 
         | grepl(list_input[1], x[j,1]) & grepl(list_input[2], x[j,1]) & grepl(list_input[5], x[j,1])
         | grepl(list_input[1], x[j,1]) & grepl(list_input[3], x[j,1]) & grepl(list_input[4], x[j,1]) 
         | grepl(list_input[1], x[j,1]) & grepl(list_input[3], x[j,1]) & grepl(list_input[5], x[j,1])
         | grepl(list_input[1], x[j,1]) & grepl(list_input[4], x[j,1]) & grepl(list_input[5], x[j,1]) 
         | grepl(list_input[2], x[j,1]) & grepl(list_input[3], x[j,1]) & grepl(list_input[4], x[j,1])
         | grepl(list_input[2], x[j,1]) & grepl(list_input[3], x[j,1]) & grepl(list_input[5], x[j,1]) 
         | grepl(list_input[2], x[j,1]) & grepl(list_input[4], x[j,1]) & grepl(list_input[5], x[j,1])
         | grepl(list_input[2], x[j,1]) & grepl(list_input[4], x[j,1]) & grepl(list_input[5], x[j,1]) 
         | grepl(list_input[1], x[j,1]) & grepl(list_input[2], x[j,1]) & grepl(list_input[3], x[j,1]) & grepl(list_input[4], x[j,1])
         | grepl(list_input[1], x[j,1]) & grepl(list_input[2], x[j,1]) & grepl(list_input[3], x[j,1]) & grepl(list_input[5], x[j,1])
         | grepl(list_input[1], x[j,1]) & grepl(list_input[2], x[j,1]) & grepl(list_input[4], x[j,1]) & grepl(list_input[5], x[j,1])
         | grepl(list_input[1], x[j,1]) & grepl(list_input[3], x[j,1]) & grepl(list_input[4], x[j,1]) & grepl(list_input[5], x[j,1]) 
         | grepl(list_input[2], x[j,1]) & grepl(list_input[3], x[j,1]) & grepl(list_input[4], x[j,1]) & grepl(list_input[5], x[j,1])
         | grepl(list_input[1], x[j,1]) | grepl(list_input[2], x[j,1]) | grepl(list_input[3], x[j,1]) | grepl(list_input[4], x[j,1]) | grepl(list_input[5],x[j,1]) == TRUE){
        list_c <- c(list_c, x[j, 2])
        list_i <- c(list_i, x[j, 1])
        list_conf <- c(list_conf, x[j, 4])
        list_supp <- c(list_supp, x[j, 3])
        list_lift <- c(list_lift, x[j, 5])
      }
    }
  }
  # Error if all the inputs are wrong   
  
  if (is.null(unlist(list_i)) == TRUE){
    return("No matching at all !!")
  }
  # Turn 'list' of input to 'data frame'  
  print('hello')
  table_input <- data.frame(matrix(unlist(list_i)))
  
  #Split the combination into pieces to determine number of TRUE
  
  for (rule in 1:(dim(table_input)[1])){
    first_proc <- gsub('[{}]', '', table_input[rule,1])
    first_proc <- gsub(' ', '', first_proc)
    second_proc <- strsplit(first_proc, ',')
    list_s <- c(list_s, second_proc)
  }
  
  # Compare and find out number of TRUE
  
  for (rows in 1:(length(list_s))){
    character <- unlist(list_s[rows])
    true_match <- input %in% character
    num_true <- length(which(true_match))
    df_con[rows, 1] <- data.frame(Input = list_i[rows], stringsAsFactors = FALSE)
    df_con[rows, 2] <- data.frame(Consequence = list_c[rows], stringsAsFactors = FALSE)
    df_con[rows, 3] <- data.frame(Confidence = list_conf[rows])
    df_con[rows, 4] <- data.frame(Support = list_supp[rows])
    df_con[rows, 5] <- data.frame(Lift = list_lift[rows])
    df_con[rows, 6] <- data.frame(Match = num_true)
    df_con[rows, 7] <- data.frame(Score = (df_con[rows, 3] * 0.5 + df_con[rows, 4] * 0.2 + df_con[rows, 5]*0.3))
  }
  
  sorted_df_con <- df_con[order(df_con[6], decreasing = TRUE), ]
  colnames(sorted_df_con) <- c('Input', 'Consequence', 'Confidence', 'Support','Lift', 'Match', 'Score')
  full_match <- data.frame()
  print(head(sorted_df_con))
  
  # Store the ones that contain all the inputs
  for (rows in 1:(dim(sorted_df_con)[1])){
    if (sorted_df_con[rows,6] == length(input)){
      full_match[rows, 1] <- data.frame(Input = sorted_df_con[rows, 1], stringsAsFactors = FALSE)
      full_match[rows, 2] <- data.frame(Consequence = sorted_df_con[rows, 2], stringsAsFactors = FALSE)
      full_match[rows, 3] <- data.frame(Confidence = sorted_df_con[rows, 3], stringsAsFactors = FALSE)
      full_match[rows, 4] <- data.frame(Support = sorted_df_con[rows, 4], stringsAsFactors = FALSE)
      full_match[rows, 5] <- data.frame(Lift = sorted_df_con[rows, 5], stringsAsFactors = FALSE)
      full_match[rows, 6] <- data.frame(Match = sorted_df_con[rows,6], stringsAsFactors = FALSE)
      full_match[rows, 7] <- data.frame(Score = (full_match[rows, 3] * 0.5 + full_match[rows, 4] * 0.2 + full_match[rows, 5]*0.3))
    } else if (sorted_df_con[rows,6] == max(sorted_df_con[6])){
      full_match[rows,1] <- data.frame(Input = sorted_df_con[rows,1], stringsAsFactors = FALSE)
      full_match[rows,2] <- data.frame(Consequence = sorted_df_con[rows,2], stringsAsFactors = FALSE)
      full_match[rows,3] <- data.frame(Confidence = sorted_df_con[rows,3], stringsAsFactors = FALSE)
      full_match[rows, 4] <- data.frame(Support = sorted_df_con[rows, 4], stringsAsFactors = FALSE)
      full_match[rows, 5] <- data.frame(Lift = sorted_df_con[rows, 5], stringsAsFactors = FALSE)
      full_match[rows, 6] <- data.frame(Match = sorted_df_con[rows,6], stringsAsFactors = FALSE)
      full_match[rows, 7] <- data.frame(Score = (full_match[rows, 3] * 0.5 + full_match[rows, 4] * 0.2 + full_match[rows, 5]*0.3))
    }
  }
  
  full_match <- na.omit(full_match)
  print(head(full_match))
  sorted_full_match <- full_match[order(full_match[7], decreasing = TRUE), ]
  exact_match <- data.frame()
  highest_score <- data.frame()
  
  # Find out the exact match and the highest confidence
  
  for (rows in 1:nrow(full_match)){
    first_proc <- gsub('[{}]', '', full_match[rows,1])
    first_proc <- gsub(' ', '', first_proc)
    second_proc <- strsplit(first_proc, ',')
    unlisted_row <- unlist(second_proc)
    print(unlisted_row)
    # Find exact match
    if (length(unlisted_row) == full_match[rows,6]){
      exact_match[rows, 1] <- data.frame(Input = full_match[rows, 1], stringsAsFactors = FALSE)
      exact_match[rows, 2] <- data.frame(Consequence = full_match[rows, 2], stringsAsFactors = FALSE)
      exact_match[rows, 3] <- data.frame(Confidence = full_match[rows, 3], stringsAsFactors = FALSE)
      exact_match[rows, 4] <- data.frame(Support = full_match[rows, 4], stringsAsFactors = FALSE)
      exact_match[rows, 5] <- data.frame(Lift = full_match[rows, 5], stringsAsFactors = FALSE)
      exact_match[rows, 6] <- data.frame(Match = full_match[rows, 6], stringsAsFactors = FALSE)
      exact_match[rows, 7] <- data.frame(Score = full_match[rows, 7], stringsAsFactors = FALSE)
    }
    if (full_match[rows, 7] == max(full_match[7])){
      highest_score[rows, 1] <- data.frame(Input = full_match[rows, 1], stringsAsFactors = FALSE)
      highest_score[rows, 2] <- data.frame(Consequence = full_match[rows, 2], stringsAsFactors = FALSE)
      highest_score[rows, 3] <- data.frame(Confidence = full_match[rows, 3], stringsAsFactors = FALSE)
      highest_score[rows, 4] <- data.frame(Support = full_match[rows, 4], stringsAsFactors = FALSE)
      highest_score[rows, 5] <- data.frame(Lift = full_match[rows, 5], stringsAsFactors = FALSE)
      highest_score[rows, 6] <- data.frame(Match = full_match[rows, 6], stringsAsFactors = FALSE)
      highest_score[rows, 7] <- data.frame(Score = full_match[rows, 7], stringsAsFactors = FALSE)
    }
  }
  
  print("Maximum matches")
  print(full_match)
  
  # If there is no exact match, print "No exact match", else, print the exact match
  if (ncol(exact_match) == 0){
    print('No exact match')
  } else {
    precise_match <- subset(exact_match,is.na(Input)==FALSE)
    print("Most precise match")
    print(precise_match)
  }
  highest_score <- na.omit(highest_score)
  final_suggestion <- unique(highest_score[2])
 
  print("Highest score")
  print(highest_score)
  print("final suggestion is...")
  print(final_suggestion)
  
  
  ##### Test Data ####
  
  data_test_df <- data.frame()
  data_test_df <- as(data_test,'data.frame')
  data_test_list <- list()
  
  for (rule in 1:(dim(data_test_df)[1])){
    first_proc <- gsub('[{}]', '', data_test_df[rule,1])
    first_proc <- gsub(' ', '', first_proc)
    second_proc <- strsplit(first_proc, ',')
    data_test_list <- c(data_test_list, second_proc)
  }
  
  # Adjust data into a shape and type that I want
  data_test_matrix <- do.call(rbind, data_test_list)
  df_test <- as.data.frame(data_test_matrix)
  df_test <- df_test[,c(1, 3, 4, 5, 6, 2)]
  colnames(df_test) <- c('SHIP_NO', 'DWG_TYPE', 'DWG_BLOCK', 'DWG_PROC', 'DWG_STAGE', 'RSN_CD')
  
  #Find number of matches and enter into a new column
  for (rows in 1: nrow(df_test)){
    true_match <- input %in% unlist(data_test_list[rows])
    num_true <- length(which(true_match))
    df_test[rows, 7] <- data.frame(True = num_true)
  }
  
  max_match <- df_test[df_test[7] == max(df_test[7]), ]
  max_match <- na.omit(max_match)
  print(head(df_test))
  print(head(max_match))
  print(nrow(max_match))
  
  # Process final_suggestion for accuracy check
  first_proc <- gsub('[{}]', '', final_suggestion[1,1])
  first_proc <- gsub(' ', '', first_proc)
  second_proc <- strsplit(first_proc, ',')
  
  # Compute accuracy of final_suggestion on test data
  abc <- apply(max_match[6], 1, function(x) {
    if (second_proc == x){
      return(TRUE)
    } else {
      return(FALSE)
    }})
  acc <- length(which(abc)) / nrow(max_match)
  
  
  print(length(which(abc)))
  print(nrow(max_match))
  
  print(acc)
}

prediction(data = data, Support = 0.001, Confidence = 0.4, 'DWG_BLOCK=S1','SHIP_NO=2','DWG_STAGE=2')
