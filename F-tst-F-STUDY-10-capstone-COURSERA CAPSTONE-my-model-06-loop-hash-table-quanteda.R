

# library(tm)
# library(slam)
# library(RWeka)
# library(parallel)
# library(ggplot2) 
# library(dplyr)

library(quanteda)

# -------------------------------------------------------------------------
# come config
# -------------------------------------------------------------------------

ngramDepth <- 6
sampleSize <- 1000
dfList <- list()   # the list that holds dataframes with word frequencies
ngramList <- list()  
predictionTables <- list() 
doPrint <- FALSE
numberOfLoops <- 1000
saveCountMax <- 10


mainData <- NULL 

# -------------------------------------------------------------------------
# preparatory work to get the file
# -------------------------------------------------------------------------

PREPARE_DATA <- function(){
  
  setwd("C:\\Users\\xandi\\Documents\\COURSERA-CAPSTONE\\Text-Files-Given\\en_US")
  #setwd("D:\\TMP\\Coursera-Capstone\\Coursera-SwiftKey\\final\\en_US")
  
  
  conn <- file("en_US.twitter.txt",open="r")  # Precondition: set the working directory to your directory by using 
  data <-readLines(conn)
  
  conn <- file("en_US.blogs.txt",open="r")  # Precondition: set the working directory to your directory by using 
  data2 <-readLines(conn)
  
  conn <- file("en_US.news.txt",open="r")  # Precondition: set the working directory to your directory by using 
  data3 <-readLines(conn)
  
  dataU <- unlist(data)
  data2U <- unlist(data2)
  data3U <- unlist(data3)
  
  mainData <<- c(dataU, data2U, data3U) 
  
  saveRDS(mainData, file="dataD4.Rda")
  mainData <<- readRDS(file="dataD4.Rda")
  
}

# mainData <<- readRDS(file="dataD4.Rda")
# loop until finished - as long as not all files fully processes
# for simplification at the bgeinning we use random samples of 100

# -------------------------------------------------------------------------
# main loop
# -------------------------------------------------------------------------

MAIN_LOOP <- function(){
  
  prepareTables() #prepare the result tables
  
  saveCount <- 0 
  
  for(loopCount in 1:numberOfLoops){
    
    
    
    printLog("-------------------- 1 ----------------------------")
    print(paste("************* loop count: ", loopCount, " ************ ", sep=" "))
    print(Sys.time())
    #get sample

    currentSample <- sample(mainData, sampleSize, replace = FALSE, prob = NULL)
    
    printLog("-------------------- 2 ----------------------------")
    # make corpus
    print("making corpus... ")
    currentcorpus <- makeCorpus(currentSample)
    
    # summary(currentcorpus, n = 3)

    
    printLog("-------------------- 3 ----------------------------")
    print("making token... ")
    currentToken <- makeToken(currentcorpus)
    
    # print(currentToken)
    
    #make ngrams loops throuhg all n in n-gram
    print("making ngrams... ")
    makeNgram(currentToken)

    printLog("-------------------- 4 ----------------------------")
    
    #add to lists
    print("fill ngrams to table... ")
    fillPredictionTables()

    printLog("-------------------- 5 ----------------------------")
    #call garbage collector
    #gc()
    
    if(saveCount > saveCountMax){
      print("saving files... ")
      saveFiles()
      saveCount<- 0 
    }else{
      saveCount <- saveCount + 1
    }
    
    print(Sys.time())
  }
  
}

# -------------------------------------------------------------------------
# make angrams out of token and put to list
# -------------------------------------------------------------------------
makeNgram <- function(t){
  for(i in 1:ngramDepth){  
    ngramList[[i]]  <<- makeOneNgram(t, i)
    printLog(paste("finished creating omakeNgram for depth: ", i, sep=" "))
  }
}

# -------------------------------------------------------------------------
# makes one ngram
# -------------------------------------------------------------------------
makeOneNgram <- function(t, i){
  ngram <- quanteda::ngrams(t, n = i)
  return(ngram)
}

# -------------------------------------------------------------------------
# tokenis it takes the corpus and make tokens
# -------------------------------------------------------------------------

makeToken <- function(currentcorpus){
 
  tokens <- quanteda::tokenize(toLower(unlist(currentcorpus)), removePunct = TRUE, simplify = TRUE)
  
  return(tokens)
   
}

# -------------------------------------------------------------------------
# prepare frequency tables
# -------------------------------------------------------------------------
saveFiles <- function(){
  
  # for(i in 1:ngramDepth){
  #   predictionTables[[i]] 
  #   filename <- paste("DF-", i, ".csv",  collapse="") 
  #   #predictionTables[[i]] <- arrange(predictionTables[[i]], desc(frequency))
  #   write.csv(predictionTables[[i]] , file = filename)
  # }
  
  for(i in 1:ngramDepth){
    
    print("saving RDS file... ")
    filename <- paste("hash-", i, ".dat",  collapse="") 
    saveRDS(predictionTables[[i]] , file=filename)
    h <- predictionTables[[i]]
    
    # print("saving CSV... ")
    # filename <- paste("hash-", i, ".csv",  collapse="") 
    # hashToStringToFile(h, filename)
    
  }
}

# -------------------------------------------------------------------------
# prepare frequency tables
# -------------------------------------------------------------------------

hashToStringToFile<- function(x, fileName){
  
  file.remove(fileName)

  sink(fileName)
  
  for(v in ls(x)){
    cat(v)
    cat(", ")
    cat(x[[v]])
    cat("\r\n")
  }
  
  sink()
  
}

# -------------------------------------------------------------------------
# prepare frequency tables
# -------------------------------------------------------------------------

printLog <- function(t){
  
  if(doPrint){
    print(t)
  }
  
}

# -------------------------------------------------------------------------
# prepare frequency tables
# -------------------------------------------------------------------------

prepareTables <- function(){
  predictionTables <<- list() 
  for(i in 1:ngramDepth){
    #predictionTables[[i]]  <<- data.frame( "nGramString" = character(), "searchString" = character(), "prediction" = character(), "frequency" = integer(), stringsAsFactors=FALSE)
    
    filename <- paste("hash-", i, ".dat",  collapse="") 
    
    if(!file.exists(filename)){
      print("creating NEW hash maps")
      predictionTables[[i]] <<- new.env()
    }else{
      print("creating hash maps from existing file")
      
      predictionTables[[i]] <<- readRDS(file=filename)
    }
  }
}

# -------------------------------------------------------------------------
# fill tables
# -------------------------------------------------------------------------
fillPredictionTables <- function(){
  for(i in 1:ngramDepth){
    #we use predictionTables[[i]] and dfList[[i]]
    tmpList <- unlist(ngramList[[i]])
    # loop through the df list
    for(r in tmpList) {
      # do stuff with row
      addRowToTable(i, r)
    }
  }
}

# -------------------------------------------------------------------------
# add a row to a specific table
# -------------------------------------------------------------------------

addRowToTable <- function(depth, row){
  
  s <- row

  tst <- is.null(predictionTables[[depth]][[s]]) 


  if(tst){
    # print("new --------------------")
    printLog("adding line")
    tmp <- 0 + 1
    #print(tmp)
    predictionTables[[depth]][[s]] <- tmp
    #print(predictionTables[[depth]][[s]])

  }else{
    #print("-------------------- existing")
    printLog("updating line")
    tmp <- predictionTables[[depth]][[s]]
    #print(tmp)
    tmp <- tmp + 1
    #print(tmp)
    predictionTables[[depth]][[s]] <- tmp
    #print(predictionTables[[depth]][[s]])
  }

}


# -------------------------------------------------------------------------
# get fragment of ngram
# -------------------------------------------------------------------------
getPredictFragment <- function(depth, ngram){
  if(depth > 1){
    x <- unlist(strsplit(ngram, " "))
    x1 = paste(x[1:length(x)-1], collapse=" ") 
    x2 = x[length(x)]
  }else{
    x1 = ngram
    x2 = ngram
  }
  return(c(x1, x2))
}
# -------------------------------------------------------------------------
# Function to get make the corpus
# -------------------------------------------------------------------------

makeCorpus <- function(d){
  # corpus <- Corpus(VectorSource(d)) # create corpus for TM processing
  # corpus <- tm_map(corpus, content_transformer(tolower))  #we do this only for English, for German it would not be a good idea 
  # corpus <- tm_map(corpus, removeNumbers) # we are not interested in numbers
  # corpus <- tm_map(corpus, removePunctuation) #punctuation does not play a role in our case
  # corpus <- tm_map(corpus, stripWhitespace)  #a further cleanup
  # printLog("finished creating corpus")
  # 
  corpus <- quanteda::corpus(d)
  
  return(corpus)
}


# -------------------------------------------------------------------------
#  create word frequency list for varius ngrams
# -------------------------------------------------------------------------
makeFrequencyLists <- function(c){
  
  for(i in 1:ngramDepth){  
    dfList[[i]]  <<- makeDataFrame(c, i)
    printLog(paste("finished creating on data frame for depth: ", i, sep=" "))
    }
}

# -------------------------------------------------------------------------
#  makes a data frame for one specific ngram size
# -------------------------------------------------------------------------

makeDataFrame <- function(corpus, nGramSize){
  
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = nGramSize, max = nGramSize))
  
  tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
  
  temp <- row_sums(tdm)
  df <- as.data.frame(temp)
  df$Words <- row.names(df)
  
  names(df) <- c('Frequency','Words')
  rownames(df) <- NULL

  printLog(paste("data frame has length: ", nrow(df), sep=" "))
  
  df <- transform(df, Words = reorder(Words, Frequency))
  df <- arrange(df, desc(Frequency))
  
  # df <- subset(df, Frequency>1)  
  
  return(df)
  
}

############################################################

# dfList <- list()
# dfList[[1]] <- makeDataFrame(corpus, 2)
# dfList[[2]] <- makeDataFrame(corpus, 3)
# dfList[[3]] <- makeDataFrame(corpus, 4)
# 


#  Grade3orAboveData<-subset(StudentData, Grade>=3)  

############################################################

predictFromNGram <- function(dfX, searchTerm){
  x <- searchTerm   # to do: clean the search term
  regExSearch <- paste0("^", x, " ")
  return(subset(dfX, grepl(regExSearch, dfX$Words))[1:5, ])
}


############################################################
pridctFromMultipleNgram <- function(dfL, searchTerm){
  
  x = predictFromNGram(dfL[[1]], searchTerm)
  print(x)
  
  x = predictFromNGram(dfL[[2]], searchTerm)
  print(x)
  
  x = predictFromNGram(dfL[[3]], searchTerm)
  print(x)
 
  
}
############################################################

#pridctFromMultipleNgram(dfList, "me the")


# predictionTables[[2]]

#head(predictionTables[[2]])

# x <- predictionTables[[1]]
# for (v in ls(x)) {
#   print(v)
#   print(x[[v]])
# }

# mainData <- readRDS(file="dataD4.Rda")
