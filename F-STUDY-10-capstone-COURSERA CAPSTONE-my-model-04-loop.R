

library(tm)
library(slam)
library(RWeka)
library(parallel)
library(ggplot2) 
library(dplyr)

# -------------------------------------------------------------------------
# come config
# -------------------------------------------------------------------------

ngramDepth <- 6
sampleSize <- 10
dfList <- list()   # the list that holds dataframes with word frequencies
predictionTables <- list() 
doPrint <- FALSE
numberOfLoops <- 10

# -------------------------------------------------------------------------
# preparatory work to get the file
# -------------------------------------------------------------------------

PREPARE_DATA <- function(){
  
  setwd("C:\\Users\\xandi\\Documents\\COURSERA-CAPSTONE\\Text-Files-Given\\en_US")
  
  
  conn <- file("en_US.twitter.txt",open="r")  # Precondition: set the working directory to your directory by using 
  data <-readLines(conn)
  
  conn <- file("en_US.blogs.txt",open="r")  # Precondition: set the working directory to your directory by using 
  data2 <-readLines(conn)
  
  conn <- file("en_US.news.txt",open="r")  # Precondition: set the working directory to your directory by using 
  data3 <-readLines(conn)
  
  dataU <- unlist(data)
  data2U <- unlist(data2)
  data3U <- unlist(data3)
  
  mainData <- c(dataU, data2U, data3U) 
  
  saveRDS(mainData, file="dataD4.Rda")
  mainData <- readRDS(file="dataD4.Rda")
  
}


# loop until finished - as long as not all files fully processes
# for simplification at the bgeinning we use random samples of 100

# -------------------------------------------------------------------------
# main loop
# -------------------------------------------------------------------------

MAIN_LOOP <- function(){
  
  prepareTables() #prepare the result tables
  
  for(loopCount in 1:numberOfLoops){
    
    printLog("-------------------- 1 ----------------------------")
    print(paste("loop count: ", loopCount, sep=" "))
    #get sample
    start.time <- Sys.time()
    currentSample <- sample(mainData, sampleSize, replace = FALSE, prob = NULL)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste("sample() took timet:", time.taken, sep=" "))
    
    printLog("-------------------- 2 ----------------------------")
    # make corpus
    start.time <- Sys.time()
    currentcorpus <- makeCorpus(currentSample)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste("makeCorpus() took timet:", time.taken, sep=" "))
    
    printLog("-------------------- 3 ----------------------------")
    #make ngrams
    start.time <- Sys.time()
    makeFrequencyLists(currentcorpus)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste("makeFrequencyLists() took timet:", time.taken, sep=" "))
    
    printLog("-------------------- 4 ----------------------------")
    #add to lists
    start.time <- Sys.time()
    fillPredictionTables()
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste("fillPredictionTables() took timet:", time.taken, sep=" "))
    
    printLog("-------------------- 5 ----------------------------")
    #call garbage collector
    #gc()
    
    start.time <- Sys.time()
    saveFiles()
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste("saveFiles() took timet:", time.taken, sep=" "))
  }
  
}

# -------------------------------------------------------------------------
# prepare frequency tables
# -------------------------------------------------------------------------
saveFiles <- function(){
  
  for(i in 1:ngramDepth){
    predictionTables[[i]] 
    filename <- paste("DF-", i, ".csv",  collapse="") 
    predictionTables[[i]] <- arrange(predictionTables[[i]], desc(frequency))
    write.csv(predictionTables[[i]] , file = filename)
  }
  
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
    predictionTables[[i]]  <<- data.frame( "nGramString" = character(), "searchString" = character(), "prediction" = character(), "frequency" = integer(), stringsAsFactors=FALSE)
    
  }
}

# -------------------------------------------------------------------------
# fill tables
# -------------------------------------------------------------------------
fillPredictionTables <- function(){
  for(i in 1:ngramDepth){
    #we use predictionTables[[i]] and dfList[[i]]
    tmpList <- dfList[[i]]
    # loop through the df list
    for(r in 1:nrow(tmpList)) {
      row <- tmpList[r,]
      # do stuff with row
      addRowToTable(i, row)
    }
  }
}

# -------------------------------------------------------------------------
# add a row to a specific table
# -------------------------------------------------------------------------

addRowToTable <- function(depth, row){
  printLog(paste("addRowToTable depth:", depth, sep=" "))
  printLog(row)
  #tmpTable <<- predictionTables[[depth]]
  printLog(paste("searching for : ", row[1, 2], sep=" "))
  
  #found <- grepl(row[1, 2], predictionTables[[depth]]$nGramString)
  #found <- grepl(row[1, 2], predictionTables[[depth]]$nGramString)
  
  found <- predictionTables[[depth]][which(predictionTables[[depth]]$nGramString == row[1, 2]),]
  
  subset(data, D1 == "E" | D2 == "E")
  
  
  
  if(any(found, na.rm = FALSE)){
    printLog("line found")
    yMin <- min(which(found == TRUE)) 
    #tstDf[yMin, 4] <<- tstDf[y, 4] + row[1, 1]
    predictionTables[[depth]][yMin, 4] <<- predictionTables[[depth]][yMin, 4] + row[1, 1]
    
  }else{
    printLog("adding line")
    y <- nrow(predictionTables[[depth]]) + 1
    
    printLog(paste("adding:", row[1, 2], sep=" "))
    printLog(paste("adding:", row[1, 1], sep=" "))
    
    a = paste("", row[1, 2], sep="")
    
    f = getPredictFragment(depth, a)
    
    predictionTables[[depth]][y, 1] <<- a
    predictionTables[[depth]][y, 2] <<- f[1]
    predictionTables[[depth]][y ,3] <<- f[2]
    predictionTables[[depth]][y, 4] <<- row[1, 1]
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
  corpus <- Corpus(VectorSource(d)) # create corpus for TM processing
  corpus <- tm_map(corpus, content_transformer(tolower))  #we do this only for English, for German it would not be a good idea 
  corpus <- tm_map(corpus, removeNumbers) # we are not interested in numbers
  corpus <- tm_map(corpus, removePunctuation) #punctuation does not play a role in our case
  corpus <- tm_map(corpus, stripWhitespace)  #a further cleanup
  printLog("finished creating corpus")
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

dfList <- list()
dfList[[1]] <- makeDataFrame(corpus, 2)
dfList[[2]] <- makeDataFrame(corpus, 3)
dfList[[3]] <- makeDataFrame(corpus, 4)



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
