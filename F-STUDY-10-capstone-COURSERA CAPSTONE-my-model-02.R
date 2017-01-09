

library(tm)
library(slam)
library(RWeka)
library(parallel)
library(ggplot2) 
library(dplyr)



setwd("C:\\Users\\xandi\\Documents\\COURSERA-CAPSTONE\\Text-Files-Given\\en_US")


conn <- file("en_US.twitter.txt",open="r")  # Precondition: set the working directory to your directory by using 
data <-readLines(conn)

conn <- file("en_US.blogs.txt",open="r")  # Precondition: set the working directory to your directory by using 
data2 <-readLines(conn)

conn <- file("en_US.news.txt",open="r")  # Precondition: set the working directory to your directory by using 
data3 <-readLines(conn)

names(data)[1]<-paste("col1")

dataU <- unlist(data)
data2U <- unlist(data2)
data3U <- unlist(data3)

d4 <- c(dataU, data2U, data3U) 


saveRDS(d4, file="dataD4.Rda")
d4 <- readRDS(file="dataD4.Rda")

dataPart <- sample(d4, 20000, replace = FALSE, prob = NULL)

dataPart <- d4

corpus <- Corpus(VectorSource(dataPart)) # create corpus for TM processing
print("done 1")
corpus <- tm_map(corpus, content_transformer(tolower))  #we do this only for English, for German it would not be a good idea 
print("done 2")
corpus <- tm_map(corpus, removeNumbers) # we are not interested in numbers
print("done 3")
corpus <- tm_map(corpus, removePunctuation) #punctuation does not play a role in our case
print("done 4")
corpus <- tm_map(corpus, stripWhitespace)  #a further cleanup
print("done 5")
#corpus <- tm_map(corpus, PlainTextDocument)  

saveRDS(corpus, file="corpus.Rda")
corpus <- readRDS(file="corpus.Rda")

makeDataFrame <- function(corpus, nGramSize){
  
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = nGramSize, max = nGramSize))
  tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
  
  temp <- row_sums(tdm)
  df <- as.data.frame(temp)
  df$Words <- row.names(df)
  
  names(df) <- c('Frequency','Words')
  rownames(df) <- NULL
  
  nrow(df)
  
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

pridctFromMultipleNgram(dfList, "me the")



# if string length 2 then search with 2. if 



predictSmart <- function(dfList, txt){
  
  c <- strsplit(txt, " " )
  x = lengths(c)
  c <- unlist(c)
  s = x-2
  if(x > 3){
    txt = paste(c[s:x], sep=" ")
  }
  
  print(txt)
  
}

  

c <- strsplit("hello this is the world", " " )
x = lengths(c)
c <- unlist(c)
s = x-2
paste(c[s:x], sep=" ")


c[2]



