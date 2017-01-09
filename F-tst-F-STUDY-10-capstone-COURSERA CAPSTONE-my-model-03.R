


library(quanteda)

setwd("C:\\Users\\xandi\\Documents\\COURSERA-CAPSTONE\\Text-Files-Given\\en_US")
quantedaData <- readRDS(file="dataD4.Rda")

quantedaDataSubset <- sample(quantedaData, 1000, replace = FALSE, prob = NULL)

quantedaCorpus <- corpus(quantedaDataSubset)
#quantedaCorpus <- corpus("i love you i love you i love you")

summary(quantedaCorpus, n = 3)

tokens <- tokenize(toLower(unlist(quantedaCorpus)), removePunct = TRUE, simplify = TRUE)

ngram1 <- quanteda::ngrams(tokens, n = 3)

ngram1[4000]


# ngrams
ngrams(LETTERS, n = 2)
ngrams(LETTERS, n = 2, skip = 1)
ngrams(LETTERS, n = 2, skip = 0:1)
ngrams(LETTERS, n = 1:2)
ngrams(LETTERS, n = c(2,3), skip = 0:1)
tokens <- tokenize("the quick brown fox jumped over the lazy dog.",
                   removePunct = TRUE, simplify = TRUE)
ngrams(tokens, n = 1:3)
ngrams(tokens, n = c(2,4), concatenator = " ")
ngrams(tokens, n = c(2,4), skip = 1, concatenator = " ")
# skipgrams
tokens <- tokenize(toLower("Insurgents killed in ongoing fighting."),
                   removePunct = TRUE, simplify = TRUE)
skipgrams(tokens, n = 2, skip = 0:1, concatenator = " ")

library(dtplyr)
library(data.table) 


someenv<-new.env()
someenv[["key"]]<-10
someenv[["keddy"]]
is.null(someenv[["key"]])
if(is.null(someenv[["key"]])){
  someenv[["key"]]<-10
}else{
  tmp <- someenv[["key"]]
  newVal <- tmp + 10
  someenv[["key"]] <- newVal
}
someenv[["key"]]


