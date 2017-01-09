library(shiny)
library(shinyjs)

# setwd("C:\\Users\\xandi\\Google Drive\\COURSERA CAPSTONE")

#setwd("C:\\Users\\xandi\\Documents\\COURSERA-CAPSTONE\\Text-Files-Given\\en_US")

# https://alexandersickert.shinyapps.io/data-science-capstone/

# ====================================================
# some variable we need 
# ====================================================
initialText <- "aaaaa"
i <- 0
result <- ""
info <- ""
depth <- 6
predictionHashMaps <<- list() 
loading <- FALSE
databaseLoaded <- FALSE
tst <<- 0

# ====================================================
# function to load at startup
# ====================================================
initializePrediction <- function(s){
  
  if(databaseLoaded){
    print("database already loaded")
  }else{
  
    for (i in 2:depth)
    {
      print(Sys.time())
      loading <<- TRUE
      info <<-"database not ready"
    
      # output$textOut2 <- renderText({getInfo(input$textIn)})
    
      # fileName <- paste("hash-Reduced-", i, ".RDA", sep="",  collapse="")
      fileName <- paste("java-", i, "-gram-consolidated.txt", sep="",  collapse="") 
    
      print("loading...")
    
      # predictionHashMaps[[i]] <<- readRDS(file=fileName)
      predictionHashMaps[[i]] <<- read.csv(file=fileName, sep=";", stringsAsFactors=F, quote="", comment.char="", header = FALSE)
    
      print("done")
      print(Sys.time())
    
    } 
    print(Sys.time())
    loading <<- FALSE
    info <<-"database now ready"
  
    print("finished loading loop")
  }
  
}

# ====================================================
# the predict function
# ====================================================
prepareString <- function(s){

  s <- tolower(s)
  # s <- gsub("[?.;!¡¿·']", "", s)
  s <- gsub("[[:punct:]]", "", s)
  s <- gsub("  ", " ", s)
  s <- gsub("  ", " ", s)
  s <- gsub("  ", " ", s)
  s <- gsub("^\\s+|\\s+$", "", s)
  
  # split the string
  
  # create waring that certain words are not in vocalbulary
  
  # combine the string to xxx_xxx
  return(s)
}
# ====================================================
# the predict function
# ====================================================
predictText <- function(s){
  # clean the string
  
  s <- prepareString(s)
  
  #  preset values 
  
  result <<- ""
  info <<- "error"
  
  t <- is.null(s) 
  
  if(t == FALSE){
    if(nchar(s) < 1){
      t <- TRUE
    }
  }
  
  
  if(t){
    
    result <<- ""
    # info <<- "string is empty"
    
    
  }else{
    for (k in depth:2)
    {
      
      # here we need to cut the string in pieces that might match to the hash length !!!!
      
      print(paste("predictText and k is: ", k, sep="",  collapse=""))
      
      found <- predictWithOneHashMap(k, s)
      
      if(found){
        print(paste("sucessfully found in k: ", k, sep="",  collapse=""))
        info <<- k
        break
      }
      
    } 
    
    # print(paste("Result: ", result, sep="",  collapse=""))
    # print(paste("Infol: ", info, sep="",  collapse=""))
    # 
    
  }
  

  return(result)
}
# ====================================================
# cut the string that it matches with the ngram size
# ====================================================
cutString <- function(s, n){
  
  c <- unlist(strsplit(s, " "))
  l <- length(c)
  
  print(paste("cutString l is: ", l, sep="",  collapse=""))
  print(paste("cutString n is: ", n, sep="",  collapse=""))
  
  if(l >= n){
    min <- l - (n - 2)
    res <- paste(c[min:l], sep= " ", collapse = " ")
  }else{
    res <- s
  }
  
  print(paste("cutString result is: ", res, sep="",  collapse=""))
  
  return(res)
}
# ====================================================
# get info about statistics
# ====================================================
getInfo <- function(s){
  
  flag <- ""
  s <- prepareString(s)
  
  
  t <- is.null(s) 
  
  if(t == FALSE){
    if(nchar(s) < 1){
      t <- TRUE
    }
  }
  
  if(t){
    x <- "No valid input string. Please type a word or several words."
  }else{
    
    x<- "The result is based on the following logic:<br><br><ul>"
    for (k in depth:2){
      res <- predictWithOneHashMapForInfo(k, s)
      x <- paste(x, "<li>The ", k, "-gram table would predict: ", res, "</li>", sep="",  collapse = "")
      if(k == 2){
        if(any(grep("NA", res))){
          flag <- "<br><b>ERROR: the last entered word is not a word in the given vocabulary. Please check.</b><br>"
        }
      }
      
    }
    
    x <- paste(x, "</ul>", sep="",  collapse = "")
    x <- paste(x, flag, sep="",  collapse = "")
    x <- paste(x, "<br>The algorithm used the non-NA result with the higest n-Gram for prediction. This means the algorithm uses the <b>'stupid backoff'</b> approach. <br><br>The application development roadmap foresees that as a next improvement the <b>Kneser-Ney</b> algorithm will be implemented to improve the prediction accuracy. The prediction quality right now is from a user experience perspective not satisfactory (according to the opinion of the developers). If the Kneser-Ney algorithm does not lead to a significant improvement, then we will use neural networks and include not just the last 1-5 words for prediction, but the entire text to provide a context for the prediction. However, this proof of concept is a good first step. ", sep="",  collapse = "")
  }
  
  
  tst <<- tst + 1
  # print(s)
  # print("in function getInfo") 
  # # return(paste("found in level: ", info, " counter ", tst, s, sep="",  collapse = ""))
  # x <- paste("found in level: ", info, sep="",  collapse = "")
  # print(paste("return value would be: ", x, sep="",  collapse = ""))

  return(x)
}


# ====================================================
# 
# ====================================================
predictWithOneHashMapForInfo <- function(h, s){
  
  print(paste("predictWithOneHashMapForInfo depth h : ", h, sep="",  collapse=""))
  print(paste("predictWithOneHashMapForInfo raw string: ", s, sep="",  collapse=""))
  
  s <- cutString(s, h) 
  
  c <- unlist(strsplit(s, " "))
  l <- length(c)
  
  print(paste("predictWithOneHashMapForInfo l : ", l, sep="",  collapse=""))
  
  if(l < ( h - 1)){
    addition <- " - string has too few nGrams to be used for prediction."
  }else{
    addition <- ""
  }
  
  
  print(paste("predictWithOneHashMapForInfo depth is: ", h, sep="",  collapse=""))
  print(paste("predictWithOneHashMapForInfo search string now: ", s, sep="",  collapse=""))
  
  df <- predictionHashMaps[[h]]
  # x <- subset(df, grepl(s, df[,1]))
  x <- subset(df, df[,1] == s)
  
  
  # x <- predictionHashMaps[[h]][[s]]
  print("----------- x -----------------")

  
  t <- FALSE
  if(is.null(x)){
    t <- TRUE
  }else{
    if(nrow(x) == 0){
      t <- TRUE
    }
  }
  
  
  if(t){
    found <- FALSE
    infoResult <- paste("NA", addition, sep="",  collapse="")
  }else{
    found <- TRUE
    r <- unlist(strsplit(x[1,2], " "))
    infoResult <- r[1:1]
    
    #calculate the percentage
    zaehler <- as.numeric(r[2:2])
    nenner <- as.numeric(r[3:3])
    s <- (zaehler/nenner)*100
    infoResult <- paste("'", infoResult, "' with a probablity of ", round(s, 1), "% based on ", nenner, " cases.", sep="",  collapse="") 
  }
  
  return(infoResult)
}

# ====================================================
# 
# ====================================================
predictWithOneHashMap <- function(h, s){
  
  print(paste("predictWithOneHashMap raw string: ", s, sep="",  collapse=""))
  
  s <- cutString(s, h) 
  
  print(paste("predictWithOneHashMap depth is: ", h, sep="",  collapse=""))
  print(paste("predictWithOneHashMap search string now: ", s, sep="",  collapse=""))
  
  df <- predictionHashMaps[[h]]
  # x <- subset(df, grepl(s, df[,1]))
  x <- subset(df, df[,1] == s)
  
  
  # x <- predictionHashMaps[[h]][[s]]
  print("----------- x -----------------")
  print(x)
  print("----------- x -----------------")
  # t <- is.null(x) 
  
  t <- FALSE
  if(is.null(x)){
    t <- TRUE
  }else{
    if(nrow(x) == 0){
      t <- TRUE
    }
  }

  
  if(t){
    found <- FALSE
  }else{
    found <- TRUE
    r <- unlist(strsplit(x[1,2], " "))
    result <<- r[1:1]
    
  }
  
  return(found)
}
# ====================================================
# 
# ====================================================


# ====================================================
# calling the load function
# ====================================================
# initializePrediction()



# ====================================================
# simple server app
# ====================================================
shinyServer(
  
 

  
  function(input, output) {
    
    initializePrediction()
    
    # Hide the loading message when the rest of the server function has executed
    hide(id = "loading-content", anim = TRUE, animType = "fade")  
    show("app-content")
    
    output$textOut <- renderText({predictText(input$textIn)})
    output$textOut2 <- renderText({
      getInfo(input$textIn)
      
      })
    
  }
)





