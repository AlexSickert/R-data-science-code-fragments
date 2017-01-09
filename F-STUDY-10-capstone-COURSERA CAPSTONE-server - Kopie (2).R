library(shiny)
library(shinyjs)

# setwd("C:\\Users\\xandi\\Google Drive\\COURSERA CAPSTONE")

# ====================================================
# some variable we need 
# ====================================================
initialText <- "aaaaa"
i <- 0
result <- ""
info <-""
depth <- 4
predictionHashMaps <<- list() 
loading <- FALSE
databaseLoaded <- FALSE

# ====================================================
# function to load at startup
# ====================================================
initializePrediction <- function(s){
  
  if(databaseLoaded){
    print("database already loaded")
  }else{
  
    for (i in 1:depth)
    {
      loading <<- TRUE
      info <<-"database not ready"
    
      # output$textOut2 <- renderText({getInfo(input$textIn)})
    
      fileName <- paste("hash-Reduced-", i, ".RDA", sep="",  collapse="")
    
      print("loading...")
    
      predictionHashMaps[[i]] <<- readRDS(file=fileName)
    
      print("done")
    
    } 
  
    loading <<- FALSE
    info <-"database now ready"
  
    print("finished loading loop")
  }
  
}

# ====================================================
# the predict function
# ====================================================
predictText <- function(s){
  # clean the string
  
  # split the string
  
  # create waring that certain words are not in vocalbulary
  
  # combine the string to xxx_xxx
  
  #  preset values 
  
  result <<- ""
  info <<-"error"
  
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
    for (k in depth:1)
    {
      
      # here we need to cut the string in pieces that might match to the hash length !!!!
      
      print(paste("predictText and k is: ", k, sep="",  collapse=""))
      
      found <- predictWithOneHashMap(k, s)
      
      if(found){
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
  print(l)
  print(paste("cutString n is: ", n, sep="",  collapse=""))
  
  if(l >= n){
    min <- l - (n - 2)
    res <- paste(c[min:l], sep= "_", collapse = "_")
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
  if(loading){
    return(info)
  }else{
    return(paste("found in level: ", info, sep="",  collapse = ""))
  }
  
}

# ====================================================
# 
# ====================================================
predictWithOneHashMap <- function(h, s){
  
  print(paste("predictWithOneHashMap raw string: ", s, sep="",  collapse=""))
  
  s <- cutString(s, h) 
  
  print(paste("predictWithOneHashMap depth is: ", h, sep="",  collapse=""))
  print(paste("predictWithOneHashMap search string now: ", s, sep="",  collapse=""))
  
  
  x <- predictionHashMaps[[h]][[s]]
  print("1")
  t <- is.null(x) 
  print("2")
  
  if(t){
    found <- FALSE
  }else{
    found <- TRUE
    r <- unlist(strsplit(x, "_"))
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
    
    
    autoInvalidate <- reactiveTimer(1000)
    
    observe({
      autoInvalidate()
      print("The value of ...")
    })
    
    output$infoText <- renderText({
      autoInvalidate()
      tst()
    })
    
    output$textOut <- renderText({predictText(input$textIn)})
    output$textOut2 <- renderText({
      getInfo(input$textIn)
      initializePrediction()
      })
    
  }
)

print("3412341")



