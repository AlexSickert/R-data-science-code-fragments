library(shiny)
library(shinyjs)

# setwd("C:\\Users\\xandi\\Google Drive\\COURSERA CAPSTONE")

setwd("C:\\Users\\xandi\\Documents\\COURSERA-CAPSTONE\\Text-Files-Given\\en_US")



# ====================================================
# some variable we need 
# ====================================================
initialText <- "aaaaa"
i <- 0
result <- ""
info <- ""
depth <- 2
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
      loading <<- TRUE
      info <<-"database not ready"
    
      # output$textOut2 <- renderText({getInfo(input$textIn)})
    
      fileName <- paste("hash-Reduced-", i, ".RDA", sep="",  collapse="")
    
      print("loading...")
    
      predictionHashMaps[[i]] <<- readRDS(file=fileName)
    
      print("done")
    
    } 
  
    loading <<- FALSE
    info <<-"database now ready"
  
    print("finished loading loop")
  }
  
}

# ====================================================
# the predict function
# ====================================================
predictText <- function(s){
  # clean the string
  
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
    for (k in depth:1)
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
  print(l)
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
  
  tst <<- tst + 1
  print(s)
  print("in function getInfo") 
  # return(paste("found in level: ", info, " counter ", tst, s, sep="",  collapse = ""))
  x <- paste("found in level: ", info, sep="",  collapse = "")
  print(paste("return value would be: ", x, sep="",  collapse = ""))
  return(x)
 
  
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
    r <- unlist(strsplit(x, " "))
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





