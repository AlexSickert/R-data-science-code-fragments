

# C:\Users\xandi\Documents\COURSERA-CAPSTONE\Text-Files-Given 
# C:\Users\xandi\Documents\COURSERA-CAPSTONE\Text-Files-Given\en_US
# en_US.twitter.txt
# en_US.blogs.txt

#  note about memory issues    http://stackoverflow.com/questions/12626637/reading-a-text-file-in-r-line-by-line

# read all in memory

#fileName <- "C:\\Users\\xandi\\Documents\\COURSERA-CAPSTONE\\Text-Files-Given\\en_US\\en_US.blogs.txt"
fileName <- "C:\\Users\\xandi\\Documents\\COURSERA-CAPSTONE\\Text-Files-Given\\en_US\\en_US.news.txt"
#fileName <- "C:\\Users\\xandi\\Documents\\COURSERA-CAPSTONE\\Text-Files-Given\\en_US\\en_US.twitter.txt"

conn <- file(fileName,open="r")
linn <-readLines(conn)
close(conn)

#for (i in 1:200){
#  print(linn[i])
#}

lenArr <- lapply(linn, nchar)
#head(lenArr)
# nchar(strn)
#lapply(lenArr,FUN=max)
# max(..., na.rm = FALSE)
m <- max(unlist(lenArr, recursive = TRUE, use.names = TRUE), na.rm = FALSE)
m

# twitter: 213
# blogs 40835
# news 5760

fileName <- "C:\\Users\\xandi\\Documents\\COURSERA-CAPSTONE\\Text-Files-Given\\en_US\\en_US.twitter.txt"

conn <- file(fileName,open="r")
linn <-readLines(conn)
close(conn)
tst <- unlist(linn, recursive = TRUE, use.names = TRUE)

res <- grep("love", tst)
# love 90956
res <- grep("hate", tst)
# 22138
# biostats
res <- grep("biostats", tst)
res
tst[res]


z <- grep("A computer once beat me at chess, but it was no match for me at kickboxing", tst)


