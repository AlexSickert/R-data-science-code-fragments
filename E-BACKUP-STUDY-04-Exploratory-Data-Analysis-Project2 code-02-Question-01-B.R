
library(data.table)
dt <- data.table(NEIsub)
res <- dt[,list(mean=mean(Emissions),by=year)]
head(res)