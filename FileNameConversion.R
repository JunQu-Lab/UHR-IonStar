# File Name Conversion

# Set the directory of files
setwd("E:\\Data")

library(DBI)
library(RSQLite)

db <- "2batch_20sample_60run_2.sdb" ##File name of the SIEVE database
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=db)
RawFiles <- dbGetQuery(con, "select * from RawFiles")


test <- as.character(paste("java -Xmx10000M -cp MSGFPlus.jar edu.ucsd.msjava.ui.MzIDToTsv -i", RawFiles$RawFile, 
                           "-showQValue 1 -showDecoy 1 -unroll 1", sep = ' '))
test <- gsub('.raw','.mzid',test)
write.table(test, 'conversion.txt', quote = FALSE, row.names = FALSE, col.names = FALSE)
