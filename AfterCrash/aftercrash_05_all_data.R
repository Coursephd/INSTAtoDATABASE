
library(data.table)
library(zoo)
library(stringi)
library(stringr)
library(openxlsx)


data <- fread("d:/Hospital_data/04_2017_DOWNLOAD/pat_txts_crf/all_data022.txt",
              sep="!", 
              header= FALSE)

subdata <- function (xsub = '@@D@8', dataout ="D8")
{
  #@@D1@
  
  data00 <- data [ V1 %like% c(xsub)]
  data0 <- data00 [!V1 %like% c("<><>")]
  
  data02 <- data0 [, c("TMP1", "TMP2", "TMP3", "TMP4", "TMP5") := tstrsplit(stri_trim(V1), "@", fixed=TRUE),]
  
  # Fix the problems with the file
  data02 <- data02[, .(V1, TMP1, TMP2, TMP3, TMP4, TMP5,  
                       splitted =unlist(strsplit(TMP5, "><"))) ,
                   by=seq_len(nrow(data02))]
  
  data03 <- data02 [, c("VAR1","VAR2") := tstrsplit(stri_trim(splitted), "<>", fixed=TRUE), ]
  data03 <- data03 [, `:=`(VAR1 = stri_replace_all(VAR1, fixed = " ",""),
                           Source = substr(TMP1, 26, 33)),]
  
  chk <- data03[, cnt :=1:.N, by =.(TMP1, VAR1)]
  data04 <- dcast(data=chk,
                  Source + cnt + TMP1 + TMP3 + TMP4 ~ VAR1,
                  fill=" ",
                  value.var = c("VAR2"))
  
  chk99 <- chk [, VAR22 := paste(VAR2, collapse = ","), by =.(TMP1, VAR1)]
  chk999 <- unique( chk99 [, c("Source", "TMP1", "TMP3", "TMP4", "VAR1", "VAR22"), ])
  
  dataout0 <- dcast(data=chk999,
                    Source + TMP1 + TMP3 + TMP4 ~ VAR1,
                    fill=" ",
                    value.var = c("VAR22"))
  
  assign(dataout, dataout0, envir=.GlobalEnv)
  saveRDS(dataout0, paste0("d:/Hospital_data/04_2017_DOWNLOAD/pat_dbs/", dataout, ".rds", sep=""))
  
}

subdata('@@D1@', "D1")
subdata('@@D2@', "D2")


subdata('@@D8@', "D8")

dd <- data.table (readRDS("d:/Hospital_data/04_2017_DOWNLOAD/pat_dbs/D8.rds") )

