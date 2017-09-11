library(data.table)
library(stringi)
library(stringr)
library(openxlsx)

########################
## Section of planning
########################

# This DT part is from01pat_url.R program
# IF there is a change in that program then that needs to be accounted for
# Create macro call for patients to get the source code
DT <- data.table(x=c(1:50000))
DT <- DT[ , `:=` (x2 = paste( "MR", str_pad(x, 6, side = "left", pad = 0), sep=""), 
                  url1 ="https://182.71.223.195/instahms/emr/EMRMainDisplay.do?_method=list&mr_no=", 
                  url2 ="&mrno=")]

DT <- DT[ , `:=` (compl_url = paste(url1, x2, url2, x2, sep="") ,
                  outfile = paste(x2, ".txt", sep=""),
                  folder = ceiling(x / 1000),
                  x3 = x + 1
         )]
DT <- DT [, `:=` (stt = min (x), 
                  end = max (x),
                  stt2 = min(x3), 
                  end2 = max(x3) ), by = .(folder) ]

DT <- DT [, `:=` (sttname = paste( "MR", str_pad(stt, 6, side = "left", pad = 0), sep=""),
                  endname = paste( "MR", str_pad(end, 6, side = "left", pad = 0), sep="") )]
DT <- DT [, fname := paste (sttname, endname, sep ="_") ]

# Primary sheet for patient info
DT2 <- DT[, c("x2", "compl_url", "fname", "outfile")]

# Create this "start to end" for VBA macro
lookup <- unique( DT[ ,c("folder", "stt2", "end2", "fname")] )


DT <- DT [key ="outfile"]

write.xlsx(DT2,
           sheetName ="sheet1",
           file = "C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pgrm\\01pat_url.xlsx", 
           colNames = TRUE, 
           borders = "columns")

wb <- loadWorkbook("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pgrm\\01pat_url.xlsx")

addWorksheet(wb, "sheet2")
writeData(wb, "sheet2", x = lookup)

saveWorkbook(wb, 
             file = "C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pgrm\\01pat_url.xlsx", 
             overwrite = TRUE)
