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

# Create ls -l output using the system function and populate the output into a data.table
# Need to understand if this will work fully correctly

urls <- data.table (system ('ls -lR --full-time C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\emrurl', intern=TRUE))
urls <- urls [, V1 := gsub(" +", " ", V1), ]
urls2 <- data.table( urls[, c("SRC1", "SRC2", "SRC3", "SCR4", "SIZE", "DATE1", "DATE2", "TIME", "outfile") 
                          := tstrsplit(V1, " ", fixed=TRUE), ])
urls2 <- data.table( urls2 [!SRC1 %in% c("drwxr-xr-x", "total")])
urls2 <- data.table( urls2 [SRC2 != "NA"])

urls2 <- urls2 [key ="outfile"]
compare <- merge (x = DT, 
                  y = urls2, 
                  by ="outfile",
                  all.x = TRUE)

write.xlsx(compare,
           sheetName ="compare",
           file = "C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pgrm\\01pat_url_compare.xlsx", 
           colNames = TRUE, 
           borders = "columns")


# Varsha's code
urls_ <- data.table (grep('Lucky',
                          grep('drw',
                               grep('total',
                                    system('ls -lR --full-time C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\emrurl',
                                           intern=TRUE),ignore.case=TRUE,value=TRUE,invert =TRUE),
                               ignore.case=TRUE,value=TRUE,invert =TRUE),
                          ignore.case=TRUE,value=TRUE,invert =TRUE))
