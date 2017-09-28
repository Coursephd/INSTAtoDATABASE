
# Get the diagnosis data

all_diag <- fread("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_mod\\all_aligndiag.txt",
                  header=F, 
                  sep="@",
                  fill=TRUE)

all_diag0 <- all_diag [V2 != "Diag__Type"]
all_diag01 <- all_diag0 [ stri_trim(V2) != "" & stri_trim(V3) != "" & stri_trim(V4) != "" ]
all_diag01 <- unique( all_diag01)

all_diag01 <- all_diag01 [ , cnt :=1:.N, by =.(V1, V2)]
all_diag01 <- all_diag01 [, `:=` (MRNo = substr(V1, 31, 38),
                                  Visit = substr(V1, 40, 43),
                                  Date = substr(V1, 45, 53)), ]

# Subset for patients having V16.0

subdiag <- unique(all_diag01 [ V3 == "V16.0", c("MRNo", "V1", "Visit", "Date"),])

# Merge the subsetted data with the complete data
v16 <- merge (x = all_diag01, 
              y = subdiag, 
              by =c("MRNo", "V1", "Visit", "Date"),
              all.y = TRUE)

rm (list = ls( pattern = "all_diag*"))


all_dose <- fread("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_mod\\all_aligndose.txt",
                  header=F, 
                  sep="@")

#all_dose0 <- head(all_dose, n = 25000)
all_dose0 <- all_dose
all_dose0 <- all_dose0 [, row :=0:.N ,by= V1]
all_dose0 <- all_dose0 [V2 != "Sl.No"]

all_dose01 <- all_dose0[, newv1 := ifelse (V2 != " " & !nchar(V3),  V2, ""), ]
all_dose01 <- all_dose01[, newv1 := ifelse (V2 != " " & V3 != " " & newv1 != " ",  V2, newv1), ]
all_dose01 <- all_dose01[, newv1 := ifelse (row == 1 ,  row, newv1), ]
all_dose01 <- all_dose01[, `:=`( lagv7 = c(NA, V7[-.N]),
                                 lagv3 = c(NA, V3[-.N])), by = V1]
all_dose01 <- all_dose01[!nzchar(newv1), newv1:=NA]
all_dose01 <- all_dose01[, newv2:=na.locf(newv1), by =V1]

all_dose01 <- all_dose01[is.na(newv1), newv1 := "999"]
all_dose01 <- all_dose01[, newv3:= ifelse(stri_trim(V7) != "" & stri_trim(lagv7) != "", as.numeric(newv2)+1, newv2), by =V1]

all_dose01 <- all_dose01[, newv3:= ifelse(stri_trim(newv1) == "999" & stri_trim(lagv7) != "" & stri_trim(V3) != "", as.numeric(newv2)+1, newv3), by =V1]

all_dose02 <- all_dose01[, newv4:= ifelse(stri_trim(newv2) == stri_trim(newv1), newv2, newv3), ]

all_dose3 <- all_dose02 [ , `:=` (V30 = paste(V3, collapse=" "),
                                  V40 = paste(V4, collapse=" "),
                                  V50 = paste(V5, collapse=" "),
                                  V60 = paste(V6, collapse=" "),
                                  V70 = paste(V7, collapse=" ") ), by =.(V1, newv4)]
all_dose4 <- all_dose3[ , c("V1", "newv4", "V30", "V40", "V50", "V60", "V70" ), with =FALSE]
all_dose5 <- unique( all_dose4 )
setnames(all_dose5, "V1", "Source")
setnames(all_dose5, "newv4", "Group")
#setnames(all_dose5, "V20", "Sl.No")
setnames(all_dose5, "V30", "Medicine_Name")
setnames(all_dose5, "V40", "Dosage")
setnames(all_dose5, "V50", "Days")
setnames(all_dose5, "V60", "Qty")
setnames(all_dose5, "V70", "Remarks")

all_dose6 <- all_dose5[, `:=`(
  Dosage = paste("'", Dosage, sep=""),
  MRNo = substr(Source, 31, 38),
  Visit = substr(Source, 40, 43),
  Date = substr(Source, 45, 53)), ]

# Merge the subsetted data with the complete data
v16dose <- merge (x = all_dose6, 
                  y = v16, 
                  by =c("MRNo", "Visit", "Date"),
                  all.y = TRUE)

rm (list = ls( pattern = "all_dose*"))




data <- fread("c:/Users/Lucky/Documents/Hospital_data/04_2017_DOWNLOAD/pat_txts_mod/all_data022.txt",
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
  
  #dataout0 <<- dcast(data=chk999,
  
  dataout0 <- dcast(data=chk999,
                    Source + TMP1 + TMP3 + TMP4 ~ VAR1,
                    fill=" ",
                    value.var = c("VAR22"))
  
  assign(dataout, dataout0, envir=.GlobalEnv)
  
}

subdata('@@D1@', "D1")
subdata('@@D2@', "D2")

subdata('@@D8@', "D8")
subdata('@@D9@', "D9")
