
# Create the dosing related aligned table

# The UNIX code:
  

for fl in `find . -name "MR*.txt"|xargs -P10 -n20 grep -l "Medicine Name"`
do
dir=`dirname $fl`
file=`basename $fl`

awk '/Medicine Name/ {print $0}' $fl|sed "s/Medicine Name/Medicine_Name/g"|fold -w1|cat -n > ../pat_txts_mod/$dir"/fwdose_"$file
sed -n '/Medicine Name/,/^$/p' $fl|sed "s/Medicine Name/Medicine_Name/g" > ../pat_txts_mod/$dir"/dose_"$file
echo ../pat_txts_mod/$dir"/fwdose_"$file ../pat_txts_mod/$dir"/dose_"$file
done

# Execute the following command to get the field width files into 1 file
#/cygdrive/c/Users/Lucky/Documents/Hospital_data/04_2017_DOWNLOAD/pat_txts_mod
find . -name "fwdose*"|xargs awk '{print FILENAME, $0}' > fwdose_all.txt
cat fwdose_all.txt |tr -s "\t" " " > fwdose02_all.txt


# The following patients have "@"
# Replaced @ by 2, 36911
# @ = at for 20313, 30342
# @ removed 34144
$ find . -name "dose*"|xargs grep "@"|cat -n
1  ./MR007001_MR008000/dose_MR007855_0003_18APR2012_3.txt:                                                                                  Mahadhanwantaram gulika @-0-2
2  ./MR019001_MR020000/dose_MR019221_0002_01MAR2015_3.txt:                                                                                    @ gms with taleesapatradi churnam
3  ./MR020001_MR021000/dose_MR020313_0002_06NOV2013_3.txt:                                                                                       with warm milk @ 7 AM & 5 PM in
4  ./MR030001_MR031000/dose_MR030342_0050_29JUN2017_3.txt:1      Dhanwantharam Gritham Vaidya        2 tsf-0-2tsf       30          2          consumed with hot water @ 8 AM
5  ./MR034001_MR035000/dose_MR034144_0004_19AUG2015_3.txt:                                                                                      1drop each nostril twice daily.@tsf
6  ./MR034001_MR035000/dose_MR034144_0006_10OCT2015_3.txt:                                                                                          1drop each nostril twice daily.@tsf
7  ./MR036001_MR037000/dose_MR036911_0002_05DEC2015_3.txt:5      Prandhara Drops 3ml - Maharishi                            15          1         @ drops to the nostirls

library(data.table)
library(zoo)
library(stringi)
library(stringr)
library(openxlsx)

setwd("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts\\MR027001_MR028000\\")
files <- list.files(pattern ="fw*")

dta <- data.table()
for (fname in files) {
  dt <- fread(paste(fname), header=F, sep="\t")
  dt$FNAME <- fname
  dta <-rbind(dta,dt)
}

dta <- fread("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_mod\\fwdose02_all.txt", 
             header=F)

setnames(dta, "V1", "FNAME")
setnames(dta, "V2", "V1")
setnames(dta, "V3", "V2")

chk2 <- dta [, lag.V2:= c(NA, V2[-.N]), by =FNAME]
chk3 <- chk2 [(lag.V2 != "" & V2 == "") | (lag.V2 == "" & V2 != "") | V1 == 1]
chk44 <- chk3 [V2 != "" ]
chk45 <- chk44[, diff := c(NA, diff(V1)), by =FNAME]
chk450 <- na.omit(chk45)

chk46 <- chk450 [, list(text = paste(diff, collapse=" ") ), by =FNAME ]
chk46 <- chk46 [, list(text = paste(text, "50", sep =" ") ), by =FNAME ]
chk47 <- chk46[, c("TMP1", "TMP2", "TMP3") := tstrsplit(FNAME, "/", fixed=TRUE), ]

chk470 <- chk47 [, `:=` (awkst1 = "gawk 'BEGIN {FIELDWIDTHS=\"",
                         awkst2 = '"; OFS ="@";} { print $1, $2, $3, $4, $5, $6;}',
                         awkst3 = "' ",
                         awkst4 = paste( "dose_", substr(TMP3, 8, 70), sep=""),
                         awkst5 = paste( "aligndose_", substr(TMP3, 8, 50), sep="" )),]

chk471 <- chk470 [, code := paste(awkst1, text, awkst2, awkst3, TMP2, "/", awkst4, "> ", TMP2, "/", awkst5, sep = ""),]

write.xlsx(DT2,
           sheetName ="sheet1",
           file = "C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pgrm\\01pat_url.xlsx", 
           colNames = TRUE, 
           borders = "columns")

write.table(chk471$code, 
            "C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_mod\\awk_dose.sh", 
            row.names=FALSE, 
            col.names=FALSE,
            quote= FALSE)

# Run the following codes in Unix
# dos2unix awk_dose.sh
# bash awk_dose.sh


# Run the AWK code to get all the aligned files into 1 file
find . -name "aligndose*"|xargs awk '{print FILENAME "@" $0}' >all_aligndose.txt

all_dose <- fread("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_mod\\all_aligndose.txt",
                  header=F, 
                  sep="@")

all_dose0 <- all_dose [V2 != "Sl.No"]
all_dose2 <- all_dose0[!nzchar(V2),V2:=NA][,V20:=na.locf(V2), by =V1]
all_dose3 <- all_dose2 [ , `:=` (V30 = paste(V3, collapse=" "),
                                V40 = paste(V4, collapse=" "),
                                V50 = paste(V5, collapse=" "),
                                V60 = paste(V6, collapse=" "),
                                V70 = paste(V7, collapse=" ") ), by =.(V1, V20)]
all_dose4 <- all_dose3[ , c("V1", "V20", "V30", "V40", "V50", "V60", "V70"), with =FALSE]
all_dose5 <- unique( all_dose4 )
setnames(all_dose5, "V1", "Source")
setnames(all_dose5, "V20", "Sl.No")
setnames(all_dose5, "V30", "Medicine_Name")
setnames(all_dose5, "V40", "Dosage")
setnames(all_dose5, "V50", "Days")
setnames(all_dose5, "V60", "Qty")
setnames(all_dose5, "V70", "Remarks")

