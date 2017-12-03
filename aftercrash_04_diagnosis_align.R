# Create the dosing related aligned table

# The UNIX code:

for fl in `find . -name "MR*.txt"|xargs -P10 -n20 grep -l "Consultation Details"`
do
dir=`dirname $fl`
file=`basename $fl`

awk '/Diag. Code/ {print $0}' $fl|sed "s/Diag. Code/Diag__Code/g"|sed "s/Diag. Type/Diag__Type/g"|fold -w1|cat -n > ../pat_txts_diagnosis/$dir"/fwdiag_"$file
sed -n '/Diag. Code/,/^$/p' $fl|sed "s/Diag. Code/Diag__Code/g"|sed "s/Diag. Type/Diag__Type/g" > ../pat_txts_diagnosis/$dir"/diag_"$file
echo ../pat_txts_diagnosis/$dir"/fw_"$file ../pat_txts_diagnosis/$dir"/fw_diag_"$file
done


# Execute the following command to get the field width files into 1 file
#/cygdrive/d/Hospital_data/04_2017_DOWNLOAD/pat_txts_diagnosis
find . -name "fwdiag*"|xargs awk '{print FILENAME, $0}' > fwdiag_all.txt
cat fwdiag_all.txt |tr -s "\t" " " > fwdiag02_all.txt

# Find files with @ symbol
#/cygdrive/d/Hospital_data/04_2017_DOWNLOAD/pat_txts_diagnosis
find . -name "diag*"|xargs grep "@"|cat -n

# These files have "@" symbol, which should be removed
1  ./MR034001_MR035000/diag_MR034565_0002_19AUG2015_3.txt
2  ./MR034001_MR035000/diag_MR034565_0003_19AUG2015_3.txt
3  ./MR040001_MR041000/diag_MR040873_0001_01JUN2016_3.txt


library(data.table)
library(zoo)
library(stringi)
library(stringr)
library(openxlsx)

setwd("D:\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_diagnosis\\")
dta <- fread("D:\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_diagnosis\\fwdiag02_all.txt", 
             header=F)

setnames(dta, "V1", "FNAME")
setnames(dta, "V2", "V1")
setnames(dta, "V3", "V2")

chk2 <- dta [, lag.V2:= c(NA, V2[-.N]), by =FNAME]
chk3 <- chk2 [(lag.V2 != "" & V2 == "") | (lag.V2 == "" & V2 != "") | V1 == 1]
chk44 <- chk3 [V2 != ""]
chk45 <- chk44[, diff := c(NA, diff(V1)), by =FNAME]
chk450 <- na.omit(chk45)

chk46 <- chk450 [, list(text = paste(diff, collapse=" ") ), by =FNAME ]
chk46 <- chk46 [, list(text = paste(text, "50", sep =" ") ), by =FNAME ]
chk47 <- chk46[, c("TMP1", "TMP2", "TMP3") := tstrsplit(FNAME, "/", fixed=TRUE), ]

chk470 <- chk47 [, `:=` (awkst1 = "gawk 'BEGIN {FIELDWIDTHS=\"",
                         awkst2 = '"; OFS ="@";} { print $1, $2, $3, $4;}',
                         awkst3 = "' ",
                         awkst4 = paste( "diag_", substr(TMP3, 8, 70), sep=""),
                         awkst5 = paste( "aligndiag_", substr(TMP3, 8, 50), sep="" )),]

chk471 <- chk470 [, code := paste(awkst1, text, awkst2, awkst3, TMP2, "/", awkst4, "> ", TMP2, "/", awkst5, sep = ""),]

write.table(chk471$code, 
            "D:\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_diagnosis\\awk_diag.sh", 
            row.names=FALSE, 
            col.names=FALSE,
            quote= FALSE)

# Run the following codes in Unix
# Convert the file to an executable file
# dos2unix awk_diag.sh
# chmod 777 awk_diag.sh
# bash awk_diag.sh

# Run the AWK code to get all the aligned files into 1 file
find . -name "aligndiag*"|xargs awk '{print FILENAME "@" $0}' >all_aligndiag.txt

all_dose <- fread("D:\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_diagnosis\\all_aligndiag.txt",
                  header=F, 
                  sep="@",
                  fill=TRUE)

all_dose0 <- all_dose [V2 != "Diag__Type"]
#all_dose01 <- all_dose0 [nzchar(V2) & nzchar(V3) & nzchar(V4) & nzchar(V5)]

all_dose01 <- all_dose0 [ stri_trim(V2) != "" & stri_trim(V3) != "" & stri_trim(V4) != "" ]
all_dose01 <- unique( all_dose01)

all_dose01 <- all_dose01 [ , cnt :=1:.N, by =.(V1, V2)]
all_dose01 <- all_dose01 [, `:=` (MRNo = substr(V1, 31, 38),
                                  Visit = substr(V1, 40, 43),
                                  Date = substr(V1, 45, 53)), ]

fwrite(all_dose, 
       "D:\\Hospital_data\\04_2017_DOWNLOAD\\pat_dbs\\adiag.csv", 
       row.names=FALSE, 
       col.names=FALSE)

