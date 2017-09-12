
# Create the dosing related aligned table

# The UNIX code:

for fl in `find . -name "MR*.txt"|xargs -P10 -n20 grep -l "Consultation Details"`
do
dir=`dirname $fl`
file=`basename $fl`

awk '/Diag. Code/ {print $0}' $fl|sed "s/Diag. Code/Diag__Code/g"|sed "s/Diag. Type/Diag__Type/g"|fold -w1|cat -n > ../pat_txts_mod/$dir"/fwdiag_"$file
sed -n '/Diag. Code/,/^$/p' $fl|sed "s/Diag. Code/Diag__Code/g"|sed "s/Diag. Type/Diag__Type/g" > ../pat_txts_mod/$dir"/diag_"$file
echo ../pat_txts_mod/$dir"/fw_"$file ../pat_txts_mod/$dir"/fw_diag_"$file
done

# Execute the following command to get the field width files into 1 file
#/cygdrive/c/Users/Lucky/Documents/Hospital_data/04_2017_DOWNLOAD/pat_txts_mod
find . -name "fwdig*"|xargs awk '{print FILENAME, $0}' > fwdig_all.txt
cat fwdig_all.txt |tr -s "\t" " " > fwdig02_all.txt
  
library(data.table)
library(zoo)
library(stringi)
library(stringr)

setwd("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts\\MR027001_MR028000\\")
files <- list.files(pattern ="fw*")

dta <- data.table()
for (fname in files) {
  dt <- fread(paste(fname), header=F, sep="\t")
  dt$FNAME <- fname
  dta <-rbind(dta,dt)
}

dta <- fread("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_mod\\fwdig02_all.txt", 
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
                         awkst4 = paste( "diag_", substr(TMP3, 7, 70), sep=""),
                         awkst5 = paste( "aligndiag_", substr(TMP3, 7, 50), sep="" )),]

chk471 <- chk470 [, code := paste(awkst1, text, awkst2, awkst3, TMP2, "/", awkst4, "> ", TMP2, "/", awkst5, sep = ""),]

write.table(chk471$code, 
            "C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_mod\\awk_diag.txt", 
            row.names=FALSE, 
            col.names=FALSE,
            quote= FALSE)

# Execute the file to get the aligned text files
# mv awk_diag.txt awk_diag.sh
# parallel --progress -j500  -eta ::: 'bash awk_diag.sh' -- Tried but does not work
# bash awk_diag.sh

# These files have "@" symbol, which should be removed
1  ./MR034001_MR035000/diag_MR034565_0002_19AUG2015_3.txt
2  ./MR034001_MR035000/diag_MR034565_0003_19AUG2015_3.txt
3  ./MR040001_MR041000/diag_MR040873_0001_01JUN2016_3.txt

# Run the AWK code to get all the aligned files into 1 file
find . -name "aligndiag*"|xargs awk '{print FILENAME "@" $0}' >all_aligndiag.txt

all_dose <- fread("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_mod\\all_aligndiag.txt",
                  header=F, 
                  sep="@",
                  fill=TRUE)

all_dose0 <- all_dose [V2 != "Diag__Type"]
all_dose01 <- all_dose0 [nzchar(V2) & nzchar(V3) & nzchar(V4) & nzchar(V5)]


write.table(all_dose, 
            "C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_dbs\\adiag.csv", 
            row.names=FALSE, 
            col.names=FALSE,
            quote= FALSE)


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

