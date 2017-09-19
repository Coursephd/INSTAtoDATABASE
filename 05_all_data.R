
# Create all data

# The UNIX code:

# Run this code at pat_txts_mod
for fl in `find . -name "MR*.txt"`
do
dir=`dirname $fl`
file=`basename $fl`

sed -f /cygdrive/c/Users/Lucky/Documents/Hospital_data/04_2017_DOWNLOAD/pgrm/replace.txt $fl > ../pat_txts_mod/$dir"/data_"$file
echo ../pat_txts_mod/$dir"/data_"$file 
done

# Run the following code to get the data into 1 file, almost 45 lakh records
/cygdrive/c/Users/Lucky/Documents/Hospital_data/04_2017_DOWNLOAD/pat_txts_mod
find . -name "data_*" -type f|xargs awk '{print FILENAME, @ $0}' > all_data.txt

library(data.table)
library(zoo)
library(stringi)
library(stringr)
library(openxlsx)

data <- fread("grep '@@D3@' c:/Users/Lucky/Documents/Hospital_data/04_2017_DOWNLOAD/pat_txts_mod/all_data.txt",
      sep="!", 
      header= FALSE)

data02 <- data [, c("TMP1", "TMP2", "TMP3", "TMP4", "TMP5") := tstrsplit(stri_trim(V1), "@", fixed=TRUE),]

rm(data)

data03 <- data02 [, c("VAR1","VAR2") := tstrsplit(stri_trim(TMP5), ":", fixed=TRUE), ]
data03 <- data03 [, `:=`(VAR1 = stri_replace_all(toupper(VAR1), fixed = " ",""),
                         Source = substr(TMP1, 26, 33)),]

chk <- data03[, cnt :=.N, by =.(TMP1, VAR1)]

data04 <- dcast(data=chk,
                subset= .(cnt == 1),
                Source + TMP1 + TMP3 + TMP4 ~ VAR1,
                fill =" ",
                value.var = c("VAR2"))

data33 <- unique(data03)
data44 <- dcast(data=data33,
                Source + TMP1 + TMP3 + TMP4 ~ VAR1,
                fill=" ",
                value.var = c("VAR2"))
