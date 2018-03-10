
library(data.table)

######################
# Take dosing data
# Create a few variables
######################
dd <- readRDS ("d:/Hospital_data/04_2017_DOWNLOAD/pat_dbs/adose.rds")
all_dose6 <- dd[, `:=`(MRNo =substr(dd$Source, 31, 38),
                       dosdate=substr(dd$Source, 45, 53),
                              Dosage = paste("'", Dosage, sep="")), ]

###############################################
# Read the older data 
# Unique patients with Metabolic, RMSD diseases
# Combine this data with dosing data
#
# Read data with diagnosis information
# combine this data with dosing data
################################################
sub <- fread("D:\\RD-417\\Drive 1\\My work\\Hospital data\\01_31JUL2016\\analysis\\rmsd_met_unique_patients.csv")
rmsd_met_diag <- fread("D:\\RD-417\\Drive 1\\My work\\Hospital data\\01_31JUL2016\\analysis\\rmsd_met_diag.csv")

rmsd_met_diag <- rmsd_met_diag[, visdate2 := visdate]
sub_dose <- merge(x=all_dose6, 
      y=sub, 
      by ="MRNo",
      all.y = TRUE)


sub_dose <- sub_dose[, dosdate2 := as.character(as.POSIXct( gsub("-", " ", dosdate), format="%d%b%Y")) ]

sub_dose02 <- merge(x=sub_dose, 
                  y=rmsd_met_diag, 
                  by.x = c("MRNo", "dosdate2"),
                  by.y = c("MRNo", "visdate"),
                  all.x = TRUE,
                  all.y = TRUE,
                  allow.cartesian = TRUE)

fwrite(sub_dose02, 
       "D:\\Hospital_data\\04_2017_DOWNLOAD\\pat_dbs\\rmsd_met_dose_diag.csv", 
       row.names=FALSE,
       quote="auto")


b01muri <- sub_dose[ tolower(Medicine_Name) %like% c("muri"), .(cnt = uniqueN(patid)), by = combine]
b05guggulu <- sub_dose [tolower(Medicine_Name) %like% c("guggul"), .(cnt = uniqueN(patid)), by = combine]

library(wordcloud)

wordcloud(tolower(sub_dose$Medicine_Name))

saveRDS(D8, "d:/Hospital_data/04_2017_DOWNLOAD/pat_dbs/D8try.rdata")
dd <- readRDS ("d:/Hospital_data/04_2017_DOWNLOAD/pat_dbs/D8try.rdata")


dd <- all_dose5[, c("tmp1v4", "tmp2v4") := tstrsplit( gsub("\\s+", " ", str_trim(newv4)), " ", fill="") , ]

dd <- dd [, V30new := ifelse ( tmp2v4 == "", str_trim(V30), str_trim(paste(tmp2v4,V30, sep="")) ) ,]

unq <- data.table (unique ( all_dose6 [, c("Dosage") ] ))


