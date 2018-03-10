
library(data.table)
library(stringi)
library(stringr)
library(openxlsx)
library(anytime)
library(zoo)
library(sqldf)

###################################################################
# Use EMRURL information and Create the start and end date
# Use this information to create total duration for which a patient
# comes in as well as find maximum number of visit equivalent info
###################################################################

vinay <- fread("D:\\Hospital_data\\04_2017_DOWNLOAD\\emrurl\\allpat_date_url02.txt", sep=",", header=FALSE)

alldata <- data.table( vinay[, c("Source", "SRC1", "SRC2", "tmp2") := tstrsplit(V1, " ", fixed=TRUE), ])

# Create a row number for each row
alldata <- alldata [, row :=1:.N, by = Source]
setkey(alldata, SRC1)

# Create a Group variable for SRC1
alldata1 <- alldata[ , javafld := .GRP, by = key(alldata) ]

# Create another group variable for patient and visit 
alldata1 <- alldata1 [, patgrp := 1:.N, by =.(Source, SRC1)]

# Transpose the data for 
alldata2 <- data.table ( dcast( data = alldata1, 
                                formula = Source + patgrp ~ SRC1,
                                value.var = c("SRC2") 
                       ))
alldata2 <- alldata2 [, dt := ifelse(date == "null" & visitDate != " ", visitDate, date)]

alldata2 <- alldata2 [, dttmp := ( as.numeric(dt)/86400000) ]
alldata2 <- alldata2 [, date2 := toupper ( format ( anydate(dttmp), "%d%b%Y"))]

alldata2 <- alldata2[order(alldata2$Source, alldata2$dttmp),]
alldata2 <- alldata2 [, patgrp02 := 1:.N, by =.(Source)]

minmax <- alldata2 [, `:=` (mindt = min(dttmp),
                            maxdt = max(dttmp),
                            maxvis = max(patgrp02),
                            MRNo =substr(Source, 21, 28)), by =.(Source)]

minmax2 <- unique( minmax [, c("Source", "mindt", "maxdt", "maxvis", "MRNo")])
minmax2 <- minmax2 [, totdur := round( maxdt - mindt + 1, 0),]

#################################
# Code to generate DIAGNOSIS data
#################################

##############################
#Read the 2016 version of data
##############################

diag <- fread("D:\\Hospital_data\\01_31JUL2016\\source\\Diagnosis.csv", check.names = FALSE)
diag <- diag[, c(1, 2, 4, 5, 6, 8, 9, 10, 23, 29, 34, 44, 53), with=FALSE ]

##############################
#Read the 2016 to 2017 version of data
##############################

diag_add <- fread("D:\\Hospital_data\\04_2017_DOWNLOAD\\source\\Diagnosis.csv", check.names = FALSE)
diag_add2 <- diag_add[, c(1, 2, 4, 5, 6, 8, 9, 10, 21, 23, 28, 35, 45), with=FALSE ]

diag_all <- rbind(diag, diag_add2)

diag2 <- diag_all[, `:=` (data = "diag",
                     type = substr(`Patient Id`, 1, 2),
                     cal = 1), ]

setnames(diag2, "MR No.", "MRNo")
setnames(diag2, "Admission Date", "visdate")
setnames(diag2, "Blood Group", "Bloodgrp")
setnames(diag2, "Age In", "AgeIn")
setnames(diag2, "First Visit Date", "fvisdate")

#######################################
# Merge the min and max dates onto DIAG
# Keep patients who are present in MINMAX2
#######################################

diag2 <- merge(x=diag2, 
                  y=minmax2, 
                  by ="MRNo",
                  all.y = TRUE)


# Create date variables and find the difference
diag2 <- diag2[, visdate0 := as.POSIXct( gsub("-", "/", visdate), format="%d/%m/%Y") ]
#diag2 <- diag2[, fvisdate := as.POSIXct( gsub("-", "/", fvisdate), format="%d/%m/%Y") ]

diag2 <- diag2[, visday := round( as.Date(visdate0) - as.Date(mindt) + 1, 0)]

diag2 <- diag2[, vismon := round(visday /30.4375, 1)]
diag2 <- diag2[, Age := ifelse(AgeIn =="M", Age/12, Age), ]
diag2 <- diag2[, AgeIn := ifelse(AgeIn =="M", "Y", AgeIn), ]

# Code ** NOT YET CODED for the missing code values
# Sort the data by patient and day
diag2 <- diag2[, Code2 := ifelse(Code =="", "ZZZ999", Code), ] [order(MRNo, visday, Code2)]

# Create number of diagnosis per patient and a counter for each diagnosis
diag2 <- diag2[ , noofdis := uniqueN(Code2), by = .(MRNo) ]

# Count number of unique diagnosis per patient per day
# Sort the data by patient and day
diag2 <- diag2[ , `:=` (IDX = 1: .N), by = .(MRNo, visdate0, visday) ] [order(MRNo, visday, IDX, Code2)]

# Get the first date of the diagnosis by each code
diag2 <- diag2 [, min := min(visday), by = .(MRNo, Code2)]

# Get the maximum date of the diagnosis (end date for each patient)
# Use this data with vital sign data to get diagnosis attached to vital sign measurements
diag2 <- diag2 [, max := max(visday), by = MRNo]

setkeyv (diag2, c("MRNo", "Code2", "Description", "min", "max", "type", "IDX", "noofdis"))

diag2 <- unique(diag2)

######################################################
# Get the disease category list for RMSD and Metabolic
######################################################

discat <- data.table( fread ("D:\\Hospital_data\\04_2017_DOWNLOAD\\pat_dbs\\discategory.csv") )

subset <- merge (x = discat[, -c("Description"), with =FALSE],
                 y = diag2,
                 by = "Code")

# create a dummy variable
subset <- subset[ ,val:=1]

subset2 <- subset[, c("MRNo", "distype", "val"), with =FALSE]
subset2 <- unique(subset2)

subset3 <- dcast (data = subset2,
                  fill =0,
                  MRNo ~ distype,
                  value.var="val")

# Create an indicator variable to determine
# Both Metabolic and RMSD = 99
# Only Metabolic = 1
# Only RMSD = 2
subset3 <- subset3 [Metabolic == 1 & RMSD == 1, combine := 99]
subset3 <- subset3 [Metabolic == 1 & RMSD == 0, combine := 1]
subset3 <- subset3 [Metabolic == 0 & RMSD == 1, combine := 2]

subset4 <- subset3 [ MRNo %in% grep ("^MR", MRNo, value = TRUE)]
subset4 <- subset4 [ as.numeric(substr(MRNo, 3, 10)) < 49000   ]

# Combine the diag2 dataset and discat data and
# determine diseases which are in RMSD and Metabolic categories vs. OTHER categories
subset5 <- merge (x = discat[, -c("Description"), with =FALSE],
                  y = diag2[, -c("Code"), with =FALSE],
                  all = TRUE,
                  by.x = "Code",
                  by.y = "Code2")

subset5 <- subset5 [,-c("Age", "AgeIn", "Gender", "City", "Country",
                        "Bloodgrp", "data", "type", "noofdis"), with =FALSE]

# Code non metabolic and non RMSD diseases as OTHER
subset5$distype[is.na(subset5$distype)] <- "OTHER"

# Count number of diseases and freq count for each patient
# ??????????????????????????????????????????????????????
# ???? Understand why Code2 is not coming out correctly
# ??????????????????????????????????????????????????????
subset6 <- subset5[, .(cnt = uniqueN(Code),
                       frq = .N), by =.(MRNo, distype)]

# Transpose the data and get in 1 row per patient
subset7 <- dcast(subset6,
                 MRNo ~ distype,
                 value.var = c("cnt", "frq"),
                 fill = 0)

# Unique patient values
n1unq <- unique ( diag2 [, c("MRNo", "Age", "AgeIn", "Gender", "City","Country"), with = FALSE] )

# No of diseases
n1dis <- unique ( diag2 [, c("MRNo", "noofdis"), with = FALSE] )

# Blood group creation
n1blood <- unique( diag2 [ Bloodgrp != "", c("MRNo", "Bloodgrp"), with =FALSE])

# OP, IP creation
n1type <- unique (diag2 [ type != "", c("MRNo", "type", "cal"), with =FALSE])
n2type <- dcast(n1type, MRNo ~ type, value.var = "cal")

# Combine all variables into 1 dataset ADSL
adsl <- Reduce(function(...) merge(..., all = TRUE, by = "MRNo"),
               list(n1unq, n1dis, n1blood, n2type, subset4, subset7))

########################################################
# Create a subsetted data for RMSD and Metabolic diseases
# For analysis create a few additional variables
########################################################

rmsd_met <- Reduce(function(...) merge(..., all.x = TRUE, by = "MRNo"),
                   list(subset5, adsl))

rmsd_met2 <- rmsd_met[combine >0, -c("date", "Patient Id", "cal", "fvisdate", "visdate"), with = FALSE]
fwrite(rmsd_met2, 
       "D:\\Hospital_data\\04_2017_DOWNLOAD\\pat_dbs\\rmsd_met_adsl.csv", 
       row.names=FALSE,
       quote="auto")

saveRDS(rmsd_met, "d:/Hospital_data/04_2017_DOWNLOAD/pat_dbs/rmsd_met.rdata")

gender <- rmsd_met[ combine > 0, .(cnt = uniqueN(MRNo)), by =.(combine, Gender)]
