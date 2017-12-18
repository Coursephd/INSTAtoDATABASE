# Tracker program for finding files

library(data.table)
library(stringi)
library(stringr)
library(openxlsx)
library(anytime)
library(zoo)
library(sqldf)


#setwd ("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\emrurl\\")

########################
## Section of planning
########################

# This DT part is from01pat_url.R program
# IF there is a change in that program then that needs to be accounted for
# Create macro call for patients to get the source code
DT <- data.table ( expand.grid (x=c(1:49000), outxls=c(1:20)) )

DT <- DT[, x2 := paste( "MR", str_pad(x, 6, side = "left", pad = 0), sep=""), ]
DT <- DT[ , `:=` (folder = ceiling(x / 1000),
                  x3 = x + 1)]

DT <- DT [, `:=` (stt = min (x), 
                  end = max (x),
                  stt2 = min(x3), 
                  end2 = max(x3) ), by = .(folder) ]

DT <- DT [, `:=` (sttname = paste( "MR", str_pad(stt, 6, side = "left", pad = 0), sep=""),
                  endname = paste( "MR", str_pad(end, 6, side = "left", pad = 0), sep="") )]
DT <- DT [, outfld := paste (sttname, endname, sep ="_"),]
DT <- DT [, Source := paste("./", outfld, "/", x2, sep=""),]

# Primary sheet for patient info
DT2 <- DT[, c("x2", "outxls", "outfld", "Source")]


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
                                value.var = c("SRC2") ))
alldata2 <- alldata2 [, dt := ifelse(date == "null" & visitDate != " ", visitDate, date)]

alldata2 <- alldata2 [, dttmp := ( as.numeric(dt)/86400000) ]
alldata2 <- alldata2 [, date2 := toupper ( format ( anydate(dttmp), "%d%b%Y"))]
alldata2 <- alldata2[order(alldata2$Source, alldata2$dttmp),]
alldata2 <- alldata2 [, patgrp02 := 1:.N, by =.(Source)]

alldata2 <- alldata2 [, nn := paste("https://182.71.223.195/instahms", displayUrl, sep="")]
# Create another group variable for patient and visit 

#alldata4 <- subset (alldata3 , !(nn %like% c('vitalForm', 'DiagnosticModule' ) ))
alldata5 <- data.table( alldata2[, c("suburl1", "suburl2", "suburl3", "suburl4") := tstrsplit(displayUrl, "=", fixed=TRUE), ])

# This step would not have been necessary,if the earlier files were not created
# .GRP function from the DATA.TABLE creates these groups without having to define these groups

alldata55 <- sqldf("select *, 
                   case 
                   When suburl1 == '/vitalForm/genericVitalForm.do?method' then 1
                   when suburl1 == '/pages/GenericDocuments/GenericDocumentsPrint.do?_method' then 2
                   when suburl1 == '/emr/print.do?method' AND suburl2 ='printConsultation&consultation_id' then 3
                   when suburl1 == '/emr/print.do?method' AND suburl2 ='printTriageSummary&consultation_id' then 4
                   when suburl1 == '/dischargesummary/dischargesummaryPrint.do?_method' then 5
                   when suburl1 == '/Service/ServicesConductionPrint.do?_method' then 6
                   when suburl1 == '/pages/DiagnosticModule/DiagReportPrint.do?_method' then 7
                   when suburl1 == '/progress/PatientProgress.do?_method' then 8
                   when suburl1 == '/wardactivities/DoctorOrderPrint.do?_method' then 9
                   when suburl1 == '/wardactivities/VisitSummaryRecord.do?_method' then 10
                   when suburl1 == '/IntakeOutput/genericIntakeOutputForm.do?method' then 11
                   when suburl1 == '/dietary/DietaryMasterPrint.do?method' then 12
                   when suburl1 == '/Service/ServiceReportsPrint.do?_method' then 13
                   when suburl1 == '/Radiology/TestDocumentsPrint.do?_method' then 14
                   when suburl1 == '/Dietary/DietaryGenericDocumentsPrint.do?_method' then 15
                   when suburl1 == '/MLCDocuments/MLCDocumentPrint.do?_method' then 16
                   when suburl1 == '/wardactivities/DoctorsNotes.do?_method' then 17
                   when suburl1 == '/InitialAssessment/InitialAssessmentPrint.do?_method' then 18
                   when suburl1 == 'Discharge' then 19
                   when suburl1 == '/GenericForms/GenericFormPrintAction.do?_method' then 20  
                   end as outxls
                   from alldata5")

alldata55 <- data.table(alldata55)
alldata55 <- alldata55 [, `:=`(outfld = substr(Source, 3, 19),
                               outfile = paste(stri_trim(substr(Source, 21, 29)), "_", 
                                               str_pad(patgrp02, 4, side = "left", pad = 0), "_", 
                                               date2, "_", 
                                               outxls, ".pdf", sep="") ), ]
alldata55 <- alldata55[, nrow := 1:.N, by =outxls]

alldata60 <- alldata55 [, .(planned =.N), by =.(Source, outfld, outxls)]

# Read the actual number of downloaded PDF files
# UNIX code
# /cygdrive/d/Hospital_data/04_2017_DOWNLOAD/pat_pdfs
# find . -name "*.pdf" -type f |xargs ls -lR --full-time > all_pdfs.txt
# cat all_pdfs.txt|tr "_/" "  "|sed "s/.pdf//g"|awk '{print $9 "/" $10 "_" $11 "/" $12, $15}' |sort| uniq -c|awk '{print $2, $3, $1}' > all_pdf_count.txt

act_pdf <- fread("d:/Hospital_data/04_2017_DOWNLOAD/pat_pdfs/all_pdf_count.txt",
                 header=FALSE)
setnames(act_pdf, "V1", "Source")
setnames(act_pdf, "V2", "outxls")
setnames(act_pdf, "V3", "sourcepdf")


# Read the actual number of downloaded TXT files
# UNIX code
# /cygdrive/d/Hospital_data/04_2017_DOWNLOAD/pat_txts
# find . -name "MR*.txt" -type f |xargs ls -lR --full-time > all_txts.txt
# cat all_txts.txt|tr "_/" "  "|sed "s/.txt//g"|awk '{print $9 "/" $10 "_" $11 "/" $12, $15}' |sort| uniq -c|awk '{print $2, $3, $1}' > all_txt_count.txt

act_txt <- fread("d:/Hospital_data/04_2017_DOWNLOAD/pat_txts/all_txt_count.txt",
                 header=FALSE)
setnames(act_txt, "V1", "Source")
setnames(act_txt, "V2", "outxls")
setnames(act_txt, "V3", "sourcetxt")

# Read the actual number of modified TXT files
# UNIX code
# /cygdrive/d/Hospital_data/04_2017_DOWNLOAD/pat_txts_mod
# find . -name "MR*.txt" -type f |xargs ls -lR --full-time > all_txts_mod.txt
# cat all_txts_mod.txt|tr "_/" "  "|sed "s/.txt//g"|awk '{print $9 "/" $10 "_" $11 "/" $12, $15}' |sort| uniq -c|awk '{print $2, $3, $1}' > all_txt_mod_count.txt

mod_txt <- fread("d:/Hospital_data/04_2017_DOWNLOAD/pat_txts_mod/all_txt_mod_count.txt",
                 header=FALSE)
setnames(mod_txt, "V1", "Source")
setnames(mod_txt, "V2", "outxls")
setnames(mod_txt, "V3", "modtxt")

# Read the actual number of dosing TXT files
# UNIX code
# The AWK part of the code is slightly different due to "diag" name in the filename
# /cygdrive/d/Hospital_data/04_2017_DOWNLOAD/pat_txts_dosing
# find . -name "dose_*.txt" -type f |xargs ls -lR --full-time > all_txts_dosing.txt
# cat all_txts_dosing.txt|tr "_/" "  "|sed "s/.txt//g"|awk '{print $9 "/" $10 "_" $11 "/" $13, $16}' |sort| uniq -c|awk '{print $2, $3, $1}' > all_txt_dosing_count.txt

dosing_txt <- fread("d:/Hospital_data/04_2017_DOWNLOAD/pat_txts_dosing/all_txt_dosing_count.txt",
                 header=FALSE)
setnames(dosing_txt, "V1", "Source")
setnames(dosing_txt, "V2", "outxls")
setnames(dosing_txt, "V3", "dosingtxt")


# Read the actual number of diagnosis TXT files
# UNIX code
# The AWK part of the code is slightly different due to "diag" name in the filename
# /cygdrive/d/Hospital_data/04_2017_DOWNLOAD/pat_txts_diagnosis
# find . -name "diag_*.txt" -type f |xargs ls -lR --full-time > all_txts_diag.txt
# cat all_txts_diag.txt|tr "_/" "  "|sed "s/.txt//g"|awk '{print $9 "/" $10 "_" $11 "/" $13, $16}' |sort| uniq -c|awk '{print $2, $3, $1}' > all_txt_diag_count.txt

diag_txt <- fread("d:/Hospital_data/04_2017_DOWNLOAD/pat_txts_diagnosis/all_txt_diag_count.txt",
                    header=FALSE)
setnames(diag_txt, "V1", "Source")
setnames(diag_txt, "V2", "outxls")
setnames(diag_txt, "V3", "diagtxt")


# Read the actual number of EMRURL files
# UNIX code
# /cygdrive/d/Hospital_data/04_2017_DOWNLOAD/emrurl
# find . -name "MR*.txt" -type f |xargs ls -lR --full-time > all_urls.txt

emrurl <- fread("d:/Hospital_data/04_2017_DOWNLOAD/emrurl/all_urls.txt",
                header=FALSE)
emrurl <- emrurl [, Source := substr(V9, 1, 28), ]
setnames(emrurl, "V5", "Noofurls")

emrurl02 <- emrurl[, c("Source", "Noofurls"), ]

setkey(DT2, "Source", "outxls")
setkey(alldata60, "Source", "outxls")
setkey(act_pdf, "Source", "outxls")
setkey(act_txt, "Source", "outxls")
setkey(mod_txt, "Source", "outxls")
setkey(emrurl02, "Source")


compare <- merge(DT2, alldata60, by = c("Source", "outxls"), all= TRUE)
compare <- merge(compare, act_pdf, by = c("Source", "outxls"), all= TRUE)
compare <- merge(compare, act_txt, by = c("Source", "outxls"), all= TRUE)
compare <- merge(compare, mod_txt, by = c("Source", "outxls"), all= TRUE)
compare <- merge(compare, diag_txt, by = c("Source", "outxls"), all= TRUE)
compare <- merge(compare, dosing_txt, by = c("Source", "outxls"), all= TRUE)


compare <- merge(compare, emrurl02, by = "Source", all= TRUE)
setnames(compare, "outfld.x", "outfld")
compare2 <- compare[, c("Source", "outfld", "outxls", "planned", "sourcepdf", "sourcetxt", "modtxt", "diagtxt", "dosingtxt", "Noofurls"), ]
compare2 <- compare2[, URLS :=ifelse(Noofurls == "6", "NODATA", "DATA-AVAILABLE"), ]

# Count the number of patients having data available, vs. no data
summary <- compare2[, .(cnt = uniqueN(Source)), by =.(outfld, URLS)]
summary2 <- dcast(data = summary, 
                  outfld ~ URLS,
                  value.var = c("cnt"))

# Find the distinct type of files present for each patient
# Get the combinations of program numbers and then find the unique patient count
# E.g. MR004001_MR005000 has 943 patients and 200 patients have only output type 7(lab data)

types <- compare2 [planned > 0]
types2 <- types[, outtype := paste(outxls, collapse=","), by = .(outfld, Source)]
types3 <- types2 [, .(cnt = uniqueN(Source)), by = .(outfld, outtype)]