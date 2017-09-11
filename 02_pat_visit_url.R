#######################################################
# Filename 02pat_visit_url.R
# Based on 01pat_url.xlsx file to get the individual
# files for each visit
# This should be executed only once
#######################################################

#2nd attempt successful + After finding the error of sorting:

vinay@viany /cygdrive/c/users/Lucky/Documents/Hospital_data/04_2017_DOWNLOAD/emrurl
$ find . -name "MR*.txt" -print|xargs awk '/displayUrl/ || /"date"/ {print FILENAME, $0}'|sed 's/\.txt//g'|tr -s '":' "  " > allpat_date_url.txt

find . -name "MR*.txt" -print|xargs awk '/displayUrl/ || /"date"/ || /"visitDate"/ {print FILENAME, $0}'|sed 's/\.txt//g'|tr -s '":' "  " > allpat_date_url02.txt
#awk '{$1 = substr($1, 21, 30)} 1'

library(data.table)
library(stringi)
library(stringr)
library(openxlsx)
library(anytime)
library(zoo)
library(sqldf)


setwd ("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\emrurl\\")

vinay <- fread("allpat_date_url02.txt", sep=",", header=FALSE)

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

curlparm <- c(" -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.8' -H 'Upgrade-Insecure-Requests: 1' -H 'User-Agent: Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.101 Safari/537.36' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' -H 'Referer: https://182.71.223.195/instahms/loginForm.do'  -H 'Cookie: JSESSIONID=")
curlparm2 <- c("; user_schema@/instahms=iaim; firstMenuIndex=0; lastUser=VmluYXltYWhhamFu' -H 'Connection: keep-alive' -H 'Cache-Control: max-age=0' --compressed --insecure >")
alldata55 <- alldata55[, `:=` (out1 = paste("echo '>>>'", nrow, " ",outfile, ";curl '", nn, "'", curlparm, sep=""), 
                               out2 = "",
                               out3 = paste(curlparm2, outfld, "/", outfile, sep=""))]
alldata6 <- alldata55 [, c("Source", "outxls", "date2", "out1", "out2", "out3","patgrp02")]

#alldata6 <- alldata55 [, c("Source", "nn", "outfld", "outfile", "outxls", "date2", "patgrp02")]

# Try the following step with write.xlsx
# Currently did the same thing using for loop
#alldata5 [, write.csv(c(.BY,.SD), paste0("02_pat_url_by_", .BY, ".csv")), by = outxls]

unq <- unique(alldata6$outxls)
rows <- alldata55 [, .(cnt = .N), by =.(outxls, suburl1, suburl2)] 

# Find the expected number of pdfs per folder
allperfld <- alldata55 [, .(cnt = .N), by = .(outfld, outxls)]
allperfld <- allperfld [,  outfld2 := paste("file", outxls, sep="")]
allperfld02 <-  dcast( data = allperfld, 
                                formula = outfld ~ outfld2,
                                value.var = c("cnt") 
                      )
write.xlsx (x = allperfld02, 
            file = paste0("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_pdfs\\02_pat_url_planned.xlsx"))


# Use data from the question
for (i in unq) {
  ll = i
  write.xlsx (x = alldata6[outxls==i], 
              file = paste0("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_pdfs\\02_pat_url_by_", ll, ".xlsx"))
}

# Do a comparison of what is planned and what is downloaded.

outxls3 <- alldata55[outxls==3, c("outfile", "outxls")]
fwrite(outxls3 [, c("outfile")] , file ="../pat_pdfs/outxls3plan.txt")

outxls6 <- alldata55[outxls==6, c("outfile", "outxls")]
fwrite(outxls6 [, c("outfile")] , file ="../pat_pdfs/outxls6plan.txt")


# Use 03pdf_diff.txt file to find data not downloaded yet
pend03 <- fread("../pat_pdfs/03pdf_diff.txt", sep=",", header=FALSE)
pend03 <- pend03 [, outfile := paste( stri_trim( str_replace_all(V1, "[\t\n]" , "")), ".pdf", sep="")]

outxls33 <- alldata55[outxls==3, c("outfile", "outxls", "out1", "out2", "out3")] 
outxls33 <- outxls33 [key ="outfile"]
pend03 <- pend03 [key ="outfile"]

compare3 <- merge (x = outxls33, 
                  y = pend03, 
                  by ="outfile",
                  all.y= TRUE)
write.xlsx (x = compare3, 
            file = paste0("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_pdfs\\02_pat_url_by_03pending.xlsx"))


# Use 03pdf_diff.txt file to find data not downloaded yet
pend06 <- fread("../pat_pdfs/06pdf03.txt", sep=",", header=FALSE)
pend06 <- pend06 [, outfile := paste( stri_trim( str_replace_all(V1, "[\t\n<]" , "")), ".pdf", sep="")]

outxls66 <- alldata55[outxls==6, c("outfile", "outxls", "out1", "out2", "out3")] 
outxls33 <- outxls33 [key ="outfile"]
pend06 <- pend06 [key ="outfile"]

compare6 <- merge (x = outxls66, 
                   y = pend06, 
                   by ="outfile",
                   all.y= TRUE)
write.xlsx (x = compare6, 
            file = paste0("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_pdfs\\02_pat_url_by_06pending.xlsx"))

# Again download files as size 0
size0 <- fread("../pat_pdfs/size01.txt", sep=",", header=FALSE)
size0 <- size0 [, outfile := paste( stri_trim( str_replace_all(V1, "[\t\n<]" , "")), ".pdf", sep="")]

outxls00 <- alldata55[, c("outfile", "outxls", "out1", "out2", "out3")] 
outxls00 <- outxls00 [key ="outfile"]
size0 <- size0 [key ="outfile"]

compare0 <- merge (x = outxls00, 
                   y = size0, 
                   by ="outfile",
                   all.y= TRUE)
write.xlsx (x = compare0, 
            file = paste0("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_pdfs\\02_pat_url_by_0size.xlsx"))


################################################################################################


#2nd attempt successful:

vinay@viany /cygdrive/c/users/Lucky/Documents/Hospital_data/04_2017_DOWNLOAD/emrurl
$ find . -name "MR*.txt" -print|xargs awk '/displayUrl/ || /"date"/ {print FILENAME, $0}'|sed 's/\.txt//g'|tr -s '":' "  " > allpat_date_url.txt

#awk '{$1 = substr($1, 21, 30)} 1'

library(data.table)
library(stringi)
library(stringr)
library(openxlsx)
library(anytime)
library(zoo)


setwd ("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\emrurl\\")

vinay <- fread("allpat_date_url.txt", sep=",", header=FALSE)

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

alldata2 <- alldata2 [, nn := paste("https://182.71.223.195/instahms", displayUrl, sep="")]
# Create another group variable for patient and visit 
alldata2 <- alldata2 [, patgrp02 := 1:.N, by =.(Source)]

#alldata4 <- subset (alldata3 , !(nn %like% c('vitalForm', 'DiagnosticModule' ) ))
alldata5 <- data.table( alldata2[, c("suburl1", "suburl2", "suburl3", "suburl4") := tstrsplit(displayUrl, "=", fixed=TRUE), ])
alldata5 <- alldata5 [, outxls := .GRP, by =.(suburl1, suburl2)]
alldata5 <- alldata5 [, `:=`(outfld = substr(Source, 3, 19),
                             outfile = paste(stri_trim(substr(Source, 21, 29)), "_", patgrp02, "_", date, "_", outxls, ".pdf", sep="") ), ]

alldata6 <- alldata5 [, c("Source", "nn", "outfld", "outfile", "outxls", "date", "patgrp02")]

# Try the following step with write.xlsx
# Currently did the same thing using for loop
#alldata5 [, write.csv(c(.BY,.SD), paste0("02_pat_url_by_", .BY, ".csv")), by = outxls]

unq <- unique(alldata6$outxls)

# Use data from the question
for (i in unq) {
  ll = i
  write.xlsx (x = alldata6[outxls==i], 
              file = paste0("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\pat_pdfs\\02_pat_url_by_", ll, ".xlsx"))
}

rows <- alldata5 [, .(cnt = .N), by =.(outxls, suburl1, suburl2)] 


C:\Users\Lucky\Documents\Hospital_data\04_2017_DOWNLOAD\pat_pdfs\MR006001_MR007000\MR006078_6_1325615400000_11.pdf#========================================================================
vinay@viany /cygdrive/c/Users/Lucky/Documents/Hospital_data/04_2017_DOWNLOAD/emrurl/
  $ find . -name "*.txt" -print |gawk '{print FILENAME, "=", $0}'|grep -vE '\"printerId|description|viewDocs|accessRights|anotation|authorized|class|contentType|doctor|externalLink|icon|pdfSupported|provider|updatedBy|updatedDate|userName'|grep ":"|tr '}{][;"' "      "|tr -s " " " "|grep -v " = \+[[:space:]]" >all_pats.txt

awk '/displayUrl/ {print FILENAME, NR, $0}' *.txt|tr -s '":' "  "|awk '{print $1, $2, $4 }'|grep -vE "vitalForm|DiagnosticModule"|cat -n

library(data.table)
library(stringi)
library(stringr)
library(openxlsx)

setwd ("C:\\Users\\Lucky\\Documents\\Hospital_data\\04_2017_DOWNLOAD\\emrurl\\")

vinay <- fread("all_pats.txt", sep=",", header=FALSE)


alldata <- data.table( vinay[, c("SRC1", "SRC2", "tmp") := tstrsplit(V1, ":", fixed=TRUE), ])
alldata <- data.table( alldata[, c("Source", "VAR") := tstrsplit(SRC1, "=", fixed=TRUE), ])

alldata <- alldata [, `:=` (SRC1 =stri_trim(VAR), 
                            SRC2 =stri_trim(SRC2)) ]

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

alldata2 <- alldata2 [, nn := paste("https://182.71.223.195/instahms", displayUrl, sep="")]

alldata3 <- alldata2 [, c("Source", "patgrp", "date", "displayUrl", "docid",  
                          "type", "title", "visitDate", "visitid", "nn"), with =FALSE]
setkey(alldata3, date)

# Create another group variable for patient and visit 
alldata4 <- alldata3 [, patgrp02 := 1:.N, by =.(Source)]
alldata4 <- alldata4 [, outfile := paste(stri_trim(Source), "_", patgrp02, "_", docid, "_", type, ".pdf", sep=""), ]
alldata4 <- data.table( alldata4[, c("Source0", "Source1") := tstrsplit(Source, "/", fixed=TRUE), ])

alldata4 <- alldata4 [order(Source, Source1, patgrp02)]

write.xlsx(alldata4, 
           file = "C:\\Users\\Lucky\\Documents\\Hospital_data\\03_2017_DOWNLOAD\\pgrm\\02pat_visit_url.xlsx", 
           colNames = TRUE, 
           borders = "columns")





#========================================================================