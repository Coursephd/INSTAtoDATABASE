
library(dplyr)
library(data.table)
library(fuzzyjoin)
library(stringr)
library(stringi)

icd10 <- fread("D:\\Hospital_data\\04_2017_DOWNLOAD\\pat_dbs\\icd10cm_codes_2018.txt", sep="\t", header= FALSE)
icd10_1 <- icd10[, c("VAR1","VAR5") := tstrsplit(stri_replace_first_fixed(V1, " ", "<>"), "<>", fixed=TRUE), ]

icd10_1 <- icd10_1 [, VAR55 :=toupper(V1)]

icd_const <- icd10_1[ VAR55 %like% c("WEIGHT") | VAR55 %like% c("COLD") ]

row_all <- fread( "grep @@D10@ D:\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_mod\\row_all02.txt", 
                  sep="\t", 
                  header= FALSE)

row_all0 <- row_all[, c("VAR1","VAR2", "VAR3", "VAR4", "VAR5", "VAR6") := tstrsplit(stri_trim(V1), "@", fixed=TRUE), ]

row_all01 <- row_all0 [ VAR4 %in% c('ASSOCIATED SIGNS SYMPTOMS', 'ADDITIONAL COMPLAINT', 'ALLERGIC_IMMUNOLOGIC', 'ASSOCIATED SYMPTOMS', 'HISTORY OF PAST ILLNESS', 'OTHER COMPLAINT', 'HEMOTOLOGICAL_LYMPHATIC',  'MAIN COMPLAINTS', 'PAST HISTORY', 'ASSOCIATED_ILLNESS_UPASHAYA_ANUPASHAYA', 'PSYCHOLOGICAL OCCUPATIONAL HISTORY', 'HISTORY OF PRESENT ILLNESS', 'CHIEF COMPLAINT DURATION', 'FAMILY HISTORY', 'SURGICAL HISTORY', 'DIAGNOSTIC HISTORY', 'ALLOPATHIC DIAGNOSIS', 'ASSOCIATED COMPLAINT WITH ONSET DURATION', 'CHIEF COMPLAINT', 'MEDICAL HISTORY', 'CHIEF COMPLAINT WITH ONSET DURATION', 'COMPLAINT' )]

row_all02 <- row_all01[, .(V1, VAR1, VAR2, VAR3, VAR4, VAR5, VAR6,  
                           splitted =unlist(strsplit(VAR5, "[[:punct:]]", perl=TRUE))) ,
                       by=seq_len(nrow(row_all01))]


med_rm <- fread("D:\\Hospital_data\\04_2017_DOWNLOAD\\prgm\\medical_remove.txt")
med_rm <- med_rm [ ,  V30 := paste("\\b", str_trim(word), collapse="\\b|", sep="")]
med_rm2 <- unique(med_rm$V30)

row_all03 <- row_all02 [, VAR500 := gsub(med_rm2, "<>", splitted),]
row_all04 <- row_all03[, .(V1, VAR1, VAR2, VAR3, VAR4, VAR5, VAR6,  
                           VAR55 =unlist(strsplit(VAR500, "<>", perl=TRUE))) ,
                       by=seq_len(nrow(row_all03))]
row_all04 <- row_all04 [VAR55 != " "]

row_const <- row_all04[ VAR55 %like% c("WEIGHT") | VAR55 %like% c("COLD")]

try <- stringdist_inner_join(x = row_const,
                             y = icd_const,
                             by = "VAR55",
                             ignore_case = TRUE,
                             method = c("jw") ,
                             max_dist=.4,
                             distance_col = "dist")

try1 <- try [order(VAR55.x, dist ) ]
try2 <- try1 [, disgrp := 1:.N, by =.(VAR55.x)] 

try20 <- try2 [ disgrp <= 15]

try3 <- dcast (data=try20,
               VAR4 + VAR55.x ~ paste( "dis", str_pad(disgrp, 3, side = "left", pad = 0), sep=""),
               fill=" ",
               value.var = c("VAR55.y"))
