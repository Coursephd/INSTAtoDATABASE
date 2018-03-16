
#library(dplyr)
library(data.table)
library(fuzzyjoin)
library(stringr)
library(stringi)

icd10 <- fread("D:\\Hospital_data\\04_2017_DOWNLOAD\\pat_dbs\\icd10cm_codes_2018.txt", sep="\t", header= FALSE)
icd10_1 <- icd10[, c("VAR1","VAR5") := tstrsplit(stri_replace_first_fixed(V1, " ", "<>"), "<>", fixed=TRUE), ]

icd10_1 <- icd10_1 [, VAR55 :=toupper(V1)]
icd_const <- icd10_1[ VAR55 %like% c("CONSTIPATION") | VAR55 %like% c("COLD") ]

row_all <- fread( "grep @@D10@ D:\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_mod\\row_all02.txt", 
                  sep="\t", 
                  header= FALSE)

row_all0 <- row_all[, c("VAR1","VAR2", "VAR3", "VAR4", "VAR5", "VAR6") := tstrsplit(stri_trim(V1), "@", fixed=TRUE), ]
row_all02 <- row_all0[, .(V1, VAR1, VAR2, VAR3, VAR4, VAR5, VAR6,  
                          splitted =unlist(strsplit(VAR5, "[[:punct:]]", perl=TRUE))) ,
                          by=seq_len(nrow(row_all0))]

row_uniq <- data.table ( VAR55 = unique( row_all02$splitted) ) [order(VAR55)]
row_const <- row_uniq[ VAR55 %like% c("CONSTIPATION") | VAR55 %like% c("COLD")]

try <- stringdist_inner_join(x = row_const,
                             y = icd_const,
                             by = "VAR55",
                             ignore_case = TRUE,
                             method = c("jw") ,
                             max_dist=.4,
                             distance_col = "dist")

try1 <- try [order(VAR55.x, dist ) ]
try2 <- try1 [, disgrp := 1:.N, by =.(VAR55.x)] 

try3 <- dcast (data=try2,
               VAR55.x ~ paste( "dis", str_pad(disgrp, 3, side = "left", pad = 0), sep=""),
               fill=" ",
               value.var = c("V1"))
