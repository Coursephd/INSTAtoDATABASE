
library(dplyr)
library(data.table)
library(fuzzyjoin)
library(stringr)
library(stringi)

icd10 <- fread("D:\\Hospital_data\\04_2017_DOWNLOAD\\pat_dbs\\icd10cm_codes_2018.txt", sep="\t", header= FALSE)
icd10_1 <- icd10[, c("VAR1","VAR5") := tstrsplit(stri_replace_first_fixed(V1, " ", "<>"), "<>", fixed=TRUE), ]

icd10_1 <- icd10_1 [, VAR5 :=toupper(V1)]

row_all <- fread( "grep @@D10@ D:\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_mod\\row_all02.txt", 
                  sep="\t", 
                  header= FALSE)


icd_const <- icd10_1[ VAR5 %like% c("CONSTIPATION") | VAR5 %like% c("BRONCHI") ]
row_const <- row_all[ V1 %like% c("CONSTIPATION") ]
row_const <- row_const[, v2 := V1, ]

row_const <- row_const[, c("VAR1","VAR2", "VAR3", "VAR4", "VAR5") := tstrsplit(stri_trim(V1), "@", fixed=TRUE), ]
row_const2 <- row_const[, VAR5 := unlist(strsplit(VAR5, ","))]

try <- stringdist_inner_join(x = row_const2,
                             y = icd_const,
                             by = "VAR5",
                             ignore_case = TRUE,
                             method = c("jw") , #c("qgram"),
                             max_dist=.4,
                             distance_col = "dist")

try2 <- try [, disgrp := 1:.N, by =.(V1.x)] 

try3 <- dcast (data=try2,
               V1.x + VAR4 + VAR5.x ~ paste("dis", disgrp, sep=""),
               fill=" ",
               value.var = c("VAR5.y"))


icd10_1 <- fuzzy_left_join(x = row_all,
                           y = icd10_1,
                           by = "V1",
                           match_fun = FALSE)