
library(dplyr)
library(data.table)
library(fuzzyjoin)
library(stringr)
library(stringi)
library(stringdist)
library(tm)

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

try <- amatch(x = row_const,
              table = icd_const,
              method = "jw",
              maxDist = .3)

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



try_osa <- stringdist_inner_join(x = row_const,
                             y = icd_const,
                             by = "VAR55",
                             ignore_case = TRUE,
                             method = c("osa") ,
                             max_dist=10,
                             distance_col = "dist")


try_qgram <- stringdist_inner_join(x = row_const,
                             y = icd_const,
                             by = "VAR55",
                             ignore_case = TRUE,
                             method = c("qgram") ,
                             max_dist=20,
                             distance_col = "dist")


try_cosine <- stringdist_inner_join(x = row_const,
                                   y = icd_const,
                                   by = "VAR55",
                                   ignore_case = TRUE,
                                   method = c("cosine") ,
                                   max_dist=.3,
                                   distance_col = "dist") 
try1 <- try_cosine[, c("seq_len", "V1.x", "VAR3", "VAR4", "VAR55.x", "VAR55.y","dist" )]
try1 <- try1 [order(seq_len, V1.x, VAR55.x, dist ) ]
try2 <- try1 [, disgrp := 1:.N, by =.(seq_len, V1.x, VAR55.x)] 

try20 <- try2 [ disgrp <= 15]

try3 <- dcast (data=try20,
               seq_len + V1.x +VAR55.x ~ paste( "dis", str_pad(disgrp, 3, side = "left", pad = 0), sep=""),
               fill=" ",
               value.var = c("VAR55.y"))



# Read the source data

row_all <- fread( "grep @@D10@ D:\\Hospital_data\\04_2017_DOWNLOAD\\pat_txts_mod\\row_all02.txt", 
                  sep="\t", 
                  header= FALSE)

row_all0 <- row_all[, c("VAR1","VAR2", "VAR3", "VAR4", "VAR5", "VAR6") := tstrsplit(stri_trim(V1), "@", fixed=TRUE), ]

row_all01 <- row_all0 [ VAR4 %in% c('ASSOCIATED SIGNS SYMPTOMS', 'ADDITIONAL COMPLAINT', 'ALLERGIC_IMMUNOLOGIC', 'ASSOCIATED SYMPTOMS', 'HISTORY OF PAST ILLNESS', 'OTHER COMPLAINT', 'HEMOTOLOGICAL_LYMPHATIC',  'MAIN COMPLAINTS', 'PAST HISTORY', 'ASSOCIATED_ILLNESS_UPASHAYA_ANUPASHAYA', 'PSYCHOLOGICAL OCCUPATIONAL HISTORY', 'HISTORY OF PRESENT ILLNESS', 'CHIEF COMPLAINT DURATION', 'FAMILY HISTORY', 'SURGICAL HISTORY', 'DIAGNOSTIC HISTORY', 'ALLOPATHIC DIAGNOSIS', 'ASSOCIATED COMPLAINT WITH ONSET DURATION', 'CHIEF COMPLAINT', 'MEDICAL HISTORY', 'CHIEF COMPLAINT WITH ONSET DURATION', 'COMPLAINT' )]

row_all20 <- data.table ( var = unique( row_all01$VAR5) )
row_all21 <- row_all20 [ , row := 1:.N] 
row_all22 <- row_all21 [ row <= 10000]

# Make a corpus from the column containing the document text:

source <- VectorSource(row_all22$var)
corpus <- Corpus(source)

# The standard steps to clean and prepare the data:

#corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

# Lets first create a document-term matrix:
mat <- DocumentTermMatrix(corpus)

# create a weighted T-Ida version of the matrix
mat4 <- weightTfIdf(mat)
mat4 <- as.matrix(mat4)


m<-t(mat4)
rownames(m) <- 1:nrow(m)

norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
num_cluster<-6
cl <- kmeans(m_norm, num_cluster)
round(cl$centers, digits = 1)
for (i in 1:num_cluster) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(cl$centers[i, ], decreasing = T)
  cat(names(s)[1:5], "\n")
}

ndocs <- length(corpus)
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- ndocs * 0.01
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * .5
dtm = DocumentTermMatrix(corpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(2, 15),
                           removePunctuation = T,
                           removeNumbers = T,
                           #stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))
                         ))
#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
#print(as.matrix(dtm))

inspect(dtm)
print(dtm)

write.csv((as.matrix(dtm)), "test.csv")
#head(sort(as.matrix(dtm)[18,], decreasing = TRUE), n=15)
dtm.matrix = as.matrix(dtm)
wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 20)

library(wordcloud)

library(quanteda)

summary(corpus)

toks <- tokens(corpus)
