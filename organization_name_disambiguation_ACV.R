# Algorithm for organization name disambiguation developed in R
#
# A hybrid methodology (largely automated + manual check) to identify equivalences between labels (i.e., the ways in which organization names are reported in the raw dataset)
#
# Application to the first three European Framework Programmes (data downloaded from https://cordis.europa.eu/ on October 1st, 2021)
#
# Copyright (C) 2022 by Dr. Andrea Ancona (andrea.ancona@uniroma1.it), Prof. Roy Cerqueti (roy.cerqueti@uniroma1.it), Prof. Gianluca Vagnani (gianluca.vagnani@uniroma1.it)
#
# An accurate description of the methodology is included in a scientific paper that has been accepted for publication in "Scientometrics", 
# titled "A novel methodology to disambiguate organization names: an application to EU Framework Programmes data" (2023).

# Load necessary packages
library(readxl)
library(stringi)
library(stringr)
library(tm)
library(dplyr)
library(stringdist)
library(writexl)
library(qdapRegex)

# Download the file "workspace_cordis" and put it in a desired folder

# Set the working directory to the early defined folder
setwd("")

# Load "workspace_cordis" 
load("workspace_cordis.RData") # datasets related to FP1, FP2, and FP3

# Isolate the organization names (the labels) with the related Country 
# In this case, the only variable widely reported 
fp1_org <- fp1[,c(6,11)]
fp2_org <- fp2[,c(6,11)]
fp3_org <- fp3[,c(6,11)]

# Create a unique data frame with all first three FPs
allfps_org <- rbind(fp1_org, fp2_org, fp3_org)

# Remove additional white spaces from labels
allfps_org$name <- str_trim(allfps_org$name, "both") 
allfps_org$name <- str_squish(allfps_org$name)

# Determine distinct couples label-country
allfps_org <- unique(allfps_org)

# Add a column for the "Code": this will be the final output of the methodology (distinct organization names after disambiguation will have distinct codes)
allfps_org <- allfps_org %>% mutate(Code = 0)


### Data pre-treatment by comparing organization names with ROR

# The ror database is already loaded in the workspace
ror_org <- vector("character", length(ror))

for(i in 1:length(ror_org)){
  cat(i, "\t")
  ror_org[i] <- ror[[i]]$name
}

# Determine the number of organization names included in the original dataset that are in the ror database (lower case letter names)
length(which(unique(tolower(allfps_org$name))%in%tolower(ror_org))) 

# Create a dataframe with associated pairs based on org-alias links in the ror database
ror_corr <- data.frame(name = NA, alias = NA)

for(i in 1:length(ror)){
  cat(i, "\t")
  if(length(ror[[i]]$aliases)>0){
    for(j in 1:length(ror[[i]]$aliases)){
      ror_corr[i-1+j,1] = ror[[i]]$name
      ror_corr[i-1+j,2] = ror[[i]]$aliases[j]
    }
  } 
}

# Remove NA columns and duplicates
ror_corr <- ror_corr[-which(is.na(ror_corr[,1])==T),]
ror_corr <- ror_corr[-which(duplicated(ror_corr)==T),]
row.names(ror_corr) <- 1:nrow(ror_corr)

# Express them in lower case letters to facilitate the following comparison
ror_corr[,1] <- tolower(ror_corr[,1])
ror_corr[,2] <- tolower(ror_corr[,2])

# Identify pairs between those labels that are in the cordis dataset 
# Define the vector of distinct organization names in raw data expressed in lower case letters
allfps_org_lower <- unique(tolower(allfps_org$name))

# Determine which ror organizations (if any) correspond with organization names in raw data (for each column of ror_corr) 
ror_matching_i <- which(ror_corr[,1]%in%allfps_org_lower)
ror_matching_j <- which(ror_corr[,2]%in%allfps_org_lower)

# Determine common positions in ror_matching_i and ror_matching_j, i.e., pairs of organizations in ror that match two organization names in raw data
final_matching_ror <- ror_matching_j[which(ror_matching_j%in%ror_matching_i)] 

# Identify matched org from ror_corr
matched_org_ror <- ror_corr[final_matching_ror,]

# Add a column to matched_org_ror where it is reported if True or False based on the assessment of ambiguous matchings
matched_org_ror <- matched_org_ror %>% mutate(nature = "TRUE")

# In our case, we identify two ambiguous matchings:

# 1.
which(tolower(allfps_org$name)=="ministry of health") #14188 (from Kenya), and 17052 (from Greece)
which(tolower(allfps_org$name)=="ministère de la santé") #6160 (from Luxembourg), and 8767 (from Morocco)
# This matching will be marked as FALSE
matched_org_ror$nature[which(matched_org_ror$name=="ministry of health")] <- "FALSE"

# 2.
which(tolower(allfps_org$name)=="royal institute of technology") #3150 (from Sweden), 3419 (from Sweden), and 8135 (N/A) 
which(tolower(allfps_org$name)=="kungliga tekniska högskolan") #6266 (from Sweden)
# This matching will be kept as TRUE

# Maintain just the first two columns of "TRUE" matchings 
matched_org_ror <- matched_org_ror[which(matched_org_ror$nature=="TRUE"),1:2]

# I will add the equivalences identified through the comparison with ror at the end of the procedure, after the manual part 


### Pre-processing of labels

# Initialize a new dataframe with the new labels; this file will be the input source to identify equivalences 
allfps_org_names <- data.frame(old_name = allfps_org$name, country= allfps_org$country,
                               new_name = NA)


### Start Acronym substitution

# Look for acronym, store them and use for string substitution 
# restrict to those included in () or - XXX or XXX -
acrlist <- c()
fullname <- c()
country <- c()

for(i in 1:nrow(allfps_org_names)){
  start <- 0
  end <- 0
  check <- 0
  #case of a string more than 9 and the acronym is in ()
  if(nchar(allfps_org_names$old_name[i])>=9){
    start <- regexpr("\\(",allfps_org_names$old_name[i])
    end <- regexpr("\\)",allfps_org_names$old_name[i])
  }
  #made correction in including cases of () at the beginning or at the end
  if(start>0 & end>0 & str_detect(str_replace_all(substring(allfps_org_names$old_name[i],start[1]+1,end[1]-1), " ", ""), "^[:upper:]+$")==TRUE && nchar(str_replace_all(substring(allfps_org_names$old_name[i],start[1]+1,end[1]-1), " ", ""))>=2 && nchar(str_replace_all(substring(allfps_org_names$old_name[i],start[1]+1,end[1]-1), " ", ""))<=5){
    acrlist <- rbind(acrlist, str_replace_all(substring(allfps_org_names$old_name[i],start[1]+1,end[1]-1), " ", ""))
    fullname <- rbind(fullname, rm_between(allfps_org_names$old_name[i], "(", ")"))
    country <- rbind(country, allfps_org_names$old_name[i])
    check <- 1
  }
  #case of a string more than 9 and the acronym is in - AAA 
  if(nchar(allfps_org_names$old_name[i])>=9 && check==0){
    start <- regexpr("\\-",allfps_org_names$old_name[i])
  }
  if(start>0 && (start[1]/nchar(allfps_org_names$old_name[i]))>0.5 & str_detect(str_replace_all(substring(allfps_org_names$old_name[i],start[1]+1,nchar(allfps_org_names$old_name[i])), " ", ""), "^[:upper:]+$")==TRUE && nchar(str_replace_all(substring(allfps_org_names$old_name[i],start[1]+1,nchar(allfps_org_names$old_name[i])), " ", ""))>=2 && nchar(str_replace_all(substring(allfps_org_names$old_name[i],start[1]+1,nchar(allfps_org_names$old_name[i])), " ", ""))<=5){
    acrlist <- rbind(acrlist, str_replace_all(substring(allfps_org_names$old_name[i],start[1]+1,nchar(allfps_org_names$old_name[i])), " ", ""))
    fullname <- rbind(fullname, substring(allfps_org_names$old_name[i],1, start[1]-1))
    country <- rbind(country, org_list[i,2])
    check <- 1
  } 
  #case of a string size is more than 9 and the acronym is in AAA- 
  if(nchar(allfps_org_names$old_name[i])>=9 && check==0){
    start <- regexpr("\\-",allfps_org_names$old_name[i])
  }
  if(start>0 && (start[1]/nchar(allfps_org_names$old_name[i]))<0.5 & str_detect(str_replace_all(substring(allfps_org_names$old_name[i],1,start[1]-1), " ", ""), "^[:upper:]+$")==TRUE && nchar(str_replace_all(substring(allfps_org_names$old_name[i], 1, start[1]-1), " ", ""))>=2 && nchar(str_replace_all(substring(allfps_org_names$old_name[i], 1, start[1]-1), " ", ""))<=5){
    acrlist <- rbind(acrlist, str_replace_all(substring(allfps_org_names$old_name[i],1, start[1]-1), " ", ""))
    fullname <- rbind(fullname, substring(allfps_org_names$old_name[i],start[1], nchar(allfps_org_names$old_name[i])))
    country <- rbind(country, org_list[i,2])
  }
}

s <- cbind(acrlist, fullname, country)
s <- data.frame(s)

# Enrich acronyms found with number of occurrences and expanded cleaned names
for(z in 1:nrow(s)){
  ap_str<-c()
  ac_str<-c()
  s$X4[z]<-nchar(s$X2[z])/nchar(s$X1[z])
  ac_str<-paste0(s$X1[z],"\\b -|", s$X1[z], "\\b-|\\-", s$X1[z], "|\\- ", s$X1[z], "|\\(", s$X1[z], "\\)", "|\\b", s$X1[z], "\\b")
  s$X5[z]<-length(grep(ac_str, allfps_org_names$old_name))
  ap_str<-removeWords(tolower(s$X2[z]), 
                      c("and", "of", "de", "l'", "et", "la", "van", "del",
                        "&", "un'", "per", "des", "le", "pour", "en", "fuer", "fur",
                        "an", "for", "d'", "du", "di", "in", "ed", "zu", "del",
                        "delle", "della", "do", "und", "at", "the", "zur", "der",
                        "da", "sur", "les", "voor", "dell'", "degli", "dei",
                        "dagli", "lo", "las", "van", "at", "von", "or", "?", "e"))
  ap_str<-gsub("[][!#$%()*,-.:;&<=>@^_`|~.{}]", "", ap_str)
  s$X6[z]<-ap_str
  ap_str<-stri_trans_general(ap_str, "Latin-ASCII")
  ap_str<-strsplit(ap_str, " ")
  s$X7[z]<-sapply(ap_str, function(x){
    toupper(paste(substring(x, 1, 1), collapse = ""))
  })
}

# Eliminate possible country abbreviation identified as acronyms
clist<-unique(allfps_org_names$country)

for (z in 1:length(clist)){
  rnum<-c()
  rnum<-which(s$X1==clist[z])
  if (length(rnum)>0){
    s <- s[-c(rnum), ]
  }
}

rownames(s) <- t(t(seq(1, nrow(s), by=1)))

# Eliminate residual empty rows
s<-s[s$X2!="", ]
rownames(s) <- t(t(seq(1, nrow(s), by=1)))

# Calculate the string similarity between the potential acronym and the defined short notation, and other measures
s$X8<-stringsim(s$X1, s$X7, method="cosine")
s$X9<-0
s$X10<-0
s$X11<-0
s$X12<-0
s$X13<-0

for(z in 1:nrow(s)){
  string0<-c()   
  string1 <- c()
  string2 <-c()
  
  string0<-unlist(strsplit(s$X6[z], " "))
  #number of characters per sub-string in the full name 
  s$X12[z]<-sum(nchar(string0))/length(string0)
  #number of characters per sub-string in the full name that are >9
  s$X13[z]<-length(which((nchar(string0)>9)==TRUE))
  string1<- strsplit(as.character(s$X1[z]), "")
  string1 = unlist(string1)
  string2<- strsplit(as.character(s$X7[z]), "")
  string2 = unlist(string2)
  #check if acronym and full name have same initials
  if(string1[1]==string2[1]){
    s$X9[z]<-1
  }
  #check if first two acronyms chars are similar to first full name initials
  if(length(string1)>=2 & length(string2)>=2 & string1[1]==string2[1] & string1[2]==string2[2]){
    s$X10[z]<-1
  }
  #check if second and third acronyms chars are similar to first full name initials 
  if(length(string1)>=3 & length(string2)>2 & string1[2]==string2[1] & string1[3]==string2[2]){
    s$X11[z]<-1
  }
}

s$X14<-paste0(s$X1, s$X2, s$X3)  

# Eliminate duplicates
s <- s[!duplicated(s[,c('X14')]),]
rownames(s) <- t(t(seq(1, nrow(s), by=1)))
s$X15<-0

# Save the acronym data 

# Col legend: "Found ACN", "Name", "Country", "ACN#", "Name clean", "ACN est", "ACN-ACN est dist", "First Match", "Second Match", "Third Match", "Average length", "Num Sub gt 9", "Duplicate check", "Acronym confirmation"
colnames(s)<-c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15")

write_xlsx(s, path = "Output_ACR.xlsx")

### Checking acronym by hand and in Col "X15" set values to 1 if confirmed 0 not confirmed

# Reload the file with the acronym checked by hand
s2 <- read_excel("Output_ACR.xlsx")

# Extract only selected acronym in Col "X15" marked with 1
s2<-s2[s2$X15==1, ]

# Eliminate  duplicates, selecting the acronym with the highest stringsim
s2s<- s2 %>% 
  group_by(X1) %>% 
  slice(which.max(X8))      

# Change in each occurrence the acronym with its full name and country matches
allfps_org_names$old_name_d<-0   #indicator of Acronym change 
allfps_org_names$old_name_R<-NA  #revised old_name list

for(z in 1:nrow(s2s)){
  cat(z, "\t")
  searchSt<-c()
  listMatch<-c()
  listMatchC<-c()
  #exact match between the acronym and the disambiguation list
  searchSt<-paste0("\\b",s2s$X1[z],"\\b")   #(?i)(?<=^|[^a-z])cat(?=$|[^a-z])
  listMatch<-grepl(searchSt, allfps_org_names$old_name)
  #exact match between countries of the acronym and the disambiguation list
  listMatchC<-grepl(s2s$X3[z], allfps_org_names$country)
  #assign the standardized label
  allfps_org_names$old_name_R[which(listMatch=="TRUE" & listMatchC=="TRUE")]<-s2s$X2[z]
  allfps_org_names$old_name_d[which(listMatch=="TRUE" & listMatchC=="TRUE")]<-1
}

# Complete the full string list
allfps_org_names$old_name_R[which(allfps_org_names$old_name_d==0)]<-allfps_org_names$old_name[which(allfps_org_names$old_name_d==0)]

### End acronym replacement

### Continue with the pre-processing of labels

# Express labels in lower case letters, and remove accents and special characters
allfps_org_names$new_name <- tolower(allfps_org_names$old_name_R) 
allfps_org_names$new_name <- stri_trans_general(allfps_org_names$new_name, "Latin-ASCII") 

# Remove a list of blocking words (i.e., conjunctions, articles, prepositions, etc.) that appear frequently in the dataset; 
# It is suggested not to remove single letters, since they could create problems in case of acronyms (e.g., i.n.i.a)
allfps_org_names$new_name <- removeWords(allfps_org_names$new_name, 
                                         c("and", "of", "de", "l'", "et", "la", "van", "del",
                                           "&", "un'", "per", "des", "le", "pour", "en", "fuer", "fur",
                                           "an", "for", "d'", "du", "di", "in", "ed", "zu", "del",
                                           "delle", "della", "do", "und", "at", "the", "zur", "der",
                                           "da", "sur", "les", "voor", "dell'", "degli", "dei",
                                           "dagli", "lo", "las", "van", "at", "von", "or", "dans", "ed"))

# Convert frequent words with the same meaning into a unique equivalent keyword in order to make the algorithm neutral to the different ways of referring to:

# 1. Institutes
allfps_org_names$new_name <- str_replace_all(allfps_org_names$new_name, 
                                             c("instituto" = "11ins11", 
                                               "institute" = "11ins11", "instituut" = "11ins11",
                                               "istituto" = "11ins11", 
                                               "institut" = "11ins11", "inst" = "11ins11")
)

# 2. National
allfps_org_names$new_name <- str_replace_all(allfps_org_names$new_name, 
                                             c("nacional" = "22nat22", "nationale" = "22nat22",
                                               "nazionale" = "22nat22", "national" = "22nat22")
)

# 3. Technical (often translated as "... of technology" from other languages into english)
allfps_org_names$new_name <- str_replace_all(allfps_org_names$new_name, 
                                             c("technology" = "33tech33", "technische" = "33tech33",
                                               "technologies" = "33tech33", "technologie" = "33tech33",
                                               "tecnica" = "33tech33", "technics" = "33tech33",
                                               "techniques" = "33tech33", "technique" = "33tech33",
                                               "tecnologicas" = "33tech33", "technical" = "33tech33",
                                               "teknologisk" = "33tech33", "tekniske" = "33tech33",
                                               "technisch" = "33tech33", "tecnico" = "33tech33", 
                                               "techn" = "33tech33")
)

# 4. Research
allfps_org_names$new_name <- str_replace_all(allfps_org_names$new_name, 
                                             c("research" = "44res44", "recherches"  = "44res44",
                                               "ricerche" = "44res44", "recherche" = "44res44",
                                               "forschung" = "44res44", "onderzoek" = "44res44",
                                               "ricerca" = "44res44")
)

# 5. Council
allfps_org_names$new_name <- str_replace_all(allfps_org_names$new_name, 
                                             c("council" = "55counc55", "consejo" = "55counc55",
                                               "consiglio" = "55counc55")
)

# 6. Centre
allfps_org_names$new_name <- str_replace_all(allfps_org_names$new_name, 
                                             c("centre" = "66cen66", "center" = "66cen66",
                                               "centro" = "66cen66", "centrum" = "66cen66",
                                               "instelling" = "66cen66", "cent" = "66cen66",
                                               "zentrum" = "66cen66")
)

# 7. Scientific
allfps_org_names$new_name <- str_replace_all(allfps_org_names$new_name, 
                                             c("scientific" = "77sci77", "scientist" = "77sci77",
                                               "scientifiques" = "77sci77", "scientifique" = "77sci77")
)

# 8. Universities
allfps_org_names$new_name <- str_replace_all(allfps_org_names$new_name, 
                                             c("uniuniversidad" = "00uni00", "university" = "00uni00", 
                                               "rijksuniversiteit" = "00uni00", "universitet" = "00uni00",
                                               "universiteit" = "00uni00","universitat" = "00uni00",
                                               "universitaet" = "00uni00", "universidade" = "00uni00",
                                               "universities" = "00uni00", "universidado" = "00uni00",
                                               "universidad" = "00uni00", "universitade" = "00uni00", "universitad" = "00uni00",
                                               "universite'" = "00uni00", "universita'" = "00uni00",
                                               "universite" = "00uni00", "universita" = "00uni00",
                                               "hojskole" = "00uni00", "hoejskole" = "00uni00", "hochschule" = "00uni00",
                                               "univ" = "00uni00"
                                             )
)

# Universities necessitate a further manipulation
# Remove "studi" from each name including "00uni00 studi", "state" from each name including "state 00uni00", "etat" from each name including "00uni00 etat", "libre" from each name including "00uni00 libre", "freie" from each name including "freie 00uni00"
common_pattern_uni <- vector("numeric", length = nrow(allfps_org_names))

# "studi" from "00uni00 studi"
for(i in 1:length(common_pattern_uni)){
  cat(i, "\t")
  common_pattern_uni[i] = length(Reduce(`intersect`,stri_extract_all_regex(c(allfps_org_names$new_name[i], "00uni00 studi"),"\\w+")))
}

for(i in 1:length(common_pattern_uni)){
  cat(i, "\t")
  if(common_pattern_uni[i]==2){
    allfps_org_names$new_name[i] = removeWords(allfps_org_names$new_name[i], "studi")
  }
}

# "state" from "state 00uni00"
for(i in 1:length(common_pattern_uni)){
  cat(i, "\t")
  common_pattern_uni[i] = length(Reduce(`intersect`,stri_extract_all_regex(c(allfps_org_names$new_name[i], "00uni00 studi"),"\\w+")))
}

for(i in 1:length(common_pattern_uni)){
  cat(i, "\t")
  if(common_pattern_uni[i]==2){
    allfps_org_names$new_name[i] = removeWords(allfps_org_names$new_name[i], "studi")
  }
}

# "etat" from "00uni00 etat"
for(i in 1:length(common_pattern_uni)){
  cat(i, "\t")
  common_pattern_uni[i] = length(Reduce(`intersect`,stri_extract_all_regex(c(allfps_org_names$new_name[i], "00uni00 studi"),"\\w+")))
}

for(i in 1:length(common_pattern_uni)){
  cat(i, "\t")
  if(common_pattern_uni[i]==2){
    allfps_org_names$new_name[i] = removeWords(allfps_org_names$new_name[i], "studi")
  }
}

# "libre" from "00uni00 libre"
for(i in 1:length(common_pattern_uni)){
  cat(i, "\t")
  common_pattern_uni[i] = length(Reduce(`intersect`,stri_extract_all_regex(c(allfps_org_names$new_name[i], "00uni00 studi"),"\\w+")))
}

for(i in 1:length(common_pattern_uni)){
  cat(i, "\t")
  if(common_pattern_uni[i]==2){
    allfps_org_names$new_name[i] = removeWords(allfps_org_names$new_name[i], "studi")
  }
}

# "freie" from "freie 00uni00"
for(i in 1:length(common_pattern_uni)){
  cat(i, "\t")
  common_pattern_uni[i] = length(Reduce(`intersect`,stri_extract_all_regex(c(allfps_org_names$new_name[i], "00uni00 studi"),"\\w+")))
}

for(i in 1:length(common_pattern_uni)){
  cat(i, "\t")
  if(common_pattern_uni[i]==2){
    allfps_org_names$new_name[i] = removeWords(allfps_org_names$new_name[i], "studi")
  }
}

# Extract all the words from each pre-processed label
allfps_org_words <- vector("list", nrow(allfps_org))
names(allfps_org_words) <- allfps_org$name
allfps_org_words[1:length(allfps_org_words)] <- stri_extract_all_words(allfps_org_names$new_name)

# Remove words words contained in specific recurring periphrases
blocking_words <- vector("list", 23)

blocking_words[[1]] <- c("chancellor", "masters", "scholars")
blocking_words[[2]] <- c("chancellors", "masters", "scholars")
blocking_words[[3]] <- c("chancellor", "master", "scholars")
blocking_words[[4]] <- c("chancelor", "masters", "scholars")
blocking_words[[5]] <- c("pro", "vice", "chancellor")
blocking_words[[6]] <- c("chancellors")
blocking_words[[7]] <- c("provost", "members", "scholars", "foundation", "other", "members", "board")
blocking_words[[8]] <- c("provost", "fellows", "scholars")
blocking_words[[9]] <- c("provost", "fellows", "scholars", "holy", "undivided", "queen", "elizabeth", "near", "hereinafter")
blocking_words[[10]] <- c("prvost", "fellows", "scholars")
blocking_words[[11]] <- c("president", "fellows", "students")
blocking_words[[12]] <- c("president", "fellows")
blocking_words[[13]] <- c("president", "scholars")
blocking_words[[14]] <- c("president", "trustees")
blocking_words[[15]] <- c("mistress", "fellows", "scholars")
blocking_words[[16]] <- c("warden", "fellows")
blocking_words[[17]] <- c("board", "governors")
blocking_words[[18]] <- c("board", "supervisors")
blocking_words[[19]] <- c("board", "regents")
blocking_words[[20]] <- c("board", "trustees")
blocking_words[[21]] <- c("registered", "trustees")
blocking_words[[22]] <- c("trustees")
blocking_words[[23]] <- c("regents")

for(i in 1:length(blocking_words)){
  cat(i, "\t")
  for(j in 1:nrow(allfps_org_names)){
    if(all(blocking_words[[i]] %in% allfps_org_words[[j]])){
      allfps_org_names$new_name[j] <- removeWords(allfps_org_names$new_name[j],
                                                  blocking_words[[i]])
    }
  }
}

# Remove new additional white spaces
allfps_org_names$new_name <- str_squish(allfps_org_names$new_name)


### Algorithm to identify equivalences between pre-processed labels 

# Extract the final list of words from each pre-processed label
allfps_org_words <- vector("list", nrow(allfps_org))
names(allfps_org_words) <- allfps_org$name
allfps_org_words[1:length(allfps_org_words)] <- stri_extract_all_words(allfps_org_names$new_name)

# Express each pre-processed label as a unique string of characters (these strings will be used for different purposes)
allfps_org_characters <- data.frame(org_name = allfps_org$name, 
                                    reduced_name = NA) 

# Remove spaces and non alphanumeric characters from pre-processed labels
allfps_org_characters$reduced_name <- str_replace_all(allfps_org_names$new_name, "[^[:alnum:]]", "")

# Initialize a matrix associating to each element [i,j], the number of common words between label_i and label_j 
allfps_comm_words_matrix <- matrix(0, nrow(allfps_org), nrow(allfps_org))

for(i in 1:(nrow(allfps_comm_words_matrix)-1)){
  cat(i, "\t")
  for(j in (i+1):nrow(allfps_comm_words_matrix)){
    allfps_comm_words_matrix[i,j] = length(intersect(allfps_org_words[[i]], allfps_org_words[[j]]))
  }
}

# Manipulate the matrix to ease following steps: make it a superior triangular matrix
diag(allfps_comm_words_matrix) <- NA
allfps_comm_words_matrix[lower.tri(allfps_comm_words_matrix)] <- NA

# Extract substrings from each string of characters obtained from pre-processed labels (similarly to the extraction of words)
# Consider substrings of length = 4 (names with less than 4 characters remain as they are)
allfps_org_characters_list <- vector("list", nrow(allfps_org))
names(allfps_org_characters_list) <- allfps_org$name

# Introduce a new function computing substrings
allsubstr <- function(x, n){
  unique(substring(x, 1:(nchar(x) - n + 1), n:nchar(x)))
}

for(i in 1:length(allfps_org_characters_list)){
  cat(i, "\t")
  allfps_org_characters_list[[i]] = allsubstr(allfps_org_characters$reduced_name[i], 4)
}

# Initialize a matrix associating to each element [i,j] the binary code 1 if string_i (related to the pre-processed label_i) and string_j (related to the pre-processed label_j) have at least one common substring, 0 otherwise
allfps_comm_strings_matrix <- matrix(0, nrow(allfps_org), nrow(allfps_org))

for(i in 1:(nrow(allfps_comm_strings_matrix)-1)){
  cat(i, "\t")
  for(j in (i+1):nrow(allfps_comm_strings_matrix)){
    if(length(intersect(allfps_org_characters_list[[i]], allfps_org_characters_list[[j]]))>=1){
      allfps_comm_strings_matrix[i,j] = 1
    }
  }
}

# Manipulate the matrix to ease following steps: make it a superior triangular matrix
diag(allfps_comm_strings_matrix) <- NA
allfps_comm_strings_matrix[lower.tri(allfps_comm_strings_matrix)] <- NA

# Initialize a matrix associating to each element [i,j], the code "YES" if the pair [label_i, label_j] satisfies certain criteria based on the number of common words and the number of consecutive common characters, "NO" otherwise
allfps_corr_matrix <- matrix(0, nrow(allfps_org), nrow(allfps_org)) 

for(i in 1:(nrow(allfps_corr_matrix)-1)){
  cat(i, "\t")
  for(j in (i+1):nrow(allfps_corr_matrix)){
    if(nchar(allfps_org_characters[i,2])==2 | nchar(allfps_org_characters[j,2])==2){
      allfps_corr_matrix[i,j]="NO"     
    }
    if(length(allfps_org_words[[i]])==1 | length(allfps_org_words[[j]])==1){
      if(is.na(allfps_org$country[i])==T | 
         is.na(allfps_org$country[j])==T){
        if(allfps_comm_words_matrix[i,j]>=1){
          allfps_corr_matrix[i,j]="YES"
        } else{
          allfps_corr_matrix[i,j]="NO"
        }
      } else{
        if(allfps_comm_strings_matrix[i,j]==1 &  
           allfps_org$country[i]==allfps_org$country[j]){
          allfps_corr_matrix[i,j]="YES"
        } else{
          allfps_corr_matrix[i,j]="NO"
        }
      }
    } else{ 
      if(is.na(allfps_org$country[i])==T | 
         is.na(allfps_org$country[j])==T){
        if(allfps_comm_words_matrix[i,j]>=2){ 
          allfps_corr_matrix[i,j]="YES"
        } else{
          allfps_corr_matrix[i,j]="NO"
        }
      } else{
        if(allfps_comm_strings_matrix[i,j]==1 &    
           allfps_org$country[i]==allfps_org$country[j]){
          allfps_corr_matrix[i,j]="YES"
        } else{
          allfps_corr_matrix[i,j]="NO"
        }
      }
    }
  }
}

# Test the similarity score between labels belonging to those pairs satisfying the aforementioned criteria (i.e., "YES" cells)
# Specifically, we propose a similarity score based on "cosine" distance (the motivation of this choice is discussed in our paper)
corr_allfps <- as.data.frame(which(allfps_corr_matrix=="YES", arr.ind = T))

corr_allfps <- corr_allfps %>% mutate(sim_cosine = NA,
                                      outcome_cosine = NA)

corr_allfps$sim_cosine <- stringsim(allfps_org_characters[corr_allfps[1:nrow(corr_allfps),1],2],
                                    allfps_org_characters[corr_allfps[1:nrow(corr_allfps),2],2],
                                    method = "cosine")

# Define two thresholds for the similarity score (the motivation of this choice is discussed in our paper)
higher_threshold <- 0.99
lower_threshold <- 0.94

# Equivalences between labels whose similarity score is lower than the lower threshold are rejected automatically
corr_allfps$outcome_cosine <- ifelse(corr_allfps$sim_cosine>=lower_threshold, "YES", "NO")

# Determine the final list of equivalences
allfps_corr <- corr_allfps[corr_allfps$outcome_cosine=="YES",1:3] 

# Order rows
allfps_corr <- allfps_corr %>% arrange(allfps_corr$row, allfps_corr$col)
row.names(allfps_corr) <- 1:nrow(allfps_corr) 

# Introduce a binary column indicating if each equivalence is ultimately classified as "TRUE" or "FALSE"
allfps_corr <- allfps_corr %>% mutate(Corr = "") 

# Link row and col numbers with organization names to allow manual checking
allfps_org_row <- data.frame(row = 1:nrow(allfps_org),
                             row_name = allfps_org$name)
allfps_corr_names <- merge(allfps_corr, allfps_org_row, by = "row")

allfps_org_col <- data.frame(col = 1:nrow(allfps_org),
                             col_name = allfps_org$name)
allfps_corr_names <- merge(allfps_corr_names, allfps_org_col, by = "col")

# Equivalences between labels whose similarity score is greater than the higher threshold are accepted automatically
allfps_corr_names$Corr <- ifelse(allfps_corr$sim_cosine>=higher_threshold, "YES", "")

# The remaining undefined equivalences need to be checked by hand

# Save allfps_corr_names as an xlsx file to check them by hand
write_xlsx(allfps_corr_names, path = "allfps_corr_names.xlsx")


### After checking equivalences by hand

# Reload the file with the equivalences checked by hand
allfps_corr_names <- read_excel("allfps_corr_names.xlsx")
allfps_corr_names <- as.data.frame(allfps_corr_names)

# Use the dataset with true equivalences only, and remove the column with sim_cosine
allfps_corr_names <- allfps_corr_names[which(allfps_corr_names$Corr=="YES"),c(1,2,4,5,6)]

# Add those pairs identified through the comparison with the ror database
to_add_ror <- data.frame(row = 0,
                         col = 0,
                         Corr = "YES",
                         row_name = matched_org_ror$name,
                         col_name = matched_org_ror$alias)

# Determine the position of each name in allfps_org
for(i in 1:nrow(to_add_ror)){
  cat(i, "\t")
  to_add_ror$row[i] <- which(tolower(allfps_org$name)==to_add_ror$row_name[i])
  to_add_ror$col[i] <- which(tolower(allfps_org$name)==to_add_ror$col_name[i])
}

# Associate them with the corresponding original name (not necessarily in lower case letters)
for(i in 1:nrow(to_add_ror)){
  cat(i, "\t")
  to_add_ror$row_name[i] <- allfps_org$name[to_add_ror$row[i]]
  to_add_ror$col_name[i] <- allfps_org$name[to_add_ror$col[i]]
}

# Organize rows such that row>col
for(i in 1:nrow(to_add_ror)){
  cat(i, "\t")
  if(to_add_ror$row[i]>to_add_ror$col[i]){
    tmp <- c(to_add_ror$row[i], to_add_ror$col[i], to_add_ror$row_name[i], to_add_ror$col_name[i])
    to_add_ror$row[i] <- as.numeric(tmp[2])
    to_add_ror$col[i] <- as.numeric(tmp[1])
    to_add_ror$row_name[i] <- tmp[4]
    to_add_ror$col_name[i] <- tmp[3]
  }
}

# Generate the final list of equivalences
allfps_corr_names <- rbind(allfps_corr_names, to_add_ror)

# Remove eventual duplicates
allfps_corr_names <- unique(allfps_corr_names)

# Re-order rows
allfps_corr_names <- allfps_corr_names %>% arrange(allfps_corr_names$row, allfps_corr_names$col)
row.names(allfps_corr_names) <- 1:nrow(allfps_corr_names)

# Extract the final list of disambiguated organizations
# Define an index "k" to assign codes to organizations, starting from k = 1000000 in order not to create overlapping with project rcn when generating the network
k <- 1000000 

# In the following codes, all the different sub-cases aim to guarantee the triangular property
for(i in 1:nrow(allfps_corr_names)){
  cat(i, "\t")
  if(allfps_org$Code[allfps_corr_names$row[i]]>0 &
     allfps_org$Code[allfps_corr_names$col[i]]>0 &
     allfps_org$Code[allfps_corr_names$row[i]] != allfps_org$Code[allfps_corr_names$col[i]]){
    allfps_org$Code[allfps_corr_names$row[i]] <- "ERROR"
    allfps_org$Code[allfps_corr_names$col[i]] <- "ERROR"
  } else if(allfps_org$Code[allfps_corr_names$row[i]]>0 &
            allfps_org$Code[allfps_corr_names$col[i]]==0){
    allfps_org$Code[allfps_corr_names$col[i]] <- allfps_org$Code[allfps_corr_names$row[i]]
  } else if(allfps_org$Code[allfps_corr_names$row[i]]==0){
    tmp <- which(allfps_corr_names$row==allfps_corr_names$row[i])
    tmp_1 <- which(allfps_org$Code[allfps_corr_names$col[tmp]]>0)
    if(length(tmp_1)==0){
      for(x in tmp){
        tmp_2 <- which(allfps_corr_names$col==allfps_corr_names$col[x])
        tmp_3 <- which(allfps_org$Code[allfps_corr_names$row[tmp_2]]>0)
        if(length(tmp_3)>0){
          w <- tmp_2[tmp_3]
          allfps_org$Code[allfps_corr_names$row[i]] <- allfps_org$Code[allfps_corr_names$row[w[1]]]
          allfps_org$Code[allfps_corr_names$col[i]] <- allfps_org$Code[allfps_corr_names$row[w[1]]]
        }
      }
      if(allfps_org$Code[allfps_corr_names$row[i]]==0){
        tmp_4 <- which(allfps_corr_names$col==allfps_corr_names$col[i])
        tmp_5 <- which(allfps_org$Code[allfps_corr_names$row[tmp_4]]>0)
        if(length(tmp_5)>0){
          j <- tmp_4[tmp_5]
          allfps_org$Code[allfps_corr_names$row[i]] <- allfps_org$Code[allfps_corr_names$row[j[1]]]
          allfps_org$Code[allfps_corr_names$col[i]] <- allfps_org$Code[allfps_corr_names$row[j[1]]]
        } else {
          for(y in tmp_4){
            tmp_6 <- which(allfps_corr_names$row==allfps_corr_names$row[y])
            tmp_7 <- which(allfps_org$Code[allfps_corr_names$col[tmp_6]]>0)
            if(length(tmp_7)>0){
              z <- tmp_6[tmp_7]
              allfps_org$Code[allfps_corr_names$row[i]] <- allfps_org$Code[allfps_corr_names$col[z[1]]]
              allfps_org$Code[allfps_corr_names$col[i]] <- allfps_org$Code[allfps_corr_names$col[z[1]]]
            } 
          } 
          if(allfps_org$Code[allfps_corr_names$row[i]]==0){
            for(t in tmp){
              tmp_8 <- which(allfps_corr_names$row==allfps_corr_names$col[t])
              tmp_9 <- which(allfps_org$Code[allfps_corr_names$col[tmp_8]]>0)
              if(length(tmp_9)>0){
                h <- tmp_8[tmp_9]
                allfps_org$Code[allfps_corr_names$row[i]] <- allfps_org$Code[allfps_corr_names$col[h[1]]]
                allfps_org$Code[allfps_corr_names$col[i]] <- allfps_org$Code[allfps_corr_names$col[h[1]]]
              }
            }
            if(allfps_org$Code[allfps_corr_names$row[i]]==0){
              k <- k + 1
              allfps_org$Code[allfps_corr_names$row[i]] <- k 
              allfps_org$Code[allfps_corr_names$col[i]] <- k
            }
          }
        }
      } 
    } else {
      j <- tmp[tmp_1]
      allfps_org$Code[allfps_corr_names$row[i]] <- allfps_org$Code[allfps_corr_names$col[j[1]]]
    }
  }
}        

for(i in 1:nrow(allfps_org)){
  cat(i, "\t")
  if(allfps_org$Code[i]==0){
    k = k + 1
    allfps_org$Code[i] = k
  }
}  

# Check if there are residual errors
length(which(allfps_org_final$Code=="ERROR")) # In our case, there are no errors

# Adding eventual equivalences to allfps_corr_names by hand to guarantee the triangular property wherever impossible automatically 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Re-run from line 704 

# When line 786 returns 0, the process is done

# Compute the number of disambiguated organization
length(unique(allfps_org$Code))

# Save the definitive list of disambiguated organizations as an xlsx file
write_xlsx(allfps_org, path = "allfps_org.xlsx")
