library(stringr)
#Get the out to be dataframe again after function
library(FakeEndoReports)

################################################################ LISTS- Sentence introduction #####################################################################################
#Import the list of phrases for sentence construction
FINDINGS <- readLines("/home/rstudio/FakeEndoReports/data/ImportPhrases")
FINDINGS<-gsub("\"","",FINDINGS)
names(FINDINGS) <- rep("x", length(FINDINGS))
FINDINGS <- as.list(FINDINGS)


##### LISTS- Sentence introduction #####
# Distribute the reports so that there are empties and Normals
FD_SentenceIntro<-ListContstructor("FD_SentenceIntro","FD_SentenceIntro1",FINDINGS)
FD_SentenceIntro1<-ListContstructor("FD_SentenceIntro1","FD_Object",FINDINGS)

##### LISTS- Basic descriptor lists #####
FD_Object<-ListContstructor("FD_Object","FD_Location",FINDINGS)
FD_Location<-ListContstructor("FD_Location","FD_PolypDescriptors",FINDINGS)

##### LISTS- Macroscopic findings #####
FD_PolypDescriptors<-ListContstructor("FD_PolypDescriptors","FD_UlcerDescriptors",FINDINGS)
FD_UlcerDescriptors<-ListContstructor("FD_UlcerDescriptors","FD_StrictureDescriptors",FINDINGS)
FD_StrictureDescriptors<-ListContstructor("FD_StrictureDescriptors","FD_InflammationDescriptors",FINDINGS)
FD_InflammationDescriptors<-ListContstructor("FD_InflammationDescriptors","FD_NoduleDescriptors",FINDINGS)
FD_NoduleDescriptors<-ListContstructor("FD_NoduleDescriptors","FD_UlcerDescSecond",FINDINGS)

##### LISTS- Secondary embellishments- generic and not location specific #####
FD_UlcerDescSecond<-ListContstructor("FD_UlcerDescSecond","FD_PolypDescSecond",FINDINGS)
FD_PolypDescSecond<-ListContstructor("FD_PolypDescSecond","FD_StrictureDescSecond",FINDINGS)
FD_StrictureDescSecond<-ListContstructor("FD_StrictureDescSecond","FD_BarrettIntro",FINDINGS)

##### LISTS- DISEASE SPECIFIC #####
FD_BarrettIntro<-ListContstructor("FD_BarrettIntro","FD_BarrettDetail1_Decsrip",FINDINGS)
FD_BarrettDetail1_Decsrip<-ListContstructor("FD_BarrettDetail1_Decsrip","FD_BarrettDetail1_Benign",FINDINGS)
FD_BarrettDetail1_Benign<-ListContstructor("FD_BarrettDetail1_Benign","FD_BarrettDetail2",FINDINGS)
FD_BarrettDetail2<-ListContstructor("FD_BarrettDetail2","FD_BarrettDetail1",FINDINGS)
FD_BarrettDetail1<-ListContstructor("FD_BarrettDetail1","FD_HiatusIntro",FINDINGS)
FD_HiatusIntro<-ListContstructor("FD_HiatusIntro","FD_OesophagitisIntro",FINDINGS)
FD_OesophagitisIntro<-ListContstructor("FD_OesophagitisIntro","FD_OesophagitisDetail",FINDINGS)
FD_OesophagitisIntro<-ListReplicator("LA Grade ",sample(c("A", "B", "C", "D")),"oesophagitis",FD_OesophagitisIntro)
FD_OesophagitisDetail<-ListContstructor("FD_OesophagitisDetail","FD_InletIntro",FINDINGS)
FD_InletIntro<-ListContstructor("FD_InletIntro","FD_EosinophilicIntro",FINDINGS)
FD_EosinophilicIntro<-ListContstructor("FD_EosinophilicIntro","FD_EosinophilicDetail",FINDINGS)
FD_EosinophilicDetail<-ListContstructor("FD_EosinophilicDetail","FD_Dilatation",FINDINGS)
FD_Dilatation<-ListContstructor("FD_Dilatation","FD_Object",FINDINGS)
FD_Dilatation<-list(unique(append(FD_Dilatation, replicate(5,paste("dilated to",sample(10:20), "mm with a CRE balloon")))))
FD_DilatationDetails<-ListContstructor("FD_DilatationDetails","FD_ContinuityAdditionals",FINDINGS)

##### LISTS- Continuity replacements random gsub  #####
FD_ContinuityAdditionals<-ListContstructor("FD_ContinuityAdditionals","END",FINDINGS)


##########################################################################################################################################################################

#Develop a function here that extracts according to regular expression from the data list.
#Something like, if you want Barrett's with dysplasia '



#Select out the strings with certain keywords but in any order
#Also get rid of any strings with numbers in them

Hiat<-data.frame(FINDINGS[grepl("[Tt]rache",FINDINGS$x),])
names(Hiat)<-"x"
Hiat<-Hiat[!grepl(".*\\d+.*",Hiat$x),]





########################################################### 1. Sentence creation #######################################################################################
FD1<-data.frame(paste0(replicate(1000,sample(FD_SentenceIntro,1,replace=F)),""))

#Fill out the non-normal endoscopies with macroscopic findings
out <- apply(FD1, 1, function(x) {
  if (stringr::str_detect(x, "Normal")) {
    return(x)
  } else {
    return(paste0(sample(FD_SentenceIntro1,1,replace=F),sample(FD_Object,1,replace=F)," in the ",sample(FD_Location,1,replace=F),"."))
  }
})
out<-listtodf(out)
##########################################################################################################################################################################







################################################################# 2. Primary conditional embellishment #########################################################################################################

#Assess the sentence so far and use conditional embellishment to add further information:

out <- apply(out, 1, function(x) {
  if (stringr::str_detect(x, "polyp")) {
    x<-gsub("\\.","",x)
    polypff<-paste(x, "which is",sample(FD_PolypDescriptors,1,replace=F))
    return(paste0(polypff, sample(FD_PolypDescSecond,1,replace=F),"."))
  }
  if (stringr::str_detect(x, "ulcer")) {
    x<-gsub("\\.","",x)
   ulcerff<-paste(x, "which is",sample(FD_UlcerDescriptors,1,replace=F))
           return(sample(paste0(ulcerff, sample(FD_UlcerDescSecond,1,replace=F),".")))
  }
  if (stringr::str_detect(x, "stricture")) {
    x<-gsub("\\.","",x)
    strictureff<-paste(x, "which is",sample(FD_StrictureDescriptors,1,replace=F))
    return(paste0(strictureff, sample(FD_StrictureDescSecond,1,replace=F),"."))
  }
  if (stringr::str_detect(x, "inflammation")) {
    x<-gsub("\\.","",x)
    return(paste(x, "which is",sample(FD_InflammationDescriptors,1,replace=F)))
  }
  if (stringr::str_detect(x, "nodule")) {
    x<-gsub("\\.","",x)
    return(paste(x, "which is",sample(FD_NoduleDescriptors,1,replace=F)," and",sample(3:10,1),"mm."))
  } else {
    return(x)
  }
})

out<-listtodf(out)
##########################################################################################################################################################################









############################################################### Minority replacement flag ###########################################################################################################


##### 4a. Minority replacement flag - Barretts

#Randomly replace strings with Normal gastroscopy in them
#1. Scatter

out<-flag("Normal gastroscopy to the duodenum.","Barretts",8,FD_BarrettIntro,out)
#2. Vary & detail
#Flag dispersal-This uses the Barretts flag and then randomly adds variation to that
out<-IntRanMultipleElements("Barrett|Columnar lined",FD_BarrettDetail1_Benign)
out<-listtodf(out)

##### 4b. Minority replacement flag Hiatus hernia  #####
#1. Scatter
out<-flag("Normal gastroscopy to the duodenum.","Hiatus",8,FD_HiatusIntro,out)
#2. Vary: Flag dispersal-This uses the Barretts flag and then randomly adds variation to that &Detail
out<-listtodf(out)


##### 4c. Minority replacement flag Oesophagitis  #####
#1. Scatter
out<-flag("Normal gastroscopy to the duodenum.","Oesophagitis",8,FD_OesophagitisIntro,out)
#2. Vary and Detail
out<-IntRanMultipleElements("oesophagitis",FD_OesophagitisDetail)
out<-listtodf(out)

##### 4d. Minority replacement flagInlet patch  #####
#1. Scatter
out<-flag("Normal gastroscopy to the duodenum.","Inlet",8,FD_InletIntro,out)
#2. Vary and Detail
out<-IntRanOneElement1("Inlet",FD_InletIntro)
out<-listtodf(out)


##### 4e. Minority replacement flag Eosinophilic  #####
#1. Scatter
out<-flag("Normal gastroscopy to the duodenum.","Eosinophilic",8,FD_EosinophilicIntro,out)
#2. Vary and Detail
out<-IntRanMultipleElements("exudates|Pull|crepe|Rings|tight",FD_EosinophilicDetail)
out<-listtodf(out)

##########################################################################################################################################################################










############################################################## 5. Additional diagnoses functions ######################################################################################


##### 5. Additional diagnoses functions #####
#Other diagnoses of relevance
#paste to the end of the sentences: BUT need to make sure there is no repetition of the already existing words
#This can be done by assessing which compartment has already been examined and then choosing and rearranging a report from another report
# Create a list of compartments and also disease specific keywords.

out<-bulker("GOJ|fundus|oesophag|stomach body|duodenal bulb|antrum|second part of the duodenum|third part of the duodenum")
out<-listtodf(out)

# Then search each report to see if word from the list is present
# - if word present from the list then store the word
# then choose a report from the dataframe not containing that word at random
# then paste that report into the current report.


##### 6. Conjunction functions #####
#If there is an 'It, ot 'This is', then replace it with an and but do so randomly - random gsubbing

v1<-sample(c(" with", ".It has"), nrow(out), replace = TRUE)
out<-gsub("\\.\\s*(?=[a-z])", " ", str_replace_all(out[,1], "\\.It has", v1), perl = TRUE)
out<-listtodf(out)

v1<-sample(c(" was also", "was"), nrow(out), replace = TRUE)
out<-gsub("\\.\\s*(?=[a-z])", " ", str_replace_all(out[,1], "was", v1), perl = TRUE)
out<-listtodf(out)

v1<-sample(c(" were also", "were"), nrow(out), replace = TRUE)
out<-gsub("\\.\\s*(?=[a-z])", " ", str_replace_all(out[,1], "were", v1), perl = TRUE)
out<-listtodf(out)

v1<-sample(c(" which is", ".\\s*It is"), nrow(out), replace = TRUE)
out<-gsub("\\.\\s*(?=[a-z])", " ", str_replace_all(out[,1], "\\.It is", v1), perl = TRUE)
out<-listtodf(out)

v1<-sample(c(" and looks", ".It looks"), nrow(out), replace = TRUE)
out<-gsub("\\.\\s*(?=[a-z])", " ", str_replace_all(out[,1], "\\.It looks", v1), perl = TRUE)
out<-listtodf(out)

v1<-sample(c(" and will", ".It will"), nrow(out), replace = TRUE)
out<-gsub("\\.\\s*(?=[a-z])", " ", str_replace_all(out[,1], "\\.It will", v1), perl = TRUE)
out<-listtodf(out)

#Further continuity phrases to be randomly placed: Random single replacement
out<-RandomSingleGsub("\nThe patient",FD_ContinuityAdditionals)

##### 7. Therapy functions #####
#Therapy types
#1) Polyp 2) RFA and EMR 3) Dilatation - oesophageal and stricture mentioned

#This has to be based on conditionals from the existing text.
out<-IntRanOneElement1("(oesophagus .* stricture)|(stricture .* oesophagus)",FD_Dilatation)

##########################################################################################################################################################################




##### Location specific functions for management, biopsies and further details#####

out2 <- apply(out, 1, function(x) {
  if (stringr::str_detect(x, "polyp.*(stomach|antrum).*\\.")) {
      FD_StomchPolypDescriptors<-list(x="The gastric polyp looks fundic",x='The gastric polyp looks malignant',x='The gastric polyp looks adenomatous',x='The gastric polyp looks hyperplastic')
    polypff<-paste(x,sample(FD_StomchPolypDescriptors,1,replace=F))
    FD_StomchPolypBiopses<-list(x="The polyp was biopsied",x='The polyp was resected by EMR',x='The polyp was tatooed',x='The polyp was too large to resect')
    return(paste0(polypff, sample(FD_PolypDescSecond,1,replace=F),sample(FD_StomchPolypBiopses,1,replace=F),"."))
  }
  else {
    return(x)
  }
})


out2<-listtodf(out2)



###################################################################### 8. Biopsy functions ##################################################################################


##### 8. Biopsy functions #####

#Pick the segment that you want to biopsy then vary the number of biopsies taken from each. Also vary the phrase.
#if not Normal endoscopy #Biopsies were taken. #Biopsies were taken from the <segment>
