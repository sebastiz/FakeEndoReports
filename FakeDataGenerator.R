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
FD_Dilatation<-ListContstructor("FD_Dilatation","FD_DilatationDetails",FINDINGS)
FD_Dilatation<-list(unique(append(FD_Dilatation, replicate(5,paste("dilated to",sample(3:20), "mm with a CRE balloon")))))
FD_DilatationDetails<-ListContstructor("FD_DilatationDetails","FD_ContinuityAdditionals",FINDINGS)

##### LISTS- Continuity replacements random gsub  #####
FD_ContinuityAdditionals<-ListContstructor("FD_ContinuityAdditionals","END",FINDINGS)


#Select out the strings with certain keywords but in any order
#Also get rid of any strings with numbers in them
ENDOFINDINGS <- readLines("/home/rstudio/FakeEndoReports/data/FindingsText")
ENDOFINDINGS<-gsub("\"","",ENDOFINDINGS)
names(ENDOFINDINGS) <- rep("x", length(ENDOFINDINGS))
ENDOFINDINGS <- data.frame(ENDOFINDINGS)
ENDOFINDINGS$ENDOFINDINGS<-as.character(ENDOFINDINGS$ENDOFINDINGS)
Hiat<-data.frame(ENDOFINDINGS[grepl("[Ss]trict",ENDOFINDINGS$ENDOFINDINGS),])
names(Hiat)<-"x"
Hiat<-Hiat[!grepl(".*\\d+.*",Hiat$x),]



########################################################### 1. Sentence creation #######################################################################################
# Sentence creation with macroscopic observations
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

################################################################# 2. Macro Conditional embellishment  ########################################################################
#Conditional embellishment based on  macroscopic observations

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
    x<-gsub("The patient has a nodule",paste0("The patient has a ",sample(3:10,1),"mm nodule."),x)
    x<-gsub("\\.","",x)

    x<-paste(x, "which is",sample(FD_NoduleDescriptors,1,replace=F))
    return(x)
  } else {
    return(x)
  }
})

out<-listtodf(out)

#Extra tidup to allow cm to be added for oesophageal lesions to give distance:
out$out<- lapply(out$out, function(x)gsub("oesophagus at",paste0("oesophagus at ",sample(22:41)," cm"),x))




############################################################### Disease description ###########################################################################################################


##### 4a. Disease eplacement flag - Barretts

#Randomly replace strings with Normal gastroscopy in them
#1. Scatter- Normal findings are changed proportionately to Barrett's intro
#lines

out<-flag("Normal gastroscopy to the duodenum.","Barretts",8,FD_BarrettIntro,out)

#2. Vary & detail
#Flag dispersal-This uses the Barretts flag and then randomly adds variation to that
out<-IntRanMultipleElements("Barrett|Columnar lined",FD_BarrettDetail1_Benign)
out<-listtodf(out)

##### 4b. Disease replacement flag Hiatus hernia  #####
#1. Scatter
out<-flag("Normal gastroscopy to the duodenum.","Hiatus",8,FD_HiatusIntro,out)
#2. Vary: Flag dispersal-This uses the Barretts flag and then randomly adds variation to that &Detail
out<-listtodf(out)



##### 4c. Disease replacement flag Oesophagitis  #####
#1. Scatter
out<-flag("Normal gastroscopy to the duodenum.","Oesophagitis",8,FD_OesophagitisIntro,out)
#2. Vary and Detail
out<-IntRanMultipleElements("oesophagitis",FD_OesophagitisDetail)
out<-listtodf(out)

##### 4d. Disease replacement flagInlet patch  #####
#1. Scatter
out<-flag("Normal gastroscopy to the duodenum.","Inlet",8,FD_InletIntro,out)
#2. Vary and Detail
out<-IntRanOneElement1("Inlet",FD_InletIntro,out)
out<-listtodf(out)

##### 4e.  Disease replacement flag Eosinophilic  #####
#1. Scatter
out<-flag("Normal gastroscopy to the duodenum.","Eosinophilic",8,FD_EosinophilicIntro,out)
#2. Vary and Detail
out<-IntRanMultipleElements("exudates|Pull|crepe|Rings|tight",FD_EosinophilicDetail)
out<-listtodf(out)


############################################################## 5. Report naturalization ######################################################################################
#Using bulking/ conjunction conversion/additional injections




# Then search each report to see if word from the list is present
# - if word present from the list then store the word
# then choose a report from the dataframe not containing that word at random
# then paste that report into the current report.

# b. Conjunction functions
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

v1<-sample(c(" and it also has", ".It has"), nrow(out), replace = TRUE)
out<-gsub("\\.\\s*(?=[a-z])", " ", str_replace_all(out[,1], "\\.It will", v1), perl = TRUE)
out<-listtodf(out)

v1<-sample(c("\nThe patient also has", "\nThe patient has"), nrow(out), replace = TRUE)
out<-gsub("\\.\\s*(?=[a-z])", " ", str_replace_all(out[,1], "\nThe patient has", v1), perl = TRUE)
out<-listtodf(out)

# c. Further continuity phrases to be randomly placed: Random single replacement
out<-RandomSingleGsub("\nThe patient",FD_ContinuityAdditionals)
out<-listtodf(out)


############################################################## 6. Location specific functions for management, biopsies and further details ################################

#The principle here is a. Add the endoscopist's impression of the possible diagnosis based on the lesion and location and b.) Add details about which biopsies were taken.
#Import the list of phrases for sentence construction
LOCATION_BIOPSES <- readLines("/home/rstudio/FakeEndoReports/data/LocationManagementAndBiopsiesPhrases")
LOCATION_BIOPSES<-gsub("\"","",LOCATION_BIOPSES)
names(LOCATION_BIOPSES) <- rep("x", length(LOCATION_BIOPSES))
LOCATION_BIOPSES <- as.list(LOCATION_BIOPSES)


out <- apply(out, 1, function(x) {
  #Stomach Location and Biopsies
  LocationAndBiopsy(x,"polyp.*(stomach|antrum|body|fundus).*\\.","FD_StomachPolypDescriptors","FD_StomachUlcerDescriptors",LOCATION_BIOPSES,
                        "FD_PolypBiopsies","FD_UlcerBiopsies")
 })
out<-listtodf(out)

out <- apply(out, 1, function(x) {
  LocationAndBiopsy(x,"ulcer.*(stomach|antrum|body|fundus)*\\.","FD_StomachUlcerDescriptors","FD_StomachStrictureDescriptors",LOCATION_BIOPSES,
                        "FD_UlcerBiopsies","FD_StrictureBiopsies")
  })
out<-listtodf(out)


out <- apply(out, 1, function(x) {
  t<-LocationAndBiopsy(x,"stricture.*(stomach|antrum|body|fundus).*\\.","FD_StomachStrictureDescriptors","FD_StomachInflammationDescriptors",LOCATION_BIOPSES,
                        "FD_StrictureBiopsies","FD_InflammationBiopsies")
  })
out<-listtodf(out)


out <- apply(out, 1, function(x) {
  t<-LocationAndBiopsy(x,"inflammation.*(stomach|antrum|body|fundus).*\\.","FD_StomachInflammationDescriptors","FD_StomachNoduleDescriptors",LOCATION_BIOPSES,
                        "FD_InflammationBiopsies","FD_NoduleBiopsies")
  })
out<-listtodf(out)


out <- apply(out, 1, function(x) {
  t<-LocationAndBiopsy(x,"nodul.*(stomach|antrum|body|fundus).*\\.","FD_StomachNoduleDescriptors","FD_OesophagealPolypDescriptors",LOCATION_BIOPSES,
                        "FD_NoduleBiopsies","END") })
out<-listtodf(out)


  #Oesophagus and GOJ Location and Biopsies
out <- apply(out, 1, function(x) {
  t<-LocationAndBiopsy(x,"polyp.*(GOJ|oesophagus).*\\.","FD_OesophagealPolypDescriptors","FD_OesophagealUlcerDescriptors",LOCATION_BIOPSES,
                        "FD_PolypBiopsies","FD_UlcerBiopsies")
  })
out<-listtodf(out)


  out <- apply(out, 1, function(x) {
  t<-LocationAndBiopsy(x,"ulcer.*(GOJ|oesophagus).*\\.","FD_OesophagealUlcerDescriptors","FD_OesophagealStrictureDescriptors",LOCATION_BIOPSES,
                        "FD_UlcerBiopsies","FD_StrictureBiopsies")
  })
  out<-listtodf(out)


  out <- apply(out, 1, function(x) {
  t<-LocationAndBiopsy(x,"stricture.*(GOJ|oesophagus).*\\.","FD_OesophagealStrictureDescriptors","FD_OesophagealInflammationDescriptors",LOCATION_BIOPSES,
                        "FD_StrictureBiopsies","FD_InflammationBiopsies")
  })
  out<-listtodf(out)


  out <- apply(out, 1, function(x) {
  t<-LocationAndBiopsy(x,"inflammation.*(GOJ|oesophagus).*\\.","FD_OesophagealInflammationDescriptors","FD_OesophagealNodularDescriptors",LOCATION_BIOPSES,
                        "FD_InflammationBiopsies","FD_NoduleBiopsies")
  })
  out<-listtodf(out)


  out <- apply(out, 1, function(x) {
  t<-LocationAndBiopsy(x,"nodul.*(GOJ|oesophagus).*\\.","FD_OesophagealNodularDescriptors","FD_OesophagealPolypDescriptors",LOCATION_BIOPSES,
                        "FD_NoduleBiopsies","END")
  })
  out<-listtodf(out)



  #Duodenum  Location and Biopsies
  out <- apply(out, 1, function(x) {
  t<-LocationAndBiopsy(x,"polyp.*duod.*\\.","FD_DuodenumPolypDescriptors","FD_DuodenumUlcerDescriptors",LOCATION_BIOPSES,
                        "FD_PolypBiopsies","FD_UlcerBiopsies")
  })
  out<-listtodf(out)


  out <- apply(out, 1, function(x) {
  t<-LocationAndBiopsy(x,"ulcer.*duod.*\\.","FD_DuodenumUlcerDescriptors","FD_DuodenumStrictureDescriptors",LOCATION_BIOPSES,
                        "FD_UlcerBiopsies","FD_StrictureBiopsies")
  })
  out<-listtodf(out)


  out <- apply(out, 1, function(x) {
  t<-LocationAndBiopsy(x,"stricture.*duod.*\\.","FD_DuodenumStrictureDescriptors","FD_DuodenumInflammationDescriptors",LOCATION_BIOPSES,
                        "FD_StrictureBiopsies","FD_InflammationBiopsies")
  })
  out<-listtodf(out)


  out <- apply(out, 1, function(x) {
  t<-LocationAndBiopsy(x,".*inflammation.*duod.*\\.","FD_DuodenumInflammationDescriptors","FD_DuodenumNodularDescriptors",LOCATION_BIOPSES,
                        "FD_InflammationBiopsies","FD_NoduleBiopsies")
  })
  out<-listtodf(out)


  out <- apply(out, 1, function(x) {
  t<-LocationAndBiopsy(x,".*nodul.*duod\\.","FD_DuodenumNodularDescriptors","FD_PolypBiopsies",LOCATION_BIOPSES,
                        "FD_NoduleBiopsies","END")
  })
  out<-listtodf(out)



  # a. Bulking
  #Other diagnoses of relevance
  #paste to the end of the sentences: BUT need to make sure there is no repetition of the already existing words
  #This can be done by assessing which compartment has already been examined and then choosing and rearranging a report from another report
  # Create a list of compartments and also disease specific keywords.

  out<-bulker("GOJ|fundus|oesophag|stomach body|duodenal bulb|antrum|second part of the duodenum|third part of the duodenum")
  out<-listtodf(out)



############################################################## 7. Therapy functions #######################################################################################
#Therapy types
#1) Polyp 2) RFA and EMR 3) Dilatation - oesophageal and stricture mentioned

#This has to be based on conditionals from the existing text.
out<-IntRanOneElement1("(oesophagus .* stricture)|(stricture .* oesophagus)",FD_Dilatation,out)
out<-data.frame(out)
out$out<-as.character(out$out)


############################################################## 8. Management functions #######################################################################################
# Add management advice based on what has been seen

#This has to be based on conditionals from the existing text.
out<-IntRanOneElement1("(oesophagus .* stricture)|(stricture .* oesophagus)",FD_Dilatation,out)
out<-listtodf(out)



############################################################## 9. Pathology functions #######################################################################################
#
# #Need to examine each sentence to determine whether biopsy taken from that compartment and if any number taken
# NATURE OF SPECIMEN:
#   x2 right colon bx, x 2 left colon bx.
#
# #Need compartment specific indication- can get from the endoscopic indication when this is done
# CLINICAL DETAILS
# Alternating diarrhoea and constipation ? microscopic colitis.
#
# #Derive this from the Nature of the Specimen
# MACROSCOPICAL DESCRIPTION
# Nature of specimen as stated on request form = ' x2 right colon bx, x 2 left
#   colon bx.'.
# Nature of specimen as stated on pot = ' x2 right colon bx, x 2 left colon
#   bx.'.
#
# Four pieces of tissue, the largest measuring 3 x 1 x 1 mm and the smallest 2 x
# 1 x 1 mm, received on a pointed cellulose strip.
# All in one.
#
# Submitted by: Peter Sounthararajah 20.04.16
# sch
#
# #Will need to gets the varieties of this again based on the caompartment sampled- may need to categorise phrases based on compartment taken eg eoe phrases/ Barrett's phrases etc.
# HISTOLOGY
# These biopsies of large bowel mucosa are within normal histological limits.
# There is no evidence of microscopic colitis.
#
# DIAGNOSIS
# Right and left colon, biopsies:
#   - normal.
# Reported by Dr Helene McCarthy and Dr Ula Mahadeva /28-04-16
# T67995, M14070, M00120


#To do:
#Create indications for the endoscopies from the endocsopy reports from STH
#Start creating the pathology reports
#NATURE OF SPECIMEN: Derived from the section about whether biopsies taken

#Create the report:  Rule: If biopsies taken and not negative then use create pathology reports:
out$pathreport<-ifelse(grepl(".*iopsi.*",out$out),"WritePathReport",NA)
#Create the number of biopsies taken
out$NumberBiopsies<-str_extract_all(out$out, paste("biopsied x[0-9]{1,2}", sep = ""))

# Rule: If number of biopsies mentioned in endoscopic findings then use this otherwise make up
# Rule: The biopsy compartment is derived from the sentence

#CLINICAL DETAILS: Derived from combination of the indication and the endoscopic findings text
#HISTOLOGY Derived from the compartment that the biopsies were taken from
#MACROSCOPICAL DESCRIPTION Derived from the Nature of specimen with sizes added
#DIAGNOSIS Derived from the Histology section
