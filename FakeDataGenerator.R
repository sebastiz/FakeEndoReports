library(stringr)
library(rprojroot)
library(randomNames)

myroot<-rprojroot::find_root("DESCRIPTION")
#Get the out to be dataframe again after function
source(here::here("R","reportMaker.R"))
source(here::here("R","listMaker.R"))



########################################################### 1. Sentence creation #######################################################################################
# Sentence creation with macroscopic observations
FD1<-data.frame(paste0(replicate(1000,sample(FD_SentenceIntro,1,replace=F)),""))

#Fill out the non-normal endoscopies with macroscopic findings
out <- data.frame(apply(FD1, 1, function(x) {
  if (stringr::str_detect(x, "Normal")) {
    return(x)
  } else {
    return(paste0(sample(FD_SentenceIntro1,1,replace=F),sample(FD_Object,1,replace=F)," in the ",sample(FD_Location,1,replace=F),"."))
  }
}))
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
out$out<- lapply(out$out, function(x) gsub("oesophagus at",paste0("oesophagus at ",sample(22:41)," cm"),x))
out<-listtodf(out)




############################################################### 4. Disease description ###########################################################################################################


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

#Extract the compartment for pathology later
Compartment<-str_extract_all(out$out, "Columnar|Barrett|stomach|antrum|body|fundus|GOJ|oesophag|oesophagitis|duod|D1|inlet|hernia")
Compartment<-lapply(as.character(Compartment),function(x)str_replace_all(x, "[[:punct:]]", ""))

#Remove duplicate words that confuse things like oesophag oesophag
Compartment<-gsub("^c","",Compartment)
Compartment<-gsub("oesophag oesophag","oesophag",Compartment)
Compartment<-gsub("oesophag hernia","oesophag",Compartment)
Compartment<-gsub("oesophag oesophag","oesophag",Compartment)
Compartment<-gsub("Barrett oesophag","Barrett",Compartment)
Compartment<-gsub("Columnar oesophag","Barrett",Compartment)

############################################################## 6. Location specific functions for management, biopsies and further details ################################

#The principle here is a. Add the endoscopist's impression of the possible diagnosis based on the lesion and location and b.) Add details about which biopsies were taken.
#Import the list of phrases for sentence construction
LOCATION_BIOPSES <- readLines(here::here("data","LocationManagementAndBiopsiesPhrases"))
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
                        "FD_NoduleBiopsies","FD_Ulcer_Recommendations")
  })
  out<-listtodf(out)

  #Start to extract elements necessary for the pathology report:
  pathreport<-ifelse(grepl(".*[Bb]iops.*",out$out),"WritePathReport",NA)

  #Create the number of biopsies taken
  NumberBiopsies<-str_extract_all(out$out, paste("biopsied x[0-9]{1,2}", sep = ""))
  #out$Bx<-paste(out$pathreport,out$NumberBiopsies,out$Compartment)
  out<-listtodf(out)

###################################################### Bulking ###################################################################################################
  # a. Bulking
  #Other diagnoses of relevance
  #paste to the end of the sentences: BUT need to make sure there is no repetition of the already existing words
  #This can be done by assessing which compartment has already been examined and then choosing and rearranging a report from another report
  # Create a list of compartments and also disease specific keywords.

  out<-paste(out$out,"COMPARTMENT_START",Compartment,"BIOPSIES TAKEN:", pathreport,"NUMBER OF BIOPSIES:",NumberBiopsies,"COMPARTMENT_END")
  out<-listtodf(out)

   out<-bulker("GOJ|fundus|oesophag|stomach body|duodenal bulb|antrum|second part of the duodenum|third part of the duodenum",out)
  # out<-listtodf(out)

##################################################################################################################################################################






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

############################################################## 9. Endocsopy indication #######################################################################################
Indic<-paste0(replicate(1000,sample(FD_Indication,1,replace=F)),"")
#Merge the Indication in to the Pathology report:
out$out<-paste0("INDICATIONS FOR PROCEDURE: ",Indic," FINDINGS: ",out$out)
out<-listtodf(out)








###############################################################################################################################################################################



############################################################## 9. Follow-up: #######################################################################################

#If there are any of the FD_Object in findings, then add the appropriate recommendations in to the text:



###############################################################################################################################################################################




############################################################## 10. Pathology functions #######################################################################################
#The psinciples of creating the pathology datasets are as follows
#1. Take from the endoscopy report which compartment was sampled (eg stomach/oesophagus etc)
#2. Create number of biopsies taken from that compartment
#3. Use this as a prompt for a pathology report to be written
#4. As a separate dataset create pathology reports for conditions from different compartmentes
#4a) Get list of commonly used phrases in pathology reports for each compartment
#4b) Separate the phrases into 1) Macroscopic Description ii) Microscopic Description
#iii) Conclusion iv)Recommendations for each condition
#Combine all the pathology reports for each condition together and randomly distribute the
#path reports to associate with endoscopic reports as long as the compartments match up

pathReport<-str_extract_all(out$out,"COMPARTMENT_START.*COMPARTMENT_END")
pathReport<-lapply(pathReport,function(x)str_replace_all(x, "[[:punct:]]", ""))
#Remove duplicate words that confuse things like oesophag oesophag
pathReport<-gsub("COMPARTMENTSTART c"," COMPARTMENTSTART",pathReport)
pathReport<-gsub("oesophag oesophag","oesophag",pathReport)
pathReport<-gsub("oesophag oesophag","oesophag",pathReport)
pathReport<-gsub("Barrett oesophag","Barrett",pathReport)
pathReport<-gsub("Columnar oesophag","Barrett",pathReport)

dff<-str_extract_all(as.character(pathReport),"COMPARTMENTSTART.*COMPARTMENTEND")
dff2<-lapply(dff,function(x) paste(x,collapse=','))
dff3<-data.frame(gsub("COMPARTMENTSTART .* BIOPSIES TAKEN: NA NUMBER OF BIOPSIES: character.*COMPARTMENTEND",NA,dff2))
names(dff3)<-"report"
dff3$report<-gsub("BIOPSIES TAKEN:","biopsies taken",dff3$report)

library(gsubfn)
#Add in a random number of biopsies
dff3$report<-gsubfn("(COMPARTMENTSTART)", function(x) paste0("x", sample(3:10, 1)), dff3$report)
dff3$report<-gsub("\"","",dff3$report)
dff3$WritePathReport<-ifelse(grepl("WritePathReport",dff3$report),paste(str_extract(dff3$report,"\\d+.*taken")),"")
dff3$report<-paste0("Nature of specimen:",dff3$report)


#Bind dff3 and out together
out2<-cbind(dff3,out)
out2$Compartment<-Compartment

out2$out<-gsub("COMPARTMENTSTART .* BIOPSIES TAKEN: NA NUMBER OF BIOPSIES: .*COMPARTMENTEND","",out2$out)


#Compartment specific pathology
###### _______Pathology- Duodenum ########################################################################################################################

# a. Sentence creation with macroscopic observations
FD_path<-data.frame(paste0("Macroscopic Description: ",replicate(1000,sample(FD_DuodenumMacroscopic_Description,1,replace=F)),"",". Microscopic Description"))

# Create the normal findings
FD_Normal<-data.frame(paste0(replicate(1000,sample(FD_DuodenumConclusion_Normal,1,replace=F)),"",". Microscopic Description"))
FD_path[,1]<-paste(FD_path[,1],FD_Normal[,1])
#Add in positive and negative findings just for bulking purposes
FD_path<-data.frame(paste(FD_path[,1],ListNegsAndPos(FD_path)))
names(FD_path)<-"report"

# b. Get the list of relevant negatives:
ListCheck<-as.character(FD_DuodenumDescription_List)

# c. Create the coeliac findings -just 100 of them and then add to the reports via rbind
FD_CD1_VA<-data.frame(paste0(replicate(100,sample(FD_DuodenumDescription_VA,1,replace=F)),""))
FD_CD2_IELs<-data.frame(paste0(replicate(100,sample(FD_DuodenumDescription_IELs,1,replace=F)),""))
FD_CD3_Inflamm<-data.frame(paste0(replicate(100,sample(FD_DuodenumDescription_Inflammation,1,replace=F)),""))
FD_CD4_Ratio<-data.frame(paste0(replicate(100,sample(FD_DuodenumDescription_Ratio,1,replace=F)),""))
FD_CD5_Conc<-data.frame(paste0(replicate(100,sample(FD_DuodenumConclusion_CD,1,replace=F)),""))
FD_path_CD<-data.frame(paste(FD_CD1_VA[,1],FD_CD2_IELs[,1],FD_CD3_Inflamm[,1],FD_CD4_Ratio[,1],"Conclusion: ",FD_CD5_Conc))
names(FD_path_CD)<-"report"

# d. Add the negatives list in before the conclusion so it is in the correct order
# d)i). Check which elements from the list are not present in the text so no contradictions
FD_path_CDlist<-ListNegsAndPos(FD_path_CD)
FD_path_CD<-data.frame(paste(FD_path_CD[,1],FD_path_CDlist,FD_CD5_Conc[,1]))
names(FD_path_CD)<-"report"

# f. bind everything together:

Final_path_Duodenum<-as.list(rbind(FD_path,FD_path_CD))
Final_path_Duodenum$report<-as.character(Final_path_Duodenum$report)



##### _______Pathology Barretts ########################################################################################################################
# NB. No normal findings as by definition already have Barrett's oesophagus

# a. Sentence creation with macroscopic observations
FD_path_SentenceStartBarr<-data.frame(paste0("Macroscopic Description: ",replicate(1000,sample(FD_BarrettsMacroscopic_Description,1,replace=F)),"",". Microscopic Description"))

# b. Get the list of relevant negatives:
ListCheck<-as.character(FD_BarrettsDescription_List)

# c. Create the Barrett's findings -just 100 of them and then add to the reports via rbind
FD_CD2_BarrettsDescription_OAC<-data.frame(paste0(replicate(100,sample(FD_BarrettsDescription_OAC,1,replace=F)),""))
names(FD_CD2_BarrettsDescription_OAC)<-"report"
FD_CD3_BarrettsDescription_D<-data.frame(paste0(replicate(100,sample(FD_BarrettsDescription_D,1,replace=F)),""))
FD_CD4_BarrettsDescription<-data.frame(paste0(replicate(100,sample(FD_BarrettsDescription,1,replace=F)),""))
FD_CD4_BarrettsConclusion2<-data.frame(paste0(" Conclusion: ",replicate(100,sample(FD_BarrettsConclusion2,1,replace=F)),""))
FD_CD5_BarrettsConclusion_OAC<-data.frame(paste0(" Conclusion: ",replicate(100,sample(FD_BarrettsConclusion_OAC,1,replace=F)),""))
FD_CD6_BarrettsConclusion_Inflamm<-data.frame(paste0(" Conclusion: ",replicate(100,sample(FD_BarrettsConclusion_Inflamm,1,replace=F)),""))
FD_CD7_BarrettsConclusion_D<-data.frame(paste0(" Conclusion: ",replicate(100,sample(FD_BarrettsConclusion_D,1,replace=F)),""))

# d. Add the negatives list in before the conclusion so it is in the correct order
# d)i). Check which elements from the list are not present in the text so no contradictions
FD_BarrettsDescription_OAClist<-ListNegsAndPos(FD_CD2_BarrettsDescription_OAC)
# d)ii). Paste the list between the sentence start and the conclusion for that disease
Barr_OAC<-data.frame(paste(FD_path_SentenceStartBarr[,1],FD_BarrettsDescription_OAClist,FD_CD5_BarrettsConclusion_OAC[,1]))
# d)iii) Always make sure that the column is named "report" so that the rbind can happen for that disease
names(Barr_OAC)<-"report"


# e.Repeat as above for the other diseases:
FD_CD3_BarrettsDescription_Dlist<-ListNegsAndPos(FD_CD3_BarrettsDescription_D)
Barr_D<-data.frame(paste(FD_path_SentenceStartBarr[,1],FD_CD3_BarrettsDescription_Dlist,FD_CD7_BarrettsConclusion_D[,1]))
names(Barr_D)<-"report"

FD_CD4_BarrettsDescriptionlist<-ListNegsAndPos(FD_CD4_BarrettsDescription)
Barr_Only<-data.frame(paste(FD_path_SentenceStartBarr[,1],FD_CD4_BarrettsDescriptionlist,FD_CD6_BarrettsConclusion_Inflamm[,1]))
names(Barr_Only)<-"report"

# f. bind everything together:
FD_path_Barr<-as.list(rbind(Barr_OAC,Barr_D,Barr_Only),stringsAsFactors=F)
FD_path_Barr$report<-as.character(FD_path_Barr$report)



###### _______Pathology- Oesophagus- non Barrett's oesophagus ########################################################################################################################

# a. Sentence creation with macroscopic observations
FD_path_SentenceStartOesoph<-data.frame(paste0("Macroscopic Description: ",replicate(1000,sample(FD_OesophagusMacroscopic_Description,1,replace=F)),"",". Microscopic Description"))

# b. Get the list of relevant negatives:
ListCheck<-as.character(FD_OesophagusDescription_List)

# c. Create the Barrett's findings -just 100 of them and then add to the reports via rbind
FD_CD2_OesophagusDescription_Inflammation<-data.frame(paste0(replicate(100,sample(FD_OesophagusDescription_Inflammation,1,replace=F)),""))
names(FD_CD2_BarrettsDescription_OAC)<-"report"
FD_CD3_OesophagusDescription_EoE<-data.frame(paste0(replicate(100,sample(FD_OesophagusDescription_EoE,1,replace=F)),""))
FD_CD4_OesophagusDescription_Dysplasia<-data.frame(paste0(replicate(100,sample(FD_OesophagusDescription_Dysplasia,1,replace=F)),""))
FD_CD4OesophagusConclusion2<-data.frame(paste0("Conclusion: ",replicate(100,sample(FD_OesophagusConclusion2,1,replace=F)),""))
FD_CD5_OesophagusConclusion_Normal<-data.frame(paste0("Conclusion: ",replicate(100,sample(FD_OesophagusConclusion_Normal,1,replace=F)),""))
FD_CD6_OesophagusConclusion_Inflammation<-data.frame(paste0("Conclusion: ",replicate(100,sample(FD_OesophagusConclusion_Inflammation,1,replace=F)),""))
FD_CD7_OesophagusConclusion_EoE<-data.frame(paste0("Conclusion: ",replicate(100,sample(FD_OesophagusConclusion_EoE,1,replace=F)),""))
FD_CD8_OesophagusConclusion_Dysplasia<-data.frame(paste0("Conclusion: ",replicate(100,sample(FD_OesophagusConclusion_Dysplasia,1,replace=F)),""))


# d. Add the negatives list in before the conclusion so it is in the correct order
# d)i). Check which elements from the list are not present in the text so no contradictions
FD_CD2_OesophagusDescription_Inflammationlist<-ListNegsAndPos(FD_CD2_OesophagusDescription_Inflammation)
# d)ii). Paste the list between the sentence start and the conclusion for that disease
Oesoph_Inflam<-data.frame(paste(FD_path_SentenceStartOesoph[,1],FD_CD2_OesophagusDescription_Inflammationlist,FD_CD6_OesophagusConclusion_Inflammation[,1]))
# d)iii) Always make sure that the column is named "report" so that the rbind can happen for that disease
names(Oesoph_Inflam)<-"report"


# e.Repeat as above for the other diseases:
FD_CD3_OesophagusDescription_EoElist<-ListNegsAndPos(FD_CD3_OesophagusDescription_EoE)
Oesoph_EoE<-data.frame(paste(FD_path_SentenceStartOesoph[,1],FD_CD3_OesophagusDescription_EoElist,FD_CD7_OesophagusConclusion_EoE[,1]))
names(Oesoph_EoE)<-"report"

FD_CD8_OesophagusConclusion_Dysplasialist<-ListNegsAndPos(FD_CD4_BarrettsDescription)
Oesoph_Dysplasia<-data.frame(paste(FD_path_SentenceStartOesoph[,1],FD_CD8_OesophagusConclusion_Dysplasialist,FD_CD8_OesophagusConclusion_Dysplasia[,1]))
names(Oesoph_Dysplasia)<-"report"

# f. bind everything together:
FD_path_Oesoph<-as.list(rbind(Oesoph_Inflam,Oesoph_EoE,Oesoph_Dysplasia),stringsAsFactors=F)
FD_path_Oesoph$report<-as.character(FD_path_Oesoph$report)


###### _______Pathology- Stomach ########################################################################################################################


###### _______Pathology- Stomach  ########################################################################################################################

# a. Sentence creation with macroscopic observations
FD_path_SentenceStartStomach<-data.frame(paste0("Macroscopic Description: ",replicate(1000,sample(FD_StomachMacroscopic_Description,1,replace=F)),""," Microscopic Description"))

# b. Get the list of relevant negatives:
#### May need to have generic list here to be included in all diseases and compartments
ListCheck<-as.character(FD_OesophagusDescription_List)

# c. Create the Barrett's findings -just 100 of them and then add to the reports via rbind
FD_CD2_StomachDescription_Inflammation<-data.frame(paste0(replicate(100,sample(FD_StomachDescription_Inflammation,1,replace=F)),""))
names(FD_CD2_StomachDescription_Inflammation)<-"report"
FD_CD3_StomachDescription_Description_General<-data.frame(paste0(replicate(100,sample(FD_StomachDescription_Description_General,1,replace=F)),""))
FD_CD4_StomachDescription_Dysplasia<-data.frame(paste0(replicate(100,sample(FD_StomachDescription_Dysplasia,1,replace=F)),""))
FD_CD4_StomachConclusion2<-data.frame(paste0("Conclusion: ",replicate(100,sample(FD_StomachConclusion2,1,replace=F)),""))
FD_CD5_StomachConclusion_Polyp<-data.frame(paste0("Conclusion: ",replicate(100,sample(FD_StomachConclusion_Polyp,1,replace=F)),""))
FD_CD6_StomachConclusion_Inflammation<-data.frame("Conclusion: ",paste0(replicate(100,sample(FD_StomachConclusion_Inflammation,1,replace=F)),""))
FD_CD7_StomachConclusion_Cancer<-data.frame(paste0("Conclusion: ",replicate(100,sample(FD_StomachConclusion_Cancer,1,replace=F)),""))


# d. Add the negatives list in before the conclusion so it is in the correct order
# d)i). Check which elements from the list are not present in the text so no contradictions
FD_CD2_StomachDescription_Inflammationlist<-ListNegsAndPos(FD_CD2_StomachDescription_Inflammation)
# d)ii). Paste the list between the sentence start and the conclusion for that disease
Stomach_Inflam<-data.frame(paste(FD_path_SentenceStartStomach[,1],FD_CD2_StomachDescription_Inflammationlist,FD_CD6_StomachConclusion_Inflammation[,1],FD_CD4_StomachConclusion2[,1]))
# d)iii) Always make sure that the column is named "report" so that the rbind can happen for that disease
names(Stomach_Inflam)<-"report"


# e.Repeat as above for the other diseases:
FD_CD4_StomachDescription_Dysplasialist<-ListNegsAndPos(FD_CD4_StomachDescription_Dysplasia)
Stomach_Dysplasia<-data.frame(paste(FD_path_SentenceStartStomach[,1],FD_CD4_StomachDescription_Dysplasialist,FD_CD7_StomachConclusion_Cancer[,1],FD_CD4_StomachConclusion2[,1]))
names(Stomach_Dysplasia)<-"report"

FD_CD3_StomachDescription_Description_Generalist<-ListNegsAndPos(FD_CD3_StomachDescription_Description_General)
Stomach_Polyp<-data.frame(paste(FD_path_SentenceStartStomach[,1],FD_CD3_StomachDescription_Description_Generalist,FD_CD5_StomachConclusion_Polyp[,1],FD_CD4_StomachConclusion2[,1]))
names(Stomach_Polyp)<-"report"

# f. bind everything together:
FD_path_Stomach<-as.list(rbind(Stomach_Inflam,Stomach_Dysplasia,Stomach_Polyp),stringsAsFactors=F)
FD_path_Stomach$report<-as.character(FD_path_Stomach$report)



##### 11. Bind the reports to the compartment specific endoscopy results #########





#If the WritePathReport column has "WritePathReport" and if fundus/body/antrum in the compartment then select a sample from  FD_path_Stomach
#and put it in a new column:


out2$PathReport<-apply(out2, 1, function(x) {
  ifelse(str_detect(x[["out"]],"WritePathReport") & grepl("fundus|antrum|body|stomach",x[["Compartment"]]),sample(FD_path_Stomach$report,1,replace=T),
         ifelse(str_detect(x[["out"]],"WritePathReport") & grepl("duod",x[["Compartment"]]),sample(Final_path_Duodenum$report,1),
                ifelse(str_detect(x[["out"]],"WritePathReport")  & grepl("Columnar|Barrett",x[["Compartment"]]),sample(FD_path_Barr$report,1),
                       ifelse(str_detect(x[["out"]],"WritePathReport") & grepl("GOJ|oesophag",x[["Compartment"]]),sample(FD_path_Oesoph$report,1),
                              ""))))
  })

out2$PathReport<-gsub("^\\d+$","",out2$PathReport)

out2$report <- ifelse(!grepl("NA", out2$WritePathReport), gsub(".*", "", out2$report), out2$report)
out2$report<-gsub("BIOPSIES TAKEN.*","biopsy specimens",out2$report)
out2$PathReport<-paste(out2$report," ",out2$PathReport,"")

#Start cleaning up the columns:
out2$report<-NULL
out2$Compartment<-NULL
out2$WritePathReport<-NULL





#further tidy up when biopsies not taken
Indications<-data.frame(Indic)
names(out2)<-c("report","path")

out3<-bulker("GOJ|fundus|oesophag|stomach body|duodenal bulb|antrum|second part of the duodenum|third part of the duodenum",out2)
out3<-listtodf(out3)

#Remove all the compartment subheadings:
out3$out<-gsub("COMPARTMENT_START\\s*\\w+\\s*.*?\\s*COMPARTMENT_END\\n?","",out3$out,perl = TRUE)


############################################################## 12. Recommendations #######################################################################################


#If there are any of the FD_Object in findings, then add the appropriate recommendations in to the text:
#if the word exists then add recommendation from a list:




addSampleText <- function(out3, keyword, sampleList,heading) {
  for (i in 1:nrow(out3)) {
    if (grepl(keyword, tolower(out3[i, "out"]))) {
      out3[i, "out"] <- paste0(out3[i, "out"],heading, sample(sampleList, 1))
    }
  }
  return(out3)
}
sampleDescriptorList_ulcer<-ListContstructor("FD_Ulcer_Recommendations","FD_Nodule_Recommendations",LOCATION_BIOPSES)
sampleDescriptorList_nodule<-ListContstructor("FD_Nodule_Recommendations","FD_Stricture_Recommendations",LOCATION_BIOPSES,)
sampleDescriptorList_stricture<-ListContstructor("FD_Stricture_Recommendations","FD_Inflammation_Recommendations",LOCATION_BIOPSES,)
sampleDescriptorList_inflammation<-ListContstructor("FD_Inflammation_Recommendations","FD_Polyp_Recommendations",LOCATION_BIOPSES,)
sampleDescriptorList_polyp<-ListContstructor("FD_Polyp_Recommendations","FD_Stricture_FollowUp",LOCATION_BIOPSES,)

out3 <- addSampleText(out3, "ulcer", sampleDescriptorList_ulcer, " RECOMMENDATION: ")
out3 <- addSampleText(out3, "nodule", sampleDescriptorList_nodule, " RECOMMENDATION: ")
out3 <- addSampleText(out3, "inflammation", sampleDescriptorList_stricture, " RECOMMENDATION: ")
out3 <- addSampleText(out3, "inflammation", sampleDescriptorList_inflammation, " RECOMMENDATION: ")
out3 <- addSampleText(out3, "polyp", sampleDescriptorList_polyp, " RECOMMENDATION: ")

############################################################## 12. Follow-up #######################################################################################
sampleDescriptorList_stricture<-ListContstructor("FD_Stricture_FollowUp","FD_Nodule_FollowUp",LOCATION_BIOPSES)
sampleDescriptorList_nodule<-ListContstructor("FD_Nodule_FollowUp","FD_Inflammation_FollowUp",LOCATION_BIOPSES)
sampleDescriptorList_inflammation<-ListContstructor("FD_Inflammation_FollowUp","FD_Polyp_FollowUp",LOCATION_BIOPSES)
sampleDescriptorList_polyp<-ListContstructor("FD_Polyp_FollowUp","FD_Ulcer_FollowUp",LOCATION_BIOPSES)
sampleDescriptorList_ulcer<-ListContstructor("FD_Ulcer_FollowUp","END",LOCATION_BIOPSES)




# Example usage
# Create a sample data frame
# Add sample text to rows containing certain words
out3 <- addSampleText(out3, "ulcer", sampleDescriptorList_ulcer," FOLLOW UP: ")
out3 <- addSampleText(out3, "nodule", sampleDescriptorList_nodule," FOLLOW UP: ")
out3 <- addSampleText(out3, "inflammation", sampleDescriptorList_stricture," FOLLOW UP: ")
out3 <- addSampleText(out3, "inflammation", sampleDescriptorList_inflammation," FOLLOW UP: ")
out3 <- addSampleText(out3, "polyp", sampleDescriptorList_polyp," FOLLOW UP: ")

###################################### 15. Top part of the endoscopy report ################################################


samplenumber <- 1000
HospitalNumberID <- paste("Hospital Number: ", sample(c(LETTERS)),
                          sample(1e+06:9999999, (samplenumber - 900), replace = T),
                          sep = ""
)
NHS_Trust <- replicate(samplenumber, c("Hospital: Random NHS Foundation Trust"))
Patient_Name <- paste("Patient Name: ", randomNames::randomNames(
  samplenumber,
  "first", "last"
))
Date_of_Birth <- paste("DOB: ", generator::r_date_of_births(samplenumber,
                                                            start = as.Date("1900-01-01"), end = as.Date("1999-01-01")
))
GeneralPractictioner <- paste("General Practitioner: Dr. ",
                              randomNames::randomNames(samplenumber, "first", "last"),
                              sep = ""
)
Date_of_ProcedureAll <- generator::r_date_of_births(samplenumber,
                                                    start = as.Date("2001-01-01"), end = as.Date("2017-01-01")
)

Date_of_Procedure <- Date_of_ProcedureAll
Date <- paste("Date of procedure: ", Date_of_Procedure)
EndoscopistList <- as.list(sample(randomNames::randomNames(
  samplenumber,
  "first", "last"
), 10, replace = T))
Second_EndoscopistList <- as.list(sample(randomNames::randomNames(
  samplenumber,
  "first", "last"
), 10, replace = T))
Endoscopist <- replicate(samplenumber, paste("Endoscopist: Dr. ",
                                             sample(EndoscopistList, 1, replace = F),
                                             sep = ""
))
Second_Endoscopist <- replicate(samplenumber, paste("2nd Endoscopist: Dr. ",
                                                    sample(Second_EndoscopistList, 1, replace = F),
                                                    sep = ""
))
MedicationsFent <- replicate(samplenumber, paste(
  "Medications: Fentanyl ",
  sample(list(
    x = "12.5mcg", x = "25mcg", x = "50mcg",
    x = "75mcg", x = "100mcg", x = "125mcg",
    x = "150mcg"
  ), 1, replace = F)
))
MedicationsMidaz <- replicate(samplenumber, paste(
  "Midazolam ",
  sample(list(
    x = "1mg", x = "2mg", x = "3mg",
    x = "4mg", x = "5mg", x = "6mg", x = "7mg"
  ),
  1,
  replace = F
  )
))
Instrument <- replicate(samplenumber, paste(
  "Instrument: ",
  sample(list(
    x = "FG1", x = "FG2", x = "FG3",
    x = "FG4", x = "FG5", x = "FG6", x = "FG7"
  ),
  1,
  replace = F
  )
))
Extent_of_Exam <- replicate(samplenumber, paste(
  "Extent of Exam: ",
  sample(list(
    x = "Failed intubation", x = "Oesophagus",
    x = "Stomach body", x = "D1", x = "D2",
    x = "Pylorus", x = "GOJ"
  ), 1, replace = F)
))

PROCEDURE_PERFORMED <- "Procedure Performed: Gastroscopy (OGD)"




########################################## 14. Follow up #################################################
#How to make this compartment specific
#########################################################################################################


out3$out<-paste0(HospitalNumberID," , ",NHS_Trust," , ",Date_of_Birth," , ",GeneralPractictioner,"  , ",Date, Endoscopist,
                 "  , ",Second_Endoscopist,"  , ", MedicationsFent,"  , ", MedicationsMidaz,"  , ",
                 Instrument,"  , ", Extent_of_Exam,"  , ",PROCEDURE_PERFORMED,"  , ",out3$out)
############################################################################################################################
############################################################################################################################
############################################################################################################################

#A little bit post post production clean up:

  out3$out<-gsub("(?i)(FOLLOW UP:)(?!.*\\1)", "",out3$out,perl=T)
  out3$out<-gsub("(?i)(RECOMMENDATION:)(?!.*\\1)", "",out3$out,perl=T)


#To do:
#Sort out the bulker so it makes sense.
#Top and tail the pathology reports
#Check sense of pathology reports and correct lists
  #Decide to tailor lists for pathology eg to get cancer lists more meaningful and also to get the Description_lists unified and mutually exclusive
#Tidy up the data.
#Start use cases and how to make it downloadable.
#Do the documentation.

#Now you have to tie the pathology reports to the endoscopy




#To do:
#Create indications for the endoscopies from the endocsopy reports from STH
#Start creating the pathology reports
#NATURE OF SPECIMEN: Derived from the section about whether biopsies taken

#Create the report:  Rule: If biopsies taken and not negative then use create pathology reports:

# Rule: If number of biopsies mentioned in endoscopic findings then use this otherwise make up
# Rule: The biopsy compartment is derived from the sentence

#CLINICAL DETAILS: Derived from combination of the indication and the endoscopic findings text
#HISTOLOGY Derived from the compartment that the biopsies were taken from
#MACROSCOPICAL DESCRIPTION Derived from the Nature of specimen with sizes added
#DIAGNOSIS Derived from the Histology section.
