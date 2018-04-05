library(FakeEndoReports)

####All the generated lists for the FakeEndoReports dataset

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

INDICATIONS <- readLines("/home/rstudio/FakeEndoReports/data/OGDIndication")
INDICATIONS<-gsub("\"","",INDICATIONS)
names(INDICATIONS) <- rep("x", length(INDICATIONS))
INDICATIONS <- as.list(INDICATIONS)
FD_Indication<-ListContstructor("START","END",INDICATIONS)


#Create the compartment list components then if that compartment present then create the path report for that compartment

##### Pathology lists- duodenum #####
#Import the list of phrases for sentence construction
PATH_DUODENUM <- readLines("/home/rstudio/FakeEndoReports/data/Path_Duodenum")
PATH_DUODENUM<-gsub("\"","",PATH_DUODENUM)
names(PATH_DUODENUM) <- rep("x", length(PATH_DUODENUM))
PATH_DUODENUM <- as.list(PATH_DUODENUM)

FD_DuodenumConclusion<-ListContstructor("Conclusion","Conclusion_CD",PATH_DUODENUM)
FD_DuodenumConclusion_CD<-ListContstructor("Conclusion_CD","Conclusion_Normal",PATH_DUODENUM)
FD_DuodenumConclusion_Normal<-ListContstructor("Conclusion_Normal","Conclusion2",PATH_DUODENUM)
FD_DuodenumConclusion2<-ListContstructor("Conclusion2","Description",PATH_DUODENUM)

FD_DuodenumDescription<-ListContstructor("Description","Description_IELs",PATH_DUODENUM)
FD_DuodenumDescription_IELs<-ListContstructor("Description_IELs","Description_Inflammation",PATH_DUODENUM)
FD_DuodenumDescription_Inflammation<-ListContstructor("Description_Inflammation","Description_List",PATH_DUODENUM)
FD_DuodenumDescription_List<-tolower(ListContstructor("Description_List","Description_Normal",PATH_DUODENUM))
FD_DuodenumDescription_Normal<-ListContstructor("Description_Normal","Description_Ratio",PATH_DUODENUM)
FD_DuodenumDescription_Ratio<-ListContstructor("Description_Ratio","Description_VA",PATH_DUODENUM)
FD_DuodenumDescription_VA<-ListContstructor("Description_VA","Macroscopic_Description",PATH_DUODENUM)

FD_DuodenumMacroscopic_Description<-ListContstructor("Macroscopic_Description","END",PATH_DUODENUM)


#####Pathology lists - Barrett's oesophagus#####
#Import the list of phrases for sentence construction
PATH_BARRETTS <- readLines("/home/rstudio/FakeEndoReports/data/Path_Barretts")
PATH_BARRETTS<-gsub("\"","",PATH_BARRETTS)
names(PATH_BARRETTS) <- rep("x", length(PATH_BARRETTS))
PATH_BARRETTS <- as.list(PATH_BARRETTS)

FD_BarrettsMacroscopic_Description<-ListContstructor("Macroscopic_Description","Description_OAC",PATH_BARRETTS)

FD_BarrettsDescription_OAC<-ListContstructor("Description_OAC","Description_List",PATH_BARRETTS)
FD_BarrettsDescription_List<-ListContstructor("Description_List","Description_D",PATH_BARRETTS)
FD_BarrettsDescription_D<-ListContstructor("Description_D","Description",PATH_BARRETTS)
FD_BarrettsDescription<-ListContstructor("Description","Conclusion2",PATH_BARRETTS)

FD_BarrettsConclusion2<-ListContstructor("Conclusion2","Conclusion_OAC",PATH_BARRETTS)
FD_BarrettsConclusion_OAC<-ListContstructor("Conclusion_OAC","Conclusion_Inflamm",PATH_BARRETTS)
FD_BarrettsConclusion_Inflamm<-tolower(ListContstructor("Conclusion_Inflamm","Conclusion_D",PATH_BARRETTS))
FD_BarrettsConclusion_D<-ListContstructor("Conclusion_D","END",PATH_BARRETTS)

#####Pathology lists -  oesophagus (non-Barrett's) #####
#Import the list of phrases for sentence construction
PATH_OESOPHAGUS <- readLines("/home/rstudio/FakeEndoReports/data/Path_Oeosophagus")
PATH_OESOPHAGUS<-gsub("\"","",PATH_OESOPHAGUS)
names(PATH_OESOPHAGUS) <- rep("x", length(PATH_OESOPHAGUS))
PATH_OESOPHAGUS <- as.list(PATH_OESOPHAGUS)

FD_OesophagusMacroscopic_Description<-ListContstructor("Macroscopic_Description","Description_List",PATH_OESOPHAGUS)

FD_OesophagusDescription_List<-ListContstructor("Description_List","Description_Inflammation",PATH_OESOPHAGUS)
FD_OesophagusDescription_Inflammation<-ListContstructor("Description_Inflammation","Description_EoE",PATH_OESOPHAGUS)
FD_OesophagusDescription_EoE<-ListContstructor("Description_EoE","Description_Dysplasia",PATH_OESOPHAGUS)
FD_OesophagusDescription_Dysplasia<-ListContstructor("Description_Dysplasia","Conclusion2",PATH_OESOPHAGUS)

FD_OesophagusConclusion2<-ListContstructor("Conclusion2","Conclusion_Normal",PATH_OESOPHAGUS)
FD_OesophagusConclusion_Normal<-ListContstructor("Conclusion_Normal","Conclusion_Inflammation",PATH_OESOPHAGUS)
FD_OesophagusConclusion_Inflammation<-ListContstructor("Conclusion_Inflammation","Conclusion_EoE",PATH_OESOPHAGUS)
FD_OesophagusConclusion_EoE<-ListContstructor("Conclusion_EoE","Conclusion_Dysplasia",PATH_OESOPHAGUS)
FD_OesophagusConclusion_Dysplasia<-ListContstructor("Conclusion_Dysplasia","END",PATH_OESOPHAGUS)


#####Pathology lists -  stomach #####
#Import the list of phrases for sentence construction
PATH_STOMACH <- readLines("/home/rstudio/FakeEndoReports/data/Path_Stomach")
PATH_STOMACH<-gsub("\"","",PATH_STOMACH)
names(PATH_STOMACH) <- rep("x", length(PATH_STOMACH))
PATH_STOMACH <- as.list(PATH_STOMACH)
FD_StomachMacroscopic_Description<-ListContstructor("Macroscopic_Description","Description_Inflammation",PATH_OESOPHAGUS)

FD_StomachDescription_Inflammation<-ListContstructor("Description_Inflammation","Description_General",PATH_OESOPHAGUS)
FD_StomachDescription_Description_General<-ListContstructor("Description_General","Description_Dysplasia",PATH_OESOPHAGUS)
FD_StomachDescription_Dysplasia<-ListContstructor("Description_Dysplasia","Conclusion2",PATH_OESOPHAGUS)

FD_StomachConclusion2<-ListContstructor("Conclusion2","Conclusion_Polyp",PATH_OESOPHAGUS)
FD_StomachConclusion_Polyp<-ListContstructor("Conclusion_Polyp","Conclusion_Inflammation",PATH_OESOPHAGUS)
FD_StomachConclusion_Inflammation<-ListContstructor("Conclusion_Inflammation","Conclusion_Cancer",PATH_OESOPHAGUS)
FD_StomachConclusion_Cancer<-ListContstructor("Conclusion_Cancer","END",PATH_OESOPHAGUS)

