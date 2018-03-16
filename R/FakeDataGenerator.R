library(stringr)
#Get the out to be dataframe again after function

listtodf<-function(thelist){
thelist<-data.frame(unlist(thelist))
names(thelist)<-c("out")
thelist[,1]<-as.character(thelist[,1])
return(thelist)
}


#Flag maker flag("Normal gastroscopy to the duodenum.","Hiatus",8)
flag<-function(inputString,outputString,proportion){
  out[sample(which(out[,1]==inputString), sum(out[,1]==inputString)/proportion), 1] <- outputString
  return(out)
}


#Report bulker- looks at what is in the report, then adds another row's report to the current to bulk it up.
#It specifically doesn't add reports that mention the same compartment to avoid contradictions
bulker <- function(inputString){
  apply(out, 1, function(x) {

  #If you get a match for the following
  if (stringr::str_detect(x, inputString)) {
    #Then store that match
    mymatch<-stringr::str_match(x, inputString)
    #Then use the match to select out any report that does not have that match in it to avoid conflicting results
    t<-data.frame(out[!grepl(mymatch,out$out),])
    # Get a random row:
    ret<-as.character(t[sample(1:nrow(t),1),])
    ret<-gsub("Normal gastroscopy to the duodenum.","",ret)
    # Now just need to paste the result into the row
    ret2<-paste0(x,"\n",ret)
    return(ret2)
  }

  else {
    return(x)
  }
})
}


#Internally relevant detail adder - interpolate
#This adds the detail to a report based on the input string (ie based on the flag)

IntRanOneElement1 <- function(inputString,listtoSample){
  out<-apply(out, 1, function(x) {
  if (stringr::str_detect(x, inputString)) {
   ret<-paste0(sample(listtoSample,1,replace=T),".")
   return(ret)
  }
  else {
    return(x)
  }
    out<-listtodf(out)
    })
  out<-listtodf(out)
}

#Always interpolate- random 1 element from a list
#This adds an extra phrase from a list based on flag- check to see if IntRanOneElement1 already does this

IntRanOneElement<-function(inputString,listtoSample){
  out$out[out$out == inputString] <- sample(listtoSample, sum(out$out == inputString), TRUE)
  listtodf(out)
  return(out)
}


#Internally relevant detail adder with extra phrase- interpolate

IntRanOneElementWithPhrase <- function(inputString,listtoSample,addedPhrase1,addedPhrase2){
  out<- apply(out, 1, function(x) {
    if (stringr::str_detect(x, inputString)) {
      ret<-paste0(x,addedPhrase1,addedPhrase2,sample(listtoSample,1,replace=T))
      return(ret)
    }
    else {
      return(x)
    }

  })
  out<-listtodf(out)
}




#Always interpolate- random number of elements from a list
#Example:  IntRanMultipleElements("exudates|Pull|crepe|Rings|tight",FD_EosinophilicDetail)

IntRanMultipleElements <- function(inputString,listtoSample){
  out<-apply(out, 1, function(x) {
    if (stringr::str_detect(x, inputString)) {
      y<-paste(sample(unlist(listtoSample), size = sample(1:3)), collapse =" ")
x<-paste(x,y)
      return(x)
    }
    else {
      return(x)
    }

  })
  out<-listtodf(out)
}


RandomSingleGsub<-function(phraseToReplace,listOfReplacements){
  out<-apply(out, 1, function(x) {
    x<-gsub(phraseToReplace,  paste0(sample(listOfReplacements,1,replace=F)),x)
  })
  out<-listtodf(out)
}

##########################################################################################################################################################################










################################################################ LISTS- Sentence introduction #####################################################################################


##### LISTS- Sentence introduction #####
FD_SentenceIntro<-list(x=" ",x="Normal gastroscopy to the duodenum.")
FD_SentenceIntro1<-list(x="The patient has ",x="There is ")

##### LISTS- Basic descriptor lists #####
FD_Object<-list(x="a stricture",x="an ulcer",x="inflammation",x="a nodule",x="a polyp")
FD_Location<-list(x="oesophagus at (cm)",x="GOJ",x="fundus",x="stomach body",x=" antrum",x="duodenal bulb",x="D1/D2 angle",x="second part of the duodenum",x="third part of the duodenum")

##### LISTS- Macroscopic findings #####
FD_PolypDescriptors<-list(x="stalked.",x="sessile.")
FD_UlcerDescriptors<-list(x="excavated.",x="deep.",x="superficial.")
FD_StrictureDescriptors<-list(x="circumferential.",x="fibrous.",x="tight.",x="friable")
FD_InflammationDescriptors<-list(x="erosive.",x="severe.",x="mild.")
FD_NoduleDescriptors<-list(x="stalked.",x="sessile.","benign-looking.")

##### LISTS- Secondary embellishments #####
FD_UlcerDescSecond<-list(x="",x="It is bleeding",x="It is not bleeding",x="It has rolled edges",x="It has a visible vessel",x="CLO test was taken")
FD_PolypDescSecond<-list(x="",x="It has a normal pit pattern",x="It looks hyperplastic",x="It looks adenomatous")
FD_StrictureDescSecond<-list(x="",x="It will not allow the scope to pass",x="It is easily traversible")

##### LISTS- Disease specific #####

#Need to slot these in to a subset of normals and also to add to some of the other reports too
FD_BarrettIntro<-list(x="The patient has Barrett's oesophagus",x="Columnar lined oesophagus is present",x="Barrett's is present")
FD_BarrettDetail1<-list(x="The segment looks flat.",
                        x="No nodularity is present.",
                        x="It is a long segment.",
                        x="Some areas of vascular abnormalities are seen.",
                        x="The pit pattern is normal.",
                        x="No abnormal pit pattern is seen.",
                        x="The segment looked strictured",
                        x="Oesophagitis was present.",
                        x="It is x cm in length.",
                        x="No loss of aceto-whitening was seen.",
                        x="Subtle nodularity noted near the GOJ.",
                        x="Gastric mucosal prolapse seen.",
                        x="This is an ultra-short segment.",
                        x="Short segment only.",
                        x="Ulceration seen at x cm.")
FD_BarrettDetail2<-list(x="FD_BarrettDetail2 1",
                        x="FD_BarrettDetail2 2.",
                        x="FD_BarrettDetail2 3.",
                        x="FD_BarrettDetail2 4.",
                        x="FD_BarrettDetail2 5.",
                        x="FD_BarrettDetail2 6.",
                        x="FD_BarrettDetail2 7.",
                        x="FD_BarrettDetail2 8.",
                        x="FD_BarrettDetail2 9.")
FD_BarrettDetail1<-list(x="FD_BarrettDetail1 1",
                        x="FD_BarrettDetail1 2.",
                        x="FD_BarrettDetail1 3.",
                        x="FD_BarrettDetail1 4.",
                        x="FD_BarrettDetail1 5.",
                        x="FD_BarrettDetail1 6.",
                        x="FD_BarrettDetail1 7.",
                        x="FD_BarrettDetail1 8.",
                        x="FD_BarrettDetail1 9.")


FD_HiatusIntro<-list(x="A large hiatus hernia is present",
                     x="Small hiatus hernia only",
                     x="Large sliding hiatus hernia",
                     x="Likely paraoesophageal hernia present")
FD_HiatusDetail<-list(x="HiatusDetail1",
                      x="HiatusDetail2y",
                      x="HiatusDetail3",
                      x="HiatusDetail14")

FD_OesophagitisIntro<-list(x="LA Grade A oesophagitis noted.",
                           x="LA Grade B oesophagitis noted.",
                           x="LA Grade C oesophagitis noted.",
                           x="LA Grade D oesophagitis noted.",
                           x="severe oesophagitis with ulceration.")
FD_OesophagitisDetail<-list(x="Food present in the oesophagus",
                            x="Gastric mucosal prolapse also seen",
                            x="The lower oesopahgeal sphincter looks widely patent",
                            x="The oesopahgitis is consistent with acid reflux.",
                            x="severe oesophagitis with ulceration.")

FD_InletIntro<-list(x="Inlet patch seen on withdrawal.",
                    x="At 18cm two inlet patches were seen.",
                    x="No evidence of inlet patch on careful inspection.")

FD_InletDetail<-list(x="PatchDetails1",
                     x="PatchDetails2.",
                     x="PatchDetails3.")

FD_EosinophilicIntro<-list(x="Linear furrowing is present",
                           x="fSeveral white exudates were seen",
                           x="Pull sign was demonstrated.",
                           x="The patient had crepe paper oesophagitis",
                           x="Rings were seen throughout the oesophagus",
                           x="The lumen was tight")

FD_EosinophilicDetail<-list(x="EoEBoo1",
                            x="EoEBoo2",
                            x="EoEBoo3",
                            x="EoEBo4",
                            x="EoEBoo5",
                            x="EoEBo06")

FD_Dilatation<-c("The stricture will need to be dilatated in radiology",
                 "dilated to 12mm with a CRE balloon",
                 "dilated to 15mm with a CRE balloon",
                 "dilated to 18mm with a CRE balloon",
                 "dilated to 20mm with a CRE balloon",
                 "A superficial mucosal tear was seen after dilatation",
                 "A deep mucosal tear was seen after dilatation")

FD_DilatationDetails<-c("A superficial mucosal tear was seen after dilatation",
                        "A deep mucosal tear was seen after dilatation")

##### LISTS- Continuity replacements random gsub  #####
FD_ContinuityAdditionals<-c("\nThe patient also" ,"\nIn addition the patient also")

##########################################################################################################################################################################









########################################################### 1. Sentence creation #######################################################################################
##### 1. Sentence creation #####
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
out<-flag("Normal gastroscopy to the duodenum.","Barretts",8)
#2. Vary & detail
#Flag dispersal-This uses the Barretts flag and then randomly adds variation to that
out<-IntRanOneElement1("Barretts",FD_BarrettIntro)
out<-listtodf(out)
out<-IntRanMultipleElements("Barrett|Columnar lined",FD_BarrettDetail1)
out<-listtodf(out)

##### 4b. Minority replacement flag Hiatus hernia  #####
#1. Scatter
out<-flag("Normal gastroscopy to the duodenum.","Hiatus",8)
#2. Vary: Flag dispersal-This uses the Barretts flag and then randomly adds variation to that &Detail
out<-IntRanOneElement1("Hiatus",FD_HiatusIntro)
out<-listtodf(out)
out<-IntRanMultipleElements("hernia",FD_HiatusDetail)
out<-listtodf(out)

##### 4c. Minority replacement flag Oesophagitis  #####
#1. Scatter
out<-flag("Normal gastroscopy to the duodenum.","Oesophagitis",8)
#2. Vary and Detail
out<-IntRanOneElement1("Oesophagitis",FD_OesophagitisIntro)
out<-listtodf(out)
out<-IntRanMultipleElements("oesophagitis",FD_OesophagitisDetail)
out<-listtodf(out)

##### 4d. Minority replacement flagInlet patch  #####
#1. Scatter
out<-flag("Normal gastroscopy to the duodenum.","Inlet",8)
#2. Vary and Detail
out<-IntRanOneElement1("Inlet",FD_InletIntro)
out<-listtodf(out)
out<-IntRanMultipleElements("nlet patch",FD_InletDetail)
out<-listtodf(out)

##### 4e. Minority replacement flag Eosinophilic  #####
#1. Scatter
out<-flag("Normal gastroscopy to the duodenum.","Eosinophilic",8)
#2. Vary and Detail
out<-IntRanOneElement1("Eosinophilic",FD_EosinophilicIntro)
out<-listtodf(out)
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










###################################################################### 8. Biopsy functions ##################################################################################


##### 8. Biopsy functions #####

#Pick the segment that you want to biopsy then vary the number of biopsies taken from each. Also vary the phrase.
#if not Normal endoscopy #Biopsies were taken. #Biopsies were taken from the <segment>
