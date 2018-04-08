#' listtodf
#'
#' Converts the list to a dataframe
#' @param thelist
#' @keywords list conversion
#' @export
#' @examples

listtodf<-function(thelist){
  thelist<-data.frame(unlist(thelist))
  names(thelist)<-c("out")
  thelist[,1]<-as.character(thelist[,1])
  return(thelist)
}

#' ListReplicator
#'
#' This creates a list with sampling so you can generate for example a list
#' with the same sentence but different lengths of things
#' @param stringToStart
#' @param sampleList
#' @param stringToEnd
#' @param storeList
#' @keywords
#' @export
#' @examples FD_OesophagitisIntro<-list(Sx="severe oesophagitis with ulceration.")
#' FD_OesophagitisIntro<-ListReplicator("LA Grade ",sample(c("A", "B", "C", "D")),"oesophagitis",FD_OesophagitisIntro)



ListReplicator<-function(stringToStart,sampleList,stringToEnd,storeList){
  repd <- replicate(4, paste(stringToStart, sampleList,stringToEnd))
  names(repd) <- rep("x", length(repd))
  long <- append(storeList, as.list(repd))
  storeList <- long[!duplicated(long)]
  names(storeList)
  return(storeList)
}

#' ListContstructor
#'
#' @param string1 the start delimiter
#' @param string2 the end delimiter
#' @param textfile imported text file
#' @keywords
#' @export
#' @examples #Import the text file first
#' FD_PolypDescriptors<-ListContstructor("FD_PolypDescriptors","FD_UlcerDescriptors",FINDINGS)

ListContstructor<-function(string1,string2,textfile){
  #Add all the extra hyphens in that make the text file more readable
  conStart<-paste0(paste0(replicate(65,"-"),collapse = '')
                   ,string1)
  conEnd<-paste0(paste0(replicate(65,"-"),collapse = '')
                 ,string2)
  #Find the delimiters by which is in the index
  start <- which(textfile==conStart)+1
  end <- which(textfile==conEnd)-1
  return(textfile[start:end])
}

#' flag
#'
#' @param inputString
#' @param outputString
#' @param proportion
#' @param sampleList the sample list for intro for that disease
#' @param mydf the dataframe
#' @keywords
#' @export
#' @examples flag("Normal gastroscopy to the duodenum.","Hiatus",8)

flag<-function(inputString,outputString,proportion,sampleList,mydf){
  mydf[sample(which(mydf[,1]==inputString), sum(mydf[,1]==inputString)/proportion), 1] <- outputString
  mydf<-IntRanOneElement1(outputString,sampleList,mydf)
  mydf<-listtodf(mydf)
  return(mydf)
}

#'Report bulker
#'
#' Report bulker- looks at what is in the report, then adds another row's report to the current to bulk it up.
#' It specifically doesn't add reports that mention the same compartment to avoid contradictions
#' @param inputString
#' @keywords cats
#' @export
#' @examples bulker()

bulker <- function(inputString,dataframeIn){
  apply(dataframeIn, 1, function(x) {
    #If you get a match for the following
    if (stringr::str_detect(x, inputString)) {
      #Then store that match
      mymatch<-stringr::str_match(x, inputString)
      #Then use the match to select any report that does not have that match in it to avoid conflicting results
      t<-data.frame(dataframeIn[!grepl(mymatch,dataframeIn$out),])
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

#'Internally relevant detail adder - interpolate
#'
#' This adds the detail to a report based on the input string (ie based on the flag)
#' @param inputString
#' @param listtoSample
#' @param mydf the dataframe
#' @keywords cats
#' @export
#' @examples IntRanOneElement1()

IntRanOneElement1 <- function(inputString,listtoSample,mydf){
  apply(mydf, 1, function(x) {
    if (stringr::str_detect(x, inputString)) {
      ret<-paste0(sample(listtoSample,1,replace=T),".")
      return(ret)
    }
    else {
      return(x)
    }
  })
}

#'Internally relevant detail adder with extra phrase- interpolate
#'
#' This function allows you to express your love of cats.
#' @param inputString
#' @param listtoSample
#' @param addedPhrase1
#' @param addedPhrase2
#' @keywords
#' @export
#' @examples IntRanOneElementWithPhrase()

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


#'IntRanNumber
#'Always interpolate- random number of elements from a list
#'
#'
#' This function allows you to express your love of cats.
#' @param inputString
#' @param listtoSample
#' @keywords cats
#' @export
#' @examples IntRanNumber("exudates|Pull|crepe|Rings|tight",FD_EosinophilicDetail)
IntRanNumber <- function(inputString,listtoSample){
  apply(out, 1, function(x) {
    if (stringr::str_detect(x, inputString)) {
      paste(sample(unlist(listtoSample), size = sample(1:3)), collapse =" ")
      return(x)
    }
    else {
      return(x)
    }
  })
  out<-listtodf(out)
}


#'IntRanMultipleElements
#'
#' #Always interpolate- random number of elements from a list
#' @param inputString
#' @param listtoSample
#' @keywords interpolate from list
#' @export
#' @examples  IntRanMultipleElements("exudates|Pull|crepe|Rings|tight",FD_EosinophilicDetail)

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


#'RandomSingleGsub
#'
#' This function allows you to express your love of cats.
#' @param phraseToReplace
#' @param listOfReplacements
#' @keywords cats
#' @export
#' @examples RandomSingleGsub()
RandomSingleGsub<-function(phraseToReplace,listOfReplacements){
  apply(out, 1, function(x) {
    gsub(phraseToReplace,  paste0(x,sample(listOfReplacements,1,replace=F)),x)
  })
  out<-listtodf(out)
}


#'LocationAndBiopsy
#'
#' #ALocationAndBiopsy
#' @param
#' @param
#' @keywords
#' @export
#' @examples

LocationAndBiopsy<-function(x,regString1,listString1,listString2,source,biopsyListString1,biopsyListString2){
  if (stringr::str_detect(x, paste0(regString1))){
    #Get the list from the phrases text
    sampleDescriptorList<-ListContstructor(listString1,listString2,source)
    ff<-paste(x,sample(sampleDescriptorList,1,replace=F))
    #Get the biopsies list from the phrases text
    sampleBiopsyList<-ListContstructor(biopsyListString1,biopsyListString2,source)
    t<-paste0(ff,sample(sampleBiopsyList,1,replace=F),".")
    return(t)
  }else {
    return(x)
  }
}


#'ListNegsAndPos
#'
#' #ListNegsAndPos
#' @param
#' @param
#' @keywords
#' @export
#' @examples
#'
ListNegsAndPos<-function(dataframeIn){
  ifelse(length(ListCheck[sapply(ListCheck, function(x) any(grepl(x, dataframeIn$report)))])>0,
  lst <- ListCheck[sapply(ListCheck, function(x) any(grepl(x, dataframeIn$report)))],
  lst <- ListCheck)
  #Then to have a random number, use another sample for selecting the count per row:
  output<- sapply(1:nrow(dataframeIn), function(x) {
    paste(paste0("There is no evidence of ",unique(sample(lst, sample(1:3, 1), replace = TRUE), collapse = ", ")),collapse = '\n')
  })
  output<-as.character(output)
  return(output)
}
