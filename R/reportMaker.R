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
#' @examples FD_OesophagitisIntro<-list(x="severe oesophagitis with ulceration.")
#' FD_OesophagitisIntro<-ListReplicator("LA Grade ",sample(c("A", "B", "C", "D")),"oesophagitis",FD_OesophagitisIntro)



ListReplicator<-function(stringToStart,sampleList,stringToEnd,storeList){
  repd <- replicate(4, paste(stringToStart, sampleList,stringToEnd))
  names(repd) <- rep("x", length(repd))
  long <- append(storeList, as.list(repd))
  storeList <- long[!duplicated(long)]
  names(storeList)
  return(storeList)
}

#' flag
#'
#' @param inputString
#' @param outputString
#' @param proportion
#' @param mydf the dataframe
#' @keywords
#' @export
#' @examples flag("Normal gastroscopy to the duodenum.","Hiatus",8)

flag<-function(inputString,outputString,proportion,mydf){
  mydf[sample(which(mydf[,1]==inputString), sum(mydf[,1]==inputString)/proportion), 1] <- outputString
  return(mydf)
}

#'Report bulker
#'
#' Report bulker- looks at what is in the report, then adds another row's report to the current to bulk it up.
#' It specifically doesn't add reports that mention the same compartment to avoid contradictions
#' @param inputString
#' @keywords cats
#' @export
#' @examples
#' bulker()

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
  out<-listtodf(out)
}

#'Internally relevant detail adder - interpolate
#'
#' This adds the detail to a report based on the input string (ie based on the flag)
#' @param inputString
#' @param listtoSample
#' @keywords cats
#' @export
#' @examples IntRanOneElement1()

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
  out<-apply(out, 1, function(x) {
    gsub(phraseToReplace,  paste0(x,sample(listOfReplacements,1,replace=F)),x)
  })
  out<-listtodf(out)
}
