library(stringr)
PatternCount <- function(text,pattern){
  lseq = nchar(text)
  lmer = nchar(pattern)
  pairs = substring(text,seq(1,lseq-lmer+1),seq(lmer,lseq))
  index = which(pairs %in% pattern)
  return(length(index-1))                   
}

FrequentWords <- function(text,k){
  lseq = nchar(text)
  pairs = substring(text,seq(1,lseq-k+1),seq(k,lseq))
  upairs = unique(pairs)
  nocc = unlist(lapply(upairs,function(x) sum(stringr::str_count(pairs,paste0("^",x,"$")))))
  count = max(nocc)
  freqmer = upairs[which(nocc == max(nocc))]
  return(noquote(sort(freqmer)))
}

RC <- function(text){
  comp = unlist(lapply(unlist(strsplit(text,"")),function(x){if(x=="A") new="T" 
  if(x=="C") new="G" 
  if(x=="G") new="C" 
  if(x=="T") new="A" 
  return(new)}))
  rcsplit = rev(comp)
  rc = paste(rcsplit,collapse="")  
  return(noquote(rc))
}

PatternMatch <- function(pattern,text){
  lseq = nchar(text)
  k = nchar(pattern)
  pairs = substring(text,seq(1,lseq-k+1),seq(k,lseq))
  index = which(pairs %in% pattern)
  return(index-1)
}