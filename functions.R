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

ClumpFinding <- function(text,k,L,t){ 
  lseq = nchar(text) 
  interval = substring(text,seq(1,lseq-L+1),seq(L,lseq)) 
  pattint = lapply(interval,function(x) substring(x,seq(1,L-k+1),seq(k,L))) 
  upattint = lapply(pattint,function(x) unique(x))
  intcount = lapply(pattint,function(x) unlist(lapply(unique(x),function(y) sum(stringr::str_count(y,paste0("^",x,"$")))))) 
  count = lapply(intcount,function(x) which(x>=t)) 
  clump = unique(unlist(mapply(function(x,y) x[y], x= pattint,y=count)))
  return(noquote(clump))
}

ClumpFindingSW <- function(text,k,L,t){ 
  lseq = nchar(text) 
  single = unlist(strsplit(text,""))
  pint = list()
  interval = substring(text,1,L) 
  pattint = substring(interval,seq(1,L-k+1),seq(k,L))
  nocc = unlist(lapply(unique(pattint),function(x) sum(stringr::str_count(pattint,paste0("^",x,"$")))))
  patnfreq = list(kmer=pattint,freq=nocc)
  pint[[1]] = pattint 
  for (i in 2:lseq){
    newpat = paste(c(unlist(strsplit(tail(pint[[i-1]],1),""))[-1],single[i+L-1]),collapse="")
    pint[[i]] = c(pint[[i-1]][-1],newpat)
    if (!newpat %in% pattint) {
      pattint = c(pattint,newpat)
      nocc = c(nocc,1)
      patnfreq$kmer = pattint
      patnfreq$freq = nocc
    }  
    else{
      patnfreq$kmer = pattint
      pos = which(pattint==newpat)
      nocc[pos] = nocc[pos] + 1
      patnfreq$freq = nocc
    }
  }
  clump = patnfreq$kmer[which(patnfreq$freq>=t)]
  return(noquote(clump))
}