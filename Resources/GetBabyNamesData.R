library(RCurl)

url.pre="https://www.retraitequebec.gouv.qc.ca/en/services-en-ligne-outils/banque-de-prenoms/Pages/recherche_par_popularite.aspx?AnRefBp="

url.post = "&NbPre=0&ChAff=PPMP&NbPrePag=500"

ds = NULL

for( Year in 2013:2018) {
	
   url = paste(url.pre,toString(Year),url.post,sep="")

   TXT = getURL(url)
   txt=strsplit(TXT,"\n")[[1]]; length(txt)

  txt[grep("Next",txt)]


   extract.names = function(gender) {
     target = paste("SexRec=",gender,"&amp;PreRec=",sep="")
     I = grep(target,txt)
     N = unlist(strsplit(txt[I],target))
     N = N[seq(2,length(N),2)] 
     N = unlist(strsplit(N,">"))
     N = N[seq(2,length(N),3)]   
     N = unlist(strsplit(N,"<"))
     N = N[seq(1,length(N),2)]
     L = nchar(N) 
     Frequency = txt[I+2]
     k = nchar( Frequency )
     Frequency = as.numeric(substr(Frequency,1,k-1))
     return( list(Name=N, Frequency = Frequency,Length=L) )
   }
   
  f = data.frame( extract.names("F"), stringsAsFactors=FALSE ) ; 
  m = data.frame( extract.names("M"), stringsAsFactors=FALSE ) ; 
  f$Year = Year ; f$Gender="F" ; f$Rank=1:500
  m$Year = Year ; m$Gender="M" ; m$Rank=1:500

  ds=rbind(ds,f)
  ds=rbind(ds,m)

} # year

setwd("/Users/jameshanley/git_repos/statbook/Resources")
write.table(ds,"QuebecBabyNames2013to2018.txt")
