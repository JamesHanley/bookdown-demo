
setwd("/Users/jameshanley/Dropbox/Courses/statbook/")
d1 = read.table("First31.txt",as.is=TRUE)
d2 = read.table("Last3352.txt",as.is=TRUE)
d = rbind(d1,d2)
str(d)
summary(d)
head(d)


ds=read.csv("veh0126.csv",as.is=TRUE,skip=6)
head(ds[,1:10],6)
tail(ds[,1:10],15)
first = function(x) strsplit(x," ")[[1]][1]
ds$Main = unlist( lapply(ds$Make,first) )
sum(ds$Total)
fr = aggregate(ds$Total,by=list(Main=ds$Main),sum) ; str(fr)
TOP = fr$Main[fr$x>10000]

ds = ds[ds$Main %in% TOP,c(1,2,125,126)]
str(ds)

Fr = aggregate(ds$Total,by=list(Main=ds$Generic.model.1),sum) ; 
Fr = Fr[order(-Fr$x),] ; str(Fr)
Fr = Fr[ Fr$x >= 1000, ] ; head(Fr)

Fr = Fr[ order(Fr$Main), ] ;
Fr$Main = gsub("VAUXHALL", "OPEL", Fr$Main)
 
head(Fr)
head(d)

Fr$Length = NA

for(i in 1:length(Fr$Main)){
	
	hits = grep(Fr$Main[i],d$Model,ignore.case=TRUE)
	if(length(hits) > 0 ) Fr$Length[i] = round(median(d$Length[hits]))	
}
Fr = Fr[!is.na(Fr$Length),]
names(Fr)[1:2] = c("Make.Model","No.Registered")
str(Fr)
head(Fr)
tail(Fr)
sum(Fr$No.Registered)

summary(Fr)
hist(Fr$Length,breaks=25)



###########


ALREADY=TRUE

if(!ALREADY) {

library(RCurl)

url.0 = "https://www.cars-data.com/en/"
www.0 = "www.cars-data.com/en/"

TXT = getURL(url.0)
txt=strsplit(TXT,"\n")[[1]]; length(txt)
txt = txt[grep("footerbrands",txt)+1]
txt = strsplit(txt,"<a href=")[[1]]
company = txt[-1]
n = length(company)

CO=rep(NA,n)
for(i in 1:n){
	CO[i]=strsplit(strsplit(company[i],">")[[1]][2],"<")[[1]][1]
}

m = matrix(NA,length(Fr$Main),length(CO))
for(i in 1:length(Fr$Main)){
    for( j in 1:length(CO)){
    	    hit = grep(CO[j],Fr$Main[i],ignore.case =TRUE)
    	    if(length(hit)==1) m[i,j] = 1
    }		
}

h = apply(m,1,sum,na.rm=TRUE)
Fr = Fr[h>0,] ; str(Fr)

H = apply(m,2,sum,na.rm=TRUE)
Make.Index = (1:length(CO))[H>0]

company = company[Make.Index]

n = length(company)

url1 =rep(NA,n)

MODEL= rep(NA,100000)
LENGTH=rep(NA,100000)

ii = 0

for( i in 33:n){
	
	print(i)
	Txt = strsplit(company[i], " rel=")[[1]][1]
	nk = nchar(Txt)
	url1[i] = substr(Txt,2,nk-1)
	TXT2 = getURL(url1[i])
    txt2=strsplit(TXT2,"\n")[[1]]; length(txt2)
    txt2=txt2[grep("col-4",txt2)]
    txt2 = txt2[ grep(url1[i],txt2) ]
    
    n2 = length(txt2)
    print(c("n2",n2))
    url2 =rep(NA,n2)
    for( j in 1:n2){
    	   #print(j)
    	   Txt2 = strsplit(txt2[j], " title=")[[1]][1]
    	   nk2 = nchar(Txt2)
    	   url2[j] = substr(Txt2,38,nk2-1)
    	   #print(url2[j])
    	   #print(noquote(""))
    	   TXT3 = getURL(url2[j])
       txt3=strsplit(TXT3,"\n")[[1]]; length(txt3)
       txt3 = txt3[grep("col-4",txt3)]
       txt3 = txt3[grep("www.cars-data.com/en",txt3)]
       #print(txt3)
       #print(noquote(""))
       n3 = length(txt3)
       #print(n3)
       url3 =rep(NA,n3)
       for( k in ceiling( median(1:n3) ) ){
       	     #print(k)
    	         Txt3 = strsplit(txt3[k], " title=")[[1]][1]
    	         Txt3 = strsplit(Txt3, "href=")[[1]][2]
    	         nk = nchar(Txt3)
    	         url3[k] = substr(Txt3,2,nk-1)

             
             TXT4 = getURL(url3[k])
             txt4=strsplit(TXT4,"\n")[[1]];
             txt4 = txt4[grep("-specs/",txt4)]
             n4 = length(txt4)
             url4 =rep(NA,n4)       
             for( m in ceiling( median(1:n4) ) ){	
             	Line = strsplit(txt4[m], " title=")[[1]]
             	model = Line[2] 
             	model = strsplit(model,">")[[1]][2]
             	model = strsplit(model,"<")[[1]][1]
             	Txt4 = Line[1]
    	        Txt4 = strsplit(Txt4, "href=")[[1]][2]
    	        nk = nchar(Txt4)
    	        url4[m] = substr(Txt4,2,nk-1)
                #print(url4[m])           
                TXT5 = getURL(url4[m])
                txt5=strsplit(TXT5,"\n")[[1]];
                
                line.no = grep("EXTERIOR DIMENSIONS",txt5)+1
                
                Txt = txt5[line.no]
                Txt = strsplit(Txt, "length")[[1]][2]
                Txt = strsplit(Txt, "</dd>")[[1]][1]
                Txt = strsplit(Txt, ">")[[1]]
                Txt = Txt[ length(Txt) ]
                ii=ii+1
                MODEL[ii] = model
                LENGTH[ii] = Txt
                #print(url4[m])
                #print(txt)
                if( (ii %% 10) == 0 ) print(ii)         	             	
             } # m                  
    	   } # k
    	   #print(noquote(""))
    }  # j
} # i


ds = data.frame(Model=MODEL,
                Length=as.numeric(substr(LENGTH,1,4)),
                stringsAsFactors=FALSE)
str(ds)
ds = ds[!is.na(ds$Length),]
str(ds)

} # ALREADY

