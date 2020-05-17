
setwd("/Users/jameshanley/Dropbox/Courses/statbook/")

ds=read.csv("veh0126.csv",as.is=TRUE,skip=6)
head(ds[,1:10],6)
tail(ds[,1:10],15)
first = function(x) strsplit(x," ")[[1]][1]
ds$Main = unlist( lapply(ds$Make,first) )
sum(ds$Total)
fr = aggregate(ds$Total,by=list(Main=ds$Main),sum) ; str(fr)
TOP = fr$Main[fr$x>10000]

dimnames(ds)

# str(ds)
# head(ds[14000:14100,1:8],100)'

sort(table(ds$Make))



head(ds); tail(ds)

length(dimnames(ds)[[2]])
Y=4:103
t=apply(ds[,Y],2,sum,na.rm=TRUE)


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

CO[c(5,9,11,12,14,15,19,20,22,23,27,29,32,34,35,37,38,40,
   44,46,47,52,55,58,59,62,64,65,67,68,70,71,72,73,74,
   75,76,79,80,82,83,84,85,87,88,89)]



url1 =rep(NA,n)

MODEL= rep(NA,10000)
LENGTH=rep(NA,10000)

ii = 0

for( i in c(5,9)){
	
	Txt = strsplit(company[i], " rel=")[[1]][1]
	nk = nchar(Txt)
	url1[i] = substr(Txt,2,nk-1)
	#print(noquote(""))
	#print(url1[i])
	#print(noquote(""))
	TXT2 = getURL(url1[i])
    txt2=strsplit(TXT2,"\n")[[1]]; length(txt2)
    txt2=txt2[grep("col-4",txt2)]
    txt2 = txt2[ grep(url1[i],txt2) ]
    
    n2 = length(txt2)
    url2 =rep(NA,n2)
    for( j in 1:2){
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
       url3 =rep(NA,n3)
       for( k in 1:2){
    	         Txt3 = strsplit(txt3[k], " title=")[[1]][1]
    	         Txt3 = strsplit(Txt3, "href=")[[1]][2]
    	         nk = nchar(Txt3)
    	         url3[k] = substr(Txt3,2,nk-1)
             #print(url3[k])
             
             TXT4 = getURL(url3[k])
             txt4=strsplit(TXT4,"\n")[[1]];
             txt4 = txt4[grep("-specs/",txt4)]
             n4 = length(txt4)
             url4 =rep(NA,n4)       
             for( m in 1:n4){	
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
                if( (ii %% 100) == 0 ) print(ii)         	             	
             } # m                  
    	   } # k
    	   print(noquote(""))
    }  # j
} # i


ds = data.frame(Model=MODEL,
                Length=as.numeric(substr(LENGTH,1,4)),
                stringsAsFactors=FALSE)
str(ds)
ds = ds[!is.na(ds$Length),]
str(ds)
summary(ds$Length)
table(ds$Length)
hist(ds$Length)

tail(ds)
head(ds)

length(unique(ds$Model))

setwd("/Users/jameshanley/Dropbox/Courses/Statbook")
write.table(ds,"AudiBMW.txt")

hist(unique(ds$Length))
