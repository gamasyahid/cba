#Import
library(readxl)
library(maxLik)
library(miscTools)
library(zoo)

excel_file <- "C:\\Users\\user\\Downloads\\Documents\\Script Collection\\1 R\\2 Projects\\1 transport economics_cba analysis_brt system\\journal\\Data Koridor Input R 3 - raw.xlsx"


data              <- read_excel("C:\\Users\\user\\Downloads\\Documents\\Script Collection\\1 R\\2 Projects\\1 transport economics_cba analysis_brt system\\journal\\Data Koridor Input R 3 - raw.xlsx")
#data              <- subset(data,data[,28]==1)
unique.ID         <- unique(data[,1])
length.data       <- rep(0,length(unique.ID))

#########Declaration#########
ID <-data[,1]
NO_SP <-data[,2]
OFTEN <-data[,3]
GENDER <-data[,4]
AGE <-data[,5]
JOB <-data[,6]
INCOME <-data[,7]
PURPOSE <-data[,8]
MODE <-data[,9]
LAST_COST <-data[,10]
LAST_TIME <-data[,11]
EXPENDITURE_HOUSEHOLD <-data[,12]
COST <-data[,13]
TIME_SAVE <-data[,14]
CHOICE <-data[,15]
INVERSE <-data[,16]

#################


dummy.id           <- rep(1,nrow(data))

for   (i in 2:nrow(data)){dummy.id[1]        <- 1
if    (data[i,2]!=data[i-1,2]){dummy.id[i]   <- dummy.id[i-1]+1}
else  {dummy.id[i]                           <- dummy.id[i-1]}}

z                  <-data.frame(dummy.id)

unique.ID2 		     <-unique(z[,1])
temp			         <-rep(1,length(unique.ID2))
no.res             <-length(unique.ID2)

for(j in unique.ID2){
  temp[j] 	       <-length(z[z[,1]==j,1])
}
last	             <-cumsum(temp)
first	             <-(last-temp)+1

list.obs           <-data.frame(first,last)

data.analysis      <-data

p1                 <-rep(0,(no.res))

##########Estimation Function#########
loglik             <- function(beta){
  
  ASC.TJ           <-beta[1]
  cons.TIME_SAVE          <-beta[2]
  cons.COST         <-beta[3]
  cons.OFTEN         <-beta[4]
  cons.INCOME         <-beta[5]
  
  
  #########Utility Function#########
  u.TJ             <- ASC.TJ+(cons.TIME_SAVE*(TIME_SAVE))+(cons.COST*(COST))+(cons.OFTEN*(OFTEN))+(cons.INCOME*(INCOME))
  u.TJ0            <-exp(-u.TJ)
  p.TJ             <-1/(1+(u.TJ0))
  p.nTJ            <-1-(p.TJ)
  p                 <-(CHOICE*p.TJ)+(INVERSE*p.nTJ)
  
  
  for (i in 1:(no.res)){
    p1[i]<- prod(p[list.obs[i,1]:list.obs[i,2],])
  }
  
  ll        <- log(p1)
  
  return(ll)
  
}

mle <- maxLik(loglik,start=c(ASC.TJ=0,cons.TIME_SAVE=0,cons.COST=0,cons.OFTEN=0,cons.INCOME=0),
              method="BFGS",print.level=1)
summary(mle)