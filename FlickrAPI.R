#####################################################
# R code to query Flickr API and build a dataframe
# Francesca Mancini
# adapted from Mauricion Alarcon https://rpubs.com/rmalarc/74406
#####################################################

#load required packages
library(RCurl)
library(XML)
library(httr)

api_key<-"your_api_key_here"      #API key and secret must be obtained from https://www.flickr.com/services/api/misc.api_keys.html

secret<- "your_secret_here"

myapp<-oauth_app("your_app_name_here",key= api_key,secret= secret)                  #creates the app passing the key and secret




ep<-oauth_endpoint(request="https://www.flickr.com/services/oauth/request_token"    #get authentication credentials from the API
                   ,authorize="https://www.flickr.com/services/oauth/authorize",
                   access="https://www.flickr.com/services/oauth/access_token")


sig<-oauth1.0_token(ep,myapp,cache=F)                                             #creates variable with authentication credentials

fl_sig <- sign_oauth1.0(myapp,sig)                                                #authenticate

baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,sep="")   #set base URL


pics<-NULL                              #creates empty object to store the data
year<-seq(2005,2016,1)                  #creates variable "year" so that query returns pictures taken between 2005 and 2014
text<-"puffin"                          #set keywords so that query returns pictures with the text "bird"
woeid<-"23424975"                       #only return pictures taken in the UK
hasgeo<-"1"                             #only return pictures that are geotagged
extras<-"date_taken,geo"                #extra information to download
perpage<-"250"                          #number of results to return per page
format<-"rest"                          #format of results

#API only returns 400 results per query so it is neccessary to loop through months to obtain all the results

for (y in 1:length(year)){                     #creates object dates
  for (m in 1:12){ daymin<-"01"
  
  ifelse ((m==4|m==6|m==9|m==11), daymax<-"30",daymax<-"31")
  if (m==2){
    ifelse (year[y]==2008|year[y]==2012|year[y]==2016, daymax<-29,daymax<-28)
  }
  
  ifelse (m==10|m==11|m==12,
          mindate<-as.character(paste(year[y],m,daymin,sep="-")),
          mindate<-as.character(paste(year[y],paste("0",m,sep="")
                                      ,daymin,sep="-")))
  
  ifelse (m==10|m==11|m==12,
          maxdate<-as.character(paste(year[y],m,daymax,sep="-")),
          maxdate<-as.character(paste(year[y],paste("0",m,sep="")
                                      ,daymax,sep="-")))
  
  
  
  getPhotos <- paste(baseURL                                           # make the request URL
                     ,"&text=",text,"&min_taken_date=",mindate,
                     "&max_taken_date=",maxdate,"&woe_id=",woeid,
                     "&has_geo=",hasgeo,"&extras=",extras,
                     "&per_page=",perpage,"&format=",format,sep="")
  
  
  
  getPhotos_data <- xmlRoot(xmlTreeParse(getURL                                    #parse URL and extract root node
                          (getPhotos,ssl.verifypeer=FALSE, useragent = "flickr") ))
  
  #results are returned in different pages so it is necessary to loop through pages to collect all the data
  #parse the total number of pages
  pages_data <- data.frame(xmlAttrs(getPhotos_data[["photos"]]))
  pages_data[] <- lapply(pages_data, as.character)
  pages_data[] <- lapply(pages_data, as.integer)
  colnames(pages_data)<- "value"
  total_pages <- pages_data["pages","value"]
  
  if(total_pages==0){break}         # stop the loop if there are no results
  
  pics_tmp<-NULL                    #create empty dataframe to store data temporarily
  
  
  # loop thru pages of photos and save the list in a DF
  for(i in c(1:total_pages)){
    getPhotos <- paste(baseURL                                                #create request URL
                       ,"&text=",text,"&min_taken_date=",mindate,
                       "&max_taken_date=",maxdate,"&woe_id=",woeid,
                       "&has_geo=",hasgeo,"&extras=",extras,
                       "&per_page=",perpage,"&format=",format,"&page="
                       ,i,sep="")
    
    getPhotos_data <- xmlRoot(xmlTreeParse(getURL                                # parse URL and extract root node
                          (getPhotos,ssl.verifypeer=FALSE, useragent = "flickr")
                          ,useInternalNodes = TRUE ))
    
    id<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"id")                 #extract photo id
    owner<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"owner")           #extract user id
    datetaken<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"datetaken")   #extract date picture was taken
    latitude<- xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"latitude")    #extract latitude
    longitude<- xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"longitude")  #extract longitude
    
    tmp_df<-data.frame(cbind(id,owner,datetaken,latitude,longitude),stringsAsFactors=FALSE)  #put together in a df
    
    tmp_df$page <- i
    pics_tmp<-rbind(pics_tmp,tmp_df)                                        
  }
  
  
  pics<-rbind(pics,pics_tmp)                                                  #add to big dataframe
  }}

################################
####Data formatting
################################

#####Date#####

library(lubridate)

pics$month<-month(ymd_hms(pics$datetaken),label=T)   #create variable month

pics$year<-year(ymd_hms(pics$datetaken))             #create variable year

####Coordinates####

options(digits=9)

pics$latitude<-as.numeric(pics$latitude)              #set latitude as numeric

pics$longitude<-as.numeric(pics$longitude)            #set longitude as numeric

##############################################################
###Remove duplicates
##############################################################
print(levels(pics$month))
pics$month<- factor(pics$month,levels(pics$month)      #reset order of month factor levels
                         [c(5,4,8,1,9,7,6,2,12,11,10,3)])

pics$dateonly<-sapply(strsplit(pics$datetaken, " "), "[[", 1)   #divide date from time


d<-duplicated(pics[,c(2,9)])                        #find multiple pictures from the same photographer in the same day

pics_unique<-pics[-which(d=="TRUE"),]                 #eliminate duplicates
str(pics_unique)

#############################################################
###Save dataset
#############################################################

library(stringr)

summary(pics_unique)                                          #make sure data is ok

which(pics_unique$latitude==0)                                #one photo has latitude of 0

pics<-pics[-which(pics$latitude==0),]                         #delete data with latitude recorded as 0

pics$datetaken<- str_replace_all(pics$datetaken, " ","_")     #substitute white spaces with "_"

write.table(pics_unique,"./flickr_puffins.txt", row.names=F,sep="\t", quote=F) #save dataset
