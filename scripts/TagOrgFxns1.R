#script run on v 2.15.3
##################################################################

#set working directory to github main folder

#laptop settings
  #setwd("C:/Users/Fantasia/Documents/GitHub/sablefish")
#workstation setings
  setwd("C:/Users/Lynx/Documents/GitHub/sablefish")
  libLocVec <- paste(getwd(),"/Rpackages",sep="")  
###################################################################
#note this sets library path to ~/sablefish/Rpackages (other libraries
#not in there will not be available)
.libPaths(libLocVec)
###############################################################
#only need to do this once, installs dependencies also
#at some point, stringr became a dependency to a necessary package
#after creation on the InstallFSA.R file created and thus has
#to be added manually
#install.packages("stringr",
#                 repos="http://cran.us.r-project.org",
#                 dependencies=TRUE)
#source("http://www.rforge.net/FSA/InstallFSA.R")
#install.packages('ALKr',
#                 repos="http://cran.us.r-project.org",
#                 dependencies=TRUE)
###############################################################
#note, need R < v3.0 (eg, 15.3) to have FSA load/run
library(FSA,lib=libLocVec)
library(ALKr,lib=libLocVec)
#############################################################
load(paste(getwd(),"/inputData/HoeningMaleData.RData",sep=""))
load(paste(getwd(),"/inputData/HoeningFemaleData.RData",sep=""))
#############################################################
tagYears<-c(1983:1994,1997:2009)
tagYearsNoAgeData <- c(1983,1984,1986,1988,1990,1992,1994)
tagYearsYesAgeData <- c(1985,1987,1989,1991,1993,1997:2009)
regionVec <- c("eg","cg","wg","bs","ai")
sexVec <- 2:1
modelAges <- 2:20
lengthBinVec<-seq(46,122,by=2)
#######################################################
maleALKpath<-"\\inputData\\malesoutput2\\outputmale"
femaleALKpath<-"\\inputData\\femalesoutput2\\outputfemale"
sableLengthDataPath<-paste(getwd(),"\\inputData\\Sablefish_length_through2010.txt",
                           sep="")
releasesPath <- paste(getwd(),"\\inputData\\releasesCSV.txt",sep="")
recovLLtablePath <- paste(getwd(),"\\inputData\\Recoveries from LL Releases.csv",sep="")
sableAgesMainTablePath <- paste(getwd(),"/inputData/sable_ages_james_5_11.txt",
                                sep="")
sablesAges1985Path <- paste(getwd(),"/inputData/1985_age.txt",
                            sep="")
sablesAges1981Path <- paste(getwd(),"/inputData/1981_age.txt",
                            sep="")

#############################################################
regionYearDfFxn <- function(tagyearvec,regionvec)
  {
   df<-data.frame(
     years=rep(tagyearvec,times=length(regionvec)),
     regions=rep(regionvec,each=length(tagyearvec)),
     stringsAsFactors=FALSE) 
  }

#############################################################
surveyLengthReadInFxn<-function(path,head=F)
{
  return(read.csv(path,header=head))
}
#############################################################
surveyLengthNamesFxn<-function(df)
{
  names(df)=c("year",
              "country",
              "vessel",
              "area",
              "region",
              "station",
              "species",
              "sex",
              "depth",
              "length",
              "freq")  
  return(df)  
}
#################################################################
maxLengthFxn <- function(df,maxLength)
{
  temp<- df$length
  temp[which(temp>maxLength)] <- maxLength 
  return(transform(df,lengthTrunc=temp))
}
##################################################################
regionsListFxn <- function()
  {
    return(
            list("Bering Sea",
               "Aleutians",
               "Western Gulf of Alaska",
               "Central Gulf of Alaska", 
               c("East Yakutat/Southeast", "West Yakutat"))
          )
  }
#################################################################
regionsVecFxn<-function()
  {
    return(c("bs","ai","wg","cg","eg"))
  }
################################################################
releasesReadInFxn <- function(path,years=1983:2009,minSize=450)
  {
    releases.temp=read.csv(path,header=T)
    temp=which(is.element(releases.temp$HaulYear,years) &
                 releases.temp$HSize>=minSize)
    releases=releases.temp[temp,]
    return(releases)
  }
###################################################################
yearListFxn <- function(startYear=1983,endYear=2009,incr=3)
{
  tempList=list(NULL)
  for(i in 1:length(seq(startYear,endYear,by=incr)))
  {
    tempList[[i]]=c(seq(startYear,endYear,by=incr)[i],
                    seq(startYear,endYear,by=incr)[i]+1,
                    seq(startYear,endYear,by=incr)[i]+2)
  }
  return(tempList)
}
####################################################################3
subsetLengthDfFxn<-function(regions,years,sex)
  {
    return(
      which(
        is.element(surveyLength$region2,regions) &
          is.element(surveyLength$year,years) &
          is.element(surveyLength$sex,sex) &
          surveyLength$lengthTrunc>=45 &
          surveyLength$lengthTrunc<=100)
    )
  }
######################################################################
lengthHistFxn <- function(histVec,beginMidPt,endMidPt)
{
  temp=hist(histVec,seq(beginMidPt,endMidPt,by=1),plot=F)   
  return(temp$counts)
}
####################################################################
percentVectorFxn <- function(fractVec,totalVec,minSize,maxSize,sex)
{
  percTemp<- fractVec/totalVec
  percTemp[which(is.nan(percTemp))] <- 0
  smuTemp <- supsmu(minSize:maxSize,percTemp)$y
  smuTemp[which(smuTemp<0)] <- 0
  smuTemp[which(smuTemp>1)] <- 1
  if(sex==1){smuTemp[42:56]=0}
  if(sex==2){smuTemp[42:56]=1}
  
  return(smuTemp)
}
#####################################################################
lengthsFxn<-function(tempList,tempLengthVec)
{
  lapply(tempList,function(tempList){tempLengthVec[tempList]}) 
}  
#####################################################################
lengthRowsFxnDeprecated <- function(tempSex)
  {
    mapply(subsetLengthDfFxn,
           years=rep(yearList,times=length(regionsList)),
           regions=rep(regionsList,each=length(yearList)),
           MoreArgs=list(sex=tempSex))
  }
lengthRowsFxnDeprecated <- function(tempSex,tempRegionsList,tempYearsVec)
{
  mapply(subsetLengthDfFxn,
         years=rep(tempYearsVec,times=length(tempRegionsList)),
         regions=rep(tempRegionsList,each=length(tempYearsVec)),
         MoreArgs=list(sex=tempSex))
}
#####################################################################
lengthRowsFxn <- function(tempSex,regionyeardf)
{
  mapply(subsetLengthDfFxn,
         years=regionyeardf$years,
         regions=regionyeardf$regions,
         MoreArgs=list(sex=tempSex))
}
########################################################################
countsFxn<-function(histFxn,lengthList,beginMid,endMid)
{
  mapply(histFxn,histVec=lengthList,
         MoreArgs=list(beginMidPt=beginMid,
                       endMidPt=endMid),
         SIMPLIFY=FALSE)  
}
######################################################################
percentSexFxn<-function(percFxn,fract,total,min,max,tempSex)
{
  mapply(percFxn,
         fractVec=fract,
         totalVec=total,
         minSize=min,maxSize=max,
         sex=tempSex,
         SIMPLIFY=FALSE)
}
##############################################################
releaseAreaFxnDeprecated <- function(df)
{
  bs=10:13
  ai=15:18
  wg=c(26,27)
  cg=c(30,35)
  eg=c(40,52,53,54)
  bc=c(65,70)
  
  area.list=list(bs,ai,wg,cg,eg)
  lengthRegionsShort=c("bs","ai","wg","cg","eg")
  release.area=rep("",dim(df)[1])
  temp=lapply(area.list,
              function(x)
              {which(is.element(df$HArea,x))})
  aa=NULL
  aa=rep(lengthRegionsShort,as.vector(unlist(lapply(temp,length))))
  release.area[unlist(temp)]=aa
  transform(df,release.area=release.area)  
}
####################################################################
releaseAreaFxn <- function(df,regionvec)
{
  bs=10:13
  ai=15:18
  wg=c(26,27)
  cg=c(30,35)
  eg=c(40,52,53,54)
  bc=c(65,70)
  
  area.list=list(bs,ai,wg,cg,eg)
  lengthRegionsShort=regionvec
  release.area=rep("",dim(df)[1])
  temp=lapply(area.list,
              function(x)
              {which(is.element(df$HArea,x))})
  aa=NULL
  aa=rep(lengthRegionsShort,as.vector(unlist(lapply(temp,length))))
  release.area[unlist(temp)]=aa
  transform(df,release.area=release.area)  
}

####################################################################
releasesLengthFxn <- function(df)
{  
  lengthVec <- signif(df$HSize,2)
  lengthVec[which(lengthVec>1000)]<-1000
  lengthVec  <- lengthVec/10
  df<-transform(df,lengthTrunc=lengthVec)
  return(df)
}
###################################################################
surveyRegionFxn <- function(df,regionvec,regionslist)
    {
        area<-rep("",dim(df)[1])
        temp<-lapply(regionslist,
                     function(x)
                     {which(is.element(df$region,x))})
        aa<-NULL
        aa<-rep(regionvec,as.vector(unlist(lapply(temp,length))))
        area[unlist(temp)]=aa
        df<-transform(df,region2=area)  
        df$region2 <- area
        return(df)
    }
########################################################
femPercentDFfxn <- function(sizerange,regionyeardf)
{
 return(
         data.frame(
              years=rep(regionyeardf$years,each=length(sizerange)),
              region=rep(regionyeardf$regions,each=length(sizerange)),
              length=rep(sizerange,nrow(regionyeardf)),
              femPerc=unlist(femalePercent)) 
      )
}
#########################################################
probFemReleaseVec<-function(fempercentdf,releasesdf)
{  
  aa=paste(fempercentdf$years,
           fempercentdf$region,
           fempercentdf$length)
  bb= paste(releasesdf$HaulYear,
            releasesdf$release.area,
            releasesdf$lengthTrunc)
  cc=match(bb,aa)
  dd=fempercentdf$femPerc[cc]
  releasesdf<-transform(releasesdf,probFem=dd)
  return(releasesdf)
}
###########################################################
assignSexFxn<-function(releasedf)
{
  
  bb<-table(releasedf$lengthTrunc)
  cc<-tapply(releasedf$probFem,
             releasedf$lengthTrunc,
             unique)
  dd<-data.frame(male.prob=1-as.numeric(cc),fem.prob=as.numeric(cc))
  ee<-as.table(as.matrix(dd[,1:2]))
  
  dimnames(ee)[[2]]<-1:2
  dimnames(ee)[[1]]=names(cc)
  releasedf<-ageKey(ee,releasedf,"lengthTrunc","sex")     
  return(releasedf)
}
#############################################################
assignSexWrapFxn<-function(releasedf)
{
  aa <- split(releasedf,list(as.factor(releasedf$HaulYear),
                            as.factor(releasedf$release.area)),
              drop=TRUE)
  
  bb <- lapply(aa,assignSexFxn)
  cc <- do.call("rbind",bb)
  return(cc)
}
########################################################
replaceSexFxn<-function(filename,
                        beginYear,
                        endYear,
                        releasedf,
                        minSize)
{
  recovTable<-read.csv(filename,header=T)
  recovTable<-subset(recovTable, HaulYear>=beginYear &
                       HaulYear<=endYear &
                       RecYear>=beginYear &
                       RecYear<=endYear)
  recovId=paste(recovTable$PrimTagNum,recovTable$PrimTagType )
  releaseId=paste(releasedf$PrimTagNum,releasedf$PrimTagType)
  gg=which(recovTable$HaulYear>=beginYear & 
           recovTable$HaulYear<=endYear &
           recovTable$HSize>=minSize & 
          (recovTable$Sex==1|recovTable$Sex==2) )
  hh=which(is.element(releaseId,recovId[gg]))
  
  releasedf$sex[hh]=recovTable$Sex[gg][match(releaseId[hh],recovId[gg])]
  
  return(releasedf)
  #from original code....
  #note:releases$Sex2[hh]=recov.table$Sex[match(release.id[hh],recov.id2)]
  #should work also if release.id and recov.id were comprised of completely unique
  #elements...but there a few repeats which mess it up a very,very trivial amount
  
}
########################################################
ageTablesReadInFxn<-function(years8710file,
                             year85file,
                             year81file)
{
  years8710 <- read.csv(years8710file,header=T)
  year85 <- read.table(year85file,header=T)
  year81 <- read.table(year81file,header=T)
  
  year85$Year <- rep(1985,nrow(year85))
  names(year85)[which(names(year85)=="Age")] <- "age"
  year81$Year <- rep(1981,nrow(year81))

   ageDf=rbind(
                  years8710[,c("Year","sex","age","length")],
                    data.frame(Year=year81$Year,
                               sex=year81$sex,
                               age=year81$age,
                               length=year81$length),
                    data.frame(Year=year85$Year,
                               sex=year85$sex,
                               age=year85$age,
                               length=year85$length)
                 )
  return(ageDf)
}
##################################################
lengthBinFxn<-function(ageseq,df,dflength)
{
  aa <- which(is.element(dflength,ageseq))
  dflength[aa]<-dflength[aa]-1  
  df$lengthBin<-dflength
  return(df)
}
##################################################

AlkFxn<-function(year,
                 yearVec,
                 femalelengths,
                 malelengths,
                 releasedf,
                 fileLocMale,
                 fileLocFemale,
                 sexType)
{
     index <- which(yearVec==year)
     if(sexType==1)
     {
         fileName <- paste(getwd(),"\\",fileLocMale,year,".out",sep="")
         Freq <- read.table(fileName,header=F)
         if(is.element(year,c(1986,1991)))
         {
           Freq[dim(Freq)[1],11] <- 1
         }
     }
     
     if(sexType==2)
     {
       fileName <- paste(getwd(),"\\",fileLocFemale,year,".out",sep="")
       Freq <- read.table(fileName,header=F)
     }
     
     
     ageKey <- prop.table(as.matrix(Freq),margin=1)
     rr <-  releasedf[releasedf$HaulYear==year & 
                      releasedf$sex==sexType & 
                      !is.na(releasedf$sex),]
     rr$age <- rep(NA,dim(rr)[1])
     dimnames(ageKey)[[2]]=as.character(2:20)
     if(sexType==1)
     {
       dimnames(ageKey)[[1]]=malelengths[[index]]
     }
     if(sexType==2)
     {
       dimnames(ageKey)[[1]]=femalelengths[[index]]
     }
     ageDist <- ageKey(ageKey,rr,"lengthBin","age")
     print(paste("sex = ",sexType," year= ",year))
     return(ageDist)
}

###########################################################
alkWrapperFxn <-function(tagyearsnoagedata,
                         tagyearsall,
                         lengthsfemales,
                         lengthsmales,
                         releasedf,
                         filelocmale,
                         filelocfemale,
                         agefxn)
{
  tempYearvec <- rep(tagyearsnoagedata,2)
  tempSexIndex <- c(rep(1,length(tagyearsnoagedata)),
                    rep(2,length(tagyearsnoagedata)) )
  
  aa<- mapply(agefxn,
              year=tempYearvec,
              sexType=tempSexIndex,
              MoreArgs=list(yearVec=tagyearsall,
                            femalelengths=lengthsfemales,
                            malelengths=lengthsmales,
                            releasedf=releasedf,
                            fileLocMale=filelocmale,
                            fileLocFemale=filelocfemale),
         SIMPLIFY=FALSE)
    bb<-NULL
    bb<- do.call("rbind",aa)
    return(bb)
  
}
###################################################################

alkYearsAgeDataFxn <-function(
                              releasedf,
                              year,
                              sex,
                              agemat)
{
  
  lengthTemplate <- sort(
    unique(
      releasedf$lengthBin[
        releasedf$HaulYear>=1997 &
          releasedf$sex==sex  
        ]))
    
  rr=releasedf[which(releasedf$HaulYear==year & 
                       releasedf$sex==sex),]
  
  if(sex==2 & is.element(year,c(2000,2004,2009)))
  {
    rr$lengthBin[which(rr$lengthBin==45)] <- 47
  }
  if(sex==1 & is.element(year,c(2003,2005)))
  {    
    rr$lengthBin[which(rr$lengthBin==79)]=77
  }
  if(sex==1 & year==2005)
  {
   tempSamp<- sample(c(45,49),size=length(which(rr$lengthBin==47)),replace=T,prob=c(.5,.5))
   rr$lengthBin[which(rr$lengthBin==47)]<- tempSamp  
  }
    
  
  
  Freq <- table(rr$lengthBin)
  tempvec <- rep(0,length(lengthTemplate))
  tempvec[match(names(Freq),lengthTemplate)]=as.numeric(Freq)
  aa <- NULL
  aa<- which(agemat$Year==year & 
               agemat$lengthBin>=45 & 
               agemat$sex==sex &
               agemat$age>1)
  bb <- NULL
  bb <- table(agemat$lengthBin[aa],agemat$age[aa])
  cc <- NULL
  cc <- matrix(0,nrow=dim(bb)[1],ncol=19)
  #if not all ages 2-20 are in the age data for
  #year/sex combo
  if(dim(bb)[2]<19)
  {
    pp <- NULL
    pp <- match(as.numeric(dimnames(bb)[[2]]),2:20)
    cc[,pp]=bb
    dimnames(cc)[[1]]=dimnames(bb)[[1]]
    dimnames(cc)[[2]]=as.character(2:20)
    
  }
  if(dim(bb)[2]==19)
  {
    cc=bb
    dimnames(cc)[[1]]=dimnames(bb)[[1]]
    dimnames(cc)[[2]]=as.character(2:20)
  }
  
  
  rr$age <- rep(NA,nrow(rr))
  
  tempagekey <- NULL
  tempagekey <-  prop.table(cc,margin=1)
  dd <-  ageKey(tempagekey,rr,"lengthBin","age")
  
  return(dd)
}
#############################################################
alkYearsAgeDataWrapperFxn <-function(alkfxn,
                                     yearvec,
                                     sexvec,
                                     agemat,
                                     releasesdf)
{
 aa <- mapply(alkfxn,
         year=yearvec,
         sex=sexvec,
         MoreArgs=list(
                  releasedf=releasesdf,
                  agemat=ageMat),
        SIMPLIFY=FALSE)
 bb <- do.call("rbind",aa)
 return(bb)
}
####################################################################
surveyLengthCountFxn<-function(df,year,sex)
{
  aa<-NULL
  aa<-which(df$year==year & df$sex==sex)
  table(df$lengthTrunc[aa])
}
###################################################################

ageKeyTableFxn<-function(agemat,year,sex,type)
{
  aa <- which(ageMat$Year==year & 
                ageMat$sex==sex & 
                !is.na(ageMat$age) &
                 ageMat$age>1 &
                 ageMat$length>=45)
  bb <- table(ageMat$lengthBin[aa],ageMat$age[aa])
  cc <- prop.table(bb,1)
  if(type==3){return(aa)}
  if(type==1){return(bb)}
  if(type==2){return(cc)}
}
##################################################################
surveyLengthCountsFxn<-function(df,year,sex)
{
  aa<-which(df$year==year & df$sex==sex & df$length>=45)
  return(table(df$lengthBin[aa]))
}

###################################################################
alkYearsNoAgeDataListFxn <- function()
{
  alkYearsNoAgeData<-vector('list',length=length(matchNoAgeYearVector))
  
  for(i in 1:length(matchNoAgeYearVector))
  {
    bb <- names(matchNoAgeYearVector[i])
    aa <- as.character(unname(matchNoAgeYearVector[i]))
    
    x <-  matrix(as.vector(femaleALKyearsAgeDataTable[[aa]]),
                 byrow=F,ncol=19)
    fi1 <- as.vector(unname(femaleSurveyLengthCounts[[aa]]))
    fi2 <-  as.vector(unname(femaleSurveyLengthCounts[[bb]]))
    
    alkYearsNoAgeData[[i]] <- kimura_chikuni(x,fi1,fi2)@alk
    dimnames(alkYearsNoAgeData[[i]])[[1]] <-  lengthBinVec[1:28]-1
    dimnames(alkYearsNoAgeData[[i]])[[2]] <-  2:20
  }
  names(alkYearsNoAgeData)<-names(matchNoAgeYearVector)
  
  return(alkYearsNoAgeData)
}    
##################################################################
releasesForADMB<-function(year,sexType,relarea,releasedf)
  {
  
    ageVec <- NULL
    ageVec <- rep(NA,length(modelAges))
  
    aa <- NULL
    aa <- which(  releasedf$HaulYear==year &
                    releasedf$sex==sexType &
                    releasedf$release.area==relarea)
    if(length(aa)>0)
    {
      bb <- NULL
      bb <- table(releasedf$age[aa])
      cc <- NULL
      cc <- match(names(bb),modelAges)
      dd <- NULL
      dd <- rep(0,length(modelAges))
      dd[cc] <- unname(as.numeric(bb))      
      ageVec <- dd
    }else{
           ageVec <- rep(0,length(modelAges))
         }
    
    return(ageVec)
  }

#aa=releasesForADMB(year=2000,sexType=1,relarea="eg",releasedf=releases)
#class(aa)
#test
print("hello world")