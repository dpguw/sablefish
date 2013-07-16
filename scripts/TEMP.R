#1 make alks for years with age data
femaleALKyearsAgeDataTable
maleALKyearsAgeDataTable
#2 use alks to age releases (kimura algo) for years with age data


ageingFxn<-function(alklist,sex,releasedf)
{
  tempTable <- prop.table(alklist,margin=1)
  tempYear <- as.numeric(names(alklist))
  tempReleases <- releasedf[which(releasedf$year==tempYear &
                                    releasedf$sex==sex),]
  ageKey(tempTable,age~lengthTrunc,data=WR.len)
  
}


#find what survey lengths (binned) that are not in the age data 
#for years that are used to fill in for missing data (the years in
#matchNoAgeYearVector)
ageMatReleaseLengthDiffFxn <- function(year,sex,agemat,releasedf)
{
  aa<- which(agemat$Year==year & 
               agemat$lengthBin>=45 & 
               agemat$sex==sex &
               agemat$age>1)
  bb <- unique(agemat$lengthBin[aa])
  cc <- which(releasedf$HaulYear==year & 
                releasedf$lengthBin>=45 & 
                releasedf$sex==sex )
  dd<- unique(releasedf$lengthBin[cc])
  sort(setdiff(bb,dd))
}

ageMatReleaseLengthDiffFxn(year=1985,sex=1,agemat=ageMat,releasedf=releases)
mapply(ageMatReleaseLengthDiffFxn,
       year=unname(matchNoAgeYearVector),
       MoreArgs=list(sex=2,agemat=ageMat,
                     releasedf=releases),SIMPLIFY=FALSE)

round(prop.table(table(ageMat$age[which(ageMat$sex==1 & ageMat$lengthBin>78)])),2)

ageKeyTableFxn

alkAgeDataYesFxn<-function(year,sex,agemat)
{
  aa<- which(agemat$Year==year & 
               agemat$lengthBin>=45 & 
               agemat$sex==sex &
               agemat$age>1)
  bb <- NULL
  bb <- table(agemat$lengthBin[aa],agemat$age[aa])
  tempagekey <-  prop.table(bb,margin=1)
  return(tempagekey)
}


alkAgeDataYesFxn(year=2000,sex=2,agemat=ageMat)
##############################################################
alkYearsNoAgeDataListFxn <- function(yearNoAgeData,yearAgeData,sex)
{
  aa<- as.character(yearAgeData)
  bb <- as.character(yearNoAgeData)
  if(sex==1)
  {
    alkList <- maleALKyearsAgeDataTable
    surveyLengthCountsList<- maleSurveyLengthCounts
  }
  if(sex==2)
  {
    alkList <- femaleALKyearsAgeDataTable
    surveyLengthCountsList <- femaleSurveyLengthCounts
  }
  
  x <-  matrix(as.vector(alkList[[aa]]),
               byrow=F,ncol=19)
  fi1 <- as.vector(unname(surveyLengthCountsList[[aa]]))
  fi2 <-  as.vector(unname(surveyLengthCountsList[[bb]]))
  
  alkYearsNoAgeData <- kimura_chikuni(x,fi1,fi2)@alk
  dimnames(alkYearsNoAgeData)[[1]] <-  lengthBinVec[1:28]-1
  dimnames(alkYearsNoAgeData)[[2]] <-  2:20
  
  return(alkYearsNoAgeData)
}



dim(x)
length(fi1)
alkYearsNoAgeDataListFxn(yearNoAgeData=names(matchNoAgeYearVector[1]),
                         yearAgeData=matchNoAgeYearVector[1],
                         sex=1)

yearNoAgeData=names(matchNoAgeYearVector[1])
yearAgeData=matchNoAgeYearVector[1]
sex=1

aa<- as.character(yearAgeData)
bb <- as.character(yearNoAgeData)
if(sex==1)
{
  alkList <- maleALKyearsAgeDataTable
  surveyLengthCountsList<- maleSurveyLengthCounts
}
if(sex==2)
{
  alkList <- femaleALKyearsAgeDataTable
  surveyLengthCountsList <- femaleSurveyLengthCounts
}

x <-  matrix(as.vector(alkList[[aa]]),
             byrow=F,ncol=19)
fi1 <- as.vector(unname(surveyLengthCountsList[[aa]]))
fi2 <-  as.vector(unname(surveyLengthCountsList[[bb]]))

dim(x)
length(fi1)
length(fi1)

alkYearsNoAgeData <- kimura_chikuni(x,fi1,fi2)@alk
dimnames(alkYearsNoAgeData)[[1]] <-  lengthBinVec[1:28]-1
dimnames(alkYearsNoAgeData)[[2]] <-  2:20

