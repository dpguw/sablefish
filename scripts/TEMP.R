library(ALKr)
data(hom)
hoenig(Ak = hom$otoliths[1:10],
       fik = replicate(10, hom$F1992, simplify = FALSE),
       fiz = list(hom$F1993))

hoenig(Ak = hom$otoliths[1:10],
          fik = replicate(10, hom$F1992, simplify = FALSE),
          fiz = list(hom$F1993))




head(surveyLength,1)

surveyLengthCountFxn<-function(df,year,sex)
{
  aa<-NULL
  aa<-which(df$year==year & df$sex==sex)
  table(df$lengthTrunc[aa])
}

aa=surveyLengthCountFxn(df=surveyLength,
                     year=2000,
                     sex=2)
sum(aa)

maleSurveyCountsByYear<-mapply(surveyLengthCountFxn,
                                 year=1983:2009,
                                   MoreArgs=
                                   list(sex=1,df=surveyLength))

femaleSurveyCountsByYear<-mapply(surveyLengthCountFxn,
                               year=1983:2009,
                               MoreArgs=
                                 list(sex=2,df=surveyLength))

names(maleSurveyCountsByYear)=1983:2009
maleSurveyCountsByYear[["2009"]]
lapply(aa,sum)

matchNoAgeYearList<-list(
                         "1983"=1985,
                         "1984"=1985,
                         "1986"=c(1985,1987)
                         "1988"=c(1987,1989),
                         "1990"=c(1989,1991),
                         "1992"=c(1991,1993),
                         "1994"=1993)

tagYearsNoAgeData <- c(1983,1984,1986,1988,1990,1992,1994)
tagYearsYesAgeData <- c(1985,1987,1989,1991,1993,1997:2009)

aa=which(ageMat$Year==2000 & ageMat$sex==1 & !is.na(ageMat$age))
bb=table(ageMat$lengthBin[aa],ageMat$age[aa])
cc=prop.table(bb,1)

head(cc)

ageKeyTableFxn<-function(agemat,year,sex)
{
  aa=which(ageMat$Year==year & ageMat$sex==sex & !is.na(ageMat$age))
  bb=table(ageMat$lengthBin[aa],ageMat$age[aa])
  cc=prop.table(bb,1)
  
}

femaleALKyearsAgeDataTable<- mapply(ageKeyTableFxn,year=tagYearsYesAgeData,
                                    MoreArgs=list(sex=2,agemat=ageMat))
maleALKyearsAgeDataTable<- mapply(ageKeyTableFxn,year=tagYearsYesAgeData,
                                    MoreArgs=list(sex=1,agemat=ageMat))

femaleALKyearsAgeDataTable[["1985"]]

surveyLength <-lengthBinFxn(ageseq=seq(46,122,by=2),
                     df=surveyLength,
                     dflength=surveyLength$lengthTrunc)

matchNoAgeYearList

names(matchNoAgeYearList)

for(i in 1:length(matchNoAgeYearList))
{
  aa=length(matchNoAgeYearList[[i]])
  for(j in 1:length(aa))
  {
    bb=which()
  }
}
#########################################################################
aa=mapply(ageKeyTableFxn,year=tagYearsYesAgeData,
       MoreArgs=list(sex=2,agemat=ageMat,type=3))
names(femaleALKyearsAgeDataTable) <- tagYearsYesAgeData


surveyLengthCountsFxn<-function(df,year,sex)
{
  aa<-which(df$year==year & df$sex==sex)
  return(table(df$lengthBin[aa]))
}

mapply(surveyLengthCountsFxn,
       year=tagYears,
       MoreArgs=list(sex=1,df=surveyLength) )


hoenig(Ak, fik, fiz, threshold = 1, maxiter = 2000,
       age_classes = colnames(Ak[[1]]),
       length_classes = rownames(Ak[[1]]), name = "",
       description = "")

AK=list(femaleALKyearsAgeDataTable[["1985"]])
AKnames=dimnames(femaleALKyearsAgeDataTable[["1985"]])[[1]]
fik=list(femaleSurveyLengthCounts[["1985"]])
fikNames=names(femaleSurveyLengthCounts[["1985"]])
setdiff(AKnames,fikNames)
fiz=list(femaleSurveyLengthCounts[["1986"]])
fizNames=names(femaleSurveyLengthCounts[["1986"]])
setdiff(AKnames,fizNames)

hoenig(AK,fik,fiz)

matrix(as.vector(femaleALKyearsAgeDataTable[["1985"]]),
       byrow=F,ncol=19)
femaleALKyearsAgeDataTable[["1985"]]

length(as.vector(femaleALKyearsAgeDataTable[["1985"]]))
length(as.vector(femaleALKyearsAgeDataTable[["1987"]]))

dim(femaleALKyearsAgeDataTable[["1987"]])
dim(femaleALKyearsAgeDataTable[["1985"]])

AK=list(
  matrix(as.vector(femaleALKyearsAgeDataTable[["1985"]]),
         byrow=F,ncol=19),
  matrix(as.vector(femaleALKyearsAgeDataTable[["1987"]]),
         byrow=F,ncol=19))
fik=list(as.vector(unname(femaleSurveyLengthCounts[["1985"]])),
         as.vector(unname(femaleSurveyLengthCounts[["1987"]])))
ageclasses=2:20
lengthclasses=dimnames(femaleALKyearsAgeDataTable[["1985"]])[[1]]
setdiff(AKnames,fikNames)
fiz=list(as.vector(unname(femaleSurveyLengthCounts[["1986"]])))

hoenig(AK,fik,fiz,
       age_classes=ageclasses,
       length_classes=lengthclasses,
       threshold=1)


kimura_chikuni(x, fi1, fi2, threshold = 1e-04,
               maxiter = 2000, age_classes = colnames(x),
               length_classes = rownames(x), name = "",
               description = "")

x= matrix(as.vector(femaleALKyearsAgeDataTable[["1985"]]),
          byrow=F,ncol=19)
fi1=as.vector(unname(femaleSurveyLengthCounts[["1985"]]))
fi2=as.vector(unname(femaleSurveyLengthCounts[["1986"]]))

aa=kimura_chikuni(x,fi1,fi2)
class(aa)
str(aa)

round(aa@alk,2)
dim(x)
length(fi1)
length(fi2)

class(x)
dim(x)
aa=which(ageMat$Year==1989)
table(ageMat$lengthBin[aa])

femaleALKyearsAgeDataTable[["1989"]]

ageMat
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

length(lengthBinVec)
