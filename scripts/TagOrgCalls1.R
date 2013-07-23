############################################################
matchNoAgeYearList<-list(
  "1983"=1985,
  "1984"=1985,
  "1986"=c(1985,1987),
  "1988"=c(1987,1989),
  "1990"=c(1989,1991),
  "1992"=c(1991,1993),
  "1994"=1993)
matchNoAgeYearVector <-c(
  "1983"=1985,
  "1984"=1985,
  "1986"=1985,
  "1988"=1987,
  "1990"=1989,
  "1992"=1991,
  "1994"=1993)
####################################################
regionsList<- regionsListFxn()
####################################################
surveyLength <- surveyLengthReadInFxn(path=sableLengthDataPath,
                                      head=FALSE)
#####################################################
surveyLength <- surveyLengthNamesFxn(surveyLength)
#####################################################
surveyLength$lengthTrunc <- maxLengthFxn(df=surveyLength,maxLengthFemale=89,maxLengthMale=79)
##########################################################
surveyLength<- surveyRegionFxn(df=surveyLength,
                               regionvec=regionVec,
                               regionslist=regionsList)
###########################################################
surveyLength <-lengthBinFxn(ageseq=lengthBinVec,
                            df=surveyLength,
                            dflength=surveyLength$lengthTrunc)
###########################################################
regionsVec <- regionsVecFxn()
######################################################
releases <- releasesReadInFxn(path=releasesPath)
releases <- releaseAreaFxn(releases,regionvec=regionVec)
releases <- subset(releases,!is.na(release.area))
releases <- subset(releases,!is.element(HArea,c(c(65,70))))
releases <- releasesLengthFxn(releases)
######################################################
regionYearDF <- regionYearDfFxn(tagyearvec=tagYears,
                                regionvec=regionVec)
#####################################################
yearList <- yearListFxn()
##################################################
maleRows <- lengthRowsFxn(tempSex=1,
                          regionyeardf=regionYearDF)
femaleRows <- lengthRowsFxn(tempSex=2,
                            regionyeardf=regionYearDF)
totalRows <- lengthRowsFxn(tempSex=c(1,2),
                           regionyeardf=regionYearDF)
##################################################
femaleLengths <- lengthsFxn(femaleRows,surveyLength$lengthTrunc) 
maleLengths <- lengthsFxn(maleRows,surveyLength$lengthTrunc) 
totalLengths <- lengthsFxn(totalRows,surveyLength$lengthTrunc)
##################################################
femaleCounts <- countsFxn(histFxn=lengthHistFxn,
                          lengthList=femaleLengths,
                          beginMid=44.5,
                          endMid=100.5) 
maleCounts <- countsFxn(histFxn=lengthHistFxn,
                        lengthList=maleLengths,
                        beginMid=44.5,
                        endMid=100.5) 
totalCounts <- countsFxn(histFxn=lengthHistFxn,
                         lengthList=totalLengths,
                         beginMid=44.5,
                         endMid=100.5) 
##################################################
femalePercent <- percentSexFxn(percentVectorFxn,
                               fract=femaleCounts,
                               total=totalCounts,
                               min=45,max=100,
                               tempSex=2)
malePercent <- percentSexFxn(percentVectorFxn,
                             fract=maleCounts,
                             total=totalCounts,
                             min=45,max=100,
                             tempSex=1)
##################################################
femPercentDF<-femPercentDFfxn(sizerange=45:100,
                              regionyeardf=regionYearDF)
########################################################
releases <- probFemReleaseVec(fempercentdf=femPercentDF,
                             releasesdf=releases)
#########################################################
releases <- assignSexWrapFxn(releasedf=releases)
#########################################################
replaceSexFxn(filename=recovLLtablePath,
              beginYear=1983,
              endYear=2009,
              releasedf=releases,
              minSize=450)
##############################################################
ageMat<-ageTablesReadInFxn(years8710file=sableAgesMainTablePath,
                           year85file=sablesAges1985Path,
                           year81file=sablesAges1981Path)

##############################################################
ageMat$age[which(ageMat$age>20)]<-20
##############################################################
ageMat$lengthTrunc <- maxLengthFxn(df=ageMat,maxLengthFemale=89,maxLengthMale=79)
##############################################################
releases$lengthTrunc <- maxLengthFxn(df=releases,maxLengthFemale=89,maxLengthMale=79)
##############################################################
ageMat<-lengthBinFxn(ageseq=lengthBinVec,
                  df=ageMat,
                  dflength=ageMat$lengthTrunc)


##############################################################
releases<-lengthBinFxn(ageseq=lengthBinVec,
                  df=releases,
                  dflength=releases$lengthTrunc)
##############################################################
#releasesWithAgesYearsNoAgeData <- alkWrapperFxn(tagyearsnoagedata=tagYearsNoAgeData,
#                                     tagyearsall=tagYears,
#                                     lengthsfemales=female.lengths,
#                                     lengthsmales=male.lengths,
#                                     releasedf=releases,
#                                     filelocmale=maleALKpath,
#                                     filelocfemale=femaleALKpath,
#                                     agefxn=AlkFxn)
#dim(releasesWithAgesYearsNoAgeData)
#####################################################################
#releasesWithAgesYearsYesAgeData <- alkYearsAgeDataWrapperFxn(
#                                       alkfxn=alkYearsAgeDataFxn,
#                                       yearvec=rep(tagYearsYesAgeData,2),
#                                       sexvec=rep(1:2,each=length(tagYearsYesAgeData)),
#                                       agemat=ageMat,
#                                       releasesdf=releases)
#############################################################
#releases <- rbind(releasesWithAgesYearsNoAgeData,
#                  releasesWithAgesYearsYesAgeData)
############################################################
maleSurveyCountsByLength <- mapply(surveyLengthCountFxn,
                                 year=1983:2009,
                                   MoreArgs=
                                   list(sex=1,df=surveyLength))
names(maleSurveyCountsByLength) <- 1983:2009
femaleSurveyCountsByLength <- mapply(surveyLengthCountFxn,
                                   year=1983:2009,
                                     MoreArgs=
                                     list(sex=2,df=surveyLength))
names(femaleSurveyCountsByLength) <- 1983:2009
#################################################################
#this creates alks for years with age data
#ageKeyTableFxn does it for individual year
femaleALKyearsAgeDataTable <- mapply(ageKeyTableFxn,year=tagYearsYesAgeData,
                                    MoreArgs=list(sex=2,agemat=ageMat,type=1))
    names(femaleALKyearsAgeDataTable) <- tagYearsYesAgeData
maleALKyearsAgeDataTable<- mapply(ageKeyTableFxn,year=tagYearsYesAgeData,
                                  MoreArgs=list(sex=1,agemat=ageMat,type=1))
    names(maleALKyearsAgeDataTable) <- tagYearsYesAgeData
######################################################################
maleSurveyLengthCounts<-mapply(surveyLengthCountsFxn,
                               year=tagYears,
                               MoreArgs=list(sex=1,df=surveyLength) ,
                               SIMPLIFY=FALSE)
    names(maleSurveyLengthCounts)<-tagYears
femaleSurveyLengthCounts <- mapply(surveyLengthCountsFxn,
                                 year=tagYears,
                                 MoreArgs=list(sex=2,df=surveyLength),
                                  SIMPLIFY=FALSE)
    names(femaleSurveyLengthCounts)<-tagYears
################################################################
#3 Create alks  for years WITHOUT age data
#using Kimura algorithm
maleAlkNoAgeDataYears<-ageingNoDataYears(sex=1)
femaleAlkNoAgeDataYears<-ageingNoDataYears(sex=2)
################################################################
#merge alks for both year types into 1 list for each sex
finalMaleAlkList<-mergeAlkListFxn(maleAlkNoAgeDataYears,
                                  maleALKyearsAgeDataTable,
                                  tagYears)

finalFemaleAlkList<-mergeAlkListFxn(femaleAlkNoAgeDataYears,
                                    femaleALKyearsAgeDataTable,
                                    tagYears)
##############################################################
releases <- ageingWrapperFxn(femalelist=finalFemaleAlkList,malelist=finalMaleAlkList)
##############################################################
maleReleaseInfo<-tableReleasesFxn(sex=1)
femaleReleaseInfo<-tableReleasesFxn(sex=2)
#########################################################
releaseInfoTablePDFfxn(maleinfo=maleReleaseInfo,
                       femaleinfo=femaleReleaseInfo,
                       pdf=FALSE,
                       pdfpath=pdfPath)
###########################################################
releaseInfoTablePDFfxn(maleinfo=maleReleaseInfo,
                       femaleinfo=femaleReleaseInfo,
                       pdf=TRUE,
                       pdfpath=pdfPath)
####################################################

print("hello world")
