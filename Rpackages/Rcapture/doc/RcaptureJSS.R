### R code from vignette source 'RcaptureJSS.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: RcaptureJSS.Rnw:361-366
###################################################
library(Rcapture)
data(hare)
desc<-descriptive(hare)
plot(desc)
closedp(hare)


###################################################
### code chunk number 2: RcaptureJSS.Rnw:372-373
###################################################
plot(desc)


###################################################
### code chunk number 3: RcaptureJSS.Rnw:391-396
###################################################
col<-rep(0,2^6-1)
mat<-histpos.t(6)
col[apply(mat,1,sum)==6]<-1
cp.m2<-closedp.mX(hare,mX=cbind(mat,col),mname="Mt without 111111")
cp.m2$results


###################################################
### code chunk number 4: RcaptureJSS.Rnw:403-407
###################################################
CI1<-profileCI(hare,m="Mth",h="Poisson",a=2)
CI1$results
CI2<-profileCI(hare,mX=cbind(mat,col),mname="Mt without 111111")
CI2$results


###################################################
### code chunk number 5: RcaptureJSS.Rnw:423-425
###################################################
data(HIV)
descriptive(HIV,dfreq=TRUE)


###################################################
### code chunk number 6: RcaptureJSS.Rnw:430-432
###################################################
desc<-descriptive(HIV,dfreq=TRUE)
plot(desc)


###################################################
### code chunk number 7: RcaptureJSS.Rnw:444-448
###################################################
mat<-histpos.t(4)
mX1<-cbind(mat,mat[,1]*mat[,2],mat[,1]*mat[,3],mat[,1]*mat[,4],mat[,2]*mat[,3],mat[,2]*mat[,4],mat[,3]*mat[,4])
cp.m1<-closedp.mX(HIV,dfreq=TRUE,mX=mX1,mname="Mt double interactions")
cp.m1$results


###################################################
### code chunk number 8: RcaptureJSS.Rnw:454-455
###################################################
summary(cp.m1$glm)$coefficients


###################################################
### code chunk number 9: RcaptureJSS.Rnw:463-468
###################################################
mX2<-cbind(mat,mat[,1]*mat[,2])
cp.m2<-closedp.mX(HIV,dfreq=TRUE,mX=mX2,mname="Mt interaction 1,2")
cp.m2$results
CI<-profileCI(HIV,dfreq=TRUE,mX=mX2,mname="Mt interaction 1,2")
CI$results


###################################################
### code chunk number 10: RcaptureJSS.Rnw:473-474
###################################################
CI<-profileCI(HIV,dfreq=TRUE,mX=mX2,mname="Mt interaction 1,2")


###################################################
### code chunk number 11: RcaptureJSS.Rnw:490-493
###################################################
data(mvole)
cp<-closedp(mvole[,11:15])
cp


###################################################
### code chunk number 12: RcaptureJSS.Rnw:500-503
###################################################
psi<-function(x){-log(3.5+x)+log(3.5)}
lgmodel<-closedp.h(mvole[,11:15],h=psi)
lgmodel$results


###################################################
### code chunk number 13: RcaptureJSS.Rnw:511-513
###################################################
xx<-uifit(cp)
xx$predicted[,c(1,4,5,6)]


###################################################
### code chunk number 14: RcaptureJSS.Rnw:525-526
###################################################
boxplot(cp)


###################################################
### code chunk number 15: RcaptureJSS.Rnw:615-617
###################################################
data(bunting)
descriptive(bunting,dfreq=TRUE)


###################################################
### code chunk number 16: RcaptureJSS.Rnw:626-629
###################################################
op.m1<-openp(bunting,dfreq=TRUE)
op.m1$model.fit[1,]
plot(op.m1)


###################################################
### code chunk number 17: RcaptureJSS.Rnw:634-635
###################################################
plot(op.m1)


###################################################
### code chunk number 18: RcaptureJSS.Rnw:648-651
###################################################
keep2<-apply(histpos.t(8),1,sum)>1
op.m2<- openp(bunting,dfreq=TRUE,keep=keep2)
op.m2$model.fit[1,]


###################################################
### code chunk number 19: RcaptureJSS.Rnw:659-667
###################################################
keep3p<-residuals(op.m2$glm,type="pearson")<4
num3<-((1:255)[keep2])[keep3p]
keep3<-rep(FALSE,255)
keep3[num3]<-TRUE
op.m3<- openp(bunting,dfreq=TRUE,keep=keep3)
tab<-data.frame(op.m2$survivals,rep("|",7),op.m3$survivals)
colnames(tab)<-c("estimate.m2","stderr.m2","|","estimate.m3","stderr.m3")
tab


###################################################
### code chunk number 20: RcaptureJSS.Rnw:692-696
###################################################
siginv<-solve(op.m2$cov[8:12,8:12])
phi<-t(rep(1,5))%*%siginv%*%op.m2$survivals[2:6,1]/(t(rep(1,5))%*%siginv%*%rep(1,5))
se<-1/sqrt(t(rep(1,5))%*%siginv%*%rep(1,5))
data.frame(estimate=phi,stderr=se,row.names="Common survival: ")


###################################################
### code chunk number 21: RcaptureJSS.Rnw:705-707
###################################################
chisq4<-t(op.m2$survivals[2:6,1]-phi*rep(1,5))%*%siginv%*%(op.m2$survivals[2:6,1]-phi*rep(1,5))
data.frame(stat=chisq4,pvalue=1-pchisq(chisq4,df=4),row.names="Chi-square test: ")


###################################################
### code chunk number 22: RcaptureJSS.Rnw:718-722
###################################################
data(duck)
op.m1<-openp(duck,dfreq=TRUE)
op.m1$model.fit[1,]
plot(op.m1)


###################################################
### code chunk number 23: RcaptureJSS.Rnw:727-728
###################################################
plot(op.m1)


###################################################
### code chunk number 24: RcaptureJSS.Rnw:737-738
###################################################
1-pchisq(op.m1$model.fit[1,1],df=49)


###################################################
### code chunk number 25: RcaptureJSS.Rnw:744-748
###################################################
keep2<-apply(histpos.t(6),1,sum)!=6
op.m2<-openp(duck,dfreq=TRUE,keep=keep2)
op.m2$model.fit[1,]
1-pchisq(op.m2$model.fit[1,1],df=48)


###################################################
### code chunk number 26: RcaptureJSS.Rnw:754-758
###################################################
keep3<-apply(histpos.t(6),1,sum)<5
op.m3<-openp(duck,dfreq=TRUE,keep=keep3)
op.m3$model.fit[1,]
1-pchisq(op.m3$model.fit[1,1],df=42)


###################################################
### code chunk number 27: RcaptureJSS.Rnw:764-766
###################################################
op.m4<-openp(duck,dfreq=TRUE,keep=keep3,m="ep")
op.m4$model.fit[1,]


###################################################
### code chunk number 28: RcaptureJSS.Rnw:773-774
###################################################
op.m3$N


###################################################
### code chunk number 29: RcaptureJSS.Rnw:793-799
###################################################
growth<-op.m3$N[3:5,1]/op.m3$N[2:4,1]
partial<-matrix(c(-op.m3$N[3,1]/op.m3$N[2,1]^2,1/op.m3$N[2,1],0,0,
0,-op.m3$N[4,1]/op.m3$N[3,1]^2,1/op.m3$N[3,1],0,
0,0,-op.m3$N[5,1]/op.m3$N[4,1]^2,1/op.m3$N[4,1]),3,4,byrow=TRUE)
sig<-partial%*%op.m3$cov[9:12,9:12]%*%t(partial)
cbind(estimate=growth,stderr=sqrt(diag(sig)))


###################################################
### code chunk number 30: RcaptureJSS.Rnw:806-810
###################################################
siginv<-solve(sig)
growth.e<-t(rep(1,3))%*%siginv%*%growth/(t(rep(1,3))%*%siginv%*%rep(1,3))
se<-1/sqrt(t(rep(1,3))%*%siginv%*%rep(1,3))
data.frame(estimate=growth.e,stderr=se,row.names="Common growth rate: ")


###################################################
### code chunk number 31: RcaptureJSS.Rnw:815-817
###################################################
chisq2<-t(growth-growth.e*rep(1,3))%*%siginv%*%(growth-growth.e*rep(1,3))
data.frame(stat=chisq2,pvalue=1-pchisq(chisq2,df=2),row.names="Chi-square test: ")


###################################################
### code chunk number 32: RcaptureJSS.Rnw:831-837
###################################################
data(hare)
keep<-rep(TRUE,2^6-1)
mat<-histpos.t(6)
keep[apply(mat,1,sum)==6]<-FALSE
op<-openp(hare,keep=keep)
op$model.fit[1,]


###################################################
### code chunk number 33: RcaptureJSS.Rnw:924-927
###################################################
data(mvole)
mvole.op<-periodhist(mvole,vt=rep(5,6))
op.m1<-openp(mvole.op,dfreq=TRUE)


###################################################
### code chunk number 34: RcaptureJSS.Rnw:933-936
###################################################
keep2<-residuals(op.m1$glm,type="pearson")<4
op.m2<-openp(mvole.op,dfreq=TRUE,keep=keep2)
op.m2$model.fit


###################################################
### code chunk number 35: RcaptureJSS.Rnw:946-949
###################################################
rd.m1<-robustd.0(mvole[,-10], vt=c(5,4,rep(5,4)),vm="Mh",vh="Chao")
rd.m1$model.fit
rd.m1$emig.fit


###################################################
### code chunk number 36: RcaptureJSS.Rnw:957-958
###################################################
rd.m1$emig.param


###################################################
### code chunk number 37: RcaptureJSS.Rnw:965-969
###################################################
rd.m2<-robustd.0(mvole[,-10], vt=c(5,4,rep(5,4)),vm="Mh",vh="Darroch")
rd.m2$model.fit
rd.m2$emig.fit
rd.m2$emig.param


###################################################
### code chunk number 38: RcaptureJSS.Rnw:981-982
###################################################
rd.m3<-robustd.0(mvole[,-10], vt=c(5,4,rep(5,4)),vm=c("Mh","none","Mh","Mh","Mh","Mh"),vh="Chao")


###################################################
### code chunk number 39: RcaptureJSS.Rnw:988-989
###################################################
rd.m4<-robustd.0(mvole[,-10], vt=c(5,4,rep(5,4)),vm="Mh",vh="Poisson",vtheta=1.5)


###################################################
### code chunk number 40: RcaptureJSS.Rnw:996-1006
###################################################
survivals<-data.frame(op.m1$survivals,rep("|",5),rd.m4$survivals)
N<-data.frame(op.m1$N,rep("|",6),rd.m4$N)
birth<-data.frame(op.m1$birth,rep("|",5),rd.m4$birth)
Ntot<-data.frame(op.m1$Ntot,c("|"),rd.m4$Ntot)
name<-c("estimate.open","stderr.open","|","estimate.robust","stderr.robust")
colnames(survivals)<-colnames(N)<-colnames(birth)<-colnames(Ntot)<- name
survivals
N
birth
Ntot


