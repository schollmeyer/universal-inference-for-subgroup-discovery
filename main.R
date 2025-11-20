setwd("C:/Paper_fallibilistic_regularization/Allbus2018")
library(foreign)
a  <- read.spss("ZA5270_v2-0-0.sav")
dat <- data.frame(a$sex,a$age,a$iscd11, a$J007_1,  a$incc, a$dw01, a$id02, a$J006,   a$dm02c,a$pa02,a$pk01,a$pk02,a$pk03,a$pk04,a$pk05,a$pk06,a$pk07,a$pk08,a$pk09, a$pv01,a$wghtpew,a$pe02,a$sm04,a$sm05,a$sm06,a$sm07,a$dn01,a$id05)
missing_indexs <-which( dat[,2]=="NICHT GENERIERBAR" | dat[,3]=="NICHT GENERIERBAR" | dat[,4] %in% c("KEIN ISSP", "KEIN ISSP RELIGION","KEINE ANGABE", "KANN NICHT SAGEN") | dat[,5]=="NICHT GENERIERBAR"|dat[,6] %in% c("DATENFEHLER","KEINE ANGABE")|dat[,7]%in%c("KEINER DER SCHICHTEN","KEINE ANGABE","WEISS NICHT","VERWEIGERT")|dat[,8]%in%c("KEIN ISSP","KEIN ISSP RELIGION","KEINE ANGABE","KANN NICHT SAGEN") |dat[,10]=="KEINE ANGABE"|dat[,20]=="NEUE BUNDESLAENDER"|dat[,9]=="KEINE ANGABE"|dat[,21]=="NEUE BUNDESLAENDER" | dat[,22]=="KEINE ANGABE" | dat[,22]=="WEISS NICHT" | dat[,23]=="KEINE ANGABE"| dat[,24]=="KEINE ANGABE"| dat[,25]=="KEINE ANGABE"| dat[,26]=="KEINE ANGABE" | dat[,28]=="KEINE ANGABE" | dat[,28]=="WEISS NICHT")   
dat <- dat[-missing_indexs,] 
 
 dat[,2]=factor(dat[,2],ordered=TRUE)
 dat[,3]=factor(dat[,3],ordered=TRUE)
 dat[,4]=factor(dat[,4],ordered=TRUE)
 dat[,5]=factor(dat[,5],ordered=TRUE)
 dat[,6]=factor(dat[,6],ordered=FALSE)
 dat[,7]=factor(dat[,7],ordered=TRUE)
 dat[,8]=factor(dat[,8],ordered=TRUE)
 dat[,9]=factor(dat[,9],ordered=FALSE)
 dat[,22]=factor(dat[,22],ordered=FALSE)
 dat[,23]=factor(dat[,23],ordered=FALSE)
 dat[,24]=factor(dat[,24],ordered=FALSE)
 dat[,25]=factor(dat[,25],ordered=FALSE)
 dat[,26]=factor(dat[,26],ordered=FALSE)
 dat[,27]=factor(dat[,27],ordered=FALSE)
 dat[,28]=factor(dat[,28],ordered=TRUE)

dim(dat)


NAMES=c("Geschlecht","Alter","Bildung","Vertrauen Bundestag","Einkommen", "Einordnungsberuf","Subjektive Schichteinstufung","Rollenbild","immigrant")#,"Politisches Interesse","Gewichtung",")
Z <- dat[,c((1:9))]#,(22:22))]#[-c(5)]
colnames(Z) <- NAMES[(1:9)]#[-c(5)]
CT <- oofos:::get_auto_conceptual_scaling(Z)
CT <- t(unique(t(CT)))
set.seed(1234567)
indexs=sample((1:dim(dat)[1]),size=50)
CT1 <- CT[indexs,]
write.csv2(CT1,"CT1.csv",quote=FALSE)

Lattice=oofos:::compute_concept_lattice(t(CT1))
Lattice <- list(extents=Lattice$intents,intents=Lattice$extents)
