library(ROSE)
library(rpart)
library(forecast)
library(ggplot2)
library(lattice)
library(recipes)
library(caret)
library(rpart.plot)
library(adabag)
library(gains)
library(FNN)
library(fastDummies)
library(neuralnet)
library(nnet)
library(psych)
library(skimr)
library(treemap)
library(dplyr)
library(RColorBrewer)
library(viridis)
library(hrbrthemes)
library(GGally)
library(gplots)
library(pROC)
library(randomForest)

#PREPARE DATA
main.data<-read.table("E:\\Users\\ASUS\\Documents\\term7\\data mining\\prozhe nahaee dade kavi\\data\\my Jalili_data.csv",
                header = T,sep = ",")
main.data=data.frame(main.data)
quantile(main.data$DARAMAD.KHALES,probs = 0.9)
DARAMAD.KHALES=ifelse(main.data$DARAMAD.KHALES>137616667,1,0)
table(DARAMAD.KHALES)

#NUMERIC DATA
SEN.S <- as.numeric((main.data$SEN.S))
S.Z <- as.numeric((main.data$S.Z))
T.O <- as.numeric((main.data$T.O))
H.KHORAKI.NOOSHIDANI <- as.numeric((main.data$H.KHORAKI.NOOSHIDANI))
H.ERTEBATAT <- as.numeric((main.data$H.ERTEBATAT))
H.BEHDASHT <- as.numeric((main.data$H.BEHDASHT))
H.HAMLONAGHL <- as.numeric((main.data$H.HAMLONAGHL))
H.KALA.MOT <- as.numeric((main.data$H.KALA.MOT))
H.MASKAN <- as.numeric((main.data$H.MASKAN))
H.MOBLEMAN <- as.numeric((main.data$H.MOBLEMAN))
H.POOSHAK <- as.numeric((main.data$H.POOSHAK))
KHARID.KALA.BADAVAM <- as.numeric((main.data$KHARID.KALA.BADAVAM))
H.SARMAYEGOZARI <- as.numeric((main.data$H.SARMAYEGOZARI))

#CATEGORIC DATA
DARAMAD.KHALES <- as.factor((DARAMAD.KHALES))
C.SH <- as.factor((main.data$C.SH))
JENS.S <- as.factor((main.data$JENS.S))
SAVAD.S <- as.factor((main.data$SAVAD.S))
M.T.S <- as.factor((main.data$M.T.S))
V.F.S <- as.factor((main.data$V.F.S))
N.T.M <- as.factor((main.data$N.T.M))
M.O.B <- as.factor((main.data$M.O.B))
OTO <- as.factor((main.data$OTO))
MO <- as.factor((main.data$MO))
DO <- as.factor((main.data$DO))
ZABT <- as.factor((main.data$ZABT))
TV <- as.factor((main.data$TV))
PC <- as.factor((main.data$PC))
TEL.H <- as.factor((main.data$TEL.H))
OJAGH.GAZ <- as.factor((main.data$OJAGH.GAZ))
M.LEBAS <- as.factor((main.data$M.LEBAS))
JAROO.B <- as.factor((main.data$JAROO.B))
M.ZARF <- as.factor((main.data$M.ZARF))
PANKE <- as.factor((main.data$PANKE))
CHARKH.KH <- as.factor((main.data$CHARKH.KH))

new.data=data.frame(C.SH,JENS.S,SEN.S,SAVAD.S,M.T.S,V.F.S,N.T.M,T.O,S.Z,M.O.B,
                    OTO,MO,DO,ZABT,TV,PC,TEL.H,OJAGH.GAZ,JAROO.B,M.LEBAS,CHARKH.KH,
                    PANKE,M.ZARF,H.KHORAKI.NOOSHIDANI,H.ERTEBATAT,H.BEHDASHT,
                    H.HAMLONAGHL,H.KALA.MOT,H.MASKAN,H.MOBLEMAN,H.POOSHAK,
                    KHARID.KALA.BADAVAM,H.SARMAYEGOZARI,DARAMAD.KHALES)
attach(new.data)
skim(new.data)
summary(new.data)
options(scipen = 99)

#visualization data

#sen.s
new.data %>%
  ggplot(aes(x=SEN.S)) +
  geom_density(fill="blueviolet", color="aquamarine4", alpha=0.5) +
  ggtitle("AGE") +
  scale_x_continuous(breaks=seq(23,96,10))

#t.o (tedade otagh)
hist(T.O,col = "pink", border = NULL,
     main = paste("N.ROOMS"))

#s.z (masahate zamin)
new.data %>%
  ggplot(aes(x=S.Z)) +
  geom_density(fill="blue3", color="blue1", alpha=0.5) +
  ggtitle("AREA") +
  scale_x_continuous(breaks=seq(21,300,30))

#hazine ha
hazine.data=new.data[24:33]
heatmap.2(cor(hazine.data),Rowv = FALSE,Colv = FALSE,dendrogram = "none",
          cellnote = round(cor(hazine.data),2),notecol = "black",
          key = FALSE,trace = 'none',margins = c(10,10))
ggpairs(hazine.data)

#gender
gender_data <- data.frame(
  group=c("men","women"),
  value=c(sum(JENS.S=="1"),sum(JENS.S=="2"))
)
ggplot(gender_data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title = "GENDER")

#m.t.s (madrak tahsili sarparast)
madrak_data <- data.frame(
  group=c("Bisavad","Ebtedai","Rahnamai","Motevasete","Diplom.Pishdaneshgahi",
          "Foghe.Diplom","Lisans","Foghe.lisans.Dr.herfei",
          "Dr.takhasosi","Savad.amuzi.gheyre.rasmi"),
  value=c(as.numeric(table(M.T.S)))
)
treemap(madrak_data,
        index="group",
        vSize="value",
        type="index"
)

#savad.s
savad_data <- data.frame(
  group=c("basavad","bisavad"),
  value=c(sum(SAVAD.S=="1"),sum(SAVAD.S=="2"))
)
ggplot(savad_data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()

#v.f.s (vaziate feli sarparast)
job.data <- data.frame(
  category=c("Shaghel","Bikar","Daramad.bedunekar",
             "Khanedar","Sayer"),
  count=c(sum(V.F.S=="1"),sum(V.F.S=="2"),
          sum(V.F.S=="3"),
          sum(V.F.S=="5"),sum(V.F.S=="6"))
)
job.data$fraction <- job.data$count / sum(job.data$count)
job.data$ymax <- cumsum(job.data$fraction)
job.data$ymin <- c(0, head(job.data$ymax, n=-1))
job.data$labelPosition <- (job.data$ymax + job.data$ymin) / 2
job.data$label <- paste0(job.data$category,"\n value: ",
                         job.data$count)
ggplot(job.data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3,
                        fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
  coord_polar(theta="y") +
  xlim(c(2,4)) +
  theme_void() +
  theme(legend.position = "none")

#n.t.m (nahve tasarofe manzel)
manzel_data <- data.frame(
  group=c("Melki.arse.ayan","Ejare","Rahn","Darbarabare.khedmat",
          "Raygan","Sayer"),
  value=c(as.numeric(table(N.T.M)/1921))
)
treemap(manzel_data,
        index="group",
        vSize="value",
        type="index"
)

#m.o.b (masalehe omde bana)
masaleh_data <- data.frame(
  group=c("Tamam.chub/Khesht.gel","Ajor.ahan","Ajor.chub","Block.simani",
          "Tamam.ajor","Khesht.chub","Sayer"),
  value=c(as.numeric(table(M.O.B)))
)
treemap(masaleh_data,
        index="group",
        vSize="value",
        type="index"
)

#SEN.S VS DARAMAD.KHALES
ggplot(new.data, aes(x = DARAMAD.KHALES, y = SEN.S, fill = DARAMAD.KHALES)) +
  geom_boxplot(color="red", fill="orange", alpha=0.2) +
  theme(legend.position = "none")+
  ggtitle("SEN.S VS DARAMAD.KHALES") +
  scale_x_discrete(labels = c('paeentar az sadak 90','balatar az sadak 90'))

#S.Z VS DARAMAD.KHALES
ggplot(new.data, aes(x = DARAMAD.KHALES, y = S.Z, fill = DARAMAD.KHALES)) +
  geom_boxplot(color="cadetblue", fill="aliceblue", alpha=0.2) +
  theme(legend.position = "none")+
  ggtitle("S.Z VS DARAMAD.KHALES") +
  scale_x_discrete(labels = c('paeentar az sadak 90','balatar az sadak 90'))

#T.O VS DARAMAD.KHALES
ggplot(new.data, aes(x = DARAMAD.KHALES, y = T.O, fill = DARAMAD.KHALES)) +
  geom_boxplot(color="chartreuse4", fill="chartreuse3", alpha=0.2) +
  theme(legend.position = "none")+
  ggtitle("T.O VS DARAMAD.KHALES") +
  scale_x_discrete(labels = c('paeentar az sadak 90','balatar az sadak 90'))

#H.KHORAKI.NOOSHIDANI VS DARAMAD.KHALES
ggplot(new.data, aes(x = DARAMAD.KHALES, y = log(H.KHORAKI.NOOSHIDANI),
                     fill = DARAMAD.KHALES)) +
  geom_boxplot(color="antiquewhite4", fill="antiquewhite2", alpha=0.2) +
  theme(legend.position = "none")+
  ggtitle("H.KHORAKI.NOOSHIDANI VS DARAMAD.KHALES") +
  scale_x_discrete(labels = c('paeentar az sadak 90','balatar az sadak 90'))

#H.ERTEBATAT VS DARAMAD.KHALES
ggplot(new.data, aes(x = DARAMAD.KHALES, y = log(H.ERTEBATAT),
                     fill = DARAMAD.KHALES)) +
  geom_boxplot(color="aquamarine4", fill="aquamarine2", alpha=0.2) +
  theme(legend.position = "none")+
  ggtitle("H.ERTEBATAT VS DARAMAD.KHALES") +
  scale_x_discrete(labels = c('paeentar az sadak 90','balatar az sadak 90'))

#H.HAMLONAGHL VS DARAMAD.KHALES
ggplot(new.data, aes(x = DARAMAD.KHALES, y = log(H.HAMLONAGHL),
                     fill = DARAMAD.KHALES)) +
  geom_boxplot(color="blueviolet", fill="darkmagenta", alpha=0.2) +
  theme(legend.position = "none")+
  ggtitle("H.HAMLONAGHL VS DARAMAD.KHALES") +
  scale_x_discrete(labels = c('paeentar az sadak 90','balatar az sadak 90'))

#H.BEHDASHT VS DARAMAD.KHALES
ggplot(new.data, aes(x = DARAMAD.KHALES, y = log(H.BEHDASHT),
                     fill = DARAMAD.KHALES)) +
  geom_boxplot(color="blue3", fill="blue", alpha=0.2) +
  theme(legend.position = "none")+
  ggtitle("H.BEHDASHT VS DARAMAD.KHALES") +
  scale_x_discrete(labels = c('paeentar az sadak 90','balatar az sadak 90'))

#H.KALA.MOT VS DARAMAD.KHALES
ggplot(new.data, aes(x = DARAMAD.KHALES, y = log(H.KALA.MOT),
                     fill = DARAMAD.KHALES)) +
  geom_boxplot(color="darkgoldenrod3", fill="darkgoldenrod1", alpha=0.2) +
  theme(legend.position = "none")+
  ggtitle("H.KALA.MOT VS DARAMAD.KHALES") +
  scale_x_discrete(labels = c('paeentar az sadak 90','balatar az sadak 90'))

#H.MASKAN VS DARAMAD.KHALES
ggplot(new.data, aes(x = DARAMAD.KHALES, y = log(H.MASKAN),
                     fill = DARAMAD.KHALES)) +
  geom_boxplot(color="brown3", fill="brown1", alpha=0.2) +
  theme(legend.position = "none")+
  ggtitle("H.MASKAN VS DARAMAD.KHALES") +
  scale_x_discrete(labels = c('paeentar az sadak 90','balatar az sadak 90'))

#H.MOBLEMAN VS DARAMAD.KHALES
ggplot(new.data, aes(x = DARAMAD.KHALES, y = log(H.MOBLEMAN),
                     fill = DARAMAD.KHALES)) +
  geom_boxplot(color="gold2", fill="gold", alpha=0.2) +
  theme(legend.position = "none")+
  ggtitle("H.MOBLEMAN VS DARAMAD.KHALES") +
  scale_x_discrete(labels = c('paeentar az sadak 90','balatar az sadak 90'))

#H.POOSHAK VS DARAMAD.KHALES
ggplot(new.data, aes(x = DARAMAD.KHALES, y = log(H.POOSHAK),
                     fill = DARAMAD.KHALES)) +
  geom_boxplot(color="red", fill="orange", alpha=0.2) +
  theme(legend.position = "none")+
  ggtitle("H.POOSHAK VS DARAMAD.KHALES") +
  scale_x_discrete(labels = c('paeentar az sadak 90','balatar az sadak 90'))

#KHARID.KALA.BADAVAM VS DARAMAD.KHALES
ggplot(new.data, aes(x = DARAMAD.KHALES, y = log(KHARID.KALA.BADAVAM),
                     fill = DARAMAD.KHALES)) +
  geom_boxplot(color="deeppink3", fill="deeppink", alpha=0.2) +
  theme(legend.position = "none")+
  ggtitle("KHARID.KALA.BADAVAM VS DARAMAD.KHALES") +
  scale_x_discrete(labels = c('paeentar az sadak 90','balatar az sadak 90'))

#H.SARMAYEGOZARI VS DARAMAD.KHALES
ggplot(new.data, aes(x = DARAMAD.KHALES, y = log(H.SARMAYEGOZARI),
                     fill = DARAMAD.KHALES)) +
  geom_boxplot(color="darkseagreen3", fill="darkseagreen", alpha=0.2) +
  theme(legend.position = "none")+
  ggtitle("H.SARMAYEGOZARI VS DARAMAD.KHALES") +
  scale_x_discrete(labels = c('paeentar az sadak 90','balatar az sadak 90'))

#DARAMAD.KHALES vs C.SH
sh_response=table(DARAMAD.KHALES,C.SH)
barplot(sh_response,
        legend.text = TRUE,
        beside = TRUE,
        main = "DARAMAD.KHALES vs C.SH",
        xlab = "",
        ylab = "",col=c("pink","green"),
        horiz=T, las=1,
        names.arg=c("code 1","code 3","code 24")
)

#DARAMAD.KHALES vs JENS.S
jens_response=table(DARAMAD.KHALES,JENS.S)
barplot(jens_response,
        legend.text = TRUE,
        beside = TRUE,
        main = "DARAMAD.KHALES vs JENS.S",
        xlab = "",
        ylab = "",col=c("chocolate","chartreuse3"),
        horiz=T, las=1,
        names.arg=c("mard","zan")
)

#DARAMAD.KHALES vs SAVAD.S
savad_response=table(DARAMAD.KHALES,SAVAD.S)
barplot(savad_response,
        legend.text = TRUE,
        beside = TRUE,
        main = "DARAMAD.KHALES vs SAVAD.S",
        xlab = "",
        ylab = "",col=c("cyan","blue3"),
        horiz=T, las=1,
        names.arg=c("ba savad","bi savad")
)

#DARAMAD.KHALES vs M.T.S
mts_response=table(DARAMAD.KHALES,M.T.S)
barplot(mts_response,
        legend.text = TRUE,
        beside = TRUE,
        main = "DARAMAD.KHALES vs M.T.S",
        xlab = "",
        ylab = "",col=c("darkorchid4","darkorchid1"),
        horiz=T, las=1,
        names.arg=c("Bisavad",
                    "Ebtedai",
                    "Rahnamai",
                    "Motevasete",
                    "Diplom.Pishdaneshgahi",
                    "Foghe.Diplom",
                    "Lisans",
                    "Foghe.lisans.Dr.herfei",
                    "Dr.takhasosi",
                    "Savad.amuzi.gheyre.rasmi")
)

#DARAMAD.KHALES vs V.F.S
vfs_response=table(DARAMAD.KHALES,V.F.S)
barplot(vfs_response,
        legend.text = TRUE,
        beside = TRUE,
        main = "DARAMAD.KHALES vs V.F.S",
        xlab = "",
        ylab = "",col=c("gold2","darkseagreen1"),
        horiz=T, las=1,
        names.arg=c("Shaghel",
                    "Bikar",
                    "Daramad.bedunekar",
                    "Khanedar",
                    "Sayer")
)

#DARAMAD.KHALES vs N.T.M
ntm_response=table(DARAMAD.KHALES,N.T.M)
barplot(ntm_response,
        legend.text = TRUE,
        beside = TRUE,
        main = "DARAMAD.KHALES vs N.T.M",
        xlab = "",
        ylab = "",col=c("deeppink","darkturquoise"),
        horiz=T, las=1,
        names.arg=c("Melki.arse.ayan",
                    "Ejare",
                    "Rahn",
                    "Darbarabare.khedmat",
                    "Raygan",
                    "Sayer")
)

#DARAMAD.KHALES vs M.O.B
mob_response=table(DARAMAD.KHALES,M.O.B)
barplot(mob_response,
        legend.text = TRUE,
        beside = TRUE,
        main = "DARAMAD.KHALES vs M.O.B",
        xlab = "",
        ylab = "",col=c("lavenderblush","magenta3"),
        horiz=T, las=1,
        names.arg=c("Tamam.chub/Khesht.gel",
                    "Ajor.ahan",
                    "Ajor.chub",
                    "Block.simani",
                    "Tamam.ajor",
                    "Khesht.chub",
                    "Sayer")
)

#H.ERTEBATAT and H.SARMAYEGOZARI vs DARAMAD.KHALES
par(xpd=TRUE)
plot(log(H.ERTEBATAT),log(H.SARMAYEGOZARI),
     ylab = "H.ERTEBATAT",xlab = "H.SARMAYEGOZARI",
     col=ifelse(DARAMAD.KHALES==1,"maroon2","royalblue4"),pch=20)
legend("topleft",inset = c(0,-0.2),
       legend = c("daramad balatar az sadak 90","daramad paeentar az sadak 90"),
       col = c("maroon2","royalblue4"),pch=20,cex=1)

#H.KALA.MOT and H.SARMAYEGOZARI vs DARAMAD.KHALES
par(xpd=TRUE)
plot(log(H.KALA.MOT),log(H.SARMAYEGOZARI),
     ylab = "H.KALA.MOT",xlab = "H.SARMAYEGOZARI",
     col=ifelse(DARAMAD.KHALES==1,"olivedrab4","lightsalmon1"),pch=20)
legend("topleft",inset = c(0,-0.2),
       legend = c("daramad balatar az sadak 90","daramad paeentar az sadak 90"),
       col = c("olivedrab4","lightsalmon1"),pch=20,cex=1)

#H.POOSHAK and H.KHORAKI.NOOSHIDANI vs DARAMAD.KHALES
par(xpd=TRUE)
plot(log(H.POOSHAK),log(H.KHORAKI.NOOSHIDANI),
     ylab = "H.POOSHAK",xlab = "H.KHORAKI.NOOSHIDANI",
     col=ifelse(DARAMAD.KHALES==1,"orange2","olivedrab3"),pch=20)
legend("topleft",inset = c(0,-0.2),
       legend = c("daramad balatar az sadak 90","daramad paeentar az sadak 90"),
       col = c("orange2","olivedrab3"),pch=20,cex=1)

#H.KALA.MOT and H.MOBLEMAN vs DARAMAD.KHALES
par(xpd=TRUE)
plot(log(H.KALA.MOT),log(H.MOBLEMAN),
     ylab = "H.KALA.MOT",xlab = "H.MOBLEMAN",
     col=ifelse(DARAMAD.KHALES==1,"orangered","black"),pch=20)
legend("topleft",inset = c(0,-0.2),
       legend = c("daramad balatar az sadak 90","daramad paeentar az sadak 90"),
       col = c("orangered","black"),pch=20,cex=1)

#H.MASKAN and H.KHORAKI.NOOSHIDANI vs DARAMAD.KHALES
par(xpd=TRUE)
plot(log(H.MASKAN),log(H.KHORAKI.NOOSHIDANI),
     ylab = "H.MASKAN",xlab = "H.KHORAKI.NOOSHIDANI",
     col=ifelse(DARAMAD.KHALES==1,"seagreen","palevioletred3"),pch=20)
legend("topleft",inset = c(0,-0.2),
       legend = c("daramad balatar az sadak 90","daramad paeentar az sadak 90"),
       col = c("seagreen","palevioletred3"),pch=20,cex=1)

#definition train and validation and test data
set.seed(42)
train_rows=sample(nrow(new.data),1500)
train_data=new.data[train_rows,]
table(train_data$DARAMAD.KHALES)
validation_rows=sample(setdiff(row.names(new.data),train_rows),300)
validation_data=new.data[validation_rows,]
table(validation_data$DARAMAD.KHALES)
test_rows=setdiff(row.names(new.data),c(train_rows,validation_rows))
test_data=new.data[test_rows,]
table(test_data$DARAMAD.KHALES)

#imbalanced data method
data.rose <- ovun.sample(DARAMAD.KHALES ~ ., data = train_data,
                         method = "both", p=0.5,N=10000, seed=1)$data
table(data.rose$DARAMAD.KHALES)

#some edits on validation data
new_validation_data <- validation_data
new_validation_data$
  M.O.B[which(!(new_validation_data$M.O.B %in% unique(data.rose$M.O.B)))] <- NA

#logistic regression models
logit.reg.train=glm(DARAMAD.KHALES~.,data=train_data,family="binomial")
logit.reg.train.predict=predict(logit.reg.train,new_validation_data,
                               type="response")
confusionMatrix(
  data=as.factor(ifelse(logit.reg.train.predict>0.5,1,0)),
  reference=new_validation_data$DARAMAD.KHALES)

#best logistic model
logit.reg.rose=glm(DARAMAD.KHALES~.,data=data.rose,family="binomial")
#data.frame(summary(logit.reg.rose)$coefficients,
#           odds=exp(coef(logit.reg.rose)))
#round(data.frame(summary(logit.reg.rose)$coefficients,
#                 odds=exp(coef(logit.reg.rose))),5)
logit.reg.rose.predict=predict(logit.reg.rose,new_validation_data,
                               type="response")
confusionMatrix(
  data=as.factor(ifelse(logit.reg.rose.predict>0.5,1,0)),
  reference=new_validation_data$DARAMAD.KHALES)

#over fitting check
logit.reg.rose.predict.train=predict(logit.reg.rose,data.rose,
                                     type="response")
confusionMatrix(
  data=as.factor(ifelse(logit.reg.rose.predict.train>0.5,1,0)),
  reference=data.rose$DARAMAD.KHALES)

#step methods
logit.reg.back=logit.reg.rose %>% stats::step(direction = "backward")
logit.reg.for=logit.reg.rose %>% stats::step(direction = "forward")
logit.reg.both=logit.reg.rose %>% stats::step(direction = "both")
logit.reg.back.predict=predict(logit.reg.back,new_validation_data,
                               type="response")
confusionMatrix(
  data=as.factor(ifelse(logit.reg.back.predict>0.5,1,0)),
  reference=new_validation_data$DARAMAD.KHALES)
logit.reg.for.predict=predict(logit.reg.for,new_validation_data,
                               type="response")
confusionMatrix(
  data=as.factor(ifelse(logit.reg.for.predict>0.5,1,0)),
  reference=validation_data$DARAMAD.KHALES)
logit.reg.both.predict=predict(logit.reg.both,new_validation_data,
                               type="response")
confusionMatrix(
  data=as.factor(ifelse(logit.reg.both.predict>0.5,1,0)),
  reference=new_validation_data$DARAMAD.KHALES)

#lift chart
logit.reg.lift=lift(relevel(as.factor(DARAMAD.KHALES),ref="1")~
                      logit.reg.back.predict,data=new_validation_data)
xyplot(logit.reg.lift,plot="gain")

#decile lift chart
gain=gains(as.numeric(new_validation_data$DARAMAD.KHALES),logit.reg.back.predict)
heights=gain$mean.resp/mean(as.numeric(new_validation_data$DARAMAD.KHALES))
decile.chart=barplot(heights,names.arg=gain$depth,
                     xlab="percentile",ylab="mean response")

#roc plot
r=roc(new_validation_data$DARAMAD.KHALES,logit.reg.back.predict)
plot.roc(r)
auc(r)

#decision tree model
class.tree=rpart(DARAMAD.KHALES~.,data=data.rose,method="class")
class.tree.plot=prp(class.tree,type=1,extra=1,split.font=1,varlen=-10,
                    box.col=ifelse(class.tree$frame$var=="<leaf>",'gray','white'))
class.tree.pred.train=predict(class.tree,data.rose,type="class")
confusionMatrix(class.tree.pred.train,data.rose$DARAMAD.KHALES)
class.tree.pred.valid=predict(class.tree,validation_data,type="class")
confusionMatrix(class.tree.pred.valid,validation_data$DARAMAD.KHALES)
roc.curve(validation_data$DARAMAD.KHALES,class.tree.pred.valid, plotit = T)

#random forest
rf=randomForest(DARAMAD.KHALES~.,data=data.rose,ntree=1000,
                mtry=4,nodesize=10,importance=TRUE)
varImpPlot(rf,type=1)
rf.pred=predict(rf,validation_data)
confusionMatrix(rf.pred,validation_data$DARAMAD.KHALES)

#knn
#preparation data for knn model
#train knn
summary(logit.reg.rose)
summary(class.tree)
knn.data=data.frame(sh=C.SH,
                    lebas=M.LEBAS,
                    ojagh=OJAGH.GAZ,
                    charkh=CHARKH.KH,
                    jaroo=JAROO.B,
                    pc=PC,
                    tv=TV,
                    mo=MO,
                    jens=JENS.S,
                    daramad=DARAMAD.KHALES,
                    ntm=N.T.M,
                    vfs=V.F.S,
                    do=DO,
                    zabt=ZABT,
                    panke=PANKE,
                    sen=SEN.S,
                    haml=H.HAMLONAGHL,
                    sz=S.Z,
                    khoraki=H.KHORAKI.NOOSHIDANI,
                    behdasht=H.BEHDASHT,
                    ertebat=H.ERTEBATAT,
                    maskan=H.MASKAN,
                    mobleman=H.MOBLEMAN,
                    pooshak=H.POOSHAK,
                    kala=KHARID.KALA.BADAVAM,
                    sarmaye=H.SARMAYEGOZARI)
NEW.C.SH.TRAIN.KNN=dummy_cols(data.rose$C.SH,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.PANKE.TRAIN.KNN=dummy_cols(data.rose$PANKE,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.ZABT.TRAIN.KNN=dummy_cols(data.rose$ZABT,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.DO.TRAIN.KNN=dummy_cols(data.rose$DO,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.V.F.S.TRAIN.KNN=dummy_cols(data.rose$V.F.S,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.N.T.M.TRAIN.KNN=dummy_cols(data.rose$N.T.M,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.M.O.B.TRAIN.KNN=dummy_cols(data.rose$M.O.B,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.JENS.TRAIN.KNN=dummy_cols(data.rose$JENS.S,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.MO.TRAIN.KNN=dummy_cols(data.rose$MO,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.TV.TRAIN.KNN=dummy_cols(data.rose$TV,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.PC.TRAIN.KNN=dummy_cols(data.rose$PC,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.JAROO.TRAIN.KNN=dummy_cols(data.rose$JAROO.B,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.CHARKH.TRAIN.KNN=dummy_cols(data.rose$CHARKH.KH,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.OJAGH.TRAIN.KNN=dummy_cols(data.rose$OJAGH.GAZ,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.LEBAS.TRAIN.KNN=dummy_cols(data.rose$M.LEBAS,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
train.knn=data.frame(sh=NEW.C.SH.TRAIN.KNN,
                     lebas=NEW.LEBAS.TRAIN.KNN,
                     ojagh=NEW.OJAGH.TRAIN.KNN,
                     charkh=NEW.CHARKH.TRAIN.KNN,
                     jaroo=NEW.JAROO.TRAIN.KNN,
                     pc=NEW.PC.TRAIN.KNN,
                     tv=NEW.TV.TRAIN.KNN,
                     mo=NEW.MO.TRAIN.KNN,
                     jens=NEW.JENS.TRAIN.KNN,
                     daramad=data.rose$DARAMAD.KHALES,
                     ntm=NEW.N.T.M.TRAIN.KNN,
                     vfs=NEW.V.F.S.TRAIN.KNN,
                     do=NEW.DO.TRAIN.KNN,
                     zabt=NEW.ZABT.TRAIN.KNN,
                     panke=NEW.PANKE.TRAIN.KNN,
                     sen=data.rose$SEN.S,
                     haml=data.rose$H.HAMLONAGHL,
                     sz=data.rose$S.Z,
                     khoraki=data.rose$H.KHORAKI.NOOSHIDANI,
                     behdasht=data.rose$H.BEHDASHT,
                     ertebat=data.rose$H.ERTEBATAT,
                     maskan=data.rose$H.MASKAN,
                     mobleman=data.rose$H.MOBLEMAN,
                     pooshak=data.rose$H.POOSHAK,
                     kala=data.rose$KHARID.KALA.BADAVAM,
                     sarmaye=data.rose$H.SARMAYEGOZARI)

#validation knn
NEW.C.SH.VALID.KNN=dummy_cols(validation_data$C.SH,remove_selected_columns = TRUE,
                              ignore_na = TRUE)
NEW.PANKE.VALID.KNN=dummy_cols(validation_data$PANKE,remove_selected_columns = TRUE,
                               ignore_na = TRUE)
NEW.ZABT.VALID.KNN=dummy_cols(validation_data$ZABT,remove_selected_columns = TRUE,
                              ignore_na = TRUE)
NEW.DO.VALID.KNN=dummy_cols(validation_data$DO,remove_selected_columns = TRUE,
                            ignore_na = TRUE)
NEW.V.F.S.VALID.KNN=dummy_cols(validation_data$V.F.S,remove_selected_columns = TRUE,
                               ignore_na = TRUE)
NEW.N.T.M.VALID.KNN=dummy_cols(validation_data$N.T.M,remove_selected_columns = TRUE,
                               ignore_na = TRUE)
NEW.M.O.B.VALID.KNN=dummy_cols(validation_data$M.O.B,remove_selected_columns = TRUE,
                               ignore_na = TRUE)
NEW.JENS.VALID.KNN=dummy_cols(validation_data$JENS.S,remove_selected_columns = TRUE,
                              ignore_na = TRUE)
NEW.MO.VALID.KNN=dummy_cols(validation_data$MO,remove_selected_columns = TRUE,
                            ignore_na = TRUE)
NEW.TV.VALID.KNN=dummy_cols(validation_data$TV,remove_selected_columns = TRUE,
                            ignore_na = TRUE)
NEW.PC.VALID.KNN=dummy_cols(validation_data$PC,remove_selected_columns = TRUE,
                            ignore_na = TRUE)
NEW.JAROO.VALID.KNN=dummy_cols(validation_data$JAROO.B,remove_selected_columns = TRUE,
                               ignore_na = TRUE)
NEW.CHARKH.VALID.KNN=dummy_cols(validation_data$CHARKH.KH,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.OJAGH.VALID.KNN=dummy_cols(validation_data$OJAGH.GAZ,remove_selected_columns = TRUE,
                               ignore_na = TRUE)
NEW.LEBAS.VALID.KNN=dummy_cols(validation_data$M.LEBAS,remove_selected_columns = TRUE,
                               ignore_na = TRUE)
valid.knn=data.frame(sh=NEW.C.SH.VALID.KNN,
                     lebas=NEW.LEBAS.VALID.KNN,
                     ojagh=NEW.OJAGH.VALID.KNN,
                     charkh=NEW.CHARKH.VALID.KNN,
                     jaroo=NEW.JAROO.VALID.KNN,
                     pc=NEW.PC.VALID.KNN,
                     tv=NEW.TV.VALID.KNN,
                     mo=NEW.MO.VALID.KNN,
                     jens=NEW.JENS.VALID.KNN,
                     daramad=validation_data$DARAMAD.KHALES,
                     ntm=NEW.N.T.M.VALID.KNN,
                     vfs=NEW.V.F.S.VALID.KNN,
                     do=NEW.DO.VALID.KNN,
                     zabt=NEW.ZABT.VALID.KNN,
                     panke=NEW.PANKE.VALID.KNN,
                     sen=validation_data$SEN.S,
                     haml=validation_data$H.HAMLONAGHL,
                     sz=validation_data$S.Z,
                     khoraki=validation_data$H.KHORAKI.NOOSHIDANI,
                     behdasht=validation_data$H.BEHDASHT,
                     ertebat=validation_data$H.ERTEBATAT,
                     maskan=validation_data$H.MASKAN,
                     mobleman=validation_data$H.MOBLEMAN,
                     pooshak=validation_data$H.POOSHAK,
                     kala=validation_data$KHARID.KALA.BADAVAM,
                     sarmaye=validation_data$H.SARMAYEGOZARI)

#test knn
NEW.C.SH.TEST.KNN=dummy_cols(test_data$C.SH,remove_selected_columns = TRUE,
                              ignore_na = TRUE)
NEW.PANKE.TEST.KNN=dummy_cols(test_data$PANKE,remove_selected_columns = TRUE,
                               ignore_na = TRUE)
NEW.ZABT.TEST.KNN=dummy_cols(test_data$ZABT,remove_selected_columns = TRUE,
                              ignore_na = TRUE)
NEW.DO.TEST.KNN=dummy_cols(test_data$DO,remove_selected_columns = TRUE,
                            ignore_na = TRUE)
NEW.V.F.S.TEST.KNN=dummy_cols(test_data$V.F.S,remove_selected_columns = TRUE,
                               ignore_na = TRUE)
NEW.N.T.M.TEST.KNN=dummy_cols(test_data$N.T.M,remove_selected_columns = TRUE,
                               ignore_na = TRUE)
NEW.M.O.B.TEST.KNN=dummy_cols(test_data$M.O.B,remove_selected_columns = TRUE,
                               ignore_na = TRUE)
NEW.JENS.TEST.KNN=dummy_cols(test_data$JENS.S,remove_selected_columns = TRUE,
                              ignore_na = TRUE)
NEW.MO.TEST.KNN=dummy_cols(test_data$MO,remove_selected_columns = TRUE,
                            ignore_na = TRUE)
NEW.TV.TEST.KNN=dummy_cols(test_data$TV,remove_selected_columns = TRUE,
                            ignore_na = TRUE)
NEW.PC.TEST.KNN=dummy_cols(test_data$PC,remove_selected_columns = TRUE,
                            ignore_na = TRUE)
NEW.JAROO.TEST.KNN=dummy_cols(test_data$JAROO.B,remove_selected_columns = TRUE,
                               ignore_na = TRUE)
NEW.CHARKH.TEST.KNN=dummy_cols(test_data$CHARKH.KH,remove_selected_columns = TRUE,
                                ignore_na = TRUE)
NEW.OJAGH.TEST.KNN=dummy_cols(test_data$OJAGH.GAZ,remove_selected_columns = TRUE,
                               ignore_na = TRUE)
NEW.LEBAS.TEST.KNN=dummy_cols(test_data$M.LEBAS,remove_selected_columns = TRUE,
                               ignore_na = TRUE)
test.knn=data.frame(sh=NEW.C.SH.TEST.KNN,
                    lebas=NEW.LEBAS.TEST.KNN,
                    ojagh=NEW.OJAGH.TEST.KNN,
                    charkh=NEW.CHARKH.TEST.KNN,
                    jaroo=NEW.JAROO.TEST.KNN,
                    pc=NEW.PC.TEST.KNN,
                    tv=NEW.TV.TEST.KNN,
                    mo=NEW.MO.TEST.KNN,
                    jens=NEW.JENS.TEST.KNN,
                    daramad=test_data$DARAMAD.KHALES,
                    ntm=NEW.N.T.M.TEST.KNN,
                    vfs=NEW.V.F.S.TEST.KNN,
                    do=NEW.DO.TEST.KNN,
                    zabt=NEW.ZABT.TEST.KNN,
                    panke=NEW.PANKE.TEST.KNN,
                    sen=test_data$SEN.S,
                    haml=test_data$H.HAMLONAGHL,
                    sz=test_data$S.Z,
                    khoraki=test_data$H.KHORAKI.NOOSHIDANI,
                    behdasht=test_data$H.BEHDASHT,
                    ertebat=test_data$H.ERTEBATAT,
                    maskan=test_data$H.MASKAN,
                    mobleman=test_data$H.MOBLEMAN,
                    pooshak=test_data$H.POOSHAK,
                    kala=test_data$KHARID.KALA.BADAVAM,
                    sarmaye=test_data$H.SARMAYEGOZARI)

#knn model
train.norm.df=train.knn
valid.norm.df=valid.knn
general.norm.df=knn.data
norm.values=preProcess(train.knn[,-20],method = c("center","scale"))
train.norm.df[,-20]=predict(norm.values,train.knn[,-20])
valid.norm.df[,-20]=predict(norm.values,valid.knn[,-20])
general.norm.df[,-10]=predict(norm.values,knn.data[,-10])
new.norm.df=predict(norm.values,test.knn[,-20])
nn=knn(train=train.norm.df[,-20],test=new.norm.df,
       cl=train.norm.df[,20],k=3)
row.names(train.knn)[attr(nn,"nn.index")]
accuracy.df=data.frame(k=seq(1,14,1),accuracy=rep(0,14))
for (i in 1:14) {
  knn.pred=knn(train.norm.df[,-20],valid.norm.df[,-20],
               cl=train.norm.df[,20],k=i)
  accuracy.df[i,2]=confusionMatrix(knn.pred,valid.norm.df[,20])$overall[1]
}
knn.pred=knn(train.norm.df[,-20],valid.norm.df[,-20],
             cl=train.norm.df[,20],k=14)
confusionMatrix(knn.pred,valid.norm.df[,20])
knn.pred.new=knn(general.norm.df[,-10],new.norm.df,
                 cl=general.norm.df[,10],k=3)
row.names(train.norm.df)[attr(nn,"nn.index")]

#neural net model
NEURAL.RESPONSE=dummy_cols(data.rose$DARAMAD.KHALES,
                           remove_selected_columns = TRUE,ignore_na = TRUE)
train.nn=data.frame(sh=NEW.C.SH.TRAIN.KNN,
                    lebas=NEW.LEBAS.TRAIN.KNN,
                    ojagh=NEW.OJAGH.TRAIN.KNN,
                    charkh=NEW.CHARKH.TRAIN.KNN,
                    jaroo=NEW.JAROO.TRAIN.KNN,
                    pc=NEW.PC.TRAIN.KNN,
                    tv=NEW.TV.TRAIN.KNN,
                    mo=NEW.MO.TRAIN.KNN,
                    jens=NEW.JENS.TRAIN.KNN,
                    daramad=NEURAL.RESPONSE,
                    ntm=NEW.N.T.M.TRAIN.KNN,
                    vfs=NEW.V.F.S.TRAIN.KNN,
                    do=NEW.DO.TRAIN.KNN,
                    zabt=NEW.ZABT.TRAIN.KNN,
                    panke=NEW.PANKE.TRAIN.KNN,
                    sen=data.rose$SEN.S,
                    haml=data.rose$H.HAMLONAGHL,
                    sz=data.rose$S.Z,
                    khoraki=data.rose$H.KHORAKI.NOOSHIDANI,
                    behdasht=data.rose$H.BEHDASHT,
                    ertebat=data.rose$H.ERTEBATAT,
                    maskan=data.rose$H.MASKAN,
                    mobleman=data.rose$H.MOBLEMAN,
                    pooshak=data.rose$H.POOSHAK,
                    kala=data.rose$KHARID.KALA.BADAVAM)

valid.nn=data.frame(sh=NEW.C.SH.VALID.KNN,
                    lebas=NEW.LEBAS.VALID.KNN,
                    ojagh=NEW.OJAGH.VALID.KNN,
                    charkh=NEW.CHARKH.VALID.KNN,
                    jaroo=NEW.JAROO.VALID.KNN,
                    pc=NEW.PC.VALID.KNN,
                    tv=NEW.TV.VALID.KNN,
                    mo=NEW.MO.VALID.KNN,
                    jens=NEW.JENS.VALID.KNN,
                    daramad=validation_data$DARAMAD.KHALES,
                    ntm=NEW.N.T.M.VALID.KNN,
                    vfs=NEW.V.F.S.VALID.KNN,
                    do=NEW.DO.VALID.KNN,
                    zabt=NEW.ZABT.VALID.KNN,
                    panke=NEW.PANKE.VALID.KNN,
                    sen=validation_data$SEN.S,
                    haml=validation_data$H.HAMLONAGHL,
                    sz=validation_data$S.Z,
                    khoraki=validation_data$H.KHORAKI.NOOSHIDANI,
                    behdasht=validation_data$H.BEHDASHT,
                    ertebat=validation_data$H.ERTEBATAT,
                    maskan=validation_data$H.MASKAN,
                    mobleman=validation_data$H.MOBLEMAN,
                    pooshak=validation_data$H.POOSHAK,
                    kala=validation_data$KHARID.KALA.BADAVAM)

neural.model=neuralnet(NEURAL.RESPONSE[,1]+NEURAL.RESPONSE[,2]~
sh..data_1+sh..data_3+sh..data_24+lebas..data_0+  
lebas..data_1+ojagh..data_0+ojagh..data_1+charkh..data_0+
charkh..data_1+jaroo..data_0+jaroo..data_1+pc..data_0+
pc..data_1+tv..data_0+tv..data_1+mo..data_0+    
mo..data_1+jens..data_1+jens..data_2+ntm..data_1+ntm..data_3+ntm..data_4+   
ntm..data_5+ntm..data_6+ntm..data_7+vfs..data_1+    
vfs..data_2+vfs..data_3+vfs..data_5+vfs..data_6+    
do..data_0+do..data_1+zabt..data_0+zabt..data_1+   
panke..data_0+panke..data_1+sen+haml+          
sz+khoraki+behdasht+ertebat+maskan+mobleman+pooshak+kala,
                       data = train.nn,hidden =1,linear.output = F)
prediction(neural.model)
plot(neural.model,rep="best")
predict.nn=compute(neural.model,train.nn[,-c(20,21)])
predicted.class=apply(predict.nn$net.result,1,which.max)-1
confusionMatrix(as.factor(ifelse(predicted.class=="1","1","0")),
                data.rose$DARAMAD.KHALES)
validation.nn=compute(neural.model,valid.nn[,-20])
validation.class=apply(validation.nn$net.result,1,which.max)-1
confusionMatrix(as.factor(ifelse(validation.class=="1","1","0")),
                validation_data$DARAMAD.KHALES)
