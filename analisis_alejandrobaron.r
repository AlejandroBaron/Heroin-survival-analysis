library(survival)
library(survminer)
datos <- read.table("D:/Users/alejandro/Desktop/MECI Supervivencia/ADDICTS.txt", header = TRUE, sep = ",")
datos<-datos[,-1]


#% de censuras
sum(datos$Status)/length(datos$Status)

#Supervivencia global con intervalo de confianza
addict.global <- survfit(Surv(Days.survival,Status) ~ 1, type = "kaplan-meier", data =datos)
plot(addict.global, xlab = "Dias hasta la salida de la clinica", ylab = "S(t)", conf.int = TRUE)
ggsurvplot(addict.global,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))



#Riesgo segun clinica
addict.clinic <- survfit(Surv(Days.survival,Status)~Clinic,data = datos)
ggsurvplot(addict.clinic,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           legend.labs = c("Clínica 1", "Clínica 2"),
           fun = "cumhaz",
           palette = c("#E7B800", "#2E9FDF"))

#Riesgo segun la prision
sum(datos$Prison==1)/nrow(datos)

addict.prision <- survfit(Surv(Days.survival,Status)~Prison,data = datos)
ggsurvplot(addict.prision,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           legend.labs = c("Sin Antecedentes", "Con Antecedentes"),
           fun = "cumhaz",
           palette = c("#99ff99","#994d00"))


#Riesgo segun la dosis

DoseLevel<-cut(datos$Dose,c(0,60,100,max(datos$Dose)),labels=c("Baja","Media","Alta"))
DoseLevel<-cut(datos$Dose,c(0,60,max(datos$Dose)),labels=c("Baja","Alta"))



hist(datos$Dose,col="#99ff99")
abline(v=60,col="red")
abline(v=100,col="red")

as.numeric(DoseLevel)
datos<-cbind(datos,DoseLevel)


#Riesgo segun la clinica


#Riesgo segun la dosis
addict.dose <- survfit(Surv(Days.survival,Status)~DoseLevel,data = datos)
ggsurvplot(addict.dose,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           legend.labs = c("Baja","Alta"),
           fun = "cumhaz",
           palette = c("#49c06d","#ee514c"))







DoseLevel<-as.numeric(datos$DoseLevel)-1
datos$DoseLevel<-DoseLevel



#Modelo de Cox

#Sin estratificar

cox.clin <- coxph(Surv(Days.survival,Status) ~ (Clinic+Prison+DoseLevel),data = datos,x = T) 
cox.clin

#Test de riesgos proporcionales
testrp <- cox.zph(cox.clin)
testrp

par(mfrow=c(2,2))
plot(testrp)



#Estratificado por clinica
cox.clin.s <- coxph(Surv(Days.survival,Status) ~ strata(Clinic)+Prison+DoseLevel,data = datos,x = T) 
cox.clin.s    


summary(cox.clin.s)

#Test de riesgos proporcionales
testrp <- cox.zph(cox.clin.s)
testrp


plot(testrp)



#library(MASS)
#stepAIC(cox.clin.si,direction="both",trace=100)

cox.clin.s
par(mfrow=c(1,2))
plot(testrp)
#c("martingale", "deviance", "score", "schoenfeld", "dfbeta", "dfbetas", "scaledsch","partial")




#Estratificado por clinica + interaccion prision con dosis

cox.clin.si <- coxph(Surv(Days.survival,Status) ~ strata(Clinic)+Prison+DoseLevel+Prison*DoseLevel,data = datos,x = T) 
cox.clin.si    
extractAIC(cox.clin.si)


summary(cox.clin.si)
testrp <- cox.zph(cox.clin.si)
testrp

par(mfrow=c(2,2))
plot(testrp)


Inter<-datos$Prison*datos$DoseLevel

datos<-cbind(datos,Inter)

########################################################  
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################



#Supervivencia segun clinica
addict.clinic <- survfit(Surv(Days.survival,Status)~Clinic,data = datos)
ggsurvplot(addict.clinic,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           legend.labs = c("Clínica 1", "Clínica 2"),
           palette = c("#E7B800", "#2E9FDF"))

#Supervivencia segun la prision


addict.prision <- survfit(Surv(Days.survival,Status)~Prison,data = datos)
ggsurvplot(addict.prision,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           legend.labs = c("Sin Antecedentes", "Con Antecedentes"),
           palette = c("#99ff99","#994d00"))



#Supervivenciasegun la dosis

DoseLevel<-cut(datos$Dose,c(0,60,max(datos$Dose)),labels=c("Baja","Alta"))

datos$DoseLevel<-DoseLevel


addict.dose <- survfit(Surv(Days.survival,Status)~DoseLevel,data = datos)
ggsurvplot(addict.dose,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           legend.labs = c("Baja","Alta"),
           palette = c("#49c06d","#ee514c"))

#Supervivencia interaccion
addict.doseinter <- survfit(Surv(Days.survival,Status)~Inter,data = datos)
ggsurvplot(addict.doseinter,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#994d00","#ee514c"))

