library(lme4)
library(MuMIn)
library(ggplot2)
library(plyr)


library(readxl)
y <- read_excel("VECTOR_ImageJ_severity_growth_082724.xlsx")
View(y)


y$Treatment<-as.factor(y$Treatment)
y$Treatment<-relevel(y$Treatment, ref="NA")



#y_noE<-y[y$Eelgrass==1,]
#y_noE<-y_noE[y_noE$Time!="T0",]
#y_noE<-y_noE[y_noE$Inoculated==1,]

#View(y_noE)
summary(y_noE)


#################Hypothesis 1,2,3###########################3
model_0<-lm(sev~Treatment+Inoculated, data=y)

model_1<-lm(sev~Treatment+Live_shell+Inoculated+Treatment*Inoculated, data=y)

model_2<-lm(sev~Treatment+Live_shell+Inoculated+Live_shell*Inoculated, data=y)

model_3<-lm(sev~Treatment*Live_shell+Inoculated, data=y)

summary(model_0)
summary(model_1)
summary(model_2)
summary(model_3)
plot(modelT1)

#simplest model that explains the most info. lowest AIC score is best model
#model 0 had lowest AIC
AICc(model_0)
AICc(model_1)
AICc(model_2)
AICc(model_3)

y_noE_summary<-ddply(y_noE, .(Treatment, Live_shell,Time), summarise, Log10=mean(Log10), sd=sd(Log10))
p1<-ggplot(y_noE_summary, aes(x=Treatment, y=Log10, fill=as.factor(Live_shell))) + geom_col(position=("dodge")) +facet_grid(Time~.)
p1     