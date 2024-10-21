library(lme4)
library(MuMIn)
library(ggplot2)
library(plyr)

#1. Sanitation methods will reduce laby cell load on shells.>no
#2. Shells that contain live animals will have a lower occurrence of laby.>yes
#3.	Laby intensity will increase through time.>no
#4.	The presence of eelgrass will increase laby intensity on shells.>no


y <- read.csv("C:/Users/Lindsay Alma/Dropbox/Eelgrass/VECTOR Summer23 FHL OA Lab Experiment/qPCR plates/Swab plates/aggregated_data_qPCR_swabs_082724.csv")
View(y)
aggregate(Log10~Treatment+Time+Live_shell, data=y, mean)

y$Treatment<-as.factor(y$Treatment)
y$Treatment<-relevel(y$Treatment, ref="C")


y_noE<-y[y$Eelgrass==1,]
y_noE<-y_noE[y_noE$Time=="T3",]
#y_noE<-y_noE[y_noE$Inoculated==1,]
View(y_noE)
summary(y_noE)


#################Hypothesis 1,2,3###########################3
model_0<-lm(Log10~Inoculated, data=y_noE)

model_1<-lm(Log10~Treatment+Live_shell+Time+Treatment, data=y_noE)

model_2<-lm(Log10~Treatment+Live_shell+Time+Live_shell, data=y_noE)

model_3<-lm(Log10~Treatment*Live_shell, data=y_noE)

summary(model_0)
summary(model_1)
summary(model_2)
summary(model_3)
plot(model_0)

#simplest model that explains the most info. lowest AIC score is best model
#model 0 had lowest AIC
AICc(model_0)
AICc(model_1)
AICc(model_2)
AICc(model_3)



View(y_noE)
# Calculate summary statistics for the filtered data
y_noE_summary <- y_noE %>%
  group_by(Treatment, Live_shell) %>%
  summarise(
    Log10 = mean(Log10, na.rm = TRUE),
    sd = sd(Log10, na.rm = TRUE)
  )

y_noE_summary <- y_noE %>%
  group_by(Live_shell) %>%
  summarise(
    Log10 = mean(Log10, na.rm = TRUE),
    sd = sd(Log10, na.rm = TRUE),
    n_samples = n()  # Count the number of samples
  )

# Print the summary
print(y_noE_summary)



# Reorder the Treatment factor levels
y_noE_summary$Treatment <- factor(y_noE_summary$Treatment, levels = c("N", "Bl", "Sal", "Dep"))



ggplot(y_noE_summary, aes(x = Treatment, y = Log10, fill = as.factor(Live_shell))) + 
  geom_col(position = "dodge", color = "black") +  # Add black outline to bars
  geom_errorbar(aes(ymin = Log10 - sd, ymax = Log10 + sd), 
                width = 0.2, 
                position = position_dodge(0.9),
                color = "black") +  # Add error bars with black color
  scale_fill_manual(values = c("orange", "green"), 
                    labels = c("dead", "live")) +  # Set colors and labels for the fill
  labs(fill = "Live or dead oyster") +  # Change the legend title
  scale_x_discrete(labels = c("Control", "Bleach", "Salinity", "Depurated")) +  # Update x-axis labels
  facet_grid(Time ~ .) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +  # Add a solid line at y = 0
  theme(
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white"),   # Set plot background to white
    panel.grid.major = element_blank(),                # Remove major grid lines
    panel.grid.minor = element_blank(),                # Remove minor grid lines
    axis.ticks = element_line(color = "black"),        # Set axis ticks color to black
    axis.text.x = element_text(size = 12, color = "black"),  # Increase x-axis text size and set color to black
    axis.text.y = element_text(color = "black"),  # Set y-axis text color to black
    axis.title.x = element_text(color = "black"), # Set x-axis title color to black
    axis.title.y = element_text(color = "black"), # Set y-axis title color to black
    legend.text = element_text(size = 12, color = "black"),   # Set legend text size and color
    legend.title = element_text(size = 14, color = "black"),  # Set legend title size and color
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add black border around each panel
    strip.background = element_blank(),  # Remove background color of facet labels
    strip.text = element_text(color = "black", size = 14)  # Set facet label text color to black and increase size
  )




#####################Hypothesis 4##############################################
y$Treatment<-as.factor(y$Treatment)
y$Treatment<-relevel(y$Treatment, ref="C")



y_E<-y[y$Time!="T0",]
y_E<-y_E[y_E$Time!="T1",]
y_E<-y_E[y_noE$Inoculated==1,]
View(y_E)
summary(y_E)

model_0<-lm(Log10~Treatment+Live_shell+Time+Eelgrass, data=y_E)

model_1<-lm(Log10~Treatment+Live_shell+Time+Eelgrass+Treatment*Eelgrass, data=y_E)

model_2<-lm(Log10~Treatment+Live_shell+Time+Eelgrass+Live_shell*Eelgrass, data=y_E)

model_3<-lm(Log10~Treatment*Live_shell+Time+Eelgrass, data=y_E)

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