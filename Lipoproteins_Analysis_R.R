library(tidyverse)
library(readxl)
library(ggplot2)
library(tableone)
library(viridisLite)
library(viridis)
library(ggpubr)
library(miceRanger)
library(gridExtra)

shapiro<-lapply(Clinica[c("Age","BMI","Heart_rate","TAS","TAD","Pcadera","Pcintura","Glucose","Insulina_drug","HOMAIR","TG","CHOL", "LDL","HDL", "ALT","AST","GGT")], shapiro.test)

nonormales<-c("Age","BMI","Heart_rate","TAS","TAD","Pcadera","Pcintura","Glucose","Insulin2","HOMAIR","TG","CHOL", "LDL","HDL", "ALT","AST","GGT")

tab1 <- CreateTableOne(strata = "Qual", data = Clinica)

p1 <- ggplot(subset(Clinica, Lipoprotein != "LDL"), aes(x=Lipoprotein, y=Colesterol, fill=Qual))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set2" )+
  ylim(0,90)+
  stat_compare_means(label="p.signif")+
  theme_classic()+
  labs(x="Lipoproteína", y="Contenido de Colesterol (mg/dL)")

p2 <- ggplot(subset(Clinica, Lipoprotein != "HDL" & Lipoprotein != "IDL" & Lipoprotein != "VLDL"), aes(x=Lipoprotein, y=Colesterol, fill=Qual))+
  geom_boxplot(width=0.40)+
  scale_fill_brewer(palette = "Set2" )+
  stat_compare_means(label="p.signif")+
  theme_classic()+
  labs(x="Lipoproteína", y="Contenido de Colesterol (mg/dL)")

ggarrange(p1, p2,ncol=2, nrow=1, common.legend = TRUE, legend="right")

ggplot(ScatterPlot, aes(x=log10(VLDL), y=Glucose)) + 
  geom_point() + 
  geom_smooth(method=lm) + 
  stat_regline_equation(label.x=1, label.y=30)+ 
  stat_cor(label.x=1, label.y=25) + 
  theme(panel.background = element_blank())