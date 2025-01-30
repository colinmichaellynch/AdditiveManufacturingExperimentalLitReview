rm(list=ls())

library(ggplot2)
library(ggpubr)
library(fpc)
library(reshape2)
library(purrr)
library(dplyr)
library(dendextend)
library(cluster)
library(patchwork)
library(vegan)
library(viridis)
library(RColorBrewer)

setwd("~/Colin/Keck Center/literature review")

# Read the .csv file
data = read.csv("litReviewData.csv")
data$Type[data$Type=="Selective Laser Melting"] = "Powder Bed"
data = subset(data, Type == "All Manufacturing" | Type == "Powder Bed")

#clean data
unique(data$Design.Type)

data$Design.Type[data$Design.Type=="Single Factor"] = "Single-Factor"
data$Design.Type[data$Design.Type=="Single value"] = "Single Value"
data$Design.Type[data$Design.Type=="Full factorial"] = "Full Factorial"
data$Design.Type[data$Design.Type=="Full-Factorial"] = "Full Factorial"
data$Design.Type[data$Design.Type=="Taguchi "] = "Taguchi"
data$Design.Type[data$Design.Type==""] = "Fractional Factorial"
data$Design.Type[data$Design.Type=="Single-Value"] = "Single Value"
data$Design.Type[data$Design.Type=="Repeated Measures"] = "Repeated Trials"
data$Design.Type[data$Design.Type=="Optimal Search"] = "Other"
data$Design.Type[data$Design.Type=="Box\x96Behnken "] = "Box-Behnken"

unique(data$Analysis.Type)

data$Analysis.Type[data$Analysis.Type=="Regression "] = "Regression"
data$Analysis.Type[data$Analysis.Type=="Linear Regression"] = "Regression"
data$Analysis.Type[data$Analysis.Type=="Baysian "] = "Bayesian"
data$Analysis.Type[data$Analysis.Type=="ANOVA, Linear Model"] = "ANOVA"
data$Analysis.Type[data$Analysis.Type=="Descriptive "] = "Descriptive"
data$Analysis.Type[data$Analysis.Type=="Desriptive"] = "Descriptive"
data$Analysis.Type[data$Analysis.Type=="t-test"] = "T-Test"
data$Analysis.Type[data$Analysis.Type=="T-Test "] = "T-Test"
data$Analysis.Type[data$Analysis.Type=="Baysian "] = "Bayesian"
data$Analysis.Type[data$Analysis.Type=="ANOVA, Linear Model"] = "Regression"
data$Analysis.Type[data$Analysis.Type==""] = "Descriptive"
data$Analysis.Type[data$Analysis.Type=="Descrpitive "] = "Descriptive"
data$Analysis.Type[data$Analysis.Type=="Descriptiveq"] = "Descriptive"
data$Analysis.Type[data$Analysis.Type=="Distribution Fitting"] = "Distribution Comparisons"

unique(data$Model.Selection)

data$Model.Selection[data$Model.Selection==""] = "None"
data$Model.Selection[data$Model.Selection!="None"] = "Selection +"
data$Model.Selection[data$Model.Selection=="None"] = "Selection -"

unique(data$Model.Adequacy)

data$Model.Adequacy[data$Model.Adequacy==""] = "None"
data$Model.Adequacy[data$Model.Adequacy!="None"] = "Adequacy +"
data$Model.Adequacy[data$Model.Adequacy=="None"] = "Adequacy -"

unique(data$Blocking)

data$Blocking[data$Blocking==""] = "None"
data$Blocking[data$Blocking!="None"] = "Blocking +"
data$Blocking[data$Blocking=="None"] = "Blocking -"

unique(data$Randomization)

data$Randomization[data$Randomization==""] = "None"
data$Randomization[data$Randomization!="None"] = "Randomization +"
data$Randomization[data$Randomization=="None"] = "Randomization -"

unique(data$Sample.Size.Justification)

data$Sample.Size.Justification[data$Sample.Size.Justification==""] = "None"
data$Sample.Size.Justification[data$Sample.Size.Justification=="Power analysis"] = "Power Analysis"
data$Sample.Size.Justification[data$Sample.Size.Justification=="Power"] = "Power Analysis"
data$Sample.Size.Justification[data$Sample.Size.Justification!="None"] = "Power +"
data$Sample.Size.Justification[data$Sample.Size.Justification=="None"] = "Power -"

data$Sample.Size[is.na(data$Sample.Size)] = 5

sophistication = c()
for(i in 1:nrow(data)){
  if(data$Design.Type[i]=="Single-Factor" | data$Design.Type[i]=="OFAT" | data$Design.Type[i]=="Repeated Trials" | data$Design.Type[i]=="Single Value" | data$Design.Type[i]=="Other"){
    sophistication[i] = "Straightforward"
  } else {
    sophistication[i] = "Sophisticated"
  }
}

data$Sophistication = sophistication

dataAllMan = subset(data, Type == "All Manufacturing")
dataPowBed = subset(data, Type == "Powder Bed")

yearVec = unique(dataPowBed$Year)
n = length(yearVec)

propBlock = c()
probRand = c()
propSampleSize = c()
propModelAdequcy = c()
propModelSelection = c()
sampleSize = c()
sampleSizeSD = c()

analysisTypeDescriptive = c()
analysisTypeRegression = c()
analysisTypeANOVA = c()
analysisTypettest = c()
analysisTypeML = c()
analysisTypeMann = c()
analysisTypeDist = c()
analysisTypeBayesian = c()
analysisTypeCI = c()

designTypeSingle = c()
designTypeOFAT = c()
designTypeRT = c()
designTypeTag = c()
designTypeFull = c()
designTypeFract = c()
designTypeCC = c()
designTypeBB = c()
designTypeSV = c()
designTypeRand = c()
designTypeOther = c()

sophisticatedPowBed = c()

for(i in 1:n){

  dataTemp = subset(dataPowBed, Year == yearVec[i])

  propBlock[i] = sum(dataTemp$Blocking=="Blocking +")/nrow(dataTemp)
  probRand[i] = sum(dataTemp$Randomization=="Randomization +")/nrow(dataTemp)
  propSampleSize[i] = sum(dataTemp$Sample.Size.Justification=="Power +")/nrow(dataTemp)
  propModelAdequcy[i] = sum(dataTemp$Model.Adequacy=="Adequacy +")/nrow(dataTemp)
  propModelSelection[i] = sum(dataTemp$Model.Selection=="Selection +")/nrow(dataTemp)

  sampleSize[i] = mean(dataTemp$Sample.Size)
  sampleSizeSD[i] = sd(dataTemp$Sample.Size)

  analysisTypeDescriptive[i] = sum(dataTemp$Analysis.Type=="Descriptive")/nrow(dataTemp)
  analysisTypeRegression[i] = sum(dataTemp$Analysis.Type=="Regression")/nrow(dataTemp)
  analysisTypeANOVA[i] = sum(dataTemp$Analysis.Type=="ANOVA")/nrow(dataTemp)
  analysisTypettest[i] = sum(dataTemp$Analysis.Type=="t-test")/nrow(dataTemp)
  analysisTypeMann[i] = sum(dataTemp$Analysis.Type=="Mann-Whitney U")/nrow(dataTemp)
  analysisTypeDist[i] = sum(dataTemp$Analysis.Type=="Distribution Comparisons")/nrow(dataTemp)
  analysisTypeBayesian[i] = sum(dataTemp$Analysis.Type=="Bayesian")/nrow(dataTemp)
  analysisTypeCI[i] = sum(dataTemp$Analysis.Type=="Confidence Interval")/nrow(dataTemp)
  analysisTypeML[i] = sum(dataTemp$Analysis.Type=="Machine Learning")/nrow(dataTemp)

  designTypeSingle[i] = sum(dataTemp$Design.Type=="Single-Factor")/nrow(dataTemp)
  designTypeOFAT[i] = sum(dataTemp$Design.Type=="OFAT")/nrow(dataTemp)
  designTypeRT[i] = sum(dataTemp$Design.Type=="Repeated Trials")/nrow(dataTemp)
  designTypeTag[i] = sum(dataTemp$Design.Type=="Taguchi")/nrow(dataTemp)
  designTypeFull[i] = sum(dataTemp$Design.Type=="Full Factorial")/nrow(dataTemp)
  designTypeFract[i] = sum(dataTemp$Design.Type=="Fractional Factorial")/nrow(dataTemp)
  designTypeCC[i] = sum(dataTemp$Design.Type=="Central Compositer")/nrow(dataTemp)
  designTypeBB[i] = sum(dataTemp$Design.Type=="Box-Behnken")/nrow(dataTemp)
  designTypeSV[i] = sum(dataTemp$Design.Type=="Single Value")/nrow(dataTemp)
  designTypeRand[i] = sum(dataTemp$Design.Type=="Randomly Chosen")/nrow(dataTemp)
  designTypeOther[i] = sum(dataTemp$Design.Type=="Other")/nrow(dataTemp)

  sophisticatedPowBed[i] = sum(dataTemp$Sophistication=="Sophisticated")/nrow(dataTemp)

}

dataGraphPowBed = data.frame(Year = 2016:2024, sampleSize = sampleSize,  sampleSizeSD)

dataGraphPowBed2 = data.frame(Year = yearVec, Props = c(propBlock, probRand, propSampleSize, propModelAdequcy, propModelSelection), Type = c(rep("Block",n), rep("Random Order",n), rep("Sample Size Justification",n), rep("Model Adequacy Check",n), rep("Model Selection",n)))

dataGraphPowBed3 = data.frame(Year = yearVec, Props = c(analysisTypeDescriptive, analysisTypeRegression, analysisTypeANOVA, analysisTypettest, analysisTypeMann, analysisTypeDist, analysisTypeBayesian, analysisTypeCI, analysisTypeML), Type = c(rep("Descriptive",n), rep("Regression",n), rep("ANOVA",n), rep("T-Test",n), rep("Mann-Whitney U Test",n), rep("KS-Test",n), rep("Bayesian Statistics",n), rep("Confidence Interval",n), rep("Machine Learning",n)))

dataGraphPowBed4 = data.frame(Year = yearVec, Props = c(designTypeSingle, designTypeOFAT, designTypeRT, designTypeTag, designTypeFull, designTypeFract, designTypeCC, designTypeBB, designTypeSV, designTypeRand, designTypeOther), Type = c(rep("Single Factor",n), rep("OFAT",n), rep("Repeated Trials",n), rep("Taguchi",n), rep("Full Factorial",n), rep("Fractional Factorial",n), rep("Central Composite",n), rep("Box-Behnken",n), rep("Single Value",n), rep("Randomly Distributed Factor Levels",n), rep("Other",n)))

yearVec = unique(dataAllMan$Year)
n = length(yearVec)
propBlock = c()
probRand = c()
propSampleSize = c()
propModelAdequcy = c()
propModelSelection = c()
sampleSize = c()
sampleSizeSD = c()

analysisTypeDescriptive = c()
analysisTypeRegression = c()
analysisTypeANOVA = c()
analysisTypettest = c()
analysisTypeML = c()
analysisTypeMann = c()
analysisTypeDist = c()
analysisTypeBayesian = c()
analysisTypeCI = c()

designTypeSingle = c()
designTypeOFAT = c()
designTypeRT = c()
designTypeTag = c()
designTypeFull = c()
designTypeFract = c()
designTypeCC = c()
designTypeBB = c()
designTypeSV = c()
designTypeRand = c()
designTypeOther = c()

sophisticatedAllMan = c()

for(i in 1:n){

  dataTemp = subset(dataAllMan, Year == yearVec[i])

  propBlock[i] = sum(dataTemp$Blocking=="Blocking +")/nrow(dataTemp)
  probRand[i] = sum(dataTemp$Randomization=="Randomization +")/nrow(dataTemp)
  propSampleSize[i] = sum(dataTemp$Sample.Size.Justification=="Power +")/nrow(dataTemp)
  propModelAdequcy[i] = sum(dataTemp$Model.Adequacy=="Adequacy +")/nrow(dataTemp)
  propModelSelection[i] = sum(dataTemp$Model.Selection=="Selection +")/nrow(dataTemp)

  sampleSize[i] = mean(dataTemp$Sample.Size)
  sampleSizeSD[i] = sd(dataTemp$Sample.Size)

  analysisTypeDescriptive[i] = sum(dataTemp$Analysis.Type=="Descriptive")/nrow(dataTemp)
  analysisTypeRegression[i] = sum(dataTemp$Analysis.Type=="Regression")/nrow(dataTemp)
  analysisTypeANOVA[i] = sum(dataTemp$Analysis.Type=="ANOVA")/nrow(dataTemp)
  analysisTypettest[i] = sum(dataTemp$Analysis.Type=="t-test")/nrow(dataTemp)
  analysisTypeMann[i] = sum(dataTemp$Analysis.Type=="Mann-Whitney U")/nrow(dataTemp)
  analysisTypeDist[i] = sum(dataTemp$Analysis.Type=="Distribution Comparisons")/nrow(dataTemp)
  analysisTypeBayesian[i] = sum(dataTemp$Analysis.Type=="Bayesian")/nrow(dataTemp)
  analysisTypeCI[i] = sum(dataTemp$Analysis.Type=="Confidence Interval")/nrow(dataTemp)
  analysisTypeML[i] = sum(dataTemp$Analysis.Type=="Machine Learning")/nrow(dataTemp)

  designTypeSingle[i] = sum(dataTemp$Design.Type=="Single-Factor")/nrow(dataTemp)
  designTypeOFAT[i] = sum(dataTemp$Design.Type=="OFAT")/nrow(dataTemp)
  designTypeRT[i] = sum(dataTemp$Design.Type=="Repeated Trials")/nrow(dataTemp)
  designTypeTag[i] = sum(dataTemp$Design.Type=="Taguchi")/nrow(dataTemp)
  designTypeFull[i] = sum(dataTemp$Design.Type=="Full Factorial")/nrow(dataTemp)
  designTypeFract[i] = sum(dataTemp$Design.Type=="Fractional Factorial")/nrow(dataTemp)
  designTypeCC[i] = sum(dataTemp$Design.Type=="Central Compositer")/nrow(dataTemp)
  designTypeBB[i] = sum(dataTemp$Design.Type=="Box-Behnken")/nrow(dataTemp)
  designTypeSV[i] = sum(dataTemp$Design.Type=="Single Value")/nrow(dataTemp)
  designTypeRand[i] = sum(dataTemp$Design.Type=="Randomly Chosen")/nrow(dataTemp)
  designTypeOther[i] = sum(dataTemp$Design.Type=="Other")/nrow(dataTemp)

  sophisticatedAllMan[i] = sum(dataTemp$Sophistication=="Sophisticated")/nrow(dataTemp)

}

dataGraphAllMan = data.frame(Year = yearVec, sampleSize = sampleSize,  sampleSizeSD = sampleSizeSD)

dataGraphAllMan2 = data.frame(Year = yearVec, Props = c(propBlock, probRand, propSampleSize, propModelAdequcy, propModelSelection), Type = c(rep("Block",n), rep("Random Order",n), rep("Sample Size Justification",n), rep("Model Adequacy Check",n), rep("Model Selection",n)))

dataGraphAllMan3 =  data.frame(Year = yearVec, Props = c(analysisTypeDescriptive, analysisTypeRegression, analysisTypeANOVA, analysisTypettest, analysisTypeMann, analysisTypeDist, analysisTypeBayesian, analysisTypeCI, analysisTypeML), Type = c(rep("Descriptive",n), rep("Regression",n), rep("ANOVA",n), rep("T-Test",n), rep("Mann-Whitney U Test",n), rep("KS-Test",n), rep("Bayesian Statistics",n), rep("Confidence Interval",n), rep("Machine Learning",n)))

dataGraphAllMan4 = data.frame(Year = yearVec, Props = c(designTypeSingle, designTypeOFAT, designTypeRT, designTypeTag, designTypeFull, designTypeFract, designTypeCC, designTypeBB, designTypeSV, designTypeRand, designTypeOther), Type = c(rep("Single Factor",n), rep("OFAT",n), rep("Repeated Trials",n), rep("Taguchi",n), rep("Full Factorial",n), rep("Fractional Factorial",n), rep("Central Composite",n), rep("Box-Behnken",n), rep("Single Value",n), rep("Randomly Distributed Factor Levels",n), rep("Other",n)))

dataGraphSophistication = data.frame(Year = yearVec, Sophistication = c(sophisticatedPowBed, sophisticatedAllMan), Field = c(rep("Powder Bed Fusion", length(sophisticatedPowBed)), rep("All Additive Manufacturing", length(sophisticatedAllMan))))

dataGraph1 = rbind(dataGraphPowBed, dataGraphAllMan)
dataGraph1$Field = c(rep("Powder Bed Fusion", nrow(dataGraphPowBed)), rep("All Additive Manufacturing", nrow(dataGraphAllMan)))

dataGraph2 = rbind(dataGraphPowBed2, dataGraphAllMan2)
dataGraph2$Field = c(rep("Powder Bed Fusion", nrow(dataGraphPowBed2)), rep("All Additive Manufacturing", nrow(dataGraphAllMan2)))

dataGraph3 = rbind(dataGraphPowBed3, dataGraphAllMan3)
dataGraph3$Field = c(rep("Powder Bed Fusion", nrow(dataGraphPowBed3)), rep("All Additive Manufacturing", nrow(dataGraphAllMan3)))

dataGraph4 = rbind(dataGraphPowBed4, dataGraphAllMan4)
dataGraph4$Field = c(rep("Powder Bed Fusion", nrow(dataGraphPowBed4)), rep("All Additive Manufacturing", nrow(dataGraphAllMan4)))

dataGraph4$Type[dataGraph4$Type=="Single Factor"] = "One-Way ANOVA"

ggplot(dataGraph2, aes(x=Year, y=Props, color = Type)) +
  geom_smooth(se=F) + facet_wrap(~Field) + theme_bw() + theme(text = element_text(size = 14)) + ylab("Prop. Papers") + scale_x_continuous(breaks = yearVec) + labs(color = "DOE & ANOVA Feature") + theme(legend.position="bottom")  + guides(color=guide_legend(nrow=2)) #http://127.0.0.1:13473/graphics/plot_zoom_png?width=1200&height=900

ggplot(dataGraph3, aes(x=Year, y=Props, color = Type)) +
  geom_smooth(se=F) + facet_wrap(~Field) + theme_bw() + theme(text = element_text(size = 14)) + ylab("Prop. Papers") + scale_x_continuous(breaks = yearVec) + labs(color = "Analysis Type") + theme(legend.position="bottom")  + guides(color=guide_legend(nrow=2))

ggplot(dataGraph4, aes(x=Year, y=Props, color = Type)) +
  geom_smooth(se=F) + facet_wrap(~Field) + theme_bw() + theme(text = element_text(size = 14)) + ylab("Prop. Papers") + scale_x_continuous(breaks = yearVec) + labs(color = "Design Type") + theme(legend.position="bottom") + geom_smooth(data = dataGraphSophistication, aes(x = Year, y = Sophistication, color = "gray"), method = 'lm', se=F, color = "gray", linetype = "dashed")  + geom_point(data = dataGraphSophistication, aes(x = Year, y = Sophistication, color = "gray"), color = "gray", size = 2) + guides(color=guide_legend(nrow=2))


mdl1 = lm(Sophistication~Year*Field, data = dataGraphSophistication)
anova(mdl1)
summary(mdl1)
#plot(mdl1)

#figure 9

p2 = ggplot(data, aes(x = Type, y = (Sample.Size))) + geom_boxplot()+ theme_bw() + theme(text = element_text(size = 14)) + ylab("Sample Size") + xlab("Type of Study")

wilcox.test(dataAllMan$Sample.Size,dataPowBed$Sample.Size)

prop.test(x = c(sum(dataAllMan$Blocking=="Blocking +"), sum(dataPowBed$Blocking=="Blocking +")), n = c(nrow(dataAllMan), nrow(dataPowBed)))

prop.test(x = c(sum(dataAllMan$Randomization=="Randomization +"), sum(dataPowBed$Randomization=="Randomization +")), n = c(nrow(dataAllMan), nrow(dataPowBed)))

prop.test(x = c(sum(dataAllMan$Sample.Size.Justification=="Power +"), sum(dataPowBed$Sample.Size.Justification=="Power +")), n = c(nrow(dataAllMan), nrow(dataPowBed)))

prop.test(x = c(sum(dataAllMan$Model.Adequacy=="Adequacy +"), sum(dataPowBed$Model.Adequacy=="Adequacy +")), n = c(nrow(dataAllMan), nrow(dataPowBed)))

prop.test(x = c(sum(dataAllMan$Model.Selection=="Selection +"), sum(dataPowBed$Model.Selection=="Selection +")), n = c(nrow(dataAllMan), nrow(dataPowBed)))

dataGraph = data.frame(PaperType = c("AM", "PBF"), DOEType = c("Blocking", "Blocking", "Random Order", "Random Order", "Sample Size Justification", "Sample Size Justification", "Model Adequacy Check","Model Adequacy Check", "Model Selection", "Model Selection"), Prop = c(0.02830189, 0.03759398, 0.09433962, 0.15037594, 0.009433962, 0.060150376, 0.1698113, 0.2406015, 0.1415094, 0.1503759))

p1 = ggplot(dataGraph, aes(x = PaperType, y = Prop, color = PaperType)) + geom_point(size = 3.5) + facet_wrap(~DOEType, ncol = 5)  + theme_bw() + theme(text = element_text(size = 14)) + xlab("") + theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1)) + theme(legend.position="none") +
  geom_segment(aes(PaperType,Prop,xend=PaperType,yend=Prop-Prop), size=1.25) + ylab("Proportion")

table(dataAllMan$Analysis.Type)/nrow(dataAllMan)
table(dataPowBed$Analysis.Type)/nrow(dataAllMan)

unique(data$Analysis.Type)

propsAll = data.frame(Type = c(rep("All Additive Manufacturing", 9), rep("Powder Bed Fusion", 9)), AnalysisType = unique(data$Analysis.Type), Proportion = c(0.15094340, 0.55660377, 0.01886792, 0.21698113, 0.02830189, 0.02830189, 0, 0, 0,     0.150943396, 0.660377358, 0.009433962, 0.320754717, 0.018867925, 0.009433962, 0.018867925, 0.018867925, 0.037735849), Count = c(16, 59, 2, 23, 3, 3, 0, 0, 0,      16, 71, 1, 34, 2, 1, 2, 2, 4), Total = c(rep(nrow(dataAllMan), 9), rep(nrow(dataPowBed), 9)))

propsAll$AnalysisType = factor(propsAll$AnalysisType, level = c('Descriptive', 'Regression', 'ANOVA', 'Bayesian', 'Confidence Interval', 'Mann-Whitney U', 'Distribution Comparisons', 'T-Test', 'Machine Learning'))

p3 = ggplot(propsAll, aes(x = AnalysisType, y = Proportion, color = Type, group = Type)) + geom_point(size = 3.5) + geom_line(size=1.25) + theme_bw() + theme(text = element_text(size = 14)) + xlab("Analysis Type")  + theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1)) + theme(legend.position = c(0.6, 0.8), legend.title = element_blank(),legend.spacing.y = unit(0, "mm"), panel.border = element_rect(colour = "black", fill=NA), axis.text = element_text(colour = 1, size = 12),legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) +
  theme(legend.position = "none")

mdl1 = glm(cbind(Count, Total-Count) ~ Type*AnalysisType,
           family=binomial(logit), data=propsAll)

summary(mdl1)
anova(mdl1, test="Chisq")

table(dataAllMan$Design.Type)/nrow(dataAllMan)
table(dataPowBed$Design.Type)/nrow(dataPowBed)

unique(data$Design.Type)

propsAll = data.frame(Type = c(rep("All Additive Manufacturing", 11), rep("Powder Bed Fusion", 11)), DesignType = unique(data$Design.Type), Proportion = c(0.09433962,0.33962264, 0.33018868, 0.05660377, 0.03773585, 0.06603774, 0.01886792, 0.03773585, 0, 0.01886792, 0,         0.03759398, 0.30075188, 0.41353383, 0.09774436, 0.03007519, 0.01503759, 0.01503759, 0.02255639, 0.01503759, 0.03007519, 0.02255639), Count = c(10, 36, 35, 6, 4, 7, 2, 4, 0, 2, 0,      5, 40, 55, 13, 4, 2, 2, 3, 2, 4, 3), Total = c(rep(nrow(dataAllMan), 11), rep(nrow(dataPowBed), 11)))

#propsAll$DesignType = factor(propsAll$DesignType, level = c('Full Factorial', 'Single-Factor', 'Taguchi', 'Repeated Trials', 'Single Value', 'Randomly Chosen', 'Central Composite', 'Fractional Factorial', 'OFAT', 'Other', 'Box-Behnken'))

propsAll$DesignType = factor(propsAll$DesignType, level = c('Full Factorial', 'Single-Factor', 'Taguchi', 'Other', 'Repeated Trials', 'Single Value', 'Fractional Factorial', 'OFAT', 'Randomly Chosen', 'Central Composite', 'Box-Behnken'))

p4 = ggplot(propsAll, aes(x = DesignType, y = Proportion, color = Type, group = Type)) + geom_point(size = 3.5) + geom_line(size=1.25) + theme_bw() + theme(text = element_text(size = 14)) + xlab("Design Type")  + theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1)) + theme(legend.position = c(0.6, 0.8), legend.title = element_blank(),legend.spacing.y = unit(0, "mm"), panel.border = element_rect(colour = "black", fill=NA), axis.text = element_text(colour = 1, size = 12),legend.background = element_blank(),legend.box.background = element_rect(colour = "black"))

mdl2 = glm(cbind(Count, Total-Count) ~ Type*DesignType,
           family=binomial(logit), data=propsAll)

summary(mdl2)
anova(mdl2, test="Chisq")

p1 / (p2 | p3 | p4) +
  plot_annotation(tag_levels = 'A')

#cluster analysis

dataCluster = data.frame(SSJust = as.factor(data$Sample.Size.Justification), Block = as.factor(data$Blocking), Random = as.factor(data$Randomization), Selection = as.factor(data$Model.Selection), Adequacy = as.factor(data$Model.Adequacy), Analysis = as.factor(data$Analysis.Type), Design = as.factor(data$Design.Type))

dataCluster$sampleSize[data$Sample.Size>mean(data$Sample.Size)] = "Big"
dataCluster$sampleSize[data$Sample.Size<=mean(data$Sample.Size)] = "Small"
dataCluster$sampleSize = as.factor(dataCluster$sampleSize)

dataCluster$Type = data$Type

dataAllMan = subset(dataCluster, Type == "All Manufacturing")
dataPowBed = subset(dataCluster, Type == "Powder Bed")

dataAllMan = dataAllMan[,1:8]
dataPowBed = dataPowBed[,1:8]

###Clustering
#https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995

#powder bed
gower.dist <- daisy(dataPowBed, metric = c("gower"))
class(gower.dist)

divisive.clust <- diana(as.matrix(gower.dist),diss = TRUE, keep.diss = TRUE)
#plot(divisive.clust, main = "Divisive")

divisive.clust <- diana(as.matrix(gower.dist),diss = TRUE, keep.diss = TRUE)
#plot(divisive.clust, main = "Divisive")

aggl.clust.c <- hclust(gower.dist, method = "complete")
#plot(aggl.clust.c,main = "Agglomerative, complete linkages")

cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)

    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]

    }

    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]

    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}
# I am capping the maximum amout of clusters by 7
# I want to choose a reasonable number, based on which I will be able to see basic differences between customer groups as a result
stats.df.divisive <- cstats.table(gower.dist, divisive.clust, 7)
stats.df.divisive

stats.df.aggl <-cstats.table(gower.dist, aggl.clust.c, 7)
stats.df.aggl

ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))),
       aes(x=cluster.number, y=avg.silwidth, color='Divisive')) +
  geom_point(size=3)+
  geom_line(size=1.2) + geom_point(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), aes(x=cluster.number, y=avg.silwidth, color='Agglomerative'), size=3) + geom_line(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), aes(x=cluster.number, y=avg.silwidth, color='Agglomerative'), size=1.2) +theme_bw()  + theme(text = element_text(size = 14)) +
  labs(x = "# Clusters", y = "Average Silhouette Width", color = "Clustering Method") +
  theme(plot.title = element_text(hjust = 0.5))

dendro <- as.dendrogram(divisive.clust)
dendro.col <- dendro %>%
  set("branches_k_color", k = 2, value =   c("darkslategray", "darkslategray4", "darkslategray3", "gold3", "black", "purple")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors",
      value = c("darkslategray")) %>%
  set("labels_cex", 0.5)
ggd1 <- as.ggdend(dendro.col)

ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 2")

ggplot(ggd1, labels = T) +
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")

dataPowBed$id.s = 1:nrow(dataPowBed)
clust.num <- cutree(divisive.clust, k = 2)
synthetic.customers.cl <- cbind(dataPowBed, clust.num)
cust.long <- melt(data.frame(lapply(synthetic.customers.cl, as.character), stringsAsFactors=FALSE), id = c("id.s", "clust.num"), factorsAsStrings=T)

cust.long.q <- cust.long %>%
  group_by(clust.num, variable, value) %>%
  mutate(count = n_distinct(id.s)) %>%
  distinct(clust.num, variable, value, count)

cust.long.p <- cust.long.q %>%
  group_by(clust.num, variable) %>%
  mutate(perc = count / sum(count)) %>%
  arrange(clust.num)

heatmap.p1 <- ggplot(cust.long.q, aes(x = clust.num, y = factor(value, ordered = T))) + geom_tile(aes(fill = count), alpha = 0.85)+
  labs(x = "Cluster #", y = NULL, fill = "Count") + theme_bw()

yearVec = 2016:2024
propClus2PowBed = c()

dataTemp = subset(data, Type =="Powder Bed")
synthetic.customers.cl$Year = dataTemp$Year
clusterVecPB = synthetic.customers.cl$clust.num

for(i in 1:length(yearVec)){
  dataTemp = subset(synthetic.customers.cl, Year == yearVec[i])
  propClus2PowBed[i] = sum(dataTemp$clust.num==2)/nrow(dataTemp)
}

#all man

gower.dist <- daisy(dataAllMan, metric = c("gower"))
class(gower.dist)

divisive.clust <- diana(as.matrix(gower.dist),diss = TRUE, keep.diss = TRUE)
#plot(divisive.clust, main = "Divisive")

divisive.clust <- diana(as.matrix(gower.dist),diss = TRUE, keep.diss = TRUE)
#plot(divisive.clust, main = "Divisive")

aggl.clust.c <- hclust(gower.dist, method = "complete")
#plot(aggl.clust.c,main = "Agglomerative, complete linkages")

cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)

    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]

    }

    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]

    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}
# I am capping the maximum amout of clusters by 7
# I want to choose a reasonable number, based on which I will be able to see basic differences between customer groups as a result
stats.df.divisive <- cstats.table(gower.dist, divisive.clust, 7)
stats.df.divisive

stats.df.aggl <-cstats.table(gower.dist, aggl.clust.c, 7)
stats.df.aggl

ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))),
       aes(x=cluster.number, y=avg.silwidth, color='Divisive')) +
  geom_point(size=3)+
  geom_line(size=1.2) + geom_point(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), aes(x=cluster.number, y=avg.silwidth, color='Agglomerative'), size=3) + geom_line(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), aes(x=cluster.number, y=avg.silwidth, color='Agglomerative'), size=1.2) +theme_bw()  + theme(text = element_text(size = 14)) +
  labs(x = "# Clusters", y = "Average Silhouette Width", color = "Clustering Method") +
  theme(plot.title = element_text(hjust = 0.5))

dendro <- as.dendrogram(divisive.clust)
dendro.col <- dendro %>%
  set("branches_k_color", k = 2, value =   c("darkslategray", "darkslategray4", "darkslategray3", "gold3", "black", "purple")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors",
      value = c("darkslategray")) %>%
  set("labels_cex", 0.5)
ggd1 <- as.ggdend(dendro.col)

ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 2")

ggplot(ggd1, labels = T) +
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")

dataAllMan$id.s = 1:nrow(dataAllMan)
clust.num <- cutree(divisive.clust, k = 2)
synthetic.customers.cl <- cbind(dataAllMan, clust.num)
cust.long <- melt(data.frame(lapply(synthetic.customers.cl, as.character), stringsAsFactors=FALSE), id = c("id.s", "clust.num"), factorsAsStrings=T)

cust.long.q <- cust.long %>%
  group_by(clust.num, variable, value) %>%
  mutate(count = n_distinct(id.s)) %>%
  distinct(clust.num, variable, value, count)

heatmap.p2 <- ggplot(cust.long.q, aes(x = clust.num, y = factor(value, ordered = T))) + geom_tile(aes(fill = count), alpha = 0.85)+
  labs(x = "Cluster #", y = NULL, fill = "Count") + theme_bw()

p1 = heatmap.p1  + theme(text = element_text(size = 14))
p2 = heatmap.p2  + theme(text = element_text(size = 14))

dataTemp = subset(data, Type =="All Manufacturing")
synthetic.customers.cl$Year = dataTemp$Year
clusterVecAM = synthetic.customers.cl$clust.num

yearVec = 2016:2024
propClus2AllMan = c()

for(i in 1:length(yearVec)){
  dataTemp = subset(synthetic.customers.cl, Year == yearVec[i])
  propClus2AllMan[i] = sum(dataTemp$clust.num==2)/nrow(dataTemp)
}

dataTime = data.frame(Year = yearVec, PropClus2 = c(propClus2PowBed, propClus2AllMan), Type = c(rep("Powder Bed Fusion", length(yearVec)), c(rep("All Additive Manufacturing", length(yearVec)))))

p3 = ggplot(dataTime, aes(x=Year, y = PropClus2, color = Type)) + geom_smooth(se=F, span = .7) + theme_bw() + theme(text = element_text(size = 14)) + ylab("Prop. Papers in Cluster 2")

(p1 | p2) / p3 +
  plot_annotation(tag_levels = 'A')

###

dataGraph = data[order(data$Type),]
dataGraph$impactFactor = dataGraph$Cited.by/(2024 - dataGraph$Year + 1)
dataGraph$Cluster = c(clusterVecAM, clusterVecPB)

ggplot(dataGraph, aes(x = as.factor(Cluster), y = impactFactor)) + geom_boxplot()
wilcox.test(dataGraph$impactFactor~as.factor(dataGraph$Cluster))

### compare PBF to tissue engineering, dont forget most TE papers had 3 replicates to estimate standard deviation

data = read.csv("ManualReviewRyan.csv")
data = subset(data, Type != "All Manufacturing")
data = subset(data, Year == 2016 | Year == 2024)
data = data[-c(9, 10),]

#clean data
unique(data$Design.Type)

data$Design.Type[data$Design.Type=="Single Factor"] = "Single-Factor"
data$Design.Type[data$Design.Type=="Single value"] = "Single Value"
data$Design.Type[data$Design.Type=="Full factorial"] = "Full Factorial"
data$Design.Type[data$Design.Type=="Full-Factorial"] = "Full Factorial"
data$Design.Type[data$Design.Type=="Taguchi "] = "Taguchi"
data$Design.Type[data$Design.Type==""] = "Fractional Factorial"
data$Design.Type[data$Design.Type=="Single-Value"] = "Single Value"
data$Design.Type[data$Design.Type=="Repeated Measures"] = "Repeated Trials"
data$Design.Type[data$Design.Type=="Optimal Search"] = "Other"
data$Design.Type[data$Design.Type=="Box\x96Behnken "] = "Box-Behnken"

unique(data$Analysis.Type)

data$Analysis.Type[data$Analysis.Type=="Regression "] = "Regression"
data$Analysis.Type[data$Analysis.Type=="Linear Regression"] = "Regression"
data$Analysis.Type[data$Analysis.Type=="Baysian "] = "Bayesian"
data$Analysis.Type[data$Analysis.Type=="ANOVA, Linear Model"] = "ANOVA"
data$Analysis.Type[data$Analysis.Type=="Descriptive "] = "Descriptive"
data$Analysis.Type[data$Analysis.Type=="Desriptive"] = "Descriptive"
data$Analysis.Type[data$Analysis.Type=="t-test"] = "T-Test"
data$Analysis.Type[data$Analysis.Type=="T-Test "] = "T-Test"
data$Analysis.Type[data$Analysis.Type=="Baysian "] = "Bayesian"
data$Analysis.Type[data$Analysis.Type=="ANOVA, Linear Model"] = "Regression"
data$Analysis.Type[data$Analysis.Type==""] = "Descriptive"
data$Analysis.Type[data$Analysis.Type=="Descrpitive "] = "Descriptive"
data$Analysis.Type[data$Analysis.Type=="Distribution Fitting"] = "Distribution Comparisons"

unique(data$Model.Selection)

data$Model.Selection[data$Model.Selection==""] = "None"
data$Model.Selection[data$Model.Selection!="None"] = "Selection +"
data$Model.Selection[data$Model.Selection=="None"] = "Selection -"

unique(data$Model.Adequacy)

data$Model.Adequacy[data$Model.Adequacy==""] = "None"
data$Model.Adequacy[data$Model.Adequacy!="None"] = "Adequacy +"
data$Model.Adequacy[data$Model.Adequacy=="None"] = "Adequacy -"

unique(data$Blocking)

data$Blocking[data$Blocking==""] = "None"
data$Blocking[data$Blocking!="None"] = "Blocking +"
data$Blocking[data$Blocking=="None"] = "Blocking -"

unique(data$Randomization)

data$Randomization[data$Randomization==""] = "None"
data$Randomization[data$Randomization!="None"] = "Randomization +"
data$Randomization[data$Randomization=="None"] = "Randomization -"

unique(data$Sample.Size.Justification)

data$Sample.Size.Justification[data$Sample.Size.Justification==""] = "None"
data$Sample.Size.Justification[data$Sample.Size.Justification=="Power analysis"] = "Power Analysis"
data$Sample.Size.Justification[data$Sample.Size.Justification=="Power"] = "Power Analysis"
data$Sample.Size.Justification[data$Sample.Size.Justification!="None"] = "Power +"
data$Sample.Size.Justification[data$Sample.Size.Justification=="None"] = "Power -"

data$Sample.Size[is.na(data$Sample.Size)] = 5

sophistication = c()
for(i in 1:nrow(data)){
  if(data$Design.Type[i]=="Single-Factor" | data$Design.Type[i]=="OFAT" | data$Design.Type[i]=="Repeated Trials" | data$Design.Type[i]=="Single Value" | data$Design.Type[i]=="Other"){
    sophistication[i] = 0
  } else {
    sophistication[i] = 1
  }
}

data$ExperimentalSophistication = sophistication

sophistication = c()
for(i in 1:nrow(data)){
  if(data$Analysis.Type[i]=="Descriptive"){
    sophistication[i] = 0
  } else {
    sophistication[i] = 1
  }
}

data$StatSophistication = sophistication

sophistication = c()
for(i in 1:nrow(data)){
  if(data$Sample.Size.Justification[i]=="Power +" | data$Randomization[i]=="Randomization +" | data$Blocking[i]=="Blocking +" | data$Model.Adequacy[i]=="Adequacy +" | data$Model.Selection[i]=="Selection +"){
    sophistication[i] = 1
  } else {
    sophistication[i] = 0
  }
}

data$FeatureSophistication = sophistication

table_data = with(data, table(Type, Year, ExperimentalSophistication))
prop_table = prop.table(table_data, margin = c(1, 2))
model = glm(ExperimentalSophistication ~ Type * Year, family = binomial, data = data)
summary(model)
prop_table[, , 2]

diversity(table(data$Design.Type[data$Type=="Tissue engineering"]), index = "shannon")
diversity(table(data$Design.Type[data$Type=="Powder Bed"]), index = "shannon")

table_data = with(data, table(Type, Year, StatSophistication))
prop_table = prop.table(table_data, margin = c(1, 2))
model = glm(ExperimentalSophistication ~ Type * Year, family = binomial, data = data)
summary(model)
prop_table[, , 2]

diversity(table(data$Analysis.Type[data$Type=="Tissue engineering"]), index = "shannon")
diversity(table(data$Analysis.Type[data$Type=="Powder Bed"]), index = "shannon")

table_data = with(data, table(Type, Year, FeatureSophistication))
prop_table = prop.table(table_data, margin = c(1, 2))
model = glm(ExperimentalSophistication ~ Type * Year, family = binomial, data = data)
summary(model)
prop_table[, , 2]

### compare PBF to orthopedic data,

rm(list=ls())

data = read.csv("ManualReviewRyan.csv")
data$Type[data$Type=="Selective Laser Melting"] = "Powder Bed"
  data = subset(data, Type == "Powder Bed" | Type == "Orthopedic Engineering")
data = subset(data, Year == 2016 | Year == 2024)
data = data[-c(9, 10),]

#clean data
unique(data$Design.Type)

data$Design.Type[data$Design.Type=="Single Factor"] = "Single-Factor"
data$Design.Type[data$Design.Type=="Single value"] = "Single Value"
data$Design.Type[data$Design.Type=="Full factorial"] = "Full Factorial"
data$Design.Type[data$Design.Type=="Full-Factorial"] = "Full Factorial"
data$Design.Type[data$Design.Type=="Taguchi "] = "Taguchi"
data$Design.Type[data$Design.Type==""] = "Fractional Factorial"
data$Design.Type[data$Design.Type=="Single-Value"] = "Single Value"
data$Design.Type[data$Design.Type=="Repeated Measures"] = "Repeated Trials"
data$Design.Type[data$Design.Type=="Optimal Search"] = "Other"
data$Design.Type[data$Design.Type=="Box\x96Behnken "] = "Box-Behnken"

unique(data$Analysis.Type)

data$Analysis.Type[data$Analysis.Type=="Regression "] = "Regression"
data$Analysis.Type[data$Analysis.Type=="Linear Regression"] = "Regression"
data$Analysis.Type[data$Analysis.Type=="Baysian "] = "Bayesian"
data$Analysis.Type[data$Analysis.Type=="ANOVA, Linear Model"] = "ANOVA"
data$Analysis.Type[data$Analysis.Type=="Descriptive "] = "Descriptive"
data$Analysis.Type[data$Analysis.Type=="Desriptive"] = "Descriptive"
data$Analysis.Type[data$Analysis.Type=="t-test"] = "T-Test"
data$Analysis.Type[data$Analysis.Type=="T-Test "] = "T-Test"
data$Analysis.Type[data$Analysis.Type=="Baysian "] = "Bayesian"
data$Analysis.Type[data$Analysis.Type=="ANOVA, Linear Model"] = "Regression"
data$Analysis.Type[data$Analysis.Type==""] = "Descriptive"
data$Analysis.Type[data$Analysis.Type=="Descrpitive "] = "Descriptive"
data$Analysis.Type[data$Analysis.Type=="Distribution Fitting"] = "Distribution Comparisons"

unique(data$Model.Selection)

data$Model.Selection[data$Model.Selection==""] = "None"
data$Model.Selection[data$Model.Selection!="None"] = "Selection +"
data$Model.Selection[data$Model.Selection=="None"] = "Selection -"

unique(data$Model.Adequacy)

data$Model.Adequacy[data$Model.Adequacy==""] = "None"
data$Model.Adequacy[data$Model.Adequacy!="None"] = "Adequacy +"
data$Model.Adequacy[data$Model.Adequacy=="None"] = "Adequacy -"

unique(data$Blocking)

data$Blocking[data$Blocking==""] = "None"
data$Blocking[data$Blocking!="None"] = "Blocking +"
data$Blocking[data$Blocking=="None"] = "Blocking -"

unique(data$Randomization)

data$Randomization[data$Randomization==""] = "None"
data$Randomization[data$Randomization!="None"] = "Randomization +"
data$Randomization[data$Randomization=="None"] = "Randomization -"

unique(data$Sample.Size.Justification)

data$Sample.Size.Justification[data$Sample.Size.Justification==""] = "None"
data$Sample.Size.Justification[data$Sample.Size.Justification=="Power analysis"] = "Power Analysis"
data$Sample.Size.Justification[data$Sample.Size.Justification=="Power"] = "Power Analysis"
data$Sample.Size.Justification[data$Sample.Size.Justification!="None"] = "Power +"
data$Sample.Size.Justification[data$Sample.Size.Justification=="None"] = "Power -"

data$Sample.Size[is.na(data$Sample.Size)] = 5

sophistication = c()
for(i in 1:nrow(data)){
  if(data$Design.Type[i]=="Single-Factor" | data$Design.Type[i]=="OFAT" | data$Design.Type[i]=="Repeated Trials" | data$Design.Type[i]=="Single Value" | data$Design.Type[i]=="Other"){
    sophistication[i] = 0
  } else {
    sophistication[i] = 1
  }
}

data$ExperimentalSophistication = sophistication

sophistication = c()
for(i in 1:nrow(data)){
  if(data$Analysis.Type[i]=="Descriptive"){
    sophistication[i] = 0
  } else {
    sophistication[i] = 1
  }
}

data$StatSophistication = sophistication

sophistication = c()
for(i in 1:nrow(data)){
  if(data$Sample.Size.Justification[i]=="Power +" | data$Randomization[i]=="Randomization +" | data$Blocking[i]=="Blocking +" | data$Model.Adequacy[i]=="Adequacy +" | data$Model.Selection[i]=="Selection +"){
    sophistication[i] = 1
  } else {
    sophistication[i] = 0
  }
}

data$FeatureSophistication = sophistication

table_data = with(data, table(Type, Year, ExperimentalSophistication))
prop_table = prop.table(table_data, margin = c(1, 2))
model = glm(ExperimentalSophistication ~ Type * Year, family = binomial, data = data)
summary(model)
prop_table[, , 2]

diversity(table(data$Design.Type[data$Type=="Orthopedic Engineering"]), index = "shannon")
diversity(table(data$Design.Type[data$Type=="Powder Bed"]), index = "shannon")

table_data = with(data, table(Type, Year, StatSophistication))
prop_table = prop.table(table_data, margin = c(1, 2))
model = glm(ExperimentalSophistication ~ Type * Year, family = binomial, data = data)
summary(model)
prop_table[, , 2]

diversity(table(data$Analysis.Type[data$Type=="Orthopedic Engineering"]), index = "shannon")
diversity(table(data$Analysis.Type[data$Type=="Powder Bed"]), index = "shannon")

table_data = with(data, table(Type, Year, FeatureSophistication))
prop_table = prop.table(table_data, margin = c(1, 2))
model = glm(ExperimentalSophistication ~ Type * Year, family = binomial, data = data)
summary(model)
prop_table[, , 2]

p1 = ggplot(data, aes(x = factor(Year), fill = Design.Type)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Type) +
  scale_y_continuous() +
  labs(
    x = "Year",
    y = "Proportion of Papers",
    fill = "Experimental Design"
  ) +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_fill_viridis(discrete = TRUE, option = "turbo") + ggtitle("a")# +
  #theme(legend.position = "bottom")

p2 = ggplot(data, aes(x = factor(Year), fill = Analysis.Type)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Type) +
  scale_y_continuous() +
  labs(
    x = "Year",
    y = "Proportion of Papers",
    fill = "Statistical Analysis"
  ) +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_fill_viridis(discrete = TRUE, option = "turbo") + ggtitle("b")# +
  #theme(legend.position = "bottom")

df_summary <- data %>%
  group_by(Year, Type) %>%
  summarise(average = mean(FeatureSophistication))  # Replace 'binary_column' with your 0/1 column name

p3 = ggplot(df_summary, aes(x = factor(Year), y = average, color = Type)) +
  geom_point(size = 3) +
  geom_line(aes(group = Type, color = Type)) +
  labs(
    x = "Year",
    y = "Propotion of Papers",
    color = "Field"
  ) +
  theme_bw() + theme(text = element_text(size = 14))  +
  scale_color_brewer(palette = "Set1") + ggtitle("c")# +
  #theme(legend.position = "bottom")  + ggtitle("C)")

