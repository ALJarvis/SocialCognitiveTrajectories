#Load packages
library(readxl)
library(ggplot2)
library(data.table)
library(psych)
library(dplyr)
library(psychometric)
library(car)
library(corrplot)
library(RColorBrewer)
library(reshape2)
library(rmarkdown)
library(flextable)
library(officer)
library(lmtest)
library(sandwich)
library(corrtable)
library(Hmisc)
library(tidyr)


# SUMMARY STATISTICS ------------------------------------------------------

#total summary
psych::describe(PISA)

#total proportion of males to females
prop.table(table(PISA$sex))

#Descriptive statistics for age groups
agebreaks <- c(40, 50, 60, 70, 81)
agelabels <- c("40-49", "50-59", "60-69", "70-80")

PISA %>%
  mutate(agegroup = cut(Age, breaks = agebreaks, labels = agelabels, right = FALSE)) %>%
  group_by(agegroup) %>%
  summarise(
    Mean = mean(Age, na.rm = TRUE),
    SD = sd(Age, na.rm = TRUE),
    N = n()
  )

# AGE AND EXEC FUNCTION ASSOCIATIONS -------------------------------------------------

#Normal regression 
MRreg <- lm(MR ~ Age, data = PISA)
summary(MRreg)

#Robust regression
MREst <- coeftest(MRreg, vcoc. = vcovHC(MRreg, type = "HC1"))
MRCI <- coefci(MRreg, vcov. = vcovHC(MRreg, type = "HC1"))

#Standardised beta
MRregS <- lm(MR ~ Age, data = PISAstandard1)
MREstS <- coeftest(MRregS, vcoc. = vcovHC(MRregS, type = "HC1"))


# CORRELATIONS ----------------------------------------------------

#Correlation matrix 
PISACor <- data.frame(PISA$happy, PISA$fear, PISA$disgust, PISA$anger, PISA$surprise, PISA$sad, PISA$neutral, PISA$think, 
                      PISA$do, PISA$Cog_ToM, PISA$feel, PISA$EC, PISA$PD, PISA$EE)


#spearman values
cormatrix <- corr.test(PISACor, method = "spearman", use = "complete.obs")$r

#Rename columns and rows
colnames(cormatrix)<- c("Happy", "Fear", "Disgust", "Anger", "Surprise", "Sad", "Neutral", "First-order Cognitive","Second-order Cognitive", 
                        "Total cognitive ToM", "Affective ToM", "Empathic Concern", "Personal Distress", "Emotional Empathy")
rownames(cormatrix)<- c("Happy", "Fear", "Disgust", "Anger", "Surprise", "Sad", "Neutral", "First-order Cognitive","Second-order Cognitive", 
                        "Total cognitive ToM", "Affective ToM", "Empathic Concern", "Personal Distress", "Emotional Empathy")
#p-values
P <- corr.test(PISACor, method = "spearman", use = "complete.obs")$p

#Create heatmap figure 
tiff(filename = "correlation.tiff", compression = "lzw", width = 8, height = 7, units = "in", res = 300)

corrplot(cormatrix, col=brewer.pal(n=8, name="RdBu"), cl.lim = c(-1, 1), tl.col = "black", tl.srt = 60, cl.ratio = 0.4, 
         p.mat = P, sig.level = 0.050, insig = "pch", pch.col = "gray49", pch.cex = 2, 
         method = "color", tl.cex = 1.2, cl.cex = 1.1, addgrid.col = "black", diag = FALSE, type = 'lower')
dev.off()

#Create excel table
save_correlation_matrix(PISACor, type = "spearman", digits = 2, use = 'lower', replace_diagonal = TRUE,
                        filename = "corrtable.csv")


# MULTIPLE LINEAR REGRESSIONS (FULLY ADJUSTED)-------------------------------------

#Happy
multiHappy <- lm(happy ~ Age + sex + education + HADSa + HADSd + MR, data = PISA)
summary(multiHappy)

#Fear
multiFear <- lm(fear ~ Age + sex + education + HADSa + HADSd + MR, data = PISA)
summary(multiFear)

#Disgust
multiDisgust <- lm(disgust ~ Age + sex + education + HADSa + HADSd + MR, data = PISA)
summary(multiDisgust)

#Anger
multiAnger <- lm(anger ~ Age + sex + education + HADSa + HADSd + MR, data = PISA)
summary(multiAnger)

#Surprise
multiSurprise <- lm(surprise ~ Age + sex + education + HADSa + HADSd + MR, data = PISA)
summary(multiSurprise)

#Sad
multiSad <- lm(sad ~ Age + sex + education + HADSa + HADSd + MR, data = PISA)
summary(multiSad)

#Neutral
multiNeutral <- lm(neutral ~ Age + sex + education + HADSa + HADSd + MR, data = PISA)
summary(multiNeutral)

#Feel
multiFeel <- lm(feel ~ Age + sex + education + HADSa + HADSd + MR, data = PISA)
summary(multiFeel)

#Think
multiThink <- lm(think ~ Age + sex + education + HADSa + HADSd + MR, data = PISA)
summary(multiThink)

#Do
multiDo <- lm(do ~ Age + sex + education + HADSa + HADSd + MR, data = PISA)
summary(multiDo)

#Cog_ToM
multiCogToM <- lm(Cog_ToM ~ Age + sex + education + HADSa + HADSd + MR, data = PISA)
summary(multiCogToM)

#EC
multiEC <- lm(EC ~ Age + sex + education + HADSa + HADSd + MR, data = PISA)
summary(multiEC)

#PD
multiPD <- lm(PD ~ Age + sex + education + HADSa + HADSd + MR, data = PISA)
summary(multiPD)

#EE
multiEE <- lm(EE ~ Age + sex + education + HADSa + HADSd + MR, data = PISA)
summary(multiEE)


# ASSUMPTION CHECKS -----------------------------------------------
# Linearity ---------------------------------------------------------------

#Happy
plot(multiHappy, 1)

#Fear
plot(multiFear, 1)

#Disgust
plot(multiDisgust, 1)

#Anger
plot(multiAnger, 1)

#Surprise
plot(multiSurprise, 1)

#Sad
plot(multiSad, 1)

#Neutral
plot(multiNeutral, 1)

#Feel
plot(multiFeel, 1)

#Think
plot(multiThink, 1)

#Do
plot(multiDo, 1)

#Cog_ToM
plot(multiCogToM, 1)

#EC
plot(multiEC, 1)

#PD
plot(multiPD, 1)

#EE
plot(multiEE, 1)

# Homoscedasticity with ncvTest() -----------------------------------------

#happy
ncvTest(multiHappy)

#fear
ncvTest(multiFear)

#disgust
ncvTest(multiDisgust) #heteroscedastic

#anger
ncvTest(multiAnger) #heteroscedastic

#surprise
ncvTest(multiSurprise) #heteroscedastic

#sad
ncvTest(multiSad) #heteroscedastic

#neutral
ncvTest(multiNeutral) #heteroscedastic

#do
ncvTest(multiDo)

#think
ncvTest(multiThink)

#feel
ncvTest(multiFeel)

#cognitive 
ncvTest(multiCogToM)

#EC
ncvTest(multiEC)

#PD
ncvTest(multiPD)

#EE
ncvTest(multiEE)

# ROBUST REGRESSIONS (FULLY ADJUSTED MODEL) ------------------------------------------------------

#Happy
happyEst <- coeftest(multiHappy, vcov. = vcovHC(multiHappy, type = "HC1"))
happyCI <- coefci(multiHappy, vcov. = vcovHC(multiHappy, type = "HC1"))

#Fear
fearEst <- coeftest(multiFear, vcov. = vcovHC(multiFear, type = "HC1"))
fearCI <- coefci(multiFear, vcov. = vcovHC(multiFear, type = "HC1"))

#Disgust
disgustEst <- coeftest(multiDisgust, vcov. = vcovHC(multiDisgust, type = "HC1"))
disgustCI <- coefci(multiDisgust, vcov. = vcovHC(multiDisgust, type = "HC1"))

#Anger
angerEst <- coeftest(multiAnger, vcov. = vcovHC(multiAnger, type = "HC1"))
angerCI <- coefci(multiAnger, vcov. = vcovHC(multiAnger, type = "HC1"))

#Surprise
surpriseEst <- coeftest(multiSurprise, vcov. = vcovHC(multiSurprise, type = "HC1"))
surpriseCI <- coefci(multiSurprise, vcov. = vcovHC(multiSurprise, type = "HC1"))

#Sad
sadEst <- coeftest(multiSad, vcov. = vcovHC(multiSad, type = "HC1"))
sadCI <- coefci(multiSad, vcov. = vcovHC(multiSad, type = "HC1"))

#Neutral
neutralEst <- coeftest(multiNeutral, vcov. = vcovHC(multiNeutral, type = "HC1"))
neutralCI <- coefci(multiNeutral, vcov. = vcovHC(multiNeutral, type = "HC1"))

#Feel
feelEst <- coeftest(multiFeel, vcov. = vcovHC(multiFeel, type = "HC1"))
feelCI <- coefci(multiFeel, vcov. = vcovHC(multiFeel, type = "HC1"))

#Think
thinkEst <- coeftest(multiThink, vcov. = vcovHC(multiThink, type = "HC1"))
thinkCI <- coefci(multiThink, vcov. = vcovHC(multiThink, type = "HC1"))

#Do
doEst <- coeftest(multiDo, vcov. = vcovHC(multiDo, type = "HC1"))
doCI <- coefci(multiDo, vcov. = vcovHC(multiDo, type = "HC1"))

#CogToM
cogEst <- coeftest(multiCogToM, vcov. = vcovHC(multiCogToM, type = "HC1"))
cogCI <- coefci(multiCogToM, vcov. = vcovHC(multiCogToM, type = "HC1"))

#EC
ECEst <- coeftest(multiEC, vcov. = vcovHC(multiEC, type = "HC1"))
ECCI <- coefci(multiEC, vcov. = vcovHC(multiEC, type = "HC1"))

#PD
PDEst <- coeftest(multiPD, vcov. = vcovHC(multiPD, type = "HC1"))
PDCI <- coefci(multiPD, vcov. = vcovHC(multiPD, type = "HC1"))

#EE
EEEst <- coeftest(multiEE, vcov. = vcovHC(multiEE, type = "HC1"))
EECI <- coefci(multiEE, vcov. = vcovHC(multiEE, type = "HC1"))

# STANDARDISED BETAS (FULLY ADJUSTED MODEL) ------------------------------------------------------

#Standardise PISA dataset (also remove sex as it needs to stay categorical)
PISAselect <- PISA %>%
  dplyr::select(-ID, -sex)

PISAselect1 <- PISA %>%
  dplyr::select(ID, sex)

PISAselect <- lapply(PISAselect, scale)

PISAstandard1 <- cbind(PISAselect, PISAselect1)

#Rerun regression models using the standardised dataset 
#Happy
multiHappyS <- lm(happy ~ Age + sex + education + HADSa + HADSd + MR, data = PISAstandard1)
happyBS <- coeftest(multiHappyS, vcov. = vcovHC(multiHappyS, type = "HC1"))

#Fear
multiFearS <- lm(fear ~ Age + sex + education + HADSa + HADSd + MR, data = PISAstandard1)
fearBS <- coeftest(multiFearS, vcov. = vcovHC(multiFearS, type = "HC1"))

#Disgust
multiDisgustS <- lm(disgust ~ Age + sex + education + HADSa + HADSd + MR, data = PISAstandard1)
disgustBS <- coeftest(multiDisgustS, vcov. = vcovHC(multiDisgustS, type = "HC1"))

#Anger
multiAngerS <- lm(anger ~ Age + sex + education + HADSa + HADSd + MR, data = PISAstandard1)
angerBS <- coeftest(multiAngerS, vcov. = vcovHC(multiAngerS, type = "HC1"))

#Surprise
multiSurpriseS <- lm(surprise ~ Age + sex + education + HADSa + HADSd + MR, data = PISAstandard1)
surpriseBS <- coeftest(multiSurpriseS, vcov. = vcovHC(multiSurpriseS, type = "HC1"))

#Sad
multiSadS <- lm(sad ~ Age + sex + education + HADSa + HADSd + MR, data = PISAstandard1)
sadBS <- coeftest(multiSadS, vcov. = vcovHC(multiSadS, type = "HC1"))

#Neutral
multiNeutralS <- lm(neutral ~ Age + sex + education + HADSa + HADSd + MR, data = PISAstandard1)
neutralBS <- coeftest(multiNeutralS, vcov. = vcovHC(multiNeutralS, type = "HC1"))

#Feel
multiFeelS <- lm(feel ~ Age + sex + education + HADSa + HADSd + MR, data = PISAstandard1)
feelBS <- coeftest(multiFeelS, vcov. = vcovHC(multiFeelS, type = "HC1"))

#Think
multiThinkS <- lm(think ~ Age + sex + education + HADSa + HADSd + MR, data = PISAstandard1)
thinkBS <- coeftest(multiThinkS, vcov. = vcovHC(multiThinkS, type = "HC1"))

#Do
multiDoS <- lm(do ~ Age + sex + education + HADSa + HADSd + MR, data = PISAstandard1)
doBS <- coeftest(multiDoS, vcov. = vcovHC(multiDoS, type = "HC1"))

#Cog_ToM
multiCogToMS <- lm(Cog_ToM ~ Age + sex + education + HADSa + HADSd + MR, data = PISAstandard1)
cogBS <- coeftest(multiCogToMS, vcov. = vcovHC(multiCogToMS, type = "HC1"))

#EC
multiECS <- lm(EC ~ Age + sex + education + HADSa + HADSd + MR, data = PISAstandard1)
ECBS <- coeftest(multiECS, vcov. = vcovHC(multiECS, type = "HC1"))

#PD
multiPDS <- lm(PD ~ Age + sex + education + HADSa + HADSd + MR, data = PISAstandard1)
PDBS <- coeftest(multiPDS, vcov. = vcovHC(multiPDS, type = "HC1"))

#EE
multiEES <- lm(EE ~ Age + sex + education + HADSa + HADSd + MR, data = PISAstandard1)
EEBS <- coeftest(multiEES, vcov. = vcovHC(multiEES, type = "HC1"))

# MODEL SUMMARY TABLE (FULLY ADJUSTED MODEL) --------------------------------------------------------------
#First need to create separate data frames for each regression model 
#Happy
happyregression <- data.frame(Outcome = c("Happy"),
                              Variable = c("Age", "Sex", "Education", "HADSa", "HADSd", "MR"), 
                              R2 = c(summary(multiHappy)$r.squared),
                              B = c(happyEst[2:7,1]),
                              BS = c(happyBS[2:7,1]), 
                              CILower = c(happyCI[2:7,1]),
                              CIUpper = c(happyCI[2:7,2]),
                              p = c(happyEst[2:7,4]))
#Fear
fearregression <- data.frame(Outcome = c("fear"),
                             Variable = c("Age", "Sex", "Education", "HADSa", "HADSd", "MR"), 
                             R2 = c(summary(multiFear)$r.squared),
                             B = c(fearEst[2:7,1]),
                             BS = c(fearBS[2:7,1]), 
                             CILower = c(fearCI[2:7,1]),
                             CIUpper = c(fearCI[2:7,2]),
                             p = c(fearEst[2:7,4]))

#Disgust
disgustregression <- data.frame(Outcome = c("disgust"),
                                Variable = c("Age", "Sex", "Education", "HADSa", "HADSd", "MR"), 
                                R2 = c(summary(multiDisgust)$r.squared),
                                B = c(disgustEst[2:7,1]),
                                BS = c(disgustBS[2:7,1]), 
                                CILower = c(disgustCI[2:7,1]),
                                CIUpper = c(disgustCI[2:7,2]),
                                p = c(disgustEst[2:7,4]))

#Anger
angerregression <- data.frame(Outcome = c("anger"),
                              Variable = c("Age", "Sex", "Education", "HADSa", "HADSd", "MR"), 
                              R2 = c(summary(multiAnger)$r.squared),
                              B = c(angerEst[2:7,1]),
                              BS = c(angerBS[2:7,1]), 
                              CILower = c(angerCI[2:7,1]),
                              CIUpper = c(angerCI[2:7,2]),
                              p = c(angerEst[2:7,4]))

#Surprise
surpriseregression <- data.frame(Outcome = c("surprise"),
                                 Variable = c("Age", "Sex", "Education", "HADSa", "HADSd", "MR"), 
                                 R2 = c(summary(multiSurprise)$r.squared),
                                 B = c(surpriseEst[2:7,1]),
                                 BS = c(surpriseBS[2:7,1]), 
                                 CILower = c(surpriseCI[2:7,1]),
                                 CIUpper = c(surpriseCI[2:7,2]),
                                 p = c(surpriseEst[2:7,4]))

#Sad
sadregression <- data.frame(Outcome = c("sad"),
                            Variable = c("Age", "Sex", "Education", "HADSa", "HADSd", "MR"), 
                            R2 = c(summary(multiSad)$r.squared),
                            B = c(sadEst[2:7,1]),
                            BS = c(sadBS[2:7,1]), 
                            CILower = c(sadCI[2:7,1]),
                            CIUpper = c(sadCI[2:7,2]),
                            p = c(sadEst[2:7,4]))

#Neutral
neutralregression <- data.frame(Outcome = c("neutral"),
                                Variable = c("Age", "Sex", "Education", "HADSa", "HADSd", "MR"), 
                                R2 = c(summary(multiNeutral)$r.squared),
                                B = c(neutralEst[2:7,1]),
                                BS = c(neutralBS[2:7,1]), 
                                CILower = c(neutralCI[2:7,1]),
                                CIUpper = c(neutralCI[2:7,2]),
                                p = c(neutralEst[2:7,4]))

#Feel
feelregression <- data.frame(Outcome = c("feel"),
                             Variable = c("Age", "Sex", "Education", "HADSa", "HADSd", "MR"), 
                             R2 = c(summary(multiFeel)$r.squared),
                             B = c(feelEst[2:7,1]),
                             BS = c(feelBS[2:7,1]), 
                             CILower = c(feelCI[2:7,1]),
                             CIUpper = c(feelCI[2:7,2]),
                             p = c(feelEst[2:7,4]))

#Think
thinkregression <- data.frame(Outcome = c("think"),
                              Variable = c("Age", "Sex", "Education", "HADSa", "HADSd", "MR"), 
                              R2 = c(summary(multiThink)$r.squared),
                              B = c(thinkEst[2:7,1]),
                              BS = c(thinkBS[2:7,1]), 
                              CILower = c(thinkCI[2:7,1]),
                              CIUpper = c(thinkCI[2:7,2]),
                              p = c(thinkEst[2:7,4]))

#Do
doregression <- data.frame(Outcome = c("do"),
                           Variable = c("Age", "Sex", "Education", "HADSa", "HADSd", "MR"), 
                           R2 = c(summary(multiDo)$r.squared),
                           B = c(doEst[2:7,1]),
                           BS = c(doBS[2:7,1]), 
                           CILower = c(doCI[2:7,1]),
                           CIUpper = c(doCI[2:7,2]),
                           p = c(doEst[2:7,4]))

#CogToM
cogregression <- data.frame(Outcome = c("cog"),
                            Variable = c("Age", "Sex", "Education", "HADSa", "HADSd", "MR"), 
                            R2 = c(summary(multiCogToM)$r.squared),
                            B = c(cogEst[2:7,1]),
                            BS = c(cogBS[2:7,1]), 
                            CILower = c(cogCI[2:7,1]),
                            CIUpper = c(cogCI[2:7,2]),
                            p = c(cogEst[2:7,4]))

#EC
ECregression <- data.frame(Outcome = c("EC"),
                           Variable = c("Age", "Sex", "Education", "HADSa", "HADSd", "MR"), 
                           R2 = c(summary(multiEC)$r.squared),
                           B = c(ECEst[2:7,1]),
                           BS = c(ECBS[2:7,1]), 
                           CILower = c(ECCI[2:7,1]),
                           CIUpper = c(ECCI[2:7,2]),
                           p = c(ECEst[2:7,4]))

#PD
PDregression <- data.frame(Outcome = c("PD"),
                           Variable = c("Age", "Sex", "Education", "HADSa", "HADSd", "MR"), 
                           R2 = c(summary(multiPD)$r.squared),
                           B = c(PDEst[2:7,1]),
                           BS = c(PDBS[2:7,1]), 
                           CILower = c(PDCI[2:7,1]),
                           CIUpper = c(PDCI[2:7,2]),
                           p = c(PDEst[2:7,4]))

#EE
EEregression <- data.frame(Outcome = c("EE"),
                           Variable = c("Age", "Sex", "Education", "HADSa", "HADSd", "MR"), 
                           R2 = c(summary(multiEE)$r.squared),
                           B = c(EEEst[2:7,1]),
                           BS = c(EEBS[2:7,1]), 
                           CILower = c(EECI[2:7,1]),
                           CIUpper = c(EECI[2:7,2]),
                           p = c(EEEst[2:7,4]))

#Combine all regression data frames together
combinedregressions <- rbind(happyregression, fearregression, disgustregression, angerregression, surpriseregression, 
                             sadregression, neutralregression, doregression, thinkregression, 
                             feelregression, cogregression, ECregression, 
                             PDregression, EEregression)

#Create flexable object
ft <- flextable(data = combinedregressions %>%
                  mutate(p = sprintf("%.3g", p))) %>%
  theme_apa() %>%
  autofit()

#Create a temp file
tmp <- tempfile(fileext = ".docx")

#Create docx file
read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "PISAtable.docx")

#Create flexable object
ft <- flextable(data = combinedregressions %>%
                  mutate(p = formatC(p, digits = 3, format = "fg"))) %>%
  theme_apa() %>%
  autofit()



# HEATMAP (FULLY ADJUSTED MODEL)----------------------------------------------------------

#manually create ECBS and EEBS given beta >1
ECBS <- c(0.08224698, 1, -0.08329326, 0.25306202, -0.32450009, 0.18817796)
EEBS <- c(0.11730606, 1, -0.16408462, 0.54757695, -0.01755468, 0.06174629)

b.heatmap <- data.frame(happyBS[2:7,1], fearBS[2:7,1], disgustBS[2:7,1], angerBS[2:7,1], surpriseBS[2:7,1], sadBS[2:7,1], 
                        neutralBS[2:7,1], thinkBS[2:7,1], doBS[2:7,1],
                        cogBS[2:7,1], feelBS[2:7,1], ECBS, PDBS[2:7,1], EEBS)
b.heatmap <- as.matrix(b.heatmap)
b.heatmap <- t(b.heatmap)
colnames(b.heatmap)<- c("Age", "Sex", "Education", "Anxiety", "Depression", "Fluid IQ")
rownames(b.heatmap)<- c("Happy", "Fear", "Disgust", "Anger", "Surprise", "Sad", "Neutral", "First-order Cognitive", "Second-order Cognitive", 
                        "Total Cognitive ToM", "Affective ToM", "Empathic Concern", "Personal Distress", "Emotional Empathy")
b.heatmap <- round(b.heatmap,3)

#Create p value dataframe
p.heatmap <- data.frame(happyEst[2:7,4], fearEst[2:7,4], disgustEst[2:7,4], angerEst[2:7,4], surpriseEst[2:7,4], sadEst[2:7,4], 
                        neutralEst[2:7,4], thinkEst[2:7,4], doEst[2:7,4], cogEst[2:7,4], feelEst[2:7,4],
                        ECEst[2:7,4], PDEst[2:7,4], EEEst[2:7,4])
p.heatmap <- as.matrix(p.heatmap)
p.heatmap <- t(p.heatmap)
colnames(p.heatmap)<- c("Age", "Sex", "Education", "Anxiety", "Depression", "Fluid IQ") 
rownames(p.heatmap)<- c("Happy", "Fear", "Disgust", "Anger", "Surprise", "Sad", "Neutral", "First-order Cognitive", "Second-order Cognitive",
                        "Total Cognitive ToM", "Affective ToM", "Empathic Concern", "Personal Distress", "Emotional Empathy")
p.heatmap <- round(p.heatmap,3)

#Create heatmap figure 
tiff(filename = "heatmapnew.tiff", compression = "lzw", width = 7, height = 8, units = "in", res = 300)

heatmapfig <- corrplot(b.heatmap, col=brewer.pal(n=8, name="RdBu"), cl.lim = c(-1, 1), tl.col = "black", tl.srt = 60, cl.ratio = 0.4, 
                       p.mat = p.heatmap, sig.level = 0.017, insig = "pch", pch.col = "gray49", pch.cex = 2, 
                       method = "color", tl.cex = 1.2, cl.cex = 1.1, addgrid.col = "black")
dev.off()


# BETA GRAPHS (FULLY ADJUSTED MODEL) -----------------------------------------------------------

### Create values and labels first ###
#create predicted values for lines and CI
ages <- 40:82
ages <- rep(ages, 4)

#Create separate data frames for each domain
happydf <- data.frame(Age = PISA$Age, happy = PISA$happy)
feardf <- data.frame(Age = PISA$Age, fear = PISA$fear)
disgustdf <- data.frame(Age = PISA$Age, disgust = PISA$disgust)
angerdf <- data.frame(Age = PISA$Age, anger = PISA$anger)
surprisedf <- data.frame(Age = PISA$Age,surprise = PISA$surprise)
saddf <- data.frame(Age = PISA$Age,sad = PISA$sad)
neutraldf <- data.frame(Age = PISA$Age,neutral = PISA$neutral)
feeldf <- data.frame(Age = PISA$Age,feel = PISA$feel)
thinkdf <- data.frame(Age = PISA$Age,think = PISA$think)
dodf <- data.frame(Age = PISA$Age,do = PISA$do)
cogdf <- data.frame(Age = PISA$Age,cog = PISA$Cog_ToM)
ECdf <- data.frame(Age = PISA$Age,EC = PISA$EC)
PDdf <- data.frame(Age = PISA$Age,PD = PISA$PD)
EEdf <- data.frame(Age = PISA$Age,EE = PISA$EE)

#Convert into long format
happydf <- melt(setDT(happydf), id.vars = c("Age"), variable.name = "domain")
feardf <- melt(setDT(feardf), id.vars = c("Age"), variable.name = "domain")
disgustdf <- melt(setDT(disgustdf), id.vars = c("Age"), variable.name = "domain")
angerdf <- melt(setDT(angerdf), id.vars = c("Age"), variable.name = "domain")
surprisedf <- melt(setDT(surprisedf), id.vars = c("Age"), variable.name = "domain")
saddf <- melt(setDT(saddf), id.vars = c("Age"), variable.name = "domain")
neutraldf <- melt(setDT(neutraldf), id.vars = c("Age"), variable.name = "domain")
feeldf <- melt(setDT(feeldf), id.vars = c("Age"), variable.name = "domain")
thinkdf <- melt(setDT(thinkdf), id.vars = c("Age"), variable.name = "domain")
dodf <- melt(setDT(dodf), id.vars = c("Age"), variable.name = "domain")
cogdf <- melt(setDT(cogdf), id.vars = c("Age"), variable.name = "domain")
ECdf <- melt(setDT(ECdf), id.vars = c("Age"), variable.name = "domain")
PDdf <- melt(setDT(PDdf), id.vars = c("Age"), variable.name = "domain")
EEdf <- melt(setDT(EEdf), id.vars = c("Age"), variable.name = "domain")

#Create label for beta values
happybeta <- data.frame(domain = unique(happydf$domain), labels = paste("B =", round(happyEst[2,1],3)), face = c("plain"))
fearbeta <- data.frame(domain = unique(feardf$domain), labels = paste("B =", round(fearEst[2,1],3)), face = c("plain"))
disgustbeta <- data.frame(domain = unique(disgustdf$domain), labels = paste("B =", round(disgustEst[2,1],3)), face = c("plain"))
angerbeta <- data.frame(domain = unique(angerdf$domain), labels = paste("B =", round(angerEst[2,1],3)), face = c("plain"))
surprisebeta <- data.frame(domain = unique(surprisedf$domain), labels = paste("B =", round(surpriseEst[2,1],3)), face = c("plain"))
sadbeta <- data.frame(domain = unique(saddf$domain), labels = paste("B =", round(sadEst[2,1],3)), face = c("plain"))
neutralbeta <- data.frame(domain = unique(neutraldf$domain), labels = paste("B =", round(neutralEst[2,1],3)), face = c("plain"))
feelbeta <- data.frame(domain = unique(feeldf$domain), labels = paste("B =", round(feelEst[2,1],3)), face = c("plain"))
thinkbeta <- data.frame(domain = unique(thinkdf$domain), labels = paste("B =", round(thinkEst[2,1],3)), face = c("plain"))
dobeta <- data.frame(domain = unique(dodf$domain), labels = paste("B =", round(doEst[2,1],3)), face = c("plain"))
cogbeta <- data.frame(domain = unique(cogdf$domain), labels = paste("B =", round(cogEst[2,1],3)), face = c("plain"))
ECbeta <- data.frame(domain = unique(ECdf$domain), labels = paste("B =", round(ECEst[2,1],3)), face = c("plain"))
PDbeta <- data.frame(domain = unique(PDdf$domain), labels = paste("B =", round(PDEst[2,1],3)), face = c("plain"))
EEbeta <- data.frame(domain = unique(EEdf$domain), labels = paste("B =", round(EEEst[2,1],3)), face = c("plain"))


#Find age only intercept value
linearhappy <- lm(happy ~ Age, data = PISA)
linearfear <- lm(fear ~ Age, data = PISA)
lineardisgust <- lm(disgust ~ Age, data = PISA)
linearanger <- lm(anger ~ Age, data = PISA)
linearsurprise <- lm(surprise ~ Age, data = PISA)
linearsad <- lm(sad ~ Age, data = PISA)
linearneutral <- lm(neutral ~ Age, data = PISA)
linearfeel <- lm(feel ~ Age, data = PISA)
linearthink <- lm(think ~ Age, data = PISA)
lineardo <- lm(do ~ Age, data = PISA)
linearcog <- lm(Cog_ToM ~ Age, data = PISA)
linearEC <- lm(EC ~ Age, data = PISA)
linearPD <- lm(PD ~ Age, data = PISA)
linearEE <- lm(EE ~ Age, data = PISA)

#Create predicted values for regression lines
happyvalues <- ages[1:43] * happyEst[2,1] + summary(linearhappy)$coefficients[1,1]
fearvalues <- ages[1:43] * fearEst[2,1] + summary(linearfear)$coefficients[1,1]
disgustvalues <- ages[1:43] * disgustEst[2,1] + summary(lineardisgust)$coefficients[1,1]
angervalues <- ages[1:43] * angerEst[2,1] + summary(linearanger)$coefficients[1,1]
surprisevalues <- ages[1:43] * surpriseEst[2,1] + summary(linearsurprise)$coefficients[1,1]
sadvalues <- ages[1:43] * sadEst[2,1] + summary(linearsad)$coefficients[1,1]
neutralvalues <- ages[1:43] * neutralEst[2,1] + summary(linearneutral)$coefficients[1,1]
feelvalues <- ages[1:43] * feelEst[2,1] + summary(linearfeel)$coefficients[1,1]
thinkvalues <- ages[1:43] * thinkEst[2,1] + summary(linearthink)$coefficients[1,1]
dovalues <- ages[1:43] * doEst[2,1] + summary(lineardo)$coefficients[1,1]
cogvalues <- ages[1:43] * cogEst[2,1] + summary(linearcog)$coefficients[1,1]
ECvalues <- ages[1:43] * ECEst[2,1] + summary(linearEC)$coefficients[1,1]
PDvalues <- ages[1:43] * PDEst[2,1] + summary(linearPD)$coefficients[1,1]
EEvalues <- ages[1:43] * EEEst[2,1] + summary(linearEE)$coefficients[1,1]

#Create minimum values for error shading
happymin <- ages[1:43] * happyCI[2,1] + summary(linearhappy)$coefficients[1,1]
fearmin <- ages[1:43] * fearCI[2,1] + summary(linearfear)$coefficients[1,1]
disgustmin <- ages[1:43] * disgustCI[2,1] + summary(lineardisgust)$coefficients[1,1]
angermin <- ages[1:43] * angerCI[2,1] + summary(linearanger)$coefficients[1,1]
surprisemin <- ages[1:43] * surpriseCI[2,1] + summary(linearsurprise)$coefficients[1,1]
sadmin <- ages[1:43] * sadCI[2,1] + summary(linearsad)$coefficients[1,1]
neutralmin <- ages[1:43] * neutralCI[2,1] + summary(linearneutral)$coefficients[1,1]
feelmin <- ages[1:43] * feelCI[2,1] + summary(linearfeel)$coefficients[1,1]
thinkmin <- ages[1:43] * thinkCI[2,1] + summary(linearthink)$coefficients[1,1]
domin <- ages[1:43] * doCI[2,1] + summary(lineardo)$coefficients[1,1]
cogmin <- ages[1:43] * cogCI[2,1] + summary(linearcog)$coefficients[1,1]
ECmin <- ages[1:43] * ECCI[2,1] + summary(linearEC)$coefficients[1,1]
PDmin <- ages[1:43] * PDCI[2,1] + summary(linearPD)$coefficients[1,1]
EEmin <- ages[1:43] * EECI[2,1] + summary(linearEE)$coefficients[1,1]

#Create maximum values for error shading
happymax <- ages[1:43] * happyCI[2,2] + summary(linearhappy)$coefficients[1,1]
fearmax <- ages[1:43] * fearCI[2,2] + summary(linearfear)$coefficients[1,1]
disgustmax <- ages[1:43] * disgustCI[2,2] + summary(lineardisgust)$coefficients[1,1]
angermax <- ages[1:43] * angerCI[2,2] + summary(linearanger)$coefficients[1,1]
surprisemax <- ages[1:43] * surpriseCI[2,2] + summary(linearsurprise)$coefficients[1,1]
sadmax <- ages[1:43] * sadCI[2,2] + summary(linearsad)$coefficients[1,1]
neutralmax <- ages[1:43] * neutralCI[2,2] + summary(linearneutral)$coefficients[1,1]
feelmax <- ages[1:43] * feelCI[2,2] + summary(linearfeel)$coefficients[1,1]
thinkmax <- ages[1:43] * thinkCI[2,2] + summary(linearthink)$coefficients[1,1]
domax <- ages[1:43] * doCI[2,2] + summary(lineardo)$coefficients[1,1]
cogmax <- ages[1:43] * cogCI[2,2] + summary(linearcog)$coefficients[1,1]
ECmax <- ages[1:43] * ECCI[2,2] + summary(linearEC)$coefficients[1,1]
PDmax <- ages[1:43] * PDCI[2,2] + summary(linearPD)$coefficients[1,1]
EEmax <- ages[1:43] * EECI[2,2] + summary(linearEE)$coefficients[1,1]

#Create domain variable
happydomain <- factor(c(rep("Happy", 43)))
feardomain <- factor(c(rep("Fear", 43)))
disgustdomain <- factor(c(rep("Disgust", 43)))
angerdomain <- factor(c(rep("Anger", 43)))
surprisedomain <- factor(c(rep("Surprise", 43)))
saddomain <- factor(c(rep("Sad", 43)))
neutraldomain <- factor(c(rep("Neutral", 43)))
feeldomain <- factor(c(rep("Feel", 43)))
thinkdomain <- factor(c(rep("Think", 43)))
dodomain <- factor(c(rep("Do", 43)))
cogdomain <- factor(c(rep("Cog", 43)))
ECdomain <- factor(c(rep("EC", 43)))
PDdomain <- factor(c(rep("PD", 43)))
EEdomain <- factor(c(rep("EE", 43)))

#Create line data 
happyline <- data.frame(ages, happydomain, happyvalues, happymin, happymax)
fearline <- data.frame(ages, feardomain, fearvalues, fearmin, fearmax)
disgustline <- data.frame(ages, disgustdomain, disgustvalues, disgustmin, disgustmax)
angerline <- data.frame(ages, angerdomain, angervalues, angermin, angermax)
surpriseline <- data.frame(ages, surprisedomain, surprisevalues, surprisemin, surprisemax)
sadline <- data.frame(ages, saddomain, sadvalues, sadmin, sadmax)
neutralline <- data.frame(ages, neutraldomain, neutralvalues, neutralmin, neutralmax)
feelline <- data.frame(ages, feeldomain, feelvalues, feelmin, feelmax)
thinkline <- data.frame(ages, thinkdomain, thinkvalues, thinkmin, thinkmax)
doline <- data.frame(ages, dodomain, dovalues, domin, domax)
cogline <- data.frame(ages, cogdomain, cogvalues, cogmin, cogmax)
ECline <- data.frame(ages, ECdomain, ECvalues, ECmin, ECmax)
PDline <- data.frame(ages, PDdomain, PDvalues, PDmin, PDmax)
EEline <- data.frame(ages, EEdomain, EEvalues, EEmin, EEmax)

#### Create figures #### 
#Happy
tiff(filename = "happy.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(happydf, aes(x = Age, y = value)) + 
  geom_jitter() + 
  geom_line(data = happyline, aes(x = ages, y = happyvalues), col = "Red", size = 1) + 
  geom_ribbon(data = happyline, aes(x = ages, ymin = happymin, ymax = happymax), inherit.aes = FALSE, alpha = 0.2) +
  xlab("Age") +
  ylab("Value") +
  #ylim(0,10) +
  ggtitle("Happy") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = happybeta, aes(label = labels, x = 71, y = 10, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Fear
tiff(filename = "fear.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(feardf, aes(x = Age, y = value)) + 
  geom_jitter() + 
  geom_line(data = fearline, aes(x = ages, y = fearvalues), col = "Red", size = 1) + 
  geom_ribbon(data = fearline, aes(x = ages, ymin = fearmin, ymax = fearmax), inherit.aes = FALSE, alpha = 0.2) +
  xlab("Age") +
  ylab("Value") +
  ylim(-1,10) +
  ggtitle("Fear") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = fearbeta, aes(label = labels, x = 71, y = 10, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Disgust
tiff(filename = "disgust.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(disgustdf, aes(x = Age, y = value)) + 
  geom_jitter() + 
  geom_line(data = disgustline, aes(x = ages, y = disgustvalues), col = "Red", size = 1) + 
  geom_ribbon(data = disgustline, aes(x = ages, ymin = disgustmin, ymax = disgustmax), inherit.aes = FALSE, alpha = 0.2) +
  xlab("Age") +
  ylab("Value") +
  #ylim(0,10) +
  ggtitle("Disgust") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = disgustbeta, aes(label = labels, x = 71, y = 10, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Anger
tiff(filename = "anger.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(angerdf, aes(x = Age, y = value)) + 
  geom_jitter() + 
  geom_line(data = angerline, aes(x = ages, y = angervalues), col = "Red", size = 1) + 
  geom_ribbon(data = angerline, aes(x = ages, ymin = angermin, ymax = angermax), inherit.aes = FALSE, alpha = 0.2) +
  xlab("Age") +
  ylab("Value") +
  #ylim(0,10) +
  ggtitle("Anger") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = angerbeta, aes(label = labels, x = 71, y = 10, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Surprise
tiff(filename = "surprise.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(surprisedf, aes(x = Age, y = value)) + 
  geom_jitter() + 
  geom_line(data = surpriseline, aes(x = ages, y = surprisevalues), col = "Red", size = 1) + 
  geom_ribbon(data = surpriseline, aes(x = ages, ymin = surprisemin, ymax = surprisemax), inherit.aes = FALSE, alpha = 0.2) +
  xlab("Age") +
  ylab("Value") +
  #ylim(0,10) +
  ggtitle("Surprise") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = surprisebeta, aes(label = labels, x = 71, y = 10, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Sad
tiff(filename = "sad.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(saddf, aes(x = Age, y = value)) + 
  geom_jitter() + 
  geom_line(data = sadline, aes(x = ages, y = sadvalues), col = "Red", size = 1) + 
  geom_ribbon(data = sadline, aes(x = ages, ymin = sadmin, ymax = sadmax), inherit.aes = FALSE, alpha = 0.2) +
  xlab("Age") +
  ylab("Value") +
  #ylim(0,10) +
  ggtitle("Sad") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = sadbeta, aes(label = labels, x = 71, y = 10, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Neutral
tiff(filename = "neutral.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(neutraldf, aes(x = Age, y = value)) + 
  geom_jitter() + 
  geom_line(data = neutralline, aes(x = ages, y = neutralvalues), col = "Red", size = 1) + 
  geom_ribbon(data = neutralline, aes(x = ages, ymin = neutralmin, ymax = neutralmax), inherit.aes = FALSE, alpha = 0.2) +
  xlab("Age") +
  ylab("Value") +
  #ylim(0,10) +
  ggtitle("Neutral") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = neutralbeta, aes(label = labels, x = 71, y = 10, hjust = 0, vjust = 1, fontface = face))
dev.off()

#Feel
tiff(filename = "feel.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(feeldf, aes(x = Age, y = value)) + 
  geom_jitter() + 
  geom_line(data = feelline, aes(x = ages, y = feelvalues), col = "Red", size = 1) + 
  geom_ribbon(data = feelline, aes(x = ages, ymin = feelmin, ymax = feelmax), inherit.aes = FALSE, alpha = 0.2) +
  xlab("Age") +
  ylab("Value") +
  #ylim(0,10) +
  ggtitle("Affective ToM") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = feelbeta, aes(label = labels, x = 71, y = 10, hjust = 0, vjust = 1, fontface = "bold"))
dev.off()

#Think
tiff(filename = "think.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(thinkdf, aes(x = Age, y = value)) + 
  geom_jitter() + 
  geom_line(data = thinkline, aes(x = ages, y = thinkvalues), col = "Red", size = 1) + 
  geom_ribbon(data = thinkline, aes(x = ages, ymin = thinkmin, ymax = thinkmax), inherit.aes = FALSE, alpha = 0.2) +
  xlab("Age") +
  ylab("Value") +
  #ylim(0,10) +
  ggtitle("First-order cognitive ToM") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = thinkbeta, aes(label = labels, x = 71, y = 10, hjust = 0, vjust = 1, fontface = "bold"))
dev.off()

#Do
tiff(filename = "do.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(dodf, aes(x = Age, y = value)) + 
  geom_jitter() + 
  geom_line(data = doline, aes(x = ages, y = dovalues), col = "Red", size = 1) + 
  geom_ribbon(data = doline, aes(x = ages, ymin = domin, ymax = domax), inherit.aes = FALSE, alpha = 0.2) +
  xlab("Age") +
  ylab("Value") +
  #ylim(0,10) +
  ggtitle("Second-order cognitive ToM") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = dobeta, aes(label = labels, x = 71, y = 10, hjust = 0, vjust = 1, fontface = "bold"))
dev.off()

#Cog
tiff(filename = "cog.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(cogdf, aes(x = Age, y = value)) + 
  geom_jitter() + 
  geom_line(data = cogline, aes(x = ages, y = cogvalues), col = "Red", size = 1) + 
  geom_ribbon(data = cogline, aes(x = ages, ymin = cogmin, ymax = cogmax), inherit.aes = FALSE, alpha = 0.2) +
  xlab("Age") +
  ylab("Value") +
  #ylim(0,10) +
  ggtitle("Total cognitive ToM") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = cogbeta, aes(label = labels, x = 71, y = 18, hjust = 0, vjust = 1, fontface = "bold"))
dev.off()

#EC
tiff(filename = "EC.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(ECdf, aes(x = Age, y = value)) + 
  geom_jitter() + 
  geom_line(data = ECline, aes(x = ages, y = ECvalues), col = "Red", size = 1) + 
  geom_ribbon(data = ECline, aes(x = ages, ymin = ECmin, ymax = ECmax), inherit.aes = FALSE, alpha = 0.2) +
  xlab("Age") +
  ylab("Value") +
  #ylim(0,10) +
  ggtitle("Empathic Concern") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = ECbeta, aes(label = labels, x = 71, y = 30, hjust = 0, vjust = 1, fontface = face))
dev.off()

tiff(filename = "PD.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(PDdf, aes(x = Age, y = value)) + 
  geom_jitter() + 
  geom_line(data = PDline, aes(x = ages, y = PDvalues), col = "Red", size = 1) + 
  geom_ribbon(data = PDline, aes(x = ages, ymin = PDmin, ymax = PDmax), inherit.aes = FALSE, alpha = 0.2) +
  xlab("Age") +
  ylab("Value") +
  #ylim(0,10) +
  ggtitle("Personal Distress") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = PDbeta, aes(label = labels, x = 71, y = 30, hjust = 0, vjust = 1, fontface = face))
dev.off()

#EE
tiff(filename = "EE.tiff", width = 4, height = 4, units = "in", res = 250)
ggplot(EEdf, aes(x = Age, y = value)) + 
  geom_jitter() + 
  geom_line(data = EEline, aes(x = ages, y = EEvalues), col = "Red", size = 1) + 
  geom_ribbon(data = EEline, aes(x = ages, ymin = EEmin, ymax = EEmax), inherit.aes = FALSE, alpha = 0.2) +
  xlab("Age") +
  ylab("Value") +
  #ylim(0,10) +
  ggtitle("Emotional Empathy Total") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(data = EEbeta, aes(label = labels, x = 71, y = 55, hjust = 0, vjust = 1, fontface = face))
dev.off()

# MULTIPLE LINEAR REGRESSIONS (MINIMAL MODEL)-------------------------------------
#Happy
minhappy <- lm(happy ~ Age + sex, data = PISA)
summary(minhappy)

#fear
minfear <- lm(fear ~ Age + sex, data = PISA)
summary(minfear)

#disgust
mindisgust <- lm(disgust ~ Age + sex, data = PISA)
summary(mindisgust)

#anger
minanger <- lm(anger ~ Age + sex, data = PISA)
summary(minanger)

#surprise
minsurprise <- lm(surprise ~ Age + sex, data = PISA)
summary(minsurprise)

#sad
minsad <- lm(sad ~ Age + sex, data = PISA)
summary(minsad)

#neutral
minneutral <- lm(neutral ~ Age + sex, data = PISA)
summary(minneutral)

#do
mindo <- lm(do ~ Age + sex, data = PISA)
summary(mindo)

#think
minthink <- lm(think ~ Age + sex, data = PISA)
summary(minthink)

#feel
minfeel <- lm(feel ~ Age + sex, data = PISA)
summary(minfeel)

#cog
mincog <- lm(Cog_ToM ~ Age + sex, data = PISA)
summary(mincog)

#EC
minEC <- lm(EC ~ Age + sex, data = PISA)
summary(minEC)

#PD
minPD <- lm(PD ~ Age + sex, data = PISA)
summary(minPD)

#EE
minEE <- lm(EE ~ Age + sex, data = PISA)
summary(minEE)



# ASSUMPTION CHECKS (MINIMAL MODEL)-----------------------------------------------

# Linearity ---------------------------------------------------------------
#Happy
plot(minhappy, 1)

#fear
plot(minfear, 1)

#disgust
plot(mindisgust, 1)

#anger
plot(minanger, 1)

#surprise
plot(minsurprise, 1)

#sad
plot(minsad, 1)

#neutral
plot(minneutral, 1)

#do
plot(mindo, 1)

#think
plot(minthink, 1)

#feel
plot(minfeel, 1)

#cog
plot(mincog, 1)

#EC
plot(minEC, 1)

#PD
plot(minPD, 1)

#EE
plot(minEE, 1)

# Normality --------------------------------------------
#Happy
sresidminhappy <- studres(minhappy)
shapiro.test(sresidminhappy) #Not normal

#fear
sresidminfear <- studres(minfear)
shapiro.test(sresidminfear) #Not normal

#disgust
sresidmindisgust <- studres(mindisgust)
shapiro.test(sresidmindisgust) #Not normal

#anger
sresidminanger <- studres(minanger)
shapiro.test(sresidminanger) #Not normal

#surprise
sresidminsurprise <- studres(minsurprise)
shapiro.test(sresidminsurprise) #Not normal

#sad
sresidminsad <- studres(minsad)
shapiro.test(sresidminsad) #Not normal

#neutral
sresidminneutral <- studres(minneutral)
shapiro.test(sresidminneutral) #Not normal

#do
sresidmindo <- studres(mindo)
shapiro.test(sresidmindo) #Not normal

#think
sresidminthink <- studres(minthink)
shapiro.test(sresidminthink) #Not normal

#feel
sresidminfeel <- studres(minfeel)
shapiro.test(sresidminfeel) #Not normal

#cog
sresidmincog <- studres(mincog)
shapiro.test(sresidmincog) #Not normal

#EC
sresidminEC <- studres(minEC)
shapiro.test(sresidminEC)

#PD
sresidminPD <- studres(minPD)
shapiro.test(sresidminPD) #Not normal

#EE
sresidminEE <- studres(minEE)
shapiro.test(sresidminEE) #Not normal


# Homoscedasticity -----------------------------------------
#happy
ncvTest(minhappy)

#fear
ncvTest(minfear)

#disgust
ncvTest(mindisgust) #homoscedastic

#anger
ncvTest(minanger)  #homoscedastic

#surprise
ncvTest(minsurprise)  #homoscedastic

#sad
ncvTest(minsad)

#neutral
ncvTest(minneutral)

#do
ncvTest(mindo)

#think
ncvTest(minthink)

#feel
ncvTest(minfeel)

#cog
ncvTest(mincog)

#EC
ncvTest(minEC)

#PD
ncvTest(minPD)

#EE
ncvTest(minEE)

# ROBUST MODELS (MINIMAL MODEL)------------------------------------------------------

#Happy
happyminEst <- coeftest(minhappy, vcov. = vcovHC(minhappy, type = "HC1"))
happyminCI <- coefci(minhappy, vcov. = vcovHC(minhappy, type = "HC1"))

#fear
fearminEst <- coeftest(minfear, vcov. = vcovHC(minfear, type = "HC1"))
fearminCI <- coefci(minfear, vcov. = vcovHC(minfear, type = "HC1"))

#disgust
disgustminEst <- coeftest(mindisgust, vcov. = vcovHC(mindisgust, type = "HC1"))
disgustminCI <- coefci(mindisgust, vcov. = vcovHC(mindisgust, type = "HC1"))

#anger
angerminEst <- coeftest(minanger, vcov. = vcovHC(minanger, type = "HC1"))
angerminCI <- coefci(minanger, vcov. = vcovHC(minanger, type = "HC1"))

#surprise
surpriseminEst <- coeftest(minsurprise, vcov. = vcovHC(minsurprise, type = "HC1"))
surpriseminCI <- coefci(minsurprise, vcov. = vcovHC(minsurprise, type = "HC1"))

#sad
sadminEst <- coeftest(minsad, vcov. = vcovHC(minsad, type = "HC1"))
sadminCI <- coefci(minsad, vcov. = vcovHC(minsad, type = "HC1"))

#neutral
neutralminEst <- coeftest(minneutral, vcov. = vcovHC(minneutral, type = "HC1"))
neutralminCI <- coefci(minneutral, vcov. = vcovHC(minneutral, type = "HC1"))

#do
dominEst <- coeftest(mindo, vcov. = vcovHC(mindo, type = "HC1"))
dominCI <- coefci(mindo, vcov. = vcovHC(mindo, type = "HC1"))

#think
thinkminEst <- coeftest(minthink, vcov. = vcovHC(minthink, type = "HC1"))
thinkminCI <- coefci(minthink, vcov. = vcovHC(minthink, type = "HC1"))

#feel
feelminEst <- coeftest(minfeel, vcov. = vcovHC(minfeel, type = "HC1"))
feelminCI <- coefci(minfeel, vcov. = vcovHC(minfeel, type = "HC1"))

#cog
cogminEst <- coeftest(mincog, vcov. = vcovHC(mincog, type = "HC1"))
cogminCI <- coefci(mincog, vcov. = vcovHC(mincog, type = "HC1"))

#EC
ECminEst <- coeftest(minEC, vcov. = vcovHC(minEC, type = "HC1"))
ECminCI <- coefci(minEC, vcov. = vcovHC(minEC, type = "HC1"))

#PD
PDminEst <- coeftest(minPD, vcov. = vcovHC(minPD, type = "HC1"))
PDminCI <- coefci(minPD, vcov. = vcovHC(minPD, type = "HC1"))

#EE
EEminEst <- coeftest(minEE, vcov. = vcovHC(minEE, type = "HC1"))
EEminCI <- coefci(minEE, vcov. = vcovHC(minEE, type = "HC1"))



# STANDARDISED BETAS (MINIMAL MODEL) ---------------------------------------------
#Happy
minhappyS <- lm(happy ~ Age + sex, data = PISAstandard1)
minhappyBS <- coeftest(minhappyS , vcov. = vcovHC(minhappyS, type = "HC1"))

#fear
minfearS <- lm(fear ~ Age + sex, data = PISAstandard1)
minfearBS <- coeftest(minfearS , vcov. = vcovHC(minfearS, type = "HC1"))

#disgust
mindisgustS <- lm(disgust ~ Age + sex, data = PISAstandard1)
mindisgustBS <- coeftest(mindisgustS , vcov. = vcovHC(mindisgustS, type = "HC1"))

#anger
minangerS <- lm(anger ~ Age + sex, data = PISAstandard1)
minangerBS <- coeftest(minangerS , vcov. = vcovHC(minangerS, type = "HC1"))

#surprise
minsurpriseS <- lm(surprise ~ Age + sex, data = PISAstandard1)
minsurpriseBS <- coeftest(minsurpriseS , vcov. = vcovHC(minsurpriseS, type = "HC1"))

#sad
minsadS <- lm(sad ~ Age + sex, data = PISAstandard1)
minsadBS <- coeftest(minsadS , vcov. = vcovHC(minsadS, type = "HC1"))

#neutral
minneutralS <- lm(neutral ~ Age + sex, data = PISAstandard1)
minneutralBS <- coeftest(minneutralS , vcov. = vcovHC(minneutralS, type = "HC1"))

#do
mindoS <- lm(do ~ Age + sex, data = PISAstandard1)
mindoBS <- coeftest(mindoS , vcov. = vcovHC(mindoS, type = "HC1"))

#think
minthinkS <- lm(think ~ Age + sex, data = PISAstandard1)
minthinkBS <- coeftest(minthinkS , vcov. = vcovHC(minthinkS, type = "HC1"))

#feel
minfeelS <- lm(feel ~ Age + sex, data = PISAstandard1)
minfeelBS <- coeftest(minfeelS , vcov. = vcovHC(minfeelS, type = "HC1"))

#cog
mincogS <- lm(Cog_ToM ~ Age + sex, data = PISAstandard1)
mincogBS <- coeftest(mincogS , vcov. = vcovHC(mincogS, type = "HC1"))

#EC
minECS <- lm(EC ~ Age + sex, data = PISAstandard1)
minECBS <- coeftest(minECS , vcov. = vcovHC(minECS, type = "HC1"))

#PD
minPDS <- lm(PD ~ Age + sex, data = PISAstandard1)
minPDBS <- coeftest(minPDS , vcov. = vcovHC(minPDS, type = "HC1"))

#EE
minEES <- lm(EE ~ Age + sex, data = PISAstandard1)
minEEBS <- coeftest(minEES , vcov. = vcovHC(minEES, type = "HC1"))


# TABLE (MINIMAL MODEL) ----------------------------------------------------------
#data frames for each regression model 
#Happy
happyminregression <- data.frame(Outcome = c("Happy"),
                                 Variable = c("Age", "Sex"), 
                                 R2 = c(summary(minhappy)$r.squared),
                                 B = c(happyminEst[2:3,1]),
                                 BS = c(minhappyBS[2:3,1]), 
                                 CILower = c(happyminCI[2:3,1]),
                                 CIUpper = c(happyminCI[2:3,2]),
                                 p = c(happyminEst[2:3,4]))

#fear
fearminregression <- data.frame(Outcome = c("fear"),
                                Variable = c("Age", "Sex"), 
                                R2 = c(summary(minfear)$r.squared),
                                B = c(fearminEst[2:3,1]),
                                BS = c(minfearBS[2:3,1]), 
                                CILower = c(fearminCI[2:3,1]),
                                CIUpper = c(fearminCI[2:3,2]),
                                p = c(fearminEst[2:3,4]))

#disgust
disgustminregression <- data.frame(Outcome = c("disgust"),
                                   Variable = c("Age", "Sex"), 
                                   R2 = c(summary(mindisgust)$r.squared),
                                   B = c(disgustminEst[2:3,1]),
                                   BS = c(mindisgustBS[2:3,1]), 
                                   CILower = c(disgustminCI[2:3,1]),
                                   CIUpper = c(disgustminCI[2:3,2]),
                                   p = c(disgustminEst[2:3,4]))


#anger
angerminregression <- data.frame(Outcome = c("anger"),
                                 Variable = c("Age", "Sex"), 
                                 R2 = c(summary(minanger)$r.squared),
                                 B = c(angerminEst[2:3,1]),
                                 BS = c(minangerBS[2:3,1]), 
                                 CILower = c(angerminCI[2:3,1]),
                                 CIUpper = c(angerminCI[2:3,2]),
                                 p = c(angerminEst[2:3,4]))


#surprise
surpriseminregression <- data.frame(Outcome = c("surprise"),
                                    Variable = c("Age", "Sex"), 
                                    R2 = c(summary(minsurprise)$r.squared),
                                    B = c(surpriseminEst[2:3,1]),
                                    BS = c(minsurpriseBS[2:3,1]), 
                                    CILower = c(surpriseminCI[2:3,1]),
                                    CIUpper = c(surpriseminCI[2:3,2]),
                                    p = c(surpriseminEst[2:3,4]))


#sad
sadminregression <- data.frame(Outcome = c("sad"),
                               Variable = c("Age", "Sex"), 
                               R2 = c(summary(minsad)$r.squared),
                               B = c(sadminEst[2:3,1]),
                               BS = c(minsadBS[2:3,1]), 
                               CILower = c(sadminCI[2:3,1]),
                               CIUpper = c(sadminCI[2:3,2]),
                               p = c(sadminEst[2:3,4]))


#neutral
neutralminregression <- data.frame(Outcome = c("neutral"),
                                   Variable = c("Age", "Sex"), 
                                   R2 = c(summary(minneutral)$r.squared),
                                   B = c(neutralminEst[2:3,1]),
                                   BS = c(minneutralBS[2:3,1]), 
                                   CILower = c(neutralminCI[2:3,1]),
                                   CIUpper = c(neutralminCI[2:3,2]),
                                   p = c(neutralminEst[2:3,4]))


#do
dominregression <- data.frame(Outcome = c("do"),
                              Variable = c("Age", "Sex"), 
                              R2 = c(summary(mindo)$r.squared),
                              B = c(dominEst[2:3,1]),
                              BS = c(mindoBS[2:3,1]), 
                              CILower = c(dominCI[2:3,1]),
                              CIUpper = c(dominCI[2:3,2]),
                              p = c(dominEst[2:3,4]))


#think
thinkminregression <- data.frame(Outcome = c("think"),
                                 Variable = c("Age", "Sex"), 
                                 R2 = c(summary(minthink)$r.squared),
                                 B = c(thinkminEst[2:3,1]),
                                 BS = c(minthinkBS[2:3,1]), 
                                 CILower = c(thinkminCI[2:3,1]),
                                 CIUpper = c(thinkminCI[2:3,2]),
                                 p = c(thinkminEst[2:3,4]))


#feel
feelminregression <- data.frame(Outcome = c("feel"),
                                Variable = c("Age", "Sex"), 
                                R2 = c(summary(minfeel)$r.squared),
                                B = c(feelminEst[2:3,1]),
                                BS = c(minfeelBS[2:3,1]), 
                                CILower = c(feelminCI[2:3,1]),
                                CIUpper = c(feelminCI[2:3,2]),
                                p = c(feelminEst[2:3,4]))


#cog
cogminregression <- data.frame(Outcome = c("cog"),
                               Variable = c("Age", "Sex"), 
                               R2 = c(summary(mincog)$r.squared),
                               B = c(cogminEst[2:3,1]),
                               BS = c(mincogBS[2:3,1]), 
                               CILower = c(cogminCI[2:3,1]),
                               CIUpper = c(cogminCI[2:3,2]),
                               p = c(cogminEst[2:3,4]))


#EC
ECminregression <- data.frame(Outcome = c("EC"),
                              Variable = c("Age", "Sex"), 
                              R2 = c(summary(minEC)$r.squared),
                              B = c(ECminEst[2:3,1]),
                              BS = c(minECBS[2:3,1]), 
                              CILower = c(ECminCI[2:3,1]),
                              CIUpper = c(ECminCI[2:3,2]),
                              p = c(ECminEst[2:3,4]))


#PD
PDminregression <- data.frame(Outcome = c("PD"),
                              Variable = c("Age", "Sex"), 
                              R2 = c(summary(minPD)$r.squared),
                              B = c(PDminEst[2:3,1]),
                              BS = c(minPDBS[2:3,1]), 
                              CILower = c(PDminCI[2:3,1]),
                              CIUpper = c(PDminCI[2:3,2]),
                              p = c(PDminEst[2:3,4]))


#EE
EEminregression <- data.frame(Outcome = c("EE"),
                              Variable = c("Age", "Sex"), 
                              R2 = c(summary(minEE)$r.squared),
                              B = c(EEminEst[2:3,1]),
                              BS = c(minEEBS[2:3,1]), 
                              CILower = c(EEminCI[2:3,1]),
                              CIUpper = c(EEminCI[2:3,2]),
                              p = c(EEminEst[2:3,4]))

#Combine all regression data frames together
combinedminregressions <- rbind(happyminregression, fearminregression, disgustminregression, angerminregression, surpriseminregression, 
                                sadminregression, neutralminregression, dominregression, thinkminregression, 
                                feelminregression, cogminregression, ECminregression, 
                                PDminregression, EEminregression)


#Create flexable object
ftmin <- flextable(data = combinedminregressions %>%
                     mutate(p = sprintf("%.3g", p))) %>%
  theme_apa() %>%
  autofit()

#Create a temp file
tmp <- tempfile(fileext = ".docx")

#Create docx file
read_docx() %>%
  body_add_flextable(ftmin) %>%
  print(target = "mintable.docx")

#Create flexable object
ftmin <- flextable(data = combinedminregressions %>%
                     mutate(p = formatC(p, digits = 3, format = "fg"))) %>%
  theme_apa() %>%
  autofit()