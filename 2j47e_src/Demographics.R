#requires variables from Cluster analysis refined factor scores.R

library(dplyr)
library(tidyr)
library(rstatix)
library(MOTE)
library(rcompanion)

#median survey completion time (in seconds)
median(mainData$Time, na.rm = TRUE)

#age
summary(mainData$Age)
sd(mainData$Age)

#gender
summary(mainData$Gender)

#diagnoses
summary(mainData[, c(71:110)])

#SCQ
summary(mainData[, c(67:70)])
mainData[, c(67:70)] %>%
  summarise(across(SCQ_S:SCQ_Total, ~ sd(.x, na.rm = TRUE)))

#medication
summary(mainData$Medication)

#schooling status
summary(mainData$SchoolEmployment)

#SEN statement/register
summary(mainData[, c(61, 62)])

#support
summary(mainData[, c(63:65)])

#trauma
summary(mainData$Trauma)

#group by k-means factor clusters

demographics <- cbind(mainData[, c(57:59, 63, 66:110)], 
                      KClusterScores$KCluster)

colnames(demographics)[50] <- 'KCluster'

#age
demographics %>%
  group_by(KCluster) %>%
  summarise(n = n(), mean = mean(Age), sd = sd(Age))

#gender
demographics %>%
  group_by(KCluster) %>%
  count(Gender)

#diagnoses
demographics %>% 
  select(KCluster, 
         paste(colnames(demographics)[c(10:49)])) %>%
  pivot_longer(-KCluster, 
               names_to = 'Diagnosis') %>%
  filter(value == "Yes") %>%
  group_by(KCluster, Diagnosis, value) %>%
  count()

#SCQ domain and total scores
demographics %>%
  select(KCluster, SCQ_S, SCQ_C, SCQ_R, SCQ_Total) %>%
  pivot_longer(-KCluster, 
               names_to = 'Domain') %>%
  group_by(KCluster, Domain) %>%
  summarise(mean = mean(value, na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE))

#medication
demographics %>%
  group_by(KCluster) %>%
  count(Medication)

#support
demographics %>%
  group_by(KCluster) %>%
  count(Support)

#trauma
demographics %>%
  group_by(KCluster) %>%
  count(Trauma)


#ChiSquared - Gender
#create dataframe of gender and cluster membership, then exclude participant 
#with 'Other' as response to analyse with Chi-squared test
data.OtherExcluded <- demographics %>%
  select(Gender, KCluster) %>%
  filter(Gender != 'Other')
data.OtherExcluded$Gender <- droplevels(data.OtherExcluded$Gender)

#contingency table
gender.contable <- table(data.OtherExcluded$KCluster, data.OtherExcluded$Gender)
#Chi-squared test
chisq.test(gender.contable)
#Cramer's V
cramerV(gender.contable, ci=TRUE)


#ChiSquared - ASD
ASD.contable <- table(demographics$KCluster, demographics$Diagnosis_ASD)
chisq.test(ASD.contable)
cramerV(ASD.contable, ci=TRUE)
#post-hoc pairwise Chi-squared with Bonferroni correction
pairwiseNominalIndependence(ASD.contable,
                            compare = "row",
                            fisher  = FALSE,
                            gtest   = FALSE,
                            chisq   = TRUE,
                            method  = "bonferroni",
                            digits  = 3)


#ChiSquared - ADHD
ADHD.contable <- table(demographics$KCluster, demographics$Diagnosis_ADHD)
chisq.test(ADHD.contable)
cramerV(ADHD.contable, ci=TRUE)
pairwiseNominalIndependence(ADHD.contable,
                            compare = "row",
                            fisher  = FALSE,
                            gtest   = FALSE,
                            chisq   = TRUE,
                            method  = "bonferroni",
                            digits  = 3)


#ChiSquared - Intellectual disability
ID.contable <- table(demographics$KCluster, demographics$Diagnosis_LearningDisability)
chisq.test(ID.contable)
cramerV(ID.contable, ci=TRUE)
pairwiseNominalIndependence(ID.contable,
                            compare = "row",
                            fisher  = FALSE,
                            gtest   = FALSE,
                            chisq   = TRUE,
                            method  = "bonferroni",
                            digits  = 3)


#ChiSquared - Learning difficulty
LDiff.contable <- table(demographics$KCluster, demographics$Diagnosis_LearningDifficulty)
chisq.test(LDiff.contable)
cramerV(LDiff.contable, ci=TRUE)


#ChiSquared - Anxiety
Anxiety.contable <- table(demographics$KCluster, demographics$Diagnosis_Anxiety)
chisq.test(Anxiety.contable)
cramerV(Anxiety.contable, ci=TRUE)


#ChiSquared - Depression
Depression.contable <- table(demographics$KCluster, demographics$Diagnosis_Depression)
chisq.test(Depression.contable)
cramerV(Depression.contable, ci=TRUE)


#ChiSquared - SPD
SPD.contable <- table(demographics$KCluster, demographics$Diagnosis_Sensory)
chisq.test(SPD.contable)
cramerV(SPD.contable, ci=TRUE)
pairwiseNominalIndependence(SPD.contable,
                            compare = "row",
                            fisher  = FALSE,
                            gtest   = FALSE,
                            chisq   = TRUE,
                            method  = "bonferroni",
                            digits  = 3)


#ChiSquared - Medication
Medication.contable <- table(demographics$KCluster, demographics$Medication)
chisq.test(Medication.contable)
cramerV(Medication.contable, ci=TRUE)


#ChiSquared - Support
Support.contable <- table(demographics$KCluster, demographics$Support)
chisq.test(Support.contable)
cramerV(Support.contable, ci=TRUE)


#ChiSquared - Trauma
Trauma.data <- demographics %>%
  select(Trauma, KCluster)
  filter(Trauma == 'Yes'| Trauma == 'No')
Trauma.data$Trauma <- droplevels(Trauma.data$Trauma)
Trauma.contable <- table(Trauma.data$KCluster, Trauma.data$Trauma)
chisq.test(Trauma.contable)
cramerV(Trauma.contable, ci=TRUE)
pairwiseNominalIndependence(Trauma.contable,
                            compare = "row",
                            fisher  = FALSE,
                            gtest   = FALSE,
                            chisq   = TRUE,
                            method  = "bonferroni",
                            digits  = 3)


#Age
#homogeneity of variance
demographics %>%
  levene_test(Age ~ as.factor(demographics$KCluster))

#Welch's ANOVA
demographics %>%
  welch_anova_test(Age ~ as.factor(KCluster)) %>%
  mutate(omegasq = omega.F(as.numeric(DFn), 
                           as.numeric(DFd), 
                           as.numeric(statistic), 
                           as.numeric(n), 
                           0.05)$omega,
         omegasqlow = omega.F(as.numeric(DFn), 
                              as.numeric(DFd), 
                              as.numeric(statistic), 
                              as.numeric(n), 
                              0.05)$omegalow,
         omegasqhigh = omega.F(as.numeric(DFn), 
                               as.numeric(DFd), 
                               as.numeric(statistic), 
                               as.numeric(n), 
                               0.05)$omegahigh)

#SCQ domain and total scores
#homogeneity of variance
demographics %>%
  filter(!is.na(SCQ_S)) %>%
  pivot_longer(c(SCQ_S,
                 SCQ_C, 
                 SCQ_R, 
                 SCQ_Total),
               names_to = c("Domain")) %>%
  group_by(Domain) %>%
  levene_test(value ~ as.factor(KCluster))

#MANOVA
SCQ.manova <- manova(cbind(SCQ_S, 
                           SCQ_C, 
                           SCQ_R, 
                           SCQ_Total) ~ KCluster, 
                     data = demographics)
#Welch's ANOVA
table5 <- demographics %>%
  filter(!is.na(SCQ_S)) %>%
  pivot_longer(c(SCQ_S,
                 SCQ_C, 
                 SCQ_R, 
                 SCQ_Total),
               names_to = c("Domain")) %>%
  group_by(Domain) %>%
  welch_anova_test(value ~ as.factor(KCluster)) %>%
  adjust_pvalue(method = "bonferroni") %>%
  mutate(omegasq = (DFn * (statistic - 1))/((DFn * (statistic - 1)) + n))

#Games-Howell post-hoc
table6 <- demographics %>%
  filter(!is.na(SCQ_S)) %>%
  pivot_longer(c(SCQ_S,
                 SCQ_C, 
                 SCQ_R, 
                 SCQ_Total),
               names_to = c("Domain")) %>%
  group_by(Domain) %>%
  games_howell_test(value ~ KCluster, detailed = TRUE)
