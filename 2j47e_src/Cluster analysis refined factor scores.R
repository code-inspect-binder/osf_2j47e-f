#requires variables generated from Factor analysis.R
library(dplyr)
library(tidyr)
library(psych)
library(rstatix)
library(MOTE)

#hierarchical
HClusterD <- dist(EFA.mainData[, c(111:116)], method = "euclidean")
HClusterFit <- hclust(HClusterD, method="ward.D2")
HCluster <- cutree(HClusterFit, k = 3)

#recode so cluster 1 = sensory, cluster 2 = safety, cluster 3 = unsafety
#to keep order of clusters consistent across analyses
HCluster <- dplyr::recode(HCluster,
                          '1' = 1, 
                          '2' = 3, 
                          '3' = 2)

#check cluster agreement using hierarchical clustering on raw items instead of factors
HCheck.HClusterD <- dist(EFA.mainData[, c(2:56)], method = "euclidean")
HCheck.HClusterFit <- hclust(HCheck.HClusterD, method="ward.D2")

HCheck.HCluster <- cutree(HCheck.HClusterFit, k = 3)

HCheckCluster <- data.frame(Factor = HCluster, 
                            Item = HCheck.HCluster)

HCheckCluster <- cbind(HCheckCluster, 
                             ifelse(HCheckCluster[1] == HCheckCluster[2], 1, 0))
colnames(HCheckCluster)[3] <- 'Agreement'

HCheckClusterAgreement <- HCheckCluster[, c(2:3)] %>%
  group_by(Item) %>%
  summarise(total = n(), 
            n_agree = sum(Agreement))  %>%
  rename(Cluster = Item)

#Cohen's Kappa
cohen.kappa(HCheckCluster[, c(1:2)])

#kmeans
KClusterFit <- kmeans(EFA.mainData[, c(111:116)], 
                          centers = 3, 
                          nstart = 25)

#create dataframe with factor scores and cluster membership
KClusterScores <- cbind(EFA.mainData[, c(111:116)], KClusterFit$cluster)
colnames(KClusterScores)[7] <- "KCluster"

#recode KCluster to keep ordering consistent - note that kmeans() will randomly 
#assign groups, so ordering has to be manually fixed
KClusterScores$KCluster <- dplyr::recode(KClusterScores$KCluster,
                                             '1' = 3, 
                                             '2' = 1, 
                                             '3' = 2)

#table of factor score mean and sd by cluster
KClusterMean <- KClusterScores %>%
  group_by(KCluster) %>%
  summarise(across(Sensory:States,list(mean = mean, 
                                       sd = sd)), 
            n = n()) %>%
  pivot_longer(c(-KCluster,
                 -n),
               names_to = c("Factor", 
                            ".value"),
               names_sep = "_")

KClusterMean$KCluster <- factor(KClusterMean$KCluster)

KClusterMean$Factor <- factor(KClusterMean$Factor,
                                   levels = unique(KClusterMean$Factor))

#testing homogeneity of variance in factors
KClusterScores %>%
  pivot_longer(c(Sensory, 
                 CognitiveDemand, 
                 ThreatToSelf, 
                 CrossSettings, 
                 Safety, 
                 States),
               names_to = c("Factor")) %>%
  group_by(Factor) %>%
  levene_test(value ~ as.factor(KCluster))

#multivariate analysis of variance
summary(manova(cbind(Sensory, 
                     CognitiveDemand, 
                     ThreatToSelf, 
                     CrossSettings, 
                     Safety, 
                     States) ~ KCluster, 
               data = KClusterScores))

#omega-squared for MANOVA
omega.F(6, 261, 81.0343, 268, 0.05)

#univariate Welch's ANOVA test with Bonferroni correction
table1 <- KClusterScores %>%
  pivot_longer(c(Sensory, 
                 CognitiveDemand, 
                 ThreatToSelf, 
                 CrossSettings, 
                 Safety, 
                 States),
               names_to = c("Factor")) %>%
  mutate(Factor = factor(Factor, 
                          levels = c("Sensory", 
                                     "CognitiveDemand", 
                                     "ThreatToSelf", 
                                     "CrossSettings", 
                                     "Safety", 
                                     "States"))) %>%
  group_by(Factor) %>%
  welch_anova_test(value ~ as.factor(KCluster)) %>%
  adjust_pvalue(method = "bonferroni")

#manually calculating omega squared and adjusted confidence intervals for each test
table1$omegasq <- apply(table1, 1, 
                        function(x) omega.F(as.numeric(x[5]), 
                                            as.numeric(x[6]), 
                                            as.numeric(x[4]), 
                                            as.numeric(x[3]), 
                                            0.05/6)$omega)
table1$omegalow <- apply(table1, 1, 
                         function(x) omega.F(as.numeric(x[5]), 
                                             as.numeric(x[6]), 
                                             as.numeric(x[4]), 
                                             as.numeric(x[3]), 
                                             0.05/6)$omegalow)
table1$omegahigh <- apply(table1, 1, 
                          function(x) omega.F(as.numeric(x[5]), 
                                              as.numeric(x[6]), 
                                              as.numeric(x[4]), 
                                              as.numeric(x[3]), 
                                              0.05/6)$omegahigh)

#post-hoc Games-Howell, p-values adjusted with Tukey method
table2 <- KClusterScores %>%
  pivot_longer(c(Sensory, 
                 CognitiveDemand, 
                 ThreatToSelf, 
                 CrossSettings, 
                 Safety, 
                 States),
               names_to = c("Factor")) %>%
  mutate(Factor = factor(Factor, 
                          levels = c("Sensory", 
                                     "CognitiveDemand", 
                                     "ThreatToSelf", 
                                     "CrossSettings", 
                                     "Safety", 
                                     "States"))) %>%
  group_by(Factor) %>%
  games_howell_test(value ~ KCluster, detailed = TRUE)

#compare cluster membership between hierarchical vs kmeans

HvsKCluster <- data.frame(HCluster = HCluster, 
                          KCluster = KClusterScores$KCluster)

HvsKCluster <- cbind(HvsKCluster, 
                           ifelse(HvsKCluster[1] == HvsKCluster[2], 1, 0))
colnames(HvsKCluster)[3] <- 'Agreement'

HvsKClusterAgreement <- HvsKCluster[, c(2:3)] %>%
group_by(KCluster) %>%
  summarise(total = n(), n_agree = sum(Agreement)) %>%
  rename(Cluster = KCluster)

#Cohen's Kappa
cohen.kappa(HvsKCluster[, c(1:2)])
