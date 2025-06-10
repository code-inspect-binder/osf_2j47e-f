#requires variables generated from Factor analysis.R 
#and Cluster analysis refined factor scores.R 
library(dplyr)
library(tidyr)
library(psych)
library(rstatix)
library(MOTE)

#create a matrix of relevant item loadings to calculate non-refined factor (scale) scores
scaleMatrix <- data.frame(data = matrix(EFA$loadings, ncol = 6))

scaleMatrix <- lapply(scaleMatrix, function(x) 
  ifelse(x >= 0.4, 1, ifelse(x <= -0.4, -1, NA)))
scaleMatrix <- data.frame(data = scaleMatrix,
                          row.names = colnames(mainData[, c(2:56)]))
colnames(scaleMatrix) <- c("Sensory", 
                           "CognitiveDemand",
                           "ThreatToSelf", 
                           "CrossSettings", 
                           "Safety", 
                           "States")

#create dataframe of non-refined factor (scale) scores
scaleScores <- data.frame(apply(scaleMatrix, 2, 
                        function(x1) apply(mainData[, c(2:56)], 1, 
                        function(x2) mean(mapply(function(x, y) 
                          ifelse(y == -1, -x, ifelse(y, x, NA)), x2, x1), na.rm = T))))


#kmeans
scale.KClusterFit <- kmeans(scaleScores, 
                                centers = 3, 
                                nstart = 25)

#create dataframe with factor scores and cluster membership
scale.KClusterScores <- cbind(scaleScores, scale.KClusterFit$cluster)
colnames(scale.KClusterScores)[7] <- "KCluster"

#recode KCluster to keep ordering consistent - note that kmeans() will randomly 
#assign groups, so ordering has to be manually fixed
scale.KClusterScores$KCluster <- dplyr::recode(scale.KClusterScores$KCluster, 
                                                   '1' = 1, 
                                                   '2' = 3, 
                                                   '3' = 2)

scale.KClusterMean <- scale.KClusterScores %>%
  group_by(KCluster) %>%
  summarise(across(Sensory:States,list(mean = mean, 
                                       sd = sd)), 
            n = n()) %>%
  pivot_longer(c(-KCluster,
                 -n),
               names_to = c("Factor", 
                            ".value"),
               names_sep = "_")

scale.KClusterMean$KCluster <- factor(scale.KClusterMean$KCluster)

scale.KClusterMean$Factor <- factor(scale.KClusterMean$Factor,
                                         levels = unique(scale.KClusterMean$Factor))

#MANOVA
summary(manova(cbind(Sensory, 
               CognitiveDemand, 
               ThreatToSelf, 
               CrossSettings, 
               Safety, 
               States) ~ KCluster, 
         data = scale.KClusterScores))

#omega-squared for MANOVA
omega.F(6, 261, 54.991, 268, 0.05)

##univariate Welch's ANOVA test with Bonferroni correction
table3 <- scale.KClusterScores %>%
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
table3$omegasq <- apply(table3, 1, 
                        function(x) omega.F(as.numeric(x[5]), 
                                            as.numeric(x[6]), 
                                            as.numeric(x[4]), 
                                            as.numeric(x[3]), 
                                            0.05/6)$omega)
table3$omegalow <- apply(table3, 1, 
                         function(x) omega.F(as.numeric(x[5]), 
                                             as.numeric(x[6]), 
                                             as.numeric(x[4]), 
                                             as.numeric(x[3]), 
                                             0.05/6)$omegalow)
table3$omegahigh <- apply(table3, 1, 
                          function(x) omega.F(as.numeric(x[5]), 
                                              as.numeric(x[6]), 
                                              as.numeric(x[4]), 
                                              as.numeric(x[3]), 
                                              0.05/6)$omegahigh)

#post-hoc Games-Howell, p-values adjusted with Tukey method
table4 <- scale.KClusterScores%>%
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


#compare kmeans cluster membership between refined (factor) and non-refined (scale) scores
FactorvsScale.Cluster <- data.frame(Factor = KClusterScores$KCluster, 
                                    Scale = scale.KClusterScores$KCluster)

FactorvsScale.Cluster <- cbind(FactorvsScale.Cluster,
                                     ifelse(FactorvsScale.Cluster[1] == FactorvsScale.Cluster[2], 1, 0))
colnames(FactorvsScale.Cluster)[3] <- 'Agreement'

FactorvsScale.ClusterAgreement <- FactorvsScale.Cluster[, c(2:3)] %>%
  group_by(Scale) %>%
  summarise(total = n(), n_agree = sum(Agreement)) %>%
  rename(Cluster = Scale)

#Cohen's Kappa
cohen.kappa(FactorvsScale.Cluster[, c(1:2)])
