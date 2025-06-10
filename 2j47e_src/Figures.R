#requires variables from Cluster analysis refined factor scores.R
#and Cluster analysis non-refined factor scores.R
library(tidyr)
library(ggplot2)
library(ggsignif)

#box and whisker plot for refined factor scores
#convert dataframe of factor scores and cluster membership to long form
KClusterScores.long <- KClusterScores %>%
  pivot_longer(c(Sensory, 
                 CognitiveDemand, 
                 ThreatToSelf, 
                 CrossSettings, 
                 Safety, 
                 States),
               names_to = c("Factor"))


KClusterScores.long$Factor <- factor(KClusterScores.long$Factor, 
                                         levels = c("Sensory", 
                                                    "CognitiveDemand", 
                                                    "ThreatToSelf", 
                                                    "CrossSettings", 
                                                    "Safety", 
                                                    "States"))

#Figure 2
ggplot(KClusterScores.long, 
       aes(x = Factor, 
           y = value, 
           fill = as.factor(KCluster))) +
  geom_boxplot(width = 0.4, 
               position=position_dodge(0.66)) +
  stat_summary(aes(group = as.factor(KCluster)), 
               fun = mean, 
               geom="point", 
               size= 2, 
               shape = 15, 
               position = position_dodge(0.66))  + 
  geom_signif(inherit.aes = FALSE,
              stat="identity",
              data=data.frame(x = c(2, 3.78, 3.78, 4, 5.78, 5.78, 6), 
                              xend = c(2.22, 4.22, 4, 4.22, 6.22, 6, 6.22),
                              y = c(3, 2.7, 3, 3.2, 2.7, 3, 3.2), 
                              annotation =c ("ns", "ns", "***", "***", "ns", "***", "*")), 
              group = c(1:7),
              aes(x = x,
                  xend = xend, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 6)) +
  scale_fill_manual(values=c("#ffffff", "#e0e0e0", "#919191"), 
                    labels = c("Sensory Sensitivity (107)", 
                               "Perceived Safety (98)", 
                               "Perceived Unsafety (63)")) + 
  labs(x = "Factor", 
       y = "Factor score",
       fill = "Cluster (n)") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'),
        text = element_text(size = 16),
        legend.position="bottom")


#box and whisker plot for non-refined factor (scale) scores
#convert dataframe of factor scores and cluster membership to long form
scale.KClusterScores.long <- scale.KClusterScores %>%
  pivot_longer(c(Sensory, 
                 CognitiveDemand, 
                 ThreatToSelf, 
                 CrossSettings, 
                 Safety, 
                 States),
               names_to = c("Factor"))

scale.KClusterScores.long$Factor <- factor(scale.KClusterScores.long$Factor, 
                                                levels = c("Sensory", 
                                                           "CognitiveDemand", 
                                                           "ThreatToSelf", 
                                                           "CrossSettings", 
                                                           "Safety", 
                                                           "States"))

#Figure 3
ggplot(scale.KClusterScores.long, 
       aes(x = Factor, 
           y = value, 
           fill = as.factor(KCluster))) +
  geom_boxplot(width = 0.4, 
               position=position_dodge(0.66)) +
  stat_summary(aes(group = as.factor(KCluster)), 
               fun = mean, 
               geom="point", 
               size= 2, 
               shape = 15, 
               position = position_dodge(0.66))  + 
  geom_signif(inherit.aes = FALSE,
              stat="identity",
              data=data.frame(x = c(1, 2,  5.78), 
                              xend = c(1.22, 2.22, 6),
                              y = c(1.05, 1.05, 1.05), 
                              annotation =c ("ns", "*", "ns")), 
              group = c(1:3),
              aes(x = x,
                  xend = xend, 
                  y = y, 
                  yend = y, 
                  annotation = annotation, 
                  group = group,
                  textsize = 6)) +
  scale_fill_manual(values=c("#ffffff", "#e0e0e0", "#919191"), 
                    labels = c("Sensory Sensitivity (88)", 
                               "Perceived Safety (106)", 
                               "Perceived Unsafety (74)")) +
  labs(x = "Factor", 
       y = "Factor score",
       fill = "Cluster (n)") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black'), 
        text = element_text(size = 16),
        legend.position="bottom")
