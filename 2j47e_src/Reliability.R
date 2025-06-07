#variables of secondary raters or time 2 responses are prefixed with "R_"
library(psych)

#interrater reliability
#create dataframe of reliability statistic for each of the 10 pairs of data
interraterReliability <- data.frame(row.names = row.names(interraterData))
for (i in c(1:10)) {
  interraterReliability[i,1] <- cohen.kappa(cbind(t(interraterData[i ,c(1:55)]), 
                                                  t(interraterData[i ,c(56:110)])))$weighted.kappa
}

#mean interrater reliability
mean(interraterReliability$V1)
#lower confidence interval
mean(interraterReliability$V1) - qt(1 - 0.05/2, 10 - 1) * sd(interraterReliability$V1)/sqrt(10)
#upper confidence interval
mean(interraterReliability$V1) + qt(1 - 0.05/2, 10 - 1) * sd(interraterReliability$V1)/sqrt(10)


#test-retest reliability
#create dataframe of reliability statistic for each of the 48 pairs of data
retestReliability <- data.frame(row.names = row.names(retestData))
for (i in c(1:48)) {
  retestReliability[i,1] <- cohen.kappa(cbind(t(retestData[i ,c(1:55)]),
                                              t(retestData[i ,c(56:110)])))$weighted.kappa
}

#mean test-retest reliability
mean(retestReliability$V1)
#lower confidence interval
mean(retestReliability$V1) - qt(1 - 0.05/2, 48 - 1) * sd(retestReliability$V1)/sqrt(48)
#upper confidence interval
mean(retestReliability$V1) + qt(1 - 0.05/2, 48 - 1) * sd(retestReliability$V1)/sqrt(48)
