library(psych)

#Diagnostic tests to determine appropriate number of factors in EFA - parallel
#analysis (includes scree plot) and Very Simple Structure test - optimal number of factors is 6
fa.parallel(mainData[, c(2:56)], 
            fm = 'ml', 
            fa = 'fa')

vss(mainData[, c(2:56)], 
    n = 8, 
    rotate = 'oblimin', 
    fm = 'mle')

#Exploratory factor analysis with 6 factors
EFA <- fa(mainData[, c(2:56)],
          nfactors = 6,
          rotate = "oblimin",
          fm="ml")

#eigenvalues of the 6 factors are all above 1
EFA$values

#appending EFA factor scores to dataframe
EFA.mainData <- cbind(mainData, EFA$scores)

colnames(EFA.mainData)[111:116] <- c("Sensory", 
                                     "CognitiveDemand", 
                                     "ThreatToSelf", 
                                     "CrossSettings", 
                                     "Safety", 
                                     "States")

#Calculating internal consistency for items with loadings >= 0.40
#Sensory
alpha(EFA.mainData[, c("Trigger_SepFromCarer",
                       "Trigger_NotUnderstandGoingOn",
                       "Trigger_Light",                   
                       "Trigger_Sound",                   
                       "Trigger_Temperature",             
                       "Trigger_Smell",                  
                       "Trigger_Touch",                   
                       "Trigger_OtherSensoryFreq")])

#Cognitive Demand
alpha(EFA.mainData[, c("Trigger_OwnRoutine",
                       "Trigger_OtherRoutine",
                       "Trigger_Expectation",
                       "Trigger_Fixation",
                       "Trigger_DemandNotMet",
                       "Trigger_Waiting",
                       "Trigger_DemandPlaced",
                       "Trigger_BoringTask")])

#Threat to Self
alpha(EFA.mainData[, c("Trigger_Disagreement",
                       "Trigger_Criticised",
                       "Trigger_Teased",
                       "Trigger_Unfair",
                       "Trigger_ConflictingInfo")])
  
  
#Cross-settings
alpha(EFA.mainData[, c("Setting_Unsafe",              
                       "Setting_Familiar",            
                       "Setting_Public",             
                       "Person_Unsafe",              
                       "Person_Familiar",              
                       "Person_Unfamiliar",              
                       "Person_Dislike")])
  
#Safety
alpha(EFA.mainData[, c("Setting_Safe",
                       "Setting_Private",
                       "Person_Safe",
                       "Person_Like")])        

#States
alpha(EFA.mainData[, c("State_Tired",                     
                       "State_HungryThirsty")])