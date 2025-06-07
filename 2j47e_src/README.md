# README

Latest version updated on 2021-10-28.

Data and code to accompany the article entitled, Establishing the transdiagnostic contextual pathways of emotional outbursts, 2021, by Justin Cheuk Yin Chung, Carmel Mevorach, and Kate Anne Woodcock.A pre-print, pre-peer-reviewed version of the manuscript can be found at https://doi.org/10.31234/osf.io/rje8p.

Correspondence about the article or this material should be addressed to Justin Chung (justincychung@googlemail.com) or Kate Woodcock (papers@katewoodcock.com), School of Psychology, University of Birmingham, Birmingham, UK, B15 2SA.

#### License

This work is licensed under the Creative Commons Attribution 4.0 International License. To view a copy of this license, visit http://creativecommons.org/licenses/by/4.0/ or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

## Data

The final datasets after exclusion are found in datasets.RData, which can be loaded through R. 

This file contains three dataframes:

1. mainData -  data from 268 participants regarding the contextual items of the Emotional Outburst Questionnaire, demographic information, and the domain and total scores of the Social Communication Questionnaire.
2. interraterData - contextual items of responses from 10 participants and their corresponding secondary raters.
3. retestData - contextual items of responses from 48 participants and their corresponding time 2 responses.

## Code

The R code for analysing and visualising the data are split into six files (please see comments within for more details):

1. Factor analysis - conducts exploratory factor analysis of the contextual items.
2. Cluster analysis refined factor scores - conducts cluster analysis of the contextual factors using refined factor scores.
3. Cluster analysis non-refined factor scores - conducts cluster analysis of the contextual factors using non-refined factor scores.
4. Demographics - generates summary statistics of demographic information for the whole sample and for each cluster.
5. Reliability - calculates interrater and test-retest reliability statistics.
6. Figures - generates figures found within the article.
