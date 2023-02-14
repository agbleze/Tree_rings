############ ASSIGNMENT: STATISTICS AND DENDROECOLOGY  ##############
### Task
### COURSE:   
### Lecturer:  
### CODE AUTHOR:    
### Quantify the differences in growth resilience
### - between the three species (here, you can focus on the dominant trees)
### - between suppressed vs. dominant trees from the same species
### with respect to the 2003 drought event using dendroecological methods, e.g., using the
### resilience/tolerance indices sensu Lloret et al. 2011 (i.e., the framework we talked about in the 
###                                                       lecture). Write a short report about this dendroecological analysis, including motivation, 
### research question, methods, results, and interpretation.

##### Data sets
##### Tree-ring data were collected from a site in the Munich gravel plain in 2009. The sampling 
##### design differentiated between suppressed and dominant individuals for each species, which is 
##### also reflected in the file names associated to the data sets (in the exam folder on Moodle): - MCFP1_d: Norway spruce, dominant
##### - MCFP1_s: Norway spruce, suppressed
##### - MCFP2_d: Scots pine, dominant
##### - MCFP2_s: Scots pine, suppressed
##### - MCFP6_d: Pedunculate oak, dominant
##### - MCFP6_s: Pedunculate oak, suppressed
##### The stand is a planted forest; thus, all individuals of a species are of approximately the same 
##### age. All files contain core-level raw measurements of tree-ring width. More information about 
##### the site and corresponding soil and climatic situation can be found in Zang et al. 2012 (PDF                                                                                         on Moodle).


library(dplR)
library(treeclim)
library(pointRes)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(gginference)
library(patchwork)

# Methodology
# The dataset analyzed is core-level raw measurements of tree-ring width from
# species such as Norway spruce, Scots pine, and Pedunculate oak with each species
# sampled from dorminant and suppressed category. The pointer year for the analysis is
# define to be 2003. Lloret et al. 2011 concept ot resilience forms the basis for analyzing
# growth resilience. Growth resilience was defined to be the resilience component of the
# resilience indices by Lloret et al. 2011.
# To prepare the data for estimation of resilience, tree means of tree ring widths were
# computed based on grouping tree cores per each tree species. Further the data was detrended,
# to normalize the influence of other exgenuous factors on tree ring growth.

# In determining the method to employ for the analysis, a number of exploratory technique
# including both visualization and statistical test to determine whether underlying assumptions have
# have been met were undertaken to chose the method of analysis from parametric and non-parametric
# methods.
# This included histograms to visualize the distribution of the data and boxplot to
# depict variance among tree species. Shapiro wirk test was undertaken to determine if
# distribution of data point was from a normal distribution. Homogeneity among tree species was
# tested using Bartlet test which is well cut out for a normally distributed data. The result
# was verified using other methods such as the Levene test. For the various statistical test
# 95% confidence interval was define as the threshold to decide whether to reject or fail to
# reject the null hypothesis.

# With regards to quantifying and determining whether  differences in growth resilience
# between Norway spruce dorminant, Scots pine dorminant, and Pedunculate oak dorminant
# is statistically significant, Analysis of Varaince (ANOVA) method was used based on
# hypothesis framework define as follows;
# H0: There is no statistically significant difference in the mean resilience value among
# various dorminant tree species
# H1: There is statistically significant difference in the mean resilience value
# of various dorminant tree species

# For analysis of suppressed and dorminant various tree species, the hypothesis was
# defined per tree species comparing the mean tree resilience value for that tree
# species. This produced a two sample t-test and for Norway spruce as an example,
# the hypothesis is framed as follows;

# H0: There is no statistically significant difference in the mean resilience value between
# various Norway spruce dorminant and norway spruce suppressed

# H1: There is statistically significant difference in the mean resilience value between
# various Norway spruce dorminant and norway spruce suppressed

# The hypothesis for Pedunculate oak dominant and Pedunculate oak suppressed is stated as
# follows
# H0: There is no statistically significant difference in the mean resilience value between
# various Pedunculate oak dominant and Pedunculate oak suppressed

# H1: There is statistically significant difference in the mean resilience value between
# various Pedunculate oak dominant and Pedunculate oak suppressed


# The hypothesis for Scots pine dorminant and Scot pine suppressed is defined as follows
# H0: There is no statistically significant difference in the mean resilience value between
# Scots pine dorminant and Scot pine suppressed

# H1: There is statistically significant difference in the mean resilience value between
# Scots pine dorminant and Scot pine suppressed



# RESULTS
# The results for the research questions defined is preceeded by exploratory visualizations
# and results of asummption testing as follows

# Figure 1: Illustrates visualization of detrended tree ring width for various tree species

# Figure 2: Illustrates histogram of resilience values of various tree species depicting
# normal distribution

# Figure 3: Illustrates boxplot of resilience values of the various tree species

# Figure 4: Illustrates shapiro wilk tests

# Figure 5: shows the result of anova test for dorminant tree species


# Interpretation

# The sample size of the tree species is generally small.
# The density plots and histograms of all dorminant tree species shows a normally
# distributed data point. This is supported by the Shapiro Wilk statistical test where the null
# hypothesis fail to be rejected at 95% confidence interval for all tree species.
# In figure X, the p-value is 0.4114 hence we fail to reject null hypothesis
# that the distribution of Norway spruce dorminant is not statistically significantly
# different from a normal distribution at a 95% confidence level. In the case of
# Pedunculate Oak, the p-value of 0.8461 suggest that the null hypothesis of distribution
# not being statistically significantly different from normal distribution fails to
# be rejected at 95% confidence interval. In a similar manner, p-value of 0.2142 for
# Scots Pine resilience data points distributions led to not rejecting the hypothesis
# that it is a normal distribution. On the basis of these exploratory results, the
# assumption of normal distribution has been proven for a parametric test to be used for
# to testing variance.

# The result for homogeneity test using bartlet test produced a p-value of 0.481
# indicating variance is equal among all dorminant tree species hence
# the null hypothesis of homogeneity fails to be rejected. With the assumption of normality
# and homogeneity satisfied, parametric method was employed to quantify and test
# whether there is statistically significant difference in resilience level among dorminant
# trees.


# In response to the objective of quantifying the difference between dorminant trees,
# the anova result shows a p-value of 0.0685 hence we fail to reject the null hypothesis
# at 95% confidence interval. This means that mean resilience value is equal
# among norway spruce, pedunculate oak and scots pine dorminant trees

# Focusing on whether there is a significant difference between resilience
# level (mean) between dorminant and suppressed Scot Pine species,
# p-value of 0.367  translates that
# there is no difference in the mean value of the resilience level between
# Scots Pine dorminant and suppress trees. In a similar manner,
# there is no difference in the mean value of the resilience level between
# pedunculate oak dorminant and suppress trees based on a p-value of 0.457 (95% confidence interval)
# This interpretation equally holds true for Norway spruce where
# p-value is 0.235 hence
# there is no difference in the mean value of resilience between
# norway spruce dorminant and suppress trees


# CONCLUSION
# It is concluded that irrespective of the type of tree species and group from which it is
# sampled (dorminant and suppressed), resilience level does not differ.


#whether to adopt parametric or non-parametric method for the analysis,
# a number of exploratory technique

# In order to quantify the difference in growth resilence, core level 


# In this task, we use data from a study in which both dominant and suppressed trees were 
# sampled to determine the size-dependent drought response of three important central 
# European tree species: Picea abies (Norway spruce), Pinus sylvestris (Scots pine), and 
# Quercus robur (pedunculate oak).

# read files
picea_abies_d <- dplR::read.rwl("MCFP1_d.rwl")
picea_abies_s <- dplR::read.rwl("MCFP1_s.rwl")
pinus_sylvestris_d <- dplR::read.rwl("MCFP2_d.rwl")
pinus_sylvestris_s <- dplR::read.rwl("MCFP2_s.rwl")
quercus_robur_d <- dplR::read.rwl("MCFP6_d.rwl")
quercus_robur_s <- dplR::read.rwl("MCFP6_s.rwl")


######## Data preprocessing  ###########
## Data preprocessing for all raw tree ring measurement is undertaken here
## This involves creating tree means and detrending the data as follows
# get ids
picea_abies_dorminant_id <- dplR::read.ids(picea_abies_d, stc = c(5,2,1))

# compute mean of tree ring width for cores of each tree
picea_abies_dorminant_tm <- dplR::treeMean(picea_abies_d, picea_abies_dorminant_id, na.rm=T)

picea_abies_dorminant_tm_yrs <- as.numeric(rownames(picea_abies_dorminant_tm))

plot(as.numeric(rownames(picea_abies_dorminant_tm)), picea_abies_dorminant_tm$`4`, type = 'l',
     xlim = c(base::min(picea_abies_dorminant_tm_yrs), base::max(picea_abies_dorminant_tm_yrs)),
     ylab = "Ring width (mm)",
     main = 'Timeseries of Picea Abies (Dominant) Tree 4',
     xlab = 'Years',
     )

dplR::spag.plot(picea_abies_dorminant_tm)

## Detrend data
# Detrending is done to remove effects such as tree age that are peculiar to a tree


picea_abies_dorminant_tm_detrend <-  dplR::detrend(rwl = picea_abies_dorminant_tm, 
                                                   method = 'Spline', nyrs = 32
                                                   )

# plot detrended data picea abies dorminant

dplR::spag.plot(picea_abies_dorminant_tm_detrend)


#### Resilience indicies for Picea abies dorminant

resilience_indices_picea_abies_dorminant <- pointRes::res.comp(data=picea_abies_dorminant_tm_detrend)

resist_picea_abies_dorminant <- resilience_indices_picea_abies_dorminant$resist
recov_picea_abies_dorminant <- resilience_indices_picea_abies_dorminant$recov
resil_picea_abies_dorminant <- resilience_indices_picea_abies_dorminant$resil

yrs_resil_picea_abies_dorminant <- as.numeric(base::rownames(resil_picea_abies_dorminant))


## using 2003 as drought event year to create resilience df of dorminant Picea abies

(resil_picea_abies_dorminant2003_df <- resil_picea_abies_dorminant %>%
    as.data.frame() %>%
    filter(rownames(resil_picea_abies_dorminant) == "2003")%>%
    pivot_longer(cols = colnames(resil_picea_abies_dorminant), 
                 names_to = "tree_id", 
                 values_to = "resilience_level"
    ) %>%
    mutate(tree_species = "picea abies dorminant")
)

# create boxplot of picea abies dorminant 2003 resilience level  
ggplot(data=resil_picea_abies_dorminant2003_df, 
       mapping=aes(y=resilience_level)
       ) + geom_boxplot() + 
  ggtitle("resilience level among dorminant picea abies species for 2003 drought") + 
  ylab("Resilience level") + xlab("Picea abies dorminant")

resil_picea_abies_dorminant2003_df


######### suppressed picea abies data processing ##########
# get ids
picea_abies_suppress_id <- dplR::read.ids(picea_abies_s, stc = c(5,2,1))

# create tree means
picea_abies_suppress_tm <- dplR::treeMean(rwl=picea_abies_s, 
                                            ids=picea_abies_suppress_id, 
                                            na.rm=T
                                          )

yrs_picea_abies_suppress_tm <- as.numeric(rownames(picea_abies_suppress_tm))

# plot tree mean of suppressed picea abies -- tree 5
plot(as.numeric(rownames(picea_abies_suppress_tm)), picea_abies_suppress_tm$`5`, type = 'l',
     xlim = c(base::min(yrs_picea_abies_suppress_tm), base::max(yrs_picea_abies_suppress_tm)),
     ylab = "Mean ring width (mm)",
     main = paste('Suppressed Picea abies (tree 5)' ),
     xlab = 'Years',
)

# plot time series of all suppressed picea abies
dplR::spag.plot(picea_abies_suppress_tm)

## detrend picea suppressed tree mean data
picea_abies_suppress_tm_detrend <-  dplR::detrend(rwl = picea_abies_suppress_tm, 
                                                  method = 'Spline', nyrs = 32
                                                  )
# plot detrended suppressed picea abies data
dplR::spag.plot(picea_abies_suppress_tm_detrend)

#### compute Resilience components of picea abies
resilience_indices_picea_abies_suppress <- pointRes::res.comp(data=picea_abies_suppress_tm_detrend)

resist_picea_abies_suppress <- resilience_indices_picea_abies_suppress$resist
recov_picea_abies_suppress <- resilience_indices_picea_abies_suppress$recov
resil_picea_abies_suppress <- resilience_indices_picea_abies_suppress$resil

# create a dataframe and filter for only 2003 drought event year for
# resilience results of picea abies
(resil_picea_abies_suppress2003_df <- resil_picea_abies_suppress %>%
                                  as.data.frame() %>%
                                  filter(rownames(resil_picea_abies_suppress) == "2003")%>%
                                  pivot_longer(cols = colnames(resil_picea_abies_suppress), 
                                               names_to = "tree_id", 
                                               values_to = "resilience_level"
                                               ) %>%
                                  mutate(tree_species = "picea abies suppress")
)


# create boxplot of picea abies suppress 2003 resilience level  
ggplot(data=resil_picea_abies_suppress2003_df, 
       mapping=aes(y=resilience_level)
) + geom_boxplot() + 
  ggtitle("resilience level among suppress picea abies species for 2003 drought") + 
  ylab("Resilience level") + xlab("Picea abies suppress")


############  data processing of dominant pinus sylvestris  ################
# get ids
pinus_sylvestris_dominant_id <- dplR::read.ids(pinus_sylvestris_d, stc = c(5,2,1))

# create tree means of dorminant pinus sylvestris
pinus_sylvestris_dominant_tm <- dplR::treeMean(rwl=pinus_sylvestris_d, 
                                         ids=pinus_sylvestris_dominant_id, 
                                          na.rm=T
                                         )

yrs_pinus_sylvestris_dominant_tm <- as.numeric(rownames(pinus_sylvestris_dominant_tm))

plot(as.numeric(rownames(pinus_sylvestris_dominant_tm)), pinus_sylvestris_dominant_tm$`7`, type = 'l',
     xlim = c(base::min(yrs_pinus_sylvestris_dominant_tm), base::max(yrs_pinus_sylvestris_dominant_tm)),
     ylab = "Mean Tree Ring width (mm)",
     main = 'Timeseries of Tree 7',
     xlab = 'Years',
)

dplR::spag.plot(pinus_sylvestris_dominant_tm)

## Detrend data for tree means of pinus sylvestris dominant
pinus_sylvestris_dominant_tm_detrend <-  dplR::detrend(rwl = pinus_sylvestris_dominant_tm, 
                                                 method = 'Spline', nyrs = 32
                                                 )
## plot detrended data of dominant pinus sylvestris
dplR::spag.plot(pinus_sylvestris_dominant_tm_detrend)

## Compute Resilience of dorminant pinus sylvestris
resilience_indices_pinus_sylvestris_dominant <- pointRes::res.comp(data=pinus_sylvestris_dominant_tm_detrend)

resist_pinus_sylvestris_dominant <- resilience_indices_pinus_sylvestris_dominant$resist
recov_pinus_sylvestris_dominant <- resilience_indices_pinus_sylvestris_dominant$recov
resil_pinus_sylvestris_dominant <- resilience_indices_pinus_sylvestris_dominant$resil

# create a dataframe and filter for only 2003 drought event year for
# resilience results of dominant pinus sylvestris 
(resil_pinus_sylvestris_dominant2003_df <- resil_pinus_sylvestris_dominant %>%
    as.data.frame() %>%
    filter(rownames(resil_pinus_sylvestris_dominant) == "2003")%>%
    pivot_longer(cols = colnames(resil_pinus_sylvestris_dominant), 
                 names_to = "tree_id", 
                 values_to = "resilience_level"
    ) %>%
    mutate(tree_species = "pinus sylvestris dominant")
)

# create boxplot of pinus sylvestris dominant 2003 resilience level  
ggplot(data=resil_pinus_sylvestris_dominant2003_df, 
       mapping=aes(y=resilience_level)
) + geom_boxplot() + 
  ggtitle("resilience level among dorminant pinus sylvestris species for 2003 drought") + 
  ylab("Resilience level") + xlab("pinus sylvestris dominant")


######## Data preprocessing of pinus sylvestris suppressed #########
# get ids for pinus sylvestris suppressed
pinus_sylvestris_suppress_id <- dplR::read.ids(pinus_sylvestris_s, stc = c(5,2,1))

# create tree means of pinus sylvestris suppressed
pinus_sylvestris_suppress_tm <- dplR::treeMean(rwl=pinus_sylvestris_s, 
                                         ids=pinus_sylvestris_suppress_id, 
                                         na.rm=T
                                        )

yrs_pinus_sylvestris_suppress_tm <- as.numeric(rownames(pinus_sylvestris_suppress_tm))

# plot time series of pinus sylvestris tree id 8
plot(as.numeric(rownames(pinus_sylvestris_suppress_tm)), pinus_sylvestris_suppress_tm$`8`, type = 'l',
     xlim = c(base::min(yrs_pinus_sylvestris_suppress_tm), base::max(yrs_pinus_sylvestris_suppress_tm)),
     ylab = "Mean Tree Ring width (mm)",
     main = 'Timeseries of suppressed pinus sylvestris Tree 8',
     xlab = 'Years',
    )
# plot time series of all suppressed pinus sylvestris
dplR::spag.plot(pinus_sylvestris_suppress_tm)

## detrend suppressed pinus sylvestris tree mean data
pinus_sylvestris_suppress_tm_detrend <-  dplR::detrend(rwl = pinus_sylvestris_suppress_tm, 
                                                 method = 'Spline', nyrs = 32
                                                  )
# plot detrended data of suppressed pinus sylvestris
dplR::spag.plot(pinus_sylvestris_suppress_tm_detrend)

# compute Resilience of suppressed pinus sylvestris
resilience_indices_pinus_sylvestris_suppress <- pointRes::res.comp(data=pinus_sylvestris_suppress_tm_detrend)

resist_pinus_sylvestris_suppress <- resilience_indices_pinus_sylvestris_suppress$resist
recov_pinus_sylvestris_suppress <- resilience_indices_pinus_sylvestris_suppress$recov
resil_pinus_sylvestris_suppress <- resilience_indices_pinus_sylvestris_suppress$resil

# transform data into df using 2003 drought event year for suppress pinus sylvestris
(resil_pinus_sylvestris_suppress2003_df <- resil_pinus_sylvestris_suppress %>%
    as.data.frame() %>%
    filter(rownames(resil_pinus_sylvestris_suppress) == "2003")%>%
    pivot_longer(cols = colnames(resil_pinus_sylvestris_suppress), 
                 names_to = "tree_id", 
                 values_to = "resilience_level"
    ) %>%
    mutate(tree_species = "pinus sylvestris suppress")
)

# create boxplot of suppressed pinus sylvestris 2003 resilience level  
ggplot(data=resil_pinus_sylvestris_suppress2003_df, 
       mapping=aes(y=resilience_level)
) + geom_boxplot() + 
  ggtitle("resilience level among suppress pinus sylvestris species for 2003 drought") + 
  ylab("Resilience level") + xlab("pinus sylvestris suppress")


######### Data processing for dominant quercus_robur_d ########
# get ids of cores of dorminant quercus_robur species
quercus_robur_dominant_id <- dplR::read.ids(quercus_robur_d, stc = c(5,2,1))

# create tree means of dorminant quercus robur
quercus_robur_dominant_tm <- dplR::treeMean(rwl=quercus_robur_d, 
                                            ids=quercus_robur_dominant_id, 
                                            na.rm=T
                                            )

yrs_quercus_robur_dominant_tm <- as.numeric(rownames(quercus_robur_dominant_tm))

plot(as.numeric(rownames(quercus_robur_dominant_tm)), quercus_robur_dominant_tm$`10`, type = 'l',
     xlim = c(base::min(yrs_quercus_robur_dominant_tm), base::max(yrs_quercus_robur_dominant_tm)),
     ylab = "Mean Tree Ring width (mm)",
     main = 'Timeseries of dominant quercus robur species Tree 10',
     xlab = 'Years',
)

dplR::spag.plot(quercus_robur_dominant_tm)

############## detrend dominant quercus robur data ##############
quercus_robur_dominant_tm_detrend <-  dplR::detrend(rwl = quercus_robur_dominant_tm, 
                                                 method = 'Spline', nyrs = 32
                                                )

# plot detrended dominant quercus robur data
dplR::spag.plot(quercus_robur_dominant_tm_detrend)

# compute Resilience of dominant quercus robur 
resilience_indices_quercus_robur_dominant <- pointRes::res.comp(data=quercus_robur_dominant_tm_detrend)

resist_quercus_robur_dominant <- resilience_indices_quercus_robur_dominant$resist
recov_quercus_robur_dominant <- resilience_indices_quercus_robur_dominant$recov
resil_quercus_robur_dominant <- resilience_indices_quercus_robur_dominant$resil

# create a dataframe and filter for only 2003 drought event year for
# resilience results of dominant quercus robur 
(resil_quercus_robur_dominant2003_df <- resil_quercus_robur_dominant %>%
    as.data.frame() %>%
    filter(rownames(resil_quercus_robur_dominant) == "2003")%>%
    pivot_longer(cols = colnames(resil_quercus_robur_dominant), 
                 names_to = "tree_id", 
                 values_to = "resilience_level"
    ) %>%
    mutate(tree_species = "quercus robur dominant")
)

# create boxplot of quercus robur dominant 2003 resilience level  
ggplot(data=resil_quercus_robur_dominant2003_df, 
       mapping=aes(y=resilience_level)
) + geom_boxplot() + 
  ggtitle("resilience level among dominant quercus robur species for 2003 drought") + 
  ylab("Resilience level") + xlab("quercus robur dominant")


######### Data processing for suppressed quercus robur #############
# get ids for cores of suppressed quercus robur
quercus_robur_suppress_id <- dplR::read.ids(quercus_robur_s, stc = c(5,2,1))

# create tree means of suppressed quercus_robur
quercus_robur_suppress_tm <- dplR::treeMean(rwl=quercus_robur_s, 
                                              ids=quercus_robur_suppress_id, 
                                              na.rm=T
                                              )

yrs_quercus_robur_suppress_tm <- as.numeric(rownames(quercus_robur_suppress_tm))

plot(as.numeric(rownames(quercus_robur_suppress_tm)), quercus_robur_suppress_tm$`14`, 
     type = 'l',
     xlim = c(base::min(yrs_quercus_robur_suppress_tm), 
              base::max(yrs_quercus_robur_suppress_tm)
              ),
     ylab = "Mean Tree Ring width (mm)",
     main = 'Timeseries of quercus robur (Suppressed) Tree 14',
     xlab = 'Years',
)

dplR::spag.plot(quercus_robur_suppress_tm)


####### Detrend mean tree ring width data of suppressed quercus robur  #########
quercus_robur_suppress_tm_detrend <-  dplR::detrend(rwl = quercus_robur_suppress_tm, 
                                                      method = 'Spline', nyrs = 32
                                                      )

# plot detrended data of suppressed quercus robur
dplR::spag.plot(quercus_robur_suppress_tm_detrend)

#### Calculate Resilience of suppressed quercus robur
resilience_indices_quercus_robur_suppress <- pointRes::res.comp(data=quercus_robur_suppress_tm_detrend)

resist_quercus_robur_suppress <- resilience_indices_quercus_robur_suppress$resist
recov_quercus_robur_suppress <- resilience_indices_quercus_robur_suppress$recov
resil_quercus_robur_suppress <- resilience_indices_quercus_robur_suppress$resil

# create a dataframe and filter for only 2003 drought event year for
# resilience results of suppressed quercus robur 
(resil_quercus_robur_suppress2003_df <- resil_quercus_robur_suppress %>%
    as.data.frame() %>%
    filter(rownames(resil_quercus_robur_suppress) == "2003")%>%
    pivot_longer(cols = colnames(resil_quercus_robur_suppress), 
                 names_to = "tree_id", 
                 values_to = "resilience_level"
    ) %>%
    mutate(tree_species = "quercus robur suppressed")
)

# create boxplot of quercus robur suppressed 2003 resilience level  
ggplot(data=resil_quercus_robur_suppress2003_df, 
       mapping=aes(y=resilience_level)
) + geom_boxplot() + 
  ggtitle("resilience level among suppressed quercus robur species for 2003 drought") + 
  ylab("Resilience level") + xlab("quercus robur suppress")


########### Quantify the differences in growth resilience ###############
### - between the three species (here, you can focus on the dominant trees)  ###

### test to determine whether to use parametric or non-parametric

## Normality test for dominant picea abies resilience
# p-value = 0.4114 for dominant picea abies resilience hence we fail to reject null hypothesis
# that the distribution is not statistically significantly
# different from a normal distribution at a 95% confidence level
shapiro_picea_abies_drom <- stats::shapiro.test(resil_picea_abies_dorminant2003_df$resilience_level) 

shapiro_picea_abies_drom

# p-value = 0.8461 for quercus_robur_dominant
stats::shapiro.test(resil_quercus_robur_dominant2003_df$resilience_level)

# p-value = 0.2142 for dorminant pinus sylvestris shapiro test
stats::shapiro.test(resil_pinus_sylvestris_dominant2003_df$resilience_level)

# Use Q-Q plot to visualize if data plausibly came from normality

(picea_abies_dorm_qqplot <- ggpubr::ggqqplot(data = resil_picea_abies_dorminant2003_df,
                             x = "resilience_level",#, ggtheme = theme_dark()
                             title = "Normal Q-Q plot of resilience of dominant Picea Abies",
                             color="red",
                             ggtheme = theme_get()
                             
                             ))


(pinus_syl_dorm_qqplot <- ggpubr::ggqqplot(data = resil_pinus_sylvestris_dominant2003_df,
                           x = "resilience_level",#, ggtheme = theme_dark()
                           title = "Normal Q-Q plot of resilience of dominant pinus sylvestris",
                           color="blue",
                           ggtheme = theme_get()
                           ))


picea_abies_dorm_qqplot + pinus_syl_dorm_qqplot

(resil_dorm <- rbind(resil_picea_abies_dorminant2003_df,
                    resil_pinus_sylvestris_dominant2003_df,
                    resil_quercus_robur_dominant2003_df
                    )
)

ggpubr::ggqqplot(data = resil_dorm, facet.by = "tree_species",
                 x = "resilience_level",#, ggtheme = theme_dark()
                 #title = "Normal Q-Q plot of resilience of dominant pinus sylvestris",
                 #color="blue",
                 ggtheme = theme_get()
)

##### testing homogeneity  #########
# visualize with boxplot 
graphics::boxplot(resil_norway_spruce_dorminant_2003, 
                  resil_pedunculate_oak_dominant_2003,
                  resil_scots_pine_dominant_2003#,
                  #xlab=c("Norway spruce", "Pedunculate oak", "Scots Pine")
                  #labels=c("Norway spruce", "Pendunculate", "Scots Pine")
)

as.data.frame(resil_norway_spruce_dorminant) %>%
  dplyr::mutate(tree_name = "norway spruce") 


norway_spruce_dorminant_2003 <- as.data.frame(resil_norway_spruce_dorminant_2003) %>%
                                  mutate(tree_name = "norway spruce") %>%
                                  rownames_to_column(var="tree")%>%
                                  rename("tree_ring_width" = resil_norway_spruce_dorminant_2003)
  


pedunculate_oak_dorminant_2003 <-  as.data.frame(resil_pedunculate_oak_dominant_2003) %>%
                                    mutate(tree_name = "pedunculate oak") %>%
                                    rownames_to_column(var="tree")%>%
                                    rename("tree_ring_width" = resil_pedunculate_oak_dominant_2003)


scots_pine_dominant_2003 <-  as.data.frame(resil_scots_pine_dominant_2003) %>%
                                mutate(tree_name = "Scots Pine") %>%
                                rownames_to_column(var="tree")%>%
                                rename("tree_ring_width" = resil_scots_pine_dominant_2003)

all_dorminant_species_2003_df <-  rbind(norway_spruce_dorminant_2003, 
                                        pedunculate_oak_dorminant_2003,
                                        scots_pine_dominant_2003
                                        )

all_dorminant_species_2003_df

# p-value = 0.481 shows that variance is equal among all dorminant tree species hence 
# the null hypothesis of homogeneity fails to be rejected. With the assumption of normality 
# and homogeneity satisfied, parametric method was employed to quantify and test 
# whether there is statistically significant difference in resilience level among dorminant 
# trees.



bartlett.test(x=all_dorminant_species_2003_df$tree_ring_width,
              g=all_dorminant_species_2003_df$tree_name, 
              data=all_dorminant_species_2003_df
              )


library(car)

car::leveneTest(y=all_dorminant_species_2003_df$tree_ring_width,
                group=all_dorminant_species_2003_df$tree_name, 
                center=mean
                )


# p-value = 0.2959 indicate the null hypothesis of homogeneity fails to be rejected
fligner.test(x=all_dorminant_species_2003_df$tree_ring_width,
             g=all_dorminant_species_2003_df$tree_name, 
             data=all_dorminant_species_2003_df
             )



### Use parametric method
# F value is 2.966 and p-value = 0.0685 which is greater than 0.05 sig. level
# hence we fail to reject the null hypothesis that mean resilience value is equal 
# among norway spruce, pedunculate oak and scots pine dorminant trees

res_aov <- aov(all_dorminant_species_2003_df$tree_ring_width ~ all_dorminant_species_2003_df$tree_name,
    data = all_dorminant_species_2003_df)

summary(res_aov)

ggbetweenstats(data=all_dorminant_species_2003_df, y= tree_ring_width,
               x=tree_name)

ggbetweenstats(all_dorminant_species_2003_df, x = tree_name, 
               y = tree_ring_width, 
               plot.type = 'boxviolin',
               var.equal = TRUE,
               type = 'parametric', p.adjust.method = 'bonferroni')

#############  Quantify the differences in growth resilience  #################
######## - between suppressed vs. dominant trees from the same species  ###########

# norway spruce is choosen for the analysis 
# diff b/t norway spruce suppressed and dominant

graphics::boxplot(resil_pedunculate_oak_suppress_2003)

resil_norway_spruce_dorminant_2003
resil_norway_spruce_suppress_2003

boxplot(resil_norway_spruce_suppress_2003)

# visualizing to determine normality of norway_spruce_suppress_2003
plot_qqplot(data=resil_norway_spruce_suppress_2003, 
            title = "Q-Q plot of norway spruce (2003 suppress)"
            )
hist(resil_norway_spruce_suppress_2003)

## shapiro test for normality on norway_spruce_suppress_2003
# p-value = 0.1356. With p-value greater than 0.05, we fail to reject the 
# null hypothesis that the data is normally distributed at 0.05 sig. level
shapiro.test(resil_norway_spruce_suppress_2003)


resil_norway_spruce_suppress_dorminant_2003_df <-  (as.data.frame(resil_norway_spruce_suppress_2003) %>%
                                                  rownames_to_column(var='tree')%>%
                                                  rename("tree_ring_width" 
                                                         = resil_norway_spruce_suppress_2003) %>%
                                                  mutate(tree_name = "norway spruce suppress")
                                                  )


resil_norway_spruce_dorminant_2003_df <-  norway_spruce_dorminant_2003 %>%
                                   mutate(tree_name = "norway spruce dorminant")




resil_ns_dorm_supp2003_df <- rbind(resil_norway_spruce_dorminant_2003_df,
                                  resil_norway_spruce_suppress_dorminant_2003_df
                                  )


# homogeneity test
# p-value = 0.2208, we fail to reject the null hypothesis that variance between
# tree groups is homogeneous
bartlett.test(x=resil_ns_dorm_supp2003_df$tree_ring_width, 
              g=resil_ns_dorm_supp2003_df$tree_name,
              data = resil_ns_dorm_supp2003_df
              )


# Levene test shows p-value = 0.5048 hence we fail to reject the null hypothesis
# at 95% sig level that variance between tree groups is homogeneous
leveneTest(y=resil_ns_dorm_supp2003_df$tree_ring_width,
           g=resil_ns_dorm_supp2003_df$tree_name,
           data = resil_ns_dorm_supp2003_df)


# p-value = 0.7822
fligner.test(x=resil_ns_dorm_supp2003_df$tree_ring_width,
             g=resil_ns_dorm_supp2003_df$tree_name)
              

# is there statistically significant difference b/t the resilience level 
# of norway spruce dorminant and suppress trees

anova_ns_dorm_supp <- aov(tree_ring_width ~ tree_name,
                          data = resil_ns_dorm_supp2003_df)

# p-value = 0.235 with F statistics values of 1.511 which translates that
# there is no difference in the mean value of the resilience level between
# norway spruce dorminant and suppress trees
summary(anova_ns_dorm_supp)



############# OR DO THE ANALYSIS FOR PEDUNCULATE OAK ####################

# pedunculate oak is choosen for the analysis 
# diff b/t norway spruce suppressed and dominant

resil_pedunculate_oak_dominant_2003
resil_pedunculate_oak_suppress_2003


boxplot(resil_pedunculate_oak_dominant_2003)
boxplot(resil_pedunculate_oak_suppress_2003)

# visualizing to determine normality of pedunculate_oak_dominant_2003
plot_qqplot(data=resil_pedunculate_oak_dominant_2003, 
            title = "Q-Q plot of Pedunculate oak dominant - 2003"
)
hist(resil_pedunculate_oak_dominant_2003)

# visualizing to determine normality of pedunculate_oak_suppress_2003
plot_qqplot(data=resil_pedunculate_oak_suppress_2003,
            title = "Q-Q plot of Pedunculate oak suppress - 2003"
            )


## shapiro test for normality on resil_pedunculate_oak_dorminant_2003
# p-value = 0.8461. With p-value greater than 0.05, we fail to reject the 
# null hypothesis that the data is normally distributed at 0.05 sig. level
shapiro.test(resil_pedunculate_oak_dominant_2003)

## shapiro test for normality on resil_pedunculate_oak_suppressed_2003
# p-value = 0.7062. With p-value greater than 0.05, we fail to reject the 
# null hypothesis that the data is normally distributed at 0.05 sig. level
shapiro.test(resil_pedunculate_oak_suppress_2003)




resil_pedunculate_oak_dorm_2003_df <-  (as.data.frame(resil_pedunculate_oak_dominant_2003) %>%
                                                      rownames_to_column(var='tree')%>%
                                                      rename("resil_value" 
                                                             = resil_pedunculate_oak_dominant_2003) %>%
                                                      mutate(tree_name = "pedunculate oak dominant")
                                        )


resil_pedunculate_oak_supp_2003_df <-  (as.data.frame(resil_pedunculate_oak_suppress_2003) %>%
                                          rownames_to_column(var='tree')%>%
                                          rename("resil_value" 
                                                 = resil_pedunculate_oak_suppress_2003) %>%
                                          mutate(tree_name = "pedunculate oak suppress")
                                        )

resil_po_dorm_supp2003_df <- rbind(resil_pedunculate_oak_dorm_2003_df,
                                   resil_pedunculate_oak_supp_2003_df
                                  )


# homogeneity test
# p-value = 0.551, we fail to reject the null hypothesis that variance between
# tree groups is homogeneous
bartlett.test(x=resil_po_dorm_supp2003_df$resil_value, 
              g=resil_po_dorm_supp2003_df$tree_name,
              data = resil_po_dorm_supp2003_df
            )


# Levene test shows p-value = 0.7076 hence we fail to reject the null hypothesis
# at 95% sig level that variance between tree groups is homogeneous
leveneTest(y=resil_po_dorm_supp2003_df$resil_value,
           g=resil_po_dorm_supp2003_df$tree_name,
           data = resil_po_dorm_supp2003_df)


# p-value = 0.6052
fligner.test(x=resil_po_dorm_supp2003_df$resil_value,
             g=resil_po_dorm_supp2003_df$tree_name)


# is there statistically significant difference b/t the resilience level 
# of pedunculate oak dorminant and suppress trees

anova_po_dorm_supp <- aov(resil_value ~ tree_name,
                          data = resil_po_dorm_supp2003_df)

# p-value = 0.457 with F statistics values of 0.579 which translates that
# there is no difference in the mean value of the resilience level between
# pedunculate oak dorminant and suppress trees
summary(anova_po_dorm_supp)







############# OR DO THE ANALYSIS FOR SCOTS PINE ####################

# Scots Pine is choosen for the analysis 
# diff b/t scots pine suppressed and dominant

resil_scots_pine_dominant_2003
resil_scots_pine_suppress_2003

scots_pine_dominant_2003

boxplot(resil_scots_pine_suppress_2003)

# visualizing to determine normality of scots_pine_suppress_2003
plot_qqplot(data=resil_scots_pine_suppress_2003,
            title = "Q-Q plot of Scots Pine suppress - 2003"
)


## shapiro test for normality on resil_scots_pine_suppressed_2003
# p-value = 0.5237. With p-value greater than 0.05, we fail to reject the 
# null hypothesis that the data is normally distributed at 0.05 sig. level
shapiro.test(resil_scots_pine_suppress_2003)


resil_scots_pine_supp_2003_df <-  (as.data.frame(resil_scots_pine_suppress_2003) %>%
                                          rownames_to_column(var='tree')%>%
                                          rename("resil_value" 
                                                 = resil_scots_pine_suppress_2003) %>%
                                          mutate(tree_name = "Scots Pine suppress")
                                    )

resil_scots_pine_dom_2003_df <- scots_pine_dominant_2003 %>%
                                rename("resil_value" = tree_ring_width) %>%
                                mutate(tree_name = "Scots Pine dorminant")

resil_scots_pine_dom_2003_df

resil_sp_dorm_supp2003_df <- rbind(resil_scots_pine_dom_2003_df,
                                   resil_scots_pine_supp_2003_df
                                  )

resil_sp_dorm_supp2003_df

# homogeneity test
# p-value = 0.551, we fail to reject the null hypothesis that variance between
# tree groups is homogeneous
bartlett.test(x=resil_sp_dorm_supp2003_df$resil_value, 
              g=resil_sp_dorm_supp2003_df$tree_name
)


# Levene test shows p-value = 0.07468 hence we fail to reject the null hypothesis
# at 95% sig level that variance between tree groups is homogeneous
leveneTest(y=resil_sp_dorm_supp2003_df$resil_value,
           g=resil_sp_dorm_supp2003_df$tree_name,
           data = resil_sp_dorm_supp2003_df)


# p-value = 0.05853
fligner.test(x=resil_sp_dorm_supp2003_df$resil_value,
             g=resil_sp_dorm_supp2003_df$tree_name)


# is there statistically significant difference b/t the resilience level 
# of Scots Pine dorminant and suppress trees

anova_sp_dorm_supp <- aov(resil_value ~ tree_name,
                          data = resil_sp_dorm_supp2003_df
                          )

# p-value = 0.367 with F statistics values of 0.858 which translates that
# there is no difference in the mean value of the resilience level between
# Scots Pine dorminant and suppress trees
summary(anova_sp_dorm_supp)


library(ggstatsplot)

ggbetweenstats(resil_sp_dorm_supp2003_df, x = tree_name, 
               y = resil_value, 
               plot.type = 'boxviolin',
               var.equal = TRUE,
               type = 'parametric', p.adjust.method = 'bonferroni')



