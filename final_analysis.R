if (!require("tidyverse",character.only = TRUE)){install.packages("tidyverse",dep=TRUE)}
if (!require("readxl",character.only = TRUE)){install.packages("readxl",dep=TRUE)}
if (!require("metafor",character.only = TRUE)){install.packages("metafor",dep=TRUE)}

# Read file #####
path <- "~/University of St Andrews/Silvia Paracchini - gen_lang_hand_meta/GenLang_hand_preference_meta_analysis/"
sheet_to_open_RD <- "MA_RD_MF_cases_new"
sheet_to_open_DLD <- "MA_DLD_MF_cases_new"

# Use MA_DLD_MF_cases for DLD and MA_RD_MF_cases for RD

All_studies_RD <- readxl::read_excel(paste0(path,'genlang_all_cohorts_updated.xlsx'), sheet = sheet_to_open_RD) %>%
  filter(cohort_name != "Multicenter Study Marburg/Würzburg cohort") %>% # This study did not pass inclusion criteria
  filter(cohort_name != "NTR cohort") %>% # This study did not pass inclusion criteria
  escalc(measure = "OR", ai = cases_NRH, bi = cases_RH, ci = controls_NRH, di = controls_RH, data = ., append = T, drop00 = T) %>%
  mutate(across(cohort_type:phenotype, as.factor)) %>% # Create factors for better analysis
  mutate(OR = exp(.$yi))

All_studies_DLD <- readxl::read_excel(paste0(path,'genlang_all_cohorts_updated.xlsx'), sheet = sheet_to_open_DLD) %>%
  filter(cohort_name != "Multicenter Study Marburg/Würzburg cohort") %>% # This study did not pass inclusion criteria
  filter(cohort_name != "NTR cohort") %>% # This study did not pass inclusion criteria
  escalc(measure = "OR", ai = cases_NRH, bi = cases_RH, ci = controls_NRH, di = controls_RH, data = ., append = T, drop00 = T) %>%
  mutate(across(cohort_type:phenotype, as.factor)) %>% # Create factors for better analysis
  mutate(OR = exp(.$yi))


study_names <- All_studies_DLD$cohort_name

# Basic model
model_DLD <- rma(yi = yi, vi = vi, measure = "OR", data = All_studies_DLD, test = 'knha')
model_RD <- rma(yi = yi, vi = vi, measure = "OR", data = All_studies_RD, test = 'knha')
summary.rma(model_DLD)
summary.rma(model_RD)
exp(model_RD$beta)[,1] # This is the overall OR
exp(model_RD$ci.lb) # This is the overall OR upper limit
exp(model_RD$ci.ub) # This is the overall OR lower limit
metafor::forest.rma(model_DLD, annotate = T, atransf = exp,
                    slab = study_names, showweights = T, header = F, 
                    order = "obs", cex = 0.7, ilab = cbind(as.character(All_studies_DLD$cohort_type), as.character(All_studies_DLD$phenotype), All_studies_DLD$cases_NRH, All_studies_DLD$cases_RH, All_studies_DLD$controls_NRH, All_studies_DLD$controls_RH),
                    ilab.xpos = c(-2.6, -1.4, 2, 2.5, 3, 3.5))
text(c(-2.6, -1.4), 13, c("Cohort type", "Phenotype"), cex = 0.7)
text(c(2, 2.5, 3, 3.5), 13, c("NRH", "RH", "NRH", "RH"), cex = 0.7)
text(c(2.25, 3.25), 14, c("Cases", "Controls"), cex = 0.7)
text(-5.5, 13, "Cohort name", pos = 4, cex = 0.7)
text(6, 13, "Odds Ratio [95% CI]", pos = 2, cex = 0.7)

metafor::forest.rma(model_RD, annotate = T, atransf = exp,
                    slab = study_names, showweights = T, header = F, 
                    order = "obs", cex = 0.7, ilab = cbind(as.character(All_studies_DLD$cohort_type), as.character(All_studies_DLD$phenotype), All_studies_DLD$cases_NRH, All_studies_DLD$cases_RH, All_studies_DLD$controls_NRH, All_studies_DLD$controls_RH),
                    ilab.xpos = c(-3.2, -1.6, 2.5, 3, 3.5, 4))
text(c(-3.2, -1.6), 13, c("Cohort type", "Phenotype"), cex = 0.7)
text(c(2.5, 3, 3.5, 4), 13, c("NRH", "RH", "NRH", "RH"), cex = 0.7)
text(c(2.75, 3.75), 14, c("Cases", "Controls"), cex = 0.7)
text(-6.3, 13, "Cohort name", pos = 4, cex = 0.7)
text(7, 13, "Odds Ratio [95% CI]", pos = 2, cex = 0.7)

metafor::regtest(x = model_DLD, model = "rma")
metafor::regtest(x = model_RD, model = "rma")

metafor::funnel(model_DLD)
metafor::funnel(model_RD)

# Model with cohort_type moderator 
model_DLD <- rma(yi = yi, vi = vi, mods = ~cohort_type, measure = "OR", data = All_studies_DLD %>% filter(cohort_name != "Manchester Language Study"), test = 'knha')
summary.rma(model_DLD)

model_RD <- rma.uni(yi = yi, vi = vi, mods = ~cohort_type, measure = "OR", data = All_studies_RD %>% filter(cohort_name != "Manchester Language Study"), test = 'knha')
summary.rma(model_RD)

metafor::regplot.rma(model_DLD, mod = "cohort_type", atransf=exp)
metafor::regplot.rma(model_RD, mod = "cohort_type", atransf=exp)

# Model with phenotype moderator
model_DLD <- rma(yi = yi, vi = vi, mods = phenotype, measure = "OR", data = All_studies_DLD, test = 'knha')
summary.rma(model_DLD)

model_RD <- rma(yi = yi, vi = vi, mods = phenotype, measure = "OR", data = All_studies_RD, test = 'knha')
summary.rma(model_RD)

metafor::regplot.rma(model_DLD, mod = "phenotype", atransf=exp)
metafor::regplot.rma(model_RD, mod = "phenotype", atransf=exp)

# Simulation analysis using synthetic controls
#Make table with marginal N and  percent RH for each row of All_studies
startcol <- which(colnames(All_studies_DLD)=='cases_NRH')

All_studies_DLD$totalN<-rowSums(All_studies_DLD[,startcol:(startcol+3)])
All_studies_DLD$casesN <-rowSums(All_studies_DLD[,startcol:(startcol+1)])
All_studies_DLD$controlsN <-rowSums(All_studies_DLD[,(startcol+2):(startcol+3)])
All_studies_DLD$pR <- (All_studies_DLD$cases_RH+All_studies_DLD$controls_RH)/All_studies_DLD$totalN
All_studies_DLD$pR[All_studies_DLD$pR==1]<-.99 #avoid values of 100% RH


nsim <- 1000
allp <- vector() #initialise vector to hold p-values from meta-analysis
for (i in 1:nsim){
  simstudy <- All_studies_DLD #clone original file
  #now overwrite with simulated values that assume null effect of handedness
  for (n in 1:nrow(All_studies_DLD)){
    simstudy$cases_RH[n] <- rbinom(1,simstudy$casesN[n],simstudy$pR[n]) #rbinom simulates data from binomial distribution, with giving N cases and given probability of R handedness
    simstudy$controls_RH[n] <- rbinom(1,simstudy$controlsN[n],simstudy$pR[n])
    simstudy$cases_NRH[n] <-simstudy$casesN[n]-simstudy$cases_RH[n]
    simstudy$controls_NRH[n] <-simstudy$controlsN[n]-simstudy$controls_RH[n]
  }
  
  #Dorothy can't cope with pipes, but this should be identical to Filippo's analysis of the real data - this time applied to simulated data
  mydat <- simstudy %>% escalc(measure = "OR", ai = cases_NRH, bi = cases_RH, ci = controls_NRH, di = controls_RH, data = ., append = T, drop00 = T) %>%
    filter(complete.cases(.$yi)) %>% # This is to remove values with empty OR as the cases/controls contain at least a cell with < 5
    mutate(OR = exp(.$yi)) # This is to get the OR
  
  model <- rma(yi = yi, vi = vi, measure = "OR", data = mydat, method = "ML")
  
  # print(summary(model))
  s<-summary.rma(model)
  allp[i]<-length(which(s$pval<.05)) #count how many significant p-values on this run
}

print('N runs with given number of significant p-values')
table(allp)
