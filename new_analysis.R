if (!require("tidyverse",character.only = TRUE)){install.packages("tidyverse",dep=TRUE)}
if (!require("readxl",character.only = TRUE)){install.packages("readxl",dep=TRUE)}
if (!require("metafor",character.only = TRUE)){install.packages("metafor",dep=TRUE)}

# Read file #####

# This is the analysis suggested by Dorothy which have only 1 m-a with moderators (sex, pheontype, cohort type)

path <- "~/University of St Andrews/Silvia Paracchini - gen_lang_hand_meta/GenLang_hand_preference_meta_analysis/"
sheet_to_open <- "sexmatch_strict_comorbid_DLD"

# Two sheets can be used:
# 1. sexmatch_strict_comorbid_DLD -> This replaces cells with < 5 counts with 0 and considers comorbid in DLD category
# 2. sexmatch_strict_comorbid_RD -> This replaces cells with < 5 counts with 0 and considers comorbid in RD catefory

All_studies <- readxl::read_excel(paste0(path,'genlang_all_cohorts_updated.xlsx'), sheet = sheet_to_open) %>%
  mutate(Full_cohort = paste(cohort_name, phenotype, sex, sep = "_")) %>%
  filter(cohort_name != "Multicenter Study Marburg/Würzburg cohort") %>% # This study did not pass inclusion criteria
  filter(cohort_name != "NTR cohort") %>% # This study did not pass inclusion criteria
  filter(cohort_name != "Toronto cohort") %>% # This study did not have enough samples males/females controls
  escalc(measure = "OR", ai = cases_NRH, bi = cases_RH, ci = controls_NRH, di = controls_RH, data = ., append = T, drop00 = T) %>%
  mutate(across(cohort_type:sex, as.factor)) %>% # Create factors for better analysis
  filter(complete.cases(.$yi)) %>% # This is to remove values with empty OR as the cases/controls contain at least a cell with < 5
  mutate(OR = exp(.$yi)) # This is to get the OR

study_names <- All_studies$Full_cohort

# Basic model
model <- rma(yi = yi, vi = vi, measure = "OR", data = All_studies, method = "ML")
summary.rma(model)
exp(model$beta)[,1] # This is the overall OR
exp(model$ci.lb) # This is the overall OR upper limit
exp(model$ci.ub) # This is the overall OR lower limit
metafor::forest.rma(model, annotate = T, atransf = exp,
                    slab = study_names, showweights = T, header = T, order = "obs")
metafor::funnel.rma(model)
metafor::regtest(model) # No plot asymmtry was highlighted


# Model with moderator
# Select one of the two models below

# With -1  we remove the reference to the intercept to better interpret the results
# As we compare 8 tests, the Bonferroni p is 0.05/8 = 0.00625
model <- rma(yi = yi, vi = vi, mods = ~ cohort_type:phenotype:sex -1, measure = "OR", data = All_studies, method = "ML")
summary.rma(model)

# Survive multiple corrections
# cohort_typeClinical:phenotypeLanguage:sexMale -> p = 0.0048
# cohort_typeClinical:phenotypeReading:sexFemale -> p = 0.0008

# This is to interpret the overall effect of moderators
model <- rma(yi = yi, vi = vi, mods = ~ cohort_type + phenotype + sex, measure = "OR", data = All_studies, method = "ML")
summary.rma(model)

metafor::forest.rma(model, annotate = T, atransf = exp,
                    slab = study_names, showweights = T, header = T, order = "obs") 
                    # ilab.xpos = c(+10, +8, +6, +4), cex = .75, addcred = T, 
                    # ilab = cbind(All_studies$cases_NRH, All_studies$total_cases, All_studies$controls_NRH, All_studies$total_controls))
# op <- par(cex = .75, font = 0.25)
# text(c(+10, +8, +6, +4), 23, c("NRH", "Total", "NRH", "Total"))
# text(c(+10, +6),       24, c("Cases", "Control"))
# par(op)

metafor::funnel.rma(model)
metafor::regtest(model) # No plot asymmtry was highlighted

