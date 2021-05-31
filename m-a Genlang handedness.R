setwd("~/University of St Andrews/Silvia Paracchini - gen_lang_hand_meta/Old")
 
library(tidyverse)
library(robumeta)
library(readxl)
library(meta)
library(metafor)

# Read file #####
LOR_SLI <- read_xlsx('genlang_all_cohorts_updated.xlsx', sheet = 'SLI-All') %>%
  filter(Study != 'Manchester', Study != 'UKDYS') %>%
  mutate(OR = (`SLI Left-Handers`/(`SLI Total N` - `SLI Left-Handers`))/(`TD Left-Handers`/(`TD Total N`-`TD Left-Handers`)) )

LOR_SLI_Female <- read_xlsx('genlang_all_cohorts_updated.xlsx', sheet = 'SLI-Female') %>%
  filter(Study != 'Manchester', Study != 'UKDYS') %>%
  mutate(OR = (`SLI Left-Handers`/(`SLI Total N` - `SLI Left-Handers`))/(`TD Left-Handers`/(`TD Total N`-`TD Left-Handers`)) )

LOR_SLI_Male <- read_xlsx('genlang_all_cohorts_updated.xlsx', sheet = 'SLI-Male') %>%
  filter(Study != 'Manchester', Study != 'UKDYS') %>%
  mutate(OR = (`SLI Left-Handers`/(`SLI Total N` - `SLI Left-Handers`))/(`TD Left-Handers`/(`TD Total N`-`TD Left-Handers`)) )


LOR_RD <- read_xlsx('genlang_all_cohorts_updated.xlsx', sheet = 'RD-All') %>%
  filter(Study != 'NTR') %>% 
  filter(Study != 'York_ALSPAC') %>%
  filter(Study != 'Manchester', Study != 'UKDYS') %>%
  mutate(OR = (`RD Left-Handers`/(`RD Total N` - `RD Left-Handers`))/(`TD Left-Handers`/(`TD Total N`-`TD Left-Handers`)) )

LOR_RD_Male <- read_xlsx('genlang_all_cohorts_updated.xlsx', sheet = 'RD-Male') %>%
  filter(Study != 'NTR') %>% 
  filter(Study != 'Manchester', Study != 'UKDYS') %>%
  mutate(OR = (`RD Left-Handers`/(`RD Total N` - `RD Left-Handers`))/(`TD Left-Handers`/(`TD Total N`-`TD Left-Handers`)) )

LOR_RD_Female <- read_xlsx('genlang_all_cohorts_updated.xlsx', sheet = 'RD-Female') %>%
  filter(Study != 'Manchester', Study != 'UKDYS') %>%
  filter(Study != 'NTR') %>%
  mutate(OR = (`RD Left-Handers`/(`RD Total N` - `RD Left-Handers`))/(`TD Left-Handers`/(`TD Total N`-`TD Left-Handers`)) )


LOR_COMBINED <- read_xlsx('genlang_all_cohorts_updated.xlsx', sheet = 'Combined2') %>%
  filter(Study != 'Peters') %>%
  filter(Study != 'Manchester', Study != 'UKDYS') %>%
  filter(Study != 'York_ALSPAC') %>%
  mutate(OR = (`Combined Left-Handers`/(`Combined Total N` - `Combined Left-Handers`))/(`TD Left-Handers`/(`TD Total N`-`TD Left-Handers`)) )

LOR_COMBINED_Female <- read_xlsx('genlang_all_cohorts_updated.xlsx', sheet = 'Combined-Female2') %>%
  filter(Study != 'Peters') %>%
  filter(Study != 'Manchester', Study != 'UKDYS') %>%
  mutate(OR = (`Combined Left-Handers`/(`Combined Total N` - `Combined Left-Handers`))/(`TD Left-Handers`/(`TD Total N`-`TD Left-Handers`)) )

LOR_COMBINED_Male <- read_xlsx('genlang_all_cohorts_updated.xlsx', sheet = 'Combined-Male2') %>%
  filter(Study != 'Peters') %>%
  filter(Study != 'Manchester', Study != 'UKDYS') %>%
  mutate(OR = (`Combined Left-Handers`/(`Combined Total N` - `Combined Left-Handers`))/(`TD Left-Handers`/(`TD Total N`-`TD Left-Handers`)) )



meta.LOR_RD <- metabin(`RD Left-Handers`, # Taken from https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/binary.html
                    `RD Total N`,
                    `TD Left-Handers`,
                    `TD Total N`,
                    data = LOR_RD,
                    studlab = Study,
                    comb.fixed = F,
                    # byvar = Cohort,
                    print.byvar = T,
                    comb.random = T,
                    level=0.95,
                    # method.tau = "SJ",
                    # hakn = TRUE, # Not sure how Marietta did it before. ONLY FOR RANDOM EFFECT use if trials have similar sample size
                    prediction = TRUE,
                    warn = TRUE,
                    label.e = "Cases",
                    label.c = "Controls",
                    title = "Non-right-handedness RD vs TD",
                    # incr = 0.1, # Only for small sample size, deafault is 0.5 in the example is found is 1
                    sm = "OR")

meta.LOR_RD_Male <- metabin(`RD Left-Handers`, # Taken from https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/binary.html
                       `RD Total N`,
                       # byvar = Cohort,
                       print.byvar = T,
                       `TD Left-Handers`,
                       `TD Total N`,
                       data = LOR_RD_Male,
                       studlab = Study,
                       comb.fixed = F,
                       comb.random = T,
                       level=0.95,
                       # method.tau = "SJ",
                       # hakn = TRUE, # Not sure how Marietta did it before. ONLY FOR RANDOM EFFECT use if trials have similar sample size
                       prediction = TRUE,
                       warn = TRUE,
                       label.e = "Cases",
                       label.c = "Controls",
                       title = "Non-right-handedness RD vs TD",
                       # incr = 0.1, # Only for small sample size, deafault is 0.5 in the example is found is 1
                       sm = "OR")

meta.LOR_RD_Female <- metabin(`RD Left-Handers`, # Taken from https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/binary.html
                            `RD Total N`,
                            `TD Left-Handers`,
                            `TD Total N`,
                            data = LOR_RD_Female,
                            studlab = Study,
                            comb.fixed = F,
                            comb.random = T,
                            level=0.95,
                            # byvar = Cohort,
                            print.byvar = T,
                            # method.tau = "SJ",
                            # hakn = TRUE, # Not sure how Marietta did it before. ONLY FOR RANDOM EFFECT use if trials have similar sample size
                            prediction = TRUE,
                            warn = TRUE,
                            label.e = "Cases",
                            label.c = "Controls",
                            title = "Non-right-handedness RD vs TD",
                            # incr = 0.1, # Only for small sample size, deafault is 0.5 in the example is found is 1
                            sm = "OR")


meta.LOR_SLI <- metabin(`SLI Left-Handers`, # Taken from https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/binary.html
                       `SLI Total N`,
                       `TD Left-Handers`,
                       `TD Total N`,
                       # byvar = Cohort,
                       print.byvar = T,
                       data = LOR_SLI,
                       studlab = Study,
                       comb.fixed = F,
                       comb.random = T,
                       level=0.95,
                       # method.tau = "SJ",
                       # hakn = TRUE, # Not sure how Marietta did it before
                       prediction = TRUE,
                       warn = TRUE,
                       label.e = "Cases",
                       label.c = "Controls",
                       title = "Non-right-handedness DLD vs TD",
                       # incr = 0.1, # Only for small sample size, deafault is 0.5 in the example is found is 1
                       sm = "OR")

meta.LOR_SLI_Female <- metabin(`SLI Left-Handers`, # Taken from https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/binary.html
                        `SLI Total N`,
                        `TD Left-Handers`,
                        `TD Total N`,
                        data = LOR_SLI_Female,
                        # byvar = Cohort,
                        print.byvar = T,
                        studlab = Study,
                        comb.fixed = F,
                        comb.random = T,
                        level=0.95,
                        # method.tau = "SJ",
                        # hakn = TRUE, # Not sure how Marietta did it before
                        prediction = TRUE,
                        warn = TRUE,
                        label.e = "Cases",
                        label.c = "Controls",
                        title = "Non-right-handedness DLD vs TD",
                        # incr = 0.1, # Only for small sample size, deafault is 0.5 in the example is found is 1
                        sm = "OR")

meta.LOR_SLI_Male <- metabin(`SLI Left-Handers`, # Taken from https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/binary.html
                               `SLI Total N`,
                               `TD Left-Handers`,
                               `TD Total N`,
                               data = LOR_SLI_Male,
                               studlab = Study,
                               comb.fixed = F,
                               comb.random = T,
                               # byvar = Cohort,
                               print.byvar = T,
                               level=0.95,
                               # method.tau = "SJ",
                               # hakn = TRUE, # Not sure how Marietta did it before
                               prediction = TRUE,
                               warn = TRUE,
                               label.e = "Cases",
                               label.c = "Controls",
                               title = "Non-right-handedness DLD vs TD",
                               # incr = 0.1, # Only for small sample size, deafault is 0.5 in the example is found is 1
                               sm = "OR")



meta.LOR_COMBINED <- metabin(`Combined Left-Handers`, # Taken from https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/binary.html
                       `Combined Total N`,
                       `TD Left-Handers`,
                       `TD Total N`,
                       data = LOR_COMBINED,
                       studlab = Study,
                       byvar = Cohort,
                       print.byvar = T,
                       comb.fixed = F,
                       comb.random = T,
                       level=0.95,
                       # method.tau = "SJ",
                       # hakn = TRUE, # Not sure how Marietta did it before
                       prediction = TRUE,
                       warn = TRUE,
                       label.e = "Combined",
                       label.c = "Controls",
                       title = "Non-right-handedness Combined vs TD",
                       # incr = 0.1, # Only for small sample size, deafault is 0.5 in the example is found is 1
                       sm = "OR")

meta.LOR_COMBINED_Female <- metabin(`Combined Left-Handers`, # Taken from https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/binary.html
                             `Combined Total N`,
                             `TD Left-Handers`,
                             `TD Total N`,
                             data = LOR_COMBINED_Female,
                             studlab = Study,
                             byvar = Cohort,
                             print.byvar = T,
                             comb.fixed = F,
                             comb.random = T,
                             level=0.95,
                             # method.tau = "SJ",
                             # hakn = TRUE, # Not sure how Marietta did it before
                             prediction = TRUE,
                             warn = TRUE,
                             label.e = "Combined",
                             label.c = "Controls",
                             title = "Non-right-handedness Combined vs TD",
                             # incr = 0.1, # Only for small sample size, deafault is 0.5 in the example is found is 1
                             sm = "OR")

meta.LOR_COMBINED_Male <- metabin(`Combined Left-Handers`, # Taken from https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/binary.html
                             `Combined Total N`,
                             `TD Left-Handers`,
                             `TD Total N`,
                             data = LOR_COMBINED_Male,
                             studlab = Study,
                             byvar = Cohort,
                             print.byvar = T,
                             comb.fixed = F,
                             comb.random = T,
                             level=0.95,
                             # method.tau = "SJ",
                             # hakn = TRUE, # Not sure how Marietta did it before
                             prediction = TRUE,
                             warn = TRUE,
                             label.e = "Combined",
                             label.c = "Controls",
                             title = "Non-right-handedness Combined vs TD",
                             # incr = 0.1, # Only for small sample size, deafault is 0.5 in the example is found is 1
                             sm = "OR")


metabias(meta.LOR_COMBINED, method.bias = "linreg", k = 8) # > can't run for N<10 studies

# Print summaries ####
summary(meta.LOR_RD)
summary(meta.LOR_RD_Female)
summary(meta.LOR_RD_Male)

summary(meta.LOR_SLI)
summary(meta.LOR_SLI_Female)
summary(meta.LOR_SLI_Male)

summary(meta.LOR_COMBINED)
summary(meta.LOR_COMBINED_Female)
summary(meta.LOR_COMBINED_Male)


# print(robumeta::robu(formula = OR ~ Cohort, data = LOR_COMBINED, studynum = study, var.eff.size = variance_100, modelweights = "CORR", small = FALSE))


# Forest plot ####
forest(meta.LOR_RD, comb.fixed = F, comb.random = T, print.pval.Q = T, print.I2.ci = T,
       print.tau.ci = T, prediction = F, overall = T, smlab = "All", lab.e = 'Reading impairment')
forest(meta.LOR_RD_Male, comb.fixed = F, comb.random = T, print.pval.Q = T, print.I2.ci = T,
       print.tau.ci = T, prediction = F, overall = T, lab.e = "Reading impairment", smlab = 'Males')
forest(meta.LOR_RD_Female, comb.fixed = F, comb.random = T, print.pval.Q = T, print.I2.ci = T,
       print.tau.ci = T, prediction = F, overall = T, lab.e = "Reading impairment", smlab = 'Females')


forest(meta.LOR_SLI, comb.fixed = F, comb.random = T, print.zval = T, print.pval.Q = T, print.I2.ci = T,
       print.tau.ci = T, prediction = F, overall = T, smlab = "All", lab.e = "Language impairment")
forest(meta.LOR_SLI_Male, comb.fixed = F, comb.random = T, print.zval = T, print.pval.Q = T, print.I2.ci = T,
       print.tau.ci = T, prediction = F, overall = T, smlab = "Males", lab.e = "Language impairment")
forest(meta.LOR_SLI_Female, comb.fixed = F, comb.random = T, print.zval = T, print.pval.Q = T, print.I2.ci = T,
       print.tau.ci = T, prediction = F, overall = T, smlab = "Females",lab.e = "Language impairment")

forest(meta.LOR_COMBINED, comb.fixed = F, comb.random = T, print.pval.Q = T, print.I2.ci = T, lab.e = 'Combined cases',
       prediction = F, overall = T, smlab = "All", subgroup = T, col.by = 'black')
forest(meta.LOR_COMBINED_Male, comb.fixed = F, comb.random = T, print.pval.Q = T, print.I2.ci = T, lab.e = 'Combined cases',
       print.tau.ci = T, prediction = F, overall = T, smlab = "Males", col.by = 'black')
forest(meta.LOR_COMBINED_Female, comb.fixed = F, comb.random = T, print.zval = T, print.pval.Q = T, print.I2.ci = T, lab.e = 'Combined cases',
       print.tau.ci = T, prediction = F, overall = T, smlab = "Females", col.by = 'black')


# Funnel plots ####
funnel(meta.LOR_RD, comb.fixed = F,backtransf = F, main = "Funnel plot of Standard Error by Log odds ratio Left Hand RD", studlab = T )
funnel(meta.LOR_SLI, comb.fixed =F ,backtransf = F, main = "Funnel plot of Standard Error by Log odds ratio Left Hand SLI", studlab = T)
funnel(meta.LOR_COMBINED, comb.fixed = F,backtransf = F, title = "Funnel plot of Standard Error by Log odds ratio Left Hand SLI", studlab = T, cex.studlab = 0.7)
