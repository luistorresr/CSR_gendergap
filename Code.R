
#### load packages ######

library(semTools)
library(lavaan)
library(plspm)
library(mediation)
library(Hmisc)
library(psych)
library(haven)
library(tidyverse)

###### Importing data file from SPSS 

original_data <- read_sav("Database/data_spss.sav")
original_data <- as.data.frame(original_data)
class(original_data)
view(original_data)

# creating a working data file to perform analysis and data manipulation  

working_data <- original_data


############### clean version of the CFA ###########################

### CSR survey model 1 - Final CFA for the best 6 dimensions model fit with this sample

CFA_model1_csr <- 'protecting =~   CSR2_rev + CSR3_rev + CSR4_rev
              compliance =~ CSR6 + CSR7 + CSR8 + CSR9 + CSR10 + CSR16 + CSR14 
              capability =~  CSR11 + CSR21 + CSR17
              embedding =~  CSR18 + CSR19 + CSR20 
              strategizing =~ CSR22 + CSR23 + CSR24 + CSR25 + CSR26 + CSR15 + CSR13
              transforming =~  CSR27 + CSR28 + CSR29 + CSR30'

CFA_fitmodel1_csr <- cfa(CFA_model1_csr, data = working_data, estimator = "ULSMV", mimic = "Mplus")
summary(CFA_fitmodel1_csr, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)
reliability(CFA_fitmodel1_csr) # Reliability

### CSR survey model 2 - Final CFA for the second order factor with this sample

CFA_model2_csr <- 'protecting =~   CSR2_rev + CSR3_rev + CSR4_rev
              compliance =~ CSR6 + CSR7 + CSR8 + CSR9 + CSR10 + CSR16 + CSR14 
              capability =~  CSR11 + CSR21 + CSR17
              embedding =~  CSR18 + CSR19 + CSR20 
              strategizing =~ CSR22 + CSR23 + CSR24 + CSR25 + CSR26 + CSR15 + CSR13
              transforming =~  CSR27 + CSR28 + CSR29 + CSR30
              CSR_development =~ protecting + compliance + capability + embedding + strategizing + transforming'

CFA_fitmodel2_csr <- cfa(CFA_model2_csr, data = working_data, estimator = "ULSMV", mimic = "Mplus")
summary(CFA_fitmodel2_csr, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)
reliability(CFA_fitmodel2_csr) # Reliability


### Gender survey - Final CFA for the best model fit with this sample

CFA_model_gender <- ' diversity =~ Gender1 + Gender2 + Gender3 + Gender4 + Gender5 + Gender6
                  Equal =~ Gender7 + Gender8 + Gender9 + Gender10 + Gender11 + Gender12
                  Infraestructure =~ Gender13 + Gender14 + Gender15 + Gender16 + Gender17 + Gender18
                  Exchange =~ Gender19 + Gender20 + Gender21'

CFA_fitmodel_gender <- cfa(CFA_model_gender, data = working_data, estimator = "ULSMV", group = NULL, mimic = "Mplus")
summary(CFA_fitmodel_gender, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)
reliability(CFA_fitmodel_gender) # reliability


############### Surveys scores ############################

# (1) create colums with sum scores for each dimension 

working_data <- working_data %>% mutate(sum_protect_negative = CRS2 + CSR3 + CSR4,
                        sum_protect_positive = CSR2_rev + CSR3_rev + CSR4_rev,
                        sum_compl = CSR6 + CSR7 + CSR8 + CSR9 + CSR10 + CSR16 + CSR14, 
                        sum_capab = CSR11 + CSR21 + CSR17,
                        sum_embed = CSR18 + CSR19 + CSR20,
                        sum_strate = CSR22 + CSR23 + CSR24 + CSR25 + CSR26 + CSR15 + CSR13,
                        sum_transf =  CSR27 + CSR28 + CSR29 + CSR30,
                        sum_diver = Gender1 + Gender2 + Gender3 + Gender4 + Gender5 + Gender6,
                        sum_equal = Gender7 + Gender8 + Gender9 + Gender10 + Gender11 + Gender12,
                        sum_infrae = Gender13 + Gender14 + Gender15 + Gender16 + Gender17 + Gender18,
                        sum_exch = Gender19 + Gender20 + Gender21)
                        
working_data <- working_data %>% mutate(sum_CSR_development = sum_protect_positive + sum_compl + sum_capab +
                                          sum_embed + sum_strate + sum_transf,
                                        sum_gender_strategies = sum_diver + sum_equal + sum_infrae + sum_exch)

# (2) create survey ratios

working_data <- working_data %>% mutate(ratio_protect_negative = sum_protect_negative / 15,
                        ratio_protect_positive = sum_protect_positive / 15,
                        ratio_compl = sum_compl / 35, 
                        ratio_capab = sum_capab / 15,
                        ratio_embed = sum_embed / 15,
                        ratio_strate = sum_strate / 35,
                        ratio_transf =  sum_transf / 20,
                        ratio_diver = sum_diver / 30,
                        ratio_equal = sum_equal / 30,
                        ratio_infrae = sum_infrae / 30,
                        ratio_exch = sum_exch / 15,
                        ratio_CSR_development = (ratio_protect_positive + ratio_compl + ratio_capab + ratio_embed + ratio_strate + ratio_transf) / 6,
                                                ratio_gender_strategies = (ratio_diver + ratio_equal + ratio_infrae + ratio_exch) / 4)


###### gender gap calculation 

# (1) creating female/male ratios

working_data <- working_data %>% mutate(ratio_employees = WomenEmployees / (1- WomenEmployees),
                        ratio_managers = WomenManagers / (1 - WomenManagers),
                        ratio_midmanagers = WomenMiddleManagers / (1 - WomenMiddleManagers))

# (2) convert to parity all values where women represent 100% (1/0 = inf)

working_data$ratio_employees[is.infinite(working_data$ratio_employees)] <- 1
working_data$ratio_managers[is.infinite(working_data$ratio_managers)] <- 1
working_data$ratio_midmanagers[is.infinite(working_data$ratio_midmanagers)] <- 1

# (3) truncate gap scores

working_data <- working_data %>% mutate(gap_employees = case_when(ratio_employees > 1 ~ 1, ratio_employees <= 1 ~ ratio_employees),
                        gap_managers = case_when(ratio_managers > 1 ~ 1, ratio_managers <= 1 ~ ratio_managers),
                        gap_midmanagers = case_when(ratio_midmanagers > 1 ~ 1, ratio_midmanagers <= 1 ~ ratio_midmanagers))


# (4) calculate composite gender gap 

working_data <- working_data %>% mutate(gap = (gap_employees + gap_managers + gap_midmanagers) / 3)


# (5) delete NA's 

working_data_no_na <- working_data %>% drop_na(gap) # clean database


############# Analysis #######################

### correlation matrix

correlation <- working_data_no_na %>% select(ratio_CSR_development,
                                          ratio_diver, 
                                          ratio_equal, 
                                          ratio_infrae, 
                                          ratio_exch,
                                          gap,
                                          CSRtime,
                                          Size)

rcorr(as.matrix(correlation), type="pearson") # with significance level
describe(correlation)


### PLS-PM model using the composite CSR measure

size= c(0, 0, 0, 0, 0, 0, 0, 0)
csr_time = c(0, 0, 0, 0, 0, 0, 0, 0)
csr_composite= c(1, 1, 0, 0, 0, 0, 0, 0)
diversity = c(0, 0, 1, 0, 0, 0, 0, 0)
opportunities = c(0, 0, 1, 0, 0, 0, 0, 0)
internal = c(0, 0, 1, 0, 0, 0, 0, 0)
external = c(0, 0, 1, 0, 0, 0, 0, 0)
gap = c(0, 0, 1, 1, 1, 1, 1, 0)

path = rbind(size,csr_time, csr_composite, diversity, opportunities, internal, external, gap)

colnames(path) = rownames(path) # add column names
innerplot(path) # plot the path matrix
mode = c("A", "A", "A", "A", "A", "A", "A", "A")

which(names(working_data_no_na)=="ratio_infrae") # checking column number

blocks = list(64, 68, 99, 95, 96, 97, 98, 107) 

scaling <- list(c("ORD"),
                c("ORD"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM")) # this is created with the calculated ratios for the CSR dimensions and the items scores for the gender survey

pls_CSR_composite <- plspm(working_data_no_na, path, blocks, modes = mode, scaling = scaling, scheme = "path", scaled = TRUE, tol = 1e-06, maxiter = 200, plscomp = NULL, boot.val = TRUE, br = 5000, dataset = TRUE)
summary(pls_CSR_composite)


### PLS model for each CSR dimension

# (1) self-protecting

size= c(0, 0, 0, 0, 0, 0, 0, 0)
csr_time = c(0, 0, 0, 0, 0, 0, 0, 0)
protecting= c(1, 1, 0, 0, 0, 0, 0, 0)
diversity = c(0, 0, 1, 0, 0, 0, 0, 0)
opportunities = c(0, 0, 1, 0, 0, 0, 0, 0)
internal = c(0, 0, 1, 0, 0, 0, 0, 0)
external = c(0, 0, 1, 0, 0, 0, 0, 0)
gap = c(0, 0, 1, 1, 1, 1, 1, 0)

path = rbind(size, csr_time, protecting, diversity, opportunities, internal, external, gap)

colnames(path) = rownames(path) # add column names
innerplot(path) # plot the path matrix
mode = c("A", "A", "A", "A", "A", "A", "A", "A")

which(names(working_data_no_na)=="ratio_protect_negative")

blocks = list(64, 68, 88, 95, 96, 97, 98, 107) 

scaling <- list(c("ORD"),
                c("ORD"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM")) # this is created with the calculated ratios for the CSR dimensions and the items scores for the gender survey

pls_protecting <- plspm(working_data_no_na, path, blocks, modes = mode, scaling = scaling, scheme = "path", scaled = TRUE, tol = 1e-06, maxiter = 200, plscomp = NULL, boot.val = TRUE, br = 5000, dataset = TRUE)
summary(pls_protecting)


# (2) compliance-seeking

size= c(0, 0, 0, 0, 0, 0, 0, 0)
csr_time = c(0, 0, 0, 0, 0, 0, 0, 0)
compliance= c(1, 1, 0, 0, 0, 0, 0, 0)
diversity = c(0, 0, 1, 0, 0, 0, 0, 0)
opportunities = c(0, 0, 1, 0, 0, 0, 0, 0)
internal = c(0, 0, 1, 0, 0, 0, 0, 0)
external = c(0, 0, 1, 0, 0, 0, 0, 0)
gap = c(0, 0, 1, 1, 1, 1, 1, 0)

path = rbind(size, csr_time, compliance, diversity, opportunities, internal, external, gap)

colnames(path) = rownames(path) # add column names
innerplot(path) # plot the path matrix
mode = c("A", "A", "A", "A", "A", "A", "A", "A")

which(names(working_data_no_na)=="ratio_compl")

blocks = list(64, 68, 90, 95, 96, 97, 98, 107) 

scaling <- list(c("ORD"),
                c("ORD"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM")) # this is created with the calculated ratios for the CSR dimensions and the items scores for the gender survey


pls_compliance <- plspm(working_data_no_na, path, blocks, modes = mode, scaling = scaling, scheme = "path", scaled = TRUE, tol = 1e-06, maxiter = 200, plscomp = NULL, boot.val = TRUE, br = 5000, dataset = TRUE)
summary(pls_compliance)


# (3) Capability-seeking

size= c(0, 0, 0, 0, 0, 0, 0, 0)
csr_time = c(0, 0, 0, 0, 0, 0, 0, 0)
capability= c(1, 1, 0, 0, 0, 0, 0, 0)
diversity = c(0, 0, 1, 0, 0, 0, 0, 0)
opportunities = c(0, 0, 1, 0, 0, 0, 0, 0)
internal = c(0, 0, 1, 0, 0, 0, 0, 0)
external = c(0, 0, 1, 0, 0, 0, 0, 0)
gap = c(0, 0, 1, 1, 1, 1, 1, 0)

path = rbind(size, csr_time, capability, diversity, opportunities, internal, external, gap)

colnames(path) = rownames(path) # add column names
innerplot(path) # plot the path matrix
mode = c("A", "A", "A", "A", "A", "A", "A", "A")

which(names(working_data_no_na)=="ratio_capab")

blocks = list(64, 68, 91, 95, 96, 97, 98, 107) 

scaling <- list(c("ORD"),
                c("ORD"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM")) # this is created with the calculated ratios for the CSR dimensions and the items scores for the gender survey

pls_capability <- plspm(working_data_no_na, path, blocks, modes = mode, scaling = scaling, scheme = "path", scaled = TRUE, tol = 1e-06, maxiter = 200, plscomp = NULL, boot.val = TRUE, br = 5000, dataset = TRUE)
summary(pls_capability)

# (4) Embedding
size= c(0, 0, 0, 0, 0, 0, 0, 0)
csr_time = c(0, 0, 0, 0, 0, 0, 0, 0)
embedding= c(1, 1, 0, 0, 0, 0, 0, 0)
diversity = c(0, 0, 1, 0, 0, 0, 0, 0)
opportunities = c(0, 0, 1, 0, 0, 0, 0, 0)
internal = c(0, 0, 1, 0, 0, 0, 0, 0)
external = c(0, 0, 1, 0, 0, 0, 0, 0)
gap = c(0, 0, 1, 1, 1, 1, 1, 0)

path = rbind(size, csr_time, embedding, diversity, opportunities, internal, external, gap)

colnames(path) = rownames(path) # add column names
innerplot(path) # plot the path matrix
mode = c("A", "A", "A", "A", "A", "A", "A", "A")

which(names(working_data_no_na)=="ratio_embed")

blocks = list(64, 68, 92, 95, 96, 97, 98, 107) 

scaling <- list(c("ORD"),
                c("ORD"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM")) # this is created with the calculated ratios for the CSR dimensions and the items scores for the gender survey


pls_embedding <- plspm(working_data_no_na, path, blocks, modes = mode, scaling = scaling, scheme = "path", scaled = TRUE, tol = 1e-06, maxiter = 200, plscomp = NULL, boot.val = TRUE, br = 5000, dataset = TRUE)
summary(pls_embedding)


# (5) strategizing

size= c(0, 0, 0, 0, 0, 0, 0, 0)
csr_time = c(0, 0, 0, 0, 0, 0, 0, 0)
strategizing= c(1, 1, 0, 0, 0, 0, 0, 0)
diversity = c(0, 0, 1, 0, 0, 0, 0, 0)
opportunities = c(0, 0, 1, 0, 0, 0, 0, 0)
internal = c(0, 0, 1, 0, 0, 0, 0, 0)
external = c(0, 0, 1, 0, 0, 0, 0, 0)
gap = c(0, 0, 1, 1, 1, 1, 1, 0)

path = rbind(size, csr_time, strategizing, diversity, opportunities, internal, external, gap)

colnames(path) = rownames(path) # add column names
innerplot(path) # plot the path matrix
mode = c("A", "A", "A", "A", "A", "A", "A", "A")

which(names(working_data_no_na)=="ratio_strate")

blocks = list(64, 68, 93, 95, 96, 97, 98, 107) 

scaling <- list(c("ORD"),
                c("ORD"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM")) # this is created with the calculated ratios for the CSR dimensions and the items scores for the gender survey


pls_strategizing <- plspm(working_data_no_na, path, blocks, modes = mode, scaling = scaling, scheme = "path", scaled = TRUE, tol = 1e-06, maxiter = 200, plscomp = NULL, boot.val = TRUE, br = 5000, dataset = TRUE)
summary(pls_strategizing)


# (6) transforming

size= c(0, 0, 0, 0, 0, 0, 0, 0)
csr_time = c(0, 0, 0, 0, 0, 0, 0, 0)
transforming= c(1, 1, 0, 0, 0, 0, 0, 0)
diversity = c(0, 0, 1, 0, 0, 0, 0, 0)
opportunities = c(0, 0, 1, 0, 0, 0, 0, 0)
internal = c(0, 0, 1, 0, 0, 0, 0, 0)
external = c(0, 0, 1, 0, 0, 0, 0, 0)
gap = c(0, 0, 1, 1, 1, 1, 1, 0)

path = rbind(size, csr_time, transforming, diversity, opportunities, internal, external, gap)

colnames(path) = rownames(path) # add column names
innerplot(path) # plot the path matrix
mode = c("A", "A", "A", "A", "A", "A", "A", "A")

which(names(working_data_no_na)=="ratio_transf")

blocks = list(64, 68, 94, 95, 96, 97, 98, 107) 

scaling <- list(c("ORD"),
                c("ORD"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM"),
                c("NUM")) # this is created with the calculated ratios for the CSR dimensions and the items scores for the gender survey


pls_transforming <- plspm(working_data_no_na, path, blocks, modes = mode, scaling = scaling, scheme = "path", scaled = TRUE, tol = 1e-06, maxiter = 200, plscomp = NULL, boot.val = TRUE, br = 5000, dataset = TRUE)
summary(pls_transforming)


### mediation

m1 <- lm(ratio_diver ~ ratio_compl + Size + CSRtime, working_data_no_na)
m2 <- lm(ratio_equal ~ ratio_compl + Size + CSRtime, working_data_no_na)
m3 <- lm(ratio_infrae ~ ratio_compl + Size + CSRtime, working_data_no_na)
m4 <- lm(ratio_exch ~ ratio_compl + Size + CSRtime, working_data_no_na)
m5 <- lm(ratio_gender_strategies ~ ratio_compl + Size + CSRtime, working_data_no_na)

y1 <- lm(gap ~ ratio_compl + ratio_diver + Size + CSRtime, working_data_no_na)
y2 <- lm(gap ~ ratio_compl + ratio_equal + Size + CSRtime, working_data_no_na)
y3 <- lm(gap ~ ratio_compl + ratio_infrae + Size + CSRtime, working_data_no_na)
y4 <- lm(gap ~ ratio_compl + ratio_exch + Size + CSRtime, working_data_no_na)

out1 <- mediate(m1, y1, boot = TRUE, boot.ci.type = "bca", treat = "ratio_compl", mediator = "ratio_diver")
out2 <- mediate(m2, y2, boot = TRUE, boot.ci.type = "bca", treat = "ratio_compl", mediator = "ratio_equal")
out3 <- mediate(m3, y3, boot = TRUE, boot.ci.type = "bca", treat = "ratio_compl", mediator = "ratio_infrae")
out4 <- mediate(m4, y4, boot = TRUE, boot.ci.type = "bca", treat = "ratio_compl", mediator = "ratio_exch")

summary(out1)
summary(out2)
summary(out3)
summary(out4)


