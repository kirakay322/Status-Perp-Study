##### RQ1A + RQ1B ##### --------------------------------------------------------

#### Mega Output ---------------------------------------------------------------

### Cleaning and subsetting ----------------------------------------------------
library(viridis)
library(dplyr)
library(tidyr)
library(tidyverse)
library(sjPlot)

d4 <- read.csv("PSS_Pooled_CLEAN_REdit.csv")

View(d4)

### Recode Sample variable -----------------------------------------------------
d4$Atonement_fact_r <- factor(d4$Atonement_fact_r, levels=c(0,1,2),
                              labels = c('High', 'Medium', 'Low'))

## Reverse code Stigma
d4$StigmaP_av_r <- 8 - d4$StigmaP_av

## Sample dummy codes
d4$Sample_DC1 <- as.numeric(dplyr::recode(d4$Sample, "0" = "1", "1" = "-1", "2" = "0"))
d4$Sample_DC2 <- as.numeric(dplyr::recode(d4$Sample, "0" = "1", "1" = "1", "2" = "-2"))

### Set up Helmert contrasts ---------------------------------------------------
## Reverse-code Atonement
my.contrasts <- list(Atonement_fact_r = contr.helmert)

### Split D4 by Status ---------------------------------------------------------
d4_lo<- d4[which(d4$SES == "0"),]
d4_hi <- d4[which(d4$SES == "1"),]


### RQ1A -----------------------------------------------------------------------
## Y1: Blame
# Step 0: Intercept

RQ1A_Y1_step0 <- lm(PResp_av ~ 1, data = d4_hi)
summary(RQ1A_Y1_step0)

# Step 1: Control variables - SONA
RQ1A_Y1_step1 <- lm(PResp_av ~ Sample_DC1 + Sample_DC2, data = d4_hi)
summary(RQ1A_Y1_step1)

anova(RQ1A_Y1_step0, RQ1A_Y1_step1)
RQ1A_Y1_rs1 <- summary(RQ1A_Y1_step0)$r.squared
RQ1A_Y1_rs2 <- summary(RQ1A_Y1_step1)$r.squared
RQ1A_Y1_rs.change1 <- RQ1A_Y1_rs1-RQ1A_Y1_rs2 # provides R-sqd-change
RQ1A_Y1_rs.change1

# Step 2: IVs of interest

RQ1A_Y1_step2 <- lm(PResp_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_re, contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y1_step2)

anova(RQ1A_Y1_step1, RQ1A_Y1_step2)
RQ1A_Y1_rs3 <- summary(RQ1A_Y1_step1)$r.squared
RQ1A_Y1_rs4 <- summary(RQ1A_Y1_step2)$r.squared
RQ1A_Y1_rs.change2 <-RQ1A_Y1_rs3-RQ1A_Y1_rs4 # provides R-sqd-change
RQ1A_Y1_rs.change2

# Step 3: Interactions

RQ1A_Y1_step3 <- lm(PResp_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*Atonement_fact_re, 
                    contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y1_step3)

anova(RQ1A_Y1_step2, RQ1A_Y1_step3)
RQ1A_Y1_rs5 <- summary(RQ1A_Y1_step2)$r.squared
RQ1A_Y1_rs6 <- summary(RQ1A_Y1_step3)$r.squared
RQ1A_Y1_rs.change3 <- RQ1A_Y1_rs5-RQ1A_Y1_rs6 # provides R-sqd-change
RQ1A_Y1_rs.change3

## Y2: Stigma
# Step 0: Intercept
RQ1A_Y2_step0 <- lm(StigmaP_av ~ 1, data = d4_hi)
summary(RQ1A_Y2_step0)

# Step 1: Control variables
RQ1A_Y2_step1 <- lm(StigmaP_av ~ Sample_DC1 + Sample_DC2, data = d4_hi)
summary(RQ1A_Y2_step1)

anova(RQ1A_Y2_step0, RQ1A_Y2_step1)
RQ1A_Y2_rs1 <- summary(RQ1A_Y2_step0)$r.squared
RQ1A_Y2_rs2 <- summary(RQ1A_Y2_step1)$r.squared
RQ1A_Y2_rs.change1 <- RQ1A_Y2_rs1-RQ1A_Y2_rs2 # provides R-sqd-change
RQ1A_Y2_rs.change1

# Step 2: IVs of interest

RQ1A_Y2_step2 <- lm(StigmaP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y2_step2)

anova(RQ1A_Y2_step1, RQ1A_Y2_step2)
RQ1A_Y2_rs3 <- summary(RQ1A_Y2_step1)$r.squared
RQ1A_Y2_rs4 <- summary(RQ1A_Y2_step2)$r.squared
RQ1A_Y2_rs.change2 <-RQ1A_Y2_rs3-RQ1A_Y2_rs4 # provides R-sqd-change
RQ1A_Y2_rs.change2

# Step 3: Interactions

RQ1A_Y2_step3 <- lm(StigmaP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*Atonement_fact_r, 
                    contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y2_step3)

anova(RQ1A_Y2_step2, RQ1A_Y2_step3)
RQ1A_Y2_rs5 <- summary(RQ1A_Y2_step2)$r.squared
RQ1A_Y2_rs6 <- summary(RQ1A_Y2_step3)$r.squared
RQ1A_Y2_rs.change3 <- RQ1A_Y2_rs5-RQ1A_Y2_rs6 # provides R-sqd-change
RQ1A_Y2_rs.change3

## Y3: Likability
# Step 0: Intercept
RQ1A_Y3_step0 <- lm(LikabilityP_av ~ 1, data = d4_hi)
summary(RQ1A_Y3_step0)

# Step 1: Control variables - SONA AND PROLIFIC 2 SIGNIFICANT
RQ1A_Y3_step1 <- lm(LikabilityP_av ~ Sample_DC1 + Sample_DC2, data = d4_hi)
summary(RQ1A_Y3_step1)

anova(RQ1A_Y3_step0, RQ1A_Y3_step1)
RQ1A_Y3_rs1 <- summary(RQ1A_Y3_step0)$r.squared
RQ1A_Y3_rs2 <- summary(RQ1A_Y3_step1)$r.squared
RQ1A_Y3_rs.change1 <- RQ1A_Y3_rs1-RQ1A_Y3_rs2 # provides R-sqd-change
RQ1A_Y3_rs.change1

# Step 2: IVs of interest

RQ1A_Y3_step2 <- lm(LikabilityP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y3_step2)

anova(RQ1A_Y3_step1, RQ1A_Y3_step2)
RQ1A_Y3_rs3 <- summary(RQ1A_Y3_step1)$r.squared
RQ1A_Y3_rs4 <- summary(RQ1A_Y3_step2)$r.squared
RQ1A_Y3_rs.change2 <-RQ1A_Y3_rs3-RQ1A_Y3_rs4 # provides R-sqd-change
RQ1A_Y3_rs.change2

# Step 3: Interactions

RQ1A_Y3_step3 <- lm(LikabilityP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y3_step3)

anova(RQ1A_Y3_step2, RQ1A_Y3_step3)
RQ1A_Y3_rs5 <- summary(RQ1A_Y3_step2)$r.squared
RQ1A_Y3_rs6 <- summary(RQ1A_Y3_step3)$r.squared
RQ1A_Y3_rs.change3 <- RQ1A_Y3_rs5-RQ1A_Y3_rs6 # provides R-sqd-change
RQ1A_Y3_rs.change3

## Y4: Personality
# Step 0: Intercept
RQ1A_Y4_step0 <- lm(PersolityP_av ~ 1, data = d4_hi)
summary(RQ1A_Y4_step0)

# Step 1: Control variables
RQ1A_Y4_step1 <- lm(PersolityP_av ~ Sample_DC1 + Sample_DC2, data = d4_hi)
summary(RQ1A_Y4_step1)

anova(RQ1A_Y4_step0, RQ1A_Y4_step1)
RQ1A_Y4_rs1 <- summary(RQ1A_Y4_step0)$r.squared
RQ1A_Y4_rs2 <- summary(RQ1A_Y4_step1)$r.squared
RQ1A_Y4_rs.change1 <- RQ1A_Y4_rs1-RQ1A_Y4_rs2 # provides R-sqd-change
RQ1A_Y4_rs.change1

# Step 2: IVs of interest 

RQ1A_Y4_step2 <- lm(PersolityP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y4_step2)

anova(RQ1A_Y4_step1, RQ1A_Y4_step2)
RQ1A_Y4_rs3 <- summary(RQ1A_Y4_step1)$r.squared
RQ1A_Y4_rs4 <- summary(RQ1A_Y4_step2)$r.squared
RQ1A_Y4_rs.change2 <-RQ1A_Y4_rs3-RQ1A_Y4_rs4 # provides R-sqd-change
RQ1A_Y4_rs.change2

# Step 3: Interactions

RQ1A_Y4_step3 <- lm(PersolityP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y4_step3)

anova(RQ1A_Y4_step2, RQ1A_Y4_step3)
RQ1A_Y4_rs5 <- summary(RQ1A_Y4_step2)$r.squared
RQ1A_Y4_rs6 <- summary(RQ1A_Y4_step3)$r.squared
RQ1A_Y4_rs.change3 <- RQ1A_Y4_rs5-RQ1A_Y4_rs6 # provides R-sqd-change
RQ1A_Y4_rs.change3

## Y5: Empathy
# Step 0: Intercept
RQ1A_Y5_step0 <- lm(EmpathyP_av ~ 1, data = d4_hi)
summary(RQ1A_Y5_step0)

# Step 1: Control variables - SONA different
RQ1A_Y5_step1 <- lm(EmpathyP_av ~ Sample_DC1 + Sample_DC2, data = d4_hi)
summary(RQ1A_Y5_step1)

anova(RQ1A_Y5_step0, RQ1A_Y5_step1)
RQ1A_Y5_rs1 <- summary(RQ1A_Y5_step0)$r.squared
RQ1A_Y5_rs2 <- summary(RQ1A_Y5_step1)$r.squared
RQ1A_Y5_rs.change1 <- RQ1A_Y5_rs1-RQ1A_Y5_rs2 # provides R-sqd-change
RQ1A_Y5_rs.change1

# Step 2: IVs of interest

RQ1A_Y5_step2 <- lm(EmpathyP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y5_step2)

anova(RQ1A_Y5_step1, RQ1A_Y5_step2)
RQ1A_Y5_rs3 <- summary(RQ1A_Y5_step1)$r.squared
RQ1A_Y5_rs4 <- summary(RQ1A_Y5_step2)$r.squared
RQ1A_Y5_rs.change2 <-RQ1A_Y5_rs3-RQ1A_Y5_rs4 # provides R-sqd-change
RQ1A_Y5_rs.change2

# Step 3: Interactions

RQ1A_Y5_step3 <- lm(EmpathyP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y5_step3)

anova(RQ1A_Y5_step2, RQ1A_Y5_step3)
RQ1A_Y5_rs5 <- summary(RQ1A_Y5_step2)$r.squared
RQ1A_Y5_rs6 <- summary(RQ1A_Y5_step3)$r.squared
RQ1A_Y5_rs.change3 <- RQ1A_Y5_rs5-RQ1A_Y5_rs6 # provides R-sqd-change
RQ1A_Y5_rs.change3

## Y6: Distress
# Step 0: Intercept
RQ1A_Y6_step0 <- lm(PDistress ~ 1, data = d4_hi)
summary(RQ1A_Y6_step0)

# Step 1: Control variables - SONA different
RQ1A_Y6_step1 <- lm(PDistress ~ Sample_DC1 + Sample_DC2, data = d4_hi)
summary(RQ1A_Y6_step1)

anova(RQ1A_Y6_step0, RQ1A_Y6_step1)
RQ1A_Y6_rs1 <- summary(RQ1A_Y6_step0)$r.squared
RQ1A_Y6_rs2 <- summary(RQ1A_Y6_step1)$r.squared
RQ1A_Y6_rs.change1 <- RQ1A_Y6_rs1-RQ1A_Y6_rs2 # provides R-sqd-change
RQ1A_Y6_rs.change1

# Step 2: IVs of interest

RQ1A_Y6_step2 <- lm(PDistress ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y6_step2)

anova(RQ1A_Y6_step1, RQ1A_Y6_step2)
RQ1A_Y6_rs3 <- summary(RQ1A_Y6_step1)$r.squared
RQ1A_Y6_rs4 <- summary(RQ1A_Y6_step2)$r.squared
RQ1A_Y6_rs.change2 <-RQ1A_Y6_rs3-RQ1A_Y6_rs4 # provides R-sqd-change
RQ1A_Y6_rs.change2

# Step 3: Interactions

RQ1A_Y6_step3 <- lm(PDistress ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y6_step3)

anova(RQ1A_Y6_step2, RQ1A_Y6_step3)
RQ1A_Y6_rs5 <- summary(RQ1A_Y6_step2)$r.squared
RQ1A_Y6_rs6 <- summary(RQ1A_Y6_step3)$r.squared
RQ1A_Y6_rs.change3 <- RQ1A_Y6_rs5-RQ1A_Y6_rs6 # provides R-sqd-change
RQ1A_Y6_rs.change3

## Y7: Incident - Responsibility
# Step 0: Intercept
RQ1A_Y7_step0 <- lm(resp_scale_1 ~ 1, data = d4_hi)
summary(RQ1A_Y7_step0)

# Step 1: Control variables - SONA different
RQ1A_Y7_step1 <- lm(resp_scale_1 ~ Sample_DC1 + Sample_DC2, data = d4_hi)
summary(RQ1A_Y7_step1)

anova(RQ1A_Y7_step0, RQ1A_Y7_step1)
RQ1A_Y7_rs1 <- summary(RQ1A_Y7_step0)$r.squared
RQ1A_Y7_rs2 <- summary(RQ1A_Y7_step1)$r.squared
RQ1A_Y7_rs.change1 <- RQ1A_Y7_rs1-RQ1A_Y7_rs2 # provides R-sqd-change
RQ1A_Y7_rs.change1

# Step 2: IVs of interest

RQ1A_Y7_step2 <- lm(resp_scale_1 ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y7_step2)

anova(RQ1A_Y7_step1, RQ1A_Y7_step2)
RQ1A_Y7_rs3 <- summary(RQ1A_Y7_step1)$r.squared
RQ1A_Y7_rs4 <- summary(RQ1A_Y7_step2)$r.squared
RQ1A_Y7_rs.change2 <-RQ1A_Y7_rs3-RQ1A_Y7_rs4 # provides R-sqd-change
RQ1A_Y7_rs.change2

# Step 3: Interactions

RQ1A_Y7_step3 <- lm(resp_scale_1 ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y7_step3)

anova(RQ1A_Y7_step2, RQ1A_Y7_step3)
RQ1A_Y7_rs5 <- summary(RQ1A_Y7_step2)$r.squared
RQ1A_Y7_rs6 <- summary(RQ1A_Y7_step3)$r.squared
RQ1A_Y7_rs.change3 <- RQ1A_Y7_rs5-RQ1A_Y7_rs6 # provides R-sqd-change
RQ1A_Y7_rs.change3

## Y8: Incident - Blame
# Step 0: Intercept
RQ1A_Y8_step0 <- lm(resp_scale_2 ~ 1, data = d4_hi)
summary(RQ1A_Y8_step0)

# Step 1: Control variables - SONA different
RQ1A_Y8_step1 <- lm(resp_scale_2 ~ Sample_DC1 + Sample_DC2, data = d4_hi)
summary(RQ1A_Y8_step1)

anova(RQ1A_Y8_step0, RQ1A_Y8_step1)
RQ1A_Y8_rs1 <- summary(RQ1A_Y8_step0)$r.squared
RQ1A_Y8_rs2 <- summary(RQ1A_Y8_step1)$r.squared
RQ1A_Y8_rs.change1 <- RQ1A_Y8_rs1-RQ1A_Y8_rs2 # provides R-sqd-change
RQ1A_Y8_rs.change1

# Step 2: IVs of interest

RQ1A_Y8_step2 <- lm(resp_scale_2 ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y8_step2)

anova(RQ1A_Y8_step1, RQ1A_Y8_step2)
RQ1A_Y8_rs3 <- summary(RQ1A_Y8_step1)$r.squared
RQ1A_Y8_rs4 <- summary(RQ1A_Y8_step2)$r.squared
RQ1A_Y8_rs.change2 <-RQ1A_Y8_rs3-RQ1A_Y8_rs4 # provides R-sqd-change
RQ1A_Y8_rs.change2

# Step 3: Interactions

RQ1A_Y8_step3 <- lm(resp_scale_2 ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_hi)
summary(RQ1A_Y8_step3)

anova(RQ1A_Y8_step2, RQ1A_Y8_step3)
RQ1A_Y8_rs5 <- summary(RQ1A_Y8_step2)$r.squared
RQ1A_Y8_rs6 <- summary(RQ1A_Y8_step3)$r.squared
RQ1A_Y8_rs.change3 <- RQ1A_Y8_rs5-RQ1A_Y8_rs6 # provides R-sqd-change
RQ1A_Y8_rs.change3

### RQ1B -----------------------------------------------------------------------
## Y1: Blame
# Regression table
# Step 0: Intercept
RQ1B_Y1_step0 <- lm(PResp_av ~ 1, data = d4_lo)
summary(RQ1B_Y1_step0)

# Step 1: Control variables - SONA Sig.
RQ1B_Y1_step1 <- lm(PResp_av ~ Sample_DC1 + Sample_DC2, data = d4_lo)
summary(RQ1B_Y1_step1)

anova(RQ1B_Y1_step0, RQ1B_Y1_step1)
RQ1B_Y1_rs1 <- summary(RQ1B_Y1_step0)$r.squared
RQ1B_Y1_rs2 <- summary(RQ1B_Y1_step1)$r.squared
RQ1B_Y1_rs.change1 <- RQ1B_Y1_rs1-RQ1B_Y1_rs2 # provides R-sqd-change
RQ1B_Y1_rs.change1

# Step 2: IVs of interest

RQ1B_Y1_step2 <- lm(PResp_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y1_step2)

anova(RQ1B_Y1_step1, RQ1B_Y1_step2)
RQ1B_Y1_rs3 <- summary(RQ1B_Y1_step1)$r.squared
RQ1B_Y1_rs4 <- summary(RQ1B_Y1_step2)$r.squared
RQ1B_Y1_rs.change2 <-RQ1B_Y1_rs3-RQ1B_Y1_rs4 # provides R-sqd-change
RQ1B_Y1_rs.change2

# Step 3: Interactions

RQ1B_Y1_step3 <- lm(PResp_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*Atonement_fact_r, 
                    contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y1_step3)

anova(RQ1B_Y1_step2, RQ1B_Y1_step3)
RQ1B_Y1_rs5 <- summary(RQ1B_Y1_step2)$r.squared
RQ1B_Y1_rs6 <- summary(RQ1B_Y1_step3)$r.squared
RQ1B_Y1_rs.change3 <- RQ1B_Y1_rs5-RQ1B_Y1_rs6 # provides R-sqd-change
RQ1B_Y1_rs.change3

## Y2: Stigma
# Step 0: Intercept
RQ1B_Y2_step0 <- lm(StigmaP_av ~ 1, data = d4_lo)
summary(RQ1B_Y2_step0)

# Step 1: Control variables - SONA Sig.
RQ1B_Y2_step1 <- lm(StigmaP_av ~ Sample_DC1 + Sample_DC2, data = d4_lo)
summary(RQ1B_Y2_step1)

anova(RQ1B_Y2_step0, RQ1B_Y2_step1)
RQ1B_Y2_rs1 <- summary(RQ1B_Y2_step0)$r.squared
RQ1B_Y2_rs2 <- summary(RQ1B_Y2_step1)$r.squared
RQ1B_Y2_rs.change1 <- RQ1B_Y2_rs1-RQ1B_Y2_rs2 # provides R-sqd-change
RQ1B_Y2_rs.change1

# Step 2: IVs of interest

RQ1B_Y2_step2 <- lm(StigmaP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y2_step2)

anova(RQ1B_Y2_step1, RQ1B_Y2_step2)
RQ1B_Y2_rs3 <- summary(RQ1B_Y2_step1)$r.squared
RQ1B_Y2_rs4 <- summary(RQ1B_Y2_step2)$r.squared
RQ1B_Y2_rs.change2 <-RQ1B_Y2_rs3-RQ1B_Y2_rs4 # provides R-sqd-change
RQ1B_Y2_rs.change2

# Step 3: Interactions

RQ1B_Y2_step3 <- lm(StigmaP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*Atonement_fact_r, 
                    contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y2_step3)

anova(RQ1B_Y2_step2, RQ1B_Y2_step3)
RQ1B_Y2_rs5 <- summary(RQ1B_Y2_step2)$r.squared
RQ1B_Y2_rs6 <- summary(RQ1B_Y2_step3)$r.squared
RQ1B_Y2_rs.change3 <- RQ1B_Y2_rs5-RQ1B_Y2_rs6 # provides R-sqd-change
RQ1B_Y2_rs.change3

## Y3: Likability
# Step 0: Intercept
RQ1B_Y3_step0 <- lm(LikabilityP_av ~ 1, data = d4_lo)
summary(RQ1B_Y3_step0)

# Step 1: Control variables - SONA Sig.
RQ1B_Y3_step1 <- lm(LikabilityP_av ~ Sample_DC1 + Sample_DC2, data = d4_lo)
summary(RQ1B_Y3_step1)

anova(RQ1B_Y3_step0, RQ1B_Y3_step1)
RQ1B_Y3_rs1 <- summary(RQ1B_Y3_step0)$r.squared
RQ1B_Y3_rs2 <- summary(RQ1B_Y3_step1)$r.squared
RQ1B_Y3_rs.change1 <- RQ1B_Y3_rs1-RQ1B_Y3_rs2 # provides R-sqd-change
RQ1B_Y3_rs.change1

# Step 2: IVs of interest

RQ1B_Y3_step2 <- lm(LikabilityP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y3_step2)

anova(RQ1B_Y3_step1, RQ1B_Y3_step2)
RQ1B_Y3_rs3 <- summary(RQ1B_Y3_step1)$r.squared
RQ1B_Y3_rs4 <- summary(RQ1B_Y3_step2)$r.squared
RQ1B_Y3_rs.change2 <-RQ1B_Y3_rs3-RQ1B_Y3_rs4 # provides R-sqd-change
RQ1B_Y3_rs.change2

# Step 3: Interactions

RQ1B_Y3_step3 <- lm(LikabilityP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y3_step3)

anova(RQ1B_Y3_step2, RQ1B_Y3_step3)
RQ1B_Y3_rs5 <- summary(RQ1B_Y3_step2)$r.squared
RQ1B_Y3_rs6 <- summary(RQ1B_Y3_step3)$r.squared
RQ1B_Y3_rs.change3 <- RQ1B_Y3_rs5-RQ1B_Y3_rs6 # provides R-sqd-change
RQ1B_Y3_rs.change3

## Y4: Personality
# Step 0: Intercept
RQ1B_Y4_step0 <- lm(PersolityP_av ~ 1, data = d4_lo)
summary(RQ1B_Y4_step0)

# Step 1: Control variables - SONA Sig.
RQ1B_Y4_step1 <- lm(PersolityP_av ~ Sample_DC1 + Sample_DC2, data = d4_lo)
summary(RQ1B_Y4_step1)

anova(RQ1B_Y4_step0, RQ1B_Y4_step1)
RQ1B_Y4_rs1 <- summary(RQ1B_Y4_step0)$r.squared
RQ1B_Y4_rs2 <- summary(RQ1B_Y4_step1)$r.squared
RQ1B_Y4_rs.change1 <- RQ1B_Y4_rs1-RQ1B_Y4_rs2 # provides R-sqd-change
RQ1B_Y4_rs.change1

# Step 2: IVs of interest

RQ1B_Y4_step2 <- lm(PersolityP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y4_step2)

anova(RQ1B_Y4_step1, RQ1B_Y4_step2)
RQ1B_Y4_rs3 <- summary(RQ1B_Y4_step1)$r.squared
RQ1B_Y4_rs4 <- summary(RQ1B_Y4_step2)$r.squared
RQ1B_Y4_rs.change2 <-RQ1B_Y4_rs3-RQ1B_Y4_rs4 # provides R-sqd-change
RQ1B_Y4_rs.change2

# Step 3: Interactions

RQ1B_Y4_step3 <- lm(PersolityP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y4_step3)

anova(RQ1B_Y4_step2, RQ1B_Y4_step3)
RQ1B_Y4_rs5 <- summary(RQ1B_Y4_step2)$r.squared
RQ1B_Y4_rs6 <- summary(RQ1B_Y4_step3)$r.squared
RQ1B_Y4_rs.change3 <- RQ1B_Y4_rs5-RQ1B_Y4_rs6 # provides R-sqd-change
RQ1B_Y4_rs.change3

## Y5: Empathy
# Step 0: Intercept
RQ1B_Y5_step0 <- lm(EmpathyP_av ~ 1, data = d4_lo)
summary(RQ1B_Y5_step0)

# Step 1: Control variables - SONA Sig.
RQ1B_Y5_step1 <- lm(EmpathyP_av ~ Sample_DC1 + Sample_DC2, data = d4_lo)
summary(RQ1B_Y5_step1)

anova(RQ1B_Y5_step0, RQ1B_Y5_step1)
RQ1B_Y5_rs1 <- summary(RQ1B_Y5_step0)$r.squared
RQ1B_Y5_rs2 <- summary(RQ1B_Y5_step1)$r.squared
RQ1B_Y5_rs.change1 <- RQ1B_Y5_rs1-RQ1B_Y5_rs2 # provides R-sqd-change
RQ1B_Y5_rs.change1

# Step 2: IVs of interest

RQ1B_Y5_step2 <- lm(EmpathyP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y5_step2)

anova(RQ1B_Y5_step1, RQ1B_Y5_step2)
RQ1B_Y5_rs3 <- summary(RQ1B_Y5_step1)$r.squared
RQ1B_Y5_rs4 <- summary(RQ1B_Y5_step2)$r.squared
RQ1B_Y5_rs.change2 <-RQ1B_Y5_rs3-RQ1B_Y5_rs4 # provides R-sqd-change
RQ1B_Y5_rs.change2

# Step 3: Interactions

RQ1B_Y5_step3 <- lm(EmpathyP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y5_step3)

anova(RQ1B_Y5_step2, RQ1B_Y5_step3)
RQ1B_Y5_rs5 <- summary(RQ1B_Y5_step2)$r.squared
RQ1B_Y5_rs6 <- summary(RQ1B_Y5_step3)$r.squared
RQ1B_Y5_rs.change3 <- RQ1B_Y5_rs5-RQ1B_Y5_rs6 # provides R-sqd-change
RQ1B_Y5_rs.change3

## Y6: Distress
# Step 0: Intercept
RQ1B_Y6_step0 <- lm(PDistress ~ 1, data = d4_lo)
summary(RQ1B_Y6_step0)

# Step 1: Control variables - SONA Sig.
RQ1B_Y6_step1 <- lm(PDistress ~ Sample_DC1 + Sample_DC2, data = d4_lo)
summary(RQ1B_Y6_step1)

anova(RQ1B_Y6_step0, RQ1B_Y6_step1)
RQ1B_Y6_rs1 <- summary(RQ1B_Y6_step0)$r.squared
RQ1B_Y6_rs2 <- summary(RQ1B_Y6_step1)$r.squared
RQ1B_Y6_rs.change1 <- RQ1B_Y6_rs1-RQ1B_Y6_rs2 # provides R-sqd-change
RQ1B_Y6_rs.change1

# Step 2: IVs of interest

RQ1B_Y6_step2 <- lm(PDistress ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y6_step2)

anova(RQ1B_Y6_step1, RQ1B_Y6_step2)
RQ1B_Y6_rs3 <- summary(RQ1B_Y6_step1)$r.squared
RQ1B_Y6_rs4 <- summary(RQ1B_Y6_step2)$r.squared
RQ1B_Y6_rs.change2 <-RQ1B_Y6_rs3-RQ1B_Y6_rs4 # provides R-sqd-change
RQ1B_Y6_rs.change2

# Step 3: Interactions

RQ1B_Y6_step3 <- lm(PDistress ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y6_step3)

anova(RQ1B_Y6_step2, RQ1B_Y6_step3)
RQ1B_Y6_rs5 <- summary(RQ1B_Y6_step2)$r.squared
RQ1B_Y6_rs6 <- summary(RQ1B_Y6_step3)$r.squared
RQ1B_Y6_rs.change3 <- RQ1B_Y6_rs5-RQ1B_Y6_rs6 # provides R-sqd-change
RQ1B_Y6_rs.change3

## Y7: Incident - Responsibility
# Step 0: Intercept
RQ1B_Y7_step0 <- lm(resp_scale_1 ~ 1, data = d4_lo)
summary(RQ1B_Y7_step0)

# Step 1: Control variables - SONA Sig.
RQ1B_Y7_step1 <- lm(resp_scale_1 ~ Sample_DC1 + Sample_DC2, data = d4_lo)
summary(RQ1B_Y7_step1)

anova(RQ1B_Y7_step0, RQ1B_Y7_step1)
RQ1B_Y7_rs1 <- summary(RQ1B_Y7_step0)$r.squared
RQ1B_Y7_rs2 <- summary(RQ1B_Y7_step1)$r.squared
RQ1B_Y7_rs.change1 <- RQ1B_Y7_rs1-RQ1B_Y7_rs2 # provides R-sqd-change
RQ1B_Y7_rs.change1

# Step 2: IVs of interest

RQ1B_Y7_step2 <- lm(resp_scale_1 ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y7_step2)

anova(RQ1B_Y7_step1, RQ1B_Y7_step2)
RQ1B_Y7_rs3 <- summary(RQ1B_Y7_step1)$r.squared
RQ1B_Y7_rs4 <- summary(RQ1B_Y7_step2)$r.squared
RQ1B_Y7_rs.change2 <-RQ1B_Y7_rs3-RQ1B_Y7_rs4 # provides R-sqd-change
RQ1B_Y7_rs.change2

# Step 3: Interactions

RQ1B_Y7_step3 <- lm(resp_scale_1 ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y7_step3)

anova(RQ1B_Y7_step2, RQ1B_Y7_step3)
RQ1B_Y7_rs5 <- summary(RQ1B_Y7_step2)$r.squared
RQ1B_Y7_rs6 <- summary(RQ1B_Y7_step3)$r.squared
RQ1B_Y7_rs.change3 <- RQ1B_Y7_rs5-RQ1B_Y7_rs6 # provides R-sqd-change
RQ1B_Y7_rs.change3

## Y8: Incident - Blame
# Step 0: Intercept
RQ1B_Y8_step0 <- lm(resp_scale_2 ~ 1, data = d4_lo)
summary(RQ1B_Y8_step0)

# Step 1: Control variables - SONA Sig.
RQ1B_Y8_step1 <- lm(resp_scale_2 ~ Sample_DC1 + Sample_DC2, data = d4_lo)
summary(RQ1B_Y8_step1)

anova(RQ1B_Y8_step0, RQ1B_Y8_step1)
RQ1B_Y8_rs1 <- summary(RQ1B_Y8_step0)$r.squared
RQ1B_Y8_rs2 <- summary(RQ1B_Y8_step1)$r.squared
RQ1B_Y8_rs.change1 <- RQ1B_Y8_rs1-RQ1B_Y8_rs2 # provides R-sqd-change
RQ1B_Y8_rs.change1

# Step 2: IVs of interest

RQ1B_Y8_step2 <- lm(resp_scale_2 ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + 
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y8_step2)

anova(RQ1B_Y8_step1, RQ1B_Y8_step2)
RQ1B_Y8_rs3 <- summary(RQ1B_Y8_step1)$r.squared
RQ1B_Y8_rs4 <- summary(RQ1B_Y8_step2)$r.squared
RQ1B_Y8_rs.change2 <-RQ1B_Y8_rs3-RQ1B_Y8_rs4 # provides R-sqd-change
RQ1B_Y8_rs.change2

# Step 3: Interactions

RQ1B_Y8_step3 <- lm(resp_scale_2 ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                      Atonement_fact_r, contrasts = my.contrasts, data = d4_lo)
summary(RQ1B_Y8_step3)

anova(RQ1B_Y8_step2, RQ1B_Y8_step3)
RQ1B_Y8_rs5 <- summary(RQ1B_Y8_step2)$r.squared
RQ1B_Y8_rs6 <- summary(RQ1B_Y8_step3)$r.squared
RQ1B_Y8_rs.change3 <- RQ1B_Y8_rs5-RQ1B_Y8_rs6 # provides R-sqd-change
RQ1B_Y8_rs.change3

### EXPLORATORY ANALYSES -------------------------------------------------------
## Y1: Blame
# Step 0: Intercept
RQExp_Y1_step0 <- lm(PResp_av ~ 1, data = d4)
summary(RQExp_Y1_step0)

# Step 1: Control variables
RQExp_Y1_step1 <- lm(PResp_av ~ Sample_DC1 + Sample_DC2, data = d4)
summary(RQExp_Y1_step1)

anova(RQExp_Y1_step0, RQExp_Y1_step1)
RQExp_Y1_rs1 <- summary(RQExp_Y1_step0)$r.squared
RQExp_Y1_rs2 <- summary(RQExp_Y1_step1)$r.squared
RQExp_Y1_rs.change1 <- RQExp_Y1_rs1-RQExp_Y1_rs2 # provides R-sqd-change
RQExp_Y1_rs.change1

# Step 2: IVs of interest

RQExp_Y1_step2 <- lm(PResp_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + SES +
                       Atonement_fact_r, contrasts = my.contrasts, data = d4)
summary(RQExp_Y1_step2)

anova(RQExp_Y1_step1, RQExp_Y1_step2)
RQExp_Y1_rs3 <- summary(RQExp_Y1_step1)$r.squared
RQExp_Y1_rs4 <- summary(RQExp_Y1_step2)$r.squared
RQExp_Y1_rs.change2 <-RQExp_Y1_rs3-RQExp_Y1_rs4 # provides R-sqd-change
RQExp_Y1_rs.change2

# Step 3: Interactions

RQExp_Y1_step3 <- lm(PResp_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*Atonement_fact_r
                     *SES, contrasts = my.contrasts, data = d4)
summary(RQExp_Y1_step3)

anova(RQExp_Y1_step2, RQExp_Y1_step3)
RQExp_Y1_rs5 <- summary(RQExp_Y1_step2)$r.squared
RQExp_Y1_rs6 <- summary(RQExp_Y1_step3)$r.squared
RQExp_Y1_rs.change3 <- RQExp_Y1_rs5-RQExp_Y1_rs6 # provides R-sqd-change
RQExp_Y1_rs.change3

## Y2: Blame
# Step 0: Intercept
RQExp_Y2_step0 <- lm(StigmaP_av ~ 1, data = d4)
summary(RQExp_Y2_step0)

# Step 1: Control variables
RQExp_Y2_step1 <- lm(StigmaP_av ~ Sample_DC1 + Sample_DC2, data = d4)
summary(RQExp_Y2_step1)

anova(RQExp_Y2_step0, RQExp_Y2_step1)
RQExp_Y2_rs1 <- summary(RQExp_Y2_step0)$r.squared
RQExp_Y2_rs2 <- summary(RQExp_Y2_step1)$r.squared
RQExp_Y2_rs.change1 <- RQExp_Y2_rs1-RQExp_Y2_rs2 # provides R-sqd-change
RQExp_Y2_rs.change1

# Step 2: IVs of interest

RQExp_Y2_step2 <- lm(StigmaP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + SES +
                       Atonement_fact_r, contrasts = my.contrasts, data = d4)
summary(RQExp_Y2_step2)

anova(RQExp_Y2_step1, RQExp_Y2_step2)
RQExp_Y2_rs3 <- summary(RQExp_Y2_step1)$r.squared
RQExp_Y2_rs4 <- summary(RQExp_Y2_step2)$r.squared
RQExp_Y2_rs.change2 <-RQExp_Y2_rs3-RQExp_Y2_rs4 # provides R-sqd-change
RQExp_Y2_rs.change2

# Step 3: Interactions

RQExp_Y2_step3 <- lm(StigmaP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*Atonement_fact_r
                     *SES, contrasts = my.contrasts, data = d4)
summary(RQExp_Y2_step3)

anova(RQExp_Y2_step2, RQExp_Y2_step3)
RQExp_Y2_rs5 <- summary(RQExp_Y2_step2)$r.squared
RQExp_Y2_rs6 <- summary(RQExp_Y2_step3)$r.squared
RQExp_Y2_rs.change3 <- RQExp_Y2_rs5-RQExp_Y2_rs6 # provides R-sqd-change
RQExp_Y2_rs.change3

## Y3: Likability
# Step 0: Intercept
RQExp_Y3_step0 <- lm(LikabilityP_av ~ 1, data = d4)
summary(RQExp_Y3_step0)

# Step 1: Control variables
RQExp_Y3_step1 <- lm(LikabilityP_av ~ Sample_DC1 + Sample_DC2, data = d4)
summary(RQExp_Y3_step1)

anova(RQExp_Y3_step0, RQExp_Y3_step1)
RQExp_Y3_rs1 <- summary(RQExp_Y3_step0)$r.squared
RQExp_Y3_rs2 <- summary(RQExp_Y3_step1)$r.squared
RQExp_Y3_rs.change1 <- RQExp_Y3_rs1-RQExp_Y3_rs2 # provides R-sqd-change
RQExp_Y3_rs.change1

# Step 2: IVs of interest

RQExp_Y3_step2 <- lm(LikabilityP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + SES +
                       Atonement_fact_r, contrasts = my.contrasts, data = d4)
summary(RQExp_Y3_step2)

anova(RQExp_Y3_step1, RQExp_Y3_step2)
RQExp_Y3_rs3 <- summary(RQExp_Y3_step1)$r.squared
RQExp_Y3_rs4 <- summary(RQExp_Y3_step2)$r.squared
RQExp_Y3_rs.change2 <-RQExp_Y3_rs3-RQExp_Y3_rs4 # provides R-sqd-change
RQExp_Y3_rs.change2

# Step 3: Interactions

RQExp_Y3_step3 <- lm(LikabilityP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                       Atonement_fact_r*SES, contrasts = my.contrasts, data = d4)
summary(RQExp_Y3_step3)

anova(RQExp_Y3_step2, RQExp_Y3_step3)
RQExp_Y3_rs5 <- summary(RQExp_Y3_step2)$r.squared
RQExp_Y3_rs6 <- summary(RQExp_Y3_step3)$r.squared
RQExp_Y3_rs.change3 <- RQExp_Y3_rs5-RQExp_Y3_rs6 # provides R-sqd-change
RQExp_Y3_rs.change3

## Y4: Personality
# Step 0: Intercept
RQExp_Y4_step0 <- lm(PersolityP_av ~ 1, data = d4)
summary(RQExp_Y4_step0)

# Step 1: Control variables
RQExp_Y4_step1 <- lm(PersolityP_av ~ Sample_DC1 + Sample_DC2, data = d4)
summary(RQExp_Y4_step1)

anova(RQExp_Y4_step0, RQExp_Y4_step1)
RQExp_Y4_rs1 <- summary(RQExp_Y4_step0)$r.squared
RQExp_Y4_rs2 <- summary(RQExp_Y4_step1)$r.squared
RQExp_Y4_rs.change1 <- RQExp_Y4_rs1-RQExp_Y4_rs2 # provides R-sqd-change
RQExp_Y4_rs.change1

# Step 2: IVs of interest

RQExp_Y4_step2 <- lm(PersolityP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + SES +
                       Atonement_fact_r, contrasts = my.contrasts, data = d4)
summary(RQExp_Y4_step2)

anova(RQExp_Y4_step1, RQExp_Y4_step2)
RQExp_Y4_rs3 <- summary(RQExp_Y4_step1)$r.squared
RQExp_Y4_rs4 <- summary(RQExp_Y4_step2)$r.squared
RQExp_Y4_rs.change2 <-RQExp_Y4_rs3-RQExp_Y4_rs4 # provides R-sqd-change
RQExp_Y4_rs.change2

# Step 3: Interactions

RQExp_Y4_step3 <- lm(PersolityP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                       Atonement_fact_r*SES, contrasts = my.contrasts, data = d4)
summary(RQExp_Y4_step3)

anova(RQExp_Y4_step2, RQExp_Y4_step3)
RQExp_Y4_rs5 <- summary(RQExp_Y4_step2)$r.squared
RQExp_Y4_rs6 <- summary(RQExp_Y4_step3)$r.squared
RQExp_Y4_rs.change3 <- RQExp_Y4_rs5-RQExp_Y4_rs6 # provides R-sqd-change
RQExp_Y4_rs.change3

## Y5: Empathy
# Step 0: Intercept
RQExp_Y5_step0 <- lm(EmpathyP_av ~ 1, data = d4)
summary(RQExp_Y5_step0)

# Step 1: Control variables
RQExp_Y5_step1 <- lm(EmpathyP_av ~ Sample_DC1 + Sample_DC2, data = d4)
summary(RQExp_Y5_step1)

anova(RQExp_Y5_step0, RQExp_Y5_step1)
RQExp_Y5_rs1 <- summary(RQExp_Y5_step0)$r.squared
RQExp_Y5_rs2 <- summary(RQExp_Y5_step1)$r.squared
RQExp_Y5_rs.change1 <- RQExp_Y5_rs1-RQExp_Y5_rs2 # provides R-sqd-change
RQExp_Y5_rs.change1

# Step 2: IVs of interest

RQExp_Y5_step2 <- lm(EmpathyP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + SES +
                       Atonement_fact_r, contrasts = my.contrasts, data = d4)
summary(RQExp_Y5_step2)

anova(RQExp_Y5_step1, RQExp_Y5_step2)
RQExp_Y5_rs3 <- summary(RQExp_Y5_step1)$r.squared
RQExp_Y5_rs4 <- summary(RQExp_Y5_step2)$r.squared
RQExp_Y5_rs.change2 <-RQExp_Y5_rs3-RQExp_Y5_rs4 # provides R-sqd-change
RQExp_Y5_rs.change2

# Step 3: Interactions

RQExp_Y5_step3 <- lm(EmpathyP_av ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                       Atonement_fact_r*SES, contrasts = my.contrasts, data = d4)
summary(RQExp_Y5_step3)

anova(RQExp_Y5_step2, RQExp_Y5_step3)
RQExp_Y5_rs5 <- summary(RQExp_Y5_step2)$r.squared
RQExp_Y5_rs6 <- summary(RQExp_Y5_step3)$r.squared
RQExp_Y5_rs.change3 <- RQExp_Y5_rs5-RQExp_Y5_rs6 # provides R-sqd-change
RQExp_Y5_rs.change3

## Y6: Distress
# Step 0: Intercept
RQExp_Y6_step0 <- lm(PDistress ~ 1, data = d4)
summary(RQExp_Y6_step0)

# Step 1: Control variables
RQExp_Y6_step1 <- lm(PDistress ~ Sample_DC1 + Sample_DC2, data = d4)
summary(RQExp_Y6_step1)

anova(RQExp_Y6_step0, RQExp_Y6_step1)
RQExp_Y6_rs1 <- summary(RQExp_Y6_step0)$r.squared
RQExp_Y6_rs2 <- summary(RQExp_Y6_step1)$r.squared
RQExp_Y6_rs.change1 <- RQExp_Y6_rs1-RQExp_Y6_rs2 # provides R-sqd-change
RQExp_Y6_rs.change1

# Step 2: IVs of interest

RQExp_Y6_step2 <- lm(PDistress ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + SES +
                       Atonement_fact_r, contrasts = my.contrasts, data = d4)
summary(RQExp_Y6_step2)

anova(RQExp_Y6_step1, RQExp_Y6_step2)
RQExp_Y6_rs3 <- summary(RQExp_Y6_step1)$r.squared
RQExp_Y6_rs4 <- summary(RQExp_Y6_step2)$r.squared
RQExp_Y6_rs.change2 <-RQExp_Y6_rs3-RQExp_Y6_rs4 # provides R-sqd-change
RQExp_Y6_rs.change2

# Step 3: Interactions

RQExp_Y6_step3 <- lm(PDistress ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                       Atonement_fact_r*SES, contrasts = my.contrasts, data = d4)
summary(RQExp_Y6_step3)

anova(RQExp_Y6_step2, RQExp_Y6_step3)
RQExp_Y6_rs5 <- summary(RQExp_Y6_step2)$r.squared
RQExp_Y6_rs6 <- summary(RQExp_Y6_step3)$r.squared
RQExp_Y6_rs.change3 <- RQExp_Y6_rs5-RQExp_Y6_rs6 # provides R-sqd-change
RQExp_Y6_rs.change3

## Y7: Incident - Responsibility
# Step 0: Intercept
RQExp_Y7_step0 <- lm(resp_scale_1 ~ 1, data = d4)
summary(RQExp_Y7_step0)

# Step 1: Control variables
RQExp_Y7_step1 <- lm(resp_scale_1 ~ Sample_DC1 + Sample_DC2, data = d4)
summary(RQExp_Y7_step1)

anova(RQExp_Y7_step0, RQExp_Y7_step1)
RQExp_Y7_rs1 <- summary(RQExp_Y7_step0)$r.squared
RQExp_Y7_rs2 <- summary(RQExp_Y7_step1)$r.squared
RQExp_Y7_rs.change1 <- RQExp_Y7_rs1-RQExp_Y7_rs2 # provides R-sqd-change
RQExp_Y7_rs.change1

# Step 2: IVs of interest

RQExp_Y7_step2 <- lm(resp_scale_1 ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + SES +
                       Atonement_fact_r, contrasts = my.contrasts, data = d4)
summary(RQExp_Y7_step2)

anova(RQExp_Y7_step1, RQExp_Y7_step2)
RQExp_Y7_rs3 <- summary(RQExp_Y7_step1)$r.squared
RQExp_Y7_rs4 <- summary(RQExp_Y7_step2)$r.squared
RQExp_Y7_rs.change2 <-RQExp_Y7_rs3-RQExp_Y7_rs4 # provides R-sqd-change
RQExp_Y7_rs.change2

# Step 3: Interactions

RQExp_Y7_step3 <- lm(resp_scale_1 ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                       Atonement_fact_r*SES, contrasts = my.contrasts, data = d4)
summary(RQExp_Y7_step3)

anova(RQExp_Y7_step2, RQExp_Y7_step3)
RQExp_Y7_rs5 <- summary(RQExp_Y7_step2)$r.squared
RQExp_Y7_rs6 <- summary(RQExp_Y7_step3)$r.squared
RQExp_Y7_rs.change3 <- RQExp_Y7_rs5-RQExp_Y7_rs6 # provides R-sqd-change
RQExp_Y7_rs.change3

## Y8: Incident - Blame
# Step 0: Intercept
RQExp_Y8_step0 <- lm(resp_scale_2 ~ 1, data = d4)
summary(RQExp_Y8_step0)

# Step 1: Control variables
RQExp_Y8_step1 <- lm(resp_scale_2 ~ Sample_DC1 + Sample_DC2, data = d4)
summary(RQExp_Y8_step1)

anova(RQExp_Y8_step0, RQExp_Y8_step1)
RQExp_Y8_rs1 <- summary(RQExp_Y8_step0)$r.squared
RQExp_Y8_rs2 <- summary(RQExp_Y8_step1)$r.squared
RQExp_Y8_rs.change1 <- RQExp_Y8_rs1-RQExp_Y8_rs2 # provides R-sqd-change
RQExp_Y8_rs.change1

# Step 2: IVs of interest

RQExp_Y8_step2 <- lm(resp_scale_2 ~ Sample_DC1 + Sample_DC2 + ESJ_av_c + SES +
                       Atonement_fact_r, contrasts = my.contrasts, data = d4)
summary(RQExp_Y8_step2)

anova(RQExp_Y8_step1, RQExp_Y8_step2)
RQExp_Y8_rs3 <- summary(RQExp_Y8_step1)$r.squared
RQExp_Y8_rs4 <- summary(RQExp_Y8_step2)$r.squared
RQExp_Y8_rs.change2 <-RQExp_Y8_rs3-RQExp_Y8_rs4 # provides R-sqd-change
RQExp_Y8_rs.change2

# Step 3: Interactions

RQExp_Y8_step3 <- lm(resp_scale_2 ~ Sample_DC1 + Sample_DC2 + ESJ_av_c*
                       Atonement_fact_r*SES, contrasts = my.contrasts, data = d4)
summary(RQExp_Y8_step3)

anova(RQExp_Y8_step2, RQExp_Y8_step3)
RQExp_Y8_rs5 <- summary(RQExp_Y8_step2)$r.squared
RQExp_Y8_rs6 <- summary(RQExp_Y8_step3)$r.squared
RQExp_Y8_rs.change3 <- RQExp_Y8_rs5-RQExp_Y8_rs6 # provides R-sqd-change
RQExp_Y8_rs.change3




##### RQ2 + RQ3 ##### ----------------------------------------------------------

#####################Data Subsets###################
#make a reverse coded SES variable
Prof1$SES10 <- 1 - Prof1$SES
Prof2$SES10 <- 1 - Prof2$SES
sona$SES10 <- 1 - sona$SES
mega$SES10 <- 1 - mega$SES
#make atonement condition a factor
#reverse code atonement so that 2 = low, 1 = med, and 0 = high
Prof1$atone.r <- 2-Prof1$Atonement
Prof1$atone.r <- factor(Prof1$atone.r, 
                        levels = c("0", "1", "2"),
                        labels = c("High", "Med", "Low"))
Prof2$atone.r <- 2-Prof2$Atonement
Prof2$atone.r <- factor(Prof2$atone.r, 
                        levels = c("0", "1", "2"),
                        labels = c("High", "Med", "Low"))
sona$atone.r <- 2-sona$Atonement
sona$atone.r <- factor(sona$atone.r, 
                       levels = c("0", "1", "2"),
                       labels = c("High", "Med", "Low"))
mega$atone.r <- 2-mega$Atonement
mega$atone.r <- factor(mega$atone.r, 
                       levels = c("0", "1", "2"),
                       labels = c("High", "Med", "Low"))
mega$atone.test <- 2 - mega$Atonement
mega$atone.test <- as.factor(mega$atone.test)


#make the sample variable a factor in the mega data set
#mega$sample <- factor(mega$sample, 
#                  levels = c("1", "2", "3"),
#                 labels = c("Prolific1", "Prolific2", "Sona"))


###Dummy coded contrasts
#Comparing Prolific 1 to Prolific 2, Prof1 = -1, sona = 0, Prof2 = 1
mega$sampleD1 <- as.numeric(dplyr::recode(mega$sample, "1" = "-1", "2" = "1", 
                                          "3" = "0"))

#Comparing Sona to Prolific1/Prolific 2, Sona = -2, Prof1 = 1, Prof 2 = 1
mega$sampleD2 <- as.numeric(dplyr::recode(mega$sample, "1" = "1", "2" = "1", 
                                          "3" = "-2"))

#High Status Condition
p1HS <- subset(Prof1, SES == 1)
p2HS <- subset(Prof2, SES == 1)
sHS <- subset(sona, SES == 1)
mHS <- subset(mega, SES ==1)

#Low status Condition
p1LS <- subset(Prof1, SES == 0)
p2LS <- subset(Prof2, SES == 0)
sLS <- subset(sona, SES == 0)
mLS <- subset(mega, SES == 0)




########################################Analysis########################################
#Setting Helmert Contrasts
contr1 <- list(atone.r = contr.helmert)

#we are using the reverse coded atonement to create the contrasts. 
#this will contrast 0 and 1 to 2 (high/med to low) and 0 to 1 (high to med)




###Research Question 2 and 3: Looking at DVs relevant to the Victim####
####Severity of the incidence####

#prolific sample 1, high status
sev_prof1h <- lm(Sev ~ esj.m*atone.r, data = p1HS, contrasts = contr1)
summary(sev_prof1h)

#prolific sample 1, low status
sev_prof1l <- lm(Sev ~ esj.m*atone.r, data = p1LS, contrasts = contr1)
summary(sev_prof1l)

#prolific sample 1 three way interaction
sev_prof1 <- lm(Sev ~ esj.m*atone.r*SES, data = Prof1)
summary(sev_prof1)

#prolific sample 2, high status
sev_prof2h <- lm(Sev ~ esj.m*atone.r, data = p2HS, contrasts = contr1)
summary(sev_prof2h)

#prolific sample 2, low status
sev_prof2l <- lm(Sev ~ esj.m*atone.r, data = p2LS, contrasts = contr1)
summary(sev_prof2l)

#prolific sample 2, three way interaction
sev_prof2 <- lm(Sev ~ esj.m*atone.r*SES, data = Prof2, contrasts = contr1)
summary(sev_prof2)

#sona sample, high status
sev_sonah <- lm(Sev ~ esj.m*atone.r, data = sHS, contrasts = contr1)
summary(sev_sonah)

#sona sample, low status
sev_sonal <- lm(Sev ~ esj.m*atone.r, data = sLS, contrasts = contr1)
summary(sev_sonal)

#sona sample, three way interaction
sev_sona <- lm(Sev ~ esj.m*atone.r*SES, data = sona, contrasts = contr1)
summary(sev_sona)


#Severity of incidence - mega sample - hierarchical regression###

#mega sample, high status,
#step 1 - sample 
m1s1 <- lm(Sev ~ sampleD1 + sampleD2, data = mHS)
summary(m1s1) #sample predicts severity of incident
#step 2 - main effects
m1s2 <- lm(Sev ~ sampleD1 + sampleD2 + esj.m + atone.r, data = mHS, contrasts = contr1)
summary(m1s2)
anova(m1s1, m1s2)
#step 3 - two way interactions
m1s3 <- lm(Sev ~ sampleD1 + sampleD2 +esj.m*atone.r, data = mHS, contrasts = contr1)
summary(m1s3)
anova(m1s2, m1s3) #no stat difference in steps


#mega sample, low status, controlled for sample differences
#step 1 - sample
m2s1 <- lm(Sev ~ sampleD1 + sampleD2, data = mLS)
summary(m2s1) #sample significant predictor
#step 2 - main effects
m2s2 <- lm(Sev ~ sampleD1 + sampleD2 + esj.m + atone.r, data = mLS, contrasts = contr1)
summary(m2s2)
anova(m2s1, m2s2)
#step 3 - two way interactions
m2s3 <- lm(Sev ~ sampleD1 + sampleD2 + esj.m*atone.r, data = mLS, contrasts = contr1)
summary(m2s3)
#plot(m2s3)
anova(m2s2, m2s3)
#plot_model(m2s3)
simple_slopes(m2s3)
ggeffect(m2s3)


#mega sample, three way interactions
#step 1 - sample
m3s1 <- lm(Sev ~ sampleD1 + sampleD2, data = mega)
summary(m3s1)
#Step 2 - main effects
m3s2 <- lm(Sev ~ sampleD1 + sampleD2 + SES + esj.m + atone.r, data = mega, contrasts = contr1)
summary(m3s2)
anova(m3s1, m3s2)
#step 3 - two way interactions
m3s3 <- lm(Sev ~ sampleD1 + sampleD2 + SES*esj.m + SES*atone.r + esj.m*atone.r, data = mega, contrasts = contr1)
summary(m3s3)
anova(m3s2, m3s3)
#step 4 - three way interaction
m3s4 <- lm(Sev ~ sampleD1 + sampleD2 + SES*esj.m*atone.r, data = mega, contrasts = contr1)
summary(m3s4)
anova(m3s3, m3s4)

#step 4 - three-way interaction with SES reverse coded
m3s4r <- lm(Sev ~ sampleD1 + sampleD2 +SES10*esj.m*atone.r, data=mega, contrasts = contr1)
summary(m3s4r)
sev_megal <- lm(Sev ~ esj.m*atone.r + sample, data = mLS, contrasts = contr1)
summary(sev_megal)




#####Victim responsibility####
#prolific sample 1, high status
TIPIv_prof1h <- lm(TIPI_V ~ esj.m*atone.r, data = p1HS, contrasts = contr1)
summary(TIPIv_prof1h)

#Prolific sample 1, low status
TIPIv_prof1l <- lm(TIPI_V ~ esj.m*atone.r, data = p1LS, contrasts = contr1)
summary(TIPIv_prof1l)

#Prolific sample 1, three way interaction
TIPIv_prof1 <- lm(TIPI_V ~ esj.m*atone.r*SES, data = Prof1, contrasts = contr1)
summary(TIPIv_prof1)

#Prolific sample 2, high status
Vresp_prof2h <- lm(Vresp ~ esj.m*atone.r, data = p2HS, contrasts = contr1)
summary(Vresp_prof2h)

#Profilic Sample 2, low status
Vresp_prof2l <- lm(Vresp ~ esj.m*atone.r, data = p2LS, contrasts = contr1)
summary(Vresp_prof2b)

#Prolific Sample 2, three way interaction
Vresp_prof2 <- lm(Vresp ~ esj.m*atone.r*SES, data = Prof2, contrasts = contr1)
summary(Vresp_prof2)

#sona sample, high status
Vresp_sonah <- lm(Vresp ~ esj.m*atone.r, data = sHS, contrasts = contr1)
summary(Vresp_sonah)

#sona sample, low status
Vresp_sonal <- lm(Vresp ~ esj.m*atone.r, data = sLS, contrasts = contr1)
summary(Vresp_sonal)

#sona sample, three way interaction
Vresp_sona <- lm(Vresp ~ esj.m*atone.r*SES, data = sona, contrasts = contr1)
summary(Vresp_sona)

#mega sample - high status - hierarchical regression
#step 1 - sample
Vresp_m1s1 <- lm(Vresp ~ sampleD1 + sampleD2, data = mHS)
summary(Vresp_m1s1)
#step 2 - main effects
Vresp_m1s2 <- lm(Vresp ~ sampleD1 + sampleD2 + esj.m + atone.r, data = mHS, contrasts = contr1)
summary(Vresp_m1s2)
anova(Vresp_m1s1, Vresp_m1s2)
#step 3 - interactions
Vresp_m1s3 <- lm(Vresp ~ sampleD1 + sampleD2 + esj.m*atone.r, data = mHS, contrasts = contr1)
summary(Vresp_m1s3)
anova(Vresp_m1s2, Vresp_m1s3)
#simple Slopes
Vresp_ss <- lm(Vresp ~ esj*Atonement, data = mHS)
simple_slopes(Vresp_ss)


#mega sample - low status - hierarchical regression
#step 1 - sample
Vresp_m2s1 <- lm(Vresp ~ sampleD1 + sampleD2, data = mLS)
summary(Vresp_m2s1)
#step 2 - main effects
Vresp_m2s2 <- lm(Vresp ~ sampleD1 + sampleD2 + esj.m + atone.r, data = mLS, contrasts = contr1)
summary(Vresp_m2s2)
anova(Vresp_m2s1, Vresp_m2s2)
#step 3 - interactions
Vresp_m2s3 <- lm(Vresp ~ sampleD1 + sampleD2 + esj.m*atone.r, data = mLS, contrasts = contr1)
summary(Vresp_m2s3)
anova(Vresp_m2s2, Vresp_m2s3)

#mega sample - three way interaction - hierarchical regressions
#step 1 - sample
Vresp_m3s1 <- lm(Vresp ~ sampleD1 + sampleD2, data = mega)
summary(Vresp_m3s1)
#step 2 - main effects
Vresp_m3s2 <- lm(Vresp ~ sampleD1 + sampleD2 + esj.m + atone.r + SES, data = mega, contrasts = contr1)
summary(Vresp_m3s2)
anova(Vresp_m3s1, Vresp_m3s2)
#step 3 - two way interactions
Vresp_m3s3 <- lm(Vresp ~ sampleD1 + sampleD2 + esj.m*atone.r + esj.m*SES + atone.r*SES, data =mega, contrasts = contr1)
summary(Vresp_m3s3)
anova(Vresp_m3s2, Vresp_m3s3)
#step 4 - three way interaction
Vresp_m3s4 <- lm(Vresp ~ sampleD1 + sampleD2 + esj.m*atone.r*SES, data = mega, contrasts = contr1)
summary(Vresp_m3s4)
anova(Vresp_m3s3, Vresp_m3s4)

Vresp_m3s5 <- lm(Vresp ~ sampleD1 + sampleD2 + esj.m*atone.r*SES10, data = mega, contrasts = contr1)
summary(Vresp_m3s5)


#####TIPI - Vict####
#Prolific sample 1, high status
TIPIv_prof1h <- lm(TIPI_V ~ esj.m*atone.r, data = p1HS, contrasts = contr1)
summary(TIPIv_prof1h)

#Prolific sample 1, low status
TIPIv_prof1l <- lm(TIPI_V ~ esj.m*atone.r, data = p1LS, contrasts = contr1)
summary(TIPIv_prof1l)

#prolific sample 1, three way interaction
TIPIv_prof1 <- lm(TIPI_V ~ esj.m*atone.r*SES, data = Prof1, contrasts = contr1)
summary(TIPIv_prof1)

#Prolific sample 2, high status
TIPIv_prof2h <- lm(TIPI_V ~ esj.m*atone.r, data = p2HS, contrasts = contr1)
summary(TIPIv_prof2h)

#Prolific sample 2, low status
TIPIv_prof2l <- lm(TIPI_V ~ esj.m*atone.r, data = p2LS, contrasts = contr1)
summary(TIPIv_prof2l)

#Prolific sample 2, three way interaction
TIPIv_prof2 <- lm(TIPI_V ~ esj.m*atone.r*SES, data = Prof2, contrasts = contr1)
summary(TIPIv_prof2)

#sona sample, high status
TIPIv_sonah <- lm(TIPI_V ~ esj.m*atone.r, data = sHS, contrasts = contr1)
summary(TIPIv_sonah)

#sona sample, low status
TIPIv_sonal <- lm(TIPI_V ~ esj.m*atone.r, data = sLS, contrasts = contr1)
summary(TIPIv_sonal)

#sona sample, three way interaction
TIPIv_sona <- lm(TIPI_V ~ esj.m*atone.r*SES, data = sona, contrasts = contr1)
summary(TIPIv_sona)

#mega sample - hierarchical regression - high status
#step 1 - sample
TIPIs1 <- lm(TIPI_V ~ sampleD1 + sampleD2, data = mHS)
summary(TIPIs1)
#step 2 - main effects
TIPIs2 <- lm(TIPI_V ~ sampleD1 + sampleD2 + atone.r + esj.m, data = mHS, contrasts = contr1)
summary(TIPIs2)
anova(TIPIs1, TIPIs2)
#step 3 - two way interactions
TIPIs3 <- lm(TIPI_V ~ sampleD1 + sampleD2 + atone.r*esj.m, data = mHS, contrasts = contr1)
summary(TIPIs3)
anova(TIPIs2, TIPIs3)


#mega sample - hierarchical regression - low status
#step 1 - sample
TIPI2s1 <- lm(TIPI_V ~ sampleD1 + sampleD2, data = mLS)
summary(TIPI2s1)
#step 2 - main effects
TIPI2s2 <- lm(TIPI_V ~ sampleD1 + sampleD2 + esj.m + atone.r, data = mLS, contrasts= contr1)
summary(TIPI2s2)
anova(TIPI2s1, TIPI2s2)
#step 3 - two way interactions
TIPI2s3 <- lm(TIPI_V ~ sampleD1 + sampleD2 + esj.m*atone.r, data = mLS, contrasts = contr1)
summary(TIPI2s3)
anova(TIPI2s2, TIPI2s3)
TIPIss <- lm(TIPI_V ~ esj*Atonement, data = mLS)
simple_slopes(TIPIss)

#mega sample - hierarchical regression - three way interaction
#step 1 - sample
TIPI3s1 <- lm(TIPI_V ~ sampleD1 + sampleD2, data = mega)
summary(TIPI3s1)
#step 2 - main effects
TIPI3s2 <- lm(TIPI_V ~ sampleD1 + sampleD2 + atone.r + esj.m + SES, data = mega, contrasts = contr1)
summary(TIPI3s2)
anova(TIPI3s1, TIPI3s2)
#step 3 - two way interactions
TIPI3s3 <- lm(TIPI_V ~ sampleD1 + sampleD2 + atone.r*esj.m + atone.r*SES + esj.m*SES, data = mega, contrasts = contr1)
summary(TIPI3s3)
anova(TIPI3s2, TIPI3s3)
#step 4 - three way interaction
TIPI3s4 <- lm(TIPI_V ~ sampleD1 + sampleD2 + atone.r*esj.m*SES, data = mega, contrasts = contr1)
summary(TIPI3s4)
anova(TIPI3s3, TIPI3s4)

TIPI3s5 <- lm(TIPI_V ~ sampleD1 + sampleD2 + atone.r*esj.m*SES10, data = mega, contrasts = contr1)
summary(TIPI3s5)




######Likeability of Vic######
#Prolific sample 1, high status
LikeV_prof1h <- lm(Like_V ~ esj.m*atone.r, data = p1HS, contrasts = contr1)
summary(LikeV_prof1h)

#Prolific sample 1, low status
LikeV_prof1l <- lm(Like_V ~ esj.m*atone.r, data = p1LS, contrasts = contr1)
Summary(LikeV_prof1l)

#Prolific sample 1, three way interaction
LikeV_prof1 <- lm(Like_V ~ esj.m*atone.r*SES, data = Prof1, contrasts = contr1)
summary(LikeV_prof1)

#Prolific Sample 2, high status
LikeV_prof2h <- lm(Like_V ~ esj.m*atone.r, data = p2HS, contrasts = contr1)
summary(LikeV_prof2h)

#Prolific Sample 2, low status
LikeV_prof2l <- lm(Like_V ~ esj.m*atone.r, data = p2LS, contrasts = contr1)
summary(LikeV_prof2l)

#Prolific sample 2, three way interaction
LikeV_prof2 <- lm(Like_V ~ esj.m*atone.r*SES, data = Prof2, contrasts = contr1)
summary(LikeV_prof2)

#sona sample, high status
LikeV_sonah <- lm(Like_V ~ esj.m*atone.r, data = sHS, contrasts = contr1)
summary(LikeV_sonah)

#sona sample, low status
LikeV_sonal <- lm(Like_V ~ esj.m*atone.r, data = sLS, contrasts = contr1)
summary(LikeV_sonal)

#sona sample, three way interaction
LikeV_sona <- lm(Like_V ~ esj.m*atone.r*SES, data = sona, contrasts = contr1)
summary(LikeV_sona)

#mega sample - hierarchical regression - high status
#step 1 - sample
Like1s1 <- lm(Like_V ~ sampleD1 + sampleD2, data = mHS)
summary(Like1s1)
#step 2 - main effect
Like1s2 <- lm(Like_V ~ sampleD1 + sampleD2 + atone.r + esj.m, data = mHS, contrasts = contr1)
summary(Like1s2)
anova(Like1s1, Like1s2)
#step 3 - two way interactions
Like1s3 <- lm(Like_V ~ sampleD1 + sampleD2 + atone.r*esj.m, data = mHS, contrasts = contr1)
summary(Like1s3)
anova(Like1s2, Like1s3)


#mega sample - hierarchical regression - low status
#step 1
Like2s1 <- lm(Like_V ~ sampleD1 + sampleD2, data = mLS)
summary(Like2s1)
#step 2 - main effects
Like2s2 <- lm(Like_V ~ sampleD1 + sampleD2 + atone.r + esj.m, data = mLS, contrasts = contr1)
summary(Like2s2)
anova(Like2s1, Like2s2)
#step 3 - two way interactions
Like2s3 <- lm(Like_V ~ sampleD1 + sampleD2 + atone.r*esj.m, data = mLS, contrasts = contr1)
summary(Like2s3)
anova(Like2s2, Like2s3)


#mega sample - hierarchical regression - three way interaction
#step 1 - sample
Like3s1 <- lm(Like_V ~ sampleD1 + sampleD2, data = mega)
summary(Like3s1)
#step 2 - main effects
Like3s2 <- lm(Like_V ~ sampleD1 + sampleD2 + atone.r + esj.m + SES, data = mega, contrasts = contr1)
summary(Like3s2)
anova(Like3s1, Like3s2)
#step 3 - two way interactions
Like3s3 <- lm(Like_V ~ sampleD1 + sampleD2 + atone.r*esj.m + atone.r*SES + esj.m*SES, data = mega, contrasts = contr1)
summary(Like3s3)
anova(Like3s2, Like3s3)
#step 4 - three way interactions
Like3s4 <- lm(Like_V ~ sampleD1 + sampleD2 + atone.r*esj.m*SES, data = mega, contrasts = contr1)
summary(Like3s4)
anova(Like3s3, Like3s4)
Like3s5 <- lm(Like_V ~ sampleD1 + sampleD2 + atone.r*esj.m*SES10, data = mega, contrasts = contr1)
summary(Like3s5)


##########SA - Victim########
#Prolific sample 1, high status
SAV_prof1h <- lm(SA_V.rc ~ esj.m*atone.r, data = p1HS, contrasts = contr1)
summary(SAV_prof1h)

#Prolific sample 1, low status
SAV_prof1l <- lm(SA_V.rc ~ esj.m*atone.r, data = p1LS, contrasts = contr1)
summary(SAV_prof1l)

#Prolific sample 1, three way interaction
SAV_prof1 <- lm(SA_V.rc ~ esj.m*atone.r*SES, data = Prof1, contrasts = contr1)
summary(SAV_prof1)

#Prolific Sample 2, high status
SAV_prof2h <- lm(SA_V.rc ~ esj.m*atone.r, data = p2HS, contrasts= contr1)
summary(SAV_prof2h)

#Prolific sample 2, low status
SAV_prof2l <- lm(SA_V.rc ~ esj.m*atone.r, data = p2LS, contrasts= contr1)
summary(SAV_prof2l)

#Prolific sample 2, three way interaction
SAV_prof2 <- lm(SA_V.rc ~ esj.m*atone.r*SES, data = Prof2, contrasts = contr1)
summary(SAV_prof2)

#sona sample, high status
SAV_sonah <- lm(SA_V.rc ~ esj.m*atone.r, data = sHS, contrasts = contr1)
summary(SAV_sonah)

#sona sample, low status
SAV_sonal <- lm(SA_V.rc ~ esj.m*atone.r, data = sLS, contrasts = contr1)
summary(SAV_sonal)

#sona sample, three way interaction
SAV_sona <- lm(SA_V.rc ~ esj.m*atone.r*SES, data = sona, contrasts = contr1)
summary(SAV_sona)

#mega - hierarchical regression - high status
#step 1 - sample
SA1s1 <- lm(SA_V.rc ~ sampleD1 + sampleD2, data = mHS)
summary(SA1s1)
#step 2 - main effects
SA1s2 <- lm(SA_V.rc ~ sampleD1 + sampleD2 + esj.m + atone.r, data = mHS, contrasts = contr1)
summary(SA1s2)
anova(SA1s1, SA1s2)
#step 3 - interactions
SA1s3 <- lm(SA_V.rc ~ sampleD1 + sampleD2 + esj.m*atone.r, data = mHS, contrasts = contr1)
summary(SA1s3)
anova(SA1s2, SA1s3)

#mega - hierarchical regression - low status
#step - sample
SA2s1 <- lm(SA_V.rc ~ sampleD1 + sampleD2, data = mLS)
summary(SA2s1)
#step 2 - main effects
SA2s2 <- lm(SA_V.rc ~ sampleD1 + sampleD2 + esj.m + atone.r, data = mLS, contrasts = contr1)
summary(SA2s2)
anova(SA2s1, SA2s2)
#step 3 - two way interactions
SA2s3 <- lm(SA_V.rc ~ sampleD1 + sampleD2 + esj.m*atone.r, data= mLS, contrasts = contr1)
summary(SA2s3)
anova(SA2s2, SA2s3)

#mega - hierarchical regression - three way interaction
#step 1 - sample
SA3s1 <- lm(SA_V.rc ~ sampleD1 + sampleD2, data = mega)
summary(SA3s1)
#step 2 - main effects
SA3s2 <- lm(SA_V.rc ~ sampleD1 + sampleD2 + esj.m + atone.r + SES, data = mega, contrasts = contr1)
summary(SA3s2)
anova(SA3s1, SA3s2)
#step 3 - two way interactions
SA3s3 <- lm(SA_V.rc ~ sampleD1 + sampleD2 + esj.m*atone.r + esj.m*SES + atone.r*SES, data = mega, contrasts = contr1)
summary(SA3s3)
anova(SA3s2, SA3s3)
#step 4 - three way interactions
SA3s4 <- lm(SA_V.rc ~ sampleD1 + sampleD2 + esj.m*atone.r*SES, data = mega, contrasts = contr1)
summary(SA3s4)
anova(SA3s3, SA3s4)
SA3s5 <- lm(SA_V.rc ~ sampleD1 + sampleD2 + esj.m*atone.r*SES10, data = mega, contrasts = contr1)
summary(SA3s5)

########Empathy for victim#######
#Prolific sample 1, high status
EmpV_prof1h <- lm(Emp_V ~ esj.m*atone.r, data = p1HS, contrasts = contr1)
summary(EmpV_prof1h)

#Prolific sample 1, low status
EmpV_prof1l <- lm(Emp_V ~ esj.m*atone.r, data = p1LS, contrasts = contr1)
summary(EmpV_prof1l)

#Prolific sample 1, three way interaction
EmpV_prof1 <- lm(Emp_V ~ esj.m*atone.r*SES, data = Prof1, contrasts = contr1)
summary(EmpV_prof1)

#Prolific Sample 2, high status
EmpV_prof2h <- lm(Emp_V ~ esj.m*atone.r, data = p2HS, contrasts = contr1)
summary(EmpV_prof2h)

#Prolific Sample 2, low status
EmpV_prof2l <- lm(Emp_V ~ esj.m*atone.r, data = p2LS, contrasts = contr1)
summary(EmpV_prof2l)

#Prolific Sample 2, three way interaction
EmpV_prof2 <- lm(Emp_V ~ esj.m*atone.r*SES, data = Prof2, contrasts = contr1)
summary(EmpV_prof2)

#sona sample, high status
EmpV_sonah <- lm(Emp_V ~ esj.m*atone.r, data = sHS, contrasts = contr1)
summary(EmpV_sonah)

#sona sample, low status
EmpV_sonal <- lm(Emp_V ~ esj.m*atone.r, data= sLS, contrasts = contr1)
summary(EmpV_sonal)

#sona sample, three way interaction
EmpV_sona <- lm(Emp_V ~ esj.m*atone.r, data = sona, contrasts = contr1)
summary(EmpV_sona)

#mega sample - hierarchical regression - high status
#step 1 - sample
Emp1s1 <- lm(Emp_V ~ sampleD1 + sampleD2, data = mHS)
summary(Emp1s1)
#step 2 - main effects
Emp1s2 <- lm(Emp_V ~ sampleD1 + sampleD2 + esj.m + atone.r, data = mHS, contrasts = contr1)
summary(Emp1s2)
anova(Emp1s1, Emp1s2)
#step 3 - two way interactions
Emp1s3 <- lm(Emp_V ~ sampleD1 + sampleD2 + esj.m*atone.r, data = mHS, contrasts = contr1)
summary(Emp1s3)
anova(Emp1s2, Emp1s3)



#mega sample- hierarchical regression =- low status
#step 1 - sample
Emp2s1 <- lm(Emp_V ~ sampleD1 + sampleD2, data = mLS)
summary(Emp2s1)
#step 2 - main effects
Emp2s2 <- lm(Emp_V ~ sampleD1 + sampleD2 + esj.m + atone.r, data = mLS, contrasts = contr1)
summary(Emp2s2)
anova(Emp2s1, Emp2s2)
#step 3 - two way interactions
Emp2s3 <- lm(Emp_V ~ sampleD1 + sampleD2 + esj.m*atone.r, data = mLS, contrasts = contr1)
summary(Emp2s3)
anova(Emp2s2, Emp2s3)

#mega sample - hierarchical regression - three way interaction
#step 1 - sample
Emp3s1 <- lm(Emp_V ~ sampleD1 + sampleD2, data = mega)
summary(Emp3s1)
#step 2 - main effects
Emp3s2 <- lm(Emp_V ~ sampleD1 + sampleD2 + esj.m + atone.r + SES, data = mega, contrasts = contr1)
summary(Emp3s2)
anova(Emp3s1, Emp3s2)
#step 3 - two way interactions
Emp3s3 <- lm(Emp_V ~ sampleD1 + sampleD2 + esj.m*atone.r + esj.m*SES + atone.r*SES, data = mega, contrasts = contr1)
summary(Emp3s3)
anova(Emp3s2, Emp3s3)
#step 4 - three way interaction
Emp3s4 <- lm(Emp_V ~ sampleD1 + sampleD2 + esj.m*atone.r*SES, data = mega, contrasts = contr1)
summary(Emp3s4)
anova(Emp3s3, Emp3s4)

Emp3s5 <- lm(Emp_V ~ sampleD1 + sampleD2 + esj.m*atone.r*SES10, data = mega, contrasts = contr1)
summary(Emp3s5)

######Distress for the Victim######
#Prolific sample 1, high status
distV_prof1h <- lm(distV ~ esj.m*atone.r, data = p1HS, contrasts = contr1)
summary(distV_prof1h)

#Prolific sample 1, low status
distV_prof1l <- lm(distV ~ esj.m*atone.r, data = p1LS, contrasts = contr1)
summary(distV_prof1l)

#Prolific sample 1, three way interactions
distV_prof1 <- lm(distV ~ esj.m*atone.r*SES, data = Prof1, contrasts = contr1)
summary(distV_prof1)

#Prolific Sample 2, high status
distV_prof2h <- lm(distV ~ esj.m*atone.r, data = p2HS, contrasts = contr1)
summary(distV_prof2h)

#Prolific Sample 2, low status
distV_prof2l <- lm(distV ~ esj.m*atone.r, data = p2LS, contrasts = contr1)
summary(distV_prof2l)

#Proflific Sample 2, three way interaction
distV_prof2 <- lm(distV ~ esj.m*atone.r*SES, data = Prof2, contrasts = contr1)
summary(distV_prof2)

#sona sample, high status
distV_sonah <- lm(distV ~ esj.m*atone.r, data = sHS, contrasts = contr1)
summary(distV_sonah)

#sona sample, low status
distV_sonal <- lm(distV ~ esj.m*atone.r, data = sLS, contrasts = contr1)
summary(distV_sonal)

#sona sample, three way interaction
distV_sona <- lm(distV ~ esj.m*atone.r*SES, data = sona, contrasts = contr1)
summary(distV_sona)

#mega sample - hierarchical regression - high status
#step 1 - sample
distV1s1 <- lm(distV ~ sampleD1 + sampleD2, data = mHS)
summary(distV1s1)
#step 2 - main effects
distV1s2 <- lm(distV ~ sampleD1 + sampleD2 + esj.m + atone.r, data = mHS, contrasts = contr1)
summary(distV1s2)
anova(distV1s1, distV1s2)
#step 3 - two way interaction
distV1s3 <- lm(distV ~ sampleD1 + sampleD2 + esj.m*atone.r, data = mHS, contrasts = contr1)
summary(distV1s3)
anova(distV1s2, distV1s3)


#mega sample - hierarchical regression - low status
#step 1 - sample
distV2s1 <- lm(distV ~ sampleD1 + sampleD2, data = mLS)
summary(distV2s1)
#step 2 - main effects
distV2s2 <- lm(distV ~ sampleD1 + sampleD2 + esj.m + atone.r, data = mLS, contrasts = contr1)
summary(distV2s2)
anova(distV2s1, distV2s2)
#step 3 - two way interactions
distV2s3 <- lm(distV ~ sampleD1 + sampleD2 + esj.m*atone.r, data = mLS, contrasts = contr1)
summary(distV2s3)
anova(distV2s2, distV2s3)

#mega sample - three way interaction
#step 1 - sample
distV3s1 <- lm(distV ~ sampleD1 + sampleD2, data = mega)
summary(distV3s1)
#step 2 - main effects
distV3s2 <- lm(distV ~ sampleD1 + sampleD2 + esj.m + atone.r + SES, data = mega, contrasts = contr1)
summary(distV3s2)
anova(distV3s1, distV3s2)
#step 3 - two way interactions
distV3s3 <- lm(distV ~ sampleD1 + sampleD2 + esj.m*atone.r + esj.m*SES + atone.r*SES, data = mega, contrasts = contr1)
summary(distV3s3)
anova(distV3s2, distV3s3)
#step 4 - three way interactions
distV3s4 <- lm(distV ~ sampleD1 + sampleD2 + esj.m*atone.r*SES, data = mega, contrasts = contr1)
summary(distV3s4)
anova(distV3s3, distV3s4)

distV3s5 <- lm(distV ~ sampleD1 + sampleD2 + esj.m*atone.r*SES10, data = mega, contrasts = contr1)
summary(distV3s5)
plot(mHS$Like_V ~ mHS$atone.r)
