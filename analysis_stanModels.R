library(tidyverse)
library(dplyr)
library(arm)
library(modEvA)
library(fastDummies)
library(rstan)
rstan_options(auto_write = TRUE) 
options(mc.cores = parallel::detectCores())
source('code/regression.r')
select <- dplyr::select

load('psdat.RData')
colnames(psdat)
table(psdat$Z,
      psdat$rdm_condition)

# U --> Model predicting Bottom Outers
# Y --> Model predicting Bottom Outcomes

### remove classes that don't have representation across both scripts
classes_by_z <- data.frame(table(psdat$class, psdat$Z)) %>%
  spread(Var2, Freq) %>%
  rename(class = Var1) %>%
  filter(
    `0` > 0 &
    `1` > 0) %>%
  select(-`1`,
         -`0`)

psdat <- psdat %>%
  inner_join(
    classes_by_z
    
  )

length(unique(psdat$class))

table(psdat$raceIMP, psdat$Z)


#### Dummy Codes ####
#  dummy codes for race
race_dummy <- fastDummies::dummy_cols(psdat$raceIMP, 
                                      remove_first_dummy = T) %>%
  distinct() %>%
  rename(raceIMP = ".data") 
names(dummy)[-1]<-sub(".data*", "", names(dummy)[-1])


#  dummy codes for class
class_dummy<-fastDummies::dummy_cols(psdat$class, 
                                     remove_first_dummy = T) %>%
  distinct() %>%
  rename(class = ".data") 
names(dummy)[-1]<-sub(".data*", "", names(dummy)[-1])



## Covariates matrixes #####
# Control Cov for U model
XctlU <- psdat %>%
  filter(Z == 0) %>%
  select(
    pretestIMP,
    Scale.Score5IMP,
    MALEIMP,
    raceIMP,
    virtualIMP,
    EIPIMP,
    IEPIMP,
    ESOLIMP,
    GIFTEDIMP,
    pre.avg_time_on_tasksIMP,
    pre_MA_total_scoreIMP,
    pre_negative_reaction_scoreIMP,
    pre_numerical_confindence_scoreIMP
  ) %>%
  left_join(
    race_dummy,
    by = "raceIMP"
  ) %>%
  select(-raceIMP)


# Control Cov for Y model
XctlY <- psdat %>%
  filter(Z == 0) %>%
  select(
    pretestIMP,
    Scale.Score5IMP,
    MALEIMP,
    raceIMP,
   # virtualIMP,
    EIPIMP,
    IEPIMP,
    ESOLIMP,
    GIFTEDIMP,
    pre.avg_time_on_tasksIMP,
    pre_MA_total_scoreIMP,
    pre_negative_reaction_scoreIMP,
    pre_numerical_confindence_scoreIMP,
    class
  ) %>%
  left_join(
    class_dummy,
    by = "class"
  ) %>%
  left_join(
    race_dummy,
    by = "raceIMP"
  ) %>%
  select(-raceIMP, -class)


# Treatment Cov for U model
XtrtU <- psdat %>%
  filter(Z == 1) %>%
  select(
    pretestIMP,
    Scale.Score5IMP,
    MALEIMP,
    raceIMP,
    virtualIMP,
    EIPIMP,
    IEPIMP,
    ESOLIMP,
    GIFTEDIMP,
    pre.avg_time_on_tasksIMP,
    pre_MA_total_scoreIMP,
    pre_negative_reaction_scoreIMP,
    pre_numerical_confindence_scoreIMP
  ) %>%
  left_join(
    race_dummy,
    by = "raceIMP"
  ) %>%
  select(-raceIMP)


# Treatment Cov for Y model
XtrtY <- psdat %>%
  filter(Z == 1) %>%
  select(
    pretestIMP,
    Scale.Score5IMP,
    MALEIMP,
    raceIMP,
    # virtualIMP,
    EIPIMP,
    IEPIMP,
    ESOLIMP,
    GIFTEDIMP,
    pre.avg_time_on_tasksIMP,
    pre_MA_total_scoreIMP,
    pre_negative_reaction_scoreIMP,
    pre_numerical_confindence_scoreIMP,
    class
  ) %>%
  left_join(
    class_dummy,
    by = "class"
  ) %>%
  left_join(
    race_dummy,
    by = "raceIMP"
  ) %>%
  select(-raceIMP, -class)







stanDat <-  list(nc= length(psdat[psdat$Z == 0,]$student_number), # 
                    nt= length(psdat[psdat$Z == 1,]$student_number), # 
                    ncovU= ncol(XctlU), # √
                    ncovY= ncol(XctlY), # √
                    
                    YctlY=  (psdat[psdat$Z == 0, ]$Y), ### IS THE OUTCOME Y OR post.total_math_score?
                #    YctlU= , this doesn't exist, right?
                    
                    YtrtY= (psdat[psdat$Z == 1, ]$Y ), ### IS THE OUTCOME Y OR post.total_math_score?
                    YtrtU=(ifelse(psdat[psdat$Z == 1, ]$anyBottom == "TRUE", 1, 0)),
                
                    XctlU=as.matrix(XctlU), # √
                    XctlY=as.matrix(XctlY), # √
                    
                    XtrtU=as.matrix(XtrtU), # √
                    XtrtY=as.matrix(XtrtY), # √
                
                    bottomOuter=(ifelse(psdat[psdat$Z == 1, ]$anyBottom == "TRUE", 1, 0))
                      )


mod <- stan('psMod.stan',data=stanDat)
warning(mod)

print(mod, pars=c('alphaTBO','alphaTNBO','alphaCBO','alphaCNBO',
                  'bottomOuterATE','notbottomOuterATE','ATEdiff'), probs=c(0.025,0.975))

