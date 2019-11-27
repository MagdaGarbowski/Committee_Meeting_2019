# Committee Meeting - Dec 2019 

library(rstan)
library(plyr)
library(data.table)
library(ggplot2)
library(gridExtra)
library(brms)
library(tidybayes)
library(ggridges)
library(ggmcmc)

setwd("/Users/MagdaGarbowski/Traits_2017_Grasses/Fall 2019/CommitteeMeeting")

# Data - from "Traits_CommitteeMeeting_2019_DataMgmt.R"

SpeciesData<-read.csv("TraitData_2017.csv") 
SpeciesData$H_num<-as.factor(SpeciesData$H_num)
Species_splits = split(SpeciesData, paste(SpeciesData$SPECIES))

# Functions 
# To get posteriors
get_posteriors<-function(models) {
  posterior_samples(models, pars = "^b")
}

# Wide to long format 
posteriors_long <- function (posteriors) {
  melt(setDT(posteriors), id.vars = c(), variable.name = "Harvest")
}

# Plot density functions 
density_plot_function<-function(data, lims){
  ggplot(data, aes(x = value, y = Harvest)) + 
    scale_x_continuous(limits = lims)+
    scale_y_discrete("Days Old", 
                     labels = c("b_H_num1" = "10", "b_H_num2" = "24", "b_H_num3" = "42", "b_H_num4" = "84"))+
    geom_density_ridges(scale = .9, bandwidth = 14, rel_min_height = 0.01,
                        fill = "grey75", colour = "black") 
}

# Specific root length model 
SRL_mod = function(df)
{   brm(
    SRL ~ 0 + H_num + (1|POP_ID),
    data = df,
    family = gaussian(),
    warmup = 3000, iter = 10000, chains = 4,  
    control = list(adapt_delta = 0.99), 
    save_all_pars = TRUE, cores= 4)
}


# Specific leaf area model 
SLA_mod = function(df)
{   brm(
  SLA ~ 0 + H_num + (1|POP_ID),
  data = df,
  family = gaussian(),
  warmup = 3000, iter = 10000, chains = 4,  
  control = list(adapt_delta = 0.99), 
  save_all_pars = TRUE, cores= 4)
}

# Root mass ratio model - needs work - probably needs a different distribution? 
RMR_mod = function(df)
{   brm(
  RMR ~ 0 + H_num + (1|POP_ID),
  data = df,
  family = lognormal(),
  warmup = 3000, iter = 10000, chains = 4,  
  control = list(adapt_delta = 0.99), 
  save_all_pars = TRUE, cores= 4)
}

# RALAR - needs work - probably needs a different distribution? 
RALAR_mod = function(df)
{   brm(
  RALAR ~ 0 + H_num + (1|POP_ID),
  data = df,
  family = Beta(),
  warmup = 3000, iter = 10000, chains = 4,  
  control = list(adapt_delta = 0.99), 
  save_all_pars = TRUE, cores= 4)
}



# Get values 
SRL_mods<-lapply(Species_splits, SRL_mod)
SRL_posteriors <- lapply(SRL_mods, get_posteriors) 
SRL_posteriors_long <- lapply(SRL_posteriors, posteriors_long)

# Get values 
SLA_mods<-lapply(Species_splits, SLA_mod)
SLA_posteriors <- lapply(SLA_mods, get_posteriors) 
SLA_posteriors_long <- lapply(SLA_posteriors, posteriors_long)

# Get values 
RMR_mods<-lapply(Species_splits, RMR_mod)
RMR_posteriors <- lapply(RMR_mods, get_posteriors) 
RMR_posteriors_long <- lapply(RMR_posteriors, posteriors_long)



#
#
#
#
#
#
#
#



# PLOTS 
# Specific root length 
SRL_plots<-lapply(SRL_posteriors_long, density_plot_function, lims = c(-500, 1500))
grid.arrange(SRL_plots$VUOC, SRL_plots$PLPA, SRL_plots$ELTR, SRL_plots$PAMU, SRL_plots$HECO, SRL_plots$HEVI,
              ncol = 2)

# Specific leaf area 
SLA_plots<-lapply(SLA_posteriors_long, density_plot_function, lims = c(-15, 1500))
grid.arrange(SLA_plots$VUOC, SLA_plots$PLPA, SLA_plots$ELTR, SLA_plots$PAMU, SLA_plots$HECO, SLA_plots$HEVI,
             ncol = 2)

# Root Mass Ratio 
RMR_plots<-lapply(RMR_posteriors_long, density_plot_function, lims = c(-50, 50))
grid.arrange(RMR_plots$VUOC, RMR_plots$PLPA, RMR_plots$ELTR, RMR_plots$PAMU, RMR_plots$HECO, RMR_plots$HEVI,
             ncol = 2)
