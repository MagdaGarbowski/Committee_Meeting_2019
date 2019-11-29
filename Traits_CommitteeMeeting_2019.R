# Committee Meeting - Dec 2019 
# November 27, 2019 
# 2017 Trait data - Traits through time 

setwd("/Users/MagdaGarbowski/CommitteeMeeting") 

library(rstan)
library(plyr)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(brms)
library(tidybayes)
library(ggridges)
library(ggmcmc)

# Data - from "Traits_CommitteeMeeting_2019_DataMgmt.R"

SpeciesData<-read.csv("TraitData_2017.csv") 
# Make RMR and RALAR 0-100 not 0-1 because model sucks
SpeciesData$RMR_100<-SpeciesData$RMR * 100 
SpeciesData$RALAR_100<-SpeciesData$RALAR * 100

SpeciesData$H_num<-as.factor(SpeciesData$H_num)
Species_splits = split(SpeciesData, paste(SpeciesData$SPECIES))

# Functions 
# To get posteriors and get them into long format
get_posteriors<-function(models) {
  tmp1<-posterior_samples(models, pars = "^b")
  tmp2<-melt(setDT(tmp1), id.vars = c(), variable.name = "Harvest")
  print(tmp2)
}

# Wide to long format 
posteriors_long <- function (posteriors) {
  melt(setDT(posteriors), id.vars = c(), variable.name = "Harvest")
}

# Plot density functions 
density_plot_function<-function(data, lims, annotate_text, x, y){
  ggplot(data, aes(x = value, y = Harvest)) + 
    scale_x_continuous(limits = lims)+
    scale_y_discrete("", 
                     labels = c("b_H_num1" = "10", "b_H_num2" = "24", "b_H_num3" = "42", "b_H_num4" = "84"))+
    geom_density_ridges(scale = .9, bandwidth = 14, rel_min_height = 0.01,
                        fill = "grey75", colour = "black")  + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())+
    annotate(geom = "text", label = annotate_text, x, y )
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
  RMR_100 ~ 0 + H_num + (1|POP_ID),
  data = df,
  family = gaussian(),
  warmup = 3000, iter = 10000, chains = 4,  
  control = list(adapt_delta = 0.99), 
  save_all_pars = TRUE, cores= 4)
}

# RALAR - needs work - probably needs a different distribution? 
RALAR_mod = function(df)
{   brm(
  RALAR_100 ~ 0 + H_num + (1|POP_ID),
  data = df,
  family = gaussian(),
  warmup = 3000, iter = 10000, chains = 4,  
  control = list(adapt_delta = 0.99), 
  save_all_pars = TRUE, cores= 4)
}

# GET VALUES 
# SRL_mods<-lapply(Species_splits, SRL_mod)
SRL_posteriors_long <- lapply(SRL_mods, get_posteriors) 

# SLA_mods<-lapply(Species_splits, SLA_mod)
SLA_posteriors_long <- lapply(SLA_mods, get_posteriors) 

# RMR_mods<-lapply(Species_splits, RMR_mod)
RMR_posteriors_long <- lapply(RMR_mods, get_posteriors) 

#RALAR_mods<-lapply(Species_splits, RALAR_mod)
RALAR_posteriors_long <- lapply(RALAR_mods, get_posteriors) 

# PLOTS 
# Specific root length 
SRL_plots<-lapply(SRL_posteriors_long, density_plot_function, 
                  lims = c(-250, 1000),
                  annotate_text = "SRL", x = 825, y = 4)

# Specific leaf area 
SLA_plots<-lapply(SLA_posteriors_long, density_plot_function, 
                  lims = c(-15, 1500),
                  annotate_text = "SLA", x = 1250, y = 3.5)

# Root Mass Ratio 
RMR_plots<-lapply(RMR_posteriors_long, density_plot_function, 
                  lims = c(-25, 100),
                  annotate_text = "RMR", x = -10, y = 1.5)

# Root area to leaf area ratio 
RALAR_plots<-lapply(RALAR_posteriors_long, density_plot_function, 
                    lims = c(-25, 120),
                    annotate_text = "RL Area", x = 95, y = 3.5)

#
#
#
# SPECIES COMPARISON GRAPHS 
# Plot all species by harvest time - function 
Species_comparsion_plots<-function(data, yvar) {
  ggplot(data, aes(x = Species, y = mean, group = Species, fill = Species))+
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high))+
    labs(y = yvar)+
    geom_point(aes(shape = Species, fill = Species ), size = 4) +
    scale_shape_manual(values=c(23,21,23,21,21,23,21))+
    scale_fill_manual(values = c("gray31","dodgerblue3","gray55", "dodgerblue1", "royalblue3","grey75","royalblue1"))+
    facet_grid(. ~ Harvest)+
    theme_bw()+
    theme(axis.title.x = element_blank())
}

# Get 90% credible intervales function 
CIfunction<-function (x){
  round(t(apply(x[ ,c("b_H_num2","b_H_num3","b_H_num4")], 2, quantile, c(.5, .05, .95))), digits = 2)
}

# Get CI - SLA 
CIs_SLA<-lapply(SLA_posteriors, CIfunction)
CIs_SLA_bound<-ldply(CIs_SLA, rbind)    # Is there a different way to do this? 
CIs_SLA_bound$harvest<-rep(c("24 Days","42 Days","84 Days"))
names(CIs_SLA_bound)<-c("Species","mean","CI_low","CI_high","Harvest")
CIs_SLA_bound$Species<-factor(CIs_SLA_bound$Species, levels = c("VUOC","PLPA","ELTR","PAMU","HEVI","HECO","ACMI")) 

# Get CI - SRL 
CIs_SRL<-lapply(SRL_posteriors, CIfunction)
CIs_SRL_bound<-ldply(CIs_SRL, rbind)    # Is there a different way to do this? 
CIs_SRL_bound$harvest<-rep(c("24 Days","42 Days","84 Days"))
names(CIs_SRL_bound)<-c("Species","mean","CI_low","CI_high","Harvest")
CIs_SRL_bound$Species<-factor(CIs_SRL_bound$Species, levels = c("VUOC","PLPA","ELTR","PAMU","HEVI","HECO","ACMI")) 

# Plots 
SLA_Hcomp_plot <- Species_comparsion_plots(CIs_SLA_bound, expression("SLA"~ (cm^{2} ~ g^{-1} )))
SRL_Hcomp_plot <- Species_comparsion_plots(CIs_SRL_bound, expression("SRL"~ (cm ~ g^{-1})))


## PDFS of plots 

pdf("~/Downloads/Traits_2017_Figures.pdf", height=12, width=8)
grid.arrange(arrangeGrob(SLA_plots$VUOC, SRL_plots$VUOC, RMR_plots$VUOC,RALAR_plots$VUOC, ncol = 4, 
                         top = textGrob ("Vulpia octoflora", gp = gpar (fontsize = 10))),
             arrangeGrob(SLA_plots$HECO, SRL_plots$HECO, RMR_plots$HECO,RALAR_plots$HECO,ncol = 4, 
                         top = textGrob ("Hesperostipa comata", gp = gpar (fontsize = 10))),
             arrangeGrob(SLA_plots$ELTR, SRL_plots$ELTR, RMR_plots$ELTR,RALAR_plots$ELTR, ncol = 4, 
                         top = textGrob ("Elymus trachycaulus", gp = gpar (fontsize = 10))),
             arrangeGrob(SLA_plots$PLPA, SRL_plots$PLPA, RMR_plots$PLPA,RALAR_plots$PLPA,ncol = 4, 
                         top = textGrob ("Plantago patagonica", gp = gpar (fontsize = 10))),
             arrangeGrob(SLA_plots$PAMU, SRL_plots$PAMU,RMR_plots$PAMU,RALAR_plots$PAMU,ncol = 4, 
                         top = textGrob ("Packera multilobata", gp = gpar (fontsize = 10))),
             arrangeGrob(SLA_plots$HEVI, SRL_plots$HEVI,RMR_plots$HEVI,RALAR_plots$HEVI, ncol = 4, 
                         top = textGrob ("Heterotheca villosa", gp = gpar (fontsize = 10))),
             left = textGrob("Days", rot = 90, vjust = 1, gp = gpar (fontsize = 12)),
             nrow= 6)
dev.off()


pdf("~/Downloads/SLA_SRL_HComparison_Figures.pdf", height=7, width=10)
grid.arrange(arrangeGrob(SLA_Hcomp_plot, ncol = 1, top = textGrob ("SLA Comparisons through time", gp = gpar (fontsize = 12))),
             arrangeGrob(SRL_Hcomp_plot, ncol = 1, top = textGrob ("SRL Comparisons through time", gp = gpar (fontsize = 12))),
             nrow = 2)
dev.off()

