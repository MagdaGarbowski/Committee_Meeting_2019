# Committee Meeting - Dec 2019 
# November 27, 2019 
# 2017 Trait data - Traits through time   

# Things to do next (as of Jan 8, 2020) 
# (1) Clean up code - make more functions? 
# (2) Split groups into : Annuals, perennial grasses, perennial forbs
# (2) will have to be done in data mgmt code
# (3) Add species and growthform columns to growth rate dataset and plot 
# (4) WTF is going on with RDMC PAMU at H_3 ? 

setwd("/Users/MagdaGarbowski/CommitteeMeeting") 

library(rstanarm)
library(rstan)
library(plyr)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(tidybayes)
library(ggridges)
library(ggmcmc)
library(bayesplot)

# Data - from "Traits_CommitteeMeeting_2019_DataMgmt.R"

SpeciesData<-read.csv("TraitData_2017.csv") 
# SpeciesData_Growthrate<-read.csv("TraitData_GrowthRates_2017.csv")

SpeciesData$H_num<-as.factor(SpeciesData$H_num)
Species_splits = split(SpeciesData, paste(SpeciesData$SPECIES))

### R-stan arm models


# SLA 
SLA_rstanarm <- function (df) {
  stan_lmer(SLA ~ 0 + H_num + (1|POP_ID), 
            data = df,
            prior_intercept = student_t(3,0,30), 
            adapt_delta = 0.999)
}

# LDMC 
LDMC_rstanarm <- function (df) {
  stan_lmer(LDMC ~ 0 + H_num + (1|POP_ID), 
            data = df,
            prior_intercept = student_t(3,0,30), 
            adapt_delta = 0.999)
}

# SRL 
SRL_rstanarm <- function (df) {
  stan_lmer(SRL ~ 0 + H_num + (1|POP_ID), 
            data = df,
            prior_intercept = student_t(3,0,30), 
            adapt_delta = 0.999)
}

# RMR 
RMR_rstanarm <- function (df) {
  stan_lmer(RMR ~ 0 + H_num + (1|POP_ID), 
            data = df,
            prior_intercept = student_t(3,0,30), 
            adapt_delta = 0.999)
}

# RDMC 
RDMC_rstanarm <- function (df) {
  stan_lmer(RDMC ~ 0 + H_num + (1|POP_ID), 
            data = df,
            prior_intercept = student_t(3,0,30), 
            adapt_delta = 0.999)
}

# RTD 
RTD_rstanarm <- function (df) {
  stan_lmer(RTD ~ 0 + H_num + (1|POP_ID), 
            data = df,
            prior_intercept = student_t(3,0,30), 
            adapt_delta = 0.999)
}



posterior_medians_intervals <- function(df){
  as.data.frame(summary(df, regex_pars = "H_num", digits = 2, probs = c(0.1, 0.5, 0.9)))
}

# Make into single dataframe for plotting 
dat_frame_function<-function(df){
  tmp<-do.call(rbind.data.frame, df)
  tmp$names<-rownames(tmp)
  tmp_2<- data.frame(do.call('rbind', strsplit(as.character(tmp$names),'.',fixed=TRUE)))
  tmp_2$GrowthForm<- ifelse(tmp_2$X1 %in% c("ACMI","ARTR","HEAN","HEVI","MACA","PAMU","PLPA"), "FORB", "GRASS")
  tmp_3<-cbind(tmp, tmp_2)
  names(tmp_3)[names(tmp_3) %in% c( "10%","50%", "90%", "X1","X2")]<-c( "lower_CI","Median", "upper_CI","Species","Harvest")
  print(tmp_3)
}

# Plot 
plot_function<-function(df,growthform, colors, shapes, Trait){
  ggplot(data = df[df$GrowthForm %in% c(growthform),], 
         aes (x = Harvest, y = Median, group = Species, shape = Species)) + 
    geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), 
                  width = 0.1, position=position_dodge(width = 0.2), color = "gray60")+
    geom_line(position=position_dodge(width = 0.2), size = .3)+
    geom_point(aes(fill = Species), size = 4, position=position_dodge(width = 0.2))+
    scale_shape_manual(values = shapes)+ 
    scale_fill_manual(values = colors)+ 
    scale_x_discrete(breaks = c("H_num1","H_num2","H_num3","H_num4"),
                     labels = c("10", "24","42","84"))+
    ylab(Trait)+
    theme_bw() + 
    theme(axis.text=element_text(size=12),
          plot.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 14))
}

SLA_rstanarm_mods<-lapply(Species_splits, SLA_rstanarm)
posterior_medians_intervals_SLA<-lapply(SLA_rstanarm_mods, posterior_medians_intervals)
Medians_intervals_SLA<-dat_frame_function(posterior_medians_intervals_SLA)

LDMC_rstanarm_mods<-lapply(Species_splits, LDMC_rstanarm)
posterior_medians_intervals_LDMC<-lapply(LDMC_rstanarm_mods, posterior_medians_intervals)
Medians_intervals_LDMC<-dat_frame_function(posterior_medians_intervals_LDMC)

SRL_rstanarm_mods<-lapply(Species_splits, SRL_rstanarm)
posterior_medians_intervals_SRL<-lapply(SRL_rstanarm_mods, posterior_medians_intervals)
Medians_intervals_SRL<-dat_frame_function(posterior_medians_intervals_SRL)

RMR_rstanarm_mods<-lapply(Species_splits, RMR_rstanarm)
posterior_medians_intervals_RMR<-lapply(RMR_rstanarm_mods, posterior_medians_intervals)
Medians_intervals_RMR<-dat_frame_function(posterior_medians_intervals_RMR)

RDMC_rstanarm_mods<-lapply(Species_splits, RDMC_rstanarm)
posterior_medians_intervals_RDMC<-lapply(RDMC_rstanarm_mods, posterior_medians_intervals)
Medians_intervals_RDMC<-dat_frame_function(posterior_medians_intervals_RDMC)

RTD_rstanarm_mods<-lapply(Species_splits, RTD_rstanarm)
posterior_medians_intervals_RTD<-lapply(RTD_rstanarm_mods, posterior_medians_intervals)
Medians_intervals_RTD<-dat_frame_function(posterior_medians_intervals_RTD)



# Plotting

SRL_GRASS_plot<-plot_function(Medians_intervals_SRL, "GRASS",  
              c("blue3", "violetred4","navajowhite2","grey65" ),
              c(21,21,21,21,21), 
              "SRL")

SRL_FORB_plot<-plot_function(Medians_intervals_SRL, "FORB",  
                             c("coral2", "chartreuse4", "grey85","goldenrod3",
                             "plum4","seagreen3","grey65"), 
                             c(22,22,22,22,22,22,22,22),
                             "SRL")

SLA_GRASS_plot<-plot_function(Medians_intervals_SLA, "GRASS",  
                              c("blue3", "violetred4","navajowhite2","grey65" ),
                              c(21,21,21,21,21), 
                              "SLA")

SLA_FORB_plot<-plot_function(Medians_intervals_SLA, "FORB",  
                             c("coral2", "chartreuse4", "grey85","goldenrod3",
                               "plum4","seagreen3","grey65"), 
                             c(22,22,22,22,22,22,22,22),
                             "SLA")

LDMC_GRASS_plot<-plot_function(Medians_intervals_LDMC, "GRASS",  
                              c("blue3", "violetred4","navajowhite2","grey65" ),
                              c(21,21,21,21,21), 
                              "LDMC")

LDMC_FORB_plot<-plot_function(Medians_intervals_LDMC, "FORB",  
                             c("coral2", "chartreuse4", "grey85","goldenrod3",
                               "plum4","seagreen3","grey65"), 
                             c(22,22,22,22,22,22,22,22),
                             "LDMC")

RMR_GRASS_plot<-plot_function(Medians_intervals_RMR, "GRASS",  
                               c("blue3", "violetred4","navajowhite2","grey65" ),
                               c(21,21,21,21,21), 
                               "RMR")

RMR_FORB_plot<-plot_function(Medians_intervals_RMR, "FORB",  
                              c("coral2", "chartreuse4", "grey85","goldenrod3",
                                "plum4","seagreen3","grey65"), 
                              c(22,22,22,22,22,22,22,22),
                              "RMR")


RDMC_GRASS_plot<-plot_function(Medians_intervals_RDMC, "GRASS",  
                              c("blue3", "violetred4","navajowhite2","grey65" ),
                              c(21,21,21,21,21), 
                              "RDMC")

RDMC_FORB_plot<-plot_function(Medians_intervals_RDMC, "FORB",  
                             c("coral2", "chartreuse4", "grey85","goldenrod3",
                               "plum4","seagreen3","grey65"), 
                             c(22,22,22,22,22,22,22,22),
                             "RDMC")


RTD_GRASS_plot<-plot_function(Medians_intervals_RTD, "GRASS",  
                               c("blue3", "violetred4","navajowhite2","grey65" ),
                               c(21,21,21,21,21), 
                               "RTD")

RTD_FORB_plot<-plot_function(Medians_intervals_RTD, "FORB",  
                              c("coral2", "chartreuse4", "grey85","goldenrod3",
                                "plum4","seagreen3","grey65"), 
                              c(22,22,22,22,22,22,22,22),
                              "RTD")

pdf("~/CommitteeMeeting/Figures_Reports/Grass_figs.pdf", height=8, width=8)
grid.arrange(SLA_GRASS_plot, LDMC_GRASS_plot, 
             SRL_GRASS_plot, RMR_GRASS_plot,
             RDMC_GRASS_plot, RTD_GRASS_plot, ncol = 2 )

dev.off()

pdf("~/CommitteeMeeting/Figures_Reports/Forb_figs.pdf", height=8, width=8)
grid.arrange(SLA_FORB_plot, LDMC_FORB_plot, 
                          SRL_FORB_plot, RMR_FORB_plot,
                          RDMC_FORB_plot, RTD_FORB_plot, ncol = 2 )
dev.off()

# Get species titles onto plots - fix below code for plotting 

posterior_plots<- function(df){
  mcmc_areas(df,
             regex = c("H_num"),
             prob = 0.8)
}

posterior_plots_SRL<-lapply(posteriors_SRL, posterior_plots)

n <- length(posterior_plots_SRL)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(posterior_plots_SRL, ncol=nCol))


