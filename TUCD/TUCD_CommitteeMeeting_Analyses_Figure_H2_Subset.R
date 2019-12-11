# Committee Meeting - Dec 2019 
# Dec 1, 2019 
# 2019 TUCD Data 

library(ggplot2)
library(lme4)
library(emmeans)
library(lmerTest)
library(plyr)

setwd("/Users/MagdaGarbowski/CommitteeMeeting/TUCD/")

TUCD_subset<-read.csv("TUCD_data_H2.csv")
levels(TUCD_subset$POP_ID.y)<-c("CONC_WR","NWMW_C", "CULT_PRY","CULT_RV","CULT_SL")

# Boxplots 

box_plot_TUCD<-function(data, colname1, colname2,var_x = data[,colname1], var_y = data[,colname2]){
  ggplot(data, aes(x = var_x, y = var_y)) + 
    geom_boxplot() + facet_grid(.~TRT)
}


TUCD_plots_SLA<-ggplot(TUCD_subset, aes(x = POP_ID.y, y = SLA)) + 
  geom_boxplot() + facet_grid(.~TRT)

TUCD_plots_SRL<-ggplot(TUCD_subset, aes(x = POP_ID.y, y = SRL)) + 
  geom_boxplot() + facet_grid(.~TRT)

TUCD_plots_RMR<-ggplot(TUCD_subset, aes(x = POP_ID.y, y = RMR)) + 
  geom_boxplot() + facet_grid(.~TRT)

TUCD_plots_RARL<-ggplot(TUCD_subset, aes(x = POP_ID.y, y = RARL)) + 
  geom_boxplot() + facet_grid(.~TRT)

TUCD_plots_Length.cm.<-ggplot(TUCD_subset, aes(x = POP_ID.y, y = Length.cm.)) + 
  geom_boxplot() + facet_grid(.~TRT)

TUCD_plots_tips<-ggplot(TUCD_subset, aes(x = POP_ID.y, y = Tips)) + 
  geom_boxplot() + facet_grid(.~TRT)


# Models - how can I put lmer into a function? 




mod1<-lmer((SLA) ~ TRT * POP_ID.y + (1|Round), TUCD_subset)
plot(mod1)
anova(mod1)

mod2<-lmer((SRL) ~ TRT * POP_ID.y + (1|Round), TUCD_subset)
plot(mod2)
anova(mod2)

mod3<-lmer((RMR) ~ TRT * POP_ID.y + (1|Round), TUCD_subset)
plot(mod3)
anova(mod3)
CLD(emmeans(mod3, ~ TRT))
CLD(emmeans(mod3, ~ POP_ID.y))

mod4<-lmer(sqrt(RARL) ~ TRT * POP_ID.y + (1|Round), TUCD_subset)
plot(mod4)
anova(mod4)

mod5<-lmer(sqrt(Length.cm.) ~ TRT * POP_ID.y + (1|Round), TUCD_subset)
plot(mod5)
anova(mod5)
CLD(emmeans(mod4, ~ TRT * POP_ID.y ), alpha = 0.1)

mod6<-lmer(log(Tips) ~ TRT * POP_ID.y + (1|Round), TUCD_subset)
plot(mod6)
anova(mod6)
CLD(emmeans(mod6, ~ TRT * POP_ID.y ), alpha = 0.05)

mod7<-lmer((RWD) ~ TRT * POP_ID.y + (1|Round), TUCD_subset)
plot(mod7)
anova(mod7)
CLD(emmeans(mod7, ~ TRT ))

mod9<-lmer((LWD) ~ TRT * POP_ID.y + (1|Round), TUCD_subset)
plot(mod8)
anova(mod8)
CLD(emmeans(mod8, ~ TRT * POP_ID.y ))

mod9<-lmer((LWD+RWD) ~ TRT * POP_ID.y + (1|Round), TUCD_subset)
plot(mod9)
anova(mod9)
CLD(emmeans(mod9, ~ TRT * POP_ID.y ))

### Aboveground/belowground figure 

TUCD_Above_Sumstats<-ddply(TUCD_subset, c("POP_ID.y","TRT"),summarise, 
                            N=length(LWD*1000),
                            mean=mean(LWD*1000), 
                            sd=sd(LWD*1000),
                            se=sd/sqrt(N))
TUCD_Above_Sumstats$BM<-"Above"
TUCD_Above_Sumstats$Type<-"Shoot"

TUCD_Below_Sumstats<-ddply(TUCD_subset, c("POP_ID.y","TRT"),summarise, 
                            N=length(RWD*1000),
                            mean=mean(RWD*1000), 
                            sd=sd(RWD*1000),
                            se=sd/sqrt(N))
TUCD_Below_Sumstats$BM<-"Below"
TUCD_Below_Sumstats$Type<-"Root"

DF<- rbind(TUCD_Above_Sumstats, TUCD_Below_Sumstats)
DF$BM <- factor(DF$BM,levels=c("Above","Below"))

# Make root values negative 
DF[ DF$Type=="Root", c("mean", "sd","se")] <- DF[ DF$Type=="Root", c("mean", "sd","se")] * -1

# Above below Figure 

TUCD_RMF_Figure<- ggplot(data=DF[DF$BM %in% c("Above", "Below"),], aes(x=POP_ID.y, y=mean, fill=TRT, group = TRT)) + 
  geom_bar(stat="identity", position="dodge", width=0.88, color="black")+
  geom_errorbar(data=DF[DF$BM %in% c("Below"),], aes (x=POP_ID.y, ymin=mean-se, ymax=mean+se, group = TRT), width=0.2, size=0.5, position=position_dodge(.9))+
  geom_errorbar(data=DF[DF$BM %in% c("Above"),], aes (x=POP_ID.y, ymin=mean-se, ymax=mean+se, group = TRT), width=0.2, size=0.5, position=position_dodge(.9))+
  scale_fill_manual(name = "Treatment",
                    labels = c("Control","Drought"),
                    values=c("gray75", "orangered4")) +
  labs(title=expression(paste("Above and Below Ground Biomass")),
       y=expression(paste("Biomass (mg)"), x="POP_ID.y"))+
  theme_bw()+
  ylim(-11, 18)+
  theme(plot.title=element_blank(),
        panel.border = element_rect(fill=NA, colour='black', size=1), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position="right",legend.text.align=0, legend.text=element_text(size=16),
        legend.title=element_text(size=16), 
        strip.text=element_text(size=16), 
        axis.title.x=element_blank(),
        axis.text.x = element_text(colour = "black",size=14), 
        axis.title.y=element_text(size=16),
        axis.text.y = element_text(colour = "black",size=14)) 

TUCD_RMF_Figure_2<-TUCD_RMF_Figure + annotate("text", 
                  label=c("bc","bc","abc","c","bc","c","ab","c","a","c"),
                  #x=DF[DF$BM %in% c("Above"),]$POP_ID,
                  x = c(.75,1.25,1.75,2.25,2.75,3.25,3.75,4.25,4.75,5.25),
                  y=(DF[DF$BM %in% c("Above"),]$mean + DF[DF$BM %in% c("Above"),]$se+1),
                  size=5, color="black") +
                  annotate("text", 
                           label = c (" *Belowground \n main effect of drought"),
                           x = c(2.75),
                           y = c(-10))

pdf("~/Downloads/TUCD_h2_Control_Drought.pdf", height=4, width=6)
TUCD_RMF_Figure_2
dev.off()
