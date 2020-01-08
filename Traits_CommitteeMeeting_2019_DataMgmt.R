### Data Mgmt Code for Trait Data 2017 
### November 27, 2019 
### 2017 Trait data - Traits through time 

setwd("/Users/MagdaGarbowski/CommitteeMeeting/")

# Load data 

filedirectory = "Data_queries/"
file_list = list.files(path=filedirectory, pattern="\\.csv", full.names = TRUE)
ll = lapply(file_list, read.csv, stringsAsFactor = FALSE)
ll = do.call(rbind, ll)
Trait_data<-ll

# Change all zeros to NA 
Trait_data[Trait_data == 0] <- NA

# Remove "late" measurements of PLPA 
Trait_data<-Trait_data [!(Trait_data$NOTES%in%c("Late")), ]

# Remove "_IN_" from ELTR_NMNW 
Trait_data$SAMPLE_ID<-gsub("_IN_","_",Trait_data$SAMPLE_ID)
Trait_data$SAMPLE_ID<-gsub("ARTRWY","ARTR",Trait_data$SAMPLE_ID)


# Create POP_ID and SPECIES columns 
tmp<-strsplit(as.character(Trait_data$SAMPLE_ID), "_")
tmp2<-do.call(rbind, tmp)
colnames(tmp2)<- c("SPECIES","LOCATION_CODE","POP_CODE","SAMPLE_NUM","LETTER","COLOR")
tmp3<-cbind(Trait_data,tmp2)
tmp3$POP_ID<-paste(tmp3$SPECIES,tmp3$LOCATION_CODE,tmp3$POP_CODE, sep = "_")
Trait_data<-tmp3

# Create SLA , SRL, RMR, LDMC, RER, RTD, AGR (absolute growth rate), PL (Plasticity through time) 

# LDMC (Leaf dry matter content) - For H1, H2, H3 all values being used, for H4 leaf values being used
# Need to select "max" weight from LWF_A or LWS_A, LWF_B or LWS_B), BWS or BWF... if NA on those three then do LWS and LWD 
# Create tmp data wait pmax of "pairs" columns 

Trait_data$LW_Max_A<-pmax(Trait_data$LWS_A,Trait_data$LWF_A)
Trait_data$LW_Max_B<-pmax(Trait_data$LWS_B,Trait_data$LWF_B)
Trait_data$BW_Max<-pmax(Trait_data$BWS_A,Trait_data$BWF_A)

Trait_data$LW_Max_Sum<-rowSums(Trait_data[,c("LW_Max_A","LW_Max_B", "BW_Max")], na.rm = TRUE)
Trait_data$LW_Max_Sum[Trait_data$LW_Max_Sum == 0] <- NA

Trait_data$LDMC <- ifelse(is.na(Trait_data[,c("LW_Max_Sum")]),
    ((Trait_data$LWD)/(Trait_data$LWS)), 
    ((rowSums(Trait_data[,c("LWD_A","LWD_B","BWD_A")], na.rm = TRUE))/(Trait_data$LW_Max_Sum)))

# SLA  - For H1, H2, H3 all values being used, for H4 leaf values being used
Trait_data$SLA <- ifelse(is.na(Trait_data[,c("LEAF_TOTAL")]), ((Trait_data$LEAVES_TOTAL)/(Trait_data$LWD)),
                         ((Trait_data$LEAF_TOTAL)/(rowSums(Trait_data[,c("LWD_A","LWD_B")], na.rm=TRUE))))

# SRL (Specific Root Length)
Trait_data$SRL <- ((Trait_data$SumOfLength.cm./Trait_data$RWD)/100)

# RDMC (Root Dry Matter Content)

Trait_data$RDMC <- (Trait_data$RWF)/(Trait_data$RWD)

# RTD (Root tissue density - g / cm ^3) 

Trait_data$RTD <- (Trait_data$RWD)/(Trait_data$SumOfRootVolume.cm3.)

# RMR (Root Mass Ratio)
Trait_data$RMR <- (Trait_data$RWD)/(rowSums(Trait_data[,c( "CWD", "SWD","LWD", "RWD", "LWD_A", "LWD_B")], na.rm = TRUE))

# RALAR (Root area leaf area ratio)
Trait_data$RALAR <- ifelse(is.na(Trait_data$COTS_TOTAL) & is.na(Trait_data$LEAVES_TOTAL), NA,
                         ((Trait_data$SumOfProjArea.cm2.)/(rowSums(Trait_data[,c("SumOfProjArea.cm2.","Total.Of.PROJ_AREA_AG")]))))

# Select rows with NA in SLA, SRL, RMR, RALAR, LDMC

Check<-Trait_data[with(Trait_data,is.na(SLA) | is.na(LDMC) | is.na(SRL) | is.na (RMR) | is.na (RALAR)),]
#write.csv(Check, "Check.csv")

# Remove non-sense values 
Trait_data$SLA[Trait_data$SLA > 2000] <- NA
Trait_data$SRL[Trait_data$SRL > 1000] <- NA
Trait_data$RMR[Trait_data$RMR > 0.95] <- NA 

# Relative Growth Rate (In own dataset with Root elongation rate (REG) and PL (Plasticity))

# Aggregate weights of aboveground (CWD, LWD, SWD, LWD_A, LWD_B) and belowground (RWD) by H_num, POP_ID 

# Sum of aboveground for each sample 
Trait_data$Above_weight_sum <- rowSums(Trait_data[,c("CWD","LWD", "SWD","LWD_A","LWD_B", "BWD_A", "SWD_A")], na.rm = TRUE)
Trait_data$Tot_weight_sum <- rowSums(Trait_data[,c("Above_weight_sum","RWD")], na.rm = TRUE)

# Add a "Days" column to dataset to calculate growth rates 
Trait_data$Days <- as.factor(Trait_data$H_num)
levels(Trait_data$Days) <- c("10","24","42","84")

Above_weight_avg<-aggregate(Trait_data$Above_weight_sum, by = list (Trait_data$Days, Trait_data$POP_ID), FUN = mean, na.rm = TRUE)
Tot_weight_avg<-aggregate(Trait_data$Tot_weight_sum, by = list (Trait_data$Days, Trait_data$POP_ID), FUN = mean, na.rm = TRUE)
Below_weight_avg<-aggregate(Trait_data$RWD, by = list (Trait_data$Days, Trait_data$POP_ID), FUN = mean, na.rm = TRUE)
RER_avg<-aggregate(Trait_data$SumOfLength.cm., by = list (Trait_data$Days, Trait_data$POP_ID), FUN = mean, na.rm = TRUE)

# COME BACK AND TURN THE FOLLOWING INTO FUNCTIONS 
# Need to make these ln 
# Calculate RGR Root, Shoot, Total - Make above datasets into a list and run commands with lapply 

RER_avg$Group.1<-as.numeric(as.character(RER_avg$Group.1))
RER_avg$RER <- with(RER_avg, ave(x, Group.2, 
                              FUN=function(val) c(NA, diff(val)/diff(Group.1))))

Above_weight_avg$Group.1<-as.numeric(as.character(Above_weight_avg$Group.1))
Above_weight_avg$Above_GR <- with(Above_weight_avg, ave(x, Group.2, 
                                 FUN=function(val) c(NA, diff(val)/diff(Group.1))))

Below_weight_avg$Group.1<-as.numeric(as.character(Below_weight_avg$Group.1))
Below_weight_avg$Below_GR <- with(Below_weight_avg, ave(x, Group.2, 
                                                            FUN=function(val) c(NA, diff(val)/diff(Group.1))))

Tot_weight_avg$Group.1<-as.numeric(as.character(Tot_weight_avg$Group.1))
Tot_weight_avg$Tot_GR <- with(Tot_weight_avg, ave(x, Group.2, 
                                                            FUN=function(val) c(NA, diff(val)/diff(Group.1))))

GrowthRates_Traits2017<-merge(RER_avg, Above_weight_avg, by = c("Group.1", "Group.2"))
GrowthRates_Traits2017_2<-merge(GrowthRates_Traits2017, Below_weight_avg, by = c("Group.1", "Group.2"))
GrowthRates_Traits2017_3<-merge(GrowthRates_Traits2017_2, Tot_weight_avg, by = c("Group.1", "Group.2"))

# Remove columns x.x and x.y
GrowthRates_Traits2017_3<-GrowthRates_Traits2017_3[,!names(GrowthRates_Traits2017_3) %in% c("x.x","x.y")]
GrowthRates_Traits2017_3[GrowthRates_Traits2017_3 < 0 ] <-NA
View(format(GrowthRates_Traits2017_3, scientific = FALSE))


### Write complete dataset to CSV 
write.csv(Trait_data, "TraitData_2017.csv")
write.csv(GrowthRates_Traits2017_3, "TraitData_GrowthRates_2017.csv")


