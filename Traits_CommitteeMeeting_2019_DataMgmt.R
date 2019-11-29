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

# Create POP_ID and SPECIES columns 
tmp<-strsplit(as.character(Trait_data$SAMPLE_ID), "_")
tmp2<-do.call(rbind, tmp)
colnames(tmp2)<- c("SPECIES","LOCATION_CODE","POP_CODE","SAMPLE_NUM","LETTER","COLOR")
tmp3<-cbind(Trait_data,tmp2)
tmp3$POP_ID<-paste(tmp3$SPECIES,tmp3$LOCATION_CODE,tmp3$POP_CODE, sep = "_")
Trait_data<-tmp3

# Create SLA , SRL, RMR, LDMC, RAR

# SLA  - For H1, H2, H3 all values being used, for H4 leaf values being used
Trait_data$SLA <- ifelse(is.na(Trait_data[,c("LEAF_TOTAL")]), ((Trait_data$LEAVES_TOTAL)/(Trait_data$LWD)),
                        ((Trait_data$LEAF_TOTAL)/(rowSums(Trait_data[,c("LWD_A","LWD_B")], na.rm=TRUE))))
# SRL 
Trait_data$SRL <- ((Trait_data$SumOfLength.cm./Trait_data$RWD)/100)

# RMR 
Trait_data$RMR <- (Trait_data$RWD)/(rowSums(Trait_data[,c("LWD", "RWD", "LWD_A", "LWD_B")], na.rm = TRUE))

# RALAR (Root area leaf area ratio)
Trait_data$RALAR <- ifelse(is.na(Trait_data$COTS_TOTAL) & is.na(Trait_data$LEAVES_TOTAL), NA,
                         ((Trait_data$SumOfProjArea.cm2.)/(rowSums(Trait_data[,c("SumOfProjArea.cm2.","Total.Of.PROJ_AREA_AG")]))))

# Remove values that are out of range 

Trait_data$SLA[Trait_data$SLA > 2000] <- NA
Trait_data$SRL[Trait_data$SRL > 1000] <- NA
Trait_data$RMR[Trait_data$RMR > 0.95] <- NA 

### Write complete dataset to CSV 
write.csv(Trait_data, "TraitData_2017.csv")

