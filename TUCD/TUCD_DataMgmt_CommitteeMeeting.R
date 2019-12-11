# Committee Meeting - Dec 2019 
# November 27, 2019 
# 2019 TUCD Data 

setwd("/Users/MagdaGarbowski/CommitteeMeeting/TUCD/")

TUCD_GH<-read.csv("ELTR_TUCD_WEIGHTS_CommitteeMeeting.csv")
TUCD_leaf_scans<-read.delim2("ELTR_TUCD_LEAF_ANALYSES.TXT", header = TRUE, sep = "\t", dec = ".")
TUCD_root_scans<-read.delim2("ELTR_TUCD_ROOT_ANALYSES.TXT", header = TRUE, sep = "\t", dec = ".")


# Keep only columns of interest 
TUCD_leaf_scans<-TUCD_leaf_scans[c("RHIZO.2016a","ProjArea.cm2.")]
TUCD_root_scans<-TUCD_root_scans[c("RHIZO.2016a","Length.cm.", "AvgDiam.mm.","Tips","Forks",
                                   "X0..L...0.5000000","X0.5000000..V...1.0000000","X1.0000000..L...1.5000000","X1.5000000..L...2.0000000",
                                   "X2.0000000..L...2.5000000", "X2.5000000..L...3.0000000","X3.0000000..L...3.5000000",
                                   "X3.5000000..L...4.0000000","X4.0000000..L...4.5000000")]

# Create sampleID columns
Sample.ID.cols<-function(data){
  tmp<-strsplit(as.character(data), "_")
  tmp2<-do.call(rbind, tmp)
  tmp3<-as.data.frame(cbind(data, tmp2))
  names(tmp3)<-c("data", "SPECIES", "LOCATION_CODE", "POP_CODE", "SAMPLE_NUMBER")
  return(tmp3)
}

# How to create columns of dataframes in function? 
Add_cols<-function(x, samplename){
  tmp<-Sample.ID.cols(x[[samplename]])
  tmp2<-cbind(tmp, x)
  tmp2$POP_ID<-paste(tmp2$LOCATION_CODE,tmp2$POP_CODE, sep = "_")
  tmp2$Sample_ID<-as.factor(paste(tmp2$SPECIES,tmp2$LOCATION_CODE, tmp2$POP_CODE,Leaf_data$SAMPLE_NUMBER, sep = "_"))
  return(tmp2)
}

Leaf_data<-Add_cols(TUCD_leaf_scans, "RHIZO.2016a")


Leaf_data_2 <- cbind(aggregate(ProjArea.cm2.~Sample_ID, sum, data=Leaf_data), table(Leaf_data$Sample_ID))[, -c(3,4)]

Root_data<-Add_cols(TUCD_root_scans)
Weight_data<-Add_cols(TUCD_GH)     ##### HERE!!! Wrong name for sample IS 

Leaf_data<-Sample.ID.cols(TUCD_leaf_scans$RHIZO.2016a)
Leaf_data<-cbind(Leaf_data,TUCD_leaf_scans)

Leaf_data$POP_ID<-paste(Leaf_data$LOCATION_CODE,Leaf_data$POP_CODE, sep = "_")
Leaf_data$Sample_ID<-as.factor(paste(Leaf_data$SPECIES,Leaf_data$LOCATION_CODE, Leaf_data$POP_CODE,Leaf_data$SAMPLE_NUMBER, sep = "_"))

Root_data<-Sample.ID.cols(TUCD_root_scans$RHIZO.2016a)
Root_data<-cbind(Root_data,TUCD_root_scans)
Root_data$POP_ID<-paste(Root_data$LOCATION_CODE,Root_data$POP_CODE, sep = "_")
Root_data$Sample_ID<-as.factor(paste(Root_data$SPECIES,Root_data$LOCATION_CODE, Root_data$POP_CODE,Root_data$SAMPLE_NUMBER, sep = "_"))

Weight_data<-Sample.ID.cols(TUCD_GH$CSU_TRAIT_ID)
Weight_data<-cbind(Weight_data, TUCD_GH)
Weight_data$POP_ID<-paste(Weight_data$LOCATION_CODE,Weight_data$POP_CODE, sep = "_")
Weight_data$Sample_ID<-paste(Weight_data$SPECIES,Weight_data$LOCATION_CODE, Weight_data$POP_CODE,Weight_data$SAMPLE_NUMBER, sep = "_")

# Merge datasets

All_TUCD<-merge(Leaf_data_2, Root_data, by = "Sample_ID")
All_TUCD<-merge(All_TUCD, Weight_data, by = "Sample_ID")
All_TUCD.2<-All_TUCD[,!(colnames(All_TUCD) %in% c("data.x","SPECIES.x","LOCATION_CODE.x","POP_CODE.x","SAMPLE_NUMBER.x",
                         "NA.x","NA.x.1","RHIZO.2016a.x","NA.y","NA.y","RHIZO.2016a.y","data.y","SPECIES.y","LOCATION_CODE.y",
                         "POP_CODE.y","SAMPLE_NUMBER.y",
                         "NA.y","NA.y","RHIZO.2016a.y"))]
TUCD_data<-All_TUCD.2

# Change all zeros to NA 
TUCD_data[TUCD_data == 0] <- NA

# Drop TMC, SP, FS from dataset for analyses
TUCD_data_Subset<-TUCD_data[!TUCD_data$POP_ID.y%in% c("UTC_TMC","UTSW_SP","CULT_FS", "UTSW_DNF"),]
TUCD_data_Subset2<-TUCD_data_Subset[!TUCD_data_Subset$H_num %in% c("1","D",""),]
TUCD_data_Subset2<-TUCD_data_Subset2[!TUCD_data_Subset2$TRT %in% c("B"),]
TUCD_data_Subset2$LWD<-as.numeric(as.character(TUCD_data_Subset2$LWD))
TUCD_data_Subset2$RWD<-as.numeric(as.character(TUCD_data_Subset2$RWD))

# Create SLA , SRL, RMR, LDMC, RAR

# SLA  - For H1, H2, H3 all values being used, for H4 leaf values being used
TUCD_data_Subset2$SLA <- ifelse(is.na(TUCD_data_Subset2[,c("LWS_A")]), ((TUCD_data_Subset2$ProjArea.cm2.)/(TUCD_data_Subset2$LWD)),
                         ((TUCD_data_Subset2$ProjArea.cm2.)/(rowSums(TUCD_data_Subset2[,c("LWD_A","LWD_B")]))))
# SRL 
TUCD_data_Subset2$SRL <- ((TUCD_data_Subset2$Length.cm./TUCD_data_Subset2$RWD))

# RMR 
TUCD_data_Subset2$RMR <- (TUCD_data_Subset2$RWD)/(rowSums(TUCD_data_Subset2[,c("LWD", "RWD", "LWD_A", "LWD_B")], na.rm = TRUE))

# RALAR (Root area leaf area ratio)
TUCD_data_Subset2$RARL <- ifelse(is.na(TUCD_data_Subset2[,c("LWS_A")]), 
                           ((TUCD_data_Subset2$ProjArea.cm2.)/(TUCD_data_Subset2[,c("Length.cm.")])),NA)

write.csv(TUCD_data_Subset2, "TUCD_data_H2.csv")



