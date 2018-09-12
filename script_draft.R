#### load raw dataset ####
library(VennDiagram)
df <- read.csv(file = "Patient Clinical Characteristics (No Dup_Trimmed Genetics).csv")

#### data cleaning ####
## remove NA
df[is.na(df)] <- 0
local({
  for(i in c("5h", "6h", "7h", "8h", "9h", "10h", "11h", "13h", "14h")){
    df <<- df[,-grep(i,colnames(df))]
  }
})
local({
  to_convert <- c("Site_Code", "GenderCode", "BMICode", "RaceCode", "StatinCode", 
                  "StatinDoseCode", "MI", "Renal_Problems", "Liver_Problems", "Hypertension", 
                  "Diabetes_Mellitus", "Hypercholesterolemia", "Myalgia", "Myalgia_Score", "Blood_Thinner",
                  "Glucose_lowering", "Cholesterol_Lowering", "Heart_protective", "BP_lowering")
  conv_to_factor <- function(datafram, c){
    datafram[, c] <- as.factor(datafram[, c])
    return(datafram)
  }
  for(i in to_convert){
    df <<- conv_to_factor(df, i)
  }
  df$Subject_ID <<- as.character(df$Subject_ID)
})

## identify which columns are classed wrong (skipped since I found alr)
type_df <- list()
local({
  data_headers <- colnames(df)
  types <- character()
  for(i in seq_along(data_headers)){
    type_i <- class(df[, i])
    types <- c(types, type_i)
  }
  type_df <<- list(header = data_headers, type = types)
})
type_df <- as.data.frame(type_df)
f_groupings <- as.character(type_df[type_df$type == "factor",1])

#### breakdown ####

## descriptive stats using f_groupings 
# [1:8] are demographics
# [9:11] are dosage info
# [12:28] are medical history
# [37:63] are genotypes
#
print(paste0(nrow(df), " patients"))
for(i in f_groupings[37:63]){
  print(paste0("In category ", "'", i, "':"))
  for(j in levels(df[,i])){
    print(paste0(j, ": ", length(df[,i][df[,i] == j])))
  }
}

#### patient selection ####

## use patients with dose 40
dose40_df <- df[df$StatinDose == 40,]
sim_pat <- dose40_df[dose40_df$StatinType == "Simvastatin",]
atv_pat <- dose40_df[dose40_df$StatinType == "Atorvastatin",]
x20hatv_qnt <- quantile(atv_pat$X2OHATV12h, probs = c(0.25, 0.75))
x40hatv_qnt <- quantile(atv_pat$X4OHATV12h, probs = c(0.25, 0.75))
atv_qnt <- quantile(atv_pat$ATVCS12h, probs = c(0.25, 0.75))
sim_qnt <- quantile(sim_pat$SIM12h, probs = c(0.25, 0.75))
simacid_qnt <- quantile(sim_pat$SIMACID12h, probs = c(0.25, 0.75))
crp_qnt <- quantile(dose40_df$COBAS_hsCRP, probs = c(0.25, 0.75))
## 
w.hsCRP <- df[df$COBAS_hsCRP > 0,]

## patients with low drug levels
low_2ohatv <- atv_pat[atv_pat$X2OHATV12h <= x20hatv_qnt[[1]],]
low_4ohatv <- atv_pat[atv_pat$X4OHATV12h <= x40hatv_qnt[[1]],]
low_atv <- atv_pat[atv_pat$ATVCS12h <= atv_qnt[[1]],]
length(low_2ohatv$Subject_ID[low_2ohatv$Subject_ID == low_4ohatv$Subject_ID])
low_20h_4oh <- intersect(low_4ohatv$Subject_ID, low_2ohatv$Subject_ID)
low_20h_atv <- intersect(low_2ohatv$Subject_ID, low_atv$Subject_ID)
low_40h_atv <- intersect(low_4ohatv$Subject_ID, low_atv$Subject_ID)
low_all_atv <- intersect(low_20h_4oh, low_20h_atv)
low_atv_total <- atv_pat[atv_pat$Subject_ID %in% low_all_atv,]

venn.plot <- venn.diagram(x = list("X2OHATV" = low_2ohatv$Subject_ID, "X4OHATV" = low_4ohatv$Subject_ID, "ATV" = low_atv$Subject_ID),
             filename = "Venn_2set_simple.png", imagetype = "png", fill=c("darkmagenta", "darkblue", "seagreen3"),
             main = "Number of subjects with low atorvastatin levels")
