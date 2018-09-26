#### load raw dataset ####
df <- read.csv(file = "Patient Clinical Characteristics (No Dup_Trimmed Genetics).csv")[,-1]

#### data cleaning ####
local({
  for(i in c("5h", "6h", "7h", "8h", "9h", "10h", "11h", "13h", "14h",
             "KEL", "HalfLife", "NoHL", "EstCMax")){
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
  df$Age <<- as.numeric(df$Age)
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
num_cols <- as.character(type_df$header[type_df$type == "numeric"])