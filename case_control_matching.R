#### A method for sample selection by case-control matching
##     Currently it cannot produce a fully matched data set 
##     with roughly 156 controls matched unevenly to cases
##     (i.e) not every case has 4 controls

## find matched controls by (in order of priority) Race, Gender, Age.
##      minimum 4 per case
match_list <- list()
for(i in seq_along(myalgia_pat)){
  i_race <- as.character(df$Race[df$Subject_ID == myalgia_pat[i]])
  i_gender <- as.character(df$Gender[df$Subject_ID == myalgia_pat[i]])
  i_age <- df$Age[df$Subject_ID == myalgia_pat[i]]
  i_dose <- df$StatinDose[df$Subject_ID == myalgia_pat[i]]
  
  ## get 2 controls from low atv
  if(!any(bot.atv$Race == i_race | bot.atv$Gender == i_gender)){
    if(!any(bot.atv$Race == i_race)){
      print(paste0(myalgia_pat[i], " no match for 'Race'"))
    }else{
      print(paste0(myalgia_pat[i], " no match for 'Gender'"))
    }
    next
  }
  i_latv <- bot.atv[bot.atv$Race == i_race & bot.atv$Gender == i_gender & bot.atv$Age == i_age & bot.atv$Subject_ID != myalgia_pat[i],]
  print(paste0(myalgia_pat[i], " has ", nrow(i_latv), " low atv control matches with age gap '0'"))
  age_gap <- 0
  while(nrow(i_latv) < 2 & age_gap <= 64){
    age_gap <- age_gap + 1
    i_latv <- bot.atv[bot.atv$Race == i_race & bot.atv$Gender == i_gender & bot.atv$Subject_ID != myalgia_pat[i] & 
                                     (bot.atv$Age >= i_age - age_gap & bot.atv$Age <= i_age + age_gap),]
  }
  print(paste0(myalgia_pat[i], " has ", nrow(i_latv), " low atv control matches with age gap ", age_gap))
       # if above while loop fails to match by age, remove it as criterion
  if(nrow(i_latv) < 2){
    i_latv <- bot.atv[bot.atv$Race == i_race & bot.atv$Gender == i_gender & bot.atv$Subject_ID != myalgia_pat[i],]
    print(paste0(myalgia_pat[i], " has ", nrow(i_latv), " low atv control matches with no age matching"))
  }
  if(nrow(i_latv) > 2){
    i_latv_pat <- sample(i_latv$Subject_ID, 2)
    i_latv <- i_latv[i_latv$Subject_ID %in% i_latv_pat,]
    print(paste0(myalgia_pat[i], " has ", nrow(i_latv), " low atv control matches sampled"))
  }else{
    i_latv_pat <- i_latv$Subject_ID
    i_latv <- i_latv[i_latv$Subject_ID %in% i_latv_pat,]
    print(paste0(myalgia_pat[i], " has ", nrow(i_latv), " low atv control matches"))
  }
  bot.atv <- bot.atv[!bot.atv$Subject_ID %in% i_latv_pat,]
  
  ## get 2 controls from high atv
  if(!any(top.atv$Race == i_race | top.atv$Gender == i_gender)){
    if(!any(top.atv$Race == i_race)){
      print(paste0(myalgia_pat[i], " no match for 'Race'"))
    }else{
      print(paste0(myalgia_pat[i], " no match for 'Gender'"))
    }
    next
  }
  i_hatv <- top.atv[top.atv$Race == i_race & top.atv$Gender == i_gender & top.atv$Age == i_age & top.atv$Subject_ID != myalgia_pat[i],]
  print(paste0(myalgia_pat[i], " has ", nrow(i_hatv), " high atv control matches with age gap '0'"))
  age_gap <- 0
  while(nrow(i_hatv) < 2 & age_gap <= 64){
    age_gap <- age_gap + 1
    i_hatv <- top.atv[top.atv$Race == i_race & top.atv$Gender == i_gender & top.atv$Subject_ID != myalgia_pat[i] & 
                        (top.atv$Age >= i_age - age_gap & top.atv$Age <= i_age + age_gap),]
  }
  print(paste0(myalgia_pat[i], " has ", nrow(i_hatv), " high atv control matches with age gap ", age_gap))
      # if above while loop fails to match by age, remove it as criterion
  if(nrow(i_hatv) < 2){
    i_hatv <- top.atv[top.atv$Race == i_race & top.atv$Gender == i_gender & top.atv$Subject_ID != myalgia_pat[i],]
    print(paste0(myalgia_pat[i], " has ", nrow(i_hatv), " high atv control matches with no age matching"))
  }
  if(nrow(i_hatv) > 2){
    i_hatv_pat <- sample(i_hatv$Subject_ID, 2)
    i_hatv <- i_hatv[i_hatv$Subject_ID %in% i_hatv_pat,]
    print(paste0(myalgia_pat[i], " has ", nrow(i_hatv), " high atv control matches sampled"))
  }else{
    i_hatv_pat <- i_hatv$Subject_ID
    i_hatv <- i_hatv[i_hatv$Subject_ID %in% i_hatv_pat,]
    print(paste0(myalgia_pat[i], " has ", nrow(i_hatv), " high atv control matches"))
  }
  top.atv <- top.atv[!top.atv$Subject_ID %in% i_hatv_pat,]
  
  # combine everything together
  i_candidates <- rbind(i_latv, i_hatv)
  match_list[[myalgia_pat[i]]] <- i_candidates$Subject_ID
  cat(myalgia_pat[i], nrow(i_candidates), "\n")
}

#### comments####

#    two possible methods
#      1) 45 myalgia patients, 100 high atv controls, 100 low atv controls, no matching by demographics
#
#      2) 45 myalgia patients, with 156 total controls, matched by demographics
