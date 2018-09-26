#### descriptive stats ####
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

#### Bulk find significant relations ####
## quantitative vs quantitative

corrs <- cor(df[,num_cols], method = "spearman")
corrs_cobas <- cor(w.hsCRP[,num_cols], method = "spearman")
corrs[is.na(corrs)] <- 0
index <- which(abs(corrs) > 0.5 & abs(corrs) < 1, arr.ind=TRUE)
index <- as.data.frame(index)
index$col <- lapply(index$col, function(x) colnames(corrs)[x])

index_cobas <- which(abs(corrs_cobas) > 0.5 & abs(corrs_cobas) < 1, arr.ind=TRUE)
index_cobas <- as.data.frame(index_cobas)
index_cobas$col <- lapply(index_cobas$col, function(x) colnames(corrs_cobas)[x])

## qualitative vs quantitative
sig.compares <- list()
ij <- 1
for(i in seq_along(f_groupings)){
  for(j in seq_along(num_cols)){
    tryCatch({
      stat_test <- kruskal.test(x = w.hsCRP[,num_cols[j]], g = w.hsCRP[,f_groupings[i]])
      if(stat_test$p.value < 0.05){
        sig.compares[[ij]] <- stat_test$data.name
        ij <- ij + 1
      }
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}


