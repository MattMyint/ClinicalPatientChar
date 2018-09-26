#### patient selection ####
## use patients with dose 40
dose40_df <- df[df$StatinDose == 40,]
sim_pat <- dose40_df[dose40_df$StatinType == "Simvastatin",]
atv_pat <- dose40_df[dose40_df$StatinType == "Atorvastatin",]
atv.cobas <- atv_pat[atv_pat$COBAS_hsCRP > 0,]
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