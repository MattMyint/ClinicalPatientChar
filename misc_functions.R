#### find associations to myalgia score####
cont.tables <- list()
for(i in colnames(atv_pat)[108:134]){
  mt <- table(atv_pat[,c("Myalgia_Score", i)])
  ft.mt <- fisher.test(mt)
  if(ft.mt$p.value < 0.05){
    print(i)
  }
}

#### Quick boxplot function ####
g.boxplot <- function(x,y,df){
  data <- df[,c(x, y)]
  g <- ggplot(data, aes_string(x, y)) + geom_boxplot(aes_string(fill = x)) +
    annotate("text", x = 1)
  plot(g)
}
