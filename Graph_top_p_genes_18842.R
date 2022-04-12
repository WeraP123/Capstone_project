#results 18842
result_rf_pos$no_genes <- c(2,3,4,5,6,7,8,9,10,15)
result_rf_wil$no_genes <- c(2,3,4,5,6,7,8,9,10,15)
library(dplyr)
result_rf <- result_rf[-10,]
result_rf <- merge(result_rf_pos,result_rf_wil, by="no_genes")
names(result_rf)[2]<- "Acc.POS"
names(result_rf)[4]<- "Acc.WILRS"


result_rf %>%
  ggplot(aes(x=no_genes))+
  geom_line(aes(y=Acc.POS,colour="POS"),size=0.7)+
  geom_line(aes(y=Acc.WILRS,colour="WIL-RS"),size=0.7)+
  scale_colour_manual("", 
                      breaks = c("POS", "WIL-RS"),
                      values = c("red", "blue")) +
  labs(title = "SUBiNN",subtitle = "GSE18842")+
  xlab(label="Number of selected genes")+
  ylab("Accuracy")+
  theme_light()
citation("randomForest")
