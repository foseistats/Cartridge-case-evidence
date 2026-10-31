cmcnew %>%ggplot(aes(x=originalMethod,y=..density..,fill = MatchorNot)) +
  geom_histogram(binwidth = 1)+xlab("CMC")+
  theme_minimal()+ggtitle("")+ scale_fill_manual(values = c("match" = "blue", "non-match" = "red"),
                      name = "",labels=c("match" = "M",
                                         "non-match" = "N"))



aggregate(data=cmc,cmc~MatchorNot, mean)[1,2]

vv=data.frame(CS=c("CS-1","CS-2","CS-3","CS-4","CS-5"),
           slr=c(log(case1),log(case2),log(case3),
                 log(case4),log(case5)))
