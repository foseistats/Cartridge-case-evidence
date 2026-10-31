#database
cmc<-tar_read(cmcdata)

#crime scene data
tar_read(CrimeScenedata)

#cmc summary histogram
tar_read(cmcsummaryhist)

##strength of evidence

#two-stage histogram
tar_read(twostageH)


#slr
tar_read(slrdata)
#tar_read(slrplot1)


#Bayesian approach
tar_read(posteriorM)
tar_read(posteriorN)
tar_read(posteriorpred)
tar_read(posteriormprobs)


#bayes factor
tar_read(bayesmn1);tar_read(bayesmn2)


##### assessing performance
##Two-stage approach
tar_read(combined_plot)
tar_read(mean_accuracy);tar_read(mean_sensitivity);tar_read(mean_specificity)

## SLR
tar_read(mean_accuracy2);tar_read(mean_sensitivity2);tar_read(mean_specificity2)
tar_read(combined_plot2)

##BF
tar_read(mean_accuracy3);tar_read(mean_sensitivity3);tar_read(mean_specificity3)
tar_read(combined_plot3)





###############################
twostagetestset<-tar_read(combined_test_set)
slrtestset<-tar_read(combined_test_set2)
bftestset<-tar_read(combined_test_set3)

twostagetestset<-cbind(twostagetestset,Method=rep("Two-stage approach",1497))
slrtestset<-cbind(slrtestset,Method=rep("SLR",1497))
bftestset<-cbind(bftestset,Method=rep("Bayes factor",1497))



write.csv(twostagetestset,"twostagest.csv")

bftestset=rename(bftestset,"decision criteria"="bf")
bftestset$`decision criteria`<-exp(bftestset$`decision criteria`)

slrtestset=rename(slrtestset,"decision criteria"="slrval")
slrtestset$`decision criteria`<-exp(slrtestset$`decision criteria`)

twostagetestset<-read.csv("twostagest.csv")
twostagetestset<-twostagetestset[,-1]
twostagetestset=rename(twostagetestset,"decision criteria"="decision.criteria")


alltestsets<-rbind(twostagetestset,slrtestset,bftestset)
alltestsets->alltestsets2
alltestsets2$Method<-factor(alltestsets2$Method,levels = c("Two-stage approach","SLR","Bayes factor"))

#ggplot(alltestsets2, aes(x=MatchorNot, y = `decision criteria`)) +
 # geom_boxplot() + 
  #geom_jitter(aes(color=barrel_info),width = 0.2, alpha = 0.5) +
  #scale_color_manual(values = rainbow(length(unique(alltestsets2$barrel_info)))) +
 # theme(legend.position = "right") +
 # theme_bw() +
 # xlab("Ground Truth")+
 # labs(color="Barrel") +
 # geom_hline(aes(yintercept = yintercept), linetype = "dashed", color = "red")+
 # ggh4x::facet_grid2(.~Method, scales = "free_y", independent = "y") +
 # scale_y_log10()



yintercepts <- data.frame(
  Method = c("Two-stage approach", "SLR", "Bayes factor"),
  yintercept = c(6, 1, 1)
)

alltestsets3 <- merge(alltestsets2, yintercepts, by = "Method")

ggplot(alltestsets3, aes(x=MatchorNot, y = `decision criteria`)) +
  geom_boxplot() + 
  geom_jitter(aes(color=barrel_info),width = 0.2, alpha = 0.5) +
  scale_color_manual(values = rainbow(length(unique(alltestsets3$barrel_info)))) +
  theme(legend.position = "right") +
  theme_bw() +
  xlab("Ground Truth")+
  labs(color="Barrel") +
  geom_hline(aes(yintercept = yintercept), linetype = "dashed", color = "black")+
  ggh4x::facet_grid2(.~Method, scales = "free_y", independent = "y") +
  scale_y_log10()+
  ylab("Decision Criteria")
