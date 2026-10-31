
#data overview histogram
cmchist1<-function(x)
  {x %>%ggplot(aes(x=cmc,y=..density..,fill = MatchorNot)) +
  geom_histogram(binwidth = 1)+xlab("CMC")+
  theme_minimal()+ggtitle("")+ scale_fill_manual(values = c("match" = "blue",
                                                        "non-match" = "red"),
                                            name = "",labels=c("match" = "Match",
                                                       "non-match" = "Non-match"))
}


# two-stage approach
twostageHist<-function(x)
{x %>%ggplot(aes(x=cmc,y=..density..,fill = MatchorNot)) +
    geom_histogram(binwidth = 1)+xlab("CMC")+
    theme_bw()+ggtitle("")+ scale_fill_manual(values = c("match" = "blue",
                                                        "non-match" = "red"),
                                          name = "",labels=c("match" = "Match",
                                                      "non-match" = "Non-match"))+
    geom_vline(xintercept = 6, linetype = "dashed", color = "green") +
  annotate("text", x = 4, y = Inf, label = "threshold ", vjust = 1.5, 
           color = "black") 
    
    
}



#Score-base Likelihood Ratios
slrfunc<-function(y,t_nc,theta_m,theta_n){
  (dbinom(x=y,size = t_nc, prob=theta_m))/(dbinom(x=y,size = t_nc,prob = theta_n))
}

#slrplot<-function(d){
#  ggplot(data=d,aes(x=CS,y=slr))+
 #   geom_point(size=2,color="black")+theme_bw()+ylab("log(slr)")+
 #   xlab("Crime Scene Cartridge")
#}




#Bayesian approach
##posterior and prior for theta_m


ppthetam<-function(m,n,o){
  ggplot(m, aes(x = x, y = density)) +
  geom_line(aes(color="posterior")) +
   labs(title = "",
       x = expression(theta[M]),
       y = "Density")+theme_bw() +xlim(0,0.6)+
    geom_line(aes(x=n,y=o,color="prior"))+
    guides(color = guide_legend(title = ""))
}

ppthetan<-function(m,n,o){
  ggplot(m, aes(x = x, y = density)) +
    geom_line(aes(color="posterior")) +
    labs(title = "",
         x = expression(theta[N]),
         y = "Density")+theme_bw()+
    geom_line(aes(x=n,y=o,color="prior"))+
    guides(color = guide_legend(title = ""))
}


## posterior predictive
postpredm<-function(m,n,o){
  ggplot(m, aes(x = x, y = density)) +
  geom_line(aes(color="M")) +
  labs(title = "",
       x = "CMC",
       y = "Density")+theme_bw()+ylim(0,0.8)+xlim(0,40)+
    geom_line(aes(n,o,color="N")) +
    guides(color = guide_legend(title = ""))
  
}


## posterior model probabilities
posteriorm.prob=function(cmc.val,PM,PNM=NULL,am,bm,cn,dn, data = NULL){
  (dbbinom(cmc.val,size = 64,alpha = am,beta =bm )*PM)/
    ((dbbinom(cmc.val,size = 64, alpha = am,beta = bm)*PM)+
       dbbinom(cmc.val,size = 64,alpha = cn,
               beta = dn)*(1-PM))
}                               


posteriorMdprobs<-function(x){
  ggplot(x, aes(x = pm, y = postm, 
               color = count, group = count)) + 
  geom_line() +
  labs(title = "",
       x = "P(M)",
       y = expression(paste("P(","M","|",y^s,",",Y^M,")"))) +
  theme_bw()+guides(color = guide_legend(title = "CMC"))

}


## Bayes factor
bayesfse<-function(ys,t_nc,x,y,w,z){
  (dbbinom(ys,t_nc,x,y))/
  (dbbinom(ys,t_nc,w,z))

}
