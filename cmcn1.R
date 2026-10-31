###load data




### whole data

barrel.w<-function(x){
  b.out=cmcnew
  means.b=aggregate(originalMethod~MatchorNot,data = b.out,FUN = mean)
  pm=means.b[1,2]
  pnm=means.b[2,2]
  return(list(pm/x,pnm/x))
}

barrel.w(64)


ggplot(cmcnew, aes(x = originalMethod, fill = MatchorNot)) +
  geom_histogram(aes(y = ..density..,group=MatchorNot), bins = 20,
                 position = "identity", alpha = 0.7) +
  ylim(0,1)+theme_bw() 





# Calculate binomial probabilities for unique values of the CMC 
cmcval=unique(cmcnew$originalMethod)
new.data<-data.frame(cmcval)
new.data<-new.data %>% mutate(MatchorNot=ifelse(cmcval<6,"non-match","match"))
probabilities <- new.data %>%
  group_by(MatchorNot, cmcval) %>%
  summarize(prob = dbinom(cmcval, size = 64, 
                          prob = ifelse(unique(MatchorNot) == "match",
                                                           0.219494, 0.005927476)))


# Create a histogram with binomial probabilities using ggplot2
ggplot(cmcnew, aes(x =originalMethod, fill = MatchorNot)) +
  geom_histogram(aes(y = ..density..), bins = 20, position = "identity", 
                 alpha = 0.7, color = "white") +
  geom_line(data = probabilities, aes(x = cmcval, y = prob, group = MatchorNot, 
                                      color = MatchorNot), size = 1) +
  labs(title = "Histogram with Binomial Probabilities Overlay",
       x = "CMC_COUNT", y = "Density", color = "MatchorNot") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal()







#### Calculate beta-binomial probabilities for unique values of the CMC,N=64 
library(extraDistr)

probabilities1 <- new.data %>%
  group_by(MatchorNot, cmcval) %>%
  summarize(prob = dbbinom(cmcval, 
                           size = 64, 
                           alpha= ifelse(unique(MatchorNot) == "match", 
                                         22.10253, 
                                         11.21317),
                           beta=ifelse(unique(MatchorNot) == "match", 
                                       78.60097, 
                                       1880.30312)))


ggplot(cmcnew, aes(x =originalMethod, fill = MatchorNot)) +
  geom_histogram(aes(y = ..density..), bins = 20, position = "identity",
                 alpha = 0.7, color = "white") +
  geom_line(data = probabilities1, aes(x = cmcval, y = prob, group = MatchorNot,
                                       color = MatchorNot), size = 1) +
  labs(title = "Histogram with Beta Binomial Probabilities Overlay",
       x = "CMC_COUNT", y = "Density", color = "MatchorNot") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal()















##### parameter estimation of "p" for removing a barrel
N=64

barrel.p<-function(x){
  b.out=cmcnew %>% filter(refBarrel!=x,targBarrel!=x)
  means.b=aggregate(originalMethod~MatchorNot,data = b.out,FUN = mean)
  pm=means.b[1,2]
  pnm=means.b[2,2]
  
  
  return(list(pm/N,pnm/N))
}
  
  barrel.p(1)
  
  

 
  
  
  
  
   
  
########## histogram for removing a barrel
  barrel.hist<-function(x){
    b.out=cmcnew %>% filter(refBarrel!=x,targBarrel!=x)
    b.hist=b.out%>%ggplot(aes(x=originalMethod,
                              y=..density..,fill = MatchorNot)) +
      geom_histogram(binwidth = 1)+xlab("CMC Count")+theme_bw()
    return(b.hist)
  }
  
barrel.hist(1)
  


#####################################################################

  


# histograms for removing a cartridge.
library(sqldf)
library(dplyr)
library(ggplot2)


c.out=sqldf("select comparisonName,originalMethod, MatchorNot 
            from cmcnew 
            where comparisonName not like 'Fadul 1-1%' 
            and comparisonName not  like '%Fadul 1-1'  ")


cmcval=unique(c.out$originalMethod)
new.data<-data.frame(cmcval)
new.data<-new.data %>% mutate(MatchorNot=ifelse(cmcval<6,"non-match","match"))

barrel.w<-function(x){
  means.b=aggregate(originalMethod~MatchorNot,data = c.out,FUN = mean)
  pm=means.b[1,2]/x
  pnm=means.b[2,2]/x
  cmcval=unique(c.out$originalMethod)
  new.data<-data.frame(cmcval)
  new.data<-new.data %>% mutate(MatchorNot=ifelse(cmcval<6,"non-match","match"))
  probabilities <- new.data %>%
    group_by(MatchorNot, cmcval) %>%
    summarize(prob = dbinom(cmcval, size = 64, 
                            prob = ifelse(unique(MatchorNot) == "match",
                                          pm, pnm)))
  
  
  # Create a histogram with binomial probabilities using ggplot2
 plot1= ggplot(c.out, aes(x =originalMethod, fill = MatchorNot)) +
    geom_histogram(aes(y = ..density..), bins = 20, position = "identity", 
                   alpha = 0.7, color = "white") +
    geom_line(data = probabilities, aes(x = cmcval, y = prob, group = MatchorNot, 
                                        color = MatchorNot), size = 1) +
    labs(title = "Histogram with Binomial Probabilities Overlay",
         x = "CMC_COUNT", y = "Density", color = "MatchorNot") +
    scale_fill_manual(values = c("lightblue", "lightgreen")) +
    theme_minimal()
  return(plot1)
}




barrel.w(64)





#################### beta binomial



##### mles for beta binomial distribution

c.out=sqldf("select comparisonName,originalMethod, MatchorNot 
            from cmcnew 
            where comparisonName not like 'Fadul 1-1%' 
            and comparisonName not  like '%Fadul 1-1'  ")


km.scores<-c.out %>% filter(MatchorNot=="match") %>% select(originalMethod)
knm.scores<-c.out%>% filter(MatchorNot=="non-match") %>% select(originalMethod)



# Likelihood function for beta-binomial distribution
log_likelihood <- function(parameters, data) {
  alpha <- exp(parameters[1])
  beta <- exp(parameters[2])
  n <- 64
  
  
  # Beta-binomial likelihood function
  likelihood_values <- lgamma(n + 1) - 
    lgamma(data + 1) -
    lgamma(n - data + 1) +
    
    lgamma(data + alpha) + 
    lgamma(n - data + beta) -
    lgamma(n + alpha + beta) + 
    
    lgamma(alpha + beta) -
    lgamma(alpha) -
    lgamma(beta)
  
  
  # Take the log-likelihood to avoid numerical issues
  log_likelihood <- sum(likelihood_values)
  
  return(log_likelihood)  # Negative log-likelihood for minimization
}

# Initial guess for parameters
initial_parameters <- c(alpha = 0.5, beta = 0.5)

# Optimize using the likelihood function
result <- optim(par = log(initial_parameters), 
                fn = log_likelihood, 
                data = km.scores,
                control = list(fnscale = -1))
result1 <- optim(par = log(initial_parameters), 
                 fn = log_likelihood, 
                 data = knm.scores,
                 control = list(fnscale = -1)
)

# MLEs of alpha and beta
mle_alpha <- exp(result$par[1])
mle_beta <- exp(result$par[2])

mle_alpha1 <- exp(result1$par[1])
mle_beta1 <- exp(result1$par[2])




#### Calculate beta-binomial probabilities for unique values of the CMC,N=64 
library(extraDistr)
cmcval=unique(c.out$originalMethod)
new.data<-data.frame(cmcval)
new.data<-new.data %>% mutate(MatchorNot=ifelse(cmcval<6,"non-match","match"))


probabilities1 <- new.data %>%
  group_by(MatchorNot, cmcval) %>%
  summarize(prob = dbbinom(cmcval, 
                           size = 64, 
                           alpha= ifelse(unique(MatchorNot) == "match", 
                                         mle_alpha, 
                                         mle_alpha1),
                           beta=ifelse(unique(MatchorNot) == "match", 
                                       mle_beta, 
                                       mle_beta1)))


ggplot(c.out, aes(x =originalMethod, fill = MatchorNot)) +
  geom_histogram(aes(y = ..density..), bins = 20, position = "identity",
                 alpha = 0.7, color = "white") +
  geom_line(data = probabilities1, aes(x = cmcval, y = prob, group = MatchorNot,
                                       color = MatchorNot), size = 1) +
  labs(title = "Histogram with Beta Binomial Probabilities Overlay",
       x = "CMC_COUNT", y = "Density", color = "MatchorNot") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal()






################ all cartridges
cartridge<-unique(cmcnew$reference)


################ cartridges and barrels
aggregate(reference~refBarrel,data=cmcnew,FUN=unique) 

aggregate(target~targBarrel,data=cmcnew,FUN=unique) 


############## cmc for held-out cartridge vrs other cartridges.

heldout.cmc<- sqldf("select comparisonName, originalMethod,MatchorNot
      from cmcnew
      where comparisonName like 'Fadul 1-1%' or
      comparisonName like '%Fadul 1-1'")


################# sensitivity and specificity

heldout.cmc<-heldout.cmc %>% 
  mutate(MatchperThres=ifelse(originalMethod<=4,"non-match","match"))

heldout.cmc$MatchorNot<-as.factor(heldout.cmc$MatchorNot)
heldout.cmc$MatchperThres<-as.factor(heldout.cmc$MatchperThres)

library(caret)
confusionMatrix(data=heldout.cmc$MatchperThres,
                reference = heldout.cmc$MatchorNot)




####################
ss.dat<-data.frame(sensitivity=c(rep(1,40)),specificity=c(rep(1,40)))
