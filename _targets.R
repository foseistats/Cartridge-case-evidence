# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble","ggplot2","extraDistr","dplyr","gridExtra","sqldf") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("functions.R")
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(cmcdata,read.csv("cmcdataset.csv") #database
  ),
  tar_target(CrimeScenedata,data.frame(CS=c("CS-1","CS-1"),
                                       S=c("CS-2a","CS-2b"),CMC=c(3,7))),
  tar_target(cmcsummaryhist,cmchist1(cmcdata)#cmc summary hist
             ),
  #Two-stage approach
  tar_target(twostageH,twostageHist(cmcdata)),
  #SLR
  tar_target(thetam,(aggregate(data=cmcdata,cmc~MatchorNot, mean)[1,2])/64),
       tar_target(thetan,(aggregate(data=cmcdata,cmc~MatchorNot, mean)[2,2])/64),
  tar_target(case1,log(slrfunc(3,64,thetam,thetan))), #CS-1 v CS-2a
  tar_target(case2,log(slrfunc(7,64,thetam,thetan))), #CS-1 v CS-2b
  tar_target(slrdata,data.frame(CS=c("CS-2a","CS-b2"),
                               slr=c(case1,case2))),#slrdata
 # tar_target(slrplot1,slrplot(slrdata)), #slrplot
#Bayesian approach
 tar_target(Match,cmcdata %>% select(cmc,MatchorNot) %>% filter(MatchorNot=="match")),
tar_target(Nonmatch,cmcdata %>% select(cmc,MatchorNot) %>% filter(MatchorNot=="non-match")),
tar_target(a,1/2+sum(Match$cmc)),
tar_target(b,1/2+sum(64-Match$cmc)),
tar_target(x_values1, seq(0,0.6 , length.out = 1000)),#prior
tar_target(density_values1, dbeta(x_values1, shape1 = 1/2, shape2 = 1/2)),#prior
tar_target(x_values2, seq(0, 0.6, length.out = 1000)),#thetam
tar_target(density_values2, dbeta(x_values2, shape1 = a, shape2 = b)),#thetam
tar_target(datam, data.frame(x = x_values2, density = density_values2)),#thetam
tar_target(posteriorM,ppthetam(datam,x_values1,density_values1)),#thetam
tar_target(c,1/2+sum(Nonmatch$cmc)),
tar_target(d,1/2+sum(64-Nonmatch$cmc)),
tar_target(x_values3, seq(0, 0.01, length.out = 1000)),
tar_target(density_values3, dbeta(x_values3, shape1 = c, shape2 = d)),
tar_target(datan, data.frame(x = x_values3, density = density_values3)),
tar_target(x_values4,seq(0, 0.01, length.out = 1000)),
tar_target(density_values4, dbeta(x_values4, shape1 = 1/2, shape2 = 1/2)),
tar_target(posteriorN,ppthetan(datan,x_values4,density_values4)),#thetan
##posterior predictive
tar_target(x_valuespd, c(0:40)),
tar_target(density_valuespdm, dbbinom(x_valuespd,size=64, alpha = a, beta=b)),
tar_target(datapdm, data.frame(x = x_valuespd, density = density_valuespdm)),
tar_target(density_valuespdn, dbbinom(x_valuespd,size=64, alpha = c, beta=d)),
tar_target(posteriorpred,postpredm(datapdm,x_valuespd,density_valuespdn)),
##posterior model probabilities
tar_target(d1 ,expand.grid(count = c(3,7), pm = seq(0, 1, length = 1001)) |>
  mutate(postm = posteriorm.prob(count, pm, 1-pm,a,b,c,d),count = factor(count))),
tar_target(posteriormprobs,posteriorMdprobs(d1)),
##Bayes factor
tar_target(bayesmn1,log(bayesfse(3,64,a,b,c,d))),#case 1
tar_target(bayesmn2,log(bayesfse(7,64,a,b,c,d))), # case 2

#Assessing performance
##two-stage approach
  tar_target(
    unique_barrels, 
    unique(c(cmcdata$refBarrel, cmcdata$targBarrel))
  ),
tar_target(unique_barrels1,unique_barrels[order(unique_barrels)]),
  tar_target(
    cross_validation_results,
    {
      sensitivity_list <- c()
      specificity_list <- c()
      accuracy_list <- c()
      combined_test_set <- data.frame()
      
      for (barrel in unique_barrels1) {
        test_set <- cmcdata %>% filter(refBarrel == barrel | targBarrel == barrel)
        train_set <- cmcdata %>% filter(!(refBarrel == barrel | targBarrel == barrel))
        
        test_set <- test_set %>%
          mutate(prediction = ifelse(cmc > 6, "match", "non-match"),
                 barrel_info = as.factor(paste( barrel)))
        
        true_positives <- sum(test_set$prediction == "match" & test_set$MatchorNot == "match")
        true_negatives <- sum(test_set$prediction == "non-match" & test_set$MatchorNot == "non-match")
        false_positives <- sum(test_set$prediction == "match" & test_set$MatchorNot == "non-match")
        false_negatives <- sum(test_set$prediction == "non-match" & test_set$MatchorNot == "match")
        
        sensitivity <- true_positives / (true_positives + false_negatives)
        specificity <- true_negatives / (true_negatives + false_positives)
        accuracy <- (true_positives + true_negatives) / nrow(test_set)
        
        sensitivity_list <- c(sensitivity_list, sensitivity)
        specificity_list <- c(specificity_list, specificity)
        accuracy_list <- c(accuracy_list, accuracy)
        
        combined_test_set <- rbind(combined_test_set, test_set)
      }
      list(
        combined_test_set = combined_test_set,
        sensitivity_list = sensitivity_list,
        specificity_list = specificity_list,
        accuracy_list = accuracy_list
      )
    }
  ),
  tar_target(
    combined_test_set,
    cross_validation_results$combined_test_set
  ),
  tar_target(
    sensitivity_list,
    cross_validation_results$sensitivity_list
  ),
  tar_target(
    specificity_list,
    cross_validation_results$specificity_list
  ),
  tar_target(
    accuracy_list,
    cross_validation_results$accuracy_list
  ),
  tar_target(
    mean_sensitivity,
    mean(sensitivity_list, na.rm = TRUE)
  ),
  tar_target(
    mean_specificity,
    mean(specificity_list, na.rm = TRUE)
  ),
  tar_target(
    mean_accuracy,
    mean(accuracy_list, na.rm = TRUE)
  ),
  tar_target(
    combined_plot,
    {
      set.seed(123)
      num_colors <- length(levels(combined_test_set$barrel_info))
      colors <- rainbow(num_colors)
      ggplot(combined_test_set, aes(x = MatchorNot, y = cmc)) +geom_boxplot()+
        geom_jitter(aes(color = barrel_info), width = 0.2, alpha = 0.5) +
        scale_color_manual(values = colors) +
        labs(color = "Barrel") +
        theme(legend.position = "right")+theme_bw() +xlab("Ground Truth")+
        ylab("CMC")+geom_hline(yintercept = 6)
    }
  ),



#### Score-base Likelihood ratio

tar_target(
  cross_validation_results2,
  {
    sensitivity_list <- c()
    specificity_list <- c()
    accuracy_list <- c()
    combined_test_set <- data.frame()
    
    for (barrel in unique_barrels1) {
      test_set <- cmcdata %>% filter(refBarrel == barrel | targBarrel == barrel)
      train_set <- cmcdata %>% filter(!(refBarrel == barrel | targBarrel == barrel))
      
      thetam<-aggregate(data=train_set,cmc~MatchorNot, mean)[1,2]/64
      thetan<-aggregate(data=train_set,cmc~MatchorNot, mean)[2,2]/64
      slrvalues<-log(slrfunc(test_set$cmc,64,thetam,thetan))
      
      test_set <- test_set %>%
        mutate(slrval=slrvalues,prediction = ifelse(slrvalues > 1, "match", "non-match"),
               barrel_info = as.factor(paste( barrel)))
      
      true_positives <- sum(test_set$prediction == "match" & test_set$MatchorNot == "match")
      true_negatives <- sum(test_set$prediction == "non-match" & test_set$MatchorNot == "non-match")
      false_positives <- sum(test_set$prediction == "match" & test_set$MatchorNot == "non-match")
      false_negatives <- sum(test_set$prediction == "non-match" & test_set$MatchorNot == "match")
      
      sensitivity <- true_positives / (true_positives + false_negatives)
      specificity <- true_negatives / (true_negatives + false_positives)
      accuracy <- (true_positives + true_negatives) / nrow(test_set)
      
      sensitivity_list <- c(sensitivity_list, sensitivity)
      specificity_list <- c(specificity_list, specificity)
      accuracy_list <- c(accuracy_list, accuracy)
      
      combined_test_set <- rbind(combined_test_set, test_set)
    }
    list(
      combined_test_set = combined_test_set,
      sensitivity_list = sensitivity_list,
      specificity_list = specificity_list,
      accuracy_list = accuracy_list
    )
  }
),
tar_target(
  combined_test_set2,
  cross_validation_results2$combined_test_set
),
tar_target(
  sensitivity_list2,
  cross_validation_results2$sensitivity_list
),
tar_target(
  specificity_list2,
  cross_validation_results2$specificity_list
),
tar_target(
  accuracy_list2,
  cross_validation_results2$accuracy_list
),
tar_target(
  mean_sensitivity2,
  mean(sensitivity_list2, na.rm = TRUE)
),
tar_target(
  mean_specificity2,
  mean(specificity_list2, na.rm = TRUE)
),
tar_target(
  mean_accuracy2,
  mean(accuracy_list2, na.rm = TRUE)
),
tar_target(
  combined_plot2,
  {
    set.seed(124)
    num_colors <- length(levels(combined_test_set2$barrel_info))
    colors <- rainbow(num_colors)
    ggplot(combined_test_set2, aes(x = MatchorNot, y = slrval)) +geom_boxplot()+
      geom_jitter(aes(color = barrel_info), width = 0.2, alpha = 0.5) +
      scale_color_manual(values = colors) +
      labs(color = "Barrel") +
      theme(legend.position = "right")+theme_bw() +xlab("Ground Truth")+
      ylab("log(SLR)")+geom_hline(yintercept = 0)
  }
),



####Bayes factor

tar_target(
  cross_validation_results3,
  {
    sensitivity_list <- c()
    specificity_list <- c()
    accuracy_list <- c()
    combined_test_set <- data.frame()
    
    for (barrel in unique_barrels1) {
      test_set <- cmcdata %>% filter(refBarrel == barrel | targBarrel == barrel)
      train_set <- cmcdata %>% filter(!(refBarrel == barrel | targBarrel == barrel))
      
      match<- train_set %>% select(cmc,MatchorNot) %>% filter(MatchorNot=="match")
      nonmatch<-train_set %>% select(cmc,MatchorNot) %>% filter(MatchorNot=="non-match")
 bayesfactor<-log((dbbinom(test_set$cmc,64,0.5+sum(match$cmc),0.5+sum(64-match$cmc)))/
      (dbbinom(test_set$cmc,64,0.5+sum(nonmatch$cmc),0.5+sum(64-nonmatch$cmc))))
      
      
      test_set <- test_set %>%
        mutate(bf=bayesfactor,prediction = ifelse(bayesfactor > 1, "match", "non-match"),
               barrel_info = as.factor(paste( barrel)))
      
      true_positives <- sum(test_set$prediction == "match" & test_set$MatchorNot == "match")
      true_negatives <- sum(test_set$prediction == "non-match" & test_set$MatchorNot == "non-match")
      false_positives <- sum(test_set$prediction == "match" & test_set$MatchorNot == "non-match")
      false_negatives <- sum(test_set$prediction == "non-match" & test_set$MatchorNot == "match")
      
      sensitivity <- true_positives / (true_positives + false_negatives)
      specificity <- true_negatives / (true_negatives + false_positives)
      accuracy <- (true_positives + true_negatives) / nrow(test_set)
      
      sensitivity_list <- c(sensitivity_list, sensitivity)
      specificity_list <- c(specificity_list, specificity)
      accuracy_list <- c(accuracy_list, accuracy)
      
      combined_test_set <- rbind(combined_test_set, test_set)
    }
    list(
      combined_test_set = combined_test_set,
      sensitivity_list = sensitivity_list,
      specificity_list = specificity_list,
      accuracy_list = accuracy_list
    )
  }
),
tar_target(
  combined_test_set3,
  cross_validation_results3$combined_test_set
),
tar_target(
  sensitivity_list3,
  cross_validation_results3$sensitivity_list
),
tar_target(
  specificity_list3,
  cross_validation_results3$specificity_list
),
tar_target(
  accuracy_list3,
  cross_validation_results3$accuracy_list
),
tar_target(
  mean_sensitivity3,
  mean(sensitivity_list3, na.rm = TRUE)
),
tar_target(
  mean_specificity3,
  mean(specificity_list3, na.rm = TRUE)
),
tar_target(
  mean_accuracy3,
  mean(accuracy_list3, na.rm = TRUE)
),
tar_target(
  combined_plot3,
  {
    set.seed(128)
    num_colors <- length(levels(combined_test_set3$barrel_info))
    colors <- rainbow(num_colors)
    ggplot(combined_test_set3, aes(x = MatchorNot, y = bf)) +geom_boxplot()+
      geom_jitter(aes(color = barrel_info), width = 0.2, alpha = 0.5) +
      scale_color_manual(values = colors) +
      labs(color = "Barrel") +
      theme(legend.position = "right")+theme_bw() +xlab("Ground Truth")+
      ylab("log(BF)")+geom_hline(yintercept = 0)
  }
),
#####plot of metrics
tar_target(two_stagemetricsdata,data.frame(Method=rep("Two-stage approach",3),
                      Metric=c("sensitvity","specificity","accuracy"),
              value=c(mean_sensitivity,mean_specificity,mean_accuracy))),
tar_target(slrmetricsdata,data.frame(Method=rep("SLR",3),
                      Metric=c("sensitvity","specificity","accuracy"),
                value=c(mean_sensitivity2,mean_specificity2,mean_accuracy2))),
tar_target(BFmetricsdata,data.frame(Method=rep("Bayes Factor",3),
                               Metric=c("sensitvity","specificity","accuracy"),
                  value=c(mean_sensitivity3,mean_specificity3,mean_accuracy3))),
tar_target(metricvalues,rbind(two_stagemetricsdata,slrmetricsdata,BFmetricsdata)),
tar_target(metrics_plot,ggplot(metricvalues, aes(x = Method, y = value, group =Metric , color = Metric)) +
             geom_point(shape=18,size=3) +
             geom_line() +
             labs(title = "",
                  x = "Method",
                  y = "Average proportion",
                  color = "Metric")+theme_bw())
)



