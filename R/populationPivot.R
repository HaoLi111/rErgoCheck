populationlPivot<-function(populationDf){
  require(dplyr)
  populationDf %>%
    group_by(sex) %>%
    summarize(Count = n(),MIN=min(dimension),LQT=quantile(dimension,.25),
              mean=mean(dimension),median=median(dimension),
              UQT=quantile(dimension,.75),
              MAX=max(dimension))
}
