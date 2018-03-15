#anthropometric MF
#input a population with number of males, females stated
#put a anthropometric data in the function
#with maleAvg
#maleVar
#femaleAvg
#femaleVar
#or put a list in the working environment
#naming it anthropometric + target(e.g. EyeHeightSitting/EyeHeightStanding)
#Using camel naming





anthropometricMF<-function(population = get("population",envir = .GlobalEnv),
                           anthropometric = "EyeHeightSitting",
                           plot = T,
                           engine = "ggplot2",
                           dataframe = T,
                           group=T){
  print(paste(anthropometric,"Random for",population))
  anthropometric<-get(paste("anthropometric",anthropometric,sep =""))

  male<-rnorm(population$male,anthropometric$maleAvg,sqrt(anthropometric$maleVar))#Avg eye height when sitting (Male) = 1232, variance = 127
  female<-rnorm(population$female,anthropometric$femaleAvg,sqrt(anthropometric$femaleVar))
  dimension<-c(male,female)
  Dimension<-data.frame(dimension,sex = rep(c("male","female"),c(population$male,population$female)))

  if(plot == T){
    if(engine == "ggplot2"){
      require(ggplot2)
      if(group ==T){
        g<-ggplot(data = Dimension,aes(x=dimension)) + geom_density(aes(group = sex,colour = sex,fill = sex),alpha = 0.2) +
          geom_rug(aes(group = sex,colour = sex),alpha = 0.05)
      }else{
        g<-ggplot(data = Dimension,aes(x=dimension)) + geom_density(aes(fill = sex),alpha = 0.2) +
          geom_rug(aes(group = sex,colour = sex),alpha = 0.05)
      }
      print(g)
    }else if(engine == "lattice"){
      densityplot(dimension)
    }else if(engine == "car"){
      require(car)
      densityPlot(dimension)
    }
  }

  if(dataframe == F){
    return(dimension)
  }else if(dataframe == T){
    return(Dimension)
  }
}
