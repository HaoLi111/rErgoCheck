#Random Sample Generation
#
ranAdu<-function(m=NA,f=NA,target = "height_eyeLevel_sit", distribution = "Normal"){
  #The following data come from
  #https://www.lifewire.com/guide-to-laptop-size-and-weight-832312
  height_eyeLevel_sit
  ans<-get(target)
  if (!is.na(m)) m<-rnorm(m,ans$m_mean,ans$m_sd)
  if (!is.na(f)) f<-rnorm(f,ans$f_mean,ans$f_sd)

}
