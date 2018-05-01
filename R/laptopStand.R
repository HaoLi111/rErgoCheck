#the adjust height of the back end calculation for adjustable laptop stands
#
#input: H-height in the population(listed as 'dimension')


adjustBackFromHeight<-function(H,
                               table = get("table",envir = .GlobalEnv),
                               laptop=get("laptop",envir = .GlobalEnv),
                               eye=get("eye",envir = .GlobalEnv)){
  heightDifference=H-table$h
  heightAllowedTilt=eye$distance*tand(-(eye$angleOptimum))
  heightDifference-heightAllowedTilt-laptop$h/2*cosd(laptop$screenTilt)#the center of the screen so /2
}
#-----------------------------------------------------------------------
#adjustAngleFromHeight
#adjust the laptop  stand tilt angle with respect to height
#
adjustAngleFromHeight<-function(H,
                                table = get("table",envir = .GlobalEnv),
                                stand=get("stand",envir = .GlobalEnv),
                                laptop=get("laptop",envir = .GlobalEnv),
                                eye=get("eye",envir = .GlobalEnv)){
  #backR=adjustBackFromHeight(H=H,table = table,laptop = laptop,eye= eye)-stand$front
  asind((adjustBackFromHeight(H=H,table = table,laptop = laptop,eye= eye)-stand$front)/laptop$h)#  this depends on the front of the laptop
}



