#the adjust height of the back end calculation for adjustable laptop stands
#
#input: H-height in the population(listed as 'dimension')


adjustBackFromHeight<-function(H,
                               table = get("table",envir = .GlobalEnv),
                               stand=get("stand",envir = .GlobalEnv),
                               laptop=get("laptop",envir = .GlobalEnv),
                               eye=get("eye",envir = .GlobalEnv)){
  heightDifference=H-table$h
  heightAllowedTilt=eye$distance*tand(-(eye$angleOptimum))
  backR=heightDifference-heightAllowedTilt-laptop$h/2*cosd(laptop$screenTilt)
}
#-----------------------------------------------------------------------
#adjustAngleFromHeight
#adjust the laptop  stand tilt angle with respect to height
#
#
adjustAngleFromHeight<-function(H,
                                table = get("table",envir = .GlobalEnv),
                                stand=get("stand",envir = .GlobalEnv),
                                laptop=get("laptop",envir = .GlobalEnv),
                                eye=get("eye",envir = .GlobalEnv)){
  heightDifference=H-table$h
  heightAllowedTilt=eye$distance*tand(-(eye$angleOptimum))
  backR=heightDifference-heightAllowedTilt-stand$front-laptop$h/2*cosd(laptop$screenTilt)
  adjustAngle=atan((backR/stand$depth))*180/pi
  adjustAngle
}
