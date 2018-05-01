#Extract 2 key variables h_standard
stand<-data.frame(depth = NULL,
                  front=NULL,
                  backL=NULL,
                  backU=NULL)


table = data.frame(h=(736.60+762.00)/2)

laptop = data.frame(h=110*2.54,
                    screenTilt=15)#11 inch height

eye = data.frame(angleOptimum = -10,distance = 500)

#all CA with target variable: tilt,
#                             front
CA_stand_eye<-function(H,
                       eye = getAnywhere('eye'),
                       laptop = getAnywhere('laptop'),
                       stand = getAnywhere('stand')){
  adFromHeight()
}
