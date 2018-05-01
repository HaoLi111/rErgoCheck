
#Xb=170 #range of the first side
#optimization of the kitchen working triangle design



#	PRELOAD GEOMETRIC
l2=function(L,c){
  l=sqrt(((L-c)/2)^2-(c/2)^2)
  return(l)
}
l1=function(L,c){
  l=(L-c)/2
  return(l)
}
#============================================
ellipse=function(x,centre.x,centre.y,L1,L2){
  d=(1-(x-centre.x)^2/L1^2)*(L2^2)
  y1=numeric(length(x))
  y=y1
  for (i in (1:length(d))){
    if (d[i]<=0) {
      y1[i]=0
    }else{
      y[i]=sqrt(d[i])
      y1[i]=centre.y+y[i]

      #y2[i]=centre.y-y[i]
    }
  }
  #return(rbind(y1,y2))
  return(y1)
}
circle=function(x,centre.x,centre.y,r){
  d=r^2-(x-centre.x)^2
  y1=numeric(length(x))
  y=y1
  for (i in (1:length(d)))
    if (d[i]<0){
      y1[i]=0
    }else{
      y[i]=sqrt(d[i])
      y1[i]=y[i]+centre.y
      #y2=-y[i]+centre.y
    }
  #return(rbind(y1,y2))
  return(y1)
}
area=function(x){
  a=sum(x[1,]-x[2,])}


kitRan<-function(Xb = 170,target =  'C',
                 low.side=120,
                 upper.side=270,
                 low.peri=400,
                 upper.peri=790){
  #	SET VALUE
  #the restriction of the working triangle
  #low.side=120      #low limit of each side is 120mm
  #upper.side=270    #upper limit of each side is 270mm
  #low.peri=400      #low limit of the parameter is 400mm
  #upper.peri=790    #upper limit of the parameter is 790mm
  #for each Xb
  base=((Xb-l1(upper.peri,Xb)):(Xb+l1(upper.peri,Xb)))/2
  range=matrix(0,2,length(base))
  range[1,]=ellipse(base,(Xb/2),0,l1(upper.peri,Xb),l2(upper.peri,Xb))#lrg
  y=circle(base,0,0,upper.side)        #large circles
  y2=circle(base,Xb,0,upper.side)
  y[y==0]=upper.peri                     #"throw out" the false points
  y2[y2==0]=upper.peri
  #=select minimum
  for (i in (1:length(range[1,]))){
    range[1,i]<-min(range[1,i],y[i],y2[i])
  }
  #small ellipse
  range[2,]=ellipse(base,(Xb/2),0,l1(low.peri,Xb),l2(low.peri,Xb))
  z=circle(base,0,0,low.side)
  z2=circle(base,Xb,0,low.side)
  for (i in (1:length(range[2,]))){
    range[2,i]<-max(range[2,i],z[i],z2[i])
  }
  base=rbind(base,base)
  #for (i in (1:(2*l1(upper.peri,Xb)))){
    #if (range[2*i-1] > range[2*i]){
    #  range=range[-c((2*i-1),2*i)]
    #  base=base[-c((2*i-1),2*i)]
    #}
  #}
  #base=t(base)
  #range=t(range)
  matplot(base,range,xlim=c(0,270),ylim=c(0,270),type="l",col='yellow',asp = 1)
  points(Xb,0,type='p',col='red')
  points(0,0,type='p',col='red')
  #
  abline(v =c(0,Xb), untf = FALSE)
  title(paste("The distribustion of ",target,
              " point with a known length of opposite side=",Xb))
}

#kitRan(Xb,"b")
