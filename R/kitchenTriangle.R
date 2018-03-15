

kitInt<-function(pa,pb,pc,asp = 1,newplot = T){
  Xa=pa[1]
  Xb=pb[1]
  Xc=pc[1]
  Ya=pa[2]
  Yb=pb[2]
  Yc=pc[2]
  if(newplot ==T){
    plot(c(Xa,Xb,Xc),c(Ya,Yb,Yc),
                      type="b",col="red",asp = asp)
  }else{
      points(c(Xa,Xb,Xc),c(Ya,Yb,Yc),
             type="b",col="red",asp = asp)
    }
  points(c(Xa,Xc),
         c(Ya,Yc),type="l",
         col="red")
}
kitRec<-function(l,w,tilt = F,depth = .6,aim = T,newplot = F,boundary.type = 'l',
                 boundary.col = "yellow",asp =1){
  if(tilt == T){
    ll<-w
    w<-l
    l<-ll
  }
  if(newplot == T){
    plot(c(0,w),c(0,l),type = 'n',asp = asp)
  }

  points(c(0,w,w,0,0),c(0,0,l,l,0),type = 'l')
  points(c(depth,w-depth,w-depth,depth,depth),c(depth,depth,l-depth,l-depth,depth),type = boundary.type,
         col = boundary.col)
}
kitTri<-function(pa,pb,pc){
  if (length(pa)==1){            #length of sides as value a,b,c
    a=pa
    b=pb
    c=pc
  }else{                        #position(xa,ya),(xb,yb),...
    a=sqrt(sum((pc-pb)^2))
    b=sqrt(sum((pa-pc)^2))
    c=sqrt(sum((pa-pb)^2))
  }
  if ((a+b)<=c | abs(a-b)>=c){
    Return("Not a Triangle")
  }
  pe=a+b+c                          #!!!perimetre
  p=pe/2
  area=sqrt(p*(p-a)*(p-b)*(p-c))       #!!!area
  if(pe>7.9){
    pchk="perimeter too large"
  }else if (pe<4.0){
    pchk="perimeter too small"
  }else{
    pchk="perimeter OK"
  }
  si=c(a,b,c)
  name=c("a","b","c")
  schk=NULL
  for (i in (1:3)){
    if (si[i]>(2.7)){
      schk[i]="too long"
    }else if(si[i]<1.2){
      schk[i]="too short"
    }else{
      schk[i]="OK"
    }
    schk[i]=paste("side",name[i],schk[i])
  }
  ds=1.95-si#Indicator of difference from the average equilateral
  est=sqrt(sum(ds)^2)#Deviation
  trilist=list("Side"=si,"Side.Check"=schk,"Perimeter"=pe,
               "Perimeter.Check"=pchk)
  geolist=list("Indicator"=ds,"Deviation"=est,"Working.Area"=area)
  return(list("Basic_Geom"=trilist,"Rating"=geolist,"Input"=list(pa,pb,pc)))
}
#--------------------------------------------------------------------
triSid<-function(pa,pb,pc) c(a=sqrt(sum((pc-pb)^2)),b=sqrt(sum((pa-pc)^2)),c=sqrt(sum((pa-pb)^2)))
triPar<-function(pa,pb,pc) sum(sqrt(sum((pc-pb)^2)),sqrt(sum((pa-pc)^2)),sqrt(sum((pa-pb)^2)))

kitTriVal<-function(pa,pb,pc,plot = FALSE,target = 'c', new.plot = FALSE){
  if(plot==FALSE){
    sl<-triSid(pa,pb,pc)
    pl<-triPar(pa,pb,pc)
    ifelse(sl[1]<2.7&sl[2]<2.7&sl[1]>1.2&sl[2]>1.2&sl[3]>1.2&sl[3]<2.7
           &sl[1]+sl[2]>sl[3]&abs(sl[1]-sl[1])<sl[3]&pl>4.0&pl<7.9,TRUE,FALSE)
  }else{
    if(target == 'a'|target =='A'){
      pa<-pc
    }else if (target == 'b'|target =='B'){
      pb<-pc
    }
    abline(v=c(pa[1],pb[1]),untf = F)
    abline(h=c(pa[2],pb[2]),untf=F)
    for (x in seq(from = 0, to = 5, by = .1)){
      for (y in seq(from = 0, to = 5, by = .1)){
        if (KitTriVal(pa,pb,c(x,y))==TRUE){
          points(x,y,col = 'green')
        }
      }
    }
  }
}



#plot(c(0,5),c(0,5),type = 'n')

kitVal_BF<-function(pa,pb){
  #points(c(pa[1],pb[1]),c(pa[2],pb[2]),col = 'dark red')
  abline(v=c(pa[1],pb[1]),untf = F)
  abline(h=c(pa[2],pb[2]),untf=F)
  for (x in seq(from = 0, to = 5, by = .1)){
    for (y in seq(from = 0, to = 5, by = .1)){
      if (kTriVal_c(pa,pb,c(x,y))==TRUE){
        points(x,y,col = 'green')
      }
    }
  }
}

#kitTriVal(c(0.3,2.1),c(1.5,1.5),c(3,2.1))
#plot(c(0,5),c(0,5),type = 'n')
#kitTriVal(c(0.3,2.1),c(1.5,1.5),c(3,2.1),plot = T)
#plot(c(0,5),c(0,5),type = 'n',asp = 1)
#kitTriVal(c(0.3,2.1),c(1.5,1.5),c(3,2.1),plot = T,target = 'a')
