stand<-data.frame(depth = 245,
                  front=35,
                  backL=80,
                  backU=130)
h_standard_table<-(736.60+762.00)/2
table = data.frame(h=h_standard_table)
#
# assume 15 degree of screen backtilt
#
laptop = data.frame(h=110*2.54,
                    screenTilt=15)#11 inch height

#

plot_workspace_xz<-function(range = list(x = c(0,1000),
                                         y = c(0,2000)),
                            table = getAnywhere("table"),
                            position_stand = get(position_stand),
                            stand = getAnywhere("stand"),
                            laptop = getAnywhere("laptop"),
                            ){
  plot(c())
}
