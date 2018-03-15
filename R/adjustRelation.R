#adjust relation
#Estmating how certain diammeter of certain part of the design must be adjusted to fit
#the population stated in the dataset
#input: targetUser:  a vector or dataset of the population
#       required: the relation function of the customized value with respect  to anthropometry
#output: a dataset with the original appended with the adjust diammeter


adjustRelation<-function(targetUser,required,plot = T){
  if(!is.data.frame(targetUser)) targetUser<-data.frame(targetUser)
  adjust_relation<-within(targetUser,{result=sapply(targetUser$dimension, required)})
  if(plot == T)
    plot(adjust_relation)
  return(adjust_relation)
}
