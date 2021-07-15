SEM<-function(subject,measurement,observer,value){
  fit<-aov(value~1+factor(observer)+factor(subject)+factor(observer)*factor(subject))
  fita<-anova(fit)
  delta2error<-fita$`Mean Sq`[4]
  delta2observer<-(fita$`Mean Sq`[1]-fita$`Mean Sq`[3])/(length(unique(subject))*length(unique(measurement)))
  delta2SO<-(fita$`Mean Sq`[3]-fita$`Mean Sq`[4])/length(unique(measurement))
  SEMintra<-sqrt(delta2error)
  SEMinter.fixed<-sqrt(delta2error+delta2SO)
  SEMinter.random<-sqrt(delta2error+delta2observer+delta2SO)

  rst<-data.frame(SEMintra,SEMinter.fixed,SEMinter.random)
  row.names(rst)<-""
  return(rst)
}
