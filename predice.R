predice<-function(MODELO,DATOS){
  DATOS = as.data.frame(DATOS)
  DATOS[,ncol(DATOS)]=factor(DATOS$LESION)
  prediModelo=c(predict(MODELO,DATOS))-1
  #devuelve un vector de tipo factor indicando que vóxeles son sano y que voxeles son lesion
  return(prediModelo)
}