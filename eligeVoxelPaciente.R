eligeVoxelPaciente<-function(FLAIR){
  
  #Eliminamos los valores fondo
  
  seg_fast=fast(file = FLAIR,type = "T2",bias_correct = FALSE,verbose = TRUE)
  
  #seg = segme
  
  mascara_cereb = FLAIR>min(FLAIR)
  
  valores = FLAIR[mascara_cereb]
  
  #calculamos el umbral a partir del cual es probable que los vóxeles sean lesion
  
  umbral = quantile(valores,0.5)+0.75*IQR(valores)
  
  #calculamos las coordenadas de esos vóxeles
  
  cordis = which((seg_fast == 2 | seg_fast == 3) & (FLAIR>=umbral))
  
  return(cordis)
}