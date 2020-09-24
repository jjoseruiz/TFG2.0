correccion<-function(IMG){
  IMG[IMG<0]=0
  print("Correcion N4")
  #Aplicamos homgenización de la imagen para evitar artefactor como sombras.
  bf_t1=extrantsr::bias_correct(file=IMG,correction = "N4")
  print("correccion finalizada")
  return (bf_t1)
}