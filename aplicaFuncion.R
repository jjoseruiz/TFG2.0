aplicaFuncion<-function(vecinos,listaFunciones,orden)
{
  #n es el número de imágenes
  vecinos_xvoxel = (2*orden+1)^3
  n = ncol(vecinos)/vecinos_xvoxel
  
  listaFeatures = matrix(nrow = nrow(vecinos),ncol = length(listaFunciones)*n)
    for(i in 1:nrow(vecinos)){
      if(i == round(nrow(vecinos)/2)){
        print("POR LA MITAD")
      }
      #print(paste0("funcion voxel-> ",i))
      lista = list()
      #variables auxiliares
      c=0
      sum = 0
      k=1
      #aplicamos la función j a la ventana iésimo
      #n es el número de imágenes
      #almacenamos en una lista. Cada posición de la lista corresponde con el conjunto de vecinos de la imagen n y vóxel iésimo
      for(p in 1:n){
        valuesk = list(vecinos[i,(c+1):(k*vecinos_xvoxel)])
        c=k*vecinos_xvoxel
        k=k+1
        #almacenamos en lista los vectores de los valores cada imagen correspondientes al vóxel iésimo
        lista = c(lista,valuesk)
      }

        for(j in 1:length(lista)){
          for(l in 1:length(listaFunciones)){
            featj = listaFunciones[[l]](lista[[j]])
            listaFeatures[i,l+sum]=featj
          }
          sum = length(listaFunciones)+sum
        }
    }
  return (listaFeatures)
}