recorreImagenes<-function(IMAGENES,COORDENADAS,MASK,orden)
{
  vecinos_xvoxel = (2*orden+1)^3
  #las IMAGENES VENDRAN EN UNA LISTA 
  #Las coordenadas serán una matriz de coordenadas
  bol=is.matrix(COORDENADAS)
  if(bol){
    mat = matrix(nrow = nrow(COORDENADAS),ncol = length(IMAGENES)*vecinos_xvoxel)
  }else{
    mat = matrix(nrow = length(COORDENADAS),ncol = length(IMAGENES)*vecinos_xvoxel)
  }
  l=0
  for(i in 1:length(IMAGENES)){
    print(paste0("imagen ",i))
    valores = valoresImagen(IMAGENES[[i]],COORDENADAS,orden)
    mat[1:nrow(valores),(1+l):(l+ncol(valores))] = valores
    l=l+ncol(valores)
  }
  return(mat)
}