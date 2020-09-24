valoresImagen<-function(IMAGEN,COORDENADAS,orden){
  #vecinos_xvoxel vecinos por 3 coordenadas cada uno.
  vecinos_xvoxel=(2*orden+1)^3
  if(!is.antsImage(IMAGEN)){
    imagenants = as.antsImage(IMAGEN)
  }else{
    imagenants = IMAGEN
  }
  if(is.matrix(COORDENADAS)){
    coord = matrix(nrow = nrow(COORDENADAS),ncol = vecinos_xvoxel,byrow = TRUE)
    for(i in 1:nrow(COORDENADAS)){
      ventana = getNeighborhoodAtVoxel(imagenants,center = c(COORDENADAS[i,1],COORDENADAS[i,2],COORDENADAS[i,3]),c(orden,orden,orden))
      coordi = ventana$values
      coord[i,1:length(coordi)]=coordi
      if(i==round(nrow(COORDENADAS)/2))
        print(paste0("voxel nº = ",i,". vas por la mitad."))
    }
    return(coord)
  }else{
    #DEVUELVE DIRECTAMENTE EL VALOR DE LOS VECINOS
    coord = matrix(nrow = length(COORDENADAS),ncol = vecinos_xvoxel)
    for(i in 1:length(COORDENADAS)){
      ventana = getNeighborhoodAtVoxel(imagenants,center = obtenCoord(COORDENADAS[i],imagenants),c(orden,orden,orden))
      coordi = ventana$values
      coord[i,1:length(coordi)]=coordi
      if(i==round(length(COORDENADAS)/2))
        print(paste0("voxel nº = ",i,". vas por la mitad."))
    }
    return(coord)
  }
}