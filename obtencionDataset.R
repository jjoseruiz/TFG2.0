library(neurobase)
library(ANTsRCore)
library(ANTsR)
library(e1071)
source("estaEnLista.R")
source("elegirVoxeles.R")
source("obtenCoord.R")
source("valoresImagen.R")
source("recorreImagenes.R")
source("aplicaFuncion.R")
#dirección para descargar el dataset --> https://drive.google.com/file/d/1o2cPsFSDkSlYaY5Sx-ZvoqyTAx7YJEzT/view?usp=sharing
listaFunciones = c(mean,min,max,sd,median,skewness)
numero_voxeles = 5000
nSujetos = 30
num_images_sujeto = 9
numImagenes = nSujetos*num_images_sujeto
#hasta que orden de vecinos queremos que se elabore el dataset
ordenMayor=3
almacen = matrix(nrow = numero_voxeles*nSujetos,ncol = length(listaFunciones)*num_images_sujeto*ordenMayor+1)
nombrecolumnas = c("MEAN_FLAIR_O1","MIN_FLAIR_O1","MAX_FLAIR_O1","SD_FLAIR_O1","MEDIAN_FLAIR_O1","SKEWNESS_FLAIR_01",
                   "MEAN_T1_O1","MIN_T1_O1","MAX_T1_O1","SD_T1_O1","MEDIAN_T1_O1","SKEWNESS_T1_01",
                   "MEAN_T2_O1","MIN_T2_O1","MAX_T2_O1","SD_T2_O1","MEDIAN_T2_O1","SKEWNESS_T2_01",
                   "MEAN_FLAIR_SYM_O1","MIN_FLAIR_SYM_O1","MAX_FLAIR_SYM_O1","SD_FLAIR_SYM_O1","MEDIAN_FLAIR_SYM_O1","SKEWNESS_FLAIR_SYM_01",
                   "MEAN_FLAIR_ASYM_O1","MIN_FLAIR_ASYM_O1","MAX_FLAIR_ASYM_O1","SD_FLAIR_ASYM_O1","MEDIAN_FLAIR_ASYM_O1","SKEWNESS_FLAIR_ASYM_01",
                   "MEAN_T1_SYM_O1","MIN_T1_SYM_O1","MAX_T1_SYM_O1","SD_T1_SYM_O1","MEDIAN_T1_SYM_O1","SKEWNESS_T1_SYM_01",
                   "MEAN_T1_ASYM_O1","MIN_T1_ASYM_O1","MAX_T1_ASYM_O1","SD_T1_ASYM_O1","MEDIAN_T1_ASYM_O1","SKEWNESS_T1_ASYM_01",
                   "MEAN_T2_SYM_O1","MIN_T2_SYM_O1","MAX_T2_SYM_O1","SD_T2_SYM_O1","MEDIAN_T2_SYM_O1","SKEWNESS_T2_SYM_01",
                   "MEAN_T2_ASYM_O1","MIN_T2_ASYM_O1","MAX_T2_ASYM_O1","SD_T2_ASYM_O1","MEDIAN_T2_ASYM_O1","SKEWNESS_T2_ASYM_01",
                   "MEAN_FLAIR_02","MIN_FLAIR_02","MAX_FLAIR_02","SD_FLAIR_02","MEDIAN_FLAIR_02","SKEWNESS_FLAIR_02",
                   "MEAN_T1_02","MIN_T1_02","MAX_T1_02","SD_T1_02","MEDIAN_T1_02","SKEWNESS_T1_02",
                   "MEAN_T2_02","MIN_T2_02","MAX_T2_02","SD_T2_02","MEDIAN_T2_02","SKEWNESS_T2_02",
                   "MEAN_FLAIR_SYM_02","MIN_FLAIR_SYM_02","MAX_FLAIR_SYM_02","SD_FLAIR_SYM_02","MEDIAN_FLAIR_SYM_02","SKEWNESS_FLAIR_SYM_02",
                   "MEAN_FLAIR_ASYM_02","MIN_FLAIR_ASYM_02","MAX_FLAIR_ASYM_02","SD_FLAIR_ASYM_02","MEDIAN_FLAIR_ASYM_02","SKEWNESS_FLAIR_ASYM_02",
                   "MEAN_T1_SYM_02","MIN_T1_SYM_02","MAX_T1_SYM_02","SD_T1_SYM_02","MEDIAN_T1_SYM_02","SKEWNESS_T1_SYM_02",
                   "MEAN_T1_ASYM_02","MIN_T1_ASYM_02","MAX_T1_ASYM_02","SD_T1_ASYM_02","MEDIAN_T1_ASYM_02","SKEWNESS_T1_ASYM_02",
                   "MEAN_T2_SYM_02","MIN_T2_SYM_02","MAX_T2_SYM_02","SD_T2_SYM_02","MEDIAN_T2_SYM_02","SKEWNESS_T2_SYM_02",
                   "MEAN_T2_ASYM_02","MIN_T2_ASYM_02","MAX_T2_ASYM_02","SD_T2_ASYM_02","MEDIAN_T2_ASYM_02","SKEWNESS_T2_ASYM_02",
                   "MEAN_FLAIR_03","MIN_FLAIR_03","MAX_FLAIR_03","SD_FLAIR_03","MEDIAN_FLAIR_03","SKEWNESS_FLAIR_03",
                   "MEAN_T1_03","MIN_T1_03","MAX_T1_03","SD_T1_03","MEDIAN_T1_03","SKEWNESS_T1_03",
                   "MEAN_T2_03","MIN_T2_03","MAX_T2_03","SD_T2_03","MEDIAN_T2_03","SKEWNESS_T2_03",
                   "MEAN_FLAIR_SYM_03","MIN_FLAIR_SYM_03","MAX_FLAIR_SYM_03","SD_FLAIR_SYM_03","MEDIAN_FLAIR_SYM_03","SKEWNESS_FLAIR_SYM_03",
                   "MEAN_FLAIR_ASYM_03","MIN_FLAIR_ASYM_03","MAX_FLAIR_ASYM_03","SD_FLAIR_ASYM_03","MEDIAN_FLAIR_ASYM_03","SKEWNESS_FLAIR_ASYM_03",
                   "MEAN_T1_SYM_03","MIN_T1_SYM_03","MAX_T1_SYM_03","SD_T1_SYM_03","MEDIAN_T1_SYM_03","SKEWNESS_T1_SYM_03",
                   "MEAN_T1_ASYM_03","MIN_T1_ASYM_03","MAX_T1_ASYM_03","SD_T1_ASYM_03","MEDIAN_T1_ASYM_03","SKEWNESS_T1_ASYM_03",
                   "MEAN_T2_SYM_03","MIN_T2_SYM_03","MAX_T2_SYM_03","SD_T2_SYM_03","MEDIAN_T2_SYM_03","SKEWNESS_T2_SYM_03",
                   "MEAN_T2_ASYM_03","MIN_T2_ASYM_03","MAX_T2_ASYM_03","SD_T2_ASYM_03","MEDIAN_T2_ASYM_03","SKEWNESS_T2_ASYM_03",
                   "LESION")
colnames(almacen)<-nombrecolumnas
j=0
pathImagenes="/Volumes/VMware Shared Folders/3D MR image database of Multiple Sclerosis patients with white matter lesion segmentations/Banco/"
for(l in 1:nSujetos){
  rootflairl = paste0(pathImagenes,"SN",l,"_FLAIR_BRAIN.nii.gz")
  roott1l = paste0(pathImagenes,"SN",l,"_T1W_BRAIN.nii.gz")
  roott2l = paste0(pathImagenes,"SN",l,"_T2W_BRAIN.nii.gz")
  rootflairsyml=paste0(pathImagenes,"SN",l,"_FLAIR_SIMETRICA.nii.gz")
  rootflairasyml=paste0(pathImagenes,"SN",l,"_FLAIR_ASIMETRICA.nii.gz")
  roott1syml=paste0(pathImagenes,"SN",l,"_T1W_SIMETRICA.nii.gz")
  roott1asyml=paste0(pathImagenes,"SN",l,"_T1W_ASIMETRICA.nii.gz")
  roott2syml=paste0(pathImagenes,"SN",l,"_T2W_SIMETRICA.nii.gz")
  roott2asyml=paste0(pathImagenes,"SN",l,"_T2W_ASIMETRICA.nii.gz")
  #FALTARIA AÑADIR EL NUEVO T2W
  rots = list(rootflairl,roott1l,roott2l,rootflairsyml,rootflairasyml,roott1syml,roott1asyml,roott2syml,roott2asyml)
  print("Leyendo imagenes")
  mismoSujeto = lapply(rots,antsImageRead)
  print("imagenes leidas")
  consenso = antsImageRead((paste0(pathImagenes,"S",l,"_CONSENSO.nii.gz")))
  MASK=antsImageRead(paste0(pathImagenes,"S",l,"_MASK.nii.gz"))
  print("Eligiendo coordenadas")
  coordenadas = elegirVoxeles(numero_voxeles,consenso,mismoSujeto[[1]],MASK)
  cont=0
  for(i in 1:ordenMayor){
    print(paste0("escribiendo vecinos de orden",i))
    vecinosi = recorreImagenes(mismoSujeto,coordenadas,MASK,i)
    dataset = aplicaFuncion(vecinosi,listaFunciones,i)
    almacen[(j+1):(j+nrow(dataset)),(cont+1):(cont+length(listaFunciones)*num_images_sujeto)]=dataset
    cont=cont+length(listaFunciones)*num_images_sujeto
  }
  #i será el número de imágenes y j el número de vóxeles por imagen
  #sabemos que los nvoxel/2 eran sanos por lo que, la feature gt será
  almacen[(j+1):(j+nrow(dataset)/2),ncol(almacen)]=1
  almacen[((j+nrow(dataset)/2)+1):(j+nrow(dataset)),ncol(almacen)]=0
  j=j+nrow(dataset)
  print(paste0("dataset añadido sujeto --> ",l))
}
write.csv(almacen,paste0(pathImagenes,"datasetNew"))

