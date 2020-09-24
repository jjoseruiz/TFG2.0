library(neurobase)
library(ANTsR)
library(ANTsRCore)
library(extrantsr)
#importante ese orden para representar antsImagenes con ortho2 y doubleortho
pathImagenes="/Users/juan/Documents/Imagenes/Banco/"
for(i in 10:30)
{
  imgFLAIR=antsImageRead(paste0(pathImagenes,"SN",i,"_FLAIR_BRAIN.nii.gz"))
  imgT1=antsImageRead(paste0(pathImagenes,"SN",i,"_T1W_BRAIN.nii.gz"))
  imgT2=antsImageRead(paste0(pathImagenes,"SN",i,"_T2W_BRAIN.nii.gz"))
  #Eliminamos los valores negativos para no tener problemas con el registro.
  mf=min(imgFLAIR)
  mt1=min(imgT1)
  mt2=min(imgT2)
  FLAIR=imgFLAIR-mf+1
  T1=imgT1-mt1+1
  T2=imgT2-mt2+1
  #reflejo flair
  reflejoFlair = reflectImage(FLAIR,0,"AffineFast",verbose=TRUE)
  reflejoFlair=reflejoFlair$warpedmovout+mf-1
  asymFlair=FLAIR-reflejoFlair
  antsImageWrite(reflejoFlair,paste0(pathImagenes,"SN",i,"_FLAIR_SIMETRICA.nii.gz"))
  antsImageWrite(asymFlair,paste0(pathImagenes,"SN",i,"_FLAIR_ASIMETRICA.nii.gz"))
  #reflejo T1
  print("Listo FLAIR")
  reflejoT1=reflectImage(T1,0,"AffineFast",verbose=TRUE)
  reflejoT1=reflejoT1$warpedmovout+mt1-1
  asymT1=T1-reflejoT1
  antsImageWrite(reflejoT1,paste0(pathImagenes,"SN",i,"_T1W_SIMETRICA.nii.gz"))
  antsImageWrite(asymT1,paste0(pathImagenes,"SN",i,"_T1W_ASIMETRICA.nii.gz"))  
  #reflejo T2
  print("Listo T1")
  reflejoT2=reflectImage(T2,0,"AffineFast",verbose=TRUE)
  reflejoT2=reflejoT2$warpedmovout+mt2-1
  asymT2=T2-reflejoT2
  antsImageWrite(reflejoT2,paste0(pathImagenes,"SN",i,"_T2W_SIMETRICA.nii.gz"))
  antsImageWrite(asymT2,paste0(pathImagenes,"SN",i,"_T2W_ASIMETRICA.nii.gz"))
  print("Listo T2")
  print(paste0("Sujeto ",i," finalizado. Quedan ",30-i," pacientes"))
}

c=antsImageRead(paste0(pathImagenes,"SN",i,"_FLAIR_BRAIN.nii.gz"))
ortho2(c)
