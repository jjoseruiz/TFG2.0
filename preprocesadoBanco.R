source("correccion.R")
source("buscaResolucion.R")
library(neurobase)
library(ANTsR)
library(extrantsr)
#Configuración del registro

#Búsqueda de imagen con mayor resolución (mayor cantidad de píxeles)
buscaResolucion()

# S1_FLAIR_CORRECTED=correccion(antsImageRead(paste0("/Users/juan/Documents/Imagenes/patient","01-05","/patient","01","/raw/patient","01","_FLAIR.nii.gz")))
# antsImageWrite(S1_FLAIR_CORRECTED,"/Users/juan/Documents/Imagenes/Banco/S1_FLAIR_CORRECTED.nii.gz")
S1_FLAIR_CORRECTED = antsImageRead("/Users/juan/Documents/Imagenes/Banco/S1_FLAIR_CORRECTED.nii.gz")
S1_MASK = antsImageRead(paste0("/Users/juan/Documents/Imagenes/patient","01-05","/patient","01","/patient","01","_brainmask.nii.gz"))
#Enlace para descargar TODAS las imágenes preprocesadas y calculadas sus simétricas, asimétricas, máscaras y GT --> https://drive.google.com/file/d/1F7IY99OY2B6rrff5_AqDhzWOJohENiOE/view?usp=sharing
#PARA MI DATASET DE MRI APLICAREMOS LA SIGUIENTE SECUENCIA DE PASOS

for (i in 1:1){
  ###LECTURA
  if(i<6){
    z="01-05"
    k=paste0("0",i)
  } else if(6 <= i & i< 11){
    z="06-10"
    k=paste0("0",i)
    if(i==10){
      k="10"
    }
  } else if(11<= i & i<16){
    z="11-15"
    k=paste0(i)
  } else if(16<=i&i<21){
    z="16-20"
    k=paste0(i)
    if(i==20){
      k="20"
    }
  } else if(21<= i&i<26){
    z="21-25"
    k=paste0(i)
  } else if(26<=i&i<30){
    z="26-30"
    k=paste0(i)
    if(i==30){
      k="30"
    }
  }
  rootflairl = paste0("/Users/juan/Documents/Imagenes/patient",z,"/patient",k,"/raw/patient",k,"_FLAIR.nii.gz")
  roott1l = paste0("/Users/juan/Documents/Imagenes/patient",z,"/patient",k,"/raw/patient",k,"_T1W.nii.gz")
  roott2l = paste0("/Users/juan/Documents/Imagenes/patient",z,"/patient",k,"/raw/patient",k,"_T2W.nii.gz")
  print(paste0("Leyendo Imagen T1 del sujeto ",i))
  print(paste0("Leyendo Imagen FLAIR del sujeto ",i))
  print(paste0("Leyendo Máscara del sujeto ",i))

  IMG_FLAIR = antsImageRead(rootflairl)
  IMG_T1 = antsImageRead(roott1l)
  IMG_T2 = antsImageRead(roott2l)
  IMG_MASK = antsImageRead(paste0("/Users/juan/Documents/Imagenes/patient",z,"/patient",k,"/patient",k,"_brainmask.nii.gz"))
  IMG_CONSENSO = antsImageRead((paste0("/Users/juan/Documents/Imagenes/patient",z,"/patient",k,"/patient",k,"_consensus_gt.nii.gz")))
  ###CORRECCIÓN
  print("Corrigiendo T1 con el algoritmo N3")
  T1_CORRECT = correccion(IMG_T1)
  T2_CORRECT = correccion(IMG_T2)
  print("Corrigiendo FLAIR con el algoritmo N3")
  FLAIR_CORRECT = correccion(IMG_FLAIR)

  
  ###REGISTRO+EXTRACCIÓN
  #Registramos la FLAIR
  print("Registrando FLAIR al espacio de la FLAIR del paciente 1")
  wtx = antsRegistration(S1_FLAIR_CORRECTED,FLAIR_CORRECT,typeofTransform = "AffineFast")
  wtt1 = antsRegistration(S1_FLAIR_CORRECTED,T1_CORRECT,typeofTransform = "AffineFast")
  wtt2 = antsRegistration(S1_FLAIR_CORRECTED,T2_CORRECT,typeofTransform = "AffineFast")
  wtmask = antsRegistration(S1_MASK,IMG_MASK,typeofTransform = "AffineFast")
    
  FLAIR_CORRECT_REGISTERED = antsApplyTransforms(fixed = S1_FLAIR_CORRECTED,FLAIR_CORRECT,transformlist = wtx$fwdtransforms)
  NEW_MASK = antsApplyTransforms(fixed = S1_MASK,IMG_MASK,transformlist = wtmask$fwdtransforms)
  T1_CORRECT_REGISTERED = antsApplyTransforms(fixed = S1_FLAIR_CORRECTED,T1_CORRECT,transformlist = wtt1$fwdtransforms)
  T2_CORRECT_REGISTERED = antsApplyTransforms(fixed = S1_FLAIR_CORRECTED,T2_CORRECT,transformlist = wtt2$fwdtransforms)
  CONSENSO_REGISTERED = antsApplyTransforms(fixed = S1_FLAIR_CORRECTED,IMG_CONSENSO,transformlist = wtx$fwdtransforms)

  ##Extracción
  print("Extrayendo Cerebro de FLAIR")
  Si_FLAIR_BRAIN_REGISTERED = maskImage(FLAIR_CORRECT_REGISTERED,NEW_MASK)
  print("Extrayendo cerebro de la T1")
  Si_T1_BRAIN_REGISTERED = maskImage(T1_CORRECT_REGISTERED,NEW_MASK)
  Si_T2_BRAIN_REGISTERED = maskImage(T2_CORRECT_REGISTERED,NEW_MASK)
  
  

  ##NORMALIZACIÓN
  ##
  #ws_flair =zscore_img(Si_FLAIR_BRAIN_REGISTERED)
  ##
  #ws_t1 = zscore_img(Si_T1_BRAIN_REGISTERED)
  ws_flair=Si_FLAIR_BRAIN_REGISTERED
  ws_t1=Si_T1_BRAIN_REGISTERED
  ws_t2=Si_T2_BRAIN_REGISTERED
  ###ESCRITURA
  print("Escribiendo Imágenes en la carpeta BRAIN_IMAGES")
  antsImageWrite(ws_flair,paste0("/Users/juan/Documents/Imagenes/Banco/","S",i,"_FLAIR_BRAIN.nii.gz"))
  antsImageWrite(ws_t1,paste0("/Users/juan/Documents/Imagenes/Banco/","S",i,"_T1W_BRAIN.nii.gz"))
  antsImageWrite(ws_t2,paste0("/Users/juan/Documents/Imagenes/Banco/","S",i,"_T2W_BRAIN.nii.gz"))
  antsImageWrite(CONSENSO_REGISTERED,paste0("/Users/juan/Documents/Imagenes/Banco/","S",i,"_CONSENSO.nii.gz"))
  antsImageWrite(NEW_MASK,paste0("/Users/juan/Documents/Imagenes/Banco/","S",i,"_MASK.nii.gz"))
}


##control de calidad manual
k = 0
k=k+1
consensoi2 = antsImageRead(paste0("/Users/juan/Documents/Imagenes/Banco/","S",k,"_CONSENSO.nii.gz"))
flair_braini1=antsImageRead(paste0("/Users/juan/Documents/Imagenes/Banco/","S",k,"_FLAIR_BRAIN.nii.gz"))
T1_braini2=antsImageRead(paste0("/Users/juan/Documents/Imagenes/Banco/","S",k,"_T1W_BRAIN.nii.gz"))
T2_braini2=antsImageRead(paste0("/Users/juan/Documents/Imagenes/Banco/","S",k,"_T2W_BRAIN.nii.gz"))
mascara= antsImageRead(paste0("/Users/juan/Documents/Imagenes/Banco/","S",k,"_MASK.nii.gz"))

double_ortho(T1_braini2,newt1)
lista = renormalizeProbabilityImages(list(T1_braini,T1_braini2),mascara)

ortho2(flair_braini,consensoi)
papaya(list(flair_braini,consensoi))
k
IMG_FLAIR = antsImageRead(paste0("/Users/juan/Documents/Imagenes/patient","06-10","/patient0",k,"/raw/patient0",k,"_FLAIR.nii.gz"))
IMG_CONSENSO = antsImageRead(paste0("/Users/juan/Documents/Imagenes/patient","06-10","/patient0",k,"/patient0",k,"_consensus_gt.nii.gz"))
ortho2(IMG_FLAIR,IMG_CONSENSO)
papaya(list(IMG_FLAIR,IMG_CONSENSO))
#Puede que tengamos que deshechar la imagen numero 30, ya que una lesion sale fuera de la masa cerebral.

#Normalizado


seq(1,30)
rootsf=c(paste0("/Users/juan/Documents/Imagenes/Banco/","S",seq(1,30),"_FLAIR_BRAIN.nii.gz"))
rootst1=c(paste0("/Users/juan/Documents/Imagenes/Banco/","S",seq(1,30),"_T1W_BRAIN.nii.gz"))
rootst2=c(paste0("/Users/juan/Documents/Imagenes/Banco/","S",seq(1,30),"_T2W_BRAIN.nii.gz"))

for (i in 1:3){
  imf=readnii(rootsf[i])
  imt1=readnii(rootst1[i])
  imt2=readnii(rootst2[i])
  antsImageWrite(100*minmax_img(imf),paste0("/Users/juan/Documents/Imagenes/Banco/","SN",i,"_FLAIR_BRAIN.nii.gz"))
  antsImageWrite(100*minmax_img(imt1),paste0("/Users/juan/Documents/Imagenes/Banco/","SN",i,"_T1W_BRAIN.nii.gz"))
  antsImageWrite(100*minmax_img(imt2),paste0("/Users/juan/Documents/Imagenes/Banco/","SN",i,"_T2W_BRAIN.nii.gz"))
  print(paste0("quedan ",30-i))  
}
imagenes = lapply(roots,readnii)
f1=readnii(roots[1])
f2=readnii(roots[2])
newf=minmax_img(list(f1,f2))
hist(newf1)
hist(f1)
