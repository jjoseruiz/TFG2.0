buscaResolucion <- function(){
  maximo=0
  for (i in 1:30){
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
    print(paste0("Paciente ",i))
    
    IMG_FLAIR = cumprod(antsImageHeaderInfo(rootflairl)$dimensions)[3]
    IMG_T1 = cumprod(antsImageHeaderInfo(roott1l)$dimensions)[3]
    IMG_T2 = cumprod(antsImageHeaderInfo(roott2l)$dimensions)[3]
    mayor=max(maximo,IMG_FLAIR,IMG_T1,IMG_T2)
    if(mayor!=maximo){
      print(paste0("El paciente con mayor resolución es el ",i))
    }
    maximo=mayor
    }
}