#Quizas ponerle un filechoose
#data <- read.csv("~/Colima94-13%20v1.0(ConNotacionIntegradora).csv", stringsAsFactors = FALSE)

#Supongo que ya tengo data
data <- read.csv("~/GitHub/mapas/Colima94-13 v1.0(ConNotacionIntegradora).csv", stringsAsFactors = FALSE)
data$dis2009


datshort=cbind(data$seccion ,data$dis2009)
a=grep( "\\[[0-9]*\\]\\[[0-9]*-[0-9]*\\]" , datshort[,2])
 Ya que se cuales son las que tiene son procedentes de division
loop
  Tomo el primer numero y veo que tiene adentro
  Le saco el numero del cual proviene
  Reviso ese numero y se lo pongo a esa celda
  
  tengo que ir sumando las veces que se hace la operacion
  si la operacion que va hacia el mismo numero se repite la cantidad de veces que dice la segunda parte todo esta bien.

  b=datshort[,2][a[1]]
  #regexp("\\[[0-9]*\\]", data$dis2009[a[1]])
  direcc=regmatches(b,regexpr("[0-9]+",b))
  datshort[,2][as.numeric(a[1])]=datshort[,2][as.numeric(direcc)]








#Forma de ver si tiene dos cajas de corchetes(regresa true o false)
grep( "\\[[0-9]*\\]\\[[0-9]*-[0-9]*\\]" , "[163][1-36]")

#una sola caja de corchetes
grep( "\\[[0-9]*:[0-9]*\\]" , "[25:27]")


a=as.integer(as.character(data$dis2009))
b=is.na(a)

contador=0
for (num in b){
  if(num==F){
    contador=contador+1
  }
  else{
    contador=contador+1
    if(
    )
  }
}
  