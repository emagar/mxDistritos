Sys.setlocale("LC_ALL","ES_ES.UTF-8")

datos<-read.csv("E:/seminario/ags.csv")
#identificar los distritos padre
seleccionar<-which(datos$dist_nuevo==18)
tabla<-table(datos$vieja[seleccionar])
d18<-as.data.frame.table(tabla)
colnames(d18)<-c("distr_viejos","frecuencia")
d18

#seleccionar<-which(datos$dist_nuevo==2)
#tabla<-table(datos$vieja[seleccionar])
#d2<-as.data.frame.table(tabla)
#colnames(d2)<-c("distr_viejos","frecuencia")
#d2


#identificar secciones
sel1<-as.numeric(datos$vieja==7)
sel2<-as.numeric(datos$dist_nuevo==18)

datos$quedan<-0
datos$ganan<-0
datos$pierden<-0
datos$nunca<-0

datos$quedan[sel1==1 & sel2==1]<-1
datos$ganan[sel1==0 & sel2==1]<-1
datos$pierden[sel1==1 & sel2==0]<-1
datos$nunca[sel1==0 & sel2==0]<-1

c<-sum(datos$quedan)
n<-sum(datos$ganan)
sum(datos$pierden)

#cuántas secciones tenía el distrito 9 (viejo)
p<-sum(sel1)

#Cuántas secciones tiene el distrito 14 (nuevo)
n<-sum(sel2)
#s=c/p+n-c

s18<- 100* c/(p+n-c)
paste("el índice de cox y katz para el distrito 1 de ags es",ss,"%")
ss<-round(s,2)
ss

s<- function (c, p, n)
    {s<-c/(p+n-c)
  return(s)
    }
s(27,27,27)
s(1,1,1)
