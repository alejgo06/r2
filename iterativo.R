setwd("~/resources/proyects/r2")
combAlex<-function(nvar=4,k=2){
  aux=c()
  for (f in 1:nvar){#se crea una matriz de 2^nvar 
    #f no es de fila es de fase
    aux<-cbind(rep(c(rep(1,2^(f-1)),rep(0,2^(f-1))),(2^(nvar-f))),aux)
  }
  ma<-(aux[rowSums(aux)==k,])#se seleccionan las que cumplan el criterio
  #para que devuelva lo mismo que la funcion de r
  #2*2^(f-1)*2^(nvar-f)=2^nvar 
  vec<-c(1:nvar)
  res<-c()
  for (i in 1:dim(ma)[1]){
    res<-cbind(res,vec[ma[i,]==1])
  }
  return(res)
}

mejorModelObesidad<-function(k=4,r2=1,fichero){
  if(missing(fichero)){
    a<-as.data.frame(read.table("datos_obesidad.csv",skip=3,sep=";",dec=",",header = T))
    #a<-as.data.frame(read.table("C:/Users/alejandro.gonzalez/Downloads/datos_obesidad.csv",skip=3,sep=";",dec=",",header = T))
    a<-a[1:252,]
  }else{
    a<-fichero
  }
  criterio<-ifelse(r2==1,c("r.squared"), c("adj.r.squared") )
  #generar las combinacioens posibles
  #md<-utils::combn(16,k)
  md<-combAlex(nvar=16,k)#cada columna es una combinaci?n, el numero 
  #de filas tene que coincidir con el numero de elementos
  #que tiene cada combinaci?n
  
  #inicializar variables
  nom<-c()
  val<-c()
  maximo<-c(0)
  mejComb<-c()
  
  #bucles
  for (i in 1:dim(md)[2]){#por cada combinacion
    #se van a seleccionar las variables y se van a guardar en un data 
    ma<-c();nombres<-c()
    for (j in 1:dim(md)[1]){
      ma<-cbind(ma,a[,3+md[j,i]])
      nombres<-c(nombres,names(a)[3+md[j,i]])
    }
    ma<-as.data.frame(ma)
    names(ma)<-nombres
    
    #se ajusta el modelo de regresi?n
    modelo<-lm(a$Fat.1~.,data=ma)
    
    #buscar el mejor r^2
    valor<-summary(modelo)[criterio][[1]]
    if(valor>maximo){
      #se actualizan los valores
      maximo=valor
      mejComb<-nombres
    }
    nom<-rbind(nom,nombres)
    val<-rbind(val,valor)
  }
  colnames(val)<-criterio
  resultado<-data.frame(nom,val)
  return(list=c(list(resultado),list(mejComb),maximo))
}
t <- proc.time() # Inicia el cron?metro
prueba2<-mejorModelObesidad(k=4,r2=1)
proc.time()-t    # Detiene el cron?metro
#6 segundos 
View(prueba2[1])#tabla
prueba2[2]#combinacion
prueba2[3]#r

#comlejidad nvar+2^nvar+nvar*k
#16+2^16+k*16=O(k)
