setwd("~/resources/proyects/r2")

#congruencial----

cong<-function(x0, a=16807,b=0,m=(2^31)-1){
  return (list(((x0*a+b)%%m),((x0*a+b)%%m)/m))#genera valores uniformes 0,1
}


#solucionInicial----
soluIni<-function(sem=123456789,k=1,nvar=4){
  x=rep(0,nvar)
  va<-c()
  while( length(va)<k){
    Uni<-cong(sem)[[2]]
    sem<-cong(sem)[[1]]
    posible<-sum(Uni>cumsum(rep(1/nvar,nvar)))+1
    if (sum(va==posible)==0){
      va<-c(va,posible)
    }
  }
  x[va]=1
  return(list(x,sem))
}

#Calcular R2----
r2<-function(vector,MatrizX,VectorY){
  ma<-as.data.frame(MatrizX[vector==1])
  #colnames(ma)
  modelo<-lm(VectorY~., data=ma)
  valor<-summary(modelo)["r.squared"][[1]]
  return(valor)
}

#vecinos----
vecinos<-function(vector,sem){
  pos1=sum(vector)
  pos0<-length(vector)-pos1
  #de las posiciones activas de vector pasa una a inactiva
  Uni<-cong(sem)[[2]]
  sem<-cong(sem)[[1]]
  d1a0<-sum(Uni>cumsum(rep(1/pos1,pos1)))+1#de la 1 a 0
  #de las posicones inactivas de vector pasar a una como activa
  Uni<-cong(sem)[[2]]
  sem<-cong(sem)[[1]]
  d0a1<-sum(Uni>cumsum(rep(1/pos0,pos0)))+1#de 0 a 1
  #se cambian
  vs<-vector
  vs[which(vector==1)[d1a0]]<-0
  vs[which(vector==0)[d0a1]]<-1
  return(list(vs,sem))
}


# codigo ----
t <- proc.time()
#cargar fichero de datos
fichero<-as.data.frame(read.table("datos_obesidad.csv",skip=3,sep=";",dec=",",header = T))
fichero<-fichero[1:252,]
MX=fichero[,4:19]
VY=fichero[,2]
#parámetros a cambiar
maxIte<-1000
nK=4
Temperatura=4000

#solucion inicial
x=soluIni(sem=493878,k=nK,nvar=16)
mv<-x[[1]]#vector x0
sem<-x[[2]]#semilla
rcuad<-r2(vector=mv,VectorY = VY,MatrizX = MX)#rcuadrado de x0
#iterar hasta numero  máximo de iteraciones
for (i in 1:maxIte){
  xa<-vecinos(mv,sem=sem)#se generan los vecinos
  va<-xa[[1]]#es el vector vecino
  sem<-xa[[2]]#semilla
  r2aux<-r2(vector=va,VectorY = VY,MatrizX = MX)#rcuadrado del vecino
  if(r2aux>rcuad){
    rcuad<-r2aux
    mv<-va
  }else{
    xa<-cong(x0=sem)
    uniforme<-xa[[2]]
    sem=xa[[1]]
    #Temperatura<-(1-(((c(1:maxIte)^(1/5))/maxIte^(1/5)))) ptra opcion
    if(uniforme<exp(-abs(rcuad-r2aux)/(Temperatura)) ){
      rcuad<-r2aux
      mv<-va
      Temperatura=Temperatura/2
    }
  }
}
#resultados

nombres<-colnames(MX)
nombres[mv==1]
proc.time()-t





