library(ggplot2)
library(reshape)
library(parallel)
jmax<-20000
kmax<-2000000
nesc<-20
nest<-1000
dist<-matrix(round(runif(nesc*nest,0,5),2),nrow = nest)

calCosto<-function(cromosoma){
  return(sum(unlist(mclapply(1:length(cromosoma), function(i) {
    return(dist[i,cromosoma[i]])
  }, mc.cores=8))))
}

selCrom<-function(costos){
  # browser()
  selCrom<-c()
  ps<-costos/sum(costos)
  ps<-1-cumsum(ps)
  while(length(selCrom)!=2){
    rposCrom<-runif(1,0,1)
    rposCromCuales<-which(ps<=rposCrom)
    if(length(rposCromCuales)==0){
      if(!(1 %in% selCrom)){
        selCrom<-c(selCrom,1)  
      }
    } else {
      sel<-min(rposCromCuales)
      if(!(sel %in% selCrom)){
        selCrom<-c(selCrom,sel)
      }
    }
  }
  return(selCrom)
}

mixCrom<-function(padreA,padreB,pmut=.001){
  pmix<-runif(1,.65,1)
  cualesA<-sample(1:length(padreA),round(pmix*length(padreA)))
  hijoA<-hijoB<-padreA
  hijoA[-cualesA]<-padreB[-cualesA]
  hijoB[cualesA]<-padreB[cualesA]
  cualesMutA<-runif(length(hijoA),0,1)
  cualesMutB<-runif(length(hijoB),0,1)
  cualesMutA<-which(cualesMutA<pmut)
  cualesMutB<-which(cualesMutB<pmut)
  hijoA[cualesMutA]<-sample(1:nesc,length(cualesMutA),replace = T)
  hijoB[cualesMutB]<-sample(1:nesc,length(cualesMutB),replace = T)
  return(list(hijoA,hijoB))
}

cromosoma<-sample(1:nesc,nest,replace = T)
cromosomas<-list(cromosoma)
costos=calCosto(cromosoma)
cromosoma<-sample(1:nesc,nest,replace = T)
cromosomas[[2]]<-cromosoma
costos=c(costos,calCosto(cromosoma))
cromosoma<-sample(1:nesc,nest,replace = T)
cromosomas[[3]]<-cromosoma
costos=c(costos,calCosto(cromosoma))
cromosoma<-sample(1:nesc,nest,replace = T)
cromosomas[[4]]<-cromosoma
costos=c(costos,calCosto(cromosoma))
cromosoma<-sample(1:nesc,nest,replace = T)
cromosomas[[5]]<-cromosoma
costos=c(costos,calCosto(cromosoma))
cromosoma<-sample(1:nesc,nest,replace = T)
cromosomas[[6]]<-cromosoma
costos=c(costos,calCosto(cromosoma))
cromosoma<-sample(1:nesc,nest,replace = T)
cromosomas[[7]]<-cromosoma
costos=c(costos,calCosto(cromosoma))

jmax<-20000
j<-j2<-0
js<-c()
mincostos<-min(costos)

while(j<jmax){
#  browser()
  j<-j+1
  minCrom<-which.min(costos)
  cromosomas2<-list(cromosomas[[minCrom]])
  costos2<-costos[minCrom]
  for(i in 1:3){
    myselCrom<-selCrom(costos)
    CromA<-cromosomas[[myselCrom[1]]]
    CromB<-cromosomas[[myselCrom[2]]]
    hijos<-mixCrom(CromA,CromB,min(.01,.003^(100/j)))
    cromosomas2<-append(cromosomas2,hijos[1])
    cromosomas2<-append(cromosomas2,hijos[2])
    costos2<-c(costos2,calCosto(hijos[[1]]),calCosto(hijos[[2]]))
  }
  cromosomas<-cromosomas2
  costos<-costos2
  if(min(costos)<mincostos){
    print(paste(j,min(costos),min(costos)/nest,mean(costos),mean(costos)/nest))
    mincostos<-min(costos)
  }
}
