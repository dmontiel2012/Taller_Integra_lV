library(ggplot2)
library(reshape)
library(parallel)
library(deldir)
library(animation)
library(Rcpp)
library(RColorBrewer)
#----------------------------------
#lectura de archivo
datos <- data.frame(read.csv("Datos/escuelasFinal.csv"))

jmax<-20000
kmax<-2000000
######
# Definici?n de Poblaci?n
######
#nest<-1000
nesc<-20
escuelas<-1:nesc
#nest<-round(20000/12)
nest<-round(1000)

#Posicion de las escuelas, reales.
posEscuela<-as.data.frame(cbind(X=datos[1:nesc,2],Y=datos[1:nesc,3]))


posEsc<-as.data.frame(cbind(X=datos[1:nesc,2],Y=datos[1:nesc,3]))
cupo<-abs(round(rnorm(nesc,(nest/nesc),1.5*(nest/nesc))))+10


calcDist<-function(ests,escs){
  return(outer(1:nest,1:nesc,function(i,j) sqrt(rowSums((ests[i,]-escs[j,])^2))))
}

# aca las distanciass
calculoDist<-function(nest,nesc){
  dis<-matrix(data = NA,nrow=nest, ncol = nesc, byrow = FALSE, dimnames = NULL ) 
  for(i in 1:nest){
    for(j in 1:nesc){
      dis[i,j]<-((sqrt((((2-1)^2)+((2-1)^2))/2))*157.48)
      
    }
    
  }
  return (dis)  
}

cppFunction('NumericMatrix calculodist( DataFrame posEstudiantes, DataFrame posEscuela,int nest,int nesc ){
            
            NumericMatrix asx(nest,nesc);
            float lt1,lt2,lg1,lg2,z;
            NumericMatrix posEstu = internal::convert_using_rfunction(posEstudiantes, "as.matrix");
            NumericMatrix posEsc = internal::convert_using_rfunction(posEscuela, "as.matrix");
            for(int i=0; i < nest; i++){ 
            for(int j=0; j< nesc; j++){
            lt1 =posEstu(i,0);
            lg1= posEstu(i,1);
            lt2= posEsc(j,0);
            lg2= posEsc(j,1);
            z= (pow((lt1-lt2),2))+(pow((lg1-lg2),2));
            z = sqrt(z/2)*157.48;
            asx(i,j)= z;
            
            }            
            }
            
            return asx;
            }')


#distancia entre pos geografico
distancia<-function(lt1,lg1,lt2,lg2){
  
  return((sqrt((((lt1-lt2)^2)+((lg1-lg2)^2))/2))*157.48)
  
}

genCirc<-function(n,pos,sdx,sdy){
  #  browser()
  xpos<-rnorm(n,pos$X,sdx)
  ypos<-rnorm(n,pos$Y,sdx)
  return(cbind(X=xpos,Y=ypos))
}


genEst<-function(nest,nesc,posEsc,cupo,dist95=0.006){
  #popEst<-matrix(0,nrow = nest,ncol=2)
  #colnames(popEst)<-c("X","Y")
  cupo2<-round(nest*cupo/sum(cupo))
  popEst<-genCirc(cupo2[1],posEsc[1,],dist95/2,dist95/2)
  for(i in 2:nesc){
    popEst<-rbind(popEst,genCirc(cupo2[i],posEsc[i,],dist95/2,dist95/2))
  }
  return(popEst)
}


#Posicion Estudiantes
posEstudiantes<-genEst(nest,nesc,posEscuela[1:nesc,],cupo[1:nesc],dist95=0.006)

distEntre<-function(nest,nesc,posEstudiantes,posEscuelas,cupo){
  #browser()
  distances<- c()
  escuela <- c()
  estudiante <-c()
  cupo2<-round(nest*cupo/sum(cupo))
  l<-1
  for(i in 1:nesc){
    for (j in 1:cupo2[i]){
      escuela[l]<-i
      estudiante[l]<-l
      distances[l]<-distancia(posEstudiantes[j,1],posEstudiantes[j,2],posEscuelas[i,1],posEscuelas[i,2])    
      l<-l+1
    }
  }
  MatrizDatos<-as.data.frame(cbind(Estudiante=estudiante,Escuela=escuela,Distancia=distances)) 
  return(MatrizDatos)
}

DistAluEsc<-distEntre(nest,nesc,posEstudiantes,posEscuela,cupo)






Sesc<-function(cromosoma,esc){
  #  browser()
  cuales<-which(cromosoma==esc)
  ti<-length(cuales)
  vi<-length(which(vuln[cuales]==1))
  Pi<-vi/ti
  return(abs(exp(-Pi/P)-exp(-(1-Pi)/(1-P))))
}

#S<-function(cromosoma){
#  return(sum(unlist(mclapply(1:nesc, function(i) {
#    cuales<-which(cromosoma==i)
#    ti<-length(cuales)
#    if(ti!=0){
#      vi<-length(which(vuln[cuales]==1))
#      Pi<-vi/ti
#      return(ti/nest*abs(exp(-Pi/P)-exp(-(1-Pi)/(1-P))))
#    } else {
#      return(0)
#    }
#  }, mc.cores=1))))
#}
cppFunction('float S(NumericVector cromosoma,int nesc,NumericVector vuln,int nest,float P){
            NumericVector sum(nesc);
            NumericVector cuales(cromosoma.size());
            int cs=0;
            int vi=0;
            float suma=0;
            for(int i=1;i<=nesc;i++){
            for(int j=1;j<cromosoma.size();j++){
            if(cromosoma(j)==i){
            cuales(cs)=j;
            cs=cs+1;
            
            }
            }
            int ti = cs;
            
            float tinest = ti/nest;
            cs=0;
            if(ti != 0){
            for(int m=0;m<ti;m++){
            if(vuln[cuales[m]]==1){
            vi = vi+1;
            }
            
            }
            float Pi = (vi*1.0)/(ti*1.0);
            
            float result=(ti/(double)nest)*((exp(-Pi/P)-exp(-(1-Pi)/(1-P)))*-1);
            sum(i-1)= fabs(result);
            
            vi=0;
            }
            
            
            
            }
            
            // sumo todo los valores
            for(int l=0;l<sum.size();l++){
            suma= suma+sum(l);
            
            }
            
            return (suma);
            }
            
            ')

#__________________________________________________________________________________________
#meanDist2<-function(cromosoma){
# return(mean(unlist(mclapply(1:length(cromosoma), function(i) {
#    return(dist[i,cromosoma[i]])
#  }, mc.cores=1))))
#}

cppFunction('float meanDist( NumericVector cromosoma, NumericMatrix dist ){
            float sum=0;
            NumericVector as(cromosoma.size()); 
            int x = cromosoma.size();
            for(int i=0; i < x; i++){ 
            sum = sum + dist(i,(cromosoma(i)-1));
            
            
            }
            return (sum/cromosoma.size());
            
            }')

distDist<-function(cromosoma){
  return((unlist(mclapply(1:length(cromosoma), function(i) {
    return(dist[i,cromosoma[i]])
  }, mc.cores=1))))
}


#costCupo<-function(cromosoma){
#  return(mean(unlist(mclapply(1:nesc, function(i) {
#    ni<-length(which(cromosoma==i))
#    return(ni*abs(cupo[i]-ni)/(cupo[i]/2)^2)
#  }, mc.cores=1))))
#}

cppFunction('float costCupo(NumericVector cromosoma,int nesc,NumericVector cupo){
            NumericVector sum(nesc);
            NumericVector cuales(cromosoma.size());
            int cs=0;
            int vi=0;
            float suma=0;
            for(int i=1;i<=nesc;i++){
            for(int j=1;j<cromosoma.size();j++){
            if(cromosoma(j)==i){
            cuales(cs)=j;
            cs=cs+1;
            
            }
            }
            float ni = cs;
            cs=0;
            float result=(ni*fabs(cupo(i-1)-ni)/pow((cupo(i-1)/2),2));
            sum(i-1)= fabs(result);
            
            vi=0;
            
            
            
            
            }
            
            // sumo todo los valores
            for(int l=0;l<sum.size();l++){
            suma= suma+sum(l);
            
            }
            
            return (suma/sum.size());
            }
            
            ')



calCosto<-function(cromosoma,alpha){
  #  costo<-(sum(unlist(mclapply(1:length(cromosoma), function(i) {
  #    return(dist[i,cromosoma[i]])
  #  }, mc.cores=1))))
  #return(alpha*meanDist(cromosoma)+(1-alpha)*S(cromosoma)+costCupo(cromosoma))
  return(alpha[1]*meanDist(cromosoma,dist)+alpha[2]*(S(cromosoma,nesc,vuln,nest,P)-.1)^2+alpha[3]*costCupo(cromosoma,nesc,cupo))
}

calCosto2<-function(cromosoma,alpha){
  costo<-sum(unlist(lapply(1:length(cromosoma), function(i) {
    return(dist[i,cromosoma[i]])
  })))
  return(costo)
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

selCromT<-function(costos,p){
  #  browser()
  selCromT<-c()
  orden<-order(costos,decreasing = F)
  ps<-cumsum((p*(1-p)^(orden-1))/sum(p*(1-p)^(orden-1)))
  while(length(selCromT)!=2){
    rposCrom<-runif(1,0,1)
    rposCromCuales<-which(ps<=rposCrom)
    if(length(rposCromCuales)==0){
      if(!(1 %in% selCromT)){
        selCromT<-c(selCromT,1)  
      }
    } else {
      sel<-max(rposCromCuales)+1
      if(!(sel %in% selCromT)){
        selCromT<-c(selCromT,sel)
      }
    }
  }
  return(selCromT)
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

difCrom<-function(cromA,cromB){
  #  browser()
  dif<-length(which(cromA!=cromB))
  return(dif/length(cromA))
}

calcPmut<-function(cromA,cromB,pmin,pmax,t){
  #  browser()
  p<-exp(-50*difCrom(cromA,cromB))*pmax+pmin
  fact<-exp(-t)
  return(min(pmax,max(pmin,p-fact)))
}



plot.esc<-function(cromosoma,escuelas,color=T){
  # browser()
  k<-0
  cromosoma_orig<-cromosoma
  cualesEsc<-as.numeric(names(table(cromosoma))) #identificar escuelas abiertas
  #  cromosoma<-cromosomas[[1]]
  cuales<-cromosoma %in% escuelas #Filtrar estudiantes según escuelas
  cromosoma<-factor(cromosoma[cuales],cualesEsc)
  vulni<-factor(vuln[cuales],0:1) #Ver estudiantes vulnerables
  #  posEstEsc<-as.data.frame(posEst[cromosomas[[1]] %in% escuelas,])
  posEstEsc<-as.data.frame(posEst[cromosoma_orig %in% escuelas,])
  posEstEsc<-cbind(posEstEsc,Esc=cromosoma,Vuln=vulni)
  posEsci<-cbind(as.data.frame(posEsc[cualesEsc,]),id=factor(cualesEsc,cualesEsc))
  posEsci<-posEsci[posEsci$id %in% escuelas,]
  posEsci<-cbind(posEsci,Si=unlist(lapply(cualesEsc,function(i) Sesc(cromosoma_orig,i))))
  
  rayos<-data.frame(X=0,Y=0,Esc=factor(unlist(lapply(1:nest,function(i) return(c(cromosoma_orig[i],cromosoma_orig[i])))),cualesEsc))
  
  for(i in unique(posEsci$id)){
    cualesEstEsc<-which(posEstEsc$Esc==i)
    rayos[2*cualesEstEsc-1,1:2]<-posEsci[i,1:2]
    rayos[2*cualesEstEsc,1:2]<-posEstEsc[cualesEstEsc,1:2]
    if(i==unique(posEsci$id)[1]){
      findhull<-posEstEsc[cualesEstEsc[chull(posEstEsc$X[cualesEstEsc],
                                             posEstEsc$Y[cualesEstEsc])],]
    } else {
      findhull<-rbind(findhull,posEstEsc[cualesEstEsc[chull(posEstEsc$X[cualesEstEsc],
                                                            posEstEsc$Y[cualesEstEsc])],])
    }
  }
  #browser()
  posEstEsc$Esc<-factor(posEstEsc$Esc,escuelas)
  posEsci$id<-factor(posEsci$id,escuelas)
  myColors <- brewer.pal(n = 8,name = "Dark2")
  #names(myColors) <- levels(posEsci$id)
  names(myColors) <- levels(myColors)
  p<-ggplot()
  p<-p+geom_polygon(data=findhull,aes(x=X,y=Y,fill=Esc),alpha=0.1)
  p<-p+geom_polygon(data=findhull,aes(x=X,y=Y,col=Esc),alpha=0)
  #p<-p+geom_path(data=rayos,aes(x=X,y=Y,color=Esc))
  p<-p+geom_point(data=posEsci,aes(x=X,y=Y,size=Si,fill=id),shape=25,color="black")
  p<-p+scale_size(limits = c(0,1),breaks=c(0,0.3,0.6,1),labels=c("Nula","Alta","Aberrante","Absoluta"))
  #p<-p+scale_color_manual(values=c("lightred","pink","lightblue","lightgreen","orange"))
  #p<-p+scale_fill_manual(values=c("lightred","pink","lightblue","lightgreen","orange"))
  p<-p+scale_color_manual(values=myColors)
  p<-p+scale_fill_manual(values=myColors)
  p<-p+geom_point(data=posEstEsc,aes(x=X,y=Y,color=Esc,shape=Vuln),size=3)
  p<-p+theme(legend.position="bottom",legend.box="horizontal")
  p<-p+annotate("text",label=paste("t=",k," - S=",round(S(cromosoma,nesc,vuln,nest,P),3),sep=""),
                x=min(posEstEsc$X),y=min(posEstEsc$Y),size=3,hjust = 0)
  if(!color) p<-p+scale_color_grey()+scale_fill_grey()
  p
}


###Parte 2
###########

num_cores <- detectCores()
cl <- makeCluster(num_cores)

jmax<-20000
kmax<-2000000
######
# Definici?n de Poblaci?n
######
#nest<-1000
nesc<-20
escuelas<-1:nesc
#nest<-round(20000/12)
nest<-round(1000)
posEsc<-as.data.frame(cbind(X=datos[1:nesc,2],Y=datos[1:nesc,3]))
cupo<-abs(round(rnorm(nesc,(nest/nesc),1.5*(nest/nesc))))+10

if(sum(cupo)<1.25*nest){
  cupo<-round(cupo/(sum(cupo)/(1.25*nest)))
}


#posEst<-genEst(nest,nesc,posEsc,cupo,5)
posEst<-genEst(nest,nesc,posEscuela[1:nesc,],cupo[1:nesc],dist95=0.006)
nest<-nrow(posEst)
#dist<-calcDist(posEst,posEsc)
dist<-calculodist(posEst,posEsc,nest,nesc)
#dist<-matrix(round(runif(nesc*nest,0,10),2),nrow = nest)

#vuln<-sample(0:1,nest,replace = T)
vuln<-rep(0,nest)
masCerca<-apply(dist,1,which.min)
for(i in 1:round(nesc/2,0)){
  vuln[which(masCerca==i)]<-1
}
v<-sum(vuln)
P<-v/nest
nv<-nest-v
vtess <- deldir(posEsc,rw=c(range(posEst[,1]),range(posEst[,2])))
plot(posEst,type="n",xlim=range(posEst[,1]),ylim=range(posEst[,2]))
points(posEsc, pch=21, col=escuelas, cex=2)
plot(vtess, wlines="tess", wpoints="none", number=FALSE, add=TRUE, lty=1)
points(posEst,col=masCerca,pch=1+vuln,cex=0.5)
plot.esc(masCerca,escuelas)
######
# Hasta ac? !!!
######


alpha<-c(5,50,10)
ncrom<-10
# cromosoma<-sample(1:nesc,nest,replace = T)
# #cromosoma<-masCerca
# cromosomas<-list(cromosoma)
# costos=calCosto(cromosoma,alpha)
# for(i in 2:(ncrom)){
#   cromosoma<-sample(1:nesc,nest,replace = T)
#   cromosomas[[i]]<-cromosoma
#   costos=c(costos,calCosto(cromosoma,alpha))
# }

cromosoma<-masCerca
#cromosoma<-masCerca
cromosomas<-list(cromosoma)
costos=calCosto(cromosoma,alpha)
for(i in 2:(ncrom)){
  cromosoma<-masCerca
  cromosomas[[i]]<-cromosoma
  costos=c(costos,calCosto(cromosoma,alpha))
}


jmax<-2000
j<-j2<-0
js<-c()
mincostos<-min(costos)
vmincostos<-min(costos)
vmaxcostos<-max(costos)
vmediancostos<-median(costos)
minS<-S(cromosomas[[which.min(costos)]],nesc,vuln,nest,P)
vmeanDist<-meanDist(cromosomas[[which.min(costos)]],dist)
vmeanCostCupo<-costCupo(cromosomas[[which.min(costos)]],nesc,cupo)
t<-0
k<-0
kmax<-2e6
p<-pmin<-0.0001
pmax<-0.01
pTourn<-0.5
print(paste(k,min(costos),median(costos),
            max(costos),S(cromosomas[[which.min(costos)]],nesc,vuln,nest,P),
            costCupo(cromosomas[[which.min(costos)]],nesc,cupo),p))

#saveGIF({
#print(plot.esc(masCerca,escuelas))
while(j<jmax && k<kmax){
  k<-k+1
  #costos2<-c()
  #cromosomas2<-list()
  minCrom<-which.min(costos)
  maxCrom<-which.max(costos)
  cromosomas2<-list(cromosomas[[minCrom]])
  costos2<-costos[minCrom]
  #maxCrom<-which.max(costos)
  #cromosomas<-cromosomas[-maxCrom]
  #costos<-costos[-maxCrom]
  for(i in 1:round(ncrom/2,0)){
    myselCrom<-selCromT(costos,pTourn)
    CromA<-cromosomas[[myselCrom[1]]]
    CromB<-cromosomas[[myselCrom[2]]]
    hijos<-mixCrom(CromA,CromB,p)
    cromosomas2<-append(cromosomas2,hijos[1])
    cromosomas2<-append(cromosomas2,hijos[2])
    costos2<-c(costos2,calCosto(hijos[[1]],alpha),calCosto(hijos[[2]],alpha))
  }
  cromosomas<-cromosomas2
  #  browser()
  costos<-costos2
  if(min(costos)<mincostos){
    p<-calcPmut(cromosomas[[minCrom]],cromosomas[[maxCrom]],pmin,pmax,k)
    j<-0
    mincostos<-min(costos)
    vmincostos<-c(vmincostos,mincostos)
    vmaxcostos<-c(vmaxcostos,max(costos))
    vmediancostos<-c(vmediancostos,median(costos))
    minS<-c(minS,S(cromosomas[[which.min(costos)]],nesc,vuln,nest,P))
    vmeanDist<-c(vmeanDist,meanDist(cromosomas[[which.min(costos)]],dist))
    vmeanCostCupo<-c(vmeanCostCupo,costCupo(cromosomas[[which.min(costos)]],nesc,cupo))
    print(paste(k,meanDist(cromosomas[[which.min(costos)]],dist),
                min(costos),median(costos),max(costos),
                S(cromosomas[[which.min(costos)]],nesc,vuln,nest,P),
                costCupo(cromosomas[[which.min(costos)]],nesc,cupo),p))
    #print(plot.esc(cromosomas[[which.min(costos)]],escuelas))
    t<-c(t,k)
    if(runif(1,0,1)<.05){
      #p<-max(p/1.5,0.0001)
    }
  } else {
    j<-j+1
    if(j%%500==0){
      p<-min(sqrt(p),pmax)
      #p<-min(1.01*p,pmax)
    }
  }
  if(k%%1000==0){
    print(k)
  }
}

cromosoma<-cromosomas[[1]]
#cromosoma<-masCerca
mincosto<-calCosto(cromosoma,alpha)
for(i in 1:5000){
  cromosoma2<-cromosoma
  cupoCrom2<-table(cromosoma2)
  minEsc<-as.numeric(names(which.min(cupoCrom2)))
  cualesMinEsc<-which(cromosoma2==minEsc)
  escSinMin<-(1:nesc)[-minEsc]
  for(j in escSinMin){
    cromosoma2[sample(cualesMinEsc,1)]<-j
    costoCrom2<-calCosto(cromosoma2,alpha)
    if(costoCrom2<mincosto){
      #      browser()
      cromosoma<-cromosoma2
      mincosto<-costoCrom2
      print(paste(i,mincosto,meanDist(cromosoma,dist),
                  S(cromosoma,nesc,vuln,nest,P),
                  costCupo(cromosoma,nesc,cupo)))
    }
  }
}
#},movie.name = "test.gif", interval=1,ani.width=800,ani.height=800)

table(masCerca)
table(cromosomas[[1]])

ggplot()+
  geom_line(aes(x=t,y=vmincostos))+
  geom_line(aes(x=t,y=vmaxcostos),col="red")+
  geom_line(aes(x=t,y=vmediancostos),col="blue")+
  geom_hline(yintercept=calCosto(masCerca,alpha),col="black",lty=2)

ggplot()+
  geom_line(aes(x=t,y=minS),col="darkgreen")+
  geom_hline(yintercept=S(masCerca,nesc,vuln,nest,P),col="black",lty=2)

ggplot()+
  geom_line(aes(x=t,y=vmeanCostCupo),col="darkgreen")+
  geom_hline(yintercept=costCupo(masCerca,nesc,cupo),col="black",lty=2)

ggplot()+
  geom_line(aes(x=t,y=vmeanDist),col="darkgreen")+
  geom_hline(yintercept=meanDist(masCerca,dist),col="black",lty=2)

#cromosomas[[1]]<-cromosoma

escuelas<-1:nesc
plot.esc(cromosoma,escuelas)
table(cromosoma)
cupo

library(doBy)
posEst2<-as.data.frame(cbind(posEst,Esc=cromosoma))
posEsc2<-summaryBy(X+Y~Esc,posEst2,FUN=mean)[,c(2:3)]
colnames(posEsc2)<-c("X","Y")
vtess <- deldir(posEsc2,rw=c(range(posEst[,1]),range(posEst[,2])))
plot(posEst,type="n",xlim=range(posEst2[,1]),ylim=range(posEst2[,2]))
points(posEsc2, pch=21, col=escuelas, cex=2)
points(posEsc, pch=22, col=escuelas, cex=2)
plot(vtess, wlines="tess", wpoints="none", number=FALSE, add=TRUE, lty=1)
points(posEst,col=cromosoma,pch=1+vuln,cex=0.5)





