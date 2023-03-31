# rotacion-datos
##SEGUNDA SESION
# generar procesos en modelo tridimensional 

rot3d <- function(Ω=25,Ø=27,k=12,x1=10,y1=13,z1=8,lambd=50,Tx=5,Ty=8,Tz=6)
{
  ax <-Ω*(pi/180)
  ay<- Ø*(pi/180)
  az<-k*(pi/180)
  xΩ <- c(1,0,0)
  yΩ <-c(0, cos(ax),sin(ax))
  zΩ <-c(0, -sin(ax),cos(ax))
  RΩ <-rbind(xΩ,yΩ,zΩ)
  #
  xØ <-c(cos(ay),0,-sin(ay))
  yØ <-c(0,1,0)
  zØ <-c(sin(ay),0,cos(ay))
  RØ <-rbind(xØ,yØ,zØ)
  #
  xk <-c(cos(az), sin(az),0)
  yk <-c(-sin(az),cos(az),0)
  zk <-c(0,0,1)
  Rk <-rbind(xk,yk,zk)
  #
  w <-c(x1,y1,z1)
  P<- matrix(w,nrow = 3,ncol = 1)
  #
  tr<-c(Tx,Ty,Tz)
  Trs<-matrix(tr,nrow = 3,ncol = 1)
  #
  n <-(t(Rk%*%RØ%*%RΩ)%*%P)*lambd+Trs
  return(list(n))
}
rot3d()


rot4d <- function(Ω=25,Ø=27,k=12,x1=10,y1=13,z1=8,lambd=50,Tx=5,Ty=8,Tz=6)
{
  ax <-Ω*(pi/180)
  ay<- Ø*(pi/180)
  az<-k*(pi/180)
  a11 <- cos(ay)*cos(az)
  a12<- (-cos(ay)*sin(az))
  a13<-(sin(ay))
  a21<-(cos(ax)*sin(az)+sin(ax)*sin(ay)*cos(az))
  a22<-(cos(ax)*cos(az)-sin(ax)*sin(ay)*sin(az))
  a23<-(-sin(ax)*cos(ay))
  a31<-(sin(ax)*sin(az)-cos(ax)*sin(ay)*cos(az))
  a32<-sin(ax)*cos(az)+cos(ax)*sin(ay)*sin(az)
  a33<-cos(ax)*cos(ay)
  m1<-c(a11,a12,a13)
  m2<-c(a21,a22,a23)
  m3<-c(a31,a32,a33)
  w <-c(x1,y1,z1)
  P<- matrix(w,nrow = 3,ncol = 1)
  tr<-c(Tx,Ty,Tz)
  Trs<-matrix(tr,nrow = 3,ncol = 1)
  RET<- (rbind(m1,m2,m3)%*%P)*lambd+Trs
  return(list(RET))
}
rot4d()

# importando el conjunto de datos 
library(readr)
library(sp)
library(raster)
data <- read_csv("D:/Universidad Distrital 2022/Fotogrametria digital/semana 7  Principios matematicos aplicados a la fotogrametria/Datos/data.csv")
data
dat<-data.frame(data$x,data$Y,data$ND)
dat
plot(dat$data.x,dat$data.Y) 

dfr <- rasterFromXYZ(dat)
dfr
nrow(data)

plot(dfr)



# Intantando procesar con un conjunto de datos
dat1<-data.frame(data$x,data$Y,data$z)
dat1

dx <-(dat1$data.x)
dx
dy <-(dat1$data.Y)
dy
dz <-(dat1$data.z)
dz
mdat1 <- rbind(dx,dy,dz)
mdat1


rot3d1 <- function(Ω=25,Ø=27,k=12,dat=mdat1 ,lambd=50,Tx=5,Ty=8,Tz=6)
{
  ax <-Ω*(pi/180)
  ay<- Ø*(pi/180)
  az<-k*(pi/180)
  xΩ <- c(1,0,0)
  yΩ <-c(0, cos(ax),sin(ax))
  zΩ <-c(0, -sin(ax),cos(ax))
  RΩ <-rbind(xΩ,yΩ,zΩ)
  #
  xØ <-c(cos(ay),0,-sin(ay))
  yØ <-c(0,1,0)
  zØ <-c(sin(ay),0,cos(ay))
  RØ <-rbind(xØ,yØ,zØ)
  #
  xk <-c(cos(az), sin(az),0)
  yk <-c(-sin(az),cos(az),0)
  zk <-c(0,0,1)
  Rk <-rbind(xk,yk,zk)
  P<- dat
  tr<-c(Tx,Ty,Tz)
  #Trs<-matrix(tr,nrow = 1,ncol = 3)
  #
  n <-t((t(Rk%*%RØ%*%RΩ)%*%dat)*lambd+tr)
  
  return(list(n))
}
rot3d1()

dff1 <- data.frame(rot3d1(),data$ND)
dff1

plot(dff1$X1,dff1$X2) 


library(plot3D)
par(mfrow=c(1,2))
scatter3D(dff1$X1,dff1$X2,dff1$X3, clab = c("Sepal", "Width (cm)"))

scatter3D(data$x,data$Y,data$z, clab = c("Sepal", "Width (cm)"))
par(mfrow=c(1,1))

#Proyección de modelo afin 

rot3d2 <- function(Ω=25,Ø=27,k=12,dat=mdat1 ,lambd=50,Tx=5,Ty=8,Tz=6)
{
  ax <-Ω*(pi/180)
  ay<- Ø*(pi/180)
  az<-k*(pi/180)
  xΩ <- c(1,0,0)
  yΩ <-c(0, cos(ax),sin(ax))
  zΩ <-c(0, -sin(ax),cos(ax))
  RΩ <-rbind(xΩ,yΩ,zΩ)
  #
  xØ <-c(cos(ay),0,-sin(ay))
  yØ <-c(0,1,0)
  zØ <-c(sin(ay),0,cos(ay))
  RØ <-rbind(xØ,yØ,zØ)
  #
  xk <-c(cos(az), sin(az),0)
  yk <-c(-sin(az),cos(az),0)
  zk <-c(0,0,1)
  Rk <-rbind(xk,yk,zk)
  #
  P<- dat
  tr<-c(Tx,Ty,Tz)
  Trs<-matrix(tr,nrow = 3,ncol = 1)
  #
  lbd <- c(lambd,0,0,0,lambd,0,0,0,lambd)
  matris <-matrix(lbd,nrow = 3,ncol = 3)
  #
  ww <- t((t(RΩ%*%RØ%*%Rk)%*%matris)%*%dat+tr)
  
  return(list(ww))
}
rot3d2()


dff2 <- data.frame(rot3d2(),data$ND)
dff2

par(mfrow=c(1,3))
scatter3D(data$x,data$Y,data$z, clab = c("dato inicial", "Width (unid)"))
scatter3D(dff1$X1,dff1$X2,dff1$X3, clab = c("T.t.conforme", "Width (und)"))
scatter3D(dff2$X1,dff2$X2,dff2$X3, clab = c("T.t.afin", "Width (unid)"))

par(mfrow=c(1,1))
