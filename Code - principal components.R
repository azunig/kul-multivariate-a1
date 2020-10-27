setwd ("C:/Users/Elena/Desktop/LEUVEN/Multivariate/MI PARTE")

load("C:/Users/Elena/Desktop/LEUVEN/Multivariate/MI PARTE/wvs.Rdata")

# Standardize table 1's items
install.packages("dplyr")
library(dplyr)


Table_1 = select(wvs,
                 V_creative,V_rich,V_secure, V_spoil_oneself, V_do_good, V_be_successful,
                 V_exciting_life, V_behave_properly, V_protect_environment, V_tradition, country)

V_creative = tapply(Table_1$V_creative,Table_1$country, mean)
V_rich = tapply(Table_1$V_rich,Table_1$country, mean)
V_secure = tapply(Table_1$V_secure,Table_1$country, mean)
V_spoil_oneself = tapply(Table_1$V_spoil_oneself,Table_1$country, mean)
V_do_good = tapply(Table_1$V_do_good,Table_1$country, mean)
V_be_successful = tapply(Table_1$V_be_successful,Table_1$country, mean)
V_exciting_life = tapply(Table_1$V_exciting_life,Table_1$country, mean)
V_behave_properly = tapply(Table_1$V_behave_properly,Table_1$country, mean)
V_protect_environment = tapply(Table_1$V_protect_environment,Table_1$country, mean)
V_tradition = tapply(Table_1$V_tradition,Table_1$country, mean)

Value_items = cbind(V_creative, V_rich, V_secure, V_spoil_oneself, V_do_good, 
                    V_be_successful,V_exciting_life,V_behave_properly, V_protect_environment, V_tradition)

# Correlation matrix

install.packages("openxlsx")
library(openxlsx) 

Corr_matrix = round(cor(Value_items),2)
write.xlsx(Corr_matrix,file="Corr_matrix.xlsx" )

# Now, we standardize:

Value_items_standardize = scale(Value_items[,1:10],center=TRUE, scale=TRUE)

# Scale (datos [], center=TRUE, scale=TRUE)
# Entre [,columnas] -- importante no poner nada al principio  
# (pues no queremos hacer nada por filas sino por columnas)
# Center=TRUE me resta la media (de la columna, pues solo especificamos columnas)
# Scale=TRUE me divide por la desviación típica (de la columna, pues solo especificamos columnas)


### NOTA IMPORTANTE: cuando tenemos este tipo de datos que hay que agrupar en función de 1 variable
#                  SIEMPRE agrupamos 1º y luego estandarizamos
#    Si lo hacemos al revés, mi tabla final no estará estandarizada y sus varianzas serán inferiores a 1 (por lo que autovalores sean inferiores a 1)


# AHORA, APLICAMOS COMPONENTES PRINCIPALES: prcomp(data[,nºcolumnas]), para que me lo aplique por columnas (pues cada columna es una variable)

PC = prcomp(Value_items_standardize[,1:10])


# Kaiser's rule: We take 2 principal components (value>1)

round(PC$sdev^2,2) 

# Sedimentation graph: 
par(pty="s", cex=1)
screeplot(PC,type="lines")

# Elbow: the slope of the curve falls drastically between 2 and 3 principal components
# Thereby, we determine that 2 is the elbow, and we will need to establish the cut before it 
# 1 principal component is optimal 
# sdev me toma los valores de la diagonal de la matriz diagonal que contiene la raíz de los autovalores
# para poder tener el autovalor y no su raíz, elevamos ésto al cuadrado
#round me sirve para redondear el resultado a dos decimales 

Eigenvalues_Variances = matrix(rep(0,10*4),ncol=4)
Eigenvalues_Variances[,1]=seq(1:10)
Eigenvalues_Variances[,2]=round(PC$sdev^2,2)
Eigenvalues_Variances[,3]=round(PC$sdev^2/10,2)
for (i in 2:10){
  Eigenvalues_Variances[i,4]=hey[i-1,3]+hey[i,3]}
  Eigenvalues_Variances[1,4]=hey[1,3]

colnames(Eigenvalues_Variances) <- c("Principal Component", "Eigenvalue","% Variance","Accumulated % Variance")
write.xlsx(Eigenvalues_Variances,file="Eigenvalues.xlsx" )

round(PC$sdev^2/10,2)  
# si queremos saber el % de varianza que me recoje cada autovalor, lo dividimos por el nº total de variables iniciales (en este caso, 10)
# The minimal accumulated variance (80%) will be obtained with 4 PC 

# Para comparar el criterio de Kaiser, aplicamos la metodología de Horn: BOOTSTRAPPING
# Create a new matrix whose values come from applying sample with replacement to the original data matrix
# REASON BEHIND IT: no correlation between variables on bootstrapped dataset

# Creamos una matriz para hacer el bootstrapping, a la que le ponemos 34 filas (nº países) y 10 columnas (nº variables)
# Todos los elementos de la matriz serán 0: luego los cambiaremos en el loop

bootstrapped_value_items = matrix(rep(0,34*10),ncol=10)

# rep(nº que queremos ponga siempre, nº veces que queremos que me ponga el nº, ncolumnas=)
# queremos que me ponga el nº 0 340 veces (34 valores por cada columna-- en total, 10 columnas)
# ncol= es necesario para saber las dimensiones de la matriz en la que queremos que pongan nuestros 340 ceros

# Sustituiremos los valores 0 de la matriz que hemos creado arriba por los de la sample con replacement

for (i in 1:10) {
  bootdata = sample(seq(1:34),size=34,replace=TRUE)
  bootstrapped_value_items[,i]<- Value_items[bootdata,i]}

# 2ª línea loop: 
# Podemos entenderla más fácil con otro ejemplo:

#  for (i in 1:10) {
#             bootstrapped_value_items[,i]<- Value_items[c(1:34),i]}

# Tomamos nuestra matriz datos original (Value_items) y queremos los valores
# de las filas 1 a la 34 y de cada columna i (cada columna por separado) 

# Estos valores los pegaremos en la matriz con todo ceros -- importante poner " ,i" para que me lo copie por columnas y no por filas
# Value_items[c(1:34),1] -- los datos me los da como una lista todo seguido unos de otros, no como una columna 
# Por tanto, es importante que indiquemos bootstrapped_value_items[,i] --para que me los pegue como una columna en matriz original

# Ahora que ya hemos entendido ésto:

#  for (i in 1:10) {
#  bootdata = sample(seq(1:34),size=34,replace=TRUE)
#  bootstrapped_value_items[,i]<- Value_itemsyes[bootdata,i]}

# bootdata contiene índices (importante!) que indican el nº de la row (no el valor en sí!) para el que queremos que me ponga el valor de la columna i

# Bootdata contiene índices que han sido elegidos al azar con remplazamiento de una secuencia de nº entre 1 y 34
# quiero que de una secuencia de nº entre 1 y 34, me elija aleatoriamente 34 nº (con remplazamiento)
# PARA ELLO USO LA FUNCIÓN: sample (datos entre los que elegimos, size=nº de datos que queremos tener al final, replace=TRUE)
# podríamos usar directamente la función sample dentro del [] de la 2ª línea, pero queda más limpio por separado


# AHORA, estandarizamos los bootstrapped values:
boot_stand_value_items = scale(bootstrapped_value_items[,1:10],center=TRUE,scale=TRUE)

# Hacemos componentes principales con standardized bootstrapped values y sacamos autovalores
PC_boot = prcomp(boot_stand_value_items)
round(PC_boot$sdev^2,2)
round(PC_boot$sdev^2/10,2)

# Mostramos en un eje los autovalores de la data con y sin bootstrap

# Si no me salen las labels y valores de los ejes, es porque tengo que ampliar los márgemes
# Para ello: par (mar=c(tamaño borde  inferior, tamaño borde izquieda, tam borde arriba, tam borde derecha))
par(mar=c(5,5,4,4))


plot(c(1:6),PC$sdev[1:6]^2, type="b", ylim=c(0,6), xlab="Component",ylab="Eigenvalue", main="Sedimentation Graph", col="blue")
# PC$sdev[1:6]^2 --> pongo [1:6] porque solo quiero que me salgan los primeros 6 autovalores
# especificar c(1:6) también es necesario para ésto

lines(c(1:6),PC_boot$sdev[1:6]^2, type="b", xlab="Component",ylab="Eigenvalue", col="red")

par(xpd=TRUE) #para que me deje poner la legend fuera del gráfico 
legend(4,5.5,c("Real data","Bootstrapped data"),lty=c(1,1), col=c("blue","red"), cex=0.9)



#######################################################################
#######################################################################

# NOW, standardized component scores:

loading_PC = PC$rotation%*%diag(PC$sd)
loading_PC_rounded = round(loading_PC,2)
write.xlsx(loading_PC_rounded,file="Standardized loadings.xlsx")


# NOW, unstandardized component scores:

PC_unstan = PC$rotation[,1:10]
PC_unstan = predict(PC)
loading_unstanPC_rounded = round(PC_unstan,2)
write.xlsx(loading_unstanPC_rounded, file="Unstandardized loadings.xlsx",replace=TRUE)

# Biplot

install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")

ggbiplot (pcobj=PC, pc.biplot=TRUE, varname.abbrev = FALSE)
require(ggbiplot)

biplot(PC, pc.biplot=TRUE)
## PARA HACER ITERACIONES CON INTERVALOS DE CONFIANZA:

iterate = 1000

boot_eigenvalues = matrix(rep(0,iterate*(10)),ncol=10)

for (i in 1:iterate){
    
    bootstrapped_iterate = matrix(rep(0,34*10),ncol=10)

    for (m in 1:10){
    indexes = sample(seq(1:34),size=n,replace=TRUE)
    bootstrapped_iterate[,m]<- Value_items[indexes,m]
    }
  
    boot_standardized = scale(bootstrapped_iterate[,1:10],center=TRUE,scale=TRUE)
    boot_pc = prcomp(boot_standardized)
    
    # Now, we compute the eigenvalues from the 1000 iterations of the content before
    # where each row contains the eigenvalues obtained for each iterations
    boot_eigenvalues[i,] = boot_pc$sdev^2
}

# Compute the mean and confidence intervals of the eigenvalues obtained with the 1000 iterations

mean_eigenvalues = apply(boot_eigenvalues,2,mean)
confidence_interval=apply(boot_eigenvalues,2,quantile, probs=c(.025,.975))

# Graph of eigenvalues obtained with the normal data VS. with bootstrapped data

par(mar=c(5,7,4,4))

plot(c(1:6),PC$sdev[1:6]^2, type="b", ylim=c(0,6), xlab="Component",ylab="Eigenvalue", main="Sedimentation Graph", col="blue")
lines(c(1:6),mean_eigenvalues[1:6], type="b", col="red")

lines(c(1:6),confidence_interval[1,1:6],lty="dashed", col="grey", lwd=2)
lines(c(1:6),confidence_interval[2,1:6],lty="dashed", col="grey", lwd=2)

par(xpd=TRUE) #para que me deje poner la legend fuera del gráfico 
legend(4,5.5,c("Real data","Bootstrapped data", "CI (Boostrapped data)"),lty=c(1,1,2),lwd=c(1,1,2),col=c("blue","red", "grey"), cex=0.9)

plot(c(1:6),PC$sdev[1:6]^2, type="b", ylim=c(0,4), xlab="Component",ylab="Eigenvalue", main="Sedimentation Graph", col="blue")
lines(c(1:6),mean_eigenvalues[1:6], type="b", col="red")

lines(c(1:6),confidence_interval[1,1:6],lty="dashed", col="grey", lwd=2)
lines(c(1:6),confidence_interval[2,1:6],lty="dashed", col="grey", lwd=2)

par(xpd=TRUE) #para que me deje poner la legend fuera del gráfico 
legend(4,5.5,c("Real data","Bootstrapped data", "CI (Boostrapped data)"),lty=c(1,1,2),lwd=c(1,1,2),col=c("blue","red", "grey"), cex=0.9)

lty=c(0.5,0.5)
lines(confidence_interval[1,1:6],lty="longdash", col="orange")
lines(confidence_interval[2,1:6],lty="dotted", col="orange")
lines(mean_eigenvalues, ylab="Eigenvalues", type="b", col="")


par(mar=c(5,7,4,4))

plot(PC$sdev^2, type="b", ylim=c(0,6), xlab="Component",ylab="Eigenvalue", main="Sedimentation Graph", col="blue")
lines(mean_eigenvalues, type="b", col="red")

lines(confidence_interval[1,],lty="dashed", col="grey", lwd=2)
lines(confidence_interval[2,],lty="dashed", col="grey", lwd=2)

par(xpd=TRUE) #para que me deje poner la legend fuera del gráfico 
legend(6.5,5.5,c("Real data","Bootstrapped data", "CI (Boostrapped data)"),lty=c(1,1,2),lwd=c(1,1,2),col=c("blue","red", "grey"), cex=0.9)



## 


