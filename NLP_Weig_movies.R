install.packages("stringr")               # Install stringr package
library("stringr")                        # Load stringr package

file0 <- 'Desktop/Ingoodwetrust/libros/NaturalLenguageProcessing/pelis_labels.csv'
label <- read.csv(file0)

label <- label[-c(1001),]
# First we charge the data. 
file <- 'Desktop/Ingoodwetrust/libros/NaturalLenguageProcessing/weight_movies_complete.csv'
Weight_movies <- read.csv(file)
pelis <- Weight_movies

# second we daleted the values that we don't need
pelis$movie_left <- NULL
pelis$description <- NULL
pelis$X <- NULL
pelis$movie_right <- NULL

#it's necesary apply PCA.
#i will separete the data in 4 diferents datasets

pelis1 <- pelis[,-(250:1015)]
#clp <- str_remove_all(pelis1, "c")                        # Apply str_remove function
#write.csv(pelis1,"Desktop/Ingoodwetrust/libros/NaturalLenguageProcessing/pelis1.csv", row.names = FALSE)

fit1 <- princomp(pelis1)
#fit <- princomp(data)
#fit <- princomp(pelis[,-(999:1015)])
summary(fit1)
datos <- data.frame(fit1$scores[,1:229])

datos <- data.frame(datos,pelis[,(250:1015)])
fit <- princomp(datos)
summary(fit)

datos2 <- data.frame(fit$scores[,1:583])


#it's necesary to make label to the data 
#ask to Dr tomorrow

#datos2 <- data.frame(datos,"label"=pelis$Label)
datos2$label <- as.factor(label)
fit.lda <- MASS::lda(label ~., data=datos2)

plot(fit.lda)
table(predict(fit.lda)$class,datos2$label)
fit$sdev^2/sum(fit$sdev^2)

#FactoMineR::PCA()
