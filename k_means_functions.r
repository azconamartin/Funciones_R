###formula para errr en kmedias iterando

###Funcion para calcular el error para kmedias
itera_ss <- function(data, k, seed=1234){
              a <- 0
              for (i in 2:k) {
                set.seed(seed)
                kmeans<-kmeans(data,i,nstart = 30)
                a[i] <- kmeans$betweenss/kmeans$totss
                print(paste("Suma de error para k = ",i, round(a[i],digits=2)))
              }
}

####funcion para graficar errores para cada grupo de kmedias
wssplot <- function(data, nc=15, seed=1234){
            wss <- (nrow(data)-1)*sum(apply(data,2,var))
              for (i in 2:nc){
              set.seed(seed)
              wss[i] <- sum(kmeans(data, centers=i)$withinss)}
              plot(1:nc, wss, type="b", xlab="Number of Clusters",
              ylab="Within groups sum of squares")}