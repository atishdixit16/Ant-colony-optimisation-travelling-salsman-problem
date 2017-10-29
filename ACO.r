set.seed(50)
n_cities <- 10
distance <- matrix((sample(n_cities^2)),n_cities,n_cities)
for (i in 1:n_cities){
	for (j in i:n_cities){
	distance[i,j]=distance[j,i]
	}
}
diag(distance) = 0
phermones <- matrix(1,n_cities,n_cities)

pathDistance <- function(path,distance) {
	d <- 0
	for (i in 1:(length(path)-1))
		d <- d + distance[ path[i] , path[i+1] ]
	return(d)
}

ACO <- function(distance,phermones, n_ants=50, max_itr=1000, probExploit=0.7, minPher=5, deltaReduce=0.9, display=FALSE, cordList) {

n_cities <- nrow(distance)
n_random <- n_cities*10
max <- -1
min <- 10e10
for (i in 1:n_random) {
	dist <- pathDistance(sample(n_cities),distance)
	if ( dist > max)
		max <- dist
	if (dist < min)
		min <- dist
}

for (itr in 1:max_itr) {
	pherClosProd <- (1/distance)*phermones
	dist_array <- NULL
	minDist <- 10e10
	for (ant in 1:n_ants) {
		path <- NULL
		citiesNotCovered <- 1:n_cities
		preCity <- sample(citiesNotCovered,1)
		path <- c(path,preCity)
		citiesNotCovered <- citiesNotCovered[-preCity]
		for (i in 1:(n_cities-2)) {
			if (runif(1) < probExploit) {
				nextCity <- citiesNotCovered[ which(pherClosProd[preCity,citiesNotCovered] == max(pherClosProd[preCity,citiesNotCovered])) ]
			}
			else {
				scaledPCproduct <- ( pherClosProd[preCity,citiesNotCovered] - min(pherClosProd[preCity,citiesNotCovered]) ) / ( max(pherClosProd[preCity,citiesNotCovered]) - min(pherClosProd[preCity,citiesNotCovered]) )
				probCut <- scaledPCproduct / sum(scaledPCproduct)
				nextCity <- sample(citiesNotCovered,1,prob=probCut)
			}
			citiesNotCovered <- citiesNotCovered[-which(citiesNotCovered==nextCity)]
			path <- c(path,nextCity)
			preCity <- nextCity 
		}
		path <- c(path,citiesNotCovered)
		if (pathDistance(path,distance) < minDist) {
			minDist <- pathDistance(path,distance)
			minPath <- path
		}
	}
	#for (i in 1:(n_cities-1)) {
	#	phermones[minPath[i],minPath[i+1]] <- (1/deltaReduce)*(  phermones[minPath[i],minPath[i+1]] + ( minPher - (min - minDist) ) )
	#	phermones[minPath[i+1],minPath[i]] <- (1/deltaReduce)*( phermones[minPath[i+1],minPath[i]] + ( minPher - (min - minDist) ) )
	#}
	phermones <- (1-deltaReduce)*phermones
	for (i in 1:(n_cities-1)) {
		phermones[minPath[i],minPath[i+1]] <- phermones[minPath[i],minPath[i+1]] + deltaReduce*(1/minDist)
		phermones[minPath[i+1],minPath[i]] <- phermones[minPath[i+1],minPath[i]] + deltaReduce*(1/minDist)
	}
	if (display==TRUE) {
		arg_min <- minPath
		plot(c( cordList[arg_min[1],1], cordList[arg_min[2],1] ) , c( cordList[arg_min[1],2], cordList[arg_min[2],2] ), type='b', xlim = c(12,28) , ylim = c(90,100) )
                for (i in 2:(n_cities)) 
                	lines(c ( cordList[arg_min[i] , 1], cordList[arg_min[i+1] , 1] ) , c( cordList[arg_min[i] , 2], cordList[arg_min[i+1] , 2] ) , type='b')
		readline()
	}

}

	return(list(path=minPath, distance=minDist))

}

a <- read.csv('f14.csv')
distance <- as.matrix(dist(a))
n_cities <- nrow(distance)
phermones <- matrix(1,n_cities,n_cities)
output <-  ACO ( distance,phermones, n_ants=10, max_itr=50, probExploit=0.6, minPher=5, deltaReduce=0.9, display=TRUE, cordList=a )
print(output)
