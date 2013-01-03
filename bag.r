# ustawienia wywolania, mozna przeladowac, mozna zostawic
bag.problem <- list(
			weights=c(1,2,3),
			values=c(1,2,4),
			max_weight=3
		)

bag.init <- function (population_size) {
	P<-c()
	for(i in population_size){
		P[i] <- sample(1:length(bag.problem$weights))		
	}
	return(P)
}

bag.mutation <- function (individual) {
	indexes<-sample(1:length(individual),2,replace=T) 
	temp<-individual[indexes[1]]
	individual[indexes[1]]<-individual[indexes[2]]
	individual[indexes[2]]<-temp
	
	return(individual)
}

bag.crossover <- function (individuals) {
	#krosujemy
	return (child)
}

bag.replacement <- function (EP, EO) {
	ni <- length(EP$P)
	lambda <- length(EO)
	if(ni==lambda)
		return EO$individuals
	return c(EO$individuals, EP$individuals[1:(ni-lambda)])	#tutaj zakładamy że EP jest posortowane malejąco
}

bag.value <- function (individual) {
	len <-length(individual)
	value <- 0
	weights <- bag.problem$weights
	values  <- bag.problem$values
	bag_free_weight <-bag.problem$max_weight
	for(pos in 1:len){
		 if(weights[pos]<bag_free_weight) {
		 	bag_free_weight <- bag_free_weight - weights[pos]
		 	value <- value + values[pos]
		 	if(bag_free_weight == 0)
		 		break
		 }
	}
	return(value)
}