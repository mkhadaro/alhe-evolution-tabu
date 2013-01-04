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

bag.crossover <- function (parent1, parent2) {
#krzyzowanie PMX oparte o http://algorytmy-genetyczne.eprace.edu.pl/664,Implementacja.html
    parentLength <- length(parent1)
	crossLength <- sample(1:parentLength,1,replace=T)#dlugosc segemntu krzyzowania
	ibeg <- sample(1:(parentLength-crossLength+1),1,replace=T)#index poczatkowy seg. krzyz.
	iend <- (ibeg+crossLength-1) #index koncowy seg. krzyz.
	SegmentParent <- matrix(data=c(parent1[ibeg:iend], parent2[ibeg:iend]), byrow = T,nrow=2, ncol=crossLength)#segemnty krzyżowania dla obojga rodziców
	
	child <-c()
	child[ibeg:iend] <-SegmentParent[1,]
	for(locus in 1:parentLength){
		soughtAllele <-parent2[locus]
		saPosParent1 <- which( SegmentParent[1,] == soughtAllele) #soughtAllele position in parent 1
		saPosParent2 <- which( SegmentParent[2,] == soughtAllele) #soughtAllele position in parent 2

		if (length(saPosParent1) ) {
			newLocus <-saPosParent1[1]
			newSoughtAllele <- SegmentParent[2,newLocus]
			nsaPosParent1 <- which( SegmentParent[1,] == newSoughtAllele)
			if(soughtAllele == newSoughtAllele)
				next
			while (length(nsaPosParent1)){
				newLocus <- nsaPosParent1[1]
				newSoughtAllele <- SegmentParent[2,newLocus]
				nsaPosParent1 <- which( SegmentParent[1,] == newSoughtAllele)			
			}	
			newAllele <- newSoughtAllele
		}
		else if(length(saPosParent2)){
			newSoughtAllele <- soughtAllele
			nsaPosParent2 <- which( SegmentParent[2,] == newSoughtAllele)
			if(soughtAllele == newSoughtAllele)
				next
			while (length(nsaPosParent2)){
				newLocus <- nsaPosParent2[1]
				newSoughtAllele <- SegmentParent[1,newLocus]
				nsaPosParent2 <- which( SegmentParent[2,] == newSoughtAllele)
			}
			newAllele <- soughtAllele
			locus <- which( parent2 == newSoughtAllele)
		}
		else{
			newAllele <- soughtAllele
			}
		
		if (is.na(child[locus])){
			child[locus] <- newAllele
		}#
	}#for
	return (child)
}

bag.replacement <- function (EP, EO) {
	ni <- length(EP$P)
	lambda <- length(EO)
	if(ni==lambda)
		return (EO$individuals)
	return( c(EO$individuals, EP$individuals[1:(ni-lambda)]))	#tutaj zakładamy że EP jest posortowane malejąco
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