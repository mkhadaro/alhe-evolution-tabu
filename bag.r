# ustawienia wywolania, mozna przeladowac, mozna zostawic
bag.data <- list(
			weights=c(1,2,3),
			values=c(1,2,4),
			max_weight=3
		)
#TESTED
bag.init <- function (population_size) {
    weights_count <- length(bag.data$weights)
	P<-matrix(ncol=weights_count, nrow=population_size,  byrow = T)
	
	for(i in 1:population_size){
		P[i,]<-sample(1:weights_count,weights_count)		
	}
	return(P)
}
#TESTED
bag.mutation <- function (individual) {
	indexes<-sample(1:length(individual),2,replace=T) 
	temp<-individual[indexes[1]]
	individual[indexes[1]]<-individual[indexes[2]]
	individual[indexes[2]]<-temp	
	return(individual)
}

#TESTED
bag.equal_individuals <-function(i1, i2) {
	return (all(i1==i2))
}

#TESTED
bag.crossover <- function (parent1, parent2) {

#krzyzowanie PMX oparte o http://algorytmy-genetyczne.eprace.edu.pl/664,Implementacja.html
#Uwaga, algorytm tam napisany jest jednak nieco bledny
    if(length(parent1)!=length(parent2))
		stop("Nie mozna krzyzowac rodzicow o roznych rozmiarach")
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

		if (length(saPosParent1)) {#Wystapilo juz w segmencie krzyzowania
			next
		}
		else if (length(saPosParent2) ){#Jest w pominietym segmencie krzyzowania, trzeba wybrac nowe miejsce.
			newSoughtAllele <- soughtAllele
			while (length(saPosParent2)){
				newLocus <- saPosParent2[1]
				newSoughtAllele <- SegmentParent[1,newLocus]
				saPosParent2 <- which( SegmentParent[2,] == newSoughtAllele)
				if(!length(saPosParent2)){
					break
				}				
			}
			saPosParent2 <- which( parent2 == newSoughtAllele)
			newLocus <- saPosParent2[1]
			child[newLocus] <- soughtAllele
		}
		else{#Nie ma go w segmentach krzyzowania, dajemu tu gdzie stoi.
			child[locus] <- soughtAllele
			}
	}#for
	return (child)
}

bag.value <- function (individual) {
	len <-length(individual)
	value <- 0
	weights <- bag.problem$weights
	values  <- bag.problem$values
	bag_free_weight <-bag.problem$max_weight
	for(pos in individual){
		 if(weights[pos]<=bag_free_weight) {
		 	bag_free_weight <- bag_free_weight - weights[pos]
		 	value <- value + values[pos]
		 	if(bag_free_weight == 0)
		 		break
		 }
	}
	return(value)
}