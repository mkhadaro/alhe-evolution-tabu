#-------------------------------------------------------------------
# BAG TESTS
#-------------------------------------------------------------------


test.bag_init<-function(){
#print(bag.init(1))
	print(bag.init(25))
}

test.bag_mutation<-function(){
	print(bag.mutation(c(1,2,3,4,5,6,7,8,9)))
	print(bag.mutation(c(1,2,3)))
	print(bag.mutation(c(1)))
	print(bag.mutation(c(1,2,3,4,5,6,7,8,9,1,1,1,1,1,1,1,1,1,1,1,1,9,1,1,1,1,1,1,1,1,1,1,1,1)))
}

test.bag_equal_individuals<-function() {
	print ("Wszedzie ponizej powinno wypisac TRUE")
	print(bag.equal_individuals(c(1,2,3,4,5,6),c(1,2,3,4,5,6))==TRUE)
	print(bag.equal_individuals(c(1),c(1,2))==FALSE)
	print(bag.equal_individuals(c(2,1),c(2))==FALSE)
	print(bag.equal_individuals(c(1),c(6))==FALSE)
	print(bag.equal_individuals(c(1,2,3,4,5,6),c(1,2,3,4,6,5))==FALSE)
}

test.bag_crossover<-function(){
	print(bag.crossover(c(1,5,6,2,3,4), c(2,3,4,1,6,5)))
	print("Powinno wypisac blad o roznych rozmiarach:")
	print(bag.crossover(c(1,5,2,3,4), c(2,3,4,1,6,5)))
}

test.bag_value <-function(){
	#Przedefiniowuje tutaj, ¿eby mieæ sta³e dane dla testów.
	bag.data <- list(
			weights=c(1,2,3),
			values=c(1,2,4),
			max_weight=3
		)
	print("Powinno zwrocic same TRUE")	
	print(bag.value(c(1,2,3))==3)
	print(bag.value(c(3,1,2))==4)
	print(bag.value(c(3,2,1))==4)
}



#-------------------------------------------------------------------
# META TESTS
#-------------------------------------------------------------------

test.meta_get_tabu_indexes <- function () {
	P <- bag.init(10)
	EP<- meta.eval(P)
	
	return (meta.get_tabu_indexes(EP, T, equal_individuals))
}

test.meta_select_tabu_tournament <- function() {
	P <- bag.init(10)
	EP<- meta.eval(P)
	T_indexes <- c(1:9) 
	num_selected <- 2
	print(EP)
	return (meta.meta_select_tabu_tournament (EP, T_indexes, num_selected))
}

test.meta_eval <- function() {
	I<-bag.init(10)
	meta.problem.value <-bag.value
	#bag.value(I[1])	
 	return (meta.eval(I))
}

