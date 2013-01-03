data <- list(
			lambda = 10,
			max_iter = 100, #Iteracje do zakonczenia algorytmu
			prob_cross = 0.8,	
			prob_mut = 0.01,
			tabu_pop_size = 3 #Ilosc populacji pamietanych przez tabu
		)


meta_evolution <- function(init, eval, value, select, mutation, crossover, scalar_params, UG) {
	P <- init(scalar_params$ni) 
	T <- list()
	for(i in 1:scalar_params$max_iter) {
		EP  <- eval(P, value)		
		O
		for(i in 1:scalar_params$lambda) {		
			if( UG() < scalar_params$prob_cross)	
				O[i] <- crossover(select(EP,T, 2))
			else
				O[i] <- select(EP, T,1)

			if(UG() < scalar_params$prob_mut) 
				O[i] <- mutation(O[i])
		}
		OE <- eval(O)
		update_tabu(T, P, scalar_params$tabu_pop_size)
		P <- replacement( PE,OE )

	} 
}


# UG - generator liczb losowych - odczyt kolejnej wartosci 
# ze zbioru liczb losowych lub generacja
UG<-function()
{
  return(runif(1,min=0,max=1))
}

meta_eval <- function(P, value) {
	len <- length(P)
	E <- c()
	for(i in 1:len) {
		E[i]<-value(P[i])
	}
	return (list(values=E, population=P))
}

meta_select_tournament <- function (EP, T, num_individual) {
	result <- c()
	for (i in 1:num_individual) {
		#wybierz 2 elementy p1 p2
		if(EP$values[p1]>EP$values[p2]) 
			result[i] <- EP$population[p1]
		else 
			result[i] <- EP$population[p2]
	}
	return(result)
}

update_tabu <- function(T, P, tabu_pop_size){
	#TODO
}


