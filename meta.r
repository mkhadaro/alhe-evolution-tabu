data <- list(
			ni =12,	#ni>lambda
			lambda = 10,
			max_iter = 100, #Iteracje do zakonczenia algorytmu
			prob_cross = 0.8,	
			prob_mut = 0.01,
			tabu_pop_size = 3 #Ilosc populacji pamietanych przez tabu
		)


meta_evolution <- function(init, eval, value, select, mutation, crossover, scalar_params, UG) {
	P <- init(scalar_params$ni) 
	T <- replicate(scalar_params$tabu_pop_size, list()) #tworzy listę tylu pustych list ile ma trzymac tabu populacji (każda populacja jest odseparowana) 
	for(i in 1:scalar_params$max_iter) {
		EP  <- eval(P, value)		
		for(i in 1:scalar_params$lambda) {		
			if( UG() < scalar_params$prob_cross)	
				O[i] <- crossover(select(EP,T, 2)) # @TODO priority=low ile osobników ze sobą krzyżujemy powinna być zparamatryzowana (tak mówił arabas na wykładzie)
			else
				O[i] <- select(EP, T,1, scalar_params$ni)

			if(UG() < scalar_params$prob_mut) 
				O[i] <- mutation(O[i])
		}
		EO <- eval(O, value)
		update_tabu(T, P, scalar_params$tabu_pop_size)
		P <- replacement( EP,EO )

	} 
}


# UG - generator liczb losowych - odczyt kolejnej wartosci 
# ze zbioru liczb losowych lub generacja
UG<-function()
{
  return(runif(1,min=0,max=1))
}

# @TODO przetestuj czy to działa; poza tym jak sprawdzić z EP długość EP
# @TODO priority=superHiperHigh zwroc posortowaną malejąco listę!!
meta_eval <- function(I, value) { #I==individuals
	len <- length(I)
	E <- c()
	for(i in 1:len) {
		E[[i]]<-value(I[i])
	}
	return (list(values=E, individuals=P))
}

# @TODO priority=high wziąźć pod uwagę T i obniżyć prawdopodobieństwo wystartowania w szrankach osobnika z tabu
# @TODO priority=low wielkość szranek powinna zostać zparametryzowana (na wykładzie było jak wielkość szranek wpływa ogólnie na algorytm)
meta_select_tabu_tournament <- function (EP, T, num_selected) {
	ni <- length(EP$individuals)
	result <- c()
	for (i in 1:num_selected) {
		p<-sample(1:ni,2,replace=T)
		if(EP$values[p[1]]>EP$values[p[2]]) 
			result[i] <- EP$population[p[1]]
		else 
			result[i] <- EP$population[p[2]]
	}
	return(result)
}

# @TODO priority=medium zaimplementować drugą selekcję
#pobierac musi parametr ni - wielkosc populacji, gdyz wylosowac ma dwa dowolne osobniki, a więc dwa dowolne indeksy
#z zakresu 1:ni
meta_select_tabu_threshold <- function (EP, T, num_selected, ni){	
	return list()
}

update_tabu <- function(T, P, scalar_params$tabu_pop_size){
	T[1]<-NULL #usuniecie pierwszego elementu
	T[tabu_pop_size]<-P
}


