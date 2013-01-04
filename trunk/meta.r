# założenia:
#	1. eval (I, value) sortuje wg malejącej oceny osobniki

meta.params <- list(
			ni =12,	#ni>lambda
			lambda = 10,
			max_iter = 100, #Iteracje do zakonczenia algorytmu
			prob_cross = 0.8,	
			prob_mut = 0.01,
			tabu_pop_size = 3, #Ilosc populacji pamietanych przez tabu
			tabu_penaulty = 0.3 # wartosci od 0 do 1, gdzie 1 oznacza brak kary, a 0 twarde tabu 
		)


meta.meta_evolution <- function () {
	P <- meta.problem.init(meta.params$ni) 
	T <- replicate(meta.params$tabu_pop_size, list()) #tworzy listę tylu pustych list ile ma trzymac tabu populacji (każda populacja jest odseparowana) 
	for(i in 1:meta.params$max_iter) {
		EP  <- meta.eval(P) 
		T_indexes <- meta.get_tabu_indexes(EP, T)
		O <- c()		
		for(i in 1:meta.params$lambda) {		
			if( meta.UG() < meta.params$prob_cross)	
				O[i] <- meta.problem.crossover(meta.select(EP,T_indexes, 2)) # @TODO priority=low ile osobników ze sobą krzyżujemy powinna być zparamatryzowana (tak mówił arabas na wykładzie)
			else
				O[i] <- meta.select(EP, T_indexes, 1)

			if(meta.UG() < meta.params$prob_mut) 
				O[i] <- meta.problem.mutation(O[i])
		}
		EO <- meta.eval(O)
		meta.update_tabu(T, P, meta.params$tabu_pop_size)
		P <- meta.replacement( EP,EO )
	} 
	print(EP)
}

# /return	zwraca te indeksy z EP które odpowiadają osobnikom które są na tabu;
#			posortowane rosnąco
meta.get_tabu_indexes <- function (EP, T) {
	result <- c()
	
	T_linear <- unlist(T)
	l_T_linear <- length(T_linear) #length of T_linear
	
	if(l_T_linear==0)
		return

	l_EP <- length(EP$individuals)

	for(i in 1:l_T_linear) {
		for(j in 1:l_EP) {
			if( meta.problem.equal_individuals(T_linear[i], (EP$individuals)[j] ) ) {
				result<-append(result, j)
			}
		}
	}	
	return (unique(sort(result)))
}
# @przetestowane	

# UG - generator liczb losowych - odczyt kolejnej wartosci 
# ze zbioru liczb losowych lub generacja
meta.UG<-function()
{
  return(runif(1,min=0,max=1))
}

# /return	zwraca posortowaną malejąco wg funkcji oceniającej value
# 			listę osobników (wraz z odpowiadającą listą obliczonych ocen);
# 			ma postać (list(c("Marian", "Marek", Judasz), c(8,6,5)))
meta.eval <- function(I) { #I==individuals
	len <- nrow (I)
	E <- c()
	for(i in 1:len) {
		E[i]<-meta.problem.value(I[i,])
	}
	I<-I[order(E, decreasing=TRUE), ]
	E<-sort(E, decreasing=TRUE)
	return (list(values=E, individuals=I))
}
# @przetestowane

# @TODO priority=low wielkość szranek powinna zostać zparametryzowana (na wykładzie było jak wielkość szranek wpływa ogólnie na algorytm)
# /description	tabu wpływa na prowdopodobieństwo wybrania osobnika do szranek
#				a nie na samą wartość osobnika gdy już jest w szrankach
#				zmniejszenie prawdopodobobieństwa wyboru sparametryzowane 
#				przez paramTabu
# /param T_indexes	te
# /return	zwraca num_selected osobników
meta.meta_select_tabu_tournament <- function (EP, T_indexes, num_selected) {
	paramTabu <- meta.params$tabu_penaulty 

	ni <- length(EP$values)
	weights<- c()	# tablica prawdopodobieństw wybrania osobnika z populacji
	
	for(i in 1:ni )	# równe szanse wybrania do szranek
		weights[i]<-1
	

	for(i in T_indexes)	# skorygowanie równych szans o zmniejszenie dla osobników z tabu
		weights[i]<-paramTabu 


	result <- matrix(byrow = T, ncol=ncol(EP$individuals), nrow=num_selected)
	
	for (i in 1:num_selected) {
		p<-sample(1:ni,2,replace=T, prob=weights)
		print(p)
		if( (EP$values[p[1]]) > (EP$values[p[2]]) ) 
			result[i, ] <- EP$individuals[p[1], ]
		else 
			result[i, ] <- EP$individuals[p[2], ]
	}
	
	return(result)
}
# @przetestowane

# @TODO priority=medium zaimplementować drugą selekcję
meta.meta_select_tabu_threshold <- function (EP, T, num_selected){	
	return (list())
}

meta.meta_update_tabu <- function(T, P, tabu_pop_size){
	T[1]<-NULL #usuniecie pierwszego elementu
	T[tabu_pop_size]<-P
}

meta.meta_replacement <- function (EP, EO) {
	ni <- length(EP$individuals)
	lambda <- length(EO)
	if(ni==lambda)
		return (EO$individuals)
	return (c(EO$individuals, EP$individuals[1:(ni-lambda)]))	#tutaj zakładamy że EP jest posortowane malejąco
}



main<-function(){
	meta.problem.crossover <<- bag.crossover
	meta.problem.crossover <<- bag.mutation
	meta.problem.init <<- bag.init
	meta.problem.equal_individuals <<- bag.equal_individuals
	meta.problem.value <<- bag.value
	meta.select <<- meta.meta_select_tabu_tournament
	meta.meta_evolution()
}

