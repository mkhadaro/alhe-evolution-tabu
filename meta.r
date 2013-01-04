# założenia:
#	1. eval (I, value) sortuje wg malejącej oceny osobniki

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
		T_indexes <- get_tabu_indexes(EP, T, equal_individuals)
		O <- c()		
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

# /return	zwraca te indeksy z EP które odpowiadają osobnikom które są na tabu
get_tabu_indexes <- function (EP, T, equal_individuals) {
	result <- c()
	i_result <- 1
	
	T_linear <- unlist(T)
	l_T_linear <- length(T_linear) #length of T_linear
	
	if(l_T_linear==0)
		return

	l_EP <- length(EP$individuals)
	
	for(i in 1:l_T_linear {
		for(j in 1:l_EP) {
			if( equal_individuals(T_linear[i], EP$I[j]) )
				result[i_result]=j;
				i_result<-1+i_result;
		}
	}	
}

# UG - generator liczb losowych - odczyt kolejnej wartosci 
# ze zbioru liczb losowych lub generacja
UG<-function()
{
  return(runif(1,min=0,max=1))
}

# /return	zwraca posortowaną malejąco wg funkcji oceniającej value
# 			listę osobników (wraz z odpowiadającą listą obliczonych ocen);
# 			ma postać (list(c("Marian", "Marek", Judasz), c(8,6,5)))
meta_eval <- function(I, value) { #I==individuals
	len <- length(I)
	E <- c()
	for(i in 1:len) {
		E[i]<-value(I[i])
	}
	I<-I[order(E, decreasing=TRUE)]
	E<-sort(E, decreasing=TRUE)
	return (list(values=E, individuals=I))
}
# @przetestowane
# kod testujący:
# I<-("ma", "mmmmmm", "mamm")
# value <- function (i) {
#	return (nchar(i))
# } 
# meta_eval(I, value)
# zwraca odpowiednio posortowaną listę

# @TODO priority=low wielkość szranek powinna zostać zparametryzowana (na wykładzie było jak wielkość szranek wpływa ogólnie na algorytm)
# /description	tabu wpływa na prowdopodobieństwo wybrania osobnika do szranek
#				a nie na samą wartość osobnika gdy już jest w szrankach
#				zmniejszenie prawdopodobobieństwa wyboru sparametryzowane 
#				przez paramTabu
# /param T_indexes	te
# /return	zwraca num_selected osobników
meta_select_tabu_tournament <- function (EP, T_indexes, num_selected) {
	paramTabu <- 0.5 	# tutaj wpisujemy wagę prawdopodobobieństwa wyboru osobnika z tabu
						# gdzie waga dla osobników nie z tabu wynosi 1

	ni <- length(EP$individuals)
	weights<- c()	# tablica prawdopodobieństw wybrania osobnika z populacji
	
	for(i in 1:ni )	# równe szanse wybrania do szranek
		weights[i]=1
	
	for(i in 1:length(T_indexes))	# skorygowanie równych szans o zmniejszenie dla osobników z tabu
		weights[T_indexes[i]]=paramTabu 

#debug
#	for(i in 1:ni)
#		print(weights[i])		

	result <- c()
	
	for (i in 1:num_selected) {
		p<-sample(1:ni,2,replace=T, weights)
		if(EP$values[p[1]]>EP$values[p[2]]) 
			result[i] <- EP$individuals[p[1]]
		else 
			result[i] <- EP$individuals[p[2]]
	}
	
	return(result)
}
# test:
# P <- c("marian", "lukasz", "kasia" )
# V <- c(8, 7, 6)
# EP<-list(individuals=P, values=V)
# T_indexes <- c(2) #Łukasz w tabu
# num_selected <- 1
# meta_select_tabu_tournament (EP, T_indexes, numselected)

# @TODO priority=medium zaimplementować drugą selekcję
meta_select_tabu_threshold <- function (EP, T, num_selected){	
	return list()
}

meta_update_tabu <- function(T, P, scalar_params$tabu_pop_size){
	T[1]<-NULL #usuniecie pierwszego elementu
	T[tabu_pop_size]<-P
}

meta_replacement <- function (EP, EO) {
	ni <- length(EP$P)
	lambda <- length(EO)
	if(ni==lambda)
		return EO$individuals
	return c(EO$individuals, EP$individuals[1:(ni-lambda)])	#tutaj zakładamy że EP jest posortowane malejąco
}


