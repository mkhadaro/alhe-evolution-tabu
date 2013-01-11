# założenia:
#	1. eval (I, value) sortuje wg malejącej oceny osobniki

meta.params <- list(
			ni =20,	#ni>lambda
			lambda = 15,
			max_iter = 50, #Iteracje do zakonczenia algorytmu
			prob_cross = 0.8,	
			prob_mut = 0.2,
			tabu_pop_size = 3, #Ilosc populacji pamietanych przez tabu
			tabu_penaulty = 0.1 # wartosci od 0 do 1, gdzie 1 oznacza brak kary, a 0 twarde tabu 
		)


meta.meta_evolution <- function () {
	P <- meta.problem.init(meta.params$ni) 
	T <- matrix(nrow=meta.params$tabu_pop_size*meta.params$ni, ncol=length(P[1, ]))
	tabu_it <- 0
	for(j in 1:meta.params$max_iter) {
		EP  <- meta.eval(P) 
		T_indexes <- meta.get_tabu_indexes(EP, T)
		O <- matrix(nrow=meta.params$lambda, ncol=length(P[1, ]))	
		#print(O)	
		for(i in 1:meta.params$lambda) {		
			if( meta.UG() < meta.params$prob_cross)	{
				parent_matrix<-meta.select(EP,T_indexes, 2)
				crossover_res<-meta.problem.crossover(parent_matrix[1,],parent_matrix[2,])
				O[i, ] <-crossover_res  # @TODO priority=low ile osobników ze sobą krzyżujemy powinna być zparamatryzowana (tak mówił arabas na wykładzie)
			}
			else
				O[i, ] <- meta.select(EP, T_indexes, 1)

			if(meta.UG() < meta.params$prob_mut) 
				O[i, ] <- meta.problem.mutation(O[i, ])
		}
		EO <- meta.eval(O)
		T<-meta.update_tabu(T, P, tabu_it)
		tabu_it <- ((tabu_it +1)%%meta.params$tabu_pop_size)	# cykliczne tabu iterator po populacjach (MRU most recently used) 
		P <- meta.replacement( EP,EO )
		#print(EP$values)
		RESULTS[NUM_EXECUTION, j] <<- EP$values[1]
		#print(bag.individual_weight(EP$individuals[1,]))
	} 
	
}

# /return	zwraca te indeksy z EP które odpowiadają osobnikom które są na tabu;
#			posortowane rosnąco
meta.get_tabu_indexes <- function (EP, T) {
	result <- c()

	size_tabu<-nrow(T)

	size_EP <- length(EP$values)

	for(i in 1:size_tabu) {
		for(j in 1:size_EP) {
			#print(meta.problem.equal_individuals(T[i, ], (EP$individuals)[j, ] ))
			if( meta.problem.equal_individuals(T[i, ], (EP$individuals)[j, ] ) ) {
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
		#print(p)
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

meta.meta_update_tabu <- function(T, P, tabu_it){
	offset <- (tabu_it)*meta.params$ni+1
	T[offset:(offset+meta.params$ni-1), ]<-P[1:meta.params$ni, ]
	return (T)
}

# @TODO przetestować
meta.meta_replacement <- function (EP, EO) {
	if(meta.params$ni==meta.params$lambda)
		return (EO$individuals)
	
	result<-matrix(byrow = T, ncol=ncol(EO$individuals), nrow=meta.params$ni)
	dif <- meta.params$ni-meta.params$lambda
	
	result[1:dif, ] <- (EP$individuals) [1:dif, ]
	result[(dif+1):meta.params$ni, ] <- (EO$individuals)[1:meta.params$lambda, ]

	return (result)
}




main<-function(){

	meta.problem.crossover <<- bag.crossover
	meta.problem.mutation <<- bag.mutation
	meta.problem.init <<- bag.init
	meta.problem.equal_individuals <<- bag.equal_individuals
	meta.problem.value <<- bag.value
	meta.select <<- meta.meta_select_tabu_tournament
	meta.update_tabu<<-meta.meta_update_tabu
	meta.replacement<<-meta.meta_replacement
	max_exec<-10
	RESULTS<<-matrix(byrow = T, ncol=meta.params$max_iter, nrow=max_exec)
	NUM_EXECUTION<<-1
	p<<-c(1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89)
	for(i in 1:max_exec) {
		set.seed(p[i])
		meta.meta_evolution()
		NUM_EXECUTION<<-NUM_EXECUTION+1
		print(".")
	}

	print(RESULTS)
	write.table(RESULTS,file="F:\\Studia\\VII Semestr Informa\\ALHE\\Projekt\\Myfile.csv",sep=";", row.names = 1:max_exec, col.names = 1:meta.params$max_iter)
	
}

