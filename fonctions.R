calcul_brut <- function(salaire){
  return (ceiling(salaire/(1-0.23)))
}

calcul_emp <- function(salaire){
  return (ceiling(salaire/(1-0.417)))
}

calcul_mensuel <- function(salaire,nb_heures){
  
}