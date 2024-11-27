# --- Calcul de net à Brut 

calcul_brut <- function(salaire){
  return (ceiling(salaire/(1-0.23)))
}

# --- Calcul part employeur

calcul_emp <- function(salaire){
  return (ceiling(salaire/(1-0.417)))
}

# --- Jsp encore

calcul_mensuel <- function(salaire,nb_heures){
  
}

# --- Nombre total d'heures ---

calcul_total_heure <- function(liste){
  print(liste)
  diff = 0
  for (jour in names(liste)){
    for (i in (1:length(liste[[jour]]))){
      if (liste[[jour]][[i]]!=""){
        liste[[jour]][[i]] <- conversion_heure_to_entier(liste[[jour]][[i]])
      }
    }
    print(liste[[jour]][[1]])
    if(liste[[jour]][[1]] != "" & liste[[jour]][[2]] != ""){
      diff = diff + (liste[[jour]][[2]] - liste[[jour]][[1]])
    }
    if(liste[[jour]][[3]] != "" & liste[[jour]][[4]] != ""){
      diff = diff + (liste[[jour]][[4]] - liste[[jour]][[3]])
    }
      
  }
  print(diff)
}

# --- Conversion d'une heure à un entier (nombre de minutes) ---
conversion_heure_to_entier <- function(heure){
  split_heure <- strsplit(heure, ":")[[1]]
  
  heures <- as.numeric(split_heure[1])
  minutes <- as.numeric(split_heure[2])  
  
  total_minutes <- heures * 60 + minutes
  
  # Afficher le résultat
  return(total_minutes)
}
