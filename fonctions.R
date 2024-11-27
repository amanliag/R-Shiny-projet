# --- Calcul de net à Brut 

calcul_brut <- function(salaire){
  return (ceiling(salaire/(1-0.23)))
}

# --- Calcul part employeur

calcul_emp <- function(salaire){
  return (ceiling(salaire/(1-0.417)))
}

# --- Calcul remboursement transport

calcul_remb_transport <-function(frais_transport){
  return (ceiling(frais_transport*0.5))
}

# --- Jsp encore

calcul_mensuel <- function(salaire,nb_heures){
  
}

# --- Nombre total d'heures ---

calcul_total_heure <- function(liste){
  bilan <- list()
  diff = 0
  for (jour in names(liste)){ # On se balade jour par jour
    for (i in (1:length(liste[[jour]]))){ # On regarde les 4 cas, matin début, matin fin etc.
      if (liste[[jour]][[i]]!=""){ # Si une valeur a été rentrée...
        liste[[jour]][[i]] <- conversion_heure_to_entier(liste[[jour]][[i]]) # ...alors on converti l'heure en entier (minutes)
      }
    }
    if(liste[[jour]][[1]] != "" & liste[[jour]][[2]] != ""){ # Si matin début et matin fin sont rentrés par l'utilisateur
      diff = diff + (liste[[jour]][[2]] - liste[[jour]][[1]]) # alors on calcule la différence
    }
    if(liste[[jour]][[3]] != "" & liste[[jour]][[4]] != ""){ # Idem pour après-midi
      diff = diff + (liste[[jour]][[4]] - liste[[jour]][[3]])
    }
      
  }
  return(diff)
}

### ----- TRAITEMENT CONVERSIONS HEURE ----- #####

# --- Conversion d'une heure à un entier (nombre de minutes) ---
conversion_heure_to_entier <- function(heure){
  split_heure <- strsplit(heure, ":")[[1]] # On sépare les minutes et les heures
  heures <- as.numeric(split_heure[1]) # On les converti en int
  minutes <- as.numeric(split_heure[2])  
  
  #- Calcul -
  total_minutes <- heures * 60 + minutes 
  
  # Afficher le résultat
  return(total_minutes)
}

# --- Conversion d'un entier à une heure (format HH:MM) ---
conversion_entier_to_heure <- function(entier){
  heure <- sprintf("%02d:%02d", entier %/% 60, entier %% 60)
  return(heure)
}
