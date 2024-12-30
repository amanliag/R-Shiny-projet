# --- Calcul de brut à net 

calcul_net <- function(salaire_brut) {
  return (ceiling(salaire_brut * (1 - 0.23)))
}

# --- Salaire brut annualisé 
calcul_salaire_mensualise_partage <- function(heures_par_semaine_f1, heures_par_semaine_f2, tarif_horaire_f1, tarif_horaire_f2, semaines_travaillees = 52) {
  salaire_f1 <- (semaines_travaillees * heures_par_semaine_f1 * tarif_horaire_f1) / 12
  salaire_f2 <- (semaines_travaillees * heures_par_semaine_f2 * tarif_horaire_f2) / 12
  salaire_total <- salaire_f1 + salaire_f2
    
  salaire_f1 <- round(salaire_f1, 2)
  salaire_f2 <- round(salaire_f2, 2)
  salaire_total <- round(salaire_total, 2)
  
  return(list(
    salaire_f1 = salaire_f1,
    salaire_f2 = salaire_f2,
    salaire_total = salaire_total
  ))
}


# --- Salaire net annualisé 
repartition_salaire_net_mensualise <- function(heures_f1, heures_f2, tarif_horaire_brut_f1, tarif_horaire_brut_f2, semaines_travaillees = 52) {
  tarif_net_f1 <- tarif_horaire_brut_f1 * (1 - 0.23)
  tarif_net_f2 <- tarif_horaire_brut_f2 * (1 - 0.23)
  
  contribution_net_f1 <- (heures_f1 * tarif_net_f1 * semaines_travaillees)/12
  contribution_net_f2 <- (heures_f2 * tarif_net_f2 * semaines_travaillees)/12
  
  total_contribution_net <- contribution_net_f1 + contribution_net_f2
  
  part_f1 <- contribution_net_f1 / total_contribution_net
  part_f2 <- contribution_net_f2 / total_contribution_net
  
  contribution_net_f1 <- round(contribution_net_f1, 2)
  contribution_net_f2 <- round(contribution_net_f2, 2)
  total_contribution_net <- round(total_contribution_net, 2)
  part_f1 <- round(part_f1, 2)
  part_f2 <- round(part_f2, 2)
  
  return(list(
    contribution_net_f1 = contribution_net_f1,
    contribution_net_f2 = contribution_net_f2,
    total_contribution_net = total_contribution_net,
    part_f1 = part_f1,
    part_f2 = part_f2
  ))
}


# --- Calcul part employeur

calcul_emp <- function(salaire){
  return (ceiling(salaire/(1-0.417)))
}

# --- Calcul remboursement transport

calcul_remb_transport <-function(frais_transport){
  return (ceiling(frais_transport*0.5))
}

# --- Charges patronales

charges_patronales <- function(salaire_brut) {
  if (is.null(salaire_brut) || !is.numeric(salaire_brut)) return(0)  
  taux_charges_patronales <- 0.4469
  charges_patronales <- salaire_brut * taux_charges_patronales
  return(round(charges_patronales, 2))
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

#### ------ VERIFICATION DES 9H max de travail par jour ------ ####
#A FAIRE