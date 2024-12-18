# Simulateur de coût de garde partagée

## Description du simulateur

L'application Shiny simule les coûts liés à une garde partagée à domicile entre deux familles. Nous cherchons à calculer plusieurs paramtres financiers pour que les familles sachent quels frais sont à engager mais aussi que l'assistante maternelle ait connaissance de l'argent qu'elle percevra : salaires nets, bruts, aides fiscales et aides de l'Etat, etc.

Pour réaliser cela, nous utilisons des informations conformes à la réglementation.

## Fonctionnalités mises en place

L'interface contient 2 onglets principaux:  Simulateur et Etude de cas. 

### Application - Onglet simulateur


Le simulateur est l'endroit où il faut saisir les données des deux familles pour réaliser les calculs de coût. 
Les inputs actuels sont le salaire brut horaire pour chaque famille, les heures travaillées par journée pour chaque famille, les repas pris en charge par jour et la possibilité d'indiquer s'il y a des périodes de vacances.
Les sorties actuelles sont les salaires nets et bruts (par heure et mensualisés), les coûts totaux pour les familles dont les salaires, les charges patronales, les indemnités des repas. 


### Application - Etude de cas

L'onglet étude de cas est encore en cours de traitement mais proposera à l'avenir des scénarios prédéfinis pour valider des calculs. 

### Prérequis pour l'utilisation de l'application

R et Rstudio sont évidemment essentiels, avec les packages suivant: shiny, shinydashboard, shinyWidgets, shinyTime, tidyr, ggplot2.

# A venir
- Ajout de graphiques pour une meilleure visualisation
-   Actuellement nous considérons que chaque famille ne fait garder qu'un enfant.
-   Un résumé de la réglementation permettant aux personnes utilisant le simulateur de connaître les critères de calculs.
- Comptabiliser correctement les semaines de vacances et les jours non travaillés exceptionnels dans les jours comptés.


