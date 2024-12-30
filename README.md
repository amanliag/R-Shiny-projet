# Simulateur de coût de garde partagée

## Description du simulateur

L'application Shiny simule les coûts liés à une garde partagée à domicile entre deux familles. Nous cherchons à calculer plusieurs parametres financiers pour que les familles sachent quels frais sont à engager mais aussi que l'assistante maternelle ait connaissance de l'argent qu'elle percevra : salaires nets, bruts, aides fiscales et aides de l'Etat, etc.

Pour réaliser cela, nous utilisons des informations conformes à la réglementation.
 
Le code de l'application se situe à cet emplacement : Garde_partagee/app.R et les librairies nécessaires sont dans le fichier dependencies.R (mais normalement, les librairies s'installent lors du lancement de l'application si elles sont absentes de l'environnement). Les fonctions princicpales se situent dans le fichier fonctions.R, même s'il demeure encore quelques fonctions incluses dans app.R . 

ATTENTION: Les semaines de congés, les jours fériés et toutes les réglementations ne sont pas encore pris en compte dans les calculs !

## Fonctionnalités mises en place

L'interface contient 4 onglets principaux:  Simulateur, Etude de cas, Législation à respecter concernant l'assistante maternelle, Droits et aides de l'employeur. 

### Onglet Simulateur

Le simulateur est l'endroit où il faut saisir les données des deux familles pour réaliser les calculs de coût. 
Les inputs actuels sont le salaire brut horaire pour chaque famille, les jours travaillés (case à cocher) puis les heures travaillées par journée pour chaque famille, les repas pris en charge par jour et la possibilité d'indiquer s'il y a des périodes de vacances.
Les sorties actuelles sont les salaires nets et bruts (par heure et mensualisés), les coûts totaux pour les familles dont les salaires, les charges patronales, les indemnités des repas. 
A cela s'ajoute deux graphiques simples à barres concernant les heures totales de chaque famille et le coût.

Des messsages d'avertissement guident l'utilisateur concernant le remplissage des informations (ex: un message d'avertissement s'affiche si le salaire brut horaire renseigné est inférieur au seuil).

### Onglet Etude de cas

L'onglet étude de cas est encore en cours de traitement mais proposera à l'avenir des scénarios prédéfinis pour valider des calculs. 

### Onglet Législation concernant l'assistante maternelle 

Cet onglet contient diverses informations concernant les droits de l'assistante maternelle et des règles à respecter pour souscrire au contrat: 
- Charges sociales et salaire minimum 
- Durée et orgnaisation du travail 
- Repos et vacances 
- Indemnités
- Formule du salaire annualisé 
- Calcul des congés payés 
- Travail de nuit 
- Précision sur les jours fériés

### Onglet Droits et aides de l'employeur

Cet onglet contient des informations quant aux aides que peut percevoir un parent : 
- Crédit d'impôt 
- Complément de libre choix du mode de garde 

### Prérequis pour l'utilisation de l'application

R et Rstudio sont évidemment essentiels, avec les packages suivant: shiny, shinydashboard, shinyWidgets, shinyTime, tidyr, ggplot2.

# A venir
- Ajout de graphiques pour une meilleure visualisation
-   Actuellement nous considérons que chaque famille ne fait garder qu'un enfant.
-   Un résumé de la réglementation permettant aux personnes utilisant le simulateur de connaître les critères de calculs.
- Comptabiliser correctement les semaines de vacances et les jours non travaillés exceptionnels dans les jours comptés.


