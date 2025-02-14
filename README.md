# Simulateur de coût de garde partagée


## Description du simulateur

L'application Shiny simule les coûts liés à une garde partagée à domicile entre deux familles. Nous cherchons à calculer plusieurs paramètres financiers pour que les familles sachent quels frais sont à engager mais aussi que l'assistante maternelle ait connaissance de l'argent qu'elle percevra : salaires nets, bruts, aides fiscales et aides de l'Etat, etc.

Pour réaliser cela, nous utilisons des informations conformes à la réglementation.
 
Le code de l'application se situe à cet emplacement : Garde_partagee/app.R et les librairies nécessaires sont dans le fichier dependencies.R (mais normalement, les librairies s'installent lors du lancement de l'application si elles sont absentes de l'environnement). Les fonctions princicpales se situent dans le fichier fonctions.R, même s'il demeure encore quelques fonctions incluses dans app.R . 


## Fonctionnalités mises en place

L'interface contient 4 onglets principaux:  Simulateur, Etude de cas, Législation à respecter concernant l'assistante maternelle, Droits et aides de l'employeur. 

### Onglet Simulateur

Le simulateur est l'endroit où il faut saisir les données des deux familles pour réaliser les calculs de coût. 
Les inputs actuels sont:
- le salaire brut horaire pour les 2 familles, 
- les jours travaillés (case à cocher) puis les heures travaillées par journée pour chaque famille, 
- les repas pris en charge par jour  pour chaque famille,
- les modes de transports pris en charge et les frais que cela coûte, 
- la possibilité d'indiquer 6 semaines de vacances.

Concernant l'aide financière: 
L'utilisateur indique s'il a le droit au Complément de libre choix du mode de garde (CMG) et doit indiquer le montant en euros. 
Le droit au crédit d'impôt est ensuite calculée automatiquement.

Les sorties actuelles sont les salaires nets et bruts (par heure et mensualisés, et prenant en compte les heures complémentaires et supplémentaires), les coûts totaux pour les familles dont les salaires, les charges patronales, les indemnités des repas, les indemnités de frais de déplacement mais aussi le montant annuel et mensuel du CMG et le droit au crédit d'impôt avec son montant si applicable.

L'assitante maternelle a aussi connaissance de:
 - Heures totales par semaine 
 - Salaire annualisé total net
 - Indemnités repas totales 
 - Indemnités déplacement 
 - Semaines de congés


A cela s'ajoute deux graphiques simples à barres concernant les heures totales de chaque famille et le coût.

Des messsages d'avertissement guident l'utilisateur concernant le remplissage des informations:
- un message d'avertissement s'affiche si le salaire brut horaire renseigné est inférieur au seuil
- un message d'avertissement s'affiche si l'amplitude horaire ou le nombre d'heures par jour dépasse 13h. Ce dernier reste jusqu'à ce que l'utilisateur ait correctement modifié les inputs.
- un message d'avertissement s'affiche si une famille demande 7 jours ou consécutifs ou que le jour de repos n'est pas le même lorsque chaque famille demande à l'assistante maternelle de travailler 6 jours.

### Onglet Etude de cas

L'onglet étude de cas n'a pas pu être réalisé.

### Onglet Législation concernant l'assistante maternelle 

Cet onglet contient diverses informations concernant les droits de l'assistante maternelle et des règles à respecter pour souscrire au contrat: 
- Charges sociales et salaire minimum 
- Durée et orgnaisation du travail 
- Repos et vacances 
- Indemnités repas et transport
- Formule du salaire annualisé 
- Calcul des congés payés 
- Travail de nuit 
- Précision sur les jours fériés

### Onglet Droits et aides de l'employeur

Cet onglet contient des informations quant aux aides que peut percevoir un parent : 
- Crédit d'impôt 
- Complément de libre choix du mode de garde 

Différents liens sont à sa disposition, dont un exemple de contrat de travail. 

### Prérequis pour l'utilisation de l'application

R et Rstudio sont évidemment essentiels, avec les packages suivant: shiny, shinydashboard, shinyWidgets, shinyTime, tidyr, ggplot2.

## A améliorer: 

Malheureusement, nous n'avons pas eu le temps de considérer: 
- la question des jours fériés 
- la vérification des bornes temporelles quant aux vacances 
- un message d'alerte pour indiquer aux utilisateurs qu'il y aura des heures complémentaires ou supplémentaires 
- Ajout de graphiques pour une meilleure visualisation 


