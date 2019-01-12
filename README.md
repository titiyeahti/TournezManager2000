# Projet Ipf S3

## Description 

L'objectif de ce projet est de coder un executable capable de trouver une solution correcte au problème du voyageur de commerce en distance euclidienne.

### Entrée

Ce programme prend 2 fichier en entrée : 


Le premier contient le paramètrage du programme et suit la syntaxe suivante :

	
	ONE|HULL
	RANDOM|NEAREST|FARTHEST
	INVERSION|REPOSITIONNEMENT


La première ligne décrit la façon dont sera initialisée le chemin : soit avec une ville aléatoire soit avec l'enveloppe convexe des villes.
La seconde décrit l'ordre dans lequel seront insérées les villes restantes.
La troisième désigne l'heuristique d'optimisation qui sera utilisée une fois toutes les villes insérées.


Le second fichier contient les données du problème c'est à dire le nom des villes et leurs coordonnées.

	
	n /* le nombre de villes */
	<nom de la ville1> <longitude> <lattitude>
	...
	...
	<nom de la villeN> <longitude> <lattitude>


où longitude et latitude sont des flotants.



### Sortie

Le programme renvoie la solution trouvée sur la sortie standard au format suivant :


	<distance> : <ville1> ... <villeN>


### Temps d'éxecution 
Ce programme a pour contrainte se terminer en un temps raisonnable pour un nombre de ville de l'odre du milier.


## Description des fichiers
Le code source du projet se trouve dans `/src` :
- la partie algorithmique est centralisée dans `/src/chemin.ml`;
- le code de l'exécutable dans `/src/main.ml`;
- les fonctions de lecture de fichiers dans `/src/parse.ml`;

Le dossier `/test_files` contient des fichier de test.



## Pour compiler


	$ cd src
	$ make main


## Pour exécuter 

		
	($ cd src)
	$ ./main.native fichier_param fichier_input


## Tester

Le script `src/test.sh` va executer le programme en prennant en argument plusieurs fichiers de `/test_files`. Pour que ce script fonctionne, il faut d'abord compiler l'exécutable. Ou plus simplement :

	
	$ cd src 
	$ make test

	

