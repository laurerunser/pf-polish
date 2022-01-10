# Projet de PF5 - 2021/2022

## Etudiante
Laure Runser
numero etudiant 21955060
identifiant Gitlab : @runser

## Fonctionnalites
Le program peut :
- lire un fichier polish depuis le disque
- avec l'option `-reprint`, affiche le program sur stdout
    - respecte l'indentation du fichier
- avec l'option `eval`, evalue le program
- avec l'option `simpl`, simplifie le program et l'affiche sur stdout
- avec l'option `vars`, fait une analyse statique des variables et affiche :
    - la liste complete des variables dans le program
    - la liste des variables qui sont accedees avant d'avoir ete initialisees
- avec l'option `sign`, fait une analyse statique des signs potentiels des variables du program
    - pour chaque variable, affiche son nom, et -0+! pour signaler un sign negatif, zero, positif ou une erreur
    - a la fin, affiche une ligne avec "safe" si le program ne peut jamais faire une division par zero, ou "divbyzero N" ou N est le numero de la premiere ligne ou il pourrait y avoir une division par zero.
    - j'ai choisi de donner un sign -0+ aux variables non initialisees, et de reserver le sign ! aux divisions par zero uniquement

## Compilation et execution
Compiler avec `make clean && make`
Executer avec `./run arg1 arg2`
Pour afficher la liste des options : `./run`

## Decoupage modulaire
J'ai decoupe le program en plusieurs modules :
- Polish : les types et la syntaxe
- Main : boucle d'execution principale qui lance les fonctionnalites en fonction de l'option
- Parse : le parsing d'un fichier
- Print : pour afficher un program
- Eval : pour evaluer un program
- Simpl : pour simplifier un program
- Var : pour faire l'analyse statique des variables initialisees/non initialisees
- Sign : pour faire l'analyse statique des signs des variables

## Organisation du travail et chronologie
20 au 26 novembre : parsing des fichiers et option -reprint
2 decembre : option -eval
11 au 22 decembre : covid, j'ai fait un peu de doc
25 decembre au 7 janvier : prise de retard a cause de la disparition de mes binomes en POO et SYS
8 janvier : options -simpl et vars
9 au 10 janvier : option -sign

## Remarques et suggestions
Je sais que ce n'est pas une excuse et que j'aurais du m'organiser autrement, mais je souhaitais tout de meme signaler que j'ai eu le covid debut decembre, qui m'a mise a pied pendant 9 jours.
J'ai aussi eu des problemes de binomes sur les 2 autres projets (POO et Systeme) donc j'ai du faire leurs parts de travail.

Je voulais aussi m'excuser du manque d'accents sur ce rapport : je l'ai fini peu avant la deadline sur un clavier qwerty, je n'ai pas eu le temps de solutionner mon clavier pour retrouver les accents.