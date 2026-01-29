# Projet Fonctionnel, rendu 0

Ce répertoire contient le projet fouine, jusqu'au rendu 0

## Compilation et execution

Les commandes ci-dessous sont à exécuter dans le répertoire principal, qui est au-dessus de bin/ et lib/.

pour lancer le programme, faire
```
dune build && dune exec bin/fouine.exe
```

## Etapes réalisées

1. opérateurs booléens et `if.. then.. else`
2. fonction `prInt` (cf. les notes de cours)
3. ajout des déclarations let.. in
4. ajout des fonctions
5. prInt natif (dans l'environnement)
6. fonctions mu-récursives
6. tuples (comparaisons, support)

## Etapes à faire
1. gérer les tuples sans parenthèses

## Fun sideprojects done
Calcul de 2^n à partir du brainfuck (attention, HEAVY use de LLMs (voir message de commit pour les détails) pour gagner en temps (car deadline proche), c'était surtout pour voir si c'était possible).

Command pour tester :
```
echo 5 | perl -ne 'chomp; print "+>".("+" x $_)."[-<[->>++<<]>>[-<<+>>]<]<.\n"' | xargs -I{} python tests/bf.py "{}" --standalone | _build/default/bin/fouine.exe
```

## Date limite pour téléverser ce rendu sur la page du portail des études :

   **lundi 2 février 2025 à 23h59**

     . indiquez brièvement au début du fichier Readme ce que vous avez pu ajouter à fouine
     . envoyez une archive **qui compile**
