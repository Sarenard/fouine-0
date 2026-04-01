# Projet Fonctionnel, rendu 0

Ce répertoire contient le projet fouine, jusqu'au rendu 0

## Compilation et execution

Les commandes ci-dessous sont à exécuter dans le répertoire principal, qui est au-dessus de bin/ et lib/.

pour lancer fouine, faire
```
dune build && _build/default/bin/fouine.exe
```

pour lancer les tests, faire
```
python tests/test.py
```

pour lancer les tests sur -showsrc, faire
```
python tests/test_codegen.py
```

## Etapes réalisées

### Rendu 0
1. opérateurs booléens et `if.. then.. else`
2. fonction `prInt` (cf. les notes de cours)
3. ajout des déclarations let.. in
4. ajout des fonctions
5. prInt natif (dans l'environnement)
6. fonctions mu-récursives
7. tuples (comparaisons, support)
### Rendu 1
1. (Maya) begin end
2. (Sarah) Opérateurs buildins gérés correctement
3. (Sarah) refs
3. (Sarah) patterns dans les let
4. (Maya) let avec _
5. (Sarah) fonction avec unit en argument
6. (Sarah) matchs
7. (Sarah) Gérer les patterns dans les fonctions
8. (Sarah) Faire tout les opérateurs buildins
9. (Sarah) Let sans in au toplevel
10. (Sarah) Typage monomorphe des entiers et booléens
11. (Sarah) Typage (tout sauf le polymorphisme)
12. (Maya) Option -showsrc avec tests
14. (Sarah) Exceptions naives (sans pattern, a fix)
15. (Sarah) Parsing des let f x = ...
16. (Sarah) Gérer les tuples sans parenthèses
17. (Maya) Listes avec parsing litéral
18. (Maya) Match avec les listes
### Bugfixes Rendu 1
TODO : Votre parseur rejette les expressions arbitraires dans la condition d’un `if`, dans les expressions arithmétiques ou dans un `raise`.
TODO : Les arguments d’une fonction doivent être évalués avant la fonction elle-même.
TODO : La limite de 1000 références fait planter l’un de nos tests.
TODO : Vos fonctions sont prioritaires sur vos séquences, e.g. `fun x -> 0;1` ne devrait pas se réduire en `1`.
TODO : Il n’y a pas de filtrage à la définition d’une fonction, i.e. `let f = function ... `.
DONE (Sarah) : Le parseur rejette l’expression `[0,1]`.
DONE (Sarah) : Les arguments d’une liste doivent être évalués de droite à gauche.
DONE (Sarah) : `-noty` devrait être `-notypes`.
### Rendu 2

## Etapes à faire
3. Types algébriques
4. Continuations

# Fun sideprojects done
Calcul de 2^n à partir du brainfuck (attention, HEAVY use de LLMs (voir message de commit pour les détails des LLMs utilisés) pour gagner en temps, c'était surtout pour voir si c'était possible), voir dans le dossier "Fun" dans le dossier de tests.

Command pour tester (calcul de 2^5) :
```
echo 5 | perl -ne 'chomp; print "+>".("+" x $_)."[-<[->>++<<]>>[-<<+>>]<]<.\n"' | xargs -I{} python tests/Fun/bf.py "{}" --standalone | _build/default/bin/fouine.exe
```

