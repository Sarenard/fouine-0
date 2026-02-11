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
### Rendu 2

## Etapes à faire
1. Faire tout les opérateurs buildins
2. let sans in au toplevel
3. fonction avec unit en argument
4. exceptions
5. gérer les tuples sans parenthèses
6. Gérer les patterns dans les fonctions
7. matchs
8. listes
9.  Erreurs
10. types algébriques

# Fun sideprojects done
Calcul de 2^n à partir du brainfuck (attention, HEAVY use de LLMs (voir message de commit pour les détails des LLMs utilisés) pour gagner en temps, c'était surtout pour voir si c'était possible), voir dans le dossier de tests.

Command pour tester :
```
echo 5 | perl -ne 'chomp; print "+>".("+" x $_)."[-<[->>++<<]>>[-<<+>>]<]<.\n"' | xargs -I{} python tests/Fun/bf.py "{}" --standalone | _build/default/bin/fouine.exe
```

