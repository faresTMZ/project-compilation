# Projet compilation

### Présentation de ce qu'on a fait
Nous avons fait ce projet en binôme ( Louis Milhaud et Faris Tazi Mezalek).
Durant ce projet, nous avons ou traiter l'analyse lexicale, l'analyse grammaticale, la vérification des types et interprétation.
Sur la partie analyse lexicale, nous avons pu traiter tous les aspects de celle-ci. 
Quant à l'analyse grammaticale, nous avons eu du mal sur la partie récursivité et le sucre syntaxique des let / fonctions qui ne marchent donc pas.
A part ca le parser fonctionne bien, avec des règles de priorité respectées
La vérification des types était plus corsée notamment quand il s'agit de traiter des types créé dans le programme.
Il a fallut jouer avec les définitions de types et des tables de hachages. Et nous avons mis du temps a comprendre la relation entre les type_def et l'affectationd des structures.
La partie interprétation nous a posé le plus de problème notamment dans l'implémentation des fonctions, applications et structures avec la gestion de la mémoire et des clôtures.

### Notre erreur
Au final quand on commence a comprendre comment jongler entre les fichiers il n'y a rien d'insurmontable.
On a eu par contre pas mal de mal au niveau syntaxique, avec des erreurs de syntaxes bêtes dont on ne trouvait pas la solution sur internet ni dans le cours (ni le sujet).
Ce genre d'erreur prend est assez chronophage, nous regrettons donc de s'y être pris aussi tard (nous aurions bien aimé rendre un projet qui fonctionne !)

### La piste d'amélioration (de Louis)
Je pense que cette semaine je finirais le parser, j'ai compris l'histoire du sucre syntaxique avec les fun imbriqués dans un grand let mais je n'ai pas su l'implémenté a temps.
L'idée que j'ai eu est d'utiliser le champs expr dans le Fun (string * typ * expr) pour imbriquer d'autres fun et ainsi avoir des fonctions a plusieurs paramètres,
cependant il faut réfléchir a comment éviter de créer la possibilité à ce que des bêtises comme "(x1 : t1) = expr" puisse être parsé comme un expr.
