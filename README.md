# MT2017

## [Sujet](http://www-verimag.imag.fr/~perin/enseignement/L3/mcal/projet/)

- Simulation de MT opérant sur un alphabet S à 2n symboles par des MT sur l'alphabet {B,D} ***Emulator.ml à compléter***
    1. Conversion d'une bande sur S en une bande équivalente sur {B,D} et inversement
    2. Simulation d'une transition de MT opérant sur S en une séquence de transitions opérant sur {B,D}
    3. Comparaison du résultat après chaque simulation d'une transition
    4. Simulation de MT sur l'alphabet {B,Z,U} par {B,D,S} (pour la mise au point) [exemple](http://www-verimag.imag.fr/~perin/enseignement/L3/mcal/projet/binary_simulation_TM_incr.html)
    5. Simulation de MT sur un alphabet S à 2n symboles par des MT sur l'alphabet {B,D}
- Une MT qui effectue la beta-réduction ***LC\_by\_MT.ml à compléter***
    1. MT (à 3 bandes) qui sélectionne un terme bien parenthésés
    2. MT (à 3 bandes) qui effectue la substitution de chaque occurence d'une variable par un terme
    3. MT (à 4 bandes) qui effectue la substitution de chaque occurence d'une variable par un terme en tenant compte du masquage de variable par un lambda 
    4. Un affichage HTML plus élégant
    5. Simulation binaire de la MT qui effectue la beta-réduction
- Bonus: (***) Génération de la MT binaire (avec fusion de certains états cf. TD1)


## TODO: 
1. Completer les fichiers: 
    - LC\_by\_MT.ml
    - Emulator.ml
    - Action.ml
    - Band.ml
    - Turing_Machine.ml
    - main.ml
