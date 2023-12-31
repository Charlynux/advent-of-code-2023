readfile =: 1!:1
writefile =: 1!:2 

repository_folder =: '/Users/charles/git-repositories/advent-of-code-2023/'
read_input =: readfile@<@(repository_folder&,)
split_lines =: <;._2
parse_numbers =: (_&".@>)
parse_input =: parse_numbers@split_lines@read_input
exampleinput =: parse_input 'data/day09-example.input'

require 'debug/dissect'

NB. fork = (f g h)
NB. -~ -> le ~ inverse la soustraction : x -~ y = y - x
NB. 2 verbe/\ => Génère des fenêtres de 2 et applique le verbe aux paires.
subtract_pairs =: (2 -~/\ ])

NB. 0 = tableau -> retourne un tableau de booléens pour chaque élément du tableau
NB. On ajoute un & entre les deux pour créer un verbe.
NB. (# = +/) encore un fork
NB. On vérifie que le nombre d'éléments (#) est égal à la somme de ceux-ci.
NB. (Si tous les éléments sont des zéros, 0&= produira un tableau de 1.)
NB. @ -> composition des deux verbes
all_zero =: (# = +/)@(0&=)

NB. Si j'applique le traitement sur chaque ligne de l'exemple, c'est ok.
subtract_pairs@>{exampleinput

NB. Cette ligne tombe à 0 0... au bout de 2.
all_zero subtract_pairs subtract_pairs 0 3 6 9 12 15

NB.-----------------------------------------------------------------
NB. Recherches pour appliquer une récursion 
NB. à base de subtract_pairs et all_zero
NB.-----------------------------------------------------------------

NB. m @. v y
<: ` >: @. (0&<) -1

($:@subtract_pairs) ` [  @. all_zero 0 3 6 9 12 15

process=:($:@subtract_pairs)`([)@.all_zero

process 0 3 6 9 12 15
process 1 3 6 10 15 21

NB. process calcule bien, mais il me faut les valeurs intermédiaires

(0 >. <:)^:a: 5

(-.@all_zero) 1 3 6 10 15 21

NB. subtract_pairs^:(-.@all_zero) 1 3 6 10 15 21

dissect '(0 >. <:)^:(< 0$0) 5'

subtract_pairs^:(-.@all_zero) 1 3 6 10 15 21
(subtract_pairs^:(-.@all_zero))^:a: 1 3 6 10 15 21
NB.-----------------------------------------------------------------
NB.-----------------------------------------------------------------

NB. On peut calculer les "tableaux" résultants pour chaque ligne de l'exemple.
(subtract_pairs^:(-.@all_zero))^:a: @>{exampleinput

NB. On récupère la première valeur de chaque ligne.
NB. Le résultat est une liste par ligne originale.
{. @> { (subtract_pairs^:(-.@all_zero))^:a: @>{exampleinput

NB. Problème : La forme est constante.
NB. On obtient jamais de listes individuelles, mais des tableaux.
NB. Les lignes de ces tableaux sont complétées par des 0.
NB. 0 étant une valeur significative dans notre cas, c'est problèmatique.

NB. 0 2  0 3 10
NB. 0 2 -2 5 5
-/\. 10 3 0 2 0

NB. SOLUTION PARTIE 2
NB. {. @> { -> Extrait la première ligne pour chaque groupe
NB. |: -> Transpose les lignes deviennent des colonnes
NB. {. -/\. -> Cf. ci-dessus : C'est l'extrapolation demandée.
NB. +/ -> On fait la somme des valeurs extrapôlées.
+/ {. -/\. |: {. @> { (subtract_pairs^:(-.@all_zero))^:a: @>{parse_input 'data/day09.input'

NB. Solution de https://github.com/jitwit/aoc/blob/a/J/23/09.ijs

in =: parse_input 'data/day09.input'
+/(n,_1)p.~"1 in+/ .*"_ 1~%.^/~i.x:n=:{:$in

dissect '+/(n,_1)p.~"1 in+/ .*"_ 1~%.^/~i.x:n=:{:$in' [ in =. parse_input 'data/day09-example.input'


