| nombre1 nombre2 resultat somme|somme := [ :a :b |    ^ a + b.]."Demander à l'utilisateur le premier nombre"nombre1 := (UIManager default request: 'Entrez le premier nombre:' translated initialAnswer: '') asNumber."Demander à l'utilisateur le deuxième nombre"nombre2 := (UIManager default request: 'Entrez le deuxième nombre:' translated initialAnswer: '') asNumber."Utiliser la fonction somme pour calculer la somme"resultat := somme value: nombre1 value: nombre2."Afficher le résultat dans la fenêtre Transcript"Transcript show: 'La somme de ', nombre1 printString, ' et ', nombre2 printString, ' est ', resultat printString; nl.