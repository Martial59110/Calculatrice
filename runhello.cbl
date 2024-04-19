       IDENTIFICATION DIVISION.
       PROGRAM-ID. runhello.
       AUTHOR. Martial FLoquet.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-A PIC S9(9)V99.
       01  WS-EXIT PIC A.
       01  WS-OPERATOR PIC X.
       01  WS-RESULT PIC S9(9)V99.
       01  WS-RESULT-TEMP PIC S9(9)V99.
       01  WS-A-CLEAN PIC -Z(7)9.99.
       01  WS-RESULT-CLEAN PIC -Z(7)9.99.
       01  WS-RESULT-TEMP-CLEAN PIC -Z(7)9.99. 
      
       
       PROCEDURE DIVISION.

      *    Déclaration de mon paragraphe MAIN

           PERFORM 0000-MAIN-START
           THRU    0000-MAIN-END.


      *    Début du Main

       0000-MAIN-START. 


      *    Stocke la valeur de WS-RESULT dans le TEMP à chaque nouvelle 
      *    boucle 
      

           SET WS-RESULT-TEMP TO WS-RESULT.

      *    IHM du début

           DISPLAY "Le résultat actuel est :" WS-RESULT.
           DISPLAY "Rentrez un opérateur (+ - / * ^):" 
           SPACE WITH NO ADVANCING ACCEPT WS-OPERATOR. 

      *    Compare l'opérateur choisi pour sauter jusqu'au bon 
      *    paragraphe

           EVALUATE WS-OPERATOR
           WHEN "+"
           PERFORM PG-ACCEPT
           PERFORM PG-ADDITION
           WHEN "-"
           PERFORM PG-ACCEPT
           PERFORM PG-SUBTRACT
           WHEN "*"
           PERFORM PG-ACCEPT
           PERFORM PG-MULTIPLY
           WHEN "/"
           PERFORM PG-ACCEPT
           PERFORM PG-DIVIDE
           WHEN "^"
           PERFORM PG-ACCEPT
           PERFORM PG-EXPO
           WHEN OTHER 
           DISPLAY "Opérateur invalide"
           END-EVALUATE.
       0000-MAIN-END.
           
       PG-ACCEPT.
           DISPLAY "Rentrez un nombre:" SPACE WS-OPERATOR SPACE
           SPACE WITH NO ADVANCING ACCEPT WS-A.

       PG-EXIT.

      *    Demande à l'utilisateur s'il veut sortir de la calculatrice

           DISPLAY"-------------------------------------------"
           DISPLAY "Voulez vous continuer ? (o/n)" SPACE 
           Space with no advancing ACCEPT WS-EXIT
           MOVE FUNCTION UPPER-CASE(WS-EXIT) to WS-EXIT
           DISPLAY"-------------------------------------------" 
           EVALUATE WS-EXIT
           WHEN "O"
           PERFORM 0000-MAIN-START
           WHEN "N"
           STOP RUN
           WHEN OTHER 
           DISPLAY "Veuillez choisir o ou n."
           PERFORM PG-EXIT
           END-EVALUATE.



      *    Paragraphes des diffèrents opérateurs

       PG-ADDITION.

           ADD WS-A to WS-RESULT.
           MOVE WS-A TO WS-A-CLEAN.
           MOVE WS-RESULT TO WS-RESULT-CLEAN.
           MOVE WS-RESULT-TEMP TO WS-RESULT-TEMP-CLEAN.
           DISPLAY"-------------------------------------------"
           DISPLAY FUNCTION TRIM(WS-RESULT-TEMP-CLEAN )SPACE "+" 
           SPACE function TRIM(WS-A-CLEAN) SPACE "="
           SPACE FUNCTION TRIM(WS-RESULT-CLEAN).
           PERFORM PG-EXIT.

       PG-SUBTRACT.

           SUBTRACT WS-A FROM WS-RESULT GIVING WS-RESULT.
           MOVE WS-A TO WS-A-CLEAN.
           MOVE WS-RESULT TO WS-RESULT-CLEAN.
           MOVE WS-RESULT-TEMP TO WS-RESULT-TEMP-CLEAN.
           DISPLAY"-------------------------------------------"
           DISPLAY FUNCTION TRIM(WS-RESULT-TEMP-CLEAN ) SPACE "-" SPACE
           function TRIM(WS-A-CLEAN) SPACE "="
           SPACE FUNCTION TRIM(WS-RESULT-CLEAN).
           PERFORM PG-EXIT.

       PG-MULTIPLY.

           MULTIPLY WS-A BY WS-RESULT GIVING WS-RESULT.
           MOVE WS-A TO WS-A-CLEAN.
           MOVE WS-RESULT TO WS-RESULT-CLEAN.
           MOVE WS-RESULT-TEMP TO WS-RESULT-TEMP-CLEAN.
           DISPLAY"-------------------------------------------"
           DISPLAY FUNCTION TRIM(WS-RESULT-TEMP-CLEAN ) SPACE "*" SPACE
           function TRIM(WS-A-CLEAN) SPACE "="
           SPACE FUNCTION TRIM(WS-RESULT-CLEAN).
           PERFORM PG-EXIT.

       PG-DIVIDE.

           IF WS-A = 0
           DISPLAY "Division par 0 impossible"
           ELSE
           DIVIDE WS-RESULT BY WS-A GIVING WS-RESULT
           MOVE WS-A TO WS-A-CLEAN
           MOVE WS-RESULT TO WS-RESULT-CLEAN
           MOVE WS-RESULT-TEMP TO WS-RESULT-TEMP-CLEAN
           DISPLAY"-------------------------------------------"
           DISPLAY FUNCTION TRIM(WS-RESULT-TEMP-CLEAN ) SPACE "/" SPACE
           function TRIM(WS-A-CLEAN) SPACE "="
           SPACE FUNCTION TRIM(WS-RESULT-CLEAN)
           END-IF
           PERFORM PG-EXIT.
           
       PG-EXPO.

           COMPUTE WS-RESULT = WS-RESULT ** WS-A
           MOVE WS-A TO WS-A-CLEAN.
           MOVE WS-RESULT TO WS-RESULT-CLEAN
           MOVE WS-RESULT-TEMP TO WS-RESULT-TEMP-CLEAN
           DISPLAY"-------------------------------------------"
           DISPLAY FUNCTION TRIM(WS-RESULT-TEMP-CLEAN ) SPACE "/" SPACE
           function TRIM(WS-A-CLEAN) SPACE "="
           SPACE FUNCTION TRIM(WS-RESULT-CLEAN)
           PERFORM PG-EXIT.