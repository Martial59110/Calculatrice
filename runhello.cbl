       IDENTIFICATION DIVISION.
       PROGRAM-ID. runhello.
       AUTHOR. Martial FLoquet.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-A PIC S9(9)V99.
       01  WS-B PIC S9(9)V99.
       01  WS-LOOP PIC 9 VALUE 1.
       01  WS-EXIT PIC A.
       01  WS-OPERATOR PIC X.
       01  WS-RESULT PIC S9(9)V99.
       01  WS-RESULT-TEMP PIC S9(9)V99.
       01  WS-A-CLEAN PIC -Z(7)9.99.
       01  WS-B-CLEAN PIC -Z(7)9.99.
       01  WS-RESULT-CLEAN PIC -Z(7)9.99.
       01  WS-RESULT-TEMP-CLEAN PIC -Z(7)9.99. 
       01  WS-DATEACTU.
           02  ANACTU       PIC 9(4).
           02  MOISACTU     PIC 9(2).
           02  JOURACTU     PIC 9(2).
       01  WS-DATE-DISPLAY.
           02 FULL-DISPLAY  PIC X(15).
       01  WS-FILLER.
           02 FILLER PIC X VALUE "/".

       01  WS-TEMPSACTU.
           02  HEUREACTU     PIC 99.
           02  MINUTEACTU    PIC 99.
           02  SECONDEACTU    PIC 99.
       
       
       PROCEDURE DIVISION.

      *    Déclaration de mon paragraphe MAIN

           PERFORM 0000-MAIN-START
           THRU    0000-MAIN-END.


      *    Début du Main

       0000-MAIN-START. 


      *    Stocke la valeur de WS-RESULT dans le TEMP à chaque nouvelle 
      *    boucle 
      

           SET WS-RESULT-TEMP TO WS-RESULT.

      *    Prend la date et heure
           
           ACCEPT  WS-DATEACTU FROM DATE YYYYMMDD.
           ACCEPT  WS-TempsActu FROM TIME.
           
      *    Format européen

           MOVE JOURACTU TO WS-DATE-DISPLAY(1:2).
           MOVE WS-FILLER TO WS-DATE-DISPLAY(3:1).
           MOVE MOISACTU TO WS-DATE-DISPLAY(4:2).
           MOVE WS-FILLER TO WS-DATE-DISPLAY(6:1).
           MOVE ANACTU TO WS-DATE-DISPLAY(7:4).

      *    Format américain

           MOVE JOURACTU TO WS-DATE-DISPLAY(9:2).
           MOVE WS-FILLER TO WS-DATE-DISPLAY(8:1).
           MOVE MOISACTU TO WS-DATE-DISPLAY(6:2).
           MOVE WS-FILLER TO WS-DATE-DISPLAY(5:1).
           MOVE ANACTU TO WS-DATE-DISPLAY(1:4).

      *    IHM du début
       
           DISPLAY"                                           "
           DISPLAY"                                           "
           DISPLAY"-------------------------------------------"
           DISPLAY "La date est : " WS-DATE-DISPLAY.
           DISPLAY "L'heure est " HeureActu ":" MinuteActu ":" 
           SECONDEACTU .
           DISPLAY "Le résultat actuel est :" WS-RESULT.
           DISPLAY"-------------------------------------------".
           DISPLAY"                                           ".
           DISPLAY"                                           ".
           IF WS-LOOP = 1
           DISPLAY "Rentrez un nombre :" 
           SPACE WITH NO ADVANCING ACCEPT WS-B
           END-IF.
           DISPLAY "Rentrez un opérateur (+ - / * ^):"
           SPACE WITH NO ADVANCING ACCEPT WS-OPERATOR. 
           
           DISPLAY"                                           "
           DISPLAY"                                           "
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
           
           ADD 1 to WS-LOOP.
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
           IF WS-LOOP = 1
           ADD WS-A to WS-B GIVING WS-RESULT
           MOVE WS-B TO WS-B-CLEAN
           ELSE
           ADD WS-A to WS-RESULT
           END-IF
           MOVE WS-A TO WS-A-CLEAN.
           MOVE WS-RESULT TO WS-RESULT-CLEAN.
           MOVE WS-RESULT-TEMP TO WS-RESULT-TEMP-CLEAN.
           DISPLAY"-------------------------------------------"
           IF WS-LOOP = 1
            DISPLAY FUNCTION TRIM(WS-B-CLEAN)SPACE "+" 
           SPACE function TRIM(WS-A-CLEAN) SPACE "="
           SPACE FUNCTION TRIM(WS-RESULT-CLEAN)
           ELSE
           DISPLAY FUNCTION TRIM(WS-RESULT-TEMP-CLEAN )SPACE "+" 
           SPACE function TRIM(WS-A-CLEAN) SPACE "="
           SPACE FUNCTION TRIM(WS-RESULT-CLEAN)
           END-IF
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