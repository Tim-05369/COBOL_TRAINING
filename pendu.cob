       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pendu.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MYS PIC X(10) VALUE "MYSTERIEUX".
       01 AFF PIC X(10) VALUE "**********".
       01 ESS PIC 99 VALUE 10.
       01 L   PIC 99 VALUE 10.
       01 I   PIC 99.
       01 LET PIC X.
       01 TRO PIC X VALUE "N".

       PROCEDURE DIVISION.
       DEBUT.
           PERFORM AFF-MOT
           PERFORM JEU UNTIL ESS = 0 OR AFF = MYS
           EVALUATE TRUE
               WHEN AFF = MYS
                   DISPLAY "Bravo! Vous avez trouvé le mot : " MYS
               WHEN OTHER
                   DISPLAY "Perdu! Le mot était : " MYS
           END-EVALUATE
           STOP RUN.

       AFF-MOT.
           DISPLAY "Mot: " WITH NO ADVANCING
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > L
               DISPLAY AFF(I:1) WITH NO ADVANCING
           END-PERFORM
           DISPLAY " ".

       JEU.
           DISPLAY "Reste ", ESS, " essais."
           DISPLAY "Lettre : " WITH NO ADVANCING
           ACCEPT LET
           PERFORM VER-LET
           PERFORM AFF-MOT
           SUBTRACT 1 FROM ESS.

       VER-LET.
           MOVE "N" TO TRO
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > L
               IF MYS(I:1) = LET
                   MOVE LET TO AFF(I:1)
                   MOVE "Y" TO TRO
               END-IF
           END-PERFORM
           IF TRO = "N"
               DISPLAY "Non, cette lettre n'est pas dans le mot."
           ELSE
               DISPLAY "Bien! Lettre trouvée."
           END-IF.

       END PROGRAM Pendu.
