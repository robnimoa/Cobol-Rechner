      ******************************************************************
      * Author: Robert-Andreas Nietsch
      * Date: 16.09.2020
      * Purpose: Bewerbungstest 12.1d
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. calculator.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT is comma.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 zahl1 pic S9(10)V99.
       01 zahl2 pic S9(10)V99.
       01 ergb pic S9(10)V99.
       01 modulo pic 9V9999.
       01 operator pic x(1).
       01 prozent1 pic x.
       01 prozent2 pic x.

       PROCEDURE DIVISION.
       beginn section.
           perform eingabe-zahlen.
           stop run.

       eingabe-zahlen section.
           display "Geben Sie die erste Zahl ein:"
           perform until zahl1 is not zero 
                   display "Geben Sie eine gültige Zahl ein."
                   display "0 ist ungültig."
                   accept zahl1
           end-perform
           display "Handelt es sich um eine Prozentzahl? (j)a oder nein"
           &"(any key)"
           accept prozent1
           if prozent1 = "j" or "J"
                   divide zahl1 by 100 giving zahl1
           end-if
           display "Geben Sie die zweite Zahl ein:"
           perform until zahl2 is not zero 
                   display "Geben Sie eine gültige Zahl ein."
                   display "0 ist ungültig."
                   accept zahl2
           end-perform
           display "Handelt es sich um eine Prozentzahl? (j)a oder nein"
           &"(any key)"
           accept prozent2
           if prozent2 = "j" or "J"
                   divide zahl2 by 100 giving zahl2
           end-if
           perform auswahl-operator
           exit.

       auswahl-operator section.
           display "Bitte geben Sie einen Rechenoperator an: + - * /"
           display "Oder berechnen Sie die Potenz mit: p"
           accept operator
           if operator = "+"
               perform addieren
               else
           if operator = "-"
               perform subtrahieren
               else
           if operator = "*" or "x" or "X"
               perform multiplizieren
               else
           if operator = "/" or ":"
               perform dividieren
               else
           if operator = "p" or "P"
               perform potenz
           else
              display "Falsche Eingabe. Bitte versuchen Sie es erneut."
              perform auswahl-operator
           exit.

       addieren section.
           add zahl1 zahl2 giving ergb
           on size error display "Achtung Überlauf"
           not on size error display ergb
           exit.

       subtrahieren section.
           subtract zahl2 from zahl1 giving ergb
           on size error display "Achtung Überlauf"
           not on size error display ergb
           exit.

       multiplizieren section.
           multiply zahl1 by zahl2 giving ergb
           on size error display "Achtung Überlauf"
           not on size error display ergb
           exit.

       dividieren section.
           divide zahl1 by zahl2 giving ergb remainder modulo
           on size error display "Achtung Überlauf"
           not on size error display ergb " Rest: " modulo
           exit.

       potenz section.
           compute ergb = zahl1 ** zahl2
           on size error display "Achtung Überlauf"
           not on size error display ergb
           exit.


       END PROGRAM calculator.
