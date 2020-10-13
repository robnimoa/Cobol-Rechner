      ******************************************************************
      * Author: Robert-Andreas Nietsch
      * Date: 16.09.2020
      * Purpose: Bewerbungstest 12.1_b
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

       PROCEDURE DIVISION.
       beginn section.
           perform eingabe-zahlen.
           stop run.

       eingabe-zahlen section.
           display "Geben Sie die erste Zahl ein:"
           accept zahl1
           display "Geben Sie die zweite Zahl ein:"
           accept zahl2
           perform auswahl-operator
           exit.

       auswahl-operator section.
           display "Bitte geben Sie einen Rechenoperator an: + - * /"
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
           end-if
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

       END PROGRAM calculator.
