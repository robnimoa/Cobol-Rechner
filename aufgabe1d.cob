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
       01  zahl1                pic S9(10)V99.
       01  zahl2                pic S9(10)V99.
       01  ergb                 pic S9(10)V99.
       01  modulo               pic 9V9999.
       01  operator             pic x(1).
       01  prozent1             pic x.
       01  prozent2             pic x.
       01  eingabe-zeichen      pic x.
               88 eingabe-menu-beenden value "9".

       PROCEDURE DIVISION.
       beginn section.
        perform until eingabe-menu-beenden
                display "Einfacher Taschenrechenr"
                display "1) Rechnung durchführen"
                display "9) Beenden"
                move zero to eingabe-zeichen
                accept eingabe-zeichen
                evaluate eingabe-zeichen
                   when "1" perform eingabe-zahlen
                   when "9" set eingabe-menu-beenden to true
        end-perform
        stop run.

       datenfelder-loeschen section.
        move zeroes to zahl1
        move zeroes to zahl2
        move zeroes to ergb
        move zeroes to modulo
        move zero to prozent1
        move zero to prozent2
        move zero to operator
        exit.

       eingabe-zahlen section.
        perform datenfelder-loeschen
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
        evaluate operator
                when "+" perform addieren
                when "-" perform subtrahieren
                when "*" perform multiplizieren
                when "x" perform multiplizieren
                when "X" perform multiplizieren
                when ":" perform dividieren
                when "/" perform dividieren
                when "p" perform potenz
                when "P" perform potenz
                when other display "Unbekannter Operator. Bitte erneut"
                        &" versuchen."
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
