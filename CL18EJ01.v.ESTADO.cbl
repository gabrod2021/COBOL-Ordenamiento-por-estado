      ******************************************************************
      * Author: GABRIELA RODRIGUEZ
      * Date: 25/09/2023
      * Purpose: 
      * DESCRIPCION: ORDENAR ARCHIVOS DE EMPLEADOS POR ESTADO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL18EJ02.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT ENT-EMPLEADOS
           ASSIGN TO '../EMPLEADOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-EMPLEADOS.


       SELECT SAL-SALIDA
           ASSIGN TO '../EMPLEADOSxESTADO.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SALIDA.

      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ENT-EMPLEADOS.
       01 REG-ENT-EMPLEADOS.
          05 ENT-EMP-ID-EMPLEADO         PIC 9(08).
          05 ENT-EMP-NOMBRE              PIC X(25).
          05 ENT-EMP-APELLIDO            PIC X(25).
          05 ENT-EMP-ESTADO              PIC X(01).

       FD SAL-SALIDA.
       01 REG-SALIDA                        PIC X(59).


       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-EMPLEADOS                   PIC X(2).
             88 FS-EMPLEADOS-OK                 VALUE '00'.
             88 FS-EMPLEADOS-EOF                VALUE '10'.
             88 FS-EMPLEADOS-NFD                VALUE '35'.
          05 FS-SALIDA                      PIC X(2).
             88 FS-SALIDA-OK                    VALUE '00'.
             88 FS-SALIDA-EOF                   VALUE '10'.
             88 FS-SALIDA-NFD                   VALUE '35'.

       01 WS-CONTADORES.
           05 WS-CONT-REG-EMPLEADOS          PIC 9(04) VALUE 0.
           05 WS-CONT-REG-SALIDA             PIC 9(06) VALUE 0.

       01 WS-VARIABLES-GENERALES.
           05 WS-IMP-ACUM                    PIC 9(10)V9(02) VALUE 0.
           05 WS-FORMAT-IMPORTE             PIC ZZZ.ZZ9.
           05 WS-I                           PIC 9(2) VALUE 0.
           05 WS-J                           PIC 9(2) VALUE 0.
           05 WS-VAR-AUXILIAR                PIC 9.
           05 WS-VALIDAR-ORDEN               PIC X(2).
               88 WS-ORDENADO-SI                      VALUE 'SI'.
               88 WS-ORDENADO-NO                      VALUE 'NO'.
           05 WS-LISTA.
               10 WS-ITEM OCCURS 10 TIMES.
                  15 WS-ITEM-VALOR PIC 9.
           05 WS-II                          PIC 9(4) VALUE 0.
           05 WS-JJ                          PIC 9(4) VALUE 0.
           05 WS-VAR-AUX2                    PIC X(59).
           05 WS-LISTA-EMP.
               10 WS-ITEM OCCURS 1217 TIMES.
                  15 WS-ITEM-EMP.
                     20 WS-ITEM-RESTO        PIC X(58).
                     20 WS-ITEM-ESTADO       PIC X(01).


      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR
              THRU 1000-INICIAR-EXIT.
      *---- PROCESAMOS UN VECTOR INTERNO PARA ENTENDER EL ALGORITMO.

           PERFORM 2000-PROCESAR-LISTA
              THRU 2000-PROCESAR-LISTA-EXIT.
      *---- AHORA VAMOS A ORDENAR EL ARCHIVO DE ENTRADA.
           PERFORM 2200-PROCESAR-ARCHIVO
              THRU 2200-PROCESAR-ARCHIVO-EXIT.

           PERFORM 3000-FINALIZAR
              THRU 3000-FINALIZAR-EXIT.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR.

           INITIALIZE WS-CONTADORES.

           PERFORM 1100-ABRIR-EMPLEADOS
              THRU 1100-ABRIR-EMPLEADOS-EXIT.

           PERFORM 1200-ABRIR-SALIDA
              THRU 1200-ABRIR-SALIDA-EXIT.

       1000-INICIAR-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-EMPLEADOS.

           OPEN INPUT ENT-EMPLEADOS.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-OK
                    PERFORM 1110-LEER-EMPLEADOS
                       THRU 1110-LEER-EMPLEADOS-EXIT
               WHEN FS-EMPLEADOS-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.

       1100-ABRIR-EMPLEADOS-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1110-LEER-EMPLEADOS.

           READ ENT-EMPLEADOS.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-OK
                    ADD 1                   TO WS-CONT-REG-EMPLEADOS
               WHEN FS-EMPLEADOS-EOF
                    MOVE 99999999           TO ENT-EMP-ID-EMPLEADO
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.

       1110-LEER-EMPLEADOS-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1200-ABRIR-SALIDA.

           OPEN OUTPUT  SAL-SALIDA.

           EVALUATE TRUE
               WHEN FS-SALIDA-OK
                    CONTINUE
               WHEN FS-SALIDA-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
           END-EVALUATE.

       1200-ABRIR-SALIDA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-LISTA.

      *---- COMPLETO DATOS DE WS-LISTA
           MOVE 5 TO WS-ITEM-VALOR(1).
           MOVE 3 TO WS-ITEM-VALOR(2).
           MOVE 9 TO WS-ITEM-VALOR(3).
           MOVE 4 TO WS-ITEM-VALOR(4).
           MOVE 8 TO WS-ITEM-VALOR(5).
           MOVE 0 TO WS-ITEM-VALOR(6).
           MOVE 1 TO WS-ITEM-VALOR(7).
           MOVE 2 TO WS-ITEM-VALOR(8).
           MOVE 7 TO WS-ITEM-VALOR(9).
           MOVE 6 TO WS-ITEM-VALOR(10).

           DISPLAY 'LISTA ORIGINAL: ' WS-LISTA.

           PERFORM 2100-ORDENAR-LISTA
              THRU 2100-ORDENAR-LISTA-EXIT.

           DISPLAY 'LISTA ORDENADA: ' WS-LISTA.

       2000-PROCESAR-LISTA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-ORDENAR-LISTA.

      *---- SETEAR EN NO ORDENADO PARA QUE ENTRE EN EL BUCLE
           MOVE 'NO'            TO WS-VALIDAR-ORDEN.

      *----VOY A REPETIR HASTA QUE ESTÈ ORDENADA LA LISTA
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10 OR
                                  WS-ORDENADO-SI
      *----PARA UNA PASADA ASUMO QUE ESTA ORDENADA
              MOVE 'SI'                        TO WS-VALIDAR-ORDEN

              PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > (10 - WS-I)

      *----SI EN UNA PASADA COMPLETA NO ENTRA EN EL IF, ESTA ORDENADA
                  IF WS-ITEM-VALOR(WS-J) >  WS-ITEM-VALOR(WS-J + 1)
      *---- AL DETECTAR UN DESORDEN SETEO EN NO, PORQUE SEGURO TENGO
      *---- QUE HACER OTRA PASADA.
                      MOVE 'NO'                TO WS-VALIDAR-ORDEN
                      MOVE WS-ITEM-VALOR(WS-J) TO WS-VAR-AUXILIAR
                      MOVE WS-ITEM-VALOR(WS-J + 1)
                                            TO WS-ITEM-VALOR(WS-J)
                      MOVE WS-VAR-AUXILIAR     TO
                                              WS-ITEM-VALOR(WS-J + 1)
                  END-IF

              END-PERFORM
           END-PERFORM.

       2100-ORDENAR-LISTA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-PROCESAR-ARCHIVO.

      *---- LEO TODO EL ARCHIVO Y LO GUARDO EN UN OCCURS
           PERFORM VARYING WS-II FROM 1 BY 1 UNTIL FS-EMPLEADOS-EOF

              MOVE REG-ENT-EMPLEADOS TO WS-ITEM-EMP (WS-II)

              PERFORM 1110-LEER-EMPLEADOS
                 THRU 1110-LEER-EMPLEADOS-EXIT

           END-PERFORM.

      *----REALIZO EL PROCEDIMIENTO DE ORDENAMIENTO DE LA LISTA EMPLEAD
      *----SETEAR EN NO ORDENADO PARA QUE ENTRE EN EL BUCLE
           MOVE 'NO'            TO WS-VALIDAR-ORDEN.

      *----VOY A REPETIR HASTA QUE ESTÈ ORDENADA LA LISTA EMPLEADOS
      *----USAR VARIABLES WS-II PARA INDICE Y WS-VAR-AUX2 COMO AUXILIAR

      *----VOY A REPETIR HASTA QUE ESTÈ ORDENADA LA LISTA
           PERFORM VARYING WS-II FROM 1 BY 1 UNTIL WS-II >
                           WS-CONT-REG-EMPLEADOS OR WS-ORDENADO-SI
      *----PARA UNA PASADA ASUMO QUE ESTA ORDENADA
              MOVE 'SI'                        TO WS-VALIDAR-ORDEN

              PERFORM VARYING WS-JJ FROM 1 BY 1 UNTIL WS-JJ >
                                     (WS-CONT-REG-EMPLEADOS- WS-II)
      *----SI EN UNA PASADA COMPLETA NO ENTRA EN EL IF, ESTA ORDENADA
                  IF WS-ITEM-ESTADO(WS-JJ) >  WS-ITEM-ESTADO(WS-JJ + 1)
      *---- AL DETECTAR UN DESORDEN SETEO EN NO, PORQUE SEGURO TENGO
      *---- QUE HACER OTRA PASADA.
                      MOVE 'NO'              TO WS-VALIDAR-ORDEN
                      MOVE WS-ITEM-EMP(WS-JJ) TO WS-VAR-AUX2
                      MOVE WS-ITEM-EMP(WS-JJ + 1)
                                            TO WS-ITEM-EMP(WS-JJ)
                      MOVE WS-VAR-AUX2     TO
                                              WS-ITEM-EMP(WS-JJ + 1)
                  END-IF

              END-PERFORM
           END-PERFORM.


      *---- EL SIGUIENTE PARRAFO LEE TODO EL OCCURS DE EMPLEADOS Y LO
      *---- GRABA EN EL ARCHIVO DE SALIDA.
           PERFORM 2300-MOVER-SALIDA
              THRU 2300-MOVER-SALIDA-EXIT.

       2200-PROCESAR-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2300-MOVER-SALIDA.

           PERFORM VARYING WS-II FROM 1 BY 1 UNTIL WS-II >
                                                 WS-CONT-REG-EMPLEADOS

              MOVE WS-ITEM-EMP (WS-II) TO REG-SALIDA

              PERFORM 2400-GRABAR-SALIDA
                 THRU 2400-GRABAR-SALIDA-EXIT

           END-PERFORM.
       2300-MOVER-SALIDA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2400-GRABAR-SALIDA.

           WRITE REG-SALIDA.

           EVALUATE TRUE
               WHEN FS-SALIDA-OK
                    ADD 1 TO WS-CONT-REG-SALIDA
               WHEN FS-SALIDA-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
           END-EVALUATE.

       2400-GRABAR-SALIDA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR.

           MOVE WS-CONT-REG-EMPLEADOS       TO WS-FORMAT-IMPORTE.
           DISPLAY 'CANTIDAD DE REGISTROS EMPLEADOS   : '
                   WS-FORMAT-IMPORTE.

           MOVE WS-CONT-REG-SALIDA          TO WS-FORMAT-IMPORTE.
           DISPLAY 'CANTIDAD DE REGISTROS SALIDA      : '
                   WS-FORMAT-IMPORTE.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

       3000-FINALIZAR-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE ENT-EMPLEADOS
                 SAL-SALIDA.

           IF NOT FS-EMPLEADOS-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO EMPLEADOS: ' FS-EMPLEADOS
           END-IF.

           IF NOT FS-SALIDA-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO SALIDA: ' FS-SALIDA
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL18EJ02.
