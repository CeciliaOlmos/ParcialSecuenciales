      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CECILIA-OLMOS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VENTAS
           ASSIGN TO
           "..\ventas.txt"
           ORGANIZATION is line sequential.
           SELECT ARTICULOS
           ASSIGN TO
           "..\articulos.txt"
           ORGANIZATION is line SEQUENTIAL.
           SELECT COMPONENTES
           ASSIGN TO
           "..\componentes.txt"
           ORGANIZATION is line sequential.
           SELECT ARCH-SORT-VENTAS
           ASSIGN TO "sortwork".
           SELECT LISTADO
           ASSIGN TO PRINTER,"..\impArt.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  VENTAS.
       01  ven-reg.
           03 ven-factura pic 9(8).
           03 ven-art pic x(3).
           03 ven-cant pic 9(3).

       FD  ARTICULOS.
       01  art-reg-cab.
           03 art-cab-tiporeg pic 9.
           03 art-cab-cod pic x(3).
           03 art-cab-nombre pic x(20).
       01  art-reg-det.
           03 art-det-tiporeg pic 9.
           03 art-det-comp pic 99.
           03 art-det-cant pic 99.

       FD  COMPONENTES.
       01  comp-registro.
           03 comp-cod pic 99.
           03 comp-nombre pic x(20).

       SD  ARCH-SORT-VENTAS.
       01  srt-ventas-reg.
           03 srt-ven-factura pic 9(8).
           03 srt-ven-articulo pic x(3).
           03 srt-ven-cant pic 9(3).

       FD  LISTADO
           LINAGE IS 60 LINES
           with FOOTING AT 50
           lines at top 1
           lines at BOTTOM 1.
       01  lis-reg pic x(80).

       WORKING-STORAGE SECTION.
       01  w-flag-sort pic 9.
           88 fin-archivo value 0.
       01  w-flag-art PIC 9.
       01  w-sort-art-ant pic 9(3).
       01  w-sort-cant pic 99.
       01  w-cant-total pic 999.
       01  w-total-comp pic 999.
       01  tabla-comp.
           03 vec-comp pic 999 OCCURS 99 times.
       01  w-i pic 9.
       01  w-flag-comp pic 9 value ZERO.
       01  cabecera0.
           03 filler       pic x(20).
           03 filler       pic x(22) value "LISTADO DE COMPONENTES".
           03 filler       pic x(12)  value spaces.
           03 filler       pic x(13) value "NRO. DE PAG. ".
           03 l-nro-pag    pic 99.
           03 filler       pic x(13).

       01  cabecera1.
           03  lin-titulo-soc.
               05 filler pic x(22) value spaces.
               05 FILLER pic x(6) value "CODIGO".
               05 filler pic x(10) value spaces.
               05 FILLER pic x(10) value "NOMBRE".
               05 filler  pic x(8) value space.
               05 FILLER pic x(8) value "CANTIDAD".
               05 filler pic x(10) value spaces.
       01  detalle1.
           03  lin-det-art.
               05 filler pic x(22) value spaces.
               05 l-cod pic x(6).

               05 l-nom pic x(20).
               05 filler pic x(8) value spaces.
               05 l-cant pic zz9 value spaces.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           SORT ARCH-SORT-VENTAS ASCENDING srt-ven-articulo
           USING VENTAS
           OUTPUT PROCEDURE IS DATOS-SALIDA.
           STOP RUN.

       DATOS-SALIDA.
           PERFORM 10-INICIO-GENERAL.
           PERFORM 100-LEER-SORT.
           PERFORM UNTIL w-flag-sort is =1
               PERFORM 110-INICIO-SORT
               PERFORM until srt-ven-articulo is not = w-sort-art-ant
               or w-flag-sort is =1
               PERFORM 120-PROCESO-SORT
               PERFORM 100-LEER-SORT
               END-PERFORM
               PERFORM 150-FIN-SORT
           END-PERFORM.
           PERFORM 90-FIN-GENERAL.

        10-INICIO-GENERAL.
           OPEN INPUT ARTICULOS.
           OPEN INPUT COMPONENTES.
           OPEN OUTPUT LISTADO.

       100-LEER-SORT.
           RETURN ARCH-SORT-VENTAS AT END MOVE 1 TO w-flag-sort.

       110-INICIO-SORT.
           MOVE srt-ven-articulo TO w-sort-art-ant.
           move zero to w-sort-cant.

       120-PROCESO-SORT.
           add srt-ven-cant to w-sort-cant.

       150-FIN-SORT.
            PERFORM 200-BUSCAR-ARTICULO.

       200-BUSCAR-ARTICULO.
           PERFORM 220-LEER-ARTICULO.
           PERFORM 300-PROCESO-ARTICULO.

       220-LEER-ARTICULO.
           READ ARTICULOS AT END MOVE 1 TO w-flag-art.

       300-PROCESO-ARTICULO.
           IF art-cab-tiporeg=1
              PERFORM 220-LEER-ARTICULO.
               PERFORM UNTIL w-flag-art=1 or art-det-tiporeg IS =1
               or art-cab-cod IS = w-sort-art-ant
                   COMPUTE w-cant-total=w-sort-cant*art-det-cant
                   add w-cant-total to vec-comp(art-det-comp)
                   PERFORM 220-LEER-ARTICULO
               END-PERFORM.

       400-PROCESO-COMPONENTE.
           PERFORM 410-LEER-COMPONENTE.
           PERFORM 420-MOSTRAR-CABECERA.
           PERFORM 430-MUESTRO.

       410-LEER-COMPONENTE.
           READ COMPONENTES AT END MOVE 1 TO w-flag-comp.

       420-MOSTRAR-CABECERA.
           DISPLAY cabecera0.
           DISPLAY cabecera1.

       430-MUESTRO.
           PERFORM UNTIL w-flag-comp=1
               PERFORM 440-ARMO-LINEA
               PERFORM 410-LEER-COMPONENTE
           END-PERFORM.

       440-ARMO-LINEA.
           MOVE comp-cod TO l-cod.
           MOVE comp-nombre TO l-nom.
           MOVE vec-comp(comp-cod) TO l-cant.
           DISPLAY detalle1.
           WRITE lis-reg.

       90-FIN-GENERAL.
           PERFORM 400-PROCESO-COMPONENTE.
           PERFORM 450-CERRAR-ARCHIVO.

       450-CERRAR-ARCHIVO.
           CLOSE ARTICULOS.
           CLOSE COMPONENTES.
           CLOSE LISTADO.
       END PROGRAM CECILIA-OLMOS.
