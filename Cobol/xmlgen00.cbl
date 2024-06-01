       ID DIVISION.                                                     00010000
      **************                                                    00020000
                                                                        00030000
       PROGRAM-ID.      XMLGEN00.                                       00040004
                                                                        00050000
       AUTHOR.          MAINFRAMETECHVIDEOS.                            00060000
                                                                        00070000
       DATE-WRITTEN.    2023/11/20.                                     00080000
                                                                        00090000
       DATE-COMPILED.   2023/11/20.                                     00100000
                                                                        00110000
       ENVIRONMENT DIVISION.                                            00120000
      **********************                                            00130000
                                                                        00140000
       CONFIGURATION SECTION.                                           00150000
      *=======================                                          00160000
                                                                        00170000
       SPECIAL-NAMES.                                                   00180000
      *--------------                                                   00190000
                                                                        00200000
           C01 IS NEXT-PAGE                                             00210000
           CURRENCY SIGN IS "RS." WITH PICTURE SYMBOL "$".              00220000
                                                                        00230000
       INPUT-OUTPUT SECTION.                                            00240000
      *======================                                           00250000
                                                                        00260000
       FILE-CONTROL.                                                    00261005
      *--------------                                                   00262005
                                                                        00263005
           SELECT INFILE   ASSIGN           TO UT-S-INFILE.             00270000
           SELECT OUTFILE  ASSIGN           TO UT-S-OUTFILE.            00280000
           SELECT ERRFILE  ASSIGN           TO UT-S-ERRFILE.            00281001
                                                                        00290000
       DATA DIVISION.                                                   00300000
      ****************                                                  00310000
                                                                        00320000
       FILE SECTION.                                                    00321006
      *===============                                                  00322006
                                                                        00323006
       FD INFILE                                                        00330005
              LABEL RECORD STANDARD                                     00340000
              BLOCK 0 RECORDS                                           00350000
              RECORDING MODE F.                                         00360000
                                                                        00370000
       01 INFILE-REC.                                                   00380005
              03 IPT-REC-ZN                 PIC X(100).                 00390000
                                                                        00400000
       FD OUTFILE                                                       00410005
              LABEL RECORD STANDARD                                     00420000
              BLOCK 0 RECORDS                                           00430000
              RECORDING MODE F.                                         00440000
                                                                        00450000
       01 OUTFILE-REC.                                                  00460005
              03 OUT-REC-ZN                 PIC X(400).                 00470017
                                                                        00480000
       FD ERRFILE                                                       00481005
              LABEL RECORD STANDARD                                     00482001
              BLOCK 0 RECORDS                                           00483001
              RECORDING MODE F.                                         00484001
                                                                        00485001
       01 ERRFILE-REC.                                                  00486005
              03 ERR-REC-ZN                 PIC X(150).                 00487001
                                                                        00488001
       WORKING-STORAGE SECTION.                                         00490000
      *=========================                                        00500000
                                                                        00510000
       01 WS-CS.                                                        00520000
      *----------                                                       00530000
                                                                        00540000
           03 WC-PGM-NM                     PIC X(08) VALUE 'XMLPARSE'. 00550000
           03 WC-PCS-NM                     PIC X(40) VALUE             00560000
              'XML GENERATION'.                                         00570000
           03 WC-IC-ON                      PIC X(01) VALUE '1'.        00580001
           03 WC-IC-OFF                     PIC X(01) VALUE '0'.        00590001
           03 WC-COMMA                      PIC X(01) VALUE ','.        00590102
           03 WC-ONE                        PIC 9(01) VALUE 1 BINARY.   00590202
           03 WC-EXE-CD-OK                  PIC X(02) VALUE '00'.       00591001
           03 WC-EXE-CD-WRN                 PIC X(02) VALUE '04'.       00592001
           03 WC-EXE-CD-ERR                 PIC X(02) VALUE '08'.       00593001
           03 WC-ERR-INFO                   PIC X(33) VALUE             00594002
              'ERROR OCCURED FOR RECORD EMP ID:'.                       00595005
           03 WC-HDR-INFO                   PIC X(50) VALUE             00600001
              '<?XML VERSION="1.0" ENCODING="ISO-8859-1"?>'.            00610001
                                                                        00620001
       01 WS-WV.                                                        00630001
      *----------                                                       00640001
                                                                        00650001
           03 WV-EXE-CD                     PIC X(02).                  00651001
           03 WV-XML-OPT-DATA               PIC X(500).                 00652016
           03 WV-XML-CODE                   PIC X(10).                  00653018
           03 WV-EMP-DTL.                                               00660001
              05 WV-EMP-DPT                 PIC X(03).                  00670001
              05 WV-EMP-NAME                PIC X(20).                  00680001
              05 WV-EMP-ID                  PIC X(10).                  00690001
              05 WV-EMP-DESG                PIC X(20).                  00700001
              05 WV-EMP-JOIN-YR             PIC X(04).                  00710001
              05 WV-EMP-SALARY              PIC 9(09).                  00720001
              05 WV-EMP-ADDR                PIC X(34).                  00730011
                                                                        00790001
           03 WV-XML-DATA-LN                PIC S9(04) BINARY.          00800005
                                                                        00810001
       01 WS-IC.                                                        00810101
      *----------                                                       00810201
                                                                        00810301
           03 WI-EOF-IC                     PIC X(01).                  00810401
           03 WI-ERR-IC                     PIC X(01).                  00810501
                                                                        00811001
       01 WS-WA.                                                        00812001
      *----------                                                       00812101
                                                                        00813001
           03 WA-READ-CTR                   PIC S9(09) PACKED-DECIMAL.  00813101
           03 WA-WRITE-CTR                  PIC S9(09) PACKED-DECIMAL.  00814001
           03 WA-ERROR-CTR                  PIC S9(09) PACKED-DECIMAL.  00815001
                                                                        00816001
       PROCEDURE DIVISION.                                              00820001
      *********************                                             00830001
                                                                        00840001
           PERFORM A010-STT-XML-GEN.                                    00850001
                                                                        00860001
           PERFORM A020-PCS-XML-GEN UNTIL WI-EOF-IC = HIGH-VALUES.      00870001
                                                                        00880001
           PERFORM A030-END-XML-GEN.                                    00890001
                                                                        00900001
           STOP RUN.                                                    00910001
                                                                        00920001
      *********************** LEVEL A **********************************00930001
                                                                        00940001
       A010-STT-XML-GEN.                                                00950001
      *------------------                                               00960001
                                                                        00970001
           DISPLAY 'INSIDE A010'.                                       00971014
                                                                        00972014
           PERFORM Z010-OPN-FILES.                                      00980001
                                                                        00990001
           PERFORM Z030-INIT-FLDS.                                      01000001
                                                                        01010001
           PERFORM Z040-READ-INFILE.                                    01040001
                                                                        01050001
       A020-PCS-XML-GEN.                                                01060001
      *------------------                                               01070001
                                                                        01080001
           DISPLAY 'INSIDE A020'.                                       01081014
                                                                        01082014
           PERFORM Z050-GEN-XML-STRUCTURE.                              01090001
                                                                        01100001
           IF WV-EXE-CD =  WC-EXE-CD-OK                                 01101001
              PERFORM Z060-CNCT-XML-HDR                                 01102101
              PERFORM Z090-WRITE-OPT-FILE                               01102201
           ELSE                                                         01103001
              PERFORM Z070-FILL-UP-ERR-DTLS                             01104001
              PERFORM Z080-WRITE-ERR-FILE                               01104101
           END-IF.                                                      01105001
                                                                        01120001
           PERFORM Z040-READ-INFILE.                                    01120103
                                                                        01121003
       A030-END-XML-GEN.                                                01130001
      *------------------                                               01140001
                                                                        01150001
           PERFORM Z020-CLS-FILES.                                      01160001
                                                                        01170001
           PERFORM Z100-PCS-CTRS.                                       01180001
                                                                        01190001
      *********************** LEVEL Z **********************************01200001
                                                                        01210001
       Z010-OPN-FILES.                                                  01220001
      *----------------                                                 01230001
                                                                        01240001
           OPEN INPUT   INFILE                                          01250001
                OUTPUT  OUTFILE                                         01260005
                        ERRFILE.                                        01270001
                                                                        01280001
       Z020-CLS-FILES.                                                  01290001
      *----------------                                                 01300001
                                                                        01310001
           CLOSE  INFILE                                                01320001
                  OUTFILE                                               01330005
                  ERRFILE.                                              01340001
                                                                        01350001
       Z030-INIT-FLDS.                                                  01360001
      *-----------------                                                01370001
                                                                        01380001
           INITIALIZE WS-WV                                             01390001
                      WS-WA.                                            01391001
                                                                        01400001
           MOVE LOW-VALUES                  TO WI-ERR-IC                01410001
                                               WI-EOF-IC.               01420001
                                                                        01430001
           MOVE WC-EXE-CD-OK                TO WV-EXE-CD.               01431002
                                                                        01432002
       Z040-READ-INFILE.                                                01440001
      *-------------------                                              01450001
                                                                        01460001
           READ INFILE                                                  01470001
                AT END                                                  01480001
                   MOVE HIGH-VALUES         TO WI-EOF-IC                01490001
                   DISPLAY 'END OF RECORDS'                             01491015
           END-READ.                                                    01500001
                                                                        01510001
           IF WI-EOF-IC = LOW-VALUES                                    01520001
                                                                        01521011
           DISPLAY 'INSIDE READ INPUT'                                  01522013
                                                                        01523011
              ADD WC-ONE                    TO WA-READ-CTR              01530002
              MOVE IPT-REC-ZN               TO WV-EMP-DTL               01531001
           END-IF.                                                      01540001
                                                                        01550001
       Z050-GEN-XML-STRUCTURE.                                          01560001
      *-----------------------                                          01570001
                                                                        01580001
           DISPLAY 'INSIDE GENERATE'.                                   01581011
                                                                        01582011
           XML GENERATE WV-XML-OPT-DATA     FROM WV-EMP-DTL             01590001
               COUNT IN WV-XML-DATA-LN                                  01600007
               NAME  OF WV-EMP-DTL          IS "Employee"               01610001
                        WV-EMP-DPT          IS "Department"             01620001
                        WV-EMP-NAME         IS "Name"                   01630001
                        WV-EMP-ID           IS "ID"                     01640001
                        WV-EMP-DESG         IS "Designation"            01650001
                        WV-EMP-JOIN-YR      IS "JoinYear"               01660001
                        WV-EMP-SALARY       IS "Salary"                 01670001
                        WV-EMP-ADDR         IS "Address"                01680001
               SUPPRESS                                                 01740001
                        EVERY NONNUMERIC ELEMENT WHEN SPACE             01750001
                        EVERY NUMERIC    ELEMENT WHEN ZERO              01760002
               ON EXCEPTION                                             01761002
                        DISPLAY 'ERROR IN XML GENERATE: ' XML-CODE      01762002
                        MOVE HIGH-VALUES    TO WI-ERR-IC                01763002
                        MOVE WC-EXE-CD-ERR  TO WV-EXE-CD                01764002
                        MOVE XML-CODE       TO WV-XML-CODE              01764106
               NOT ON EXCEPTION                                         01765002
                        MOVE WC-EXE-CD-OK   TO WV-EXE-CD                01766002
           END-XML.                                                     01770001
                                                                        01780002
       Z060-CNCT-XML-HDR.                                               01790005
      *-------------------                                              01800005
                                                                        01810002
           IF WV-XML-DATA-LN > ZEROS                                    01820002
              STRING WC-HDR-INFO            DELIMITED BY SIZE           01830002
                     WV-XML-OPT-DATA        DELIMITED BY SIZE           01840002
                     INTO OUT-REC-ZN                                    01850010
              END-STRING                                                01860002
           END-IF.                                                      01870002
                                                                        01880002
       Z070-FILL-UP-ERR-DTLS.                                           01890002
      *-----------------------                                          01900002
                                                                        01910002
           STRING    WC-ERR-INFO            DELIMITED BY SIZE           01920002
                     WV-EMP-ID              DELIMITED BY SPACE          01930002
                     WC-COMMA               DELIMITED BY SIZE           01931002
                     WV-EMP-NAME            DELIMITED BY SIZE           01940002
                     WV-XML-CODE            DELIMITED BY SIZE           01950006
                     INTO ERR-REC-ZN                                    01960002
           END-STRING.                                                  01970002
                                                                        01980002
       Z080-WRITE-ERR-FILE.                                             01990002
      *---------------------                                            02000002
                                                                        02010002
           WRITE  ERRFILE-REC.                                          02020009
                                                                        02030002
           ADD WC-ONE                       TO WA-ERROR-CTR.            02040002
                                                                        02050002
       Z090-WRITE-OPT-FILE.                                             02060002
      *---------------------                                            02070002
                                                                        02080002
           WRITE  OUTFILE-REC.                                          02090009
                                                                        02100002
           ADD WC-ONE                       TO WA-WRITE-CTR.            02110002
                                                                        02120002
       Z100-PCS-CTRS.                                                   02130002
      *---------------                                                  02140002
                                                                        02150002
           DISPLAY ' TOTAL NUMBER OF RECORDS READ:' WA-READ-CTR.        02160002
                                                                        02170002
           DISPLAY ' TOTAL NUMBER OF OUTPUT RECORD:'WA-WRITE-CTR.       02180002
                                                                        02190002
           DISPLAY ' TOTAL NUMBER OF ERROR RECORD:'WA-ERROR-CTR.        02200002
                                                                        02210002
