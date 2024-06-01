       ID DIVISION.                                                     00010000
      **************                                                    00020000
                                                                        00030000
       PROGRAM-ID.      XMLGEN01.                                       00040000
                                                                        00050000
       AUTHOR.          MAINFRAMETECHVIDEOS.                            00060000
                                                                        00070000
       DATE-WRITTEN.    2023/11/23.                                     00080000
                                                                        00090000
       DATE-COMPILED.   2023/11/23.                                     00100000
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
       FILE-CONTROL.                                                    00270000
      *--------------                                                   00280000
                                                                        00290000
           SELECT INFILE   ASSIGN           TO UT-S-INFILE.             00300000
           SELECT OUTFILE  ASSIGN           TO UT-S-OUTFILE.            00310000
           SELECT ERRFILE  ASSIGN           TO UT-S-ERRFILE.            00320000
                                                                        00330000
       DATA DIVISION.                                                   00340000
      ****************                                                  00350000
                                                                        00360000
       FILE SECTION.                                                    00370000
      *===============                                                  00380000
                                                                        00390000
       FD INFILE                                                        00400000
              LABEL RECORD STANDARD                                     00410000
              BLOCK 0 RECORDS                                           00420000
              RECORDING MODE F.                                         00430000
                                                                        00440000
       01 INFILE-REC.                                                   00450000
              03 FTN-CD                     PIC X(02).                  00451000
              03 IPT-REC-ZN                 PIC X(198).                 00460000
                                                                        00470000
       FD OUTFILE                                                       00480000
              LABEL RECORD STANDARD                                     00490000
              BLOCK 0 RECORDS                                           00500000
              RECORDING MODE F.                                         00510000
                                                                        00520000
       01 OUTFILE-REC.                                                  00530000
              03 OUT-REC-ZN                 PIC X(900).                 00540008
                                                                        00550000
       FD ERRFILE                                                       00560000
              LABEL RECORD STANDARD                                     00570000
              BLOCK 0 RECORDS                                           00580000
              RECORDING MODE F.                                         00590000
                                                                        00600000
       01 ERRFILE-REC.                                                  00610000
              03 ERR-REC-ZN                 PIC X(200).                 00620000
                                                                        00630000
       WORKING-STORAGE SECTION.                                         00640000
      *=========================                                        00650000
                                                                        00660000
       01 WS-CS.                                                        00670000
      *----------                                                       00680000
                                                                        00690000
           03 WC-PGM-NM                     PIC X(08) VALUE 'XMLPARSE'. 00700000
           03 WC-PCS-NM                     PIC X(40) VALUE             00710000
              'XML GENERATION'.                                         00720000
           03 WC-IC-ON                      PIC X(01) VALUE '1'.        00730000
           03 WC-IC-OFF                     PIC X(01) VALUE '0'.        00740000
           03 WC-FTN-01                     PIC X(02) VALUE '01'.       00741001
           03 WC-FTN-02                     PIC X(02) VALUE '02'.       00742001
           03 WC-COMMA                      PIC X(01) VALUE ','.        00750000
           03 WC-ONE                        PIC 9(01) VALUE 1 BINARY.   00760000
           03 WC-EXE-CD-OK                  PIC X(02) VALUE '00'.       00770000
           03 WC-EXE-CD-WRN                 PIC X(02) VALUE '04'.       00780000
           03 WC-EXE-CD-ERR                 PIC X(02) VALUE '08'.       00790000
           03 WC-ERR-INFO                   PIC X(33) VALUE             00800000
              'ERROR OCCURED FOR RECORD EMP ID:'.                       00810000
           03 WC-HDR-INFO                   PIC X(50) VALUE             00820000
              '<?XML VERSION="1.0" ENCODING="ISO-8859-1"?>'.            00830000
                                                                        00840000
       01 WS-WV.                                                        00850000
      *----------                                                       00860000
                                                                        00870000
           03 WV-EXE-CD                     PIC X(02).                  00880000
           03 WV-XML-OPT-DATA               PIC X(900).                 00890009
           03 WV-XML-CODE                   PIC X(10).                  00900007
           03 WV-EMP-ID                     PIC X(10).                  00901001
           03 WV-EMP-NAME                   PIC X(20).                  00902001
           03 WV-EMP-DTL.                                               00910000
              05 WV-01-EMP-REC.                                         00910200
                 07 WV-01-EMP-DPT           PIC X(03).                  00911000
                 07 WV-01-EMP-NAME          PIC X(20).                  00912000
                 07 WV-01-EMP-ID            PIC X(10).                  00913000
                 07 WV-01-EMP-DESG          PIC X(20).                  00914001
                 07 WV-01-EMP-JOIN-YR       PIC X(04).                  00915000
                 07 WV-01-EMP-SALARY        PIC 9(09).                  00916000
                 07 WV-01-EMP-ADDR.                                     00916100
                    09 WV-01-EMP-STREET     PIC X(10).                  00917000
                    09 WV-01-EMP-ZIP        PIC X(06).                  00917100
                    09 WV-01-EMP-CITY       PIC X(10).                  00917200
                    09 WV-01-EMP-STATE      PIC X(10).                  00917300
                 07 WV-01-EMP-SKILL         PIC X(10).                  00917400
              05 WV-02-EMP-REC.                                         00918000
                 07 WV-02-EMP-DPT           PIC X(03).                  00920000
                 07 WV-02-EMP-NAME          PIC X(20).                  00930000
                 07 WV-02-EMP-ID            PIC X(10).                  00940000
                 07 WV-02-EMP-DESG          PIC X(20).                  00950000
                 07 WV-02-EMP-JOIN-YR       PIC X(04).                  00960000
                 07 WV-02-EMP-SALARY        PIC 9(09).                  00970000
                 07 WV-02-EMP-ADDR.                                     00980000
                    09 WV-02-EMP-STREET     PIC X(10).                  00981000
                    09 WV-02-EMP-ZIP        PIC X(06).                  00982000
                    09 WV-02-EMP-CITY       PIC X(10).                  00983000
                    09 WV-02-EMP-STATE      PIC X(10).                  00984000
                 07 WV-02-EMP-SKILLS OCCURS 5 TIMES.                    00985000
                    09 WV-02-EMP-SKILL      PIC X(10).                  00986000
                 07 WV-02-EMP-TYPE          PIC X(10).                  00987000
                 07 WV-02-EMP-REL-EXP       PIC X(02).                  00988000
                 07 WV-02-EMP-TOT-EXP       PIC X(02).                  00989000
                 07 WV-02-EMP-CMP-LCN       PIC X(10).                  00989100
                 07 WV-02-EMP-PRJ-NM        PIC X(10).                  00989200
                                                                        00990000
           03 WV-XML-DATA-LN                PIC S9(04) BINARY.          01000000
                                                                        01010000
       01 WS-IC.                                                        01020000
      *----------                                                       01030000
                                                                        01040000
           03 WI-EOF-IC                     PIC X(01).                  01050000
           03 WI-ERR-IC                     PIC X(01).                  01060000
                                                                        01070000
       01 WS-WA.                                                        01080000
      *----------                                                       01090000
                                                                        01100000
           03 WA-READ-CTR                   PIC S9(09) PACKED-DECIMAL.  01110000
           03 WA-WRITE-CTR                  PIC S9(09) PACKED-DECIMAL.  01120000
           03 WA-ERROR-CTR                  PIC S9(09) PACKED-DECIMAL.  01130000
                                                                        01140000
       PROCEDURE DIVISION.                                              01150000
      *********************                                             01160000
                                                                        01170000
           PERFORM A010-STT-XML-GEN.                                    01180000
                                                                        01190000
           PERFORM A020-PCS-XML-GEN UNTIL WI-EOF-IC = HIGH-VALUES.      01200000
                                                                        01210000
           PERFORM A030-END-XML-GEN.                                    01220000
                                                                        01230000
           STOP RUN.                                                    01240000
                                                                        01250000
      *********************** LEVEL A **********************************01260000
                                                                        01270000
       A010-STT-XML-GEN.                                                01280000
      *------------------                                               01290000
                                                                        01300000
           PERFORM Z010-OPN-FILES.                                      01330000
                                                                        01340000
           PERFORM Z030-INIT-FLDS.                                      01350000
                                                                        01360000
           PERFORM Z040-READ-INFILE.                                    01370000
                                                                        01380000
       A020-PCS-XML-GEN.                                                01390000
      *------------------                                               01400000
                                                                        01430000
           PERFORM B010-EXC-FTN-CD.                                     01440000
                                                                        01450000
           IF WV-EXE-CD =  WC-EXE-CD-OK                                 01460000
              PERFORM Z070-CNCT-XML-HDR                                 01470000
              PERFORM Z100-WRITE-OPT-FILE                               01480000
           ELSE                                                         01490000
              PERFORM Z080-FILL-UP-ERR-DTLS                             01500000
              PERFORM Z090-WRITE-ERR-FILE                               01510000
           END-IF.                                                      01520000
                                                                        01530000
           PERFORM Z040-READ-INFILE.                                    01540000
                                                                        01550000
       A030-END-XML-GEN.                                                01560000
      *------------------                                               01570000
                                                                        01580000
           PERFORM Z020-CLS-FILES.                                      01590000
                                                                        01600000
           PERFORM Z200-PCS-CTRS.                                       01610000
                                                                        01620000
      *********************** LEVEL B **********************************01630000
                                                                        01630100
       B010-EXC-FTN-CD.                                                 01630201
      *----------------                                                 01630300
                                                                        01630400
           EVALUATE FTN-CD                                              01630500
               WHEN WC-FTN-01                                           01630600
                    PERFORM Z050-GEN-XML-STR-WOT-ATTRIB                 01630700
               WHEN WC-FTN-02                                           01630800
                    PERFORM Z060-GEN-XML-STR-WITH-ATTRIB                01630900
               WHEN OTHER                                               01631000
                    CONTINUE                                            01631100
           END-EVALUATE.                                                01631200
                                                                        01631300
      *********************** LEVEL Z **********************************01632000
                                                                        01640000
       Z010-OPN-FILES.                                                  01650000
      *----------------                                                 01660000
                                                                        01670000
           OPEN INPUT   INFILE                                          01680000
                OUTPUT  OUTFILE                                         01690000
                        ERRFILE.                                        01700000
                                                                        01710000
       Z020-CLS-FILES.                                                  01720000
      *----------------                                                 01730000
                                                                        01740000
           CLOSE  INFILE                                                01750000
                  OUTFILE                                               01760000
                  ERRFILE.                                              01770000
                                                                        01780000
       Z030-INIT-FLDS.                                                  01790000
      *-----------------                                                01800000
                                                                        01810000
           INITIALIZE WS-WV                                             01820000
                      WS-WA.                                            01830000
                                                                        01840000
           MOVE LOW-VALUES                  TO WI-ERR-IC                01850000
                                               WI-EOF-IC.               01860000
                                                                        01870000
           MOVE WC-EXE-CD-OK                TO WV-EXE-CD.               01880000
                                                                        01890000
       Z040-READ-INFILE.                                                01900000
      *-------------------                                              01910000
                                                                        01920000
           READ INFILE                                                  01930000
                AT END                                                  01940000
                   MOVE HIGH-VALUES         TO WI-EOF-IC                01950000
           END-READ.                                                    01970000
                                                                        01980000
           IF WI-EOF-IC = LOW-VALUES                                    01990000
              ADD WC-ONE                    TO WA-READ-CTR              02030000
           END-IF.                                                      02050000
                                                                        02060000
       Z050-GEN-XML-STR-WOT-ATTRIB.                                     02070000
      *----------------------------                                     02080000
                                                                        02110000
           MOVE SPACES                      TO WV-XML-OPT-DATA.         02110113
           MOVE IPT-REC-ZN                  TO WV-01-EMP-REC.           02111006
                                                                        02112006
           XML GENERATE WV-XML-OPT-DATA     FROM WV-01-EMP-REC          02120000
               COUNT IN WV-XML-DATA-LN                                  02130000
               NAME  OF WV-01-EMP-REC       IS "Employee"               02140000
                        WV-01-EMP-DPT       IS "Department"             02150000
                        WV-01-EMP-NAME      IS "Name"                   02160000
                        WV-01-EMP-ID        IS "Id"                     02170000
                        WV-01-EMP-DESG      IS "Designation"            02180000
                        WV-01-EMP-JOIN-YR   IS "JoinYear"               02190000
                        WV-01-EMP-SALARY    IS "Salary"                 02200000
                        WV-01-EMP-ADDR      IS "Address"                02210000
                        WV-01-EMP-STREET    IS "Street"                 02211000
                        WV-01-EMP-ZIP       IS "Zipcode"                02212000
                        WV-01-EMP-CITY      IS "City"                   02213000
                        WV-01-EMP-STATE     IS "State"                  02214000
                        WV-01-EMP-SKILL     IS "Skill"                  02215000
               SUPPRESS                                                 02220000
                        EVERY NONNUMERIC ELEMENT WHEN SPACE             02230000
                        EVERY NUMERIC    ELEMENT WHEN ZERO              02240000
               ON EXCEPTION                                             02250000
                        DISPLAY 'ERROR IN XML GENERATE: ' XML-CODE      02260000
                        MOVE HIGH-VALUES    TO WI-ERR-IC                02270000
                        MOVE WC-EXE-CD-ERR  TO WV-EXE-CD                02280000
                        MOVE XML-CODE       TO WV-XML-CODE              02290000
                        MOVE WV-01-EMP-ID   TO WV-EMP-ID                02291001
                        MOVE WV-01-EMP-NAME TO WV-EMP-NAME              02292002
               NOT ON EXCEPTION                                         02300000
                        MOVE WC-EXE-CD-OK   TO WV-EXE-CD                02310000
           END-XML.                                                     02320000
                                                                        02330000
       Z060-GEN-XML-STR-WITH-ATTRIB.                                    02331000
      *------------------------------                                   02331101
                                                                        02332000
           MOVE SPACES                      TO WV-XML-OPT-DATA.         02332113
           MOVE IPT-REC-ZN                  TO WV-02-EMP-REC.           02332206
                                                                        02332306
           XML GENERATE WV-XML-OPT-DATA     FROM WV-02-EMP-REC          02333000
               WITH     ATTRIBUTES                                      02333100
               NAME OF  WV-02-EMP-REC       IS "Employee"               02333205
                        WV-02-EMP-DPT       IS "Department"             02333305
                        WV-02-EMP-NAME      IS "Name"                   02333405
                        WV-02-EMP-ID        IS "Id"                     02333505
                        WV-02-EMP-DESG      IS "Designation"            02333605
                        WV-02-EMP-JOIN-YR   IS "JoinYear"               02333705
                        WV-02-EMP-SALARY    IS "Salary"                 02333805
                        WV-02-EMP-ADDR      IS "Address"                02333905
                        WV-02-EMP-STREET    IS "Street"                 02334005
                        WV-02-EMP-ZIP       IS "Zipcode"                02334105
                        WV-02-EMP-CITY      IS "City"                   02334205
                        WV-02-EMP-STATE     IS "State"                  02334305
                        WV-02-EMP-SKILLS    IS "Skill"                  02334412
                        WV-02-EMP-SKILL     IS "Name"                   02334912
                        WV-02-EMP-TYPE      IS "Type"                   02335011
                        WV-02-EMP-REL-EXP   IS "RelevantExp"            02335111
                        WV-02-EMP-TOT-EXP   IS "TotalExp"               02335211
                        WV-02-EMP-CMP-LCN   IS "CompanyLocation"        02335311
                        WV-02-EMP-PRJ-NM    IS "ProjectName"            02335411
               TYPE OF  WV-02-EMP-DPT       IS ATTRIBUTE                02335511
                        WV-02-EMP-NAME      IS ELEMENT                  02335611
                        WV-02-EMP-ID        IS ELEMENT                  02335711
                        WV-02-EMP-DESG      IS ELEMENT                  02335811
                        WV-02-EMP-JOIN-YR   IS ELEMENT                  02335911
                        WV-02-EMP-SALARY    IS ELEMENT                  02336011
                        WV-02-EMP-STREET    IS ELEMENT                  02336111
                        WV-02-EMP-ZIP       IS ELEMENT                  02336211
                        WV-02-EMP-CITY      IS ELEMENT                  02336311
                        WV-02-EMP-STATE     IS ELEMENT                  02336411
                        WV-02-EMP-SKILL     IS ELEMENT                  02336511
                        WV-02-EMP-TYPE      IS ELEMENT                  02336611
                        WV-02-EMP-REL-EXP   IS ELEMENT                  02336711
                        WV-02-EMP-TOT-EXP   IS ELEMENT                  02336811
                        WV-02-EMP-CMP-LCN   IS ELEMENT                  02336911
                        WV-02-EMP-PRJ-NM    IS ELEMENT                  02337011
               SUPPRESS                                                 02337900
                        EVERY NONNUMERIC ELEMENT WHEN SPACE             02338000
                        EVERY NUMERIC    ELEMENT WHEN ZERO              02338100
               ON EXCEPTION                                             02338200
                        DISPLAY 'ERROR IN XML GENERATE: ' XML-CODE      02338300
                        MOVE HIGH-VALUES    TO WI-ERR-IC                02338400
                        MOVE WC-EXE-CD-ERR  TO WV-EXE-CD                02338500
                        MOVE XML-CODE       TO WV-XML-CODE              02338600
                        MOVE WV-02-EMP-ID   TO WV-EMP-ID                02338701
                        MOVE WV-02-EMP-NAME TO WV-EMP-NAME              02338801
               NOT ON EXCEPTION                                         02338900
                        MOVE WC-EXE-CD-OK   TO WV-EXE-CD                02339000
           END-XML.                                                     02339100
                                                                        02339200
                                                                        02339300
       Z070-CNCT-XML-HDR.                                               02340000
      *-------------------                                              02350000
                                                                        02360000
           IF WV-XML-DATA-LN > ZEROS                                    02370000
              STRING FUNCTION TRIM(WC-HDR-INFO TRAILING)                02380010
                                            DELIMITED BY SIZE           02381010
                     WV-XML-OPT-DATA        DELIMITED BY SIZE           02390000
                     INTO OUT-REC-ZN                                    02400000
              END-STRING                                                02410000
           END-IF.                                                      02420000
                                                                        02430000
       Z080-FILL-UP-ERR-DTLS.                                           02440000
      *-----------------------                                          02450000
                                                                        02460000
           STRING    WC-ERR-INFO            DELIMITED BY SIZE           02470000
                     WV-EMP-ID              DELIMITED BY SPACE          02480000
                     WC-COMMA               DELIMITED BY SIZE           02490000
                     WV-EMP-NAME            DELIMITED BY SIZE           02500000
                     WV-XML-CODE            DELIMITED BY SIZE           02510000
                     INTO ERR-REC-ZN                                    02520000
           END-STRING.                                                  02530000
                                                                        02540000
       Z090-WRITE-ERR-FILE.                                             02550000
      *---------------------                                            02560000
                                                                        02570000
           WRITE  ERRFILE-REC.                                          02580000
                                                                        02590000
           ADD WC-ONE                       TO WA-ERROR-CTR.            02600000
                                                                        02610000
       Z100-WRITE-OPT-FILE.                                             02620000
      *---------------------                                            02630000
                                                                        02640000
           WRITE  OUTFILE-REC.                                          02650000
                                                                        02660000
           ADD WC-ONE                       TO WA-WRITE-CTR.            02670000
                                                                        02680000
       Z200-PCS-CTRS.                                                   02690000
      *---------------                                                  02700000
                                                                        02710000
           DISPLAY ' TOTAL NUMBER OF RECORDS READ:' WA-READ-CTR.        02720000
                                                                        02730000
           DISPLAY ' TOTAL NUMBER OF OUTPUT RECORD:'WA-WRITE-CTR.       02740000
                                                                        02750000
           DISPLAY ' TOTAL NUMBER OF ERROR RECORD:'WA-ERROR-CTR.        02760000
                                                                        02770000
