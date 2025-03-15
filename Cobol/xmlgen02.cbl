       ID DIVISION.                                                     00010000
      *************                                                     00020000
                                                                        00030000
       PROGRAM-ID.      XMLGEN02.                                       00040000
                                                                        00050000
       AUTHOR.          MAINFRAMETECHVIDEOS.                            00060000
                                                                        00070000
       DATE-WRITTEN.    2023/12/09.                                     00080000
                                                                        00090000
       DATE-COMPILED.                                                   00100000
                                                                        00110000
       ENVIRONMENT DIVISION.                                            00120000
      **********************                                            00121000
                                                                        00130000
       INPUT-OUTPUT SECTION.                                            00140000
      *=====================                                            00150000
                                                                        00160000
       FILE-CONTROL.                                                    00170000
      *-------------                                                    00180000
                                                                        00190000
           SELECT INFILE   ASSIGN           TO UT-S-INFILE.             00200000
           SELECT OUTFILE  ASSIGN           TO UT-S-OUTFILE.            00210000
                                                                        00220000
       DATA DIVISION.                                                   00230000
      ****************                                                  00240000
                                                                        00250000
       FILE SECTION.                                                    00260000
      *=============                                                    00270000
                                                                        00280000
       FD INFILE                                                        00290000
           LABEL RECORD STANDARD                                        00300004
           BLOCK 0 RECORDS                                              00310004
           RECORDING MODE IS F.                                         00320004
                                                                        00330000
       01 INFILE-REC-ZN.                                                00340000
          03 INFILE-REC                     PIC X(200).                 00350004
                                                                        00360000
       FD OUTFILE                                                       00370000
           LABEL RECORD STANDARD                                        00371004
           BLOCK 0 RECORDS                                              00372004
           RECORDING MODE IS F.                                         00373004
                                                                        00380000
       01 OUTFILE-REC-ZN.                                               00390000
          03 OUTFILE-REC                    PIC X(1000).                00400000
                                                                        00410000
       WORKING-STORAGE SECTION.                                         00420001
      *-------------------------                                        00430001
                                                                        00440001
       01 WS-WA.                                                        00450001
      *----------                                                       00451001
                                                                        00452004
           03 WA-READ-CTR                   PIC S9(05) PACKED-DECIMAL.  00460002
           03 WA-WRITE-CTR                  PIC S9(05) PACKED-DECIMAL.  00470002
                                                                        00480001
       01 WS-WB.                                                        00481005
      *----------                                                       00482004
                                                                        00483004
           03 WB-EOF-INFILE                 PIC X(01).                  00484005
                                                                        00485004
       01 WS-CS.                                                        00490001
      *---------                                                        00500001
                                                                        00510001
           03 WC-PGM-NM                     PIC X(08) VALUE 'XMLGEN02'. 00520002
           03 WC-PCS-NM                     PIC X(30) VALUE             00530002
              'XML GENERATION WITH ALL OPTION'.                         00540002
           03 WC-IC-ON                      PIC X(01) VALUE '1'.        00550002
           03 WC-IC-OFF                     PIC X(01) VALUE '0'.        00560002
           03 WC-EXC-CD-OK                  PIC X(02) VALUE '00'.       00570002
           03 WC-EXC-CD-WRN                 PIC X(02) VALUE '04'.       00580002
           03 WC-EXC-CD-ERR                 PIC X(02) VALUE '08'.       00590002
           03 WC-ONE                        PIC 9(01) VALUE  1.         00600002
           03 WC-NP                         PIC X(03) VALUE 'PRE'.      00601003
           03 WC-NS                         PIC X(15) VALUE             00602003
              'http://example'.                                         00603003
                                                                        00610001
       01 WS-WV.                                                        00620001
      *---------                                                        00630001
                                                                        00640002
           03 WV-OUTPUT-XML                 PIC X(900).                 00650011
           03 WV-FMT-OUT-XML                PIC X(900).                 00650111
           03 WV-EXC-CD                     PIC X(02).                  00650210
           03 WV-COUNTER                    PIC S9(05) BINARY.          00651004
           03 WV-GREETING-MSG.                                          00660002
              05 WV-MSG.                                                00670002
                 07 WV-MSG-SEVERITY         PIC X(01).                  00680002
                 07 WV-MSG-DATE             PIC X(10).                  00690002
                 07 WV-MSG-TYPE             PIC X(10).                  00700002
                 07 WV-MSG-CATEGORY         PIC X(10).                  00710002
                 07 WV-MSG-TEXT             PIC X(30).                  00720002
                                                                        00730002
       PROCEDURE DIVISION.                                              00740002
      *********************                                             00750002
                                                                        00760002
           PERFORM A010-STT-XML-GEN.                                    00770002
                                                                        00780002
           PERFORM A020-PCS-XML-GEN                                     00790002
                                UNTIL WB-EOF-INFILE = HIGH-VALUES.      00791006
                                                                        00800002
           PERFORM A030-END-XML-GEN.                                    00810002
                                                                        00820002
           STOP RUN.                                                    00830002
                                                                        00840002
       A010-STT-XML-GEN.                                                00850002
      *-----------------                                                00860002
                                                                        00870002
           PERFORM Z010-OPN-FILES.                                      00871002
                                                                        00872002
           PERFORM Z030-INIT-FLDS.                                      00875002
                                                                        00876002
           PERFORM Z040-READ-INFILE.                                    00877002
                                                                        00878002
       A020-PCS-XML-GEN.                                                00880002
      *------------------                                               00890002
                                                                        00900002
           PERFORM B010-PCS-XML-GEN.                                    00901002
                                                                        00902002
           PERFORM Z040-READ-INFILE.                                    00903002
                                                                        00904002
       A030-END-XML-GEN.                                                00910002
      *------------------                                               00920002
                                                                        00930002
           PERFORM Z900-DISPLAY-CTR.                                    00930102
                                                                        00930202
           PERFORM Z020-CLS-FILES.                                      00931002
                                                                        00932002
      ************************ LEVEL B *********************************00940002
                                                                        00941002
       B010-PCS-XML-GEN.                                                00942004
      *-----------------                                                00943002
                                                                        00944002
           PERFORM Z050-GEN-XML.                                        00945002
                                                                        00946002
           IF WV-EXC-CD = WC-EXC-CD-OK                                  00947012
              PERFORM Z060-WRITE-OUTFILE                                00947112
           END-IF.                                                      00947212
                                                                        00948012
      ************************ LEVEL Z *********************************00950002
                                                                        00960002
       Z010-OPN-FILES.                                                  00970002
      *----------------                                                 00980002
                                                                        00990002
           OPEN INPUT  INFILE                                           01000002
                OUTPUT OUTFILE.                                         01010002
                                                                        01020002
       Z020-CLS-FILES.                                                  01030002
      *---------------                                                  01040002
                                                                        01050002
           CLOSE INFILE                                                 01060002
                 OUTFILE.                                               01070002
                                                                        01080002
       Z030-INIT-FLDS.                                                  01090002
      *---------------                                                  01100002
                                                                        01110002
           INITIALIZE WS-WB                                             01120004
                      WS-WA                                             01130004
                      WS-WV.                                            01140002
                                                                        01150002
           MOVE LOW-VALUES                  TO WB-EOF-INFILE.           01160002
           MOVE WC-EXC-CD-OK                TO WV-EXC-CD.               01161010
                                                                        01170002
       Z040-READ-INFILE.                                                01180002
      *-----------------                                                01190002
                                                                        01200002
           READ INFILE                                                  01210002
             AT END                                                     01220002
                MOVE HIGH-VALUES            TO WB-EOF-INFILE            01230002
           END-READ.                                                    01240002
                                                                        01250002
           IF WB-EOF-INFILE = LOW-VALUES                                01260002
              ADD WC-ONE                    TO WA-READ-CTR              01270002
              MOVE INFILE-REC               TO WV-GREETING-MSG          01280002
           END-IF.                                                      01290002
                                                                        01300002
       Z050-GEN-XML.                                                    01310002
      *--------------                                                   01320002
                                                                        01330002
           XML GENERATE WV-OUTPUT-XML FROM WV-GREETING-MSG              01340002
                  COUNT IN WV-COUNTER                                   01350002
                   WITH ENCODING 1208                                   01360015
                   WITH XML-DECLARATION                                 01370002
                   WITH ATTRIBUTES                                      01371002
                        NAMESPACE           IS WC-NS                    01380003
                        NAMESPACE-PREFIX    IS WC-NP                    01390003
                   NAME OF  WV-GREETING-MSG IS "Greeting"               01400002
                            WV-MSG          IS "Msg"                    01401002
                            WV-MSG-SEVERITY IS "Msg-Severity"           01402002
                            WV-MSG-DATE     IS "Msg-Date"               01403002
                            WV-MSG-TYPE     IS "Msg-Type"               01404002
                            WV-MSG-CATEGORY IS "Msg-Category"           01405002
                            WV-MSG-TEXT     IS "Msg-Text"               01406002
                   TYPE OF  WV-MSG-SEVERITY IS ATTRIBUTE                01410002
                            WV-MSG-DATE     IS ATTRIBUTE                01411002
                            WV-MSG-TYPE     IS ELEMENT                  01412002
                            WV-MSG-CATEGORY IS ELEMENT                  01413002
                            WV-MSG-TEXT     IS ELEMENT                  01414002
               SUPPRESS                                                 01420002
                            EVERY NUMERIC ELEMENT WHEN ZEROS            01421003
                            EVERY NONNUMERIC ELEMENT WHEN SPACES        01422003
                     ON EXCEPTION                                       01430002
                            DISPLAY 'ERROR IN XML GENERATION:'XML-CODE  01431003
                            MOVE WC-EXC-CD-ERR TO WV-EXC-CD             01432010
                NOT  ON EXCEPTION                                       01440002
                            DISPLAY 'SUCCESSFULLY GENRATED XML'         01441003
           END-XML.                                                     01450002
                                                                        01460002
       Z060-WRITE-OUTFILE.                                              01461004
      *-------------------                                              01462004
                                                                        01463004
           MOVE FUNCTION DISPLAY-OF(                                    01463309
                FUNCTION NATIONAL-OF(WV-OUTPUT-XML,1208),00875)         01463409
                                            TO WV-FMT-OUT-XML.          01463511
           MOVE WV-FMT-OUT-XML (1:WV-COUNTER)                           01463613
                                            TO OUTFILE-REC.             01463711
                                                                        01463809
           WRITE OUTFILE-REC-ZN.                                        01464005
                                                                        01465004
           ADD WC-ONE                       TO WA-WRITE-CTR.            01466004
                                                                        01467004
       Z900-DISPLAY-CTR.                                                01470004
      *-----------------                                                01480004
                                                                        01490004
           DISPLAY 'NO OF RECORD READ :'WA-READ-CTR.                    01500004
           DISPLAY 'NO OF RECORD WRITE :'WA-WRITE-CTR.                  01510004
                                                                        01520004
