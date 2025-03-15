       CBL XMLPARSE(XMLSS)                                              00001013
       ID DIVISION.                                                     00010000
      **************                                                    00020000
                                                                        00021007
       PROGRAM-ID.     XMLPAR01.                                        00030002
                                                                        00040000
       AUTHOR.         MAINFRAMETECHVIDEOS.                             00050002
                                                                        00060000
       DATE-WRITTEN.   2023/12/10.                                      00070002
                                                                        00080000
       DATE-COMPILED.                                                   00090002
                                                                        00100002
       ENVIRONMENT DIVISION.                                            00110002
      ***********************                                           00120002
                                                                        00130002
       INPUT-OUTPUT SECTION.                                            00140003
      *=====================                                            00150002
                                                                        00160002
       FILE-CONTROL.                                                    00170002
      *--------------                                                   00180002
                                                                        00190002
           SELECT  INFILE    ASSIGN         TO UT-S-INFILE.             00200003
           SELECT  BOOKFILE  ASSIGN         TO UT-S-BOOKFILE.           00210004
           SELECT  FOODFILE  ASSIGN         TO UT-S-FOODFILE.           00211004
                                                                        00220003
       DATA DIVISION.                                                   00230003
      ****************                                                  00240003
                                                                        00250003
       FILE SECTION.                                                    00260003
      *=============                                                    00270003
                                                                        00280003
       FD INFILE                                                        00290003
              LABEL RECORD STANDARD                                     00300008
              BLOCK 0 RECORDS                                           00310008
              RECORDING MODE IS F.                                      00320008
                                                                        00330003
       01 INFILE-REC-ZN.                                                00340003
          03 FTN-CD                         PIC X(02).                  00341006
          03 INFILE-REC                     PIC X(798).                 00350006
                                                                        00360003
       FD FOODFILE                                                      00370004
              LABEL  RECORD STANDARD                                    00380008
              BLOCK 0 RECORDS                                           00390008
              RECORDING MODE IS F.                                      00400008
                                                                        00410003
       01 FOOD-REC-ZN.                                                  00420004
          03 FOOD-REC                       PIC X(250).                 00430004
                                                                        00440003
       FD BOOKFILE                                                      00441004
              LABEL  RECORD STANDARD                                    00442008
              BLOCK 0 RECORDS                                           00443008
              RECORDING MODE IS F.                                      00444008
                                                                        00445004
       01 BOOK-REC-ZN.                                                  00446004
          03 BOOK-REC                       PIC X(250).                 00447004
                                                                        00448004
       WORKING-STORAGE SECTION.                                         00480003
      *=========================                                        00490003
                                                                        00500003
       01 WS-WA.                                                        00500105
      *---------                                                        00500205
                                                                        00500305
          03 WA-READ-CTR                    PIC S9(05) PACKED-DECIMAL.  00500405
          03 WA-BOOK-CTR                    PIC S9(05) PACKED-DECIMAL.  00500505
          03 WA-FOOD-CTR                    PIC S9(05) PACKED-DECIMAL.  00500605
                                                                        00500705
       01 WS-WI.                                                        00501003
      *----------                                                       00502003
                                                                        00502103
          03 WI-EOF-REC                     PIC X(01).                  00503003
          03 WI-EOF-INFILE                  PIC X(01).                  00503105
                                                                        00504003
       01 WS-CS.                                                        00510003
      *-------------------------                                        00511003
                                                                        00512003
          03 WC-PGM-NM                      PIC X(08) VALUE 'XMLPAR01'. 00520003
          03 WC-PCS-NM                      PIC X(50) VALUE             00530003
             'XML PARSING TO COPYBOOK'.                                 00540003
          03 WC-IC-ON                       PIC X(01) VALUE '1'.        00550003
          03 WC-IC-OFF                      PIC X(01) VALUE '0'.        00560003
          03 WC-EXC-CD-OK                   PIC X(02) VALUE '00'.       00560130
          03 WC-EXC-CD-WRN                  PIC X(02) VALUE '04'.       00560230
          03 WC-EXC-CD-ERR                  PIC X(02) VALUE '08'.       00560330
          03 WC-FTN-BK-CD                   PIC X(02) VALUE '02'.       00560406
          03 WC-FTN-FD-CD                   PIC X(02) VALUE '01'.       00560506
          03 WC-ONE                         PIC 9(01) VALUE 1.          00560606
          03 WC-FOOD-NM                     PIC X(04) VALUE 'name'.     00561015
          03 WC-FOOD-PRICE                  PIC X(05) VALUE 'price'.    00562015
          03 WC-FOOD-DESC                   PIC X(11) VALUE             00563003
             'description'.                                             00564015
          03 WC-FOOD-CALORY                 PIC X(08) VALUE 'calories'. 00565015
          03 WC-BOOK-ID                     PIC X(02) VALUE 'id'.       00566015
          03 WC-BOOK-AUTHOR                 PIC X(06) VALUE 'author'.   00566132
          03 WC-BOOK-TITLE                  PIC X(05) VALUE 'title'.    00567015
          03 WC-BOOK-GENRE                  PIC X(05) VALUE 'genre'.    00568015
          03 WC-BOOK-PRICE                  PIC X(05) VALUE 'price'.    00569015
          03 WC-BOOK-PUB-DT                 PIC X(12) VALUE             00569103
             'publish_date'.                                            00569215
          03 WC-BOOK-DESC                   PIC X(11) VALUE             00569303
             'description'.                                             00569415
          03 WC-XML-STT-DOC                 PIC X(21) VALUE             00569506
             'START-OF-DOCUMENT'.                                       00569606
          03 WC-XML-END-DOC                 PIC X(19) VALUE             00569706
             'END-OF-DOCUMENT'.                                         00569806
          03 WC-XML-STT-ELM                 PIC X(20) VALUE             00569906
             'START-OF-ELEMENT'.                                        00570006
          03 WC-XML-END-ELM                 PIC X(18) VALUE             00570106
             'END-OF-ELEMENT'.                                          00570206
          03 WC-XML-CONT-CHR                PIC X(22) VALUE             00570306
             'CONTENT-CHARACTERS'.                                      00570406
          03 WC-XML-ATTRIB-CHR              PIC X(24) VALUE             00570543
             'ATTRIBUTE-CHARACTERS'.                                    00570643
          03 WC-XML-ATTRIB-NAME             PIC X(18) VALUE             00570740
             'ATTRIBUTE-NAME'.                                          00570840
                                                                        00571003
       01 WS-WV.                                                        00580003
      *----------                                                       00590035
                                                                        00600003
          03 WV-EXC-CD                      PIC X(02).                  00601030
          03 WV-ELEMENT-NM                  PIC X(12).                  00610003
          03 WV-INPUT                       PIC X(300).                 00610135
          03 WV-WRK-PRICE                   PIC X(06).                  00611046
          03 WV-XML-TEXT                    PIC X(20).                  00612003
          03 WV-FILE-REC.                                               00620003
             05 WV-FOOD-REC.                                            00630003
                07 WV-BREAK-FAST-MENU.                                  00640003
                   09 WV-NAME               PIC X(20).                  00650003
                   09 WV-PRICE              PIC $Z9.99.                 00660003
                   09 WV-DESC               PIC X(100).                 00670003
                   09 WV-CALORY             PIC X(03).                  00680003
                07 WV-RSV-ZN                PIC X(129).                 00681048
             05 WV-CATLG-REC.                                           00690003
                07 WV-BOOK-REC.                                         00700003
                   09 WV-ID                 PIC X(05).                  00710003
                   09 WV-AUTHOR             PIC X(25).                  00720003
                   09 WV-TITLE              PIC X(25).                  00730044
                   09 WV-GENRE              PIC X(20).                  00740003
                   09 WV-PRICE              PIC $Z9.99.                 00750003
                   09 WV-PUB-DT             PIC X(10).                  00760003
                   09 WV-DESC               PIC X(120).                 00770003
                07 WV-RSV-ZN                PIC X(39).                  00771048
                                                                        00780003
       PROCEDURE DIVISION.                                              00790003
      ********************                                              00800003
                                                                        00810003
           PERFORM A010-STT-XML-PARSE.                                  00820003
                                                                        00830003
           PERFORM A020-PCS-XML-PARSE                                   00840006
                                UNTIL WI-EOF-INFILE = HIGH-VALUES.      00841006
                                                                        00850003
           PERFORM A030-END-XML-PARSE.                                  00860003
                                                                        00870003
           STOP RUN.                                                    00880003
                                                                        00890003
      **************************** LEVEL A *****************************00891004
                                                                        00892004
       A010-STT-XML-PARSE.                                              00900003
      *--------------------                                             00910003
                                                                        00920003
           PERFORM Z010-OPN-FILES.                                      00930003
                                                                        00940003
           PERFORM Z030-INIT-FLDS.                                      00950003
                                                                        00960003
           PERFORM Z040-READ-INFILE.                                    00970003
                                                                        00980003
       A020-PCS-XML-PARSE.                                              00990003
      *-------------------                                              01000003
                                                                        01010003
           XML PARSE WV-INPUT                                           01020023
               PROCESSING PROCEDURE B010-XML-HANDLE                     01021004
            ON EXCEPTION                                                01022004
               DISPLAY 'XML PARSE ERROR XML-CODE: ' XML-CODE            01023004
               MOVE WC-EXC-CD-ERR           TO WV-EXC-CD                01023134
           END-XML.                                                     01024004
                                                                        01024122
           IF WV-EXC-CD = WC-EXC-CD-OK                                  01024234
              PERFORM B020-WRITE-OPT-FILE                               01024334
           END-IF.                                                      01024434
                                                                        01024534
           PERFORM Z040-READ-INFILE.                                    01024634
                                                                        01025016
       A030-END-XML-PARSE.                                              01040003
      *--------------------                                             01050003
                                                                        01060003
           PERFORM Z990-PCS-CTRS.                                       01061030
                                                                        01062030
           PERFORM Z020-CLS-FILES.                                      01070003
                                                                        01080003
      **************************** LEVEL B *****************************01100004
                                                                        01101004
       B010-XML-HANDLE.                                                 01102004
      *----------------                                                 01103004
                                                                        01104004
           EVALUATE XML-EVENT                                           01105004
               WHEN WC-XML-STT-DOC                                      01105106
                    PERFORM Z050-MVE-LOW-VAL                            01105206
                    PERFORM Z090-INIT-WRK-FLDS                          01105306
               WHEN WC-XML-STT-ELM                                      01105408
                    PERFORM Z070-ENRICH-ELE-VAL                         01105506
               WHEN WC-XML-END-ELM                                      01105608
                    PERFORM Z080-INIT-ELE-VAL                           01105706
               WHEN WC-XML-END-DOC                                      01105806
                    PERFORM Z060-MVE-HIGH-VAL                           01106106
               WHEN WC-XML-CONT-CHR                                     01106206
                    PERFORM C010-PCS-XML-PARSE                          01106306
               WHEN WC-XML-ATTRIB-CHR                                   01106430
                    PERFORM D020-PCS-BK-XML                             01106530
               WHEN WC-XML-ATTRIB-NAME                                  01106640
                    PERFORM Z070-ENRICH-ELE-VAL                         01106740
               WHEN OTHER                                               01106806
                    CONTINUE                                            01106930
           END-EVALUATE.                                                01107104
                                                                        01108004
       B020-WRITE-OPT-FILE.                                             01109030
      *---------------------                                            01109130
                                                                        01109230
           IF FTN-CD = WC-FTN-FD-CD                                     01109339
              PERFORM Z210-WRITE-FD-FILE                                01109430
           ELSE                                                         01109530
              PERFORM Z220-WRITE-BK-FILE                                01109630
           END-IF.                                                      01109730
                                                                        01109830
      **************************** LEVEL C *****************************01110006
                                                                        01120006
       C010-PCS-XML-PARSE.                                              01120106
      *--------------------                                             01120206
                                                                        01120306
           EVALUATE FTN-CD                                              01120406
               WHEN WC-FTN-FD-CD                                        01120506
                    PERFORM D010-PCS-FD-XML                             01120606
               WHEN WC-FTN-BK-CD                                        01120706
                    PERFORM D020-PCS-BK-XML                             01120806
               WHEN OTHER                                               01120906
                    CONTINUE                                            01121006
           END-EVALUATE.                                                01121106
                                                                        01121206
      **************************** LEVEL D *****************************01122006
                                                                        01122106
       D010-PCS-FD-XML.                                                 01122206
      *-----------------                                                01122306
                                                                        01122406
           EVALUATE WV-ELEMENT-NM                                       01122506
               WHEN WC-FOOD-NM                                          01122606
                    PERFORM Z100-MVE-FD-NM                              01122706
               WHEN WC-FOOD-PRICE                                       01122806
                    PERFORM Z110-MVE-FD-PRICE                           01122906
               WHEN WC-FOOD-DESC                                        01123006
                    PERFORM Z120-MVE-FD-DESC                            01123106
               WHEN WC-FOOD-CALORY                                      01123206
                    PERFORM Z130-MVE-FD-CALORY                          01123306
               WHEN OTHER                                               01123406
                    CONTINUE                                            01123506
           END-EVALUATE.                                                01123606
                                                                        01123706
       D020-PCS-BK-XML.                                                 01123806
      *-----------------                                                01123906
                                                                        01124006
           EVALUATE WV-ELEMENT-NM                                       01124230
               WHEN WC-BOOK-ID                                          01124330
                    PERFORM Z140-MVE-BK-ID                              01124432
               WHEN WC-BOOK-AUTHOR                                      01124532
                    PERFORM Z150-MVE-BK-AUTHOR                          01124632
               WHEN WC-BOOK-TITLE                                       01124732
                    PERFORM Z160-MVE-BK-TITLE                           01124833
               WHEN WC-BOOK-GENRE                                       01124932
                    PERFORM Z170-MVE-BK-GENRE                           01125032
               WHEN WC-BOOK-PRICE                                       01125132
                    PERFORM Z180-MVE-BK-PRICE                           01125232
               WHEN WC-BOOK-PUB-DT                                      01125332
                    PERFORM Z190-MVE-BK-PUB-DT                          01125432
               WHEN WC-BOOK-DESC                                        01125532
                    PERFORM Z200-MVE-BK-DESC                            01125632
           END-EVALUATE.                                                01125732
                                                                        01125832
      **************************** LEVEL Z *****************************01125932
                                                                        01126006
       Z010-OPN-FILES.                                                  01130006
      *----------------                                                 01140006
                                                                        01150006
           OPEN INPUT  INFILE                                           01160006
                OUTPUT FOODFILE                                         01170006
                       BOOKFILE.                                        01180006
                                                                        01190006
       Z020-CLS-FILES.                                                  01200006
      *---------------                                                  01210006
                                                                        01220006
           CLOSE  INFILE                                                01230007
                  FOODFILE                                              01240006
                  BOOKFILE.                                             01250006
                                                                        01260006
       Z030-INIT-FLDS.                                                  01270006
      *---------------                                                  01280006
                                                                        01290006
           INITIALIZE WS-WV                                             01300014
                      WS-WA.                                            01301014
                                                                        01310006
           MOVE LOW-VALUES                  TO WI-EOF-INFILE            01320006
                                               WI-EOF-REC.              01330006
                                                                        01331030
           MOVE WC-EXC-CD-OK                TO WV-EXC-CD.               01332030
                                                                        01340006
       Z040-READ-INFILE.                                                01350006
      *-----------------                                                01360006
                                                                        01370006
           READ INFILE                                                  01380006
             AT END                                                     01390006
                MOVE HIGH-VALUES            TO WI-EOF-INFILE            01400006
           END-READ.                                                    01410006
                                                                        01420006
           IF WI-EOF-INFILE = LOW-VALUES                                01430006
              INITIALIZE WV-INPUT                                       01431035
              ADD WC-ONE                    TO WA-READ-CTR              01440006
              MOVE FUNCTION TRIM(INFILE-REC TRAILING)                   01442023
                                            TO WV-INPUT                 01443023
           END-IF.                                                      01450006
                                                                        01460006
                                                                        01470006
       Z050-MVE-LOW-VAL.                                                01480006
      *-----------------                                                01490006
                                                                        01500006
           MOVE LOW-VALUES                  TO WI-EOF-REC.              01510006
                                                                        01520006
       Z060-MVE-HIGH-VAL.                                               01530006
      *-------------------                                              01540006
                                                                        01550006
           MOVE HIGH-VALUES                 TO WI-EOF-REC.              01560006
                                                                        01570006
       Z070-ENRICH-ELE-VAL.                                             01580006
      *--------------------                                             01590006
                                                                        01600006
           MOVE XML-TEXT                    TO WV-ELEMENT-NM.           01610006
                                                                        01620006
       Z080-INIT-ELE-VAL.                                               01630006
      *------------------                                               01640006
                                                                        01650006
           MOVE SPACES                      TO WV-ELEMENT-NM.           01660006
                                                                        01670006
       Z090-INIT-WRK-FLDS.                                              01680006
      *--------------------                                             01690006
                                                                        01700006
           INITIALIZE WV-FOOD-REC                                       01710006
                      WV-WRK-PRICE                                      01710106
                      WV-CATLG-REC.                                     01711047
                                                                        01720006
       Z100-MVE-FD-NM.                                                  01730006
      *----------------                                                 01740006
                                                                        01750006
           MOVE XML-TEXT                    TO WV-NAME.                 01751006
                                                                        01752006
       Z110-MVE-FD-PRICE.                                               01760006
      *------------------                                               01770006
                                                                        01780006
           MOVE XML-TEXT                    TO WV-WRK-PRICE.            01781026
                                                                        01781130
           COMPUTE WV-PRICE IN WV-BREAK-FAST-MENU =                     01781206
                   FUNCTION NUMVAL-C(WV-WRK-PRICE).                     01781329
                                                                        01782006
       Z120-MVE-FD-DESC.                                                01790006
      *------------------                                               01800006
                                                                        01810030
           MOVE XML-TEXT                    TO WV-DESC                  01811006
                                            IN WV-BREAK-FAST-MENU.      01811106
                                                                        01812006
       Z130-MVE-FD-CALORY.                                              01820006
      *--------------------                                             01830006
                                                                        01840006
           MOVE XML-TEXT                    TO WV-CALORY.               01850006
                                                                        01860030
       Z140-MVE-BK-ID.                                                  01870030
      *----------------                                                 01880030
                                                                        01890030
           MOVE XML-TEXT                    TO WV-ID.                   01900030
                                                                        01910030
       Z150-MVE-BK-AUTHOR.                                              01920030
      *--------------------                                             01930030
                                                                        01940030
           MOVE XML-TEXT                    TO WV-AUTHOR.               01950030
                                                                        01960030
       Z160-MVE-BK-TITLE.                                               01970030
      *-------------------                                              01980030
                                                                        01990030
           MOVE XML-TEXT                    TO WV-TITLE.                02000030
                                                                        02010030
       Z170-MVE-BK-GENRE.                                               02020030
      *-------------------                                              02030030
                                                                        02040030
           MOVE XML-TEXT                    TO WV-GENRE.                02050030
                                                                        02060030
       Z180-MVE-BK-PRICE.                                               02070030
      *-------------------                                              02080030
                                                                        02090030
           MOVE XML-TEXT                    TO WV-WRK-PRICE.            02100030
                                                                        02110030
           COMPUTE  WV-PRICE IN WV-BOOK-REC =                           02120030
                    FUNCTION NUMVAL-C(WV-WRK-PRICE).                    02130045
                                                                        02140030
       Z190-MVE-BK-PUB-DT.                                              02150030
      *--------------------                                             02160030
                                                                        02170030
           MOVE XML-TEXT                    TO WV-PUB-DT.               02180030
                                                                        02190030
       Z200-MVE-BK-DESC.                                                02200030
      *------------------                                               02210030
                                                                        02220030
           MOVE XML-TEXT                    TO WV-DESC                  02230030
                                            IN WV-BOOK-REC.             02240032
                                                                        02250030
       Z210-WRITE-FD-FILE.                                              02260030
      *--------------------                                             02270030
                                                                        02280030
           INITIALIZE FOOD-REC.                                         02281030
                                                                        02282030
           MOVE WV-FOOD-REC                 TO FOOD-REC.                02283030
                                                                        02283137
           WRITE FOOD-REC-ZN.                                           02283238
                                                                        02284030
           ADD WC-ONE                       TO WA-FOOD-CTR.             02284130
                                                                        02285030
       Z220-WRITE-BK-FILE.                                              02290030
      *--------------------                                             02300030
                                                                        02310030
           INITIALIZE BOOK-REC.                                         02320030
                                                                        02330030
           MOVE WV-CATLG-REC                TO BOOK-REC.                02340030
                                                                        02341036
           WRITE BOOK-REC-ZN.                                           02342038
                                                                        02350030
           ADD WC-ONE                       TO WA-BOOK-CTR.             02360030
                                                                        02370030
       Z990-PCS-CTRS.                                                   02380030
      *---------------                                                  02390030
                                                                        02400030
           DISPLAY 'NUMBER OF RECORDS READ :-' WA-READ-CTR.             02410030
           DISPLAY 'NUMBER OF FOOD RECORDS :-' WA-FOOD-CTR.             02420030
           DISPLAY 'NUMBER OF BOOK RECORDS :-' WA-BOOK-CTR.             02430030
                                                                        02440030
