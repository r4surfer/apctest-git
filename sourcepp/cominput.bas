        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   CCC    OOO   M   M  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  C   C  O   O  MM MM    I    NN  N  P   P  U   U    T     *~
            *  C      O   O  M M M    I    N N N  PPPP   U   U    T     *~
            *  C   C  O   O  M   M    I    N  NN  P      U   U    T     *~
            *   CCC    OOO   M   M  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * COMINPUT - ENTER/EDIT COMMON TERMS FOR PERSONEL.  FILE IS *~
            *            'COMTERM'.  CALLS 'COMSUB' TO PERFORM THE      *~
            *            INPUT/EDIT/REVIEW, ETC.                        *~
            *            SEE THE SOURCE CODE FOR 'COMSUB' FOR THE LIST  *~
            *            OF TYPES (CLASSES) OF TERMS ALLOWED.           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/10/83 ! ORIGINAL                                 ! GLW *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            pass$162,                    /* FOR COMSUB                 */~
            ddescr$30, text1$50, text2$50,                               ~
            plowkey$100,                                                 ~
            rslt$20,                     /* TEXT FROM FILE OPENING     */~
            axd$4                        /* ALT KEY POINTER FROM OPEN'G*/


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #04 ! COMTERM  ! File of common terms for personel.       *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other         "
        REM *************************************************************
            select #04, "COMTERM",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  162,                                  ~
                        keypos =   47, keylen =  16,                     ~
                        alt key  1, keypos =   17, keylen =  46,         ~
                            key  2, keypos =    1, keylen =  62


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("LINKING TO DATA BASE FOR COMMON TERMS")
            f2% = 1%
            call "OPENFILE" (#04, "SHARE", f2%, rslt$, axd$)
                     if f2% = 0% then goto L00810
            call "OPENFILE" (#04, "OUTPT", f2%, rslt$, axd$)
                     close #4
            call "OPENFILE" (#04, "SHARE", f2%, rslt$, axd$)

L00810: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)



L00920:     accept                                                       ~
               at (01,02),                                               ~
                  "DIRECTLY MANAGE PERSONNEL COMMON TERMS",              ~
               at (01,67),                                               ~
                  "DATE:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (05,02),                                               ~
                  "THIS PROGRAM ALLOWS YOU TO ENTER, EDIT, OR REVIEW THE ~
        ~TERMS THAT ARE COMMON TO ",                                      ~
               at (06,02),                                               ~
                  "THE PERSONNEL FUNCTION.  YOU CAN THEREBY INSURE THAT A~
        ~LL OF YOUR EMPLOYEES     ",                                      ~
               at (07,02),                                               ~
                  "WHO USE THE CAELUS PERSONNEL MANAGEMENT SYSTEM WILL DR~
        ~AW FROM THE SAME LIBRARY ",                                      ~
               at (08,02),                                               ~
                  "OF DEFINITIONS.  THEREFORE, WHEN YOU DECIDE THAT A CER~
        ~TAIN JOB TITLE IS TO BE  ",                                      ~
               at (09,02),                                               ~
                  "CALLED 'WELDING-ARC', FOR EXAMPLE, YOU CAN BE ASSURED ~
        ~THAT EVERYONE WILL USE   ",                                      ~
               at (10,02),                                               ~
                  "EXACTLY THAT SAME TERM WITHOUT DANGER OF MISSPELLINGS ~
        ~OR DATA ENTRY ERRORS.    ",                                      ~
               at (12,02),                                               ~
                  "ADDING TO OR CHANGING THE COMMON TERMS DICTIONARY IS V~
        ~ERY SIMPLE.  WHEN YOU    ",                                      ~
               at (13,02),                                               ~
                  "PRESS (ENTER) YOU WILL BE TAKEN TO A SEARCH SELECTION ~
        ~SCREEN THAT WILL ALLOW   ",                                      ~
               at (14,02),                                               ~
                  "YOU TO EITHER LOCATE A GIVEN TERM, OR CERTAIN TYPES OF~
        ~ TERMS, OR TO ENTER      ",                                      ~
               at (15,02),                                               ~
                  "A NEW TERM.  IF YOU WANT TO CHANGE THE DATA ON FILE FO~
        ~R AN EXISTING TERM       ",                                      ~
               at (16,02),                                               ~
                  "JUST INPUT THAT TERM AFTER ASKING TO 'DEFINE A NEW TER~
        ~M'.  IT'S EASY AND FUN.  ",                                      ~
               at (17,02),                                               ~
                  "SO JUST PRESS (ENTER) NOW AND READ THE SCREENS FOR INS~
        ~TRUCTIONS.               ",                                      ~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(ENTER)TO PROCEED TO MANAGE PERSONNEL COMMON TERMS",  ~
               at (24,02),                                               ~
                  "(12)PRINT DICTIONARY LISTINGS       (13)INSTRUCTIONS",~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(000c0d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L01550
                  call "MANUAL" ("COMINPUT")
                  goto L00920

L01550:        if keyhit% <> 15 then L01590
                  call "PRNTSCRN"
                  goto L00920

L01590:        if keyhit% <> 16 then L01630
                  goto L65000


L01630:        if keyhit% <> 0% then goto L01660
               del% = 1%
               init(" ") pass$
               do% = 0%
               call "COMSUB" (#04, do%, pass$, del%, rslt%)
               rslt% = rslt%
               goto L00920

L01660:        if keyhit% <> 12% then goto L00920


L01685: accept                                                           ~
               at (01,20),                                               ~
        "PRINT DICTIONARY LISTINGS OF COMMON TERMS",                     ~
               at (03,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (04,03),                                               ~
        "! USE THE FUNCTION KEYS SHOWN TO PRINT THE LIST IN THE ORDER DES~
        ~IRED        !",                                                  ~
               at (05,03),                                               ~
        "!  PF-1   IN ORDER BY THE TERMS THEMSELVES                      ~
        ~            !",                                                  ~
               at (06,03),                                               ~
        "!  PF-2   IN ORDER BY THE DESCRIPTION OF THE TERMS              ~
        ~            !",                                                  ~
               at (07,03),                                               ~
        "!  PF-3   IN ORDER BY THE TYPE OR CLASS OF TERM                 ~
        ~            !",                                                  ~
               at (08,03),                                               ~
        "!  PF-16  EXIT WITHOUT PRINTING A LIST       PF-15  PRINT SCREEN~
        ~            !",                                                  ~
               at (09,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
           keys(hex(0102030f10)), key(hk%)

           if hk% = 16% then goto  L00920

           if hk% <> 15% then goto L02445
                     call "PRNTSCRN"
                     goto L01685


L02445:    init(hex(00)) plowkey$

           if hk% <> 1% then goto L02530
           key% = 0%
           call "SHOWMSG" ("PRINTING LIST OF COMMON TERMS IN TERM ORDER")
           goto L02680

L02530:    if hk% <> 2% then goto  L02602
           call "SHOWMSG" ("PRINTING LIST OF COMMON TERMS IN DESCRIPTION ~
        ~ORDER")
           key% = 1%
           goto L02680


L02602:    if hk% <> 3% then goto  L01685
           call "SHOWMSG" ("PRINTING LIST OF COMMON TERMS IN TYPE ORDER")
           key% = 2%


L02680:    select printer(134)
           thru% = 0%
           pge% = 0%
           line% = 1000%

L02720:    call "PLOWALTS" (#04, plowkey$, key%, 0%, f1%)
                     if f1% = 1% then goto L02740
                     close printer
                     goto L01685

L02740:    if line% > 55% then gosub header
           get #4, using L02760 , dtype$, ddescr$, dterm$, text1$, text2$
L02760:              FMT CH(16), CH(30), CH(16), CH(50), CH(50)

           if key% <> 2% then goto L02776
           if dtype$ = oldtype$ then goto  L02776
           if thru% > 0% then  gosub header
           oldtype$ = dtype$
L02776:    line% = line% + 3%
           print using L02830 , dterm$, dtype$, ddescr$, text1$
           print using L02850 , text2$
           print
           thru% = 1%
           goto L02720

L02802: % ----------------  ----------------  ---------------------------~
        ~---  -------------------------------------------------
L02810: % DICTIONARY OF COMMON PERSONNEL TERMS LISTING AS OF ########    ~
        ~                                            PAGE #####
L02820: % TERM              TYPE OR CLASS     DESCRIPTION                ~
        ~     DESCRIPTIVE TEXT ABOUT THE TERM
L02830: % ################  ################  ###########################~
        ~###  ##################################################
L02850: %                                                                ~
        ~     ##################################################

        header
           pge% = pge% + 1%
           print page
           print using L02810, date$, pge%
           print
           print using L02820
           print using L02802
           print
           line% = 5%
           return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            close #4
            end
