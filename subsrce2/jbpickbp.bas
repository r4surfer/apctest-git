        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB   PPPP   IIIII   CCC   K   K  BBBB   PPPP    *~
            *    J    B   B  P   P    I    C   C  K  K   B   B  P   P   *~
            *    J    BBBB   PPPP     I    C      KKK    BBBB   PPPP    *~
            *  J J    B   B  P        I    C   C  K  K   B   B  P       *~
            *   J     BBBB   P      IIIII   CCC   K   K  BBBB   P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBPICKBP - This is a subroutine to print byproducts list  *~
            *       for  range of Job Numbers.  Its a subroutine so that*~
            *            it can be called from the release or as a      *~
            *            stand alone for reprints without a lot of      *~
            *            hassles.                                       *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/04/86 ! ORIGINAL(cloned from JBPICKSL)           ! HDC *~
            * 03/15/87 ! Removed some excess code                 ! HDC *~
            * 03/16/87 ! Removed obsolete subroutines.            ! ERN *~
            * 06/10/87 ! Minor mod to HNYMASTR (PARTTYPE$).       ! JIM *~
            * 02/15/89 ! Added Job description & Control Number   ! MJB *~
            *          !  to header & reformatted that header!    !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "JBPICKBP" (firstjob$,       /* FIRST JOB TO PRINT         */~
                         lastjob$,       /* LAST JOB TO PRINT          */~
                               #1,       /* JBMASTR2 CHANNEL #         */~
                               #2,       /* PIPOUT CHANNEL #           */~
                               #3,       /* WCOUT CHANNEL #            */~
                               #4,       /* HNYMASTR CHANNEL #         */~
                               #5,       /* HNYQUAN CHANNEL #          */~
                               #6,       /* SYSFILE2 CHANNEL #         */~
                            exline%,     /* 0-2 YEILDS 2-4 LINES OF    */~
                                         /* 'FILL IN THE BLANKS' ON    */~
                                         /*  WORKSHEET.  IE PRINTS AT  */~
                                         /*  LEAST 2 IN ANY CASE       */~
                            calstart$,   /*  FIRST DAY IN PROD CAL     */~
                            cutoff%)     /*  END DATE FOR PICK (OFFSET)*/



        dim calstart$6,                  /* FIRST DAY IN PLANNING CAL  */~
            ctlnbr$19,                   /* Shop Floor Control Number  */~
            currentjob$8,                /* JOB BEING PROCESSED        */~
            date$8,                      /* TODAYS DATE                */~
            firstjob$8,                  /* FIRST JOB TO PRINT FOR     */~
            hddate$45,                   /* Header Date                */~
            header$80,                   /* Print header               */~
            jbpart$25,                   /* PART JOB IS BUILDING       */~
            jbpartdescr$32,              /* PART DESCRIPTION           */~
            jbquant$10,                  /* QUANTITY TO BUILD          */~
            jbworkcenter$4,              /* FIRST WC FOR JOB           */~
            jobdescr$30,                 /* Job Description            */~
            lastjob$8,                   /* LAST JOB TO PRINT FOR      */~
            part$25,                     /* COMPONENT PART             */~
            partdescr$32,                /* PART DESCRIPTION           */~
            parttype$3,                  /* HNYMASTR Part type         */~
            pickby$8,                    /* PICKING CUTOFF DATE        */~
            pipoutplow$56,               /* WORK VARIABLE              */~
            quantity$10,                 /* QUANTITY OF COMP NEEDED    */~
            readkey$50,                  /* WORK VARIABLE              */~
            stdate$8,                    /*                            */~
            tagnr$19,                    /*                            */~
            temp$6,                      /* WORK VARIABLE              */~
            unit$4,                      /* UNIT of STOCKING           */~
            userid$3,                    /* User ID                    */~
            wcoutplow$35                 /* WORK VARIABLE              */

        dim f1%(6)                       /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R5.01.07 09/07/89 Patch Release R5.01.07          "
        REM *************************************************************
            if firstjob$>lastjob$ then end         /*IDIOTS            */
            call "DATE" addr("HD", hddate$)
            call "SPCESMSH" (hddate$, 2%)
            call "STRING" addr("CT", hddate$, 45%)
            date$=date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)

        REM NOW DO IT
        REM FIND TRICK FOR FIRSTJOB
           currentjob$=firstjob$
           currentjob$=currentjob$ addc all(hex(ff))
           printer%=0

L10030:    call "PLOWNEXT" (#1,currentjob$,0%,f1%(1))
           if f1%(1)=0 then L65000
                get #1, using L10055, currentjob$, jobdescr$, tagnr$,     ~
                                     jbpart$, qty, stdate$, ctlnbr$
L10055:         FMT CH(8), CH(30), CH(19), CH(25), PD(14,4),             ~
                           XX(77), CH(6), POS(1120), CH(19)
           if currentjob$ > lastjob$ then L65000

           call "CONVERT" (qty, 0.2, jbquant$)
           call "GETCODE" (#4, jbpart$, jbpartdescr$, 0%, 99, f1%(4))
           call "DATEFMT" (stdate$)
           page%, printed%=0
           line%=99
           if jobdescr$ = jbpartdescr$ then jobdescr$ = " "
           header$ = "JOB: " & currentjob$
           if jobdescr$ = " " then L10102
                header$ = header$ & " (" & jobdescr$ & ") "
L10102:    if ctlnbr$ = " " then L10104
                header$ = header$ & "Ctl # " & ctlnbr$
L10104:    call "STRING" addr("CT", header$, 80%)
           jbworkcenter$=" "
           workcenter%=999
           init (hex(00)) wcoutplow$
           str(wcoutplow$,,19)=tagnr$
L10125:    call "PLOWNEXT" (#3, wcoutplow$, 19%, f1%(3))
           if f1%(3)=0 then L10175
           get #3, using L10140, day%
L10140:        FMT XX(23), BI(4)
           if day%>=workcenter% then L10125
           workcenter%=day%
           get #3, using L10160, jbworkcenter$
L10160:        FMT XX(19), CH(4)
           goto L10125
           printer%=0

L10175:    init (hex(00)) pipoutplow$
           str(pipoutplow$,,19)=tagnr$
L10185:    call "PLOWNEXT" (#2, pipoutplow$, 19%, f1%(2))
                if f1%(2)=0 then L10470   /* END THIS ONE GRACEFULLY    */
           v% = val(str(pipoutplow$,45,4),4) - 1
           if v% + 1 > cutoff% then L10185        /* BEYOND CUTOFF DATE */
           call "DATE" addr ("G+", calstart$, v%, temp$, err%)
           pickby$ = temp$
           call "DATEFMT" (pickby$)
           if err% <> 0 then pickby$ = " "
           get #2, using L10230, part$, qty
L10230:        FMT XX(19), CH(25), XX(12), PD(14,4)
           if qty >=  0 then L10185
           if qty < 0 then qty = qty * (-1)
           partdescr$ = "  ** NOT ON FILE **"
           call "READ100" (#4, part$, f1%(4))
                if f1%(4) = 0 then L10335
           get #4, using L10305, partdescr$, unit$, parttype$
L10305:         FMT XX(25), CH(32), XX(20),CH(4), POS(180), CH(3)
            convert parttype$ to type%, data go to L10335
            if type% > 489% and type% < 500% then L10330
            if type% > 789% and type% < 800% then L10330
            goto L10335
L10330:     partdescr$=str(partdescr$,,18) & ":TOOL NO ISSUE"
L10335:    printer%,printed%=1
           if line%>=55 then  gosub L11000       /* HEAD'EM UP          */
           call "CONVERT" (qty, 0.2, quantity$)
           readkey$ = part$
           print using L12172,part$,pickby$,quantity$,unit$
           print using L12180, partdescr$
           line%=line%+2

           for i%=1 to exline%
           print using L12210
           print skip(1)
           line%=line%+2
           next i%

           print skip(1)
           line%=line%+1
           goto L10185

L10470:    if printed% = 0 then L10030
           print using L12235
           goto L10030


L11000: REM HEADER
           select printer (134)
           print page
           page%=page%+1%
           line%=10%
           print using L12010, userid$, page%
           print using L12045, hddate$
           print using L12050, header$
           print skip(1)
           print using L12070, jbpart$, jbpartdescr$
           print using L12090, jbquant$, jbworkcenter$, stdate$
           print skip(1)
           print using L12122
           print using L12130
           print skip(1)
           return

        REM HERE COME THE IMAGES
L12010: %Run By: ###          * * * * * JOB BY-PRODUCTS LIST * * * * *   ~
        ~            PAGE: ###

L12045: %                  #############################################

L12050: %################################################################~
        ~################

L12070: % PART NO #########################   ###########################~
        ~#####        START ON

L12090: % QUANTITY TO BUILD  ##########       INITIAL WORK CENTER  ####  ~
        ~             ########

L12122: %PART NUMBER/DESCRIPTION      DATE   PLANNED QTY UNIT  ACTUAL QTY~
        ~   BIN    WHSE: LOT

L12130: %========================== ======== ========== =====  ==========~
        ~ =======  ===========


L12172: %#########################  ######## ##########  ####  __________~
        ~  _____   ____:______
L12180: %    ################################
L12210: %    ################################                  __________~
        ~  _____   ____:______
L12235: %                      * * * END OF BY-PRODUCTS LIST * * *

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
