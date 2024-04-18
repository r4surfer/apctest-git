        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  BBBB    OOO   M   M  X   X  PPPP   L       OOO   DDDD    *~
            *  B   B  O   O  MM MM   X X   P   P  L      O   O  D   D   *~
            *  BBBB   O   O  M M M    X    PPPP   L      O   O  D   D   *~
            *  B   B  O   O  M   M   X X   P      L      O   O  D   D   *~
            *  BBBB    OOO   M   M  X   X  P      LLLLL   OOO   DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMXPLOD - BOM Explosion Subroutine.                      *~
            *            Allows user to walk down a Bill selecting      *~
            *            interactively what BOM ID's and components     *~
            *            to explode (really nothing more than a driver  *~
            *            for the infamous PLOWCODE).  Currently allows  *~
            *            user to walk down 100 levels before limit      *~
            *            reached (add more by expanding array size).    *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/02/86 ! Original                                 ! LDJ *~
            * 09/24/86 ! BOMMASTR FORMAT CHANGED                  ! LKM *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


        sub "BOMXPLOD" (assy$,           /* Part Assembly to Explode   */~
                        bom$,            /* BOM ID of Above            */~
                        #1,              /* HNYMASTR File Channel      */~
                        #2)              /* BOMMASTR File Channel      */~
                                         /* The 1st 2 arguments may be */~
                                         /* passed in blank but if ASSY*/~
                                         /* is blank BOM should be also*/

        dim                                                              ~
            assembly$(100)28,            /* This is where you've been  */~
            assy$25,                     /* Assembly Part Code         */~
            bom$3,                       /* Assembly BOM ID            */~
            bomid$3,                     /* BOM ID                     */~
            descr_map(10),               /* PLOWCODE Argument          */~
            dummy(1),                    /* PLOWCODE Argument          */~
            dummy$(1)99,                 /* PLOWCODE Argument          */~
            hdr$(3)80,                   /* PLOWCODE Argument          */~
            part$25,                     /* Assembly Part Code         */~
            partdescr$79,                /* Assembly Part Code         */~
            plowkey$99                   /* Miscellaneous Read/Plow Key*/~

        dim f1%(02)                      /* Record-on-file Flags       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.17.01 11/20/86 Order process & planning #2     "
        REM *************************************************************

L10000:         l% = 1%
                plowkey$ = assy$
                hdr$(2) = "  Part Assemblies             Part Description~
        ~s"
                hdr$(1) = "  BOM ID's For Selected Part"
                hdr$(3) = hex(ac) & "Select the Starting Assy to Explode ~
        ~using Cursor & RETURN or PF16 to Exit  "
                partdescr$ = hex(06) & "Select the Part Assembly to "    ~
                           & "Explode"
                dummy(1) = 0
                REM *** Get Part & BOMID To Explode ***
                call "PLOWCODE" (#2, plowkey$, partdescr$,-8025%, -.32,  ~
                     f1%(2), hdr$(), 0, 0, dummy(),dummy$(),"Y","Y",#1)
                if f1%(2) = 0% then end
                part$ = plowkey$
                str(plowkey$,26%) = bom$
                hdr$(1) = "  BOM ID's For Selected Starting Assembly"
                if assy$ > " " then                                      ~
                hdr$(3) = hex(ac) & "Select the Rev of the Assy to Explod~
        ~e using Cursor & RETURN or PF16 to Return"                       ~
                else                                                     ~
                hdr$(3) = hex(ac) & "Select the Rev of the Assy to Explod~
        ~e using Cursor & RETURN or PF16 to ReSelect"
                partdescr$ = hex(06) & "Select which BOM Rev to use to " ~
                           & "explode Part: " & part$
                dummy(1) = 0
                call "PLOWCODE" (#2, plowkey$, partdescr$, 7025%,  .30,  ~
                     f1%(2), hdr$(), 3, 0, dummy(),dummy$(),"Y","Y")
                if f1%(2) = 0% and assy$ > " " then end
                if f1%(2) = 0% then L10000
                bomid$ = str(plowkey$,26%,3%)

        REM *************************************************************~
            *            Start Explosion / Implosion Loop               *~
            *************************************************************
L10640:         assembly$(l%) = str(part$) & bomid$

L10680:         REM *** ReEntry Point for Backing Up ***
                part$ = assembly$(l%)
                bomid$ = str(assembly$(l%),26%)
                descr_map(1) =-180.03 : descr_map(2) = 61
                descr_map(3) =   1.25 : descr_map(4) = 1
                descr_map(5) =  57.08 : descr_map(6) = 27.104
                descr_map(7) =  65.08 : descr_map(8) = 38.104
                descr_map(9) =  89.02 : descr_map(10) = 52
                dummy(1) = -54.03
                dummy$(1) = "  0"
                hdr$(1) = "  Seq #      Component Part Code       Qty Req~
        ~'d  Times Used  Marker  Part Type"
                hdr$(3) = hex(ac) & "Select the Component Assy to Explode~
        ~ using Cursor & RETURN or PF16 to BackUp"
                plowkey$ = str(part$) & bomid$
                partdescr$=hex(06) & "Below are components of: " & part$ ~
                 & ", BOM: " & bomid$ & ".  (Level=  )"
                convert l% to str(partdescr$,len(partdescr$)-2,2),pic(##)
                call "PLOWCODE" (#2, plowkey$, partdescr$, 9028%, .65,   ~
                      f1%(2), hdr$(),0,-1,dummy(),dummy$(),"Y","Y",#1,   ~
                      descr_map())
                if f1%(2) = 1% then L11160
                   l% = l%-1%
                   if l% > 0% then L10680
                   if assy$ > " " and bom$ > " " then end
                   goto L10000

L11160:         REM *** Select BOM ID ONLY for Selected Sub-Assembly ***
                part$ = key(#2,1)
                plowkey$ = part$
                hdr$(1) = "  BOM ID's For Selected Part"
                hdr$(3) = hex(ac) & "Select the Rev of the Assy to Explod~
        ~e using Cursor & RETURN or PF16 to BackUp"
                partdescr$ = hex(06) & "Select which BOM Rev to use to " ~
                           & "explode Part: " & part$
                dummy(1) = 0
                call "PLOWCODE" (#2, plowkey$, partdescr$, 7025%,  .30,  ~
                     f1%(2), hdr$(), 3, 0, dummy(),dummy$(),"Y","Y")
                if f1%(2) = 0% then L10680
                bomid$ = str(plowkey$,26%,3%)
                l% = l% + 1%
                goto L10640
