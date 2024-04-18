        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   TTTTT  X   X  RRRR   EEEEE  FFFFF   SSS   BBBB    *~
            *  P   P    T     X X   R   R  E      F      S      B   B   *~
            *  PPPP     T      X    RRRR   EEEE   FFFF    SSS   BBBB    *~
            *  P        T     X X   R   R  E      F          S  B   B   *~
            *  P        T    X   X  R   R  EEEEE  F       SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PTXREFSB - Subroutine to retrieve the CMS internal part   *~
            *            and description or the Reference part number   *~
            *            and description based on the parameters        *~
            *            supplied.                                      *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/22/93 ! Original                                 ! MLJ *~
            * 03/03/95 ! Type 2% no longer returns the CMS part   ! JDH *~
            *          !  description since that should already be!     *~
            *          !  in place at the line item level.        !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "PTXREFSB"  (type%,          /* 1% = CMS, 2% = Reference   */~
                         reftype$,       /* Reference Type 'C' or 'M'  */~
                         refcode$,       /* Reference Code             */~
                         refpart$,       /* Reference Part Number      */~
                         refdesc$,       /* Reference Description      */~
                         intpart$,       /* CMS Internal Part Number   */~
                         intdesc$,       /* CMS Part Description       */~
                         #2,             /* HNYMASTR Channel           */~
                         ret%)           /* 1% = Found, 0% = Not Found */

        dim descr$60,                    /* Description for PLOWCODE   */~
            descr_map(6),                /* Description Map for Plowcde*/~
            hdr$(2)79,                   /* Headings for Plowcode      */~
            incl_excl(1),                /* Reference Type Code        */~
            incl_excl$(1),               /* Reference Type Code        */~
            intdesc$32,                  /* CMS Part Description       */~
            intpart$25,                  /* CMS Internal Part Number   */~
            readkey$99,                  /* Misc Read Key              */~
            refcode$9,                   /* Reference Code             */~
            refdesc$30,                  /* Reference Part Description */~
            refpart$25,                  /* Reference Part Number      */~
            reftype$1                    /* Reference Type Code        */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if open, -1 if it      */~
                                         /*   doesn't exist, 0 if not  */~
                                         /*   yet checked              */~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.01 06/23/95 Patch Finalization of R6.04.01  "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! CUSPTXRF ! Customer Part Number Cross Reference Fil *~
            * #02 ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CUSPTXRF",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   26, keylen =  35,                     ~
                        alt key  1, keypos =  1, keylen = 60

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))
*          CALL "OPENCHCK" (#2, FS%(2%), F2%(2%), 0%, RSLT$(2%))

        REM *************************************************************~
            *         M A I N    S U B R O U T I N E    L O G I C       *~
            *************************************************************

        REM Determine what is to be retrieved and act accordingly...
            ret% = 0%
            if type% = 2% then L11000

        REM Return CMS Part, CMS Descr and Ref Descr...
            readkey$ = str(reftype$) & str(refcode$) & str(refpart$)
            if reftype$ = "C" then descr$ = hex(06) & "Customer Part " & ~
                                            "Numbers for " & refcode$    ~
                              else descr$ = hex(06) & "Manufacturer's" & ~
                                            " Part Numbers for "       & ~
                                            refcode$
            f12%     = 0%
            if reftype$ = "C" then str(hdr$(1%),3%) = "Customer's Part "&~
                                                      "Number"           ~
                              else str(hdr$(1%),3%) = "Manufacturer's " &~
                                                      "Part Number"
            str(hdr$(1%),29%) = "Description"
            str(hdr$(1%),60%) = "CMS Part Number"
            descr_map(1%) = 36.250  :  descr_map(2%) = 01.0
            descr_map(3%) = 61.300  :  descr_map(4%) = 27.0
            descr_map(5%) =  1.230  :  descr_map(6%) = 58.0

            call "PLOWCODE" (#1, readkey$, descr$, 9010%, 0.30, f12%,    ~
                             hdr$(), 0, 0, incl_excl(), incl_excl$(),    ~
                             "D", " ",      #2%, descr_map())

            if f12% = 0% then L65000
                refpart$ = str(readkey$,11%,25%)
            get #1 using L10320, intpart$, refdesc$
L10320:         FMT CH(25), POS(61), CH(30)
            gosub cms_description
            ret% = 1%
            goto L65000

L11000: REM Return Ref Part, Ref Descr and CMS Descr...
            readkey$ = str(intpart$) & str(reftype$) & str(refcode$) &   ~
                       str(refpart$)
            call "PLOWALTS" (#1, readkey$, 1%, 35%, f1%(1%))
                if f1%(1%) = 0% then L11080
            get #1 using L11060, refpart$, refdesc$
L11060:         FMT POS(36), CH(25), CH(30)
            if f1%(1%) = 1% then L11100
L11080:         refpart$ = " "
                refdesc$ = "** No Cross Reference **"
                goto L65000
L11100
*           GOSUB CMS_DESCRIPTION     /* Don't change Descr even */
                                      /* if passed in for type 2%. */
             ret% = 1%
             goto L65000

        REM Subroutine to get CMS Part Description
        cms_description
            call "READ100" (#2, str(intpart$), f1%(2%))
                if f1%(2%) = 0% then L12070
            get #2 using L12050, intdesc$
L12050:         FMT POS(26), CH(32)
L12070:     return

L65000: REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

            end
