        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB   RRRR   EEEEE  TTTTT   AAA    GGG           *~
            *    J    B   B  R   R  E        T    A   A  G              *~
            *    J    BBBB   RRRR   EEEE     T    AAAAA  G GGG          *~
            *  J J    B   B  R   R  E        T    A   A  G   G          *~
            *   J     BBBB   R   R  EEEEE    T    A   A   GGG           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBRETAG  - CONVERT PIP TAGS FROM 'OLDTAG' TO 'NEWTAG'     *~
            *            CAN BE USED BY RESCHEDULER OR CAN REPLACE DATA *~
            *            SAVE IN JBRELSUB                               *~
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
            * 09/26/85 ! ORIGINAL                                 ! KEN *~
            * 01/26/86 ! FIXED PIPCROSS FMT ON THIRD KEY          ! KEN *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "JBRETAG" (                                                  ~
                     oldtag$,            /* TAG CURRENTLY ON FILE      */~
                     newtag$,            /* TAG IT WILL BECOME         */~
                     #8,                 /* JBCROSS2                   */~
                     #2,                 /* PIPMASTR                   */~
                     #23,                /* WCOUT                      */~
                     #33,                /* PIPIN                      */~
                     #34,                /* PIPOUT                     */~
                     #35,                /* PIPCROSS                   */~
                     #36)                /* JBPIPXRF                   */

        dim newtag$19, oldtag$19, readkey$200

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.17.01 11/20/86 Order process & planning #2     "
        REM *************************************************************
            if oldtag$ = " " then end
            if newtag$ = " " then end

        REM FIRST DEAL WITH THE PIPOUTS

           init(hex(00)) readkey$
           str(readkey$,1,19) = str(oldtag$,1,19)

L02050:    call "PLOWNXT1" (#34, readkey$, 19%, f1%)
              if f1% = 0% then L03000
           delete #34
           put #34, using L02090, newtag$
L02090:        FMT POS(1), CH(19)
           write #34, eod goto L02050
           goto L02050

L03000: REM NOW CHANGE THE WC USEAGE

           init(hex(00)) readkey$
           str(readkey$,1,19) = str(oldtag$,1,19)

L03050:    call "PLOWNXT1" (#23, readkey$,  19%, f1%)
              if f1% = 0% then L04000
           delete #23
           put #23, using L03090, newtag$
L03090:        FMT POS(9), CH(19)
           write #23, eod goto L03050
           goto L03050

L04000: REM JBPIPXRF

           call "REDALT1" (#36, oldtag$, 1%, f1%)
              if f1%=0 then L04100
           delete #36
           put #36, using L04060, newtag$
L04060:       FMT POS(45), CH(19)
           write #36, eod goto L04100

L04100:    init(hex(00)) readkey$
           str(readkey$,1,19) = str(oldtag$,1,19)

L04130:    call "PLOWNXT1" (#36, readkey$, 19%, f1%)
              if f1% = 0% then L05000
           delete #36
           put #36, using L04170, newtag$
L04170:       FMT POS(1), CH(19)
           write #36, eod goto L04130
           goto L04130

L05000: REM JBCROSS2

           call "READ101" (#8, oldtag$, f1%)
              if f1% = 0% then L06000
           delete #8
           put #8, using L05070, newtag$
           put #8, using L05080, newtag$
L05070:       FMT POS(29), CH(19)
L05080:       FMT POS(76), CH(19)
           write #8, eod goto L06000

L06000: REM PIPCROSS

           init (hex(00)) readkey$
           str(readkey$,1,19) = str(oldtag$,1,19)

L06050:    call "PLOWAL1" (#35, readkey$, 1%, 19%, f1%)
              if f1% = 0 then L06130
           delete #35
           put #35, using L06090, newtag$
L06090:        FMT POS(20), CH(19)
           write #35, eod goto L06050
           goto L06050

L06130:    init (hex(00)) readkey$
           str(readkey$,1,19) = str(oldtag$,1,19)

L06160:    call "PLOWAL1" (#35, readkey$, 2%, 19%, f1%)
              if f1% = 0 then L07000
           delete #35
           put #35, using L06200, newtag$
L06200:         FMT POS(39), CH(19)
           write #35
           goto L06160

L07000: REM NOW FOR THE PIPINS

           call "READ101" (#33, oldtag$, f1%)
              if f1% = 0% then L65000
           delete #33
           put #33, using L07060, newtag$
L07060:        FMT POS(30), CH(19)
           write #33, eod goto L65000

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
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
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
