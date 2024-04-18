        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   IIIII  PPPP   RRRR   V   V  W   W   SSS   BBBB    *~
            *  P   P    I    P   P  R   R  V   V  W   W  S      B   B   *~
            *  PPPP     I    PPPP   RRRR   V   V  W W W   SSS   BBBB    *~
            *  P        I    P      R   R   V V   WW WW      S  B   B   *~
            *  P      IIIII  P      R   R    V    W   W   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPRVWSB - PIP REVIEW FUNCTIONS MENU AND DRIVER FOR       *~
            *            PLNRSUB,  PIPDSUB,  PIPTSUB                    *~
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
            * 06/03/88 ! ORIGINAL                                 ! RJM *~
            * 11/04/88 ! Pass HNYALTRS to PIPDSUB, PLNRSUB        ! KAB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "PIPRVWSB" (                                                 ~
                       part$,            /* PART TO BE REVIEWED        */~
                       keyhit%,          /* KEYHIT OUT                 */~
                       #1,               /* DEMMASTR                   */~
                       #2,               /* PIPMASTR                   */~
                       #4,               /* HNYMASTR                   */~
                       #5,               /* HNYDETAL                   */~
                       #6,               /* RTEALTRS                   */~
                       #7,               /* RTEMASTR                   */~
                       #9,               /* JBMASTR2                   */~
                       #11,              /* WCMASTR                    */~
                       #12,              /* CALMASTR                   */~
                       #15,              /* BOMMASTR                   */~
                       #23,              /* WCOUT                      */~
                       #24,              /* ENGMASTR                   */~
                       #33,              /* PIPIN                      */~
                       #34,              /* PIPOUT                     */~
                       #35,              /* PIPCROSS                   */~
                       #40,              /* SFMASTR2                   */~
                       #41,              /* SFCUM2                     */~
                       #42,              /* PORLSE                     */~
                       #43,              /* VBKMASTR                   */~
                       #60,              /* SYSFILE2                   */~
                       #16)              /* HNYALTRS                   */
        dim                                                              ~
            part$25,                                                     ~
            reviewpart$25

        REM *************************************************************~
            *                                                           *~
            *     LINK TO PLANNING REVIEW FUNCTIONS                     *~
            *                                                           *~
            *************************************************************

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L62062
            cms2v$ = "R5.01.03 11/15/88 Patch Release                   "
L62062: REM *************************************************************
        review_functions

            gosub L63000
                if keyhit% =  1% then L65000
                if keyhit% =  2% then gosub full_review
                if keyhit% =  3% then gosub piptrace
                if keyhit% =  4% then gosub pipdisp
                if keyhit% = 16% then L65000
                if keyhit% = 32% then L65000
            goto review_functions

L63000: REM *************************************************************~
            *                                                           *~
            *  SELECT REVIEW FUNCTION OR YOUR CHOICE                    *~
            *                                                           *~
            *************************************************************

        accept                                                           ~
               at (01,03),                                               ~
        "SELECT PLANNING REVIEW FUNCTION",                               ~
               at (04,03),                                               ~
        "Select One -",                                                  ~
               at (06,10),                                               ~
        "PF1) Start Over",                                               ~
               at (07,10),                                               ~
        "PF2) Full Planning System Review Functions",                    ~
               at (08,10),                                               ~
        "PF3) Trace PIP Activities",                                     ~
               at (09,10),                                               ~
        "PF4) Display PIP Files",                                        ~
               at (18,10),                                               ~
        "PF13) Instructions",                                            ~
               at (20,10),                                               ~
        "PF15) Print Screen",                                            ~
               at (21,10),                                               ~
        "PF16) RETURN ",                                                 ~
               at (22,10),                                               ~
        "PF32) EXIT PROGRAM IMMEDIATELY",                                ~
                                                                         ~
               keys(hex(010203040d0f1020)),                              ~
               key (keyhit%)

               if keyhit% <> 13% then L63350
                  call "MANUAL" ("PIPSCAN ")
                  goto L63000

L63350:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L63000

        REM *************************************************************~
            *                                                           *~
            *     LINK TO PIP DISPLAY ROUTINE                           *~
            *                                                           *~
            *************************************************************

        pipdisp

            call "PIPDSUB" (#60, #33, #34, #2, #4, #1, #5, #7, #11, #12, ~
                  #15, #23, #24, #35, #40, #41, #9, #42, #43, #6, #16)
            return

        REM *************************************************************~
            *                                                           *~
            *     LINK TO PIP TRACING ROUTINE                           *~
            *                                                           *~
            *************************************************************

        piptrace

            reviewpart$=part$
            call "PIPTSUB" (reviewpart$, #60, #33, #34, #2, #4, #9, #1)
            return

        REM *************************************************************~
            *                                                           *~
            *     LINK TO PLANNING REVIEW FUNCTIONS                     *~
            *                                                           *~
            *************************************************************

        full_review

             reviewpart$=part$:mode%=0%
             REM
             call "PLNRSUB" (mode%, reviewpart$,                         ~
                             #1, #2, #4, #7, #11, #12, #15, #23, #24,    ~
                             #33, #34, #35, #40, #41, #5, #6, #16)

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

            end
