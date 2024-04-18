        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   IIIII  DDDD    AAA   TTTTT  EEEEE  DDDD    *~
            *  A   A  R   R    I    D   D  A   A    T    E      D   D   *~
            *  AAAAA  RRRR     I    D   D  AAAAA    T    EEEE   D   D   *~
            *  A   A  R   R    I    D   D  A   A    T    E      D   D   *~
            *  A   A  R   R  IIIII  DDDD   A   A    T    EEEEE  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARIDATED - Manages Payment Schedule for invoices with     *~
            *            'Dated' Terms.                                 *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/28/86 ! Original                                 ! ERN *~
            * 07/10/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ARIDATED"   (hdr1$, hdr2$,  /* 1st and 2nd Screen Lines   */~
                          netinv,        /* Net Invoice Amount         */~
                          invdate$,      /* Invoice Date               */~
                          amts(),        /* Payment Amounts            */~
                          pcts(),        /* Discount Percents          */~
                          disc$(),       /* Discount Due Dates         */~
                          net$(),        /* Net Due Dates              */~
                          ret%)          /*  1 = Start Over Taken      */
                                         /*  2 = Lines  9 = Header     */
                                         /* 16 = Continue with save    */

        dim                                                              ~
            amts(30),                    /* Payment Amounts            */~
            blankdate$8,                 /* Blank date for comparison  */~
            date$8,                      /* Invoice Date- unformatted  */~
            disc$(30)6,                  /* Discount Due Dates         */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hdr$(5)10,                   /* Column Headings            */~
            hdr1$79, hdr2$79,            /* 1st and 2nd Screen Lines   */~
            invdate$8,                   /* Invoice Date- formatted    */~
            io$(30)41,                   /* Display/Input String       */~
            lfac$1,                      /* FAC                        */~
            inpmessage$79,               /* Informational Message      */~
            net$(30)6,                   /* Net Due Dates              */~
            pcts(30),                    /* Discount Percents          */~
            pf$(3)79, pfkey$32           /* PF Prompts and Keys        */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = invdate$ : call "DATEOK" (date$, invdate%, errormsg$)

            edtmessage$  = "Press (RETURN) to modify Payment Schedule -"&~
                           "or- PF-16 to exit screen.          "

            gosub format_io

            hdr$(1) = "##"
            hdr$(2) = "    Amount"
            hdr$(3) = "Disc%"
            hdr$(4) = "Disc Due"
            hdr$(5) = " Net Due"


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        if str(io$()) = " " then L11180

        editpg1
            inpmessage$ = edtmessage$
            gosub set_facs_display
            gosub'101(1%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then gosub datasave
                  if keyhit%  =  9 then gosub datasave
                  if keyhit%  = 16 then gosub datasave
                  if keyhit% <>  0 then       editpg1
            gosub'051                   /* Check Enables, Set Defaults */
L11180:     gosub set_facs_edit
L11190:     gosub'101(2%)               /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11190
            gosub'151                   /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
            goto editpg1


        set_facs_display
            lfac$ = hex(84)
            gosub set_facs
            return

        set_facs_edit
            for i% = 1% to 30%
                str(io$(i%), 7,1) = hex(82)
                str(io$(i%),18,1) = hex(82)
                str(io$(i%),24,1) = hex(81)
                str(io$(i%),33,1) = hex(81)
            next i%
            return

        set_facs_8c
            lfac$ = hex(8c)
            gosub set_facs
            return

        set_facs
            for i% = 1% to 30%
                str(io$(i%), 7,1), str(io$(i%),18,1),                    ~
                str(io$(i%),24,1), str(io$(i%),33,1) = lfac$
            next i%
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051
            inpmessage$ = "Modify data and then press (RETURN)"
            return


        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                return clear all
                ret% = 1%
                goto exit_program

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Puts Variables passed in from program into IO array.      *~
            *************************************************************
        format_io
            init (" ") io$()
            amtleft   = netinv

            for i% = 1% to 30%
                if amts(i%) = 0 then L30190
                     amtleft = amtleft - amts(i%)
                     str(io$(i%), 1, 6) = net$(i%)
                     call "CONVERT" (amts(i%), 2.2, str(io$(i%), 8,10))
                     call "CONVERT" (pcts(i%), 2.2, str(io$(i%),19, 5))
                     str(io$(i%),25, 8) = disc$(i%)
                     call "DATEFMT" (str(io$(i%),25, 8))
                     str(io$(i%),34, 8) = net$(i%)
                     call "DATEFMT" (str(io$(i%),34, 8))
L30190:     next i%

            return


*        IO$ Elements are formatted as follows:
*
*
*          nnnnnnfAAAAAAAAAAfdddddfDDDDDDDDfNNNNNNNN
*
*           1, 6   n = Net Due Date- Unformatted for sort
*           7, 1   f = FAC for Amount
*           8,10   A = Amount
*          18, 1   f = FAC for Discount Percent
*          19, 5   d = Discount Percent
*          24, 1   f = FAC for Discount Due Date (formatted)
*          25, 8   D = Discount Due Date
*          33, 1   f = FAC for Net Due Date
*          34, 8   N = Net Due Date (formatted)


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Move data from IO array back to caller variables.         *~
            *************************************************************
        datasave
            if amtleft = 0 then L31110
                errormsg$ = "You must distribute all of the Net"   &     ~
                            " Invoice Amount"
                return

L31110:     init (" ") disc$(), net$()
            mat amts = zer
            mat pcts = zer
            ret%     = keyhit%
            return clear all

            for i% = 1% to 30%
                if str(io$(i%),8,10) = " " then exit_program
                     convert str(io$(i%), 8,10) to amts(i%)
                     convert str(io$(i%),19, 5) to pcts(i%)
                     call "DATUNFMT" (str(io$(i%),25,8))
                     disc$(i%) = str(io$(i%),25,6)
                     call "DATUNFMT" (str(io$(i%),34,8))
                     net$ (i%) = str(io$(i%),34,6)
            next i%

            goto exit_program

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(edit%)
            gosub set_pf

L40090:     accept                                                       ~
               at (01,02), fac(hex(8c)), hdr1$                  , ch(79),~
               at (02,02), fac(hex(ac)), hdr2$                  , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), "Net Invoice Amount:",                        ~
               at (04,22), fac(hex(84)), netinv,     pic(-##,###,###.00),~
               at (04,40), "Amount Undistributed: ",                     ~
               at (04,62), fac(hex(84)), amtleft,    pic(-##,###,###.00),~
                                                                         ~
               at (05,02), fac(hex(ac)), hdr$(1)                , ch(02),~
               at (05,06), fac(hex(ac)), hdr$(2)                , ch(10),~
               at (05,17), fac(hex(ac)), hdr$(3)                , ch(05),~
               at (05,23), fac(hex(ac)), hdr$(4)                , ch(08),~
               at (05,32), fac(hex(ac)), hdr$(5)                , ch(08),~
               at (05,42), fac(hex(ac)), hdr$(1)                , ch(02),~
               at (05,46), fac(hex(ac)), hdr$(2)                , ch(10),~
               at (05,57), fac(hex(ac)), hdr$(3)                , ch(05),~
               at (05,63), fac(hex(ac)), hdr$(4)                , ch(08),~
               at (05,72), fac(hex(ac)), hdr$(5)                , ch(08),~
                                                                         ~
               at (06,02), " 1",         at (06,42), "16",               ~
               at (07,02), " 2",         at (07,42), "17",               ~
               at (08,02), " 3",         at (08,42), "18",               ~
               at (09,02), " 4",         at (09,42), "19",               ~
               at (10,02), " 5",         at (10,42), "20",               ~
               at (11,02), " 6",         at (11,42), "21",               ~
               at (12,02), " 7",         at (12,42), "22",               ~
               at (13,02), " 8",         at (13,42), "23",               ~
               at (14,02), " 9",         at (14,42), "24",               ~
               at (15,02), "10",         at (15,42), "25",               ~
               at (16,02), "11",         at (16,42), "26",               ~
               at (17,02), "12",         at (17,42), "27",               ~
               at (18,02), "13",         at (18,42), "28",               ~
               at (19,02), "14",         at (19,42), "29",               ~
               at (20,02), "15",         at (20,42), "30",               ~
                                                                         ~
               at (06,06), fac(str(io$( 1), 7,1)), str(io$( 1), 8, 10),  ~
               at (06,17), fac(str(io$( 1),18,1)), str(io$( 1),19,  5),  ~
               at (06,23), fac(str(io$( 1),24,1)), str(io$( 1),25,  8),  ~
               at (06,32), fac(str(io$( 1),33,1)), str(io$( 1),34,  8),  ~
                                                                         ~
               at (07,06), fac(str(io$( 2), 7,1)), str(io$( 2), 8, 10),  ~
               at (07,17), fac(str(io$( 2),18,1)), str(io$( 2),19,  5),  ~
               at (07,23), fac(str(io$( 2),24,1)), str(io$( 2),25,  8),  ~
               at (07,32), fac(str(io$( 2),33,1)), str(io$( 2),34,  8),  ~
                                                                         ~
               at (08,06), fac(str(io$( 3), 7,1)), str(io$( 3), 8, 10),  ~
               at (08,17), fac(str(io$( 3),18,1)), str(io$( 3),19,  5),  ~
               at (08,23), fac(str(io$( 3),24,1)), str(io$( 3),25,  8),  ~
               at (08,32), fac(str(io$( 3),33,1)), str(io$( 3),34,  8),  ~
                                                                         ~
               at (09,06), fac(str(io$( 4), 7,1)), str(io$( 4), 8, 10),  ~
               at (09,17), fac(str(io$( 4),18,1)), str(io$( 4),19,  5),  ~
               at (09,23), fac(str(io$( 4),24,1)), str(io$( 4),25,  8),  ~
               at (09,32), fac(str(io$( 4),33,1)), str(io$( 4),34,  8),  ~
                                                                         ~
               at (10,06), fac(str(io$( 5), 7,1)), str(io$( 5), 8, 10),  ~
               at (10,17), fac(str(io$( 5),18,1)), str(io$( 5),19,  5),  ~
               at (10,23), fac(str(io$( 5),24,1)), str(io$( 5),25,  8),  ~
               at (10,32), fac(str(io$( 5),33,1)), str(io$( 5),34,  8),  ~
                                                                         ~
               at (11,06), fac(str(io$( 6), 7,1)), str(io$( 6), 8, 10),  ~
               at (11,17), fac(str(io$( 6),18,1)), str(io$( 6),19,  5),  ~
               at (11,23), fac(str(io$( 6),24,1)), str(io$( 6),25,  8),  ~
               at (11,32), fac(str(io$( 6),33,1)), str(io$( 6),34,  8),  ~
                                                                         ~
               at (12,06), fac(str(io$( 7), 7,1)), str(io$( 7), 8, 10),  ~
               at (12,17), fac(str(io$( 7),18,1)), str(io$( 7),19,  5),  ~
               at (12,23), fac(str(io$( 7),24,1)), str(io$( 7),25,  8),  ~
               at (12,32), fac(str(io$( 7),33,1)), str(io$( 7),34,  8),  ~
                                                                         ~
               at (13,06), fac(str(io$( 8), 7,1)), str(io$( 8), 8, 10),  ~
               at (13,17), fac(str(io$( 8),18,1)), str(io$( 8),19,  5),  ~
               at (13,23), fac(str(io$( 8),24,1)), str(io$( 8),25,  8),  ~
               at (13,32), fac(str(io$( 8),33,1)), str(io$( 8),34,  8),  ~
                                                                         ~
               at (14,06), fac(str(io$( 9), 7,1)), str(io$( 9), 8, 10),  ~
               at (14,17), fac(str(io$( 9),18,1)), str(io$( 9),19,  5),  ~
               at (14,23), fac(str(io$( 9),24,1)), str(io$( 9),25,  8),  ~
               at (14,32), fac(str(io$( 9),33,1)), str(io$( 9),34,  8),  ~
                                                                         ~
               at (15,06), fac(str(io$(10), 7,1)), str(io$(10), 8, 10),  ~
               at (15,17), fac(str(io$(10),18,1)), str(io$(10),19,  5),  ~
               at (15,23), fac(str(io$(10),24,1)), str(io$(10),25,  8),  ~
               at (15,32), fac(str(io$(10),33,1)), str(io$(10),34,  8),  ~
                                                                         ~
               at (16,06), fac(str(io$(11), 7,1)), str(io$(11), 8, 10),  ~
               at (16,17), fac(str(io$(11),18,1)), str(io$(11),19,  5),  ~
               at (16,23), fac(str(io$(11),24,1)), str(io$(11),25,  8),  ~
               at (16,32), fac(str(io$(11),33,1)), str(io$(11),34,  8),  ~
                                                                         ~
               at (17,06), fac(str(io$(12), 7,1)), str(io$(12), 8, 10),  ~
               at (17,17), fac(str(io$(12),18,1)), str(io$(12),19,  5),  ~
               at (17,23), fac(str(io$(12),24,1)), str(io$(12),25,  8),  ~
               at (17,32), fac(str(io$(12),33,1)), str(io$(12),34,  8),  ~
                                                                         ~
               at (18,06), fac(str(io$(13), 7,1)), str(io$(13), 8, 10),  ~
               at (18,17), fac(str(io$(13),18,1)), str(io$(13),19,  5),  ~
               at (18,23), fac(str(io$(13),24,1)), str(io$(13),25,  8),  ~
               at (18,32), fac(str(io$(13),33,1)), str(io$(13),34,  8),  ~
                                                                         ~
               at (19,06), fac(str(io$(14), 7,1)), str(io$(14), 8, 10),  ~
               at (19,17), fac(str(io$(14),18,1)), str(io$(14),19,  5),  ~
               at (19,23), fac(str(io$(14),24,1)), str(io$(14),25,  8),  ~
               at (19,32), fac(str(io$(14),33,1)), str(io$(14),34,  8),  ~
                                                                         ~
               at (20,06), fac(str(io$(15), 7,1)), str(io$(15), 8, 10),  ~
               at (20,17), fac(str(io$(15),18,1)), str(io$(15),19,  5),  ~
               at (20,23), fac(str(io$(15),24,1)), str(io$(15),25,  8),  ~
               at (20,32), fac(str(io$(15),33,1)), str(io$(15),34,  8),  ~
                                                                         ~
               at (06,46), fac(str(io$(16), 7,1)), str(io$(16), 8, 10),  ~
               at (06,57), fac(str(io$(16),18,1)), str(io$(16),19,  5),  ~
               at (06,63), fac(str(io$(16),24,1)), str(io$(16),25,  8),  ~
               at (06,72), fac(str(io$(16),33,1)), str(io$(16),34,  8),  ~
                                                                         ~
               at (07,46), fac(str(io$(17), 7,1)), str(io$(17), 8, 10),  ~
               at (07,57), fac(str(io$(17),18,1)), str(io$(17),19,  5),  ~
               at (07,63), fac(str(io$(17),24,1)), str(io$(17),25,  8),  ~
               at (07,72), fac(str(io$(17),33,1)), str(io$(17),34,  8),  ~
                                                                         ~
               at (08,46), fac(str(io$(18), 7,1)), str(io$(18), 8, 10),  ~
               at (08,57), fac(str(io$(18),18,1)), str(io$(18),19,  5),  ~
               at (08,63), fac(str(io$(18),24,1)), str(io$(18),25,  8),  ~
               at (08,72), fac(str(io$(18),33,1)), str(io$(18),34,  8),  ~
                                                                         ~
               at (09,46), fac(str(io$(19), 7,1)), str(io$(19), 8, 10),  ~
               at (09,57), fac(str(io$(19),18,1)), str(io$(19),19,  5),  ~
               at (09,63), fac(str(io$(19),24,1)), str(io$(19),25,  8),  ~
               at (09,72), fac(str(io$(19),33,1)), str(io$(19),34,  8),  ~
                                                                         ~
               at (10,46), fac(str(io$(20), 7,1)), str(io$(20), 8, 10),  ~
               at (10,57), fac(str(io$(20),18,1)), str(io$(20),19,  5),  ~
               at (10,63), fac(str(io$(20),24,1)), str(io$(20),25,  8),  ~
               at (10,72), fac(str(io$(20),33,1)), str(io$(20),34,  8),  ~
                                                                         ~
               at (11,46), fac(str(io$(21), 7,1)), str(io$(21), 8, 10),  ~
               at (11,57), fac(str(io$(21),18,1)), str(io$(21),19,  5),  ~
               at (11,63), fac(str(io$(21),24,1)), str(io$(21),25,  8),  ~
               at (11,72), fac(str(io$(21),33,1)), str(io$(21),34,  8),  ~
                                                                         ~
               at (12,46), fac(str(io$(22), 7,1)), str(io$(22), 8, 10),  ~
               at (12,57), fac(str(io$(22),18,1)), str(io$(22),19,  5),  ~
               at (12,63), fac(str(io$(22),24,1)), str(io$(22),25,  8),  ~
               at (12,72), fac(str(io$(22),33,1)), str(io$(22),34,  8),  ~
                                                                         ~
               at (13,46), fac(str(io$(23), 7,1)), str(io$(23), 8, 10),  ~
               at (13,57), fac(str(io$(23),18,1)), str(io$(23),19,  5),  ~
               at (13,63), fac(str(io$(23),24,1)), str(io$(23),25,  8),  ~
               at (13,72), fac(str(io$(23),33,1)), str(io$(23),34,  8),  ~
                                                                         ~
               at (14,46), fac(str(io$(24), 7,1)), str(io$(24), 8, 10),  ~
               at (14,57), fac(str(io$(24),18,1)), str(io$(24),19,  5),  ~
               at (14,63), fac(str(io$(24),24,1)), str(io$(24),25,  8),  ~
               at (14,72), fac(str(io$(24),33,1)), str(io$(24),34,  8),  ~
                                                                         ~
               at (15,46), fac(str(io$(25), 7,1)), str(io$(25), 8, 10),  ~
               at (15,57), fac(str(io$(25),18,1)), str(io$(25),19,  5),  ~
               at (15,63), fac(str(io$(25),24,1)), str(io$(25),25,  8),  ~
               at (15,72), fac(str(io$(25),33,1)), str(io$(25),34,  8),  ~
                                                                         ~
               at (16,46), fac(str(io$(26), 7,1)), str(io$(26), 8, 10),  ~
               at (16,57), fac(str(io$(26),18,1)), str(io$(26),19,  5),  ~
               at (16,63), fac(str(io$(26),24,1)), str(io$(26),25,  8),  ~
               at (16,72), fac(str(io$(26),33,1)), str(io$(26),34,  8),  ~
                                                                         ~
               at (17,46), fac(str(io$(27), 7,1)), str(io$(27), 8, 10),  ~
               at (17,57), fac(str(io$(27),18,1)), str(io$(27),19,  5),  ~
               at (17,63), fac(str(io$(27),24,1)), str(io$(27),25,  8),  ~
               at (17,72), fac(str(io$(27),33,1)), str(io$(27),34,  8),  ~
                                                                         ~
               at (18,46), fac(str(io$(28), 7,1)), str(io$(28), 8, 10),  ~
               at (18,57), fac(str(io$(28),18,1)), str(io$(28),19,  5),  ~
               at (18,63), fac(str(io$(28),24,1)), str(io$(28),25,  8),  ~
               at (18,72), fac(str(io$(28),33,1)), str(io$(28),34,  8),  ~
                                                                         ~
               at (19,46), fac(str(io$(29), 7,1)), str(io$(29), 8, 10),  ~
               at (19,57), fac(str(io$(29),18,1)), str(io$(29),19,  5),  ~
               at (19,63), fac(str(io$(29),24,1)), str(io$(29),25,  8),  ~
               at (19,72), fac(str(io$(29),33,1)), str(io$(29),34,  8),  ~
                                                                         ~
               at (20,46), fac(str(io$(30), 7,1)), str(io$(30), 8, 10),  ~
               at (20,57), fac(str(io$(30),18,1)), str(io$(30),19,  5),  ~
               at (20,63), fac(str(io$(30),24,1)), str(io$(30),25,  8),  ~
               at (20,72), fac(str(io$(30),33,1)), str(io$(30),34,  8),  ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% <> 13 then L42050
                  call "MANUAL" ("ARIDATED")
                  goto L40090

L42050:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40090

        set_pf
         if edit% > 2% then L42200     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "(2)Line Items                                     "&~
                    "             (15)Print Screen"
           pf$(3) = "(9)Header Screens                                 "&~
                    "             (16)End Schedule"
           pfkey$ = hex(0102ffffffffffff09ffffff0dff0f10ffffff00)
           return

L42200:                                  /* Edit Mode- Field Enabled   */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151
            errormsg$ = " "
            amtleft   = netinv
            gosub set_facs_8c

        for i% = 1% to 30%
*        Payment Amount
            if str(io$(i%),8,10) <> " " then L50150
L50140:         io$(i%) = " "  :  goto L50660
L50150:     convert str(io$(i%),8,10) to amt, data goto L50170
            goto L50190
L50170:         errormsg$ = "Invalid entry for Payment Amount"
                str(io$(i%),7,1) = hex(82) : return
L50190:     if amt = 0 then L50140
            call "CONVERT" (amt, 2.2, str(io$(i%),8,10))
            amtleft = amtleft - amt

*        Discount Percent
            if str(io$(i%),19,5) = " " then str(io$(i%),19,5) = "0"
            convert str(io$(i%),19,5) to pct, data goto L50270
            goto L50310
L50270:         errormsg$ = "Invalid entry for Discount Percent"
                goto L50300
L50290:         errormsg$ = "Discount Percent must be 0 - 50"
L50300:         str(io$(i%),18,1) = hex(82) : return
L50310:     call "CONVERT" (pct, 2.2, str(io$(i%),19,5))
            if pct < 0 or pct > 50 then L50290
            if pct = 0 then str(io$(i%),25,8) = " "


*        Discount Due Date
            if (str(io$(i%),25%,8%) = " " or ~
                str(io$(i%),25%,8%) = blankdate$) and pct = 0 then L50480
                if str(io$(i%),25%,8%) = " " or ~
                   str(io$(i%),25%,8%) = blankdate$ then                 ~
                                    str(io$(i%),25,8) = str(io$(i%),34,8)
                call "DATEOK" (str(io$(i%),25,8), disc%, errormsg$)
                if errormsg$ = " " then L50430
L50420:              str(io$(i%),24,1) = hex(81) : return
L50430:         if disc% < invdate% then                                 ~
                     errormsg$ = "Discount Due Date must be on or"  &    ~
                                 " after Invoice Date."
                if errormsg$ <> " " then L50420

L50480
*        Net Due Date
            if str(io$(i%),34%,8%) = " " or ~
               str(io$(i%),34%,8%) = blankdate$ then                     ~
                                    str(io$(i%),34,8) = str(io$(i%),25,8)
            if str(io$(i%),25%,8%) = " " or ~
               str(io$(i%),25%,8%) = blankdate$ then disc% = 0%
            call "DATEOK" (str(io$(i%),34,8), net%, errormsg$)
            if errormsg$ = " " then L50550
L50540:         str(io$(i%),33,1) = hex(81) : return
L50550:     if net% < invdate% then                                      ~
                errormsg$ = "Net Due Date must be on or after" &         ~
                            " Invoice Date."
            if net% < disc% then                                         ~
                errormsg$ = "Net Due Date must be on or after" &         ~
                            " Discount Due Date"
            if errormsg$ <> " " then L50540

            date$ = str(io$(i%),34,8)
            call "DATUNFMT" (date$)
            str(io$(i%),,6) = date$
L50660: next i%

*        All valid.  Sort by Net Due Date then pack array to top
            call "SORT" addr(str(io$()), 30%, 41%)
            call "LINSMASH" (io$())
            return


        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program

            end ret%
