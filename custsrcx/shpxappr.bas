        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   H   H  PPPP   X   X   AAA   PPPP   PPPP   RRRR    *~
            *  S      H   H  P   P   X X   A   A  P   P  P   P  R   R   *~
            *   SSS   HHHHH  PPPP     X    AAAAA  PPPP   PPPP   RRRR    *~
            *      S  H   H  P       X X   A   A  P      P      R   R   *~
            *   SSS   H   H  P      X   X  A   A  P      P      R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPXAPPR - Approves Scheduled Export Shipments            *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/04/87 ! Original                                 ! MJB *~
            * 08/03/93 ! PRR 12489.  Corrected call to MANUAL.    ! JDH *~
            * 11/19/97 ! Moved RHHSRCE Mods into this version     ! DJD *~
            * 11/19/97 ! Revised to 60403                         ! DJD *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            avail$(100)184,              /* 1st half SHPHDRS Record    */~
            avail2$(100)117,             /* 2nd half SHPHDRS Record    */~
            cursor%(2),                  /* Cursor location for edit   */~
            cusname$(100)30,             /* Customer Name              */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            filekey$50,                  /* a file key                 */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            line2$79,                    /* Screen Line #2             */~
            ok$(100)1,                   /* Approval Flag              */~
            okfac$(13)1,                 /* Screen Facs                */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkey$32,                    /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            ship_date$(100)8,            /* Formatted Ship Date        */~
            titles$79,                   /* Screen column titles       */~
            userid$3                     /* Current User Id            */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 11/19/97 General Release  Purchase Jobs  "
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
            * # 1 ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * # 2 ! BCKPRIDX ! SO Document Print Index File             *~
            * # 3 ! BCKMASTR ! BACKLOG MASTER FILE (GET STORE NUMBER)   *~
            * # 4 ! CUSTOMER ! Customer Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "SHPHDRS",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  28                      ~


	    /* CHANGE 1 - CHANGED RECORD LENGTH OF BCKPRIDX TO 64 */
            select # 2, "BCKPRIDX",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =   11, keylen =  29,                     ~
                        alt key  1, keypos =    1, keylen =  39          ~

            select # 3, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup     ~

            select # 4, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup     ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1),   0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 100%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4),   0%, rslt$( 4))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "SHPXAPPR: " & str(cms2v$,,8)
            j% = 1%
            init (" ") avail$(), avail2$()
            str(titles$,  4) =  "Cus Code"
            str(titles$, 15) =  "Customer Name"
            str(titles$, 47) =  "S. O. Number"
            str(titles$, 65) =  "BOL"
            str(titles$, 70) =  "Ship Date"

        REM *************************************************************~
            *        L O A D   T A B L E   F O R   S E L E C T          *~
            *************************************************************
            call "SHOSTAT" ("Loading Scheduled Uninvoiced " &            ~
                             "Export Shipments for Approval")

        load_available
L10070:     call "READNEXT" (#1, f1%(1))
                if f1%(1) = 0% then display_for_select
            get #1 using L10100, invnr$, export$, expappr$
L10100:         FMT POS(234), CH(8), POS(246), CH(1), CH(1)
            if export$ <> "Y" or invnr$ <> " " then L10070
            if expappr$ = "Y" then ok$(j%) = "*"
            get #1 using L10140, str(avail$(j%),2,), avail2$(j%)
L10140:         FMT CH(183), CH(117)
            plowkey$ = str(avail$(j%),2,9)
            call "DESCRIBE" (#4, plowkey$, cusname$(j%), 0%, f1%(4))
                if f1%(4) = 0% then cusname$(j%) = "Unknown Customer"
            ship_date$(j%) = str(avail$(j%),33,6)
            call "DATEFMT" (ship_date$(j%))
            j% = j% + 1%
            goto load_available

        REM *************************************************************~
            *       D I S P L A Y   F O R   U S E R   S E L E C T       *~
            *************************************************************
        display_for_select
            if j% = 0% then none_there
            inpmessage$ = "Mark to APPROVE  -or-  Blank to Remove " &    ~
                          "Approval  &  Press RETURN"
            k% = 1%

L11090:     gosub L40000
              errormsg$ = " "
              if keyhit% =  1 then gosub startover
              if keyhit% =  2 then k% = 1%
              if keyhit% =  3 then k% = max(1%, min(j%, j%-13%))
              if keyhit% =  4 then k% = max(1%, k%-12%)
              if keyhit% =  5 then k% = max(1%, min(j%, k%+13%, j%-13%))
              if keyhit% =  6 then k% = max(1%, k%-1%)
              if keyhit% =  7 then k% = max(1%, min(j%, k%+1%, j%-1%))
              if keyhit% = 16 then       exit_program
              if keyhit% <> 0 then L11090


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Update the approval flag and rewrite the appropriate      *~
            *   records.                                                *~
            *************************************************************
L19090
*       * First ask user what documents to print
            doc% = 2%
            call "ASKUSER" (doc%, "***** DOCUMENT SELECTION *****",      ~
                 "Press PF1 for no Documents",                           ~
                 "Press PF2 for BOLs, Press PF3 for Pick Lists",         ~
                 "Press PF4 for Both")
            if doc% < 1 or doc% > 4 then L19090
            call "SHOSTAT" ("Setting Approval Flags & Creating "  &      ~
                            "Document file as requested")
            for k% = 1% to j%
                if (str(avail2$(k%),64,1) <> "Y" and ok$(k%)  = " ")  or ~
                   (str(avail2$(k%),64,1)  = "Y" and ok$(k%) <> " ")     ~
                        then L19400  /* next K% */
                gosub kill_old
                if ok$(k%) <> " " then str(avail2$(k%),64,1) = "Y"       ~
                                  else str(avail2$(k%),64,1) = " "

                filekey$ = str(avail$(k%),2,28)
                call "READ101" (#1, filekey$, f1%(1))
                    if f1%(1) = 0% then record_gone
                put #1 using L19210, str(avail$(k%),2,), avail2$(k%)
L19210:             FMT CH(183), CH(117)
                rewrite #1

                if ok$(k%) = " " then goto L19400

                if doc% = 1% then L19400
                if doc% = 3% then L19350

*       * Put BOL in tickler file
                write #2 using L19320, "B", str(avail$(k%),30, 3),        ~
                    str(avail$(k%),33, 6), "B", str(avail$(k%), 2, 9),   ~
                    str(avail$(k%),11,16),  str(avail$(k%),27, 3), " ",  ~
                    " "
L19320:             FMT CH(1),CH(3),CH(6),CH(1),CH(9),CH(16),CH(3),CH(6),~
                        CH(20)
                if doc% = 2% then next k%

/* CHANGE 2 - ABOVE CODE STARTING WITH STR(AVAIL(K%),11,16 TO THE END */
/* OF THE FORMAT STATEMENT					      */

L19350
*       * Put Pick List in tickler file
                write #2 using L19320, "P", str(avail$(k%),30, 3),        ~
                    str(avail$(k%),33, 6), "P", str(avail$(k%), 2, 9),   ~
                    str(avail$(k%),11,16),  str(avail$(k%),27, 3), " ",  ~
                    " "
                    /* CHANGE 3 - ABOVE CODE STARTING AT THE STR.. 11,16 */
                    /* ENDING WITH THE DOUBLE QUOTES                     */


L19400:     next k%

            goto exit_program

        kill_old
            plowkey$ = "P" & str(avail$(k%), 2, 9) &                     ~
                             str(avail$(k%),11,16) & str(avail$(k%),27,3)
            call "READ101" (#2, plowkey$, f1%(2))
                if f1%(2) = 1% then delete #2

            plowkey$ = "B" & str(avail$(k%), 2, 9) &                     ~
                             str(avail$(k%),11,16) & str(avail$(k%),27,3)
            call "READ101" (#2, plowkey$, f1%(2))
                if f1%(2) = 1% then delete #2

            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants.   *~
            *************************************************************
        startover
            u3% = 2%
L29916:     call "STARTOVR" (u3%)
            if u3%  = 1% then return
            if u3% <> 0% then L29916
                for l% = 1% to j%
                     str(avail$(l%),1,1) = " "
                next l%
                return clear all
                goto display_for_select


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        FMT                 /* FILE: SHPHDRS                           */~
            CH(9),          /* Customer Ship-to Address Identifier     */~
            CH(16),         /* sales order number                      */~
            CH(3),          /* Bill of Lading Number                   */~
            CH(3),          /* Warehouse or Store                      */~
            CH(6),          /* Ship Date                               */~
            CH(6),          /* Shipping Carrier Code                   */~
            CH(20),         /* how ship information                    */~
            CH(20),         /* f.o.b. information                      */~
            2*CH(50),       /* Shipping Instructions                   */~
            CH(6),          /* Ship Date                               */~
            CH(20),         /* Freight/ Air Bill Number                */~
            PD(14,4),       /* Number of Cartons                       */~
            PD(14,4),       /* Shipment Weight                         */~
            PD(14,4),       /* freight amount                          */~
            CH(8),          /* Invoice Number                          */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* Export Invoice Flag                     */~
            CH(1),          /* Approved for Export Flag                */~
            CH(53)          /* Filler (Internal, unused space)         */~

        FMT                 /* FILE: BCKPRIDX                          */~
            CH(1),          /* Type - used generically for special desc*/~
            CH(3),          /* Warehouse or Store                      */~
            CH(6),          /* Line ship date for Quote                */~
            CH(1),          /* Type - used generically for special desc*/~
            CH(9),          /* Customer Code                           */~
            CH(16),         /* sales order number                      */~
            CH(3),          /* Bill of Lading Number                   */~
            CH(6)           /* Date that record was setup on the system*/~

L40000: REM *************************************************************~
            *        D I S P L A Y   F O R   S E L E C T I O N          *~
            *-----------------------------------------------------------*~
            * Display available for use selection                       *~
            *************************************************************
            gosub setpf
            init (hex(9c)) okfac$()
            for i% = 1% to 13%
                if str(avail$(k% + i% - 1%), 2, 9) = " " then L40049
                okfac$(i%) = hex(81)
L40049:     next i%

L40060: accept                                                           ~
            at (01,02), "Approve Scheduled EXPORT Shipments",            ~
            at (01,66), "Today:",                                        ~
            at (01,73), fac(hex(8c)),  date$                    , ch(08),~
            at (02,02), fac(hex(ac)),  line2$                   , ch(79),~
            at (04,02), fac(hex(94)),  errormsg$                , ch(79),~
                                                                         ~
            at (06,02), fac(hex(ac)),  titles$                  , ch(79),~
                                                                         ~
            at (07,02), fac(okfac$(1%)), ok$(k%)                , ch(01),~
            at (07,05), fac(hex(8c)),  str(avail$(k%   ), 2, 9) , ch(09),~
            at (07,16), fac(hex(8c)),  cusname$(k%)             , ch(30),~
            at (07,48), fac(hex(8c)),  str(avail$(k%   ),11,16) , ch(16),~
            at (07,66), fac(hex(8c)),  str(avail$(k%   ),27, 3) , ch(03),~
            at (07,71), fac(hex(8c)),  ship_date$(k%)           , ch(08),~
                                                                         ~
            at (08,02), fac(okfac$(2%)), ok$(k%+1%)             , ch(01),~
            at (08,05), fac(hex(8c)),  str(avail$(k%+1%), 2, 9) , ch(09),~
            at (08,16), fac(hex(8c)),  cusname$(k%+1%)          , ch(30),~
            at (08,48), fac(hex(8c)),  str(avail$(k%+1%),11,16) , ch(16),~
            at (08,66), fac(hex(8c)),  str(avail$(k%+1%),27, 3) , ch(03),~
            at (08,71), fac(hex(8c)),  ship_date$(k%+1%)        , ch(08),~
                                                                         ~
            at (09,02), fac(okfac$(3%)), ok$(k%+2%)             , ch(01),~
            at (09,05), fac(hex(8c)),  str(avail$(k%+2%), 2, 9) , ch(09),~
            at (09,16), fac(hex(8c)),  cusname$(k%+2%)          , ch(30),~
            at (09,48), fac(hex(8c)),  str(avail$(k%+2%),11,16) , ch(16),~
            at (09,66), fac(hex(8c)),  str(avail$(k%+2%),27, 3) , ch(03),~
            at (09,71), fac(hex(8c)),  ship_date$(k%+2%)        , ch(08),~
                                                                         ~
            at (10,02), fac(okfac$(4%)), ok$(k%+3%)             , ch(01),~
            at (10,05), fac(hex(8c)),  str(avail$(k%+3%), 2, 9) , ch(09),~
            at (10,16), fac(hex(8c)),  cusname$(k%+3%)          , ch(30),~
            at (10,48), fac(hex(8c)),  str(avail$(k%+3%),11,16) , ch(16),~
            at (10,66), fac(hex(8c)),  str(avail$(k%+3%),27, 3) , ch(03),~
            at (10,71), fac(hex(8c)),  ship_date$(k%+3%)        , ch(08),~
                                                                         ~
            at (11,02), fac(okfac$(5%)), ok$(k%+4%)             , ch(01),~
            at (11,05), fac(hex(8c)),  str(avail$(k%+4%), 2, 9) , ch(09),~
            at (11,16), fac(hex(8c)),  cusname$(k%+4%)          , ch(30),~
            at (11,48), fac(hex(8c)),  str(avail$(k%+4%),11,16) , ch(16),~
            at (11,66), fac(hex(8c)),  str(avail$(k%+4%),27, 3) , ch(03),~
            at (11,71), fac(hex(8c)),  ship_date$(k%+4%)        , ch(08),~
                                                                         ~
            at (12,02), fac(okfac$(6%)), ok$(k%+5%)             , ch(01),~
            at (12,05), fac(hex(8c)),  str(avail$(k%+5%), 2, 9) , ch(09),~
            at (12,16), fac(hex(8c)),  cusname$(k%+5%)          , ch(30),~
            at (12,48), fac(hex(8c)),  str(avail$(k%+5%),11,16) , ch(16),~
            at (12,66), fac(hex(8c)),  str(avail$(k%+5%),27, 3) , ch(03),~
            at (12,71), fac(hex(8c)),  ship_date$(k%+5%)        , ch(08),~
                                                                         ~
            at (13,02), fac(okfac$(7%)), ok$(k%+6%)             , ch(01),~
            at (13,05), fac(hex(8c)),  str(avail$(k%+6%), 2, 9) , ch(09),~
            at (13,16), fac(hex(8c)),  cusname$(k%+6%)          , ch(30),~
            at (13,48), fac(hex(8c)),  str(avail$(k%+6%),11,16) , ch(16),~
            at (13,66), fac(hex(8c)),  str(avail$(k%+6%),27, 3) , ch(03),~
            at (13,71), fac(hex(8c)),  ship_date$(k%+6%)        , ch(08),~
                                                                         ~
            at (14,02), fac(okfac$(8%)), ok$(k%+7%)             , ch(01),~
            at (14,05), fac(hex(8c)),  str(avail$(k%+7%), 2, 9) , ch(09),~
            at (14,16), fac(hex(8c)),  cusname$(k%+7%)          , ch(30),~
            at (14,48), fac(hex(8c)),  str(avail$(k%+7%),11,16) , ch(16),~
            at (14,66), fac(hex(8c)),  str(avail$(k%+7%),27, 3) , ch(03),~
            at (14,71), fac(hex(8c)),  ship_date$(k%+7%)        , ch(08),~
                                                                         ~
            at (15,02), fac(okfac$(9%)), ok$(k%+8%)             , ch(01),~
            at (15,05), fac(hex(8c)),  str(avail$(k%+8%), 2, 9) , ch(09),~
            at (15,16), fac(hex(8c)),  cusname$(k%+8%)          , ch(30),~
            at (15,48), fac(hex(8c)),  str(avail$(k%+8%),11,16) , ch(16),~
            at (15,66), fac(hex(8c)),  str(avail$(k%+8%),27, 3) , ch(03),~
            at (15,71), fac(hex(8c)),  ship_date$(k%+8%)        , ch(08),~
                                                                         ~
            at (16,02), fac(okfac$(10%)), ok$(k%+9%)            , ch(01),~
            at (16,05), fac(hex(8c)),  str(avail$(k%+9%), 2, 9) , ch(09),~
            at (16,16), fac(hex(8c)),  cusname$(k%+9%)          , ch(30),~
            at (16,48), fac(hex(8c)),  str(avail$(k%+9%),11,16) , ch(16),~
            at (16,66), fac(hex(8c)),  str(avail$(k%+9%),27, 3) , ch(03),~
            at (16,71), fac(hex(8c)),  ship_date$(k%+9%)        , ch(08),~
                                                                         ~
            at (17,02), fac(okfac$(11%)),  ok$(k%+10%)          , ch(01),~
            at (17,05), fac(hex(8c)),  str(avail$(k%+10%),2, 9) , ch(09),~
            at (17,16), fac(hex(8c)),  cusname$(k%+10%)         , ch(30),~
            at (17,48), fac(hex(8c)),  str(avail$(k%+10%),11,16), ch(16),~
            at (17,66), fac(hex(8c)),  str(avail$(k%+10%),27, 3), ch(03),~
            at (17,71), fac(hex(8c)),  ship_date$(k%+10%)       , ch(08),~
                                                                         ~
            at (18,02), fac(okfac$(12%)),  ok$(k%+11%)          , ch(01),~
            at (18,05), fac(hex(8c)),  str(avail$(k%+11%),2, 9) , ch(09),~
            at (18,16), fac(hex(8c)),  cusname$(k%+11%)         , ch(30),~
            at (18,48), fac(hex(8c)),  str(avail$(k%+11%),11,16), ch(16),~
            at (18,66), fac(hex(8c)),  str(avail$(k%+11%),27, 3), ch(03),~
            at (18,71), fac(hex(8c)),  ship_date$(k%+11%)       , ch(08),~
                                                                         ~
            at (19,02), fac(okfac$(13%)),  ok$(k%+12%)          , ch(01),~
            at (19,05), fac(hex(8c)),  str(avail$(k%+12%),2, 9) , ch(09),~
            at (19,16), fac(hex(8c)),  cusname$(k%+12%)         , ch(30),~
            at (19,48), fac(hex(8c)),  str(avail$(k%+12%),11,16), ch(16),~
            at (19,66), fac(hex(8c)),  str(avail$(k%+12%),27, 3), ch(03),~
            at (19,71), fac(hex(8c)),  ship_date$(k%+12%)       , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$),                                       ~
                     key (keyhit%)

               if keyhit% <> 13% then L41040
                     call "MANUAL" ("SHPXAPPR")
                     goto L40060

L41040:        if keyhit% <> 15% then L41080
                     call "PRNTSCRN"
                     goto L40060

L41080:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        setpf
           pf$(1) = "(1)Start Over  (2)First   (5)Next                 "&~
                    "             (13)Instructions"
           pf$(2) = "               (3)Last    (6)Down                 "&~
                    "             (15)Print Screen"
           pf$(3) = "               (4)Prev    (7)Up                   "&~
                    "             (16)Exit Program"
           pfkey$ = hex(01020304050607ffffffffff0dff0f10ffffff00)
           if k% <> 1% then L41250
                str(pf$(1),16,8), str(pf$(3),16,8), str(pf$(2),27,7) = " "
                str(pfkey$, 2,1), str(pfkey$, 4,1), str(pfkey$, 6,1)     ~
                                                              = hex(ff)
L41250:    if k% + 13% < j% then L41290
                str(pf$(2),16,8), str(pf$(1),27,7), str(pf$(3),27,7) = " "
                str(pfkey$, 3,1), str(pfkey$, 5,1), str(pfkey$, 7,1)     ~
                                                              = hex(ff)
L41290:    return

        record_gone
*       ** Somebody got my record between load & update!!
            goto exit_program

        none_there
            ask% = 0%
            call "ASKUSER" (ask%, "***** FILE EMPTY ERROR *****",        ~
                                  "There are NO Shipments Scheduled!",   ~
                             " ", "Press any key to EXIT program")
            goto exit_program

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
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
