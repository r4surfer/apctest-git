        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M   SSS   TTTTT  L      RRRR   PPPP    *~
            *  A   A  R   R  MM MM  S        T    L      R   R  P   P   *~
            *  AAAAA  RRRR   M M M   SSS     T    L      RRRR   PPPP    *~
            *  A   A  R  R   M   M      S    T    L      R  R   P       *~
            *  A   A  R   R  M   M   SSS     T    LLLLL  R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMSTLRP - Prints A/R Settlements Report.                 *~
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
            * 12/17/86 ! Original                                 ! ERN *~
            * 05/17/89 ! Added document date to listing-Thanks MJB! GGO *~
            * 11/10/89 ! Took out currency column if MC not on.   ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            billto$9,                    /* Bill-to Customer Code      */~
            date_stld$8,                 /* Date Settled               */~
            compname$60,                 /* Company Name               */~
            curr$1,                      /* Is Multi-currency on?      */~
            currency$4,                  /* Transaction Currency       */~
            date$8,                      /* Run Date                   */~
            docdate$8,                   /* Document date              */~
            last_billto$9,               /* Last Bill-to read          */~
            last_stl$8,                  /* Date of last settling run  */~
            level$7,                     /* Level of settling          */~
            name$27,                     /* Bill-to Name               */~
            plowkey$50,                  /* A Plow Key                 */~
            po$16,                       /* PO                         */~
            runtime$8,                   /* Report Run Time            */~
            srcedoc$8,                   /* Source Document Number     */~
            statutory$4,                 /* Statutory currency         */~
            stlmnt$14,                   /* Settlement Number          */~
            type$7                       /* Source Document Type       */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            axd$4,                       /* Alt Indices Byte           */~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "
        REM *************************************************************

            mat f2% = con
                     /* The variable F2%() should not be modified.     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! ARMSTLRF ! A/R Settlements File                     *~
            * #2  ! SYSFILE2 ! CMS System File                          *~
            * #3  ! CUSTOMER ! Customer Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "ARMSTLRF",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  28

            select #2,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #3,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            u3% = 2%
            call "ASKUSER" (u3%, "** A/R SETTLEMENTS REPORT **",         ~
                            "Press PF-16 to Continue with this Report",  ~
                            "-- OR --",                                  ~
                            "Press RETURN to Cancel and Return to Menu")
            if u3% <> 16% then exit_program

            call "OPENFILE" (#1, "IO   ", f2%(1), rslt$(1), axd$)
            if f2%(1) = 0% then L02440
                u3% = 2%
                call "ASKUSER" (u3%, "** A/R SETTLEMENTS REPORT **",     ~
                                "Unable to open report file (ARMSTLRF);",~
                                "Either it is in use or doesn't exist.", ~
                                "Press RETURN to return to menu...")
                goto exit_program
L02440:     call "OPENFILE" (#2, "SHARE", f2%(2), rslt$(2), axd$)
            if f2%(2) <> 0% then exit_program
            call "OPENFILE" (#3, "SHARE", f2%(3), rslt$(3), axd$)
            if f2%(3) <> 0% then exit_program


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            print page
            call "SHOSTAT" ("Printing A/R SETTLEMENTS REPORT")

            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (runtime$)
            call "COMPNAME" (12%, compname$, f1%(2))
            call "SETPRNT" ("ARM007", " ", 0%, 0%)
            select printer (134)
            line% = 857%

            call "READ100" (#2, "ARM.LAST.SETTLING   ", f1%(2))
            if f1%(2) = 1% then L09230
                call "ASKUSER" (2%, "** A/R SETTLEMENTS REPORT **",      ~
                                "A Settling Pass has never been run.",   ~
                                " ", "Press RETURN to return to menu...")
                goto exit_program
L09230:     get #2 using L09240, last_stl$
L09240:         FMT XX(20), CH(6)
            call "DATEFMT" (last_stl$)

            curr$ = "N" : statutory$ = " "
            call "READ100" (#2, "SWITCHS.CUR", f1%(2))
             if f1%(2) <> 0% then get #02 using L09300, curr$, statutory$
L09300:          FMT POS(21), CH(1), CH(4)
             if curr$ <> "Y" then statutory$ = " "

        REM *************************************************************~
            *                P R I N T    R E P O R T                   *~
            *-----------------------------------------------------------*~
            * Dump file.                                                *~
            *************************************************************

        report_loop
            call "READNEXT" (#1, f1%(1))
            if f1%(1) = 1% then L10120
                gosub report_totals
                goto  clear_file

L10120:     get #1 using L10140, billto$, stlmnt$, date_stld$, level$,    ~
                                currency$, po$, transamt, type$,         ~
                                srcedoc$, docdate$
L10140:         FMT CH(9), CH(12), XX(7), CH(6), CH(1), CH(4), POS(87),  ~
                    CH(16), POS(112), PD(14,4), POS(131), CH(2), CH(8),  ~
                    POS(147), CH(6)
            if currency$ = " " then currency$ = statutory$
            if first% <> 0% then L10153
               firstcurrency$  = currency$:first% = 1%
L10153:     if firstcurrency$ <> currency$ then mixed% = 1%
            stlmnt$ = str(stlmnt$,,8) & "-" & str(stlmnt$,9,2) & "-" &   ~
                                              str(stlmnt$, 11)
            call "DATEFMT" (date_stld$)
            call "DATEFMT" (docdate$)
            items% = items% + 1%
            if level$ = "P" then level$ = "PAYMENT"
            if level$ = "S" then level$ = "STL GRP"
            if level$ = "B" then level$ = "BILL-TO"
            if level$ = "C" then level$ = "CURENCY"
            if type$ = "II" then type$ = "INVOICE"
            if type$ = "IC" then type$ = "CR MEMO"
            if type$ = "IF" then type$ = "FIN CHG"
            if type$ = "IA" then type$ = "ADJ INV"
            if type$ = "CP" then type$ = "PAYMENT"
            if type$ = "CU" then type$ = "UNAPPLD"
            if type$ = "CA" then type$ = "PAY ADJ"
            if type$ = "CB" then type$ = "BF PMNT"
            if type$ = "CD" then type$ = "BF DIST"
            if billto$ = last_billto$ then L10360
                call "DESCRIBE" (#3, billto$, name$, 0%, f1%(3))
                billto% = 1%
                last_billto$ = billto$
L10360:     if line% > 55% then gosub page_heading
            if billto% = 1% then                                         ~
                print using L10910, billto$, name$, stlmnt$, srcedoc$,    ~
                                   type$, docdate$, po$, date_stld$,     ~
                                   level$, currency$, transamt           ~
                            else                                         ~
                print using L10910, " "    , " "  , stlmnt$, srcedoc$,    ~
                                   type$, docdate$, po$, date_stld$,     ~
                                   level$, currency$, transamt
            line%   = line% + 1%
            billto% = 0%
            total   = total + transamt
            goto report_loop


            report_totals
                if items% = 0% then return
                     print
                     if mixed% = 0% then print using L10940, items%, total~
                                    else print using L10960, items%
                     print
                     print "** END OF REPORT **"
                     return


            page_heading
                page% = page% + 1%
                line% = 7%
                print page
                print using L10760, date$, runtime$, compname$
                print using L10790, last_stl$, page%
                print
                if curr$ = "Y" then print using L10820                    ~
                               else print using L10840
                if curr$ = "Y" then print using L10850                    ~
                               else print using L10872
                if curr$ = "Y" then print using L10880                    ~
                               else print using L10902
                billto% = 1%
                return




L10760: %RUN DATE: ######## ########                 ####################~
        ~########################################             ARMSTLRP-ARM~
        ~007
L10790: %FILE LAST SETTLED: ########                         A / R   S E ~
        ~T T L E M E N T S   R E P O R T                          PAGE: ##~
        ~##
L10820:  % BILL-TO                                              SOURCE  D~
        ~OCMNT                             DATE    SETTLE  TRANS
L10840:  % BILL-TO                                              SOURCE  D~
        ~OCMNT                             DATE    SETTLE
L10850:  %CUSTOMER  CUSTOMER NAME               SETTLEMENT NO. DOCUMENT  ~
        ~TYPE   DATED    CUSTOMER PO      SETTLED   LEVEL  CURR  TRANS AMO~
        ~UNT
L10872:  %CUSTOMER  CUSTOMER NAME               SETTLEMENT NO. DOCUMENT  ~
        ~TYPE   DATED    CUSTOMER PO      SETTLED   LEVEL        TRANS AMO~
        ~UNT
L10880:  %--------- --------------------------- -------------- -------- -~
        ~------ -------- ---------------- -------- ------- ---- ----------~
        ~---
L10902:  %--------- --------------------------- -------------- -------- -~
        ~------ -------- ---------------- -------- -------      ----------~
        ~---
L10910:  %######### ########################### ############## ######## #~
        ~###### ######## ################ ######## ####### #### -#####,###~
        ~.##
L10940:  %                                                               ~
        ~           ** REPORT TOTALS (###,### ITEMS)            -#####,###~
        ~.##
L10960:  %                                                               ~
        ~           ** REPORT TOTALS (###,### ITEMS)       MULTIPLE CURREN~
        ~CIES

        clear_file
            call "SHOSTAT" ("REPORT COMPLETED")
            if items% > 0% then L11060
                call "ASKUSER" (2%, "** A/R SETTLEMENTS REPORT **",      ~
                                "There are No Settlements on file.",     ~
                                " ", "Press any PF Key to exit.")
                goto exit_program

L11060:     u3% = 2%
            call "ASKUSER" (u3%, "** A/R SETTLEMENTS REPORT **",         ~
                     "Enter PF-8 to CLEAR Settlement's Report File,",    ~
                     "- OR -", "Enter RETURN to NOT CLEAR File.")
            if u3%  = 0% then exit_program
            if u3% <> 8% then L11060
                call "SHOSTAT" ("Clearing Settlement's Report File")
                plowkey$ = hex(00)
                call "DELETE" (#1, plowkey$, 0%)


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
            call "SHOSTAT" ("One Moment Please")
            call "SETPRNT" ("ARM007", " ", 0%, 1%)
            end
