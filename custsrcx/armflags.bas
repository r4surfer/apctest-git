        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M  FFFFF  L       AAA    GGG    SSS    *~
            *  A   A  R   R  MM MM  F      L      A   A  G      S       *~
            *  AAAAA  RRRR   M M M  FFFF   L      AAAAA  G GGG   SSS    *~
            *  A   A  R R    M   M  F      L      A   A  G   G      S   *~
            *  A   A  R  R   M   M  F      LLLLL  A   A   GGG    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMFLAGS - Set switches for Accounts Receivable Module.   *~
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
            * 05/30/86 ! ORIGINAL                                 ! ERN *~
            * 06/01/90 ! Added Allow Duplicate Invoice Numbers.   ! JDH *~
            * 05/26/94 ! Added Allow Neg. Qty Invoice Lines to    ! JDH *~
            *          !   Affect Inventory?                      !     *~
            *02/26/2013! (AWD001) mod for esc acct                ! CMG *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                              /* NORMAL OLD VARIABLES       */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second screen line         */~
            pf$(3)79,                    /* PF Key Prompts             */~
            pfk$10                       /* PF Keys Available          */

        dim                              /* IN ORDER OF APPEARANCE     */~
            acct$16, descr$30,           /* Test Acct and Description  */~
            dup_inv$1,                   /* Allow duplicate invoice #s */~
            invstore$3,                  /* Invoice # Assign Store     */~
            postdisc$1,                  /* Post Discounts?            */~
            neg_inv$1,                   /* Allow neg qty to affect inv*/~
            nextfc$8,                    /* Next Finance Charge INV#   */~
            hnyactve$1,                  /* Inventory Interface Active */~
            aractive$1,                  /* A/R Active                 */~
            shipacct$16,                 /* Shipped, not billed Acct   */~
            shipacctdescr$30,            /*                            */~
            prnttype$10,                 /* Types to Print Invoices For*/~
            frtacct$16,                  /* Freight Account            */~
            frtacctdescr$30,             /*                            */~
            taxacct$16,                  /* Sales Tax Account          */~
            taxacctdescr$30,             /*                            */~
            fcacct$16,                   /* Finance Charge Account     */~
            fcacctdescr$30,              /*                            */~
            cashacct$16,                 /* Cash in Bank Account       */~
            cashacctdescr$30,            /*                            */~
            discacct$16,                 /* Allowed Discounts Acct     */~
            discacctdescr$30,            /*                            */~
            unaldisc$16,                 /* Unallowed Discounts Acct   */~
            unaldiscdescr$30             /*                            */
            
       dim  escacct$16,                  /* (AWD001) ESC Accct         */~
            escacctdescr$30              /* (AWD001) ESC ACCT descr    */     



        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.01 07/28/94 CMS Patch Release R6.03.01      "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            * #2  ! STORNAME ! Store Master File                        *~
            * #3  ! GLMAIN   ! Chart of Accounts File                   *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "SYSFILE2",                                      ~
                        varc, indexed,                                   ~
                        recsize =  500,                                  ~
                        keypos =     1, keylen =  20

            select #2,  "STORNAME",                                      ~
                        varc, indexed,                                   ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            select #3,  "GLMAIN",                                        ~
                        varc, indexed,                                   ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, 0%, f2%(1),   0%, " ")
            call "OPENCHCK" (#2, 0%, f2%(2),   0%, " ")
            call "OPENCHCK" (#3, 0%, f2%(3),   0%, " ")


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date  :  call "DATEFMT" (date$)

*        See if this User is a Data Base or Module Administrator
            call "CMSMACHK" ("ARM", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then L09160
            pf$(1) = "You must be a Data Base or A/R Module Administrato"~
                     & "r to run this program."
            call "ASKUSER" (0%, "SECURITY CHECK", " ", pf$(1), " ")
            goto L65000

L09160
*        Set some variables
            str(line2$,62) = "ARMFLAGS: " & cms2v$

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * No real input mode - just load defaults from disk.        *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$

            gosub L30000   /* Load then Format stuff all nice and purty */


L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

L11055:     gosub'051(0%)
L11060:     gosub'111(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then gosub L30100
                  if keyhit%  =  9% then       mod_admin
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       L65000
                  if keyhit% <>  0% then       L11060
L11110:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 16% then L11060

            gosub'051(fieldnr%)
                  if enabled% = 0% then L11190
L11160:     gosub'111(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11160
L11190:     gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11160
                  if fieldnr% = cursor%(1%) - 4% then L11055 else L11110


        mod_admin    /* Allow maintenance of ARM Module Administrators */
            call "CMSMAINP" ("ARM", "Accounts Receivable")
            goto L11000


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L31000
            goto  L65000


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES fields for the Page 1 of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
            if fieldnr% <> 0% then L20110
                inpmessage$ = "Place cursor at field to modify and" &    ~
                " Press (RETURN); PF-32 to EXIT program."
                return
L20110:     enabled% = 1%

                  on fieldnr% gosub L20270,         /* Invoice # Assign */~
                                    L20310,         /* Duplicate Inv #s */~
                                    L20880,         /* Neg Qty Affct Inv*/~
                                    L20320,         /* Post Discounts   */~
                                    L20370,         /* Next FC Inv#     */~
                                    L20420,         /* HNY Active?      */~
                                    L20470,         /* Inventory Active?*/~
                                    L20510,         /* GL-Interim Shpng */~
                                    L20560,         /*    Freight       */~
                                    L20600,         /*    Sales Tax     */~
                                    L20640,         /*    FC Account    */~
                                    L20680,         /*    Cash          */~
                                    L20730,         /*    Allowed Discs */~
                                    L20780,         /*    Unallowd Disc */~
                                    L20830,         /* Inv Print Types  */~
                                    L20930          /* Freight Acct (AWD001)*/
                     return

L20270
*        Default/Enable for INVOICE NUMBER ASSIGNMENT
            inpmessage$ = "Enter Store which contains next Invoice # " & ~
                          "or leave blank to use invoicing store."
            return

L20310
*        Default/Enable for Allow Duplicate Invoice Numbers
            inpmessage$ = "Enter 'Y' if the same invoice number can b" & ~
                           "e used for different customers."
            return

L20320
*        Default/Enable for POST DISCOUNTS SEPERATELY
            inpmessage$ =                                                ~
              "Enter 'N' to post only Net Extensions to G/L."
            return

L20370
*        Default/Enable for NEXT FINANCE CHARGE NUMBER
            inpmessage$ =                                                ~
              "Enter Next Invoice Number for Finance Charges.         "
            return

L20420
*        Default/Enable for INVENTORY ACTIVE?
            inpmessage$ =                                                ~
              "Enter 'Y' if A/R-Inventory Interface is Active."
            return

L20470
*        Default/Enable for A/R ACTIVE?
            inpmessage$ = "Enter 'Y' if Accounts Receivable is Active."
            return

L20510
*        Default/Enable for SHIPPED, NOT BILLED INVENTORY ACCOUNT
            inpmessage$ = "Enter Account Default for Shipped but " &     ~
                          "Not Billed Inventory.      "
            return

L20560
*        Default/Enable for FREIGHT ACCOUNT
            inpmessage$ = "Enter Account Default for Freight.    "
            return

L20600
*        Default/Enable for SALES TAX ACCOUNT
            inpmessage$ = "Enter Account Default for Sales Tax.  "
            return

L20640
*        Default/Enable for FINANCE CHARGE ACCOUNT
            inpmessage$ = "Enter Account Default for Finance Charges."
            return

L20680
*        Default/Enable for CASH IN BANK ACCOUNT
            inpmessage$ = "Enter Account Default for Cash Receipts Ca" & ~
                          "sh-in-Bank."
            return

L20730
*        Default/Enable for ALLOWED DISCOUNTS ACCOUNT
            inpmessage$ = "Enter Account Default for Cash Receipts Al" & ~
                          "lowed Discounts."
            return

L20780
*        Default/Enable for UNALLOWED DISCOUNTS ACCOUNT
            inpmessage$ = "Enter Account Default for Cash Receipts Un" & ~
                          "allowed Discounts."
            return

L20830
*        Default/Enable for INVOICE TYPES TO PRINT
            inpmessage$ = "Enter Invoice Types that Invoices are to"   & ~
                          " be printed for."
            return

L20880
*        Default/Enable for NEG QTY AFFECT INVENTORY
            inpmessage$ = "Enter 'Y'es if Negative Quantity Invoice"   & ~
                          " Line Items can Affect Inventory"
            return
            
L20930            
*        Default/Enable for ENERGY SURCHARGE ACCOUNT  (AWD001)
            inpmessage$ = "Enter Account Default for Energy Surcharge" & ~
                          "s."
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%  /* PUT MSG AREA AT BOTTOM OF SCREEN  */
            call "STARTOVR" (keyhit1%)
                if keyhit1% = 1% then return

                return clear all
                goto inputmode

L30000: REM *************************************************************~
            *                  L O A D   D A T A                        *~
            * --------------------------------------------------------- *~
            * Pull data from switch's string and format.                *~
            *************************************************************

        call "READ100" (#1, "SWITCHS.ARM", f1%(1))
        if f1%(1) = 1% then L30310

*        No record on file or User wants to reset to system defaults
L30100:  e1% = 0%  : if keyhit% = 2% then e1% = -1% /* Return Dflts */
         e%  = e1% : call "BCKSWTCH" ("AR ", "INVSTORE", invstore$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("AR ", "POSTDISC", postdisc$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("AR ", "NEXTFC  ", nextfc$  , e, e%)
         e%  = e1% : call "BCKSWTCH" ("AR ", "HNYACTVE", hnyactve$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("AR ", "ARACTIVE", aractive$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("AR ", "SHIPACCT", shipacct$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("AR ", "FRTACCT ", frtacct$ , e, e%)
         e%  = e1% : call "BCKSWTCH" ("AR ", "TAXACCT ", taxacct$ , e, e%)
         e%  = e1% : call "BCKSWTCH" ("AR ", "FCACCT  ", fcacct$  , e, e%)
         e%  = e1% : call "BCKSWTCH" ("AR ", "CASHACCT", cashacct$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("AR ", "DISCACCT", discacct$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("AR ", "UNALDISC", unaldisc$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("AR ", "PRNTTYPE", prnttype$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("AR ", "DUP_INV ", dup_inv$ , e, e%)
         e%  = e1% : call "BCKSWTCH" ("AR ", "NEG_INV ", neg_inv$ , e, e%)

         init (" ") shipacctdescr$, frtacctdescr$, taxacctdescr$,        ~
                    fcacctdescr$, cashacctdescr$, discacctdescr$,        ~
                    unaldiscdescr$, escacctdescr$
        return


L30310:     get #1 using L30350, invstore$, postdisc$, nextfc$, shipacct$,~
                                frtacct$, taxacct$, fcacct$, cashacct$,  ~
                                discacct$, unaldisc$, hnyactve$,         ~
                                aractive$, prnttype$, dup_inv$, neg_inv$,~
/*(AWD001)*/                     escacct$
L30350:         FMT XX(20), CH(3), CH(1), CH(8), 7*CH(9), 2*CH(1),       ~
                    CH(10), CH(1), CH(1), CH(9)

            call "DESCRIBE" (#3, shipacct$, shipacctdescr$, 0%, f1%(3))
                call "GLFMT"    (shipacct$)
            call "DESCRIBE" (#3,  frtacct$,  frtacctdescr$, 0%, f1%(3))
                call "GLFMT"    ( frtacct$)
            call "DESCRIBE" (#3,  taxacct$,  taxacctdescr$, 0%, f1%(3))
                call "GLFMT"    ( taxacct$)
            call "DESCRIBE" (#3,   fcacct$,   fcacctdescr$, 0%, f1%(3))
                call "GLFMT"    (  fcacct$)
            call "DESCRIBE" (#3, cashacct$, cashacctdescr$, 0%, f1%(3))
                call "GLFMT"    (cashacct$)
            call "DESCRIBE" (#3, discacct$, discacctdescr$, 0%, f1%(3))
                call "GLFMT"    (discacct$)
            call "DESCRIBE" (#3, unaldisc$, unaldiscdescr$, 0%, f1%(3))
                call "GLFMT"    (unaldisc$)
/* (AWD001) */                
            call "DESCRIBE" (#3, escacct$, escacctdescr$, 0%, f1%(3))
                call "GLFMT"    (escacct$)         
/*(\AWD001) */                       
            if pos("YN" = dup_inv$) = 0% then dup_inv$ = "Y"
            if pos("YN" = neg_inv$) = 0% then neg_inv$ = "N"
            return


L31000: REM *************************************************************~
            *                  S A V E   D A T A                        *~
            * --------------------------------------------------------- *~
            * Write switches back to file                               *~
            *************************************************************


            call "GLUNFMT"    (shipacct$)
            call "GLUNFMT"    ( frtacct$)
            call "GLUNFMT"    ( taxacct$)
            call "GLUNFMT"    (  fcacct$)
            call "GLUNFMT"    (cashacct$)
            call "GLUNFMT"    (discacct$)
            call "GLUNFMT"    (unaldisc$)
            call "GLUNFMT"    (escacct$)      /* (AWD001) */

            call "READ101" (#1, "SWITCHS.ARM", f1%(1))
            put #1 using L31200, "SWITCHS.ARM", invstore$, postdisc$,     ~
                                nextfc$, shipacct$, frtacct$, taxacct$,  ~
                                fcacct$, cashacct$, discacct$, unaldisc$,~
                                hnyactve$, aractive$, prnttype$,         ~
                                dup_inv$, neg_inv$, escacct$, " ", " "
L31200:         FMT CH(20), CH(3), CH(1), CH(8), 7*CH(9), 2*CH(1),       ~
                    CH(10), CH(1), CH(1), CH(9), CH(182), CH(200)
            if f1%(1) = 0% then write #1 else rewrite #1
            return


        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            * --------------------------------------------------------- *~
            * Screen for Editing Page 1 of document.                    *~
            *************************************************************

        deffn'111(fieldnr%)
            if fieldnr% = 0% then init(hex(86)) lfac$()  else            ~
                                  init(hex(8c)) lfac$()
            gosub set_pf_keys

                  on fieldnr% gosub L40290,         /* Inv Number Assign*/~
                                    L40290,         /* Duplicate Inv #s */~
                                    L40290,         /* Neg Qty Affct Inv*/~
                                    L40290,         /* Post Discounts   */~
                                    L40290,         /* Next FC Number   */~
                                    L40290,         /* HNY Active       */~
                                    L40290,         /* A/R Active       */~
                                    L40290,         /* Ship Account     */~
                                    L40290,         /* Freight Account  */~
                                    L40290,         /* Tax Account      */~
                                    L40290,         /* FC Account       */~
                                    L40290,         /* Cash Account     */~
                                    L40290,         /* Discs Account    */~
                                    L40290,         /* Unallowed Disc   */~
                                    L40290,         /* Types to Print   */~
                                    L40290          /* ESCacct (AWD001) */
                     goto L40360

                  REM Set FAC'S for UPPER/LOWER CASE input
                      lfac$(fieldnr%) = hex(80)
                      return
L40290:           REM Set FAC'S for UPPER CASE ONLY input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC'S for NUMERIC ONLY input
                      lfac$(fieldnr%) = hex(82)
                      return

L40360:   accept                                                         ~
            at (01,02), "Manage A/R Module Behavior Switches",           ~
            at (01,67), "Date:",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (05,02),                                                  ~
             "Invoice Number Assignment (blank/ Store Number)          ",~
            at (05,70), fac(lfac$(1%)), invstore$               , ch(03),~
            at (06,02),                                                  ~
             "Allow Duplicate Invoice Numbers? (Y/N)                   ",~
            at (06,70), fac(lfac$(2%)), dup_inv$                , ch(01),~
            at (07,02),                                                  ~
             "Allow Negative Quantity Invoice Lines to Affect Inventory? ~
        ~(Y/N)",                                                          ~
            at (07,70), fac(lfac$(3%)), neg_inv$                , ch(01),~
            at (08,02),                                                  ~
             "Post Sales Discounts separately in General Ledger? (Y/N) ",~
            at (08,70), fac(lfac$(4%)), postdisc$               , ch(01),~
            at (09,02),                                                  ~
             "Next Finance Charge Invoice Number                       ",~
            at (09,70), fac(lfac$(5%)), nextfc$                 , ch(08),~
            at (10,02), "Is Inventory Interface Active? (Y/N)",          ~
            at (10,70), fac(lfac$(6%)), hnyactve$               , ch(01),~
            at (11,02), "Is Accounts Receivable Active? (Y/N)",          ~
            at (11,70), fac(lfac$(7%)), aractive$               , ch(01),~
            at (12,02),                                                  ~
             "G/L: Shipped, not Billed          ",                       ~
            at (12,30), fac(lfac$(8%)), shipacct$               , ch(12),~
            at (12,49), fac(hex(8c))  , shipacctdescr$          , ch(30),~
            at (13,02),                                                  ~
             "     Freight Account              ",                       ~
            at (13,30), fac(lfac$(9%)), frtacct$                , ch(12),~
            at (13,49), fac(hex(8c))  , frtacctdescr$           , ch(30),~
            at (14,02),                                                  ~
             "     Sales Tax Account            ",                       ~
            at (14,30), fac(lfac$(10%)), taxacct$               , ch(12),~
            at (14,49), fac(hex(8c))  , taxacctdescr$           , ch(30),~
            at (15,02),                                                  ~
             "     Finance Charge Account       ",                       ~
            at (15,30), fac(lfac$(11%)), fcacct$                , ch(12),~
            at (15,49), fac(hex(8c))  , fcacctdescr$            , ch(30),~
            at (16,02),                                                  ~
             "     Cash-in-Bank Account         ",                       ~
            at (16,30), fac(lfac$(12%)), cashacct$              , ch(12),~
            at (16,49), fac(hex(8c))  , cashacctdescr$          , ch(30),~
            at (17,02),                                                  ~
             "     Allowed Cash Discounts       ",                       ~
            at (17,30), fac(lfac$(13%)), discacct$              , ch(12),~
            at (17,49), fac(hex(8c))  , discacctdescr$          , ch(30),~
            at (18,02),                                                  ~
             "     Unallowed Cash Discs         ",                       ~
            at (18,30), fac(lfac$(14%)), unaldisc$              , ch(12),~
            at (18,49), fac(hex(8c))  , unaldiscdescr$          , ch(30),~
                                                                         ~
            at (19,02), "Invoice Types to be Printed",                   ~
            at (19,30), fac(lfac$(15%)), prnttype$              , ch(07),~
            at (19,49), "Types are: A, C, D, F, G, M, & O",              ~
/*AWD001*/  at (20,02),                                                  ~
             " Energy Surcharge Account         ",                       ~
            at (20,30), fac(lfac$(16%)), escacct$               , ch(12),~
            at (20,49), fac(hex(8c))  , escacctdescr$           , ch(30),~            
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)), pf$(1)                     ,ch(79),~
            at (23,02), fac(hex(8c)), pf$(2)                     ,ch(79),~
            at (24,02), fac(hex(8c)), pf$(3)                     ,ch(79),~
                                                                         ~
                keys(pfk$),                                              ~
                key (keyhit%)

                if keyhit% <> 13% then L41010
                     call "MANUAL" ("ARMFLAGS")
                     goto L40360

L41010:         if keyhit% <> 15% then L41050
                     call "PRNTSCRN"
                     goto L40360

L41050:         close ws
                call "SCREEN" addr("C", n%, "I", i$(), cursor%())
                                        n% = n%
                return

        set_pf_keys
          if fieldnr% <> 0% then L41220
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                    (9)Maintain A/R Modu" &        ~
                     "le Administrators      (15)Print Screen"
            pf$(3) = "(2)Reset using Defaults                 " &        ~
                     "                       (16)SAVE DATA   "
            str(pf$(3),63,1) = hex(84)
            pfk$ = hex(000102090d0f1020ffffff)
            return

L41220:     pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfk$ = hex(00010d0fffffffffffff)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 1.                        *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50220,         /* Invoice # assign */~
                                    L50280,         /* Duplicate Inv #s */~
                                    L51350,         /* Neg Qty Affct Inv*/~
                                    L50290,         /* Post Discounts   */~
                                    L50340,         /* Next Finance Chg */~
                                    L50390,         /* HNY Active?      */~
                                    L50440,         /* A/R Active?      */~
                                    L50760,         /* Ship Acct        */~
                                    L50820,         /* Frt              */~
                                    L50880,         /* Tax              */~
                                    L50940,         /* FC               */~
                                    L51000,         /* Cash             */~
                                    L51060,         /* Disc             */~
                                    L51120,         /* Unallowed        */~
                                    L51260,         /* Types to Print   */~
                                    L51320          /* ESC ACCT (AWD001)*/
                     return

L50220
*        Test Data for INVOICE NUMBER ASSIGNMENT
            if invstore$ = " " then return
            call "READ100" (#2, invstore$, f1%(2))
            if f1%(2) = 1% then return
                errormsg$ = "Leave blank or enter a valid Store code."
                return

L50280
*        Test Data for Allow Duplicate Invoice Numbers
            if dup_inv$ = "Y" or dup_inv$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
            return

L50290
*        Test Data for POST DISCOUNTS
            if postdisc$ = "Y" or postdisc$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
            return

L50340
*        Test Data for NEXT FINANCE CHARGE NUMBER
            if nextfc$ <> " " then L50490
                errormsg$ = "Can not be blank."
                return

L50390
*        Test Data for INVENTORY ACTIVE?
            if pos("YN" = hnyactve$) <> 0% then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L50440
*        Test Data for A/R ACTIVE?
            if pos("YN" = aractive$) <> 0% then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L50490
*         First Determine how much (if any) of a prefix exists.
            for p% = 8% to 1% step - 1%
                t1$ = str(nextfc$,p%,1)
                if t1$ <> " " and (t1$ < "0" or t1$ > "9") then L50560
            next p%
            p% = 0% : s1% = 1% : s2% = 8% : t2$ = nextfc$ : goto L50660

L50560:     if p% <= 3% then L50590
                errormsg$ = "Prefix cannot be longer than 3 characters."
                return
L50590:     s1% = p% + 1% : s2% =  8% - p%
            t1$ = str(nextfc$,,p%)  :   t2$ = str(nextfc$,s1%,s2%)

*        Test Prefix Portion
            if pos(" " = t1$) = 0% then L50660
                errormsg$ = "Prefix cannot contain blanks" : return

L50660
*        Test Suffix (numeric) portion
            convert t2$ to t2%, data goto L50680  :  goto L50690
L50680:         errormsg$ = "Invalid numeric portion."  : return
L50690:     convert t2% to t2$, pic(00000000)

*        Now concoct up the end result
            if p% = 0% then nextfc$ = t2$ else                           ~
                            nextfc$ = str(t1$,,p%) & str(t2$,s1%,s2%)
            return

L50760
*        Test Data for SHIPPED BUT NOT BILLED INVENTORY ACCOUNT
            acct$ = shipacct$ : gosub test_acct
            if errormsg$ <> " " then return
                shipacct$ = acct$  : shipacctdescr$ = descr$
                return

L50820
*        Test Data for FREIGHT ACCOUNT
            acct$ =  frtacct$ : gosub test_acct
            if errormsg$ <> " " then return
                frtacct$ = acct$  :  frtacctdescr$ = descr$
                return

L50880
*        Test Data for SALES TAX ACCOUNT
            acct$ =  taxacct$ : gosub test_acct
            if errormsg$ <> " " then return
                taxacct$ = acct$  :  taxacctdescr$ = descr$
                return

L50940
*        Test Data for FINANCE CHARGE ACCOUNT
            acct$ =  fcacct$ : gosub test_acct
            if errormsg$ <> " " then return
                fcacct$ = acct$  :  fcacctdescr$ = descr$
                return

L51000
*        Test Data for CASH IN BANK ACCOUNT
            acct$ =  cashacct$ : gosub test_acct
            if errormsg$ <> " " then return
                cashacct$ = acct$  :  cashacctdescr$ = descr$
                return

L51060
*        Test Data for CASH DISCOUNTS ACCOUNT
            acct$ =  discacct$ : gosub test_acct
            if errormsg$ <> " " then return
                discacct$ = acct$  :  discacctdescr$ = descr$
                return

L51120
*        Test Data for UNALLOWED DISCOUNTS ACCOUNT
            acct$ =  unaldisc$ : gosub test_acct
            if errormsg$ <> " " then return
                unaldisc$ = acct$  :  unaldiscdescr$ = descr$
                return


        test_acct
            call "GETCODE" (#3, acct$, descr$, 0%, 0.30, f1%(3))
            if f1%(3) = 1% then return
                errormsg$ = "INVALID ACCOUNT NUMBER"
                return


L51260
*        Test WHICH INVOICE TYPES TO PRINT        PRNTTYPE$
            for t% = 1% to 10%
                if pos(" ACDFGMO" = str(prnttype$,t%,1)) <> 0% then L51310
                     errormsg$ = "Invalid Invoice Type entered"
                     return
L51310:     next t%
            return

L51350
*        Test Data for Allow Neg Qty Invoice Lines to Affect Inventory
            if neg_inv$ = "Y" or neg_inv$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
            return
            
L51320:   
*        Test Data for ESC ACCOUNT         AWD001
            acct$ =  escacct$ : gosub test_acct
            if errormsg$ <> " " then return
                escacct$ = acct$  :  escacctdescr$ = descr$
                return         

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Closes all the files currently open, and also displays    *~
            * a message (ONLY if in foreground) while linking to the    *~
            * next program.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")

            end
