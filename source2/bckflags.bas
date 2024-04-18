        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    CCC   K   K  FFFFF  L       AAA    GGG    SSS    *~
            *  B   B  C      K  K   F      L      A   A  G      S       *~
            *  BBBB   C      KKK    FFFF   L      AAAAA  G GGG   SSS    *~
            *  B   B  C      K  K   F      L      A   A  G   G      S   *~
            *  BBBB    CCC   K   K  F      LLLLL  A   A   GGG    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKFLAGS - Set switches for Order Processing Module.      *~
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
            * 05/13/88 ! Added flag for allowing SOs to be Deleted! JDH *~
            * 05/25/88 ! PRR fix at line 2260                     ! JDH *~
            * 02/17/89 ! Proj 7890206 Wording change; add new code! JIM *~
            * 08/08/89 ! PROJ 7880907 allow over/uner shipments   ! LAB *~
            * 11/01/89 ! PROJ 7880907 allow over/uner shipments   ! JDH *~
            * 03/12/90 ! Added 2nd screen and three new parameters! JEF *~
            *          !   for printing bin locations             !     *~
            * 05/24/91 ! PRR 11906 Acknowledgement @ Cr Release?  ! JIM *~
            * 05/28/91 ! ALLFREE.                                 ! JIM *~
            * 06/10/91 ! Wording change for bin location printing.! JBK *~
            * 01/22/92 ! PRR 10609 Default SO Demand Type.  PRRs  ! JDH *~
            *          !   11905,11249,11623 Credit Hold changes. !     *~
            * 06/05/92 ! PRR 12469 Auto Approve Export Schls flag.! JDH *~
            * 06/15/92 ! Added Credit Hold Override at order entry! JDH *~
            *          !  & Offset Days for due date and ship date! JDH *~
            * 03/01/94 ! PRR 13024. Close Order Auto-Option added.! JDH *~
            * 03/16/94 ! PRR 12840. Only use A/R in Credit Check. ! JDH *~
            *          ! Third screen and logical groupings.      !     *~
            * 08/09/94 ! Removed 'WANG'.                          ! JDH *~
            * 12/12/94 ! Added 3 New Precious Metal Surcharge Flgs! RJH *~
            * 03/09/95 ! Added 2 Print flags for Xref Parts.      ! RJH *~
            * 08/24/95 ! Fixed line count on screen 3.            ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                              /* NORMAL OLD VARIABLES       */~
            ask$(3)80,                   /* ASKUSER, of course         */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            grp_header$(7)79,            /* Group Labels - Display     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second screen line         */~
            pf$(3)79,                    /* PF Key Prompts             */~
            pfk$12                       /* PF Keys Available          */

        dim                                                              ~
            acknwldg$1,                  /* Print ACKs at BCKCRINP?    */~
            allocate$1,                  /* Allocate Inventory?        */~
            auto_app$1,                  /* Export Auto Approvals      */~
            bin_cnt$1,                   /* # of Bins to Print         */~
            binprt$1,                    /* Print Bins on Pick Lists   */~
            bol$1,                       /* Print BOLs at SO entry     */~
            class$1,                     /* BCKUPDTE:  Submittal Class */~
            closeso$1,                   /* allow close of so @ invoice*/~
            crhold$1,                    /* Put Order on Credit Hold?  */~
            crhold_aronly$1,             /* Use A/R Only for Cr Hold?  */~
            delete$,                     /* Allow Deletion of SOs      */~
            dem_type$1,                  /* Default SO Demand Type     */~
            discs$1,                     /* Show line discs on SO/INV  */~
            idle$3,                      /* BCKUPDTE:  Idle Time before*/~
            inc_loc$1,                   /* Include Loc. In Bin Search */~
            manadjre$1,                  /* Mandatory Adj Reason Code? */~
            offstdue$3,                  /* Offset days for due date   */~
            offstshp$3,                  /* Offset days for ship date  */~
            ov_crhld$1,                  /* Override Credit Hold       */~
            ovrship$1,                   /* overshipments allowed flag */~
            pct$5,                       /* overshipment % allowed     */~
            picklist$1,                  /* Print Pick List at SO ent? */~
            pm_inv$1,                    /* Precious Metal INV Flag    */~
            pm_on$1,                     /* Precious Metal ON Flag     */~
            pm_so$1,                     /* Precious Metal SO Flag     */~
            soassign$3,                  /* So Number Assignment       */~
            submit$3,                    /* BCKUPDTE:  Wait Time for   */~
            xref_cus$1,                  /* Print Customer Xref Part   */~
            xref_mnf$1                   /* Print Manufactur Xref Part */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            * #2  ! STORNAME ! Store Master File                        *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =     1, keylen =  20

            select #2,  "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3


        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, 0%, f2%(1),   0%, " ")
            call "OPENCHCK" (#2, 0%, f2%(2),   0%, " ")

            if f2%(1) = 0% then L09000
                call "ASKUSER" (2%, "SYSFILE2 MISSING",                  ~
                           "Unable to open the file SYSFILE2.",          ~
                           "Please correct before running this program.",~
                           "Press RETURN to return to menu.")
            goto exit_program


L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date  :  call "DATEFMT" (date$)

*        See if this User is a Data Base or Module Administrator
            call "CMSMACHK" ("BCK", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then L09160
            pf$(1) = "You must be a Data Base or SO Module Administrator"~
                     & " to run this program."
            call "ASKUSER" (0%, "SECURITY CHECK", " ", pf$(1), " ")
            goto exit_program

L09160
*        Set some variables
            str(line2$,62) = "BCKFLAGS: " & cms2v$

            grp_header$(1%) = "General Sales Order Behavior:"
            grp_header$(2%) = "Background Processing Options:"
            grp_header$(3%) = "Credit Control Options:"
            grp_header$(4%) = "Shipping/Scheduling Options:"
            grp_header$(5%) = "Printed Documents Options:"
            grp_header$(6%) = "Allocation and Demand Defaults:"
            grp_header$(7%) = "Precious Metal Surcharge Defaults:"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * No real input mode - just load defaults from disk.        *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$
            pg% = 0%

            gosub L30000   /* Load then Format stuff all nice and purty */


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg1
            pg% = 1%

            gosub'051(0%)
            lastfieldnr% = 0%
L11060:     gosub'111(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then gosub L30100
                  if keyhit%  =  5% then       editpg2
                  if keyhit%  =  9% then       mod_admin
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       exit_program
                  if keyhit% <>  0% then       L11060
L11110:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% > 5% and fieldnr% < 9% then editpg1
            if fieldnr% > 8% then fieldnr% = fieldnr% - 3%
            if fieldnr% < 1% or fieldnr% > 8% then editpg1
            if fieldnr%  = lastfieldnr% then editpg1
            gosub'051(fieldnr%)
                  if enabled% = 0% then L11190
L11160:     gosub'111(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11160
L11190:     gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11160
                  lastfieldnr% = fieldnr%
                  goto L11110

        REM *************************************************************~
            *         E D I T   M O D E   S E C O N D   P A G E         *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg2
            pg% = 2%

            gosub'052(0%)
            lastfieldnr% = 0%
L12090:     gosub'112(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then gosub L30100
                  if keyhit%  =  4% then       editpg1
                  if keyhit%  =  5% then       editpg3
                  if keyhit%  =  9% then       mod_admin
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       exit_program
                  if keyhit% <>  0% then       L12090
L12170:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% > 3% and fieldnr% < 6% then editpg2
            if fieldnr% > 5% then fieldnr% = fieldnr% - 2%
            if fieldnr% > 6% and fieldnr% < 9% then editpg2
            if fieldnr% > 8% then fieldnr% = fieldnr% - 2%
            if fieldnr% < 1% or fieldnr% > 9% then editpg2
            if lastfieldnr% = fieldnr% then editpg2

            gosub'052(fieldnr%)
                  if enabled% = 0% then L12250
L12220:     gosub'112(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12220
L12250:     gosub'152(fieldnr%)
                  if errormsg$ <> " " then L12220
                  lastfieldnr% = fieldnr%
                  goto L12170

        mod_admin    /* Allow maintenance of BCK Module Administrators */
            call "CMSMAINP" ("BCK", "Sales Orders")
            on pg% goto editpg1, editpg2, editpg3

        REM *************************************************************~
            *         E D I T   M O D E   T H I R D     P A G E         *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg3
            pg% = 3%

            gosub'053(0%)
            lastfieldnr% = 0%
L14090:     gosub'113(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then gosub L30100
                  if keyhit%  =  4% then       editpg2
                  if keyhit%  =  9% then       mod_admin
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       exit_program
                  if keyhit% <>  0% then       L14090
L14170:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% > 9% and fieldnr% < 13% then editpg3
            if fieldnr% > 12% then fieldnr% = fieldnr% - 3%
            if fieldnr% < 1% or fieldnr% > 11% then editpg3
            if lastfieldnr% = fieldnr% then editpg3

            gosub'053(fieldnr%)
                  if enabled% = 0% then L14250
L14220:     gosub'113(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L14220
L14250:     gosub'153(fieldnr%)
                  if errormsg$ <> " " then L14220
                  lastfieldnr% = fieldnr%
                  goto L14170

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L31000
            goto  exit_program_warning


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

                  on fieldnr% gosub L20270,         /* SO Number Assgign*/~
                                    L20320,         /* Allow Delete     */~
                                    L20370,         /* Offset Days      */~
                                    L20420,         /* Mand Adj Reason? */~
                                    L20470,         /* Allow closing    */~
                                    L20560,         /* Submit Wait      */~
                                    L20610,         /* Idle Time        */~
                                    L20660          /* Submit Class     */
                     return

L20270
*        Default/Enable for SO NUMBER ASSIGNMENT
            inpmessage$ = "'m' = Manual Only; 's' = by SO Store Code;" & ~
                          " -or- Store Number with sequence."
            return

L20320
*        Default/Enable for ALLOW DELETE
            inpmessage$ = "Enter 'Y' to allow Sales Orders and/or Line "&~
                "Items to be Deleted."
            return

L20370
*        Default/Enable for Offset Days for Due Date & Ship Date
            inpmessage$ = "Enter Offset Number of Days (0-999); Blank " &~
                          "for NO Defaults."
            return

L20420
*        Default/Enable for MANDATORY ADJUSTMENT REASON CODE
            inpmessage$ = "Enter 'Y' for Mandatory Adjustment Reason Co"&~
                "des."
            return

L20470
*        Default/Enable for ALLOW CLOSING SALES ORDER LINE ITEMS AT INV.
            inpmessage$ = "Enter 'Y' to allow Closing of Sales Order It"&~
                "ems @ Invoicing; 'A' for Auto-Close"
            return

L20560
*        Default/Enable for SUBIT WAIT TIME
            inpmessage$ = "Enter maximum number of seconds to wait"   &  ~
                          " for SO Update to come up."
            return

L20610
*        Default/Enable for IDLE TIME
            inpmessage$ = "How many minutes should SO Update remain"  &  ~
                          " idle before it should remove itself?"
            return

L20660
*        Default/Enable for SUBMITTAL CLASS
            inpmessage$ = "Enter the procedure submit class (or"  &      ~
                          " '*' to not submit)."
            return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES fields for the Page 2 of Input. *~
            *************************************************************

            deffn'052(fieldnr%)
            if fieldnr% <> 0% then L22110
                inpmessage$ = "Place cursor at field to modify and" &    ~
                " Press (RETURN); PF-32 to EXIT program."
                return
L22110:     enabled% = 1%

                  on fieldnr% gosub L22180,         /* Credit Hold      */~
                                    L22240,         /* CrHold Only A/R  */~
                                    L22290,         /* Override Cr Hold */~
                                    L22335,         /* Allow overship   */~
                                    L22355,         /* Ovrshp System %  */~
                                    L22400,         /* Auto Approvals   */~
                                    L22450,         /* PM ON Flag       */~
                                    L22500,         /* PM SO Flag       */~
                                    L22550          /* PM INV Flag      */
                     return

L22180
*        Default/Enable for CREDIT HOLD
            inpmessage$ =                                                ~
              "'N'=No Auto-Hold; 'Y'=Auto-Hold; 'O'=Auto-Hold w/" &      ~
              "Override Manual Hold/Release"
            return

L22240
*        Default/Enable for CREDIT HOLD USE ONLY A/R
            inpmessage$ = "'Y'= Use only A/R Balance when Checking " &   ~
                          "Credit Limit Intrusion; 'N'= A/R+Sales"
            return

L22290
*        Default/Enable for Override Credit Holds at Order Entry
            inpmessage$ = "Enter 'Y' to Allow Override of Credit Hold " &~
                          "Status at Order Entry."
            return

L22335
*        Default/Enable for ALLOW OVERSHIPMENT AT SHIPPING
            inpmessage$ = "Enter 'Y' to allow Overshipment at Shipping."
            return

L22355
*        Default/Enable for SYSTEM DEFAULT OVERSHIPMENT %
            inpmessage$ = "Enter Percent of Overshipment Allowed."
            if ovrship$ = "N" then enabled% = 0%
            return

L22400
*        Default/Enable for Auto Approvals of Export Schedules
            inpmessage$ = "Enter 'Y' if Export Schedules are to be " &   ~
                          "Automatically Approved."
            return

L22450
*        Default/Enable for Precious Metal Sub-system ON Flag
            inpmessage$ = "Enter 'Y' if Precious Metal Surcharge Su" &   ~
                          "b-System is to be Active."
            return

L22500
*        Default/Enable for Precious Metal Surcharge at SO Flag
            inpmessage$ = "Enter 'Y' if Precious Metal Surcharge ar" &   ~
                          "e to be Added at SO Entry."
            return

L22550
*        Default/Enable for Precious Metal Surcharge at INV Flag
            inpmessage$ = "Enter 'Y' if Precious Metal Surcharge ar" &   ~
                          "e to be Updated at Invoicing."
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES fields for the Page 3 of Input. *~
            *************************************************************

            deffn'053(fieldnr%)
            if fieldnr% <> 0% then L24110
                inpmessage$ = "Place cursor at field to modify and" &    ~
                " Press (RETURN); PF-32 to EXIT program."
                return
L24110:     enabled% = 1%

                  on fieldnr% gosub L24210,         /* Pick List Print  */~
                                    L24220,         /* Print Bin on Pick*/~
                                    L24270,         /* Include Locations*/~
                                    L24320,         /* # of Bins to Prnt*/~
                                    L24380,         /* BOL Print        */~
                                    L24420,         /* Acknowledgment?  */~
                                    L24460,         /* Show Discounts   */~
                                    L24500,         /* Print Cust Xref  */~
                                    L24535,         /* Print Manuf Xref */~
                                    L24570,         /* Allocate Invntry */~
                                    L24800          /* Default Dem Type */
                     return

L24210
*        Default/Enable for PICK LIST PRINT
            inpmessage$ = "'Y'= Print, 'N'= Don't Print."
            return

L24220
*        Default/Enable for PRINT BINS ON PICK LISTS
            inpmessage$ = "N=No; Y=Inv. Master Loc.; Q=Inv. Qty Loc.;" & ~
                          " A=ALL Loc.; M=Inv. Master or Qty Loc"
            return

L24270
*        Default/Enable for INCLUDE LOCATIONS
            inpmessage$ = "Should Warehouse Locations be Included in " & ~
                          "Search for Bin Locations? (Y/N)"
            return

L24320
*        Default/Enable for NUMBER OF BINS TO PRINT
            inpmessage$ = "Enter Maximum # of Bin Locations to print " & ~
                          "on Pick Lists, etc. (0-9)"
            if bin_cnt$ = " " then bin_cnt$ = "0"
            return

L24380
*        Default/Enable for BOL PRINT
            inpmessage$ = "'Y'= Print, 'N'= Don't Print."
            return

L24420
*        Default/Enable for Acknowledgement at Credit Release?
            inpmessage$ = "'Y'= Print, 'N'= Don't Print."
            return

L24460
*        Default/Enable for SHOW DISCOUNTS
            inpmessage$ = "Enter 'Y' to show line item discounts sep" &  ~
                          "arately on orders and invoices."
            return

L24500
*        Default/Enable for XREF_CUS$
            inpmessage$ =                                                ~
              "'N'=Print CMS Only, 'Y'=Print Cust Xref Parts, 'B'=Print B~
        ~oth Xref & CMS"
             return

L24535
*        Default/Enable for XREF_MNF$
            inpmessage$ =                                                ~
              "'N'=Print CMS Only, 'Y'=Print Mnftr Xref Parts, 'B'=Print ~
        ~Both Xref & CMS"
             return

L24570
*        Default/Enable for ALLOCATE
            inpmessage$ =                                                ~
              "'N'= Don't allocate; 'A'= Allocate from ATC; 'C'= Alloca"&~
              "te Complete."
            return

L24800
*        Default/Enable for Default SO Demand Type
            inpmessage$ = "Enter '1' = Nettable, '2' = Non-Nettable"
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

        call "READ100" (#1, "SWITCHS.BCK         ", f1%(1))
        if f1%(1) = 1% then L30490

*        No record on file or User wants to reset to system defaults
L30100:  e1% = 0%  : if keyhit% = 2% then e1% = -1% /* Return Dflts */
         on pg% goto L30120, L30280, L30370
L30120:  e%  = e1% : call "BCKSWTCH" ("BCK", "SBMTWAIT", submit$  , e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "SBMTCLAS", class$   , e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "IDLETIME", idle$    , e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "SOASSIGN", soassign$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "DELETE  ", delete$  , e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "MANADJRE", manadjre$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "OFFSTDUE", offstdue$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "CLOSESO ", closeso$ , e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "OFFSTSHP", offstshp$, e, e%)
         goto L30470
L30280:  e%  = e1% : call "BCKSWTCH" ("BCK", "CRHOLDAR", crhold_aronly$, ~
                                                                    e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "CRHOLD  ", crhold$  , e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "XAUTOAPP", auto_app$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "OVRRIDCR", ov_crhld$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "OVRSHIP ", ovrship$ , e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "OVRSHIP%", pct$     , e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "PM_ON   ", pm_on$   , e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "PM_SO   ", pm_so$   , e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "PM_INV  ", pm_inv$  , e, e%)
         goto L30470
L30370:  e%  = e1% : call "BCKSWTCH" ("BCK", "BINPRINT", binprt$  , e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "INC_LOC ", inc_loc$ , e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "PRNTNUMB", bin_cnt$ , e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "DEM_TYPE", dem_type$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "ALLOCATE", allocate$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "PICKLIST", picklist$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "BOL     ", bol$     , e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "ACKNWLDG", acknwldg$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "DISCS   ", discs$   , e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "XREF_CUS", xref_cus$, e, e%)
         e%  = e1% : call "BCKSWTCH" ("BCK", "XREF_MNF", xref_mnf$, e, e%)
L30470: return

L30490:     get #1 using L30560, submit%, class$, idle%, soassign$,       ~
                                crhold$, allocate$, picklist$, bol$,     ~
                                discs$, delete$, manadjre$, ovrship$,    ~
                                closeso$, pct, binprt$, inc_loc$,        ~
                                bin_cnt%, acknwldg$, dem_type$,          ~
                                auto_app$, ov_crhld$, offstdue$,         ~
                                offstshp$, crhold_aronly$, pm_on$,       ~
                                pm_so$, pm_inv$, xref_cus$, xref_mnf$
L30560:         FMT XX(20), BI(2), CH(1), BI(2), CH(3), 9*CH(1),         ~
                    PD(14,4), CH(1), CH(1), BI(1), CH(1), CH(1), CH(1),  ~
                    CH(1), 2*CH(3), CH(1), 3*CH(1), 2*CH(1)
            convert submit% to submit$, pic(##0)
            convert idle%   to idle$  , pic(##0)
            if manadjre$ = " " then manadjre$ = "N"
            if ovrship$ = " " then ovrship$ = "N"
            if closeso$ = " " then closeso$ = "N"
            call "CONVERT" (pct, -1.2, pct$)
            if binprt$ = hex(00) then binprt$ = " "
            if binprt$ = " " then binprt$ = "N"
            if inc_loc$ = hex(00) then inc_loc$ = " "
            if inc_loc$ = " " then inc_loc$ = "N"
            convert bin_cnt% to bin_cnt$, pic(0)
            if acknwldg$ = hex(00) then acknwldg$ = " "
            if acknwldg$ = " " then acknwldg$ = "N"
            if dem_type$ = hex(00) then dem_type$ = " "
            if dem_type$ = " " then dem_type$ = "1"
            if auto_app$ = " " then auto_app$ = "N"
            if ov_crhld$ = " " then ov_crhld$ = "N"
            if crhold_aronly$ = " " then crhold_aronly$ = "N"
            return

L31000: REM *************************************************************~
            *                  S A V E   D A T A                        *~
            * --------------------------------------------------------- *~
            * Write switchs back to file                                *~
            *************************************************************

            submit%, idle% = 100%
            bin_cnt% = 0%
            convert submit$ to submit%, data goto L31080
L31080:     convert idle$   to idle%  , data goto L31082
L31082:     convert bin_cnt$ to bin_cnt%, data goto L31090
L31090:     call "READ101" (#1, "SWITCHS.BCK", f1%(1))
            put #1 using L31140, "SWITCHS.BCK", submit%, class$, idle%,   ~
                                soassign$, crhold$, allocate$,           ~
                                picklist$, bol$, discs$, delete$,        ~
                                manadjre$, ovrship$, closeso$, pct,      ~
                                binprt$, inc_loc$, bin_cnt%, acknwldg$,  ~
                                dem_type$, auto_app$, ov_crhld$,         ~
                                offstdue$, offstshp$, crhold_aronly$,    ~
                                pm_on$, pm_so$, pm_inv$, xref_cus$,      ~
                                xref_mnf$, " ", " "
L31140:        FMT CH(20), BI(2), CH(1), BI(2), CH(3), 9*CH(1), PD(14,4),~
                   CH(1), CH(1), BI(1), CH(1), CH(1), CH(1), CH(1),      ~
                   2*CH(3), CH(1), 3*CH(1), 2*CH(1), CH(216), CH(220)
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

                  on fieldnr% gosub L41135,         /* SO Number Assgign*/~
                                    L41150,         /* Allow Delete     */~
                                    L41150,         /* Offset Days      */~
                                    L41150,         /* Mand Adj Reason  */~
                                    L41150,         /* Close SO @ Inv.  */~
                                    L41165,         /* Submit Wait      */~
                                    L41165,         /* Idle Time        */~
                                    L41150          /* Submit Class     */
                     goto L41185

L41135:           REM Set FAC'S for UPPER/LOWER CASE input
                      lfac$(fieldnr%) = hex(80)
                      return
L41150:           REM Set FAC'S for UPPER CASE ONLY input
                      lfac$(fieldnr%) = hex(81)
                      return
L41165:           REM Set FAC'S for NUMERIC ONLY input
                      lfac$(fieldnr%) = hex(82)
                      return

L41185:   accept                                                         ~
            at (01,02), "Manage SO Module Behavior Switches - Page 1",   ~
            at (01,67), "Date:",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (03,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (05,02), fac(hex(ac)), grp_header$(1%)           , ch(79),~
            at (06,03),                                                  ~
             "SO Number Assignment (m/s/Store Number)                  ",~
            at (06,70), fac(lfac$(1%)), soassign$               , ch(03),~
            at (07,03),                                                  ~
             "Allow Sales Order Deletions? (Y/N)                       ",~
            at (07,70), fac(lfac$(2%)), delete$                 , ch(01),~
            at (08,03),                                                  ~
             "Offset Number of Days for Default Due Date and Ship Date?",~
            at (08,70), fac(lfac$(3%)), offstdue$               , ch(03),~
            at (08,75), fac(lfac$(3%)), offstshp$               , ch(03),~
            at (09,03),                                                  ~
             "Mandatory Adjustment Reason Codes? (Y/N)                 ",~
            at (09,70), fac(lfac$(4%)), manadjre$               , ch(01),~
            at (10,03),                                                  ~
             "Allow Closing Sales Order Line Items at Invoicing? (Y/N/A)"~
        ,                                                                ~
            at (10,70), fac(lfac$(5%)), closeso$                , ch(01),~
            at (13,02), fac(hex(ac)), grp_header$(2%)           , ch(79),~
            at (14,03),                                                  ~
             "Time out for Background Task Submit (seconds) ",           ~
            at (14,70), fac(lfac$(6%)), submit$                 , ch(03),~
            at (15,03),                                                  ~
             "Idle time until task ends itself (minutes)    ",           ~
            at (15,70), fac(lfac$(7%)), idle$                   , ch(03),~
            at (16,03),                                                  ~
             "Background Procedure Submittal Class (A-Z)    ",           ~
            at (16,70), fac(lfac$(8%)), class$                  , ch(01),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)), pf$(1)                     ,ch(79),~
            at (23,02), fac(hex(8c)), pf$(2)                     ,ch(79),~
            at (24,02), fac(hex(8c)), pf$(3)                     ,ch(79),~
                                                                         ~
                keys(pfk$),                                              ~
                key (keyhit%)

                if keyhit% <> 13 then L41480
                     call "MANUAL" ("BCKFLAGS")
                     goto L41185

L41480:         if keyhit% <> 15 then L41500
                     call "PRNTSCRN"
                     goto L41185

L41500:         close ws
                call "SCREEN" addr("C", n%, "I", i$(), cursor%())
                return

        set_pf_keys
          if fieldnr% <> 0% then L42060
            pf$(1%) = "(1)Start Over       (4)Previous Screen  " &       ~
                     "(5)Next Screen         (13)Instructions"
            pf$(2%) = "                    (9)Maintain SO Modul" &       ~
                     "e Administrators       (15)Print Screen"
            pf$(3%) = "(2)Reset using Defaults                 " &       ~
                     "                       (16)SAVE DATA   "
            str(pf$(3%),63%,1%) = hex(84)
            pfk$ = hex(0001020405090d0f1020ffff)
            if pg% < 2% then str(pf$(1%),21%,18%) = " "
            if pg% < 2% then str(pfk$,4%,1%) = hex(ff)
            if pg% > 2% then str(pf$(1%),41%,14%) = " "
            if pg% > 2% then str(pfk$,5%,1%) = hex(ff)
            return

L42060:     pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                                       "
            pfk$ = hex(00010d0fffffffffffff)
            return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   2       *~
            * --------------------------------------------------------- *~
            * Screen for Editing Page 2 of document.                    *~
            *************************************************************

        deffn'112(fieldnr%)
            if fieldnr% = 0% then init(hex(86)) lfac$()  else            ~
                                  init(hex(8c)) lfac$()
            gosub set_pf_keys

                  on fieldnr% gosub L43190,         /* Credit Hold      */~
                                    L43190,         /* CrHold A/R Only  */~
                                    L43190,         /* Override Cr Hold */~
                                    L43190,         /* Allow Overship   */~
                                    L43220,         /* Overship %       */~
                                    L43190,         /* Auto Ex Approvals*/~
                                    L43190,         /* PM ON Flag       */~
                                    L43190,         /* PM SO Flag       */~
                                    L43190          /* PM INV Flag      */
                     goto L43260

                  REM Set FAC'S for UPPER/LOWER CASE input
                      lfac$(fieldnr%) = hex(80)
                      return
L43190:           REM Set FAC'S for UPPER CASE ONLY input
                      lfac$(fieldnr%) = hex(81)
                      return
L43220:           REM Set FAC'S for NUMERIC ONLY input
                      lfac$(fieldnr%) = hex(82)
                      return

L43260:   accept                                                         ~
            at (01,02), "Manage SO Module Behavior Switches - Page 2",   ~
            at (01,67), "Date:",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02), fac(hex(ac)), grp_header$(3%)           , ch(79),~
            at (07,03),                                                  ~
             "Place Orders over Credit Limit automatically on CR Hold? (Y~
        ~/N/O)",                                                          ~
            at (07,70), fac(lfac$(1%)), crhold$                 , ch(01),~
            at (08,03),                                                  ~
             "Use ONLY A/R Balance when Calculating Credit Limit Intrusio~
        ~n?",                                                             ~
            at (08,70), fac(lfac$(2%)), crhold_aronly$          , ch(01),~
            at (09,03),                                                  ~
             "Allow Override of Credit Hold at Order Entry? (Y/N)      ",~
            at (09,70), fac(lfac$(3%)), ov_crhld$               , ch(01),~
            at (11,02), fac(hex(ac)), grp_header$(4%)           , ch(79),~
            at (12,03),                                                  ~
             "Allow Overshipment at Shipping? (Y/N)                    ",~
            at (12,70), fac(lfac$(4%)), ovrship$                , ch(01),~
            at (13,03), "Allowable Overshipment Percent",                ~
            at (13,70), fac(lfac$(5%)), pct$                    , ch(05),~
            at (14,03),                                                  ~
             "Are Export Schedules to be Auto-Approved? (Y/N)          ",~
            at (14,70), fac(lfac$(6%)), auto_app$               , ch(01),~
            at (16,02), fac(hex(ac)), grp_header$(7%)           , ch(79),~
            at (17,03),                                                  ~
             "Precious Metal Sub-System ON? (Y/N)                      ",~
            at (17,70), fac(lfac$(7%)), pm_on$                  , ch(01),~
            at (18,03),                                                  ~
             "Precious Metal Surcharge at SO Entry? (Y/N)              ",~
            at (18,70), fac(lfac$(8%)), pm_so$                  , ch(01),~
            at (19,03),                                                  ~
             "Precious Metal Surcharge Update at Invoicing? (Y/N)      ",~
            at (19,70), fac(lfac$(9%)), pm_inv$                 , ch(01),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)), pf$(1)                     ,ch(79),~
            at (23,02), fac(hex(8c)), pf$(2)                     ,ch(79),~
            at (24,02), fac(hex(8c)), pf$(3)                     ,ch(79),~
                                                                         ~
                keys(pfk$),                                              ~
                key (keyhit%)

                if keyhit% <> 13 then L43550
                     call "MANUAL" ("BCKFLAGS")
                     goto L43260

L43550:         if keyhit% <> 15 then L43590
                     call "PRNTSCRN"
                     goto L43260

L43590:         close ws
                call "SCREEN" addr("C", n%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   3       *~
            * --------------------------------------------------------- *~
            * Screen for Editing Page 3 of document.                    *~
            *************************************************************

        deffn'113(fieldnr%)
            if fieldnr% = 0% then init(hex(86)) lfac$()  else            ~
                                  init(hex(8c)) lfac$()
            gosub set_pf_keys

                  on fieldnr% gosub L45230,         /* Pick List Print  */~
                                    L45230,         /* Print Bin Locatns*/~
                                    L45230,         /* Include Locations*/~
                                    L45230,         /* # of Bins to Prnt*/~
                                    L45230,         /* BOL Print        */~
                                    L45230,         /* Acknowledgement? */~
                                    L45230,         /* Discounts        */~
                                    L45230,         /* Xref Cust Part   */~
                                    L45230,         /* Xref Mnfctur Part*/~
                                    L45230,         /* Allocate Invntry */~
                                    L45230          /* Default Dem Type */
                     goto L45300

                  REM Set FAC'S for UPPER/LOWER CASE input
                      lfac$(fieldnr%) = hex(80)
                      return
L45230:           REM Set FAC'S for UPPER CASE ONLY input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC'S for NUMERIC ONLY input
                      lfac$(fieldnr%) = hex(82)
                      return

L45300:   accept                                                         ~
            at (01,02), "Manage SO Module Behavior Switches - Page 3",   ~
            at (01,67), "Date:",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (03,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (05,02), fac(hex(ac)), grp_header$(5%)           , ch(79),~
            at (06,03),                                                  ~
             "Print Pick Lists at time of Order entry? (Y/N)           ",~
            at (06,70), fac(lfac$(1%)), picklist$               , ch(01),~
            at (07,03),                                                  ~
             "Source of Locations Data Printed on Ship Pick Lists?     ",~
            at (07,70), fac(lfac$(2%)), binprt$                 , ch(01),~
            at (08,03),                                                  ~
             "Warehouse Locations to be Included in Search for Bins? (Y/N~
        ~)",                                                              ~
            at (08,70), fac(lfac$(3%)), inc_loc$                , ch(01),~
            at (09,03),                                                  ~
             "Maximum # of Bin Locations to Print on Pick Lists?       ",~
            at (09,70), fac(lfac$(4%)), bin_cnt$                , ch(01),~
            at (10,03),                                                  ~
             "Print Bill of Lading at time of Order entry? (Y/N)       ",~
            at (10,70), fac(lfac$(5%)), bol$                    , ch(01),~
            at (11,03), "Print Acknowledgements at Release of Credit Hold~
        ~? (Y/N)",                                                        ~
            at (11,70), fac(lfac$(6%)), acknwldg$               , ch(01),~
            at (12,03),                                                  ~
             "Show line discount separately on Orders and Invoices? (Y/N)~
        ~",                                                               ~
            at (12,70), fac(lfac$(7%)), discs$                  , ch(01),~
                                                                         ~
            at (13,03),                                                  ~
             "When Available Print Customer Cross-reference Part? (Y/N/B)~
        ~",                                                               ~
            at (13,70), fac(lfac$(8%)), xref_cus$               , ch(01),~
            at (14,03),                                                  ~
             "When Available Print Manufctr Cross-reference Part? (Y/N/B)~
        ~",                                                               ~
            at (14,70), fac(lfac$(9%)), xref_mnf$               , ch(01),~
                                                                         ~
            at (17,02), fac(hex(ac)), grp_header$(6%)           , ch(79),~
            at (18,03),                                                  ~
             "Allocate Inventory at Sales Order Posting Time? (N/A/C)  ",~
            at (18,70), fac(lfac$(10%)), allocate$              , ch(01),~
            at (19,03),                                                  ~
             "Default Sales Order Demand Type ('1' or '2')?            ",~
            at (19,70), fac(lfac$(11%)), dem_type$              , ch(01),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)), pf$(1%)                    ,ch(79),~
            at (23,02), fac(hex(8c)), pf$(2%)                    ,ch(79),~
            at (24,02), fac(hex(8c)), pf$(3%)                    ,ch(79),~
                                                                         ~
                keys(pfk$),                                              ~
                key (keyhit%)

                if keyhit% <> 13 then L45700
                     call "MANUAL" ("BCKFLAGS")
                     goto L45300

L45700:         if keyhit% <> 15 then L45720
                     call "PRNTSCRN"
                     goto L45300

L45720:         close ws
                call "SCREEN" addr("C", n%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 1.                        *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50220,         /* SO Number Assgign*/~
                                    L50290,         /* Allow Delete     */~
                                    L50335,         /* Offset Days      */~
                                    L51050,         /* Mand Adj Reason  */~
                                    L51150,         /* Allow Closing    */~
                                    L50550,         /* Submit Wait      */~
                                    L50620,         /* Idle Time        */~
                                    L50690          /* Submit Class     */
                     return

L50220
*        Test Data for SO NUMBER ASSIGNMENT
            if soassign$ = "m" or soassign$ = "s" then return
            call "READ100" (#2, soassign$, f1%(2))
            if f1%(2) = 1% then return
                errormsg$ = "Enter 'm', 's' or a valid Store code."
                return

L50290
*        Test Data for ALLOW DELETE
            if delete$ = "Y" or delete$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L50335
*        Test Data for Offset # of Days for Default Due Date & Ship Date
            if offstdue$ = " " then L50385
                convert offstdue$ to temp, data goto L50365
                if temp < 0 then L50375
                call "CONVERT" (temp, 0.0, offstdue$)
                goto L50385
L50365:              errormsg$ = "Offset for Due Date in error."
                     return
L50375:              errormsg$ = "Offset Can't be a Negative Number."
                     return
L50385:     if offstshp$ = " " then return
                convert offstshp$ to temp, data goto L50410
                if temp < 0 then L50375
                call "CONVERT" (temp, 0.0, offstshp$)
                goto L50420
L50410:              errormsg$ = "Offset for Ship Date in error."
                     return
L50420:     if offstdue$ = " " then return  /* At least one blank date */
                if offstdue$ >= offstshp$ then return
                     errormsg$ = "Ship Date Offset must be equal to or "&~
                                 "less than the Due Date Offset."
                     return

L50550
*        Test Data for SUBMIT WAIT
            convert submit$ to n%, data goto L50570  :  goto L50580
L50570:         errormsg$ = "Allowable range: 20 - 480." : return
L50580:     if n% < 20% or n% > 480% then L50570
            convert n% to submit$, pic(##0)
            return

L50620
*        Test Data for IDLE TIME BEFORE CANCEL
            convert idle$ to n%, data goto L50640  :  goto L50650
L50640:         errormsg$ = "Allowable range: 10 - 999." : return
L50650:     if n% < 10% or n% > 999% then L50640
            convert n% to idle$, pic(##0)
            return

L50690
*        Test Data for SUBMITTAL CLASS
            if (class$ >= "A" and class$ <= "Z") or class$ = "*"         ~
                                                            then return
                errormsg$ = "Class must be letter between A & Z OR '*'."
                return

L51050
*        Test Data for MANDATORY ADJUSTMENT REASON CODE
            if manadjre$ = "Y" or manadjre$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L51150
*        Test Data for ALLOW CLOSING SALES ORDER LINE ITEMS AT INVOICE
            if closeso$ = "Y" or closeso$ = "N" then return
            if closeso$ = "A" then return
                errormsg$ = "Enter 'Y', 'N' or 'A'."
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 2.                        *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52130,         /* Credit Hold      */~
                                    L52180,         /* CrHold Only A/R  */~
                                    L52230,         /* Override Cr Hold */~
                                    L52280,         /* Allow Overship   */~
                                    L52330,         /* Overship %       */~
                                    L52600,         /* Auto Approvals   */~
                                    L52650,         /* PM ON Flag       */~
                                    L52700,         /* PM SO Flag       */~
                                    L52750          /* PM INV Flag      */
                     return

L52130
*        Test Data for CREDIT HOLD
            if pos("YNO" = crhold$) <> 0 then return
                errormsg$ = "Enter 'Y'es, 'O'verride, or 'N'o."
            return

L52180
*        Test Data for Only Using A/R Balances in Credit Control
            if pos("YN" = crhold_aronly$) > 0 then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L52230
*        Test Data for Override Credit Hold
            if pos("YN" = ov_crhld$) > 0 then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L52280
*        Test Data for ALLOW OVERSHIPMENTS AT SHIPPING
            if ovrship$ = "Y" or ovrship$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L52330
*        Test Data for ALLOWABLE OVERSHIPMENT PERCENT
            if pct$ = " " then pct$ = "0"
            call "NUMTEST" (pct$, 0, 999, errormsg$, 1.2, pct)
                if  errormsg$ <> " " then return
            return

L52600
*        Test Data for Export Schedules Auto-Approvals
            if pos("YN" = auto_app$) > 0 then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L52650
*        Test Data for Precious Metal Surcharge Sub-system ON
            if pos("YN" = pm_on$) > 0 then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L52700
*        Test Data for Precious Metal Surcharge at SO Entry
            if pos("YN" = pm_so$) > 0 then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L52750
*        Test Data for Precious Metal Surcharge at Invoice Update
            if pos("YN" = pm_inv$) > 0 then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 3.                        *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L53160,         /* Pick List Print  */~
                                    L53170,         /* Print Bins       */~
                                    L53220,         /* Include Locations*/~
                                    L53270,         /* # of Bins to prnt*/~
                                    L53330,         /* Print BOL        */~
                                    L53380,         /* Acknowledgment?  */~
                                    L53430,         /* Show Discounts   */~
                                    L53650,         /* Xref Customer Prt*/~
                                    L53700,         /* Xref Manufctr Prt*/~
                                    L53480,         /* Allocation Deflt */~
                                    L53600          /* Default Dem Type */
                     return

L53160
*        Test Data for PICK LIST PRINTING
            if picklist$ = "Y" or picklist$ = "N" then return
                errormsg$ = "Enter 'Y' -or- 'N'."
                return

L53170
*        Test Data for PRINT BIN LOCATONS ON PICK LIST
            if pos("YNAQM" = binprt$) <> 0 then return
            errormsg$ = "Please Enter 'Y', 'N', 'A', 'Q', or 'M'."
            return

L53220
*        Test Data for INCLUDE LOCATIONS IN BIN # SEARCH
            if pos("YN" = inc_loc$) > 0 then return
            errormsg$ = "Enter 'Y' or 'N'."
            return

L53270
*        Test Data for MAXIMUM # of BINS TO PRINT ON PICK LIST
            convert bin_cnt$ to bin_cnt%, data goto L53300
            if bin_cnt% > -1% and bin_cnt% < 11% then return
L53300:          errormsg$ = "Please Enter a number from 0 thru 9."
                 return

L53330
*        Test Data for BOL PRINTING
            if bol$ = "Y" or bol$ = "N" then return
                errormsg$ = "Enter 'Y' -or- 'N'."
                return

L53380
*        Test Data for Print Acknowledgement at Credit Release?
            if acknwldg$ = "Y" or acknwldg$ = "N" then return
                errormsg$ = "Enter 'Y' -or- 'N'."
                return

L53430
*        Test Data for DISCOUNTS PRINTING
            if discs$ = "Y" or discs$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L53480
*        Test Data for ALLOCATE INVENTORY
            if allocate$ = "N" or allocate$ = "A"                        ~
                               or allocate$ = "C" then return
                errormsg$ = "Enter 'A', 'C' or 'N'."
                return

L53600
*        Test Data for Default SO Demand Type
            if pos("12" = dem_type$) <> 0 then return
                 errormsg$ = "Please Enter '1' or '2'."
                 return

L53650
*        Test Data for XREF_CUS$
            if xref_cus$ = "N" or xref_cus$ = "Y"                        ~
                               or xref_cus$ = "B" then return
                errormsg$ = "Enter 'Y', 'N' or 'B'."
                return

L53700
*        Test Data for XREF_CUS$
            if xref_mnf$ = "N" or xref_mnf$ = "Y"                        ~
                               or xref_mnf$ = "B" then return
                errormsg$ = "Enter 'Y', 'N' or 'B'."
                return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
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

        exit_program_warning
            ask% = 2%
            ask$()  = " "
            ask$(1%)= "Changes to FLAGS will not impact tasks (" &       ~
                      "BACKGROUND or FOREGROUND)"
            ask$(2%)= "currently executing.  They must be termi" &       ~
                      "nated and re-initiated."
            ask$(3%)= "Press any PF key to continue."

            call "ASKUSER" (ask%, "* * * NOTE * * *",                    ~
                            ask$(1%), ask$(2%), ask$(3%))

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "ALLFREE"
            end
