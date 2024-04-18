        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  V   V  BBBB   K   K  FFFFF  L       AAA    GGG    SSS    *~
            *  V   V  B   B  K  K   F      L      A   A  G      S       *~
            *  V   V  BBBB   KKK    FFFF   L      AAAAA  G GGG   SSS    *~
            *   V V   B   B  K  K   F      L      A   A  G   G      S   *~
            *    V    BBBB   K   K  F      LLLLL  A   A   GGG    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VBKFLAGS - Set switchs for Purchase Order Module.         *~
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
            * 12/26/85 ! ORIGINAL                                 ! ERN *~
            * 08/05/87 ! Fixed Error Message to Reflect 99999     ! DAW *~
            * 05/24/88 ! Spelling errors                          ! MJB *~
            * 03/31/88 ! Added Switch controlling Recvr Tickets   ! MDE *~
            * 01/04/89 ! Added Switch for Reject Codes Required   ! MJB *~
            * 12/12/89 ! Added Switch for Posting POs at std cost ! JBK *~
            *          !    for Grace project #7890207            ! JBK *~
            * 01/29/92 ! PRR 10715 - Added Switch to Exclude QC in! MLJ *~
            *          !    Quantity to Pay default.              !     *~
            * 05/21/92 ! PRR 12028 - Added switches to print      ! MLJ *~
            *          !  Revision Date on PO and Print Received  !     *~
            *          !  and Net Due Qtys on PO.                 !     *~
            * 09/23/92 ! Added Receiver Ticket Size Option.       ! JDH *~
            * 02/23/93 ! Added Defer Cost at Directive time.      ! JDH *~
            * 04/19/94 ! PRR 12960. Added 'R' option to post @ std! JDH *~
            * 06/20/94 ! Add Vendor Service Flags                 ! RJH *~
            * 06/30/94 ! Changed PF Prompt from 'SO' to 'PO'.     ! ERN *~
            *          !  Clone-of-my heart I adore you...        !     *~
            * 11/18/94 ! Changed References of 'SO' to 'PO'.      ! LDJ *~
            *          ! Removed non-functional VSA Print Flag.   !     *~
            * 04/04/95 ! PRR 13378. Added switch to kit complete  ! JDH *~
            *          !   purchase jobs.                         !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            ask$(3)80,                   /* ASKUSER, of course         */~
            chnghistory$1,               /* Keep History of PO Changes */~
            class$1,                     /*            Submittal Class */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            defer_cost$1,                /* Defer costs at Directive   */~
            errormsg$79,                 /* Error message              */~
            helpdescr$78,                /* Help Description Message   */~
            i$(24)80,                    /* Screen Image               */~
            idle$3,                      /* PO UPDATE: Idle Time before*/~
            inpmessage$79,               /* Informational Message      */~
            iswas$1,                     /*              Show Is/Was   */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second screen line         */~
            pf$(3)79,                    /* PF Key Prompts             */~
            pfk$10,                      /* PF Keys Available          */~
            pj_kit$1,                    /* Kit PJs Complete?          */~
            poassign$3,                  /* Po Number Assignment       */~
            poprdate$1,                  /* PO Printing: Revision Date */~
            popropt$1,                   /* PO Print Options           */~
            poprpnd$1,                   /* PO Printing: Rcv'd, ND  Qty*/~
            posort$1,                    /* PO Printing: Output Seq    */~
            postdcst$1,                  /* PO posting at std cost     */~
            qtytopay$1,                  /* Qty to Pay Excludes QC     */~
            rcptslimit$5,                /* Percent Receipts Limit     */~
            rcvmode$1,                   /* Default Received Mode      */~
            rcvrs$1,                     /* Receiver Printing          */~
            rjcodes$1,                   /* Reject Codes Flag          */~
            submit$3,                    /*            Wait Time for   */~
            tkt_size$1,                  /* Receiver Ticket Size       */~
            vsa_actv$1,                  /* Vendor Service Active Flag */~
            vsa_gen$1                    /* Gen. Vendor Service Flag   */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.01 06/23/95 Patch finalization of R6.04.00  "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            * #2  ! STORNAME ! Store Master File                        *~
            * #50 ! HELPFILE ! Workfile for help                        *~
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

            select #50, "HELPFILE",                                      ~
                        varc,     indexed,  recsize =  80,               ~
                        keypos = 1, keylen = 1

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, 0%, f2%(1%), 100%, " ")
            call "OPENCHCK" (#2, 0%, f2%(1%),   0%, " ")

            call "WORKOPEN" (#50, "IO   ", 10%, 1%)
            write #50, using L02300, "N", "In Purchasing: Use the Purchas"&~
                                        "e Price as the Inventory Cost."
            write #50, using L02300, "Y", "In Purchasing: Use Standard Co"&~
                                        "st as the Inventory Cost."
            write #50, using L02300, "R", "In Receiving:  Use Standard Co"&~
                                        "st as the Inventory Cost."
            write #50, using L02300, "B", "Both Purchasing & Receiving: U"&~
                                        "se Std Cost (Combo 'R' & 'Y' b"&~
                                        "elow)"
L02300:         FMT CH(1), CH(79)

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date  :  call "DATEFMT" (date$)

*        See if this User is a Data Base or Module Administrator
            call "CMSMACHK" ("VBK", lfac$(1%), lfac$(2%))
            if lfac$(1%) = "Y" or lfac$(2%) = "Y" then L09160
            pf$(1%)= "You must be a Data Base or PO Module Administrator"~
                     & " to run this program."
            call "ASKUSER" (0%, "SECURITY CHECK", " ", pf$(1%), " ")
            goto exit_program

L09160
*        Set some variables
            str(line2$,62%) = "VBKFLAGS: " & cms2v$

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
        editpg1
            pg% = 1%

L11055:     gosub'051(0%)                /* Set Input Messages         */
L11060:     gosub'111(0%)                /* Edit Screen                */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then gosub L30080  /* Re-set w/Dflts */
                  if keyhit%  =  5% then       editpg2
                  if keyhit%  =  9% then       mod_admin
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       exit_program
                  if keyhit% <>  0% then       L11060
            fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 17% then L11060

            gosub'051(fieldnr%)          /* Set Input Messages         */
                  if enabled% = 0% then L11190
L11160:     gosub'111(fieldnr%)          /* Edit Screen                */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11160
L11190:     gosub'151(fieldnr%)          /* Test Data Entered          */
                  if errormsg$ <> " " then L11160
            goto L11055


        mod_admin    /* Allow maintenance of VBK Module Administrators */
            call "CMSMAINP" ("VBK", "Purchase Orders")
            goto L11000


        REM *************************************************************~
            *         E D I T   M O D E   S E C O N D   P A G E         *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg2
            pg% = 2%

L12080:     gosub'052(0%)
L12090:     gosub'112(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then gosub L30100
                  if keyhit%  =  4% then      editpg1
                  if keyhit%  =  9 then       mod_admin
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 32 then       exit_program
                  if keyhit% <>  0 then       L12090
L12170:     fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% > 3% then L12090

            gosub'052(fieldnr%)
                  if enabled% = 0% then L12250
L12220:     gosub'112(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12220
L12250:     gosub'152(fieldnr%)
                  if errormsg$ <> " " then L12220
                  if fieldnr% = cursor%(1) - 5% then L12080 else L12170

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L31000                  /* Save Data                  */
            goto  exit_program_warning


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES fields for the Page 1 of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
            if fieldnr% <> 0% then L20035
                inpmessage$ = "Place cursor at field to modify and" &    ~
                " Press (RETURN); PF-32 to EXIT program."
                return
L20035:     enabled% = 1%

                  on fieldnr% gosub L20100,         /* PO Number Assgign*/~
                                    L20200,         /* Percent Rcpts Lmt*/~
                                    L20300,         /* Changes History  */~
                                    L20350,         /* PO Print Option  */~
                                    L20400,         /* PO Sorting       */~
                                    L20500,         /* Show Is/Was      */~
                                    L20550,         /* Revision Date    */~
                                    L20575,         /* Rcv'd, Net D Qtys*/~
                                    L20600,         /* Default Received */~
                                    L20700,         /* Submit Wait      */~
                                    L20800,         /* Idle Time        */~
                                    L20900,         /* Submit Class     */~
                                    L21005,         /* Receiver Print   */~
                                    L21050,         /* Reject Codes     */~
                                    L21100,         /* Post at std cost */~
                                    L21200,         /* Quantity to Pay  */~
                                    L21250          /* Defer Cost @ Dir */
                     return

L20100
*        Default/Enable for PO NUMBER ASSIGNMENT
            inpmessage$ = "'m' = Manual Only; 's' = by PO Store Code;" & ~
                          " -or- Store Number with sequence."
            return

L20200
*        Default/Enable for Percent Receipts Limit
            inpmessage$ = "Enter 0 - 99999."
            return

L20300
*        Default/Enable for KEEP HISTORY OF PO CHANGES
            inpmessage$ = "Enter 'Y' to keep history of changes."
            return

L20350
*        Default/Enable for PO PRINT OPTION
            inpmessage$ = "'Blank' = Ask, '1' = Don't Print, '2' " &     ~
                          "= Standard, and '3' = Current Rev. Level"
            return

L20400
*        Default/Enable for PO PRINT OUTPUT SEQUENCE
            inpmessage$ = "'0' = by PO Number, '1' = by Vendor/PO, " &   ~
                          "'2' = by User ID/Vendor/PO Number"
            return

L20500
*        Default/Enable for SHOW IS/WAS
            inpmessage$ = "Enter 'Y' to show Is/Was for changed line" &  ~
                          " items."
            return

L20550
*        Default/Enable for Print REVISION DATE
            inpmessage$ = "Enter 'Y' to print Revision Date on PO."
            return

L20575
*        Default/Enable for Print RECEIVED AND NET DUE QTYS
            inpmessage$ = "Enter 'Y' to print Received and Net Due"   &  ~
                          " Quantities on PO."
            return

L20600
*        Default/Enable for Default RECEIVER Mode
            inpmessage$ = "R= To Receiver Hold, Q= To QC, I= To Inven" & ~
                        "tory, D= Dist, or Blank."
            return

L20700
*        Default/Enable for SUBIT WAIT TIME
            inpmessage$ = "Enter maximum number of seconds to wait"   &  ~
                          " for PO Update Background Task"
            return

L20800
*        Default/Enable for IDLE TIME
            inpmessage$ = "How many minutes should PO Update remain"  &  ~
                          " idle before it removes itself?"
            return

L20900
*        Default/Enable for SUBMITTAL CLASS
            inpmessage$ = "Enter the procedure submit class (or"  &      ~
                          " '*' to not submit)."
            return

L21005
*        Default/Enable for Receiver Printing
            inpmessage$ = "0=Manual Rcvr Tkt Print, 1=Auto Tkt Print;" & ~
            " Size(Inch): A=2-5/8, B=3, C=3-1/2"
            if tkt_size$ = " " then tkt_size$ = "C"
            return

L21050
*        Default/Enable for Receiving Reject Codes Required
            inpmessage$ = "Enter 'Y' if Receiving Rejection Codes are" & ~
            " required or 'N' if Not Required"
            return

L21100
*        Default/Enable for Posting PO items at standard cost
            inpmessage$ = "'N' to Default Inv Cost " &                   ~
            "as Price; 'Y', 'R' or 'B' as Std Cost; Blank for Help."
            return

L21200
*        Default/Enable for Excluding QC from Quantity to Pay
            inpmessage$ = "Enter 'Y' to Exclude QC from the Quantity" &  ~
            " To Pay Default."
            return

L21250
*        Default/Enable for Defer Costs at Directive time
            inpmessage$ = "Enter 'Y' to Defer Costs at Directive Crea" & ~
                          "tion until Cut Over into a PO."
            if defer_cost$ = " " then defer_cost$ = "N"
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

                  on fieldnr% gosub L22250,         /* Default VSA Actv */~
                                    L22310,         /* Default VSA Gen  */~
                                    L22400          /* Kit PJs Complete */

                     return

L22250
*        Default/Enable for Vendor Service Active Flag
            inpmessage$ = "Enter 'Y' to Allow Vendor Service Advices." & ~
                          "(Y/N)."
            if vsa_actv$ = " " then vsa_actv$ = "N"
            return

L22310
*        Default/Enable to Generate Vendor Service Advises.
            inpmessage$ = "Enter 'Y' to Generate Vendor Service Advic" & ~
                          "es at Vendor Route Steps(Y/N)."
            if vsa_gen$ = " " then vsa_gen$ = "N"
            return

L22400
*        Default/Enable to Kit Purchase Jobs Complete.
            inpmessage$ = "Enter 'Y' to Kit Purchase Jobs Complete; t" & ~
                          "his is the line item default."
            if pj_kit$ = " " then pj_kit$ = "Y"
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

        call "READ100" (#1, "SWITCHS.VBK", f1%(1))
        if f1%(1) = 1% then L30190
L30080:     e1% = 0%  : if keyhit% = 2% then e1% = -1% /* Return Dflts */
            e%  = e1% : call "VBKSWTCH" ("POHISTRY", chnghistory$, e, e%)
            e%  = e1% : call "VBKSWTCH" ("SBMTCLAS", class$      , e, e%)
L30100:     e%  = e1% : call "VBKSWTCH" ("IDLETIME", idle$       , e, e%)
            e%  = e1% : call "VBKSWTCH" ("POISWAS ", iswas$      , e, e%)
            e%  = e1% : call "VBKSWTCH" ("POASSIGN", poassign$   , e, e%)
            e%  = e1% : call "VBKSWTCH" ("POPROPT ", popropt$    , e, e%)
            e%  = e1% : call "VBKSWTCH" ("POSORT  ", posort$     , e, e%)
            e%  = e1% : call "VBKSWTCH" ("RCPTSLMT", rcptslimit$ , e, e%)
            e%  = e1% : call "VBKSWTCH" ("RCVRPRNT", rcvrs$      , e, e%)
            e%  = e1% : call "VBKSWTCH" ("SBMTWAIT", submit$     , e, e%)
            e%  = e1% : call "VBKSWTCH" ("RCVMODE ", rcvmode$    , e, e%)
            e%  = e1% : call "VBKSWTCH" ("REJCODES", rjcodes$    , e, e%)
            e%  = e1% : call "VBKSWTCH" ("POSTDCST", postdcst$   , e, e%)
            e%  = e1% : call "VBKSWTCH" ("QTYTOPAY", qtytopay$   , e, e%)
            e%  = e1% : call "VBKSWTCH" ("POPRDATE", poprdate$   , e, e%)
            e%  = e1% : call "VBKSWTCH" ("POPRPND ", poprpnd$    , e, e%)
            e%  = e1% : call "VBKSWTCH" ("TKTSIZE ", tkt_size$   , e, e%)
            e%  = e1% : call "VBKSWTCH" ("DEFERCST", defer_cost$ , e, e%)
            if pg% = 1% then return
            e%  = e1% : call "VBKSWTCH" ("VSA ACTV", vsa_actv$   , e, e%)
            e%  = e1% : call "VBKSWTCH" ("VSA GEN ", vsa_gen$    , e, e%)
            e%  = e1% : call "VBKSWTCH" ("PJ KIT C", pj_kit$     , e, e%)

        return

L30190:     get #1 using L30220, submit%, class$, idle%, poassign$,       ~
                                rcptslimit$, chnghistory$, posort$,      ~
                                iswas$, rcvrs$, popropt$, rcvmode$,      ~
                                rjcodes$, postdcst$, qtytopay$,          ~
                                poprdate$, poprpnd$, tkt_size$,          ~
                                defer_cost$,vsa_actv$,vsa_gen$, pj_kit$
L30220:         FMT XX(20), BI(2), CH(1), BI(2), CH(3), CH(5), 16*CH(1)

            convert submit% to submit$, pic(##0)
            convert idle%   to idle$  , pic(##0)
            return


L31000: REM *************************************************************~
            *                  S A V E   D A T A                        *~
            * --------------------------------------------------------- *~
            * Write switchs back to file                                *~
            *************************************************************

            submit%, idle% = 100%
            convert submit$ to submit%, data goto L31080
L31080:     convert idle$   to idle%  , data goto L31090
L31090:     call "READ101" (#1, "SWITCHS.VBK", f1%(1%))
            put #1 using L31140, "SWITCHS.VBK", submit%, class$, idle%,   ~
                                poassign$, rcptslimit$, chnghistory$,    ~
                                posort$, iswas$, rcvrs$, popropt$,       ~
                                rcvmode$, rjcodes$, postdcst$, qtytopay$,~
                                poprdate$, poprpnd$, tkt_size$,          ~
                                defer_cost$, vsa_actv$, vsa_gen$,        ~
                                pj_kit$, " ", " "

L31140:         FMT CH(20), BI(2), CH(1), BI(2), CH(3), CH(5), 16*CH(1), ~
                    CH(231), CH(220)
            if f1%(1%) = 0% then write #1 else rewrite #1
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

                  on fieldnr% gosub L41230,         /* PO Number Assgign*/~
                                    L41290,         /* Percent Rcpts Lmt*/~
                                    L41260,         /* Changes History  */~
                                    L41260,         /* PO Print Option  */~
                                    L41260,         /* PO Sorting       */~
                                    L41260,         /* Show Is/Was      */~
                                    L41260,         /* Revision Date    */~
                                    L41260,         /* Rcv'd, ND Qtys   */~
                                    L41260,         /* Default Recevr MD*/~
                                    L41290,         /* Submit Wait      */~
                                    L41290,         /* Idle Time        */~
                                    L41260,         /* Submit Class     */~
                                    L41260,         /* Receiver Printing*/~
                                    L41260,         /* Reject Codes     */~
                                    L41260,         /* cost PO's at std */~
                                    L41260,         /* Quantity to Pay  */~
                                    L41260          /* Defer Cost @ Dir */
                     goto L41330

L41230:           REM Set FAC'S for UPPER/LOWER CASE input
                      lfac$(fieldnr%) = hex(80)
                      return
L41260:           REM Set FAC'S for UPPER CASE ONLY input
                      lfac$(fieldnr%) = hex(81)
                      return
L41290:           REM Set FAC'S for NUMERIC ONLY input
                      lfac$(fieldnr%) = hex(82)
                      return

L41330:   accept                                                         ~
            at (01,02), "Manage PO Module Behavior Switches",            ~
            at (01,67), "Date:",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (03,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (04,02),                                                  ~
             "PO Number Assignment (m/s/Store Number)                  ",~
            at (04,70), fac(lfac$( 1%)), poassign$              , ch(03),~
            at (05,02),                                                  ~
             "Maximum Allowed Percentage of Receipt over Order Quantity",~
            at (05,70), fac(lfac$( 2%)), rcptslimit$            , ch(05),~
            at (06,02),                                                  ~
             "Keep a history of all Changes Made to POs? (Y/N)         ",~
            at (06,70), fac(lfac$( 3%)), chnghistory$           , ch(01),~
            at (07,02),                                                  ~
             "PO Printing: Print Option                                ",~
            at (07,70), fac(lfac$( 4%)), popropt$               , ch(01),~
            at (08,02),                                                  ~
             "             Output Sequence                             ",~
            at (08,70), fac(lfac$( 5%)), posort$                , ch(01),~
            at (09,02),                                                  ~
             "             Show Is/Was on Purchase Order? (Y/N)        ",~
            at (09,70), fac(lfac$( 6%)), iswas$                 , ch(01),~
            at (10,02),                                                  ~
             "             Print Revision Date on P.O.? (Y/N)          ",~
            at (10,70), fac(lfac$( 7%)), poprdate$              , ch(01),~
            at (11,02),                                                  ~
             "             Print Rcv'd and Net Due qtys on P.O.? (Y/N) ",~
            at (11,70), fac(lfac$( 8%)), poprpnd$               , ch(01),~
            at (12,02),                                                  ~
             "Default Receiving Mode (R,Q,I,D),                        ",~
            at (12,70), fac(lfac$( 9%)), rcvmode$               , ch(01),~
            at (13,02),                                                  ~
             "PO Update: Time out for Background Task Submit (seconds) ",~
            at (13,70), fac(lfac$(10%)), submit$                , ch(03),~
            at (14,02),                                                  ~
             "           Idle time until task ends itself (minutes)    ",~
            at (14,70), fac(lfac$(11%)), idle$                  , ch(03),~
            at (15,02),                                                  ~
             "           Background Procedure Submittal Class (A-Z)    ",~
            at (15,70), fac(lfac$(12%)), class$                 , ch(01),~
                                                                         ~
            at (16,02), "Receiver Printing Option & Size"               ,~
            at (16,70), fac(lfac$(13%)), rcvrs$                 , ch(01),~
            at (16,73), fac(lfac$(13%)), tkt_size$              , ch(01),~
            at (17,02), "Reject Codes Required in Receiving? (Y/N)",     ~
            at (17,70), fac(lfac$(14%)), rjcodes$               , ch(01),~
                                                                         ~
            at (18,02),                                                  ~
             "Default POs Inv Cost as Std Cost (Std Costed Parts Only)? (~
        ~N/Y/R/B)",                                                       ~
            at (18,70), fac(lfac$(15%)), postdcst$              , ch(01),~
                                                                         ~
            at (19,02),                                                  ~
             "Default Quantity to Pay Excludes QC Quantity? (Y/N)",      ~
            at (19,70), fac(lfac$(16%)), qtytopay$              , ch(01),~
                                                                         ~
            at (20,02), "Defer Cost Assignment at Directive Creation? (Y/~
        ~N)",                                                             ~
            at (20,70), fac(lfac$(17%)), defer_cost$            , ch(01),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)), pf$(1%)                    ,ch(79),~
            at (23,02), fac(hex(8c)), pf$(2%)                    ,ch(79),~
            at (24,02), fac(hex(8c)), pf$(3%)                    ,ch(79),~
                                                                         ~
                keys(pfk$),                                              ~
                key (keyhit%)

                if keyhit% <> 13% then L41860
                     call "MANUAL" ("VBKFLAGS")
                     goto L41330

L41860:         if keyhit% <> 15% then L41900
                     call "PRNTSCRN"
                     goto L41330

L41900:         close ws
                call "SCREEN" addr("C", n%, "I", i$(), cursor%())
                return

        set_pf_keys
          if fieldnr% <> 0% then L42090
            pf$(1%) = "(1)Start Over       (5)Next Screen      " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                    (9)Maintain PO Modul" &       ~
                     "e Administrators       (15)Print Screen"
            pf$(3%) = "(2)Reset using Defaults                 " &       ~
                     "(32) Exit (no save)    (16)SAVE DATA   "
            str(pf$(3%),63%,1%) = hex(84)
            pfk$ = hex(00010205090d0f1020ffffff)
            if pg% = 2% then str(pf$(1%),21%,18%) = "(4)Previous Screen"
            if pg% = 2% then str(pfk$,4%,1 ) = hex(04)
            return

L42090:     pf$(1%) = "(1)Start Over                           " &       ~
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

                  on fieldnr% gosub L43200,         /* Vendor Servc Actv*/~
                                    L43200,         /* Vendor Servc Gen */~
                                    L43200          /* Kit PJs Complete */

                     goto L43270

                  REM Set FAC'S for UPPER/LOWER CASE input
                      lfac$(fieldnr%) = hex(80)
                      return
L43200:           REM Set FAC'S for UPPER CASE ONLY input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC'S for NUMERIC ONLY input
                      lfac$(fieldnr%) = hex(82)
                      return

L43270:   accept                                                         ~
            at (01,02), "Manage PO Module Behavior Switches - Page 2",   ~
            at (01,67), "Date:",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02),                                                  ~
             "Allow Vendor Service Advices? (Y/N)                      ",~
            at (06,70), fac(lfac$( 1%)), vsa_actv$              , ch(01),~
            at (07,02),                                                  ~
             "Generate Vendor Service Advises? (Y/N)                   ",~
            at (07,70), fac(lfac$( 2%)), vsa_gen$               , ch(01),~
                                                                         ~
            at (08,02),                                                  ~
             "Kit Purchase Jobs Complete? (Y/N)                        ",~
            at (08,70), fac(lfac$( 3%)), pj_kit$                , ch(01),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)), pf$(1%)                    ,ch(79),~
            at (23,02), fac(hex(8c)), pf$(2%)                    ,ch(79),~
            at (24,02), fac(hex(8c)), pf$(3%)                    ,ch(79),~
                                                                         ~
                keys(pfk$),                                              ~
                key (keyhit%)

                if keyhit% <> 13% then L43560
                     call "MANUAL" ("BCKFLAGS")
                     goto L43270

L43560:         if keyhit% <> 15% then L43600
                     call "PRNTSCRN"
                     goto L43270

L43600:         close ws
                call "SCREEN" addr("C", n%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 1.                        *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* PO Number Assgign*/~
                                    L50200,         /* Percent Rcpts Lmt*/~
                                    L50300,         /* Changes History  */~
                                    L50350,         /* PO Print Option  */~
                                    L50400,         /* PO Sorting       */~
                                    L50500,         /* Show Is/Was      */~
                                    L50550,         /* Received Date    */~
                                    L50575,         /* Rcv'd, ND Qtys   */~
                                    L50600,         /* Receiving Mode   */~
                                    L50700,         /* Submit Wait      */~
                                    L50800,         /* Idle Time        */~
                                    L50900,         /* Submit Class     */~
                                    L50991,         /* Receiver printing*/~
                                    L51015,         /* Reject Codes Reqd*/~
                                    L51100,         /* Use std costs    */~
                                    L51200,         /* Quantity to Pay  */~
                                    L51250          /* Defer Cost @ Dir */
                     return

L50100
*        Test Data for PO NUMBER ASSIGNMENT
            if poassign$ = "m" or poassign$ = "s" then return
            call "READ100" (#2, poassign$, f1%(2))
            if f1%(2) = 1% then return
                errormsg$ = "Enter 'm', 's' or a valid Store code."
                return

L50200
*        Test Data for PERCENT RECEIPTS LIMIT
            if rcptslimit$ = " " then rcptslimit$ = "0"
            call "NUMTEST" (rcptslimit$, 0, 99999, errormsg$, 0.2, 0)
            return

L50300
*        Test Data for KEEP HISTORY OF PO CHANGES
            if chnghistory$ = "N" and iswas$ = "Y" then                  ~
                errormsg$ = "Must keep Change History in order to show" &~
                            " Is/Was on POs."
            if chnghistory$ = "Y" or chnghistory$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L50350
*        Test Data for PO PRINTING: PRINT OPTION
            if popropt$  = " " then return
            if popropt$ >= "1" and popropt$ <= "3" then return
                errormsg$ = "ENTER ' ', 1, 2 OR 3."
                return

L50400
*        Test Data for PO PRINTING: OUTPUT SEQ
            if posort$ >= "0" and posort$ <= "2" then return
                errormsg$ = "Enter 0, 1 or 2."
                return

L50500
*        Test Data for SHOW IS/WAS
            if iswas$ = "Y" and chnghistory$ = "N" then                  ~
                errormsg$ = "Cannot show Is/Was if Change History is"  & ~
                            " not kept."
            if iswas$ = "Y" or iswas$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return
L50550
*        Test Data for Default REVISION DATE
            if poprdate$ = "Y" or poprdate$ = "N" then return
               errormsg$ = "Please Enter 'Y' or 'N'."
               return

L50575
*        Test Data for Default RCV'D and NET DUE QUANTIES
            if poprpnd$ = "Y" or poprpnd$ = "N" then return
               errormsg$ = "Please Enter 'Y' or 'N'."
               return

L50600
*        Test Data for Default RECEIVER Mode
           if rcvmode$ = " " then return
            if rcvmode$ = "R" then return
             if rcvmode$ = "Q" then return
              if rcvmode$ = "I" then return
               if rcvmode$ = "D" then return
                errormsg$ = "Enter R, Q, I or D or Blank"
                return

L50700
*        Test Data for SUBMIT WAIT
            convert submit$ to n%, data goto L50720  :  goto L50730
L50720:         errormsg$ = "Allowable range: 20 - 480." : return
L50730:     if n% < 20% or n% > 480% then L50720
            convert n% to submit$, pic(##0)
            return

L50800
*        Test Data for IDLE TIME BEFORE CANCEL
            convert idle$ to n%, data goto L50820  :  goto L50830
L50820:         errormsg$ = "Allowable range: 10 - 999." : return
L50830:     if n% < 10% or n% > 999% then L50820
            convert n% to idle$, pic(##0)
            return

L50900
*        Test Data for SUBMITTAL CLASS
            if (class$ >= "A" and class$ <= "Z") or class$ = "*"         ~
                                                            then return
                errormsg$ = "Class must be letter between A & Z OR '*'."
                return
L50991: REM  Test Data For Receiver Printing Option
             if rcvrs$ <> "0" and rcvrs$ <> "1" then L50995
                 if (tkt_size$ < "A" or tkt_size$ > "C") then L50997
                 return
L50995:      errormsg$ = "Receiver Printing Option Must Be '1' Or '0'"
             return
L50997:      errormsg$ = "Receiver Ticket Size Must Be 'A', 'B', or 'C'"
             return

L51015: REM  Test Data For Rejection Codes Required
            if rjcodes$ = "Y" or rjcodes$ = "N" then return
                errormsg$ = "Please Enter 'Y' or 'N'."
                return

L51100
*        Test data for use standard cost for std costed parts
            helpdescr$ = hex(06) & "Default POs Inv Cost as Std Cost?" & ~
                         hex(94) & "(Std Costed Parts Only for 'B', '" & ~
                                   "R' & 'Y')"
            call "PLOWCODE" (#50, postdcst$, helpdescr$, 0%, .75,        ~
                                                                 f1%(50%))
            if f1%(50%) <> 0% then return
                errormsg$ = "Please Enter 'N', 'Y', 'R', or 'B'."
                return

L51200
*        Test data for exclusion of QC quantity
            if qtytopay$ = "Y" or qtytopay$ = "N" then return
                errormsg$ = "Please Enter 'Y' or 'N'."
                return

L51250
*        Test data for Defer Cost at Directive time
            if defer_cost$ = "Y" or defer_cost$ = "N" then return
                errormsg$ = "Please Enter 'Y' or 'N'."
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 2.                        *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51490,         /* Vendor Servc Actv*/~
                                    L51540,         /* Vendor Servc Gen */~
                                    L51640          /* Kit PJs Complete */
                     return

L51490
*        Test Data for Activate Vendor Service
            if pos("YN" = vsa_actv$) > 0 then L51515
                errormsg$ = "Enter 'Y' or 'N'."
                return
L51515:     if vsa_actv$ = "Y" then return
                vsa_gen$ = "N"
                return

L51540
*        Test Data for Generate Vendor Service Advisories
            if vsa_gen$ = "Y" and vsa_actv$ <> "Y" then L51600
            if vsa_gen$ = "Y" or  vsa_gen$ = "N" then return
               errormsg$ = "Please Enter 'Y' or 'N'."
               return
L51600:        errormsg$ = "VSA Active Flag Must be 'Y' for VSA Generate ~
        ~ to be 'Y'."
               return

L51640
*        Test Data for Kit Purchase Jobs Complete
            if pos("YN" = pj_kit$) > 0 then return
               errormsg$ = "Please Enter 'Y' or 'N'."
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
            end
