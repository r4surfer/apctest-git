        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  FFFFF  L       AAA    GGG    SSS    *~
            *  H   H  NN  N  Y   Y  F      L      A   A  G      S       *~
            *  HHHHH  N N N   Y Y   FFFF   L      AAAAA  G GGG   SSS    *~
            *  H   H  N  NN    Y    F      L      A   A  G   G      S   *~
            *  H   H  N   N    Y    F      LLLLL  A   A   GGG    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYFLAGS - Set switches for Inventory Module.  Sets the   *~
            *            valid entry format for all 'new' Lot           *~
            *            numbers.  User indicates what type of character*~
            *            is expected/allowed in each position of the    *~
            *            Lot number, what constants or separator        *~
            *            characters are required, and the valid lengths *~
            *            for a Lot number (minimum and maximum).  Also  *~
            *            sets the entry format for all 'new' serial     *~
            *            numbers.  User indicates what type of character*~
            *            is expected/allowed in each position of the    *~
            *            serial number, what constants or separator     *~
            *            characters are required, whether automatic     *~
            *            assignment of serial numbers is to take place, *~
            *            and the valid lengths for a serial number      *~
            *            (minimum and maximum).                         *~
            *            The next available serial number is also       *~
            *            maintained by this program if automatic        *~
            *            assigment is turned on.                        *~
            *            Inventory module administrators are defined    *~
            *            via this program also.                         *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/06/87 ! Original (cloned from BCKFLAGS)          ! LDJ *~
            * 06/30/87 ! Fixed format field dimensions on screen  ! MJB *~
            * 09/01/87 ! Added Calendar Type to store movement    ! LKM *~
            *          ! history by Gregorian or fiscal calendar, !     *~
            *          ! neither, or both                         !     *~
            * 12/12/89 ! Added three flags for Accountancy Phase 1! JDH *~
            *          !  1) How to post Inv Movement Variances   !     *~
            *          !  2) How to post Inv Revaluation Amounts  !     *~
            *          !  3) Default acct for Inv Revaluation     !     *~
            * 01/30/90 ! Added 1 flag for Locations               ! MLJ *~
            *          !  1) Valid Locations Required (Y/N)       !     *~
            * 03/13/90 ! Copied spelling correction done by LAB.  ! MLJ *~
            * 06/10/91 ! Moved Location question to 2nd page and  ! JBK *~
            *          !  added question for Location Audits      !     *~
            * 09/04/91 ! Took out Location Audits (superficially).! JDH *~
            * 06/04/92 ! MPS/PFM- Added (1) Usage Capture Method  ! MLJ *~
            *          !   Flag (2) Include Store Code When       !     *~
            *          !   Capturing Usage Flag (3)Usage Detail,  !     *~
            *          !   Summary, Neither, Both Flag (4) Default!     *~
            *          !   Calendar Code for Non-MPS Parts Flag.  !     *~
            *          !   Added ALLFREE.                         !     *~
            * 06/11/92 ! MPS/PFM - removed all references to loc  ! MLJ *~
            *          !   audit flag.                            !     *~
            * 08/17/92 ! MPS/PFM - Added Requested Usage Date Flag! JBK *~
            * 10/12/92 ! Add Location Transaction Audit Flag      ! RJH *~
            * 02/12/93 ! PRR 12688 - Adjust 2nd Screen to allow   ! RJH *~
            *          !  the Calander Description to 30 chars.   !     *~
            * 04/28/93 ! PRRs 10716 & 11937 Add default Source,   ! JIM *~
            *          !   Destination & WIP accts for HNYADDNS,  !     *~
            *          !   HNYWDWAL, and HNYFLUSH, respectively.  !     *~
            * 04/28/93 ! Revaluation Inventory Account DIM'd 12.  ! JIM *~
            * 04/28/93 ! All 30-char descriptions display CH(30). ! JIM *~
            * 09/16/93 ! PRR 13022  Don't read on hold @ DATALOAD.! JDH *~
				* 06/24/96 ! Added tmp date to retrieve month         ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                              /* NORMAL OLD VARIABLES       */~
            auto_seq$1,                  /* Automatic Assignment ?     */~
            cal$1,                       /* Type of calendar           */~
            cursor%(2),                  /* Cursor location for edit   */~
            calcode$8, caldesc$30,       /* Dflt Cal Code for non-MPS  */~
            date$8,                      /* Date for screen display    */~
            defsource$12, defsrcdesc$30, /* Default Source/HNYADDNS    */~
            def_destn$12, defdstdesc$30, /* Default Dest'n/HNYWDWAL    */~
            defintwip$12, defwipdesc$30, /* Default Int WIP/HNYFLUSH   */~
            dsnb$1,                      /* Dtl, Sumry, Neither, Both  */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second screen line         */~
            validlocations$1,            /* Must Locations Be Valid?   */~
            location_audit$1,            /* Location Transaction Audit */~
            lot_format$(1)16,            /* Lot Number Format          */~
            lotlevel$1,                  /* Lot Tracking Detail/Summary*/~
            lottrack$1,                  /* System Lot Track Flag      */~
            lot_unique$1,                /* Lot Uniqueness             */~
            max_lot_len$2,               /* Maximum Lot Number Len     */~
            max_ser_len$2,               /* Maximum Serial Number Len  */~
            method_msg$(2)25,            /* Descriptive message        */~
            method_type$(3)25,           /* Descriptive message choice */~
            min_lot_len$2,               /* Minimum Lot Number Len     */~
            min_ser_len$2,               /* Minimum Serial Number Len  */~
            move_var_method$1,           /* Method to post Inv Move Var*/~
            next_avail$(1)20,            /* Next Available Serial Nbr  */~
            p%(2),                       /* Search Receiver Array      */~
            pf$(3)79,                    /* PF Key Prompts             */~
            pf4$20,                      /* PF Key 4 Prompt            */~
            pfk$20,                      /* PF Keys Available          */~
            readkey$20,                  /* File Read Key              */~
            req_usage_date$1,            /* Date for Requested Usage   */~
            req_usage_date_descr$24,     /* Method to post Inv Revalue */~
            revaluation_method$1,        /* Method to post Inv Revalue */~
            revaluation_acct$12,         /* Account to post Inv Revalue*/~
            revaluation_desc$30,         /* Account description        */~
            ser_format$(1)20,            /* Serial Number Format       */~
				tmp_dt1$8,                   /* Temp formated date         */~
				tmp_dt2$8,                   /* Temp date CCYYMMDD         */~
            usage$1,                     /* Usage Method Capture Flag  */~
            usestore$1                   /* Include Stores (usage cap.)*/~


        dim f2%(20),                     /* = 0 if the file is open    */~
            f1%(20)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            * #4  ! GLMAIN   ! General Ledger Master File               *~
            * #9  ! PFMCAL   ! Forecast/Usage Calendar File             *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =     1, keylen =  20

            select #4,  "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #9,  "PFMCAL",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 8,                          ~
                        alternate key 1, keypos = 39, keylen = 6, dup

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, 0%, f2%(1),   0%, " ")
            call "OPENCHCK" (#4, 0%, f2%(4),   0%, " ")
            call "OPENCHCK" (#9, 0%, f2%(9),   0%, " ")

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
            call "CMSMACHK" ("HNY", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then L09160
            pf$(1) ="You must be a Data Base or HNY Module Administrator"~
                     & " to run this program."
            call "ASKUSER" (0%, "SECURITY CHECK", " ", pf$(1), " ")
            goto exit_program

L09160
*        Set some variables
            str(line2$,62) = "HNYFLAGS: " & cms2v$
            method_type$(1) = "Inv Variance Accts (12)  "
            method_type$(2) = "Inv Adjustment Account   "
            method_type$(3) = "Std Cost Revaluation Acct"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * No real input mode - just load defaults from disk.        *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$
            pg% = 0%

            gosub dataload /*Load then Format stuff all nice and purty */


L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editmode
            pg%  = 1%
            pf4$ = " "
            gosub'051(0%)
            gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then gosub reset_to_defaults
                  if keyhit%  =  5 then       editpg2
                  if keyhit%  =  9 then       mod_admin
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 32 then       exit_program
                  if keyhit% <>  0 then       editmode
            fieldnr% = cursor%(1) - 3%
            if cursor%(1) = 4% and cursor%(2) > 34% then fieldnr% = 2%
            if cursor%(1) > 4% then fieldnr% = cursor%(1) - 2%
            if cursor%(1) = 5% and cursor%(2) > 34% then fieldnr% = 4%
            if cursor%(1) > 5% then fieldnr% = cursor%(1) - 1%
            if cursor%(1) = 6% and cursor%(2) > 45% then fieldnr% = 6%
            if cursor%(1) > 6% then fieldnr% = cursor%(1)
            if cursor%(1) = 8% and cursor%(2) > 34% then fieldnr% = 9%
            if cursor%(1) > 8% then fieldnr% = cursor%(1) + 1%
            if fieldnr% < 1% or fieldnr% > 15% then editmode
            field% = fieldnr%
            for fieldnr% = field% to 15%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L11340
                      if fieldnr% <> field% then L11320
L11240:               if fieldnr% > 1 then pf4$ = "(4)Previous Field"    ~
                                      else pf4$ = " "
L11260:         gosub'111(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L11310
L11290:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 0% then L11290
                         goto L11240
L11310:               if keyhit% <>  0 then L11260
L11320:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L11260
L11340:     next fieldnr%
            goto editmode

        mod_admin    /* Allow maintenance of HNY Module Administrators */
            call "CMSMAINP" ("HNY", "Inventory")
            on pg% goto L11000, L11500

L11500: REM *************************************************************~
            *        E D I T   M O D E   S E C O N D   P A G E          *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg2
            pg%  = 2%
            pf4$ = " "
            gosub'052(0%)
            gosub'112(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then gosub reset_to_defaults
                  if keyhit%  =  4 then       editmode
                  if keyhit%  =  9 then       mod_admin
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 32 then       exit_program
                  if keyhit% <>  0 then       editpg2
            fieldnr% = cursor%(1) - 4%
            if fieldnr% < 1% or fieldnr% > 13% then editpg2
            field% = fieldnr%
            for fieldnr% = field% to 13%
                gosub'052(fieldnr%)
                      if enabled% = 0% then L11850
                      if fieldnr% <> field% then L11830
L11730:               if fieldnr% > 1 then pf4$ = "(4)Previous Field"    ~
                                      else pf4$ = " "
L11750:         gosub'112(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L11820
L11780:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 0% then L11780
                         goto L11730
L11820:               if keyhit% <>  0 then L11750
L11830:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L11750
L11850:     next fieldnr%
            goto editpg2

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "READ101" (#1, "SWITCHS.HNY", f1%(1))
            gosub dataput
            if f1%(1) = 0% then write #1 else rewrite #1
            goto  exit_program


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

            on fieldnr% gosub L20097,         /* Lot Tracking Enforced? */~
                              L20137,         /* Detail or Summary ?    */~
                              L20200,         /* Maximum Lot # Len      */~
                              L20250,         /* Minimum Lot # Len      */~
                              L20291,         /* Lot Uniqueness         */~
                              L20300,         /* Lot Nbr Format         */~
                              L20370,         /* Auto Assignment        */~
                              L20420,         /* Maximum Ser # Len      */~
                              L20470,         /* Minimum Ser # Len      */~
                              L20570,         /* Serial Nbr Format      */~
                              L20610,         /* Next Available Serial #*/~
                              L20750,         /* Maximum Ser # Qty      */~
                              L20760,         /* Minimum File Size      */~
                              L20770,         /* Avg # Components       */~
                              L20820          /* Calendar Type          */
                     return

L20097: REM Def/Enable Lot Tracking Enforced    LOTTRACK$
            inpmessage$ = "Enter 'Y' if Lot Tracking Rules are " &       ~
                          "to be enforced or 'N' if not."
            return
L20137: REM Def/Enable Detail or Summary Track  LOTLEVEL$
            inpmessage$ = "Enter 'S' for Lot Movement Tracking " &       ~
                        "at a Summary level or 'D' for Detailed Tracking"
            if lotrack$ = "N" then enabled% = 0%
            if lotrack$ = "N" then lotlevel$ = "S"
            return
L20200: REM Def/Enable Maximum Lot Number Len   MAX_LOT_LEN$
            inpmessage$ = "Enter the length of the longest Lot " &       ~
                          "number allowed."
            if max_lot_len$ = " " then max_lot_len$ = " 6"
            return
L20250: REM Def/Enable Minimum Lot Number Len   MIN_LOT_LEN$
            inpmessage$ = "Enter the length of the shortest Lot " &      ~
                          "number allowed."
            if min_lot_len$ = " " then min_lot_len$ = max_lot_len$
            return
L20291: REM Def/Enable Lot Uniqueness           LOT_UNIQUE$
            inpmessage$ = "Enter 'U' if Lots must be Unique for all " &  ~
                          "parts or 'P' if only by part"
            if lottrack$ = "N" then enabled% = 0%
            return
L20300: REM Def/Enable Lot Number Format        LOT_FORMAT$(1)
            inpmessage$ = "Define the valid Lot number format."
            max_lot_len =  6
            convert max_lot_len$ to max_lot_len, data goto L20340
L20340:     redim lot_format$(1)max_lot_len
            if lot_format$(1) = " " then lot_format$(1) = all("#")
            if lottrack$ = "N" then lot_format$(1) = all("#")
            if lottrack$ = "N" then enabled% = 0%
            return
L20370: REM Def/Enable Auto Assignment of Serial#'s AUTO_SEQ$
            inpmessage$ = "Enter 'Y' to automatically assign Serial " &  ~
                          "numbers or 'N'."
            if auto_seq$ = " " then auto_seq$ = "N"
            return
L20420: REM Def/Enable Maximum Serial Number Len   MAX_SER_LEN$
            inpmessage$ = "Enter the length of the longest serial " &    ~
                          "number allowed."
            if max_ser_len$ = " " then max_ser_len$ = "16"
            return
L20470: REM Def/Enable Minimum Serial Number Len   MIN_SER_LEN$
            inpmessage$ = "Enter the length of the shortest serial " &   ~
                          "number allowed."
            if min_ser_len$ = " " then min_ser_len$ = max_ser_len$
            return
L20570: REM Def/Enable Serial Number Format        SER_FORMAT$(1)
            inpmessage$ = "Define the valid serial number format."
            max_ser_len =  6
            convert max_ser_len$ to max_ser_len, data goto L20586
L20586:     redim ser_format$(1)max_ser_len
            if ser_format$(1) = " " then ser_format$(1) = all("#")
            return
L20610: REM Def/Enable Next Avail Serial Number    NEXT_AVAIL$(1)
            if auto_seq$ = "Y" then L20680
               next_avail$(1) = " "
               enabled% = 0%
               return
L20680:     inpmessage$ = "Enter the Next Available Serial Number"
            if next_avail$(1) > " " then return
            next_avail$(1) = ser_format$(1)
            redim next_avail$(1)max_ser_len
            tran(next_avail$(1),"0+")replacing
            if pos(next_avail$(1)="0") = 0% then return
            str(next_avail$(1),pos(-next_avail$(1)="0"),1%) = "1"
L20731:     search next_avail$(1) = "%%" to p%()
            if p%(1) = 0% then return
            tmp_dt1$ = date
            call "DATFMTC" ( tmp_dt1$, tmp_dt%, tmp_dt2$ )
            str(next_avail$(1),p%(1),2%) = str(tmp_dt2$,3%,2%)
            goto L20731
            return
L20750: REM Maximum Ser # Qty
            inpmessage$ = "Enter the max qty allowed in any transaction" ~
                        & " involving a S/N part."
            return
L20760: REM Minimum File Size
            inpmessage$ = "Enter the expected volume of new S/N's " &    ~
                          "generated per month."
            return
L20770: REM Avg # Components
            inpmessage$ = "Enter the avg # of components (1 level) " &   ~
                          "in a manufactured S/N part."
            return

L20820: REM Calendar Type
            inpmessage$ = "Enter 'F' for Fiscal, 'G' for Gregorian, 'B' f~
        ~or both or Blank for Neither."
            if cal$ = " " then cal$ = "G"
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES fields for the Page 2 of Input. *~
            *************************************************************

            deffn'052(fieldnr%)
            if fieldnr% <> 0% then L21110
                inpmessage$ = "Place cursor at field to modify and" &    ~
                " Press (RETURN); PF-32 to EXIT program."
                return
L21110:     enabled% = 1%

            on fieldnr% gosub L21300,         /* Inv Move Var Method    */~
                              L21400,         /* Revaluation Method     */~
                              L21500,         /* Revaluation Deflt Acct */~
                              L21600,         /* Valid Locations?       */~
                              L21644,         /* Locations Transac.Audit*/~
                              L21660,         /* Usage Dtl,Sum,Neith,Bth*/~
                              L21720,         /* Include Stores (Usage) */~
                              L21800,         /* Requested Usage Date   */~
                              L21910,         /* Dflt Cal Code, non-MPS */~
                              L21960,         /* Usage Capture Method   */~
                              L22020,         /* Def Source Acct        */~
                              L22060,         /* Def Destination Acct   */~
                              L22110          /* Def Interim WIP Acct   */
                     return

L21300: REM Def/Enable Inv Movement Var Method  MOVE_VAR_METHOD$
            inpmessage$ = "Enter 'V' to post to Inv Variance Accts;" &   ~
                          " 'A' to post to Inv Adjustment Acct"
            return
L21400: REM Def/Enable Revaluation Method       REVALUATION_METHOD$
            inpmessage$ = "Enter 'V' to post to Inv Variance Accts;" &   ~
                          " 'R' to post to Inv Revaluation Acct"
            return
L21500: REM Def/Enable Revaluation Method       REVALUATION_ACCT$
            inpmessage$ = "Enter Default Inventory Revaluation Account."
            return

L21600: REM Must Locations be Valid?            VALIDLOCATIONS$
            inpmessage$ = "Enter 'Y' if locations must be on file before ~
        ~they can be used."
            if validlocations$ = " " then validlocations$ = "N"
            return

L21644: REM Write a Location Transaction Audit File  LOCATION_AUDIT
            inpmessage$ = "Enter 'Y' if Locations Transaction Audit File ~
        ~will be writen to."
            if location_audit$ = " " then location_audit$ = "N"
            return

L21660: REM Usage Detail, Summary, Neither or Both        DSNB$
            inpmessage$ = "Capture Usage as: 'D'etail, 'S'ummary, 'N'ei"&~
                "ther or 'B'oth?"
            if dsnb$ = " " then dsnb$ = "N"
            return

L21720: REM Include Stores Code when Capturing Usage      USESTORE$
            inpmessage$ = "Include the Stores Code when Capturing Usage D~
        ~ata ('Y' OR 'N')."
            if usestore$ = " " then usestore$ = "N"
            return

L21800: REM Requested Usage Date to be Used       REQ_USAGE_DATE$
            inpmessage$ = "Capture Requested Usage by 'R'equired Ship "& ~
                "Date or 'O'riginal Order Due Date"
            if req_usage_date$ = " " then req_usage_date$ = "R"
            if req_usage_date$ = "R" then req_usage_date_descr$ =        ~
                "Required Ship Date"
            if req_usage_date$ = "O" then req_usage_date_descr$ =        ~
                "Original Order Due Date"
            return

L21910: REM Default Calendar Code for non-MPS parts       CALCODE$
            inpmessage$ = "Enter the default Calendar Code for non-MPS "&~
                "parts."
            return

L21960: REM Usage Capture Method     USAGE$
            inpmessage$ = "Enter 'Y' for MPS parts only; 'N' for all pa"&~
                "rts."
            if usage$ = " " then usage$ = "Y"
            return

L22020: REM Def/Enable Default Source Account             DEFSOURCE$
            inpmessage$ = "Enter Default Source Account for Inventory Add~
        ~itions."
            return

L22060: REM Def/Enable Default Destination Account        DEF_DESTN$
            inpmessage$ = "Enter Default Destination Account for Inventor~
        ~y Withdrawals."
            return

L22110: REM Def/Enable Default Interim BackFlush/WIP Acct DEFINTWIP$
            inpmessage$ = "Enter Default Interim BackFlush (or WIP) Acct ~
        ~for BackFlushing."
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
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

        REM *************************************************************~
            *                  L O A D   D A T A                        *~
            * --------------------------------------------------------- *~
            * Pull data from switch's string and format.                *~
            *************************************************************
        dataload
            call "READ100" (#1, "SWITCHS.HNY", f1%(1))
            if f1%(1) = 1% then L30260

*        No record on file or User wants to reset to system defaults
        reset_to_defaults
            if pg% = 2% then goto L30232
                max_lot_len$, min_lot_len$ = "6"
                max_lot_len, min_lot_len    = 6
                max_ser_len$, min_ser_len$ = "6"
                max_ser_len, min_ser_len    = 6
                lotlevel$ = "S" : lottrack$ = "N" : lot_unique$ = "U"
                max_ser_qty%  = 100%
                min_filesize% = 1000%
                mult%         = 10%
                lot_format$(1) = " "
                str(lot_format$(1),,max_lot_len) = all("#")
                auto_seq$ = "N"
                ser_format$(1) = " "
                str(ser_format$(1),,max_ser_len) = all("#")
                next_avail$(1) = " "
            if pg% = 1% then return
L30232:         move_var_method$    = "V"
                revaluation_method$ = "V"
                revaluation_acct$   = " "
                revaluation_desc$   = " "
                method_msg$(1)      = method_type$(1)
                method_msg$(2)      = method_type$(1)
                validlocations$     = "N"
                location_audit$     = "N"
                usage$              = "Y"
                usestore$           = "N"
                dsnb$               = "N"
                caldesc$, calcode$  = " "
                req_usage_date$     = "R"
                req_usage_date_descr$ = "Required Ship Date"
                init (" ") defsource$, defsrcdesc$, def_destn$,          ~
                     defdstdesc$, defintwip$, defwipdesc$
            return

L30260:     get #1 using L35060,                                          ~
            readkey$,       /* Acts as variable text key to SYSFILE2 sy*/~
            max_lot_len$,   /* Maximum allowed or valid length for a Lo*/~
            min_lot_len$,   /* Minimum allowed or valid length for a Lo*/~
            lot_format$(1), /* Describes the valid entry format for new*/~
            auto_seq$,      /* Automatic Numbering Sequence Flag.      */~
            max_ser_len$,   /* Maximum allowed or valid length for a se*/~
            min_ser_len$,   /* Minimum allowed or valid length for a se*/~
            ser_format$(1), /* Describes the valid entry format for new*/~
            next_avail$(1), /* Next available number                   */~
            max_ser_qty%,   /* Maximum transaction qty for any trans w/*/~
            min_filesize%,  /* Minimum file size in terms of number of */~
            mult%,          /* Size multiplier                         */~
            lottrack$,      /* System Level  Lot Tracking Required Flag*/~
            lot_unique$,    /* Lots must be totally unique or unique on*/~
            lotlevel$,      /* Keep Summary only or Detailed Lot moveme*/~
            cal$,           /* Calendar Type for usage history         */~
            move_var_method$,/* Inv Movement Variance Method           */~
            revaluation_method$,/* Inv Revaluation Method              */~
            revaluation_acct$,  /* Inv Revaluation Account             */~
            validlocations$,  /* Set to Y if Locations must be on file */~
            usage$,         /* Usage Capture Method Flag               */~
            usestore$,      /* Include Store Code when Capturing Usage */~
            dsnb$,          /* Usage Detail, Summary, Neither or Both  */~
            calcode$,       /* Default Calendar Code for non-MPS parts */~
            req_usage_date$,/* Date of Requested Usage                 */~
            location_audit$,/* Location Transaction Audit Flag         */~
            defsource$, def_destn$, defintwip$ /* Default G/L #s       */

            max_lot_len = 6
            convert max_lot_len$ to max_lot_len, data goto L30428
L30428:     min_lot_len = 3
            convert min_lot_len$ to min_lot_len, data goto L30440
L30440:     max_ser_len = 6
            convert max_ser_len$ to max_ser_len, data goto L30460
L30460:     min_ser_len = 6
            convert min_ser_len$ to min_ser_len, data goto L30480
            if calcode$ <> " " then                                      ~
                call "DESCRIBE" (#9, calcode$, caldesc$, 0%, f1%(9))
            revaluation_desc$ = " " : if revaluation_acct$ = " "         ~
                then goto L30486
L30480:     call "DESCRIBE" (#4, revaluation_acct$, revaluation_desc$,   ~
                               0%, f1%(4))
            if f1%(4%) = 0% then revaluation_desc$ = "Acct not on file"
            call "GLFMT" (revaluation_acct$)
L30486:     method_msg$(1) = method_type$(1)
            method_msg$(2) = method_type$(1)
            if move_var_method$ = " " then move_var_method$ = "V"
            if move_var_method$ = "A" then method_msg$(1)= method_type$(2)
            if revaluation_method$ = " " then revaluation_method$ = "V"
            if revaluation_method$ = "R" then                            ~
                                    method_msg$(2)= method_type$(3)
            if req_usage_date$ = "R" then req_usage_date_descr$ =        ~
                     "Required Ship Date"
            if req_usage_date$ = "O" then req_usage_date_descr$ =        ~
                     "Original Order Due Date"
            defsrcdesc$ = " " : if defsource$ = " " then goto L30554
                call "DESCRIBE" (#4, defsource$, defsrcdesc$, 0%, f1%(4%))
                if f1%(4%) = 0% then defsrcdesc$ = "Acct not on file"
                call "GLFMT" (defsource$)
L30554:     defdstdesc$ = " " : if def_destn$ = " " then goto L30594
                call "DESCRIBE" (#4, def_destn$, defdstdesc$, 0%, f1%(4%))
                if f1%(4%) = 0% then defdstdesc$ = "Acct not on file"
                call "GLFMT" (def_destn$)
L30594:     defwipdesc$ = " " : if defintwip$ = " " then goto L30634
                call "DESCRIBE" (#4, defintwip$, defwipdesc$, 0%, f1%(4%))
                if f1%(4%) = 0% then defwipdesc$ = "Acct not on file"
                call "GLFMT" (defintwip$)
L30634:     return

        REM *************************************************************~
            *                  S A V E   D A T A                        *~
            * --------------------------------------------------------- *~
            * Write switchs back to file                                *~
            *************************************************************
        dataput
            call "GLUNFMT" (revaluation_acct$)
            call "GLUNFMT" (defsource$)
            call "GLUNFMT" (def_destn$)
            call "GLUNFMT" (defintwip$)
            call "READ101" (#1, "SWITCHS.HNY", f1%(1))
            put #1 using L35060,                                          ~
            "SWITCHS.HNY",  /* Acts as variable text key to SYSFILE2 sy*/~
            max_lot_len$,   /* Maximum allowed or valid length for a Lo*/~
            min_lot_len$,   /* Minimum allowed or valid length for a Lo*/~
            lot_format$(1), /* Describes the valid entry format for new*/~
            auto_seq$,      /* Automatic Numbering Sequence Flag.      */~
            max_ser_len$,   /* Maximum allowed or valid length for a se*/~
            min_ser_len$,   /* Minimum allowed or valid length for a se*/~
            ser_format$(1), /* Describes the valid entry format for new*/~
            next_avail$(1), /* Next available number                   */~
            max_ser_qty%,   /* Maximum transaction qty for any trans w/*/~
            min_filesize%,  /* Minimum file size in terms of number of */~
            mult%,          /* Size multiplier                         */~
            lottrack$,      /* System Level  Lot Tracking Required Flag*/~
            lot_unique$,    /* Lots must be totally unique or unique on*/~
            lotlevel$,      /* Keep Summary only or Detailed Lot moveme*/~
            cal$,           /* Use fiscal or Gregorian calendar        */~
            move_var_method$,/* Inv Movement Variance Method           */~
            revaluation_method$,/* Inv Revaluation Method              */~
            revaluation_acct$,/* Default Inv Revaluation Account       */~
            validlocations$,/* Set to Y if Locations must be on file   */~
            usage$,         /* Usage Capture Method Flag               */~
            usestore$,      /* Include Store Code when Capturing Usage */~
            dsnb$,          /* Usage Detail, Summary, Neither, Both    */~
            calcode$,       /* Default Calendar Code for non-MPS parts */~
            req_usage_date$,   /* Requested Usage Date                 */~
            location_audit$,/* Location Transaction Audit Flag         */~
            defsource$, def_destn$, defintwip$, /* Default G/L #s      */~
            " ", " "        /* Filler (Internal, unused space)         */

            return


        REM *************************************************************~
            *                 F I L E   F O R M A T S                   *~
            *-----------------------------------------------------------*~
            * File format statments for GET & PUT statements.           *~
            *************************************************************

L35060: FMT                 /* FILE: SYS2-HNY                          */~
            CH(20),         /* Acts as variable text key to SYSFILE2 sy*/~
            CH(2),          /* Maximum allowed or valid length for a Lo*/~
            CH(2),          /* Minimum allowed or valid length for a Lo*/~
            CH(16),         /* Describes the valid entry format for new*/~
            CH(1),          /* Automatic Numbering Sequence Flag.      */~
            CH(2),          /* Maximum allowed or valid length for a se*/~
            CH(2),          /* Minimum allowed or valid length for a se*/~
            CH(20),         /* Describes the valid entry format for new*/~
            CH(20),         /* Next available number                   */~
            BI(2),          /* Maximum transaction qty for any trans w/*/~
            BI(3),          /* Minimum file size in terms of number of */~
            BI(1),          /* Size multiplier                         */~
            CH(1),          /* System Level  Lot Tracking Required Flag*/~
            CH(1),          /* Lots must be totally unique or unique on*/~
            CH(1),          /* Keep Summary only or Detailed Lot moveme*/~
            CH(1),          /* Use fiscal or Gregorian calendar        */~
            CH(1),          /* Inv Movement Variance Method            */~
            CH(1),          /* Inv Revaluation Method                  */~
            CH(9),          /* Default Inv Revaluation Account         */~
            CH(1),          /* Valid Locations Required?               */~
            CH(1),          /* Usage Capture Method Flag               */~
            CH(1),          /* Include Store Code when Capturing Usage */~
            CH(1),          /* Usage Detail, Summary, Neither or Both  */~
            CH(8),          /* Default Calendar Code for non-MPS parts */~
            CH(1),          /* Requested Usage Date                    */~
            CH(1),          /* Location Transaction Audit Flag         */~
            3*CH(9),        /* Default source, Destination, WIP A/#s   */~
            CH(163), CH(190)/* Filler (Internal, unused space)         */

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            * --------------------------------------------------------- *~
            * Screen for Editing Page 1 of document.                    *~
            *************************************************************

        deffn'111(fieldnr%)
            if fieldnr% = 0% then init(hex(86)) lfac$()  else            ~
                                  init(hex(8c)) lfac$()
            gosub set_pf_keys

                  on fieldnr% gosub L41300,   /* Lot Tracking Enforced? */~
                                    L41300,   /* Detail or Summary ?    */~
                                    L41330,   /* Maximum Lot # Len      */~
                                    L41330,   /* Minimum Lot # Len      */~
                                    L41300,   /* Lot Uniqueness         */~
                                    L41300,   /* Lot Nbr Format         */~
                                    L41300,   /* Auto Assignment        */~
                                    L41330,   /* Maximum Ser # Len      */~
                                    L41330,   /* Minimum Ser # Len      */~
                                    L41300,   /* Serial Nbr Format      */~
                                    L41300,   /* Next Available Serial #*/~
                                    L41330,   /* Maximum Ser # Qty      */~
                                    L41330,   /* Minimum File Size      */~
                                    L41330,   /* Avg # Components       */~
                                    L41300    /* Calendar Type          */
                     goto L41370

                  REM Set FAC'S for UPPER/LOWER CASE input
                      lfac$(fieldnr%) = hex(80)
                      return
L41300:           REM Set FAC'S for UPPER CASE ONLY input
                      lfac$(fieldnr%) = hex(81)
                      return
L41330:           REM Set FAC'S for NUMERIC ONLY input
                      lfac$(fieldnr%) = hex(82)
                      return

L41370:   accept                                                         ~
            at (01,02), "Manage Inventory Module Behavior Switches",     ~
            at (01,67), "Date:",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (03,02), fac(hex(94)), errormsg$                 , ch(79),~
            at (04,02),                                                  ~
             "Lot Tracking Enforced? (Y/N)",                             ~
            at (04,31), fac(lfac$( 1)), lottrack$               , ch(01),~
            at (04,35), "Detail or Summary Tracking?",                   ~
            at (04,65), fac(lfac$( 2)), lotlevel$               , ch(01),~
            at (05,02),                                                  ~
             "Maximum Lot Number Length ",                               ~
            at (05,29), fac(lfac$( 3)), max_lot_len$            , ch(02),~
            at (05,35),                                                  ~
             "Minimum Lot Number Length ",                               ~
            at (05,65), fac(lfac$( 4)), min_lot_len$            , ch(02),~
            at (06,02),                                                  ~
             "Lots Unique Across Parts or Within Parts?",                ~
            at (06,44), fac(lfac$( 5)), lot_unique$             , ch(01),~
            at (06,50),                                                  ~
             "Lot Format is ",                                           ~
            at (06,65), fac(lfac$( 6)), str(lot_format$(1),,             ~
                                                     min(6,max_lot_len)),~
            at (07,02),                                                  ~
             "Automatic Assignment of Serial Numbers? (Y/N)",            ~
            at (07,65), fac(lfac$( 7)), auto_seq$               , ch(01),~
            at (08,02),                                                  ~
             "Maximum Serial Number Length ",                            ~
            at (08,31), fac(lfac$( 8)), max_ser_len$            , ch(02),~
            at (08,35),                                                  ~
             "Minimum Serial Number Length ",                            ~
            at (08,65), fac(lfac$( 9)), min_ser_len$            , ch(02),~
            at (09,02),                                                  ~
             "Serial Number Format is ",                                 ~
            at (09,27), fac(lfac$(10)), str(ser_format$(1),,             ~
                                                    min(20,max_ser_len)),~
            at (10,02),                                                  ~
             "Next Available Serial Number is ",                         ~
            at (10,35), fac(lfac$(11)), str(next_avail$(1),,             ~
                                                    min(20,max_ser_len)),~
            at (11,02),                                                  ~
             "Max Transaction Qty for any Transaction involving Serial Nu~
        ~mbered Parts:",                                                  ~
            at (11,74), fac(lfac$(12)), max_ser_qty%        ,pic(######),~
            at (12,02),                                                  ~
            "Initial Number of Records to Create S/N Master File For ?:",~
            at (12,65), fac(lfac$(13)), min_filesize%       ,pic(######),~
            at (13,02),                                                  ~
             "Avg number of components (1 level) in a Serial Numbered Par~
        ~t:",                                                             ~
            at (13,65), fac(lfac$(14)), mult%               ,pic(####),  ~
            at (14,02),"Calendar structure for recording usage history:",~
            at (14,52), fac(lfac$(15)), cal$,                     ch(01),~
            at (16,03),                                                  ~
        "The above Format Fields define valid entry formats for Lot & Ser~
        ~ial Numbers.",                                                   ~
            at (17,03),                                                  ~
        "'#' = Any character allowed, '+' = Numeric only, %% = Last two d~
        ~igits of the",                                                   ~
           at (18,03),                                                   ~
        "year default in, and any other character is a constant or requir~
        ~ed separator",                                                   ~
           at (19,03),                                                   ~
        "character: Ex. ###-+++-%% would allow serial numbers such as ABC~
        ~-167-87.",                                                       ~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)), pf$(1)                     ,ch(79),~
            at (23,02), fac(hex(8c)), pf$(2)                     ,ch(79),~
            at (24,02), fac(hex(8c)), pf$(3)                     ,ch(79),~
                                                                         ~
                keys(pfk$),                                              ~
                key (keyhit%)

                if keyhit% <> 13 then L42130
                     call "MANUAL" ("HNYFLAGS")
                     goto L41370

L42130:         if keyhit% <> 15 then L42170
                     call "PRNTSCRN"
                     goto L41370

L42170:         close ws
                call "SCREEN" addr("C", n%, "I", i$(), cursor%())
                return
                n% = n%

        set_pf_keys
          if fieldnr% <> 0% then L42340
            pf$(1) = "(1)Start Over       (5)Next Screen      " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                    (9)Maintain HNY Module" &      ~
                     " Administrators      (15)Print Screen"
            pf$(3) = "(2)Reset using Defaults                 " &        ~
                     "                       (16)SAVE DATA   "
            str(pf$(3),63,1) = hex(84)
            pfk$ = hex(000102ffff05090d0f1020ffffff)
            return

L42340:     pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            str(pf$(2),20%,20%) = pf4$
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfk$ = hex(0001040d0fffffffffffff)
            return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   2       *~
            * --------------------------------------------------------- *~
            * Screen for Editing Page 2 of document.                    *~
            *************************************************************

        deffn'112(fieldnr%)
            if fieldnr% = 0% then init(hex(86)) lfac$()  else            ~
                                  init(hex(8c)) lfac$()
            gosub set_pf_keys_2

                  on fieldnr% gosub L43310,   /* Inv Move Var Method    */~
                                    L43310,   /* Revaluation Method     */~
                                    L43310,   /* Revaluation Account    */~
                                    L43310,   /* Valid Locations Req'd  */~
                                    L43310,   /* Locations Transac.Audit*/~
                                    L43310,   /* Usage Dtl,Sum,Neith,Bth*/~
                                    L43310,   /* Include Stores (Usage) */~
                                    L43310,   /* Request Usage Date     */~
                                    L43310,   /* Dflt non-MPS Cal Code  */~
                                    L43310,   /* Usage Capture Method   */~
                                    L43310,   /* Def Source Account     */~
                                    L43310,   /* Def Destination Account*/~
                                    L43310    /* Def Interim WIP Account*/
                     goto L43380

                  REM Set FAC'S for UPPER/LOWER CASE input
                      lfac$(fieldnr%) = hex(80)
                      return
L43310:           REM Set FAC'S for UPPER CASE ONLY input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC'S for NUMERIC ONLY input
                      lfac$(fieldnr%) = hex(82)
                      return

L43380:   accept                                                         ~
            at (01,02), "Manage Inventory Module Behavior Switches",     ~
            at (01,67), "Date:",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (03,02), fac(hex(94)), errormsg$                 , ch(79),~
            at (05,02),                                                  ~
             "Post Variances caused by Inventory Moves to ",             ~
            at (05,48), fac(lfac$( 1)), move_var_method$        , ch(01),~
            at (05,51), fac(hex(8c)),   method_msg$(1)          , ch(25),~
            at (06,02),                                                  ~
             "Post Standard Cost Revaluation Amounts to ",               ~
            at (06,48), fac(lfac$( 2)), revaluation_method$     , ch(01),~
            at (06,51), fac(hex(8c)),   method_msg$(2)          , ch(25),~
            at (07,02),                                                  ~
             "Default Inventory Revaluation Acct",                       ~
            at (07,37), fac(lfac$( 3)), revaluation_acct$       , ch(12),~
            at (07,51), fac(hex(8c)),   revaluation_desc$       , ch(30),~
            at (08,02),                                                  ~
             "Valid Inventory Locations enforced? (Y/N)",                ~
            at (08,48), fac(lfac$( 4)), validlocations$,          ch(01),~
            at (09,02), "Location Transaction Audit Enabled?(Y/N)",      ~
            at (09,48), fac(lfac$( 5)), location_audit$          ,ch(01),~
            at (10,02), "Usage Capture Level (D, S, N, B)",              ~
            at (10,48), fac(lfac$( 6)), dsnb$                    ,ch(01),~
            at (11,02), "Include Store Code in Usage Capture? (Y/N)",    ~
            at (11,48), fac(lfac$( 7)), usestore$                ,ch(01),~
            at (12,02), "Requested Usage Capture Date",                  ~
            at (12,48), fac(lfac$( 8)), req_usage_date$          ,ch(01),~
            at (12,51), fac(hex(8c)),   req_usage_date_descr$    ,ch(24),~
            at (13,02), "Default Non-MPS Calendar Code",                 ~
            at (13,41), fac(lfac$( 9)), calcode$                 ,ch(08),~
            at (13,51), fac(hex(8c)),   caldesc$                 ,ch(30),~
            at (14,02), "Capture Usage for MPS Parts only? (Y/N)",       ~
            at (14,48), fac(lfac$(10)), usage$                   ,ch(01),~
            at (15,02),                                                  ~
             "Inventory Additions Source Default",                       ~
            at (15,37), fac(lfac$(11%)), defsource$             , ch(12),~
            at (15,51), fac(hex(8c)),    defsrcdesc$            , ch(30),~
            at (16,02),                                                  ~
             "Inventory W/drawals Dest'n Default",                       ~
            at (16,37), fac(lfac$(12%)), def_destn$             , ch(12),~
            at (16,51), fac(hex(8c)),    defdstdesc$            , ch(30),~
            at (17,02),                                                  ~
             "Interim BackFlushing (WIP) Default",                       ~
            at (17,37), fac(lfac$(13%)), defintwip$             , ch(12),~
            at (17,51), fac(hex(8c)),    defwipdesc$            , ch(30),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$              ,ch(79),~
            at (22,02), fac(hex(8c)), pf$(1)                     ,ch(79),~
            at (23,02), fac(hex(8c)), pf$(2)                     ,ch(79),~
            at (24,02), fac(hex(8c)), pf$(3)                     ,ch(79),~
                                                                         ~
                keys(pfk$),                                              ~
                key (keyhit%)

                if keyhit% <> 13 then L44170
                     call "MANUAL" ("HNYFLAGS")
                     goto L43380

L44170:         if keyhit% <> 15 then L44210
                     call "PRNTSCRN"
                     goto L43380

L44210:         close ws
                call "SCREEN" addr("C", n%, "I", i$(), cursor%())
                return
                n% = n%

        set_pf_keys_2
          if fieldnr% <> 0% then L44380
            pf$(1) = "(1)Start Over       (4)Previous Screen  " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                    (9)Maintain HNY Module" &      ~
                     " Administrators      (15)Print Screen"
            pf$(3) = "(2)Reset using Defaults                 " &        ~
                     "                       (16)SAVE DATA   "
            str(pf$(3),63,1) = hex(84)
            pfk$ = hex(000102ff04090d0f1020ffffff)
            return

L44380:     pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            str(pf$(2),20%,20%) = pf4$
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfk$ = hex(0001040d0fffffffffffff)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 1.                        *~
            *************************************************************

            deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50222,         /* Lot Tracking Enforced? */~
                              L50226,         /* Detail or Summary ?    */~
                              L50240,         /* Maximum Lot # Len      */~
                              L50270,         /* Minimum Lot # Len      */~
                              L50311,         /* Lot Uniqueness         */~
                              L50320,         /* Lot Nbr Format         */~
                              L50360,         /* Auto Assignment        */~
                              L50400,         /* Maximum Ser # Len      */~
                              L50430,         /* Minimum Ser # Len      */~
                              L50470,         /* Serial Nbr Format      */~
                              L50640,         /* Next Available Serial #*/~
                              L50900,         /* Maximum Ser # Qty      */~
                              L50940,         /* Minimum File Size      */~
                              L50980,         /* Avg # Components       */~
                              L51030          /* Calendar Type          */
            return

L50222: REM Test for Lot Tracking Enforced     LOTTRACK$
            if lottrack$ = "Y" or lottrack$ = "N" then return
            errormsg$ = "Must be Y or N !"
            return
L50226: REM Test for Lot Detail Level          LOTLEVEL$
            if lotlevel$ = "D" or lotlevel$ = "S" then return
            errormsg$ = "Must be S or D !"
            return
L50240: REM Test for Maximum Lot Number Len    MAX_LOT_LEN$
            call "NUMTEST" (max_lot_len$,3, 6, errormsg$,0, max_lot_len)
            return
L50270: REM Test for Minimum Lot Number Len    MIN_LOT_LEN$
            call "NUMTEST" (min_lot_len$,3,max_lot_len, errormsg$,0,     ~
                            min_lot_len)
            min_lot_len = min_lot_len
            return
L50311: REM Test for Lot Uniqueness            LOT_UNIQUE$
            if lot_unique$ = "U" or lot_unique$ = "P" then return
            errormsg$ = "Must be U or P !"
            return
L50320: REM Test for Lot Number Format         LOT_FORMAT$(1)
            if len(lot_format$(1)) < max_lot_len then errormsg$ =        ~
               "Format Length must be as long as the maximum length above"
            return
L50360: REM Test for Auto Assignment              AUTO_SEQ$
            if auto_seq$ = "Y" or auto_seq$ = "N" then return
            errormsg$ = "Must be Y or N"
            return
L50400: REM Test for Maximum Serial Number Len    MAX_SER_LEN$
            call "NUMTEST" (max_ser_len$,3,20, errormsg$,0, max_ser_len)
            return
L50430: REM Test for Minimum Serial Number Len    MIN_SER_LEN$
            call "NUMTEST" (min_ser_len$,3,max_ser_len, errormsg$,0,     ~
                            min_ser_len)
            return
L50470: REM Test for Serial Number Format         SER_FORMAT$(1)
            if len(ser_format$(1)) < min_ser_len then errormsg$ =        ~
         "Format Length must at least as long as the minimum length above"
            if errormsg$ > " " then return
            c%, d% = 0%
            for x% = 1 to len(ser_format$(1))
                if str(ser_format$(1),x%,1%) = "+" then c% = c% + 1%
                if str(ser_format$(1),x%,1%) = "#" then d% = d% + 1%
            next x%
            if c% + d% > 2% then L50610
               errormsg$ = "Format must contain at least 3 '+'s or " &   ~
                           "3 '#'s or a combination thereof"
               return
L50610:     if auto_seq$ = "Y" and c% < 5% then errormsg$ =              ~
               "Format must contain at least 5 '+'s for Auto Assignment!"
            return
L50640: REM Test Next Available Serial Number
            if auto_seq$ <> "Y" then return
            if len(next_avail$(1)) >= min_ser_len then L50710
               errormsg$ = "Next Serial Number must be at least as long "~
                         & "as the Minimum Serial Number Length field!"
               return
L50710:     for x% = 1% to len(next_avail$(1))
                if str(ser_format$(1),x%,1%) = "+" then L50850
                if x% > 19% then L50780
                if str(ser_format$(1),x%,2%) <> "%%" then L50780
                   tmp_dt1$ = date
		   call "DATFMTC" ( tmp_dt1$, tmp_dt%, tmp_dt2$ )
                   str(next_avail$(1),x%,2%) = str(tmp_dt2$,3%,2%)
                   x% = x% + 1%
                   goto L50880
L50780:         if str(ser_format$(1),x%,1%) = str(next_avail$(1),x%,1%) ~
                   then L50880
                if str(ser_format$(1),x%,1%) = "#" then L50880
L50810:            errormsg$ = "Sorry, but a Serial Number MUST be " &   ~
                               "in the format " & ser_format$(1)
                   x% = 99%
                   goto L50880
L50850:         REM *** Test for Digits ***
                if str(next_avail$(1),x%,1%) < "0" or                    ~
                   str(next_avail$(1),x%,1%) > "9" then L50810
L50880:     next x%
            return
L50900: REM Maximum Ser # Qty
            if max_ser_qty% >= 50% and max_ser_qty% <= 1000% then return
            errormsg$ = "Must be between 50 and 1,000"
            return
L50940: REM Minimum File Size
            if min_filesize%>= 100% and min_filesize%<=999999% then return
            errormsg$ = "Must be between 100 and 999,999"
            return
L50980: REM Avg # Components
            if mult% >= 1% and mult% <= 300% then return
            errormsg$ = "Must be between 1 and 300"
            return

L51030: REM Calendar Type
            if cal$ = " " then return
            if cal$ = "F" or cal$ = "G" or cal$ = "B" then return
            errormsg$ = "Must be 'F', 'G', 'B' OR Blank"
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 2.                        *~
            *************************************************************

            deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52300,         /* Inv Move Var Method    */~
                              L52400,         /* Revaluation Method     */~
                              L52500,         /* Revaluation Account    */~
                              L52600,         /* Valid Locations Req'd  */~
                              L52700,         /* Locations Transac.Audit*/~
                              L52800,         /* Usage Dtl,Sum,Neith,Bth*/~
                              L52880,         /* Stores Code (Usage)    */~
                              L52930,         /* Requested Usage Date   */~
                              L53050,         /* Dflt non-MPS Cal Code  */~
                              L53130,         /* Usage Capture Method   */~
                              L53230,         /* Def Source Account     */~
                              L53330,         /* Def Destination Account*/~
                              L53430          /* Def Interim WIP Account*/
            return

L52300: REM Test Inventory Movement Variance Method
            if move_var_method$ = " " then move_var_method$ = "V"
            if move_var_method$ = "V" then L52370
            if move_var_method$ = "A" then L52380
                errormsg$ = "Please enter 'V' for Variance Accts -or-" & ~
                            " 'A' for Adjustment Acct."
                return
L52370:     method_msg$(1) = method_type$(1) : return
L52380:     method_msg$(1) = method_type$(2) : return

L52400: REM Test Inventory Revaluation Method
            if revaluation_method$ = " " then revaluation_method$ = "V"
            if revaluation_method$ = "V" then L52470
            if revaluation_method$ = "R" then L52480
                errormsg$ = "Please enter 'V' for Variance Accts -or-" & ~
                            " 'R' for Revaluation Acct."
                return
L52470:     method_msg$(2) = method_type$(1) : return
L52480:     method_msg$(2) = method_type$(3) : return

L52500: REM Test Revaluation Account
            revaluation_desc$ = " "
            if revaluation_acct$ = " " then return
            call "GETCODE" (#4, revaluation_acct$, revaluation_desc$,    ~
                               0%, 0.30, f1%(4))
            if f1%(4) = 1 then return
                errormsg$ = "Invalid G/L Account Number: " &             ~
                             revaluation_acct$
            return

L52600: REM Test for Valid Locations Enforced
            if validlocations$ = "Y" or validlocations$ = "N" then L52630
                errormsg$ = "Must be 'Y' or 'N'"
                return
L52630:     if validlocations$ = "N" and location_audit$ <> "N" then     ~
                gosub location_askuser
                fieldnr% = 6%
            return

L52700: REM Test for Locations Transaction Audit
            if location_audit$ = "Y" or location_audit$ = "N" then L52735
                errormsg$ = "Must be 'Y' or 'N'"
                return
L52735:     if validlocations$ = "N" and location_audit$ <> "N" then     ~
                gosub location_askuser
            return

L52800: REM Test for usage level- Detail, Summary, Neither, Both DSNB$
            if pos("DSNB" = dsnb$) > 0% then return
            errormsg$ = "Must be 'D', 'S', 'N' or 'B'"
            return

L52880: REM Test for Include Stores Code with Usage Capture   USESTORE$
            if usestore$ = "Y" or usestore$ = "N" then return
            errormsg$ = "Must be 'Y' or 'N'."
            return

L52930: REM Test for Request Usage Capture Date     REQ_USAGE_DATE$
            if pos("RO" = req_usage_date$) > 0% then L52970
                errormsg$ = "Must be 'R' or 'O'"
                return
L52970:     if req_usage_date$ = "R" then req_usage_date_descr$ =        ~
                "Required Ship Date"
            if req_usage_date$ = "O" then req_usage_date_descr$ =        ~
                "Original Order Due Date"
            return

L53050: REM Test for Default non-MPS Calendar Code               CALCODE$
            caldesc$ = " " : if calcode$ = " " then return
            if calcode$ = "?" then calcode$ = " "
            call "PLOWCODE" (#9, calcode$, caldesc$, 0%, .30, f1%(9))
            if f1%(9) <> 0% then return
                errormsg$ = "Calendar Code not on file. Try again."
                return

L53130: REM Test for Usage Capture Method Flag     USAGE$
            if pos("YN" = usage$) = 0% then L53200
            if usage$ = "Y" then return
            if usage$ = "N" and calcode$ <> " " then return
                errormsg$ = "Can Only be 'Y' when Default Calendar" &    ~
                            " Code is NOT Specified"
                return
L53200:     errormsg$ = "Must be 'Y'es or 'N'o"
            return

L53230: REM Test Def Source Account                        DEFSOURCE$
            defsrcdesc$ = " "
            if defsource$ = " " then return   /* Blanks are Okey-Dokey */
            if defsource$ = "?" then defsource$ = " "
            call "GETCODE" (#4, defsource$, defsrcdesc$,     /* GLMAIN */~
                0%, 0.30, f1%(4%))
            if f1%(4%) = 0% then errormsg$ = "You must select a valid G"&~
                "/L Acct # or enter blanks."
            return

L53330: REM Test Def Destination Account                   DEF_DESTN$
            defdstdesc$ = " "
            if def_destn$ = " " then return   /* Blanks are Okey-Dokey */
            if def_destn$ = "?" then def_destn$ = " "
            call "GETCODE" (#4, def_destn$, defdstdesc$,     /* GLMAIN */~
                0%, 0.30, f1%(4%))
            if f1%(4%) = 0% then errormsg$ = "You must select a valid G"&~
                "/L Acct # or enter blanks."
            return

L53430: REM Test Def Interim BackFlush/WIP Account         DEFINTWIP$
            defwipdesc$ = " "
            if defintwip$ = " " then return   /* Blanks are Okey-Dokey */
            if defintwip$ = "?" then defintwip$ = " "
            call "GETCODE" (#4, defintwip$, defwipdesc$,     /* GLMAIN */~
                0%, 0.30, f1%(4%))
            if f1%(4%) = 0% then errormsg$ = "You must select a valid G"&~
                "/L Acct # or enter blanks."
            return

        location_askuser

            call "ASKUSER" (2%, "***** LOCATION MANAGEMENT *****",       ~
                  "Valid Locations should be Enforced when",             ~
                  "Location Transaction Audits are Enabled",             ~
                  "Press Any Key to Continue.             ")

            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "ALLFREE"
            end
