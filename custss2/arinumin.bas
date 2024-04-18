        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *              Last Mod - 12/06/00  - CMG                   *~
            *   AAA   RRRR   IIIII  N   N  U   U  M   M  IIIII  N   N   *~
            *  A   A  R   R    I    NN  N  U   U  MM MM    I    NN  N   *~
            *  AAAAA  RRRR     I    N N N  U   U  M M M    I    N N N   *~
            *  A   A  R   R    I    N  NN  U   U  M   M    I    N  NN   *~
            *  A   A  R   R  IIIII  N   N   UUU   M   M  IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARINUMIN - Handles logic for entry of Customer Code,      *~
            *            Invoice Number, SO & BOL Numbers.  Changed to  *~
            *            a subroutine to free up space in ARIINPUT and  *~
            *            to lend a bit more intellegence to the process.*~
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
            * 12/01/87 ! Original                                 ! ERN *~
            * 03/06/89 ! Exports now scan'd; added Select SO & BOL! JDH *~
            * 05/30/89 ! SESSION and POSTDATE only displayed for  ! MLJ *~
            *          !   non-export invoices,  PLOWHDR$ spacing !     *~
            *          !   modified, PLOWKEY$ in buffer scan now  !     *~
            *          !   all HEX zeros.                         !     *~
            * 10/11/89 ! Init vars @ INPUTMODE, all appropriate   ! JDH *~
            *          !   PLOWs now honor the Export flag.       !     *~
            * 06/03/91 ! PRR 11594- Add ARIMASTR to call. Add scan! JBK *~
            *          !  of recurring masters for inv type 'R'   !     *~
            * 05/09/95 ! PRR 13406 - Changed element 14 on #11260 ! MLJ *~
            *          !  to valid UFB (#15) instead of dummy 999.!     *~
            * 12/06/00 ! Mod to be able to pull up old invoice and! CMG *~
            *          !     edit for only 'C'redits (EWD001)     !     *~            
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ARINUMIN"   (invtype$,      /* Invoice Type Code          */~
                          invtypedescr$, /* Type Code spelled out      */~
                          fd3$,          /* Invoice# Field Prompt      */~
                          session$,      /* Invoice Session            */~
                          postdate$,     /* Post Date (Formatted)      */~
                          #9 ,           /* ARIBUFFR                   */~
                          #5 , #6 ,      /* BCKMASTR, BCKLINES         */~
                          #1 , #17,      /* CUSTOMER, SHPHDRS          */~
                          #8 , #25,      /* GENCODES, ARMTRIAL         */~
                          #15,           /* ARIMASTR                   */~
                          errormsg$,     /* Data Load Error Message    */~
                          cuscode$,      /* Ship-to Customer           */~
                          invnr$,        /* Invoice Number             */~
                          so$,           /* Sales Order Number         */~
                          bol$,          /* Bill of Lading Number      */~
                          keyhit%)       /* Return Code                */
                                         /*   0- Do Data Load          */
                                         /*  16- Exit Program          */

        dim                                                              ~
            billxref$9,                  /* Bill-to Cross Reference    */~
            bol$3,                       /* Bill of Lading Number      */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9, cusdescr$30,      /* Ship-to Customer, Name     */~
            descr_m(8),                  /* Descr map for Plowcode     */~
            errormsg$79,                 /* Error message              */~
            fd$(3)26, fd3$26,            /* Data Field Descriptors     */~
            i$(24)80,                    /* Screen Image               */~
            info$79,                     /* Misc Informative text      */~
            invnr$8,                     /* Sales Order Number         */~
            invtype$1,                   /* Type of Invoice            */~
            invtypedescr$30,             /* Screen Header              */~
            ix(1), ix$(1)6,              /* Include/Exclude for Plow   */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            msg$(3)79,                   /* Screen Messages            */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pgmname$8,                   /* ID for MANUAL calls        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowhdr$(3)79,               /* Plowcode Headers           */~
            postdate$8,                  /* Post Date (from Session)   */~
            posthdr$20,                  /* Screen Post Date Heading   */~
            session$6,                   /* Session Number             */~
            sesnhdr$15,                  /* Screen Session Heading     */~
            so$16,                       /* Sales Order - BOL          */~
            status$16,                   /* Customer's Status          */~
            userid$3                     /* Current User Id            */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.01 06/23/95 Patch Finalization of R6.04.01  "
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            init (" ")  posthdr$, sesnhdr$
            call "EXTRACT" addr("ID", userid$)
            pgmname$ = "ARIINPT" & invtype$
            if invtype$ = "X" then                                       ~
                str(line2$,46) = sesnhdr$                                ~
            else                                                         ~
                str(line2$,46) = "Session: " & session$
            if invtype$ <> "X" then                                      ~
                str(posthdr$) = "     Post: " & postdate$
            str(line2$,62) = str(pgmname$) & ": " & str(cms2v$,,8)
            if pos("OX" = invtype$) = 0% then let so$, bol$ = " "
            cusdescr$ = " "
            if cuscode$ <> " " then                                      ~
                call "GETCODE" (#1, cuscode$, cusdescr$, 0%, 99, f1%)

*        Describe Fields and Input Messages.
            fd$(), msg$() = " "  :  fd$(3) = fd3$
            lfac$() = hex(81)
            if pos("OX" = invtype$) = 0% then L09290
                fd$(1)  = "Sales Order & BOL"
                fd$(2)  = "Ship-to Customer Code"
                msg$(1) = "For a NEW Invoice, Enter Sales Order Number" &~
                            " (and BOL if it exists);"
                msg$(2) = "Enter Invoice Number to assign it, or"       &~
                          " leave it blank to have it assigned."
                msg$(3) = "To RECALL a previous entry, Enter its"       &~
                          " Customer Code and Invoice Number."
                goto L10000

L09290:         lfac$(1) = hex(9c)
                fd$(2)   = "Ship-to Customer Code"
                if invtype$ = "F" then str(fd$(2),,4) = "Bill"
                msg$(1)  = "For a NEW Invoice, enter the Customer Code" &~
                            " and Invoice Number;"
                msg$(2)    = "You may leave the Invoice Number blank"   &~
                             " to have it automatically assigned."
                msg$(3)   = "To RECALL a previous entry, enter its"     &~
                            " Customer Code and Invoice Number."
                if invtype$ <> "M" then L09410
                     msg$(2) = msg$(1)
                     msg$(1) = " "
L09410:         if invtype$ <> "R" then L09460
                     msg$(1) = " "
                     msg$(2) = "To add or modify a Recurring Master,"   &~
                               " enter the Customer Code and the"
                     msg$(3) = "Recurring Group Code."
L09460:         if invtype$ <> "G" then L10000
                     msg$(1), msg$(2) = " "
                     msg$(3) = "To edit a generated invoice, enter its" &~
                               " Customer Code and Invoice Number."

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Get enough data to determine what user wants to do.       *~
            *************************************************************

        inputmode

L10090:     gosub data_screen
                errormsg$   = " "
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  6% then gosub get_customer
                if keyhit%  =  7% then gosub get_so
                if keyhit%  =  8% then gosub get_bol
                if keyhit%  =  9% then gosub get_invoice
                if keyhit%  = 10% then gosub scan_buffer
                if keyhit%  = 16% then       exit_program
                if keyhit% <>  0% then       L10090
            gosub test_data
                if errormsg$ <> " " then L10090

           goto exit_program


        get_customer
           cusdescr$ = hex(06) & "Select Customer Number"
           call "GETCODE" (#1, cuscode$, cusdescr$, 0%, 1.30, f1%)
           return

        get_so
           if invtype$ = "C" then find_invoice           /* (EWD001) */
           if invtype$ = "X"                                             ~
              then info$ = hex(06) & "Select Export Sales Order"         ~
              else info$ = hex(06) & "Select Non-Export Sales Order"
           if cuscode$ = " " then u3% = 5000% else u3% = 5009%
           if invtype$ = "X" then ix(1) =  857.010                       ~
                             else ix(1) = -857.010
           ix$(1) = "Y     "
           plowhdr$(1) = "  Sales Order        PO Number"
           if cuscode$ = " " then plowhdr$(1) = "  Customer" &           ~
                                                       str(plowhdr$(1),2)
           plowkey$ = str(cuscode$,,9) & so$
           call "PLOWCODE" (#5, plowkey$, info$, u3%, 0.16, f1%,         ~
                                plowhdr$(), 0, 0, ix(), ix$())
           if f1% = 0% then return
               get #5 using L10390, cuscode$, so$
L10390:              FMT CH(9), CH(16)
               call "GETCODE" (#1, cuscode$, cusdescr$, 0%, 99, f1%)
               return

        get_bol
           if invtype$ = "X"                                             ~
              then info$ = hex(06) & "Select Export BOL"                 ~
              else info$ = hex(06) & "Select Non-Export BOL"
           if cuscode$ = " " then u3% = 5000% else u3% = 5009%
           if invtype$ = "X" then ix(1) =  246.010                       ~
                             else ix(1) = -246.010
           ix$(1) = "Y     "
           plowhdr$(1) = "  Sales Order     BOL Number"
           if cuscode$ = " " then plowhdr$(1) = "  Customer" &           ~
                                                       str(plowhdr$(1),2)
           plowkey$ = str(cuscode$,,9) & so$
           call "PLOWCODE" (#17, plowkey$, info$, u3%, 0.00, f1%,        ~
                                 plowhdr$(), 0, 0, ix(), ix$())
           if f1% = 0% then return
               so$      = str(plowkey$,10,16)
               bol$     = str(plowkey$,26, 3)
               cuscode$ = str(plowkey$, 1, 9)
               call "GETCODE" (#1, cuscode$, cusdescr$, 0%, 99, f1%)
               return

        get_invoice
           info$ = hex(06) & "Select Invoice (Type '" & invtype$ &       ~
                             "') to Edit..."
           init (hex(00)) plowkey$
           plowhdr$(1) = "  Customer Invoice    PO              SO"
           plowkey$ = str(session$,,6) & str(invtype$,,1) & str(cuscode$)
           call "PLOWCODE" (#9, plowkey$, info$, 1007%, 1.35, f1%,       ~
                                                              plowhdr$())
           if f1% = 0% then return
L10510:        get #9 using L10520, cuscode$, invnr$, so$, bol$
L10520:             FMT CH(9), CH(8), XX(16), CH(16), CH(3)
               call "GETCODE" (#1, cuscode$, cusdescr$, 0%, 99, f1%)
               return

        scan_buffer
            if invtype$ = "R" then scan_recurring
            ix$(1) = "X     "
            if invtype$ = "X" then ix(1) = 2007.010                      ~
                              else ix(1) = -2007.010
           if invtype$ = "X"                                             ~
              then info$ = hex(06) & "EXPORT Invoices in all Sessions"   ~
              else info$ = hex(06) & "NON-EXPORT Invoices in all Sessions"
           plowhdr$(1)= "         Customer Invoice    PO              SO"
           init (hex(00)) plowkey$
           call "PLOWCODE" (#9, plowkey$, info$, 5000%, 1.35, f1%,       ~
                            plowhdr$(), 0, 0, ix(), ix$())
           if f1% = 0% then return
           if str(plowkey$,,7) = str(session$,,6) & str(invtype$,,1)     ~
                                                   then L10510 else return

        scan_recurring
            info$ = hex(06) & "Master RECURRING Invoices"
            ix$(1) = "R     "  :  ix(1) = 891.010
            init (hex(00)) plowkey$
            if cuscode$ = " " then L11170

            plowkey$ = str(cuscode$,,9) & "RCR-"
            info$ = str(info$,,26) & " for " & cuscode$ & "  " &         ~
                    cusdescr$
            u3% = 9009%  :  u4 = 0.35
            plowhdr$(1) = "  Invoice   PO                Expire Date"
            descr_m(1) =   10.080  :  descr_m(2) = 0001.0
            descr_m(3) =   18.160  :  descr_m(4) = 0011.0
            descr_m(5) =  527.061  :  descr_m(6) = 0029.0
            descr_m(7) =    0      :  descr_m(8) =    0
            goto L11260

L11170:     plowkey$ = "RCR-"
            u3% = 9004%  :  u4 = 1.35
            plowhdr$(1) = "  Customer   Invoice   PO                Expir~
        ~e Date"
            descr_m(1) =   01.090  :  descr_m(2) = 0001.0
            descr_m(3) =   10.080  :  descr_m(4) = 0012.0
            descr_m(5) =   18.160  :  descr_m(6) = 0022.0
            descr_m(7) =  527.061  :  descr_m(8) = 0040.0

L11260:     call "PLOWCODE" (#15, plowkey$, info$, u3%, u4, f1%,         ~
                            plowhdr$(), 0, 0, ix(), ix$(), "D", " ",     ~
                            #15, descr_m())
            if f1% = 0% then return
                get #15 using L11310, cuscode$, invnr$
L11310:              FMT CH(9), CH(8)
                call "GETCODE" (#1, cuscode$, cusdescr$, 0%, 99, f1%)
                return

        find_invoice                   /* (EWD001) - Begin */
           fd$(3) = "Invoice Num to Recall"        
           info$ = hex(06) & "Select Invoice (Type '" & invtype$ &       ~
                             "') to Edit..."        
           init (hex(00)) plowkey$
           plowhdr$(1) = "  Customer Invoice    PO              SO"
           plowkey$ = str(cuscode$) & str(invnr$,1%,8%) 
           call "PLOWCODE" (#15, plowkey$, info$, 0%, 0.35, f1%)
           
           if f1% = 0% then return
               get #15 using L10520, cuscode$, invnr$, so$, bol$

               call "GETCODE" (#1, cuscode$, cusdescr$, 0%, 99, f1%)
               return                  /* (EWD001) - End */
               
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
            init (" ") cuscode$, invnr$, so$, bol$, cusdescr$
            fd$(3) = fd3$
                return clear all
                goto   inputmode

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        data_screen
              gosub set_pf1

L40090:     accept                                                       ~
               at (01,02), fac(hex(8c)), invtypedescr$,                  ~
               at (01,62), fac(hex(8c)), posthdr$               , ch(20),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(8c)),   fd$(1),                       ~
               at (06,30), fac(lfac$( 1)), so$                  , ch(16),~
               at (06,47), fac(lfac$( 1)), bol$                 , ch(03),~
                                                                         ~
               at (07,02), fac(hex(8c)),   fd$(2),                       ~
               at (07,30), fac(lfac$( 2)), cuscode$             , ch(09),~
               at (07,49), fac(hex(8c))  , cusdescr$            , ch(30),~
                                                                         ~
               at (08,02), fac(hex(8c)),   fd$(3),                       ~
               at (08,30), fac(lfac$( 3)), invnr$               , ch(08),~
                                                                         ~
               at (19,02), fac(hex(84)),   msg$(1)              , ch(79),~
               at (20,02), fac(hex(84)),   msg$(2)              , ch(79),~
               at (21,02), fac(hex(a4)),   msg$(3)              , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40400
                  call "MANUAL" (pgmname$) : goto L40090

L40400:        if keyhit% <> 15 then L40430
                  call "PRNTSCRN" : goto L40090

L40430:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            pf$(1) = "(1)Start Over   (6)Select Customer      " &        ~
                     " ( 9)Recall Invoice    (13)Instructions"
            pf$(2) = "                (7)Select Sales Order   " &        ~
                     " (10)Scan Buffer File  (15)Print Screen"
            pf$(3) = "                (8)Select BOL           " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffff060708ff090affff0dff0f1000)
            if pos("OX" = invtype$) <> 0% then L40570
                str(pf$(2),16,22) = " " : str(pfkeys$,7,1) = hex(ff)
                str(pf$(3),16,22) = " " : str(pfkeys$,8,1) = hex(ff)                
             if pos("C" = invtype$) <> 0% then L40600
L40570:     if invtype$ <> "R" then L40590
                str(pf$(1),42,18) = " " : str(pfkeys$,9,1) = hex(ff)
                str(pf$(2),42,20) = "(10)Scan Recurring"
L40590:     return
L40600:         str(pf$(2),16,22) = " (7)Find Prev Invoice"
                str(pfkeys$,7,1) = hex(07)
            return
        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        test_data
            errormsg$ = " "
            onfile%   = 0%

*        Recurring Invoices occur here....
            if invtype$ <> "R" then L50270

            if cuscode$ = " " then                                       ~
                errormsg$ = "You must supply the Customer Code for a"  & ~
                            " Recurring Invoice."                        ~
                else gosub load_customer
            if errormsg$ <> " " then return

            if str(invnr$,,4) = "RCR-" then invnr$ = str(invnr$,5)
            info$ = hex(06) & "Select Recurring Group Code"
            plowkey$ = "RECURGRPS" & invnr$
            call "PLOWCODE" (#8, plowkey$, info$, 9%, 0.30, f1%)
            if f1% = 1% then invnr$ = "RCR-" & str(plowkey$,10)          ~
                        else errormsg$ = "Invalid Recurring Group Code."
            return

L50270
*        If a Sales Order # entered, test it and get its Customer Code.
*        The BOL will be tested in the main program's data load section.
            if pos("OX" = invtype$) = 0% then L50290
                if so$ <> " " or (cuscode$ <> " " and invnr$ <> " ")     ~
                                                               then L50290
                     errormsg$ = "For a New Invoice, enter SO-BOL; to"  &~
                                 " Edit an Invoice, enter Customer &"   &~
                                 " Invoice #s."
                     return
L50290:     if so$ = " " then L50390

            plowkey$ = str(so$,,16) & hex(000000)
            call "PLOWNEXT" (#6, plowkey$, 16%, f1%)
            if f1% = 1% then L50360
                errormsg$ = "Sales Order not on file."
                return
L50360:     get #6 using L50370, cuscode$
L50370:         FMT CH(9)

L50390
*        Now we are getting warmer.  Check if we have enough to
*        determine if the invoice is in the buffer or not.
            if cuscode$ <> " " then L50440
                errormsg$ = "Please enter Customer Code."
                return
L50440:     if invnr$ = " " and pos("MG" = invtype$) > 0% then           ~
                     errormsg$ = "Invoice number must be supplied."
            if str(invnr$,,4) = "RCR-" and invtype$ <> "R" then          ~
                     errormsg$ = "The prefix 'RCR-' is reserved for"  &  ~
                                  " Recurring Invoices."
            if errormsg$ <> " " then return
            plowkey$ = str(cuscode$,,9) & invnr$
            call "READ100" (#9, plowkey$, onfile%)
            if invtype$ <> "G" or onfile% = 1% then L50550
                errormsg$ = "You may only edit invoices in the buffer."
                return
L50550:     gosub load_customer : if errormsg$ <> " " then return
            if onfile% = 1% then return
              plowkey$ = str(billxref$,,9) & str(invnr$,,8) & hex(00)
              call "PLOWNEXT" (#25, plowkey$, 17%, f1%)
              if f1% = 0% then return
                u3% = 2%
                call "ASKUSER" (u3%, "DUPLICATE SETTLEMENT",             ~
                     "The Invoice Number already exists as a settlement",~
                     "number for this customer's bill-to.",              ~
                     "Press PF-16 to accept or RETURN to re-enter.")
                if u3% <> 16% then errormsg$ = hex(00)
                return

        load_customer
            call "READ100" (#1, cuscode$, f1%)
            if f1% = 1% then L50730
                errormsg$ = "Customer not on file.  Use PF-6 for list."
                return
L50730:     if onfile% = 1% then return
                get #1 using L50750, cusdescr$, billxref$, status$
L50750:              FMT XX(9), CH(30), POS(780), CH(9), POS(793), CH(1)
                if invtype$ <> "F" or billxref$ = cuscode$ then  L50800
                     errormsg$ = "Customer must be a Bill-to for"  &     ~
                                 " Finance Charge Invoices."
                     return
L50800:         if status$ = "A" then return
                     if status$ = "H" then status$ = "On Hold"
                     if status$ = "I" then status$ = "Inactive"
                     if status$ = "D" then status$ = "To be Deleted"
                     u3% = 2%
                     call "ASKUSER" (u3%, "STATUS CHECK",                ~
                          "This Customer is " & status$ & ".",           ~
                          "Press PF-16 to continue with this Customer,", ~
                          "-or- Press (RETURN) to re-enter Customer.")
                     if u3% = 16% then return
                          errormsg$ = hex(00)
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
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            end