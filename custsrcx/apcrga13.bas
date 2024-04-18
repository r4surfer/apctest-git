        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRGA13                             *~
            *  Creation Date     - 02/29/96                             *~
            *  Last Modified Date- 01/11/06                             *~
            *  Description       - This Program moves RGA Line Items to *~
            *                      a status of 'Voided' and sets the    *~
            *                      header status to 'Open'.             *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/29/96 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 04/04/96 ! Change Reason Code to 3 bytes (Complaint)! JBF *~
            *          !                                          !     *~
            * 10/31/97 ! Changed Program Version ID To 60403      ! DJD *~
            * 01/11/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            *          !                                          !     *~
            *************************************************************

        dim rga_hd_rec$80,               /* RGA Header File Record     */~
/*PAR000*/  rga_dt_rec$(2%)256,         /* RGA Detail File Record     */~
            rga_number$4,                /* RGA Number                 */~
            dtlkey$6,                    /* APCRGADT File Read Key     */~
            date$8,                      /* Date Scanned               */~
            dateout$8,                   /* Date for Screen Display    */~
            errormsg$60,                 /* Error message              */~
            inp_text$79,                 /* Input Prompt Text          */~
            i$(24%)80,                   /* Screen Input Area          */~
            inpmessage$79,               /* Informational Message      */~
            cursor%(2%),                 /*                            */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pfkeys$32,                   /* PF Keys Variable           */~
            progid$,                     /* Screen Line #2 Program ID  */~
            line2$79,                    /* Screen Line #2 Time Field  */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            userid$3                     /* Current User Id            */

        dim f1%(2%),                                                     ~
            f2%(2%),                                                     ~
            fs%(2%),                                                     ~
            rslt$(2%)20


        dim workdate8$8,                  /* mm-dd-yy date             */~
            workdate6$6                   /* Packed Date Format        */
        
   
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 10/31/97  RGA Voided Status Program     "

            mat f2% = con
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! APCRGAHD ! APC RGA Header Master File               *~
            * #02 ! APCRGADT ! APC RGA Detail Master File               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "APCRGAHD",                                      ~
                        varc,     indexed,  recsize =   80,              ~
                        keypos =   12, keylen =   4,                     ~
                        alt key  1, keypos =  10, keylen =   6,          ~
                            key  2, keypos =   1, keylen =  15

            select #2,  "APCRGADT",                                      ~
/*PAR000*/              varc,     indexed,  recsize =  512,              ~
                        keypos =   12, keylen =   6,                     ~
                        alt key  1, keypos =  10, keylen =   8,          ~
                            key  2, keypos =   1, keylen =  17

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%),  f2%(1%),   0%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%),  f2%(2%),   0%, rslt$(2%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)

            workdate6$ = date
            date$ = date
            call "DATEFMT" (date$)

            progid$   = "APCRGA13: " & str(cms2v$,,8)
            inp_text$ = "Enter RGA Number"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode
            init(" ") errormsg$, dateout$, dtlkey$, rga_number$,         ~
                      rga_hd_rec$, rga_dt_rec$()
            fieldnr% = 1%
            option%  = 1%
            gosub update_rga

            goto inputmode

        update_rga
            gosub'100(fieldnr%, option%)

            if keyhit% =  1%        then startover
            if keyhit% = 16%        then exit_sub
            if keyhit% <> 0%        then update_rga
            gosub check_rga
                if errormsg$ <> " " then update_rga

            gosub update_status
                if errormsg$ <> " " then update_rga

        return

        REM *************************************************************~
            *               P R O C E S S   D A T A                     *~
            *************************************************************
        update_status
            gosub process_apcrgahd

            dtlkey$             = all(hex(20))
            str(dtlkey$,1%,4%)  = rga_number$
            gosub process_apcrgadt

        return

        process_apcrgadt
            read #2,hold,key > dtlkey$, using L19150, rga_dt_rec$(),        ~
                eod goto L19290
L19150:         FMT 2*CH(256)                /*PAR000*/

            if str(rga_dt_rec$(),12%,4%) <> rga_number$ then L19290
            delete #2

            str(rga_dt_rec$(),233%,3%) = userid$
/* Y2K */
            str(rga_dt_rec$(),236%,6%) = workdate6$
/* Y2K */
            str(rga_dt_rec$(),10%,2%)  = "21"
            put   #2, using L19150, rga_dt_rec$()

            write #2, eod goto L19270

L19270:     dtlkey$  = str(rga_dt_rec$(),12%,6%)
            goto process_apcrgadt
L19290: return

        process_apcrgahd
            read #1,hold,key = rga_number$, using L30090, rga_hd_rec$,    ~
                eod goto L19440

            delete #1

            str(rga_hd_rec$,40%,3%) = userid$
/* Y2K */
            str(rga_hd_rec$,43%,6%) = workdate6$
/* Y2K */
            str(rga_hd_rec$,10%,2%) = "05"
            put   #1, using L30090, rga_hd_rec$

            write #1, eod goto L19440

L19440: return

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
            errormsg$ = " "
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            rec% = 0%
            read  #1,key = rga_number$, using L30090, rga_hd_rec$,        ~
                eod goto L30120
L30090:         FMT CH(80)

            rec% = 1%
L30120: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

        REM *************************************************************~
            *               F O R M A T    S T A T E M E N T S          *~
            *************************************************************

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
        deffn'100(fieldnr%, option%)
            dateout$ = " "
            call "TIME" (dateout$)

            str(line2$,72%) = dateout$
            inpmessage$     = inp_text$
            gosub set_pfkeys

            accept                                                       ~
                at (01,02)                                      ,        ~
                   "RGAII Voided Status"                        ,        ~
                at (01,36), "APC Building Products"             ,        ~
                at (01,66), "Today:"                            ,        ~
                at (01,73), fac(hex(8c)),   date$               , ch(08),~
                                                                         ~
                at (02,02), fac(hex(ac)),   line2$              , ch(79),~
                                                                         ~
                at (04,02), fac(hex(94)),   errormsg$           , ch(79),~
                                                                         ~
                at (07,02), "RGA No.    : "                     ,        ~
                at (07,16), fac(lfac$(1%)), rga_number$         , ch(04),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1)              , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2)              , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3)              , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <> 15% then L40370
                     call "PRNTSCRN"

L40370:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

            return

        set_pfkeys
        REM                          /*  Input Mode             */
            pf$(1) = "(1)Start Over                            " &       ~
                     "                                      "
            pf$(2) = "                                         " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "                                         " &       ~
                     "                      (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************
        check_rga
            errormsg$ = " "
            convert rga_number$ to rga_number%, data goto L50140

            convert rga_number% to rga_number$, pic(0000)

            gosub dataload

            if rec% = 1%             then L50160
L50140:         errormsg$ = "Invalid RGA Number! Re-enter Again."
                goto L50180
L50160:     if str(rga_hd_rec$,10%,2%) < "04" then L50180
                errormsg$ = "RGA ("& rga_number$ &") Dispersed !!"
L50180: return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_sub
            call "SHOSTAT" ("One Moment Please")

        end
