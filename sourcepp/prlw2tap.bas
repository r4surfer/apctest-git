        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   RRRR   L      W   W   222   TTTTT   AAA   PPPP    *~
            *  P   P  R   R  L      W   W      2    T    A   A  P   P   *~
            *  PPPP   RRRR   L      W   W   222     T    AAAAA  PPPP    *~
            *  P      R  R   L      W W W  2        T    A   A  P       *~
            *  P      R   R  LLLLL   W W   22222    T    A   A  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLW2TAP - Creates a W-2 Magnetic tape file.              *~
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
            * 11/20/87 ! ORIGINAL                                 ! DAW *~
            * 02/22/88 ! Cleaned record layouts.                  ! DAW *~
            * 11/09/88 ! Removed STOP Statements                  ! MJB *~
            * 12/10/88 ! Made SSA-required mods for 1988 report.  ! JIM *~
            * 11/20/89 ! Added field to 'W', 'I' and 'T' records  ! MJB *~
            *          !  for 1989 changes.                       !     *~
            * 11/22/89 ! Modified to use PRLWYYYY as input        ! MJB *~
            * 01/03/90 ! Added indicators for Pension Plan,       ! MJB *~
            *          !  Deferred Compensation and Statutory Emp !     *~
            * 01/12/90 ! Changed PRLWYYYY Record length to 330    ! MJB *~
            * 11/26/90 ! General Clean-up, hopefully making future! KAB *~
            *          ! changes into a 'monkey see, monkey do'   !     *~
            *          ! type task. (Sources for new information  !     *~
            *          ! are, unfortunately, another issue).      !     *~
            *          !  *** Some Clean-Up of the file dump ***  !     *~
            *          ! No allusion toward standards, but at     !     *~
            *          ! least its more legible for programmer    !     *~
            *          ! types.  DISPLAY print-out may still be   !     *~
            *          ! better but at least this now includes    !     *~
            *          ! 'when,' 'where from', 'where to' and     !     *~
            *          ! 'who' type information.                  !     *~
            * 12/04/91 ! PRR 12142 - Added Medicare tax and wage  ! JBK *~
            *          ! base for 1991.  Expanded Box17 items.    !     *~
            *          ! Added coding for Wages > 10 million $.   !     *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 02/13/95 ! Added Third Party W/H and increase size  ! JBK *~
            *          !  of Medicare Wages and Tax fields for '94!     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                              /* General Purpose Variables  */~
                box17sumto$14,           /* Control Box 17 Codes       */~
                msg_text$(5)70,          /* GETPARM Message Text       */~
                pfmask$4,                /* GETPARM PF Key Mask        */~
                pfkey$1,                 /* GETPARM PF Key Def/Returned*/~
                w2year$4,                /* Extract File Suffix        */~
                prname$8,                /* Extract File Name          */~
                msg$60,                  /* General Purpose Error Msg  */~
                namekey$100,             /* Extract File Plow Key      */~
                savekey$100,             /* Extract File Saved Key     */~
                add_wages$1,             /* Employee Additional Wages  */~
                date$8,                  /* System Date                */~
                time$8,                  /* System Time                */~
                line$(32)10,             /* Print Lines                */~
                line1$100,               /* Print Lines                */~
                file1$8, lib1$8, vol1$6, /* File, lib, vol of input    */~
                file2$8, lib2$8, vol2$6, /* File, lib, vol of output   */~
                userid$3,                /* Who to blame               */~
                filler$250,              /* Filler (Spaces)            */~
                i%(1)                    /* Search Target              */

        dim                              /* Type A Record Variables    */~
                pay_year$4,              /* Payment Year               */~
                ein$9,                   /* Transmitter E.I.N. #       */~
                forgn_a$1,               /* Foreign Address flag       */~
                tran_name$50,            /* Transmitter Name           */~
                address$40,              /* Street Address             */~
                city$25,                 /* City                       */~
                state$2,                 /* State                      */~
                zip_code$5,              /* Zip/Foreign Postal code    */~
                zip_extn$5               /* Zip Code extension         */~

        dim                              /* Type B Record Variables    */~
                computer$8,              /* Computer                   */~
                label$2,                 /* Internal Labeling          */~
                density$2,               /* Density                    */~
                character$3,             /* Character Set              */~
                forgn_b$1,               /* Foreign Address flag       */~
                ret_name$44,             /* File Return Name           */~
                ret_address$35,          /* File Return Address        */~
                ret_city$20,             /* File Return City           */~
                ret_state$2,             /* File Return State          */~
                ret_zip_code$5,          /* Zip/Foreign Postal code    */~
                ret_zip_extn$5           /* Zip/Foreign extension      */

        dim                              /* Type E Record Variables    */~
                local69$9,               /* State/Local 69 Number      */~
                name_code$1,             /* Name Code                  */~
                employment$1,            /* Type of Emploment          */~
                block$2,                 /* Blocking Factor            */~
                est_number$4,            /* Establishment Number       */~
                liability$1,             /* Liability Limitation       */~
                thirdparty$12,           /* 3rd Party Withholding      */~
                otherein$9               /* Other EIN                  */

        dim                              /* Type W Record Variables    */~
                ssn$9,                   /* Social Security Number     */~
                empname$27,              /* Employee Name              */~
                empaddress$40,           /* Street Address             */~
                empcity$25,              /* City                       */~
                empstate$10,             /* State                      */~
                empextn$5,               /* Zip code extension         */~
                empzip$5,                /* Zip Code                   */~
                statutory$1,             /* Statutory Employee Code    */~
                ficawages$7,             /* Social Security Wages      */~
                tips$7,                  /* Social Security Tips       */~
                wages$9,                 /* Annual Wages & Compensatn  */~
                ficatax$6,               /* Social Security Tax        */~
                tax$9,                   /* Federal Income Tax         */~
                tipsalloc$7,             /* Allocated Tips             */~
                fringes$9,               /* Fringe Benifits            */~
                nq457$9,                 /* Non Qualified 457          */~
                nqn457$9,                /* Non Qualified non 457      */~
                dcass$7,                 /* Dep Care Assistance        */~
                control$7,               /* Control Number             */~
                insurance$7,             /* Employer Cost of Insur.    */~
                unfica$7,                /* Uncollected SSA Tax Tips   */~
                aeic$7,                  /* A.E.I.C.                   */~
                pplan$1,                 /* Pension Plan Indicator     */~
                dcomp$1,                 /* Deffered Comp Indicator    */~
                dcomp_amt$9,             /* Deffered Comp Amount       */~
                medicarewages$9,         /* Medicare Wages             */~
                medicaretax$7            /* Medicare Tax               */

        dim                              /* Extract Fields not Directly*/~
                                         /* Passed to the Mag Tape File*/~
                ext_ssno$11,             /* SSAN                       */~
                ext_empname$30,          /* Employee Name              */~
                ext_empaddr$(3)30,       /* Employee Address           */~
                ext_ezip$9,              /* Employee Zip Code          */~
                ext_box$(8)1,            /* W-2 Check Off Boxes        */~
                ext_box17$(4)1,          /* Box 17 Identifiers         */~
                ext_box17(4)             /* Box 17 amounts             */

        dim                              /* Type I Record Variables    */~
                sficawages$10,           /* Social Security Wages      */~
                stips$10,                /* Social Security Tips       */~
                swages$10,               /* Annual Wages & Comp        */~
                sficatax$10,             /* FICA Taxes                 */~
                stax$10,                 /* Federal Income Tax         */~
                scontrol$7,              /* Control Number             */~
                sinsurance$10,           /* Employer Cost of Insur     */~
                sunfica$10,              /* Uncollected FICA on Tips   */~
                saeic$11,                /* A.E.I.C.                   */~
                stipsalloc$10,           /* Allocated Tips             */~
                sfringes$10,             /* Fringe Benifits            */~
                sdcomp_amt$10,           /* Deffered Comp Amount       */~
                sdcass$10,               /* Dep Care Assistance        */~
                snq457$10,               /* Non Qualified 457          */~
                snqn457$10,              /* Non Qualified non 457      */~
                smedicarewages$11,       /* Medicare Wages             */~
                smedicaretax$10          /* Medicare Tax               */

        dim                              /* Type T Record Variables    */~
                tcount$7,                /* Number of Employees        */~
                tficawages$13,           /* Social Security Wages      */~
                ttips$12,                /* Annual Tips                */~
                twages$13,               /* Annual Wages               */~
                tficatax$12,             /* FICA Withheld              */~
                ttax$12,                 /* Federal Withholdings       */~
                tinsurance$12,           /* Employer Cost of Insur     */~
                tunfica$12,              /* Uncollected FICA on Tips   */~
                taeic$12,                /* A.E.I.C.                   */~
                ttipsalloc$12,           /* Allocated Tips             */~
                tfringes$12,             /* Fringe Benifits            */~
                tdcomp_amt$13,           /* Deferred Amount            */~
                tdcass$12,               /* Dep Care Assistance Total  */~
                tnq457$13,               /* Non Qualified 457          */~
                tnqn457$13,              /* Non Qualified non 457      */~
                tmedicarewages$13,       /* Medicare Wages             */~
                tmedicaretax$12          /* Medicare Tax               */

        dim                              /* Type F Record Variables    */~
                fcount$7,                /* Total Employees            */~
                fficawages$16,           /* SSA wages                  */~
                ftips$16,                /* SSA tips                   */~
                fwages$16,               /* Total wages, tips, etc.    */~
                fficatax$16,             /* SSA tax withheld           */~
                ftax$16,                 /* Fed income tax withheld    */~
                faeic$16                 /* AEIC total                 */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            fs%(64)                      /*  for OPENCHCK              */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.01 06/23/95 Patch finalization of R6.04.00  "

            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            * #5  ! PRLWYYYY ! Payroll W2 Extract File for Year YYYY    *~
            * #10 ! PRW2TAPE ! Magnetic W-2 Tape file                   *~
            *************************************************************~

            select #1,  "SYSFILE2", varc, indexed, recsize = 500,        ~
                        keypos =    1, keylen =  20

            select #5,  "PRLWYYYY",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos = 1,    keylen = 13,                      ~
                        alt key  1, keypos = 40, keylen = 12,            ~
                            key  2, keypos = 14, keylen = 38

            select #10, "PRW2TAPE",       consec,  recsize = 275,        ~
                        eod goto L65000


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, " ")

        set_getparm
            pfmask$ = hex(00010000)  :  pfkey$ = "@"
            msg_text$(1) = " "
            msg_text$(2) = hex(84) & "Enter the Reporting" &             ~
                           " Year for the W2 Extract File"
            msg_text$(3) = " "
            msg_text$(4) = hex(84) & "Press RETURN to Continue "  &      ~
                                     "or PF-16 to EXIT"
            str(msg_text$(1),69) = hex(0d)
            str(msg_text$(2),69) = hex(0d)
            str(msg_text$(3),69) = hex(0d)
            str(msg_text$(4),69) = hex(0d)

            w2year$ = "    "
L05140:     call "GETPARM" addr("I ", "R", "  Year  ", pfkey$,           ~
                           "W204", "W2TAP ", msg_text$(), 350%,          ~
                            "K", "W2YEAR  ", w2year$, 4%,                ~
                            "A", 14%, "A", 45%, "A",                     ~
                            "T", "W2 Reporting Year ", 17%,              ~
                            "A", 14%, "A", 20%, "P", pfmask$, "E")

            if pfkey$ = "P" then L65000
            if pfkey$ <> "@" then L05140

            call "SHOSTAT" ("Opening Files, One Moment Please")

            prname$ = "PRLW" & str(w2year$)
            call "PUTPRNAM" addr(#5, prname$)

            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, " ")

            if f2%(5) = 0% then L05430

L05340:     ask% = 0%
            call "ASKUSER" (ask%, "*** NO FILE ***",                     ~
                 "The W-2 File for " & w2year$ & " Cannot be Found",     ~
                 "    Press PF-1 to Re-Select the Reporting Year",       ~
                 " or Press PF-16 to EXIT This Program")
            if ask% = 1% then set_getparm
            if ask% = 16% then L65000
            goto L05340

L05430: REM *************************************************************~
            *  C H E C K   F O R   F I L E   A N D   D E S T R O Y      *~
            *************************************************************

            call "OPENFILE" (#10, "IO   ", f2%(10), " ", " ")
            if f2%(10) <> 0 then L05580 /* If file not found, create it */
            call "READNEXT" (#10, f1%(10))
            if f1%(10) = 0 then L05570 /* Delete Empty & Create new */
L05510:         keyhit% = 0%
                call "ASKUSER" (keyhit%, "Data File contains records.",  ~
                     "Press PF-16 to EXIT", "-or-", "Press PF-1 to " &   ~
                     "DELETE file and continue.")
                if keyhit% = 16% then L65000
                if keyhit% <> 1% then L05510
L05570:     call "FILEBGON" (#10)
L05580:     call "OPENFILE" (#10, "OUTPT", f2%(10), " ", " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            printed% = 0%
            box17sumto$ = "UUIDDDDD****UU"     /* Control Box 17 Codes */
                        /* ABCDEFGHIJKLMN - Box 17 Code Map */
            convert 0% to thirdparty$, pic(000000000000)

            call "READ100" (#1, "MAGNETIC W-2 REC A", f1%(1))
                if f1%(1) <> 0 then L09125
            msg$ = "Magnetic Record 'A' NOT FOUND. Run PRLW2MTP"
            goto no_file

L09125:     get #1 using L09180,           /* 'MAGNETIC W-2 REC A'       */~
                pay_year$,               /* Payment Year               */~
                ein$,                    /* Transmitter E.I.N. #       */~
                forgn_a$,                /* Foreign Address flag       */~
                tran_name$,              /* Transmitter Name           */~
                address$,                /* Street Address             */~
                city$,                   /* City                       */~
                state$,                  /* State                      */~
                zip_code$,               /* Zip/Foreign Postal code    */~
                zip_extn$                /* Zip Code extension         */

L09180:     FMT XX(20),                  /* 'MAGNETIC W-2 REC A'       */~
                XX( 1),                  /* Record Indentifier - 'A'   */~
                CH( 4),                  /* Payment Year               */~
                CH( 9),                  /* Transmitter E.I.N. #       */~
                XX( 8),                  /* Filler                     */~
                CH( 1),                  /* Foreign Address flag       */~
                CH(50),                  /* Transmitter Name           */~
                CH(40),                  /* Street Address             */~
                CH(25),                  /* City                       */~
                CH( 2),                  /* State                      */~
                XX( 8),                  /* Filler                     */~
                CH( 5),                  /* Zip/Foreign Postal code    */~
                CH( 5)                   /* Zip Code extension         */

            call "READ100" (#1, "MAGNETIC W-2 REC B", f1%(1))
                if f1%(1) <> 0 then L09325
            msg$ = "Magnetic Record 'B' NOT FOUND. Run PRLW2MTP"
            goto no_file

L09325:     get #1 using L09390 ,          /* 'MAGNETIC W-2 REC B'       */~
                computer$,               /* Computer                   */~
                label$,                  /* Internal Labeling          */~
                density$,                /* Density                    */~
                character$,              /* Character Set              */~
                forgn_b$,                /* Foreign Address flag       */~
                ret_name$,               /* File Return Name           */~
                ret_address$,            /* File Return Address        */~
                ret_city$,               /* File Return City           */~
                ret_state$,              /* File Return State          */~
                ret_zip_code$,           /* Zip/Foreign Postal code    */~
                ret_zip_extn$            /* Zip/Foreign extension      */

L09390:     FMT XX(20),                  /* 'MAGNETIC W-2 REC B'       */~
                XX(1),                   /* Record Identifier - 'B'    */~
                XX(4),                   /* Payment Year               */~
                XX(9),                   /* Transmitters E.I.N. #      */~
                CH(8),                   /* Computer                   */~
                CH(2),                   /* Internal Labeling          */~
                XX(1),                   /* Filler                     */~
                CH(2),                   /* Density                    */~
                CH(3),                   /* Character Set              */~
                XX(115),                 /* Filler                     */~
                CH(1),                   /* Foreign Address flag       */~
                CH(44),                  /* File Return Name           */~
                CH(35),                  /* File Return Address        */~
                CH(20),                  /* File Return City           */~
                CH(2),                   /* File Return State          */~
                CH(5),                   /* Zip/Foreign Postal code    */~
                CH(5)                    /* Zip/Foreign extension      */

            call "READ100" (#1, "MAGNETIC W-2 REC E", f1%(1))
                if f1%(1) <> 0 then L09525
            msg$ = "Magnetic Record 'E' NOT FOUND. Run PRLW2MTP"
            goto no_file

L09525:     get #1 using L09570 ,                                          ~
                local69$,                /* State/Local 69 Number      */~
                name_code$,              /* Name Code                  */~
                employment$,             /* Type of Emploment          */~
                block$,                  /* Blocking Factor            */~
                est_number$,             /* Establishment Number       */~
                liability$,              /* Liability Limitation       */~
                otherein$                /* Other EIN                  */~

L09570:     FMT XX(20),                  /* 'MAGNETIC W-2 REC E'       */~
                XX(1),                   /* Record Identifier - 'E'    */~
                XX(4),                   /* Payment Year               */~
                XX(9),                   /* Employer E.I.N. #          */~
                CH(9),                   /* State/Local 69 Number      */~
                XX(135),                 /* Filler                     */~
                CH(1),                   /* Name Code                  */~
                CH(1),                   /* Type of Emploment          */~
                CH(2),                   /* Blocking Factor            */~
                CH(4),                   /* Establishment Number       */~
                XX(88),                  /* Unused                     */~
                CH(1),                   /* Liability Limitation       */~
                XX(1),                   /* Unused                     */~
                CH(9)                    /* Other EIN                  */

            call "READ100" (#1, "MAGNETIC W-2 REC *", f1%(1))
                if f1%(1) <> 0 then L09750
            msg$ = "Magnetic Record '*' NOT FOUND. Run PRLW2MTP"
            goto no_file

L09750:     get #1 using L09570 ,                                          ~
                box17sumto$              /* Box 17 Map                 */

            FMT XX(20),                  /* 'MAGNETIC W-2 REC *'       */~
                XX(1),                   /* Record Identifier - '*'    */~
                XX(4),                   /* Payment Year               */~
                XX(9),                   /* Employer E.I.N. #          */~
                CH(14)                   /* Box 17 Map                 */

            goto L10000

        no_file
            u3% = 0%
            call "ASKUSER" (u3%, "***** FILE ERROR *****", msg$, " ",    ~
                 "Press PF-16 to EXIT this program")
                if u3% <> 16% then no_file
                goto L65000

L10000: REM *************************************************************~
            *              M A I N   P R O C E S S I N G                *~
            *                                                           *~
            * Print the W-2s to a data file for transfer to tape.       *~
            *************************************************************

            call "SHOSTAT" ("Creating W-2 Data File for Tape")
            next% = 1%

        REM Write out Header records

            put #10 using L10270,         /* Type A Record              */~
                "A",                     /* Record Indentifier - 'A'   */~
                pay_year$,               /* Payment Year               */~
                ein$,                    /* Transmitter E.I.N. #       */~
                filler$,                 /* Blanks                     */~
                forgn_a$,                /* Foreign Address flag       */~
                tran_name$,              /* Transmitter Name           */~
                address$,                /* Street Address             */~
                city$,                   /* City                       */~
                state$,                  /* State                      */~
                filler$,                 /* Blanks                     */~
                zip_code$,               /* Zip/Foreign Postal code    */~
                zip_extn$,               /* Zip Code extension         */~
                filler$                  /* Blanks                     */

L10270:     FMT CH( 1),                  /* Record Indentifier - 'A'   */~
                CH( 4),                  /* Payment Year               */~
                CH( 9),                  /* Transmitter E.I.N. #       */~
                CH( 8),                  /* Blank                      */~
                CH( 1),                  /* Foreign Address flag       */~
                CH(50),                  /* Transmitter Name           */~
                CH(40),                  /* Street Address             */~
                CH(25),                  /* City                       */~
                CH( 2),                  /* State                      */~
                CH(13),                  /* Blanks                     */~
                CH( 5),                  /* Zip/Foreign Postal code    */~
                CH( 5),                  /* Zip Code extension         */~
                CH(112)                  /* Blanks                     */

            write #10
            gosub print_line

            put #10 using L10640,         /* Type B Record              */~
                "B",                     /* Record Identifier - 'B'    */~
                pay_year$,               /* Payment Year               */~
                ein$,                    /* Transmitters E.I.N. #      */~
                computer$,               /* Computer                   */~
                label$,                  /* Internal Labeling          */~
                filler$,                 /* Blanks                     */~
                density$,                /* Density                    */~
                character$,              /* Character Set              */~
                filler$,                 /* Blanks                     */~
                forgn_b$,                /* Foreign Address flag       */~
                ret_name$,               /* File Return Name           */~
                ret_address$,            /* File Return Address        */~
                ret_city$,               /* File Return City           */~
                ret_state$,              /* File Return State          */~
                filler$,                 /* Blanks                     */~
                ret_zip_code$,           /* Zip/Foreign Postal code    */~
                ret_zip_extn$,           /* Zip/Foreign extension      */~
                filler$                  /* Blanks                     */

L10640:     FMT CH( 1),                  /* Record Identifier - 'B'    */~
                CH( 4),                  /* Payment Year               */~
                CH( 9),                  /* Transmitters E.I.N. #      */~
                CH( 8),                  /* Computer                   */~
                CH( 2),                  /* Internal Labeling          */~
                CH( 1),                  /* Blanks                     */~
                CH( 2),                  /* Density                    */~
                CH( 3),                  /* Character Set              */~
                CH(115),                 /* Blanks                     */~
                CH( 1),                  /* Foreign Address flag       */~
                CH(44),                  /* File Return Name           */~
                CH(35),                  /* File Return Address        */~
                CH(20),                  /* File Return City           */~
                CH( 2),                  /* File Return State          */~
                CH( 5),                  /* Blanks                     */~
                CH( 5),                  /* Zip/Foreign Postal code    */~
                CH( 5),                  /* Zip/Foreign extension      */~
                CH(13)                   /* Blanks                     */

            write #10
            gosub print_line

            put #10 using L11080,         /* Type E Record              */~
                "E",                     /* Record Identifier - 'E'    */~
                pay_year$,               /* Payment Year               */~
                ein$,                    /* Employer E.I.N. #          */~
                local69$,                /* State/Local 69 Number      */~
                tran_name$,              /* Employer Name              */~
                address$,                /* Street Address             */~
                city$,                   /* City                       */~
                state$,                  /* State                      */~
                zip_extn$,               /* Zip/Foreign extension      */~
                zip_code$,               /* Zip Code                   */~
                name_code$,              /* Name Code                  */~
                employment$,             /* Type of Emploment          */~
                block$,                  /* Blocking Factor            */~
                est_number$,             /* Establishment Number       */~
                filler$,                 /* Blanks                     */~
                thirdparty$,             /* Third Party Withholding    */~
                filler$,                 /* Blanks                     */~
                liability$,              /* Liability Limitation       */~
                forgn_a$,                /* Foreign Address flag       */~
                filler$,                 /* Blanks                     */~
                otherein$,               /* Other EIN                  */~
                filler$                  /* Blanks                     */

L11080:     FMT CH(1),                   /* Record Identifier - 'E'    */~
                CH( 4),                  /* Payment Year               */~
                CH( 9),                  /* Employer E.I.N. #          */~
                CH( 9),                  /* State/Local 69 Number      */~
                CH(50),                  /* Employer Name              */~
                CH(40),                  /* Street Address             */~
                CH(25),                  /* City                       */~
                CH(10),                  /* State                      */~
                CH( 5),                  /* Zip/Foreign extension      */~
                CH( 5),                  /* Zip Code                   */~
                CH( 1),                  /* Name Code                  */~
                CH( 1),                  /* Type of Emploment          */~
                CH( 2),                  /* Blocking Factor            */~
                CH( 4),                  /* Establishment Number       */~
                CH( 1),                  /* Blanks                     */~
                CH(12),                  /* Third Party Withholding    */~
                CH(75),                  /* Blanks                     */~
                CH( 1),                  /* Liability Limitation       */~
                CH( 1),                  /* Foreign Address flag       */~
                CH( 1),                  /* Blanks                     */~
                CH( 9),                  /* Other EIN                  */~
                CH( 9)                   /* Blanks                     */

            write #10
            gosub print_line

*       **
        REM Start a sequential read through PRLWYYYY file.
*       **
            namekey$ = all(hex(00))

        main_loop
            call "PLOWALTS" (#5, namekey$, 2%, 0%, f1%(5)) /* By name */
                if f1%(5) = 0 then end_of_report

            get #5, using L12175,                                         ~
                    seq%,                /* Sequence No.               */~
                    ext_ssno$,           /* SSAN                       */~
                    ext_empname$,        /* Employee Name              */~
                    ext_empaddr$(),      /* Employee Address           */~
                    ext_ezip$,           /* Employee Zip Code          */~
                    tax,                 /* FIT Withheld               */~
                    wages,               /* FIT Wages                  */~
                    ficatax,             /* FICA Withheld              */~
                    ficawages,           /* FICA Wages                 */~
                    tips,                /* Fica Tips                  */~
                    fringes,             /* Fringe Benefits            */~
                    ext_box$(),          /* W-2 Check Off Boxes        */~
                    ext_box17$(),        /* Box 17 Identifiers         */~
                    medicaretax,         /* Medicare Tax               */~
                    medicarewages,       /* Medicare Wages             */~
                    tipsalloc,           /* Allocated Tips             */~
                    aeic,                /* A.E.I.C.                   */~
                    dcass,               /* Dep Care Assistance        */~
                    nq457,               /* Non Qualified 457          */~
                    nqn457,              /* Non Qualified non 457      */~
                    ext_box17()          /* Box 17 Amounts             */

L12175:         FMT XX(12),              /* Employee Code              */~
                    BI(1),               /* Sequence No.               */~
                    XX(26),              /* Last Name                  */~
                    CH(11),              /* SSAN                       */~
                    XX(1),               /* Sequence No.               */~
                    CH(30),              /* Employee Name              */~
                    3*CH(30),            /* Employee Address           */~
                    CH(9),               /* Employee Zip Code          */~
                    6*PD(14,4),          /* Amounts - FIT, Wages,      */~
                                         /*         - FICA, FICA Wages,*/~
                                         /*         - Tips, Fringes    */~
                    XX(16),              /* State Amounts - Wages, Tax */~
                    XX(2),               /* State Abbreviation         */~
                    XX(16),              /* Local Amounts - Wages, Tax */~
                    XX(8),               /* Locality                   */~
                    XX(12),              /* Old Box 16 Label  (filler) */~
                    XX(8),               /* Old Deferred Comp (filler) */~
                    8*CH(1),             /* W-2 Check Off Boxes        */~
                    XX(10),              /* State Id Number            */~
                    4*CH(1),             /* Box 17 Identifiers         */~
                    XX(24),              /* Box 18 Labels              */~
                    PD(14,4),            /* Medicare Tax               */~
                    PD(14,4),            /* Medicare Wages             */~
                    XX(2),               /* Filler (character)         */~
                    PD(14,4),            /* Allocated Tips             */~
                    PD(14,4),            /* A.E.I.C.                   */~
                    PD(14,4),            /* Dep Care Assistance        */~
                    PD(14,4),            /* Non Qualified 457          */~
                    PD(14,4),            /* Non Qualified non 457      */~
                    XX(16),              /* Box 18 Amounts             */~
                    4*PD(14,4),          /* Box 17 Amounts             */~
                    XX(28),              /* 2nd St, Wage, Tax, St, SID */~
                    XX(24),              /* 2nd Lc, Wage, Tax, Loc     */~
                    XX(6)                /* Filler                     */

            if seq% > 1% then main_loop

*        Set-up for additional employee wages on supp. extract records
            add_wages, wages_processed = 0  :  uniqueness_factor = .01
            add_wages$ = " "

            savekey$ = namekey$
            total_wages = wages
            gosub determine_wages

            empname$ = ext_empname$
            empzip$ = str(ext_ezip$,,5)  :  empextn$ = " "
            if str(ext_ezip$,6) <> " " then                              ~
                                       empextn$ = "-" & str(ext_ezip$,6)
            empaddress$ = ext_empaddr$(1) & ext_empaddr$(2)
            search ext_empaddr$(3) = "," to i%()
            if i%(1) <  1 then all_city
            if i%(1) > 27 then all_city
            if i%(1) =  1 then all_state
            empcity$ = str(ext_empaddr$(3),,i%(1)-1%)
            empstate$ = str(ext_empaddr$(3),i%(1)+2,2)
              goto L12435
        all_city
            empcity$ = ext_empaddr$(3) : empstate$ = " "
              goto L12435
        all_state
            empcity$ = " " : empstate$ = str(ext_empaddr$(3),i%(1)+2,2)

L12435:     ssn$ = str(ext_ssno$,,3) & str(ext_ssno$,5,2) &              ~
                                                       str(ext_ssno$,8,4)
            statutory$, pplan$, dcomp$ = " "
            if ext_box$(1) <> " " then statutory$ = "S"
            if ext_box$(3) <> " " then pplan$ = "P"
            if ext_box$(7) <> " " then dcomp$ = "D"

            wages          = round(wages         ,2)
            tax            = round(tax           ,2)
            ficatax        = round(ficatax       ,2)
            ficawages      = round(ficawages     ,2)
            tips           = round(tips          ,2)
            tipsalloc      = round(tips          ,2)
            fringes        = round(fringes       ,2)
            insurance      = 0
            unfica         = 0
            aeic           = round(aeic          ,2)
            dcomp_amt      = 0
            dcass          = round(dcass         ,2)
            nq457          = round(nq457         ,2)
            nqn457         = round(nqn457        ,2)
            medicaretax    = round(medicaretax   ,2)
            medicarewages  = round(medicarewages ,2)

            if str(ext_box17$()) = " " then L12650
L12545:     for i% = 1% to 4%
                j% = pos("ABCDEFGHIJKLMN" = ext_box17$(i%))
                if j% <  1% then L12635
                if j% > 14% then L12635
                if str(box17sumto$, j%, 1%) = "*" then L12635
                if str(box17sumto$, j%, 1%) = "U" then L12615
                if str(box17sumto$, j%, 1%) = "I" then L12620
                if str(box17sumto$, j%, 1%) = "D" then L12625
                goto L12635

L12615:     unfica     = unfica     + round(ext_box17(i%), 2):goto L12635
L12620:     insurance  = insurance  + round(ext_box17(i%), 2):goto L12635
L12625:     dcomp_amt  = dcomp_amt  + round(ext_box17(i%), 2):goto L12635

L12635:     next i%

L12650:     call "PLOWALTS" (#5, namekey$, 2%, 37%, f1%(5)) /* By name */
               if f1%(5) = 0% then L13000
            get #5 using L12680, add_wages, ext_box17$(), ext_box17()
L12680:         FMT POS(189), PD(14,4), POS(309), 4*CH(1), XX(42),       ~
                    XX(56), 4*PD(14,4)
            if add_wages = 0 then L12690    /* Check for addition wages */
                add_wages$ = "Y"           /* Additional wages in extr */
                total_wages = total_wages + add_wages
L12690:     if str(ext_box17$()) = " " then L12650 else L12545

L13000: REM Write W-2 Record                                             ~

        REM Actual Write Logic For Fed Destined W-2
            wages%         = 100 * wages
            tax%           = 100 * tax
            ficawages%     = 100 * ficawages
            ficatax%       = 100 * ficatax
            tips%          = 100 * tips
            tipsalloc%     = 100 * tipsalloc
            fringes%       = 100 * fringes
            insurance%     = 100 * insurance
            unfica%        = 100 * unfica
            aeic%          = 100 * aeic
            dcomp_amt%     = 100 * dcomp_amt
            dcass%         = 100 * dcass
            nq457%         = 100 * nq457
            nqn457%        = 100 * nqn457
            medicaretax%   = 100 * medicaretax
            medicarewages% = 100 * medicarewages

            convert wages%         to wages$         ,pic(000000000)
            convert tax%           to tax$           ,pic(000000000)
            convert ficawages%     to ficawages$     ,pic(0000000)
            convert ficatax%       to ficatax$       ,pic(000000)
            convert tips%          to tips$          ,pic(0000000)
            convert tipsalloc%     to tipsalloc$     ,pic(0000000)
            convert fringes%       to fringes$       ,pic(000000000)
            convert insurance%     to insurance$     ,pic(0000000)
            convert unfica%        to unfica$        ,pic(0000000)
            convert aeic%          to aeic$          ,pic(0000000)
            convert dcomp_amt%     to dcomp_amt$     ,pic(000000000)
            convert dcass%         to dcass$         ,pic(0000000)
            convert nq457%         to nq457$         ,pic(000000000)
            convert nqn457%        to nqn457$        ,pic(000000000)
            convert medicaretax%   to medicaretax$   ,pic(0000000)
            convert medicarewages% to medicarewages$ ,pic(000000000)

            put #10 using L14390,                                         ~
                "W",                     /* Record Identifier - 'W'    */~
                ssn$,                    /* Social Security Number     */~
                empname$,                /* Employee Name              */~
                empaddress$,             /* Street Address             */~
                empcity$,                /* City                       */~
                empstate$,               /* State                      */~
                empextn$,                /* Zip code extension         */~
                empzip$,                 /* Zip Code                   */~
                statutory$,              /* Statutory Employee Code    */~
                ficawages$,              /* Social Security Wages      */~
                filler$,                 /* Blank                      */~
                tips$,                   /* Social Security Tips       */~
                filler$,                 /* Blank                      */~
                wages$,                  /* Annual Wages & Compensatn  */~
                filler$,                 /* Blank                      */~
                ficatax$,                /* Social Security Tax        */~
                tax$,                    /* Federal Income Tax         */~
                filler$,                 /* Blank                      */~
                tipsalloc$,              /* Allocated Tips             */~
                filler$,                 /* Blank                      */~
                fringes$,                /* Fringe Benifits            */~
                medicarewages$,          /* Medicare Wages             */~
                medicaretax$,            /* Medicare Tax               */~
                filler$,                 /* Filler                     */~
                nq457$,                  /* Non Qualified 457          */~
                filler$,                 /* Blank                      */~
                nqn457$,                 /* Non Qualified non 457      */~
                filler$,                 /* Blank                      */~
                dcass$,                  /* Dep Care Assistance        */~
                control$,                /* Control Number             */~
                insurance$,              /* Employer Cost of Insur.    */~
                unfica$,                 /* Uncollected SSA Tax Tips   */~
                aeic$,                   /* A.E.I.C.                   */~
                filler$,                 /* Blank                      */~
                pplan$,                  /* Pension Plan Indicator     */~
                filler$,                 /* Blank                      */~
                dcomp$,                  /* Deferred Comp Indicator    */~
                filler$,                 /* Blank                      */~
                dcomp_amt$               /* Deferred Comp Amount       */

L14390:     FMT CH( 1),                  /* Record Identifier - 'W'    */~
                CH( 9),                  /* Social Security Number     */~
                CH(27),                  /* Employee Name              */~
                CH(40),                  /* Street Address             */~
                CH(25),                  /* City                       */~
                CH(10),                  /* State                      */~
                CH( 5),                  /* Zip code extension         */~
                CH( 5),                  /* Zip Code                   */~
                CH( 1),                  /* Statutory Employee Code    */~
                CH( 7),                  /* Social Security Wages      */~
                CH( 1),                  /* Blank                      */~
                CH( 7),                  /* Social Security Tips       */~
                CH( 1),                  /* Blank                      */~
                CH( 9),                  /* Annual Wages & Compensatn  */~
                CH( 1),                  /* Blank                      */~
                CH( 6),                  /* Social Security Tax        */~
                CH( 9),                  /* Federal Income Tax         */~
                CH( 1),                  /* Blank                      */~
                CH( 7),                  /* Allocated Tips             */~
                CH( 1),                  /* Blank                      */~
                CH( 9),                  /* Fringe Benifits            */~
                CH( 9),                  /* Medicare Wages             */~
                CH( 7),                  /* Medicare Tax               */~
                CH( 8),                  /* Filler                     */~
                CH( 9),                  /* Non Qualified 457          */~
                CH( 1),                  /* Blank                      */~
                CH( 9),                  /* Non Qualified non 457      */~
                CH( 1),                  /* Blank                      */~
                CH( 7),                  /* Dep Care Assistance        */~
                CH( 7),                  /* Control Number             */~
                CH( 7),                  /* Employer Cost of Insur.    */~
                CH( 7),                  /* Uncollected SSA Tax Tips   */~
                CH( 7),                  /* A.E.I.C.                   */~
                CH( 1),                  /* Blank                      */~
                CH( 1),                  /* Pension Plan Indicator     */~
                CH( 1),                  /* Blank                      */~
                CH( 1),                  /* Deffered Comp Indicator    */~
                CH( 1),                  /* Blank                      */~
                CH( 9)                   /* Deffered Comp Amount       */

            write #10
            gosub print_line

            next%       = next%  + 1% : tcount%     = tcount% + 1%

            swages          = swages          + wages%
            stax            = stax            + tax%
            sficawages      = sficawages      + ficawages%
            sficatax        = sficatax        + ficatax%
            stips           = stips           + tips%
            stipsalloc      = stipsalloc      + tipsalloc%
            sfringes        = sfringes        + fringes%
            sinsurance      = sinsurance      + insurance%
            sunfica         = sunfica         + unfica%
            saeic           = saeic           + aeic%
            sdcomp_amt      = sdcomp_amt      + dcomp_amt%
            sdcass          = sdcass          + dcass%
            snq457          = snq457          + nq457%
            snqn457         = snqn457         + nqn457%
            smedicaretax    = smedicaretax    + medicaretax%
            smedicarewages  = smedicarewages  + medicarewages%

            if next%  = 42% then gosub subtotal

            if total_wages <> wages_processed then                       ~
                gosub supplemental_record

            str(namekey$,38) = all(hex(ff))

            goto main_loop

        REM **************************************************************
        REM      P R I N T   S U B   T O T A L S
        REM **************************************************************

        subtotal
            convert stax           to stax$           ,pic(0000000000)
            convert swages         to swages$         ,pic(0000000000)
            convert sficatax       to sficatax$       ,pic(0000000000)
            convert sficawages     to sficawages$     ,pic(0000000000)
            convert stips          to stips$          ,pic(0000000000)
            convert sdcass         to sdcass$         ,pic(0000000000)
            convert sinsurance     to sinsurance$     ,pic(0000000000)
            convert sunfica        to sunfica$        ,pic(0000000000)
            convert saeic          to saeic$          ,pic(00000000000)
            convert stipsalloc     to stipsalloc$     ,pic(0000000000)
            convert sfringes       to sfringes$       ,pic(0000000000)
            convert sdcomp_amt     to sdcomp_amt$     ,pic(0000000000)
            convert snq457         to snq457$         ,pic(0000000000)
            convert snqn457        to snqn457$        ,pic(0000000000)
            convert smedicaretax   to smedicaretax$   ,pic(0000000000)
            convert smedicarewages to smedicarewages$ ,pic(00000000000)

            put #10 using L16780,                                         ~
                "I",                     /* Record Identifier - 'I'    */~
                sficawages$,             /* Social Security Wages      */~
                filler$,                 /* Blank                      */~
                stips$,                  /* Social Security Tips       */~
                filler$,                 /* Blank                      */~
                swages$,                 /* Annual Wages & Comp        */~
                filler$,                 /* Blank                      */~
                sficatax$,               /* FICA Taxes                 */~
                filler$,                 /* Blank                      */~
                stax$,                   /* Federal Income Tax         */~
                scontrol$,               /* Control Number             */~
                sinsurance$,             /* Employer Cost of Insur     */~
                sunfica$,                /* Uncollected FICA on Tips   */~
                saeic$,                  /* A.E.I.C.                   */~
                stipsalloc$,             /* Allocated Tips             */~
                sfringes$,               /* Fringe Benifits            */~
                filler$,                 /* Blank                      */~
                sdcomp_amt$,             /* Deffered Comp Amount       */~
                filler$,                 /* Blank                      */~
                sdcass$,                 /* Dep Care Assistance        */~
                filler$,                 /* Blank                      */~
                snq457$,                 /* Non Qualified 457          */~
                filler$,                 /* Blank                      */~
                snqn457$,                /* Non Qualified non 457      */~
                filler$,                 /* Blank                      */~
                smedicarewages$,         /* Medicare Wages             */~
                filler$,                 /* Blank                      */~
                smedicaretax$,           /* Medicare Tax               */~
                filler$                  /* Blank                      */

L16780:     FMT CH( 1),                  /* Record Identifier - 'I'    */~
                CH(10),                  /* Social Security Wages      */~
                CH( 1),                  /* Blank                      */~
                CH(10),                  /* Social Security Tips       */~
                CH( 1),                  /* Blank                      */~
                CH(10),                  /* Annual Wages & Comp        */~
                CH( 1),                  /* Blank                      */~
                CH(10),                  /* FICA Taxes                 */~
                CH( 1),                  /* Blank                      */~
                CH(10),                  /* Federal Income Tax         */~
                CH( 7),                  /* Control Number             */~
                CH(10),                  /* Employer Cost of Insur     */~
                CH(10),                  /* Uncollected FICA on Tips   */~
                CH(11),                  /* A.E.I.C.                   */~
                CH(10),                  /* Allocated Tips             */~
                CH(10),                  /* Fringe Benifits            */~
                CH( 1),                  /* Blank                      */~
                CH(10),                  /* Deffered Comp Amount       */~
                CH( 1),                  /* Blank                      */~
                CH(10),                  /* Dep Care Assistance        */~
                CH( 1),                  /* Blank                      */~
                CH(10),                  /* Non Qualified 457          */~
                CH( 1),                  /* Blank                      */~
                CH(10),                  /* Non Qualified non 457      */~
                CH( 1),                  /* Blank                      */~
                CH(11),                  /* Medicare Wages             */~
                CH( 1),                  /* Blank                      */~
                CH(10),                  /* Medicare Tax               */~
                CH(95)                   /* Blank                      */

            write #10
            gosub print_line

            next%  = 1%

            twages      = twages      + swages       : swages     = 0
            ttax        = ttax        + stax         : stax       = 0
            tficawages  = tficawages  + sficawages   : sficawages = 0
            tficatax    = tficatax    + sficatax     : sficatax   = 0
            ttips       = ttips       + stips        : stips      = 0
            ttipsalloc  = ttipsalloc  + stipsalloc   : stipsalloc = 0
            tfringes    = tfringes    + sfringes     : sfringes   = 0
            tinsurance  = tinsurance  + sinsurance   : sinsurance = 0
            tunfica     = tunfica     + sunfica      : sunfica    = 0
            taeic       = taeic       + saeic        : saeic      = 0
            tdcomp_amt  = tdcomp_amt  + sdcomp_amt   : sdcomp_amt= 0
            tdcass      = tdcass      + sdcass       : sdcass     = 0
            tnq457      = tnq457      + snq457       : snq457     = 0
            tnqn457     = tnqn457     + snqn457      : snqn457    = 0

            tmedicaretax    = tmedicaretax   + smedicaretax
            tmedicarewages  = tmedicarewages + smedicarewages
            smedicaretax    = 0  :  smedicarewages = 0

            return

        REM **************************************************************
        REM      P R I N T   T O T A L S
        REM **************************************************************

        end_of_report
            if next% > 1% then gosub subtotal

        REM Final Totals
            convert tcount%         to tcount$         ,pic(0000000)
            convert ttax            to ttax$           ,pic(000000000000)
            convert twages          to twages$         ,pic(0000000000000)
            convert tficatax        to tficatax$       ,pic(000000000000)
            convert tficawages      to tficawages$     ,pic(0000000000000)
            convert ttips           to ttips$          ,pic(000000000000)
            convert tinsurance      to tinsurance$     ,pic(000000000000)
            convert tunfica         to tunfica$        ,pic(000000000000)
            convert taeic           to taeic$          ,pic(000000000000)
            convert ttipsalloc      to ttipsalloc$     ,pic(000000000000)
            convert tfringes        to tfringes$       ,pic(000000000000)
            convert tdcomp_amt      to tdcomp_amt$     ,pic(0000000000000)
            convert tdcass          to tdcass$         ,pic(000000000000)
            convert tnq457          to tnq457$         ,pic(0000000000000)
            convert tnqn457         to tnqn457$        ,pic(0000000000000)
            convert tmedicaretax    to tmedicaretax$   ,pic(000000000000)
            convert tmedicarewages  to tmedicarewages$ ,pic(0000000000000)

            put #10 using L18500,                                         ~
                "T",                     /* Record Identifier - 'T'    */~
                tcount$,                 /* Number of Employees        */~
                tficawages$,             /* Social Security Wages      */~
                filler$,                 /* Blank                      */~
                ttips$,                  /* Annual Tips                */~
                twages$,                 /* Annual Wages               */~
                filler$,                 /* Blank                      */~
                tficatax$,               /* FICA Withheld              */~
                filler$,                 /* Blank                      */~
                ttax$,                   /* Federal Withholdings       */~
                tinsurance$,             /* Employer Cost of Insur     */~
                tunfica$,                /* Uncollected FICA on Tips   */~
                taeic$,                  /* A.E.I.C.                   */~
                ttipsalloc$,             /* Allocated Tips             */~
                tfringes$,               /* Fringe Benifits            */~
                filler$,                 /* Blank                      */~
                tdcomp_amt$,             /* Deferred Amount            */~
                filler$,                 /* Blank                      */~
                tdcass$,                 /* Dep Care Assistance Total  */~
                filler$,                 /* Blank                      */~
                tnq457$,                 /* Non Qualified 457          */~
                filler$,                 /* Blank                      */~
                tnqn457$,                /* Non Qualified non 457      */~
                filler$,                 /* Blank                      */~
                tmedicarewages$,         /* Medicare Wages             */~
                filler$,                 /* Blank                      */~
                tmedicaretax$,           /* Medicare Tax               */~
                filler$                  /* Blanks                     */

L18500:     FMT CH( 1),                  /* Record Identifier - 'T'    */~
                CH( 7),                  /* Number of Employees        */~
                CH(13),                  /* Social Security Wages      */~
                CH( 1),                  /* Blank                      */~
                CH(12),                  /* Annual Tips                */~
                CH(13),                  /* Annual Wages               */~
                CH( 1),                  /* Blank                      */~
                CH(12),                  /* FICA Withheld              */~
                CH( 1),                  /* Blank                      */~
                CH(12),                  /* Federal Withholdings       */~
                CH(12),                  /* Employer Cost of Insur     */~
                CH(12),                  /* Uncollected FICA on Tips   */~
                CH(12),                  /* A.E.I.C.                   */~
                CH(12),                  /* Allocated Tips             */~
                CH(12),                  /* Fringe Benifits            */~
                CH( 1),                  /* Blank                      */~
                CH(13),                  /* Deferred Amount            */~
                CH( 1),                  /* Blank                      */~
                CH(12),                  /* Dep Care Assistance Total  */~
                CH( 1),                  /* Blank                      */~
                CH(13),                  /* Non Qualified 457          */~
                CH( 1),                  /* Blank                      */~
                CH(13),                  /* Non Qualified non 457      */~
                CH( 1),                  /* Blank                      */~
                CH(13),                  /* Medicare Wages             */~
                CH( 1),                  /* Blank                      */~
                CH(12),                  /* Medicare Tax               */~
                CH(60)                   /* Blanks                     */

            write #10
            gosub print_line

            fwages      = fwages      + twages       : twages     = 0
            ftax        = ftax        + ttax         : ttax       = 0
            fficawages  = fficawages  + tficawages   : tficawages = 0
            fficatax    = fficatax    + tficatax     : tficatax   = 0
            ftips       = ftips       + ttips        : ttips      = 0
            ftipsalloc  = ftipsalloc  + ttipsalloc   : ttipsalloc = 0
            ffringes    = ffringes    + tfringes     : tfringes   = 0
            finsurance  = finsurance  + tinsurance   : tinsurance = 0
            funfica     = funfica     + tunfica      : tunfica    = 0
            faeic       = faeic       + taeic        : taeic      = 0
            fdcomp_amt  = fdcomp_amt  + tdcomp_amt   : tdcomp_amt = 0
            fdcass      = fdcass      + tdcass       : tdcass     = 0
            fnq457      = fnq457      + tnq457       : tnq457     = 0
            fnqn457     = fnqn457     + tnqn457      : tnqn457    = 0

            fmedicaretax    = fmedicaretax    + tmedicaretax
            fmedicarewages  = fmedicarewages  + tmedicarewages
            tmedicaretax    = 0  :  tmedicarewages  = 0

            fcount%     = fcount%     + tcount%      : tcount%    = 0
        /* Note - Here we could loop back, create another type 'E' rec. */
        /* record and process another extract file. Someday, someone.?. */
        /* But be careful. Current 'B' and 'E' records inherit some info*/
        /* information from the type 'A' record, which may not be       */
        /* appropriate for that type of processing.                     */

        REM **************************************************************
        REM      P R I N T   T O T A L S   T Y P E   F
        REM **************************************************************

            convert fcount%    to fcount$     ,pic(0000000)
            convert fficawages to fficawages$ ,pic(0000000000000000)
            convert ftips      to ftips$      ,pic(0000000000000000)
            convert fwages     to fwages$     ,pic(0000000000000000)
            convert fficatax   to fficatax$   ,pic(0000000000000000)
            convert ftax       to ftax$       ,pic(0000000000000000)
            convert faeic      to faeic$      ,pic(0000000000000000)

            put #10 using L20290,                                         ~
                "F",                     /* Record Identifier - 'F'    */~
                fcount$,                 /* Total Employees            */~
                filler$,                 /* Filler                     */~
                fficawages$,             /* SSA wages                  */~
                filler$,                 /* Filler                     */~
                ftips$,                  /* SSA tips                   */~
                filler$,                 /* Filler                     */~
                fwages$,                 /* Total wages, tips, etc.    */~
                filler$,                 /* Filler                     */~
                fficatax$,               /* SSA tax withheld           */~
                filler$,                 /* Filler                     */~
                ftax$,                   /* Fed income tax withheld    */~
                filler$,                 /* Filler                     */~
                faeic$,                  /* AEIC total                 */~
                filler$                  /* Filler                     */

L20290:     FMT CH( 1),                  /* Record Identifier - 'F'    */~
                CH( 7),                  /* Total Employees            */~
                CH(1),                   /* Filler                     */~
                CH(16),                  /* SSA wages                  */~
                CH(1),                   /* Filler                     */~
                CH(16),                  /* SSA tips                   */~
                CH(1),                   /* Filler                     */~
                CH(16),                  /* Total wages, tips, etc.    */~
                CH(1),                   /* Filler                     */~
                CH(16),                  /* SSA tax withheld           */~
                CH(1),                   /* Filler                     */~
                CH(16),                  /* Fed income tax withheld    */~
                CH(1),                   /* Filler                     */~
                CH(16),                  /* AEIC total                 */~
                CH(165)                  /* Filler                     */

            write #10
            gosub print_line
            printed% = -1%
            gosub page_footer
            goto L65000

        supplemental_record
L21010:     if total_wages = wages_processed then return
            if add_wages$ = "Y" then namekey$ = savekey$
            if add_wages$ <> "Y" then L21180

*       Process any earnings found on the additional extract records.
L21050:     call "PLOWALTS" (#5, namekey$, 2%, 37%, f1%(5)) /* By name */
               if f1%(5) <> 0% then L21070
                     add_wages$ = " "
                     goto L21010
L21070:     get #5 using L21080, wages
L21080:         FMT POS(189), PD(14,4)
            if wages = 0 then L21050
                gosub determine_wages
                gosub process_extra_w_record
                goto L21050

*       Still possible that there may be earnings leftover, so
*       process them.

L21180:     wages = total_wages - wages_processed
            gosub determine_wages
            gosub process_extra_w_record
            goto L21010

        determine_wages
            wages = min(wages, 1e7 - uniqueness_factor)
            uniqueness_factor = uniqueness_factor + .01
            wages_processed = wages_processed + wages
            return

        process_extra_w_record
            wages          = round(wages         ,2)
            wages%         = 100 * wages

            convert wages%         to wages$         ,pic(000000000)
            convert 0%             to tax$           ,pic(000000000)
            convert 0%             to ficawages$     ,pic(0000000)
            convert 0%             to ficatax$       ,pic(000000)
            convert 0%             to tips$          ,pic(0000000)
            convert 0%             to tipsalloc$     ,pic(0000000)
            convert 0%             to fringes$       ,pic(000000000)
            convert 0%             to insurance$     ,pic(0000000)
            convert 0%             to unfica$        ,pic(0000000)
            convert 0%             to aeic$          ,pic(0000000)
            convert 0%             to dcomp_amt$     ,pic(000000000)
            convert 0%             to dcass$         ,pic(0000000)
            convert 0%             to nq457$         ,pic(000000000)
            convert 0%             to nqn457$        ,pic(000000000)
            convert 0%             to medicaretax$   ,pic(0000000)
            convert 0%             to medicarewages$ ,pic(000000000)

            put #10 using L14390,                                         ~
                "W",                     /* Record Identifier - 'W'    */~
                ssn$,                    /* Social Security Number     */~
                empname$,                /* Employee Name              */~
                empaddress$,             /* Street Address             */~
                empcity$,                /* City                       */~
                empstate$,               /* State                      */~
                empextn$,                /* Zip code extension         */~
                empzip$,                 /* Zip Code                   */~
                statutory$,              /* Statutory Employee Code    */~
                ficawages$,              /* Social Security Wages      */~
                filler$,                 /* Blank                      */~
                tips$,                   /* Social Security Tips       */~
                filler$,                 /* Blank                      */~
                wages$,                  /* Annual Wages & Compensatn  */~
                filler$,                 /* Blank                      */~
                ficatax$,                /* Social Security Tax        */~
                tax$,                    /* Federal Income Tax         */~
                filler$,                 /* Blank                      */~
                tipsalloc$,              /* Allocated Tips             */~
                filler$,                 /* Blank                      */~
                fringes$,                /* Fringe Benifits            */~
                medicarewages$,          /* Medicare Wages             */~
                medicaretax$,            /* Medicare Tax               */~
                filler$,                 /* Filler                     */~
                nq457$,                  /* Non Qualified 457          */~
                filler$,                 /* Blank                      */~
                nqn457$,                 /* Non Qualified non 457      */~
                filler$,                 /* Blank                      */~
                dcass$,                  /* Dep Care Assistance        */~
                control$,                /* Control Number             */~
                insurance$,              /* Employer Cost of Insur.    */~
                unfica$,                 /* Uncollected SSA Tax Tips   */~
                aeic$,                   /* A.E.I.C.                   */~
                filler$,                 /* Blank                      */~
                pplan$,                  /* Pension Plan Indicator     */~
                filler$,                 /* Blank                      */~
                dcomp$,                  /* Deferred Comp Indicator    */~
                filler$,                 /* Blank                      */~
                dcomp_amt$               /* Deferred Comp Amount       */

            write #10  :  gosub print_line

            next%       = next%  + 1% : tcount%     = tcount% + 1%

            swages          = swages          + wages%

            if next%  = 42% then gosub subtotal

            return

        REM *************************************************************~
            *   P R I N T   L I N E   O N   P R I N T E R               *~
            *************************************************************

        print_line
            if printed% <> 0% then L35160
               call "TIME" (time$)
               date$ = date
               call "DATEFMT" (date$)
               call "GETNAMES" addr(#5, file1$, lib1$, vol1$)
               call "GETNAMES" addr(#10, file2$, lib2$, vol2$)
               call "EXTRACT" addr("ID", userid$)
               select printer(134)
               print page
               printed% = 1%
               gosub page_header
L35160:     if line% > 55% then gosub page_footer
            record% = record% + 1%
            get #10 using L35190, str(line$(),1,275)
L35190:         FMT CH(275)

            put str(line1$) using L37180, line$(1), line$(2), line$(3),   ~
                     line$(4), line$(5), line$(6), line$(7), line$(8)
            for i% = 1% to 8% : str(line1$,11%*i%,1) = "." :next i%
            print using L37090, record%, line1$

            put str(line1$) using L37180, line$(9), line$(10), line$(11), ~
                  line$(12), line$(13), line$(14), line$(15), line$(16)
            for i% = 1% to 8% : str(line1$,11%*i%,1) = "." :next i%
            print using L37110, line1$

            put str(line1$) using L37180, line$(17), line$(18), line$(19),~
                   line$(20), line$(21), line$(22), line$(23), line$(24)
            for i% = 1% to 8% : str(line1$,11%*i%,1) = "." :next i%
            print using L37130, line1$

            put str(line1$) using L37180, line$(25), line$(26), line$(27),~
                   line$(28), line$(29), line$(30), line$(31), line$(32)
            for i% = 1% to 4% : str(line1$,11%*i%,1) = "." :next i%
            str(line1$,88,1) = " "
            print using L37150, line1$

            print

            line% = line% + 5%
            return

        REM PRINTED OUTPUT HEADINGS
        page_footer
            print using L37040
            print using L37060
            if printed% = -1% then return
        page_header
            print page
            print using L37000, date$, time$
            print using L37020, file2$, lib2$, vol2$, file1$, lib1$,      ~
                               vol1$, userid$
            print
            print using L37060
            print using L37040
            print
            line% = 7%
            return

L37000: %DATE: ######## TIME: ########     PAYROLL W-2 MAGNETIC TAPE FILE~
        ~ GENERATION

L37020: %CONTENTS OF FILE ######## IN ######## ON ######, GENERATED FROM ~
        ~FILE ######## IN ######## ON ###### BY ###

L37040: %                         POSITION       1234567890.1234567890.12~
        ~34567890.1234567890.1234567890.1234567890.1234567890.1234567890.
L37060: %                                        0        1 1        2 2 ~
        ~       3 3        4 4        5 5        6 6        7 7        8

L37090: %RECORD NO. #######  POSITION PLUS   0   ########################~
        ~################################################################
L37110: %                                   80   ########################~
        ~################################################################
L37130: %                                  160   ########################~
        ~################################################################
L37150: %                                  240   ########################~
        ~################################################################

L37180: %########## ########## ########## ########## ########## #########~
        ~# ########## ##########

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

            call "SHOSTAT" ("Closing Files, One Moment Please")
            end
