        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   RRRR   L      W   W   222   EEEEE  X   X  TTTTT   *~
            *  P   P  R   R  L      W   W      2  E       X X     T     *~
            *  PPPP   RRRR   L      W   W   222   EEEE     X      T     *~
            *  P      R   R  L      W W W  2      E       X X     T     *~
            *  P      R   R  LLLLL   W W   22222  EEEEE  X   X    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLW2EXT - Extracts W-2 data from the payroll system for  *~
            *            maintenance and use in generating W-2s,        *~
            *            either printed of on Mag Tape.                 *~
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
            * 11/20/89 ! ORIGINAL (somewhat cloned from W2PRT)    ! MJB *~
            * 01/12/90 ! Added State ID # and changed PRLWYYYY    ! MJB *~
            *          !  record length to 330 Ch.                !     *~
            * 12/10/90 ! Record length to 500 Ch. (1990)          ! KAB *~
            * 12/03/91 ! PRR 12142 - Added Medicare tax and wage  ! JBK *~
            *          !  base for 1991 tax changes.              !     *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 11/23/92 ! Largely re-wrote for 1992 W2's and new   ! JBK *~
            *          !  Parameter Mapping File (PRLW2MAP).      !     *~
            *          !  PRR 11767 - Added Deduction Extract.    !     *~
            * 11/24/93 ! Modified program for 1993 W2's and new   ! JBK *~
            *          !  box numbering requirements.  Changed    !     *~
            *          !  variable names to reflect box # changes.!     *~
            * 12/14/93 ! Minor Tweek to keep UNIX happy.          ! JBK *~
            * 08/27/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            box$(8)1,                    /* Boxes in Box 15            */~
            box13$(100)1,                /* Employee Box 13 Codes      */~
            box13(100),                  /* Employee Box 13 Values     */~
            box13codes$(4)1,             /* Box 13 Codes for Output    */~
            box14$(100)12,               /* Employee Box 14 Labels     */~
            box14(100),                  /* Employee Box 14 Values     */~
            box14lbl$(2)12,              /* Box 14 Labels for Output   */~
            city$(100)12,                /* CITY FOR LINE ITEMS        */~
            cmthd$(100)6,                /* LINE CITY METHOD OF DEDUCTI*/~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dedcode$(100)6,              /* Mapping Deduction Codes    */~
            dedw2box$(100)2,             /* Mapping Ded Codes W2 Boxes */~
            dedw2box%(100),              /* Mapping Ded Codes W2 Boxes */~
            dedw2boxcode$(100)1,         /* Mapping Ded's Box Codes    */~
            dedboxlabel$(100)12,         /* Mapping Ded's Box Labels   */~
            empaddr$(3)30,               /* EMPLOYEE ADDRESS           */~
            empcity$20,                  /* CITY (FOR EMPLOYEE ADDRESS)*/~
            empcode$12,                  /* EMPLOYEE NUMBER            */~
            empname$30,                  /* EMPLOYEES NAME FOR PRINT   */~
            employer$30,                 /* Employer Name              */~
            empstate$2,                  /* STATE (FOR EMPLOYEE ADDRES)*/~
            empzip$9,                    /* ZIP (FOR EMPLOYEE ADDRESS) */~
            e_paid$1,                    /* Employee Paid Deduct. (YN) */~
            e_state$(100)2,              /* Employee State Codes Array */~
            e_state(100,2),              /* Employee State Tax & Earns */~
            e_stateid$(100)10,           /* Employee State ID's Array  */~
            e_smthd$(101)6,              /* Employee State Method Array*/~
            e_cmthd$(100)6,              /* Employee City Method Array */~
            e_city$(100)8,               /* Employee City Names Array  */~
            e_city(100,2),               /* Employee City Tax & Earns  */~
            first$10,                    /* EMP FIRST NAME             */~
            fed(20),                     /* Federal Amounts for Output */~
            fedid$10,                    /* Employer Federal ID NO.    */~
            fedmthd$6,                   /* FEDERAL INCOME TAX DED CODE*/~
            fedtaxbox$2,                 /* W2 Box for Federal Tax     */~
            fedwagebox$2,                /* W2 Box for Federal Wages   */~
            ficamthd$6,                  /* F.I.C.A. TAX DED CODE      */~
            ficataxbox$2,                /* W2 Box for Fica Tax        */~
            ficawagebox$2,               /* W2 Box for Fica Wages      */~
            last$15,                     /* EMP LAST NAME              */~
            lbl11$12,                    /* Box 11 Label               */~
            lc$(2)8,                     /* Local Codes for Output     */~
            ltax(2),                     /* Local Taxes for Output     */~
            lwages(2),                   /* Local Wages for Output     */~
            medicaremthd$6,              /* Medicare Tax Ded Code      */~
            medicaretaxbox$2,            /* W2 Box for Medicare Tax    */~
            medicarewagebox$2,           /* W2 Box for Medicare Wages  */~
            method$6,                    /* EMP Method of deduction    */~
            middle$1,                    /* EMP MIDDLE INITIAL         */~
            msg_text$(6)70,              /* Text for GETPARM Screen    */~
            pfkey$1,                     /* PF Key                     */~
            pfmask$4,                    /* PF Key Mask                */~
            prname$8,                    /* W2 Extract File Name       */~
            readkey$100,                 /* WORK VARIABLE              */~
            rs$2,                        /* Record Size, Just because  */~
            st$(2)2,                     /* State Codes for Output     */~
            search%(1),                  /* FOR 'SEARCH' STATEMENT     */~
            sid$(2)10,                   /* EMPLOYERS STATE ID NUMBER  */~
            ssn$11,                      /* EMP SOCIAL SECURITY NUMBER */~
            state$(100)2,                /* STATES WITH INCOME TAXES   */~
            stateid$(100)10,             /* EMPLOYERS STATE ID NUMBERS */~
            smthd$(100)6,                /* LINE STATE METHOD OF DEDUCT*/~
            stax(2),                     /* State Tax for Output       */~
            swages(2),                   /* State Wages for Output     */~
            temp$1,                      /* Temporary Variable         */~
            write_flag$1,                /* Write Supplemental W2 Flag */~
            w2year$4                     /* Year select                */

        dim f2%(16),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(16),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(16)20,                 /* TEXT FROM FILE OPENING     */~
            fs%(16)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            * #2  ! PRLW2STA ! W-2 Cross Reference File                 *~
            * #4  ! EMPDEDXN ! Employee deduction file                  *~
            * #5  ! PERMASTR ! Personnel master file-ties to EMPMASTR i *~
            * #6  ! PRLW2MAP ! Payroll W2 Mapping File                  *~
            * #10 ! PRLWYYYY ! Payroll W2 Data Extract File             *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20

            select #2,  "PRLW2STA",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 50,                                    ~
                        keypos = 1,    keylen = 12

            select #4,  "EMPDEDXN",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =  15,                     ~
                        alt key  1, keypos =   16, keylen =  18, dup

            select #5, "PERMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  950,                                  ~
                        keypos =  39,  keylen = 12,                      ~
                        alt key  1, keypos =   28, keylen =  23,         ~
                            key  2, keypos =    2, keylen =  49,         ~
                            key  3, keypos =    1, keylen =  50

            select #6, "PRLW2MAP",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1300,                                  ~
                        keypos =   1,  keylen = 11

            select #10, "PRLWYYYY",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos = 1, keylen = 13,                         ~
                        alt key  1, keypos = 40, keylen = 12,            ~
                            key  2, keypos = 14, keylen = 38


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1),    0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2),    0%, rslt$( 2))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4),    0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5),    0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%(6%), f2%(6%),    0%, rslt$(6%))

*       ** Make Sure we got the right record size, in case of old file
            call "GETUFBRS" addr(#10, rs$)

            pfmask$ = hex(00010000)  :  pfkey$ = "@"
            msg_text$(1) = hex(84) & "Enter the Reporting" &             ~
                           " Year for the W2 Extract File"
            msg_text$(2) = " "
            msg_text$(3) = hex(8420202020) & "WARNING - If the File "  & ~
                           "Already Exists you will be asked if you "
            msg_text$(4) = hex(842020202020) & "Want to DELETE the C"  & ~
                           "urrent File and Create a New One."
            msg_text$(5) = " "
            msg_text$(6) = "Press 'RETURN' to Continue, or PF-16 to EXIT"
            str(msg_text$(1),69) = hex(0d)
            str(msg_text$(2),69) = hex(0d)
            str(msg_text$(3),69) = hex(0d)
            str(msg_text$(4),69) = hex(0d)
            str(msg_text$(5),69) = hex(0d)

            call "DATEFMT" ( date, 0%, str( w2year$, 1%, 4% ))

L05080:     call "GETPARM" addr("I ", "R", "  Year  ", pfkey$,           ~
                           "W201", "W2EXT ", msg_text$(), 350%,          ~
                            "K", "W2YEAR  ", w2year$, 4%,                ~
                            "A", 14%, "A", 45%, "A",                     ~
                            "T", "W2 Reporting Year ", 17%,              ~
                            "A", 14%, "A", 20%, "P", pfmask$, "E")

            if pfkey$ = "P" then L65000
            if pfkey$ <> "@" then L05080

            prname$ = "PRLW" & str(w2year$)
            call "PUTPRNAM" addr(#10, prname$)

            call "OPENCHCK" (#10, fs%(10), f2%(10),    0%, rslt$(10))
            if fs%(10) < 0% then L05350
L05230:     ask% = 2%
            call "ASKUSER" (ask%, "***** EXISTING FILE *****",           ~
                 "The W2 Extract File for " & w2year$ &                  ~
                 " Already Exists!",                                     ~
                 "If You Continue, it will be SCRATCHED and RE-CREATED", ~
                 "Press PF-16 to EXIT this program or PF-27 "   &        ~
                 "to Continue and Re-Create the File")

            if ask% = 16% then L65000
            if ask% <> 27% then L05230
            call "FILEBGON" (#10)

L05350
*       ** Make Sure we use the right record size, in case of old file
            call "PUTUFBRS" addr(#10, rs$)
            call "OPENCHCK" (#10, fs%(10), f2%(10),  500%, rslt$(10))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            call "READNEXT" (#6, f1%(6%))
                if f1%(6%) = 0% then L09350

            get #6, using L09130, temp$
L09130:         FMT POS(11), CH(1)
            if temp$ <> "1" then L09350
            get #6, using L09210,  fedid$, employer$, fedmthd$, fedtaxbox$,~
                                 fedwagebox$, ficamthd$, ficataxbox$,    ~
                                 ficawagebox$, medicaremthd$,            ~
                                 medicaretaxbox$, medicarewagebox$,      ~
                                 dedcode$(), dedw2box$(),                ~
                                 dedw2boxcode$()
L09210:         FMT CH(10), POS(12), CH(30), POS(142), CH(6), 2*CH(2),   ~
                    CH(6), 2*CH(2), CH(6), 2*CH(2), 100*CH(6),           ~
                    100*CH(2), 100*CH(1)

            call "READNEXT" (#6, f1%(6%))
                if f1%(6%) = 0% then L09350

            get #6, using L09290, temp$
L09290:         FMT POS(11), CH(1)
            if temp$ <> "2" then L09350
            get #6, using L09320,  dedboxlabel$()
L09320:         FMT POS(12), 100*CH(12)
            goto L09580

L09350
*        General purpose ASKUSER for mapping parameter errors
            ask% = 2%
            call "ASKUSER" (ask%, "*** NO W-2 PARAMETERS ***",           ~
                 "You have NOT Defined the Parameters for the W-2",      ~
                 "You must run PRLW2INP before you may Continue",        ~
                 "Press PF-16 to EXIT This program")
            goto L65000

L09580
*        Set-up mapping boxes
            convert fedtaxbox$       to fedtaxbox%
            convert fedwagebox$      to fedwagebox%
            convert ficataxbox$      to ficataxbox%
            convert ficawagebox$     to ficawagebox%
            convert medicaretaxbox$  to medicaretaxbox%
            convert medicarewagebox$ to medicarewagebox%

            mat dedw2box% = zer
            for i% = 1% to 100%
            if dedw2box$(i%) = " " then L09700
                convert dedw2box$(i%) to dedw2box%(i%)
L09700:     next i%

*        Load up state and city deductions
            l% = 0%
            readkey$ = " "
L09750:     call "PLOWNEXT" (#2, readkey$, 0%, f1%(2%))
                if f1%(2%) = 0% then L09820
            l% = l% + 1%
            get #2, using L09800, smthd$(l%), cmthd$(l%), state$(l%),      ~
                                stateid$(l%), city$(l%)
L09800:         FMT CH(6), CH(6), CH(2), CH(10), CH(12)
            goto L09750
L09820:
            lbl11$ = " "  :  defcomp = 0

        REM *************************************************************~
            *              M A I N   P R O C E S S I N G                *~
            *-----------------------------------------------------------*~
            * Select Data and write the Extract File                    *~
            *************************************************************

            call "SHOSTAT" ("Extracting W-2 Information")

        read_loop

*        Initialize for a new employee
            mat fed   = zer    :  mat box13 = zer  :  mat box14 = zer
            mat e_state = zer  :  mat e_city = zer
            init (" ")  box13$(), box14$(), e_state$(), e_city$(),       ~
                        e_stateid$(), e_smthd$(), e_cmthd$()
            seq%, d%, box13%, box14%, s% = 0%

*        Get an employee and process his deductions
            call "READNEXT" (#5, f1%(5%))
                if f1%(5%) = 0% then L65000

            get #5, using L11230, last$, first$, middle$, ssn$, empcode$, ~
                                 empaddr$(1), empaddr$(2), empcity$,     ~
                                 empstate$, empzip$
L11230:         FMT XX(1), CH(15), CH(10), CH(1), CH(11), CH(12), XX(10),~
                    2*CH(30), CH(20), XX(20), CH(2), CH(9)

            empaddr$(3%) = empcity$ & ", " & empstate$ & "."
            empname$ = first$ & " " & last$
            if middle$ = " " then L11310
            empname$ = first$ & " " & middle$ & ". " & last$

L11310
*        Process the employee's deduction records
            readkey$ = empcode$

        load_deduction
            call "PLOWNEXT" (#4, readkey$, 12%, f1%(4%))
                if f1%(4%) = 0% then end_of_employee

            get #4, using L11410, method$, e_paid$, withheld, earnings
L11410:         FMT POS(34), CH(6), POS(52), CH(1), POS(141),            ~
                    PD(14,4), POS(213),  /* year to date withheld      */~
                    PD(14,4)             /* Year To Date Dollars Subjec*/

            if withheld = 0 and earnings = 0 then load_deduction

*        Check for Federal FIT Dedcution
            if method$ <> fedmthd$ then fica_check
            fed(fedtaxbox%)  = fed(fedtaxbox%)  + withheld
            fed(fedwagebox%) = fed(fedwagebox%) + earnings
            goto end_federal_boxes

        fica_check
            if method$ <> ficamthd$ then medicare_check
            fed(ficataxbox%)  = fed(ficataxbox%)  + withheld
            fed(ficawagebox%) = fed(ficawagebox%) + earnings
            goto end_federal_boxes

        medicare_check
            if method$ <> medicaremthd$ then other_deduction_check
            fed(medicaretaxbox%)  = fed(medicaretaxbox%)  + withheld
            fed(medicarewagebox%) = fed(medicarewagebox%) + earnings
            goto end_federal_boxes

        other_deduction_check
            search str(dedcode$()) = str(method$) to search%() step 6%
            if search%(1%) = 0% then state_check
            ded% = (search%(1%) + 5%) / 6%

*        1993 boxes #11, #13, and #14 require special handling
            if dedw2box%(ded%) = 11% then box11_process
            if dedw2box%(ded%) = 13% then box13_process
            if dedw2box%(ded%) = 14% then box14_process

*        1993 other boxes processing
            fed(dedw2box%(ded%)) = fed(dedw2box%(ded%)) + withheld
            goto end_federal_boxes

        box11_process
            if dedw2boxcode$(ded%) = "N" then  L12040
                fed(11%) = fed(11%) + withheld
                goto end_federal_boxes
L12040:     fed(15%) = fed(15%) + withheld
            goto end_federal_boxes

        box13_process
*        Box 13 values goto seperate arrays for later processing
            if box13% = 0% then L12190
                search str(box13$()) = dedw2boxcode$(ded%) to search%()  ~
                                                              step 1%
                if search%(1%) = 0% then L12190
                     box13(search%(1%)) = box13(search%(1%)) + withheld
                     goto end_federal_boxes

L12190:     box13%         = box13% + 1%
            box13$(box13%) = dedw2boxcode$(ded%)
            box13(box13%)  = box13(box13%) + withheld
            goto end_federal_boxes

        box14_process
*        Box 14 values goto seperate arrays for later processing
            if box14% = 0% then L12400
                search str(box14$()) = dedboxlabel$(ded%) to search%()   ~
                                                             step 12%
                if search%(1%) = 0% then L12400
                     temp% = (search%(1%) + 11%) / 12%
                     box14(temp%) = box14(temp%) + withheld
                     goto end_federal_boxes

L12400:     box14%         = box14% + 1%
            box14$(box14%) = dedboxlabel$(ded%)
            box14(box14%)  = box14(box14%) + withheld
            goto end_federal_boxes

        end_federal_boxes
            d% = d% + 1%
            goto end_of_deduction

        state_check
            if e_paid$ <> "Y" then end_of_deduction

            search str(smthd$()) = method$ to search%() step 6%
            if search%(1%) = 0% then city_check

            state% = (search%(1%) + 5%) / 6%
            if s% = 0% then L12720
                search str(e_smthd$()) = method$ to search%() step 6%
                if search%(1%) = 0% then L12720
                     temp% = (search%(1%) + 5%) / 6%
                     goto L12750
L12720:     s% = s% + 1%
            temp% = s%

L12750:     e_smthd$(temp%) = smthd$(state%)
            e_state$(temp%) = state$(state%)
            e_stateid$(temp%) = stateid$(state%)
            e_state(temp%, 1%) = e_state(temp%, 1%) + earnings
            e_state(temp%, 2%) = e_state(temp%, 2%) + withheld
            goto end_of_deduction

        city_check
            search str(cmthd$()) = method$ to search%() step 6%
            if search%(1%) = 0% then end_of_deduction

            city% = (search%(1%) + 5%) / 6%
            if s% = 0% then L13230
                search str(e_cmthd$()) = method$ to search%() step 6%
                if search%(1%) = 0% then L13040
                     temp% = (search%(1%) + 5%) / 6%
                     if smthd$(city%) <> e_smthd$(temp%) then L13180
                          e_city(temp%, 1%) = e_city(temp%, 1%) + earnings
                          e_city(temp%, 2%) = e_city(temp%, 2%) + withheld
                          goto end_of_deduction

L13040:         temp% = 1%
L13050:         search str(e_smthd$(),temp%) = smthd$(city%) to search%()~
                                                                step 6%
                if search%(1%) = 0% then L13230
                     search%(1%) = search%(1%) + temp% - 1%
                     temp% = search%(1%) + 6%
                     e_state% = (search%(1%) + 5%) / 6%
                     if e_cmthd$(e_state%) <> " " then L13180
                          e_cmthd$(e_state%) = cmthd$(city%)
                          e_city$(e_state%)  = city$(city%)
                          e_city(e_state%,1%) = e_city(e_state%,1%) +    ~
                                                                 earnings
                          e_city(e_state%,2%) = e_city(e_state%,2%) +    ~
                                                                 withheld
                          goto end_of_deduction

L13180:         if e_cmthd$(e_state%) <>   cmthd$(city%) then L13050
                     e_city(e_state%,1%) = e_city(e_state%,1%) + earnings
                     e_city(e_state%,2%) = e_city(e_state%,2%) + withheld
                     goto end_of_deduction

L13230:     s% = s% + 1%
            e_smthd$(s%)   = smthd$(city%)
            e_cmthd$(s%)   = cmthd$(city%)
            e_city$(s%)    = city$(city%)
            e_city(s%, 1%) = e_city(s%, 1%) + earnings
            e_city(s%, 2%) = e_city(s%, 2%) + withheld
            goto end_of_deduction

        end_of_deduction
            goto load_deduction

        end_of_employee
            if d% = 0% and s% = 0% then goto read_loop
            box13out%, box14out%, stateout% = 1%
            gosub initialize_for_output
            gosub pension_box_check
            gosub load_box13
            gosub load_box14
            gosub load_state_city
            gosub write_extract

            mat fed = zer
L14110:     write_flag$ = "N"
            gosub initialize_for_output
            gosub load_box13
            gosub load_box14
            gosub load_state_city
            if write_flag$ <> "Y" then read_loop
            gosub write_extract
            goto L14110

        initialize_for_output
*        Initial State and City output variables
            mat swages = zer
            mat stax   = zer
            mat lwages = zer
            mat ltax   = zer
            init (" ")  st$(), lc$(), sid$()

*        Clear Box 13 and 14 output variables
            init (" ")  box13codes$(), box14lbl$()
            for i% = 16% to 20%
                fed(i%) = 0
            next i%
            return

        pension_box_check
            init (" ")  box$()
            if box13% = 0% then return
            search str(box13$(),,box13%) = "D" to search%() step 1%
                if search%(1%) <> 0% then L16150
            search str(box13$(),,box13%) = "E" to search%() step 1%
                if search%(1%) <> 0% then L16150
            search str(box13$(),,box13%) = "F" to search%() step 1%
                if search%(1%) <> 0% then L16150
            search str(box13$(),,box13%) = "G" to search%() step 1%
                if search%(1%) <> 0% then L16160
            search str(box13$(),,box13%) = "H" to search%() step 1%
                if search%(1%) <> 0% then L16150
            return

L16150:     box$(3%) = "X"
L16160:     box$(7%) = "X"
            return

        load_box13
            if box13% = 0% then return
            if box13out% > box13% then return
            write_flag$ = "Y"

            for i% = 1% to 3%
                if box13out% > box13% then L17100
                     box13codes$(i%) = box13$(box13out%)
                     fed(15% + i%)   = box13(box13out%)
                     box13out%       = box13out% + 1%
L17100:     next i%
            return

        load_box14
            if box14% = 0% then return
            if box14out% > box14% then return
            write_flag$ = "Y"

            for i% = 1% to 2%
                if box14out% > box14% then L18100
                     box14lbl$(i%)   = box14$(box14out%)
                     fed(18% + i%)   = box14(box14out%)
                     box14out%       = box14out% + 1%
L18100:     next i%
            return

        load_state_city
            if s% = 0% then return
            if stateout% > s% then return
            write_flag$ = "Y"

            for i% = 1% to 2%
                if stateout% > s% then L19150
                     swages(i%) = e_state   (stateout%, 1%)
                     stax  (i%) = e_state   (stateout%, 2%)
                     st$   (i%) = e_state$  (stateout%)
                     lwages(i%) = e_city    (stateout%, 1%)
                     ltax  (i%) = e_city    (stateout%, 2%)
                     lc$   (i%) = e_city$   (stateout%)
                     sid$  (i%) = e_stateid$(stateout%)
                     stateout%  = stateout% + 1%
L19150:     next i%
            return

        write_extract
            seq% = seq% + 1%
            write #10 using L20160, empcode$, seq%, last$, first$,        ~
                                   middle$, ssn$, seq%, empname$,        ~
                                   empaddr$(), empzip$, fed(2%),         ~
                                   fed(1%), fed(4%), fed(3%),            ~
                                   fed(7%), fed(12%), swages(1%),        ~
                                   stax(1%), st$(1%), lwages(1%),        ~
                                   ltax(1%), lc$(1), lbl11$, defcomp,    ~
                                   box$(), sid$(1%), box13codes$(),      ~
                                   box14lbl$(), fed(6%), fed(5%), " ",   ~
                                   fed(8%), fed(9%), fed(10%), fed(11%), ~
                                   fed(15%), fed(19%), fed(20%),         ~
                                   fed(16%), fed(17%), fed(18%), 0,      ~
                                   swages(2%), stax(2%), st$(2%),        ~
                                   sid$(2%), lwages(2%), ltax(2%),       ~
                                   lc$(2%), " "
L20160:         FMT CH(12), BI(1), CH(15), CH(10), CH(1), CH(11),        ~
                    BI(1), CH(30), 3*CH(30), CH(9), 8*PD(14,4), CH(2),   ~
                    2*PD(14,4), CH(8), CH(12), PD(14,4), 8*CH(1),        ~
                    CH(10), 4*CH(1), 2*CH(12), 2*PD(14,4), CH(2),        ~
                    13*PD(14,4), CH(2), CH(10), 2*PD(14,4), CH(8), CH(6)

            return

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

