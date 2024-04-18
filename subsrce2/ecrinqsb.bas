        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  EEEEE   CCC   RRRR   IIIII  N   N   QQQ    SSS   BBBB    *~
            *  E      C   C  R   R    I    NN  N  Q   Q  S      B   B   *~
            *  EEEE   C      RRRR     I    N N N  Q   Q   SSS   BBBB    *~
            *  E      C   C  R   R    I    N  NN  Q Q Q      S  B   B   *~
            *  EEEEE   CCC   R   R  IIIII  N   N   QQQ    SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ECRINQSB - Inquiry Routine For Parts To Allow User To     *~
            *            View Past Or Pending ECR's For The Passed In   *~
            *            Part.  Has 2 Functions.  One Function Allows   *~
            *            Viewing Of Ecr And Related Info (If Any).  The *~
            *            Other Function Will Determine If There Is Any  *~
            *            ECR info to view and returns a PFKey prompt    *~
            *            value which will be either blank (if no ECR    *~
            *            info), or have the value "ECR Info" appended   *~
            *            to the passed in PFKey Prompt if there is ECRs.*~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1994, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/11/94 ! Original                                 ! LDJ *~
            * 07/15/96 ! Changes for the year 2000.               ! DXL *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "ECRINQSB" (operation$,      /* "C" to Check for ECR Info  */~
                                         /*    and return PFKey Prompt */~
                                         /* "A" to Perform Inquiry on  */~
                                         /*     ALL ECR's              */~
                                         /* "O" to Perform Inquiry on  */~
                                         /*     Open ECR's Only        */~
                        part$,           /* Part to Do Inquiry/Check on*/~
                        pfkey$,          /* IN:  PFKey # to Use        */~
                                         /*      e.g. "(11)".          */~
                                         /* OUT: Formatted PFKey Prompt*/~
                                         /*    Will be BLANK if no ECRs*/~
                                         /*    Else "ECR Info" will be */~
                                         /*    appended to the passed  */~
                                         /*    in value.               */~
                        #06,             /* SYSFILE2                   */~
                        #07)             /* HNYMASTR                   */

        dim                                                              ~
            blankdate$8,                 /* Blank date for comparison  */~
            columnhdr$(3)80,             /* PLOWCODE Column Headers    */~
            descr_map(14),               /* PLOWCODE Description Map   */~
            ecr_number$16,               /* ECR Number                 */~
            incl_excl(1), incl_excl$(1)8,/* PLOWCODE Include/Exclude   */~
            operation$1,                 /* Op Arg                     */~
            part$25,                     /* Part Number to Check/Disply*/~
            pfkey$14,                    /* PFKey Prompt Arg           */~
            plowkey$99,                  /* Miscell Read/Plow Key      */~
            reason$15,                   /* Reason for ECR             */~
            reasondescr$32,              /* Reason for ECR             */~
            status$15,                   /* Current Status             */~
            statusdescr$32,              /* Current Status             */~
            syslib$8,                    /* System Library Name        */~
            sysvol$6,                    /* System Volume Name         */~
            title$40                     /* Title of ECR               */

        dim f2%(14),                     /* FILE STATUS FLAGS FOR      */~
            f1%(14),                     /* RECORD-ON-FILE FLAGS       */~
            fs%(14),                     /* FILE STATUS FLAGS FOR      */~
            rslt$(14)20                  /* RETURN CODE FROM "FILEOPEN"*/~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! USERLCMS ! CMS Program Access Control User Info fil *~
            * #02 ! GENCODES ! System General Codes file.               *~
            * #03 ! ECRAUTHU ! ECT Users who may Approve/Deny ECR Appro *~
            * #04 ! ECRMASTR ! ECT Engineering Change Request Master Fi *~
            * #05 ! ECRAPPRV ! ECT module ECR Approvals Tracking file   *~
            * #06 ! SYSFILE2 ! Caelus Management System Information     *~
            * #07 ! HNYMASTR ! Inventory Master File                    *~
            * #08 ! BOMMASTR ! BOM relationship file                    *~
            * #09 ! RTEMASTR ! Production routing master file           *~
            * #10 ! ENGMASTR ! Engineering Calendar Master              *~
            * #11 ! CALMASTR ! Manufacturing Calendar Master File       *~
            * #12 ! TXTFILE  ! System Text File                         *~
            * #13 ! STCBOMXF ! BOM and Std Cost Cross Reference File    *~
            * #14 ! DUMMY    ! Dummy File for PLOWCODE call             *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "USERLCMS",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos = 1,    keylen =  3

            select #02, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #03, "ECRAUTHU",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  18,                     ~
                        alt key  1, keypos =   16, keylen =  18

            select #04, "ECRMASTR",                                      ~
                        varc,     indexed,  recsize =  1024,             ~
                        keypos =    9, keylen =  16,                     ~
                        alt key  1, keypos =    1, keylen =  24,         ~
                            key  2, keypos =  125, keylen =  28, dup,    ~
                            key  3, keypos =   25, keylen =  40, dup,    ~
                            key  4, keypos =  410, keylen =  15, dup

            select #05, "ECRAPPRV",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =  1,   keylen =         31,              ~
                        alt key  1, keypos =   17, keylen =  15, dup

            select #08, "BOMMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56

            select #09, "RTEMASTR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    5, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  35

            select #10, "ENGMASTR",                                      ~
                        varc, indexed, recsize = 2015,                   ~
                        keypos =    1, keylen =  29

            select #11, "CALMASTR",                                      ~
                        varc, indexed, recsize = 1962,                   ~
                        keypos =    1, keylen =   2

            select #12, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11

            select #13, "STCBOMXF",                                      ~
                        varc, indexed, recsize = 72,                     ~
                        keypos = 29, keylen = 33,                        ~
                        alt key 1, keypos =  1, keylen = 36,             ~
                            key 2, keypos = 37, keylen = 36

            select #14, "DUMMY",                                         ~
                        varc, indexed, recsize = 4 ,                     ~
                        keypos =  1, keylen =  2

            call "OPENCHCK" (#04, fs%(04), f2%(04), 200%, rslt$(04))
            if operation$ = "C" then check_for_ecr_info

            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 400%, rslt$(05))
            call "OPENCHCK" (#08, fs%(08), f2%(08), 0%, rslt$(08))
            call "OPENCHCK" (#09, fs%(09), f2%(09), 0%, rslt$(09))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))
            call "OPENCHCK" (#13, fs%(13), f2%(13), 0%, rslt$(13))

            if f2%(1%) <> 0% then L03050
            call "EXTRACT" addr("XL",syslib$,"XV",sysvol$)
            call "READFDR" addr("USERLCMS", syslib$, sysvol$, 0%, f2%(1%))
            if f2%(1%) <> 0% then L03050
            call "PUTNAMES" addr(#1, "USERLCMS", syslib$, sysvol$)
            call "WORKOPN2" (#1, "SHARE", 0%, f2%(1))

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

L03050: REM *************************************************************~
            *                C A L L   E C R A P S U B                  *~
            *-----------------------------------------------------------*~
            * Call ECRAPSUB specifying Inquiry/View Option.             *~
            *************************************************************
            columnhdr$(3%) = hex(ac) & "Engineering Change Request Tracki~
        ~ng - ECR Inquiry for Part"
            str(columnhdr$(3%),62%) = "ECRINQSB: " & str(cms2v$,,8)
            columnhdr$(1%) = "Control #  ECR Number       Orig Date  Sche~
        ~d Date  Date Enacted   Status"
            mat descr_map = zer
            descr_map(01%) = 001.08  : descr_map(02%) = 001 /* Control  */
            descr_map(03%) = 009.16  : descr_map(04%) = 010 /* ECR Nbr  */
            descr_map(05%) = 156.081 : descr_map(06%) = 027 /* Orig Date*/
            descr_map(07%) = 164.081 : descr_map(08%) = 038 /* Sched Dat*/
            descr_map(09%) = 425.081 : descr_map(10%) = 051 /* Enact Dat*/
            descr_map(11%) = 410.15  : descr_map(12%) = 065 /* Status   */
            descr_map(13%) = 025.40  : descr_map(14%) =1027 /* Title    */
            mat incl_excl = zer
            if operation$ = "A" then L03190
               incl_excl(1%) = 425.08 : incl_excl$(1%) = " "
L03190:     plowkey$ = part$
            str(plowkey$,26%,3%) = hex(000000)
            title$ = hex(06) & "ECR's for:" & hex(84) & part$
            if operation$ = "O" then str(title$,2%) = "Open " &          ~
                                     str(title$,2%)
            call "PLOWCODE" (#04, plowkey$, title$, 9025%, 2.4, f1%(04%),~
                             columnhdr$(),  0, 0,   incl_excl(),         ~
                             incl_excl$(), "D", " ", #14, descr_map())
            if f1%(04%) = 0% then exit_program
            ecr_number$ = key(#04)

            call "ECRAPSUB" ("V",        /* Inquiry Mode               */~
                        ecr_number$,     /* ECR Number     CH(16)      */~
                        title$,          /* Title of ECR   CH(40)      */~
                        reason$,         /* Reason for ECR CH(15)      */~
                        reasondescr$,    /* Reason for ECR CH(32) Descr*/~
                        status$,         /* Current Status CH(15)      */~
                        statusdescr$,    /* Current Status CH(32) Descr*/~
                        #01,             /* USERLCMS                   */~
                        #02,             /* GENCODES                   */~
                        #03,             /* ECRAUTHU                   */~
                        #04,             /* ECRMASTR                   */~
                        #05,             /* ECRAPPRV                   */~
                        #06,             /* SYSFILE2                   */~
                        #07,             /* HNYMASTR                   */~
                        #08,             /* BOMMASTR                   */~
                        #09,             /* RTEMASTR                   */~
                        #10,             /* ENGMASTR                   */~
                        #11,             /* CALMASTR                   */~
                        #12,             /* TXTFILE                    */~
                        #13,             /* STCBOMXF                   */~
                        f1%(14%))        /* Return Code to Caller      */

            goto exit_program

        check_for_ecr_info
            pf$ = hex(8c) & str(pfkey$,,max(1%,pos(pfkey$=")")))
            pfkey$ = " "
            plowkey$ = part$
            str(plowkey$,26%,3%) = hex(000000)
L11020:     call "PLOWALTS" (#04, plowkey$, 2%, 25%, f1%(4%))
            if f1%(4%) = 0% then exit_program
            pfkey$ = pf$ & "ECR Info"
            get #04, using L11100, chg_enacted_date$
            if chg_enacted_date$ <> " " and ~
               chg_enacted_date$ <> blankdate$ then L11020
            pfkey$ = hex(84) & str(pfkey$,2%)
            pfkey$ = pfkey$  & hex(8c)
L11100:     FMT POS(425), CH(08)         /* Date ECR Implemented       */

            goto exit_program

        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1994, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        exit_program
            end
