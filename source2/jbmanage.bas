        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB   M   M   AAA   N   N   AAA    GGG   EEEEE   *~
            *    J    B   B  MM MM  A   A  NN  N  A   A  G      E       *~
            *    J    BBBB   M M M  AAAAA  N N N  AAAAA  G GGG  EEEE    *~
            *  J J    B   B  M   M  A   A  N  NN  A   A  G   G  E       *~
            *   J     BBBB   M   M  A   A  N   N  A   A   GGG   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBMANAGE - Performs nearly all management and reporting   *~
            *            activities for CMS II Shop Floor Control.      *~
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
            * 01/27/83 ! ORIGINAL  (JBMANAG2)                     ! GLW *~
            * 06/22/83 ! ALTERED JOB OPEN, PICK AND KIT FUNCTIONS ! KAB *~
            * 01/18/84 ! Correct WC In/Out Paging, Error Handling ! ECR *~
            * 02/02/84 ! JBCMPSUB USES USERS HNYDATE              ! KAB *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION   ! RAC *~
            * 10/16/85 ! CHANGED CALL SYNTAX TO SEV SUBS (BCKGND) ! HES *~
            *          ! JBKITSUB,JBHNYSUB,JBCMPSUB,JBPICKSL,     !     *~
            *          ! WCDINSUB,JBRELSUB.                       !     *~
            * 03/16/86 ! Change JBSTATUS file fmt                 ! HES *~
            * 02/05/87 ! Changes for Serial Number Control.       ! LDJ *~
            * 03/16/87 ! Replaced call to DESCRIBE w/ GETCODE     ! ERN *~
            * 07/08/87 ! Program changed to a driver (as opposed  ! ERN *~
            *          !   to a putter) for the job management    !     *~
            *          !   functions.  Renamed JBMANAGE.          !     *~
            * 06/30/88 ! Change Arquments for call to JBACTSUB    ! RJM *~
            * 01/03/89 ! Removed JOBMASTR from program and from   ! MJB *~
            *          !  argument list for JBPICKCR              !     *~
            * 03/14/90 ! Bypass #17 in OPEN Loop                  ! KAB *~
            * 08/27/91 ! Control Number Pegging Project.          ! SID *~
            *          !   Added Alt. Keys to JBMASTR2 File.      !     *~
            * 05/28/93 ! Pass PIPOUT to JBHNYSUB                  ! KAB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            date$8,                      /* Date for Screen Display    */~
            errormsg$79,                 /* Error Message              */~
            hdr$38,                      /* Screen Header              */~
            job$8, jobdescr$32,          /* Job Number & Description   */~
            inpmessage$79,               /* Input Message              */~
            jnl$3,                       /* JBHNYSUB Journal ID        */~
            line2$79,                    /* 2nd Screen Line            */~
            mode$2,                      /* JBHNYSUB mode              */~
            pfkeys$32,                   /* PF Keys Available          */~
            opt$(10)38,                  /* Selection Options          */~
            userid$3,                    /* User ID                    */~
                                                                         ~
            f1%(64),                     /* Record-on-File Flags       */~
            f2%(64)                      /* File Status Flags          */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.04 06/29/93 SFC & Cycle Count Enhancements  "
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *  3  ! HNYMASTR ! Inventory Master                         *~
            *  4  ! JBMASTR2 ! Job Master                               *~
            *  5  ! JBMATER2 ! Job Material Ledger                      *~
            *  7  ! JBCREDIT ! Job Credits Ledger                       *~
            *  8  ! JBSTATUS ! Job Status Tracking                      *~
            *  10 ! RTEMASTR ! Standard & Alt Work Center Routings      *~
            *  11 ! WCMASTR  ! Work Center Master                       *~
            *  12 ! GLMAIN   ! General Ledger Master                    *~
            *  13 ! CALMASTR ! Production Calendar                      *~
            *  15 ! PERMASTR ! Personnel Master                         *~
            *  16 ! GENCODES ! General Codes Files                      *~
            *  18 ! BOMMASTR ! Bills of Materials Structures            *~
            *  19 ! ENGMASTR ! BOM and Rte Effectivity Dates            *~
            *  20 ! USERINFO ! User Posting Dates                       *~
            *  23 ! WCOUT    ! Work Center Cross Reference              *~
            *  34 ! PIPOUT   ! Planned Position Out                     *~
            *  36 ! JBPIPXRF ! Option Part Tag                          *~
            *  45 ! JBCROSS2 ! Job RTE/BOM used cross ref.              *~
            *  52 ! HNYQUAN  ! Inventory Quantities                     *~
            *  54 ! SYSFILE2 ! System File                              *~
            *  59 ! STORNAME ! Store Master File                        *~
            *  61 ! SERTIF   ! Additions buffer for inventory S/N's     *~
            *  62 ! SERMASTR ! Serial Number Tracking Master File       *~
            *  63 ! SERWORK  ! Temporary Serial #'s Work File           *~
            *************************************************************


            select #3, "HNYMASTR",                                       ~
                       varc, indexed, recsize = 900,                     ~
                       keypos = 1, keylen = 25,                          ~
                       alt key  1, keypos = 102, keylen = 9, dup,        ~
                           key  2, keypos =  90, keylen = 4, dup

            select #4, "JBMASTR2",                                       ~
                       varc, indexed, recsize = 1300,                    ~
                       keypos = 1, keylen = 8,                           ~
                       alt key 1, keypos = 1120, keylen = 19, dup,       ~
                           key 2, keypos = 1139, keylen = 25, dup

            select #5, "JBMATER2",                                       ~
                       varc, indexed, recsize = 400,                     ~
                       keypos = 1, keylen = 22,                          ~
                       alt key  1, keypos = 23, keylen = 48

            select #7, "JBCREDIT",                                       ~
                       varc, indexed, recsize = 500,                     ~
                       keypos = 1, keylen = 22,                          ~
                       alt key  1, keypos = 23, keylen = 48

            select #8,  "JBSTATUS",                                      ~
                        varc, indexed, recsize = 200,                    ~
                        keypos = 1, keylen = 12,                         ~
                        alt key  1, keypos =  21, keylen = 44,           ~
                            key  2, keypos =  29, keylen = 36

            select #10, "RTEMASTR",                                      ~
                         varc, indexed, recsize = 400,                   ~
                         keypos = 5, keylen = 31,                        ~
                         alt key  1, keypos =  1, keylen = 35

            select #11, "WCMASTR",                                       ~
                        varc, indexed, recsize = 2024,                   ~
                        keypos = 2, keylen = 5,                          ~
                        alt key  1, keypos = 1, keylen = 6

            select #12, "GLMAIN",                                        ~
                        varc, indexed, recsize = 300,                    ~
                        keypos = 1, keylen = 9

            select #13, "CALMASTR",                                      ~
                        varc, indexed, recsize = 1962,                   ~
                        keypos = 1, keylen = 2

           select #15, "PERMASTR",                                       ~
                        varc, indexed, recsize = 950,                    ~
                        keypos = 39, keylen = 12

            select #16, "GENCODES",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos =  1, keylen =  24

            select #18, "BOMMASTR",                                      ~
                        varc, indexed, recsize = 150,                    ~
                        keypos = 26, keylen = 31,                        ~
                        alt key   1, keypos =  1, keylen = 56

            select #19, "ENGMASTR",                                      ~
                        varc, indexed, recsize = 2015,                   ~
                        keypos = 1, keylen = 29

            select #20, "USERINFO",                                      ~
                        varc, indexed, recsize = 150,                    ~
                        keypos = 1, keylen = 3

            select #23, "WCOUT",                                         ~
                        varc, indexed, recsize = 68,                     ~
                        keypos = 9, keylen = 23,                         ~
                        alt key  1, keypos = 1, keylen = 27

            select #34, "PIPOUT",                                        ~
                        varc, indexed, recsize = 64,                     ~
                        keypos = 1, keylen = 56,                         ~
                        alt key  1, keypos = 20, keylen = 37

            select #36, "JBPIPXRF",                                      ~
                        varc, indexed, recsize = 63,                     ~
                        keypos = 1, keylen = 63,                         ~
                        alt key  1, keypos = 45, keylen = 19

            select #45, "JBCROSS2",                                      ~
                        varc, indexed, recsize = 94,                     ~
                        keypos = 29, keylen = 19,                        ~
                        alt key   1, keypos =  1, keylen = 47,           ~
                            key   2, keypos = 48, keylen = 47

            select #52, "HNYQUAN",                                       ~
                        varc, indexed, recsize = 650,                    ~
                        keypos = 17, keylen = 44,                        ~
                        alt key   1, keypos =  1, keylen = 44

            select #54, "SYSFILE2",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos = 1, keylen = 20

            select #59, "STORNAME",                                      ~
                        varc, indexed, recsize = 300,                    ~
                        keypos = 1, keylen = 3

            select #61, "SERTIF",                                        ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 1, keylen = 62

            select #62, "SERMASTR",                                      ~
                        varc, indexed, recsize =  300,                   ~
                        keypos = 52, keylen =  45,                       ~
                        alt key   1, keypos =  32, keylen =  45,         ~
                            key   2, keypos =   1, keylen =  76

            select #63, "SERWORK",                                       ~
                        varc, indexed, recsize = 48,                     ~
                        keypos = 1, keylen = 23


        call "SHOSTAT" ("Preparing for Shop Floor Control And Review " & ~
                        "Functions")
            call "OPENCHCK" (# 3, 0%, f2%( 3), 0%, " ")
            call "OPENCHCK" (# 4, 0%, f2%( 4), 0%, " ")
            call "OPENCHCK" (# 5, 0%, f2%( 5), 0%, " ")
            call "OPENCHCK" (# 7, 0%, f2%( 7), 0%, " ")
            call "OPENCHCK" (# 8, 0%, f2%( 8), 100%, " ")
            for f% = 10% to 23%
                on f%-13% goto L03620,,,L03620,,,,L03620,L03620
                     call "OPENCHCK" (#f%, 0%, f2%(f%), 0%, " ")
L03620:     next f%
            call "OPENCHCK" (#34, 0%, f2%(34), 0%, " ")
            call "OPENCHCK" (#36, 0%, f2%(36), 0%, " ")
            call "OPENCHCK" (#45, 0%, f2%(45), 0%, " ")
            call "OPENCHCK" (#52, 0%, f2%(52), 0%, " ")
            call "OPENCHCK" (#54, 0%, f2%(54), 0%, " ")
            call "OPENCHCK" (#59, 0%, f2%(59), 0%, " ")


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date : call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)

            str(line2$,62) = "JBMANAGE: " & str(cms2v$,,8)
            inpmessage$ = "To see a list of Jobs on file, blank Job" &   ~
                          " Number and Press RETURN."

*        Set up Descriptors for Management Functions
            hdr$     = "      Job Management Functions        "
            opt$( 1) = "( 1) Issue Materials (Kit) to Jobs    "
            opt$( 2) = "( 2) Print Pick Lists for Jobs        "
            opt$( 3) = "( 3) Move Components from Job to Stock"
            opt$( 4) = "( 4) Move Components from Job to Job  "
            opt$( 5) = "( 5) Report Job Work Center Movement  "
            opt$( 6) = "( 6) Make Direct Cost Entries         "
            opt$( 7) = "                                      "
            pfkeys$  = hex(00010203040506ffffffffffff0dff0f10)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        menu

          accept                                                         ~
            at(01,02),                                                   ~
                "CAELUS MANAGEMENT SYSTEMS - MASTER SHOP FLOOR CONTROL", ~
            at(01,67), "Date: ",                                         ~
            at(01,73), fac(hex(8c)), date$,                              ~
            at(02,02), fac(hex(ac)), line2$,                             ~
            at(04,02), fac(hex(94)), errormsg$, ch(79),                  ~
                                                                         ~
            at(06,02), "Default Job to Manage: ",                        ~
            at(06,25), fac(hex(81)), job$,                               ~
            at(06,35), fac(hex(84)), jobdescr$,                          ~
                                                                         ~
            at(08,20), fac(hex(ac)), hdr$    ,                           ~
            at(09,20), fac(hex(84)), opt$( 1),                           ~
            at(10,20), fac(hex(84)), opt$( 2),                           ~
            at(11,20), fac(hex(84)), opt$( 3),                           ~
            at(12,20), fac(hex(84)), opt$( 4),                           ~
            at(13,20), fac(hex(84)), opt$( 5),                           ~
            at(14,20), fac(hex(84)), opt$( 6),                           ~
            at(15,20), fac(hex(84)), opt$( 7),                           ~
            at(16,20), fac(hex(84)), opt$( 8),                           ~
            at(17,20), fac(hex(84)), opt$( 9),                           ~
                                                                         ~
            at(21,02), fac(hex(ac)), inpmessage$,                        ~
            at(22,65), "(13)Instructions",                               ~
            at(23,65), "(15)Print Screen",                               ~
            at(24,65), "(16)Exit Program",                               ~
                keys(pfkeys$), key(keyhit%)


            if keyhit%  = 13% then call "MANUAL" ("JBMANAGE")
            if keyhit%  = 15% then call "PRNTSCRN"
            if keyhit%  = 16% then L65000

            if keyhit%  =  1% then issue_parts
            if keyhit%  =  2% then prnt_picklist
            if keyhit%  =  3% then job_to_hny
            if keyhit%  =  4% then job_to_job
            if keyhit%  =  5% then report_inout
            if keyhit%  =  6% then direct_entries

            if keyhit% <>  0% then menu
                call "GETCODE" (#4, job$, jobdescr$, 0%, 0, f1%(4))
                if f1%(4) = 0% or keyhit% = 0% then menu


        issue_parts
            call "JBKITSUB"  ("03", "MPR", #3, #4, #34, #52, #54, #59,   ~
                              #36, #7, #20, #61, #62, #63, job$, f2%())
            goto menu


        prnt_picklist
            call "JBPICKCR" (#54, #18, #34, #23, #3, #4, #10, #11,       ~
                             #45, #52, #19, #13)
            close printer
            goto menu

        job_to_hny
            jnl$ = "MPW" : mode$ = "RJ" : goto L10700
        job_to_job
            jnl$ = "MJR" : mode$ = "JJ"
L10700:     call "JBHNYSUB"                                              ~
                        ("03", jnl$, #20, #54, #4, #3, #52, #5, #59,     ~
                         #61, #62, #63, #34, job$, mode$, f2%(54))
            goto menu

        report_inout
            call "JBACTSUB" (#4, #8, #3, #11, #15, #16, #10, #45, #18,   ~
                             #19, #54, job$, " ", " ", " ", 0)
            goto menu

        direct_entries
            call "JBDIRSUB" (" ", " ", " ", " ", job$, " ",              ~
                             #54, #4, #3, #12, #20, #11, #16)
            goto menu

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Gets us outta here! (So long Doogan [HES]).               *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            call "SHOSTAT" ("Closing Files, One Moment Please")
            end
