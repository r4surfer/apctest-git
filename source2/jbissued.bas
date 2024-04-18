        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   IIIII   SSS    SSS   U   U  EEEEE  DDDD    *~
            *    J    B   B    I    S      S      U   U  E      D   D   *~
            *    J    BBBB     I     SSS    SSS   U   U  EEEE   D   D   *~
            *  J J    B   B    I        S      S  U   U  E      D   D   *~
            *   J     BBBB   IIIII   SSS    SSS    UUU   EEEEE  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBISSUED - Issue materials to jobs.                       *~
            *            Simply a driver for 'JBKITSUB'.                *~
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
            * 04/07/83 ! ORIGINAL                                 ! KEN *~
            * 10/07/83 ! CHANGED INTO SUBROUTINE                  ! HES *~
            * 04/18/84 ! ADDED HNYDATE LOGIC                      ! HES *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            * 10/03/85 ! Changed to background posting            ! HES *~
            * 02/10/87 ! Changes for Serial Number Handling       ! LDJ *~
            * 06/15/87 ! Standard Costing Changes                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RETURN CODE FROM FILE OPEN */~
            rslt$(64)20                  /*                            */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard costs to 12            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #3  ! HNYMASTR ! Inventory master (descriptions)          *~
            * #4  ! JBMASTR2 ! Job master file                          *~
            * #7  ! JBCREDIT ! Job finished goods ledger                *~
            * #20 ! USERINFO ! Users posting dates                      *~
            * #34 ! PIPOUT   ! Planned position out                     *~
            * #36 ! JBPIPXRF ! Job PIP xref                             *~
            * #52 ! HNYQUAN  ! Inventory quantity  file                 *~
            * #54 ! SYSFILE2 ! Inventory default values                 *~
            * #59 ! STORNAME ! Store codes                              *~
            * #61 ! SERTIF   ! Additions buffer for inventory S/N's     *~
            * #62 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #63 ! SERWORK  ! Temporary Serial #'s Work File           *~
            *************************************************************


            select #3, "HNYMASTR",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 900,                                    ~
                       keypos = 1, keylen = 25

            select #4, "JBMASTR2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 1300,                                   ~
                       keypos = 1, keylen = 8

            select #7,   "JBCREDIT",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 22,                        ~
                         alternate key 1, keypos = 23, keylen = 48

            select #20, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select #34, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  64,                                   ~
                        keypos = 1, keylen = 56,                         ~
                        alternate key 1, keypos = 20, keylen = 37

           select #36, "JBPIPXRF",varc, indexed, recsize=63,             ~
                        keypos=1, keylen=63,                             ~
                        alt key 1, keypos=45, keylen=19

            select #52, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos= 17, keylen = 44,                         ~
                        alternate key 1, keypos =  1, keylen = 44

            select #54, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #59, "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            select #61, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 62

            select #62, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #63, "SERWORK",                                       ~
                        varc,     indexed,  recsize =   48,              ~
                        keypos = 1, keylen = 23

            call "SHOSTAT" ("Opening Files, One Moment Please")
            rslt$(20) = "REQUIRED"
            call "OPENCHCK" (# 3, f1%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, f1%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 7, f1%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (#20, f1%(20), f2%(20), 0%, rslt$(20))
            call "OPENCHCK" (#34, f1%(34), f2%(34), 0%, rslt$(34))
            call "OPENCHCK" (#36, f1%(36), f2%(36), 0%, rslt$(36))
            call "OPENCHCK" (#52, f1%(52), f2%(52), 0%, rslt$(52))
            call "OPENCHCK" (#54, f1%(54), f2%(54), 0%, rslt$(54))
            call "OPENCHCK" (#59, f1%(59), f2%(59), 0%, rslt$(59))

            if f2%(20) <> 0 then exit_program

        REM *************************************************************~
            *                I S S U E   P A R T S                      *~
            *-----------------------------------------------------------*~
            * Complete ability to issue parts to a job.                 *~
            *************************************************************

          call "JBKITSUB"  ("03", "MPR", #3, #4, #34, #52, #54, #59, #36,~
                            #7, #20, #61, #62, #63, " ", f2%())

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
