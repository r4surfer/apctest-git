        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   DDDD    AAA   N   N  PPPP    OOO    SSS   TTTTT   *~
            *  C   C  D   D  A   A  NN  N  P   P  O   O  S        T     *~
            *  C      D   D  AAAAA  N N N  PPPP   O   O   SSS     T     *~
            *  C   C  D   D  A   A  N  NN  P      O   O      S    T     *~
            *   CCC   DDDD   A   A  N   N  P       OOO    SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CDANPOST - Posts Master File changes (add, change, delete)*~
            *            for the master file identified by the passed   *~
            *            in file channel.  Change history written to    *~
            *            the file CDANMTIF where it will reside until   *~
            *            picked up & transmitted to a CDA PC by         *~
            *            CDAVSCOM.                                      *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/10/86 ! Original                                 ! LDJ *~
            * 11/02/88 ! Filename changed from CDANMTIF to CDAMTIF! LDJ *~
            *          ! now supports 'S' command.                !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "CDANPOST" (#3,              /* File # of Modified File    */~
                        opcode$)         /* A,C,D,F, or S              */

        dim                                                              ~
            datetime$7,                  /* Date Time Stamp            */~
            file$8,                      /* File Name of Passed in file*/~
            files$(12)8,                 /* Files Supported            */~
            file_download$(6)1,          /* Down Load Files Flags      */~
            lastfile$8,                  /* File Name of Last File     */~
            lib$8,                       /* Library of passed in File  */~
            library$(6)8,                /* Databases Supported by CDAN*/~
            node$(6)16,                  /* CDAN Node Names            */~
            opcode$1,                    /* Add, Change, Delete, Full F*/~
            p%(6),                       /* Search Receiver Array      */~
            readkey$20,                  /* SYSDFLTS Read Key          */~
            record$(8)248,               /* File Record Area           */~
            ufbkd$2,                     /* Key Displacement Rel to 0  */~
            ufbkl$1,                     /* Key Length this File       */~
            update$6,                    /* File Eligible for Update?  */~
            userid$3,                    /* Current User Id            */~
            vol$6                        /* Volume of passed in File   */~

        dim f2%(02),                     /* = 0 if the file is open    */~
            f1%(02),                     /* = 1 if READ was successful */~
            fs%(02),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            axd$4,                       /* Obsolete                   */~
            rslt$(02)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "
        REM *************************************************************

            if fs%(1%) < 0% then end     /* No SYSDFLTS File           */
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSDFLTS ! System Wide (non-database dependant) Sys *~
            * #2  ! CDAMTIF  ! Master File Change History file for CDAN *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SYSDFLTS",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #2,  "CDAMTIF",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  31

            if fs%(1) = 1% then L09000     /* Already Opened ...         */
            call "OPENOLIB" (#1, "SHARE", f2%(1%), rslt$(1%), axd$)
            if f2%(1%) <> 0% then fs%(1) = -1%
            if f2%(1%) <> 0% then end
            fs%(1%) = 1%

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for file passed in      *~
            *************************************************************
            call "GETNAMES" addr(#3, file$, lib$, vol$)
            if file$ = lastfile$ then L09340
            lastfile$ = file$
            call "GETUFBKL" addr(#3, ufbkl$)       /* Key Length (Bin) */
            call "GETUFBDK" addr(#3, ufbkd$)       /* Key Displ. (Bin) */
            key_pos% = val(ufbkd$,2) + 1%
            descr_pos% = val(ufbkl$) + key_pos%
            if descr_pos% > 1900% then descr_pos% = 0%
            if userid$ > " " then L09230
               call "EXTRACT" addr("ID", userid$)
               readkey$ = "CDAN NAMES"
               call "READ100" (#1, readkey$, f1%(1%))
               if f1%(1%) = 1% then L09210
                  fs%(1%) = -1%
                  close #1
                  end
L09210:        get #1 using L09220, node$(), library$(), file_download$()
L09220:        FMT POS(21), 6*CH(16), 6*CH(8), POS(178), 6*CH(1)
L09230:     update$ = "NNNNNN"
            if opcode$ = "S" then send_command
            for x% = 1% to 6%
               if lib$ <> library$(x%) then L09330
               if file_download$(x%) <> "Y" then L09330
               readkey$ = "CDAN" & node$(x%)
               call "READ100" (#1, readkey$, f1%(1%))
               if f1%(1%) = 0% then L09330
               get #1 using L09370, files$()
               search files$() = file$ to p%() step 8%
               if p%(1%) > 0% then str(update$,x%,1%) = "Y"
L09330:     next x%
L09340:     if update$ = "NNNNNN" then end
            if fs%(2%) = 0% then                                         ~
               call "OPENCHCK" (#2, fs%(2%), f2%(2%),1000%, rslt$(2%))
L09370:     FMT POS(21), 12*CH(8)

        REM *************************************************************~
            *             U P D A T E   T I F   F I L E                 *~
            *-----------------------------------------------------------*~
            * Get Key value (& description - if applicable) & stuff into*~
            * TIF file.                                                 *~
            *************************************************************
            get #3, str(record$(),1%)
            for x% = 1% to 6%
                if str(update$,x%,1%) <> "Y" then L10121
L10103:         call "GETDTTM" addr(datetime$)
                put #2 using L35030, node$(x%), file$, datetime$, userid$,~
                          opcode$, key_pos%, descr_pos%, record$(), " "
                write #2, eod goto L10103
L10121:     next x%
            end

        send_command
            if fs%(2) = 0% then                                          ~
               call "OPENCHCK" (#2, fs%(2), f2%(2),1000%, rslt$(2))
            redim record$(14)79
            get #3, node$(1%), record$()
                FMT CH(16), 14*CH(79)
            redim record$(8)248
            call "GETDTTM" addr(datetime$)
L10170:     write #2 using L35030, node$(1%), " ", datetime$, userid$,    ~
                       opcode$, 0%, 0%, record$(), eod goto L10170
            end

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************
L35030: FMT                 /* FILE: CDAMTIF                           */~
            CH(16),         /* Node Name to Download Record To         */~
            CH(8),          /* Name of a File                          */~
            CH(7),          /* Date / Time Stamp                       */~
            CH(3),          /* User who initiated transaction          */~
            CH(1),          /* Identifies type of transaction          */~
            BI(2),          /* Key Position                            */~
            BI(2),          /* Description Position                    */~
            8*CH(248),      /* Record Area                             */~
            CH(1)           /* Filler                                  */
