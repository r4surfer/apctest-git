        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  CCCC   M   M  SSSS    SSS   EEEEE   CCC   U   U  RRRR    *~
            *  C      MM MM  S      S      E      C   C  U   U  R   R   *~
            *  C      M M M  SSSS    SSS   EEEE   C      U   U  RRRR    *~
            *  C      M   M      S      S  E      C   C  U   U  R   R   *~
            *  CCCC   M   M  SSSS    SSS   EEEEE   CCC    UUU   R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CMSSECUR -                                                *~
            *          - THIS IS A GENERAL PURPOSE SUBROUTINE TO DISPLAY*~
            *            QUICK REFERENCE TEXT TO HELP A USER WITH       *~
            *            OVERVIEW INFO ABOUT CMS SYSTEM SECURITY        *~
            *            (CMS =  Program Access Control)                *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/19/85 ! ORIGINAL                                 ! GLW *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


        sub "CMSSECUR" (name$, returncode%)


            REM This subroutine will be changed later to be a general    ~
                purpose quick reference tool.  At that time the text to  ~
                be displayed will be in a data file which will be keyed  ~
                by NAME$.  For now it just displays the info about SES   ~
                shown in the ACCEPT statement below and NAME$ is not used

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
L00620:     cms2v$ = "04.15.09 01/27/86 Object code size and dfloat     "
        REM *************************************************************
        accept                                                           ~
               at (01,03),                                               ~
        "          CAELUS MANAGEMENT SYSTEMS - OVERVIEW OF SYSTEM SECURIT~
        ~Y",                                                              ~
               at (02,03),                                               ~
        "----------------------------------------------------------------~
        ~-------------",                                                  ~
               at (03,03),                                               ~
        "The program CMSCONTR regulates function access at three levels:"~
        ,                                                                ~
               at (04,03),                                               ~
        "(1) Menu Level - You can set User specific, Department, or Compa~
        ~ny wide menus",                                                  ~
               at (05,03),                                               ~
        "    of the same name.  A given user will see the first that exis~
        ~ts for them.",                                                   ~
               at (06,03),                                               ~
        "(2) Program access - You can set Access Requirements for program~
        ~s and    ",                                                      ~
               at (07,03),                                               ~
        "    Access Rights for users to limit who can run which programs ~
        ~or procs.  ",                                                    ~
               at (08,03),                                               ~
        "(3) File access - Thru SECURITY you can set file access rights."~
        ,                                                                ~
               at (09,03),                                               ~
        "----------------------------------------------------------------~
        ~-------------",                                                  ~
               at (10,03),                                               ~
        "Follow the steps below to set up a NEW USER to be authorized to ~
        ~run CMS:",                                                       ~
               at (12,03),                                               ~
        "(2) Be sure that at least the first MENU listed for that person ~
        ~has been ",                                                      ~
               at (13,03),                                               ~
        "    created - run CMSMENUG to generate menus.",                 ~
               at (11,03),                                               ~
        "(1) Run CMSUSRIN & enter info about the user (that sets up Wang ~
        ~SECURITY too).",                                                 ~
               at (14,03),                                               ~
        "----------------------------------------------------------------~
        ~-------------",                                                  ~
               at (15,03),                                               ~
        "(3) To further control program access (this is optional):",     ~
               at (16,03),                                               ~
        "    (a) Define access requirements for programs; run the program~
        ~ CMSPGMIN.",                                                     ~
               at (17,03),                                               ~
        "    (b) Set access rights for users; run CMSUSRIN, press PF-6.",~
               at (18,03),                                               ~
        "(4) To further control file access (this is optional):",        ~
               at (19,03),                                               ~
        "    (a) Define access requirements for data files by setting fil~
        ~e protection",                                                   ~
               at (20,03),                                               ~
        "        classes on files in your data base; COMMAND PROCESSOR, p~
        ~ress PF-5.",                                                     ~
               at (21,03),                                               ~
        "    (b) Set data file access rights for users; run CMSUSRIN, pre~
        ~ss PF-9 or",                                                     ~
               at (22,03),                                               ~
        "        run Wang SECURITY.",                                    ~
               at (23,03),                                               ~
        "----------------------------------------------------------------~
        ~-------------",                                                  ~
               at (24,03),                                               ~
        "Press:  (13)for the full users manual       (15)print screen    ~
        ~   (16)return",                                                  ~
            keys(hex(0d0f10)), key(keyhit%)

            if keyhit% <> 15% then goto L01370
                call "PRNTSCRN"
                goto  L00620

L01370:     if keyhit% = 13% then returncode% = 1% else returncode% = 0%

            end

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

