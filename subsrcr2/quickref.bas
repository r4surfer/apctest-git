        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   QQQ   U   U  IIIII   CCC   K   K  RRRR   EEEEE  FFFFF   *~
            *  Q   Q  U   U    I    C   C  K  K   R   R  E      F       *~
            *  Q   Q  U   U    I    C      KKK    RRRR   EEEE   FFFF    *~
            *  Q Q Q  U   U    I    C   C  K  K   R   R  E      F       *~
            *   QQQ    UUU   IIIII   CCC   K   K  R   R  EEEEE  F       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * QUICKREF - THIS IS A GENERAL PURPOSE SUBROUTINE TO DISPLAY*~
            *            QUICK REFERENCE TEXT TO HELP A USER WITH       *~
            *            OVERVIEW INFO AND DEFINITIONS OF TERMS.  IT    *~
            *            PASSES BACK A RETURN CODE = 1 IF THE USER WANTS*~
            *            TO SEE FULL USERS MANUAL, = 0 IF NOT.          *~
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

        sub "QUICKREF" (name$, returncode%)


            REM This subroutine will be changed later to be a general    ~
                purpose quick reference tool.  At that time the text to  ~
                be displayed will be in a data file which will be keyed  ~
                by NAME$.  For now it just displays the info about SES   ~
                shown in the ACCEPT statement below and NAME$ is not used

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L01928
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat    "
L01928: REM *************************************************************

L10000: accept                                                           ~
               at (01,03),                                               ~
        "        SYSTEM ELEMENT STRUCTURING -  Definition of Terms Used",~
               at (02,03),                                               ~
        "----------------------------------------------------------------~
        ~-------------",                                                  ~
               at (03,03),                                               ~
        "ELEMENT - The name of something as defined in the SES 'dictionar~
        ~y'.",                                                            ~
               at (04,03),                                               ~
        "   TYPE - Identifies the 'kind' of element to the 'dictionary'",~
               at (05,03),                                               ~
        "          (program, file, type, group, menu, procedure, field, e~
        ~tc.).",                                                          ~
               at (06,03),                                               ~
        "RELATION- An optional means of general classification for report~
        ~ing and",                                                        ~
               at (07,03),                                               ~
        "          listing convenience.  May be ignored if desired.",    ~
               at (08,03),                                               ~
        "----------------------------------------------------------------~
        ~-------------",                                                  ~
               at (09,03),                                               ~
        "STRUCTURE - An expression of how ELEMENTS are related one to ano~
        ~ther.  Much",                                                    ~
               at (10,03),                                               ~
        " (BOE)      like a Bill of Materials describes how each componen~
        ~t part is",                                                      ~
               at (11,03),                                               ~
        "            related to a given assembly.  An SES STRUCTURE could~
        ~ just as well",                                                  ~
               at (12,03),                                               ~
        "            be called a BILL OF ELEMENTS.",                     ~
               at (13,03),                                               ~
        "  LIBRARY - More than one BILL of Elements (STRUCTURE) can be de~
        ~fined for a",                                                    ~
               at (14,03),                                               ~
        "            given 'parent' ELEMENT.  Each STRUCTURE is assigned ~
        ~to a LIBRARY",                                                   ~
               at (15,03),                                               ~
        "            (or group or family) to distinguish one BOE from ano~
        ~ther.",                                                          ~
               at (16,03),                                               ~
        "  VERSION - Each BOE in a given LIBRARY can have different VERSI~
        ~ONS so if",                                                      ~
               at (17,03),                                               ~
        "            it changes through time, you can also see all previo~
        ~us VERSIONS.",                                                   ~
               at (18,03),                                               ~
        "In manufacturing terms, an ELEMENT is like a part.  It is of a t~
        ~ype called",                                                     ~
               at (19,03),                                               ~
        "'raw materials' and could also be classified as 'bar stock' (REL~
        ~ATIONSHIP).",                                                    ~
               at (20,03),                                               ~
        "That part is used in a BOM for an assembly (STRUCTURE or BOE).  ~
        ~The BOM for",                                                    ~
               at (21,03),                                               ~
        "the assembly could be different in different plants (LIBRARY).  ~
        ~In each plant",                                                  ~
               at (22,03),                                               ~
        "the BOM could have different 'revision levels' (VERSION or RELEA~
        ~SE VERSION).",                                                   ~
               at (23,03),                                               ~
        "----------------------------------------------------------------~
        ~-------------",                                                  ~
               at (24,03),                                               ~
        "Press: (13)to see a full users manual   (15)to print screen     ~
        ~(16)to return",                                                  ~
            keys(hex(0d0f10)), key(keyhit%)

            if keyhit% <> 15% then goto L10750
                call "PRNTSCRN"
                goto  L10000

L10750:     if keyhit% = 13% then returncode% = 1% else returncode% = 0%

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

