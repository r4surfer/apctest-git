        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            * ZZZZZZ                                                    *~
            *  PPPP   L       OOO   W   W   CCC    OOO   DDDD   EEEEE   *~
            *  P   P  L      O   O  W   W  C   C  O   O  D   D  E       *~
            *  PPPP   L      O   O  W   W  C      O   O  D   D  EEEE    *~
            *  P      L      O   O  W W W  C   C  O   O  D   D  E       *~
            *  P      LLLLL   OOO    W W    CCC    OOO   DDDD   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLOWCODE -  This subroutine is very similar to the GETCODE*~
            *             subroutine in that it is a combination verify *~
            *             and inquiry routine for most standard Caelus  *~
            *             files.  This routine differs primarily by     *~
            *             restricting the inquiry to a range of key     *~
            *             values (similiar to the PLOW logic) and by its*~
            *             use of sophisticated windowing techniques.    *~
            *                                                           *~
            * Example  -  PLOWKEY$ = "TYPE    " & TYPE$                 *~
            *             CALL "PLOWCODE" (#1, PLOWKEY$,                *~
            *                                  TYPE_DESCRIPTION$,       *~
            *                                  8%,                      *~
            *                                  1.5,                     *~
            *                                  F1%(1))                  *~
            *             IF F1%(1) > 0% THEN TYPE$ = STR(PLOWKEY$,9%)  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1984, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/07/84 ! ORIGINAL (and proud of it !)             ! LDJ *~
            * 04/11/85 ! Added Caller defined Message.            ! ERN *~
            * 12/03/85 ! Change to window if F1% negative         ! LDJ *~
            * 12/10/85 ! Bug fixes caused by 12/03 changes        ! LDJ *~
            * 04/08/86 ! Extensive changes-now looks like GETCODE ! LDJ *~
            *          !  Also added ability to PRINT listing &   !     *~
            *          !  did some general clean-up.              !     *~
            *          !  (Now supports duplicate keys, call to   !     *~
            *          !  MANUAL uses main program (caller) name, !     *~
            *          !  Key length & description positions now  !     *~
            *          !  float (supports key lengths of upto 75  !     *~
            *          !  characters BEFORE or AFTER the break    !     *~
            *          !  point), now deals with blank key values !     *~
            *          !  as follows;  if high level allowed to   !     *~
            *          !  explode downward, if low level and no   !     *~
            *          !  other non-blank partial (post break%)   !     *~
            *          !  keys returns with first record - no user!     *~
            *          !  display).  Probably added a bunch of    !     *~
            *          !  sneaky bugs with all this crap too.     !     *~
            *          !  P.S. You can also now inhibit redisplay !     *~
            *          !  of the prior screen on exit.            !     *~
            * 04/26/86 ! Even More Changes!  Added extra, optional! LDJ *~
            *          !  arguments after the F1% argument.       !     *~
            *          !  These arguments include Column Header   !     *~
            *          !  Line to display, Key Len after Break,   !     *~
            *          !  and some include/exclude parameters.    !     *~
            *          !  Note that these changes really bloated  !     *~
            *          !  this already fat (segment 1) routine.   !     *~
            * 06/11/86 ! Added Description Mapping Ability.       ! LDJ *~
            * 06/25/86 ! Bug Fixes (to as yet unreleased version.)! LDJ *~
            * 08/07/86 ! Minor Bug Fixes.                         ! LDJ *~
            * 09/24/86 ! Minor bug fix.  Did not use proper key   ! LDJ *~
            *          !   value to retrieve description from alt !     *~
            *          !   file when HIGH_LEVEL% = 1% and         !     *~
            *          !   BRK% < the key length of the alt file. !     *~
            * 09/26/86 ! Change for O.S. 7.10 incompatibility.    ! LDJ *~
            *          !   Call to SCREEN no longer supports      !     *~
            *          !   passing the Workstation UFB directly.  !     *~
            * 10/10/86 ! Added Report Writer Option - enhanced    ! LDJ *~
            *          !   Print Headings while at it.  Also      !     *~
            *          !   added range selection to Include /     !     *~
            *          !   Exclude arguments.                     !     *~
            * 10/22/86 ! Added the Description Overlay Option to  ! LDJ *~
            *          !   the DISPLAY$ argument (option "D").    !     *~
            *          !   Also added Key Path # for Alt File.    !     *~
            * 11/05/86 ! Added Description Search Functions       ! LDJ *~
            *          !   (ala GETCODE).                         !     *~
            * 01/09/87 ! Corrected minor 'anomaly' uncovered by   ! LDJ *~
            *          !   by HES (added lines 9481, 23541-23544).!     *~
            * 01/28/87 ! Another 'minor anomaly'.  If BREAK% and  ! LDJ *~
            *          !   KEY_DESCLEN both negative and the key  !     *~
            *          !   passed in is on file and the alternate !     *~
            *          !   File UFB is present the wrong key was  !     *~
            *          !   constructed to read the alt file with  !     *~
            *          !   (HIGH_LEVEL% wasn't set yet). 9076 - 77!     *~
            * 05/21/87 ! Added Hardcoded GLMAIN incld/Excd logic  ! HES *~
            * 06/03/87 ! Added "User Interrupt" Features.         ! LDJ *~
            * 10/26/87 ! Now correctly displays 12 char GL acct   ! HES *~
            * 11/25/87 ! Added 'd' option to DISPLAY argument.    ! LDJ *~
            *          !   (corrected minor bug - if passed descr !     *~
            *          !   len = 0 but high level is enabled & a  !     *~
            *          !   a len for the high level descr was     !     *~
            *          !   given then had invalid string pos on a !     *~
            *          !   Word Search or Sounds Like operation.) !     *~
            * 05/02/88 ! Bug Fix - possible illegal string operand! LDJ *~
            *          !   line 13190 - added line 13186. deffn'77!     *~
            * 09/29/88 ! Expanded max print line length to 160.   ! LDJ *~
            *          !   minor change to prompt text logic.     !     *~
            * 04/19/89 ! O.S. 7.20/30 Comapatability (WSXIO).     ! KAB *~
            * 05/11/89 ! Expanded HEADER$ to 5 elements - 4th/5th ! MJB *~
            *          !  used as 2nd & 3rd title line on report  !     *~
            * 01/23/92 ! Here under duress.  Remove $UNPACK stmt. ! KAB *~
            *          ! Fixed up extra column in report routine. !     *~
            *          ! Provide support for selection on         !     *~
            *          ! key path with duplicates allowed.        !     *~
            * 01/23/92 ! Slight Modification. If primary and      ! KAB *~
            *          !   Secondary Files are the Same, Second   !     *~
            *          !   is regarded as a DUMMY (Same as AUTHOR)!     *~
            * 08/02/93 ! UNIX mod for screen printing.            ! JDH *~
            * 02/11/94 ! Re-introduced lost Century Date support  ! LDJ *~
            *          ! Changed floating pt constants to integers!     *~
            *          ! Made change to fix minor bug with anti-  !     *~
            *          ! plow in some situations (PRR w/no number)!     *~
            *          ! Attempted to speed up Include/Exclude    !     *~
            *          ! logic as well as DESCR_MAP routine.      !     *~
            * 06/20/94 ! Platform Sensitive for Keyboard Interrupt! KAB *~
            * 11/17/95 ! GUIizing changes - CoStar                ! LDJ *~
            * 03/28/97 ! If GUI Mode always force window size to  ! LDJ *~
            *          ! Full screen.  Also corrected issue with  ! LDJ *~
            *          ! GENCODES when displaying tables only -   !     *~
            *          ! problem appears in GENCDSIN.             !     *~
            *          ! GUI ListBox now used for any simple query!     *~
            *          ! against a file with < n records (and     !     *~
            *          ! still used for all GENCODES queries). No !     *~
            *          ! longer uses WSXIO for listbox -now GETCMD!     *~
            * 06/11/97 ! Add (F4)Previous Screen functionality    ! RJH *~
            * 06/20/97 ! Fix old Bug (see DEFFN'101)              ! LDJ *~
            *************************************************************


            sub "ZPLOWCOD" (#1,          /* UFB of File to Search      */~
                                                                         ~
                 key$,        /* Coming In: Alternate or Primary Key of*/~
                              /*            record to search for.      */~
                              /* Going Out: Alternate or Primary Key   */~
                              /*            Passed in if found or if no*/~
                              /*            selection made.            */~
                              /*            Appropriate key of selected*/~
                              /*            record if selection made.  */~
                                                                         ~
                 descr$,      /* IN: If first character = HEX(06) used */~
                              /*     as message in screen title area.  */~
                              /* OUT:                                  */~
                              /*     Selected Record description field */~
                              /*     going out. The description field  */~
                              /*     is usually assumed to immediately */~
                              /*     follow the primary key in the     */~
                              /*     record per standard Caelus design.*/~
                                                                         ~
                break%,       /* Control Break.  Only those records    */~
                              /* which equal the first BREAK%          */~
                              /* characters of the passed key value    */~
                              /* will be retrieved for inquiry         */~
                              /* selection. (Break point=0% means all  */~
                              /* records in file are eligible for      */~
                              /* retrieval). If BREAK% < 0 then the    */~
                              /* assumption is that only the key values*/~
                              /* which differ in the first BREAK%      */~
                              /* characters will be eligible for test/ */~
                              /* display/carry-back. This option must  */~
                              /* be accompanied by a negative          */~
                              /* KEY_DESCLEN as described below.       */~
                              /*                                       */~
                              /* If ABS(BREAK%) > 999% then additional */~
                              /* arguments are indicated by the        */~
                              /* thousands  digit, i.e.                */~
                              /* EXTRA_ARGS% = BREAK%/1000%.           */~
                                                                         ~
                key_desclen,  /* Floating Point where the Integer      */~
                              /* portion is the KEY number to be used  */~
                              /* (0-16). This number indicates if the  */~
                              /* key is a primary (0) or an alternate  */~
                              /* (>0). If an alt key is specified the  */~
                              /* file must already have an existing    */~
                              /* keypath for that number.              */~
                              /*                                       */~
                              /* The fraction or portion to the right  */~
                              /* of the decimal is assumed to be the   */~
                              /* length of the description field in the*/~
                              /* record.  For example:                 */~
                              /*   .30 or .3 means length 30           */~
                              /*   .03 means length 3, etc.            */~
                              /* A length of 0 (.0) is assumed to mean */~
                              /* there is no description & consequently*/~
                              /* one will not be displayed or returned.*/~
                              /*                                       */~
                              /* If this argument is negative AND      */~
                              /* BREAK% is <> 0% then an optional      */~
                              /* feature is enabled which allows       */~
                              /* plowing the values BEFORE the break   */~
                              /* only, i.e. show only values which     */~
                              /* values which differ in the first      */~
                              /* BREAK% characters.  When in this mode */~
                              /* if the user selects an entry and      */~
                              /* BREAK% > 0 then the routine reverts to*/~
                              /* the primary mode, exploding all file  */~
                              /* entries which have the same BREAK%    */~
                              /* number of characters as the selected  */~
                              /* value.  If BREAK% < 0 then the entry  */~
                              /* selected will be returned to the      */~
                              /* calling program.  Confused ?  Try it. */~
                                                                         ~
               f1%,           /* Return Status for Call.               */~
                              /* 1 = Record Found/Selected.            */~
                              /* 0 = Record not on file OR file not    */~
                              /*     open OR no selection made OR      */~
                              /*     invalid Call argument.            */~
                              /* When passed in if F1% is < 0 then     */~
                              /* ABS(F1%) = row nbr to open window     */~
                              /* above/below (if in CHUI mode).        */~
                                                                         ~
               columnhdr$(),  /* Column Header to Replace the one      */~
                              /* supplied by PLOWCODE.  This is an     */~
                              /* optional argument which must be       */~
                              /* present if ABS(BREAK%) >= 1000%.      */~
                              /* If present but blank the standard     */~
                              /* PLOWCODE header is used. Argument must*/~
                              /* be a 2 or 3 element array.  If present*/~
                              /* the second element will override the  */~
                              /* 1st one at higher level (pre-break)   */~
                              /* display screen only. If present       */~
                              /* element 3 will replace the normal text*/~
                              /* displayed by PLOWCODE on Line 1 of the*/~
                              /* screen (if partial screen not         */~
                              /* displayed). Since this is an array    */~
                              /* literal constants may NOT be used in  */~
                              /* the calling syntax.                   */~
                              /*  The 4th & 5th element if present,    */~
                              /* will be used as the second line of a  */~
                              /* 2 or 3 column title on a report.      */~
                                                                         ~
               length,        /* Length of Key to Display following    */~
                              /* BREAK%.  This is an optional argument */~
                              /* which must be present if ABS(BREAK%)  */~
                              /* >= 2000%. Format is  LL.DD  where LL  */~
                              /* is the Length of Key to Display       */~
                              /* following BREAK%.  If LL is 0 then the*/~
                              /* entire keylength after BREAK% will be */~
                              /* displayed.  DD may be used to override*/~
                              /* the description length when displaying*/~
                              /* key values after the break & BREAK% is*/~
                              /* positive and KEY_DESCLEN is negative. */~
                                                                         ~
               descr_pos,     /* Starting Position of description field*/~
                              /* if not immediately following prime key*/~
                              /* in record.  This is an optional       */~
                              /* argument which must be present if     */~
                              /* ABS(BREAK%) >= 3000%.  Format is      */~
                              /* + or - PPPP.AAAAK .   If PPPP is zero */~
                              /* then the position of the description  */~
                              /* field is assumed to be immediately    */~
                              /* following the primary key in the      */~
                              /* record.  The integer portion of the   */~
                              /* number (PPPP) defines the description */~
                              /* position for the primary file (arg 1) */~
                              /* if non-zero.                          */~
                              /* The fraction or portion to the right  */~
                              /* of the decimal (AAAA) defines the     */~
                              /* description position in the alternate */~
                              /* file (arg 14) if non-zero. The        */~
                              /* fractional portion must be expressed  */~
                              /* as ten-thousandths; e.g. .0001 is     */~
                              /* is position 1 in the alt file.  If the*/~
                              /* sign of this argument is negative it  */~
                              /* is assumed that PPPP is also the      */~
                              /* field to use as the Key to Read the   */~
                              /* specified alt file (argument 14) with.*/~
                              /* If arg 14 is present K is the key path*/~
                              /* to read the alternate file by.        */~
                                                                         ~
               incl_excl(),   /* Include / Exclude flag(s).            */~
                              /* This is an optional argument which    */~
                              /* must be present if ABS(BREAK%)>=5000%.*/~
                              /* The format of this field is PP.LLF    */~
                              /* where PP is position in the record    */~
                              /* area  LL is the length of a data field*/~
                              /* to compare against the following      */~
                              /* argument to determine whether or not  */~
                              /* to include or exclude records in the  */~
                              /* display, and F is 0 to test using the */~
                              /* primary file's record area or 1 to    */~
                              /* test using alternate file 1 record    */~
                              /* area.  If the sign of the field is    */~
                              /* positive then INCLUDE is assumed.  If */~
                              /* negative the EXCLUDE is assumed.  If  */~
                              /* zero then this and the corresponding  */~
                              /* element in the next argument are      */~
                              /* ignored.  This argument is an array so*/~
                              /* that multiple include/exclude         */~
                              /* arguments may be passed.  The number  */~
                              /* of elements in this array (minimum of */~
                              /* 1) must equal the number of elements  */~
                              /* in the following argument!            */~
                              /* Boolean AND logic is applied to       */~
                              /* multiple tests (more than 1 element)  */~
                              /* if applicable.  In other words all    */~
                              /* conditions must be true for the record*/~
                              /* to be eligible for display/carryback. */~
                              /* The one exception is multiple tests   */~
                              /* against the same field. These tests   */~
                              /* are treated as logical ORs enclosed   */~
                              /* within parentheses with the resulting */~
                              /* expression ANDed with the remainder of*/~
                              /* the test elements. (I realize the     */~
                              /* above is confusing if you've forgotten*/~
                              /* your high school truth tables).       */~
                              /* Since this is an array literal        */~
                              /* constants may NOT be used in the      */~
                              /* calling syntax.                       */~
                                                                         ~
               incl_excl$(),  /* Include / Exclude Value(s).           */~
                              /* This is an optional argument which    */~
                              /* must be present if ABS(BREAK%)>=5000%.*/~
                              /* The value in each element of this     */~
                              /* array is compared against a field in  */~
                              /* the record area (defined by           */~
                              /* INCL_EXCL()) to determine whether or  */~
                              /* not the record is eligible for        */~
                              /* display.  Since this is an array      */~
                              /* literal constants may NOT be used in  */~
                              /* the calling syntax.                   */~
                              /* Ranges may be tested (Include only    */~
                              /* records within the range or Exclude   */~
                              /* records within the range) by using the*/~
                              /* "<" character in the first position   */~
                              /* followed by the ending range value    */~
                              /* optionally followed by a ">" and the  */~
                              /* beginning range value.  Range tests   */~
                              /* are inclusive of the test values.     */~
                                                                         ~
               display$,      /* ReDisplay Original Screen on Exit flag*/~
                              /* -or- Generate Report Only Flag.       */~
                              /* This is an optional argument which    */~
                              /* must be present if ABS(BREAK%)>=6000%.*/~
                              /* If > " " (and NOT equal to "d")       */~
                              /* then this is a flag to PLOWCODE       */~
                              /* telling him to not redisplay the      */~
                              /* original screen when returning to the */~
                              /* calling program.  It also has the     */~
                              /* effect of suppressing the Screen      */~
                              /* prompts for the Window Manipulation   */~
                              /* PF Keys.  "Y" is the expected value   */~
                              /* to use if none of the other possible  */~
                              /* flag values are used but display      */~
                              /* suppression is desired.               */~
                              /* If = "D" or = "d" these are flags     */~
                              /* to display only the description field */~
                              /* on the screen > i.e. overlay the key/ */~
                              /* code area.  Has the same effect as    */~
                              /* little "r" below on printed reports.  */~
                              /* The difference between "D" & "d" is   */~
                              /* that "D" suppresses the Windowing PF  */~
                              /* Keys and ReDisplay of the original    */~
                              /* screen on exit.  "d" does NOT suppress*/~
                              /* these functions.                      */~
                              /* If = "R" this is a flag to PLOWCODE   */~
                              /* to not display any screens or perform */~
                              /* any key validation.  Instead jump     */~
                              /* straight into the Report Generation   */~
                              /* routine, generate a report using the  */~
                              /* "normal" parameters to control whats  */~
                              /* reported and what it looks like (e.g. */~
                              /* control breaks & screen mapping also  */~
                              /* apply to report generation).  Return  */~
                              /* to caller when report finished -      */~
                              /* F1% = 0% if nothing printed,          */~
                              /* F1% = 1% if report printed.           */~
                              /* If = "r" (little r) then same as Big  */~
                              /* R option above except key values not  */~
                              /* printed on report, only description   */~
                              /* area (may be mapped by DESCR_MAP()    */~
                              /* below to include key values).         */~
                              /* Leave blank to let normal processing  */~
                              /* occur.                                */~
                                                                         ~
               suppress$,     /* Suppress message "...return with that */~
                              /* code." flag.                          */~
                              /* This is an optional argument which    */~
                              /* must be present if ABS(BREAK%)>=7000%.*/~
                              /* If = "Y" this is a flag to PLOWCODE   */~
                              /* telling him to not to display the     */~
                              /* above.  Leave blank to let normal     */~
                              /* processing occur.                     */~
                                                                         ~
               #3,            /* Alternate File Channel to Read Key    */~
                              /* Description from. This is an optional */~
                              /* argument which must be present if     */~
                              /* ABS(BREAK%) >= 8000%. This argument is*/~
                              /* ignored if the file associated with   */~
                              /* this UFB is not open (e.g. dummy      */~
                              /* select statement). Otherwise PLOWCODE */~
                              /* will use the current displayable code */~
                              /* (key value) as the key to this file & */~
                              /* attempt to read & get a description   */~
                              /* from this file.                       */~
                                                                         ~
               descr_map())   /* Description Field Mapping.            */~
                              /* This is an optional argument which    */~
                              /* must be present if ABS(BREAK%)>=9000%.*/~
                              /* This argument is an N element array   */~
                              /* (as determined by the caller) which   */~
                              /* uses pairs of elements. Therefore the */~
                              /* array should never have an odd number */~
                              /* of elements.  Each pair of elements is*/~
                              /* used to map data fields from the      */~
                              /* record area into the screen /         */~
                              /* description area. The first element in*/~
                              /* each pair describes the source field  */~
                              /* from the record area. If the value of */~
                              /* this element is zero it will be       */~
                              /* ignored. The format of this element   */~
                              /* is PP.LLF where PP is position in the */~
                              /* record area, LL is the length of a    */~
                              /* data field, and F is a format flag as */~
                              /* follows: 0 = no special formatting    */~
                              /*          1 = change to 8 char date fmt*/~
                              /*          2 = change to 8 char time fmt*/~
                              /* If the sign of the first element      */~
                              /* in a pair is negative then the        */~
                              /* record area from the alternate file   */~
                              /* channel will be used (if available).  */~
                              /* The second element in each pair       */~
                              /* describes the output position in the  */~
                              /* screen/description field where the    */~
                              /* format is +/-OPPP.LLDM. Where the Sign*/~
                              /* of the element controls the field     */~
                              /* 'Type', PPP is the output position    */~
                              /* in the description area, LL is the    */~
                              /* output length (if PD or BI), D is     */~
                              /* the number of decimal places if the   */~
                              /* 'Type' is PD - Packed Decimal, and    */~
                              /* M is the minimum number of decimal    */~
                              /* places to display/print if the 'Type' */~
                              /* is PD with the default being zero.    */~
                              /* 'O' is the Output Line to Display /   */~
                              /* Print with 0 the default for line 1,  */~
                              /* 1 = line 2, and 2 = line 3 (maximum is*/~
                              /* 3 per record).                        */~
                              /* The value of LL * the Sign of this    */~
                              /* element determines the field type as  */~
                              /* follows:                              */~
                              /*   If 0 then this is a character or    */~
                              /*     'string' field.  Output length =  */~
                              /*     the input length.                 */~
                              /*   If > 0 then this is a PD or packed  */~
                              /*     decimal field whose output length */~
                              /*     equals the value of the decimal   */~
                              /*     portion (LL) and the internal     */~
                              /*     (max) number of decimal places =D.*/~
                              /*     The minimum number of decimal     */~
                              /*     places to display/print is        */~
                              /*     determined by M (default = 0).    */~
                              /*   If < 0 then this is a BI or integer */~
                              /*     field whose output length equals  */~
                              /*     the value of the decimal portion  */~
                              /*     (LL).                             */

        REM *************************************************************~
            *                  PLOWCODE Variables                       *~
            *************************************************************

        dim                              /* Miscellaneous Variables    */~
            aid$1,                       /* Workstation AID Byte (PFK) */~
            answer$60,                   /* Input Receiver - Find      */~
            codelen$2,                   /* GENCODES Code Field length */~
            column$(5)160,               /* Optional Column Headers    */~
            columnhdr$(1)1,              /* Optional Column Headers    */~
            command$256,                 /* SENDCMD argument           */~
            company$60,                  /* Company Name Field         */~
            date$8,                      /* System Date                */~
            des$(3)160,                  /* Work Area Description field*/~
            descr_map(2),                /* Description Mapping args   */~
            disp$1,                      /* Redisplay Original Screen? */~
            display$1,                   /* Redisplay Original Screen? */~
            eof$1,                       /* End Of File Flag           */~
            f1%(3),                      /* File Status Variable       */~
            filekey$200,                 /* Key for Read               */~
            findkey$200,                 /* Temp Work Key              */~
            incl_excl(1),                /* Optional Inc/Exc Flags     */~
            incl_excl$(1)1,              /* Optional Inc/Exc Flags     */~
            iosw_receiver$8,             /* IO Word Status Bytes       */~
            key1$200,                    /* KEY$ Contents Modified     */~
            key1$(20)200, keyp$(20)200,  /* Full Key Values of Selectns*/~
            no_display$1,                /* Display Screen on Exit Flag*/~
            ok$1,                        /* Record OK to Display/Print?*/~
            p%(2),                       /* Search Receiver Array      */~
            plowkey$200,                 /* Miscellaneous Plow & Read  */~
            print$160,                   /* Print Record               */~
            prname$8,                    /* Parameter Reference Name   */~
            prname2$8,                   /* Parameter Reference Name   */~
            program$8,                   /* Program Name for MANUAL    */~
            readkey$200,                 /* Key for Plow               */~
            report_option$1,             /* 'R' or 'r' if report only  */~
            s1$(24)80,                   /* Prior Screen Contents      */~
            soundex1$4,                  /* Soundex Code Searching For */~
            soundex2$4,                  /* Soundex Code To Check      */~
            suppress$1,                  /* Supress Message ?          */~
            supres$1,                    /* Supress Message ?          */~
            temp$256,                    /* Temporary Work Variable    */~
            ufbkl$1,                     /* Key Length this File       */~
            ufbkd$2,                     /* Key Displacement Rel to 0  */~
            uw$1,                        /* CoStar MAGIC Char - x7F    */~
            work$(8)256                  /* Work Area for Transfer     */~

        dim     /* These Variables should not be disturbed or moved !  */~
            od$(1)4,                     /* Screen Order Area          */~
            s$(24)80                     /* Current Screen Text Array  */

        dim     /* Passed Arguments                                    */~
            descr$100,                   /* Returned Description       */~
            key$200                      /* Read Key                   */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            select #5,  "CRTFILE", consec, recsize = 1924

            select #2, "PRINTER",  printer, recsize = 162

        REM *************************************************************~
            *     Check to See If the File is Open on this Channel      *~
            *              If Not Exit Immediately !                    *~
            ************************************************************
            call "CHECKGUI" addr(gui%) : uw$=hex(7f)
            if gui% = 1% then f1% = 0%  /* don't allow windows in GUI  */
            if f1% < 0% then scrrow% = abs(f1%) else scrrow% = 0%
            f1% = 0%
            call "GETUFBS1" addr(#1, ufbf1%)
                 if ufbf1% = 0% then exit_routine
                      /* If UFBF1 Flag last Bit = 1 then file is Open  */


        REM *************************************************************~
            *       Get or calculate the file parameters needed         *~
            *      (Key #, key length, position, description length).   *~
            *************************************************************
            key1$ = key$
            args% = abs(break%) / 1000%
            if break% < 0% and key_desclen < 0 then high_level% = 1%     ~
                                               else high_level% = 0%
            break% = mod(abs(break%),1000%) * sgn(break%)
            brk% = break% :  break% = abs(break%)
            init (hex(00)) ufbkl$, ufbkd$
            call "GETUFBKL" addr(#1, ufbkl$)       /* Key Length (BIN) */
            call "GETUFBDK" addr(#1, ufbkd$)       /* Key Displ. (BIN) */
            disp% = 1+val(ufbkl$)+val(ufbkd$,2)
            key%    = int(abs(key_desclen))
            deslen% = mod(abs(key_desclen)*100,100)
            if args% < 3% then L09145
               if int(descr_pos) = 0% then L09145
               disp% = abs(descr_pos)              /*Primary File Displ*/
L09145:     call "GETPRNAM" addr(#1, prname$)
            fs3%, max_len% = 0% : prname2$ = " "
            if args% > 5% then no_display$, disp$ = display$             ~
                           else no_display$, disp$ = " "
            report_option$=no_display$ : tran(no_display$,"DdRr")replacing
            if args% < 5% then L09360
               if dim(incl_excl(),1) < 2% then L09240
               REM *** Do Quick & Dirty Shell Sort on INCL_EXCL Array ***
               for i% = 2% to dim(incl_excl(),1)
                   if abs(incl_excl(i%))>=abs(incl_excl(i%-1%)) then L09230
                   for j% = 1% to i%-1%
                       if abs(incl_excl(j%))<abs(incl_excl(i%)) then L09225
                       temp = incl_excl(j%) : work$(8%) = incl_excl$(j%)
                       incl_excl(j%) = incl_excl(i%)
                       incl_excl$(j%) = incl_excl$(i%)
                       incl_excl(i%) = temp : incl_excl$(i%) = work$(8%)
L09225:            next j%
L09230:        next i%
               REM *** End Shell Sort ***
L09240:     if args% < 8% then L09360
               call "CHKFLS2" addr(#1, #3, fs3%)
               if fs3% = 0% then L09295
               init (hex(00)) ufbkl$, ufbkd$
               call "GETUFBKL" addr(#3, ufbkl$)    /* Key Length (BIN) */
               call "GETUFBDK" addr(#3, ufbkd$)    /* Key Displ. (BIN) */
               disp2% = 1%+val(ufbkl$)+val(ufbkd$,2)
               call "GETPRNAM" addr(#3, prname2$)
               key3%  = mod(abs(descr_pos)*100000,10)
               if int(descr_pos) = descr_pos then L09295
               disp2% = mod(abs(descr_pos)*10000,10000)
L09295:     REM *** Squeeze zero entries to end of DESCR_MAP()
            if args% < 9% then L09360
            dm% = dim(descr_map(),1) : if dm% < 4% then L09360
            for i% = 1% to dm% - 1% step 2%
                if descr_map(i%) <> 0 then L09355
                if i% > dm% - 3% then L09355
                for j% = i% to dm% - 3% step 2%
                    u% = j%+2%
L09330:             if descr_map(u%) <> 0 then L09335
                       if u% >= dm% - 1% then L09350
                       u% = u% + 2%
                       goto L09330
L09335:             descr_map(j%)    = descr_map(u%)
                    descr_map(j%+1%) = descr_map(u%+1%)
                    descr_map(u%), descr_map(u%+1%) = 0
L09350:         next j%
L09355:     next i%
L09360: REM *************************************************************~
            *      Does the passed key exist on file as is ?            *~
            *If it's on file, then get right back with minimum effort.  *~
            *************************************************************
            if prname$ <> "GLMAIN" then L09405 /* Special GLMAIN Handling*/
               plowkey$ = " "
               call "GLVALID" (key1$,"         ", plowkey$)
               call "GLUNFMT" (key1$)
               if plowkey$ <> " " then key1$ = " "
L09405:     plowkey$ = key1$
            if args% > 1% then dlen% = min(77%,abs(length)) else dlen%=0%
            if args% > 1% then deslen1% = mod(abs(length)*100,100)       ~
                          else deslen1% = deslen%
            if deslen1% = 0% then deslen1% = deslen%
            if no_display$ = "R" then L09540
            if dlen% = 0% or brk% < 0% then L09455
               str(plowkey$,break%+dlen%+1%) = all(hex(00))
L09445:        call "PLOWALTS" (#1, plowkey$, key%, break%+dlen%, f1%)
               goto L09475
L09455:     str(plowkey$,break%+1%) = all(hex(00))
L09460:     if brk% >= 0% or break% = len(str(key(#1,key%))) then        ~
                               call "REDALT0" (#1, key1$, key%, f1%)     ~
            else call "PLOWALTS" (#1, plowkey$, key%, break%, f1%)
L09475:     if f1% = 0 then L09540
               keylen% = len(str(key(#1,key%)))
               if dlen% = 0% then displen% = keylen%-break%              ~
                             else displen% = dlen%
L09495:        gosub include_exclude_test
               if ok$ = "Y" then L09515
                  if brk% >= 0% and dlen% = 0% then L09540
                  on sgn(dlen%) + 1% goto L09460,L09445
L09515:        key1$ = key(#1,key%)
               gosub'88(key1$)
               if deslen% > 0% then descr$ = des$(1%)
               goto exit_routine

L09540: REM *************************************************************~
            * Make sure that there's records on file within the given   *~
            * range, or if KEY_DESCLEN < 0 within the entire file.      *~
            *************************************************************
            plowkey$ = key1$
            call "PLOWALTS" (#1, plowkey$, key%, break%, f1%)
            eof$ = "N"
            if f1% = 1% then L09605
L09580:     eof$ = "Y"
            str(plowkey$,break%+1%) = all(hex(00))
L09590:     call "PLOWALTS" (#1, plowkey$, key%, break%, f1%)
            if f1% = 0% and eof$ <> "Y" then L09580
            if f1% = 0% then L09635
L09605:        keylen% = len(str(key(#1,key%)))
               if dlen% = 0% then displen% = keylen%-break%              ~
                             else displen% = dlen%
               gosub include_exclude_test
               if ok$ = "Y" then L09655
               goto L09590
L09635:     if key_desclen >=0 then exit_routine
            plowkey$ = all(hex(00))
            call "PLOWALTS" (#1, plowkey$, key%, 0%, f1%)
            if f1% = 0% then exit_routine
L09655:     keylen% = len(str(key(#1,key%)))
            if dlen% = 0% then displen% = keylen%-break%                 ~
                          else displen% = dlen%
            if prname$ = "GLMAIN" then keylen% = 12%
            if dlen% + abs(brk%) >= keylen% then dlen%=0% /* Dummy Test*/
            if high_level% = 1% and break% = keylen% then L09735
            REM *** Blank Keys Test ***
L09690:     if str(plowkey$,break%+1%) <> " " then L09735
            call "PLOWALTS" (#1, plowkey$, key%, break%, f1%)
            if f1% = 1% then L09690
            plowkey$ = key1$
            str(plowkey$,break%+1%) = all(hex(00))
            call "PLOWALTS" (#1, plowkey$, key%, break%, f1%)
            if f1% = 1% then L09495
            goto exit_routine

L09735:     if eof$ = "N" or key_desclen < 0 then L09770
            if break% = 0% then L09750
            if str(key1$,,break%) = str(plowkey$,,break%) then L09770
L09750:     key1$ = plowkey$
            if brk% < 0% or dlen% > 0% then                              ~
               str(key1$,break%+1%) = all(hex(00))

L09770: REM *************************************************************~
            * Perform Initialization Routine Once & Once Only.          *~
            *************************************************************
            gosub initialize
            if no_display$ = "R" then print_report

        REM *************************************************************~
            *           M A I N   P R O G R A M   B E G I N S           *~
            *************************************************************

        main_control
            gosub write_screen
            gosub read_screen
            on keyhit% + 1% gosub check_it,        /* PF 0             */~
                                  higher_level,    /* PF 1             */~
                                  first_screen,    /* PF 2             */~
                                  ,                /* PF 3             */~
                                  prev_screen,     /* PF 4             */~
                                  next_screen_page,/* PF 5             */~
                                  find_soundex,    /* PF 6             */~
                                  next_line,       /* PF 7             */~
                                  find_code,       /* PF 8             */~
                                  find_word,       /* PF 9             */~
                                  shrink_up,       /* PF 10            */~
                                  expand_down,     /* PF 11            */~
                                  window_flip,     /* PF 12            */~
                                  documentation,   /* PF 13            */~
                                  ,                /* PF 14            */~
                                  print_screen,    /* PF 15            */~
                                  quit,            /* PF 16            */~
                                  ,                /* PF 17            */~
                                  ,                /* PF 18            */~
                                  ,                /* PF 19            */~
                                  ,                /* PF 20            */~
                                  ,                /* PF 21            */~
                                  ,                /* PF 22            */~
                                  ,                /* PF 23            */~
                                  ,                /* PF 24            */~
                                  ,                /* PF 25            */~
                                  shrink_down,     /* PF 26            */~
                                  expand_up,       /* PF 27            */~
                                  ,                /* PF 28            */~
                                  ,                /* PF 29            */~
                                  ,                /* PF 30            */~
                                  print_file,      /* PF 31            */~
                                  quit             /* PF 32            */

            goto main_control

        check_it
            if r% < fst_cde_line% or r% > lst_cde_line% then return
            readkey$ = key1$(r%)
            if prname$ = "GLMAIN" then call "GLUNFMT" (readkey$)
            if high_level% = 1% and brk% > 0% then L11380

            if high_level% = 1% or dlen% = 0% then L11080

               str(readkey$,break%+dlen%+1%) =all(hex(00))
                   goto check_specific
L11040:        call "PLOWALTS" (#1, readkey$, key%,break%+dlen%, f1%)
               goto L11176

L11080:     if brk% < 0% then str(readkey$,abs(brk%)+1%) =all(hex(00))
               goto check_specific
L11100:     if brk% < 0% and keylen% > abs(brk%) then                    ~
                 call "PLOWALTS" (#1, readkey$, key%, abs(brk%), f1%)    ~
            else call "REDALT0" (#1, readkey$, key%, f1%)
               goto L11176

        check_specific
            call "REDALT0" (#1, keyp$(r%), 0%, f1%)
L11176:        if f1% = 0% then return

            key1$ = key(#1,key%)
            gosub include_exclude_test
            if ok$ = "Y" then L11260
               if high_level% = 1% then L11100
               if dlen% > 0% then L11040
               key1$(lst_cde_line%) = key1$(fst_cde_line%)
               keyp$(lst_cde_line%) = keyp$(fst_cde_line%)
               gosub next_screen_page
               return

L11260:     gosub'88(key1$)
            descr$ = des$(1%)
            goto quit

L11380:     str(readkey$,save_break%+1%) = all(hex(00))
            call "PLOWALTS" (#1, readkey$, key%, save_break%, f1%)
                f1%(1%) = f1%
                if f1% = 0% then L11520       /* NONE, JUST RESET       */
L11440:     if str(readkey$,save_break%+1%) <> " " then L11600
            call "PLOWALTS" (#1, readkey$, key%, save_break%, f1%)
               if f1% <> 0% then L11440
                  f1%  = 1%                  /* THERE IS AT LEAST ONE */

L11520:     break% = save_break%
            displen%=max(dlen%,abs(sgn(dlen%-1%))*(keylen%-break%))
            if high_level% = 1% then displen% = break%
            if f1% = 0% then return else check_specific

L11600:     break% = save_break%
            key1$ = str(readkey$,,break%)
            high_level% = 0% : f1% = 0%
            if dlen%=0% then displen%=keylen%-break% else displen%=dlen%
            gosub'106(r0%)
            gosub first_screen
            return

        higher_level
            if key_desclen >= 0 then return
            if break% < 1% then return
            high_level% = 1% : str(plowkey$,break%+1%) = all(hex(00))
            mode%, break% = 0% : displen% = save_break% : f1%(1%) = 1%
            gosub'106(r0%)
            gosub next_screen
            return

        first_screen
            plowkey$=key1$
            str(plowkey$,break%+1%) = all(hex(00))
            gosub'101(fst_cde_line%,lst_cde_line%)
            gosub'105(lst_wnd_line%)
            r% = fst_cde_line% : c% = 2%
            return

        next_screen_page
            if f1%(1%) = 0% then return
            for x% = lst_cde_line% to fst_cde_line% step -1%
                plowkey$ = key1$(x%)
                if pos(str(plowkey$,break%+1%,displen%) > hex(00)) > 0%  ~
                   then x% = 0%
            next x%
            if prname$ <> "GLMAIN" then L12190
               call "GLUNFMT" (plowkey$)
               str(plowkey$,break%+1%,9%) = addc all(hex(ff))
               goto next_screen
L12190:     if high_level% = 1% then                                     ~
               str(plowkey$,save_break%+1%) = all(hex(00))               ~
            else                                                         ~
               str(plowkey$,break%+dlen%+1%,displen%) = addc all(hex(ff))

        next_screen
            if f1%(1%) = 0% then return
            gosub'101 (fst_cde_line%,    /* First Row to Replace       */~
                       lst_cde_line%) /* Last Row to Replace        */
            r% = fst_cde_line% : c% = 2%
L12260:     gosub'105(lst_wnd_line%)
            return

        Prev_screen  /* read backwards thru file */
           stop% = 2% * (lst_cde_line% - fst_cde_line%)
           for k% = 1% to stop%    /* Backup 2 screen fulls */
             read_back:
               read #01, key key%  < key(#01, key%), eod goto ps_done
               plowkey$ = key(#01, key%)
               gosub include_exclude_test
                  if ok$ <> "Y" then read_back /* back some more */
           next k%

        ps_done
            if prname$ <> "GLMAIN" then L12300
               call "GLUNFMT" (plowkey$)
               str(plowkey$,break%+1%,9%) = addc all(hex(ff))
               goto next_screen
L12300:     if high_level% = 1% then                                     ~
               str(plowkey$,save_break%+1%) = all(hex(00))               ~
            else                                                         ~
               str(plowkey$,break%+dlen%+1%,displen%) = addc all(hex(ff))

           f1%(1%) = 1%
           gosub next_screen
           return

        find_soundex
            if high_level% = 0% then len% = min(50%, deslen%)
            if high_level% = 1% then len% = min(50%, deslen1%)
            if len% < 1% then return
            gosub'77(18%, hex(a4) & "Spell Out What The " &              ~
             "Description Sounds Like, Then Press (RETURN)",             ~
            hex(8c) & "(6)Sounds Like:" & hex(80))
            if keyhit% = 1% then return
            call "SOUNDEX" (answer$, soundex1$)
            answer$ = or all(hex(20)) : f1%(1%) = 1% : mode% = 1%
            gosub keyboard_enable
            gosub first_screen
            return

        next_line
            enabled_flag% = 0% : reads% = 1%
L12420:     if f1%(1%) = 0% or reads% = 0% then L12260
            if r9% < 9% then L12470
            str(s$(),(fst_cde_line%-1%)*80%+1%,(r9%-8%)*80%) =           ~
              str(s$(),(fst_cde_line%)*80%+1%,(r9%-8%)*80%)
            str(key1$(),1%) = str(key1$(),201%)
            str(keyp$(),1%) = str(keyp$(),201%)
L12470:     gosub'101(lst_cde_line%,lst_cde_line%)
            r% = r% - 1%
            if r% < fst_cde_line% then r% = fst_cde_line%
            gosub interrupt_check
            if aid$ <> hex(00) then L12260
            gosub write_screen
            goto L12420

        find_code
            len% = displen% : if prname$ = "GLMAIN" then len% = 12%
            gosub'77(22%, hex(a4) &                                      ~
            "Enter As Much Of The Code As Is Known, Then Press (RETURN)",~
            hex(8c) & "(8)Search For Code:" & hex(81))
            if keyhit% = 1% then return
            if prname$ <> "GLMAIN" then L13025
               temp$ = answer$
               call "GLVALID" (temp$, answer$, iosw_receiver$)
               str(plowkey$,break%+1%,9%) = answer$ addc all(hex(ff))
               goto L13030
L13025:     str(plowkey$,break%+1%,len%) = answer$ addc all(hex(ff))
L13030:     f1%(1%) = 1% : mode% = 0% : nonext% = 1%
            gosub keyboard_enable
            gosub next_screen
            return

        keyboard_enable
            if no_display$ = "R" then return
            if keyhit% <> 2% and keyhit% <> 5% and keyhit% <> 0% and     ~
               keyhit% <> 7% then return
            if keyhit% = 7% and enabled_flag% = 1% then return
            if args% < 5% and mode% = 0% then return
            on mode% + 1% gosub L13120, L13125, L13135
            s$(lst_wnd_line%) = " "
            if len(s$(lst_wnd_line%-1%)) > 1% then                       ~
            s$(lst_wnd_line%-1%)=s$(lst_wnd_line%-1%) & hex(94) & answer$
            s$(lst_wnd_line%-2%) = hex(8c) & "(1)Cancel Search"
            od$(1%)=bin(lst_wnd_line%-2%) & hex(a4) & bin(3%) &          ~
                   bin(lst_wnd_line%-2%)
            call "WSXIO" addr("X", #5, hex(80), od$(1%),                 ~
             str(s$(),(lst_wnd_line%-3%)*80%+1%,240%),240%,iosw_receiver$)
            enabled_flag% = 1%
            return
L13120:     s$(lst_wnd_line%-1%) = hex(8c)
            if keyhit% = 0% then                                         ~
               s$(lst_wnd_line%-1%) = hex(8c) & "Now Searching For Code:"
               return
L13125:     s$(lst_wnd_line%-1%) = hex(8c) &                             ~
              "Now Searching for Descriptions Which Sound Like:" : return
L13135:     s$(lst_wnd_line%-1%) = hex(8c) &                             ~
              "Now Searching for Descriptions Containing:"       : return

        deffn'77(x%,                               /* Screen Column    */~
                 s$(lst_wnd_line%-3%),             /* Field Prompt     */~
                 s$(lst_wnd_line%-1%))             /* PF Key Prompt    */
            answer$ = " "
            str(s$(lst_wnd_line%-3%),,1%) = hex(a4)
            s$(lst_wnd_line%) = " "
            s$(lst_wnd_line%-2%) = hex(8c) & "(1)Cancel Search"
            str(s$(lst_wnd_line%-1%),x%,len%) = all(hex(0b))
            if x%+len% < 81% then                                        ~
               str(s$(lst_wnd_line%-1%),x%+len%) = hex(8c)
            if len% < 50% + (30%-x%) then                                ~
               str(s$(lst_wnd_line%-1%),x%+len%)=hex(8c)
L13202:     save_row% = r% : save_col% = c% : override% = 1%
            r% = lst_wnd_line% - 1% : c% = x%
            gosub write_screen
            od$(1%)=bin(lst_wnd_line%-1%) & hex(000000)
            call "WSXIO" addr("X", #5, hex(40), od$(1%),                 ~
              s$(lst_wnd_line%-1%), 80%, iosw_receiver$)
            aid$ = str(iosw_receiver$,3%,1%)
            gosub decode_aid_byte
            r% = save_row% : c% = save_col%
            if keyhit% = 1% then L13275
            if keyhit% <> 0% then L13202
            answer$ = str(s$(lst_wnd_line%-1%),x%,len%)
            tran(answer$,hex(200b))replacing
            return
L13275:     gosub'105(lst_wnd_line%)
            return

        find_word
            if high_level% = 0% then len% = min(50%, deslen%)
            if high_level% = 1% then len% = min(50%, deslen1%)
            if len% < 1% then return
            gosub'77(22%, hex(a4) &                                      ~
             "Enter the Text To Search For, Then Press (RETURN)",        ~
            hex(8c) & "(9)Search For Text:" & hex(80))
            if keyhit% = 1% then return
            answer$ = or all(hex(20)) : f1%(1%) = 1% : mode% = 2%
            gosub keyboard_enable
            gosub first_screen
            return

        shrink_up
            if disp$ > " " and disp$ <> "d" then return
            if r9% < 9% then return
            s$(lst_wnd_line%) = s1$(lst_wnd_line%) : r9% = r9% - 1%
            gosub update_window_variables
            if r% > lst_cde_line% then r% = lst_cde_line%
            plowkey$=key1$(lst_cde_line%)
            if prname$ = "GLMAIN" then call "GLUNFMT" (plowkey$)
            gosub'105(lst_wnd_line%)
            return

        expand_down
            if disp$ > " " and disp$ <> "d" then return
            if r0% + r9% > 24% then return
            r9% = r9% + 1%
            gosub update_window_variables
            gosub'101(lst_cde_line%,lst_cde_line%)
            gosub'105(lst_wnd_line%)
            return

        window_flip
            if disp$ > " " and keyhit% = 12% and disp$ <> "d" then return
            plowkey$=key1$(fst_cde_line%)
            if prname$ <> "GLMAIN" then L13480
               call "GLUNFMT" (plowkey$)
               str(plowkey$,break%+1%,9%) = addc all(hex(ff))
               goto L13485
L13480:     str(plowkey$,break%+1%,displen%) = addc all(hex(ff))
L13485:     f1%(1%) = 1%
            if r9% > 23% then L13510
               r0% = 1% : r9% = 24%
               gosub update_window_variables
               goto L14140
L13510:     r0% = r10% : r9% = r19%
            gosub update_window_variables
            r% = fst_cde_line% : c% = 2%
            if r0% < 2% then L14060
                for x% = 1% to r0%-1%
                    s$(x%) = s1$(x%)
                next x%
L14060:     if r0% + r9% > 24% then L14140
                for x% = r0% + r9% to 24%
                    s$(x%) = s1$(x%)
                next x%
L14140:     gosub next_screen
            gosub'106(r0%)
            return

        documentation
            call "WSXIO" addr("C", #5)
            call "ZMANUAL" (program$)
            close ws
            call "WSXIO" addr("O", 255%, "Y", #5, x%)
            override% = 1%
            return

        print_screen
            call "WSXIO" addr("C", #5)
            call "ZPRNTSCR"
            call "WSXIO" addr("O", 255%, "Y", #5, x%)
            return

        shrink_down
            if disp$ > " " and disp$ <> "d" then return
            if r9% < 9% then return
            s$(r0%) = s1$(r0%)
            plowkey$=key1$(fst_cde_line%)
            if prname$ <> "GLMAIN" then L14620
               call "GLUNFMT" (plowkey$)
               str(plowkey$,break%+1%,9%) = addc all(hex(ff))
               goto L14640
L14620:     str(plowkey$,break%+1%,displen%) = addc all(hex(ff))
L14640:     f1%(1%) = 1% : r0% = r0% + 1% : r9% = r9% - 1%
            gosub update_window_variables
            if r% < fst_cde_line% then r% = fst_cde_line%
            gosub next_screen
            gosub'106(r0%)
            return

        expand_up
            if disp$ > " " and disp$ <> "d" then return
            if r0% < 2% then return
            r0% = r0% - 1% : r9% = r9% + 1%
            gosub update_window_variables
            plowkey$=key1$(r0%+4%)
            if prname$ <> "GLMAIN" then L14940
               call "GLUNFMT" (plowkey$)
               str(plowkey$,break%+1%,9%) = addc all(hex(ff))
               goto L14960
L14940:     str(plowkey$,break%+1%,displen%) = addc all(hex(ff))
L14960:     f1%(1%) = 1%
            gosub next_screen
            gosub'106(r0%)
            return

        print_report
            gosub print_file
            goto quit

        print_file
            REM *** Initialization & Setup ***
            call "WSXIO" addr("C", #5)
            call "ZSHOSTAT" ("Now Printing List of Codes in " & prname$)
            call "WSXIO" addr("O", 255%, "Y", #5, x%)
            date$ = date : call "DATEFMT" (date$)
            time$ = " "  : call "TIME" (time$)
            call "COMPNAME" (2%, company$, x%)
            if report_option$ = "r" then width% = max(deslen%,80%)       ~
            else width% = max(80%,description_pos% + deslen%)
            width% = max(width%,min(160%,len(column$(high_level%+1%))))
            width% = max(width%, max_len%)
            if report_option$ = "R" then width% = width% + displen% + 3%
            width% = min(160%,width%)
            override% = 1% : l%, page% = 0%
            call "GETNAMES" addr(#1, prtfile$, prtlib$, prtvol$)
            call "READFDR"  addr(prtfile$, prtlib$, prtvol$, 0%,         ~
                                 "RC", rc%, return%)
            rc% = max(1000%,rc%/3%)
            call "OPENPRT" (#2, rc%, prtfile$, prtlib$, prtvol$, return%)
            if no_display$ = "R" then q% = 1% else q% = 4%
            readkey$ = key1$
            str(readkey$,break%+1%) = all(hex(00))

            REM *** Plow & Print Loop ***
L15310:     call "PLOWALTS" (#1, readkey$, key%, break%, f1%(2%))
            if f1%(2%) = 0% then L15620
L15330:     readkey$ = key(#1, key%)
            gosub include_exclude_test
            if ok$ <> "Y" then L15570
            gosub'88(key(#1,key%))       /* Get Description Area       */
            on mode% gosub sounds_like_check, word_search_check
            if ok$ <> "Y" then L15560
            if prname$ = "GLMAIN" then call "GLFMT" (readkey$)
            if l% < 1% then gosub print_heading
            if high_level%=0% then str(print$,q%)=str(readkey$,break%+1%,~
               displen%)  else str(print$,q%)=str(readkey$,,save_break%)
            if prname$ = "GLMAIN" then call "GLUNFMT" (readkey$)
            if high_level% > 0% then str(readkey$,save_break%+1%) =      ~
               all(hex(ff))
            if high_level% = 0% and dlen% > 0% then                      ~
               str(readkey$,save_break%+1%+dlen%) = all(hex(ff))
            for u% = 1% to 3%
                if des$(u%) = " " and u% > 1% then L15550
                if report_option$ = "r" or no_display$ = "D"             ~
                    then print$ = des$(u%)                               ~
                else str(print$,description_pos% - (4%-q%)) = des$(u%)
                gosub print_line
                l% = l% - 1% : print$ = " "
L15550:     next u%
L15560:     if high_level%>0% or dlen%>0% or prname$=prname2$ then L15310
L15570:     call "READNEXT" (#1, f1%(1%))
            if f1%(1%) = 0% then L15620
            if break% < 1% then L15330
            if str(key(#1,key%),,break%)=str(readkey$,,break%) then L15330
            f1%(1%) = 0%
L15620:     print$ = "   ********** END OF LISTING **********"
            call "STRING" addr("CT", str(print$,,width%), width%)
            write #2, hex(0002), str(print$,,len(print$))
            close #2
            return

        print_line
            write #2, hex(0001), str(print$,,len(print$))
            print$ = " "
            return

        print_heading
            page% = page% + 1% : l% = 52%
            print$ = company$
            call "STRING" addr("CT", str(print$,,width%), width%)
            str(print$,,8%) = date$
            str(print$,width%-14%) = str(program$,,8%) & ":UTL001"
            write #2, hex(8001), str(print$,,len(print$))
            print$ = "Codes Listing From File: " & prname$
            if no_display$ = "R" and                                     ~
               str(descr$,,1%) = hex(06) then print$ = str(descr$,2%)
            call "STRING" addr("CT", str(print$,,width%), width%)
            str(print$,,8%) = time$ : str(print$,width%-10%) = "Page:"
            convert page% to str(print$,width%-4%,5%),pic(#####)
            gosub print_line
            if sgn(save_break%) + high_level% <> 1% then L15911
               print$ =                                                  ~
            "Listed are the Entries Under: " & str(readkey$,,save_break%)
               call "STRING" addr("CT", str(print$,,width%), width%)
               gosub print_line
L15911:     if mode% = 0% then L15914
               print$ = str(s$(lst_cde_line%+1%),2%)
               call "STRING" addr("CT", str(print$,,width%), width%)
L15914:     gosub print_line
            str(print$,q%) = "Code"
            if deslen%>0% then str(print$,description_pos% - (q%-4%)) =  ~
               "Description"
            if column$(high_level%+1%) > " " then                        ~
               print$ = " " & column$(high_level%+1%)
            if no_display$ = "R" then print$ = str(print$,2%)
            if no_display$ = "D" then print$ = str(print$,4%)
            gosub print_line
            if column$(4%) = " " then L16080
            print$ = column$(4%)
            gosub print_line
L16080:     if column$(5%) = " " then L16100
            print$ = column$(5%)
            gosub print_line
L16100:     str(print$,,width%) = all("_")
            write #2, hex(0000), str(print$,,len(print$))
            print$ = " "
            return

        quit
            if disp$ > " " and disp$ <> "d" then L16280
            od$(1%)=hex(01000101)
            call "WSXIO" addr("X", #5, hex(80), od$(1%), s1$(),1920%,    ~
                              iosw_receiver$)
L16280:     call "WSXIO" addr("C", #5)
            goto exit_routine

        deffn'101(r1%,r2%)
            gosub keyboard_enable
            if nonext% <> 0% then L16360
            if pos(str(plowkey$) <> hex(00)) = 0%  then L16360
            if fs(#1) <> "02" then L16360
               call "READNEXT" (#1, f1%(1%))
               goto L16380
L16360:     call "PLOWALTS" (#1, plowkey$, key%, break%, f1%(1%))
L16380:     if f1%(1%) = 0% then L16940
            nonext% = 0%
L16400:     plowkey$ = key(#1, key%)
            gosub bailout_test           /* Check for User Bailout     */
            if reads% = 0% then L16940  /* User Canceled Search       */
            gosub include_exclude_test
            if ok$ <> "Y" then L16820
            gosub'88(key(#1,key%))       /* Get Description Area       */
            on mode% gosub sounds_like_check, word_search_check
            if ok$ <> "Y" then L16760
            if prname$ = "GLMAIN" then call "GLFMT" (plowkey$)
            key1$(r1%) = plowkey$
            keyp$(r1%) = key(#1, 0%)

            s$(r1%)=hex(860b8c)
            if gui% = 1% then s$(r1%) = hex(2086b4)
            saveme% = displen%
            if prname$ = "GLMAIN" then displen% = 12%
            if high_level%=0% then                                       ~
               str(s$(r1%),4%)=str(plowkey$,break%+1%,displen%)          ~
            else                                                         ~
               str(s$(r1%),4%)=str(plowkey$,,save_break%)
            if prname$ = "GLMAIN" then call "GLUNFMT" (plowkey$)
            displen% = saveme%
            saveme% = description_pos%
            if prname$ = "GLMAIN" then description_pos% = description_pos% + 3%
            for d% = 1% to 3%
               if des$(d%) = " " and d% > 1% then L16650
               str(s$(r1%),description_pos%) = des$(d%)
               if d% > 1% then key1$(r1%) = all(hex(00))
               if d% > 1% then keyp$(r1%) = all(hex(00))
               r1% = r1% + 1%
               if r1% <= r2% then L16649
                  d% = 3% : goto L16650
L16649:        s$(r1%)=hex(8c)
L16650:     next d%
            description_pos% = saveme%
            if high_level% > 0% then str(plowkey$,save_break%+1%) =      ~
               all(hex(ff))
            if high_level% = 0% and dlen% > 0% then                      ~
               str(plowkey$,save_break%+1%+dlen%) = all(hex(ff))
            if r1% > r2% then return
L16760:     if high_level%>0% or dlen%>0% or prname$=prname2$ then L16360
L16820:     call "READNEXT" (#1, f1%(1%))
            if f1%(1%) = 0% then L16940
            if break% < 1% then L16400
            if str(key(#1,key%),,break%)=str(plowkey$,,break%) then L16400
            f1%(1%) = 0%

L16940:     REM *** END OF FILE ***
               for x% = r1% to r2%
                 s$(x%) = " "
                 key1$(x%) = all(hex(00))
                 keyp$(x%) = all(hex(00))
               next x%
               nonext% = 1%
               gosub'105(lst_wnd_line%)  /* Turn Off PF Key Literals   */
               return

        include_exclude_test
            ok$ = "Y"
            get #1,str(work$())
            gosub glmain_include_test
            if ok$ = "N" then return
            if args% < 5% then return
            i% = dim(incl_excl(),1)
            for u% = i% to 1% step -1%
                if incl_excl(u%) <> 0 then L17200
                     u% = 1% : goto L17420
L17200:         if mod(abs(incl_excl(u%))*1000,10) > 0 then              ~
                   gosub get_alt_record_and_description   /* Alt Test  */
                on sgn(incl_excl(u%)) + 2% gosub L17460,   /* Exclude   */~
                                                      ,   /* Don't Care*/~
                                                 L17540    /* Include   */
                if mod(abs(incl_excl(u%))*1000,10) > 0 then              ~
                   get #1, str(work$())                   /* Restore   */
L17300:         if u% = 1% then L17420
                if abs(incl_excl(u%)) <> abs(incl_excl(u%-1%)) then L17400
                if sgn(incl_excl(u%))  < 0% then L17400
                   if ok$ <> "Y" then L17380
                      u% = u% - 1% : goto L17300
L17380:            ok$ = "Y"
L17400:         if ok$ = "N" then u% = 1%
L17420:     next u%
            return
L17460:     REM *** Exclude Test ***
            if str(incl_excl$(u%),,1%) = "<" then L17602
            if str(work$(),abs(incl_excl(u%)),mod(abs(incl_excl(u%))*100,~
               100)) = incl_excl$(u%) then ok$ = "N"
            return
L17540:     REM *** Include Test ***
            if str(incl_excl$(u%),,1%) = "<" then L17621
            if str(work$(),incl_excl(u%),mod(incl_excl(u%)*100,          ~
               100)) <> incl_excl$(u%) then ok$ = "N"
            return
L17602:     REM *** Exclude Range Test ***
            gosub L17633
            if str(work$(),abs(incl_excl(u%)),mod(abs(incl_excl(u%))*100,~
               100)) > str(incl_excl$(u%),2%,rc%) then return
            if x% = 0% then L17615
            if str(work$(),abs(incl_excl(u%)),mod(abs(incl_excl(u%))*100,~
               100)) < str(incl_excl$(u%),x%+2%) then return
L17615:     ok$ = "N"
            return
L17621:     REM *** Include Range Test ***
            gosub L17633
            if str(work$(),incl_excl(u%),mod(incl_excl(u%)*100,          ~
               100)) >  str(incl_excl$(u%),2%,rc%) then ok$ = "N"
            if x% = 0% then return
            if str(work$(),incl_excl(u%),mod(incl_excl(u%)*100,          ~
               100)) <  str(incl_excl$(u%),x%+2%) then ok$ = "N"
            return
L17633:     REM *** Look For Closing End of Range (If Any) ***
            x% = pos(str(incl_excl$(u%),2%) = ">")
            if x% = 0% then rc% = mod(abs(incl_excl(u%))*100,100)        ~
                       else rc% = x% - 1%
            return
        glmain_include_test
            if prname$ <> "GLMAIN" then return
            if str(work$(),41%,1%) = hex(01) then ok$ = "N"
            if str(work$(),41%,1%) = hex(02) then ok$ = "N"
        return

        get_alt_record_and_description
            des$(1%) = str(work$(), disp%, deslen%)
            if fs3% = 0% then return
            findkey$ = key(#1, key%)
            if high_level% > 0% then filekey$ = str(findkey$,,abs(brk%)) ~
            else filekey$ = str(findkey$,break%+1%,displen%)
            if descr_pos < 0 then filekey$ = des$(1%)
            call "REDALT0" (#3, filekey$, key3%, f1%(3%))
            if f1%(3%) = 0% then return
            get #3, str(work$())
            des$(1%) = str(work$(),disp2%,deslen%)
            if high_level% = 1% then des$(1%)=str(work$(),disp2%,deslen1%)
            return

        deffn'88(findkey$)
            des$() = " "
            if deslen% = 0% then return
            gosub get_alt_record_and_description
            if fs3% > 0% and f1%(3%) = 1% then get #1, str(work$())
            if args% < 9% then return
            if max(descr_map())>0 or min(descr_map())<0 then des$()=" "
            for u% = 1% to dm% step 2%
                if descr_map(u%) <> 0 then L18040
                   u% = dm% : goto L18110
L18040:         output_len% = mod(abs(descr_map(u%+1%))*100,100)
                input_len% =  mod(abs(descr_map(u%))*100,100)
                output_pos% = mod(abs(descr_map(u%+1%)),1000%)
                output_line%= abs(descr_map(u%+1%)) / 1000%
                x% = max(input_len%,output_len%)
                on sgn(descr_map(u%))+2% gosub get_alt,,get_primary
                if max_len% < output_pos% + x% then                      ~
                   max_len% = output_pos% + x% - 1%
L18110:     next u%
            return

        get_alt
            if fs3% > 0% and f1%(3%) = 1% then get #3,str(work$())
            if fs3% > 0% and f1%(3%) = 1% then L18230                     ~
            else str(des$(output_line%+1%),output_pos%,x%) = " "
            return

        get_primary
            if fs3% > 0% and f1%(3%) = 1% then get #1,str(work$())
L18230:     if output_len% > 0% then L18410         /* Convert Field  */
            REM *** Must be a Character Field - No Conversion Needed ***
            str(des$(output_line%+1%),output_pos%,input_len%) =          ~
                     str(work$(), abs(descr_map(u%)),input_len%)
            REM *** Check for Format Code - Special Conversion Needed?***
            format% =  mod(abs(descr_map(u%))*1000,10)
            on format% gosub L18320, L18360
            return

L18320:     REM *** Convert to Formatted Date ***
            if input_len% = 8% then                                      ~
              call "DATFMTC" (str(des$(output_line%+1%),output_pos%,10%))~
            else                                                         ~
              call "DATEFMT" (str(des$(output_line%+1%),output_pos%,8%))
            return

L18360:     REM *** Convert to Formatted Time ***
            if str(des$(output_line%+1%),output_pos%,input_len%) = " "   ~
               then return
            str(des$(output_line%+1%),output_pos%+6%,2%)= " "
            call "TIMEOK" (str(des$(output_line%+1%),output_pos%,8%),d,  ~
                           " ")
            return

L18410:     REM *** Convert PD or BI to String for Display ***
            d,m = 0                     /* Number of decimal places   */
            REM *** If Negative then Integer, Else Floating Point ***
            if sgn(descr_map(u%+1%)) < 0% then L18550
               d = mod(abs(descr_map(u%+1%))*1000,10)
               m = mod(abs(descr_map(u%+1%))*10000,10)

            upkl% = input_len%
            if upkl% = 0% then L18580

            upksgn% = upkl%
            upkbase% = abs(descr_map(u%))
            upkdec% = d

            init (hex(00)) upkwork$
            cupkl% = min(8%, upkl%)

            str(upkwork$, 9% - cupkl%, cupkl%) =                         ~
                str(work$(), upkbase% + upksgn% - cupkl%, cupkl%)
            get upkwork$ using L18484, x
L18484:         FMT PD(15,0)
            x = x / (10 ^ upkdec%)

L18490:     upkl% = upkl% - cupkl%
            if upkl% <= 0% then L18580

            cupkl% = min(7%, upkl%)
            init (hex(00)) upkwork$
            str(upkwork$,8%,1%) =                                        ~
               str(work$(),upkbase% + upksgn% - 1%, 1%) and hex(0f)

            str(upkwork$, 8% - cupkl%, cupkl%) =                         ~
                str(work$(), upkbase% + upkl% - cupkl%, cupkl%)
            get upkwork$ using L18484, upkx
            x = x + (upkx * (10 ^ (14% - upkdec%)))
            goto L18490

L18550:     REM *** Must be an Integer (We hope) ***
            on input_len% gosub L18630, L18650, L18670, L19010

L18580:     REM *** Now that we've got the number, Put it into Descr ***
            call "CONVERT" (x, m+(d/10), str(des$(output_line%+1%),      ~
                            output_pos%,output_len%))
            return

L18630:     x = val(str(work$(), abs(descr_map(u%)),input_len%), 1)
            return
L18650:     x = val(str(work$(), abs(descr_map(u%)),input_len%), 2)
            return
L18670:     x = val(str(work$(), abs(descr_map(u%)),input_len%), 3)
            return
L19010:     x = val(str(work$(), abs(descr_map(u%)),input_len%), 4)
            return

        sounds_like_check
            temp$ = str(des$(),1%)
            call "SOUNDEX" (temp$, soundex2$)
            if soundex1$ = soundex2$ then return
            temp$ = str(temp$,,len(answer$)) or all(hex(20))
            if temp$ = answer$ then return
            ok$ = "N"
            return

        word_search_check
            temp$ = str(des$(),1%) or all(hex(20))
            search temp$ = answer$ to p%()
            if p%(1%) > 0% then return
            ok$ = "N"
            return

        bailout_test        /* Test to See if User Bailed Out via PF 1 */
            reads% = reads% + 1% /* Increment # Reads Since last Tested*/
            if mod(reads%,50%) > 0% then return /* Hasn't Been 50 reads*/
            gosub interrupt_check             /* Check for PF Key Hit  */
            if aid$ = "A" then reads% = 0%    /* Flag to Bail Out (PF1)*/
            return

        REM *************************************************************~
            *                 W R I T E   S C R E E N                   *~
            *************************************************************
        write_screen:
            on override% goto L19360
            od$(1%)=bin(fst_cde_line%,1) & hex(a0) & bin(c%,1) & bin(r%,1)
            start% = (fst_cde_line%-1%)*80%+1% : slength% = (r9%-7%)*80%
            goto L19380
L19360:     od$(1%)=bin(1%,1) & hex(a0) & bin(c%,1) & bin(r%,1)
            start% = 1% : slength% = 1920%
L19380:     call "WSXIO" addr("X", #5, hex(80), od$(1%),str(s$(), start%,~
                              slength%),slength%, iosw_receiver$)
            override% = 0%
            return

        REM *************************************************************~
            *                 R E A D   S C R E E N                     *~
            *************************************************************
        read_screen:
            od$(1%)=bin(01%,1) & hex(000000)
            call "WSXIO" addr("X", #5, hex(40), od$(1%), s1$(), rlen%,   ~
                     iosw_receiver$)
            aid$ = str(iosw_receiver$,3%,1%)
            gosub decode_aid_byte
            rlen% = 0%
            c% = val(str(od$(1%),3%,1%),1) : r% =val(str(od$(1%),4%,1%),1)
            if r% >= fst_cde_line% then L19560
L19550:        r% = fst_cde_line% : c% = 2% : return
L19560:     if r% <= lst_cde_line% then return
               goto L19550

        interrupt_check
            str(iosw_receiver$,3%,1%) = hex(00)
            call "WSXIO" addr("A", #5, str(iosw_receiver$,3%,1%))
            if str(iosw_receiver$,3%,1%) < hex(40) then                ~
                 str(iosw_receiver$,3%,1%) = hex(00)
            aid$ = str(iosw_receiver$,3%,1%)
            return

        decode_aid_byte
            keyhit% = val(aid$,1) - 64%
            if keyhit% > 16% then keyhit% = keyhit% - 16%
            return

        set_gui_prompts
            s$(x%-3%) = " "
            if mode% = 1% then s$(x%-3%) = hex(a4) &                     ~
            "Shown are those descriptions which sound like: " & answer$
            if mode% = 2% then s$(x%-3%) = hex(a4) &                     ~
            "Shown are those descriptions which contain: " & answer$
            s$(x%-2%) = hex(8c) & "(2)First (4/5)Prev/Next (7)Scroll (~
            ~13)Instructions (15/31)Print/Rpt (16)Cancel"
            if supres$ = "Y" then str(s$(x%-2%),74%) = "Return"
            s$(x%-1%) = hex(8c) &                                        ~
              "(6)Sounds Like Search (8)Code Search (9)Word Match Search"
            if high_level% = 0% and key_desclen < 0 and brk% >= 0% then  ~
               s$(x%-1%) = hex(84) & "(1)Prior Level" & s$(x%-1%)
            if f1%(1%) = 0% then str(s$(x%-2%),11%,25%) = " "
            s$(x%) = all("_")
            override% = 1%
            return

        deffn'105(x%)     /* Set Bottom Window Text & Prompts          */
            on gui% goto set_gui_prompts
            s$(x%-3%) = hex(ac) & "Position cursor (TAB) to line and pres~
        ~s (RETURN) to return with that code."
            if (r0% = 1% and column$(3%) = " ") or supres$ = "Y" then    ~
               s$(x%-3%) = hex(ac) & "Available PF Keys Are :"
            if mode% = 1% then s$(x%-3%) = hex(a4) &                     ~
            "Shown are those descriptions which sound like: " & answer$
            if mode% = 2% then s$(x%-3%) = hex(a4) &                     ~
            "Shown are those descriptions which contain: " & answer$
            s$(x%-2%) = hex(8c) & "(2)First (4/5)Prev/Next (7)Scroll (~
            ~13)Instructions (15/31)Print/Rpt (16)Cancel"
            if supres$ = "Y" then str(s$(x%-2%),74%) = "Return"
            s$(x%-1%) = hex(8c) &                                        ~
              "Find by:(6)Sounds Like (8)Code (9)Word Match   " &        ~
              "Cursor & RETURN:Return With Code"
            if supres$ = "Y" then                                        ~
               s$(x%-1%) = hex(8c) &                                     ~
              "Find by:(6)Sounds Like (8)Code (9)Word Match  "
            s$(x%) = hex(8c) & "               (10/26)Shrink Up/Down (11/~
        ~27)Expand Up/Down (12)Full Screen Flip"
            if high_level% = 0% and key_desclen < 0 and brk% >= 0% then  ~
            s$(x%) = hex(84) & "(1)Prior Level" & hex(8c) & "(10/26)Sh" &~
              "rink Up/Down (11/27)Expand Up/Down (12)Full Screen Flip"
            if disp$ > " " and disp$ <> "d" then str(s$(x%),17%) = " "
            if x% < 24% then str(s$(x%),,1%) = hex(a4)
            if r9% < 9% then str(s$(x%),17%,21%) = " "
            if r9% > 23% then str(s$(x%),39%,21%) = " "
            if f1%(1%) = 0% then str(s$(x%-2%),11%,25%) = " "
            override% = 1%
            return

        set_gui_header
            if x% > 1% then s$(x%-1%) = all("_")
            s$(x%) = hex(a4) & "Select Desired Entry using Keyboard or Mo~
        ~use to return with the selected code."
            if supres$ = "Y" then s$(x%) = hex(a4) &                     ~
              "Shown below are the codes related to your Inquiry"
            s$(x%+1%) = hex(8c) & " You may cancel your search using func~
        ~tion key 16 or the Button for same."
            s$(x%+2%)  = hex(2020ac) & "CODE"
            goto L22010

        deffn'106(x%)     /* Set Top Window Text & Prompts          */
            on gui% goto set_gui_header
            s$(x%) = hex(a4)
            if x% = 1% then                                              ~
            s$(x%) = hex(8c) & "Position cursor (TAB) to line and press (~
        ~RETURN) to return with that code."
            if x% = 1% and supres$ = "Y" then s$(x%) = hex(8c) &         ~
              "Shown below are the codes related to your Inquiry"
            s$(x%+1%) = hex(8c) & " Use PF(16) to cancel search and retur~
        ~n for manual entry of code."
            s$(x%+2%)  = hex(ac) & "  CODE"
L22010:     if str(descr$,,1%) = hex(06) then                            ~
               s$(x%+1%) = hex(8420) & str(descr$,2%)
            if high_level% = 0% then curlen% = displen%
            on (sgn(save_break%)+high_level%) gosub L22190,L22250
            description_pos% = max(15%,min(80%,curlen%+7%))
            if no_display$ = "D" and high_level% = 0% then               ~
                 description_pos% = 4%
            if deslen% > 0% then                                         ~
               str(s$(x%+2%),description_pos%) = "Description"
            if prname$ = "GLMAIN" then                                   ~
               str(s$(x%+2%),description_pos%) = "   Description"
            if column$(high_level%+1%) > " " then                        ~
               s$(x%+2%)  = hex(ac) & column$(high_level%+1%)
            if str(s$(x%+2%),65%) = " " then                             ~
               str(s$(x%+2%),65%) = "FILE: " & prname$
            if x% = 1% and column$(3%) > " " then                        ~
            s$(x%) = column$(3%)
            override% = 1%
            return

L22190:     REM *** LOW LEVEL, BREAK POINT > 0 ***
            if key_desclen >= 0 then return
            s$(x%+1%) = hex(8c20) & "Shown below are the CODES Under"    ~
                       & hex(84) & str(key1$,,break%)
            return

L22250:     REM *** HIGH LEVEL ***
            if x% = 1% then                                              ~
            s$(x%) = hex(8c) & "Position cursor (TAB) to line and press (~
        ~RETURN) to explode/implode that line."
            curlen% = save_break%
            return

        REM *************************************************************~
            *   Set the variables which indicate the last window line,  *~
            *   the first codes line, and the last 'codes' line.        *~
            *   Invoked any time R0% or R9% (first screen line & total  *~
            *   number of lines) values are changed.                    *~
            *************************************************************
        update_window_variables
            lst_wnd_line% = r0% + r9% - 1%
            fst_cde_line% = r0% + 3% : lst_cde_line% = r0% + r9% - 5%
            return

        REM *************************************************************~
            * Note;  This routine is a perfect example of Law 6 of the  *~
            *        8 Laws of Project Management;                      *~
            *        "No program is ever completely debugged; Attempts  *~
            *        to debug a program inevitably introduce new bugs   *~
            *        that are even harder to find".                     *~
            *************************************************************

        REM *************************************************************~
            *               I N I T I A L I Z A T I O N                 *~
            *************************************************************
initialize
        REM *************************************************************~
            * Close the Workstation and reopen it using WSXIO routine to*~
            * allow windowing, etc.                                     *~
            *************************************************************
            if gui% = 0% then CHUI_Init
            rem *** Get User Specified Limit on Using Listbox from Environment
            call "CMSLBOX" addr(lb_limit%)
            rem *** get number of records in primary file ***
            call "GETNAMES" addr(#1, prtfile$, prtlib$, prtvol$)
            call "READFDR"  addr(prtfile$, prtlib$, prtvol$, 0%,         ~
                                 "RC", rc%, return%)
            rem *** if a GENCODES Table - use a GUI Listbox ***
            if prname$ = "GENCODES" and args% < 2% then Gencodes_Listbox
            rem *** if a simple query, and number of records < $CMSLBOX, then use a GUI Listbox ***
            if args% < 2% and rc% < lb_limit% and brk% >= 0% and key_desclen >= 0 then Std_Listbox

CHUI_Init:  close ws
            call "WSXIO" addr("O", 255%, "Y", #5, x%)
            rlen% = 1920% : f1% = 0% : nonext% = 1%
            gosub read_screen  /*Read Screen Contents & Cursor Position*/
            c% = val(str(od$(1%),3%,1%),1) : r% =val(str(od$(1%),4%,1%),1)
            if scrrow% > 0% then r% = scrrow%
            r0% = 1% : r9% = r% - 1%
            if r% > 12% then L22662
               r0% = r% + 1% : r9% = 25% - r0%
L22662:     gosub update_window_variables
            REM *** Continue Initialization ***
            r10% = r0% : r19% = r9% : mode%, y% = 0% : keyhit% = -1%
            if program$ = " " then call "EXTRACT" addr("CF", program$)
L22720:     x% = pos(str(s1$(),y%+1%) > hex(7f))
            if x% = 0% then L23080
                y% = x% + y%
                str(s1$(),y%,1%) = boole hex(08)
                str(s1$(),y%,1%) = bool8 hex(fd)
                str(s1$(),y%,1%) = or hex(04)
                if y% < 1920% then L22720
L23080:     mat s$ = s1$


            for x% = fst_cde_line% to lst_cde_line%
                s$(x%) = " "
            next x%
            init(hex(00)) key1$(), keyp$() : rlen% = 0% : f1%(1%) = 1%
            str(column$(),1%) = " "
            if args% < 1% then L23300
               for x% = 1% to min(5%,dim(columnhdr$(),1))
                   column$(x%) = columnhdr$(x%)
               next x%
            if args% > 6% then supres$ = suppress$ else supres$ = " "
L23300:     save_break% = break%
            plowkey$ = key1$
            if key_desclen >= 0 or break% = 0% then L23540
            if brk% >= 0% then L23460
               gosub higher_level
               goto L23520
L23460:     call "PLOWALTS" (#1, plowkey$, key%, break%, f1%(1%))
            if f1%(1%) > 0% then L23520
               gosub higher_level
L23520:     plowkey$ = key1$
            if str(plowkey$,break%+1%) = "?" then str(plowkey$,break%+1%)~
               = all(hex(00))
L23540:     call "PLOWALTS" (#1, plowkey$, key%, break%, f1%(1%))
            if f1%(1%) = 0% then L23560
            gosub include_exclude_test
            if ok$ <> "Y" then L23540
L23560:     if f1%(1%) = 0% then str(plowkey$,break%+1%) = all(hex(00))  ~
            else plowkey$ = key1$
            if high_level% = 0% and dlen% > 0% then                      ~
               str(plowkey$,save_break%+1%+dlen%) = all(hex(ff))
            key1$(fst_cde_line%) = plowkey$
            keyp$(fst_cde_line%) = key(#1, 0%)
          if pos(str(key1$(fst_cde_line%),break%+1%,displen%)>hex(00))=0%~
              then str(key1$(fst_cde_line%),break%+displen%,1%)=hex(01)
            if prname$="GLMAIN" then                                     ~
               call "GLFMT" (str(key1$(fst_cde_line%),,12%))
            f1%(1%) = 1%
            gosub L23900
            if scrrow% = 0% then gosub window_flip else gosub next_screen
            return
L23900:     r% = fst_cde_line% : c% = 2%
            gosub'106(r0%)
            return

Gencodes_Listbox
            if str(key1$,,1%) = hex(00) then CHUI_Init
            str(readkey$,,9%) = all(hex(00))
            str(readkey$,10%) = str(key1$,,9%)
            call "READ100" (#1,readkey$,f1%(1%))
            if f1%(1%) = 1% then get #1 using L24006, codelen$            ~
            else codelen$ = "15"
L24006:     FMT POS(55),CH(2)
            convert codelen$ to displen%


Std_Listbox
            REM *** Hide Screen Cursor ***
            call "SENDCMD" (uw$ & "UWSETUP3,1" & uw$)
            plowkey$ = key1$

            command$ = uw$ & "UWSCRIPTcaelus/sdk/listbox.scr,4,"
            if prname$ = "GENCODES" then ~
               column$(1%) = "Select a " & str(plowkey$,,break%) ~
            else ~
               column$(1%) = "Select a Code"
            if str(descr$,,1%)=hex(06) then column$(1%)=str(descr$,2%)

            command$ = command$ & column$(1%) & ",1,4,10,22,75,CoStar" & uw$
            call "SENDCMD" (str(command$,,len(command$)))
            str(plowkey$,break%+1%) = " "
Std_LPlow:  call "PLOWALTS" (#1, plowkey$, key%, break%, f1%(1%))
            if f1%(1%) = 0% then Show_Listbox
            get #1, str(work$())
            gosub'88(plowkey$)
            if prname$ = "GLMAIN" then call "GLUNFMT" (plowkey$)
            key1$(1%) = str(plowkey$,break%+1%,displen%)
            tran(des$(1%),";,'" & hex(22))replacing
            command$ = str(key1$(1%),,displen%+2%) & des$(1%) & "," &    ~
                       str(key1$(1%),,displen%) & ","
            call "SENDCMD" (str(command$,,len(command$)))
            goto Std_LPlow

Show_Listbox
            call "SENDCMD" (uw$)
List_Wait:  call "GETCMD" (9999%,keyhit%,readkey$)
            if keyhit% < 0% then List_Wait /* Timed Out */
            f1% = 0%
            if keyhit% > 0% then End_ListBox     /* User Canceled */
            if break% > 0% then ~
                key1$ = str(plowkey$,,break%) & str(readkey$,1%,displen%) ~
            else ~
                key1$ = str(readkey$,1%,displen%)
            if prname$ = "GLMAIN" then call "GLUNFMT" (key1$)
            if deslen% > 0% then ~
               call "DESCRIBE" (#1, key1$,descr$,0%,f1%)    ~
            else ~
                descr$ = " "
End_ListBox: REM *** Restore Screen Cursor ***
            call "SENDCMD" (uw$ & "UWSETUP3,0" & uw$)
            goto exit_routine

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1984, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

exit_routine
            if f1% = 1% then key$ = key1$
            if str(descr$,,1%) = hex(06) or f1% = 0% then descr$ = " "
            if f1% > 0% and prname$ = "GLMAIN" then call "GLFMT" (key$)

            end
