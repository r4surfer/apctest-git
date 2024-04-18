REM *THISPROGRAMWASGENERATEDUSINGGENPGMPROGRAMWHICHISPROPRIETARY*~
    *                                                           *~
    *  M   M  M   M  DDDD    AAA   TTTTT  FFFFF  IIIII  X   X   *~
    *  MM MM  MM MM  D   D  A   A    T    F        I     X X    *~
    *  M M M  M M M  D   D  AAAAA    T    FFFF     I      X     *~
    *  M   M  M   M  D   D  A   A    T    F        I     X X    *~
    *  M   M  M   M  DDDD   A   A    T    F      IIIII  X   X   *~
    *                                                           *~
    *-----------------------------------------------------------*~
    * MMDATFIX - Performs century date conversion on date fields*~
    *            for all files identified in MMDATMAP           *~
    *-----------------------------------------------------------*~
    *  This program contains valuable trade secrets and         *~
    *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
    *  embodying substantial creative efforts and confidential  *~
    *  information.  Unauthorized use, copying, decompiling,    *~
    *  translating, disclosure, or transfer of it is prohibited.*~
    *  Copyright (c) 1996, an unpublished work by CAELUS,       *~
    *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
    *-----------------------------------------------------------*~
    *                  M O D I F I C A T I O N S                *~
    *---WHEN---+----------------WHAT----------------------+-WHO-*~
    * 07/16/96 ! Original                                 ! DER *~
    * 06/17/97 ! Add Archive File processing use ARCHVREC ! DER *~
    * 09/03/97 ! Added Reverse (1000000) to xor           ! DXL *~
    * 11/19/97 ! Added INTDOC01 & INTDOC02 files.         ! DXL *~
    * 01/22/98 ! Use OpenChek on Sysfile2 & Archvrec      ! DER *~
    * 01/22/98 ! Change way var fields are loaded (array) ! DER *~
    PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVED

    dim                                                              ~
        aletter$1,                   /* "A" for key in mmlstdne    */~
        cursor%(2),                  /* Cursor location for edit   */~
        cnvrtses$3,                  /* "YES" or "NO"              */~
        date$8,                      /* Date for screen display    */~
        date1$8,                     /* First date found in INDOC0x*/~
        date2$8,                     /* Next date found in INDOC0x */~
        dbugmode$3,                  /* YES or anything else NO    */~
        dsp_files$12,                /* Display 'Files - nnnn'     */~
        dt_ch02$2,                   /* Date 2 character           */~
        dt_ch04$4,                   /* Date 4 character           */~
        dt_ch06$6,                   /* Date 6 character           */~
        dt_ch08$8,                   /* Date 8 character           */~
        dt_ch10$10,                  /* Date 10 character          */~
        dt_err$(12)80,               /* Date error msg holder      */~
        dt_feld$(100)16,             /* Date field names in DATMAP */~
        dt_frmt%(100),               /* Date field format - DATMAP */~
        dt_numb%(12),                /* Date field number holder   */~
        dt_rv06$6,                   /* Date reversed 6 character  */~
        dt_rv08$8,                   /* Date reversed 8 character  */~
        dt_strt%(100),               /* Date start pos. - DATMAP   */~
        edtmessage$79,               /* Edit screen message        */~
        errormsg$79,                 /* Error message              */~
        f8_name$8,                   /* 8 char file name           */~
        f8_stru$8,                   /* 8 char file structure      */~
        f16_stru$16,                 /* 16 char file structure     */~
        f_st_title$50,               /* File status title          */~
        files$(2000,2)16,            /* Files to process           */~
        file_name$8,                 /* Disk file name of Arch     */~
        file_nm$16,                  /* File name in DATMAP        */~
        file_stru$8,                 /* Structure of Arch file     */~
        fname$(2000)20,              /* dat file names to process  */~
        i$(24)80,                    /* Screen Image               */~
        in_lib$8,                    /* Database Library           */~
        in_vol$6,                    /* Database Volume            */~
        inpmessage$79,               /* Informational Message      */~
        lfac$(20)1,                  /* Field Attribute Characters */~
        line2$79,                    /* Screen Line #2             */~
        miss_files$(136)8,           /* Arch. files not in DB      */~
        miss_lines$(17)79,           /* Arch files missing screen  */~
        pf$(3)79,                    /* PF Screen Literals         */~
        pfkeyS$32,                   /* PF Key Hex Values          */~
        plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
        plowvf1$99,                  /* Plow key VF1 in SYSFILE2   */~
        plwarchiv$99,                /* Plow key for ARCHVREC      */~
        plwdatmap$99,                /* Plow key for DATMAP        */~
        prt_endtm$8,                 /* print file end time        */~
        prt_file$8,                  /* print Filename             */~
        prt_f_mess$(6)33,            /* print File messages        */~
        prt_f_stat%(6),              /* print accum file stats     */~
        prt_msg$33,                  /* print status message       */~
        prt_reckey$30,               /* print record key           */~
        prt_strtm$8,                 /* print file start time      */~
        prvlib$8,                    /* Previous entered library   */~
        prvvol$6,                    /* Previous entered volume    */~
        rec$(8)256,                  /* Miscellaneous Record holdr */~
        recusr$(8)256,               /* USERLCMS Record holdr      */~
        rec_key$30,                  /* record key in DATMAP       */~
        ses_ver$6,                   /* ses version in DATMAP      */~
        slash$1,                     /* "/" on UNIX "\" for NT     */~
        strt_file%(2),               /* Start file for SEARCH      */~
        time$8,                      /* well, what do ya think     */~
        userid$3,                    /* Current User Id            */~
        usr_prcnt%,                  /* User Update status percent */~
        vf1sys2$20,                  /* VF1 in SYSFILE2            */~
        vf_file$(200)8,              /* VarFld Files               */~
        vf_strn$(200)20              /* VarFld Strings id of date  */

    dim f2%(64),                     /* = 0 if the file is open    */~
        f1%(64),                     /* = 1 if READ was successful */~
        rslt$(64)20                  /* Text from file opening     */


rem *************************************************************~
    *                  Release Version ID Section               *~
    *************************************************************
    dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
rem *************************************************************

    mat f2% = con

rem *************************************************************~
    *   Warning to make a backup of datafiles first             *~
    *************************************************************
    keyhit% = 0%
    call "ASKUSER" (keyhit%, "*** Backup your data first! ***",            ~
       "A back-up of your data will provide recovery from a power outage.",~
       "Press PF9 if you have a back-up.",                                 ~
       "Any other function key will EXIT." )
    if keyhit% <> 9% then exit_program



REM *************************************************************~
    *                  S E L E C T   F I L E S                  *~
    *-----+----------+------------------------------------------*~
    *File#!  PRName  ! D E S C R I P T I O N                    *~
    *-----+----------+------------------------------------------*~
    * #01 ! MMDATMAP ! Data file for millie data conversion sys *~
    * #02 ! MMLSTDNE ! Status file of Database files fixed      *~
    * #10 ! SYSFILE2 ! Sysfile2                                 *~
    * #11 ! ARCHVREC ! Archive History Record File              *~
    * #50 ! INTDOC01 ! Special handling of OUTLIB file          *~
    * #51 ! INTDOC02 ! Special handling of OUTLIB file          *~
    * #63 ! USERLCMX ! New USERLCMS file reclen 600             *~
    * #64 ! < file > ! All .dat files in directory              *~
    *************************************************************~
    *       File Selection and Open Calls                       *~
    *************************************************************

   select #01, "MMDATMAP",                                      ~
          varc, indexed, recsize = 2000,                        ~
                         keypos  =    1, keylen =  46

   select #02, "MMLSTDNE",                                      ~
          varc, indexed, recsize =  116,                        ~
                         keypos  =    1, keylen =   1

   select #10, "SYSFILE2",                                      ~
          varc, indexed, recsize =  500,                        ~
                         keypos  =    1, keylen =  20

   select #11, "ARCHVREC",                                      ~
          varc, indexed, recsize =  150,                        ~
                         keypos  =    1, keylen =  30

   select #50, "INTDOC01",                                      ~
          varc, indexed, recsize = 1000,                        ~
                         keypos  =   25, keylen =  16,          ~
              alt key 1, keypos  =   17, keylen =  24,          ~
                  key 2, keypos  =    1, keylen =  40,          ~
                  key 3, keypos  =   41, keylen =  50, dup

   select #51, "INTDOC02",                                      ~
          varc, indexed, recsize =  256,                        ~
                         keypos  =   45, keylen =  31,          ~
              alt key 1, keypos  =    1, keylen =  75,          ~
                  key 2, keypos  =   23, keylen =  44,          ~
                  key 3, keypos  =   39, keylen =  28

   select #63, "USERLCMX",                                      ~
          varc, indexed, recsize =  600,                        ~
                         keypos  =    1, keylen =   3,          ~
              alt key 1, keypos  =    4, keylen =  30, dup

   select #64, "VAR_FILE",                                      ~
          varc, indexed, recsize = 2024,                        ~
                         keypos  =    1, keylen =  10


   rslt$(01%)  = "REQUIRED"

   call "SHOSTAT" ("Opening files, one moment please...")

   call "OPENOLIB" (#01, "SHARE", f2%(01%), rslt$(01%), "    " )


REM *************************************************************~
    *                I N I T I A L I Z A T I O N                *~
    *-----------------------------------------------------------*~
    * Initializes information necessary for program.            *~
    *************************************************************
    call "EXTRACT" addr("ID", userid$)
    date$ = date
    call "DATEFMT" (date$)
    edtmessage$  = "To Modify Displayed Values, Position Cursor" & ~
                   " to Desired Value & Press (RETURN)."

    str(line2$,62%) = "MMDATFIX: " & STR(CMS2V$,,8)

    f_st_title$     = "Millennium Date Conversion"
    aletter$        = "A"

    true%           = 1%
    false%          = 0%

    /* Some subs use zero to indicate success */
    f_success%      = 0%

    /* Handling of archive files                         */
    /* file structure and file name elements of files$() */
    /* f_stru is the file name in MMDATMAP that contains */
    /* the position of the dates for f_name file name    */
    /* example: GLDE(YYYY) is the disk file and its      */
    /* structure is that of GLDETAIL in this case        */
    /* files$(nnn, f_stru%) = "GLDETAIL" (structure)     */
    /* files$(nnn, f_name%) = "GLDE1994" (disk file)     */
    f_stru%         = 1%
    f_name%         = 2%

    /* Used to calc what element of fname$() gets replaced */
    /* as the structure name in files$() */
    what_ele%       = 0%

    /* will be set to how many variable fields need to be checked */
    max_var_fld%    = 0%

    /* no compile complaints */
    ses_rln%        = 0%

    /* debug mode 1=active        */
    de_bug% = false%

    slash$ = "/"


REM *************************************************************~
    *       I N P U T   M O D E   M A I N   P R O G R A M       *~
    *-----------------------------------------------------------*~
    * Handles normal input for data entry screens.              *~
    *************************************************************

inputmode
    gosub initialize_variables

    for fieldnr% = 1% TO 4%
      inp1a
        /* Default - Enables */
        gosub'051(fieldnr%)
        if enabled% = false% then inp1e

      inp1b
        /* Display - Accept  */
        gosub'101(fieldnr%, 1%)
        if keyhit%  =  1% then gosub startover
        if keyhit% <>  4% then       inp1d

      inp1c
        fieldnr% = max(1%, fieldnr% - 1%)
        gosub'051(fieldnr%)
        if enabled% = true% then inp1b
        if fieldnr% = 1% then inp1a
        goto inp1c

      inp1d
        if keyhit% = 16% and fieldnr% = 1% then exit_program
        if keyhit% <> 0%                   then inp1b

      inp1e
        /* Edit Field for Valid Entry */
        gosub'151(fieldnr%)
        if errormsg$ <> " " then inp1b
    next fieldnr%


REM *************************************************************~
    *        E D I T   M O D E   M A I N   P R O G R A M        *~
    *-----------------------------------------------------------*~
    * Handles operation of EDIT MODE for data entry screens.    *~
    *************************************************************

editpg1
    lastfieldnr% = 0%

    /* Display Screen - No Entry */
    gosub'101(0%, 2%)
    if keyhit%  =  1% then gosub startover
    if keyhit%  =  9% then gosub chk_archives
    if keyhit%  =  9% then       dataconvert
    if keyhit%  = 16% then       exit_program
    if keyhit% <>  0% then       editpg1


  edt1a
    fieldnr% = cursor%(1%) - 5%
    /* special handling */
    if fieldnr% > 1 then fieldnr% = fieldnr%-1%

    if fieldnr% > 3% then fieldnr% = fieldnr%-2%
    if fieldnr% < 1% OR fieldnr% > 4%  then editpg1
    if fieldnr% = lastfieldnr%         then editpg1

    /* Check Enables, Set Defaults */
    gosub'051(fieldnr%)
    
    if enabled% = false% then editpg1

  edt1b
    /* Display & Accept Screen     */
    gosub'101(fieldnr%, 2%)
    if keyhit%   =  1%  then gosub startover
    if keyhit%  <>  0%  then       edt1b

    /* Edit Field for Valid Entry  */
    gosub'151(fieldnr%)
    if errormsg$ <> " " then edt1b
    lastfieldnr% = fieldnr%
    goto edt1a


    /************************************************************~
    *   chk_archives                                            *~
    *-----------------------------------------------------------*~
    * Uses ARCHVREC to check if all archive files are present   *~
    *  in the selected directory.                               *~
    *    if all archive file are not present:                   *~
    *       displays those files and allows user to exit.       *~
    *    if all files are present, returns (keyhit still 9)     *~
    *       and proceeds to dataconvert                         *~
    ************************************************************/

chk_archives
    init( " " ) miss_files$(), file_nm$
    have_arch%  = false%
    miss_num%   = 0%

    /* sort the sucker */
    call "SORT" addr( fname$(), dim(fname$(),1%), len(str(fname$(1%))) )

    /* Open archvrec file  */
    call "OPENCHCK" (#11, f2%(11%), 0%, 0%, " " )
    /* if it did not open, get out of dodge */
    if f2%(11%) <> true% then exit_program

    have_arch%  = true%

    /* The search for missing archive files is on */
    plwarchiv$ = all( hex(00) )
    
  get_next_arch
    call "READ102" (#11, plwarchiv$, f1%(11%))
    if f1%(11%) = false% then chk_archives_any
    
    /* get the archive file data */
    get #11 using ARCHVREC_fmt, file_stru$, file_name$

    /* see if the archive file exists on disk */
    search fname$() = str(file_name$) to strt_file%() step len(str(fname$(1%)))

    /* if not on disk, add to MIA */
    if strt_file%(1%) = 0% then miss_num% = miss_num% + 1%
    if strt_file%(1%) = 0% then ~
       miss_files$(miss_num%) = str(file_name$, 1%, 8%)

    /* can only accommodate 136 missing files on screen */
    if miss_num% = 136% then chk_archives_any


    /* since there could be multiple parent and */
    /* diskfile entries, by-pass the others */
    plwarchiv$ = str(file_stru$, 1%, 8%) & ~
                 str(file_name$, 1%, 8%) & ~
                 all( hex(ff) )

    goto get_next_arch


  chk_archives_any
    /* if archive disk files missing, display them to user */
    /* if user wishes to continue anyway, keyhit will be 9 */
    /* and when returning to caller, will do the data convert */
    if miss_num% > 0% then gosub miss_arch_scrn

    close #11
return





    /************************************************************~
    *   dataconvert                                             *~
    *-----------------------------------------------------------*~
    * Provides user one last chance to exit, if continue, then  *~
    *  Converts all dates identified in files using mmdatmap    *~
    ************************************************************/


dataconvert
    keyhit% = 0%
    call "ASKUSER" (keyhit%, "*** Are you ABSOLUTELY sure? ***", ~
         "File conversion about to proceed.",                    ~
         "Confirm acceptance with PF25",                         ~
         "Any other function key will EXIT." )
    if keyhit% <> 25% then exit_program

    gosub prt_setup
    /* First Handle SES files, if specified.               */
    if cnvrtses$ = "NO " then archive_files
    call "SHOSTAT" ("Processing INTDOC01")
    prt_rectot% = 0%: prt_datfld% = 2%
    prt_reckey$ = " "
    call "OPENOLIB" (#50, "IO   ", f2%(50%), rslt$(50%), "    " )
    call "OPENOLIB" (#51, "IO   ", f2%(51%), rslt$(51%), "    " )
    prt_strtm$   = str( time, 1%, 6% )
    call "TIMEOK" (prt_strtm$, 0%, 0%)
    if f2%(50%) <> 0% then intdoc01_done
    plowkey$ = all(hex(00))
plow_intdoc01
    call "PLOWNXT1" (#50, plowkey$, 0%, f1%(50%))
    if f1%(50%) = 0% then intdoc01_done
    rec_ok% = 0%
    get #50 using intdoc01_fmt, date1$, date2$
    if date1$ >= " " then call "DATECONV" (date1$) else rec_ok% = rec_ok% + 1%
    if date2$ >= " " then call "DATECONV" (date2$) else rec_ok% = rec_ok% + 1%
    if rec_ok% < 2% then rewrite #50 using intdoc01_fmt, date1$, date2$
    goto plow_intdoc01

intdoc01_fmt: fmt pos(937), ch(6), ch(6)
    
intdoc01_done
    prt_file$ = "INTDOC01"
    if f2%(50%) = 0% then prt_msg$ = "Data converted." ~
       else prt_msg$ = "Error opening file."
    gosub prt_detail
    
    call "SHOSTAT" ("Processing INTDOC02")
    prt_strtm$   = str( time, 1%, 6% )
    call "TIMEOK" (prt_strtm$, 0%, 0%)
    if f2%(51%) <> 0% then done_ses
    plowkey$ = all(hex(00))
plow_intdoc02
    call "PLOWNXT1" (#51, plowkey$, 0%, f1%(51%))
    if f1%(51%) = 0% then done_ses
    rec_ok% = 0%
    get #51 using intdoc02_fmt, date1$, date2$
    if date1$ >= " " then call "DATECONV" (date1$) else rec_ok% = rec_ok% + 1%
    if date2$ >= " " then call "DATECONV" (date2$) else rec_ok% = rec_ok% + 1%
    if rec_ok% < 2% then rewrite #51 using intdoc02_fmt, date1$, date2$
    goto plow_intdoc02

intdoc02_fmt: fmt pos(202), ch(6), ch(6)

done_ses
    prt_file$ = "INTDOC02"
    if f2%(51%) = 0% then prt_msg$ = "Data converted." ~
       else prt_msg$ = "Error opening file."
    gosub prt_detail
    
    close #50
    close #51

archive_files

    /* To keep code generic and handle the archive files   */
    /* we will store the file structure file name and the  */
    /* actual file name of all files, then we will load    */
    /* from ARCHVREC the file structure and disk file name */
    /* See discussion at variable inits, just below file selects */

    mat redim files$(num_files%,2%)16

    for one_file% = 1% to num_files%
        files$(one_file%,f_stru%) = fname$(one_file%)
        files$(one_file%,f_name%) = fname$(one_file%)
    next one_file%

    /* Store Variable Field info */
    gosub get_varfld

    if have_arch%  = false% then go_speed_racer

    /* Open archvrec file  */
    call "OPENCHCK" (#11, f2%(11%), 0%, 0%, " " )
    /* if it did not open, get out of dodge */
    if f2%(11%) <> true% then go_speed_racer

    /* got the generic load done, now process ARCHVREC */
    plwarchiv$ = all( hex(00) )
    
  get_next_arch_file
    call "READ102" (#11, plwarchiv$, f1%(11%))
    if f1%(11%) = false% then close_arch_file
    
    /* get the archive file data */
    get #11 using ARCHVREC_fmt, file_stru$, file_name$

    /* see if the archive file exists on disk */
    search fname$() = file_name$ to strt_file%() step len(str(fname$(1%)))
    if strt_file%(1%) > 0% then what_ele% = ((strt_file%(1%)+19%)/20%)

    /* if it does, add the parent file to file structure */
    /* at same element                                   */
    if strt_file%(1%) > 0% then ~
       files$(what_ele%,f_stru%) = file_stru$ & all( hex(20) )

    /* set what_ele% to default */
    what_ele%  = 0%

    /* since there could be multiple parent and */
    /* diskfile entries, by-pass the others */
    plwarchiv$ = str(file_stru$, 1%, 8%) & ~
                 str(file_name$, 1%, 8%) & ~
                 all( hex(ff) )

    goto get_next_arch_file

  close_arch_file
    /* close ARCHVREC file */
    close #11


  go_speed_racer   /* sometimes difficulty with meaningful labels */
    
    /* initialize vars used in mmlstdne */
    file_nm$ = all( hex(00) )
    rec_key$ = all( hex(00) )
    break%   = 0%
    plowkey$ = all( hex(00) )

    /* set up file status vars */
    f_conv% = 1%
    f_done% = 2%
    f_xcon% = 3%
    f_ferr% = 4%
    f_xrec% = 5%
    f_nodt% = 6%
    prt_f_mess$(f_conv%) = "Data converted."
    prt_f_mess$(f_done%) = "Data already converted."
    prt_f_mess$(f_xcon%) = "No conversion specified."
    prt_f_mess$(f_ferr%) = "Error opening file."
    prt_f_mess$(f_xrec%) = "No records in file."
    prt_f_mess$(f_nodt%) = "File specified, but no dates."
    for x% = f_conv% to f_nodt%
        prt_f_stat%(x%) = 0%
    next x%


    /* setup-n-open mmlstdne.dat */
    gosub set_up_lstdne

    /* Check to see if re-start, lstdne_exist set in lstdne setup */
    start_file% = 1%
    if lstdne_exist% = f_success% then gosub find_last_file

    /* Process them files  */
    for one_file% = start_file% to num_files%

        f8_name$  = str( files$(one_file%,f_name%), 1%,  8% )
        f8_stru$  = str( files$(one_file%,f_stru%), 1%,  8% )
        f16_stru$ = str( files$(one_file%,f_stru%), 1%, 16% )

        /* store file structure to lookup dates */
        plwdatmap$   = str(f16_stru$) & all( hex(00) )

        /* init print vars  */
        /* store the disk file name */
        prt_file$    = f8_name$
        prt_reckey$  = all( hex(20) )
        prt_strtm$   = str( time, 1%, 6% )
        call "TIMEOK" (prt_strtm$, 0%, err%)
        prt_rectot%  = 0%
        prt_1stpas%  = true%
        chk_readfdr% = true%

      process_same_file     /* catch all occurs of same file in datmap */
        /* initialize record counters */
        record%      = 0%
        prt_recred%  = 0%
        prt_reccnv%  = 0%
        prt_datfld%  = 0%
        prt_mesnum%  = 0%

        /* Plow in mmdatmap for file  */
        call "PLOWNEXT" (#01, plwdatmap$, 16%, f1%(01%))
        /* Not found on 1st pass - cont. search  */
        if f1%(01%)    = false% and ~
           prt_1stpas% = true%  then prt_mesnum% = f_xcon%
        if f1%(01%)    = false% then goto process_next_file
        prt_1stpas%    = false%

          /* Load datmap data */
        get #01 using mmdatmap_fmt, file_nm$, rec_key$,   ~
                       break%, ses_ver$, ses_rln%,   ~
                       dt_strt%(), dt_frmt%(), dt_feld$()

        /* check to make sure there is some dates to process  */
        if max( dt_frmt%() ) <= 0% then prt_mesnum% = f_nodt%
        if max( dt_frmt%() ) <= 0% then goto process_next_file

        /* find out how many dates are in record layout  */
        how_mny_dt% = 0%
        gosub what_dates
        gosub find_vf1
        gosub prt_dates

        prt_reckey$ = rec_key$

        if chk_readfdr% = false% then goto no_readfdr

        /* get rec count, rec length, key position, key length  */
        call "READFDR" addr( f8_name$,                                    ~
                             in_lib$, in_vol$, 0%,                        ~
                             "RC", tot_recs%,                             ~
                             "RS", rec_len%,                              ~
                             "KP", key_pos%,                              ~
                             "KL", key_len%,                              ~
                             f2%(64%) )

        /* Did something ugly happen?  */
        if f2%(64%) <> f_success% then prt_mesnum% = f_ferr%
        if f2%(64%) <> f_success% then goto process_next_file

        prt_rectot% = tot_recs%

        /* Display file status */
        gosub filestatus

        /* if zero records next file  */
        if tot_recs% = 0% then prt_mesnum% = f_xrec%
        if tot_recs% = 0% then goto process_next_file
        chk_readfdr% = false%

      no_readfdr
        prt_reckey$ = rec_key$

        /* set-up file handle (channel) prior to opening file */
        gosub file_setup

        /* Open file  */
        call "WORKOPN2" (#64%, "IO   ", 200%, f2%(64%)) 
        if f2%(64%) <> f_success% then prt_mesnum% = f_ferr%
        if f2%(64%) <> f_success% then goto process_next_file

        /* calculate percentages for displaying filestatus */
        disp_per% = 1%
        if tot_recs% > 10% and tot_recs% < 100% then ~
           disp_per% = ( tot_recs%/10% ) * usr_prcnt%
        if tot_recs% > 99% then ~
           disp_per% = ( tot_recs%/100% ) * usr_prcnt%

        /* find out if there is a date in the key (1% = true) */
        dt_n_key% = false%
        gosub is_datekey

        /* setup plowkey and break - plowbrk% = key_len% */
        plowbrk% = 0%
        plowkey$ = all( hex(00) )
        if break%   <> 0%  then plowbrk% = break%
        if rec_key$ <> " " then plowkey$ = str( rec_key$, 1%, break% ) & ~
                                          ~all( hex(00) )

        /* special processing for userlcms file */
        if f8_name$ = "USERLCMS" then gosub procusrl
        if f8_name$ = "USERLCMS" and ~
           f2%(63%) <> f_success%  then goto process_next_file
        if f8_name$ = "USERLCMS" then goto  jst4usrl

        /* Check to see if the first rec is equal to the plowkey  */
        call "READ101" (#64%, plowkey$, f1%(64%))
        /* success proc-rec, otherwise flow thru to Plow for key greater */
        if f1%(64%) <> false% then goto chk_read

      plw_recs
        call "PLOWNXT1" (#64%, plowkey$, plowbrk%, f1%(64%))
      chk_read
        if f1%(64%) = true% then record% = record%+1%
        prt_recred% = record%
        if f1%(64%) = true% then gosub process_record
        if f1%(64%) = true% then goto plw_recs

        /* special for userlcms  */
      jst4usrl

        /* Done so close the file */
        close #64%
        /* Display file status  */
        gosub filestatus

        if prt_recred% = 0% then prt_mesnum% = f_xrec%
        if prt_recred% = 0% then goto no_chkconv

        if prt_reccnv% > 0% then prt_mesnum% = f_conv% ~
                            else prt_mesnum% = f_done%

        /* special handling for userlcms  */
        if prt_reccnv% > 0% and f8_name$ = "USERLCMS" then gosub rnamusrl

      no_chkconv
        gosub prt_file_done

        goto process_same_file

      process_next_file

        gosub prt_file_done

        gosub next_lstdne

    /* Next file */
    next one_file% 

    gosub prt_done
    gosub prt_clnup

goto exit_program



/************************************************************************/
/* Subroutines                                                          */
/************************************************************************/



rem ***************************************************************~
    *  Process record from data file                              *~
    ***************************************************************


process_record
    get #64%, str( rec$(), 1%, rec_len% )

    dt_fld% = 0%
    /* to make a check whether record has been prev. processed, find a   */
    /* valid date field (!12) since no date conv. (0) is one             */
    /* of the formats.  Generally this will be the first date field.     */
    for ch_fld% = 1% to how_mny_dt%
        if dt_frmt%(ch_fld%) <> 12% then dt_fld% = ch_fld%
        if dt_fld% > 0% then ch_fld% = how_mny_dt%+1
    next ch_fld%

    /* if no valid dates to check - push on (ie. no_process)  */
    if dt_fld% = 0% then goto no_process

    /* initialize the did_it var (did_it% = true% if already done) */
    did_it% = false%
    If dt_frmt%(dt_fld%) =  1% then gosub dt_chek01
    if dt_frmt%(dt_fld%) =  2% then gosub dt_chek02
    if dt_frmt%(dt_fld%) =  3% then gosub dt_chek03
    if dt_frmt%(dt_fld%) =  4% then gosub dt_chek04
    if dt_frmt%(dt_fld%) =  5% then gosub dt_chek05
    /* 6 here for clarity, but does nothing  */
    if dt_frmt%(dt_fld%) =  6% then gosub dt_chek06
    if dt_frmt%(dt_fld%) =  7% then gosub dt_chek07
    if dt_frmt%(dt_fld%) =  8% then gosub dt_chek08
    if dt_frmt%(dt_fld%) =  9% then gosub dt_chek09
    if dt_frmt%(dt_fld%) = 10% then gosub dt_chek10
    if dt_frmt%(dt_fld%) = 11% then gosub dt_chek11
    if dt_frmt%(dt_fld%) = 13% then gosub dt_chek13

    /* already processed record?, if so exit out */
    if did_it% = true% then no_process

    prt_reccnv% = prt_reccnv% + 1%

    if f8_name$ = "CALMASTR" then gosub proc_cal
    if f8_name$ = "CALMASTR" then goto wrt_record

    for dt_fld% = 1% to how_mny_dt%
        if dt_frmt%(dt_fld%) =  1% then gosub dt_conv01
        if dt_frmt%(dt_fld%) =  2% then gosub dt_conv02
        If dt_frmt%(dt_fld%) =  3% then gosub dt_conv03
        if dt_frmt%(dt_fld%) =  4% then gosub dt_conv04
        if dt_frmt%(dt_fld%) =  5% then gosub dt_conv05
        /* 6 here for clarity, but does nothing  */
        if dt_frmt%(dt_fld%) =  6% then gosub dt_conv06
        if dt_frmt%(dt_fld%) =  7% then gosub dt_conv07
        if dt_frmt%(dt_fld%) =  8% then gosub dt_conv08
        if dt_frmt%(dt_fld%) =  9% then gosub dt_conv09
        if dt_frmt%(dt_fld%) = 10% then gosub dt_conv10
        if dt_frmt%(dt_fld%) = 11% then gosub dt_conv11
        if dt_frmt%(dt_fld%) = 13% then gosub dt_conv13
    next dt_fld%

  wrt_record

    if dt_n_key% = true% then gosub nw_wrt else gosub re_wrt 

    /* Update data in mmlstdne  */
    if de_bug% = true% then gosub updt_lstdne

  no_process
    /* Display file status if at requested percent  */
    if mod( record%, disp_per% ) = 0% then gosub filestatus

return


nw_wrt
    Delete #64%
    write #64%, str( rec$(), 1%, rec_len% )
return


re_wrt
    rewrite #64%, str( rec$(), 1%, rec_len% )
return


filestatus
    call "FILESTAT" (f_st_title$, f8_name$, record%, tot_recs%)
return


proc_cal
    if str( plowkey$, 1%, 2% ) = "10" or str( plowkey$, 1%, 2% ) = "11" ~
       then gosub dt_cal_01
    if str( plowkey$, 1%, 2% ) = "20" then gosub dt_cal_07
return


rem ***************************************************************~
    *  Debugging file setup                                       *~
    ***************************************************************


set_up_lstdne
    /* check for file exists, f2 = 0 if exists */
    call "READFDR" addr( "mmlstdne", in_lib$, in_vol$, 0%,        ~ 
                         "RC", tot_recs%, f2%(02%) )

    /* lstdne_exist used to find starting point of files, if it exists */
    lstdne_exist% = f2%(02%)

    call "PUTPRNAM" addr(#02, "mmlstdne")
    call "PUTNAMES" addr(#02, "mmlstdne", in_lib$, in_vol$ )

    if f2%(02%) = f_success% then goto open_lstdne

    /* Create mmlstdne file  */
    call "WORKOPN2" (#02, "OUTSP", 1%, f2%(02%)) 
    if f2%(02%) <> f_success% then goto exit_program
    /* write a blank record so we can always re-write  */
    write #02 using MMLSTDNE_fmt, aletter$, file_nm$, plowkey$
    close #02

  open_lstdne
    call "WORKOPN2" (#02, "IO   ", 1%, f2%(02%)) 
    if f2%(02%) <> f_success% then goto exit_program
return


updt_lstdne
    call "READ101" (#02, aletter$, f1%(02%)) 
    /* re-write record  */
    if f1%(02%) = true% then ~
       rewrite #02 using MMLSTDNE_fmt, aletter$, file_nm$, plowkey$
return


next_lstdne
    if one_file% <> num_files% then ~
       file_nm$ = f8_name$     else ~
       file_nm$ = all( hex(00) )

    plowkey$ = all( hex(00) )
    call "READ101" (#02, aletter$, f1%(02%)) 
    /* re-write record  */
    if f1%(02%) = true% then ~
       rewrite #02 using MMLSTDNE_fmt, aletter$, file_nm$, plowkey$
return


find_last_file
    call "READ100" (#02, aletter$, f1%(02%))
    if f1%(02%) = true% then ~
       get #02 using MMLSTDNE_fmt, aletter$, file_nm$, plowkey$
    if f1%(02%) = true% then ~
       search fname$() = file_nm$ to strt_file%() step len(str(fname$(1%)))
    if strt_file%(1%) > 0% then start_file% = (strt_file%(1%)+15%)/16% 
return


file_setup
    call "PUTPRNAM" addr(#64%, f8_name$)
    call "PUTNAMES" addr(#64%, f8_name$, in_lib$, in_vol$ )
return


rem ***************************************************************~
    *  How many dates and are dates in key?                       *~
    ***************************************************************

what_dates
    for i% = 1% to 100%
        if dt_feld$(i%) <> " " then how_mny_dt% = how_mny_dt% + 1% ~
                               else i% = 101%
    next i%
return


get_varfld
    plowvf1$ = all( hex(00) )
    max_var_fld%    = 0%
    vf1_pos% = 0%
    /* Open sysfile2 file  */
    call "OPENCHCK" (#10, f2%(10%), 0%, 0%, " " )
    /* if it did not open, get out of dodge */
    if f2%(10%) <> true% then exit_program
    plowvf1$ = "VF1" & hex(3A) & all( hex(00) )

  get_varfld_nxt
    call "PLOWNEXT" (#10%, plowvf1$, 4%, f1%(10%))
    if f1%(10%) = false% then get_varfld_exit
    get #10% using sys2_fmt, vf1sys2$

    for is_dt% = 1% to 10%
        /* store only those files which have varfields that contain dates */
        if str( vf1sys2$, ((is_dt%*2%)-1%), 1% ) <> "D" then chk_nxt_dt_fld
            max_var_fld% = max_var_fld% + 1%
            vf_file$(max_var_fld%) = str( plowvf1$, 5%, 8% )
            vf_strn$(max_var_fld%) = vf1sys2$
            is_dt%       = 10%
      chk_nxt_dt_fld
    next is_dt%
    goto get_varfld_nxt

  get_varfld_exit
    close #10

return

sys2_fmt: FMT POS(52), CH(20)


find_vf1
    /* since we only stored those varfield files that contain dates search */
    /* for the file name, if found, get varfield dates, otherwise exit */
    for vf_file% = 1% to max_var_fld%
        if vf_file$(vf_file%) <> f8_stru$ then chk_4_nxt_vf
        vf1_pos% = 0%

        for i% = 1% to how_mny_dt%
            if dt_frmt%(i%) <> 12% then next_vf1
            vf1_pos% = dt_strt%(i%)
            vf1sys2$ = vf_strn$(vf_file%)

            for is_dt% = 1% to 10%
                if str( vf1sys2$, ((is_dt%*2%)-1%), 1% ) <> "D" then next_is_dt
                how_mny_dt% = how_mny_dt% + 1%
                dt_strt%(how_mny_dt%) = vf1_pos% + ((is_dt%-1%)*20%)
                dt_frmt%(how_mny_dt%) = 1%
              next_is_dt
            next is_dt%

          next_vf1
        next i%

      chk_4_nxt_vf
    next vf_file%
return


is_datekey
    For i% = 1% to how_mny_dt%
        if dt_strt%(i%) >= key_pos% and ~
           dt_strt%(i%) <= (key_pos%+key_len%)-1% then dt_n_key% = true%
        if dt_n_key% = true% then i% = how_mny_dt%+1%
    next i%
return



rem ***************************************************************~
    *  Date conversion checks to see if we already did conversion *~
    ***************************************************************

dt_chek01
    /* Std. Date Conversion                 From CH(6)    To PD(11,1) */
    if str( rec$(), dt_strt%(dt_fld%), 1% ) = hex(00) then did_it% = true%
return


dt_chek02
    /* Century Date Conversion               From CH(8)    To PD(11,1) */
    if str( rec$(), dt_strt%(dt_fld%), 1% ) = hex(00) then did_it% = true%
Return


dt_chek03
    /* YY to CCYY                            From CH(2)    To BI(2)    */
    dt_ch02%  = val( str( rec$(), dt_strt%(dt_fld%), 2% ), 2% )
    if dt_ch02% > 99% and dt_ch02% < 2100% then did_it% = true%
return


dt_chek04
    /* YYMM to CCYYMM                        From CH(4)    To BI(4)    */
    dt_ch04%  = val( str( rec$(), dt_strt%(dt_fld%), 4% ), 4% )
    if dt_ch04% > 9999% and dt_ch04% < 210000% and dt_ch04% <> 202020% then did_it% = true%
return


dt_chek05
    /* Formatted Date Conversion             From CH(8)    To PD(11,1) */
    if str( rec$(), dt_strt%(dt_fld%), 1% ) = hex(00) then did_it% = true%
return


dt_chek06
    /* Reverse (1000000) to xor              From CH(6)    To PD(11,1) */
    dt_rv06$  = str( rec$(), dt_strt%(dt_fld%), 6% ) xor hex(FFFFFFFFFFFF)
    if str( dt_rv06$, 1%, 1% ) = hex(00) then did_it% = true%
return


dt_chek07
    /* YY to CCYY                            From BI(4)    To BI(4)    */
    dt_bi04%  = val( str( rec$(), dt_strt%(dt_fld%), 4% ), 4% )
    if dt_bi04% > 99% and dt_bi04% < 2100% then did_it% = true%
return


dt_chek08
    /* YYMMDD to CCYYMMDD                    From BI(4)    To BI(4)    */
    dt_bi04%  = val( str( rec$(), dt_strt%(dt_fld%), 4% ), 4% )
    if dt_bi04% > 999999% and dt_bi04% < 21000000% then did_it% = true%
return


dt_chek09
    /* Reverse Date using XOR                From CH(6)    To PD(11,1) */
    dt_rv06$  = str( rec$(), dt_strt%(dt_fld%), 6% ) xor hex(FFFFFFFFFFFF)
    if str( dt_rv06$, 1%, 1% ) = hex(00) then did_it% = true%
return


dt_chek10
    /* Reverse Date using -999999            From CH(6)    To PD(11,1) */
    if str( rec$(), dt_strt%(dt_fld%), 1% ) = hex(00) then did_it% = true%
return


dt_chek11
    /* Reverse Date using -1000000           From CH(6)    To PD(11,1) */
    if str( rec$(), dt_strt%(dt_fld%), 1% ) = hex(00) then did_it% = true%
return


dt_chek13
    /*  YYMM  to CCYYMM                      From CH(6)    To CH(6)    */
    dt_ch06$ = str( rec$(), dt_strt%(dt_fld%), 6%)
    dt_ch06% = 0%
    convert dt_ch06$ to dt_ch06%, data goto dt_chek13_a
dt_chek13_a
    if dt_ch06% > 190000 then did_it% = true%
return


rem ***************************************************************~
    *  Date conversion rules (subs)                               *~
    ***************************************************************

dt_conv01
    /* Std. Date Conversion                 From CH(6)    To PD(11,1) */
    dt_ch08$  = str( rec$(), dt_strt%(dt_fld%), 6% )
    if str(dt_ch08$, 1%, 1%) = " " then dt_conv01_blnk
    call "DATEOKC"  ( dt_ch08$, dt_numb%(01%), dt_err$(01%) )
    if dt_err$(01%) <> " " then return
  dt_conv01_blnk
    call "DATUNFMT" ( dt_ch08$ )
    str( rec$(), dt_strt%(dt_fld%), 6% ) = dt_ch08$
return


dt_conv02
    /* Century Date Conversion               From CH(8)    To PD(11,1) */
    dt_ch10$  = str( rec$(), dt_strt%(dt_fld%), 8% )
    if str(dt_ch10$, 1%, 1%) = " " then dt_conv02_blnk
    call "DATEOKC"  ( dt_ch10$, dt_numb%(02%), dt_err$(02%) )
    if dt_err$(02%) <> " " then return
  dt_conv02_blnk
    call "DATUFMTC" ( dt_ch10$ )
    str( rec$(), dt_strt%(dt_fld%), 8% ) = dt_ch10$
return


dt_conv03
    /* YY to CCYY                            From CH(2)    To BI(2)    */
    dt_ch02$  = str( rec$(), dt_strt%(dt_fld%), 2% )
    dt_ch02%  = 0%
    convert dt_ch02$ to dt_ch02%, data goto dt_conv03_blnk
    if dt_ch02% > 20% then dt_ch02% = dt_ch02% + 1900% ~
                      else dt_ch02% = dt_ch02% + 2000%
  dt_conv03_blnk
    str( rec$(), dt_strt%(dt_fld%), 2% ) = bin( dt_ch02%, 2% )
return


dt_conv04
    /* YYMM to CCYYMM                        From CH(4)    To BI(4)    */
    dt_ch04$  = str( rec$(), dt_strt%(dt_fld%), 4% )
    dt_ch04%  = 0%
    convert dt_ch04$ to dt_ch04%, data goto no_yr_conv
    /* check to see if data is just months, if so NO year conversion   */
    if dt_ch04% >= 1% and dt_ch04% <= 13 then goto no_yr_conv
    if dt_ch04% > 2000% then dt_ch04% = dt_ch04% + 190000% ~
                        else dt_ch04% = dt_ch04% + 200000%
  no_yr_conv
    str( rec$(), dt_strt%(dt_fld%), 4% ) = bin( dt_ch04%, 4% )
return


dt_conv05
    /* Formatted Date Conversion             From CH(8)    To PD(11,1) */
    dt_ch08$  = str( rec$(), dt_strt%(dt_fld%), 8% )
    if str(dt_ch08$, 1%, 2%) = "  " then dt_conv05_blnk
    call "DATEOKC"  ( dt_ch08$, dt_numb%(05%), dt_err$(05%) )
    if dt_err$(05%) <> " " then return
  dt_conv05_blnk
    call "DATUNFMT" ( dt_ch08$ )
    str( rec$(), dt_strt%(dt_fld%), 6% ) = dt_ch08$
return


dt_conv06
    /* Reverse Date 1000000 to xor           From CH(6)    To PD(11,1) */
    dt_rv06$  = str( rec$(), dt_strt%(dt_fld%), 6% )
    dt_rv06% = 0%
    convert dt_rv06$ to dt_rv06%, data goto dt_conv06_a
dt_conv06_a
    dt_rv06% = 1000000% - dt_rv06%
    convert dt_rv06% to dt_rv08$, pic(00000000)
    call "DATECONV" ( dt_rv08$ )
    dt_rv06$ = str(dt_rv08$,1%,6%) xor hex(ffffffffffff)
    str( rec$(), dt_strt%(dt_fld%), 6% ) = dt_rv06$
return


dt_conv07
    /* YY to CCYY                            From BI(4)    To BI(4)    */
    dt_bi04%  = val( str( rec$(), dt_strt%(dt_fld%), 4% ), 4% )
    if dt_bi04% > 20% then dt_bi04% = dt_bi04% + 1900% ~
                      else dt_bi04% = dt_bi04% + 2000%
    str( rec$(), dt_strt%(dt_fld%), 4% ) = bin( dt_bi04%, 4% )
return


dt_conv08
    /* YYMMDD to CCYYMMDD                    From BI(4)    To BI(4)    */
    dt_bi04%  = val( str( rec$(), dt_strt%(dt_fld%), 4% ), 4% )
    if dt_bi04% > 200000% then dt_bi04% = dt_bi04% + 19000000% ~
                          else dt_bi04% = dt_bi04% + 20000000%
    str( rec$(), dt_strt%(dt_fld%), 4% ) = bin( dt_bi04%, 4% )
return


dt_conv09
    /* Reverse Date using XOR                From CH(6)    To PD(11,1) */
    dt_rv06$  = str( rec$(), dt_strt%(dt_fld%), 6% ) xor hex(FFFFFFFFFFFF)
    call "DATECONV" ( dt_rv06$ )
    str( rec$(), dt_strt%(dt_fld%), 6% ) = dt_rv06$  xor hex(FFFFFFFFFFFF)
return


dt_conv10
    /* Reverse Date using -999999            From CH(6)    To PD(11,1) */
    dt_rv06$  = str( rec$(), dt_strt%(dt_fld%), 6% )
    dt_rv06%  = 0%
    convert dt_rv06$ to dt_rv06%, data goto dt_conv10_blnk
  dt_conv10_blnk
    dt_rv06% = 999999% - dt_rv06%
    dt_rv08% = 99999999% - (19000000 + dt_rv06%)
    convert dt_rv08% to dt_rv08$, pic(00000000)
    call "DATECONV" ( dt_rv08$ )
    dt_rv06$ = str(dt_rv08$, 1%, 6%)

    str( rec$(), dt_strt%(dt_fld%), 6% ) = dt_rv06$
return


dt_conv11
    /* Reverse Date using -1000000           From CH(6)    To PD(11,1) */
    dt_rv06$  = str( rec$(), dt_strt%(dt_fld%), 6% )
    dt_rv06%  = 0%
    convert dt_rv06$ to dt_rv06%, data goto dt_conv11_blnk
  dt_conv11_blnk
    dt_rv06% = (1000000 - dt_rv06%) + 19000000
    dt_rv06% = 100000000 - dt_rv06%
    convert dt_rv06% to dt_rv08$, pic(00000000)
    call "DATECONV" ( dt_rv08$ )
    str( rec$(), dt_strt%(dt_fld%), 6% ) = str( dt_rv08$, 1%, 6% )
return


dt_conv13
    /*  YYMM  to CCYYMM                      From CH(6)    To CH(6)    */
    dt_ch06$ = str( rec$(), dt_strt%(dt_fld%), 6%)
    dt_rv06%  = 0%
    convert dt_ch06$ to dt_ch06%, data goto dt_conv13_blnk
  dt_conv13_blnk
    convert (dt_ch06%+190000) to dt_ch06$, pic(000000)
    str( rec$(), dt_strt%(dt_fld%), 6%) = dt_ch06$
return
    

dt_cal_01
    recoffset% = dt_strt%(1%)
    for caldt% = 1% to 245%
       /* Std. Date Conversion                 From CH(6)    To PD(11,1) */
       dt_ch08$  = str( rec$(), recoffset%, 6% )
       call "DATEFMT"  ( dt_ch08$ )
       call "DATUNFMT" ( dt_ch08$ )
       str( rec$(), recoffset%, 6% ) = dt_ch08$
       recoffset% = recoffset% + 6%
    next caldt%
    prt_datfld% = 245%
return


dt_cal_07
    recoffset% = dt_strt%(1%)
    for caldt% = 1% to 490%
       /* YY to CCYY                            From BI(4)    To BI(4)    */
       dt_bi04%  = val( str( rec$(), recoffset%, 4% ), 4% )
       if dt_bi04% > 20% then dt_bi04% = dt_bi04% + 1900% ~
                         else dt_bi04% = dt_bi04% + 2000%
       str( rec$(), recoffset%, 4% ) = bin( dt_bi04%, 4% )
       recoffset% = recoffset% + 4%
    next caldt%
    prt_datfld% = 490%
return



rem ***************************************************************~
    *  Print subroutines                                          *~
    ***************************************************************


prt_setup
    /* setup printer */
    page% = 0%
    /* make starting line greater then max line to get a header */
    line% = prt_mxline% + 1%
    call "SETPRNT" ( " ", "MMDT", 0%, 0% )
    select printer
    time$ = time
    call "TIME" (time$)
return


prt_header  
    /* print footer if necessary  */
    if page% > 0% then gosub prt_footer
    /* print header info  */
    print page
    page% = page% + 1%
    line% = 8%

    print using prthd1, date$, time$, page%
    print using prthd2, in_vol$, in_lib$
    print
    print using prthd3
    print using prthd4
    print using prthd5
return


prt_footer  
    /* print footer info  */
    print
    print using prtft1
    print using prtft2, prt_f_stat%(f_conv%), ~
                        prt_f_stat%(f_done%), ~
                        prt_f_stat%(f_xcon%)
    print using prtft3, prt_f_stat%(f_xrec%), ~
                        prt_f_stat%(f_ferr%), ~
                        prt_f_stat%(f_nodt%)
return


prt_detail  
    prt_endtm$  = str( time, 1%, 6% )
    call "TIMEOK" (prt_endtm$, 0%, err%)

    line% = line% + 1%
    if line% > prt_mxline% then gosub prt_header

    /* print detail line   */
    if prt_rectot% > 0% ~
       then print using prtdta,      ~
                        prt_file$,   ~
                        prt_reckey$, ~
                        prt_strtm$,  ~
                        prt_endtm$,  ~
                        prt_rectot%, ~
                        prt_recred%, ~
                        prt_reccnv%, ~
                        prt_datfld%, ~
                        prt_msg$     ~
       else print using prtdtb,      ~
                        prt_file$,   ~
                        prt_reckey$, ~
                        prt_strtm$,  ~
                        prt_endtm$,  ~
                        prt_datfld%, ~
                        prt_msg$

    /* print codes, so clear it */
    prt_msg$ = all( hex(20) )
return


prt_done  
    prt_endtm$  = str( time, 1%, 6% )
    call "TIMEOK" (prt_endtm$, 0%, err%)
    line% = line% + 3%
    print
    print
    print using prtcmp, prt_endtm$
    /* prepare to print footer  */
    for x% = line% to prt_mxline%
        print
    next x%
    /* print footer   */
    gosub prt_footer
return


/* top row */
      /* 0        1         2         3         4         5         6    6 */
      /* 12345678901234567890123456789012345678901234567890123456789012345 */
/* bot row */
      /* 67890123456789012345678901234567890123456789012345678901234567890 */
      /* 6   7         8         9         0         1         2         3 */

prthd1: %Date: ########  Time: ########               Millennium Date File~
        ~ Conversion Results                                     Page: ###
prthd2: %Volume: ######  Library: ########

prthd3: %                                                              |------~
        ~  Records  ------|   Date
prthd4: %  File    Record Key (if any)               Start    Finish     Total~
        ~     Read  Convert  Fields  Status Messages
prthd5: %--------  ------------------------------  --------  --------  -------~
        ~  -------  -------  ------  --------------------------------------

prtdta: %########  ##############################  ########  ########  #######~
        ~  #######  #######     ###  #################################

prtdtb: %########  ##############################  ########  ########         ~
        ~                       ###  #################################

prtcmp: %*********************************************   Conversion Comple~
        ~ted at ########   *******************************************

prtft1: %Cummulative File Status
prtft2: %Files Converted  = ####      Files Already converted = ####          ~
        ~Files where no conversion was specified  = ####
prtft3: %Files no Records = ####      Files with Open Error   = ####          ~
        ~Files specified, but no dates to convert = ####


prt_clnup
    /* close printer */
    close printer
    call "SETPRNT" ( " ", "MMDT", 0%, 1% )
return


prt_dates
    prt_datfld% = 0
    /* how many actual dates are there? */
    for ch_fld% = 1% to how_mny_dt%
        if dt_frmt%(ch_fld%) >= 1% and dt_frmt%(ch_fld%) <= 11% and ~
           dt_frmt%(ch_fld%) <> 6% then prt_datfld% = prt_datfld% + 1%
    next ch_fld%
return


prt_file_done
    if prt_mesnum% = 0% then prt_file_done_exit
    prt_f_stat%(prt_mesnum%) = prt_f_stat%(prt_mesnum%)+1%
    prt_msg$ = prt_f_mess$(prt_mesnum%)
    gosub prt_detail

  prt_file_done_exit
return



rem ***************************************************************~
    *  userlcms subroutines                                       *~
    ***************************************************************

procusrl
   /* if rec_len > 400% then already been done */
   if rec_len% > 400% then return

   prt_datfld% = 4%

   /* create new file - usrlcmx */
   call "PUTPRNAM" addr(#63, "userlcmx")
   call "PUTNAMES" addr(#63, "userlcmx", in_lib$, in_vol$ )

   /* Create mmlstdne file */
   call "WORKOPN2" (#63, "OUTSP", tot_recs%, f2%(63%)) 
   if f2%(63%) <> f_success% then prt_mesnum% = f_ferr%
   if f2%(63%) <> f_success% then return

  plw_lcms
    call "PLOWNXT1" (#64%, plowkey$, plowbrk%, f1%(64%))
    if f1%(64%) = true% then record% = record%+1%
    prt_recred% = record%
    if f1%(64%) = true% then gosub process_lcms
    if f1%(64%) = true% then goto plw_lcms

    /* done processing/writing new records - so close file */
    close #63
return


process_lcms
    recusr$() = all( hex( 00 ) )

    get #64%, str( rec$(), 1%, rec_len% )

    /* from start to first date - last-modify-date */
    str( recusr$(),   1%,  44% ) = str( rec$(),   1%,  44% )

    /* last-modify-date field */
    /* Std. Date Conversion                 From CH(6)    To PD(11,1) */
    dt_ch08$  = str( rec$(), 45%, 6% )
    call "DATECONV"  ( dt_ch08$ )
    str( recusr$(),  45%, 6% ) = dt_ch08$

    /* from last-modify-date to start of logon from date  */
    str( recusr$(),  51%, 331% ) = str( rec$(),  51%, 331% )

    /* logon from date field   */
    /* Unique conversion                    From BI(3)    To PD(11,1) */
    dt_bi03% = val( str( rec$(), 383%, 3% ), 3% )
    convert dt_bi03% to dt_ch06$, pic(000000)
    call "DATECONV" ( dt_ch06$ )
    str( recusr$(), 383%, 6% ) = dt_ch06$

    /* logon to date field     */
    /* Unique conversion                    From BI(3)    To PD(11,1) */
    dt_bi03% = val( str( rec$(), 386%, 3% ), 3% )
    convert dt_bi03% to dt_ch06$, pic(000000)
    call "DATECONV" ( dt_ch06$ )
    str( recusr$(), 389%, 6% ) = dt_ch06$

    /* from logon to date to start of last logon date  */
    str( recusr$(), 395%,   6% ) = str( rec$(), 389%,   6% )

    /* last logon date field   */
    /* Unique conversion                    From BI(4)    To PD(11,1) */
    dt_bi03% = val( str( rec$(), 396%, 3% ), 3% )
    convert dt_bi03% to dt_ch06$, pic(000000)
    call "DATECONV" ( dt_ch06$ )
    str( recusr$(), 402%, 6% ) = dt_ch06$

    /* from last logon date to previous record end */
    str( recusr$(), 408%,   1% ) = str( rec$(), 400%,   1% )

    /* blank to end of new record  */
    str( recusr$(), 409%, 192% ) = " "

    prt_recred%  = prt_recred% + 1%
    prt_reccnv%  = prt_reccnv% + 1%

    write #63, str( recusr$(), 1%, 600% )

return


rnamusrl
   /* rename userlcmx to userlcms */
   call "SCRATCH" addr( "F", "userlcms", in_lib$, in_vol$, ~
                                "B", " ", f2%(64%) )
   if f2%(64%) <> f_success% then return                                

   /* rename userlcmx to userlcms  */
   call "RENAME" addr( "F", "userlcmx", in_lib$, in_vol$, ~
                            "userlcms", " ", "B", " ", " ", f2%(63%) ) 

   if f2%(63%) <> f_success% then prt_mesnum% = f_ferr%
return



REM *************************************************************~
    *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
    *-----------------------------------------------------------*~
    * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
    *************************************************************

deffn'051(fieldnr%)
    enabled% = true%
    on fieldnr% gosub df101,         /* Database Volume        */~
                      df102,         /* Update Status          */~
                      df103,         /* dbugmode               */~
                      df104          /* convert ses files      */
return


/* Def/Enable Database Volume/Library    in_vol$/in_lib$ */
df101
    if in_vol$ = " " then call "EXTRACT" addr( "IV", in_vol$ )
    if in_lib$ = " " then call "EXTRACT" addr( "IL", in_lib$ )
return


/* Def/Enable File Update Status (in %)   usr_prcnt% */
df102
    if usr_prcnt% < 1% or usr_prcnt% > 100% then usr_prcnt% = 5%
return


/* Def/Enable dbugmode$ also sets de_bug% */
df103
    de_bug% = false% 
    if dbugmode$ = "YES" then de_bug% = true% else dbugmode$ = "NO "
return


/* Def/Enable cnvrtses$                   */
df104
    if cnvrtses$ <> "YES" then cnvrtses$ = "NO "
return


rem *************************************************************~
    *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
    *-----------------------------------------------------------*~
    * Initializes Variable Field Input Messages.                *~
    *************************************************************

deffn'050(scrnr%, fieldnr%)
    if fieldnr% <> 0% then im000
    inpmessage$ = edtmessage$
return


/* Define the Input Message for the Screen/Field Indicated */
im000
    if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
    read inpmessage$
return


scrn1_msg
    data  ~
        "Enter Database Volume/Library (Must be filled in and valid) ",~
        "Enter File Update Status (between 1 and 100 percent)   ",~
        "Enter YES for debugging operation                      ",~
        "Enter NO unless doing final conversion before going live."


rem *************************************************************~
    * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
    *-----------------------------------------------------------*~
    * Initializes all defined screen variables to blank         *~
    *************************************************************

initialize_variables
    INIT(" ") errormsg$, inpmessage$, in_lib$, in_vol$, dsp_files$, ~
              prvlib$, prvvol$, dbugmode$, vf_file$(),  vf_strn$()

    call "EXTRACT" addr( "IV", in_vol$, "IL", in_lib$ )
    miss_num%   =  0%
    have_arch%  = false%
    num_files%  =  0%
    prt_mxline% = 55%
return


REM *************************************************************~
    * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
    *-----------------------------------------------------------*~
    * Gives User the ability to START OVER when s/he wants to   *~
    * or will return User back to where they were.  Must push   *~
    * two buttons to start over for safety.                     *~
    *************************************************************

startover
    u3% = 2%
    call "STARTOVR" (U3%)
    if u3% = 1% then startover_exit

    return clear all
    goto inputmode

  startover_exit
return


REM *************************************************************~
    *           L O A D   D A T A   F R O M   F I L E           *~
    *-----------------------------------------------------------*~
    * Loads data from File Record Area into Program Variables.  *~
    *************************************************************
/*  dataload */
/*  return */


REM *************************************************************~
    *          S T U F F   D A T A   I N T O   F I L E          *~
    *-----------------------------------------------------------*~
    * Stuffs data from Program Variables into File Record Area. *~
    *************************************************************
/*  dataput */
/*  return */


REM *************************************************************~
    *        F O R M A T    S T A T E M E N T S                 *~
    *-----------------------------------------------------------*~
    * FORMAT Statements for Data Files.                         *~
    *************************************************************

ARCHVREC_fmt: FMT         /* FILE: ARCHVREC                          */~
          CH(8),          /* Parent file of Archive                  */~
          CH(8)           /* Archive file name                       */~


MMDATMAP_fmt: FMT         /* FILE: MMDATMAP                          */~
          CH(16),         /* Name of the file to convert.            */~
          CH(30),         /* Holds Key value for current record or He*/~
          BI(2),          /* Key length for current record           */~
          CH(6),          /* SES Version                             */~
          BI(2),          /* Length of the records to convert        */~
          100*BI(2),      /* Start position for each date field      */~
          100*BI(1),      /* Code for the conversion for each date fi*/~
          100*CH(16),     /* Name of each date field to be converted */~
          CH(44)          /* Unused Space                            */~


MMLSTDNE_fmt: FMT         /* FILE: MMLSTDNE                          */~
          CH(1),          /* "A" in key                              */~
          CH(16),         /* Name of the file to convert.            */~
          CH(99)          /* plow key of curr record                 */~


rem *************************************************************~
    *               S C R E E N   P A G E   1                   *~
    *-----------------------------------------------------------*~
    * Document Input and Edit Screen.                           *~
    *************************************************************

deffn'101(fieldnr%, edit%)
    gosub'050(1%, fieldnr%)
    gosub set_pf1
    if fieldnr% > 0% then init(hex(8C)) lfac$()                ~
                     else init(hex(86)) lfac$()

    on fieldnr% gosub upr1,          /* Database Volume/Library */   ~
                      num1,          /* Update Status           */   ~
                      upr1,          /* debug mode              */   ~
                      upr1           /* convert ses files       */
    goto scr1a


/* upl1:     lfac$(fieldnr%) = hex(80)  :  return    Up / Low   */
upr1:     lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
num1:     lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */


scr1a
    accept                                                       ~
       at (01,02), "Millennium Date File Conversion",            ~
       at (01,66), "Today:",                                     ~
       at (01,73), fac(hex(8C)), date$                  , CH(08),~
       at (02,02), fac(hex(AC)), line2$                 , CH(79),~
       at (04,02), fac(hex(94)), errormsg$              , CH(79),~
                                                                 ~
       at (06,02), "Database Volume",                            ~
       at (06,30), fac(lfac$( 1)), in_vol$              , CH(06),~
                                                                 ~
       at (07,02), "Database Library",                           ~
       at (07,30), fac(lfac$( 1)), in_lib$              , CH(08),~
                                                                 ~
       at (07,42), fac(hex(8C)),   dsp_files$,                   ~
                                                                 ~
       at (08,02), "File Update Status (in %)",                  ~
       at (08,30), fac(lfac$( 2)), usr_prcnt%         , PIC(###),~
                                                                 ~
       at (11,02), "Debugging Mode",                             ~
       at (11,30), fac(lfac$( 3)), dbugmode$            , CH(03),~
                                                                 ~
       at (12,02), "Convert SES Files?",                         ~
       at (12,30), fac(lfac$( 4)), cnvrtses$            , CH(03),~
                                                                 ~
       at (21,02), fac(hex(A4)),   inpmessage$          , CH(79),~
       at (22,02), fac(hex(8C)),   pf$(1)               , CH(79),~
       at (23,02), fac(hex(8C)),   pf$(2)               , CH(79),~
       at (24,02), fac(hex(8C)),   pf$(3)               , CH(79),~
                                                                 ~
              keys(pfkeys$), key(keyhit%)

       if keyhit%  = 13% or keyhit%  = 15% then gosub keyhit1315
       if keyhit%  = 13% or keyhit%  = 15% then       scr1a


       close WS
       call "SCREEN" addr ("C", U3%, "I", i$(), cursor%())
return


set_pf1
    /*  Input Mode             */
    if edit% = 2% then scr1f
    pf$(1) = "(1)Start Over                                                                  "
    pf$(2) = "                 (4)Previous Field                             (15)Print Screen"
    pf$(3) = "                                                               (16)Exit Program"
    pfkeys$ = hex(01FFFF04FFFFFFFFFFFFFFFFFFFF0F1000)

    if fieldnr% = 1% then scr1d
    str(pf$(3),64)    = " "
    str(pfkeys$,16,1) = hex(FF)

  scr1d
    if fieldnr% > 1% then set_pf1_exit
    str(pf$(2),18,26) = " "
    str(pfkeys$, 4,1) = hex(FF)
    goto set_pf1_exit

  scr1f
    /*  Edit Mode - Select Fld */
    if fieldnr% > 0% then scr1g
    pf$(1) = "(1)Start Over                                                                  "
    pf$(2) = "                                                               (15)Print Screen"
    pf$(3) = "                                          (9)Convert Data      (16)Exit Program"
    pfkeys$ = HEX(01FFFFFFFFFFFFFF09FFFFFFFFFF0F1000)
    goto set_pf1_exit

  scr1g
    /*  Edit Mode - Enabled    */
    pf$(1) = "(1)Start Over                                                                  "
    pf$(2) = "                                                               (15)Print Screen"
    pf$(3) = "                                                                               "
    pfkeys$ = HEX(01FFFFFFFFFFFFFFFFFFFFFFFFFF0FFF00)


  set_pf1_exit
return



    /* Screen Subroutine (Misc and Asundry) */
    /* Internal routine to handle PF 13's and 15's. */
    keyhit1315
        if keyhit% = 13% then call "MANUAL" ("MMDATFIX")
        if keyhit% = 15% then call "PRNTSCRN"
    return


rem *************************************************************~
    * Archive files missing screen                              *~
    *-----------------------------------------------------------*~
    * Allows user to view archive files missing from disk       *~
    *   They may PrtScr to restore and then repeat process      *~
    *or Continue processing anyway                              *~
    *************************************************************

miss_arch_scrn
    gosub set_pf2
    str( line2$, 1%, 36% ) = "Archive Files missing from directory"
    inpmessage$ = "Advise restoring from backup prior to continuing."

    /* set up screen display of missing files */
    for ln% = 1% to 17%
        miss_lines$(ln%) = miss_files$( (ln%-1%)+   1% ) & "  " & ~
                           miss_files$( (ln%-1%)+  18% ) & "  " & ~
                           miss_files$( (ln%-1%)+  35% ) & "  " & ~
                           miss_files$( (ln%-1%)+  52% ) & "  " & ~
                           miss_files$( (ln%-1%)+  69% ) & "  " & ~
                           miss_files$( (ln%-1%)+  86% ) & "  " & ~
                           miss_files$( (ln%-1%)+ 103% ) & "  " & ~
                           miss_files$( (ln%-1%)+ 120% )
    next ln%


  arch_scrn
    accept                                                       ~
       at (01,02), "Millennium Date File Conversion",            ~
       at (01,66), "Today:",                                     ~
       at (01,73), fac(hex(8C)), date$                  , CH(08),~
       at (02,02), fac(hex(AC)), line2$                 , CH(79),~
       at (03,02), "These are the files:",                       ~
                                                                 ~
       at (04,02), fac(hex(8C)), miss_lines$( 1%)       , CH(79),~
       at (05,02), fac(hex(8C)), miss_lines$( 2%)       , CH(79),~
       at (06,02), fac(hex(8C)), miss_lines$( 3%)       , CH(79),~
       at (07,02), fac(hex(8C)), miss_lines$( 4%)       , CH(79),~
       at (08,02), fac(hex(8C)), miss_lines$( 5%)       , CH(79),~
       at (09,02), fac(hex(8C)), miss_lines$( 6%)       , CH(79),~
       at (10,02), fac(hex(8C)), miss_lines$( 7%)       , CH(79),~
       at (11,02), fac(hex(8C)), miss_lines$( 8%)       , CH(79),~
       at (12,02), fac(hex(8C)), miss_lines$( 9%)       , CH(79),~
       at (13,02), fac(hex(8C)), miss_lines$(10%)       , CH(79),~
       at (14,02), fac(hex(8C)), miss_lines$(11%)       , CH(79),~
       at (15,02), fac(hex(8C)), miss_lines$(12%)       , CH(79),~
       at (16,02), fac(hex(8C)), miss_lines$(13%)       , CH(79),~
       at (17,02), fac(hex(8C)), miss_lines$(14%)       , CH(79),~
       at (18,02), fac(hex(8C)), miss_lines$(15%)       , CH(79),~
       at (19,02), fac(hex(8C)), miss_lines$(16%)       , CH(79),~
       at (20,02), fac(hex(8C)), miss_lines$(17%)       , CH(79),~
                                                                 ~
       at (21,02), fac(hex(A4)),   inpmessage$          , CH(79),~
       at (22,02), fac(hex(8C)),   pf$(1)               , CH(79),~
       at (23,02), fac(hex(8C)),   pf$(2)               , CH(79),~
       at (24,02), fac(hex(8C)),   pf$(3)               , CH(79),~
                                                                 ~
              keys(pfkeys$), key(keyhit%)

    if keyhit%  =  1%                   then gosub startover
    if keyhit%  = 13% or keyhit%  = 15% then gosub keyhit1315
    if keyhit%  = 13% or keyhit%  = 15% then       arch_scrn

    str( line2$, 1%, 36% ) = "                                    "

    close WS
    call "SCREEN" addr ("C", U3%, "I", i$(), cursor%())
return


set_pf2
    pf$(1) = "(1)Start Over                                                                  "
    pf$(2) = "                                                               (15)Print Screen"
    pf$(3) = "                                          (9)Convert Anyway    (16)Exit Program"
    pfkeys$ = HEX(01FFFFFFFFFFFFFF09FFFFFFFFFF0F1000)
return



REM *************************************************************~
    *                     T E S T   D A T A                     *~
    *-----------------------------------------------------------*~
    * Test data for the items on Screen 1.                      *~
    *************************************************************

deffn'151(fieldnr%)
    errormsg$ = " "
    on fieldnr% gosub td101,         /* Database Volume/Library */~
                      td102,         /* Update Status           */~
                      td103,         /* Debugging mode          */~
                      td104          /* Convert SES Files       */
return


/* Test for Database Volume/Library    in_vol$/in_lib$  */
td101
    if in_vol$   = " " then errormsg$ = "Volume must not be empty!"
    if in_lib$   = " " then errormsg$ = "Library must not be empty!" 
    if errormsg$ = " " and ( in_vol$ <> prvvol$ or ~
                             in_lib$ <> prvlib$ or ~
                             num_files% = 0% )  then gosub files_in_dir
    prvlib$    = in_lib$
    prvvol$    = in_vol$
return


/* Test for File Update Status (in %)    usr_prcnt%  */
td102
    if usr_prcnt% < 1% or usr_prcnt% > 100% then usr_prcnt% = 5
return


/* Test for debugging mode               dbugmode$ */
td103
    de_bug% = false%
    if dbugmode$ = "YES" then de_bug% = true% else dbugmode$ = "NO " 
return


/* Test if Converting SES Files          cnvrtses$ */
td104
    de_bug% = false%
    if cnvrtses$ = "YES" then return
    if cnvrtses$ = "NO " then return
    errormsg$ = "Enter YES or NO"
return


files_in_dir
    call "SHOSTAT" ("Locating database files, One moment please...")
    call "READVTOC" addr( "U", in_lib$, in_vol$, 1%, dim(fname$(),1%),     ~
                          len(str(fname$(1%))), fname$(1%), rc%, files% )
    /* presumably loaded files */
    errormsg$  = " "
    dsp_files$ = " "
    if rc% <> f_success% then ~
       errormsg$ = "Invalid Volume/Library or no files"
    if errormsg$ <> " " then goto end_files_in_dir
    /* let's get files$ into something manageable (DAT) */
    num_files% = 0%
    for eye% = (files%+1%) to dim(fname$(),1%)
        fname$(eye%) = all(hex(FF))
    next eye%
    for eye% = 1% to files%
        /* remove if not .DAT file */ 
        str_pos% = pos(-fname$(eye%) = ".")
        if str( fname$(eye%), str_pos%, 4% ) = ".dat" ~
           then fname$(eye%) = str( fname$(eye%), 1%, (str_pos%-1%) ) ~
           else fname$(eye%) = all(hex(FF))

        /* if files$ now blank, continue   */
        if str(fname$(eye%),1%,1%) = hex(FF) then goto next_dat_chk

        /* have a DAT file, so count it    */
        num_files% = num_files%+1%

        /* look for and cut out path seperator */
        str_pos% = pos(-fname$(eye%) = slash$ )
        if str_pos% = 0% then goto next_dat_chk
        fname$(eye%) = str( fname$(eye%), (str_pos% + 1%) ) 
        tran(fname$(eye%), "AaBbCcDdEeFfGgHhIiJjKkLlMm" & ~
                          ~"NnOoPpQqRrSsTtUuVvWwXxYyZz") replacing

      next_dat_chk
    next eye%

    errormsg$ = "No files in Volume/Library to process"
    if num_files% = 0% then goto end_files_in_dir

    /* we have files so clear errormsg$                       */
    errormsg$  = " " 
    dsp_files$ = "Files - "
    convert num_files% to str( dsp_files$, 9%, 4% ), pic(####)

  end_files_in_dir
return

rem THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAEUS,INC~
    *                          E X I T                          *~
    *-----------------------------------------------------------*~
    * Terminates execution (files closed automatically).        *~
    *-----------------------------------------------------------*~
    *  This program contains valuable trade secrets and         *~
    *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
    *  embodying substantial creative efforts and confidential  *~
    *  information.  Unauthorized use, copying, decompiling,    *~
    *  translating, disclosure, or transfer of it is prohibited.*~
    *  Copyright (c) 1996, an unpublished work by CAELUS,       *~
    *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
    CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

exit_program
    end

