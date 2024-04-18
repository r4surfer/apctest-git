
        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLN9B                             *~ 
            *  Creation Date     - 03/26/96                             *~
            *  Last Modified Date- 05/07/2019                           *~
            *  Written By        - J. Browning Fields                   *~
            *  Description       - This Program returns a sort key for  *~
            *                      Production report programs.          *~
            *                                                           *~
            *  Special Comments  - Subroutine                           *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/26/96 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 04/11/97 ! Mod to Fix Problem when SORT_KEY$ equal  ! RHH *~
            *          !   to (30) or Greater than 60             !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 02/02/98 ! Mod to Support the new Wood Surround     ! RHH *~
            *          ! Codes.                                   !     *~
            * 04/30/98 ! Mods for new Sub (COMPRESS) that packs   ! RHH *~
            *          !   40 Char's in 30 Char's Space use for   !     *~
            *          !   Sort Index area in 'APCPLNDT'          !     *~
            * 05/25/98 ! (EWD001) Mod to Put Parts at Beginning   ! RHH *~
            * 10/08/98 ! (EWD002) Mod to add New Sort code 'T'    ! RHH *~
            *          !   Same as "2" with Special Features      !     *~
            *          !   Applicable only to Wood Surround Prod  !     *~
            * 10/29/98 ! (EWD003) Mod to wood_descend routine     !     *~
            * 12/12/98 ! (EWD004) Mod to Change sort from 12 to 15! RHH *~
            *          !   characters.                            !     *~
            * 10/18/00 ! (EWD005) Mod to put Tempered Glass 1st   ! RHH *~       
            * 10/31/02 ! (EWD006) Mod to put Sunclean Glass 2nd   ! CMG *~
            * 06/24/03 ! (EWD007) Mod to exclude certain wood code! CMG *~         
            *          !          from wood surround sort.        !     *~  
            * 10/30/03 ! (EWD008) Mod to resort glass by          ! CMG *~
            *          !          'GLASSSORT' table.              !     *~
            * 04/28/04 ! (EWD009) Mod to reverse the logic for the! CMG *~
            *          !          'PLANFRONT' woodsurround table  !     *~
            *          !           to make entry easier.          !     *~
            * 08/25/04 ! (AWD010) Mod to take out compression     ! CMG *~
            * 02/02/05 ! (AWD011) Mod for new sort PRE-CUT GLASS  ! CMG *~
            *11/02/2009! (AWD012) Mod for new sort Lowe366 GLASS  ! CMG *~
            *04/04/2011! (AWD013) mod to reverse Lowe366 glass    ! CMG *~
            *06/29/2011! (AWD014) Change meaning of Code W        ! CMG *~
            *02/18/2013! (AWD015) mod for foam sort               ! CMG *~
            *05/07/2014! (IM8061) mod for spacer                  ! CMG *~
            *12/22/2015! (SR67154) mods for NFRC 2016             ! CMG *~
            *06/05/2018! (CR1528) Add casing, SDL, applied fin to ! RDB *~
            *          !          to woodsurround (sort code 4)   !     *~
            *05/07/20191 (CR2013) Flange / Fin sort Series 130    ! CMN *~
            *************************************************************

        sub "APCPLN9B"   (sort_data$,    /* Planning Sort Data Flag    */~
                          sort_code$,    /* Planning Sort Code         */~
                          sort_rec$,     /* APCPLNDT Sort Record       */~
                          bcksubpt_rec$, /* BCKSUBPT Sort Record AWD015*/~
                          sort_key$,     /* Reporting Sort Key         */~
                          #1)            /* GENCODES                   */

        dim sort_data$1,                 /* Planning Sort Data Flag    */~
            sort_code$15, sort_value$1,  /* Planning Sort Code         */~
            sort_key$60,                 /* Reporting Sort Key         */~
            sort_rec$256,                /* APCPLNDT - DETAIL RECORD   */~
            bcksubpt_rec$256,            /* BCKSUBPT - Srt Rec (AWD015)*/~
            foam$1,                      /* Foam - (AWD015)            */~
            dt_seq$5,                    /* Seq    (AWD015)            */~
            pd$40,                       /* pad Characters '0'         */~
            dt_in$40,                    /* Uncompressed Sort Data     */~
            dt_out$30,                   /* Compressed Sort Data       */~
            errormsg$40,                 /* Error Display              */~ 
            readkey$24, gls$2,           /* GENCODES File Read Key     */~
            desc$,                       /* GENCODES Description       */~
            width$4,                     /* Width  Variable for Ascend */~
            height$3,                    /* Height Variable for Ascend */~
            wood$3,                      /* Wood/S Variable for Ascend */~
            co_or$2,                     /* Cot/Or Variable            */~
            seq$5,                       /* Special Sort (EWD002)      */~ 
            table_code$50,               /* Table Code Value String    */~
            table_rec$(50%)128,          /* Table Array for Code/Pos.  */~
            been_here_before$1,          /* Flag for array loaded      */~
            part$25,                     /* Part Number  (EWD009)      */~
            apcwood_code$3,              /* Wood Surround Code         */~
            sub_part$20,                 /* Sub Part No.       (CR1528)*/~
            spacer$1,                    /* (IM8061) spacer            */~
            flange$1,                    /* (CR2013) flange            */~
            rank$1                       /* (CR2013) flange rank       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Planning Create Sort Index Code   "
            pname$ = "APCPLN9B - Rev: R7.00"
            pd$ = "0000000000000000000000000000000000000000"

        REM *************************************************************
        REM MAT F2% = CON

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! GENCODES ! System Code Table File                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            init(" ") sort_key$, readkey$, sort_value$
            i% = 1% : k% = 1%

        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *-----------------------------------------------------------*~
            * Processes data from calling program to produce key.       *~
            *************************************************************
        REM BUILD_KEY
            if  been_here_before$   = "Y"       then L00840
                been_here_before$   = "Y"
                readkey$            =  all(hex(20))
                str(readkey$,1%,9%) = "PLAN SORT"
                gosub dataload

L00840:     init(" ") dt_seq$
            dt_seq$ = str(sort_rec$,111,5)
            
            for j% = 1% to 15%                   /* (EWD004) Change Max */
                sort_value$ = str(sort_code$,j%,1%)
                if sort_value$  > " " then L00920
                     j% = 15% : goto L01030      /* (EWD004) Force End  */

                                                 /* (EWD005) - Remove   */
        REM Skip Seq. # ("3") in Sort key if for Data ("0") (vs. Report)
        REM       if sort_value$ <> "3" then L00920
        REM          if sort_data$ = "0" then L01030
                                                 /* (EWD005) - Remove   */
 
L00920:         i% = pos(table_code$ = sort_value$)
                p$ = str(table_rec$(i%),36%,3%)
                convert p$ to p%,               data goto L00960

L00960:         l$ = str(table_rec$(i%),40%,2%)
                convert l$ to l%,               data goto L00990

L00990:         gosub process_sort_key

                k% = k% + l%

L01030:     next j%

            goto exit_sub

        process_sort_key
            rh% = pos("BCKDEQTVW4XYZ*/@" = sort_value$)       /* (EWD002)  */
                                            /* Added Code 'W' (EWD006)  */
                                            /* Added Code '4' (EWD007)  */
                                            /* Added Code 'X' (EWD008)  */
                                            /* Added Code 'Y' (AWD011)  */
                                            /* Added Code 'Z' (AWD012)  */
                                            /* Added Code '*' (AWD015)  */
                                            /* Added Code '/' (IM8061)  */
                                            /* Added Code '@' (CR2013)  */
            on rh% goto descend_width, descend_height, descend_wood,     ~
                             set_cottage, set_oriel, set_parts,          ~
                             set_special_sort, set_tempered, /* (EW5005)*/ ~
                             set_lowe366_front,              /* (AWD014)*/ ~
                             set_wood_beg,                   /* (EWD006)*/ ~
                             set_glass,                      /* (EWD008)*/ ~
                             set_precut,                     /* (AWD011)*/ ~  
                             set_lowe366_back,               /* (AWD012)*/ ~
                             set_foam_back,                  /* (AWD015)*/ ~
                             set_spacer,                     /* (IM8061)*/ ~
                             set_fin_flange                  /* (CR2013)*/                             

        REM Skip to byte 61 of Sort Key if next field crosses byte 60
            if k% + l%               > 60% then k% = (60% - l%) + 1%
            str(sort_key$,k%,l%)     = str(sort_rec$,p%,l%)
        return

        descend_width
            width$ = str(sort_rec$,p%,l%)
            convert width$ to width%, data goto L01230

L01230:     width% = 9999 - width%
            convert width% to width$, pic(0000)

            if k% + l%               > 60% then k% = (60% - l%) + 1%
            str(sort_key$,k%,l%) = width$
        return

        descend_height
            height$ = str(sort_rec$,p%,l%)
            convert height$ to height%, data goto L01340

L01340:     height% = 999 - height%
            convert height% to height$, pic(000)

            if k% + l%               > 60% then k% = (60% - l%) + 1%
            str(sort_key$,k%,l%) = height$
        return
                                          /* (EWD003) - Begin          */
        descend_wood                      /* Mod for New Wood Surround */
            wood$ = str(sort_rec$,p%,l%)  /* Codes. A00 thru Z00       */
            seq% = 0%
            if wood$ = "000" then goto L01490
               seq$ = str(sort_rec$,106%,5%)
               convert seq$ to seq%, data goto L01345
L01345:                                   /* Use Product sort Seq      */  
               if seq% > 999% then seq% = 998%
               goto L01495

L01490:     seq% = 999%                     /* Not Wood Surround       */ 
L01495:     convert seq% to wood$, pic(000)

            if k% + l%               > 60% then k% = (60% - l%) + 1%
            str(sort_key$,k%,l%) = wood$
        return
                                           /* (EWD003) - End           */ 

        set_cottage
            readkey$              =  all(hex(20))
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) =  str(sort_rec$,p%,l%)
            read #1,key = readkey$, using L01610 , co_or$, eod goto L01640
L01610:         FMT POS(25), CH(02)

            if  co_or$            = "CO" then L01650
L01640:         co_or$            = "00"
L01650:     if k% + l%               > 60% then k% = (60% - l%) + 1%
            str(sort_key$,k%,l%)  =  co_or$
        return

        set_oriel
            readkey$              =  all(hex(20))
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) =  str(sort_rec$,p%,l%)
            read #1,key = readkey$, using L01610 , co_or$, eod goto L01760

            if  co_or$            = "OR" then L01770
L01760:         co_or$            = "00"
L01770:     if k% + l%               > 60% then k% = (60% - l%) + 1%
            str(sort_key$,k%,l%)  =  co_or$
        return

        set_parts                          /* (EWD001) - 05/25/98    */
            str(sort_key$,k%,l%) = "9"     /* put parts at beginning */
            if str(sort_rec$,p%,l%) = "Y" then str(sort_key$,k%,l%) = "1"
        return  

        set_special_sort                   /* (EWD002) - 10/08/98    */
            wood$ = str(sort_rec$,217%,3%)  
            seq$  = str(sort_rec$,106%,5%) /* Prod Sort Seq. No.     */
            if wood$ = "000" then goto L01800
               goto L01820                 /* (Is) a Wood Surround   */  

L01800:     seq% = 0%                      /* Not wood Surround      */ 
            convert seq$ to seq%, data goto L01810

L01810:     seq1% = 65000% - seq%

            convert seq1% to seq$, pic(00000)

L01820:     str(sort_key$,k%,l%) = seq$
        return                             /* (EWD002)               */

        set_tempered                       /* (EWD005) - 10/18/00    */
            str(sort_key$,k%,1%) = "9"     /* Put Tempered Glass 1st */

            readkey$              =  all(hex(20))
            str(readkey$,1%,9%)   = "PLAN TEMP"
            str(readkey$,10%,15%) =  str(sort_rec$,p%,2%)
            read #1,key = readkey$, using L01610 , gls$, eod goto L01830

            if str(gls$,1%,1%) = "T" then str(sort_key$,k%,1%) = "1"
L01830: return  
                                           /* (EWD005)                */

        set_sunclean                       /* (EWD006) - 10/31/02    */
            str(sort_key$,k%,1%) = "9"     /* Put Sunclean Glass 2nd */

            readkey$              =  all(hex(20))
            str(readkey$,1%,9%)   = "PLAN SUNC"
            str(readkey$,10%,15%) =  str(sort_rec$,p%,2%)
            read #1,key = readkey$, using L01610 , gls$, eod goto L01870

            str(sort_key$,k%,1%) = "1"
L01870: return  
                                           /* (EWD006)                */

        set_wood_beg                       /* (EWD007) - 06/24/03    */
                                           /*   (EWD009)   -  Begin  */
            str(sort_key$,k%,1%) = "9"           
rem   Now extract the code from the part number...
           init(" ") part$, apcwood_code$

           part$ = str(sort_rec$,189%,25%)
           if len(part$) = 22% then apcwood_code$ = str(part$,20%,3%)     ~
                               else apcwood_code$ = str(part$,23%,3%)
           
rem   Make sure not Special Shape               
           if str(part$,7%,2%) > "99" and apcwood_code$ < "A00" then return
           
/* CR1528+  Add Casing, SDL and Applied Fin */
  
           sub_part$ = str(bcksubpt_rec$,48%,20%)       
         /* check for casing */
           if str(sub_part$,6%,1%) = "1" or str(sub_part$,6%,1%) = "2" ~
                then str(sort_key$,k%,1%) = "1"
                
         /* check for SDL */
           if str(sub_part$,8%,1%) = "1" then str(sort_key$,k%,1%) = "1"
           
         /* check for 8900 series all brands and part locks */         
           series$ = str(bcksubpt_rec$,169%,16%)
           gosub check_appl_nail
           if (str(part$,12%,1%) = "3" or str(part$,12%,1%) = "4") and   ~
              mdl8900% = 1% then str(sort_key$,k%,1%) = "1" 
             
/* CR1528 - */
rem   Make sure this is not an old Code
            if apcwood_code$ < "A00" then return
            if apcwood_code$ < "A00" and str(part$,7%,2%) < "A0" then return
REM         if str(sort_rec$,p%,1%) = "Y" then str(sort_key$,k%,1%) = "1"  ~
               else str(sort_key$,k%,1%) = "9"
                                               /*  (EWD009)    - END    */
REM            str(sort_key$,k%,1%) = "9"
               str(sort_key$,k%,1%) = "1"

            readkey$              =  all(hex(20))
            str(readkey$,1%,9%)   = "PLANFRONT"
            str(readkey$,10%,3%) =  str(sort_rec$,189%,3%)
            str(readkey$,13%,3%) =  apcwood_code$

            read #1,key = readkey$, using L01610 , gls$, eod goto L01880

REM         str(sort_key$,k%,1%) = "1"
            str(sort_key$,k%,1%) = "9"              /*  (EWD009) */

L01880: return  
                                           /* (EWD007)                */

        set_glass                          /* (EWD008) - 10/30/03    */
            str(sort_key$,k%,l%) = "ZZ"

            readkey$, gls$        =  all(hex(20))
            str(readkey$,1%,9%)   = "GLASSSORT"
            str(readkey$,10%,3%) =  str(sort_rec$,193%,2%)

            read #1,key = readkey$, using L01610 , gls$, eod goto L01920

            str(sort_key$,k%,l%) = str(gls$,1%,2%)
L01920: return  
                                           /* (EWD008)                */

        set_precut                         /* (AWD011) - 02/07/05    */
            str(sort_key$,k%,1%) = "9"

            gosub lookup_hinge
            if co_or$ = "CO" or co_or$ = "OR" then return


            if str(sort_rec$,42%,3%) = "033" then return  /* Not FGO */

            readkey$, desc$       =  all(hex(20))
            str(readkey$,1%,9%)   = "GLASSPRE1"
            str(readkey$,10%,3%)  =  str(sort_rec$,189%,3%)        /* Model    */
            str(readkey$,13%,2%)  =  str(sort_rec$,193%,2%)        /* Gls Code */
            str(readkey$,15%,4%)  =  str(sort_rec$,201%,4%)        /* Width    */
            str(readkey$,19%,3%)  =  str(sort_rec$,205%,3%)        /* Height   */
REM            call "SHOSTAT" (" Readkey -->  " & readkey$) 

            read #1,key = readkey$, using L01610 , readkey$, gls$, eod goto no_precut

                      str(sort_key$,k%,1%) = "1"
REM                      call "SHOSTAT" (" SortKey -->  " & str(sort_key$,k%,1%))

no_precut
        return  

/* (AWD012) set default to Z for end, if lowe366 by planultra table */
/*   in APCPLNDT pos 254, 1 then set to 0 for front loaded          */

        set_lowe366_back                                /* (AWD012) */
REM !            str(sort_key$,k%,l%) = "Z"             /* Lowe366 Sort */
REM !            if str(sort_rec$,p%,l%) = "1" then str(sort_key$,k%,l%) = "0"
            str(sort_key$,k%,l%) = "0"             /* Lowe366 Sort */
            if str(sort_rec$,p%,l%) = "1" then str(sort_key$,k%,l%) = "Z"
        return 

        lookup_hinge
            readkey$, co_or$      =  all(hex(20))
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) =  str(sort_rec$,197%,2%)         /* Hinge   */
            read #1,key = readkey$, using L01610 , co_or$, eod goto no_hinge
       no_hinge
       return
                                           /* (AWD011)                */

/* (AWD014)  */

        set_lowe366_front
            str(sort_key$,k%,l%) = "Z"             /* Lowe366 Sort */
            if str(sort_rec$,p%,l%) = "1" then str(sort_key$,k%,l%) = "0"
        return 
/* (AWD014\) */

/* (AWD015)  */

        set_foam_back
            str(sort_key$,k%,l%) = "0"             /* Foam Sort */
            foam$ = str(bcksubpt_rec$,p%,l%)
/* (SR67154) */           
REM            IF FOAM$ <> "0" THEN STR(SORT_KEY$,K%,L%) = "Z"
            if foam$ = "3" or foam$ = "4" then str(sort_key$,k%,l%) = "Z"
/* (\SR67154) */            
        return 
/* (AWD015\) */
/* (IM8061) */
        set_spacer
          spacer% = 1%
          spacer$ = str(bcksubpt_rec$,p%,l%)
          convert spacer$ to spacer%, data goto SpacerError
          
SpacerError:

          if spacer% = 0% then spacer% = 1%   /* Defualt to Tin */
          convert spacer% to spacer$, pic(0) 
                   
          str(sort_key$,k%,l%) = spacer$
        return
/* (IM8061\) */
/* + (CR2013) */
        set_fin_flange
            init(" ") rank$
            rank$ = "9"                /* Default Rank to End */
            flange% = 0%
            flange$ = str(bcksubpt_rec$,p%,1%)
            convert flange$ to flange%, data goto FlangeError
            
FlangeError:        
        
            if flange% < 4 then return
            if flange% > 7 then return
            
            if flange% = 6% then rank$ = "1"   /* Fin Only     */
            if flange% = 5% then rank$ = "2"   /* Flange w Fin */
            if flange% = 4% then rank$ = "3"   /* Flange Only  */
            if flange% = 7% then rank$ = "4"   /* No Flange/Fin*/
            
            str(sort_key$,k%,l%) = rank$            
        return
/* - (CR2013) */        
        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            read #1,key > readkey$, using L01890 , table_rec$(i%),         ~
                eod goto L02000
L01890:         FMT CH(128)

        REM STOP LOADING IF PAST PLAN_SORT IN GENCODES
            if str(table_rec$(i%),1%,9%) = str(readkey$,1%,9%) then L01960
                table_rec$(i%)           = all(hex(20))
                goto L02000

L01960:     str(table_code$,i%,1%)       = str(table_rec$(i%),10%,1%)
            readkey$                     = str(table_rec$(i%),1%,24%)
            i%                           = i% + 1%
            goto dataload
L02000: return

        REM *************************************************************~
            *                L O O K  U P  F I N                        *~
            *-----------------------------------------------------------*~
            * Set flag if series found to have fin       (CR1528)       *~
            *-----------------------------------------------------------*
        check_appl_nail
          mdl8900% = 0%
          init(" ") table$, genkey$, codeLen$, descr1$, descr2$, descr3$
          generr% = 0%
          table$ = "APPLNAIL"
          genkey$ = series$
          call "GENREAD" (table$, genkey$, descr1$, codeLen$, descr2$,  ~
                          descr3$, generr%)
            if generr% = 0% then mdl8900% = 1%
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_sub                             /* New Data Compression   */
            init(" ") dt_in$, dt_out$        /* Conversion of Index    */  
REM            dt_in% = 40% : dt_out% = 0%      /* Max Characters (40)    */
REM            dt_in$ = str(sort_key$,1%,40%)
REM            ln% = len(dt_in$)                /* Find out Actual length */
REM            ln1% = 40% - ln%                 /* Calc the No. of Pad    */
                                             /* Characters needed      */
REM            str(dt_in$,ln%+1%,ln1%) = str(pd$,1%,ln1%) 
REM            call "COMPRESS" (dt_in$,dt_in%,dt_out$,dt_out%, ret%)
REM            if ret% <> 0% then gosub L03000   /* Error has Occurred */
REM            init(" ") sort_key$
REM            str(sort_key$,1%,30%) = dt_out$  


        end
L03000: errormsg$ = "Sort Error - "& str(sort_rec$,24%,18%)
        call "SHOSTAT" (errormsg$) : stop
        dt_out$ = sort_key$
        return
