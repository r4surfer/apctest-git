        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - ORACUST                              *~
            *  Creation Date     - 06/22/2011                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - David Speight                        *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *06/22/2018| CR1459 Update edi dealer nbr in CLNTCODEX! RDB *~
            *01/14/2019! CR1855 Add xref to Caelus Customer file  ! RDB *~
            *************************************************************
   
        sub "ORACUST" (#1,              /* S.O. No.                   */~
                       trans$,                                         ~
                       action$,                                        ~
                       file$,                                     ~
                       fields$(),                                       ~
                       no_fields%,                                     ~
                       error%)          /* Error Flag from File Open  */

        dim                              /*                            */~
            error$256,                   /* Error String               */~
            field$256,                   /* Query Return Field         */~
            stmt1$250,                   /* First Query String         */~
            stmt2$250,                   /* Second Query String        */~
            fields$(100)64,                                              ~
            rec$(6)200                   /* Customer file record       */

        dim edimsg$1,                   /* return code for AWDEDIAD   */~
            f31$1                       /* flag for call              */
        
        dim storexref$9                 /* moving to filler space     */
        
        dim message$256

            found = 1
            f31$ = "N"
            if action$   = "A" then goto add_record
            if action$   = "C" then goto change_record
            if action$   = "D" then goto delete_record
            goto FINI
  
change_record:
add_record:
            key$ = fields$(1%)  & "        "
            read #1, key = key$, hold, eod goto add_rec
             get #1, using L25000, rec$()
             goto set_field

L25000:         FMT 6*CH(200)
add_rec:    init(" ") rec$()
            str(rec$(),1,9) = key$ & "         "
            found = 0
set_field:
        for l% = 1% to no_fields%
        if fields$(l%) = "NULL" then fields$(l%) = "    "
        fx = 0
        if (l% > 2% and l% < 12%) and fields$(3%) <= "     " then  ~
               fx = 1
REM don't update if bill to name missing
            if (l% =  1%)              then str(rec$(),001,09) = fields$(l%)
            if (l% =  2%)              then str(rec$(),010,30) = fields$(l%)
            if (l% =  3% and fx = 0)   then str(rec$(),040,30) = fields$(l%)
            if (l% =  4% and fx = 0)   then str(rec$(),070,30) = fields$(l%)
            if (l% =  5% and fx = 0)   then str(rec$(),100,30) = fields$(l%)
            if (l% =  6% and fx = 0)   then str(rec$(),130,30) = fields$(l%)
            if (l% =  7% and fx = 0)   then str(rec$(),160,30) = fields$(l%)
            if (l% =  8% and fx = 0)   then str(rec$(),190,18) = fields$(l%)
            if (l% =  9% and fx = 0)   then str(rec$(),208,02) = fields$(l%)
REM         IF (L% = 10%)              THEN STR(REC$(),210,01) = FIELDS$(L%)
REM         IF (L% = 11% AND FX = 0)   THEN STR(REC$(),211,09) = FIELDS$(L%)
            if (l% = 11% and fx = 0)   then gosub reformat_zipcode1

            if (l% = 12%)              then gosub set_cusopen
REM         IF (L% = 13%)              THEN STR(REC$(),226,01) = FIELDS$(L%)
REM         IF (L% = 14%)              THEN STR(REC$(),227,01) = FIELDS$(L%)
            if (l% = 15%)              then str(rec$(),228,10) = fields$(l%)
REM         IF (L% = 16%)              THEN STR(REC$(),238,01) = FIELDS$(L%)
            IF (L% = 17%)              THEN STR(REC$(),239,11) = FIELDS$(L%)
REM         IF (L% = 18%)              THEN STR(REC$(),250,03) = FIELDS$(L%)
            if (l% = 19%)              then str(rec$(),253,30) = fields$(l%)
            if (l% = 20%)              then str(rec$(),283,30) = fields$(l%)
            if (l% = 21%)              then str(rec$(),313,30) = fields$(l%)
            if (l% = 22%)              then str(rec$(),343,30) = fields$(l%)
            if (l% = 23%)              then str(rec$(),373,30) = fields$(l%)
            if (l% = 24%)              then str(rec$(),403,18) = fields$(l%)
            if (l% = 25%)              then str(rec$(),421,02) = fields$(l%)
REM         IF (L% = 26%)              THEN STR(REC$(),423,01) = FIELDS$(L%)
            if (l% = 27%)              then gosub reformat_zipcode2
REM         IF (L% = 27%)              THEN STR(REC$(),424,09) = FIELDS$(L%)
            if (l% = 28%)              then str(rec$(),433,20) = fields$(l%)
            if (l% = 29%)              then str(rec$(),453,10) = fields$(l%)
REM CR1855  if (l% = 30%)              then str(rec$(),463,09) = fields$(l%)
/* CR1855 */
            if (l% = 30%)              then storexref$ = fields$(l%)
REM         IF (L% = 31%)              THEN STR(REC$(),472,09) = FIELDS$(L%)
/* CR 1459 */  
            if (l% = 31%) and str(fields$(1%),1%,2%) = "LB" and ~
                fields$(31%) <> " "          then f31$ = "Y"  
                   
REM         IF (L% = 32%)              THEN STR(REC$(),481,09) = FIELDS$(L%)
REM         IF (L% = 33%)              THEN STR(REC$(),490,09) = FIELDS$(L%)
REM         IF (L% = 34%)              THEN STR(REC$(),499,09) = FIELDS$(L%)
            if (l% = 35%)              then str(rec$(),508,09) = fields$(l%)
            if (l% = 36%)              then gosub set_discpercent
            if (l% = 37%)              then str(rec$(),525,01) = fields$(l%)
            if (l% = 38%)              then gosub set_crlimit
            if (l% = 39%)              then gosub set_lastmoddate
REM         IF (L% = 40%)              THEN STR(REC$(),540,03) = FIELDS$(L%)
REM 41 is terms
            if (l% = 41%)              then str(rec$(),543,20) = fields$(l%)
            if (l% = 42%)              then str(rec$(),563,20) = fields$(l%)
            if (l% = 43%)              then str(rec$(),583,20) = fields$(l%)
            if (l% = 44%)              then str(rec$(),603,09) = fields$(l%)
            if (l% = 45%)              then str(rec$(),612,01) = "Y"
            if (l% = 46%)              then str(rec$(),613,01) = "Y"
REM 47-8 are shipping intructions
            if (l% = 47% or l% = 48%) then gosub filter_ctrl_char
            if (l% = 47%)              then str(rec$(),614,50) = fields$(l%)
            if (l% = 48%)              then str(rec$(),664,50) = fields$(l%)
            if (l% = 49%)              then str(rec$(),714,04) = fields$(l%)
REM         IF (L% = 50%)              THEN STR(REC$(),718,04) = FIELDS$(L%)
REM         IF (L% = 51%)              THEN STR(REC$(),722,04) = FIELDS$(L%)
            if (l% = 52%)              then gosub set_commsplit01
            if (l% = 53%)              then gosub set_commsplit02
            if (l% = 54%)              then gosub set_commsplit03
            if (l% = 55%)              then str(rec$(),729,04) = fields$(l%)
            if (l% = 56%)              then str(rec$(),733,01) = " "
            if (l% = 57%)              then str(rec$(),734,01) = fields$(l%)
REM         IF (L% = 58%)              THEN STR(REC$(),735,30) = FIELDS$(L%)
/* CR1855 */
            IF (l% = 30%)              then str(rec$(),735,09) = storexref$
/* filler 744 to 764 */
REM         IF (L% = 59%)              THEN STR(REC$(),765,06) = FIELDS$(L%)
            if (l% = 60%)              then str(rec$(),771,09) = fields$(l%)
            if (l% = 61%)              then str(rec$(),780,09) = fields$(l%)
REM         IF (L% = 62%)              THEN STR(REC$(),789,04) = FIELDS$(L%)
            if (l% = 63%)              then str(rec$(),793,01) = fields$(l%)
            if (l% = 64%)              then str(rec$(),794,01) = fields$(l%)
            if (l% = 65%)              then str(rec$(),795,25) = fields$(l%)
            if (l% = 66%)              then str(rec$(),820,20) = fields$(l%)
            if (l% = 67%)              then str(rec$(),840,20) = fields$(l%)
            if (l% = 67%) and fields$(66%) > "0" then ~
                                            str(rec$(),840,20) = "03"

            if (l% = 68%)              then str(rec$(),860,20) = fields$(l%)
            if (l% = 69%)              then str(rec$(),880,20) = fields$(l%)

/* 900-939, fields 5&6 have cut-off C, D, & E  */
REM         if (l% = 70%)              then str(rec$(),900,20) = fields$(l%)
REM         if (l% = 71%)              then str(rec$(),920,20) = fields$(l%)

/* cut off codes B-E   */
            if (l% = 70%)              then str(rec$(),900,02) = fields$(l%)
            if (l% = 94%)              then str(rec$(),902,02) = fields$(l%)
            if (l% = 97%)              then str(rec$(),904,02) = fields$(l%)
            if (l% = 99%)              then str(rec$(),906,02) = fields$(l%)

/* delivery codes B-E  */
            if (l% = 71%)              then str(rec$(),920,02) = fields$(l%)
            if (l% = 96%)              then str(rec$(),922,02) = fields$(l%)
            if (l% = 98%)              then str(rec$(),924,02) = fields$(l%)
            if (l% = 100%)             then str(rec$(),926,02) = fields$(l%)


            if (l% = 72%)              then str(rec$(),940,20) = fields$(l%)
            if (l% = 73%)              then str(rec$(),960,20) = fields$(l%)
            if (l% = 74%)              then str(rec$(),980,20) = fields$(l%)
            if (l% = 75%)              then str(rec$(),1000,20) = fields$(l%)
            if (l% = 76%)              then str(rec$(),1020,01) = "Y"
            if (l% = 77%)              then str(rec$(),1021,01) = "O"
            if (l% = 78%)              then str(rec$(),1022,01) = fields$(l%)
            if (l% = 79%)              then str(rec$(),1023,02) = "  "
            if (l% = 80%)              then str(rec$(),1025,10) = fields$(l%)
REM         IF (L% = 81%)              THEN STR(REC$(),1035,09) = FIELDS$(L%)
            if (l% = 82%)              then str(rec$(),1044,01) = " "
REM         IF (L% = 83%)              THEN STR(REC$(),1045,04) = FIELDS$(L%)
            if (l% = 84%)              then str(rec$(),1049,09) = fields$(l%)
            if (l% = 85%)              then gosub set_days
            if (l% = 86%)              then gosub set_lastmoddate2
REM         IF (L% = 87%)              THEN STR(REC$(),1066,03) = FIELDS$(L%)
REM         IF (L% = 88%)              THEN STR(REC$(),1069,04) = FIELDS$(L%)
REM         IF (L% = 89%)              THEN STR(REC$(),1073,03) = FIELDS$(L%)
REM         IF (L% = 90%)              THEN STR(REC$(),1076,06) = FIELDS$(L%)
REM         IF (L% = 91%)              THEN STR(REC$(),1082,09) = FIELDS$(L%)
REM         IF (L% = 92%)              THEN STR(REC$(),1091,01) = FIELDS$(L%)
REM         IF (L% = 93%)              THEN STR(REC$(),1092,01) = FIELDS$(L%)
REM         IF (L% = 94%)              THEN STR(REC$(),1093,01) = FIELDS$(L%)
            if (l% = 95%)              then str(rec$(),1094,01) = fields$(l%)
REM         IF (L% = 96%)              THEN STR(REC$(),1095,106) = FIELDS$(L%)
         next l%

             put #1, using L25000, rec$()
             if found = 1 then rewrite #1               ~
               else write #1
               
             if f31$ = "Y" then ~
                   call "AWDEDIAD" (fields$(1%), fields$(31%), edimsg$) 
         goto FINI

delete_record:
            key$ = fields$(1%)  & "        "
            read #1, key = key$, hold, eod goto FINI
            delete #1
         goto FINI  

set_cusopen:        /* date 220,6,1 */
            if str(rec$(),0220,006) <> "      " then return /* init only */
             colval$ = "0"
             gosub convert_date
             str(rec$(),0220,006) = date$
         return

set_discpercent:    /* dec 517,8,1 */
            if str(rec$(),0517,008) <> "        " then return /* init only */
             colval$ = "0"
             gosub convert_dec
             str(rec$(),0517,008) = dec$
         return

set_crlimit:        /* dec 526,8,1 */
             colval$ = fields$(l%)
             if colval$ = "        " then colval$ = "0"
             gosub convert_dec
             str(rec$(),0526,008) = dec$
         return

set_lastmoddate:    /* date 534,6,1 */
            if str(rec$(),0534,006) <> "      " then return /* init only */
             colval$ = "0"
             gosub convert_date
             str(rec$(),0534,006) = date$
         return

set_commsplit01:    /* bin 726,1 */
             gosub convert_bin
             str(rec$(),0726,001) = bin$
         return
set_commsplit02:    /* bin 727,1 */
             gosub convert_bin
             str(rec$(),0727,001) = bin$
         return
set_commsplit03:    /* bin 728,1 */
             gosub convert_bin
             str(rec$(),0728,001) = bin$
         return
set_days:           /* bin 1058,2 */
             colval$ = "0" /*init to zero */
             gosub convert_bin
             str(rec$(),1058,002) = bin2$
         return
set_lastmoddate2:    /* date 1060,6,1 */
            if str(rec$(),1060,006) <> "      " then return /* init only */
            colval$ = "0"
            gosub convert_date
            str(rec$(),1060,006) = date$
         return

convert_date:
            init(hex(00)) date$
            colval = 0.0
            convert colval$ to colval, data goto bad_date
            put date$, using date_fmt, colval
date_fmt:     FMT PD(11,1)
bad_date:
         return

convert_bin:
            init(hex(00)) bin$, bin2$
            colval% = 0
REM         CONVERT COLVAL$ TO COLVAL%, DATA GOTO BAD_BIN
            put bin$, using bin_fmt, colval%
            put bin2$, using bin2_fmt, colval%
bin_fmt:      FMT BI(01)
bin2_fmt:     FMT BI(02)
REM   bad_bin:
         return

reformat_zipcode1:
            gosub reformat_zipcode
            str(rec$(),211,09) = zipcode$
         return

reformat_zipcode2:
            gosub reformat_zipcode
            str(rec$(),424,09) = zipcode$
         return

reformat_zipcode:
            zipcode$ = "         "
            x = 1
            for y = 1 to 20
             if str(fields$(l%),y,1) < "0" or ~
                str(fields$(l%),y,1) > "9" then skip_char
             str(zipcode$,x,1) = str(fields$(l%),y,1)
             x = x + 1
skip_char:
             if x > 9 then y = 21
            next y
         return

filter_ctrl_char:
            for y = 1 to 50
             if str(fields$(l%),y,1) < " " then ~
                str(fields$(l%),y,1) = " "
            next y
         return

convert_dec:
            init(hex(00)) dec$
            colval = 0.0
            convert colval$ to colval, data goto bad_dec
            put dec$, using dec_fmt, colval
dec_fmt:     FMT PD(15,4)
bad_dec:
         return

FINI:
        end

        
