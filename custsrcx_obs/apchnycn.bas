        REM *************************************************************~
            * (APCHNYCN) - Rebuild all Part Descriptions for all        *~
            *              Manufactured Parts, Using the Current        *~
            *              Validity File (AMTBOMIF).                    *~
            *                       Written By                          *~
            *                   Roy H. Hoffman - 03/23/92               *~
            *          !                                          !     *~
            * 11/26/97 ! Checked for 60403 compliance             ! DJD *~
            *          !                                          !     *~
            *************************************************************

            dim descr$32, vfs$200
            dim part$25, desc$32, readkey$50
            dim apc_scr$120, apc_prt$60, apc_sze$20
            dim ct$5, cnt$5, cnt1$5, hdr$40, msg$(3)79

        dim f2%(3),                      /* File Status Flags          */~
            fs%(3),                      /* File Status Flags          */~
            f1%(3),                      /* Record-on-file Flags       */~
            rslt$(3)20                   /* Returned from "OPENFILE"   */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #1  ! HNYMASTR ! Inventory Master File                    *~
            * #2  ! GENCODES ! General Codes File                       *~
            * #3  ! AMTBOMIF ! Master Validity File                     *~
            *************************************************************

            select #1,  "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90, keylen = 4, dup,  ~
                                   key 3, keypos = 26, keylen = 32, dup

            select #2,  "GENCODES",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos =    1,  keylen = 24

            select #3,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =  1,   keylen =  32


            call "SHOSTAT"  ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1 , fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (#2 , fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (#3 , fs%( 3), f2%( 3), 0%, rslt$( 3))

            init(" ") descr$, vfs$, readkey$, cnt$, cnt1$, desc$, hdr$,  ~
                      msg$(), ct$

            stop " Begin Process or Abort "

            call "SHOSTAT"  ("Scanning 'HNYMASTR' File")
            ct%, cnt%, cnt1% = 0%
            part$ = all(hex(00))
        scan_next
            read #1,hold,key > part$, using L00620, part$,eod goto scan_done
L00620:         FMT CH(25)
            ct% = ct% + 1%
            if len(part$) < 19 then goto scan_next
               cnt% = cnt% + 1%
               if mod(cnt%,100%) <> 0 then goto L00700
                  convert ct% to ct$, pic(#####)
                  call "SHOSTAT" ("Records Scanned (" & ct$ & ")")

L00700:        call "APCDESCR" (part$, apc_scr$, apc_prt$, apc_sze$, #3, ~
                                                           er% )
               if er% > 1% then goto scan_next
                  get #1, using L00740  , descr$, vfs$
L00740:              FMT POS(26), CH(32), POS(506), CH(200)
                  str(descr$,17%,16%) = apc_sze$
                  str(vfs$,101%,60%)  = apc_prt$
                  str(vfs$,161%,20%)  = apc_sze$
                  str(descr$,1%,16%) = str(apc_prt$,1%,16%)
                  gosub calculate_units
                  put #1 using L00740     , descr$, vfs$
                  rewrite #1
                  cnt1% = cnt1% + 1%
            goto scan_next
        scan_done
            convert ct% to ct$, pic(#####)
            call "SHOSTAT" ("Records Scanned (" & ct$ & ")")

        goto exit_program
            convert er% to er$, pic(00)
            stop "Error-("&er$&")- Part No.---> " &part$& "  " &apc_prt$
        goto scan_next

        calculate_units                  /* ONLY MANUFACTURED PARTS */
            readkey$ = all(hex(00))
            readkey$ = "SYS CODES" & str(part$,1%,3%)
            call "DESCRIBE" (#2 , readkey$, desc$, 0%, f1%(2%))
            if f1%(2%) = 0% then return           /* NO FACTOR DEFINED*/
            if str(desc$,21%,1%) <> " " then goto L01000
               str(desc$,21%,5%) = "00.00"
                                         /* Convert Width  */
L01000:     convert str(part$,13%,4%) to a1%,data goto L01010
L01010:                                  /* Convert Height */
            convert str(part$,17%,3%) to a2%,data goto L01030
L01030:                                  /* Always Round Up*/
            if str(part$,16%,1%) <> "0" then a1% = a1% + 10%
            if str(part$,19%,1%) <> "0" then a2% = a2% + 10%
                                    /* UNIT_I% = UNITED INCHES       */
            unit_i% = int(a1%/10) + int(a2%/10)
            convert str(desc$,21%,5%) to fact, data goto L01090
L01090:
            fact = round(fact,2)
            x    = round( ((fact * unit_i%)/116.0),2)
                                         /* SAVE IN VARIABLE FIELD 3 */
            convert x to str(vfs$,41%,12%),pic(#########.##)
        return

        exit_program
            convert cnt%  to cnt$,  pic(#####)
            convert cnt1% to cnt1$, pic(#####)

            comp% = 2%
            hdr$ = "******* Inventory Scan *******"
            msg$(1) = "Scanned: "& cnt$ & "  Updated: " & cnt1$
            msg$(2) = "              I N V E N T O R Y                  "
            msg$(3) = "        Press any Key to Continue!!!!            "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        end

