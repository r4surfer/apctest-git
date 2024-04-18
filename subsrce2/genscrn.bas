*************************************************************************~
*                                                                       *~
*    GGG   EEEEE  N   N   SSS    CCC   RRRR   N   N                     *~
*   G      E      NN  N  S      C      R   R  NN  N                     *~
*   G  GG  EEE    N N N   SSS   C      RRRR   N N N                     *~
*   G   G  E      N   N      S  C      R  R   N  NN                     *~
*    GGG   EEEEE  N   N   SSS    CCC   R   R  N   N                     *~
*                                                                       *~
*-----------------------------------------------------------------------*~
* GENSCRN  - Subroutine used by the Caelus Program Generator to generate*~
*            the screen I-O sections of a generated program. (Note;     *~
*            split out of GENPGM  because of compiler limitations - too *~
*            big!).                                                     *~
*-----------------------------------------------------------------------*~
*                        M O D I F I C A T I O N S                      *~
*---WHEN---+----------------WHAT----------------------------------+-WHO-*~
* 11/12/80 ! ORIGINAL [SCRNSUB]                                   ! BW  *~
* 04/11/83 ! ADDED PRINT OF CALL "MANUAL" ("OUTFILE")             ! ECR *~
* 01/01/86 ! Lots of Changes.                                     ! LDJ *~
* 06/04/86 ! Minor Modifications                                  ! LDJ *~
* 12/19/86 ! More Minor Mods                                      ! MJB *~
* 03/20/87 ! Changed PF Key Handling                              ! MJB *~
* 03/12/96 ! UNIXed the puppy.  Changed name to GENSCRN.          ! ERN *~
* 05/22/96 ! Minor fixes                                          ! LDJ *~
*************************************************************************

        sub "GENSCRN" (#1, title$, prompt$(), alias$(), variable$(), ~
                               datatype$(), fieldlen$(), maxfields%(),   ~
                               maxpages%, outfile$)

        dim                                                              ~
            alias$(6,15)18,              /* FIELD ALIAS NAMES          */~
            datatype$(6,15)1,            /* DATA TYPE FOR FIELD        */~
            descvar$50,                  /* VARIABLE FOR DESCRIPTIONS  */~
            fieldlen$(6,15)2,            /* FIELD LENGTH               */~
            line$7,                      /* LINE NUMBER FOR CODE GEN   */~
            maxfields%(6),               /* MAXIMUM # OF FIELDS EA PAGE*/~
            outfile$8,                   /* Generated Program Name     */~
            prompt$(6,15)27,             /* PROMPTS FOR INPUT FIELDS   */~
            promst$50,                   /* PROMPT NAME FOR WRITING    */~
            t$1,                         /* A tilde for your thoughts  */~
            title$40,                    /* SCREEN TITLE INFORMATION   */~
            variable$(6,15)19,           /* VARIABLE NAMES TO USE      */~
            z$255

*************************************************************************~
*              G E N E R A T E   I N P U T   S C R E E N S              *~
* ----------------------------------------------------------------------*~
* GENERATES INPUT SCREENS.                                              *~
*************************************************************************

REM *********************************************************************~
    *                      Release Version ID Section                   *~
    *********************************************************************
        dim cms2v$50 : goto  L10062
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
L10062: REM *************************************************************

        init (hex(20)) z$
        t$ = hex(7E)

        /* Write the generic screen procedures descriptions */
        gosub putz : write #1, using L21000, t$
        gosub putz : write #1, using L21010, t$
        gosub putz : write #1, using L21020, t$
        gosub putz : write #1, using L21030, t$
        gosub putz : write #1, using L21040, t$
        gosub putz : write #1, using L21050, t$
        gosub putz : write #1, using L21060, t$
        gosub putz : write #1, using L21070, t$
        gosub putz : write #1, using L21080, " "
        gosub blank_line

        /* Write the generic screen procedure upper/lower */
        gosub putz : write #1, using L21100, " "
        gosub putz : write #1, using L21110, " "
        gosub return_line
        gosub blank_line

        /* Write the generic screen procedure upper only */
        gosub putz : write #1, using L21200, " "
        gosub putz : write #1, using L21210, " "
        gosub return_line
        gosub blank_line

        /* Write the generic screen procedure numeric only */
        gosub putz : write #1, using L21300, " "
        gosub putz : write #1, using L21310, " "
        gosub return_line
        gosub blank_line

        /* Write the generic screen procedure manual or prnt scrn */
        gosub putz : write #1, using L21400, " "
        gosub putz : write #1, using L21410, outfile$, t$
        gosub putz : write #1, using L21420, " "
        gosub return_line
        gosub blank_line

        /* Write the generic screen procedure to screen */
        gosub putz : write #1, using L21500, " "
        gosub putz : write #1, using L21510, " "
        gosub putz : write #1, using L21520, " "
        gosub return_line
        gosub blank_line

        /* Write each screen */
        for page% = 1% to maxpages%
            gosub putz : write #1, using L25000, t$
            gosub putz : write #1, using L25030, page%, t$
            gosub putz : write #1, using L25060, t$
            gosub putz : write #1, using L25090, t$
            gosub putz : write #1, using L25120, " "
            gosub blank_line
            gosub putz : write #1, using L25170, page%
            gosub putz : write #1, using L25185, page%
            gosub putz : write #1, using L25190, page%, t$
            gosub putz : write #1, using L25200, page%
            gosub blank_line

            gosub putz : write #1, using L25210, t$
            gosub putz : write #1, using L25240, " "
            gosub blank_line

            for line% = 1% to maxfields%(page%)
                if datatype$(page%, line%) = "1" then line$ = "uprlowr"
                if datatype$(page%, line%) = "2" then line$ = "upronly"
                if datatype$(page%, line%) = "3" then line$ = "numeric"
                if datatype$(page%, line%) = "4" then line$ = "upronly"
                if maxfields%(page%) <> 1% then L10440
                    gosub putz : write #1, using L25290, line$, alias$(page%, line%)
                    goto L10570
L10440:         if line% <> 1% then L10490
                    gosub putz : write #1, using L25260, line$, alias$(page%, line%), t$
                    goto L10570
L10490:         if line% <> maxfields%(page%) then L10540
                    gosub putz : write #1, using L25350, line$, alias$(page%, line%)
                    goto L10570
L10540:         gosub putz :     write #1, using L25320, line$, alias$(page%, line%), t$
L10570:     next line%
            gosub blank_line


            /* Now generate screen header part. */
            gosub putz : write #1, using L25530, page%
            gosub putz : write #1, using L25540, t$
            gosub putz : write #1, using L25560, t$
            promst$ = title$ & hex(22) & ","
            gosub putz : write #1, using L25590, promst$, t$  /* TITLE$ */
            gosub putz : write #1, using L25620, t$
            gosub putz : write #1, using L25650, t$
            gosub putz : write #1, using L25680, t$
            gosub putz : write #1, using L25710, t$

            /* Now generate screen fields. */
            for line% = 1% to maxfields%(page%)
                convert fieldlen$(page%, line%) to length%
                convert length% to length$, pic(##)
                tran(length$, "0 ") replacing
                if datatype$(page%, line%) <> "4" then L11000
                   temp% = pos(variable$(page%, line%) = "$")
                   descvar$ = variable$(page%, line%)
                   descvar$ = str(descvar$,1,temp%-1) & "descr" &        ~
                              str(descvar$, temp%)
L11000:         convert line% + 5 to temp$, pic(##)
                tran(str(temp$, 1, 2), "0 ") replacing
                temp$ = temp$ & ","
                gosub putz: write #1, using L26040, t$
                promst$ = prompt$(page%, line%) & hex(22) & ","
                gosub putz : write #1, using L25770, temp$, promst$, t$
                gosub putz : write #1, using L25800, temp$, line%,       ~
                                       variable$(page%, line%), length$, t$
                if datatype$(page%, line%) <> "4" then L11140
                   gosub putz : write #1, using L25830, temp$, descvar$, t$
L11140:     next line%

            /* Now generate screen trailer. */
            gosub putz : write #1, using L26040, t$
            gosub putz : write #1, using L25890, t$
            gosub putz : write #1, using L25950, t$
            gosub putz : write #1, using L25980, t$
            gosub putz : write #1, using L26010, t$
            gosub putz : write #1, using L26040, t$
            gosub putz : write #1, using L26070, " "
            gosub blank_line
            gosub putz : write #1, using L26110, " "
            gosub putz : write #1, using L26120, page%
            gosub blank_line
            gosub putz : write #1, using L26250, " "
            gosub return_line
            gosub blank_line


            /* Now generate function key settings */
            /* function keys for input mode */
            gosub putz : write #1, using L26310, page%
            gosub putz : write #1, using L26320, " "
            gosub putz : write #1, using L26330, " "
            gosub putz : write #1, using L26340, " "
            gosub putz : write #1, using L26350, " "
            gosub putz : write #1, using L26360, " "
            gosub blank_line
            gosub putz : write #1, using L26370, " "
            gosub putz : write #1, using L26380, t$
            gosub putz : write #1, using L26390, " "
            gosub putz : write #1, using L26400, t$
            gosub putz : write #1, using L26410, " "
            gosub return_line
            gosub blank_line

            gosub putz : write #1, using L26610, page%
            gosub putz : write #1, using L26620, " "
            gosub putz : write #1, using L26660, page%
            gosub blank_line
            gosub putz : write #1, using L26670, " "
            gosub putz : write #1, using L26680, " "

            on page% gosub setlo, setmid, setmid, setmid, setmid, sethi

            gosub putz : write #1, using L26850, page%
            gosub blank_line
            gosub putz : write #1, using L26870, page%
            gosub putz : write #1, using L26890, " "
            gosub putz : write #1, using L26940, " "
            gosub putz : write #1, using L26990, " "
            gosub putz : write #1, using L27040, " "
            gosub blank_line

            gosub putz : write #1, using L27060, page%
            gosub return_line
            gosub blank_line

            gosub blank_line
        next page%
    goto exit_subroutine


setlo                 /* For Page #1 */
        gosub putz : write #1, using L26730, " "
        gosub putz : if maxpages% > page% then L12430
            gosub putz : write #1, using L26780, " "
            gosub putz : write #1, using L26830, " "
            return
L12430: gosub putz : write #1, using L26790, " "
        gosub putz : write #1, using L26836, " "
        return

setmid              /* For Page #2 - #5 */
        gosub putz : write #1, using L26750, " "
        if maxpages% > page% then L12630
            gosub putz : write #1, using L26780, " "
            gosub putz : write #1, using L26843, " "
            return
L12630: gosub putz : write #1, using L26790, " "
        gosub putz : write #1, using L26847, " "
        return

sethi                /* For Page #6 */
        gosub putz : write #1, using L26750, " "
        gosub putz : write #1, using L26780, " "
        gosub putz : write #1, using L26843, " "
        return


*************************************************************************~
*        I M A G E   S T A T E M E N T S   F O R   S C R E E N          *~
*************************************************************************

L21000: %rem *************************************************************#
L21010: %    *   G E N E R I C    S C R E E N    S U B R O U T I N E S   *#
L21020: %    *-----------------------------------------------------------*#
L21030: %    * uprlowr       - FAC Upper or Lower Case                   *#
L21040: %    * upronly       - FAC Upper Case Only                       *#
L21050: %    * numeric       - FAC Numeric                               *#
L21060: %    * man_or_prtscr - Manual or Print Screen                    *#
L21070: %    * get_screen    - Get screen and cursor image               *#
L21080: %#   *************************************************************

L21100: %uprlowr#
L21110: %    lfac$(fieldnr%) = hex(80)#


L21200: %upronly#
L21210: %    lfac$(fieldnr%) = hex(81)#


L21300: %numeric#
L21310: %    lfac$(fieldnr%) = hex(82)#


L21400: %man_or_prtscr#
L21410: %    if keyhit% = 13% then call "MANUAL" ("########") #
L21420: %                     else call "PRNTSCRN"#


L21500: %get_screen#
L21510: %    close WS#
L21520: %    call "SCREEN" addr ("C", U3%, "I", i$(), cursor%())#


L25000: %rem *************************************************************#
L25030: %    *               S C R E E N   P A G E   #                   *#
L25060: %    *-----------------------------------------------------------*#
L25090: %    * Document Input and Edit Screen.                           *#
L25120: %    *************************************************************

L25170: %deffn'10#(fieldnr%, edit%)
L25185: %    gosub'050(#%, fieldnr%)
L25190: %    if edit% = false% then gosub setpf_pg#_inp #
L25200: %                      else gosub setpf_pg#_edt

L25210: %    if fieldnr% > 0% then init(hex(8C)) lfac$()                #
L25240: %                     else init(hex(86)) lfac$()#

L25260: %    on fieldnr% gosub #######,         /* ################# */ #
L25290: %    on fieldnr% gosub #######          /* ################# */
L25320: %                      #######,         /* ################# */ #
L25350: %                      #######          /* ################# */

L25530: %  scr#_dsply
L25540: %    accept                                                       #
L25560: %       at (01,02),                                               #
L25590: %          "##########################################            #
L25620: %       at (01,66), "Today:",                                     #
L25650: %       at (01,73), fac(hex(8C)), date$                  , CH(08),#
L25680: %       at (02,02), fac(hex(AC)), line2$                 , CH(79),#
L25710: %       at (04,02), fac(hex(94)), errormsg$              , CH(79),#
L25770: %       at (###02), "####################################         #
L25800: %       at (###30), fac(lfac$(##)), ###################  , CH(##),#
L25830: %       at (###49), fac(hex(8C)),   ###################  , CH(32),#
L25890: %       at (21,02), fac(hex(A4)),   inpmessage$          , CH(79),#
L25950: %       at (22,02), fac(hex(8C)),   pf$(1%)              , CH(79),#
L25980: %       at (23,02), fac(hex(8C)),   pf$(2%)              , CH(79),#
L26010: %       at (24,02), fac(hex(8C)),   pf$(3%)              , CH(79),#
L26040: %                                                                 #
L26070: %       keys(pfkeys$), key(keyhit%)#

L26110: %#   if keyhit% = 13% or keyhit% = 15% then gosub man_or_prtscr
L26120: %    if keyhit% = 13% or keyhit% = 15% then goto  scr#_dsply

L26250: %    gosub get_screen#
      /* return */


L26310: %setpf_pg#_inp
L26320: %#   /* Input Mode */
L26330: %#   pf$(1%) = "(1)Start Over                                                  (13)Instructions"
L26340: %#   pf$(2%) = "                 (4)Previous Field                             (15)Print Screen"
L26350: %#   pf$(3%) = "                                                               (16)Exit Program"
L26360: %#   pfkeys$ = hex(01FFFF04FFFFFFFFFFFFFFFF0DFF0F1000)

L26370: %#   /* Field one, turn off previous field, otherwise turn off exit */
L26380: %    if fieldnr% = 1% then str(pf$(2%), 18%, 26%) = " " #
L26390: %#                    else str(pf$(3%), 64%, 16%) = " "
L26400: %    if fieldnr% = 1% then str(pfkeys$,  4%,  1%) = hex(FF) #
L26410: %#                    else str(pfkeys$, 16%,  1%) = hex(FF)
/* return */


L26610: %setpf_pg#_edt
L26620: %#   /* Edit Mode */
L26660: %    if fieldnr% > 0% then setpf_pg#_edtfld

L26670: %#   /* Edit Mode - no field selected */
L26680: %#   pf$(1%) = "(1)Start Over                                                  (13)Instructions"

L26730: %#   pf$(2%) = "                                                               (15)Print Screen"
L26750: %#   pf$(2%) = "                 (4)Previous Screen                            (15)Print Screen"
L26780: %#   pf$(3%) = "                                                               (16)Save Data   "
L26790: %#   pf$(3%) = "                 (5)Next Screen                                (16)Save Data   "

L26830: %#   pfkeys$ = HEX(01FFFFFFFFFFFFFFFFFFFFFF0DFF0F1000)
L26836: %#   pfkeys$ = HEX(01FFFFFF05FFFFFFFFFFFFFF0DFF0F1000)
L26843: %#   pfkeys$ = HEX(01FFFF04FFFFFFFFFFFFFFFF0DFF0F1000)
L26847: %#   pfkeys$ = HEX(01FFFF0405FFFFFFFFFFFFFF0DFF0F1000)

L26850: %    goto setpf_pg#_edtexit

L26870: %  setpf_pg#_edtfld

L26890: %#   pf$(1%) = "(1)Start Over                                                  (13)Instructions"
L26940: %#   pf$(2%) = "                                                               (15)Print Screen"
L26990: %#   pf$(3%) = "                                                                               "
L27040: %#   pfkeys$ = HEX(01FFFFFFFFFFFFFFFFFFFFFF0DFF0FFF00)

L27060: %  setpf_pg#_edtexit
/* return */

************************************************************************

putz
    put #1, str(z$, 1%, 255%)
return


/* Generic Blank Line */
blank_line
    gosub putz
    write #1, using BLNKLN, " "
  BLNKLN: %#
return


/* Generic Return line */
return_line
    gosub putz
    write #1, using RTRNLN, " "
  RTRNLN: %return#

    gosub blank_line

return



/************************************************************
   Exit sub
*************************************************************/
exit_subroutine

    end
