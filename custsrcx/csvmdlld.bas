        REM *************************************************************~
            *                                                           *~
            *  Program Name      - CSVMDLLD                             *~
            *  Creation Date     -                                      *~
            *  Last Modified Date- 01/29/2019                           *~
            *  Written By        - Christie Sanders                     *~
            *                                                           *~
            *  Description       - This Program loads data into file    *~
            *                      NFRCMDL                              *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *01/29/2019! (CR1908) Mod to add sashfoam             ! CMN *~
            *          !                                          !     *~
            *************************************************************
            
        dim readrec$256,                 /* Used By EWDOPEN            */~
            readkey$100,                 /* Readkey                    */~
            axd$(25)4,                   /*                            */~
            f2%(25%),                    /* = 0 if the file is open    */~
            f1%(25%),                    /* = 1 if READ was successful */~
            fs%(25%),                    /* = 1 if file open, -1 if it */~
            rslt$(25%)20                 /* Text from file opening     */
            
        dim nfrcmdlkey$20,                                               ~
            fields$(20%)100,                                             ~
            warehouse$4,                                                 ~
            series$10,                                                   ~
            style$10,                                                    ~
            model$5,                                                     ~
            framecode$5,                                                 ~
            foamframe$5,                                                 ~
            sashcode$5,                                                  ~
            sashfoam$5      /* (CR1908) */
            

            
            select #10,  "CSVMDLWK",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,    keylen = 100

            select #20,  "NFRCMDL",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 25,    keylen = 20,                     ~
                        alt key 1, keypos = 1, keylen = 44
                        
                        
        call "OPENCHCK" (#10, fs%(10%), f2%(10%),500%, rslt$(10%))
        call "OPENCHCK" (#20, fs%(20%), f2%(20%),500%, rslt$(20%))

        init(" ") readkey$, readrec$, fields$()
        count%, c%, p% = 0%
readNxt:

        read #10, key > readkey$, eod goto readDone
        
        get #10, using READFMT1, readrec$, eod goto readDone
READFMT1:   FMT CH(256)
        
        readkey$ = str(readrec$,1%,100%)
REM Should be 39% columns in the spreadsheet
        c% = 21%
        for i% = 1% to 11%      /* (CR1908) change from 10% to 11% */
          p% = pos(str(readrec$,c%,(256%-c%)) = ",")
          if p% = 0% then p% = pos(str(readrec$,c%,(256%-c%)) = " ")
          fields$(i%) = str(readrec$,c%,(p%-1%))
          c% = c% + p%
        next i%
        
         gosub convert_data
         gosub write_data
        
        goto readNxt
        
        readDone
        

        end
        
        convert_data
          init(" ") warehouse$, series$, style$, model$, framecode$,    ~
            foamframe$, sashcode$, sashcode$ 
          
          warehouse$  = fields$(1%)
          series$     = fields$(2%)
          style$      = fields$(3%)
          model$      = fields$(4%)
          framecode$  = fields$(5%)
          foamframe$  = fields$(6%)
          sashcode$   = fields$(7%)
          sashfoam$   = fields$(11%)     /* (CR1908) */              
          
          number$ = fields$(8%)
          gosub convert_number
          igthickness = convNumber          

          number$ = fields$(9%)
          gosub convert_number
          sdligthickness = convNumber          
          
          number$ = fields$(10%)
          gosub convert_number
          tripleigthickness = convNumber    
 
        return

        convert_number
         convNumber = 0.00
         convert number$ to convNumber, data goto badNum
        badNum
        return

        write_data
           init(" ") nfrcmdlkey$
           str(nfrcmdlkey$,01%,05%) = model$
           str(nfrcmdlkey$,06%,05%) = framecode$
           str(nfrcmdlkey$,11%,05%) = foamframe$
           str(nfrcmdlkey$,16%,05%) = sashcode$
           
           read #20, hold, key = nfrcmdlkey$, eod goto noNFRCMDL
        
               delete #20
noNFRCMDL:        
        
           put #20, using WRITEFMT1, warehouse$,                          ~
                                     series$,                             ~
                                     style$,                              ~
                                     model$,                              ~
                                     framecode$,                          ~
                                     foamframe$,                          ~ 
                                     sashcode$,                           ~
                                     igthickness,                         ~
                                     sdligthickness,                      ~
                                     tripleigthickness,                   ~
                                     sashfoam$,                           ~
                                     " "
                                     
                                     
                                     
           write #20, eod goto WRITE_ERROR
           
        return   
           
        WRITE_ERROR
        return

WRITEFMT1:                 FMT       CH(04),             /* warehouse   */~
                                     CH(10),             /* series      */~
                                     CH(10),             /* style       */~
                                     CH(05),             /* model       */~
                                     CH(05),             /* framecode   */~
                                     CH(05),             /* foamframe   */~
                                     CH(05),             /* sashcode    */~
                                     PD(14,4),           /* IGthickness */~
                                     PD(14,4),           /* SDLIGthick  */~
                                     PD(14,4),           /* TripIGthick */~
                                     CH(05),      /* (CR1908) Foam Sash */~
                                     CH(183)             /* Filler      */
                                     

                                     
                                     