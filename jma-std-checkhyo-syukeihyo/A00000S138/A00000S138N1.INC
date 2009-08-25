      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       01  S138N1.
              03  S138N1-PAGE              PIC  X(03).
              03  S138N1-SRYYM             PIC  X(16).
              03  S138N1-SYSYMD            PIC  X(22).
              03  S138N1-NYUGAI            PIC  X(04).
              03  S138N1-SRYKANAME         PIC  X(20).
              03  S138N1-CITY-AREA.
                  05  S138N1-KBN1-TBL OCCURS 14.
                      07  S138N1-KENSU      PIC  X(04).
                      07  S138N1-NISSU      PIC  X(04).
                      07  S138N1-TENSU      PIC  X(09).
                      07  S138N1-MADOGK     PIC  X(09).
      *--- (2008/12/11) line add start monma ---------------------------
                      07  S138N1-JIHIGK     PIC  X(09).
      *--- (2008/12/11) line add end   monma ---------------------------
                      07  S138N1-HKNGK      PIC  X(09).
                      07  S138N1-KOHGK      PIC  X(09).
                      07  S138N1-SYUEKI     PIC  X(09).
      *--- 2009/08/20 ADD START ----------------------------------------
      *                07  S138N1-SNISSU     PIC  X(04).
                      07  S138N1-SNISSU     PIC  X(05).
      *--- 2009/08/20 ADD END ------------------------------------------
                      07  S138N1-SSEIKYU    PIC  X(09).
                      07  S138N1-SFTNGK     PIC  X(09).
                      07  FILLER1         PIC  X(09).
                      07  FILLER2         PIC  X(09).
                  05  S138N1-KOHI-TBL OCCURS 20.
                      07  S138N1-KOHNAME     PIC  X(24).
                      07  S138N1-KKENSU      PIC  X(04).
                      07  S138N1-KNISSU      PIC  X(04).
                      07  S138N1-KTENSU      PIC  X(09).
                      07  FILLER3          PIC  X(09).
                      07  FILLER4          PIC  X(09).
                      07  FILLER5          PIC  X(09).
                      07  FILELR6          PIC  X(09).
                      07  FILLER7          PIC  X(09).
                      07  FILLER8          PIC  X(09).
              03  S138N1-KUMIAI-AREA.
                  05  S138N1-KBN2-TBL OCCURS 11.
                      07  S138N1-K-KENSU      PIC  X(04).
                      07  S138N1-K-NISSU      PIC  X(04).
                      07  S138N1-K-TENSU      PIC  X(09).
                      07  S138N1-K-MADOGK     PIC  X(09).
      *--- (2008/12/11) line add start monma ---------------------------
                      07  S138N1-K-JIHIGK     PIC  X(09).
      *--- (2008/12/11) line add end   monma ---------------------------
                      07  S138N1-K-HKNGK      PIC  X(09).
                      07  S138N1-K-KOHGK      PIC  X(09).
                      07  S138N1-K-SYUEKI     PIC  X(09).
      *--- 2009/08/20 CHANGE START -------------------------------------
      *                07  S138N1-K-SNISSU     PIC  X(04).
                      07  S138N1-K-SNISSU     PIC  X(05).
      *--- 2009/08/20 CHANGE END ---------------------------------------
                      07  S138N1-K-SSEIKYU    PIC  X(09).
                      07  S138N1-K-SFTNGK     PIC  X(09).
                      07  FILLER9           PIC  X(09).
                      07  FILLER10           PIC  X(09).
                  05  S138N1-KOHI2-TBL OCCURS 20.
                      07  S138N1-K-KOHNAME     PIC  X(24).
                      07  S138N1-K-KKENSU      PIC  X(04).
                      07  S138N1-K-KNISSU      PIC  X(04).
                      07  S138N1-K-KTENSU      PIC  X(09).
                      07  FILLER11            PIC  X(09).
                      07  FILLER12            PIC  X(09).
                      07  FILLER13            PIC  X(09).
                      07  FILLER14            PIC  X(09).
                      07  FILLER15            PIC  X(09).
                      07  FILLER16            PIC  X(09).
              03  S138N1-GOKEI-AREA.
                  05  S138N1-GOKEI-TBL.
                      07  S138N1-GKENSU      PIC  X(04).
                      07  S138N1-GNISSU      PIC  X(04).
                      07  S138N1-GTENSU      PIC  X(09).
                      07  S138N1-GMADOGK     PIC  X(09).
      *--- (2008/12/11) line add start monma ---------------------------
                      07  S138N1-GJIHIGK     PIC  X(09).
      *--- (2008/12/11) line add end   monma ---------------------------
                      07  S138N1-GHKNGK      PIC  X(09).
                      07  S138N1-GKOHGK      PIC  X(09).
                      07  S138N1-GSYUEKI     PIC  X(09).
      *--- 2009/08/20 CHANGE START -------------------------------------
      *                07  S138N1-SGNISSU     PIC  X(04).
                      07  S138N1-SGNISSU     PIC  X(05).
      *--- 2009/08/20 CHANGE END ---------------------------------------
                      07  S138N1-SGSEIKYU    PIC  X(09).
                      07  S138N1-SGFTNGK     PIC  X(09).
                      07  FILLER17          PIC  X(09).
                      07  FILLER18          PIC  X(09).
