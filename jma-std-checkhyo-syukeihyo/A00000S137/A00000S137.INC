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
       01  S137.
              03  S137-PAGE              PIC  X(03).
              03  S137-SYSYMD            PIC  X(22).
              03  S137-SRYYM             PIC  X(16).
              03  S137-NYUGAI            PIC  X(04).
              03  S137-SRYKANAME         PIC  X(20).
              03  S137-HKN-TBL OCCURS 53.
                  05  S137-KENSU         PIC  X(04).
                  05  S137-NISSU         PIC  X(04).
                  05  S137-TENSU         PIC  X(09).
                  05  S137-MADOGK        PIC  X(09).
                  05  S137-HKNGK         PIC  X(09).
                  05  S137-KOHGK         PIC  X(09).
                  05  S137-SYUEKI        PIC  X(09).
              03  S137-KOHTAN-TBL OCCURS 10.
                  05  S137-KTKENSU       PIC  X(04).
                  05  S137-KTNISSU       PIC  X(04).
                  05  S137-KTTENSU       PIC  X(09).
                  05  S137-KTMADOGK      PIC  X(09).
                  05  S137-KTHKNGK       PIC  X(09).
                  05  S137-KTKOHGK       PIC  X(09).
                  05  S137-KTSYUEKI      PIC  X(09).
                  05  S137-KOHTNAME      PIC  X(24).
              03  S137-GKOHTAN-TBN.
                  05  S137-GKTKENSU      PIC  X(04).
                  05  S137-GKTNISSU      PIC  X(04).
                  05  S137-GKTTENSU      PIC  X(09).
                  05  S137-GKTMADOGK     PIC  X(09).
                  05  S137-GKTHKNGK      PIC  X(09).
                  05  S137-GKTKOHGK      PIC  X(09).
                  05  S137-GKTSYUEKI     PIC  X(09).
              03  S137-KOH-TBL OCCURS 30.
                  05  S137-KKENSU       PIC  X(04).
                  05  S137-KNISSU       PIC  X(04).
                  05  S137-KTENSU       PIC  X(09).
                  05  S137-KOHNAME      PIC  X(24).
              03  S137-GKOH-TBN.
                  05  S137-GKKENSU      PIC  X(04).
                  05  S137-GKNISSU      PIC  X(04).
                  05  S137-GKTENSU      PIC  X(09).
              03  S137-GOKEI-TBL.
                  05  S137-GKENSU       PIC  X(04).
                  05  S137-GNISSU       PIC  X(04).
                  05  S137-GTENSU       PIC  X(09).
                  05  S137-GMADOGK      PIC  X(09).
                  05  S137-GHKNGK       PIC  X(09).
                  05  S137-GKOHGK       PIC  X(09).
                  05  S137-GSYUEKI      PIC  X(09).
