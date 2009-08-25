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
       01  S137N2.
           03  S137N2-PAGE              PIC  X(03).
           03  S137N2-SYSYMD            PIC  X(22).
           03  S137N2-SRYYM             PIC  X(16).
           03  S137N2-NYUGAI            PIC  X(04).
           03  S137N2-SRYKANAME         PIC  X(20).
           03  S137N2-HKN-TBL OCCURS 7.
               05  S137N2-KENSU         PIC  X(04).
               05  S137N2-NISSU         PIC  X(04).
               05  S137N2-TENSU         PIC  X(09).
               05  S137N2-MADOGK        PIC  X(09).
      *--- 2008/12/18 ADD START ----------------------------------------
               05  S137N2-JIHIGK        PIC  X(09).
      *--- 2008/12/18 ADD END ------------------------------------------
               05  S137N2-HKNGK         PIC  X(09).
               05  S137N2-KOHGK         PIC  X(09).
               05  S137N2-SYUEKI        PIC  X(09).
      *--- 2009/08/20 CHANGE START -------------------------------------
      *         05  S137N2-SNISSU        PIC  X(04).
               05  S137N2-SNISSU        PIC  X(05).
      *--- 2009/08/20 CHANGE END ---------------------------------------
               05  S137N2-SSEIKYU       PIC  X(09).
               05  S137N2-SFTNGK        PIC  X(09).
           03  S137N2-KOHTAN-TBL OCCURS 10.
               05  S137N2-KTKENSU       PIC  X(04).
               05  S137N2-KTNISSU       PIC  X(04).
               05  S137N2-KTTENSU       PIC  X(09).
               05  S137N2-KTMADOGK      PIC  X(09).
      *--- 2008/12/18 ADD START ----------------------------------------
               05  S137N2-KTJIHIGK      PIC  X(09).
      *--- 2008/12/18 ADD END ------------------------------------------
               05  S137N2-KTHKNGK       PIC  X(09).
               05  S137N2-KTKOHGK       PIC  X(09).
               05  S137N2-KTSYUEKI      PIC  X(09).
      *--- 2009/08/20 CHANGE START -------------------------------------
      *         05  S137N2-SKTNISSU      PIC  X(04).
               05  S137N2-SKTNISSU      PIC  X(05).
      *--- 2009/08/20 CHANGE END ---------------------------------------
               05  S137N2-SKTSEIKYU     PIC  X(09).
               05  S137N2-SKTFTNGK      PIC  X(09).
               05  S137N2-KOHTNAME      PIC  X(24).
           03  S137N2-GKOHTAN-TBN.
               05  S137N2-GKTKENSU      PIC  X(04).
               05  S137N2-GKTNISSU      PIC  X(04).
               05  S137N2-GKTTENSU      PIC  X(09).
               05  S137N2-GKTMADOGK     PIC  X(09).
      *--- 2008/12/18 ADD START ----------------------------------------
               05  S137N2-GKTJIHIGK     PIC  X(09).
      *--- 2008/12/18 ADD END ------------------------------------------
               05  S137N2-GKTHKNGK      PIC  X(09).
               05  S137N2-GKTKOHGK      PIC  X(09).
               05  S137N2-GKTSYUEKI     PIC  X(09).
      *--- 2009/08/20 CHANGE START -------------------------------------
      *         05  S137N2-SGKTNISSU     PIC  X(04).
               05  S137N2-SGKTNISSU     PIC  X(05).
      *--- 2009/08/20 CHANGE END ---------------------------------------
               05  S137N2-SGKTSEIKYU    PIC  X(09).
               05  S137N2-SGKTFTNGK     PIC  X(09).
           03  S137N2-KOH-TBL OCCURS 30.
               05  S137N2-KKENSU       PIC  X(04).
               05  S137N2-KNISSU       PIC  X(04).
               05  S137N2-KTENSU       PIC  X(09).
               05  S137N2-KOHNAME      PIC  X(24).
           03  S137N2-GKOH-TBN.
               05  S137N2-GKKENSU      PIC  X(04).
               05  S137N2-GKNISSU      PIC  X(04).
               05  S137N2-GKTENSU      PIC  X(09).
           03  S137N2-GOKEI-TBL.
               05  S137N2-GKENSU       PIC  X(04).
               05  S137N2-GNISSU       PIC  X(04).
               05  S137N2-GTENSU       PIC  X(09).
               05  S137N2-GMADOGK      PIC  X(09).
      *--- 2008/12/18 ADD START ----------------------------------------
               05  S137N2-GJIHIGK      PIC  X(09).
      *--- 2008/12/18 ADD END ------------------------------------------
               05  S137N2-GHKNGK       PIC  X(09).
               05  S137N2-GKOHGK       PIC  X(09).
               05  S137N2-GSYUEKI      PIC  X(09).
      *--- 2009/08/20 CHANGE START -------------------------------------
      *         05  S137N2-SGNISSU      PIC  X(04).
               05  S137N2-SGNISSU      PIC  X(05).
      *--- 2009/08/20 CHANGE END ---------------------------------------
               05  S137N2-SGSEIKYU     PIC  X(09).
               05  S137N2-SGFTNGK      PIC  X(09).
