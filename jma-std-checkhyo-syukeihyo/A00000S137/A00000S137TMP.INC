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
       01  S137TMP-REC.
           03  S137TMP-KEY.
               05  S137TMP-NYUGAIKBN       PIC  X(01).
               05  S137TMP-SRYKA           PIC  X(02).
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
               05  S137TMP-SRYYMKBN        PIC  X(01).
      * 20080512 返戻が正しく印字されない不具合を修正END
           03  S137TMP-SRY-TBL OCCURS 53.
               05  S137TMP-KENSU           PIC  9(04).
               05  S137TMP-NISSU           PIC  9(04).
               05  S137TMP-TENSU           PIC  9(09).
      *--- 2008/12/18 ADD START ----------------------------------------
      *         05  S137TMP-MADOGK          PIC  9(09).
               05  S137TMP-MADOGK          PIC  S9(09).
               05  S137TMP-JIHIGK          PIC  9(09).
      *--- 2008/12/18 ADD END ------------------------------------------
               05  S137TMP-HKNGK           PIC  9(09).
      *--- 2010/01/13 CHANGE START -------------------------------------
      *         05  S137TMP-KOHGK           PIC  9(09).
               05  S137TMP-KOHGK           PIC  S9(09).
      *--- 2010/01/13 CHANGE END ---------------------------------------
               05  S137TMP-SYUEKI          PIC  9(09).
      *--- 2009/08/20 CHANGE START -------------------------------------
      *         05  S137TMP-SNISSU          PIC  9(04).
               05  S137TMP-SNISSU          PIC  9(05).
      *--- 2009/08/20 CHANGE END ---------------------------------------
               05  S137TMP-SSEIKYU         PIC  9(09).
               05  S137TMP-SFTNGK          PIC  9(09).
           03  S137TMP-KOH-MAX             PIC  9(02).
           03  S137TMP-KOHTAN-TBL OCCURS 10.
               05  S137TMP-KOHTAN-KEY.
                   07  S137TMP-KOHTNUM      PIC  X(03).
                   07  S137TMP-KOHTNUM2     PIC  X(03).
               05  S137TMP-KTKENSU          PIC  9(04).
               05  S137TMP-KTNISSU          PIC  9(04).
               05  S137TMP-KTTENSU          PIC  9(09).
      *--- 2008/12/18 ADD START ----------------------------------------
      *         05  S137TMP-KTMADOGK         PIC  9(09).
               05  S137TMP-KTMADOGK         PIC  S9(09).
               05  S137TMP-KTJIHIGK         PIC  9(09).
      *--- 2008/12/18 ADD END ------------------------------------------
               05  S137TMP-KTHKNGK          PIC  9(09).
      *--- 2010/01/13 CHANGE START -------------------------------------
      *         05  S137TMP-KTKOHGK          PIC  9(09).
               05  S137TMP-KTKOHGK          PIC  S9(09).
      *--- 2010/01/13 CHANGE END ---------------------------------------
               05  S137TMP-KTSYUEKI         PIC  9(09).
      *--- 2009/08/20 CHANGE START -------------------------------------
      *         05  S137TMP-SKTNISSU         PIC  9(04).
               05  S137TMP-SKTNISSU         PIC  9(05).
      *--- 2009/08/20 CHANGE END ---------------------------------------
               05  S137TMP-SKTSEIKYU        PIC  9(09).
               05  S137TMP-SKTFTNGK         PIC  9(09).
           03  S137TMP-KOH-TBL OCCURS 50.
               05  S137TMP-KOH-KEY.
                   07  S137TMP-RJNKBN       PIC  X(03).
                   07  S137TMP-KOHNUM       PIC  X(03).
               05  S137TMP-KKENSU           PIC  9(04).
               05  S137TMP-KNISSU           PIC  9(04).
               05  S137TMP-KTENSU           PIC  9(09).
           03  S137TMP-GOKEI-TBL.
               05  S137TMP-GKENSU           PIC  9(04).
               05  S137TMP-GNISSU           PIC  9(04).
               05  S137TMP-GTENSU           PIC  9(09).
      *--- 2008/12/18 ADD START ----------------------------------------
      *         05  S137TMP-GMADOGK          PIC  9(09).
               05  S137TMP-GMADOGK          PIC  S9(09).
               05  S137TMP-GJIHIGK          PIC  9(09).
      *--- 2008/12/18 ADD END ------------------------------------------
               05  S137TMP-GHKNGK           PIC  9(09).
      *--- 2010/01/13 CHANGE START -------------------------------------
      *         05  S137TMP-GKOHGK           PIC  9(09).
               05  S137TMP-GKOHGK           PIC  S9(09).
      *--- 2010/01/13 CHANGE END ---------------------------------------
               05  S137TMP-GSYUEKI          PIC  9(09).
      *--- 2009/08/20 CHANGE START -------------------------------------
      *         05  S137TMP-SGNISSU          PIC  9(04).
               05  S137TMP-SGNISSU          PIC  9(05).
      *--- 2009/08/20 CHANGE END ---------------------------------------
               05  S137TMP-SGSEIKYU         PIC  9(09).
               05  S137TMP-SGFTNGK          PIC  9(09).
