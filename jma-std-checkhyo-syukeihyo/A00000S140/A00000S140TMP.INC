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
      *******************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev  修正者      日付      内容
      *   01.00.01    @@@@-田中   11/01/05  コメント履歴削除
      *******************************************************************
       01  S140TMP-REC.
           03  S140TMP-KEY.
               05  S140TMP-NYUGAIKBN       PIC  X(01).
               05  S140TMP-SRYKA           PIC  X(02).
           03  S140TMP-KOKUHO-TBL          OCCURS 14.
               05  S140TMP-KENSU           PIC  9(04).
               05  S140TMP-NISSU           PIC  9(04).
               05  S140TMP-TENSU           PIC  9(09).
               05  S140TMP-MADOGK          PIC  S9(09).
               05  S140TMP-JIHIGK          PIC  9(09).
               05  S140TMP-HKNGK           PIC  9(09).
               05  S140TMP-KOHGK           PIC  S9(09).
               05  S140TMP-SYUEKI          PIC  9(09).
               05  S140TMP-SNISSU          PIC  9(05).
               05  S140TMP-SSEIKYU         PIC  9(09).
               05  S140TMP-SFTNGK          PIC  9(09).
           03  S140TMP-KOH1-MAX            PIC  9(02).
           03  S140TMP-KOHI1-TBL           OCCURS 300.
               05  S140TMP-KOH1-KEY.
                   07  S140TMP-RJNKBN      PIC  X(03).
                   07  S140TMP-KOHNUM      PIC  X(03).
               05  S140TMP-KKENSU          PIC  9(04).
               05  S140TMP-KNISSU          PIC  9(04).
               05  S140TMP-KTENSU          PIC  9(09).
           03  S140TMP-HKNJA-TBL           OCCURS 300.
               05  S140TMP-HKNJANUM-KEY.
                   07  S140TMP-HKNJANUM    PIC  X(08).
               05  S140TMP-HKENSU          PIC  9(04).
               05  S140TMP-HNISSU          PIC  9(04).
               05  S140TMP-HTENSU          PIC  9(09).
               05  S140TMP-HMADOGK         PIC  S9(09).
               05  S140TMP-HJIHIGK         PIC  9(09).
               05  S140TMP-HHKNGK          PIC  9(09).
               05  S140TMP-HKOHGK          PIC  S9(09).
               05  S140TMP-HSYUEKI         PIC  9(09).
               05  S140TMP-HSNISSU         PIC  9(05).
               05  S140TMP-HSSEIKYU        PIC  9(09).
               05  S140TMP-HSFTNGK         PIC  9(09).
           03  S140TMP-GOKEI-TBL.
               05  S140TMP-GKENSU          PIC  9(04).
               05  S140TMP-GNISSU          PIC  9(04).
               05  S140TMP-GTENSU          PIC  9(09).
               05  S140TMP-GMADOGK         PIC  S9(09).
               05  S140TMP-GJIHIGK         PIC  9(09).
               05  S140TMP-GHKNGK          PIC  9(09).
               05  S140TMP-GKOHGK          PIC  S9(09).
               05  S140TMP-GSYUEKI         PIC  9(09).
               05  S140TMP-SGNISSU         PIC  9(05).
               05  S140TMP-SGSEIKYU        PIC  9(09).
               05  S140TMP-SGFTNGK         PIC  9(09).
