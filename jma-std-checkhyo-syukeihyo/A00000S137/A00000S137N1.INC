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
      *  コピー句修正履歴
      *  Maj/Min/Rev  修正者      日付      内容
      *   01.00.01    @@@@-田中   11/01/05  コメント履歴削除
      *******************************************************************
       01  S137N1.
           03  S137N1-PAGE              PIC  X(03).
           03  S137N1-SYSYMD            PIC  X(22).
           03  S137N1-SRYYM             PIC  X(16).
           03  S137N1-NYUGAI            PIC  X(04).
           03  S137N1-SRYKANAME         PIC  X(20).
           03  S137N1-HKN-TBL OCCURS 46.
               05  S137N1-KENSU         PIC  X(04).
               05  S137N1-NISSU         PIC  X(04).
               05  S137N1-TENSU         PIC  X(09).
               05  S137N1-MADOGK        PIC  X(09).
               05  S137N1-JIHIGK        PIC  X(09).
               05  S137N1-HKNGK         PIC  X(09).
               05  S137N1-KOHGK         PIC  X(09).
               05  S137N1-SYUEKI        PIC  X(09).
               05  S137N1-SNISSU        PIC  X(05).
               05  S137N1-SSEIKYU       PIC  X(09).
               05  S137N1-SFTNGK        PIC  X(09).
