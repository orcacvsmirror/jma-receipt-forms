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
      *****************************************************************
      *
      *     保険別請求チェック 起動PG      
      *            
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *             @@@@-長屋     07/09/00  4.0対応
      *****************************************************************
       01  A00000S100-LNK.
           03  S100LNK-PRTKANRI-RENNUM              PIC 9(04).
           03  S100LNK-PRTKANRI-TBL-KEY             PIC X(08).
           03  S100LNK-PRTKANRI-TBL-GROUP           PIC X(14).
           03  S100LNK-PRTKANRI-SHORI-RENNUM        PIC 9(04).
           03  S100LNK-PRTKANRI-SRYYM               PIC X(06).
           03  S100LNK-PRTKANRI-SKYYMD              PIC X(08).
           03  S100LNK-PRTKANRI-SHELLID             PIC X(20).
           03  S100LNK-PRTKANRI-PRIORITY            PIC 9(04).
           03  S100LNK-PRTKANRI-TERMID              PIC X(64).
           03  S100LNK-PRTKANRI-OPID                PIC X(16).
           03  S100LNK-PRTKANRI-PRTNM               PIC X(16).
           03  S100LNK-JOBID                        PIC 9(07).
           03  S100LNK-SHELLID                      PIC X(08).
      ***07/09/00 nagaya 4.0対応 start
      *     03  S100LNK-HOSPID                       PIC X(24).
           03  S100LNK-HOSPNUM                      PIC 9(2).
      ***07/09/00 nagaya 4.0対応 end
           03  S100LNK-YM                           PIC X(06).
           03  S100LNK-NYUGAIKBN                    PIC X(01).
           03  S100LNK-SPRT-TITLE                   PIC X(100).
