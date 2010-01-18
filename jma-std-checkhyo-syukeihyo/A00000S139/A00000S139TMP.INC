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
       01  S139TMP-REC.
              03  S139TMP-KEY.
                  05  S139TMP-SRYKA           PIC  X(02).
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
                  05  S139TMP-SRYYMKBN           PIC  X(01).
      * 20080512 返戻が正しく印字されない不具合を修正END
              03  S139TMP-MEISAI-AREA.
                  05  S139TMP-NYUGAI-TBL OCCURS 2.
      * 20080423 H20年4月改正対応START tanaka
      *                07  S139-HKNTMP-TBL OCCURS  16.
                      07  S139-HKNTMP-TBL OCCURS  18.
      * 20080423 H20年4月改正対応END
                          09  S139TMP-KENSU      PIC  9(04).
                          09  S139TMP-NISSU      PIC  9(04).
                          09  S139TMP-TENSU      PIC  9(09).
      *--- (2008/12/26) line change start monma -------------------------
      *                    09  S139TMP-MADOGK     PIC  9(09).
                          09  S139TMP-MADOGK     PIC  S9(09).
      *--- (2008/12/26) line change end   monma -------------------------
      *--- (2008/12/18) line add start monma ----------------------------
                          09  S139TMP-JIHIGK     PIC  9(09).
      *--- (2008/12/18) line add end   monma ----------------------------
                          09  S139TMP-HKNGK      PIC  9(09).
      *--- 2010/01/13 CHANGE START -------------------------------------
      *                    09  S139TMP-KOHGK      PIC  9(09).
                          09  S139TMP-KOHGK      PIC  S9(09).
      *--- 2010/01/13 CHANGE END ---------------------------------------
                          09  S139TMP-SYUEKI     PIC  9(09).
                          09  S139TMP-SNISSU     PIC  9(05).
                          09  S139TMP-SSEIKYU    PIC  9(09).
                          09  S139TMP-SFTNGK     PIC  9(09).
      * 20080423 H20年4月改正対応START tanaka
      *    １、社保 ２、国保 ３、公費 ４、保険合計 ５、後期高齢者
      *                07  S139TMP-SYOKEI-TBL OCCURS 4.
                      07  S139TMP-SYOKEI-TBL OCCURS 5.
      * 20080423 H20年4月改正対応END
                          09  S139TMP-S-KENSU      PIC  9(04).
                          09  S139TMP-S-NISSU      PIC  9(04).
                          09  S139TMP-S-TENSU      PIC  9(09).
      *--- (2008/12/26) line change start monma -------------------------
      *                    09  S139TMP-S-MADOGK     PIC  9(09).
                          09  S139TMP-S-MADOGK     PIC  S9(09).
      *--- (2008/12/26) line change end   monma -------------------------
      *--- (2008/12/18) line add start monma ----------------------------
                          09  S139TMP-S-JIHIGK     PIC  9(09).
      *--- (2008/12/18) line add end   monma ----------------------------
                          09  S139TMP-S-HKNGK      PIC  9(09).
      *--- 2010/01/13 CHANGE START -------------------------------------
      *                    09  S139TMP-S-KOHGK      PIC  9(09).
                          09  S139TMP-S-KOHGK      PIC  S9(09).
      *--- 2010/01/13 CHANGE END ---------------------------------------
                          09  S139TMP-S-SYUEKI     PIC  9(09).
                          09  S139TMP-S-SNISSU     PIC  9(05).
                          09  S139TMP-S-SSEIKYU    PIC  9(09).
                          09  S139TMP-S-SFTNGK     PIC  9(09).
      * 20080423 H20年4月改正対応START tanaka
      *    １、自費 ２、労災 ３、自賠責 ４、公害 ５、合計
      *                07  S139TMP-ETC-TBL OCCURS 4.
                      07  S139TMP-ETC-TBL OCCURS 5.
      * 20080423 H20年4月改正対応END
                          09  S139TMP-ETC-KENSU      PIC  9(04).
                          09  S139TMP-ETC-NISSU      PIC  9(04).
                          09  S139TMP-ETC-TENSU      PIC  9(09).
      *--- (2008/12/26) line change start monma -------------------------
      *                    09  S139TMP-ETC-MADOGK     PIC  9(09).
                          09  S139TMP-ETC-MADOGK     PIC  S9(09).
      *--- (2008/12/26) line change end   monma -------------------------
      *--- (2008/12/18) line add start monma ----------------------------
                          09  S139TMP-ETC-JIHIGK     PIC  9(09).
      *--- (2008/12/18) line add end   monma ----------------------------
                          09  S139TMP-ETC-HKNGK      PIC  9(09).
                          09  S139TMP-ETC-SYUEKI     PIC  9(09).
                          09  S139TMP-ETC-SNISSU     PIC  9(05).
                          09  S139TMP-ETC-SSEIKYU    PIC  9(09).
                          09  S139TMP-ETC-SFTNGK     PIC  9(09).
      * 20080423 H20年4月改正対応START tanaka
      *    総合計エリア １ 保険 ２ 自費 ３ 労災 ４ 自賠責 ５ 公害 ６ 総合計
      *            05  S139TMP-GOKEI-TBL OCCURS 5.
                  05  S139TMP-GOKEI-TBL OCCURS 6.
      * 20080423 H20年4月改正対応END
                      07  S139TMP-GKENSU      PIC  9(04).
                      07  S139TMP-GNISSU      PIC  9(04).
                      07  S139TMP-GTENSU      PIC  9(09).
      *--- (2008/12/26) line change start monma -------------------------
      *                07  S139TMP-GMADOGK     PIC  9(09).
                      07  S139TMP-GMADOGK     PIC  S9(09).
      *--- (2008/12/26) line change end   monma -------------------------
      *--- (2008/12/18) line add start monma ----------------------------
                      07  S139TMP-GJIHIGK     PIC  9(09).
      *--- (2008/12/18) line add end   monma ----------------------------
                      07  S139TMP-GHKNGK      PIC  9(09).
      *--- 2010/01/13 CHANGE START -------------------------------------
      *                07  S139TMP-GKOHGK      PIC  9(09).
                      07  S139TMP-GKOHGK      PIC  S9(09).
      *--- 2010/01/13 CHANGE END ---------------------------------------
                      07  S139TMP-GSYUEKI     PIC  9(09).
                      07  S139TMP-SGNISSU     PIC  9(05).
                      07  S139TMP-SGSEIKYU    PIC  9(09).
                      07  S139TMP-SGFTNGK     PIC  9(09).
