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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000D500.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 日計表（ＨＣ１７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/09/29    @@@@-有村     新規作成(ORCBD500のプログラムID変更Ver)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-本橋    03/10/14  収納明細情報の取得方法変更
      *              @@@@-半田    03/11/11  自費保険番号拡張対応の判定ロジック追加
      *              @@@@-半田    03/12/11  帳票タイトルの変更対応
      *              @@@@-半田    03/12/24  労災、自陪責の保険手数を未印字にする
      *              @@@@-半田    04/02/02  労災、自陪責の保険手数を印字する
      *                                     医療収益の算出方法の変更
      *              @@@@-半田    04/04/21  非保険診療時の自費列診療金額算出のバグ対応
      *              @@@@-本橋    04/08/18  点数、金額がない患者は印字しないようにする
      *                                     ページごとの小計を追加
      *              @@@@-本橋    04/09/16  MONPE化
      *              @@@@-鈴木    05/02/28  open-cobol 0.23への対応による変更
      *                                     (INCファイルのみ)
      *              @@@@-本橋    05/09/14  有効期限が切られた診療科名が印字されない
      *                                     不具合を修正
      *                                     ORCMCPSUB → MCPSUBに変更
      *              @@@@-高嶋    06/08/01  sarge対応
      *              @@@@-高嶋    06/10/21  自費追加対応
      *              @@@@-高嶋    06/12/18  自賠責計算式変更対応
      *              @@@@-内田    06/12/22  特別療養費（068,069）を国保で扱う
      *              @@@@-内田    07/03/20  消費税内税の影響対応
      *              @@@@-高嶋    07/09/10  4.0対応
      *              @@@@-田中    08/03/24  H20年4月改正対応
      *              @@@@-田中    08/06/11  健保準拠時の自賠責の計算を修正
      *              @@@@-門間    08/11/04  1枚対応
      *              @@@@-門間    08/11/07  公害が自賠責に集計されていた不具合の修正
      *              @@@@-田中    08/12/16  自費分の減免金額を集計するように修正
      *              @@@@-田中    08/12/17  医療収益に減免金額を集計するように修正
      *              @@@@-田中    09/02/06  減免金額の計算式を修正
      *              @@@@-田中    09/02/17  減免計算式の条件式を修正
      *              @@@@-田中    09/02/20  印字順序の変更パラメータの追加
      *              @@@@-田中    09/02/24  合計の頁を作成
      *              @@@@-門間    09/03/02  自費保険の詳細を出力するように修正
      *              @@@@-門間    09/03/23  外来１頁目に入院最終頁の入外区分・
      *                                     診療科が印字されていた不具合を修正
      *              @@@@-門間    09/04/08  医療収益の計算式を修正
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域 
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //RECEERR//.
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      ***** 03/09/29
      *     COPY    "HC17V011S.INC".
           COPY    "A00000D500.INC".
      *--- 2009/02/24 ADD START ----------------------------------------
           COPY    "A00000D5002.INC".
      *--- 2009/02/24 ADD END ------------------------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *     COPY    "A00000D5001.INC".
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *****
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-GET             PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI2       PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-S               PIC 9(05).
           03  IDX-N               PIC 9(05).
           03  IDX-MAX             PIC 9(05).
      *
           03  IDX2                PIC 9(04).
           03  JIHIIDX             PIC 9(04).
           03  IDX-SUM             PIC 9(04).
      *
      *=== 自費追加対応 UPDATE By Takashima(2006/10/21) start
           03  IDX3                PIC 9(04).
      *=== 自費追加対応 UPDATE By Takashima(2006/10/21) end
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     03  WRK-PARA-HOSPID     PIC X(24).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *=== 4.0対応 By Takashima(2007/09/10) end
           03  WRK-PARA-DENPPRTYMD PIC X(08).
      *--- 2009/02/20 ADD START ----------------------------------------
           03  WRK-PARA-SORTKBN    PIC X(01).
           03  WRK-PARA-GOKEIKBN   PIC X(01).
      *--- 2009/02/20 ADD END ------------------------------------------
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH              PIC X(64).
      *
           03  WRK-RECEERR           PIC X(200).
      *
           03  WRK-PARA-DENPPRTYMDWH PIC X(22).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-RENNUM            PIC 9(03).
           03  WRK-PTNUM             PIC X(20).
      *** 20030407 変更 患者番号を８桁に拡張 
      ***     03  WRK-PTNUM6            PIC 9(06). 
           03  WRK-PTNUM8            PIC 9(08). 
      *** 20030407 変更 患者番号を８桁に拡張 
      *  
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      * 20030407 追加
           03  WRK-NYUGAIKBN         PIC X(01).
           03  WRK-SRYKA             PIC X(02).
      * 20030407 追加
      *
      *****03  WRK-IN                PIC X(01).
      *
      * 20030224点数及び金額の桁数を８桁から７桁に変更
      ***    03  WRK-MONEY             PIC 9(08).
      ***    03  WRK-JIHI              PIC 9(08).
      *--- 2008/12/16 ADD START ----------------------------------------
      *     03  WRK-MONEY             PIC 9(07).
           03  WRK-MONEY             PIC S9(07).
      *--- 2008/12/16 ADD END ------------------------------------------
           03  WRK-JIHI              PIC S9(07).
           03  WRK-SYUEKI            PIC S9(07).
      * 20030224点数及び金額の桁数を８桁から７桁に変更
      *
      * 20030224点数及び金額の桁数を８桁から７桁に変更
      ***    03  WRK-JIHI2            PIC 9(08).
           03  WRK-JIHI2            PIC 9(07).
           03  WRK-JIHI3            PIC 9(07).
      * 20030224点数及び金額の桁数を８桁から７桁に変更
      *
      *=== 自費追加対応 UPDATE By Takashima(2006/10/21) start
      *     03  WRK-TJIHI-G       OCCURS  5.
           03  WRK-TJIHI-G       OCCURS  10.
      *=== 自費追加対応 UPDATE By Takashima(2006/10/21) end
               05  WRK-TJIHI     PIC X(08).
      * 20030407 追加 入院自費名称
      *=== 自費追加対応 UPDATE By Takashima(2006/10/21) start
      *     03  WRK-TJIHIN-G      OCCURS  5.
           03  WRK-TJIHIN-G      OCCURS  10.
      *=== 自費追加対応 UPDATE By Takashima(2006/10/21) end
               05  WRK-TJIHIN    PIC X(08).
      * 20030407 追加 入院自費名称
      *
           03  WRK-4001-TENTANKA PIC 9(02)V9(01).
      *
           03  WRK-RENNUM-X          PIC ZZ9.
           03  WRK-PAGE              PIC ZZ9.
      * 20030224点数及び金額の桁数を８桁から７桁に変更 --> これにより編集タイプの桁数も変更
      ***     03  WRK-Z9                PIC ZZZZZZZ9.
      ***     03  WRK-ZZ                PIC ZZZZZZZZ.
      ***     03  WRK-S9                PIC -------9.
      ***     03  WRK-SZ                PIC -------Z.
           03  WRK-Z9                PIC ZZZZZZ9.
           03  WRK-ZZ                PIC ZZZZZZZ.
           03  WRK-ZZ9               PIC ZZZZZZZ9.
           03  WRK-ZZZ               PIC ZZZZZZZZ.
           03  WRK-S9                PIC -------9.
           03  WRK-SZ                PIC -------Z.
           03  WRK-SZ9               PIC --------9.
           03  WRK-SZZ               PIC --------Z.
      *--- 2008/12/16 ADD START ----------------------------------------
           03  WRK-S9-2              PIC ------9.
           03  WRK-Z9-2              PIC ------Z.
      *--- 2008/12/16 ADD END ------------------------------------------
      *--- 2009/02/06 ADD START ----------------------------------------
           03  WRK-COMPFTN           PIC S9(10).
      *--- 2009/02/06 ADD END ------------------------------------------
      *--- 2009/02/24 ADD START ----------------------------------------
           03  WRK-SZ10              PIC ---------Z.
      *--- 2009/02/24 ADD END ------------------------------------------
      * 20030224点数及び金額の桁数を８桁から７桁に変更 --> これにより編集タイプの桁数も変更
      *
      *** 01  SUM-AREA                             VALUE   ZERO.
      ***     03  SUM-OCC               OCCURS  15.
      ***         05  SUM-KENSU         PIC 9(05).
      ***         05  SUM-HKNTEN        PIC 9(08).
      ***         05  SUM-MONEY         PIC 9(08).
       01  SUM-AREA                             VALUE   ZERO.
           03  SUM-HKNTEN      PIC 9(08).
           03  SUM-HKNKEN      PIC 9(08).
      *=== 自費追加対応 UPDATE By Takashima(2006/10/21) start
      *     03  SUM-OCC         OCCURS  14.
      * 20080324 H20年4月改正対応START tanaka
      *     03  SUM-OCC         OCCURS  19.
           03  SUM-OCC         OCCURS  20.
      * 20080324 H20年4月改正対応END
      *=== 自費追加対応 UPDATE By Takashima(2006/10/21) end
      *--- 2008/12/16 CHANGE START -------------------------------------
      *         05  SUM-MONEY   PIC 9(08).
               05  SUM-MONEY   PIC S9(08).
      *--- 2008/12/16 CHANGE END ---------------------------------------
               05  SUM-KEN     PIC 9(08).
           03  SUM-SYUEKI      PIC 9(08).
           03  SUM-SEIKYU      PIC S9(08).
           03  SUM-RYOSYU      PIC S9(08).
           03  SUM-SEIKYUKEN   PIC 9(08).
           03  SUM-RYOSYUKEN   PIC 9(08).
      *
       01  SUM-G-AREA                           VALUE   ZERO.
           03  SUM-G-HKNTEN      PIC 9(09).
      *=== 自費追加対応 UPDATE By Takashima(2006/10/21) start
      *     03  SUM-G-OCC         OCCURS  14.
      * 20080324 H20年4月改正対応START tanaka
      *     03  SUM-G-OCC         OCCURS  19.
           03  SUM-G-OCC         OCCURS  20.
      * 20080324 H20年4月改正対応END
      *=== 自費追加対応 UPDATE By Takashima(2006/10/21) end
      *--- 2008/12/16 CHANGE START -------------------------------------
      *         05  SUM-G-MONEY   PIC 9(09).
               05  SUM-G-MONEY   PIC S9(09).
      *--- 2008/12/16 CHANGE END ----------------------------------------
           03  SUM-G-SYUEKI      PIC S9(09).
           03  SUM-G-SEIKYU      PIC S9(09).
           03  SUM-G-RYOSYU      PIC S9(09).
      *
      *--- 2009/02/24 ADD START ----------------------------------------
       01  SUM-GG-AREA            VALUE   ZERO.
           03  SUM-GG-HKNTEN      PIC 9(09).
           03  SUM-GG-SYUEKI      PIC S9(10).
           03  SUM-GG-OCC         OCCURS  20.
               05  SUM-GG-MONEY   PIC S9(08).
           03  SUM-GG-SEIKYU      PIC S9(10).
           03  SUM-GG-RYOSYU      PIC S9(10).
      *--- 2009/02/24 ADD END ------------------------------------------
      *** 20030407 収納明細抽出レコード変更
      *** 01  WRK-SMEI-REC.
      ***     03  WRK-SMEI-TBL      OCCURS  15000.
      ***         05  WRK-SMEI-HOSPID          PIC X(24).
      ***         05  WRK-SMEI-NYUGAIKBN       PIC X(01).
      ***         05  WRK-SMEI-PTID            PIC 9(10).
      ***         05  WRK-SMEI-SORT-KEY. 
      ***           07  WRK-SMEI-DENPNUM       PIC 9(07).
      ***           07  WRK-SMEI-DENPEDANUM    PIC 9(02).
      ***         05  WRK-SMEI-SKYMONEY        PIC S9(07).
      ***         05  WRK-SMEI-NYUHEN-MONEY    PIC S9(07).
      ***         05  WRK-SMEI-NYUHEN-YMD      PIC X(08).
      ***     03  WRK-SV-SMEI-TBL              PIC X(66). 
       01  WRK-SMEI-REC.
           03  WRK-SMEI-TBL      OCCURS  15000.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         05  WRK-SMEI-HOSPID          PIC X(24).
               05  WRK-SMEI-HOSPNUM         PIC 9(02).
      *=== 4.0対応 By Takashima(2007/09/10) end
               05  WRK-SMEI-PTID            PIC 9(10).
               05  WRK-SMEI-SORT-KEY. 
                 07  WRK-SMEI-NYUGAIKBN     PIC X(01).
                 07  WRK-SMEI-SRYKA         PIC X(02).
                 07  WRK-SMEI-DENPNUM       PIC 9(07).
                 07  WRK-SMEI-DENPEDANUM    PIC 9(02).
      *--- 2009/02/20 ADD START ----------------------------------------
               05  WRK-SMEI-SORT-KEY2. 
                 07  WRK-SMEI-NYUGAIKBN     PIC X(01).
                 07  WRK-SMEI-SRYKA         PIC X(02).
                 07  WRK-SMEI-PTNUM         PIC X(20).
      *--- 2009/02/20 ADD END ------------------------------------------
               05  WRK-SMEI-SKYMONEY        PIC S9(07).
               05  WRK-SMEI-NYUHEN-MONEY    PIC S9(07).
               05  WRK-SMEI-NYUHEN-YMD      PIC X(08).
           03  WRK-SV-SMEI-TBL              PIC X(68). 
      *** 20030407 収納明細抽出レコード変更
      *
       01  WRK-CONS-AREA.
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973". 
      * 20080324 H20年4月改正対応START tanaka
      *    公害保険番号 
           03  WRK-CONS-KOGAI-HKNNUM   PIC X(03)   VALUE   "975". 
      * 20080324 H20年4月改正対応END
      *
           COPY    "CPSHELLTBL.INC".
      *     
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     COPY    "ORCA-DBPATH".         
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
      *
      *    自費管理情報
           COPY  "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    労災医療機関情報
           COPY  "CPSK4001.INC".
      *
      *    収納マスタ
      * 01  BD002-REC.
      *     COPY    "CPBD002.INC".
      *
      *    患者保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスタ
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL  FLG-END  =  1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-AREA
           INITIALIZE                  WRK-SMEI-REC
           INITIALIZE                  SUM-G-AREA
      *=== 4.0対応 By Takashima(2007/09/10) start
           INITIALIZE                  SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *--- 2009/02/24 ADD START ----------------------------------------
           INITIALIZE                  SUM-GG-AREA
      *--- 2009/02/24 ADD END ----------------------------------------
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                                         WRK-PARA-HOSPID
                                               WRK-PARA-HOSPNUM
                                               RECEERR
      *=== 4.0対応 By Takashima(2007/09/10) end
                                               WRK-PARA-DENPPRTYMD
      *=== 4.0対応 By Takashima(2007/09/10) start
      *--- 2009/02/20 ADD START ----------------------------------------
                                               WRK-PARA-SORTKBN
                                               WRK-PARA-GOKEIKBN
      *--- 2009/02/20 ADD END ------------------------------------------
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO   SPA-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *    ステップ管理開始処理
           MOVE    "STS"              TO   SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM        TO   JOB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-JOBID     TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID   TO   JOB-SHELLID
           MOVE    "ORCBD002"         TO   JOB-PGID
      ***     MOVE    "日計表（伝票発行日）"
      * 20031211 帳票タイトル変更 
      ***     MOVE    "日計表（明細）"   TO  JOB-SHELLMSG
           MOVE    "日計表明細（窓口領収金）"   TO  JOB-SHELLMSG
      * 20031211 帳票タイトル変更 
           CALL    "ORCSJOB"          USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                      SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    "RECEERR"       TO  RECEERR-FILE-ID
      *     MOVE    LNK-PRTKANRI-TERMID
      *                             TO  RECEERR-TERMID
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD   TO   WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO   WRK-SYSYMDWH
      *
      *    対象年月日編集
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO   WRK-PARA-DENPPRTYMDWH
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE                 TO   SYS-1001-REC
           INITIALIZE                    SYS-1001-REC
           MOVE    "1001"                TO   SYS-1001-KANRICD
           MOVE    "*"                   TO   SYS-1001-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1001-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
      ***  20050914  MOTO
           MOVE  WRK-PARA-DENPPRTYMD     TO   SYS-1001-EDYUKYMD
                                              SYS-1001-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1001-REC          TO   MCPDATA-REC
           PERFORM   800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC       TO   SYS-1001-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    SYS-1001-HOSPID     TO  WRK-PARA-HOSPID
             MOVE    SYS-1001-HOSPNUM  TO   WRK-PARA-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           ELSE
             MOVE    "医療機関ＩＤが取得できませんでした。"
                                         TO  WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    自費情報取得
      *    区分11
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "11"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                            SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2       =   ZERO
             MOVE    MCPDATA-REC       TO   SYS-1013-REC
             MOVE    SYS-1013-JIHINAME TO   WRK-TJIHIN(1)
           ELSE
             MOVE    SPACE             TO   WRK-TJIHIN(1)
           END-IF
      *  
      *    区分12
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "12"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHIN(2)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHIN(2)
           END-IF
      *  
      *    区分13
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE   "1013"                TO   SYS-1013-KANRICD
           MOVE   "13"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE   SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE   WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE   WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE   SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHIN(3)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHIN(3)
           END-IF
      *  
      *    区分14
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "14"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE  WRK-PARA-DENPPRTYMD    TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHIN(4)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHIN(4)
           END-IF
      *
      *    区分15
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "15"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHIN(5)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHIN(5)
           END-IF
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *
      *    区分16
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "16"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHIN(6)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHIN(6)
           END-IF
      *
      *    区分17
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "17"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2        =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHIN(7)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHIN(7)
           END-IF
      *
      *    区分18
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "18"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHIN(8)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHIN(8)
           END-IF
      *
      *    区分19
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "19"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHIN(9)
           ELSE
             MOVE   SPACE               TO  WRK-TJIHIN(9)
           END-IF
      *
      *    区分10
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "10"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1013-EDYUKYMD
                                            SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHIN(10)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHIN(10)
           END-IF
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *  
      *    自費情報取得
      *    区分21
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "21"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHI(1)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHI(1)
           END-IF
      *  
      *    区分22
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "22"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF    FLG-SYSKANRI2   =   ZERO
             MOVE  MCPDATA-REC          TO   SYS-1013-REC
             MOVE  SYS-1013-JIHINAME    TO   WRK-TJIHI(2)
           ELSE
             MOVE  SPACE                TO   WRK-TJIHI(2)
           END-IF
      *  
      *    区分23
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "23"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE  WRK-PARA-DENPPRTYMD    TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHI(3)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHI(3)
           END-IF
      *
      *    区分24
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "24"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHI(4)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHI(4)
           END-IF
      *
      *    区分25
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "25"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF    FLG-SYSKANRI2   =   ZERO
               MOVE  MCPDATA-REC        TO   SYS-1013-REC
               MOVE  SYS-1013-JIHINAME  TO   WRK-TJIHI(5)
           ELSE
               MOVE  SPACE              TO   WRK-TJIHI(5)
           END-IF                                     
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *
      *    区分26
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "26"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHI(6)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHI(6)
           END-IF
      *  
      *    区分27
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "27"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHI(7)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHI(7)
           END-IF
      *  
      *    区分28
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "28"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHI(8)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHI(8)
           END-IF
      *  
      *    区分29
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "29"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHI(9)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHI(9)
           END-IF
      *  
      *    区分20
           MOVE    SPACE                TO   SYS-1013-REC
           INITIALIZE                        SYS-1013-REC
           MOVE    "1013"               TO   SYS-1013-KANRICD
           MOVE    "20"                 TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1013-EDYUKYMD
                                             SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC         TO   MCPDATA-REC
           PERFORM 820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME   TO   WRK-TJIHI(10)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHI(10)
           END-IF
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *  
      *    労災医療機関情報
           MOVE    SPACE                TO   SYS-4001-REC
           INITIALIZE                        SYS-4001-REC
           MOVE    "4001"               TO   SYS-4001-KANRICD
           MOVE    "*"                  TO   SYS-4001-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-4001-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-4001-EDYUKYMD
                                             SYS-4001-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-4001-REC         TO   MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-4001-REC
             EVALUATE  SYS-4001-TENKBN
               WHEN  "1"
                 MOVE    11.5           TO   WRK-4001-TENTANKA
               WHEN  "2"
                 MOVE    12             TO   WRK-4001-TENTANKA
               WHEN  OTHER
                 MOVE    10             TO   WRK-4001-TENTANKA
             END-EVALUATE
           ELSE
             MOVE    10                 TO   WRK-4001-TENTANKA
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM  310-DATA-SELECT-SEC
      *
           PERFORM  UNTIL FLG-END   =   1
      *** 20030407 削除 ヘッダー部印刷タイミング変更の為削除
      *        帳票編集<見出し>処理
      ***         PERFORM 310-HEAD-HEN-SEC
      *** 20030407 削除 ヘッダー部印刷タイミング変更の為削除
      *        帳票編集<明細>処理
             PERFORM  320-BODY-HEN-SEC
      *    帳票印刷処理
      *--- 2009/02/24 ADD START ----------------------------------------
      *--- 2009/03/23 DELETE START -------------------------------------
      *       IF   WRK-PARA-GOKEIKBN  NOT =  "2"
      *--- 2009/03/23 DELETE END   -------------------------------------
      *--- 2009/02/24 ADD END ------------------------------------------
               IF   FLG-GET  =  1
      *--- 2009/03/23 DELETE START -------------------------------------
      *           PERFORM 390-PRINT-OUT-SEC
      *--- 2009/03/23 DELETE END   -------------------------------------
      *--- 2009/03/23 ADD START ----------------------------------------
                 EVALUATE  WRK-PARA-GOKEIKBN
                   WHEN  "1"
                     PERFORM  390-PRINT-OUT-SEC
      *--- 2009/03/23 ADD START ----------------------------------------
                     IF   WRK-SMEI-NYUGAIKBN(IDX-S) NOT =
                                        WRK-SMEI-NYUGAIKBN(IDX-S - 1)
                       OR WRK-SMEI-SRYKA(IDX-S)     NOT =
                                        WRK-SMEI-SRYKA(IDX-S - 1)
      *--- 2009/03/23 ADD END   ----------------------------------------
                       PERFORM  400-PRINT-OUT-SEC
      *--- 2009/03/23 ADD START ----------------------------------------
                       INITIALIZE       SUM-GG-AREA
                     END-IF
      *--- 2009/03/23 ADD END   ----------------------------------------
                   WHEN  "2"
      *--- 2009/03/23 ADD START ----------------------------------------
                     IF   WRK-SMEI-NYUGAIKBN(IDX-S) NOT =
                                        WRK-SMEI-NYUGAIKBN(IDX-S - 1)
                       OR WRK-SMEI-SRYKA(IDX-S)     NOT =
                                        WRK-SMEI-SRYKA(IDX-S - 1)
      *--- 2009/03/23 ADD END   ----------------------------------------
                       PERFORM  400-PRINT-OUT-SEC
      *--- 2009/03/23 ADD START ----------------------------------------
                       INITIALIZE       SUM-GG-AREA
                     END-IF
      *--- 2009/03/23 ADD END ----------------------------------------
                   WHEN  OTHER
                     PERFORM  390-PRINT-OUT-SEC
                 END-EVALUATE
      *--- 2009/03/23 ADD END   ----------------------------------------
               END-IF
      *--- 2009/02/24 ADD START ----------------------------------------
      *--- 2009/03/23 DELETE START -------------------------------------
      *       END-IF
      *--- 2009/03/23 DELETE END   -------------------------------------
      *--- 2009/02/24 ADD END ------------------------------------------
           END-PERFORM
      *
      *--- 2009/02/24 ADD START ----------------------------------------
      *    合計印字処理
      *--- 2009/03/23 DELETE START -------------------------------------
      *     IF   WRK-PARA-GOKEIKBN  =  "1"  OR  "2"
      *       PERFORM  320-HEAD-HEN-SEC
      *       PERFORM  3000-GOUKEI-SET-SEC
      *       PERFORM  400-PRINT-OUT-SEC
      *     END-IF
      *--- 2009/03/23 DELETE END   -------------------------------------
      *--- 2009/02/24 ADD END ------------------------------------------
      *    帳票印刷処理
           IF  FLG-GET = ZERO
             PERFORM 310-HEAD-HEN-SEC
             MOVE  "＊＊＊  対象データが１件も存在しません。  ＊＊＊"
                                               TO   HC17-JOBMSG
             PERFORM 390-PRINT-OUT-SEC
           END-IF
      *
      *=== sarge対応 DEL By Takashima(2006/08/01) start
      *     PERFORM 910-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/08/01) end
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象データ選定処理
      *****************************************************************
       310-DATA-SELECT-SEC          SECTION.
      *
           MOVE     0       TO   IDX-S
      *******************************************
      ***  20031014  @@@@-MOTOHASI            ***
      ***  入院患者を取得(入外区分:1)ここから ***
      *******************************************
           PERFORM 910-DBSELECT-SEC
      *
      *    収納明細テーブルより変数配列に対象データを抽出する。
      *
           PERFORM   UNTIL   FLG-SYUMEI  =  1
      * 20030707 追加 
             PERFORM   900-SYUNOU-READ-SEC
             INITIALIZE            PTINF-REC 
      *=== 4.0対応 By Takashima(2007/09/10) start
      *      MOVE SMEI-HOSPID TO PTINF-HOSPID
             MOVE   SMEI-HOSPNUM     TO   PTINF-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
             MOVE   SMEI-PTID        TO   PTINF-PTID
             PERFORM   900-PTINF-READ-SEC
      *--- 2009/02/20 ADD START ----------------------------------------
             PERFORM   900-PTNUM-READ-SEC
      *--- 2009/02/20 ADD END ------------------------------------------
      * 20030707 追加
      *    入力された日付と入返金日が一致しているかをチェック
      ***  20030707 収納明細情報抽出条件変更
      ***  収納情報にレコードが存在しないデータは抽出しない。
      ***  伝票状態区分 = 3(請求取消)は対象外データとして取り扱う
      ***  患者情報にレコードが存在しないデータは抽出しない。
      ***  テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う
             IF  ( WRK-PARA-DENPPRTYMD   =   SMEI-NYUHEN-YMD
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         AND WRK-PARA-HOSPID      =      SMEI-HOSPID )
                 AND  WRK-PARA-HOSPNUM  =  SMEI-HOSPNUM )
      *=== 4.0対応 By Takashima(2007/09/10) end
                 AND  FLG-SYUNOU  =  ZERO
                 AND  SYU-DENPJTIKBN   NOT =  "3"
                 AND  FLG-PTINF  =  ZERO
                 AND  PTINF-TSTPTNUMKBN   NOT =  "1"
      ***  20030707 収納明細情報抽出条件変更
      *
      * 20040818 追加 MOTO  診療訂正で請求金額、入返金が０のデータは対象外
               IF   ( SMEI-JOUTAIKBN  NOT =  "8"
                   AND  SMEI-JOUTAIKBN  NOT =  "3" )
                   OR  SMEI-SKYMONEY  NOT =  0
                   OR  SMEI-NYUHEN-MONEY  NOT =  0
      * 20040818 追加 MOTO
                 ADD     1                TO   IDX-S
      *=== 4.0対応 By Takashima(2007/09/10) start
      *              MOVE   SMEI-HOSPID 
      *                   TO    WRK-SMEI-HOSPID(IDX-S)
                 MOVE   SMEI-HOSPNUM      TO   WRK-SMEI-HOSPNUM(IDX-S)
      *=== 4.0対応 By Takashima(2007/09/10) end
                 MOVE   SMEI-NYUGAIKBN    TO   WRK-SMEI-NYUGAIKBN(IDX-S)
      * 20030407 追加  診療科コード
                 MOVE   SMEI-SRYKA        TO   WRK-SMEI-SRYKA(IDX-S)
      * 20030407 追加  診療科コード
                 MOVE   SMEI-PTID         TO   WRK-SMEI-PTID(IDX-S)
                 MOVE   SMEI-DENPNUM      TO   WRK-SMEI-DENPNUM(IDX-S)
                 MOVE   SMEI-DENPEDANUM 
                                      TO   WRK-SMEI-DENPEDANUM(IDX-S)
                 MOVE   SMEI-SKYMONEY     TO   WRK-SMEI-SKYMONEY(IDX-S)
                 MOVE   SMEI-NYUHEN-MONEY 
                                      TO   WRK-SMEI-NYUHEN-MONEY(IDX-S)
                 MOVE   SMEI-NYUHEN-YMD
                                      TO   WRK-SMEI-NYUHEN-YMD(IDX-S)
      *--- 2009/02/20 ADD START ----------------------------------------
                 MOVE   PTNUM-PTNUM       TO   WRK-SMEI-PTNUM(IDX-S)
      *--- 2009/02/20 ADD END ------------------------------------------
               END-IF
             END-IF
             PERFORM 910-DBFETCH-SEC
           END-PERFORM
      *******************************************
      ***  20031014  @@@@-MOTOHASI            ***
      ***  入院患者を取得(入外区分:1)ここまで ***
      *******************************************

      *=== sarge対応 ADD By Takashima(2006/08/01) start
           PERFORM 910-DBCLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/08/01) end

      *******************************************
      ***  20031014  @@@@-MOTOHASI            ***
      ***  外来患者を取得(入外区分:2)ここから ***
      *******************************************
           PERFORM 920-DBSELECT-SEC
      *
      *    収納明細テーブルより変数配列に対象データを抽出する。
      *
           PERFORM  UNTIL   FLG-SYUMEI   =   1
      * 20030707 追加 
             PERFORM   900-SYUNOU-READ-SEC
             INITIALIZE   PTINF-REC 
      *=== 4.0対応 By Takashima(2007/09/10) start
      *      MOVE SMEI-HOSPID TO PTINF-HOSPID
             MOVE   SMEI-HOSPNUM   TO   PTINF-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
             MOVE   SMEI-PTID      TO   PTINF-PTID
             PERFORM   900-PTINF-READ-SEC
      *--- 2009/02/20 ADD START ----------------------------------------
             PERFORM   900-PTNUM-READ-SEC
      *--- 2009/02/20 ADD END ------------------------------------------
      * 20030707 追加
      *    入力された日付と入返金日が一致しているかをチェック
      ***  20030707 収納明細情報抽出条件変更
      ***  収納情報にレコードが存在しないデータは抽出しない。
      ***  伝票状態区分 = 3(請求取消)は対象外データとして取り扱う
      ***  患者情報にレコードが存在しないデータは抽出しない。
      ***  テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う
             IF  ( WRK-PARA-DENPPRTYMD   =   SMEI-NYUHEN-YMD
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         AND WRK-PARA-HOSPID      =      SMEI-HOSPID )
                 AND WRK-PARA-HOSPNUM   =   SMEI-HOSPNUM )
      *=== 4.0対応 By Takashima(2007/09/10) end
                 AND  FLG-SYUNOU   =   ZERO
                 AND  SYU-DENPJTIKBN   NOT =  "3"
                 AND  FLG-PTINF   =   ZERO
                 AND  PTINF-TSTPTNUMKBN  NOT =  "1"
      ***  20030707 収納明細情報抽出条件変更
      *
      * 20040818 追加 MOTO  診療訂正で請求金額、入返金が０のデータは対象外
               IF   ( SMEI-JOUTAIKBN  NOT =  "8"
                   AND   SMEI-JOUTAIKBN  NOT =  "3" ) 
                   OR    SMEI-SKYMONEY  NOT =  0
                   OR    SMEI-NYUHEN-MONEY  NOT =  0
      * 20040818 追加 MOTO
                 ADD     1               TO   IDX-S
      *=== 4.0対応 By Takashima(2007/09/10) start
      *              MOVE   SMEI-HOSPID 
      *                   TO    WRK-SMEI-HOSPID(IDX-S)
                 MOVE   SMEI-HOSPNUM     TO   WRK-SMEI-HOSPNUM(IDX-S)
      *=== 4.0対応 By Takashima(2007/09/10) end
                 MOVE   SMEI-NYUGAIKBN   TO   WRK-SMEI-NYUGAIKBN(IDX-S)
      * 20030407 追加  診療科コード
                 MOVE   SMEI-SRYKA       TO   WRK-SMEI-SRYKA(IDX-S)
      * 20030407 追加  診療科コード
                 MOVE   SMEI-PTID        TO   WRK-SMEI-PTID(IDX-S)
                 MOVE   SMEI-DENPNUM     TO   WRK-SMEI-DENPNUM(IDX-S)
                 MOVE   SMEI-DENPEDANUM  TO   WRK-SMEI-DENPEDANUM(IDX-S)
                 MOVE   SMEI-SKYMONEY    TO   WRK-SMEI-SKYMONEY(IDX-S)
                 MOVE   SMEI-NYUHEN-MONEY
                                   TO    WRK-SMEI-NYUHEN-MONEY(IDX-S)
                 MOVE   SMEI-NYUHEN-YMD  TO   WRK-SMEI-NYUHEN-YMD(IDX-S)
      *--- 2009/02/20 ADD START ----------------------------------------
                 MOVE   PTNUM-PTNUM      TO   WRK-SMEI-PTNUM(IDX-S)
      *--- 2009/02/20 ADD END ------------------------------------------
               END-IF
             END-IF
             PERFORM 920-DBFETCH-SEC
           END-PERFORM
      *******************************************
      ***  20031014  @@@@-MOTOHASI            ***
      ***  外来患者を取得(入外区分:2)ここまで ***
      *******************************************
      *
      *  配列の最終レコード番号を設定
          MOVE IDX-S TO IDX-MAX
      *
           IF   IDX-MAX     >    1
      *    対象データが１件以上存在する場合は抽出フラグON
             MOVE   1         TO   FLG-GET
      *    対象データが１件以上存在する場合は終了フラグOFF
             MOVE   ZERO      TO   FLG-END
      *    配列に１件以上のデータが抽出されている場合はソーティング処理を行う。
      *    変数配列によるソーティング
      *    最小値より並び替えをする為にインデックスの設定
             MOVE    1        TO   IDX-S
             MOVE    2        TO   IDX-N
             PERFORM  UNTIL IDX-S  NOT <  IDX-MAX
               PERFORM  UNTIL IDX-N  >  IDX-MAX
      *--- 2009/02/24 ADD START ----------------------------------------
                 IF  WRK-PARA-SORTKBN   =   "1"
                   IF  WRK-SMEI-SORT-KEY2(IDX-S)  >
                       WRK-SMEI-SORT-KEY2(IDX-N) 
                     MOVE WRK-SMEI-TBL(IDX-N)  TO   WRK-SV-SMEI-TBL
                     MOVE WRK-SMEI-TBL(IDX-S)  TO   WRK-SMEI-TBL(IDX-N)
                     MOVE WRK-SV-SMEI-TBL      TO   WRK-SMEI-TBL(IDX-S)
                   END-IF
                 ELSE
      *--- 2009/02/24 ADD END ------------------------------------------
                   IF  WRK-SMEI-SORT-KEY(IDX-S)  >
                       WRK-SMEI-SORT-KEY(IDX-N) 
                     MOVE WRK-SMEI-TBL(IDX-N)  TO   WRK-SV-SMEI-TBL
                     MOVE WRK-SMEI-TBL(IDX-S)  TO   WRK-SMEI-TBL(IDX-N)
                     MOVE WRK-SV-SMEI-TBL      TO   WRK-SMEI-TBL(IDX-S)
                   END-IF
      *--- 2009/02/24 ADD START ----------------------------------------
                 END-IF
      *--- 2009/02/24 ADD END ------------------------------------------
                 ADD   1      TO   IDX-N
               END-PERFORM
      *    N番目の値を決定する為にインデックスの再設定
               ADD    1       TO   IDX-S
               COMPUTE  IDX-N  =  IDX-S  +  1
             END-PERFORM
           ELSE
             IF   IDX-MAX  =  ZERO
      *    対象データが１件も存在しない場合は抽出フラグOFF
               MOVE   ZERO    TO   FLG-GET
      *    対象データが１件も存在しない場合は終了フラグON
               MOVE   1       TO   FLG-END
             ELSE 
      *    対象データが１件でも存在する場合は抽出フラグオフON
               MOVE   1       TO   FLG-GET
      *    対象データが１件でも存在する場合は終了フラグOFF
               MOVE   ZERO    TO   FLG-END
             END-IF
           END-IF
      *
           PERFORM 910-DBCLOSE-SEC
      *  配列用員インデックスの再設定
           MOVE     1         TO   IDX-S
      *
           .
       310-DATA-SELECT-EXT.
           EXIT.
      *
      *--- 2009/02/24 ADD START ----------------------------------------
      *****************************************************************
      *    合計帳票編集<ヘッダー部>処理
      *****************************************************************
       320-HEAD-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
      *
      *--- 2009/03/23 DELETE START -------------------------------------
      *     ADD     1                   TO  CNT-PAGE
      *--- 2009/03/23 DELETE END   -------------------------------------
      *
           INITIALIZE                  D5002
           MOVE    WRK-PARA-DENPPRTYMDWH   TO   D5002-SYSYMD
      *
      *--- 2009/03/23 DELETE START -------------------------------------
      *     MOVE    CNT-PAGE                TO   WRK-PAGE
      *     MOVE    WRK-PAGE                TO   D5002-PAGE
      *--- 2009/03/23 DELETE END   -------------------------------------
      *
      *    入外区分名称設定
           EVALUATE WRK-NYUGAIKBN
             WHEN  "1"
               MOVE   "入院"         TO   D5002-NYUGAIKBN
      *    入院自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
                        UNTIL  IDX2   >  10 
                 MOVE   WRK-TJIHIN(IDX2)   TO   D5002-TJIHI(IDX2)
               END-PERFORM
             WHEN  "2"
               MOVE   "外来"         TO   D5002-NYUGAIKBN
      *    自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
                        UNTIL    IDX2   >  10 
                 MOVE   WRK-TJIHI(IDX2)   TO   D5002-TJIHI(IDX2)
               END-PERFORM
           END-EVALUATE
      *
      *    診療科名称取得
           INITIALIZE                        SYS-1005-REC
           MOVE    "1005"               TO   SYS-1005-KANRICD
           MOVE    WRK-SRYKA            TO   SYS-1005-KBNCD
           MOVE    SPA-HOSPNUM          TO   SYS-1005-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1005-EDYUKYMD
                                             SYS-1005-STYUKYMD
           MOVE    SYS-1005-REC         TO   MCPDATA-REC
           PERFORM 820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1005-REC
             MOVE   SYS-1005-SRYKANAME  TO   D5002-SRYKA
           ELSE
             MOVE   SPACE               TO   D5002-SRYKA
           END-IF
      *
           .
       320-HEAD-HEN-EXT.
           EXIT.
      *
      *--- 2009/02/24 ADD END ------------------------------------------
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
      *
      *--- 2009/03/23 DELETE START -------------------------------------
      *     ADD     1                   TO  CNT-PAGE
      *--- 2009/03/23 DELETE END   -------------------------------------
      *
           INITIALIZE                  HC17
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *     INITIALIZE                      HC171     
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *     (08/11/04) LINE DEL END   MONMA ----------------------------
      *
      ***     MOVE    "伝票発行日"        TO  HC17-MIDASI
      ***     MOVE    WRK-PARA-DENPPRTYMDWH
      ***                                 TO  HC17-SRYYMD
      ***     MOVE    WRK-SYSYMDWH        TO  HC17-SYSYMD
           MOVE    WRK-PARA-DENPPRTYMDWH   TO   HC17-SYSYMD
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *     MOVE    WRK-PARA-DENPPRTYMDWH
      *                                 TO  HC171-SYSYMD
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
      *
      *--- 2009/03/23 DELETE START -------------------------------------
      *     MOVE    CNT-PAGE                TO   WRK-PAGE
      *     MOVE    WRK-PAGE                TO   HC17-PAGE
      *--- 2009/03/23 DELETE END   -------------------------------------
      *
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *     ADD     1                   TO  CNT-PAGE
      *     MOVE    CNT-PAGE            TO  WRK-PAGE
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *    MOVE    WRK-PAGE            TO  HC171-PAGE
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
      *
      * 20030407 追加  入外区分名称設定
           EVALUATE WRK-NYUGAIKBN
             WHEN  "1"
               MOVE   "入院"         TO   HC17-NYUGAIKBN
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *----
      *             MOVE "入院"         TO  HC171-NYUGAIKBN
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      ***          入院自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *                      UNTIL    IDX2   >    5 
      *                      MOVE     WRK-TJIHIN(IDX2)
      *                          TO   HC17-SYSYMD(IDX2)
                        UNTIL  IDX2   >  10 
                 MOVE   WRK-TJIHIN(IDX2)   TO   HC17-TJIHI(IDX2)
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
               END-PERFORM
             WHEN  "2"
               MOVE   "外来"         TO   HC17-NYUGAIKBN
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *             MOVE "外来"         TO  HC171-NYUGAIKBN
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
      ***          外来自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *                      UNTIL    IDX2   >    5 
                        UNTIL    IDX2   >  10 
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
                 MOVE   WRK-TJIHI(IDX2)   TO   HC17-TJIHI(IDX2)
               END-PERFORM
           END-EVALUATE
      * 20030407 追加  入外区分名称設定
      *
      * 20030407 追加  診療科名称設定
      *    診療科名称取得
           INITIALIZE                        SYS-1005-REC
           MOVE    "1005"               TO   SYS-1005-KANRICD
           MOVE    WRK-SRYKA            TO   SYS-1005-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1005-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD  TO   SYS-1005-EDYUKYMD
                                             SYS-1005-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1005-REC         TO   MCPDATA-REC
           PERFORM 820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC          TO   SYS-1005-REC
             MOVE   SYS-1005-SRYKANAME   TO   HC17-SRYKA
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *----
      *         MOVE  SYS-1005-SRYKANAME TO  HC171-SRYKA
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
           ELSE
             MOVE   SPACE               TO   HC17-SRYKA
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *----
      *         MOVE  SPACE              TO  HC171-SRYKA
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
           END-IF
      * 20030407 追加  診療科名称設定
      *
      *** 20030407削除
      ***    PERFORM  VARYING  IDX2   FROM 1  BY  1
      ***                       UNTIL       IDX2   >         5 
      ***         MOVE    WRK-TJIHI(IDX2)   TO   HC17-TJIHI(IDX2)
      ***    END-PERFORM
      *** 20030407削除
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           MOVE    ZERO                        TO   CNT-LINE
      *
      *** 20040907  MOTOHASI
           INITIALIZE                          SUM-G-AREA
      *** 20040907  MOTOHASI
      *
      * 20030407 追加
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO   WRK-NYUGAIKBN
           MOVE    WRK-SMEI-SRYKA(IDX-S)       TO   WRK-SRYKA
      *    帳票編集<見出し>処理
      *--- 2009/02/24 CHANGE START -------------------------------------
      *    PERFORM 310-HEAD-HEN-SEC
      *--- 2009/03/23 CHANGE START -------------------------------------
      *     IF  WRK-PARA-GOKEIKBN  NOT =  "2"
      *       PERFORM  310-HEAD-HEN-SEC
      *     END-IF
      *
           EVALUATE  WRK-PARA-GOKEIKBN
             WHEN  "1"
             PERFORM  310-HEAD-HEN-SEC
             PERFORM  320-HEAD-HEN-SEC
             WHEN  "2"
             PERFORM  320-HEAD-HEN-SEC
             WHEN  OTHER
             PERFORM  310-HEAD-HEN-SEC
           END-EVALUATE
      *
      *--- 2009/03/23 CHANGE END   -------------------------------------
      *--- 2009/02/24 CHANGE END ---------------------------------------
      * 20030407 追加
      *
           PERFORM   UNTIL   FLG-END   =   1
      *---- (08/11/04) LINE ADD START MONMA ----------------------------
      *               OR      CNT-LINE    =   30
                       OR  CNT-LINE   =   21
      *---- (08/11/04) LINE ADD END   MONMA ----------------------------
      ***    収納テーブル再読み込み
             PERFORM 901-SYUNOU-READ-SEC
             IF   FLG-SYUNOU   =   ZERO
      *
      * 20030407 追加
      * 入外区分ブレイクチェック処理
      *--- 2009/03/23 ADD START ----------------------------------------
               IF   WRK-SMEI-NYUGAIKBN(IDX-S)  NOT =  WRK-NYUGAIKBN
                 MOVE   WRK-SMEI-NYUGAIKBN(IDX-S)  TO  WRK-NYUGAIKBN
                 MOVE   WRK-SMEI-SRYKA(IDX-S)      TO  WRK-SRYKA
      *--- 2009/03/23 ADD START ----------------------------------------
      **
      *--- 2009/02/24 CHANGE START -------------------------------------
      *** 20040907  MOTOHASI  小計印字
      *           PERFORM   3001-SYOUKEI-SET-SEC
      *** 20040907  MOTOHASI
      *
      *          帳票編集<見出し>処理
      *           PERFORM   390-PRINT-OUT-SEC
      *
      *** 20040907  MOTOHASI
      *           INITIALIZE                    SUM-G-AREA
      *           MOVE   ZERO                       TO   CNT-LINE
      *           MOVE   WRK-SMEI-NYUGAIKBN(IDX-S)  TO   WRK-NYUGAIKBN
      *           MOVE   WRK-SMEI-SRYKA(IDX-S)      TO   WRK-SRYKA
      **          帳票編集<見出し>処理
      *           PERFORM  310-HEAD-HEN-SEC
      *** 20040907  MOTOHASI
                 EVALUATE   WRK-PARA-GOKEIKBN
                   WHEN  "1"
      *    小計印字
                     PERFORM   3001-SYOUKEI-SET-SEC
                     PERFORM   390-PRINT-OUT-SEC
                     INITIALIZE                    SUM-G-AREA
      *    帳票編集<見出し>処理
                     PERFORM   310-HEAD-HEN-SEC
      *    合計印字
      *--- 2009/03/23 DELETE START -------------------------------------
      *               PERFORM   320-HEAD-HEN-SEC
      *--- 2009/03/23 DELETE END   -------------------------------------
                     PERFORM   3000-GOUKEI-SET-SEC
                     PERFORM   400-PRINT-OUT-SEC
                     INITIALIZE       SUM-GG-AREA
      *--- 2009/03/23 ADD START ----------------------------------------
                     PERFORM   320-HEAD-HEN-SEC
      *--- 2009/03/23 ADD END   ----------------------------------------
                   WHEN  "2"
      *    合計印字
      *--- 2009/03/23 DELETE START -------------------------------------
      *               PERFORM   320-HEAD-HEN-SEC
      *--- 2009/03/23 DELETE START -------------------------------------
                     PERFORM   3000-GOUKEI-SET-SEC
                     PERFORM   400-PRINT-OUT-SEC
                     INITIALIZE      SUM-GG-AREA
      *--- 2009/03/23 ADD START ----------------------------------------
                     PERFORM   320-HEAD-HEN-SEC
      *--- 2009/03/23 ADD END   ----------------------------------------
                   WHEN  OTHER
      *    小計印字
                     PERFORM   3001-SYOUKEI-SET-SEC
                     PERFORM   390-PRINT-OUT-SEC
                     INITIALIZE                    SUM-G-AREA
      *    帳票編集<見出し>処理
                     PERFORM  310-HEAD-HEN-SEC
                 END-EVALUATE
                 MOVE   ZERO                       TO  CNT-LINE
                 MOVE   WRK-SMEI-NYUGAIKBN(IDX-S)  TO  WRK-NYUGAIKBN
                 MOVE   WRK-SMEI-SRYKA(IDX-S)      TO  WRK-SRYKA
      *--- 2009/02/24 CHANGE END ---------------------------------------
               END-IF      
      *
      * 診療科ブレイクチェック処理
               IF WRK-SMEI-SRYKA(IDX-S)  NOT =  WRK-SRYKA
      *
      *--- 2009/03/23 ADD START ----------------------------------------
                 MOVE   WRK-SMEI-NYUGAIKBN(IDX-S)  TO  WRK-NYUGAIKBN
                 MOVE   WRK-SMEI-SRYKA(IDX-S)      TO  WRK-SRYKA
      *
                 EVALUATE   WRK-PARA-GOKEIKBN
                   WHEN  "1"
      *    小計印字
                     PERFORM   3001-SYOUKEI-SET-SEC
                     PERFORM   390-PRINT-OUT-SEC
                     INITIALIZE                    SUM-G-AREA
      *    帳票編集<見出し>処理
                     PERFORM   310-HEAD-HEN-SEC
      *    合計印字
                     PERFORM   3000-GOUKEI-SET-SEC
                     PERFORM   400-PRINT-OUT-SEC
                     INITIALIZE       SUM-GG-AREA
      *
                     PERFORM   320-HEAD-HEN-SEC
      *
                   WHEN  "2"
      *    合計印字
                     PERFORM   3000-GOUKEI-SET-SEC
                     PERFORM   400-PRINT-OUT-SEC
                     INITIALIZE      SUM-GG-AREA
                     PERFORM   320-HEAD-HEN-SEC
                   WHEN  OTHER
      *    小計印字
                     PERFORM   3001-SYOUKEI-SET-SEC
                     PERFORM   390-PRINT-OUT-SEC
                     INITIALIZE                    SUM-G-AREA
                     PERFORM  310-HEAD-HEN-SEC
                 END-EVALUATE
      *--- 2009/03/23 ADD END   ----------------------------------------
      *** 20040907  MOTOHASI  小計印字
      *--- 2009/03/23 DELETE START -------------------------------------
      *           PERFORM 3001-SYOUKEI-SET-SEC
      *--- 2009/03/23 DELETE END   -------------------------------------
      *** 20040907  MOTOHASI
      *
      *          帳票編集<見出し>処理
      *--- 2009/03/23 DELETE START -------------------------------------
      *           PERFORM 390-PRINT-OUT-SEC
      *--- 2009/03/23 DELETE END   -------------------------------------
      *
      *** 20040907  MOTOHASI
                 INITIALIZE                    SUM-G-AREA
      *** 20040907  MOTOHASI
      *
                 MOVE    ZERO                      TO   CNT-LINE
                 MOVE    WRK-SMEI-NYUGAIKBN(IDX-S) TO   WRK-NYUGAIKBN
                 MOVE    WRK-SMEI-SRYKA(IDX-S)     TO   WRK-SRYKA
      *          帳票編集<見出し>処理
      *--- 2009/03/23 DELETE START -------------------------------------
      *           PERFORM 310-HEAD-HEN-SEC
      *--- 2009/03/23 DELETE END   -------------------------------------
               END-IF
      * 20030407 追加
      *
      *    患者情報読み込み
               INITIALIZE           PTINF-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    WRK-SMEI-HOSPID(IDX-S)   TO  PTINF-HOSPID
               MOVE   WRK-SMEI-HOSPNUM(IDX-S)  TO   PTINF-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
               MOVE   WRK-SMEI-PTID(IDX-S)     TO   PTINF-PTID
               PERFORM   900-PTINF-READ-SEC
      *
               ADD     1             TO  CNT-LINE
      *    連番
      ***         ADD     1                   TO  WRK-RENNUM
      ***         MOVE    WRK-RENNUM          TO  WRK-RENNUM-X
      ***         MOVE    WRK-RENNUM-X        TO  HC17-RENNUM (CNT-LINE)
      *    伝票番号
               MOVE   WRK-SMEI-DENPNUM(IDX-S)
                             TO  HC17-RENNUM (CNT-LINE)
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *         MOVE    WRK-SMEI-DENPNUM(IDX-S)
      *                       TO  HC171-RENNUM (CNT-LINE)
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
      *    患者番号
               INITIALIZE   ORCSPTIDAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE  WRK-PARA-HOSPID  TO  SPTID-HOSPID
               MOVE   WRK-PARA-HOSPNUM        TO   SPTID-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
               MOVE   WRK-SMEI-PTID(IDX-S)    TO   SPTID-PTID
               CALL   "ORCSPTID"              USING
                                              ORCSPTIDAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                                SPA-1009-TBL
                                              SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF    SPTID-RC  =  ZERO
      *** 20030407 変更 患者番号を８桁に拡張
      ***                MOVE  SPTID-PTNUM TO  WRK-PTNUM6
      ***                MOVE  WRK-PTNUM6  TO  HC17-PTNUM (CNT-LINE)
                 MOVE    SPTID-PTNUM          TO   WRK-PTNUM8
                 MOVE    WRK-PTNUM8           TO   HC17-PTNUM (CNT-LINE)
      *** 20030407 変更 患者番号を８桁に拡張
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *                MOVE  WRK-PTNUM8  TO  HC171-PTNUM (CNT-LINE)
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
               ELSE
                 MOVE    SPACE         TO   HC17-PTNUM (CNT-LINE)
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *                MOVE  SPACE       TO  HC171-PTNUM (CNT-LINE)
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
               END-IF
      *    氏名
               MOVE    PTINF-NAME      TO   HC17-NAME  (CNT-LINE)
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *         MOVE    PTINF-NAME          TO  HC171-NAME  (CNT-LINE)
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
      *
      ***   伝票番号枝番が１の場合は保険点数、保険別請求額の明細を印字(開始)
               IF     WRK-SMEI-DENPEDANUM(IDX-S)   =   1
      ***          PERFORM 900-SYUNOU-READ-SEC
                 IF  FLG-SYUNOU  = ZERO
                   IF WRK-PARA-DENPPRTYMD < "20080401"
                     PERFORM 320-BODY-HEN-SUB-SEC
                   ELSE
                     PERFORM 330-BODY-HEN-SUB-SEC
                   END-IF
                 END-IF
               END-IF
      *
      *    請求金額
               INITIALIZE  WRK-SZ
               MOVE   WRK-SMEI-SKYMONEY(IDX-S)   TO   WRK-SZ
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
               MOVE   WRK-SZ     TO   HC17-SEIKYU-G(CNT-LINE)
      *       MOVE WRK-SZ TO HC171-SEIKYU  (CNT-LINE)
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
               IF   WRK-SMEI-SKYMONEY(IDX-S)  NOT =  0
                 ADD   WRK-SMEI-SKYMONEY(IDX-S)  TO   SUM-SEIKYU
                 ADD   1          TO   SUM-SEIKYUKEN
      *
      *** 20040907  MOTOHASI  小計印字
                 ADD WRK-SMEI-SKYMONEY(IDX-S)    TO   SUM-G-SEIKYU
      *--- 2009/02/24 ADD START ----------------------------------------
                 ADD   WRK-SMEI-SKYMONEY(IDX-S)  TO   SUM-GG-SEIKYU
      *--- 2009/02/24 ADD END ------------------------------------------
      *** 20040907  MOTOHASI  小計印字
      *
               END-IF
      *
      *    領収金額
               INITIALIZE  WRK-SZ
               MOVE   WRK-SMEI-NYUHEN-MONEY(IDX-S) TO   WRK-SZ
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
               MOVE   WRK-SZ         TO   HC17-RYOSYU-G(CNT-LINE)
      *       MOVE WRK-SZ TO HC171-RYOSYU  (CNT-LINE)
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
               IF   WRK-SMEI-NYUHEN-MONEY(IDX-S)  NOT =  0
                 ADD    WRK-SMEI-NYUHEN-MONEY(IDX-S)  TO   SUM-RYOSYU
                 ADD    1          TO    SUM-RYOSYUKEN
      *
      *** 20040907  MOTOHASI  小計印字
                 ADD WRK-SMEI-NYUHEN-MONEY(IDX-S)   TO   SUM-G-RYOSYU
      *--- 2009/02/24 ADD START ----------------------------------------
                 ADD WRK-SMEI-NYUHEN-MONEY(IDX-S)   TO   SUM-GG-RYOSYU
      *--- 2009/02/24 ADD END ------------------------------------------
      *** 20040907  MOTOHASI  小計印字
      *
               END-IF
             END-IF
      ***     収納テーブル収納テーブル再読み込み
      *
      *    配列インデックスのカウントアップ
             ADD  1  TO  IDX-S
      *    配列のデータを全件出力した場合は合計表の印刷を行う為にループより脱出
             IF IDX-S   >   IDX-MAX
               MOVE    1      TO   FLG-END
             END-IF 
           END-PERFORM     
      *
      *** 20040907  MOTOHASI  小計印字
           PERFORM 3001-SYOUKEI-SET-SEC
      *** 20040907  MOTOHASI
      *--- 2009/03/23 ADD START ----------------------------------------
           PERFORM 3000-GOUKEI-SET-SEC
      *--- 2009/03/23 ADD END   ----------------------------------------
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>サブ処理(H20年4月以前)
      *****************************************************************
       320-BODY-HEN-SUB-SEC      SECTION.
      *
      *    保険番号情報読み込み
           PERFORM 900-HKNNUM-READ-SEC
           IF   FLG-HKNNUM  =  1
      *    該当レコードが存在しない場合は単価を10円に設定する。
             MOVE    10        TO   HKN-TENTNK
           END-IF
      *
      *    保険点数
           MOVE     ZERO       TO  SYU-TOTAL-HKNTEN
           EVALUATE        SYU-SYUHKNNUM
      ***   20040202  労災／自賠責の場合に保険点数を印字する。
      ***             WHEN    WRK-CONS-JIBAI-HKNNUM
      ***             WHEN    WRK-CONS-ROUSAI-HKNNUM
      ***                 CONTINUE
      ***   20040202  労災／自賠責の場合も保険点数を印字する。
             WHEN    OTHER
               PERFORM VARYING IDZ FROM    1   BY  1
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *                        UNTIL   IDZ >       11
      *--- 2008/12/16 ADD START ----------------------------------------
      *                       UNTIL   IDZ >       15
                       UNTIL   IDZ   >  16
      *--- 2008/12/16 ADD END ------------------------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
                 ADD    SYU-HKNTEN (IDZ)   TO   SYU-TOTAL-HKNTEN
               END-PERFORM
               MOVE    SYU-TOTAL-HKNTEN    TO   WRK-Z9
               MOVE    WRK-Z9              TO   HC17-HKNTEN (CNT-LINE)
               ADD     SYU-TOTAL-HKNTEN    TO   SUM-HKNTEN
               ADD     1                   TO   SUM-HKNKEN
      *
      *** 20040907  MOTOHASI  小計印字
               ADD   SYU-TOTAL-HKNTEN      TO   SUM-G-HKNTEN
      *** 20040907  MOTOHASI  小計印字
      *--- 2009/02/24 ADD START ----------------------------------------
               ADD   SYU-TOTAL-HKNTEN      TO   SUM-GG-HKNTEN
      *--- 2009/02/24 ADD END ------------------------------------------
      *
           END-EVALUATE
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                  TO   IDX 
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6                 TO   IDX
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *             WHEN    WRK-CONS-JIBAI-HKNNUM
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
      * 20080324 H20年4月改正対応START tanaka
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    20                TO   IDX
      * 20080324 H20年4月改正対応END
      *
      * 自費エリア拡張に伴い修正 20021204
      ***                MOVE    9                   TO  IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *                MOVE    14                   TO  IDX
      *---- (08/11/07) LINE ADD START MONMA ----------------------------
             WHEN    WRK-CONS-JIBAI-HKNNUM
      *---- (08/11/07) LINE ADD END   MONMA ----------------------------
               MOVE    19                TO   IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
             WHEN    WRK-CONS-ROUSAI-HKNNUM
      * 自費エリア拡張に伴い修正 20021204
      ***                 MOVE    8                   TO  IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *                MOVE    13                   TO  IDX
               MOVE    18                TO   IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
             WHEN    WRK-CONS-JIHI-HKNNUM
      * 自費エリア拡張に伴い修正 20021204
      ***                 MOVE    7                   TO  IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *               MOVE    12                   TO  IDX
               MOVE    17                TO   IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
             WHEN    "060"
             WHEN    "067"
      *=== 特別療養費対応 ADD By Uchida(2006/12/22) start
             WHEN    "068"
             WHEN    "069"
      *=== 特別療養費対応 ADD By Uchida(2006/12/22) end
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    5               TO   IDX
               ELSE
                 MOVE    4               TO   IDX
               END-IF
             WHEN    OTHER
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    3               TO   IDX
               ELSE
                 PERFORM 3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1           TO   IDX
                   WHEN    "2"
                     MOVE    2           TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
      * 20031111追加
      * 保険別区分例外条件(自費条件拡張)
           IF   SYU-SYUHKNNUM(1:2)   =   "98"
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *              MOVE    12    TO      IDX
             MOVE     17        TO   IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
               END-IF
      * 20031111追加
      *
           IF   IDX   >   ZERO
      *
      *            自費以外のとき自費分だけ集計
      *    自費エリア拡張に伴い修正 20021204
      ***             IF      IDX             =   7
      * 20040421追加  自費金額算出用変数の初期化(バグ対応)
             INITIALIZE     WRK-JIHI
             INITIALIZE     WRK-JIHI3
      * 20040421追加  自費金額算出用変数の初期化(バグ対応)
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *             IF      IDX             =   12
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) start
      *       IF   IDX   =   17
      **=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *         MOVE    ZERO            TO   WRK-JIHI
      ** 20040421追加  自費金額算出用変数の初期化(バグ対応)
      *         MOVE    ZERO            TO   WRK-JIHI3  
      ** 20040421追加  自費金額算出用変数の初期化(バグ対応)
      *       ELSE
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) end
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) start
               IF  SYU-SRYYMD  <=  "20070331"
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) end
                 COMPUTE   WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
                                       +   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
                                       +   SYU-JIHI-TAX
      *
                 COMPUTE   WRK-JIHI3   =   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
                                       +   SYU-JIHI-TAX
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) start
               ELSE
                 COMPUTE   WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
                                       +   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
      *
                 COMPUTE   WRK-JIHI3   =   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
               END-IF
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) end
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) start
      *       END-IF
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) end
      *  20030416  変更
      ***             COMPUTE WRK-MONEY       =   SYU-SKYMONEY
      ***                                     -   WRK-JIHI
             COMPUTE   WRK-MONEY     =   SYU-SKYMONEY
                                     -   WRK-JIHI3
      *  20030416  変更
      *
      *--- 2008/12/16 ADD START ----------------------------------------
      *             MOVE    WRK-MONEY     TO  WRK-Z9
             MOVE   WRK-MONEY     TO   WRK-S9-2
      *--- 2008/12/16 ADD END ------------------------------------------
      *--- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *--- 2008/12/16 ADD START ----------------------------------------
      *             MOVE    WRK-Z9        TO  HC17-MONEY (IDX CNT-LINE)
             MOVE   WRK-S9-2      TO   HC17-MONEY(IDX CNT-LINE)
      *--- 2008/12/16 ADD END ------------------------------------------
      *             IF  IDX  >  16
      *                 COMPUTE IDX3      =   IDX  -  16
      *                 MOVE    WRK-Z9    TO  HC171-MONEY (IDX3 CNT-LINE)
      *             ELSE
      *                 MOVE    WRK-Z9    TO  HC17-MONEY (IDX CNT-LINE)
      *             END-IF
      *--- 08/11/04 LINE DEL END   MONMA -------------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
             ADD   WRK-MONEY     TO   SUM-MONEY(IDX)
             ADD   1             TO   SUM-KEN(IDX)
      *
      *** 20040907  MOTOHASI  小計印字
             ADD   WRK-MONEY     TO   SUM-G-MONEY (IDX)
      *** 20040907  MOTOHASI  小計印字
      *--- 2009/02/24 ADD START ----------------------------------------
             ADD   WRK-MONEY     TO   SUM-GG-MONEY(IDX)
      *--- 2009/02/24 ADD END ----------------------------------------
      *
      *    自費エリア拡張に伴い修正 20021204
      ***             IF      IDX             =   7
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *             IF      IDX    =   12
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) start
      *       IF   IDX   =   17
      **=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *         CONTINUE
      *       ELSE
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) end
               IF   WRK-JIHI  >  ZERO
      *    自費エリア拡張に伴い修正 20021204
      ***                    MOVE    WRK-JIHI
      ***                                    TO  WRK-ZZ
      ***                    MOVE    "*"     TO  WRK-ZZ (1:1)
      ***                    MOVE    WRK-ZZ  TO  HC17-MONEY (7   CNT-LINE)
                 PERFORM  VARYING IDX2  FROM 1  BY  1
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *                                  UNTIL    IDX2       >    5
                          UNTIL   IDX2  >  10
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
                   INITIALIZE WRK-JIHI2
                   COMPUTE WRK-JIHI2   =   SYU-JIHI-TAXNASI(IDX2)
                                       +   SYU-JIHI-TAXARI(IDX2)
                   IF   WRK-JIHI2  >  ZERO
                     MOVE   WRK-JIHI2      TO   WRK-ZZ
      ***                 MOVE "*"  TO  WRK-ZZ(1:1)
                     COMPUTE JIHIIDX = IDX2 + 6 
                     MOVE   WRK-ZZ    TO   HC17-MONEY(JIHIIDX CNT-LINE)
                     ADD   WRK-JIHI2  TO   SUM-MONEY(JIHIIDX)
                     ADD   1          TO   SUM-KEN(JIHIIDX)
      *
      *** 20040907  MOTOHASI  小計印字
                     ADD   WRK-JIHI2  TO   SUM-G-MONEY(JIHIIDX)
      *** 20040907  MOTOHASI  小計印字
      *--- 2009/02/24 ADD START ----------------------------------------
                     ADD   WRK-JIHI2  TO   SUM-GG-MONEY(JIHIIDX)
      *--- 2009/02/24 ADD END ------------------------------------------
      *
                   END-IF
                 END-PERFORM
      *    自費エリア拡張に伴い修正 20021204
               END-IF
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) start
      *       END-IF
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) end
           END-IF
      *
      ****    20030224追加
      *    医療収益計
      ***           COMPUTE  WRK-SYUEKI = 
      ***                    (SYU-TOTAL-HKNTEN * HKN-TENTNK)
      ***                      +  WRK-JIHI 
      ***                      +  SYU-RSISHOSHIN-MONEY 
      ***                      +  SYU-RSISAISHIN-MONEY 
      ***                      +  SYU-RSISDO-MONEY 
      ***                      +  SYU-RSIETC-MONEY 
      ***                      +  SYU-CHOSEI 
      *--- 2009/02/06 ADD START ----------------------------------------
      *--- 2009/02/17 CHANGE START -------------------------------------
      *--- 医療収益の計算式を修正 2009/04/08 CHANGE START --------------
           IF  SYU-KOH4HKNNUM  NOT =  SPACE
      *       MOVE   SYU-KOH4COMPFTN    TO   WRK-COMPFTN
             MOVE   SYU-KOH4HKNFTNMONEY    TO   WRK-COMPFTN
           ELSE
             IF  SYU-KOH3HKNNUM  NOT =  SPACE
      *         MOVE   SYU-KOH3COMPFTN    TO   WRK-COMPFTN
               MOVE   SYU-KOH3HKNFTNMONEY    TO   WRK-COMPFTN
             ELSE
               IF  SYU-KOH2HKNNUM  NOT =  SPACE
      *           MOVE   SYU-KOH2COMPFTN    TO   WRK-COMPFTN
                 MOVE   SYU-KOH2HKNFTNMONEY    TO   WRK-COMPFTN
               ELSE
                 IF  SYU-KOH1HKNNUM  NOT =  SPACE
      *             MOVE   SYU-KOH1COMPFTN    TO   WRK-COMPFTN
                   MOVE   SYU-KOH1HKNFTNMONEY    TO   WRK-COMPFTN
                 ELSE
                   IF  SYU-SYUHKNNUM  NOT =  SPACE
      *               MOVE   SYU-SYUCOMPFTN     TO   WRK-COMPFTN
                     MOVE   SYU-SYUHKNFTNMONEY    TO   WRK-COMPFTN
      *--- 医療収益の計算式を修正 2009/04/08 CHANGE END   --------------
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
      *--- 2009/02/17 CHANGE END ---------------------------------------
      *--- 2009/02/06 ADD END ------------------------------------------
           EVALUATE IDX 
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *              WHEN 12
             WHEN 17
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *    自費の場合
               MOVE   SYU-SKYMONEY         TO   WRK-SYUEKI
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *              WHEN 13
             WHEN 18
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *    労災の場合
               COMPUTE  WRK-SYUEKI ROUNDED   =
                        (SYU-TOTAL-HKNTEN  *  WRK-4001-TENTANKA)
                               +  WRK-JIHI
                               +  SYU-RSISHOSHIN-MONEY
                               +  SYU-RSISAISHIN-MONEY
                               +  SYU-RSISDO-MONEY
                               +  SYU-RSIETC-MONEY
                               +  SYU-CHOSEI
      * 20030411 追加
      * 20030814 削除
      * 老人一部負担金、公費一部負担金は保険点数に含まれている為削除
      *                        +  SYU-RJN-FTN
      *                        +  SYU-KOH-FTN
      * 20030814 削除
                               +  SYU-YKZ-FTN
                               +  SYU-RYOYOHI-SKJ
                               +  SYU-RYOYOHI-SKJ-JIHI
      * 20040202 削除 自己負担額は食事療養費に含まれている為、削除する。
      ***                  +  SYU-SKYMONEY-SKJ-KEI
      ***                  +  SYU-SKYMONEY-SKJ-JIHI-KEI
      * 20040202 削除 自己負担額は食事療養費に含まれている為、削除する。
                               +  SYU-RMSAGAKU
      * 20040202 変更
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *              WHEN 14
             WHEN 19
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *    自賠責の場合
      * 20080611 健保準拠時の自賠責の計算を修正START tanaka
               MOVE   SYU-RSIJIBAI-SKYMONEY   TO   WRK-SYUEKI
      *               COMPUTE  WRK-SYUEKI ROUNDED = 
      *               (SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA)
      *=== 自賠責計算式変更対応 By Takashima(2006/12/18) start
      *               COMPUTE  WRK-SYUEKI =
      *                      ( WRK-SYUEKI * (SYS-4001-KSNRATE / 100) ) 
      *                     +  WRK-SYUEKI
      *                   COMPUTE  WRK-SYUEKI ROUNDED =
      *                      ( WRK-SYUEKI * (SYS-4001-KSNRATE1 / 100) ) 
      *               COMPUTE  WRK-SYUEKI  =
      *                        WRK-SYUEKI
      *=== 自賠責計算式変更対応 By Takashima(2006/12/18) end
      *                     +  SYU-RSISHOSHIN-MONEY
      *                     +  SYU-RSISAISHIN-MONEY
      *                     +  SYU-RSISDO-MONEY
      *                     +  SYU-RSIETC-MONEY
      *                     +  WRK-JIHI
      *                     +  SYU-CHOSEI
      * 20030411 追加
      * 20030814 削除
      * 老人一部負担金、公費一部負担金は保険点数に含まれている為削除
      ***                  +  SYU-RJN-FTN
      ***                  +  SYU-KOH-FTN
      * 20030814 削除
      *                     +  SYU-YKZ-FTN
      *                     +  SYU-RYOYOHI-SKJ
      *                     +  SYU-RYOYOHI-SKJ-JIHI
      * 20040202 削除 自己負担額は食事療養費に含まれている為、削除する。
      ***                  +  SYU-SKYMONEY-SKJ-KEI
      ***                  +  SYU-SKYMONEY-SKJ-JIHI-KEI
      * 20040202 削除 自己負担額は食事療養費に含まれている為、削除する。
      *                    +  SYU-RMSAGAKU
                           
      * 20080611 健保準拠時の自賠責の計算を修正END
      * 20040202 変更
      * 20080324 H20年4月改正対応START tanaka
             WHEN 20
               COMPUTE   WRK-SYUEKI   =   SYU-RSIJIBAI-SKYMONEY
      * 20080324 H20年4月改正対応END
             WHEN OTHER
      *    労災、自賠責、自費以外の場合
               COMPUTE   WRK-SYUEKI   =  (SYU-TOTAL-HKNTEN * HKN-TENTNK)
                                      +  WRK-JIHI
                                      +  SYU-CHOSEI
      * 20030411 追加
      * 20030814 削除
      * 老人一部負担金、公費一部負担金は保険点数に含まれている為削除
      ***                  +  SYU-RJN-FTN
      ***                  +  SYU-KOH-FTN
      * 20030814 削除
                                      +  SYU-YKZ-FTN
                                      +  SYU-RYOYOHI-SKJ
                                      +  SYU-RYOYOHI-SKJ-JIHI
      * 20040202 削除 自己負担額は食事療養費に含まれている為、削除する。
      ***                  +  SYU-SKYMONEY-SKJ-KEI
      ***                  +  SYU-SKYMONEY-SKJ-JIHI-KEI
      * 20040202 削除 自己負担額は食事療養費に含まれている為、削除する。
                                      +  SYU-RMSAGAKU
      *--- 2008/12/16 ADD START ----------------------------------------
      *--- 2009/02/06 CHANGE START -------------------------------------
      *         COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  -  SYU-DISCOUNT-MONEY
               COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI
                                    -  (WRK-COMPFTN
                                    +  SYU-SKYMONEY-SKJ-KEI
                                    +  SYU-SKYMONEY-SKJ-JIHI-KEI
                                    +  SYU-SKYMONEY-LIFE-KEI
                                    +  SYU-SKYMONEY-LIFE-JIHI-KEI
                                    +  SYU-RMSAGAKU
                                    +  SYU-CHOSEI 
                                    +  WRK-JIHI
                                    -  SYU-SKYMONEY)
      *--- 2009/02/06 CHANGE END ---------------------------------------
      *--- 2008/12/16 ADD END ------------------------------------------
      * 20030411 追加
           END-EVALUATE
           MOVE   WRK-SYUEKI   TO   WRK-SZ
           MOVE   WRK-SZ       TO   HC17-SYUEKI (CNT-LINE)
      *
      *** 20040907  MOTOHASI
           IF   WRK-SYUEKI  NOT =  0
             ADD  WRK-SYUEKI   TO   SUM-G-SYUEKI
      *--- 2009/02/24 ADD START ----------------------------------------
             ADD  WRK-SYUEKI   TO   SUM-GG-SYUEKI
      *--- 2009/02/24 ADD END ------------------------------------------
           END-IF
      *** 20040907  MOTOHASI
      *
      ****    20030224追加
      *
           .
       320-BODY-HEN-SUB-EXT.
           EXIT.
      * 20080324 H20年4月改正対応START tanaka
      *****************************************************************
      *    帳票編集<明細行>サブ処理(H20年4月以降)
      *****************************************************************
       330-BODY-HEN-SUB-SEC      SECTION.
      *
      *  保険番号情報読み込み
           PERFORM 900-HKNNUM-READ-SEC
           IF   FLG-HKNNUM  =  1
      *    該当レコードが存在しない場合は単価を10円に設定する。
             MOVE 10     TO HKN-TENTNK
           END-IF
      *
      *    保険点数
           MOVE    ZERO          TO   SYU-TOTAL-HKNTEN
           EVALUATE    SYU-SYUHKNNUM
             WHEN  OTHER
               PERFORM  VARYING  IDZ   FROM  1   BY  1
      * 20080324 H20年4月改正対応START tanaka
      *                       UNTIL   IDZ >       15
                        UNTIL   IDZ  >  16
      * 20080324 H20年4月改正対応END
                 ADD   SYU-HKNTEN(IDZ)     TO   SYU-TOTAL-HKNTEN
               END-PERFORM
               MOVE   SYU-TOTAL-HKNTEN      TO   WRK-Z9
               MOVE   WRK-Z9                TO   HC17-HKNTEN (CNT-LINE)
               ADD    SYU-TOTAL-HKNTEN      TO   SUM-HKNTEN
               ADD    1                     TO   SUM-HKNKEN
      *
               ADD    SYU-TOTAL-HKNTEN      TO   SUM-G-HKNTEN
      *--- 2009/02/24 ADD START ----------------------------------------
               ADD    SYU-TOTAL-HKNTEN      TO   SUM-GG-HKNTEN
      *--- 2009/02/24 ADD END ------------------------------------------
      *
           END-EVALUATE
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                     TO   IDX
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE   6                     TO   IDX
      * 20080324 H20年4月改正対応START tanaka
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE   20                    TO   IDX
      * 20080324 H20年4月改正対応END
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE   19                    TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE   18                    TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
               MOVE    17                   TO   IDX
             WHEN   "060"
             WHEN   "067"
             WHEN   "068"
             WHEN   "069"
               IF   SYU-KOH1HKNNUM   =  "027"
                 MOVE    5                  TO   IDX
               ELSE
                 MOVE    4                  TO   IDX
               END-IF
             WHEN   "039"
             WHEN   "040"
               MOVE        5                TO   IDX
             WHEN   OTHER
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    5                  TO   IDX
               ELSE
                 PERFORM   3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1              TO   IDX
                   WHEN    "2"
                     MOVE    2              TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
      *    保険別区分例外条件(自費条件拡張)
           IF   SYU-SYUHKNNUM(1:2) = "98"
             MOVE    17    TO      IDX
           END-IF
      *
           IF   IDX   >   ZERO
      *
      *    自費以外のとき自費分だけ集計
             INITIALIZE WRK-JIHI
             INITIALIZE WRK-JIHI3
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) start
      *       IF    IDX   =   17
      *         MOVE    ZERO            TO   WRK-JIHI
      *         MOVE    ZERO            TO   WRK-JIHI3
      *       ELSE
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) end
               IF  SYU-SRYYMD  <=  "20070331"
                 COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                     +   SYU-TOTAL-TGMONEY-TAX
                                     +   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
                                     +   SYU-JIHI-TAX
      *
                 COMPUTE WRK-JIHI3   =   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
                                     +   SYU-JIHI-TAX
               ELSE
                 COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                     +   SYU-TOTAL-TGMONEY-TAX
                                     +   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
      *
                 COMPUTE WRK-JIHI3   =   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
               END-IF
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) start
      *       END-IF
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) end
             COMPUTE    WRK-MONEY    =   SYU-SKYMONEY  -  WRK-JIHI3
      *
      *--- 2008/12/16 ADD START ----------------------------------------
      *             MOVE    WRK-MONEY     TO  WRK-Z9
             MOVE    WRK-MONEY     TO   WRK-S9-2
      *--- 2008/12/16 ADD END ------------------------------------------
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *             IF  IDX  >  16
      *                 COMPUTE IDX3      =   IDX  -  16
      *                 MOVE    WRK-Z9    TO  HC171-MONEY (IDX3 CNT-LINE)
      *             ELSE
      *--- 2008/12/16 ADD START ----------------------------------------
      *                 MOVE    WRK-Z9    TO  HC17-MONEY (IDX CNT-LINE)
             MOVE    WRK-S9-2      TO   HC17-MONEY(IDX CNT-LINE)
      *--- 2008/12/16 ADD END ------------------------------------------
      *             END-IF
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
             ADD     WRK-MONEY     TO   SUM-MONEY(IDX)
             ADD     1             TO   SUM-KEN(IDX)
      *
             ADD     WRK-MONEY     TO   SUM-G-MONEY(IDX)
      *--- 2009/02/24 ADD START ----------------------------------------
             ADD     WRK-MONEY     TO   SUM-GG-MONEY(IDX)
      *--- 2009/02/24 ADD END ------------------------------------------
      *
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) start
      *       IF   IDX  =   17
      *         CONTINUE
      *       ELSE
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) end
               IF   WRK-JIHI    >   ZERO
                 PERFORM  VARYING  IDX2  FROM  1  BY  1
                          UNTIL    IDX2   >   10
                   INITIALIZE WRK-JIHI2
                   COMPUTE   WRK-JIHI2  =   SYU-JIHI-TAXNASI(IDX2)
                                        +   SYU-JIHI-TAXARI(IDX2)
                   IF   WRK-JIHI2  >  ZERO
                     MOVE WRK-JIHI2  TO  WRK-ZZ
                     COMPUTE  JIHIIDX  =  IDX2  +  6 
                     MOVE   WRK-ZZ     TO   HC17-MONEY(JIHIIDX CNT-LINE)
                     ADD    WRK-JIHI2  TO   SUM-MONEY(JIHIIDX)
                     ADD    1          TO   SUM-KEN(JIHIIDX)
      *
                     ADD    WRK-JIHI2  TO   SUM-G-MONEY(JIHIIDX)
      *--- 2009/02/24 ADD START ----------------------------------------
                     ADD    WRK-JIHI2  TO   SUM-GG-MONEY(JIHIIDX)
      *--- 2009/02/24 ADD END ------------------------------------------
      *
                   END-IF
                 END-PERFORM
               END-IF
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) start
      *       END-IF
      *--- 自費保険使用時の不具合を修正 By Monma (2009/03/02) end
           END-IF
      *
      *    医療収益計算
      *--- 2009/02/06 ADD START ----------------------------------------
      *--- 2009/02/17 CHANGE START -------------------------------------
      *--- 医療収益の計算式を修正 2009/04/08 CHANGE START --------------
           IF  SYU-KOH4HKNNUM  NOT =  SPACE
      *       MOVE   SYU-KOH4COMPFTN    TO   WRK-COMPFTN
             MOVE   SYU-KOH4HKNFTNMONEY    TO   WRK-COMPFTN
           ELSE
             IF  SYU-KOH3HKNNUM  NOT =  SPACE
      *         MOVE   SYU-KOH3COMPFTN    TO   WRK-COMPFTN
               MOVE   SYU-KOH3HKNFTNMONEY    TO   WRK-COMPFTN
             ELSE
               IF  SYU-KOH2HKNNUM  NOT =  SPACE
      *           MOVE   SYU-KOH2COMPFTN    TO   WRK-COMPFTN
                 MOVE   SYU-KOH2HKNFTNMONEY    TO   WRK-COMPFTN
               ELSE
                 IF  SYU-KOH1HKNNUM  NOT =  SPACE
      *             MOVE   SYU-KOH1COMPFTN    TO   WRK-COMPFTN
                   MOVE   SYU-KOH1HKNFTNMONEY    TO   WRK-COMPFTN
                 ELSE
                   IF  SYU-SYUHKNNUM  NOT =  SPACE
      *               MOVE   SYU-SYUCOMPFTN     TO   WRK-COMPFTN
                     MOVE   SYU-SYUHKNFTNMONEY    TO   WRK-COMPFTN
      *--- 医療収益の計算式を修正 2009/04/08 CHANGE END   --------------
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
      *--- 2009/02/17 CHANGE END ---------------------------------------
      *--- 2009/02/06 ADD END ------------------------------------------
           EVALUATE IDX
             WHEN 17
      *    自費の場合
               MOVE  SYU-SKYMONEY  TO   WRK-SYUEKI
             WHEN 18
      *    労災の場合
               COMPUTE  WRK-SYUEKI ROUNDED   =
                             (SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA)
                                +  WRK-JIHI
                                +  SYU-RSISHOSHIN-MONEY
                                +  SYU-RSISAISHIN-MONEY
                                +  SYU-RSISDO-MONEY
                                +  SYU-RSIETC-MONEY
                                +  SYU-CHOSEI
                                +  SYU-YKZ-FTN
                                +  SYU-RYOYOHI-SKJ
                                +  SYU-RYOYOHI-SKJ-JIHI
                                +  SYU-RMSAGAKU
             WHEN 19
      ***  自賠責の場合
      * 20080611 健保準拠時の自賠責の計算を修正START tanaka
      *               COMPUTE  WRK-SYUEKI ROUNDED = 
      *              (SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA)
      *               COMPUTE  WRK-SYUEKI ROUNDED =
      *                      ( WRK-SYUEKI * (SYS-4001-KSNRATE1 / 100) ) 
               MOVE  SYU-RSIJIBAI-SKYMONEY  TO  WRK-SYUEKI
      *                             SYU-SYUCOMPFTN
      *                      +  SYU-RSISHOSHIN-MONEY
      *                      +  SYU-RSISAISHIN-MONEY
      *                      +  SYU-RSISDO-MONEY
      *                      +  SYU-RSIETC-MONEY
      *                      +  WRK-JIHI
      *                      +  SYU-CHOSEI
      *                      +  SYU-YKZ-FTN
      **                     +  SYU-RYOYOHI-SKJ
      **                     +  SYU-RYOYOHI-SKJ-JIHI
      *                      +  SYU-RMSAGAKU
      * 20080611 健保準拠時の自賠責の計算を修正END
      ***  公害の場合
             WHEN 20
               COMPUTE   WRK-SYUEKI  =  SYU-RSIJIBAI-SKYMONEY
             WHEN OTHER
      ***  労災、自賠責、自費、公害以外の場合
               COMPUTE  WRK-SYUEKI  =
                          (SYU-TOTAL-HKNTEN  *  HKN-TENTNK)
                             +  WRK-JIHI
                             +  SYU-CHOSEI
                             +  SYU-YKZ-FTN
                             +  SYU-RYOYOHI-SKJ
                             +  SYU-RYOYOHI-SKJ-JIHI
                             +  SYU-RMSAGAKU
      *--- 2008/12/16 ADD START ----------------------------------------
      *--- 2009/02/06 CHANGE START -------------------------------------
      *         COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  -  SYU-DISCOUNT-MONEY
               COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI
                                    -  (WRK-COMPFTN
                                    +  SYU-SKYMONEY-SKJ-KEI
                                    +  SYU-SKYMONEY-SKJ-JIHI-KEI
                                    +  SYU-SKYMONEY-LIFE-KEI
                                    +  SYU-SKYMONEY-LIFE-JIHI-KEI
                                    +  SYU-RMSAGAKU
                                    +  SYU-CHOSEI 
                                    +  WRK-JIHI
                                    -  SYU-SKYMONEY)
      *--- 2009/02/06 CHANGE END ---------------------------------------
      *--- 2008/12/16 ADD END ------------------------------------------
           END-EVALUATE
           MOVE  WRK-SYUEKI   TO   WRK-SZ
           MOVE  WRK-SZ       TO   HC17-SYUEKI(CNT-LINE)
      *
           IF   WRK-SYUEKI  NOT =  0
               ADD  WRK-SYUEKI   TO   SUM-G-SYUEKI
      *--- 2009/02/24 ADD START ----------------------------------------
               ADD  WRK-SYUEKI   TO   SUM-GG-SYUEKI
      *--- 2009/02/24 ADD END ------------------------------------------
           END-IF
      *
           .
       320-BODY-HEN-SUB-EXT.
           EXIT.
      *
      * 20080324 H20年4月改正対応END
      *****************************************************************
      *    小計印字処理
      *****************************************************************
       3001-SYOUKEI-SET-SEC          SECTION.
      *
      *    保険点数
           IF  SUM-G-HKNTEN  >  9999999
      *--- 2008/12/16 CHANGE START ----------------------------------------
      *    8桁の場合
      *       INITIALIZE  WRK-ZZZ
      *       MOVE    SUM-G-HKNTEN     TO   WRK-ZZZ
      *       MOVE    WRK-ZZZ          TO   HC17-GG-HKNTEN
             INITIALIZE  WRK-SZ
             MOVE    SUM-G-HKNTEN       TO   WRK-SZ
             MOVE    WRK-SZ             TO   HC17-GG-HKNTEN
           ELSE
      *    7桁以下の場合
      *       INITIALIZE  WRK-ZZ
      *       MOVE    SUM-G-HKNTEN     TO   WRK-ZZ
      *       MOVE    WRK-ZZ           TO   HC17-G-HKNTEN
             INITIALIZE  WRK-Z9-2
             MOVE    SUM-G-HKNTEN       TO   WRK-Z9-2
             MOVE    WRK-Z9-2           TO   HC17-G-HKNTEN
      *--- 2008/12/16 CHANGE END ---------------------------------------
           END-IF
      *    保険別金額（社保本人〜自賠責）
           PERFORM  VARYING  IDX-SUM   FROM 1  BY  1
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *              UNTIL    IDX-SUM   >    14
      * 20080324 H20年4月改正対応START tanaka
      *              UNTIL    IDX-SUM   >    19
                    UNTIL    IDX-SUM   >    20
      * 20080324 H20年4月改正対応END
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
             IF  SUM-G-MONEY (IDX-SUM)  >  9999999
      *    8桁の場合
      *--- 2008/12/16 CHANGE START ----------------------------------------
      *         INITIALIZE  WRK-ZZZ
      *         MOVE     SUM-G-MONEY(IDX-SUM)   TO   WRK-ZZZ
               INITIALIZE  WRK-SZ
               MOVE     SUM-G-MONEY(IDX-SUM)   TO   WRK-SZ
      *--- 2008/12/16 CHANGE END ---------------------------------------
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *           MOVE     WRK-ZZZ   TO   HC17-GG-MONEY (IDX-SUM)
      *           IF  IDX-SUM  >  16
      *               COMPUTE  IDX3      =    IDX-SUM  -  16
      *               MOVE     WRK-ZZZ   TO   HC171-GG-MONEY (IDX3)
      *           ELSE
      *--- 2008/12/16 CHANGE START ----------------------------------------
      *         MOVE     WRK-ZZZ   TO   HC17-GG-MONEY(IDX-SUM)
               MOVE     WRK-SZ   TO   HC17-GG-MONEY(IDX-SUM)
      *--- 2008/12/16 CHANGE END ---------------------------------------
      *          END-IF
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
             ELSE
      *    7桁以下の場合
      *--- 2008/12/16 CHANGE START ----------------------------------------
      *         INITIALIZE  WRK-ZZ
      *         MOVE     SUM-G-MONEY (IDX-SUM)   TO   WRK-ZZ
               INITIALIZE  WRK-Z9-2
               MOVE     SUM-G-MONEY (IDX-SUM)   TO   WRK-Z9-2
      *--- 2008/12/16 CHANGE END ---------------------------------------
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *           MOVE     WRK-ZZ   TO   HC17-G-MONEY (IDX-SUM)
      *           IF  IDX-SUM  >  16
      *               COMPUTE  IDX3      =    IDX-SUM  -  16
      *               MOVE     WRK-ZZ   TO   HC171-G-MONEY (IDX3)
      *           ELSE
      *--- 2008/12/16 CHANGE START ----------------------------------------
      *         MOVE     WRK-ZZ     TO   HC17-G-MONEY(IDX-SUM)
               MOVE     WRK-Z9-2   TO   HC17-G-MONEY(IDX-SUM)
      *--- 2008/12/16 CHANGE END ---------------------------------------
      *           END-IF
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
             END-IF
           END-PERFORM
      *    医療収益
           IF  SUM-G-SYUEKI  >  9999999
      *    8桁の場合
               INITIALIZE  WRK-SZZ
             MOVE    SUM-G-SYUEKI     TO   WRK-SZZ
             MOVE    WRK-SZZ          TO   HC17-GG-SYUEKI
           ELSE
      *    7桁以下の場合
               INITIALIZE  WRK-SZ
             MOVE    SUM-G-SYUEKI     TO   WRK-SZ
             MOVE    WRK-SZ           TO   HC17-G-SYUEKI
           END-IF
      *    請求金額
           IF  SUM-G-SEIKYU  >  9999999
      *    8桁の場合
             INITIALIZE               WRK-SZZ
             MOVE    SUM-G-SEIKYU     TO   WRK-SZZ
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
             MOVE    WRK-SZZ          TO   HC17-GG-SEIKYU
      *         MOVE     WRK-SZZ         TO    HC171-GG-SEIKYU
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
           ELSE
      *    7桁以下の場合
             INITIALIZE               WRK-SZ
             MOVE    SUM-G-SEIKYU     TO   WRK-SZ
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
             MOVE    WRK-SZ           TO   HC17-G-SEIKYU
      *         MOVE     WRK-SZ         TO    HC171-G-SEIKYU
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
           END-IF
      *    領収金額
           IF  SUM-G-RYOSYU  >  9999999
      *    8桁の場合
             INITIALIZE               WRK-SZZ
             MOVE    SUM-G-RYOSYU     TO   WRK-SZZ
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
             MOVE    WRK-SZZ          TO   HC17-GG-RYOSYU
      *         MOVE     WRK-SZZ         TO    HC171-GG-RYOUSYU
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
           ELSE
      *    7桁以下の場合
             INITIALIZE               WRK-SZ
             MOVE    SUM-G-RYOSYU     TO   WRK-SZ
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
      *=== 自費追加対応 ADD By Takashima(2006/10/21) start
             MOVE    WRK-SZ           TO   HC17-G-RYOSYU
      *         MOVE     WRK-SZ         TO    HC171-G-RYOUSYU
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
           END-IF
           .
       3001-SYOUKEI-SET-EXT.
           EXIT.
      *
      *--- 2009/02/24 ADD START ----------------------------------------
      *****************************************************************
      *    合計印字処理
      *****************************************************************
       3000-GOUKEI-SET-SEC          SECTION.
      *
      *    保険点数
           INITIALIZE   WRK-SZZ
           MOVE    SUM-GG-HKNTEN    TO   WRK-SZZ
           MOVE    WRK-SZZ          TO   D5002-HKNTEN
      *
      *    保険別金額（社保本人〜自賠責）
           PERFORM  VARYING  IDX-SUM   FROM 1  BY  1
                    UNTIL    IDX-SUM   >   20
             INITIALIZE  WRK-SZ
             MOVE    SUM-GG-MONEY(IDX-SUM)   TO   WRK-SZ
             MOVE    WRK-SZ         TO   D5002-MONEY(IDX-SUM)
           END-PERFORM
      *
      *    医療収益
           INITIALIZE   WRK-SZ10
           MOVE    SUM-GG-SYUEKI    TO   WRK-SZ10
           MOVE    WRK-SZ10         TO   D5002-SYUEKI
      *
      *    請求金額
           INITIALIZE   WRK-SZ10
           MOVE    SUM-GG-SEIKYU    TO   WRK-SZ10
           MOVE    WRK-SZ10         TO   D5002-SEIKYU
      *
      *    領収金額
           INITIALIZE   WRK-SZ10
           MOVE    SUM-GG-RYOSYU    TO   WRK-SZ10
           MOVE    WRK-SZ10         TO   D5002-RYOSYU
           .
       3000-GOUKEI-SET-EXT.
           EXIT.
      *
      *--- 2009/02/24 ADD END ------------------------------------------
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3201-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *   保険組合せ読み出し
           INITIALIZE                  HKNCOMBI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    SYU-HOSPID        TO  COMB-HOSPID
           MOVE    SYU-HOSPNUM       TO   COMB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    SYU-PTID          TO   COMB-PTID
           MOVE    SYU-HKNCOMBINUM   TO   COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI   =   ZERO
      *    患者保険情報読み込み
             INITIALIZE              PTHKNINF-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    SYU-HOSPID        TO  PTHKN-HOSPID
             MOVE    SYU-HOSPNUM     TO  PTHKN-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
             MOVE    SYU-PTID        TO  PTHKN-PTID
             MOVE    COMB-HKNID      TO  PTHKN-HKNID
             PERFORM 900-PTHKNINF-READ-SEC
           END-IF
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *
      *--- 2009/03/23 ADD START ----------------------------------------
           ADD     1                       TO   CNT-PAGE
           MOVE    CNT-PAGE                TO   WRK-PAGE
           MOVE    WRK-PAGE                TO   HC17-PAGE
      *--- 2009/03/23 ADD END   ----------------------------------------
      *
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"                   TO   SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM   TO   SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
      *****MOVE   "HC17"               TO  SPRT-PRTID
      ***   変更(2002.11.26)
      *** 03/09/29
      *     MOVE   "HC17V011S"          TO  SPRT-PRTID
      ***  20040916 MOTOHASI MONPEに変更
      ***     MOVE   "A00000D500"         TO  SPRT-PRTID
      *
      * 20080324 H20年4月改正対応START tanaka
           IF   WRK-PARA-DENPPRTYMD  <  "20080401"
             MOVE   "A00000D500O.red"         TO   SPRT-PRTID
           ELSE
             MOVE   "A00000D500N.red"         TO   SPRT-PRTID
           END-IF
      * 20080324 H20年4月改正対応END
      *
      ***  20040916 MOTOHASI MONPEに変更
      ***
      ***     MOVE   "A14002H51"    TO  SPRT-PRTID
      ***   変更(2002.11.26)
      ***     MOVE    "日計表（伝票発行日）"
      * 20031211 帳票タイトル変更 
      ***     MOVE    "日計表（明細）"  TO  SPRT-TITLE
           MOVE    "日計表明細（窓口領収金）"   TO   SPRT-TITLE
      * 20031211 帳票タイトル変更 
           MOVE    HC17                  TO   SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID   TO   SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID     TO   SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM    TO   SPRT-PRTNM
      ***  20030814 削除
      ***  site-libを実行環境にする為コメントアウト
      ***     MOVE    "1"          TO  SPRT-SITEKBN
      ***  20030814 削除
           CALL    "ORCSPRT"             USING
                                         ORCSPRTAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
                                         SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *---- (08/11/04) LINE DEL START MONMA ----------------------------
           IF   SPRT-RETURN   =   ZERO
      **=== 自費追加対応 ADD By Takashima(2006/10/21) start
      *         MOVE   "A00000D5001.red"         TO  SPRT-PRTID
      *         MOVE    "日計表明細（窓口領収金）"
      *                                     TO  SPRT-TITLE
      *         MOVE    HC171     TO  SPRT-PRTDATA
      *         MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
      *         MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
      *         MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      *         CALL    "ORCSPRT"           USING
      *                                     ORCSPRTAREA
      **=== 4.0対応 By Takashima(2007/09/10) start
      *                                     SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *         IF      SPRT-RETURN         =   ZERO
      *             CONTINUE
      *         ELSE
      *             MOVE    "印刷ＤＢに更新できませんでした"
      *                                        TO  WRK-RECEERR
      *             PERFORM 500-ERR-HENSYU-SEC
      *         END-IF
      *=== 自費追加対応 ADD By Takashima(2006/10/21) end
      *---- (08/11/04) LINE DEL END   MONMA ----------------------------
             CONTINUE
           ELSE
             MOVE   "印刷ＤＢに更新できませんでした"   TO   WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *--- 2009/02/24 ADD START ----------------------------------------
      *****************************************************************
      *    総合計帳票印刷処理
      *****************************************************************
       400-PRINT-OUT-SEC                SECTION.
      *
      *--- 2009/03/23 ADD START ----------------------------------------
           ADD     1                           TO   CNT-PAGE
           MOVE    CNT-PAGE                    TO   WRK-PAGE
           MOVE    WRK-PAGE                    TO   D5002-PAGE
      *--- 2009/03/23 ADD END   ----------------------------------------
      *
           INITIALIZE                          ORCSPRTAREA
           MOVE    "INS"                       TO   SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM         TO   SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY        TO   SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP      TO   SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM          TO   SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD         TO   SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID        TO   SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM   TO   SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY       TO   SPRT-PRIORITY
           IF   WRK-PARA-DENPPRTYMD  <  "20080401"
             MOVE   "A00000D500O2.red"         TO   SPRT-PRTID
           ELSE
             MOVE   "A00000D500N2.red"         TO   SPRT-PRTID
           END-IF
           MOVE    "日計表明細（窓口領収金）"  TO   SPRT-TITLE
           MOVE    D5002                       TO   SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID         TO   SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID           TO   SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM          TO   SPRT-PRTNM
           CALL    "ORCSPRT"                   USING
                                               ORCSPRTAREA
                                               SPA-AREA
           IF   SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE   "印刷ＤＢに更新できませんでした"   TO   WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       400-PRINT-OUT-EXT.
           EXIT.
      *
      *--- 2009/02/24 ADD END ------------------------------------------
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF    STS-RECEERR   =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT              RECEERR-FILE
      *
             MOVE    WRK-RECEERR       TO   RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *       
      *    ジョブ管理開始処理
             MOVE    "JBE"             TO   SJOBKANRI-MODE
             INITIALIZE                  JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
             MOVE    SPA-HOSPNUM       TO   JOB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
             MOVE    WRK-PARA-JOBID    TO   JOB-JOBID
             MOVE    WRK-PARA-SHELLID  TO   JOB-SHELLID
             MOVE    WRK-RECEERR       TO   JOB-YOBI
             MOVE    "9999"            TO   JOB-ERRCD
             CALL    "ORCSJOB"         USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           END-IF
      *
           MOVE    1                   TO   FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
      *    ステップ管理終了処理
           MOVE    "STE"               TO   SJOBKANRI-MODE
           INITIALIZE                       JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   JOB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-JOBID      TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO   JOB-SHELLID
           MOVE    CNT-PAGE            TO   JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           PERFORM   900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO   LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO   LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO   WRK-HENYMDG
           INSPECT WRK-HENYMDG  REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細情報　入返金発行日で検索(入院患者)
      *****************************************************************
       910-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  SYUMEI-REC
      *
      ***  @@@@-MOTOHASI  20031014  医療機関ID、入外区分(1)、入返金日をセット
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    WRK-PARA-HOSPID     TO  SMEI-HOSPID
           MOVE    WRK-PARA-HOSPNUM    TO   SMEI-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    "1"                 TO   SMEI-NYUGAIKBN
           MOVE    WRK-PARA-DENPPRTYMD TO   SMEI-NYUHEN-YMD
      ***  @@@@-MOTOHASI  20031014
           MOVE    SYUMEI-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
      ***  @@@@-MOTOHASI  20031014  取得キーを変更
      ***     MOVE    "SYUMEI-ALL"    TO  ORC-DBPATH
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"        TO   MCP-TABLE
           MOVE    "key4"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC  =  ZERO
             PERFORM 910-DBFETCH-SEC
           ELSE
             MOVE    1                 TO   FLG-SYUMEI
           END-IF
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納明細情報を検索処理(入院患者)
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO   MCP-FUNC
      ***  @@@@-MOTOHASI  20031014  取得キーを変更
      ***    MOVE    "SYUMEI-ALL"     TO  ORC-DBPATH
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"        TO   MCP-TABLE
           MOVE    "key4"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    ZERO              TO   FLG-SYUMEI
             MOVE    MCPDATA-REC       TO   SYUMEI-REC
           ELSE
               MOVE    1               TO   FLG-SYUMEI
           END-IF  
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細情報　入返金発行日で検索(外来患者) 20031014  @@@@-MOTOHASI 追加
      *****************************************************************
       920-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  SYUMEI-REC
      *
      ***  @@@@-MOTOHASI  20031014  医療機関ID、入外区分(2)、入返金日をセット
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    WRK-PARA-HOSPID     TO  SMEI-HOSPID
           MOVE    WRK-PARA-HOSPNUM     TO   SMEI-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    "2"                  TO   SMEI-NYUGAIKBN
           MOVE    WRK-PARA-DENPPRTYMD  TO   SMEI-NYUHEN-YMD
      ***  @@@@-MOTOHASI  20031014
           MOVE    SYUMEI-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
      ***  @@@@-MOTOHASI  20031014  取得キーを変更
      ***     MOVE    "SYUMEI-ALL"    TO  ORC-DBPATH
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"         TO   MCP-TABLE
           MOVE    "key4"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF    MCP-RC   =   ZERO
              PERFORM   920-DBFETCH-SEC
           ELSE
             MOVE    1                  TO  FLG-SYUMEI
           END-IF
           .
       920-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納明細情報を検索処理(外来患者) 20031014  @@@@-MOTOHASI 追加
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO   MCP-FUNC
      ***  @@@@-MOTOHASI  20031014  取得キーを変更
      ***    MOVE    "SYUMEI-ALL"     TO  ORC-DBPATH
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"        TO   MCP-TABLE
           MOVE    "key4"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    ZERO              TO   FLG-SYUMEI
             MOVE    MCPDATA-REC       TO   SYUMEI-REC
           ELSE
             MOVE    1                 TO   FLG-SYUMEI
           END-IF
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"        TO   MCP-TABLE
           MOVE    "key4"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       900-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE                 SYUNOU-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    SMEI-HOSPID    TO  SYU-HOSPID
           MOVE    SMEI-HOSPNUM    TO   SYU-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    SMEI-NYUGAIKBN  TO   SYU-NYUGAIKBN
           MOVE    SMEI-PTID       TO   SYU-PTID
           MOVE    SMEI-DENPNUM    TO   SYU-DENPNUM
      *
           MOVE    SYUNOU-REC      TO   MCPDATA-REC
           MOVE    "DBSELECT"      TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"    TO   MCP-TABLE
           MOVE    "key"           TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                   MCPAREA
                                   MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                   SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *       MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *       CALL    "MCPSUB"             USING
             MOVE    "tbl_syunou"        TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                         MCPAREA
                                         MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                         SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   SYUNOU-REC
               MOVE    ZERO              TO   FLG-SYUNOU
             ELSE
               MOVE    1                 TO   FLG-SYUNOU
             END-IF
           ELSE
             MOVE    1                   TO   FLG-SYUNOU
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"              TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL     "MCPSUB"            USING
           MOVE    "tbl_syunou"          TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                         MCPAREA
                                         MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                         SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       901-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE                   SYUNOU-REC
      *
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    WRK-SMEI-HOSPID(IDX-S)
      *                   TO  SYU-HOSPID
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)     TO   SYU-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO   SYU-NYUGAIKBN
           MOVE    WRK-SMEI-PTID(IDX-S)        TO   SYU-PTID
           MOVE    WRK-SMEI-DENPNUM(IDX-S)     TO   SYU-DENPNUM
      *
           MOVE    SYUNOU-REC                  TO   MCPDATA-REC
           MOVE    "DBSELECT"                  TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"                TO   MCP-TABLE
           MOVE    "key"                       TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"                 USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                               MCPAREA
                                               MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"            TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_syunou"         TO   MCP-TABLE
             MOVE    "key"                TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                          MCPAREA
                                          MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                          SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF    MCP-RC  =   ZERO
               MOVE    MCPDATA-REC        TO   SYUNOU-REC
               MOVE    ZERO               TO   FLG-SYUNOU
             ELSE
               MOVE    1                  TO   FLG-SYUNOU
             END-IF
           ELSE
             MOVE    1                    TO   FLG-SYUNOU
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"             TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"        TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY   TO  MCP-PATH
      *     CALL     "MCPSUB"            USING
           MOVE    "tbl_syunou"           TO   MCP-TABLE
           MOVE    "key"                  TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                          MCPAREA
                                          MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                          SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           .
       901-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号情報検索
      *****************************************************************
       900-HKNNUM-READ-SEC                 SECTION.
      *
           INITIALIZE                      HKNNUM-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    SYU-HOSPID          TO  HKN-HOSPID
           MOVE    SYU-HOSPNUM          TO   HKN-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    SYU-SYUHKNNUM        TO   HKN-HKNNUM
           MOVE    WRK-PARA-DENPPRTYMD  TO   HKN-TEKSTYMD
           MOVE    ALL "9"              TO   HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *     CALL    "MCPSUB"           USING
           MOVE    "tbl_hknnum"         TO   MCP-TABLE
           MOVE    "key5"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *         CALL    "MCPSUB"           USING
             MOVE    "tbl_hknnum"       TO   MCP-TABLE
             MOVE    "key5"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =  ZERO
               MOVE    MCPDATA-REC      TO   HKNNUM-REC
               MOVE    ZERO             TO   FLG-HKNNUM
             ELSE
               MOVE    1                TO   FLG-HKNNUM
             END-IF
           ELSE
             MOVE    1                  TO   FLG-HKNNUM
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *     CALL     "MCPSUB"               USING
           MOVE    "tbl_hknnum"         TO   MCP-TABLE
           MOVE    "key5"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
      ***     MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"           TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_syskanri"     TO   MCP-TABLE
             MOVE    "key10"            TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =   ZERO
               MOVE    ZERO             TO   FLG-SYSKANRI
             ELSE
               MOVE    1                TO   FLG-SYSKANRI
             END-IF
           ELSE
               MOVE    1                TO   FLG-SYSKANRI
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       810-SYSKANRI-READ-SEC           SECTION.
      *
      ***     MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"           TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_syskanri"      TO   MCP-TABLE
             MOVE    "key10"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                         MCPAREA
                                         MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                         SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =   ZERO
               MOVE    ZERO              TO   FLG-SYSKANRI2
             ELSE
               MOVE    1                 TO   FLG-SYSKANRI2
             END-IF
           ELSE
             MOVE    1                   TO   FLG-SYSKANRI2
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"            TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key10"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                         MCPAREA
                                         MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                         SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       820-SYSKANRI-READ-SEC           SECTION.
      *
      ***     MOVE    WRK-SYSYMD           TO  ORC-DBYMD
           MOVE    "DBSELECT"           TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_syskanri"     TO   MCP-TABLE
             MOVE    "key10"            TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =   ZERO
               MOVE    ZERO             TO   FLG-SYSKANRI2
             ELSE
               MOVE    1                TO   FLG-SYSKANRI2
             END-IF
           ELSE
             MOVE    1                  TO   FLG-SYSKANRI2
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"            TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"                 USING
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           .
       820-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC             TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptinf"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                         MCPAREA
                                         MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                         SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_ptinf"         TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                         MCPAREA
                                         MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                         SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   PTINF-REC
               MOVE    ZERO              TO   FLG-PTINF
             ELSE
               MOVE    1                 TO   FLG-PTINF
               INITIALIZE                PTINF-REC
             END-IF
           ELSE
             MOVE    1                   TO   FLG-PTINF
             INITIALIZE                  PTINF-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptinf"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *--- 2009/02/20 ADD START ----------------------------------------
      *****************************************************************
      *    患者番号マスタ読込処理
      *****************************************************************
       900-PTNUM-READ-SEC         SECTION.
      *
           INITIALIZE                    PTNUM-REC
           MOVE    SMEI-PTID             TO   PTNUM-PTID
           MOVE    PTNUM-REC             TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
           MOVE    "tbl_ptnum"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
             MOVE    "tbl_ptnum"         TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   PTNUM-REC
             ELSE
               INITIALIZE                PTNUM-REC
             END-IF
           ELSE
             INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
           MOVE    "tbl_ptnum"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *--- 2009/02/20 ADD END ------------------------------------------
      *****************************************************************
      *    患者保険マスター読込処理
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTHKNINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_pthkninf"        TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                         MCPAREA
                                         MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                         SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-PTHKNINF-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_pthkninf"      TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                         MCPAREA
                                         MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                         SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   PTHKNINF-REC
               MOVE    ZERO              TO   FLG-PTHKNINF
             ELSE
               MOVE    1                 TO   FLG-PTHKNINF
               INITIALIZE                PTHKNINF-REC
             END-IF
           ELSE
             MOVE    1                   TO   FLG-PTHKNINF
             INITIALIZE                  PTHKNINF-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTHKNINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_pthkninf"        TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                         MCPAREA
                                         MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                         SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_hkncombi"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_hkncombi"     TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO   HKNCOMBI-REC
               MOVE    ZERO             TO   FLG-HKNCOMBI
             ELSE
               MOVE    1                TO   FLG-HKNCOMBI
               INITIALIZE               HKNCOMBI-REC
             END-IF
           ELSE
             MOVE    1                  TO   FLG-HKNCOMBI
             INITIALIZE                 HKNCOMBI-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_hkncombi"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO   MCP-FUNC.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE           TO   MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                                  MCPDATA-REC.
                                       MCPDATA-REC
                                       SPA-AREA
                                        .
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           MOVE    "DBSTART"           TO   MCP-FUNC.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE           TO   MCP-TABLE
                                            MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                                  MCPDATA-REC.
                                       MCPDATA-REC
                                       SPA-AREA
                                        .
      *=== 4.0対応 By Takashima(2007/09/10) end
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"           TO   MCP-FUNC.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           MOVE    "DBDISCONNECT"       TO   MCP-FUNC.
      *=== 4.0対応 By Takashima(2007/09/10) end
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *=== 4.0対応 By Takashima(2007/09/10) end
           .
       900-DBCLOSE-EXT.
           EXIT.
