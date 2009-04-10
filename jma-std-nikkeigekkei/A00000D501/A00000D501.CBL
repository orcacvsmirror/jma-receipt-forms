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
       PROGRAM-ID.             A00000D501.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 日計表（ＨＣ１７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/09/29    @@@@-有村     新規作成(ORCBD501のプログラムID変更Ver)
      *  05/10/17    @@@@-呉秀凡   Open-cobol0.23への対応
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-本橋    03/10/14  収納明細情報の取得方法変更
      *              @@@@-半田    03/11/11  自費保険番号拡張対応の判定ロジック追加
      *              @@@@-半田    03/12/11  帳票タイトルの変更対応
      *              @@@@-半田    03/12/24  労災、自陪責の保険手数を未印字にする
      *              @@@@-半田    04/02/02  労災、自陪責の保険手数を印字する
      *                                     医療収益の算出方法の変更
      *              @@@@-半田    04/04/05  初診件数の内訳を新患件数、再患件数に分類する。
      *              @@@@-半田    04/04/21  非保険診療時の自費列診療金額算出のバグ対応
      *              @@@@-本橋    04/09/16  MONPE化
      *              @@@@-本橋    04/10/05  日医標準レセプトソフトver2.4.0対応による
      *                                     受診履歴テーブル−最終区分(lastkbn)の削除
      *              @@@@-本橋    05/04/04  自費５項目の名称が登録されていなくても
      *                                     金額を印字するようにする
      *              @@@@-本橋    05/09/14  有効期限が切られた診療科名が印字されない
      *                                     不具合を修正
      *                                     その他件数欄を追加
      *                                     ORCMCPSUB → MCPSUBに変更
      *              @@@@-高嶋    06/08/01  sarge対応
      *              @@@@-高嶋    06/10/24  自費追加対応
      *              @@@@-高嶋    06/12/18  自賠責計算式変更対応
      *              @@@@-内田    06/12/22  特別療養費（068,069）を国保で扱う
      *              @@@@-内田    07/03/20  消費税内税の影響対応
      *              @@@@-吉川    07/06/22  初診、再診、その他振分け修正
      *              @@@@-吉川    07/07/04  初診、再診、その他振分け追加修正
      *                                     新患件数の集計誤りの修正
      *              @@@@-高嶋    07/09/10  4.0対応
      *              @@@@-吉川    07/12/18  在宅等が再診で集計される不具合を修正
      *              @@@@-田中    08/03/24  H20年4月改正対応
      *              @@@@-田中    08/06/11  健保準拠時の自賠責の計算を修正
      *              @@@@-田中    08/12/04  新患数の集計条件を変更
      *              @@@@-田中    08/12/16  自費分の減免金額を集計するように修正
      *              @@@@-田中    08/12/17  医療収益に減免金額を集計するように修正
      *              @@@@-田中    09/02/06  減免金額の計算式を修正
      *              @@@@-田中    09/02/17  減免計算式の条件式を修正
      *              @@@@-門間    09/03/02  自費保険の詳細を出力するように修正
      *              @@@@-門間    09/04/08  医療収益の計算式を修正
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-劉洋   06/03/26  同日再診時の集計誤りの修正
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
      ***** 03/09/29
      *     COPY    "HC17V012S.INC".
           COPY    "A00000D501.INC".
      *****
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
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
      *--- 2008/12/04 ADD START ----------------------------------------
           03  FLG-JYURRK2         PIC 9(01).
      *--- 2008/12/04 ADD END ------------------------------------------
           03  FLG-HKNNUM          PIC 9(01).
           03  DEBUG-SW            PIC 9(01).
      *
      *  件数用フラグ領域
      ****2005/10/17 @@@@-BRAND  START
      *    03  FLG-KENSUU          PIC 9(01).
           03  FLG-KENSUU.
      ****2005/10/17 @@@@-BRAND  END
               05  FLG-HKN         PIC 9(01).
               05  FLG-SYUEKI      PIC 9(01).
               05  FLG-SEIKYU      PIC 9(01).
               05  FLG-RYOSYU      PIC 9(01).
               05  FLG-JIHI-G.
      *=== 自費追加対応 UPDATE By Takashima(2006/10/24) start
      *         07  FLG-JIHI        PIC 9(01)  OCCURS  5.
                   07  FLG-JIHI    PIC 9(01)  OCCURS  10.
      *=== 自費追加対応 UPDATE By Takashima(2006/10/24) end
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
           03  IDX7G.    
              05  IDX7             PIC 9(02).
           03  IDX8                PIC 9(04).
      *
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(05).
           03  JIHIIDX             PIC 9(04).
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
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH                PIC X(64).
      *
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-PARA-DENPPRTYMDWH PIC X(22).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-RENNUM            PIC 9(03).
           03  WRK-PTNUM             PIC X(20).
           03  WRK-PTNUM6            PIC 9(06). 
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
      * 20030530 追加
           03  WRK-PTID              PIC 9(10).
      * 20030530 追加
      *
      *****03  WRK-IN                PIC X(01).
      *
      *--- 2008/12/16 CHANGE START -------------------------------------
      *     03  WRK-MONEY             PIC 9(08).
           03  WRK-MONEY             PIC S9(08).
      *--- 2008/12/16 CHANGE END ---------------------------------------
           03  WRK-JIHI              PIC 9(08).
           03  WRK-JIHI2             PIC 9(08).
           03  WRK-JIHI3             PIC 9(08).
      *
      * 20030405 追加
           03  WRK-CREYMD            PIC X(08).
      * 20030405 追加
           03  WRK-SYUEKI            PIC 9(08).
      *=== 自費追加対応 UPDATE By Takashima(2006/10/24) start
      *     03  WRK-TJIHI-G       OCCURS  5.
           03  WRK-TJIHI-G       OCCURS  10.
      *=== 自費追加対応 UPDATE By Takashima(2006/10/24) end
               05  WRK-TJIHI         PIC X(08).
               05  WRK-TJIHI-FLG     PIC 9(01).
      * 20030407 追加 入院自費名称
      *=== 自費追加対応 UPDATE By Takashima(2006/10/24) start
      *     03  WRK-TJIHIN-G          OCCURS  5.
           03  WRK-TJIHIN-G          OCCURS  10.
      *=== 自費追加対応 UPDATE By Takashima(2006/10/24) end
               05  WRK-TJIHIN        PIC X(08).
               05  WRK-TJIHIN-FLG    PIC 9(01).
      * 20030407 追加 入院自費名称
           03  WRK-4001-TENTANKA PIC 9(02)V9(01).
      *
      * 20030620 追加 入外別印刷診療科件数
      * 入院用印刷診療科件数 
           03  WRK-PRNSRKA-CNT       PIC 9(03).
      * 外来用印刷診療科件数 
           03  WRK-PRGSRKA-CNT       PIC 9(03).
      * 20030620 追加 入外印刷診療科件数
      *
           03  WRK-RENNUM-X          PIC ZZ9.
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-Z9                PIC ZZZZZZZ9.
           03  WRK-ZZ                PIC ZZZZZZZZ.
           03  WRK-S9                PIC -------9.
           03  WRK-SZ                PIC -------Z.
           03  WRK-TMP-K             PIC X(07).
           03  WRK-COUNTS            PIC 9(04).
      *--- 2009/02/06 ADD START ----------------------------------------
           03  WRK-COMPFTN           PIC S9(10).
      *--- 2009/02/06 ADD END ------------------------------------------
      *
      *** 01  SUM-AREA              VALUE   ZERO.
      ***     03  SUM-OCC               OCCURS  15.
      ***         05  SUM-KENSU         PIC 9(05).
      ***         05  SUM-HKNTEN        PIC 9(08).
      ***         05  SUM-MONEY         PIC 9(08).
      *
      * 診療科別加算エリア
       01  SUM-AREA                 VALUE   ZERO.
           03  SUM-HKNTEN           PIC 9(09).
           03  SUM-SHKNKEN          PIC 9(09).
           03  SUM-RHKNKEN          PIC 9(09).
           03  SUM-OHKNKEN          PIC 9(09).
      * 20040405 追加
           03  SUM-SAHKNKEN         PIC 9(09).
      * 20040405 追加
      *=== 自費追加対応 UP By Takashima(2006/10/24) start
      *     03  SUM-OCC              OCCURS  14.
      * 20080325 H20年4月改正対応START tanaka
           03  SUM-OCC              OCCURS  20.
      *     03  SUM-OCC              OCCURS  19.
      * 20080325 H20年4月改正対応END
      *=== 自費追加対応 UP By Takashima(2006/10/24) end
      *--- 2008/12/16 CHANGE START -------------------------------------
      *         05  SUM-MONEY        PIC 9(09).
               05  SUM-MONEY        PIC S9(09).
      *--- 2008/12/16 CHANGE END ---------------------------------------
               05  SUM-OKEN         PIC 9(09).
               05  SUM-RKEN         PIC 9(09).
               05  SUM-SKEN         PIC 9(09).
      * 20040405 追加
               05  SUM-SAKEN        PIC 9(09).
      * 20040405 追加
           03  SUM-SYUEKI           PIC S9(09).
           03  SUM-SEIKYU           PIC S9(09).
           03  SUM-RYOSYU           PIC S9(09).
           03  SUM-SSYUEKIKEN       PIC 9(09).
           03  SUM-SSEIKYUKEN       PIC 9(09).
           03  SUM-SRYOSYUKEN       PIC 9(09).
           03  SUM-RSYUEKIKEN       PIC 9(09).
           03  SUM-RSEIKYUKEN       PIC 9(09).
           03  SUM-RRYOSYUKEN       PIC 9(09).
           03  SUM-OSYUEKIKEN       PIC 9(09).
           03  SUM-OSEIKYUKEN       PIC 9(09).
           03  SUM-ORYOSYUKEN       PIC 9(09).
      * 20040405 追加
           03  SUM-SASYUEKIKEN      PIC 9(09).
           03  SUM-SASEIKYUKEN      PIC 9(09).
           03  SUM-SARYOSYUKEN      PIC 9(09).
      * 20040405 追加
      * 20030530 追加
           03  SUM-JYUSHINKEN       PIC 9(09).
      * 20030530 追加
      *
      * 入外区分別加算エリア
       01  SUM-AREA-GK              VALUE   ZERO.
           03  SUM-HKNTEN-GK        PIC 9(09).
           03  SUM-SHKNKEN-GK       PIC 9(09).
           03  SUM-RHKNKEN-GK       PIC 9(09).
           03  SUM-OHKNKEN-GK       PIC 9(09).
      * 20040405 追加
           03  SUM-SAHKNKEN-GK      PIC 9(09).
      * 20040405 追加
      *=== 自費追加対応 UP By Takashima(2006/10/24) start
      *     03  SUM-OCC-GK           OCCURS  14.
      * 20080325 H20年4月改正対応START tanaka
      *     03  SUM-OCC-GK           OCCURS  19.
           03  SUM-OCC-GK           OCCURS  20.
      * 20080325 H20年4月改正対応END
      *=== 自費追加対応 UP By Takashima(2006/10/24) end
      *--- 2008/12/16 CHANGE START -------------------------------------
      *         05  SUM-MONEY-GK     PIC 9(09).
               05  SUM-MONEY-GK     PIC S9(09).
      *--- 2008/12/16 CHANGE END ---------------------------------------
               05  SUM-OKEN-GK      PIC 9(09).
               05  SUM-RKEN-GK      PIC 9(09).
               05  SUM-SKEN-GK      PIC 9(09).
      * 20040405 追加
               05  SUM-SAKEN-GK     PIC 9(09).
      * 20040405 追加
           03  SUM-SYUEKI-GK        PIC S9(09).
           03  SUM-SEIKYU-GK        PIC S9(09).
           03  SUM-RYOSYU-GK        PIC S9(09).
           03  SUM-SSYUEKIKEN-GK    PIC 9(09).
           03  SUM-SSEIKYUKEN-GK    PIC 9(09).
           03  SUM-SRYOSYUKEN-GK    PIC 9(09).
           03  SUM-RSYUEKIKEN-GK    PIC 9(09).
           03  SUM-RSEIKYUKEN-GK    PIC 9(09).
           03  SUM-RRYOSYUKEN-GK    PIC 9(09).
           03  SUM-OSYUEKIKEN-GK    PIC 9(09).
           03  SUM-OSEIKYUKEN-GK    PIC 9(09).
           03  SUM-ORYOSYUKEN-GK    PIC 9(09).
      * 20040405 追加
           03  SUM-SASYUEKIKEN-GK   PIC 9(09).
           03  SUM-SASEIKYUKEN-GK   PIC 9(09).
           03  SUM-SARYOSYUKEN-GK   PIC 9(09).
      * 20040405 追加
      * 20030530 追加
           03  SUM-JYUSHINKEN-GK    PIC 9(09).
      * 20030530 追加
      *
      *** 01  WRK-SMEI-REC.
      ***     03  WRK-SMEI-TBL      OCCURS  15000.
      ***         05  WRK-SMEI-HOSPID        PIC X(24).
      ***         05  WRK-SMEI-NYUGAIKBN     PIC X(01).
      ***         05  WRK-SMEI-PTID          PIC 9(10).
      ***         05  WRK-SMEI-SORT-KEY. 
      ***           07  WRK-SMEI-DENPNUM     PIC 9(07).
      ***           07  WRK-SMEI-DENPEDANUM  PIC 9(02).
      ***         05  WRK-SMEI-SKYMONEY      PIC S9(07).
      ***         05  WRK-SMEI-NYUHEN-MONEY  PIC S9(07).
      ***         05  WRK-SMEI-NYUHEN-YMD    PIC X(08).
      *
      *** 01  WRK-SMEI-REC.
      ***    03  WRK-SMEI-TBL      OCCURS  15000.
      ***         05  WRK-SMEI-HOSPID        PIC X(24).
      ***         05  WRK-SMEI-PTID          PIC 9(10).
      ***         05  WRK-SMEI-SORT-KEY. 
      ***           07  WRK-SMEI-NYUGAIKBN   PIC X(01).
      ***           07  WRK-SMEI-SRYKA       PIC X(02).
      ***           07  WRK-SMEI-DENPNUM     PIC 9(07).
      ***           07  WRK-SMEI-DENPEDANUM  PIC 9(02).
      ***         05  WRK-SMEI-SKYMONEY      PIC S9(07).
      ***         05  WRK-SMEI-NYUHEN-MONEY  PIC S9(07).
      ***         05  WRK-SMEI-NYUHEN-YMD    PIC X(08).
      *
       01  WRK-SMEI-REC.
           03  WRK-SMEI-TBL      OCCURS  15000.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         05  WRK-SMEI-HOSPID        PIC X(24).
               05  WRK-SMEI-HOSPNUM       PIC 9(02).
      *=== 4.0対応 By Takashima(2007/09/10) end
      *** (注) ソートキーの構成は日計表明細(ORCBD500)と異なる。
               05  WRK-SMEI-SORT-KEY. 
                   07  WRK-SMEI-NYUGAIKBN   PIC X(01).
                   07  WRK-SMEI-SRYKA       PIC X(02).
                   07  WRK-SMEI-PTID        PIC 9(10).
                   07  WRK-SMEI-DENPNUM     PIC 9(07).
                   07  WRK-SMEI-DENPEDANUM  PIC 9(02).
               05  WRK-SMEI-SKYMONEY        PIC S9(07).
               05  WRK-SMEI-NYUHEN-MONEY    PIC S9(07).
               05  WRK-SMEI-NYUHEN-YMD      PIC X(08).
      * 20040405 追加                     
               05  WRK-SMEI-PTINFCRE-YMD    PIC X(08).
      * 20040405 追加
      *
      *配列ソート時の退避エリア
           03  WRK-SV-SMEI-TBL            PIC X(76).
      *
           03  WRK-SV-SMEI-DENPNUM        PIC 9(07).
      *
       01   WRK-JYURRK-AREA.
           03  WRK-JR-DENPNUM-SV             PIC  9(07).
           03  WRK-JR-TBL           OCCURS  100. 
               05  WRK-JR-SORT-KEY.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *          07  WRK-JR-HOSPID         PIC  X(24).
                   07  WRK-JR-HOSPNUM        PIC  9(02).
      *=== 4.0対応 By Takashima(2007/09/10) end
                   07  WRK-JR-PTID           PIC  9(10).
                   07  WRK-JR-NYUGAIKBN      PIC  X(01).
                   07  WRK-JR-DENPNUM        PIC  9(07).
                   07  WRK-JR-SRYKA          PIC  X(02).
                   07  WRK-JR-SRYYMD         PIC  X(08).
                   07  WRK-JR-RENNUM         PIC  9(01).
                   07  WRK-JR-DOUJI-RENNUM   PIC  9(01).
                   07  WRK-JR-KAIKEI-RENNUM  PIC  9(03).
                   07  WRK-JR-EDANUM         PIC  9(01).
                   07  WRK-JR-LASTKBN        PIC  X(01). 
      *配列ソート時の退避エリア
           03  WRK-JR-TBL-SV              PIC  X(59).
      *
       01   WRK-JYUSHIN-AREA.
           03   WRK-JYUSHIN-PTID-TBL    OCCURS 50000. 
               05   WRK-JYUSHIN-PTID      PIC  9(10).
      *
       01  WRK-CONS-AREA.
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973". 
      * 20080325 H20年4月改正対応START tanaka
      *    公害保険番号 
           03  WRK-CONS-KOGAI-HKNNUM   PIC X(03)   VALUE   "975". 
      * 20080325 H20年4月改正対応END
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
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *--- 2008/12/04 ADD START ----------------------------------------
       01  JYURRK2-REC.
           COPY    "CPJYURRK.INC"      REPLACING  //JYURRK//
                                       BY         //JYURRK2//.
      *--- 2008/12/04 ADD END ------------------------------------------
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
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
                   UNTIL       FLG-END     =   1
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
           INITIALIZE                  SUM-AREA-GK
           INITIALIZE                  WRK-SMEI-REC
           INITIALIZE                  WRK-JYUSHIN-AREA
      *=== 4.0対応 By Takashima(2007/09/10) start
           INITIALIZE                  SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM     TO   SPA-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *    ステップ管理開始処理
           MOVE    "STS"                TO   SJOBKANRI-MODE
           INITIALIZE                   JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   JOB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-JOBID       TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID     TO   JOB-SHELLID
           MOVE    "ORCBD002"           TO   JOB-PGID
      ***     MOVE    "日計表（伝票発行日）"
      * 20031211 帳票タイトル変更 
      ***     MOVE    "日計表（合計）"  TO  JOB-SHELLMSG
           MOVE    "日計表合計（窓口領収金）"   TO  JOB-SHELLMSG
      * 20031211 帳票タイトル変更 
           CALL    "ORCSJOB"            USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    "RECEERR"       TO  RECEERR-FILE-ID
      *     MOVE    LNK-PRTKANRI-TERMID
      *                             TO  RECEERR-TERMID
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
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
           PERFORM   31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO   WRK-SYSYMDWH
      *
      *    対象年月日編集
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
           PERFORM   31012-SEIWA-HEN-SEC
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
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1001-EDYUKYMD
                                              SYS-1001-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1001-REC          TO   MCPDATA-REC
           PERFORM   800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI   =   ZERO
             MOVE   MCPDATA-REC          TO   SYS-1001-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    SYS-1001-HOSPID     TO  WRK-PARA-HOSPID
             MOVE   SYS-1001-HOSPNUM     TO   WRK-PARA-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           ELSE    
             MOVE    "医療機関ＩＤが取得できませんでした。"
                                         TO   WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    自費情報取得
      *    区分11
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "11"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO   WRK-TJIHIN(1)
             MOVE   1                    TO   WRK-TJIHIN-FLG(1)
           ELSE
             MOVE   SPACE                TO   WRK-TJIHIN(1)
           END-IF
      *  
      *    区分12
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "12"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC          TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO   WRK-TJIHIN(2)
             MOVE   1                    TO   WRK-TJIHIN-FLG(2)
           ELSE
             MOVE  SPACE                 TO   WRK-TJIHIN(2)
           END-IF
      *  
      *    区分13
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "13"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2       =   ZERO
             MOVE   MCPDATA-REC           TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME     TO   WRK-TJIHIN(3)
             MOVE   1                     TO   WRK-TJIHIN-FLG(3)
           ELSE
             MOVE   SPACE                 TO   WRK-TJIHIN(3)
           END-IF
      *
      *    区分14
           MOVE    SPACE                  TO   SYS-1013-REC
           INITIALIZE                          SYS-1013-REC
           MOVE    "1013"                 TO   SYS-1013-KANRICD
           MOVE    "14"                   TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM            TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD    TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD    TO   SYS-1013-EDYUKYMD
                                               SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC           TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC           TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME     TO   WRK-TJIHIN(4)
             MOVE   1                     TO   WRK-TJIHIN-FLG(4)
           ELSE
             MOVE   SPACE                 TO   WRK-TJIHIN(4)
           END-IF
      *
      *    区分15
           MOVE    SPACE                  TO   SYS-1013-REC
           INITIALIZE                          SYS-1013-REC
           MOVE    "1013"                 TO   SYS-1013-KANRICD
           MOVE    "15"                   TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM            TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD    TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD    TO   SYS-1013-EDYUKYMD
                                               SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC           TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC           TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME     TO   WRK-TJIHIN(5)
             MOVE   1                     TO   WRK-TJIHIN-FLG(5)
           ELSE
             MOVE  SPACE                  TO   WRK-TJIHIN(5)
           END-IF
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *
      *    区分16
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "16"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF    FLG-SYSKANRI2   =   ZERO
             MOVE  MCPDATA-REC           TO   SYS-1013-REC
             MOVE  SYS-1013-JIHINAME     TO   WRK-TJIHIN(6)
             MOVE  1                     TO   WRK-TJIHIN-FLG(6)
           ELSE
             MOVE  SPACE                 TO   WRK-TJIHIN(6)
           END-IF
      *
      *    区分17
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "17"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
            MOVE   WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
            MOVE   WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC          TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO   WRK-TJIHIN(7)
             MOVE   1                    TO   WRK-TJIHIN-FLG(7)
           ELSE
             MOVE   SPACE                TO   WRK-TJIHIN(7)
           END-IF
      *
      *    区分18
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "18"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC          TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO   WRK-TJIHIN(8)
             MOVE   1                    TO   WRK-TJIHIN-FLG(8)
           ELSE
               MOVE   SPACE              TO   WRK-TJIHIN(8)
           END-IF
      *
      *    区分19
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "19"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
           MOVE  WRK-PARA-DENPPRTYMD     TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI2   =   ZERO
             MOVE  MCPDATA-REC           TO   SYS-1013-REC
             MOVE  SYS-1013-JIHINAME     TO   WRK-TJIHIN(9)
             MOVE  1                     TO   WRK-TJIHIN-FLG(9)
           ELSE
             MOVE  SPACE                 TO   WRK-TJIHIN(9)
           END-IF
      *
      *    区分10
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "10"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                               SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =   ZERO
             MOVE   MCPDATA-REC          TO  SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO  WRK-TJIHIN(10)
             MOVE   1                    TO  WRK-TJIHIN-FLG(10)
           ELSE
             MOVE   SPACE                TO  WRK-TJIHIN(10)
           END-IF
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
      *  
      *    自費情報取得
      *    区分21
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "21"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO    WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC          TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO   WRK-TJIHI(1)
             MOVE   1                    TO   WRK-TJIHI-FLG(1)
           ELSE
             MOVE   SPACE                TO   WRK-TJIHI(1)
           END-IF
      *
      *    区分22
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "22"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC          TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO   WRK-TJIHI(2)
             MOVE   1                    TO   WRK-TJIHI-FLG(2)
           ELSE
             MOVE   SPACE                TO   WRK-TJIHI(2)
           END-IF
      *  
      *    区分23
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "23"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC          TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO   WRK-TJIHI(3)
             MOVE   1                    TO   WRK-TJIHI-FLG(3)
           ELSE
             MOVE   SPACE                TO   WRK-TJIHI(3)
           END-IF
      *  
      *    区分24
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "24"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC          TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO   WRK-TJIHI(4)
             MOVE   1                    TO   WRK-TJIHI-FLG(4)
           ELSE
             MOVE   SPACE                TO   WRK-TJIHI(4)
           END-IF
      *
      *    区分25
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "25"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC          TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO   WRK-TJIHI(5)
             MOVE   1                    TO   WRK-TJIHI-FLG(5)
           ELSE
             MOVE   SPACE                TO   WRK-TJIHI(5)
           END-IF
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *
      *    区分26
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "26"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
           MOVE  WRK-PARA-DENPPRTYMD     TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC           TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME     TO   WRK-TJIHI(6)
             MOVE   1                     TO   WRK-TJIHI-FLG(6)
           ELSE
             MOVE   SPACE               TO   WRK-TJIHI(6)
           END-IF
      *
      *    区分27
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "27"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
           MOVE  WRK-PARA-DENPPRTYMD     TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC          TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO   WRK-TJIHI(7)
             MOVE   1                    TO   WRK-TJIHI-FLG(7)
           ELSE
             MOVE   SPACE                TO   WRK-TJIHI(7)
           END-IF
      *
      *    区分28
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "28"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC          TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO   WRK-TJIHI(8)
             MOVE   1                    TO   WRK-TJIHI-FLG(8)
           ELSE
             MOVE   SPACE                 TO   WRK-TJIHI(8)
           END-IF
      *
      *    区分29
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "29"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF    FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC          TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO   WRK-TJIHI(9)
             MOVE   1                    TO   WRK-TJIHI-FLG(9)
           ELSE
             MOVE  SPACE                 TO   WRK-TJIHI(9)
           END-IF
      *
      *    区分20
           MOVE    SPACE                 TO   SYS-1013-REC
           INITIALIZE                         SYS-1013-REC
           MOVE    "1013"                TO   SYS-1013-KANRICD
           MOVE    "20"                  TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-1013-EDYUKYMD
                                              SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC          TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC          TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME    TO   WRK-TJIHI(10)
             MOVE   1                    TO   WRK-TJIHI-FLG(10)
           ELSE
             MOVE   SPACE                TO   WRK-TJIHI(10)
           END-IF
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
      *  
      *    労災医療機関情報
           MOVE    SPACE                 TO   SYS-4001-REC
           INITIALIZE                         SYS-4001-REC
           MOVE    "4001"                TO   SYS-4001-KANRICD
           MOVE    "*"                   TO   SYS-4001-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-4001-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD   TO   SYS-4001-EDYUKYMD
                                              SYS-4001-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-4001-REC          TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE  MCPDATA-REC    TO   SYS-4001-REC
             EVALUATE  SYS-4001-TENKBN
               WHEN  "1"
                 MOVE    11.5     TO   WRK-4001-TENTANKA
               WHEN  "2"
                 MOVE    12       TO   WRK-4001-TENTANKA
               WHEN  OTHER
                 MOVE    10       TO   WRK-4001-TENTANKA
             END-EVALUATE
           ELSE
             MOVE  10   TO  WRK-4001-TENTANKA
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
           PERFORM  UNTIL   FLG-END  =  1
      *    収納情報集計処理
             PERFORM 320-BODY-HEN-SEC
           END-PERFORM
      *
      *    帳票編集<診療科合計ページ>印刷処理
      * 20030620 変更
      ***     IF  FLG-GET = 1
      ***         PERFORM 330-TERM-KA-PRINT-SEC
      ***     END-IF
           EVALUATE WRK-NYUGAIKBN
             WHEN "1"
               IF  WRK-PRNSRKA-CNT  >  1
                 PERFORM   330-TERM-KA-PRINT-SEC
               END-IF
             WHEN "2"
               IF  WRK-PRGSRKA-CNT  >  1
                 PERFORM   330-TERM-KA-PRINT-SEC
               END-IF
           END-EVALUATE
      * 20030620 変更
      *
      *    帳票編集<合計ページ>印刷処理
           PERFORM 330-TERM-GK-PRINT-SEC
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
           MOVE    0     TO   IDX-S
      *
      *******************************************
      ***  20031014  @@@@-MOTOHASI            ***
      ***  入院患者を取得(入外区分:1)ここから ***
      *******************************************
           PERFORM 910-DBSELECT-SEC
      *
      *    収納明細テーブルより変数配列に対象データを抽出する。
      *
           PERFORM  UNTIL   FLG-SYUMEI  =  1
      *    入力された日付と入返金日が一致しているかをチェック
      * 
      * 20030704 追加 
             PERFORM   900-SYUNOU-READ-SEC
             INITIALIZE   PTINF-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *      MOVE SMEI-HOSPID TO PTINF-HOSPID
             MOVE    SMEI-HOSPNUM       TO   PTINF-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
             MOVE    SMEI-PTID          TO   PTINF-PTID
             PERFORM   900-PTINF-READ-SEC
      * 20030704 追加
      *
      ***  20030704 収納明細情報抽出条件変更
      *    収納情報にレコードが存在しないデータは抽出しない。
      *    伝票状態区分 = 3(請求取消)は対象外データとして取り扱う
      *    患者情報にレコードが存在しないデータは抽出しない。
      *    テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う
             IF  ( WRK-PARA-DENPPRTYMD   =   SMEI-NYUHEN-YMD
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         AND WRK-PARA-HOSPID      =      SMEI-HOSPID )
                 AND WRK-PARA-HOSPNUM   =   SMEI-HOSPNUM )
      *=== 4.0対応 By Takashima(2007/09/10) end
                 AND  FLG-SYUNOU   =  ZERO
                 AND  SYU-DENPJTIKBN  NOT =  "3"
                 AND  FLG-PTINF   =   ZERO
                 AND  PTINF-TSTPTNUMKBN  NOT =  "1"
      ***  20030704 収納明細情報抽出条件変更
      * 
               ADD    1           TO    IDX-S
      *=== 4.0対応 By Takashima(2007/09/10) start
      *              MOVE   SMEI-HOSPID 
      *                   TO    WRK-SMEI-HOSPID(IDX-S)
               MOVE   SMEI-HOSPNUM     TO    WRK-SMEI-HOSPNUM(IDX-S)
      *=== 4.0対応 By Takashima(2007/09/10) end
               MOVE   SMEI-NYUGAIKBN   TO   WRK-SMEI-NYUGAIKBN(IDX-S)
               MOVE   SMEI-SRYKA       TO   WRK-SMEI-SRYKA(IDX-S)
               MOVE   SMEI-PTID        TO   WRK-SMEI-PTID(IDX-S)
               MOVE   SMEI-DENPNUM     TO   WRK-SMEI-DENPNUM(IDX-S)
               MOVE   SMEI-DENPEDANUM  TO   WRK-SMEI-DENPEDANUM(IDX-S)
               MOVE   SMEI-SKYMONEY    TO   WRK-SMEI-SKYMONEY(IDX-S)
               MOVE   SMEI-NYUHEN-MONEY
                                       TO   WRK-SMEI-NYUHEN-MONEY(IDX-S)
               MOVE   SMEI-NYUHEN-YMD
                                       TO   WRK-SMEI-NYUHEN-YMD(IDX-S)
      ***  20040405 患者情報作成日付追加
               MOVE   PTINF-CREYMD     TO   WRK-SMEI-PTINFCRE-YMD(IDX-S)
      ***  20040405 患者情報作成日付追加
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
      *    入力された日付と入返金日が一致しているかをチェック
      * 
      * 20030704 追加 
             PERFORM 900-SYUNOU-READ-SEC
             INITIALIZE   PTINF-REC 
      *=== 4.0対応 By Takashima(2007/09/10) start
      *      MOVE SMEI-HOSPID TO PTINF-HOSPID
             MOVE   SMEI-HOSPNUM      TO   PTINF-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
             MOVE   SMEI-PTID         TO   PTINF-PTID
             PERFORM   900-PTINF-READ-SEC
      * 20030704 追加
      *
      ***  20030704 収納明細情報抽出条件変更
      *    収納情報にレコードが存在しないデータは抽出しない。
      *    伝票状態区分 = 3(請求取消)は対象外データとして取り扱う
      *    患者情報にレコードが存在しないデータは抽出しない。
      *    テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う
             IF  ( WRK-PARA-DENPPRTYMD  =  SMEI-NYUHEN-YMD
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         AND WRK-PARA-HOSPID      =      SMEI-HOSPID )
                 AND WRK-PARA-HOSPNUM   =  SMEI-HOSPNUM )
      *=== 4.0対応 By Takashima(2007/09/10) end
                 AND  FLG-SYUNOU   =  ZERO
                 AND  SYU-DENPJTIKBN  NOT =  "3"
                 AND  FLG-PTINF   =  ZERO
                 AND  PTINF-TSTPTNUMKBN  NOT =  "1"
      ***  20030704 収納明細情報抽出条件変更
      *
               ADD     1               TO   IDX-S
      *=== 4.0対応 By Takashima(2007/09/10) start
      *              MOVE   SMEI-HOSPID 
      *                   TO    WRK-SMEI-HOSPID(IDX-S)
               MOVE   SMEI-HOSPNUM     TO   WRK-SMEI-HOSPNUM(IDX-S)
      *=== 4.0対応 By Takashima(2007/09/10) end
               MOVE   SMEI-NYUGAIKBN   TO   WRK-SMEI-NYUGAIKBN(IDX-S)
               MOVE   SMEI-SRYKA       TO   WRK-SMEI-SRYKA(IDX-S)
               MOVE   SMEI-PTID        TO   WRK-SMEI-PTID(IDX-S)
               MOVE   SMEI-DENPNUM     TO   WRK-SMEI-DENPNUM(IDX-S)
               MOVE   SMEI-DENPEDANUM  TO   WRK-SMEI-DENPEDANUM(IDX-S)
               MOVE   SMEI-SKYMONEY    TO   WRK-SMEI-SKYMONEY(IDX-S)
               MOVE   SMEI-NYUHEN-MONEY 
                                       TO   WRK-SMEI-NYUHEN-MONEY(IDX-S)
               MOVE   SMEI-NYUHEN-YMD  TO   WRK-SMEI-NYUHEN-YMD(IDX-S)
      ***  20040405 患者情報作成日付追加
               MOVE   PTINF-CREYMD     TO   WRK-SMEI-PTINFCRE-YMD(IDX-S)
      ***  20040405 患者情報作成日付追加
             END-IF
             PERFORM   920-DBFETCH-SEC
           END-PERFORM
      *******************************************
      ***  20031014  @@@@-MOTOHASI            ***
      ***  外来患者を取得(入外区分:2)ここまで ***
      *******************************************

      *
      *    配列の最終レコード番号を設定
           MOVE     IDX-S         TO   IDX-MAX
      *
           IF   IDX-MAX   >  1
      *    対象データが１件以上存在する場合は抽出フラグON
             MOVE    1            TO   FLG-GET
      *    対象データが１件以上存在する場合は終了フラグOFF
             MOVE    ZERO         TO   FLG-END
      *    配列に１件以上のデータが抽出されている場合はソーティング処理を行う。
      *    変数配列によるソーティング
      *    最小値より並び替えをする為にインデックスの設定
             MOVE    1            TO   IDX-S
             MOVE    2            TO   IDX-N
             PERFORM  UNTIL  IDX-S   NOT <   IDX-MAX
               PERFORM  UNTIL  IDX-N  >  IDX-MAX
                 IF  WRK-SMEI-SORT-KEY(IDX-S)
                     >  WRK-SMEI-SORT-KEY(IDX-N) 
                   MOVE   WRK-SMEI-TBL(IDX-N)  TO   WRK-SV-SMEI-TBL
                   MOVE   WRK-SMEI-TBL(IDX-S)  TO   WRK-SMEI-TBL(IDX-N)
                   MOVE   WRK-SV-SMEI-TBL      TO   WRK-SMEI-TBL(IDX-S)
                 END-IF
                 ADD    1         TO   IDX-N
               END-PERFORM
      *    N番目の値を決定する為にインデックスの再設定
               ADD    1           TO   IDX-S
               COMPUTE   IDX-N   =   IDX-S   +   1
             END-PERFORM
           ELSE
             IF   IDX-MAX   =   ZERO
      *    対象データが１件も存在しない場合は抽出フラグOFF
               MOVE   ZERO        TO   FLG-GET
      *    対象データが１件も存在しない場合は終了フラグON
               MOVE   1           TO   FLG-END
             ELSE 
      *    対象データが１件でも存在する場合は抽出フラグオフON
               MOVE   1           TO   FLG-GET
      *    対象データが１件でも存在する場合は終了フラグOFF
               MOVE   ZERO        TO   FLG-END
             END-IF
           END-IF
      *
           PERFORM   910-DBCLOSE-SEC
      *    配列用員インデックスの再設定
           MOVE     1             TO  IDX-S
      *
           .
       310-DATA-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<合計カウント>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           MOVE    ZERO     TO   CNT-LINE
      * 20030407 追加
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO   WRK-NYUGAIKBN
           MOVE    WRK-SMEI-SRYKA(IDX-S)       TO   WRK-SRYKA
      * 20030407 追加
      *
      * 20030620 追加
           EVALUATE WRK-NYUGAIKBN
             WHEN "1"
      *    入院診療科数カウントアップ
               MOVE   1        TO   WRK-PRNSRKA-CNT
             WHEN "2"
      *    外来診療科数カウントアップ
               MOVE   1        TO   WRK-PRGSRKA-CNT
           END-EVALUATE
      * 20030620 追加
      *
           PERFORM  UNTIL   FLG-END   =   1
      ***   件数用フラグの初期化
             INITIALIZE   FLG-KENSUU
      *
      ***   収納テーブル再読み込み
             PERFORM   901-SYUNOU-READ-SEC
             IF    FLG-SYUNOU  =  ZERO
      *
      *     入外区分ブレイクチェック処理
               IF  WRK-SMEI-NYUGAIKBN(IDX-S)  NOT =  WRK-NYUGAIKBN
      *      合計カウンターに加算
                 ADD   SUM-HKNTEN          TO   SUM-HKNTEN-GK
                 ADD   SUM-SHKNKEN         TO   SUM-SHKNKEN-GK
                 ADD   SUM-RHKNKEN         TO   SUM-RHKNKEN-GK
      * 20050914 MOTO
                 ADD   SUM-OHKNKEN         TO   SUM-OHKNKEN-GK
      * 20050914 MOTO
                 ADD   SUM-SAHKNKEN        TO   SUM-SAHKNKEN-GK
      * 20040405 追加
                 PERFORM  VARYING  IDX2   FROM  1  BY 1
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *                    UNTIL        IDX2   >     14 
                          UNTIL        IDX2   >     20
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
                   ADD   SUM-MONEY(IDX2)   TO   SUM-MONEY-GK(IDX2)
                   ADD   SUM-RKEN(IDX2)    TO   SUM-RKEN-GK(IDX2)
                   ADD   SUM-SKEN(IDX2)    TO   SUM-SKEN-GK(IDX2)
      * 20050914 MOTO
                   ADD   SUM-OKEN(IDX2)    TO   SUM-OKEN-GK(IDX2)
      * 20050914 MOTO
      * 20040405 追加
                   ADD   SUM-SAKEN(IDX2)   TO   SUM-SAKEN-GK(IDX2)
      * 20040405 追加
                 END-PERFORM
                 ADD   SUM-SYUEKI          TO   SUM-SYUEKI-GK
                 ADD   SUM-SEIKYU          TO   SUM-SEIKYU-GK
                 ADD   SUM-RYOSYU          TO   SUM-RYOSYU-GK
                 ADD   SUM-SSYUEKIKEN      TO   SUM-SSYUEKIKEN-GK
                 ADD   SUM-SSEIKYUKEN      TO   SUM-SSEIKYUKEN-GK
                 ADD   SUM-SRYOSYUKEN      TO   SUM-SRYOSYUKEN-GK
                 ADD   SUM-RSYUEKIKEN      TO   SUM-RSYUEKIKEN-GK
                 ADD   SUM-RSEIKYUKEN      TO   SUM-RSEIKYUKEN-GK
                 ADD   SUM-RRYOSYUKEN      TO   SUM-RRYOSYUKEN-GK
      * 20050914 MOTO
                 ADD   SUM-OSYUEKIKEN      TO   SUM-OSYUEKIKEN-GK
                 ADD   SUM-OSEIKYUKEN      TO   SUM-OSEIKYUKEN-GK
                 ADD   SUM-ORYOSYUKEN      TO   SUM-ORYOSYUKEN-GK
      * 20050914 MOTO
      * 20040405 追加
                 ADD   SUM-SASYUEKIKEN     TO   SUM-SASYUEKIKEN-GK
                 ADD   SUM-SASEIKYUKEN     TO   SUM-SASEIKYUKEN-GK
                 ADD   SUM-SARYOSYUKEN     TO   SUM-SARYOSYUKEN-GK
      * 20040405 追加
      * 20030620 変更
      ***       入院の印刷診療科数が２科以上の場合に科別集計頁を印字する。
      ***          PERFORM 330-TERM-KA-PRINT-SEC
                 IF   WRK-NYUGAIKBN   =  "1"
                     AND WRK-PRNSRKA-CNT   >  1
                   PERFORM 330-TERM-KA-PRINT-SEC
                 END-IF
      * 20030620 変更
                 PERFORM 330-TERM-GK-PRINT-SEC
                 INITIALIZE           SUM-AREA-GK
                 INITIALIZE           SUM-AREA
      *
      * ブレイクキー項目設定
                 MOVE   WRK-SMEI-NYUGAIKBN(IDX-S)   TO   WRK-NYUGAIKBN
                 MOVE   WRK-SMEI-SRYKA(IDX-S)       TO   WRK-SRYKA
      *
      * 20030530 追加
      *    PTIDブレイクキー項目の設定
                 INITIALIZE           WRK-PTID
      * 20030530 追加
      *
      * 20030620 追加
                 EVALUATE WRK-NYUGAIKBN
                   WHEN "1"
      *** 入院診療科数カウントアップ
                     ADD     1    TO   WRK-PRNSRKA-CNT
                   WHEN "2"
      *** 外来診療科数カウントアップ
                     ADD     1    TO   WRK-PRGSRKA-CNT
                 END-EVALUATE
      * 20030620 追加
      *
               END-IF
      *
      *    診療科ブレイクチェック処理
               IF   WRK-SMEI-SRYKA(IDX-S)  NOT =  WRK-SRYKA
                 PERFORM 330-TERM-KA-PRINT-SEC
      *      合計カウンターに加算
                 ADD   SUM-HKNTEN          TO   SUM-HKNTEN-GK
                 ADD   SUM-SHKNKEN         TO   SUM-SHKNKEN-GK
                 ADD   SUM-RHKNKEN         TO   SUM-RHKNKEN-GK
      * 20050914 MOTO
                 ADD   SUM-OHKNKEN         TO   SUM-OHKNKEN-GK
      * 20050914 MOTO
      * 20040405 追加
                 ADD   SUM-SAHKNKEN        TO   SUM-SAHKNKEN-GK
      * 20040405 追加
                 PERFORM  VARYING   IDX2   FROM 1  BY 1
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *                  UNTIL        IDX2   >     14 
                          UNTIL        IDX2   >  20 
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
                   ADD   SUM-MONEY(IDX2)   TO   SUM-MONEY-GK(IDX2)
                   ADD   SUM-RKEN(IDX2)    TO   SUM-RKEN-GK(IDX2)
                   ADD   SUM-SKEN(IDX2)    TO   SUM-SKEN-GK(IDX2)
      * 20050914 MOTO
                   ADD   SUM-OKEN(IDX2)    TO   SUM-OKEN-GK(IDX2)
      * 20050914 MOTO
      * 20040405 追加
                   ADD   SUM-SAKEN(IDX2)   TO   SUM-SAKEN-GK(IDX2)
      * 20040405 追加
                 END-PERFORM
                 ADD   SUM-SYUEKI            TO   SUM-SYUEKI-GK
                 ADD   SUM-SEIKYU            TO   SUM-SEIKYU-GK
                 ADD   SUM-RYOSYU            TO   SUM-RYOSYU-GK
                 ADD   SUM-SSYUEKIKEN        TO   SUM-SSYUEKIKEN-GK
                 ADD   SUM-SSEIKYUKEN        TO   SUM-SSEIKYUKEN-GK
                 ADD   SUM-SRYOSYUKEN        TO   SUM-SRYOSYUKEN-GK
                 ADD   SUM-RSYUEKIKEN        TO   SUM-RSYUEKIKEN-GK
                 ADD   SUM-RSEIKYUKEN        TO   SUM-RSEIKYUKEN-GK
                 ADD   SUM-RRYOSYUKEN        TO   SUM-RRYOSYUKEN-GK
      * 20050914 MOTO
                 ADD   SUM-OSYUEKIKEN        TO   SUM-OSYUEKIKEN-GK
                 ADD   SUM-OSEIKYUKEN        TO   SUM-OSEIKYUKEN-GK
                 ADD   SUM-ORYOSYUKEN        TO   SUM-ORYOSYUKEN-GK
      * 20050914 MOTO
      * 20040405 追加
                 ADD   SUM-SASYUEKIKEN       TO   SUM-SASYUEKIKEN-GK
                 ADD   SUM-SASEIKYUKEN       TO   SUM-SASEIKYUKEN-GK
                 ADD   SUM-SARYOSYUKEN       TO   SUM-SARYOSYUKEN-GK
      * 20040405 追加
                 INITIALIZE                  SUM-AREA
      *
      * ブレイクキー項目設定
                 MOVE   WRK-SMEI-NYUGAIKBN(IDX-S)  TO   WRK-NYUGAIKBN
                 MOVE   WRK-SMEI-SRYKA(IDX-S)      TO   WRK-SRYKA
      *
      * 20030530 追加
      *    PTIDブレイクキー項目の設定
                 INITIALIZE    WRK-PTID
      * 20030530 追加
      * 20030620 追加
                 EVALUATE  WRK-NYUGAIKBN
                   WHEN "1"
      *** 入院診療科数カウントアップ  
                     ADD   1    TO   WRK-PRNSRKA-CNT
                   WHEN "2"
      *** 外来診療科数カウントアップ  
                     ADD   1    TO   WRK-PRGSRKA-CNT
                 END-EVALUATE
      * 20030620 追加
      *
               END-IF
      * 20030407 追加
      *
      ***   伝票番号枝番が１の場合は保険点数、保険別請求額の明細を印字(開始)
               IF   WRK-SMEI-DENPEDANUM(IDX-S)   =   1
      ***           PERFORM 900-SYUNOU-READ-SEC
                 IF  FLG-SYUNOU  = ZERO
      * 20080325 H20年4月改正対応START tanaka
      *             PERFORM 320-BODY-HEN-SUB-SEC
                   IF WRK-PARA-DENPPRTYMD   <   "20080401"
                     PERFORM   320-BODY-HEN-SUB-SEC
                   ELSE
                     PERFORM   330-BODY-HEN-SUB-SEC
                   END-IF
      * 20080325 H20年4月改正対応END
      *  受診履歴集計
                   IF SYU-NYUGAIKBN = "1"
      ***        入院の場合は初診、再診別に件数を集計する。
                     PERFORM   320-CNT-SEC
                   ELSE
      ***        外来の場合は初診、再診別に件数を集計する。
                     PERFORM   900-JYURRK-READ-SEC
      * 20040405 追加
                     MOVE   WRK-SMEI-PTINFCRE-YMD(IDX-S) TO   WRK-CREYMD
      * 20040405 追加
                     PERFORM   320-JYURRK-CNT-SEC
                   END-IF
                 END-IF
               END-IF
      *
      *    請求金額
               INITIALIZE  WRK-SZ
               MOVE   WRK-SMEI-SKYMONEY(IDX-S) TO WRK-SZ
               IF  WRK-SMEI-SKYMONEY(IDX-S)  NOT =  0
                 ADD   WRK-SMEI-SKYMONEY(IDX-S)    TO   SUM-SEIKYU
      ***         ADD       1    TO   SUM-SEIKYUKEN
                 MOVE      1    TO   FLG-SEIKYU
               END-IF
      *    領収金額
               INITIALIZE  WRK-SZ
               MOVE   WRK-SMEI-NYUHEN-MONEY(IDX-S)   TO   WRK-SZ
               IF   WRK-SMEI-NYUHEN-MONEY(IDX-S)  NOT =  0
                 ADD WRK-SMEI-NYUHEN-MONEY(IDX-S)   TO   SUM-RYOSYU
      ***         ADD         1    TO   SUM-RYOSYUKEN
                 MOVE      1      TO   FLG-RYOSYU
               END-IF
      *
      *
      *  受診履歴集計
      ***        IF   WRK-SV-SMEI-DENPNUM   NOT =
      ***             WRK-SMEI-DENPNUM(IDX-S)
      ***         AND SYU-SRYYMD = WRK-PARA-DENPPRTYMD
      ***         AND WRK-SMEI-DENPEDANUM  = 1   
      ***            PERFORM 900-JYURRK-READ-SEC
      ***            PERFORM 320-JYURRK-CNT-SEC
      ***            MOVE  WRK-SMEI-DENPNUM(IDX-S)
      ***                  TO WRK-SV-SMEI-DENPNUM
      ***        END-IF 
      ***      PERFORM 900-JYURRK-READ-SEC
      ***      PERFORM 320-JYURRK-CNT-SEC
      *
             END-IF
      ***     収納テーブル収納テーブル再読み込み
      *
      * 20030530 追加
      *    PTIDブレイクキー項目の設定
      ***       MOVE WRK-SMEI-PTID(IDX-S)  TO  WRK-PTID
      * 20030530 追加
      *
      *    配列インデックスのカウントアップ
             ADD    1        TO    IDX-S
      *    配列のデータを全件出力した場合は合計表の印刷を行う為にループより脱出
             IF IDX-S   >   IDX-MAX
               MOVE    1     TO    FLG-END
             END-IF 
      *
           END-PERFORM
      *
      *      合計カウンターに加算
           ADD   SUM-HKNTEN      TO   SUM-HKNTEN-GK
           ADD   SUM-SHKNKEN     TO   SUM-SHKNKEN-GK
           ADD   SUM-RHKNKEN     TO   SUM-RHKNKEN-GK
      * 20050914 MOTO
           ADD   SUM-OHKNKEN     TO   SUM-OHKNKEN-GK
      * 20050914 MOTO
      * 20040405 追加
           ADD   SUM-SAHKNKEN    TO   SUM-SAHKNKEN-GK
      * 20040405 追加
           PERFORM  VARYING  IDX2   FROM  1   BY 1
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *             UNTIL        IDX2   >     14 
                    UNTIL  IDX2   >  20 
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
             ADD   SUM-MONEY(IDX2)     TO   SUM-MONEY-GK(IDX2)
             ADD   SUM-RKEN(IDX2)      TO   SUM-RKEN-GK(IDX2)
             ADD   SUM-SKEN(IDX2)      TO   SUM-SKEN-GK(IDX2)
      * 20050914 MOTO
             ADD   SUM-OKEN(IDX2)      TO   SUM-OKEN-GK(IDX2)
      * 20050914 MOTO
      * 20040405 追加
             ADD   SUM-SAKEN(IDX2)     TO   SUM-SAKEN-GK(IDX2)
      * 20040405 追加
           END-PERFORM
           ADD   SUM-SYUEKI            TO   SUM-SYUEKI-GK
           ADD   SUM-SEIKYU            TO   SUM-SEIKYU-GK
           ADD   SUM-RYOSYU            TO   SUM-RYOSYU-GK
           ADD   SUM-SSYUEKIKEN        TO   SUM-SSYUEKIKEN-GK
           ADD   SUM-SSEIKYUKEN        TO   SUM-SSEIKYUKEN-GK
           ADD   SUM-SRYOSYUKEN        TO   SUM-SRYOSYUKEN-GK
           ADD   SUM-RSYUEKIKEN        TO   SUM-RSYUEKIKEN-GK
           ADD   SUM-RSEIKYUKEN        TO   SUM-RSEIKYUKEN-GK
           ADD   SUM-RRYOSYUKEN        TO   SUM-RRYOSYUKEN-GK
      * 20050914 MOTO
           ADD   SUM-OSYUEKIKEN        TO   SUM-OSYUEKIKEN-GK
           ADD   SUM-OSEIKYUKEN        TO   SUM-OSEIKYUKEN-GK
           ADD   SUM-ORYOSYUKEN        TO   SUM-ORYOSYUKEN-GK
      * 20050914 MOTO
      * 20040405 追加
           ADD   SUM-SASYUEKIKEN       TO   SUM-SASYUEKIKEN-GK
           ADD   SUM-SASEIKYUKEN       TO   SUM-SASEIKYUKEN-GK
           ADD   SUM-SARYOSYUKEN       TO   SUM-SARYOSYUKEN-GK
      * 20040405 追加
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<合計カウント>サブ処理
      *****************************************************************
       320-BODY-HEN-SUB-SEC      SECTION.
      *
      *
      *    保険番号情報読み込み
           PERFORM 900-HKNNUM-READ-SEC
           IF   FLG-HKNNUM  =   1
      *    該当レコードが存在しない場合は単価を10円に設定する。
             MOVE    10      TO   HKN-TENTNK
           END-IF
      *
      *    保険点数
           MOVE    ZERO      TO   SYU-TOTAL-HKNTEN
           EVALUATE      SYU-SYUHKNNUM
      ***   20040202  労災／自賠責の場合に保険点数を印字する。
      ***             WHEN    WRK-CONS-JIBAI-HKNNUM
      ***             WHEN    WRK-CONS-ROUSAI-HKNNUM
      ***                 CONTINUE
      ***   20040202  労災／自賠責の場合に保険点数を印字する。
             WHEN    OTHER    
               PERFORM  VARYING IDZ   FROM  1   BY  1
      *=== 自費、基本診療項目追加対応 ADD By Takashima(2006/10/24) start
      *                         UNTIL   IDZ >       11
      * 20080325 H20年4月改正対応START tanaka
      *                         UNTIL   IDZ >       15
                        UNTIL   IDZ   >  16
      * 20080325 H20年4月改正対応END
      *=== 自費、基本診療項目追加対応 ADD By Takashima(2006/10/24) end
                 ADD     SYU-HKNTEN (IDZ)
                                               TO   SYU-TOTAL-HKNTEN
               END-PERFORM
               MOVE    SYU-TOTAL-HKNTEN      TO   WRK-Z9
               ADD     SYU-TOTAL-HKNTEN      TO   SUM-HKNTEN
               MOVE  1                       TO   FLG-HKN
           END-EVALUATE        
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                    TO   IDX 
           EVALUATE  SYU-SYUHKNNUM
             WHEN  SPACE
               MOVE    6                   TO   IDX
      * 20080325 H20年4月改正対応START tanaka
             WHEN  WRK-CONS-KOGAI-HKNNUM
               MOVE    20                  TO   IDX
      * 20080325 H20年4月改正対応END
             WHEN  WRK-CONS-JIBAI-HKNNUM
      * 自費エリア拡張に伴い修正 20021204
      ***                MOVE    9                   TO  IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *                MOVE    14                   TO  IDX
               MOVE    19                  TO   IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
             WHEN    WRK-CONS-ROUSAI-HKNNUM    
      * 自費エリア拡張に伴い修正 20021204
      ***                 MOVE    8                   TO  IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *                MOVE    13                   TO  IDX
               MOVE    18                  TO   IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
             WHEN    WRK-CONS-JIHI-HKNNUM    
      * 自費エリア拡張に伴い修正 20021204
      ***                 MOVE    7                   TO  IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *                 MOVE    12                   TO  IDX
               MOVE    17                  TO   IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
             WHEN    "060"
             WHEN    "067"
      *=== 特別療養費対応 ADD By Uchida(2006/12/22) start
             WHEN    "068"
             WHEN    "069"
      *=== 特別療養費対応 ADD By Uchida(2006/12/22) end
               IF   SYU-KOH1HKNNUM   =   "027" 
                 MOVE    5                 TO   IDX
               ELSE
                 MOVE    4                 TO   IDX
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
           IF   SYU-SYUHKNNUM(1:2) = "98"
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *              MOVE    12    TO      IDX
             MOVE      17                TO   IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
           END-IF
      * 20031111追加
      *
           IF   IDX   >   ZERO
      *
      *            自費以外のとき自費分だけ集計
      * 自費エリア拡張に伴い修正 20021204
      ***             IF      IDX             =   7
      * 20040421追加  自費金額算出用変数の初期化(バグ対応)
             INITIALIZE WRK-JIHI
             INITIALIZE WRK-JIHI3
      * 20040421追加  自費金額算出用変数の初期化(バグ対応)
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *             IF      IDX             =   12
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) start
      *       IF   IDX   =   17
      **=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *         MOVE    ZERO            TO   WRK-JIHI
      ** 20040421追加  自費金額算出用変数の初期化(バグ対応)
      *         MOVE    ZERO            TO   WRK-JIHI3  
      ** 20040421追加  自費金額算出用変数の初期化(バグ対応)
      *       ELSE
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) end
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) start
               IF  SYU-SRYYMD  <=  "20070331"
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) end
                 COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                     +   SYU-TOTAL-TGMONEY-TAX
                                     +   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
                                     +   SYU-JIHI-TAX
      *
                 COMPUTE WRK-JIHI3   =   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
                                     +   SYU-JIHI-TAX
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) start
               ELSE
                 COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                     +   SYU-TOTAL-TGMONEY-TAX
                                     +   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
      *
                 COMPUTE WRK-JIHI3   =   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
               END-IF
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) start
      *       END-IF
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) end
      *
      *** 20030416 変更  
      ***             COMPUTE WRK-MONEY       =   SYU-SKYMONEY
      ***                                     -   WRK-JIHI
             COMPUTE WRK-MONEY   =   SYU-SKYMONEY   -   WRK-JIHI3
      *** 20030416 変更  
      *              
             MOVE    WRK-MONEY     TO   WRK-Z9
             ADD     WRK-MONEY     TO   SUM-MONEY(IDX)
      ***             ADD     1         TO  SUM-KEN (IDX)
      * 自費エリア拡張に伴い修正 20021204
      ***             IF      IDX             =   7
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *             IF      IDX             =   12
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) start
      *       IF   IDX  =   17
      **=== 自費追加対応 ADD By Takashima(2006/10/24) end
      *         CONTINUE
      *       ELSE
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) end
               IF  WRK-JIHI   >   ZERO
      * 自費エリア拡張に伴い修正 20021204
                 PERFORM  VARYING  IDX2   FROM  1  BY  1
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *                                  UNTIL    IDX2       >    5
                          UNTIL  IDX2  >  10
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
                   INITIALIZE   WRK-JIHI2
                   COMPUTE   WRK-JIHI2   =   SYU-JIHI-TAXNASI(IDX2)
                                         +   SYU-JIHI-TAXARI(IDX2)
                   IF   WRK-JIHI2  >  ZERO
                     MOVE   WRK-JIHI2   TO   WRK-ZZ
                     COMPUTE   JIHIIDX  =  IDX2  +  6 
                     ADD   WRK-JIHI2    TO   SUM-MONEY(JIHIIDX)
      ***             ADD    1          TO     SUM-KEN(JIHIIDX)
                     MOVE    1          TO   FLG-JIHI(IDX2)
                   END-IF 
                 END-PERFORM
      * 自費エリア拡張に伴い修正 20021204
               END-IF
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) start
      *       END-IF
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) end
           END-IF
      *
      ****    20030213追加
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
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *              WHEN 12 
             WHEN 17 
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
      ***  自費の場合
               MOVE  SYU-SKYMONEY    TO   WRK-SYUEKI
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *              WHEN 13 
             WHEN 18 
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
      ***  労災の場合
               COMPUTE  WRK-SYUEKI ROUNDED = 
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
      * 20040202 変更
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *              WHEN 14
             WHEN 19
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
      ***  自賠責の場合
      * 20080611 健保準拠時の自賠責の計算を修正 START tanaka
               MOVE  SYU-RSIJIBAI-SKYMONEY    TO   WRK-SYUEKI
      *               COMPUTE  WRK-SYUEKI ROUNDED = 
      *               (SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA)
      * 20080611 健保準拠時の自賠責の計算を修正END
      *=== 自賠責計算式変更対応 By Takashima(2006/12/18) start
      *               COMPUTE  WRK-SYUEKI =
      *                      ( WRK-SYUEKI * (SYS-4001-KSNRATE / 100) ) 
      *                     +  WRK-SYUEKI
      *=== 自賠責計算式変更対応 By Takashima(2006/12/18) end
      * 20080611 健保準拠時の自賠責の計算を修正START tanaka
      *               COMPUTE  WRK-SYUEKI ROUNDED = 
      *                      ( WRK-SYUEKI * (SYS-4001-KSNRATE1 / 100) ) 
      *               COMPUTE  WRK-SYUEKI  =
      *                        WRK-SYUEKI
      *                     +  SYU-RSISHOSHIN-MONEY
      *                     +  SYU-RSISAISHIN-MONEY
      *                     +  SYU-RSISDO-MONEY
      *                     +  SYU-RSIETC-MONEY
      *                     +  WRK-JIHI
      *                     +  SYU-CHOSEI
      * 20080611 健保準拠時の自賠責の計算を修正END
      * 20030411 追加
      * 20030814 削除
      * 老人一部負担金、公費一部負担金は保険点数に含まれている為削除
      ***                  +  SYU-RJN-FTN
      ***                  +  SYU-KOH-FTN
      * 20030814 削除
      * 20080611 健保準拠時の自賠責の計算を修正START tanaka
      *                     +  SYU-YKZ-FTN
      *                     +  SYU-RYOYOHI-SKJ
      *                     +  SYU-RYOYOHI-SKJ-JIHI
      * 20080611 健保準拠時の自賠責の計算を修正END
      * 20040202 削除 自己負担額は食事療養費に含まれている為、削除する。
      ***                  +  SYU-SKYMONEY-SKJ-KEI
      ***                  +  SYU-SKYMONEY-SKJ-JIHI-KEI
      * 20040202 削除 自己負担額は食事療養費に含まれている為、削除する。
      * 20080611 健保準拠時の自賠責の計算を修正START tanaka
      *                     +  SYU-RMSAGAKU
      * 20080611 健保準拠時の自賠責の計算を修正END
      * 20040202 変更
      * 20080325 H20年4月改正対応START tanaka
      ***  公害の場合
             WHEN 20
               COMPUTE   WRK-SYUEKI   =   SYU-RSIJIBAI-SKYMONEY
      * 20080325 H20年4月改正対応END
             WHEN OTHER
      ***  労災、自賠責以外の場合
               COMPUTE  WRK-SYUEKI  =  (SYU-TOTAL-HKNTEN * HKN-TENTNK)
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
           ADD    WRK-SYUEKI     TO   SUM-SYUEKI
           MOVE    1             TO   FLG-SYUEKI
      ****    20030213追加
      *
           .
       320-BODY-HEN-SUB-EXT.
           EXIT.
      *
      * 20080325 H20年4月改正対応START tanaka
      *****************************************************************
      *    帳票編集<合計カウント>サブ処理(H20年4月以降)
      *****************************************************************
       330-BODY-HEN-SUB-SEC      SECTION.
      *
      *
      *    保険番号情報読み込み
           PERFORM 900-HKNNUM-READ-SEC
           IF   FLG-HKNNUM   =   1
             MOVE   10       TO   HKN-TENTNK
           END-IF
      *
      *    保険点数
           MOVE    ZERO  TO  SYU-TOTAL-HKNTEN
           EVALUATE    SYU-SYUHKNNUM
             WHEN    OTHER    
               PERFORM VARYING IDZ FROM  1   BY  1
                       UNTIL   IDZ  >  16
                 ADD   SYU-HKNTEN (IDZ)     TO   SYU-TOTAL-HKNTEN
               END-PERFORM
               MOVE   SYU-TOTAL-HKNTEN      TO   WRK-Z9
               ADD    SYU-TOTAL-HKNTEN      TO   SUM-HKNTEN
               MOVE  1                      TO   FLG-HKN
           END-EVALUATE
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                TO  IDX 
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6                    TO   IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    20                   TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    19                   TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    18                   TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
               MOVE    17                   TO   IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF   SYU-KOH1HKNNUM   =   "027" 
                 MOVE    4                   TO   IDX
               ELSE
                 MOVE    3                   TO   IDX
               END-IF
             WHEN    "039"
             WHEN    "040"
               MOVE   4                      TO   IDX
             WHEN    OTHER
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    4                   TO   IDX
               ELSE
                 PERFORM 3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1               TO   IDX
                   WHEN    "2"
                     MOVE    2               TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
           IF   SYU-SYUHKNNUM(1:2)  =  "98"
             MOVE    17    TO      IDX
           END-IF
      *
           IF   IDX   >   ZERO
      *
             INITIALIZE WRK-JIHI
             INITIALIZE WRK-JIHI3
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) start
      *       IF   IDX   =   17
      *         MOVE    ZERO            TO   WRK-JIHI
      *         MOVE    ZERO            TO   WRK-JIHI3
      *       ELSE
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) end
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
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) start
      *       END-IF
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) end
      *
             COMPUTE   WRK-MONEY   =   SYU-SKYMONEY  -   WRK-JIHI3
      *
             MOVE    WRK-MONEY     TO  WRK-Z9
             ADD     WRK-MONEY     TO  SUM-MONEY(IDX)
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) start
      *       IF   IDX   =   17
      *         CONTINUE
      *       ELSE
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) end
               IF   WRK-JIHI    >   ZERO
                 PERFORM  VARYING IDX2  FROM 1  BY  1
                          UNTIL   IDX2   >  10
                   INITIALIZE WRK-JIHI2
                   COMPUTE WRK-JIHI2  =  SYU-JIHI-TAXNASI(IDX2)
                                      +  SYU-JIHI-TAXARI(IDX2)
                   IF   WRK-JIHI2   >   ZERO
                     MOVE   WRK-JIHI2     TO   WRK-ZZ
                     COMPUTE   JIHIIDX   =   IDX2   +   6 
                     ADD    WRK-JIHI2    TO   SUM-MONEY(JIHIIDX)
                     MOVE    1           TO   FLG-JIHI(IDX2)
                   END-IF 
                 END-PERFORM
               END-IF
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) start
      *       END-IF
      *--- 自費保険の詳細を出力するように修正 By Monma (2009/03/02) end
           END-IF
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
               MOVE    SYU-SKYMONEY  TO   WRK-SYUEKI
             WHEN 18
      *    労災の場合
               COMPUTE  WRK-SYUEKI ROUNDED  =
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
               MOVE    SYU-RSIJIBAI-SKYMONEY     TO   WRK-SYUEKI
      *        COMPUTE  WRK-SYUEKI ROUNDED = 
      *        (SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA)
      *        COMPUTE  WRK-SYUEKI ROUNDED = 
      *               ( WRK-SYUEKI * (SYS-4001-KSNRATE1 / 100) )
      *        COMPUTE  WRK-SYUEKI =
      *                 WRK-SYUEKI
      *              +  SYU-RSISHOSHIN-MONEY
      *              +  SYU-RSISAISHIN-MONEY
      *              +  SYU-RSISDO-MONEY
      *              +  SYU-RSIETC-MONEY
      *              +  WRK-JIHI
      *              +  SYU-CHOSEI
      *              +  SYU-YKZ-FTN
      *              +  SYU-RYOYOHI-SKJ
      *              +  SYU-RYOYOHI-SKJ-JIHI
      *              +  SYU-RMSAGAKU
      * 20080611 健保準拠時の自賠責の計算を修正END
             WHEN 20
      ***  公害の場合
               COMPUTE   WRK-SYUEKI  =  SYU-RSIJIBAI-SKYMONEY
             WHEN OTHER
      ***  労災、自賠責以外の場合
               COMPUTE   WRK-SYUEKI  =  (SYU-TOTAL-HKNTEN * HKN-TENTNK)
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
           ADD      WRK-SYUEKI     TO   SUM-SYUEKI
           MOVE       1            TO   FLG-SYUEKI
      *
           .
       330-BODY-HEN-SUB-EXT.
           EXIT.
      *
      * 20080325 H20年4月改正対応END
      *****************************************************************
      *    件数集計サブ処理
      *****************************************************************
       320-CNT-SEC                    SECTION.
      *
      ***  保険別件数カウント 
           ADD    1       TO   SUM-RKEN(IDX)
      ***  自費分件数カウント 
           PERFORM  VARYING IDX2  FROM 1  BY  1
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *        UNTIL  IDX2       >  5
                    UNTIL  IDX2   >  10
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
             IF  FLG-JIHI(IDX2)  = 1
               COMPUTE  JIHIIDX  =  IDX2  +  6 
               ADD     1       TO   SUM-RKEN(JIHIIDX)
             END-IF
           END-PERFORM
      ***  保険点数件数カウント
           IF  FLG-HKN = 1
             ADD      1        TO   SUM-RHKNKEN
           END-IF
      ***  医療収益計件数カウント
           IF  FLG-SYUEKI = 1
             ADD      1        TO   SUM-RSYUEKIKEN
           END-IF
      ***  窓口請求金額件数カウント
           IF  FLG-SEIKYU = 1
             ADD      1        TO   SUM-RSEIKYUKEN
           END-IF
      ***  窓口領収金額件数カウント
           IF  FLG-RYOSYU = 1
             ADD      1        TO   SUM-RRYOSYUKEN
           END-IF
           .
       320-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報集計サブ処理
      *****************************************************************
       320-JYURRK-CNT-SEC      SECTION.
      *
           MOVE JYURRK-SRYYMD(7:2) TO  IDX7G
      ***         INITIALIZE   SYUNOU-REC
      ***         MOVE  JYURRK-HOSPID      TO  SYU-HOSPID
      ***         MOVE  JYURRK-NYUGAIKBN   TO  SYU-NYUGAIKBN
      ***         MOVE  JYURRK-PTID        TO  SYU-PTID
      ***         MOVE  JYURRK-DENPNUM     TO  SYU-DENPNUM
      ***         PERFORM 900-SYUNOU-READ2-SEC
           IF   WRK-PARA-DENPPRTYMD  <  "20080401"
             PERFORM   320-HOKEN-GROUPING-SEC
           ELSE
             PERFORM   330-HOKEN-GROUPING-SEC
           END-IF
      *--- 2008/12/04 ADD START ----------------------------------------
      *    患者初診療日取得
           PERFORM   900-JYURRK2-READ-SEC
           
           INITIALIZE               SRYACCT-REC
           MOVE    JYURRK-HOSPNUM         TO   ACCT-HOSPNUM 
           MOVE    JYURRK-NYUGAIKBN       TO   ACCT-NYUGAIKBN
           MOVE    JYURRK-PTID            TO   ACCT-PTID
           MOVE    JYURRK-SRYKA           TO   ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)     TO   ACCT-SRYYM
      *    診療 会計情報SELECT(KEY=3)
           MOVE    SRYACCT-REC            TO   MCPDATA-REC
           MOVE    "DBSELECT"             TO   MCP-FUNC
           MOVE    "tbl_sryacct"          TO   MCP-TABLE
           MOVE    "key2"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"            TO   MCP-FUNC
             MOVE    "tbl_sryacct"        TO   MCP-TABLE
             MOVE    "key2"               TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC        TO   SRYACCT-REC
               MOVE    ZERO               TO   FLG-SRYACCT
             ELSE     
               MOVE    1                  TO   FLG-SRYACCT
               INITIALIZE                 SRYACCT-REC
             END-IF
           ELSE
             MOVE  1                      TO   FLG-SRYACCT
             INITIALIZE                   SRYACCT-REC
           END-IF
           MOVE    "DBCLOSECURSOR"        TO   MCP-FUNC
           MOVE    "tbl_sryacct"          TO   MCP-TABLE
           MOVE    "key2"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF   FLG-SRYACCT   =  ZERO
               AND   SYU-SRYYMD  =  JYURRK2-SRYYMD
               AND   ACCT-DAY(1 IDX7)   >   ZERO
               AND   JYURRK-RENNUM  =  1
      *    新患件数としてカウントする。
      *    保険別件数カウント 
             ADD    1   TO   SUM-SKEN(IDX)
      *    自費分件数カウント 
             PERFORM  VARYING IDX2  FROM 1  BY  1
                      UNTIL   IDX2   >  10
               IF  FLG-JIHI(IDX2)  = 1
                 COMPUTE  JIHIIDX = IDX2 + 6 
                 ADD   1       TO  SUM-SKEN(JIHIIDX)
               END-IF  
             END-PERFORM
      *    保険点数件数カウント
             IF  FLG-HKN = 1
               ADD     1       TO   SUM-SHKNKEN
             END-IF
      *    医療収益計件数カウント
             IF  FLG-SYUEKI = 1
               ADD     1       TO   SUM-SSYUEKIKEN
             END-IF
      *    窓口請求金額件数カウント
             IF  FLG-SEIKYU = 1
               ADD     1       TO   SUM-SSEIKYUKEN
             END-IF
      *    窓口領収金額件数カウント
             IF  FLG-RYOSYU = 1
               ADD     1       TO   SUM-SRYOSYUKEN
             END-IF
           END-IF
           INITIALIZE                SRYACCT-REC
           MOVE    ZERO                TO   FLG-SRYACCT
      *--- 2008/12/04 ADD END ----------------------------------------
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE  JYURRK-HOSPID      TO  ACCT-HOSPID
           MOVE    JYURRK-HOSPNUM      TO   ACCT-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    JYURRK-NYUGAIKBN    TO   ACCT-NYUGAIKBN
           MOVE    JYURRK-PTID         TO   ACCT-PTID
           MOVE    JYURRK-SRYKA        TO   ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO   ACCT-SRYYM
           MOVE    "11"                TO   ACCT-SRYKBN
      *=== 初診、再診、その他振分け追加修正 ADD By Yoshikawa(2007/07/04) start
      *           PERFORM 900-SRYACCT-READ-SEC
      *  診療会計情報SELECT(KEY=3)
           MOVE    SRYACCT-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE  PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *         CALL  "MCPSUB"             USING
           MOVE    "tbl_sryacct"       TO   MCP-TABLE
           MOVE    "key3"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                     SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC    =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *       MOVE  PATH-TBL-SRYACCT-KEY3  TO  MCP-PATH
      *       CALL  "MCPSUB"         USING
             MOVE    "tbl_sryacct"     TO   MCP-TABLE
             MOVE    "key3"            TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC  =  ZERO
               MOVE  MCPDATA-REC     TO   SRYACCT-REC
               MOVE  ZERO            TO   FLG-SRYACCT
             ELSE
               MOVE    1             TO   FLG-SRYACCT
               INITIALIZE                 SRYACCT-REC
             END-IF
           ELSE
             MOVE   1                TO   FLG-SRYACCT
             INITIALIZE                   SRYACCT-REC
           END-IF
      *--- 2008/12/04 DELETE START -------------------------------------
      **  初診データ探索処理
      *         MOVE    ZERO    TO    FLG-INPUT
      *         PERFORM   UNTIL   FLG-SRYACCT  =  1
      *            IF  ACCT-DAY(1 IDX7)   >   ZERO
      **  初診データが見つかったら入力フラグをオン
      *               MOVE  1   TO   FLG-INPUT
      *            END-IF
      **  診療会計情報FETCH
      *            MOVE  SRYACCT-REC     TO  MCPDATA-REC
      *            MOVE  "DBFETCH"       TO  MCP-FUNC
      **=== 4.0対応 By Takashima(2007/09/10) start
      **            MOVE  PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      **            CALL  "MCPSUB"        USING
      *            MOVE    "tbl_sryacct"          TO  MCP-TABLE
      *            MOVE    "key3"                 TO  MCP-PATHNAME
      *            CALL    "ORCDBMAIN"   USING
      **=== 4.0対応 By Takashima(2007/09/10) end
      *                                  MCPAREA
      *                                  MCPDATA-REC
      **=== 4.0対応 By Takashima(2007/09/10) start
      *                                  SPA-AREA
      **=== 4.0対応 By Takashima(2007/09/10) end
      *            IF     MCP-RC     =   ZERO
      *               MOVE   MCPDATA-REC   TO  SRYACCT-REC
      *               MOVE   ZERO          TO  FLG-SRYACCT
      *            ELSE
      *               MOVE   1             TO  FLG-SRYACCT
      *               INITIALIZE               SRYACCT-REC
      *            END-IF
      *         END-PERFORM
      *--- 2008/12/04 DELETE END ---------------------------------------
      *  診療会計情報CLOSE
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE  PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *         CALL  "MCPSUB"         USING
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *  入力フラグがオンの場合、診療会計終了フラグを切る
      *--- 2008/12/04 CHANGE START -------------------------------------
      *         IF   FLG-INPUT       =   1
      *            MOVE   ZERO      TO  FLG-SRYACCT
      *         END-IF
      **=== 初診、再診、その他振分け追加修正 ADD By Yoshikawa(2007/07/04) end
      **
      *         IF     FLG-SRYACCT    =  ZERO
      **=== 初診、再診、その他振分け修正 DEL By Yoshikawa(2007/06/22) start
      **            AND   ACCT-DAY(1 IDX7)  >  ZERO
      **=== 初診、再診、その他振分け修正 DEL By Yoshikawa(2007/06/22) end
      *            AND   JYURRK-RENNUM  =  1
      **=== 新患件数の集計誤りの修正 ADD By Yoshikawa(2007/07/04) start
      **  初診の件数としてカウントする。 
      *** 20040405 追加
      **            IF  WRK-CREYMD(1:6) = WRK-PARA-DENPPRTYMD(1:6)
      *            IF  WRK-CREYMD = WRK-PARA-DENPPRTYMD
      **=== 新患件数の集計誤りの修正 ADD By Yoshikawa(2007/07/04) end
      ****  新患件数としてカウントする。
      ** 20040405 追加
      ****  保険別件数カウント 
      *                  ADD    1   TO   SUM-SKEN(IDX)
      ****  自費分件数カウント 
      *                  PERFORM  VARYING IDX2  FROM 1  BY  1
      **=== 自費追加対応 ADD By Takashima(2006/10/24) start
      **                      UNTIL    IDX2       >    5
      *                      UNTIL    IDX2       >    10
      **=== 自費追加対応 ADD By Takashima(2006/10/24) end
      *                      IF  FLG-JIHI(IDX2)  = 1
      *                           COMPUTE  JIHIIDX = IDX2 + 6 
      *                           ADD   1   TO  SUM-SKEN(JIHIIDX)
      *                      END-IF 
      *                  END-PERFORM
      ****  保険点数件数カウント 
      *                  IF  FLG-HKN = 1
      *                       ADD   1  TO   SUM-SHKNKEN
      *                  END-IF 
      ****  医療収益計件数カウント 
      *                  IF  FLG-SYUEKI = 1
      *                       ADD   1  TO   SUM-SSYUEKIKEN
      *                  END-IF 
      ****  窓口請求金額件数カウント 
      *                  IF  FLG-SEIKYU = 1
      *                       ADD   1  TO   SUM-SSEIKYUKEN
      *                  END-IF 
      ****  窓口領収金額件数カウント 
      *                  IF  FLG-RYOSYU = 1
      *                       ADD   1  TO   SUM-SRYOSYUKEN
      *                  END-IF 
      ** 20040405 追加
      *            ELSE
           IF   FLG-SRYACCT  =  ZERO
               AND   ACCT-DAY(1 IDX7)   >   ZERO
               AND   JYURRK-RENNUM  =  1
      *--- 2008/12/04 CHANGE END ---------------------------------------
      *    初診件数としてカウントする。
      ***  保険別件数カウント 
             ADD    1   TO   SUM-SAKEN(IDX)
      ***  自費分件数カウント 
             PERFORM  VARYING IDX2  FROM 1  BY  1
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *                      UNTIL    IDX2       >    5
                      UNTIL    IDX2       >    10
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
               IF  FLG-JIHI(IDX2)  =  1
                  COMPUTE  JIHIIDX  =  IDX2  +  6 
                  ADD    1       TO   SUM-SAKEN(JIHIIDX)
               END-IF 
             END-PERFORM
      *    保険点数件数カウント
             IF   FLG-HKN   =   1
               ADD    1          TO   SUM-SAHKNKEN
             END-IF
      *    医療収益計件数カウント
             IF  FLG-SYUEKI  =  1
               ADD    1          TO   SUM-SASYUEKIKEN
             END-IF
      *    窓口請求金額件数カウント
             IF  FLG-SEIKYU  =  1
               ADD    1          TO   SUM-SASEIKYUKEN
             END-IF
      *    窓口領収金額件数カウント
             IF  FLG-RYOSYU  =  1
               ADD    1          TO   SUM-SARYOSYUKEN
             END-IF
      *--- 2008/12/04 DELETE START -------------------------------------
      *     END-IF 
      *--- 2008/12/04 DELETE END ---------------------------------------
      * 20040405 追加
           ELSE 
      *=== 初診、再診、その他振分け追加修正 ADD By Yoshikawa(2007/07/04) start
             MOVE  2                     TO   JYURRK-RENNUM
             MOVE "DBSELECT"             TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *           MOVE PATH-TBL-JYURRK-KEY5 TO MCP-PATH
      *           CALL "MCPSUB"            USING
             MOVE    "tbl_jyurrk"        TO   MCP-TABLE
             MOVE    "key5"              TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                         MCPAREA
                                         MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                         SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF  MCP-RC  = ZERO
               MOVE   ZERO               TO   FLG-JYURRK
               MOVE   JYURRK-ZAINUM1     TO   ACCT-ZAINUM
             ELSE
               MOVE   1                  TO   FLG-JYURRK
             END-IF
      *
             MOVE   "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *           MOVE  PATH-TBL-JYURRK-KEY5   TO  MCP-PATH
      *           CALL  "MCPSUB"   USING
             MOVE   "tbl_jyurrk"         TO   MCP-TABLE
             MOVE   "key5"               TO   MCP-PATHNAME
             CALL   "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                         MCPAREA
                                         MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                         SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *=== 初診、再診、その他振分け追加修正 ADD By Yoshikawa(2007/07/04) end
      *
      ***  20050914  MOTO
      *=== 4.0対応 By Takashima(2007/09/10) start
      *           MOVE  JYURRK-HOSPID      TO  ACCT-HOSPID
             MOVE  JYURRK-HOSPNUM     TO   ACCT-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
             MOVE  JYURRK-NYUGAIKBN   TO   ACCT-NYUGAIKBN
             MOVE  JYURRK-PTID        TO   ACCT-PTID
             MOVE  JYURRK-SRYKA       TO   ACCT-SRYKA
             MOVE  JYURRK-SRYYMD(1:6) TO   ACCT-SRYYM
             MOVE "12"                TO   ACCT-SRYKBN
      *=== 初診、再診、その他振分け追加修正 ADD By Yoshikawa(2007/07/04) start
      *           PERFORM 900-SRYACCT-READ-SEC
             MOVE  SRYACCT-REC        TO   MCPDATA-REC
             MOVE  "DBSELECT"         TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *           MOVE  PATH-TBL-SRYACCT-KEY5  TO  MCP-PATH
      *           CALL  "MCPSUB"               USING
             MOVE  "tbl_sryacct"      TO   MCP-TABLE
             MOVE  "key5"             TO   MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                      MCPAREA
                                      MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                      SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =  ZERO
               MOVE   "DBFETCH"       TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *              MOVE   PATH-TBL-SRYACCT-KEY5 TO  MCP-PATH
      *              CALL   "MCPSUB"              USING
               MOVE    "tbl_sryacct"  TO   MCP-TABLE
               MOVE    "key5"         TO   MCP-PATHNAME
               CALL    "ORCDBMAIN"    USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                      MCPAREA
                                      MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                      SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF   MCP-RC   =  ZERO
                 MOVE   MCPDATA-REC   TO   SRYACCT-REC
                 MOVE   ZERO          TO   FLG-SRYACCT
               ELSE
                 MOVE   1             TO   FLG-SRYACCT
                 INITIALIZE                SRYACCT-REC
               END-IF
             ELSE
               MOVE    1              TO   FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
             END-IF
      *
             MOVE    "DBCLOSECURSOR"  TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *           MOVE  PATH-TBL-SRYACCT-KEY5  TO  MCP-PATH
      *           CALL  "MCPSUB"  USING
             MOVE    "tbl_sryacct"    TO   MCP-TABLE
             MOVE    "key5"           TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                      MCPAREA
                                      MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                      SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *=== 初診、再診、その他振分け追加修正 ADD By Yoshikawa(2007/07/04) end
      *
             IF     FLG-SRYACCT    =  ZERO
                 AND   ACCT-DAY(1 IDX7)  >  ZERO
      *=== 初診、再診、その他振分け修正 ADD By Yoshikawa(2007/06/22) start
                 AND   JYURRK-SRYKBN(1)  >  "00"
      *=== 初診、再診、その他振分け修正 ADD By Yoshikawa(2007/06/22) end
      *=== 在宅等の誤振分け対応 ADD By Yoshikawa(2007/12/18) start
                 AND   ACCT-SRYKBN   =  "12"
      *=== 在宅等の誤振分け対応 ADD By Yoshikawa(2007/12/18) end
      *
      ***  20050914  MOTO
      *
      *    再診の件数としてカウントする。
      ***  保険別件数カウント 
               ADD     1   TO   SUM-RKEN(IDX)
      ***  自費分件数カウント 
               PERFORM  VARYING IDX2  FROM 1  BY  1
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *                      UNTIL    IDX2       >    5
                        UNTIL    IDX2  >  10
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
                 IF  FLG-JIHI(IDX2)  =   1
                   COMPUTE  JIHIIDX   =   IDX2   +   6 
                   ADD   1   TO    SUM-RKEN(JIHIIDX)
                 END-IF 
               END-PERFORM
      *    保険点数件数カウント 
               IF  FLG-HKN  =  1
                 ADD   1  TO   SUM-RHKNKEN
               END-IF 
      *    医療収益計件数カウント
               IF  FLG-SYUEKI = 1
                 ADD   1  TO   SUM-RSYUEKIKEN
               END-IF 
      *    窓口請求金額件数カウント
               IF  FLG-SEIKYU = 1
                 ADD   1  TO   SUM-RSEIKYUKEN
               END-IF 
      *    窓口領収金額件数カウント 
               IF  FLG-RYOSYU = 1
                 ADD   1  TO   SUM-RRYOSYUKEN
               END-IF 
      *
      ***  20050914  MOTO
             ELSE
      *    その他の件数としてカウントする。
      ***  保険別件数カウント 
               ADD     1   TO   SUM-OKEN(IDX)
      ***  自費分件数カウント 
               PERFORM  VARYING IDX2  FROM 1  BY  1
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *                      UNTIL    IDX2       >    5
                        UNTIL    IDX2   >   10
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
                 IF  FLG-JIHI(IDX2)  = 1
                   COMPUTE  JIHIIDX  =  IDX2  +  6 
                   ADD     1    TO   SUM-OKEN(JIHIIDX)
                 END-IF 
               END-PERFORM
      *    保険点数件数カウント 
               IF  FLG-HKN  =  1
                 ADD   1  TO   SUM-OHKNKEN
               END-IF 
      *    医療収益計件数カウント 
               IF  FLG-SYUEKI  =  1
                 ADD   1  TO   SUM-OSYUEKIKEN
               END-IF 
      *    窓口請求金額件数カウント
               IF  FLG-SEIKYU = 1
                 ADD   1  TO   SUM-OSEIKYUKEN
               END-IF 
      *    窓口領収金額件数カウント 
               IF  FLG-RYOSYU = 1
                 ADD   1  TO   SUM-ORYOSYUKEN
               END-IF 
             END-IF
      ***  20050914  MOTO
      *
           END-IF
      *
      * 20030530 追加
      *    現在処理中のレコードが同一患者のレコードでない場合は、
      *    受診者数のカウントを行う。(同日診療の場合は実患者数をカウントする。)
      *    但し、現状は科別集計においては受診者数の印字は行わない。
      ***       IF  WRK-SMEI-PTID(IDX-S) NOT  =  WRK-PTID
      ***           ADD  1       TO      SUM-JYUSHINKEN
      ***       END-IF
           MOVE     1        TO      IDX3 
           PERFORM  UNTIL  IDX3  >  50000
             IF  WRK-SMEI-PTID(IDX-S)  =  WRK-JYUSHIN-PTID(IDX3)
      * 既に同日の診療が受診件数として計上されている場合は
      * 全科合計の受診者数に計上しない.
      ****2006/03/20 @@@@-BRAND START
      *                MOVE 99999   TO      IDX3
               MOVE    50000     TO   IDX3
      ****2006/03/20 @@@@-BRAND END
             END-IF
      ****2006/03/20 @@@@-BRAND START
      *       IF  WRK-JYUSHIN-PTID(IDX3)  =  ZERO
             IF  (WRK-JYUSHIN-PTID(IDX3)  =  ZERO)
                 AND  (IDX3  NOT =  50000)
      ****2006/03/20 @@@@-BRAND END
      * 同日の診療が発生していない場合は
      * 全科合計の受診者数に計上する。
               MOVE   WRK-SMEI-PTID(IDX-S)  TO   WRK-JYUSHIN-PTID(IDX3)
               ADD    1       TO   SUM-JYUSHINKEN-GK
      ****2006/03/20 @@@@-BRAND START
      *                MOVE 99999   TO      IDX3
               MOVE   50000   TO   IDX3
      ****2006/03/20 @@@@-BRAND END
             END-IF
             ADD 1            TO   IDX3
           END-PERFORM
      * 20030530 追加
      *
           .
       320-JYURRK-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険グループ判別処理
      *****************************************************************
       320-HOKEN-GROUPING-SEC      SECTION.
      *
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                TO   IDX
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6               TO   IDX
      * 20080325 H20年4月改正対応START tanaka
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    20              TO   IDX
      * 20080325 H20年4月改正対応END
             WHEN    WRK-CONS-JIBAI-HKNNUM
      * 自費エリア拡張に伴い修正 20021204
      ***                MOVE    9                   TO  IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *                MOVE    14                   TO  IDX
               MOVE    19              TO   IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
             WHEN    WRK-CONS-ROUSAI-HKNNUM
      * 自費エリア拡張に伴い修正 20021204
      ***                 MOVE    8                   TO  IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *                MOVE    13                   TO  IDX
               MOVE    18              TO   IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
             WHEN    WRK-CONS-JIHI-HKNNUM
      * 自費エリア拡張に伴い修正 20021204
      ***                 MOVE    7                   TO  IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *                MOVE    12                   TO  IDX
               MOVE    17              TO   IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
             WHEN    "060"
             WHEN    "067"
      *=== 特別療養費対応 ADD By Uchida(2006/12/22) start
             WHEN    "068"
             WHEN    "069"
      *=== 特別療養費対応 ADD By Uchida(2006/12/22) end
               IF   SYU-KOH1HKNNUM   =   "027" 
                 MOVE    5             TO   IDX
               ELSE
                 MOVE    4             TO   IDX
               END-IF
             WHEN    OTHER
               IF   SYU-KOH1HKNNUM   =  "027" 
                 MOVE    3             TO   IDX
               ELSE
                 PERFORM 3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1         TO   IDX
                   WHEN    "2"
                     MOVE    2         TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
      * 20031111追加
      * 保険別区分例外条件(自費条件拡張)
           IF   SYU-SYUHKNNUM(1:2) = "98"
      *=== 自費追加対応 ADD By Takashima(2006/10/24) start
      *              MOVE    12    TO      IDX
             MOVE      17       TO   IDX
      *=== 自費追加対応 ADD By Takashima(2006/10/24) end
           END-IF
      * 20031111追加
      *
           .
       320-HOKEN-GROUPING-EXT.
           EXIT.
      * 20080325 H20年4月改正対応START tanaka
      *****************************************************************
      *    保険グループ判別処理(20年4月以降)
      *****************************************************************
       330-HOKEN-GROUPING-SEC      SECTION.
      *
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                TO  IDX 
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6               TO   IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE   20               TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    19              TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    18              TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM     
               MOVE    17              TO   IDX
             WHEN    "039"
             WHEN    "040"
               MOVE    4               TO   IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF      SYU-KOH1HKNNUM   =   "027"
                 MOVE    4             TO   IDX
               ELSE
                 MOVE    3             TO   IDX
               END-IF
             WHEN    OTHER
               IF      SYU-KOH1HKNNUM   =   "027"
                 MOVE    4             TO   IDX
               ELSE
                 PERFORM 3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1         TO   IDX
                   WHEN    "2"
                     MOVE    2         TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
           IF   SYU-SYUHKNNUM(1:2) = "98"
             MOVE    17                TO   IDX
           END-IF
      *
           .
       320-HOKEN-GROUPING-EXT.
           EXIT.
      * 20080325 H20年4月改正対応END
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3201-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *   保険組合せ読み出し
           INITIALIZE                 HKNCOMBI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    SYU-HOSPID        TO  COMB-HOSPID
           MOVE    SYU-HOSPNUM        TO   COMB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    SYU-PTID           TO   COMB-PTID
           MOVE    SYU-HKNCOMBINUM    TO   COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF   FLG-HKNCOMBI  =  ZERO
      *    患者保険情報読み込み
             INITIALIZE             PTHKNINF-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    SYU-HOSPID        TO  PTHKN-HOSPID
             MOVE    SYU-HOSPNUM      TO  PTHKN-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
             MOVE    SYU-PTID         TO  PTHKN-PTID
             MOVE    COMB-HKNID       TO  PTHKN-HKNID
             PERFORM   900-PTHKNINF-READ-SEC
           END-IF
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票<診療科合計>印刷処理
      *****************************************************************
      *    
       330-TERM-KA-PRINT-SEC         SECTION.
      *
           INITIALIZE            HC17B
      *
           MOVE    WRK-PARA-DENPPRTYMDWH    TO   HC17B-SYSYMD
      *
      ***    PERFORM  VARYING  IDX2   FROM 1  BY  1
      ***                       UNTIL       IDX2   >         5 
      ***         MOVE    WRK-TJIHI(IDX2)   TO   HC17B-TJIHI(IDX2)
      ***    END-PERFORM
      *
      * 20030407 追加  入外区分名称設定
           EVALUATE WRK-NYUGAIKBN
             WHEN  "1"
               MOVE   "入院"         TO   HC17B-NYUGAIKBN
      ***      入院自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
      *=== 自費追加対応 UP By Takashima(2006/10/24) start
      *                      UNTIL    IDX2   >    5 
                        UNTIL    IDX2   >  10 
      *=== 自費追加対応 UP By Takashima(2006/10/24) end
                 MOVE    WRK-TJIHIN(IDX2)  TO   HC17B-TJIHI(IDX2)
               END-PERFORM
      *=== 自費追加対応 UP By Takashima(2006/10/24) start
      *             MOVE  "請求件数"    TO  HC17B-KENSU1
               MOVE   "請求件数"    TO   HC17B-KENSU1(1)
               MOVE   "請求件数"    TO   HC17B-KENSU1(2)
      *=== 自費追加対応 UP By Takashima(2006/10/24) end
             WHEN "2"
               MOVE   "外来"        TO   HC17B-NYUGAIKBN
      *    外来自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
      *=== 自費追加対応 UP By Takashima(2006/10/24) start
      *                      UNTIL    IDX2   >    5 
                        UNTIL    IDX2   >    10 
      *=== 自費追加対応 UP By Takashima(2006/10/24) end
                 MOVE    WRK-TJIHI(IDX2)    TO   HC17B-TJIHI(IDX2)
               END-PERFORM
      *=== 自費追加対応 UP By Takashima(2006/10/24) start
      *             MOVE  "再診件数"    TO  HC17B-KENSU1
      *             MOVE  "初診件数"    TO  HC17B-KENSU2
      ** 20050914 MOTO
      *             MOVE  "その他件数"    TO  HC17B-KENSU4
      ** 20050914 MOTO
      ** 20040405 追加
      *             MOVE  "(新患件)"    TO  HC17B-KENSU3
      **             MOVE  "新患件数"    TO  HC17B-KENSU3
      **             MOVE  "再患件数"    TO  HC17B-KENSU4
      ** 20040405 追加
               MOVE    "再診件数"           TO   HC17B-KENSU1(1)
               MOVE    "初診件数"           TO   HC17B-KENSU2(1)
               MOVE    "その他件数"         TO   HC17B-KENSU4(1)
               MOVE    "(新患件)"           TO   HC17B-KENSU3(1)
               MOVE    "再診件数"           TO   HC17B-KENSU1(2)
               MOVE    "初診件数"           TO   HC17B-KENSU2(2)
               MOVE    "その他件数"         TO   HC17B-KENSU4(2)
               MOVE    "(新患件)"           TO   HC17B-KENSU3(2)
      *=== 自費追加対応 UP By Takashima(2006/10/24) end
           END-EVALUATE
      * 20030407 追加  入外区分名称設定
      *
      * 20030407 追加  診療科名称設定
      *    診療科名称取得
           INITIALIZE                   SYS-1005-REC
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
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1005-REC
             MOVE   SYS-1005-SRYKANAME  TO   HC17B-SRYKA
           ELSE
             MOVE   SPACE               TO   HC17B-SRYKA
           END-IF
      * 20030407 追加  診療科名称設定
      *
           IF  FLG-GET = ZERO
      *    対象データが１件も存在しなかった場合
             MOVE  "＊＊＊  対象データが１件も存在しません。＊＊＊"
                                        TO   HC17B-JOBMSG
             MOVE    1                  TO   WRK-PAGE
             MOVE    WRK-PAGE           TO   HC17B-PAGE
           ELSE
      *    対象データが１件以上存在した場合
             ADD     1                  TO   CNT-PAGE
             MOVE    CNT-PAGE           TO   WRK-PAGE
             MOVE    WRK-PAGE           TO   HC17B-PAGE
      *    保険点数、件数合計
             MOVE    SUM-HKNTEN         TO   WRK-Z9
      *=== 自費追加対応 UP By Takashima(2006/10/24) start
      *       MOVE    WRK-Z9   TO  HC17B-GHKNTEN
             MOVE    WRK-Z9             TO   HC17B-GHKNKEN
      *=== 自費追加対応 UP By Takashima(2006/10/24) end
             IF    WRK-NYUGAIKBN   =  "2" 
      * 20040417 編集 Y.Takahashi
      **          MOVE    SUM-SHKNKEN   TO  WRK-Z9
      **          MOVE    WRK-Z9   TO  HC17B-SHKNKEN
      *** 20040405 追加
      **          MOVE    SUM-SAHKNKEN  TO  WRK-Z9
      **          MOVE    WRK-Z9   TO  HC17B-SAHKNKEN
      *** 20040405 追加
      *--- 2008/12/04 CHANGE START -------------------------------------
      *          COMPUTE WRK-Z9   =   SUM-SHKNKEN   +   SUM-SAHKNKEN
               MOVE    SUM-SAHKNKEN     TO   WRK-Z9
      *--- 2008/12/04 CHANGE END ---------------------------------------
               MOVE    WRK-Z9           TO   HC17B-SHKNKEN
               MOVE    SUM-SHKNKEN      TO   WRK-Z9
               MOVE    WRK-Z9(2:7)      TO   WRK-TMP-K
               MOVE    1                TO   WRK-COUNTS
               INSPECT  WRK-TMP-K   TALLYING
                        WRK-COUNTS  FOR  LEADING  SPACE
               MOVE     WRK-TMP-K       TO   HC17B-SAHKNKEN(2:7)
               MOVE    "("            TO   HC17B-SAHKNKEN(WRK-COUNTS:1)
               MOVE    ")"            TO   HC17B-SAHKNKEN(9:1)
      */20040417 編集 Y.Takahashi
      * 20050914 MOTO
               MOVE    SUM-OHKNKEN      TO   WRK-Z9
               MOVE    WRK-Z9           TO   HC17B-OHKNKEN
      * 20050914 MOTO
             END-IF
             MOVE    SUM-RHKNKEN         TO   WRK-Z9
             MOVE    WRK-Z9              TO   HC17B-RHKNKEN
      *
      *    保険別金額、件数合計
             PERFORM VARYING IDX FROM    1   BY  1
      *=== 自費追加対応 UP By Takashima(2006/10/24) start
      *             UNTIL    IDX   >    14
      *       IF    IDX = 7
      *         OR  IDX = 8
      *         OR  IDX = 9
      *         OR  IDX = 10
      *         OR  IDX = 11
                    UNTIL    IDX   >    20
             IF    IDX = 7
                 OR  IDX = 8
                 OR  IDX = 9
                 OR  IDX = 10
                 OR  IDX = 11
                 OR  IDX = 12
                 OR  IDX = 13
                 OR  IDX = 14
                 OR  IDX = 15
                 OR  IDX = 16
      *=== 自費追加対応 UP By Takashima(2006/10/24) end
               COMPUTE IDX8 = IDX - 6
               EVALUATE WRK-NYUGAIKBN
                 WHEN "1"
      *    入院自費項目の設定
      *** 20050404  MOTO  削除
      ***               IF  WRK-TJIHIN-FLG(IDX8) =  1
      *    自費エリアは有効である場合に金額、件数を転送する
      *--- 2008/12/16 CHANGE START -------------------------------------
      *             MOVE  SUM-MONEY(IDX)    TO  WRK-Z9
      *             MOVE  WRK-Z9   TO  HC17B-GMONEY(IDX)
                   MOVE  SUM-MONEY(IDX)    TO  WRK-S9
                   MOVE  WRK-S9   TO  HC17B-GMONEY(IDX)
      *--- 2008/12/16 CHANGE END ---------------------------------------
                   MOVE  SUM-RKEN(IDX)       TO  WRK-Z9
                   MOVE  WRK-Z9   TO  HC17B-RKENSU(IDX)
      ***             MOVE  SUM-SKEN (IDX)       TO  WRK-Z9
      ***             MOVE  WRK-Z9   TO  HC17B-SKENSU  (IDX)
      ***           END-IF
                 WHEN "2"
      *    外来自費項目の設定
      *** 20050404  MOTO  削除
      ***               IF  WRK-TJIHI-FLG(IDX8) =  1
      *    自費エリアは有効である場合に金額、件数を転送する。
      *--- 2008/12/16 CHANGE START -------------------------------------
      *             MOVE   SUM-MONEY(IDX)    TO  WRK-Z9
      *             MOVE   WRK-Z9   TO  HC17B-GMONEY(IDX)
                   MOVE   SUM-MONEY(IDX)    TO  WRK-S9
                   MOVE   WRK-S9   TO  HC17B-GMONEY(IDX)
      *--- 2008/12/16 CHANGE END ---------------------------------------
                   MOVE   SUM-RKEN(IDX)       TO  WRK-Z9
                   MOVE   WRK-Z9   TO  HC17B-RKENSU(IDX)
      * 20050914 MOTO
                   MOVE   SUM-OKEN(IDX)       TO  WRK-Z9
                   MOVE   WRK-Z9   TO  HC17B-OKENSU  (IDX)
      * 20050914 MOTO
      * 20040417 編集 Y.Takahashi
      **                 MOVE  SUM-SKEN (IDX)       TO  WRK-Z9
      **                 MOVE  WRK-Z9   TO  HC17B-SKENSU  (IDX)
      *** 20040405 追加
      **                 MOVE  SUM-SAKEN (IDX)      TO  WRK-Z9
      **                 MOVE  WRK-Z9   TO  HC17B-SAKENSU  (IDX)
      *** 20040405 追加
      *--- 2008/12/04 CHANGE START -------------------------------------
      *          COMPUTE WRK-Z9 =  SUM-SKEN(IDX)  + SUM-SAKEN(IDX)
                   MOVE    SUM-SAKEN(IDX)      TO   WRK-Z9
      *--- 2008/12/04 CHANGE END ---------------------------------------
                   MOVE    WRK-Z9              TO   HC17B-SKENSU (IDX)
                   MOVE    SUM-SKEN (IDX)      TO   WRK-Z9
                   MOVE    WRK-Z9(2:7)         TO   WRK-TMP-K
                   MOVE    1                   TO   WRK-COUNTS
                   INSPECT WRK-TMP-K  TALLYING
                           WRK-COUNTS  FOR  LEADING  SPACE
                   MOVE    WRK-TMP-K  TO  HC17B-SAKENSU(IDX)(2:7)
                   MOVE    "("    TO  HC17B-SAKENSU(IDX)(WRK-COUNTS:1)
                   MOVE    ")"    TO  HC17B-SAKENSU(IDX)(9:1)
      */20040417 編集 Y.Takahashi
      ***               END-IF
               END-EVALUATE
             ELSE
      *--- 2008/12/16 CHANGE START -------------------------------------
      *         MOVE    SUM-MONEY(IDX)      TO   WRK-Z9
      *         MOVE    WRK-Z9   TO   HC17B-GMONEY(IDX)
               MOVE    SUM-MONEY(IDX)      TO   WRK-S9
               MOVE    WRK-S9   TO   HC17B-GMONEY(IDX)
      *--- 2008/12/16 CHANGE END ---------------------------------------
               MOVE    SUM-RKEN(IDX)       TO   WRK-Z9
               MOVE    WRK-Z9   TO   HC17B-RKENSU(IDX)
               IF WRK-NYUGAIKBN =   "2" 
      * 20040417 編集 Y.Takahashi
      **            MOVE SUM-SKEN (IDX)       TO  WRK-Z9
      **            MOVE WRK-Z9   TO  HC17B-SKENSU  (IDX)
      *** 20040405 追加
      **            MOVE SUM-SAKEN (IDX)      TO  WRK-Z9
      **            MOVE WRK-Z9   TO  HC17B-SAKENSU (IDX)
      *** 20040405 追加
      *--- 2008/12/04 CHANGE START -------------------------------------
      *            COMPUTE  WRK-Z9  =  SUM-SKEN(IDX)  +  SUM-SAKEN(IDX)
                 MOVE   SUM-SAKEN(IDX)      TO   WRK-Z9
      *--- 2008/12/04 CHANGE END ---------------------------------------
                 MOVE   WRK-Z9              TO   HC17B-SKENSU (IDX)
                 MOVE   SUM-SKEN (IDX)      TO   WRK-Z9
                 MOVE   WRK-Z9(2:7)         TO   WRK-TMP-K
                 MOVE   1                   TO   WRK-COUNTS
                 INSPECT WRK-TMP-K  TALLYING
                        WRK-COUNTS FOR LEADING SPACE
                 MOVE   WRK-TMP-K  TO   HC17B-SAKENSU(IDX)(2:7)
                 MOVE   "("        TO   HC17B-SAKENSU(IDX)(WRK-COUNTS:1)
                 MOVE   ")"        TO   HC17B-SAKENSU(IDX)(9:1)
      */20040417 編集 Y.Takahashi
      * 20050914 MOTO
                  MOVE    SUM-OKEN (IDX)       TO  WRK-Z9
                  MOVE    WRK-Z9   TO  HC17B-OKENSU  (IDX)
      * 20050914 MOTO
               END-IF
             END-IF 
           END-PERFORM
      *
      *    医療収益計、窓口請求金額合計、窓口領収金額合計
      *    金額合計
           MOVE    SUM-SYUEKI      TO   WRK-S9
           MOVE    WRK-S9          TO   HC17B-GSYUEKI
           MOVE    SUM-SEIKYU      TO   WRK-S9
           MOVE    WRK-S9          TO   HC17B-GSEIKYU
           MOVE    SUM-RYOSYU      TO   WRK-S9
           MOVE    WRK-S9          TO   HC17B-GRYOSYU
      *
      *    再診
           MOVE    SUM-RSYUEKIKEN  TO   WRK-Z9
           MOVE    WRK-Z9          TO   HC17B-RSYUEKIKEN
      *
      *    初診
             IF   WRK-NYUGAIKBN =   "2"
      * 20040417 編集 Y.Takahashi
      **          MOVE  SUM-SSYUEKIKEN       TO   WRK-Z9
      **          MOVE  WRK-Z9               TO   HC17B-SSYUEKIKEN
      *** 20040405 追加
      **          MOVE  SUM-SASYUEKIKEN      TO   WRK-Z9
      **          MOVE  WRK-Z9               TO   HC17B-SASYUEKIKEN
      *--- 2008/12/04 CHANGE START -------------------------------------
      *          COMPUTE WRK-Z9  =  SUM-SSYUEKIKEN  +  SUM-SASYUEKIKEN
               MOVE    SUM-SASYUEKIKEN     TO  WRK-Z9
      *--- 2008/12/04 CHANGE END ---------------------------------------
               MOVE    WRK-Z9              TO  HC17B-SSYUEKIKEN
               MOVE    SUM-SSYUEKIKEN      TO  WRK-Z9
               MOVE    WRK-Z9(2:7)         TO  WRK-TMP-K
               MOVE    1                   TO  WRK-COUNTS
               INSPECT WRK-TMP-K  TALLYING
                       WRK-COUNTS FOR LEADING SPACE
               MOVE    WRK-TMP-K  TO  HC17B-SASYUEKIKEN(2:7)
               MOVE    "("        TO  HC17B-SASYUEKIKEN(WRK-COUNTS:1)
               MOVE    ")"        TO  HC17B-SASYUEKIKEN(9:1)
      */20040417 編集 Y.Takahashi
      * 20040405 追加
      * 20050914 MOTO
      *    その他
               MOVE  SUM-OSYUEKIKEN    TO   WRK-Z9
               MOVE  WRK-Z9            TO   HC17B-OSYUEKIKEN
      * 20050914 MOTO
             END-IF
      *
      *20030530 追加
      *    受診者数
      ***       MOVE  SUM-JYUSHINKEN       TO   WRK-Z9
      ***       MOVE  WRK-Z9               TO   HC17B-JYUSHINCNT
      *20030530 追加
      *
           END-IF
      * 
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                   TO   SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM  TO   SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
      *****MOVE   "HC17"               TO  SPRT-PRTID
      ***   変更(2002.11.26)
      ***** 03/09/29 
      ***  20040916 MOTOHASI MONPEに変更
      ***     MOVE   "HC17V012S"          TO  SPRT-PRTID
      ***     MOVE   "A00000D501"         TO  SPRT-PRTID
      * 20080325 H20年4月改正対応START tanaka
           IF   WRK-PARA-DENPPRTYMD  <  "20080401"
             MOVE   "A00000D501O.red"      TO   SPRT-PRTID
           ELSE 
             MOVE   "A00000D501N.red"      TO   SPRT-PRTID
           END-IF
      * 20080325 H20年4月改正対応END
      ***  20040916 MOTOHASI MONPEに変更
      *****
      ***   変更(2002.11.26)
      ***     MOVE    "日計表（伝票発行日）"
      ***     MOVE    "日計表（合計）" TO  SPRT-TITLE
           MOVE    "日計表合計（窓口領収金）"   TO   SPRT-TITLE
      * 20031211 帳票タイトル変更 
           MOVE    HC17B   TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID    TO   SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID      TO   SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM     TO   SPRT-PRTNM
      ***  20030814 削除
      ***  site-libを実行環境にする為コメントアウト
      ***     MOVE    "1"          TO  SPRT-SITEKBN
      ***  20030814 削除
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF    SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE    "印刷ＤＢに更新できませんでした"   TO   WRK-RECEERR
             PERFORM    500-ERR-HENSYU-SEC
           END-IF
      *
           .
       330-TERM-KA-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *          帳票<入外区分別全科合計>印刷処理
      *****************************************************************
      *    
       330-TERM-GK-PRINT-SEC         SECTION.
      *
           INITIALIZE            HC17B
      *
           MOVE    WRK-PARA-DENPPRTYMDWH     TO   HC17B-SYSYMD
      *
      ***    PERFORM  VARYING  IDX2   FROM 1  BY  1
      ***                       UNTIL       IDX2   >         5 
      ***         MOVE    WRK-TJIHI(IDX2)   TO   HC17B-TJIHI(IDX2)
      ***    END-PERFORM
      *
      * 20030407 追加  入外区分名称設定
           EVALUATE WRK-NYUGAIKBN
             WHEN "1"
               MOVE "入院"         TO  HC17B-NYUGAIKBN
      *    入院自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
      *=== 自費追加対応 UP By Takashima(2006/10/24) start
      *                      UNTIL    IDX2   >    5 
                        UNTIL    IDX2   >  10 
      *=== 自費追加対応 UP By Takashima(2006/10/24) end
                 MOVE     WRK-TJIHIN(IDX2)   TO   HC17B-TJIHI(IDX2)
               END-PERFORM
      *=== 自費追加対応 UP By Takashima(2006/10/24) start
      *             MOVE  "請求件数"    TO  HC17B-KENSU1
               MOVE   "請求件数"    TO   HC17B-KENSU1(1)
               MOVE   "請求件数"    TO   HC17B-KENSU1(2)
      *=== 自費追加対応 UP By Takashima(2006/10/24) end
             WHEN  "2"
               MOVE   "外来"         TO   HC17B-NYUGAIKBN
      ***       外来自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
      *=== 自費追加対応 UP By Takashima(2006/10/24) start
      *                      UNTIL    IDX2   >    5 
                         UNTIL    IDX2  >  10 
      *=== 自費追加対応 UP By Takashima(2006/10/24) end
                 MOVE   WRK-TJIHI(IDX2)   TO   HC17B-TJIHI(IDX2)
               END-PERFORM
      *=== 自費追加対応 UP By Takashima(2006/10/24) start
      *             MOVE  "再診件数"    TO  HC17B-KENSU1
      *             MOVE  "初診件数"    TO  HC17B-KENSU2
      ** 20050914 MOTO
      *             MOVE  "その他件数"    TO  HC17B-KENSU4
      ** 20050914 MOTO
      ** 20040405 追加
      *             MOVE  "(新患数)"    TO  HC17B-KENSU3
      **             MOVE  "新患件数"    TO  HC17B-KENSU3
      **             MOVE  "再患件数"    TO  HC17B-KENSU4
      ** 20040405 追加
               MOVE    "再診件数"      TO   HC17B-KENSU1(1)
               MOVE    "初診件数"      TO   HC17B-KENSU2(1)
               MOVE    "その他件数"    TO   HC17B-KENSU4(1)
               MOVE    "(新患数)"      TO   HC17B-KENSU3(1)
               MOVE    "再診件数"      TO   HC17B-KENSU1(2)
               MOVE    "初診件数"      TO   HC17B-KENSU2(2)
               MOVE    "その他件数"    TO   HC17B-KENSU4(2)
               MOVE    "(新患数)"      TO   HC17B-KENSU3(2)
      *=== 自費追加対応 UP By Takashima(2006/10/24) end
      * 20030530 追加  受信者数名称設定
               MOVE  "受診者数"        TO   HC17B-TJYUSHIN
      * 20030530 追加  受信者数名称設定
           END-EVALUATE
      * 20030407 追加  入外区分名称設定
      * 診療科名称 
      * 20030620 変更
      ***     MOVE  "全科合計"    TO  HC17B-SRYKA
           EVALUATE WRK-NYUGAIKBN
             WHEN "1"
               IF  WRK-PRNSRKA-CNT  >   1
      *    診療科が２つ以上の場合は全科合計を設定する。
                 MOVE  "全科合計"     TO   HC17B-SRYKA
               ELSE
      *    診療科名称取得
      *    診療科が１つの場合は診療科名称を設定する。
                 INITIALIZE           SYS-1005-REC
                 MOVE  "1005"         TO   SYS-1005-KANRICD
                 MOVE  WRK-SRYKA      TO   SYS-1005-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
                 MOVE  SPA-HOSPNUM    TO   SYS-1005-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
                 MOVE  WRK-PARA-DENPPRTYMD   TO   WRK-SYMD
      ***  20050914  MOTO
                 MOVE  WRK-PARA-DENPPRTYMD   TO   SYS-1005-EDYUKYMD
                                                  SYS-1005-STYUKYMD
      ***  20050914  MOTO
                 MOVE  SYS-1005-REC   TO  MCPDATA-REC
                 PERFORM 820-SYSKANRI-READ-SEC
                 IF    FLG-SYSKANRI2   =   ZERO
                   MOVE  MCPDATA-REC          TO   SYS-1005-REC
                   MOVE  SYS-1005-SRYKANAME   TO   HC17B-SRYKA
                 ELSE
                   MOVE  SPACE                TO   HC17B-SRYKA
                 END-IF
               END-IF
             WHEN "2"
               IF  WRK-PRGSRKA-CNT  >   1
      *    診療科が２つ以上の場合は全科合計を設定する。
                 MOVE  "全科合計"    TO  HC17B-SRYKA
               ELSE
      *    診療科名称取得
      *    診療科が１つの場合は診療科名称を設定する。
                 INITIALIZE                      SYS-1005-REC
                 MOVE   "1005"              TO   SYS-1005-KANRICD
                 MOVE   WRK-SRYKA           TO   SYS-1005-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
                 MOVE   SPA-HOSPNUM         TO   SYS-1005-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
                 MOVE   WRK-PARA-DENPPRTYMD TO   WRK-SYMD
      ***  20050914  MOTO
                 MOVE   WRK-PARA-DENPPRTYMD TO   SYS-1005-EDYUKYMD
                                                 SYS-1005-STYUKYMD
      ***  20050914  MOTO
                 MOVE   SYS-1005-REC        TO   MCPDATA-REC
                 PERFORM 820-SYSKANRI-READ-SEC
                 IF    FLG-SYSKANRI2        =   ZERO
                   MOVE   MCPDATA-REC       TO   SYS-1005-REC
                   MOVE   SYS-1005-SRYKANAME   TO   HC17B-SRYKA
                 ELSE
                   MOVE   SPACE             TO   HC17B-SRYKA
                 END-IF
               END-IF
           END-EVALUATE
      * 20030620 変更
      *
           IF  FLG-GET = ZERO
      *    対象データが１件も存在しなかった場合
             MOVE  "＊＊＊  対象データが１件も存在しません。＊＊＊"
                     TO  HC17B-JOBMSG
             MOVE    1                TO   WRK-PAGE
             MOVE    WRK-PAGE         TO   HC17B-PAGE
           ELSE
      *
      *    対象データが１件以上存在した場合
             ADD     1                TO   CNT-PAGE
             MOVE    CNT-PAGE         TO   WRK-PAGE
             MOVE    WRK-PAGE         TO   HC17B-PAGE
      *  保険点数、件数合計
             MOVE    SUM-HKNTEN-GK    TO   WRK-Z9
      *=== 自費追加対応 UP By Takashima(2006/10/24) start
      *       MOVE    WRK-Z9   TO  HC17B-GHKNTEN
             MOVE    WRK-Z9           TO   HC17B-GHKNKEN
      *=== 自費追加対応 UP By Takashima(2006/10/24) end
             IF   WRK-NYUGAIKBN   =   "2" 
      * 20040417 編集 Y.Takahasshi
      **          MOVE    SUM-SHKNKEN-GK   TO  WRK-Z9
      **          MOVE    WRK-Z9   TO  HC17B-SHKNKEN
      *** 20040405 追加
      **          MOVE    SUM-SAHKNKEN-GK  TO  WRK-Z9
      **          MOVE    WRK-Z9   TO  HC17B-SAHKNKEN
      ** 20040405 追加
      *--- 2008/12/04 CHANGE START -------------------------------------
      *          COMPUTE WRK-Z9  =  SUM-SHKNKEN-GK  +  SUM-SAHKNKEN-GK
                MOVE    SUM-SAHKNKEN-GK     TO  WRK-Z9
      *--- 2008/12/04 CHANGE END ---------------------------------------
                MOVE    WRK-Z9              TO   HC17B-SHKNKEN
                MOVE    SUM-SHKNKEN-GK      TO   WRK-Z9
                MOVE    WRK-Z9(2:7)         TO   WRK-TMP-K
                MOVE    1                   TO   WRK-COUNTS
                INSPECT WRK-TMP-K  TALLYING
                        WRK-COUNTS FOR LEADING  SPACE
                MOVE    WRK-TMP-K           TO   HC17B-SAHKNKEN(2:7)
                MOVE    "("          TO   HC17B-SAHKNKEN(WRK-COUNTS:1)
                MOVE    ")"          TO   HC17B-SAHKNKEN(9:1)
      */20040417 編集 Y.Takahasshi
      * 20050914 MOTO
                MOVE    SUM-OHKNKEN-GK      TO   WRK-Z9
                MOVE    WRK-Z9              TO   HC17B-OHKNKEN
      * 20050914 MOTO
             END-IF
             MOVE    SUM-RHKNKEN-GK         TO   WRK-Z9
             MOVE    WRK-Z9                 TO   HC17B-RHKNKEN
      *
      *  保険別金額、件数合計
             PERFORM  VARYING IDX  FROM    1   BY  1
      *=== 自費追加対応 UP By Takashima(2006/10/24) start
      *             UNTIL    IDX   >    14
      *       IF    IDX = 7
      *         OR  IDX = 8
      *         OR  IDX = 9
      *         OR  IDX = 10
      *         OR  IDX = 11
      * 20080325 H20年4月改正対応START tanaka
      *             UNTIL    IDX   >    19
                   UNTIL    IDX   >  20
      * 20080325 H20年4月改正対応END
             IF    IDX = 7
                 OR  IDX = 8
                 OR  IDX = 9
                 OR  IDX = 10
                 OR  IDX = 11
                 OR  IDX = 12
                 OR  IDX = 13
                 OR  IDX = 14
                 OR  IDX = 15
                 OR  IDX = 16
      *=== 自費追加対応 UP By Takashima(2006/10/24) end
               COMPUTE  IDX8  =  IDX  -  6
               EVALUATE WRK-NYUGAIKBN
                 WHEN "1"
      *  入院自費項目の設定
      *** 20050404  MOTO  削除
      ***               IF  WRK-TJIHIN-FLG(IDX8) =  1
      *  自費エリアは有効である場合に金額、件数を転送する。
      *--- 2008/12/16 CHANGE START -------------------------------------
      *             MOVE   SUM-MONEY-GK(IDX)  TO   WRK-Z9 
      *             MOVE   WRK-Z9             TO   HC17B-GMONEY(IDX)
                   MOVE   SUM-MONEY-GK(IDX)  TO   WRK-S9 
                   MOVE   WRK-S9             TO   HC17B-GMONEY(IDX)
      *--- 2008/12/16 CHANGE END ---------------------------------------
                   MOVE   SUM-RKEN-GK(IDX)   TO   WRK-Z9
                   MOVE   WRK-Z9             TO   HC17B-RKENSU(IDX)
      ***                    MOVE  SUM-SKEN-GK (IDX)   TO  WRK-Z9
      ***                    MOVE  WRK-Z9   TO  HC17B-SKENSU  (IDX)
      ***               END-IF
                 WHEN "2"
      *  外来自費項目の設定
      *** 20050404  MOTO  削除
      ***               IF  WRK-TJIHI-FLG(IDX8) =  1
      *  自費エリアは有効である場合に金額、件数を転送する。
      *--- 2008/12/16 CHANGE START -------------------------------------
      *             MOVE   SUM-MONEY-GK(IDX)  TO   WRK-Z9 
      *             MOVE   WRK-Z9             TO   HC17B-GMONEY(IDX)
                   MOVE   SUM-MONEY-GK(IDX)  TO   WRK-S9 
                   MOVE   WRK-S9             TO   HC17B-GMONEY(IDX)
      *--- 2008/12/16 CHANGE END ---------------------------------------
                   MOVE   SUM-RKEN-GK(IDX)   TO   WRK-Z9
                   MOVE   WRK-Z9             TO   HC17B-RKENSU(IDX)
      * 20050914 MOTO
                   MOVE   SUM-OKEN-GK(IDX)   TO   WRK-Z9
                   MOVE   WRK-Z9             TO   HC17B-OKENSU(IDX)
      * 20050914 MOTO
      * 20040417 編集 Y.Takahashi
      **                 MOVE  SUM-SKEN-GK (IDX)  TO  WRK-Z9
      **                 MOVE  WRK-Z9   TO  HC17B-SKENSU  (IDX)
      *** 20040405 追加
      **                 MOVE  SUM-SAKEN-GK (IDX) TO  WRK-Z9
      **                 MOVE  WRK-Z9   TO  HC17B-SAKENSU (IDX)
      *** 20040405 追加
      *--- 2008/12/04 CHANGE START -------------------------------------
      *                 COMPUTE  WRK-Z9   =   SUM-SKEN-GK(IDX)
      *                                   +   SUM-SAKEN-GK(IDX)
                   MOVE    SUM-SAKEN-GK(IDX) TO   WRK-Z9
      *--- 2008/12/04 CHANGE END ---------------------------------------
                   MOVE    WRK-Z9            TO   HC17B-SKENSU(IDX)
                   MOVE    SUM-SKEN-GK(IDX)  TO   WRK-Z9
                   MOVE    WRK-Z9(2:7)       TO   WRK-TMP-K
                   MOVE    1                 TO   WRK-COUNTS
                   INSPECT WRK-TMP-K  TALLYING
                           WRK-COUNTS FOR LEADING SPACE
                   MOVE    WRK-TMP-K     TO   HC17B-SAKENSU(IDX)(2:7)
                   MOVE    "("  TO   HC17B-SAKENSU(IDX)(WRK-COUNTS:1)
                   MOVE    ")"           TO   HC17B-SAKENSU(IDX)(9:1)
      */20040417 編集 Y.Takahashi
      ***               END-IF
               END-EVALUATE
             ELSE
      *--- 2008/12/16 CHANGE START -------------------------------------
      *         MOVE    SUM-MONEY-GK(IDX)    TO   WRK-Z9
      *         MOVE    WRK-Z9               TO   HC17B-GMONEY(IDX)
               MOVE    SUM-MONEY-GK(IDX)    TO   WRK-S9
               MOVE    WRK-S9               TO   HC17B-GMONEY(IDX)
      *--- 2008/12/16 CHANGE END ---------------------------------------
               MOVE    SUM-RKEN-GK(IDX)     TO   WRK-Z9
               MOVE    WRK-Z9               TO   HC17B-RKENSU(IDX)
               IF   WRK-NYUGAIKBN   =  "2" 
      * 20040417 編集 Y.Takahashi
      **            MOVE    SUM-SKEN-GK (IDX)       TO  WRK-Z9
      **            MOVE    WRK-Z9   TO  HC17B-SKENSU  (IDX)
      *** 20040405 追加
      **            MOVE    SUM-SAKEN-GK (IDX)      TO  WRK-Z9
      **            MOVE    WRK-Z9   TO  HC17B-SAKENSU  (IDX)
      *** 20040405 追加
      *--- 2008/12/04 CHANGE START -------------------------------------
      *            COMPUTE WRK-Z9   =   SUM-SKEN-GK(IDX)
      *                             +   SUM-SAKEN-GK(IDX)
                 MOVE    SUM-SAKEN-GK(IDX)  TO   WRK-Z9
      *--- 2008/12/04 CHANGE END ---------------------------------------
                 MOVE    WRK-Z9             TO   HC17B-SKENSU(IDX)
                 MOVE    SUM-SKEN-GK (IDX)  TO   WRK-Z9
                 MOVE    WRK-Z9(2:7)        TO   WRK-TMP-K
                 MOVE    1                  TO   WRK-COUNTS
                 INSPECT  WRK-TMP-K  TALLYING
                          WRK-COUNTS FOR LEADING SPACE
                 MOVE     WRK-TMP-K   TO   HC17B-SAKENSU(IDX)(2:7)
                 MOVE    "("     TO   HC17B-SAKENSU(IDX)(WRK-COUNTS:1)
                 MOVE    ")"     TO   HC17B-SAKENSU(IDX)(9:1)
      */20040417 編集 Y.Takahashi
      * 20050914 MOTO
                 MOVE    SUM-OKEN-GK(IDX)   TO   WRK-Z9
                 MOVE    WRK-Z9             TO   HC17B-OKENSU(IDX)
      * 20050914 MOTO
               END-IF
             END-IF 
           END-PERFORM
      *
      *    医療収益計、窓口請求金額合計、窓口領収金額合計
      *    金額合計
           MOVE  SUM-SYUEKI-GK  TO   WRK-S9
           MOVE  WRK-S9         TO   HC17B-GSYUEKI
           MOVE  SUM-SEIKYU-GK  TO   WRK-S9
           MOVE  WRK-S9         TO   HC17B-GSEIKYU
           MOVE  SUM-RYOSYU-GK  TO   WRK-S9
           MOVE  WRK-S9         TO   HC17B-GRYOSYU
      *
      *    再診
           MOVE  SUM-RSYUEKIKEN-GK    TO   WRK-Z9
           MOVE  WRK-Z9               TO   HC17B-RSYUEKIKEN
      *
      *    初診
           IF WRK-NYUGAIKBN   =  "2" 
      * 20040417 編集 Y.Takahashi
      **          MOVE  SUM-SSYUEKIKEN-GK    TO   WRK-Z9
      **          MOVE  WRK-Z9               TO   HC17B-SSYUEKIKEN
      *** 20040405 追加
      **          MOVE  SUM-SASYUEKIKEN-GK   TO   WRK-Z9
      **          MOVE  WRK-Z9               TO   HC17B-SASYUEKIKEN
      *** 20040405 追加 
      *--- 2008/12/04 CHANGE START -------------------------------------
      *          COMPUTE  WRK-Z9  =  SUM-SSYUEKIKEN-GK
      *                           +  SUM-SASYUEKIKEN-GK
             MOVE    SUM-SASYUEKIKEN-GK  TO   WRK-Z9
      *--- 2008/12/04 CHANGE END -------------------------------------
             MOVE    WRK-Z9              TO   HC17B-SSYUEKIKEN
             MOVE    SUM-SSYUEKIKEN-GK   TO   WRK-Z9
             MOVE    WRK-Z9(2:7)         TO   WRK-TMP-K
             MOVE    1                   TO   WRK-COUNTS
             INSPECT WRK-TMP-K  TALLYING
                     WRK-COUNTS FOR LEADING SPACE
             MOVE    WRK-TMP-K    TO   HC17B-SASYUEKIKEN(2:7)
             MOVE    "("          TO   HC17B-SASYUEKIKEN(WRK-COUNTS:1)
             MOVE    ")"          TO   HC17B-SASYUEKIKEN(9:1)
      *
      * 20050914 MOTO
      *  その他
             MOVE   SUM-OSYUEKIKEN-GK    TO   WRK-Z9
             MOVE   WRK-Z9               TO   HC17B-OSYUEKIKEN
      * 20050914 MOTO
      *
      *20030530 追加
      *  受診者数
             MOVE   SUM-JYUSHINKEN-GK    TO   WRK-Z9
             MOVE   WRK-Z9               TO   HC17B-JYUSHINCNT
      *20030530 追加
      *
             END-IF
           END-IF
      * 
           INITIALIZE                  ORCSPRTAREA
           MOVE   "INS"                      TO   SPRT-MODE
           MOVE   LNK-PRTKANRI-RENNUM        TO   SPRT-RENNUM
           MOVE   LNK-PRTKANRI-TBL-KEY       TO   SPRT-TBL-KEY
           MOVE   LNK-PRTKANRI-TBL-GROUP     TO   SPRT-TBL-GROUP
           MOVE   LNK-PRTKANRI-SRYYM         TO   SPRT-SRYYM
           MOVE   LNK-PRTKANRI-SKYYMD        TO   SPRT-SKYYMD
           MOVE   LNK-PRTKANRI-SHELLID       TO   SPRT-SHELLID
           MOVE   LNK-PRTKANRI-SHORI-RENNUM  TO   SPRT-SHORI-RENNUM
           MOVE   LNK-PRTKANRI-PRIORITY      TO   SPRT-PRIORITY
      *****MOVE   "HC17"               TO  SPRT-PRTID
      ***   変更(2002.11.26)
      ***** 03/09/29
      ***  20040916 MOTOHASI MONPEに変更
      ***     MOVE   "HC17V012S"          TO  SPRT-PRTID
      ***     MOVE   "A00000D501"         TO  SPRT-PRTID
      * 20080325 H20年4月改正対応START tanaka
      *     MOVE   "A00000D501.red"         TO  SPRT-PRTID
           IF    WRK-PARA-DENPPRTYMD   <  "20080401"
             MOVE   "A00000D501O.red"        TO   SPRT-PRTID
           ELSE 
             MOVE   "A00000D501N.red"        TO   SPRT-PRTID
           END-IF
      * 20080325 H20年4月改正対応END
      ***  20040916 MOTOHASI MONPEに変更
      ***
      ***   変更(2002.11.26)
      ***     MOVE    "日計表（伝票発行日）"
      * 20031211 帳票タイトル変更 
      ***     MOVE    "日計表（合計）"     TO  SPRT-TITLE
           MOVE    "日計表合計（窓口領収金）"    TO   SPRT-TITLE
      * 20031211 帳票タイトル変更 
           MOVE    HC17B                  TO   SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID    TO   SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID      TO   SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM     TO   SPRT-PRTNM
      ***  20030814 削除
      ***  site-libを実行環境にする為コメントアウト
      ***     MOVE    "1"          TO  SPRT-SITEKBN
      ***  20030814 削除
           CALL    "ORCSPRT"              USING
                                          ORCSPRTAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE    "印刷ＤＢに更新できませんでした"   TO   WRK-RECEERR
             PERFORM   500-ERR-HENSYU-SEC
           END-IF
      *
           .
       330-TERM-GK-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF   STS-RECEERR   =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT            RECEERR-FILE
      *
             MOVE    WRK-RECEERR       TO   RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *       
      *    ジョブ管理開始処理
             MOVE    "JBE"             TO   SJOBKANRI-MODE
             INITIALIZE                     JOBKANRI-REC
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
           MOVE    "STE"             TO   SJOBKANRI-MODE
           INITIALIZE                JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM       TO   JOB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-JOBID    TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID  TO   JOB-SHELLID
           MOVE    CNT-PAGE          TO   JOB-UPDCNT
           CALL    "ORCSJOB"         USING
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
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO   WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
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
           MOVE    WRK-PARA-HOSPNUM     TO   SMEI-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    "1"                  TO   SMEI-NYUGAIKBN
           MOVE    WRK-PARA-DENPPRTYMD  TO   SMEI-NYUHEN-YMD
      ***  @@@@-MOTOHASI  20031014
           MOVE    SYUMEI-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
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
      *                                  MCPDATA-REC
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             PERFORM 910-DBFETCH-SEC
           ELSE
             MOVE    1         TO   FLG-SYUMEI
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
           MOVE    "DBFETCH"          TO   MCP-FUNC
      ***  @@@@-MOTOHASI  20031014  取得キーを変更
      ***    MOVE    "SYUMEI-ALL"     TO  ORC-DBPATH
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"       TO   MCP-TABLE
           MOVE    "key4"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                      MCPAREA
                                      MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                      SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    ZERO             TO   FLG-SYUMEI
             MOVE    MCPDATA-REC      TO   SYUMEI-REC
           ELSE
             MOVE    1                TO   FLG-SYUMEI
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
           MOVE    WRK-PARA-HOSPNUM    TO   SMEI-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    "2"                 TO   SMEI-NYUGAIKBN
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
           IF   MCP-RC   =   ZERO
             PERFORM 920-DBFETCH-SEC
           ELSE
             MOVE    1                 TO   FLG-SYUMEI
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
      ***  @@@@-MOTOHASI  20031014  取得キーを変更
      ***    MOVE    "SYUMEI-ALL"     TO  ORC-DBPATH
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
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
      *     MOVE    "DBCLOSE"        TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
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
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       900-JYURRK-READ-SEC                 SECTION.
      *
           INITIALIZE                  JYURRK-REC
      *
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    WRK-SMEI-HOSPID(IDX-S)
      *                   TO   JYURRK-HOSPID
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)    TO   JYURRK-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-SMEI-PTID(IDX-S)       TO   JYURRK-PTID
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)  TO   JYURRK-NYUGAIKBN
           MOVE    WRK-SMEI-DENPNUM(IDX-S)    TO   JYURRK-DENPNUM
      ***  20041005  Ver 2.4.0対応による削除  MOTO
      ***     MOVE    "1"              TO   JYURRK-LASTKBN
      *
           MOVE    JYURRK-REC                 TO   MCPDATA-REC
           MOVE    "DBSELECT"                 TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_jyurrk"               TO   MCP-TABLE
           MOVE    "key6"                     TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                              MCPAREA
                                              MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                              SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"                TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_jyurrk"             TO   MCP-TABLE
             MOVE    "key6"                   TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"              USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                              MCPAREA
                                              MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                              SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC         TO   JYURRK-REC
               MOVE    ZERO                TO   FLG-JYURRK
             ELSE
               MOVE    1                   TO   FLG-JYURRK
             END-IF
           ELSE
             MOVE     1                    TO   FLG-JYURRK
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"             TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"         TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_jyurrk"            TO   MCP-TABLE
           MOVE    "key6"                  TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                           MCPAREA
                                           MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                           SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           .
       900-JYURRK-READ-EXT.
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
           MOVE    SMEI-HOSPNUM      TO   SYU-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    SMEI-NYUGAIKBN    TO   SYU-NYUGAIKBN
           MOVE    SMEI-PTID         TO   SYU-PTID
           MOVE    SMEI-DENPNUM      TO   SYU-DENPNUM
      *
           MOVE    SYUNOU-REC        TO   MCPDATA-REC
           MOVE    "DBSELECT"        TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"      TO   MCP-TABLE
           MOVE    "key"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                     MCPAREA
                                     MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                     SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"       TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_syunou"    TO   MCP-TABLE
             MOVE    "key"           TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"     USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                     MCPAREA
                                     MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                     SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF    MCP-RC   =   ZERO
               MOVE    MCPDATA-REC   TO   SYUNOU-REC
               MOVE    ZERO          TO   FLG-SYUNOU
             ELSE
               MOVE    1             TO   FLG-SYUNOU
             END-IF
           ELSE
             MOVE    1               TO   FLG-SYUNOU
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"              TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"   TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL     "MCPSUB"            USING
           MOVE    "tbl_syunou"      TO   MCP-TABLE
           MOVE    "key"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                     MCPAREA
                                     MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                     SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
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
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)    TO   SYU-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)  TO   SYU-NYUGAIKBN
           MOVE    WRK-SMEI-PTID(IDX-S)       TO   SYU-PTID
           MOVE    WRK-SMEI-DENPNUM(IDX-S)    TO   SYU-DENPNUM
      *
           MOVE    SYUNOU-REC                 TO   MCPDATA-REC
           MOVE    "DBSELECT"                 TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"               TO   MCP-TABLE
           MOVE    "key"                      TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                              MCPAREA
                                              MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                              SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"                TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_syunou"             TO   MCP-TABLE
             MOVE    "key"                    TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"              USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                              MCPAREA
                                              MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                              SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC            TO   SYUNOU-REC
               MOVE    ZERO                   TO   FLG-SYUNOU
             ELSE
               MOVE    1                      TO   FLG-SYUNOU
             END-IF
           ELSE
             MOVE    1                        TO   FLG-SYUNOU
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"             TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"            TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY   TO  MCP-PATH
      *     CALL     "MCPSUB"            USING
           MOVE    "tbl_syunou"               TO   MCP-TABLE
           MOVE    "key"                      TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
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
             IF    MCP-RC   =   ZERO
               MOVE    ZERO             TO   FLG-SYSKANRI
             ELSE
               MOVE    1                TO   FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                  TO   FLG-SYSKANRI
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key10"                    TO  MCP-PATHNAME
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
             IF    MCP-RC   =   ZERO
               MOVE    ZERO             TO   FLG-SYSKANRI2
             ELSE
               MOVE    1                TO   FLG-SYSKANRI2
             END-IF
           ELSE
             MOVE    1                  TO   FLG-SYSKANRI2
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
      *         CALL    "MCPSUB"        USING
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
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"                 USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
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
           MOVE    PTINF-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
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
             MOVE    "DBFETCH"         TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *       CALL    "MCPSUB"             USING
             MOVE    "tbl_ptinf"       TO   MCP-TABLE
             MOVE    "key"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC     TO   PTINF-REC
               MOVE    ZERO            TO   FLG-PTINF
             ELSE
               MOVE    1               TO   FLG-PTINF
               INITIALIZE              PTINF-REC
             END-IF
           ELSE
             MOVE    1                 TO   FLG-PTINF
             INITIALIZE                PTINF-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptinf"         TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
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
      *****************************************************************
      *    患者保険マスター読込処理
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTHKNINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-PTHKNINF-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_pthkninf"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC    =   ZERO
               MOVE    MCPDATA-REC         TO   PTHKNINF-REC
               MOVE    ZERO                TO   FLG-PTHKNINF
             ELSE
               MOVE    1                   TO   FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
             END-IF
           ELSE
             MOVE      1                   TO   FLG-PTHKNINF
             INITIALIZE                    PTHKNINF-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTHKNINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_pthkninf"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
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
           MOVE    HKNCOMBI-REC        TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_hkncombi"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC     TO   HKNCOMBI-REC
               MOVE    ZERO            TO   FLG-HKNCOMBI
             ELSE
               MOVE    1               TO   FLG-HKNCOMBI
               INITIALIZE              HKNCOMBI-REC
             END-IF
           ELSE
             MOVE    1                 TO   FLG-HKNCOMBI
             INITIALIZE                HKNCOMBI-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    診療会計情報読込処理
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    SRYACCT-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryacct"         TO   MCP-TABLE
           MOVE    "key3"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                         MCPAREA
                                         MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF    MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_sryacct"       TO   MCP-TABLE
             MOVE    "key3"              TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                         MCPAREA
                                         MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                         SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF    MCP-RC   =   ZERO
               MOVE    MCPDATA-REC   TO  SRYACCT-REC
               MOVE    ZERO              TO   FLG-SRYACCT
             ELSE
               MOVE    1                 TO   FLG-SRYACCT
               INITIALIZE                SRYACCT-REC
             END-IF
           ELSE
             MOVE    1                   TO   FLG-SRYACCT
             INITIALIZE                  SRYACCT-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryacct"         TO  MCP-TABLE
           MOVE    "key3"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                         MCPAREA
                                         MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                         SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *      
      *--- 2008/12/04 ADD START ---------------------------------------
      *****************************************************************
      *    受診履歴情報検索処理
      *****************************************************************
       900-JYURRK2-READ-SEC                SECTION.
      *
           INITIALIZE                  JYURRK2-REC
      *
           MOVE    JYURRK-HOSPNUM      TO   JYURRK2-HOSPNUM
           MOVE    JYURRK-PTID         TO   JYURRK2-PTID
           MOVE    JYURRK-NYUGAIKBN    TO   JYURRK2-NYUGAIKBN
           MOVE    "00000000"          TO   JYURRK2-SRYYMD
           MOVE    "99999999"          TO   JYURRK2-UPSRYYMD
      *
           MOVE    JYURRK2-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_jyurrk"        TO   MCP-TABLE
           MOVE    "key14"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC    =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
             MOVE    "tbl_jyurrk"      TO   MCP-TABLE
             MOVE    "key14"           TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC     TO   JYURRK2-REC
               MOVE    ZERO            TO   FLG-JYURRK2
             ELSE
               MOVE    1               TO   FLG-JYURRK2
             END-IF
           ELSE
             MOVE    1                 TO   FLG-JYURRK2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_jyurrk"        TO   MCP-TABLE
           MOVE    "key14"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-JYURRK2-READ-EXT.
           EXIT.
      *
      *--- 2008/12/04 ADD END -----------------------------------------
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"             TO   MCP-FUNC.
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
           MOVE    "DBSTART"            TO   MCP-FUNC.
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
           .
       900-DBCLOSE-EXT.
           EXIT.

