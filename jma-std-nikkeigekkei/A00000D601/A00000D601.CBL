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
       PROGRAM-ID.             A00000D601.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 日計表合計（診療費請求明細）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/02/26    @@@@-半田     新規作成
      *  04/05/06    @@@@-本橋     項目名(入院料)を外来時はスペースにする
      *  04/05/20    @@@@-有村     保険給付額、自費負担額集計方法修正
      *  04/09/16    @@@@-本橋     MONPE化
      *  04/10/06    @@@@-本橋     日医標準レセプトソフトver2.4.0対応による
      *                            受診履歴テーブル−最終区分(lastkbn)の削除
      *  05/09/14    @@@@-本橋     その他件数欄を追加
      *                            ORCMCPSUB → MCPSUBに変更
      *  06/04/01    @@@@-T.T      同日再診時の集計誤りの修正
      *  06/08/01    @@@@-高嶋     sarge対応
      *  06/10/30    @@@@-高嶋     基本診療項目追加対応
      *  06/12/20    @@@@-高嶋     自賠責計算式変更対応
      *  06/12/22    @@@@-内田     特別療養費（068,069）を国保で扱う
      *  07/03/20    @@@@-内田     消費税内税の影響対応
      *  07/06/22    @@@@-吉川     初診、再診、その他の振り分け及び
      *                            受診者数の集計誤りの修正
      *  07/07/04    @@@@-吉川     初診、再診、その他振分け追加修正
      *  07/09/10    @@@@-高嶋     4.0対応
      *  07/12/18    @@@@-吉川     在宅等が再診で集計される不具合を修正
      *  08/03/26    @@@@-田中     H20年4月改正対応
      *  08/06/10    @@@@-田中     健保準拠時の自賠責の計算を修正
      *  09/05/26    @@@@-門間     生活療養費・生活負担額が保険分金額・患者負担金額・
      *                            医療収益に加算されていない不具合を修正
      *  10/02/09    @@@@-田中     治験対応
      *  10/03/09    @@@@-田中     初診判定条件追加
      *  10/04/27    @@@@-田中     再診の判定条件を修正
      *  10/07/06    @@@@-門間     労災・自賠責保険時の初診の判定条件追加
      *  10/07/23    @@@@-門間     医療保険の初診判定条件を追加
      *  10/09/09    @@@@-田中     外来時に入院料を集計するよう修正
      *  10/10/22    @@@@-門間     初診判定条件を修正
      *****************************************************************
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
           COPY    "A00000D601.INC".
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
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  DEBUG-SW            PIC 9(01).
      *--- 2010/10/22 ADD START ----------------------------------------
           03  FLG-ACCTEND         PIC 9(01).
      *--- 2010/10/22 ADD END   ----------------------------------------
      *
      *  件数用フラグ領域
           03  FLG-KENSUU          PIC 9(01).
             05  FLG-HKN           PIC 9(01).
             05  FLG-SYUEKI        PIC 9(01).
             05  FLG-SEIKYU        PIC 9(01).
             05  FLG-RYOSYU        PIC 9(01).
             05  FLG-JIHI-G.
               07  FLG-JIHI        PIC 9(01)  OCCURS  5.
      *--- 2010/04/27 ADD START ----------------------------------------
           03  FLG-SRYACT          PIC 9(01).
      *--- 2010/04/27 ADD START ----------------------------------------
           03  FLG-JYURRK2         PIC 9(01).
      *--- 2010/04/27 ADD END ------------------------------------------
      *--- 2010/04/27 ADD END ------------------------------------------
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
      *--- 2010/04/27 ADD START ----------------------------------------
           03  IDX10                  PIC 9(02).
           03  IDX11                  PIC 9(02).
      *--- 2010/04/27 ADD END ------------------------------------------
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
           03  WRK-BIRTHDAY.
            05 WRK-BIRTHY            PIC 9(04).
            05 WRK-BIRTHM            PIC 9(02).
            05 WRK-BIRTHD            PIC 9(02).
           03  WRK-NENREI            PIC 9(02).
           03  WRK-NYUGAIKBN         PIC X(01).
           03  WRK-SRYKA             PIC X(02).
           03  WRK-PTID              PIC 9(10).
           03  WRK-FTNRATE           PIC 9(01)V9(02).
           03  WRK-KYFRATE           PIC 9(01)V9(02).
      *****03  WRK-IN                PIC X(01).
      *
           03  WRK-SYUHKN-KBN        PIC 9(01).
           03  WRK-MONEY             PIC 9(08).
           03  WRK-JIHI              PIC 9(08).
           03  WRK-JIHI2             PIC 9(08).
           03  WRK-JIHI3             PIC 9(08).
      * 20080326  H20年4月改正対応START tanaka
           03  WRK-JIHI4             PIC 9(08).
      * 20080326  H20年4月改正対応END
           03  WRK-4001-TENTANKA     PIC 9(02)V9(01).
           03  WRK-4001-KSNMONEY     PIC 9(08).
           03  WRK-RSI-MONEY1        PIC S9(09)V9(01). 
           03  WRK-RSI-MONEY2        PIC S9(09)V9(01). 
      *
      * 入院用印刷診療科件数 
           03  WRK-PRNSRKA-CNT       PIC 9(03).
      * 外来用印刷診療科件数 
           03  WRK-PRGSRKA-CNT       PIC 9(03).
      *
           03  WRK-RENNUM-X          PIC ZZ9.
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-Z9-5              PIC ZZZZ9. 
           03  WRK-Z9                PIC ZZZZZZZ9.
           03  WRK-ZZ                PIC ZZZZZZZZ.
           03  WRK-S9                PIC -------9.
           03  WRK-SZ                PIC -------Z.
      *--- 2010/04/27 ADD START ----------------------------------------
           03  WRK-DENPNUM       PIC 9(07).
      *--- 2010/04/27 ADD END ------------------------------------------
      *
      * 診療科別加算エリア
       01  SUM-AREA                 VALUE   ZERO.
           03  SUM-JYUSHINKEN       PIC 9(05).
           03  SUM-SHKNKEN          PIC 9(05).
           03  SUM-RHKNKEN          PIC 9(05).
           03  SUM-OHKNKEN          PIC 9(05).
           03  SUM-HKNTEN           PIC 9(09).
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) start
      *     03  SUM-TEN-TBL          OCCURS  11.
      * 20080326  H20年4月改正対応START tanaka
      *     03  SUM-TEN-TBL          OCCURS  15.
           03  SUM-TEN-TBL          OCCURS  16.
      * 20080326  H20年4月改正対応END
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) end
               05  SUM-TEN          PIC 9(09).
           03  SUM-HKNFTN           PIC S9(09).
           03  SUM-JIHIFTN          PIC S9(09).
           03  SUM-SEIKYU           PIC S9(09).
           03  SUM-RYOSYU           PIC S9(09).
      *
      * 入外区分別加算エリア
       01  SUM-AREA-GK              VALUE   ZERO.
           03  SUM-JYUSHINKEN-GK    PIC 9(05).
           03  SUM-SHKNKEN-GK       PIC 9(05).
           03  SUM-RHKNKEN-GK       PIC 9(05).
           03  SUM-OHKNKEN-GK       PIC 9(05).
           03  SUM-HKNTEN-GK        PIC 9(09).
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) start
      *     03  SUM-TEN-TBL-GK       OCCURS  11.
      * 20080326  H20年4月改正対応START tanaka
      *     03  SUM-TEN-TBL-GK       OCCURS  15.
           03  SUM-TEN-TBL-GK       OCCURS  16.
      * 20080326  H20年4月改正対応END
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) end
               05  SUM-TEN-GK       PIC 9(09).
           03  SUM-HKNFTN-GK        PIC S9(09).
           03  SUM-JIHIFTN-GK       PIC S9(09).
           03  SUM-SEIKYU-GK        PIC S9(09).
           03  SUM-RYOSYU-GK        PIC S9(09).
      *
       01  WRK-SMEI-REC.
           03  WRK-SMEI-TBL      OCCURS  15000.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         05  WRK-SMEI-HOSPID        PIC X(24).
               05  WRK-SMEI-HOSPNUM       PIC 9(02).
      *=== 4.0対応 By Takashima(2007/09/10) start
      *** (注) ソートキーの構成は日計表明細(ORCBD500)と異なる。
               05  WRK-SMEI-SORT-KEY. 
                 07  WRK-SMEI-NYUGAIKBN   PIC X(01).
                 07  WRK-SMEI-SRYKA       PIC X(02).
                 07  WRK-SMEI-PTID        PIC 9(10).
                 07  WRK-SMEI-DENPNUM     PIC 9(07).
                 07  WRK-SMEI-DENPEDANUM  PIC 9(02).
               05  WRK-SMEI-SKYMONEY      PIC S9(07).
               05  WRK-SMEI-NYUHEN-MONEY  PIC S9(07).
               05  WRK-SMEI-NYUHEN-YMD    PIC X(08).
      *
      *配列ソート時の退避エリア
      ***     03  WRK-SV-SMEI-TBL            PIC X(66). 
           03  WRK-SV-SMEI-TBL            PIC X(68).
      *
           03  WRK-SV-SMEI-DENPNUM        PIC 9(07).
      *
       01   WRK-JYURRK-AREA.
          03  WRK-JR-DENPNUM-SV           PIC  9(07).
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
          03  WRK-JR-TBL-SV               PIC  X(59). 
      *
       01   WRK-JYUSHIN-AREA.
         03   WRK-JYUSHIN-PTID-TBL        OCCURS 50000. 
           05   WRK-JYUSHIN-PTID          PIC  9(10).
      *
       01  WRK-CONS-AREA.
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973". 
      * 20080326  H20年4月改正対応START tanaka
      *    公害保険番号 
           03  WRK-CONS-KOGAI-HKNNUM   PIC X(03)   VALUE   "975". 
      * 20080326  H20年4月改正対応END
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
      *    患者労災マスタ
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
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
      *--- 2010/04/27 ADD START ----------------------------------------
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *--- 2010/04/27 ADD END ------------------------------------------
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
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
           MOVE    WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "A00000D601"    TO  JOB-PGID
           MOVE    "日計表合計（診療請求明細）"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
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
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SYSYMDWH 
      *
      *    対象年月日編集
           MOVE    WRK-PARA-DENPPRTYMD TO    WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-PARA-DENPPRTYMDWH 
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
      ***  20050914  MOTO
           MOVE  WRK-PARA-DENPPRTYMD   TO  SYS-1001-EDYUKYMD
                                           SYS-1001-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    SYS-1001-HOSPID     TO  WRK-PARA-HOSPID
               MOVE    SYS-1001-HOSPNUM     TO  WRK-PARA-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           ELSE    
               MOVE    "医療機関ＩＤが取得できませんでした。"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF                                     
      *  
      *    労災医療機関情報
           MOVE    SPACE                   TO  SYS-4001-REC
           INITIALIZE                          SYS-4001-REC
           MOVE    "4001"                  TO  SYS-4001-KANRICD
           MOVE    "*"                     TO  SYS-4001-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM             TO  SYS-4001-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD     TO  WRK-SYMD
      ***  20050914  MOTO
           MOVE  WRK-PARA-DENPPRTYMD   TO  SYS-4001-EDYUKYMD
                                           SYS-4001-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-4001-REC            TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
                   MOVE  MCPDATA-REC    TO  SYS-4001-REC
                   EVALUATE  SYS-4001-TENKBN
                       WHEN  "1"    
                             MOVE  11.5 TO  WRK-4001-TENTANKA
                       WHEN  "2"    
                             MOVE  12   TO  WRK-4001-TENTANKA
                       WHEN  OTHER    
                             MOVE  10   TO  WRK-4001-TENTANKA
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
           PERFORM              UNTIL   FLG-END     =   1
      *        収納情報集計処理
               PERFORM 320-BODY-HEN-SEC
           END-PERFORM
      *
      *    帳票編集<診療科合計ページ>印刷処理
           EVALUATE WRK-NYUGAIKBN
               WHEN "1"
                    IF  WRK-PRNSRKA-CNT  >   1
                        PERFORM 330-TERM-KA-PRINT-SEC
                    END-IF
               WHEN "2"
                    IF  WRK-PRGSRKA-CNT  >   1
                        PERFORM 330-TERM-KA-PRINT-SEC
                    END-IF
           END-EVALUATE
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
           MOVE 0  TO IDX-S
           PERFORM 910-DBSELECT-SEC
      *
      *    収納明細テーブルより変数配列に対象データを抽出する。
      *
           PERFORM              UNTIL   FLG-SYUMEI     =   1
      *    入力された日付と入返金日が一致しているかをチェック
      * 
            PERFORM 900-SYUNOU-READ-SEC
            INITIALIZE   PTINF-REC 
      *=== 4.0対応 By Takashima(2007/09/10) start
      *      MOVE SMEI-HOSPID TO PTINF-HOSPID
            MOVE SMEI-HOSPNUM TO PTINF-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
            MOVE SMEI-PTID   TO PTINF-PTID
            PERFORM 900-PTINF-READ-SEC
      *
      ***  収納情報にレコードが存在しないデータは抽出しない。
      ***  伝票状態区分 = 3(請求取消)は対象外データとして取り扱う
      ***  患者情報にレコードが存在しないデータは抽出しない。
      ***  テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う
             IF  ( WRK-PARA-DENPPRTYMD  =      SMEI-NYUHEN-YMD
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         AND WRK-PARA-HOSPID      =      SMEI-HOSPID )
               AND WRK-PARA-HOSPNUM      =      SMEI-HOSPNUM )
      *=== 4.0対応 By Takashima(2007/09/10) end
               AND  FLG-SYUNOU          =      ZERO
               AND  SYU-DENPJTIKBN      NOT =  "3"   
               AND  FLG-PTINF           =      ZERO
               AND  PTINF-TSTPTNUMKBN   NOT =  "1"   
      * 
                    ADD 1  TO IDX-S
      *=== 4.0対応 By Takashima(2007/09/10) start
      *              MOVE   SMEI-HOSPID 
      *                   TO    WRK-SMEI-HOSPID(IDX-S)
                    MOVE   SMEI-HOSPNUM 
                         TO    WRK-SMEI-HOSPNUM(IDX-S)
      *=== 4.0対応 By Takashima(2007/09/10) end
                    MOVE   SMEI-NYUGAIKBN 
                         TO    WRK-SMEI-NYUGAIKBN(IDX-S)
                    MOVE   SMEI-SRYKA 
                         TO    WRK-SMEI-SRYKA(IDX-S)
                    MOVE   SMEI-PTID 
                         TO    WRK-SMEI-PTID(IDX-S)
                    MOVE   SMEI-DENPNUM 
                         TO    WRK-SMEI-DENPNUM(IDX-S)
                    MOVE    SMEI-DENPEDANUM 
                         TO    WRK-SMEI-DENPEDANUM(IDX-S)
                    MOVE   SMEI-SKYMONEY 
                         TO    WRK-SMEI-SKYMONEY(IDX-S)
                    MOVE   SMEI-NYUHEN-MONEY 
                         TO    WRK-SMEI-NYUHEN-MONEY(IDX-S)
                    MOVE   SMEI-NYUHEN-YMD 
                         TO    WRK-SMEI-NYUHEN-YMD(IDX-S)
             END-IF
             PERFORM 910-DBFETCH-SEC
          END-PERFORM 
          PERFORM 910-DBCLOSE-SEC
      *
          PERFORM 920-DBSELECT-SEC
      *
      *    収納明細テーブルより変数配列に対象データを抽出する。
      *
           PERFORM              UNTIL   FLG-SYUMEI     =   1
      *    入力された日付と入返金日が一致しているかをチェック
      * 
            PERFORM 900-SYUNOU-READ-SEC
            INITIALIZE   PTINF-REC 
      *=== 4.0対応 By Takashima(2007/09/10) start
      *      MOVE SMEI-HOSPID TO PTINF-HOSPID
            MOVE SMEI-HOSPNUM TO PTINF-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
            MOVE SMEI-PTID   TO PTINF-PTID
            PERFORM 900-PTINF-READ-SEC
      *
      ***  収納情報にレコードが存在しないデータは抽出しない。
      ***  伝票状態区分 = 3(請求取消)は対象外データとして取り扱う
      ***  患者情報にレコードが存在しないデータは抽出しない。
      ***  テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う
             IF  ( WRK-PARA-DENPPRTYMD  =      SMEI-NYUHEN-YMD
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         AND WRK-PARA-HOSPID      =      SMEI-HOSPID )
               AND WRK-PARA-HOSPNUM      =      SMEI-HOSPNUM )
      *=== 4.0対応 By Takashima(2007/09/10) end
               AND  FLG-SYUNOU          =      ZERO
               AND  SYU-DENPJTIKBN      NOT =  "3"   
               AND  FLG-PTINF           =      ZERO
               AND  PTINF-TSTPTNUMKBN   NOT =  "1"   
      * 
                    ADD 1  TO IDX-S
      *=== 4.0対応 By Takashima(2007/09/10) start
                    MOVE   SMEI-HOSPNUM 
                         TO    WRK-SMEI-HOSPNUM(IDX-S)
      *=== 4.0対応 By Takashima(2007/09/10) end
                    MOVE   SMEI-NYUGAIKBN 
                         TO    WRK-SMEI-NYUGAIKBN(IDX-S)
                    MOVE   SMEI-SRYKA 
                         TO    WRK-SMEI-SRYKA(IDX-S)
                    MOVE   SMEI-PTID 
                         TO    WRK-SMEI-PTID(IDX-S)
                    MOVE   SMEI-DENPNUM 
                         TO    WRK-SMEI-DENPNUM(IDX-S)
                    MOVE    SMEI-DENPEDANUM 
                         TO    WRK-SMEI-DENPEDANUM(IDX-S)
                    MOVE   SMEI-SKYMONEY 
                         TO    WRK-SMEI-SKYMONEY(IDX-S)
                    MOVE   SMEI-NYUHEN-MONEY 
                         TO    WRK-SMEI-NYUHEN-MONEY(IDX-S)
                    MOVE   SMEI-NYUHEN-YMD 
                         TO    WRK-SMEI-NYUHEN-YMD(IDX-S)
             END-IF
             PERFORM 920-DBFETCH-SEC
          END-PERFORM 
          PERFORM 920-DBCLOSE-SEC
      *
      *  配列の最終レコード番号を設定
          MOVE IDX-S TO IDX-MAX
      *
          IF   IDX-MAX     >    1
      *    対象データが１件以上存在する場合は抽出フラグON       
            MOVE  1  TO  FLG-GET
      *    対象データが１件以上存在する場合は終了フラグOFF      
            MOVE  ZERO    TO   FLG-END
      *    配列に１件以上のデータが抽出されている場合はソーティング処理を行う。
      *    変数配列によるソーティング
      *    最小値より並び替えをする為にインデックスの設定
             MOVE 1  TO  IDX-S
             MOVE 2  TO  IDX-N
             PERFORM UNTIL IDX-S NOT  <  IDX-MAX
              PERFORM  UNTIL IDX-N  >  IDX-MAX
                IF WRK-SMEI-SORT-KEY(IDX-S) > 
                    WRK-SMEI-SORT-KEY(IDX-N) 
                      MOVE WRK-SMEI-TBL(IDX-N)
                           TO  WRK-SV-SMEI-TBL
                      MOVE WRK-SMEI-TBL(IDX-S)
                           TO  WRK-SMEI-TBL(IDX-N)
                      MOVE WRK-SV-SMEI-TBL 
                           TO  WRK-SMEI-TBL(IDX-S)
                END-IF
                ADD 1  TO  IDX-N
              END-PERFORM
      *    N番目の値を決定する為にインデックスの再設定
              ADD 1  TO  IDX-S
              COMPUTE IDX-N = IDX-S + 1
             END-PERFORM
          ELSE
            IF   IDX-MAX     =     ZERO
      *    対象データが１件も存在しない場合は抽出フラグOFF       
                  MOVE  ZERO    TO   FLG-GET
      *    対象データが１件も存在しない場合は終了フラグON      
                  MOVE  1            TO   FLG-END
            ELSE 
      *    対象データが１件でも存在する場合は抽出フラグオフON       
                  MOVE  1            TO  FLG-GET
      *    対象データが１件でも存在する場合は終了フラグOFF      
                  MOVE  ZERO    TO   FLG-END
            END-IF
          END-IF
      *
      *  配列用員インデックスの再設定      
          MOVE 1     TO   IDX-S
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
           MOVE    ZERO     TO  CNT-LINE
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO  WRK-NYUGAIKBN
           MOVE    WRK-SMEI-SRYKA(IDX-S)       TO  WRK-SRYKA
      *
           EVALUATE WRK-NYUGAIKBN
               WHEN "1"
      *** 入院診療科数カウントアップ  
                    MOVE   1    TO WRK-PRNSRKA-CNT
              WHEN "2"
      *** 外来診療科数カウントアップ  
                    MOVE   1    TO WRK-PRGSRKA-CNT
           END-EVALUATE
      *
           PERFORM   UNTIL   FLG-END     =   1
      ***   件数用フラグの初期化
            INITIALIZE   FLG-KENSUU
      *
      ***   収納テーブル再読み込み
            PERFORM 901-SYUNOU-READ-SEC
            IF    FLG-SYUNOU = ZERO
      *
      *     入外区分ブレイクチェック処理
            IF  WRK-SMEI-NYUGAIKBN(IDX-S)  NOT = WRK-NYUGAIKBN
      *      合計カウンターに加算
                ADD  SUM-SHKNKEN     TO     SUM-SHKNKEN-GK
                ADD  SUM-RHKNKEN     TO     SUM-RHKNKEN-GK
      ***  20050914  MOTO
                ADD  SUM-OHKNKEN     TO     SUM-OHKNKEN-GK
      ***  20050914  MOTO
                ADD  SUM-HKNTEN      TO     SUM-HKNTEN-GK
                PERFORM VARYING      IDX2   FROM  1  BY 1
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) start
      *                  UNTIL        IDX2   >    11
      * 20080326  H20年4月改正対応START tanaka
      *                  UNTIL        IDX2   >    15 
                        UNTIL        IDX2   >    16 
      * 20080326  H20年4月改正対応END
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) end
                    ADD SUM-TEN(IDX2) TO SUM-TEN-GK(IDX2)
                END-PERFORM
                ADD  SUM-HKNFTN      TO     SUM-HKNFTN-GK
                ADD  SUM-JIHIFTN     TO     SUM-JIHIFTN-GK
                ADD  SUM-SEIKYU      TO     SUM-SEIKYU-GK
                ADD  SUM-RYOSYU      TO     SUM-RYOSYU-GK
      *
      ***       入院の印刷診療科数が２科以上の場合に科別集計頁を印字する。
      ***          PERFORM 330-TERM-KA-PRINT-SEC
                IF   WRK-NYUGAIKBN   =      "1"
                 AND WRK-PRNSRKA-CNT >      1
                     PERFORM 330-TERM-KA-PRINT-SEC
                END-IF
      *
                PERFORM 330-TERM-GK-PRINT-SEC
                INITIALIZE                  SUM-AREA-GK
                INITIALIZE                  SUM-AREA
      *
      * ブレイクキー項目設定
                MOVE    WRK-SMEI-NYUGAIKBN(IDX-S) TO  WRK-NYUGAIKBN
                MOVE    WRK-SMEI-SRYKA(IDX-S)     TO  WRK-SRYKA
      *
      *    PTIDブレイクキー項目の設定
                INITIALIZE  WRK-PTID
      *
                EVALUATE WRK-NYUGAIKBN
                    WHEN "1"
      *** 入院診療科数カウントアップ  
                         ADD   1    TO WRK-PRNSRKA-CNT
                    WHEN "2"
      *** 外来診療科数カウントアップ  
                         ADD   1    TO WRK-PRGSRKA-CNT
                END-EVALUATE
      *
            END-IF
      *
      *     診療科ブレイクチェック処理
            IF  WRK-SMEI-SRYKA(IDX-S)  NOT = WRK-SRYKA
                PERFORM 330-TERM-KA-PRINT-SEC
      *      合計カウンターに加算
                ADD  SUM-SHKNKEN     TO     SUM-SHKNKEN-GK
                ADD  SUM-RHKNKEN     TO     SUM-RHKNKEN-GK
      ***  20050914  MOTO
                ADD  SUM-OHKNKEN     TO     SUM-OHKNKEN-GK
      ***  20050914  MOTO
                ADD  SUM-HKNTEN      TO     SUM-HKNTEN-GK
                PERFORM VARYING      IDX2   FROM  1  BY 1
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) start
      *                  UNTIL        IDX2   >    11 
      * 20080326  H20年4月改正対応START tanaka
      *                  UNTIL        IDX2   >    15 
                        UNTIL        IDX2   >    16
      * 20080326  H20年4月改正対応END
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) end
                    ADD SUM-TEN(IDX2) TO SUM-TEN-GK(IDX2)
                END-PERFORM
                ADD  SUM-HKNFTN      TO     SUM-HKNFTN-GK
                ADD  SUM-JIHIFTN     TO     SUM-JIHIFTN-GK
                ADD  SUM-SEIKYU      TO     SUM-SEIKYU-GK
                ADD  SUM-RYOSYU      TO     SUM-RYOSYU-GK
                INITIALIZE                  SUM-AREA
      *
      * ブレイクキー項目設定
                MOVE    WRK-SMEI-NYUGAIKBN(IDX-S) TO  WRK-NYUGAIKBN
                MOVE    WRK-SMEI-SRYKA(IDX-S)     TO  WRK-SRYKA
      *
      *    PTIDブレイクキー項目の設定
                 INITIALIZE  WRK-PTID
      *
                EVALUATE WRK-NYUGAIKBN
                    WHEN "1"
      *** 入院診療科数カウントアップ  
                         ADD   1    TO WRK-PRNSRKA-CNT
                    WHEN "2"
      *** 外来診療科数カウントアップ  
                         ADD   1    TO WRK-PRGSRKA-CNT
                END-EVALUATE
      *
            END-IF      
      *
      ***   伝票番号枝番が１の場合は保険点数、保険別請求額の明細を印字(開始)
             IF     WRK-SMEI-DENPEDANUM(IDX-S)   =     1
      ***         PERFORM 900-SYUNOU-READ-SEC
               IF  FLG-SYUNOU  = ZERO
                   PERFORM 320-BODY-HEN-SUB-SEC
      *  受診履歴集計
                IF SYU-NYUGAIKBN = "1"
      ***        入院の場合は初診、再診別に件数を集計する。
                   PERFORM 320-NYUIN-CNT-SEC
                ELSE
      ***        外来の場合は初診、再診別に件数を集計する。
      *--- 2010/04/27 CHANGE START -------------------------------------
      *             PERFORM 900-JYURRK-READ-SEC
      *             PERFORM 320-GAIRAI-CNT-SEC
                     PERFORM  900-JYURRK-SELECT-SEC
                     PERFORM  UNTIL  FLG-JYURRK  =  1
                       IF   JYURRK-DENPNUM  NOT =  WRK-DENPNUM
                         PERFORM  320-GAIRAI-CNT-SEC
                       END-IF
                       PERFORM  900-JYURRK-FETCH-SEC
                     END-PERFORM
                     PERFORM  900-JYURRK-CLOSE-SEC 
      *--- 2010/04/27 CHANGE END ---------------------------------------
                END-IF
               END-IF
             END-IF
      *
      *    請求金額
             INITIALIZE  WRK-SZ
             MOVE WRK-SMEI-SKYMONEY(IDX-S) TO WRK-SZ
             IF   WRK-SMEI-SKYMONEY(IDX-S)  NOT =  0
                   ADD WRK-SMEI-SKYMONEY(IDX-S)
                           TO   SUM-SEIKYU
                   MOVE      1    TO   FLG-SEIKYU
             END-IF
      *    領収金額
             INITIALIZE  WRK-SZ
             MOVE WRK-SMEI-NYUHEN-MONEY(IDX-S) TO WRK-SZ
             IF   WRK-SMEI-NYUHEN-MONEY(IDX-S)  NOT =  0
                   ADD WRK-SMEI-NYUHEN-MONEY(IDX-S)
                           TO  SUM-RYOSYU
                   MOVE      1    TO   FLG-RYOSYU
             END-IF
      *
            END-IF
      ***     収納テーブル収納テーブル再読み込み
      *
      *    配列インデックスのカウントアップ
             ADD  1     TO  IDX-S
      *    配列のデータを全件出力した場合は合計表の印刷を行う為にループより脱出
             IF IDX-S   >   IDX-MAX
                MOVE    1   TO   FLG-END
             END-IF 
      *
           END-PERFORM     
      *
      *      合計カウンターに加算
           ADD  SUM-SHKNKEN     TO     SUM-SHKNKEN-GK
           ADD  SUM-RHKNKEN     TO     SUM-RHKNKEN-GK
      ***  20050914  MOTO
           ADD  SUM-OHKNKEN     TO     SUM-OHKNKEN-GK
      ***  20050914  MOTO
           ADD  SUM-HKNTEN      TO     SUM-HKNTEN-GK
           PERFORM VARYING      IDX2   FROM  1  BY 1
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) start
      *             UNTIL        IDX2   >    11
      * 20080326  H20年4月改正対応START tanaka
      *             UNTIL        IDX2   >    15 
                   UNTIL        IDX2   >    16 
      * 20080326  H20年4月改正対応END
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) end
                   ADD SUM-TEN(IDX2)   TO    SUM-TEN-GK(IDX2)
           END-PERFORM
           ADD  SUM-HKNFTN      TO     SUM-HKNFTN-GK
           ADD  SUM-JIHIFTN     TO     SUM-JIHIFTN-GK
           ADD  SUM-SEIKYU      TO     SUM-SEIKYU-GK
           ADD  SUM-RYOSYU      TO     SUM-RYOSYU-GK
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
      *   患者情報読み込み       
          INITIALIZE                           PTINF-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *    MOVE    WRK-SMEI-HOSPID(IDX-S)   TO  PTINF-HOSPID
          MOVE    WRK-SMEI-HOSPNUM(IDX-S)   TO  PTINF-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
          MOVE    WRK-SMEI-PTID(IDX-S)     TO  PTINF-PTID
          PERFORM 900-PTINF-READ-SEC
      *   保険組合情報
          INITIALIZE                           HKNCOMBI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *    MOVE    WRK-SMEI-HOSPID(IDX-S)   TO  COMB-HOSPID
          MOVE    WRK-SMEI-HOSPNUM(IDX-S)   TO  COMB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
          MOVE    WRK-SMEI-PTID(IDX-S)     TO  COMB-PTID
          MOVE    SYU-HKNCOMBINUM          TO  COMB-HKNCOMBINUM
          PERFORM 900-HKNCOMBI-READ-SEC
      *   患者保険情報検索
          INITIALIZE                           PTHKNINF-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *    MOVE    WRK-SMEI-HOSPID(IDX-S)   TO  PTHKN-HOSPID
          MOVE    WRK-SMEI-HOSPNUM(IDX-S)   TO  PTHKN-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
          MOVE    WRK-SMEI-PTID(IDX-S)     TO  PTHKN-PTID
          MOVE    COMB-HKNID               TO  PTHKN-HKNID
          PERFORM 900-PTHKNINF-READ-SEC
      *
      *  保険番号情報読み込み
            PERFORM 900-HKNNUM-READ-SEC
            IF   FLG-HKNNUM  =   1
      ***        該当レコードが存在しない場合は単価を10円に設定する。
                 MOVE 10     TO  HKN-TENTNK
            END-IF
      *
      *    保険点数
            MOVE    ZERO  TO  SYU-TOTAL-HKNTEN
            PERFORM VARYING IDZ FROM    1   BY  1
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) start
      *              UNTIL   IDZ >       11
      * 20080326  H20年4月改正対応START tanaka
      *              UNTIL   IDZ >       15
                    UNTIL   IDZ >       16
      * 20080326  H20年4月改正対応END
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) end
                    ADD     SYU-HKNTEN (IDZ)
                            TO  SYU-TOTAL-HKNTEN
                    ADD     SYU-HKNTEN (IDZ)
                            TO  SUM-TEN (IDZ)
            END-PERFORM
            ADD     SYU-TOTAL-HKNTEN  TO  SUM-HKNTEN
            MOVE  1                   TO  FLG-HKN
      *
      * 保険種別  
            EVALUATE    SYU-SYUHKNNUM
                WHEN    SPACE
      ***               "公費単独"
                        MOVE  3          TO  WRK-SYUHKN-KBN
                WHEN    "980" THRU "989"    
      *--- 2010/02/09 ADD START ----------------------------------------
                WHEN    "900"  THRU  "919"
      *--- 2010/02/09 ADD END ------------------------------------------
      ***               "自費"
                        MOVE  4          TO  WRK-SYUHKN-KBN
                WHEN    WRK-CONS-ROUSAI-HKNNUM    
      ***               "労災"
                        MOVE  5          TO  WRK-SYUHKN-KBN
      *                 患者労災情報検索
                        INITIALIZE           PTRSIINF-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                  MOVE SYU-HOSPID  TO  PTRSI-HOSPID
                        MOVE SYU-HOSPNUM  TO  PTRSI-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
                        MOVE SYU-PTID    TO  PTRSI-PTID
                        MOVE COMB-HKNID  TO  PTRSI-HKNID
                        PERFORM 900-PTRSIINF-READ-SEC
                WHEN    WRK-CONS-JIBAI-HKNNUM
      ***               "自賠責"
                        MOVE  6          TO  WRK-SYUHKN-KBN
      *                 患者労災情報検索
                        INITIALIZE           PTRSIINF-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                  MOVE SYU-HOSPID  TO  PTRSI-HOSPID
                        MOVE SYU-HOSPNUM  TO  PTRSI-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
                        MOVE SYU-PTID    TO  PTRSI-PTID
                        MOVE COMB-HKNID  TO  PTRSI-HKNID
                        PERFORM 900-PTRSIINF-READ-SEC
      * 20080326  H20年4月改正対応START tanaka
                WHEN    WRK-CONS-KOGAI-HKNNUM
                        MOVE  7          TO  WRK-SYUHKN-KBN
                WHEN    "039"
                WHEN    "040"
                        MOVE  8          TO  WRK-SYUHKN-KBN
      * 20080326  H20年4月改正対応END
                WHEN    "060"
                WHEN    "067"
      *=== 特別療養費対応 By Uchida(2006/12/22) start
                WHEN    "068"
                WHEN    "069"
      *=== 特別療養費対応 By Uchida(2006/12/22) end
      ***               "国保"
                        MOVE  1          TO  WRK-SYUHKN-KBN
                WHEN    OTHER
      ***               "社保"
                        MOVE  2          TO  WRK-SYUHKN-KBN
            END-EVALUATE
      *
      * 主保険患者負担割合、主保険給付割合の算出
      *
            IF   WRK-SYUHKN-KBN   =  1
             OR  WRK-SYUHKN-KBN   =  2
      *          年齢の算出
                 MOVE  SYU-SRYYMD         TO  WRK-SYSYMD
                 MOVE  PTINF-BIRTHDAY     TO  WRK-BIRTHDAY
                 COMPUTE WRK-NENREI       =   WRK-SYSYY  - WRK-BIRTHY
                 IF    WRK-SYSMM          <   WRK-BIRTHM
                       COMPUTE WRK-NENREI =   WRK-NENREI - 1
                 ELSE
                  IF   WRK-SYSMM          =   WRK-BIRTHM
                   AND WRK-SYSDD          <   WRK-BIRTHD
                       COMPUTE WRK-NENREI =   WRK-NENREI - 1
                  END-IF 
                 END-IF
      *
                 IF    SYU-SYUHKNNUM =  "060"
                       EVALUATE   PTHKN-HOJOKBN
                           WHEN   ZERO
                                  MOVE   ZERO  TO  WRK-FTNRATE 
                           WHEN   1
                                  MOVE   0.1   TO  WRK-FTNRATE 
                           WHEN   2
                                  MOVE   0.2   TO  WRK-FTNRATE 
                           WHEN   3
                                  MOVE   0.3   TO  WRK-FTNRATE 
                           WHEN   OTHER
                                  MOVE   0.3   TO  WRK-FTNRATE 
                       END-EVALUATE 
                 ELSE 
                       EVALUATE   PTHKN-HOJOKBN
                           WHEN   SPACE
                                  MOVE   0.3   TO  WRK-FTNRATE 
                           WHEN   8
                                  MOVE   0.2   TO  WRK-FTNRATE 
                           WHEN   9
                                  MOVE   0.1   TO  WRK-FTNRATE 
                           WHEN   OTHER
                                  MOVE   ZERO  TO  WRK-FTNRATE 
                       END-EVALUATE 
                 END-IF
      *
      *          三歳未満、負担割合３割以上のチェック
                 IF    WRK-NENREI         <=  2
                  AND  WRK-FTNRATE        >   0.2
                       MOVE    0.2        TO  WRK-FTNRATE
                 END-IF
      *=== 特別療養費対応 By Uchida(2006/12/22) start
                 IF  SYU-SYUHKNNUM  =  "068"  OR  "069"
                     MOVE  1                   TO  WRK-FTNRATE
                 END-IF
      *=== 特別療養費対応 By Uchida(2006/12/22) end
                 COMPUTE WRK-KYFRATE   =  1 - WRK-FTNRATE 
            END-IF
      *
      *    保険負担学
               IF   WRK-SYUHKN-KBN   =  1
                OR  WRK-SYUHKN-KBN   =  2
                OR  WRK-SYUHKN-KBN   =  5
                OR  WRK-SYUHKN-KBN   =  6
      * 20080326  H20年4月改正対応START tanaka
                OR  WRK-SYUHKN-KBN   =  7
                OR  WRK-SYUHKN-KBN   =  8
      * 20080326  H20年4月改正対応END
                    EVALUATE  WRK-SYUHKN-KBN
                    WHEN  1 THRU 2
      * 20080326  H20年4月改正対応START tanaka
                    WHEN  8
      * 20080326  H20年4月改正対応END
      ***             国保、社保、後期高齢者の場合
      ***** 04/05/20 DEL 
      *                  COMPUTE WRK-MONEY ROUNDED     = 
      *                  (SYU-TOTAL-HKNTEN * HKN-TENTNK) * WRK-KYFRATE
      ***** 04/05/20 INS
                        IF     SYU-KOH-HKNNUM(1) =  "027"
                         COMPUTE  WRK-MONEY  =
                          (SYU-TOTAL-HKNTEN * 10) - SYU-KOH-COMPFTN(1)
                        ELSE
                         COMPUTE  WRK-MONEY  =
                          (SYU-TOTAL-HKNTEN * 10) - SYU-SYUCOMPFTN
                        END-IF
      *****
                        COMPUTE WRK-MONEY = 
                           WRK-MONEY
                        +  SYU-RYOYOHI-SKJ  
      ***  2009/05/26 ADD START ----------------------------------------
                        +  SYU-RYOYOHI-LIFE
      ***  2009/05/26 ADD END   ----------------------------------------
      ***           レセプト１円単位表示区分が"1"の場合は１０円未満を四捨五入する。
                    IF  HKN-RECEFTNDSPKBN  =  "1"
                        IF  WRK-MONEY(8:1) >   4
                            MOVE   ZERO    TO  WRK-MONEY(8:1)
                            ADD    10      TO  WRK-MONEY
                        ELSE
                            MOVE   ZERO    TO  WRK-MONEY(8:1)
                        END-IF 
                    END-IF 
                    WHEN  5
      ***             労災の場合
                        COMPUTE  WRK-MONEY  = 
                        (SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA)
                        COMPUTE WRK-MONEY           = 
                        WRK-MONEY
                        +  SYU-RSISHOSHIN-MONEY 
                        +  SYU-RSISAISHIN-MONEY 
                        +  SYU-RSISDO-MONEY 
                        +  SYU-RSIETC-MONEY
                        +  SYU-RYOYOHI-SKJ  
      ***  2009/05/26 ADD START ----------------------------------------
                        +  SYU-RYOYOHI-LIFE
      ***  2009/05/26 ADD END   ----------------------------------------
                    WHEN  6
      ***             自賠の場合
      *=== 自賠責計算式変更対応 By Takashima(2006/12/20) start
      *                  COMPUTE  WRK-RSI-MONEY1 = 
      *                  SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA
      *                  COMPUTE  WRK-RSI-MONEY2 =
      *                  WRK-RSI-MONEY1 * (SYS-4001-KSNRATE / 100)
      *                  COMPUTE  WRK-MONEY  = 
      *                     WRK-RSI-MONEY1
      *                  +  WRK-RSI-MONEY2
      * 20080610 健保準拠時の自賠責の計算を修正START tanaka
      *               COMPUTE  WRK-MONEY ROUNDED = 
      *              (SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA)
      *               COMPUTE  WRK-MONEY ROUNDED =
      *                      ( WRK-MONEY * (SYS-4001-KSNRATE1 / 100) ) 
                        IF  SYU-PTFTNRATE NOT =  ZERO
      *                      COMPUTE  WRK-MONEY  =
      *                         WRK-MONEY
      * 20080610 健保準拠時の自賠責の計算を修正END
      *=== 自賠責計算式変更対応 By Takashima(2006/12/20) end
      *                      +  SYU-RSISHOSHIN-MONEY 
      *                      +  SYU-RSISAISHIN-MONEY 
      *                      +  SYU-RSISDO-MONEY 
      *                      +  SYU-RSIETC-MONEY
      *                      +  SYU-RYOYOHI-SKJ  
                            MOVE ZERO  TO   WRK-MONEY 
                        ELSE
                            COMPUTE  WRK-MONEY  = 
                               SYU-RSIJIBAI-SKYMONEY
                            -  SYU-JIHI-TOTAL
                            -  SYU-JIHI-TOTAL-TAX
                            -  SYU-TOTAL-TGMONEY
                            -  SYU-TOTAL-TGMONEY-TAX
                            -  SYU-CHOSEI
                            -  SYU-YKZ-FTN
                            -  SYU-SKYMONEY-SKJ-JIHI-KEI
                            -  SYU-SKYMONEY-LIFE-JIHI-KEI
                            -  SYU-RMSAGAKU
                        END-IF
      * 20080326  H20年4月改正対応START tanaka
      *    公害の場合
                    WHEN  7
                        COMPUTE WRK-JIHI4  = 
                           SYU-TOTAL-TGMONEY
                        +  SYU-TOTAL-TGMONEY-TAX
                        +  SYU-JIHI-TOTAL
                        +  SYU-JIHI-TOTAL-TAX
                        +  SYU-RMSAGAKU
                        +  SYU-SKYMONEY-SKJ-JIHI-KEI
      ***  2009/05/26 ADD START ----------------------------------------
                        +  SYU-SKYMONEY-LIFE-JIHI-KEI
      ***  2009/05/26 ADD END   ----------------------------------------
                        IF      SYU-SRYYMD  <  "20070401"
                            COMPUTE WRK-JIHI4    =   WRK-JIHI4 
                                                +   SYU-JIHI-TAX
                        END-IF
                        COMPUTE WRK-MONEY   =   SYU-RSIJIBAI-SKYMONEY
                                                -  WRK-JIHI4
      * 20080326  H20年4月改正対応END
                    
                    END-EVALUATE
      ***  2009/05/26 ADD START ----------------------------------------
                    COMPUTE WRK-MONEY  =  WRK-MONEY
                                       -  SYU-SKYMONEY-SKJ-KEI
                                       -  SYU-SKYMONEY-LIFE-KEI
      ***  2009/05/26 ADD END   ----------------------------------------
                    ADD    WRK-MONEY  TO  SUM-HKNFTN
               END-IF  
      *
      *    自費負担学
      *
               IF   WRK-SYUHKN-KBN   =  1
                OR  WRK-SYUHKN-KBN   =  2
                OR  WRK-SYUHKN-KBN   =  3
                OR  WRK-SYUHKN-KBN   =  4
      * 20080326  H20年4月改正対応START tanaka
                OR  WRK-SYUHKN-KBN   =  8
      * 20080326  H20年4月改正対応END
                    EVALUATE WRK-SYUHKN-KBN
                    WHEN 1 THRU 2
      * 20080326  H20年4月改正対応START tanaka
                    WHEN 8
      * 20080326  H20年4月改正対応END
      ***           国保、社保の場合
      ***** 04/05/20 DEL 
      *                  COMPUTE WRK-MONEY ROUNDED     = 
      *                  (SYU-TOTAL-HKNTEN * HKN-TENTNK) * WRK-FTNRATE
      ***** 04/05/20 INS
                        IF     SYU-KOH-HKNNUM(1)  =  "027"
                          MOVE  SYU-KOH-COMPFTN(1)   TO  WRK-MONEY
                        ELSE
                          MOVE  SYU-SYUCOMPFTN   TO  WRK-MONEY
                        END-IF
      *****
      ***           レセプト１円単位表示区分が"1"の場合は１０円未満を四捨五入する。
                        IF  WRK-MONEY(8:1) >   4
                            MOVE   ZERO    TO  WRK-MONEY(8:1)
                            ADD    10      TO  WRK-MONEY
                        ELSE
                            MOVE   ZERO    TO  WRK-MONEY(8:1)
                        END-IF 
                    WHEN 3
      ***           公費単独の場合
                        COMPUTE WRK-MONEY ROUNDED     = 
                        (SYU-TOTAL-HKNTEN * HKN-TENTNK)
      ***           レセプト１円単位表示区分が"1"の場合は１０円未満を四捨五入する。
                        IF  WRK-MONEY(8:1) >   4
                            MOVE   ZERO    TO  WRK-MONEY(8:1)
                            ADD    10      TO  WRK-MONEY
                        ELSE
                            MOVE   ZERO    TO  WRK-MONEY(8:1)
                        END-IF 
                    END-EVALUATE
      *
                 IF WRK-SYUHKN-KBN  = 4
      ***           自費の場合
                    MOVE  SYU-SKYMONEY  TO  WRK-MONEY
                 ELSE
      ***           自費以外の場合
      *             自費分の集計
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) start
                     IF  SYU-SRYYMD  <=  "20070331"
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) end
                         COMPUTE WRK-JIHI    =   
                            SYU-TOTAL-TGMONEY  
                         +  SYU-TOTAL-TGMONEY-TAX  
                         +  SYU-JIHI-TOTAL
                         +  SYU-JIHI-TOTAL-TAX  
                         +  SYU-JIHI-TAX
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) start
                     ELSE
                         COMPUTE WRK-JIHI    =   
                            SYU-TOTAL-TGMONEY  
                         +  SYU-TOTAL-TGMONEY-TAX  
                         +  SYU-JIHI-TOTAL
                         +  SYU-JIHI-TOTAL-TAX  
                     END-IF
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) end
                    COMPUTE WRK-MONEY   = 
                       WRK-MONEY
                    +  WRK-JIHI
                    +  SYU-SKYMONEY-SKJ-KEI
                    +  SYU-SKYMONEY-SKJ-JIHI-KEI
      ***  2009/05/26 ADD START ----------------------------------------
                    +  SYU-SKYMONEY-LIFE-KEI
                    +  SYU-SKYMONEY-LIFE-JIHI-KEI
      ***  2009/05/26 ADD END   ----------------------------------------
                    +  SYU-RMSAGAKU
                 END-IF  
                 ADD   WRK-MONEY  TO  SUM-JIHIFTN
               END-IF  
      *
           .
       320-BODY-HEN-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院件数集計処理
      *****************************************************************
       320-NYUIN-CNT-SEC                    SECTION.
      *
      ***  保険点数件数カウント 
           IF  FLG-HKN = 1
               ADD   1  TO   SUM-RHKNKEN
           END-IF 
           .           
       320-NYUIN-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来件数集計処理
      *****************************************************************
       320-GAIRAI-CNT-SEC      SECTION.
      *
           MOVE  JYURRK-SRYYMD(7:2) TO  IDX7G
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE  JYURRK-HOSPID      TO  ACCT-HOSPID  
           MOVE  JYURRK-HOSPNUM      TO  ACCT-HOSPNUM  
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE  JYURRK-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE  JYURRK-PTID        TO  ACCT-PTID
           MOVE  JYURRK-SRYKA     TO  ACCT-SRYKA
           MOVE  JYURRK-SRYYMD(1:6) TO  ACCT-SRYYM
           MOVE "11"                TO  ACCT-SRYKBN
      *=== 初診、再診、その他振分け追加修正 ADD By Yoshikawa(2007/07/04) start
      *           PERFORM 900-SRYACCT-READ-SEC
      *  診療会計情報SELECT(KEY=3)
           MOVE  SRYACCT-REC      TO  MCPDATA-REC
           MOVE  "DBSELECT"       TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE  PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *     CALL  "MCPSUB"             USING
           MOVE    "tbl_sryacct"          TO  MCP-TABLE
           MOVE    "key3"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                      MCPAREA
                                      MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                      SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC         =   ZERO
               MOVE  "DBFETCH"    TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE  PATH-TBL-SRYACCT-KEY3  TO  MCP-PATH
      *         CALL  "MCPSUB"         USING
               MOVE    "tbl_sryacct"        TO  MCP-TABLE
               MOVE    "key3"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"    USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                      MCPAREA
                                      MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                      SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF      MCP-RC         =   ZERO
                   MOVE  MCPDATA-REC  TO  SRYACCT-REC
                   MOVE  ZERO         TO  FLG-SRYACCT
      *--- 2010/10/22 ADD START ----------------------------------------
                   INITIALIZE  FLG-ACCTEND
                   PERFORM  UNTIL  FLG-ACCTEND  =  1
                     IF  JYURRK-ZAINUM(1)  =  ACCT-ZAINUM
                         OR  JYURRK-ZAINUM(2)  =  ACCT-ZAINUM
                         OR  JYURRK-ZAINUM(3)  =  ACCT-ZAINUM
                         OR  JYURRK-ZAINUM(4)  =  ACCT-ZAINUM
                         OR  JYURRK-ZAINUM(5)  =  ACCT-ZAINUM
                         OR  JYURRK-ZAINUM(6)  =  ACCT-ZAINUM
                         OR  JYURRK-ZAINUM(7)  =  ACCT-ZAINUM
                         OR  JYURRK-ZAINUM(8)  =  ACCT-ZAINUM
                         OR  JYURRK-ZAINUM(9)  =  ACCT-ZAINUM
                         OR  JYURRK-ZAINUM(10)  =  ACCT-ZAINUM
                         OR  JYURRK-ZAINUM(11)  =  ACCT-ZAINUM
                         OR  JYURRK-ZAINUM(12)  =  ACCT-ZAINUM
                         OR  JYURRK-ZAINUM(13)  =  ACCT-ZAINUM
                         OR  JYURRK-ZAINUM(14)  =  ACCT-ZAINUM
                         OR  JYURRK-ZAINUM(15)  =  ACCT-ZAINUM
                       MOVE    MCPDATA-REC       TO   SRYACCT-REC
                       MOVE    ZERO              TO   FLG-SRYACCT
                       MOVE    1                 TO   FLG-ACCTEND
                     ELSE
                       MOVE    "DBFETCH"         TO   MCP-FUNC
                       MOVE    "tbl_sryacct"     TO   MCP-TABLE
                       MOVE    "key3"            TO   MCP-PATHNAME
                       CALL    "ORCDBMAIN"       USING
                                                 MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
                       IF   MCP-RC  =  ZERO
                         MOVE    MCPDATA-REC     TO   SRYACCT-REC
                         MOVE    ZERO            TO   FLG-SRYACCT
                       ELSE
                         MOVE    1               TO   FLG-ACCTEND
                       END-IF
                     END-IF
                   END-PERFORM
      *--- 2010/10/22 ADD END   ----------------------------------------
               ELSE
                   MOVE  1            TO  FLG-SRYACCT
                   INITIALIZE             SRYACCT-REC
               END-IF
           ELSE
               MOVE  1            TO  FLG-SRYACCT
               INITIALIZE             SRYACCT-REC
           END-IF
      *--- 2010/10/22 DELETE START -------------------------------------
      **  初診データ探索処理
      *     MOVE    ZERO    TO    FLG-INPUT
      *     PERFORM   UNTIL   FLG-SRYACCT  =  1
      *        IF  ACCT-DAY(1 IDX7)   >   ZERO
      **  初診データが見つかったら入力フラグをオン
      *           MOVE  1   TO   FLG-INPUT
      *        END-IF
      **  診療会計情報FETCH
      *        MOVE  SRYACCT-REC     TO  MCPDATA-REC
      *        MOVE  "DBFETCH"       TO  MCP-FUNC
      **=== 4.0対応 By Takashima(2007/09/10) start
      **        MOVE  PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      **        CALL  "MCPSUB"        USING
      *        MOVE    "tbl_sryacct"          TO  MCP-TABLE
      *        MOVE    "key3"                 TO  MCP-PATHNAME
      *        CALL    "ORCDBMAIN"   USING
      **=== 4.0対応 By Takashima(2007/09/10) end
      *                              MCPAREA
      *                              MCPDATA-REC
      **=== 4.0対応 By Takashima(2007/09/10) start
      *                              SPA-AREA
      **=== 4.0対応 By Takashima(2007/09/10) end
      *        IF     MCP-RC     =   ZERO
      *           MOVE   MCPDATA-REC   TO  SRYACCT-REC
      *           MOVE   ZERO          TO  FLG-SRYACCT
      *        ELSE
      *           MOVE   1             TO  FLG-SRYACCT
      *           INITIALIZE               SRYACCT-REC
      *        END-IF
      *     END-PERFORM
      *--- 2010/10/22 DELETE END   -------------------------------------
      *  診療会計情報CLOSE
           MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE  PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *     CALL  "MCPSUB"         USING
           MOVE    "tbl_sryacct"          TO  MCP-TABLE
           MOVE    "key3"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"   USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                  MCPAREA
                                  MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                  SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *  入力フラグがオンの場合、診療会計終了フラグを切る
      *--- 2010/10/22 DELETE START -------------------------------------
      *     IF   FLG-INPUT       =   1
      *        MOVE   ZERO      TO  FLG-SRYACCT
      *     END-IF
      *--- 2010/10/22 DELETE END   -------------------------------------
      *=== 初診、再診、その他振分け追加修正 ADD By Yoshikawa(2007/07/04) end
      *--- 2010/04/27 ADD START ----------------------------------------
           COMPUTE  JYURRK-EDANUM  =  JYURRK-EDANUM  +  1
           MOVE    JYURRK-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key22"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    ZERO             TO   FLG-JYURRK2
             MOVE    "DBFETCH"        TO   MCP-FUNC
             MOVE    "tbl_jyurrk"     TO   MCP-TABLE
             MOVE    "key22"          TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF   MCP-RC  =  ZERO
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
           MOVE    "key22"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *--- 2010/04/27 ADD END ------------------------------------------
      *--- 2010/07/23 ADD START ----------------------------------------
      *--- 2010/10/22 ADD START ----------------------------------------
           IF  JYURRK-RENNUM  NOT = 1
      *--- 2010/10/22 ADD END   ----------------------------------------
             INITIALIZE                     SRYACCT-REC
             MOVE    JYURRK-HOSPNUM         TO   ACCT-HOSPNUM
             MOVE    JYURRK-NYUGAIKBN       TO   ACCT-NYUGAIKBN
             MOVE    JYURRK-PTID            TO   ACCT-PTID
             MOVE    JYURRK-SRYKA           TO   ACCT-SRYKA
             MOVE    JYURRK-SRYYMD(1:6)     TO   ACCT-SRYYM
             MOVE    JYURRK-ZAINUM          TO   ACCT-ZAINUM
      *
             MOVE    SRYACCT-REC            TO   MCPDATA-REC
             MOVE    "DBSELECT"             TO   MCP-FUNC
             MOVE    "tbl_sryacct"          TO   MCP-TABLE
             MOVE    "key5"                 TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"            USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *
             IF   MCP-RC   =   ZERO
               MOVE    "DBFETCH"            TO   MCP-FUNC
               MOVE    "tbl_sryacct"        TO   MCP-TABLE
               MOVE    "key5"               TO   MCP-PATHNAME
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
      *
             MOVE    "DBCLOSECURSOR"        TO   MCP-FUNC
             MOVE    "tbl_sryacct"          TO   MCP-TABLE
             MOVE    "key5"                 TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"            USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *--- 2010/10/22 ADD START ----------------------------------------
           END-IF
      *--- 2010/10/22 ADD END   ----------------------------------------
      *--- 2010/07/23 ADD END   ----------------------------------------
      *--- 2010/07/06 ADD START ----------------------------------------
             IF  JYURRK-HKNKBN  =  "0"
      *--- 2010/07/06 ADD END   ----------------------------------------
      *
               IF     FLG-SRYACCT    =  ZERO
      *=== 初診、再診、その他振分け修正 DEL By Yoshikawa(2007/06/22) start
      *            AND   ACCT-DAY(1 IDX7)  >  ZERO
      *=== 初診、再診、その他振分け修正 DEL By Yoshikawa(2007/06/22) end
                  AND   JYURRK-RENNUM  =  1
      *--- 2010/03/09 ADD START ----------------------------------------
                  AND   JYURRK-SRYKBN(1)  >  "00"
      *--- 2010/07/23 ADD START ----------------------------------------
                  AND   ACCT-SRYKBN  =  "11"
      *--- 2010/07/23 ADD END   ----------------------------------------
                  AND   (JYURRK-DOUJI-RENNUM  =  1  
                  OR   JYURRK-DOUJI-RENNUM  =  ZERO)  
      *--- 2010/03/09 ADD END ------------------------------------------
      *  初診の件数としてカウントする。 
      ***  保険点数件数カウント 
                 IF  FLG-HKN = 1
                   ADD    1        TO   SUM-SHKNKEN
      *--- 2010/04/27 ADD START ----------------------------------------
                   MOVE    JYURRK-DENPNUM   TO   WRK-DENPNUM
      *--- 2010/04/27 ADD END ------------------------------------------
                 END-IF 
               ELSE 
      *--- 2010/04/27 ADD + DELETE START -------------------------------
      *=== 初診、再診、その他振分け追加修正 ADD By Yoshikawa(2007/07/04) start
      *           MOVE  2                   TO JYURRK-RENNUM
      *           MOVE "DBSELECT"           TO MCP-FUNC
      **=== 4.0対応 By Takashima(2007/09/10) start
      **           MOVE PATH-TBL-JYURRK-KEY5 TO MCP-PATH
      **           CALL "MCPSUB"            USING
      *           MOVE    "tbl_jyurrk"      TO  MCP-TABLE
      *           MOVE    "key5"            TO  MCP-PATHNAME
      *           CALL    "ORCDBMAIN"      USING
      **=== 4.0対応 By Takashima(2007/09/10) end
      *                                    MCPAREA
      *                                    MCPDATA-REC
      **=== 4.0対応 By Takashima(2007/09/10) start
      *                                    SPA-AREA
      **=== 4.0対応 By Takashima(2007/09/10) end
      *              IF  MCP-RC  = ZERO
      *                  MOVE  ZERO           TO  FLG-JYURRK
      *                  MOVE  JYURRK-ZAINUM1 TO  ACCT-ZAINUM
      *              ELSE
      *                  MOVE  1              TO  FLG-JYURRK
      *              END-IF
      **
      *           MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
      **=== 4.0対応 By Takashima(2007/09/10) start
      **           MOVE  PATH-TBL-JYURRK-KEY5   TO  MCP-PATH
      **           CALL  "MCPSUB"   USING
      *           MOVE    "tbl_jyurrk"      TO  MCP-TABLE
      *           MOVE    "key5"            TO  MCP-PATHNAME
      *           CALL    "ORCDBMAIN"      USING
      **=== 4.0対応 By Takashima(2007/09/10) end
      *                            MCPAREA
      *                            MCPDATA-REC
      **=== 4.0対応 By Takashima(2007/09/10) start
      *                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *=== 初診、再診、その他振分け追加修正 ADD By Yoshikawa(2007/07/04) end
      *
      ***  20050914  MOTO
                 PERFORM  VARYING  IDX10 FROM  1  BY  1
                          UNTIL  IDX10  >  15
                   MOVE    JYURRK-ZAINUM(IDX10)  TO   ACCT-ZAINUM
      *--- 2010/04/27 ADD + DELETE END ---------------------------------
      *=== 4.0対応 By Takashima(2007/09/10) start
      *           MOVE  JYURRK-HOSPID      TO  ACCT-HOSPID  
                   MOVE  JYURRK-HOSPNUM      TO  ACCT-HOSPNUM  
      *=== 4.0対応 By Takashima(2007/09/10) end
                   MOVE  JYURRK-NYUGAIKBN   TO  ACCT-NYUGAIKBN
                   MOVE  JYURRK-PTID        TO  ACCT-PTID
                   MOVE  JYURRK-SRYKA       TO  ACCT-SRYKA
                   MOVE  JYURRK-SRYYMD(1:6) TO  ACCT-SRYYM
      *--- 2010/04/27 DELETE START -------------------------------------
      *             MOVE "12"                TO  ACCT-SRYKBN
      *--- 2010/04/27 DELETE END ---------------------------------------
      *=== 初診、再診、その他振分け追加修正 ADD By Yoshikawa(2007/07/04) start
      *           PERFORM 900-SRYACCT-READ-SEC
                   MOVE  SRYACCT-REC        TO  MCPDATA-REC
                   MOVE  "DBSELECT"         TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *           MOVE  PATH-TBL-SRYACCT-KEY5  TO  MCP-PATH
      *           CALL  "MCPSUB"               USING
                   MOVE    "tbl_sryacct"        TO  MCP-TABLE
                   MOVE    "key5"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                              MCPAREA
                                              MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                              SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
                   IF     MCP-RC             =  ZERO
                      MOVE   "DBFETCH"         TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *              MOVE   PATH-TBL-SRYACCT-KEY5 TO  MCP-PATH
      *              CALL   "MCPSUB"              USING
                      MOVE    "tbl_sryacct"        TO  MCP-TABLE
                      MOVE    "key5"               TO  MCP-PATHNAME
                      CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                                 MCPAREA
                                                 MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                                 SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
                      IF     MCP-RC           =  ZERO
                         MOVE MCPDATA-REC     TO  SRYACCT-REC
                         MOVE ZERO            TO  FLG-SRYACCT
                      ELSE
                         MOVE   1             TO  FLG-SRYACCT
                         INITIALIZE               SRYACCT-REC
                      END-IF
                   ELSE
                      MOVE    1              TO  FLG-SRYACCT
                      INITIALIZE                  SRYACCT-REC
                   END-IF
      *
                   MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *           MOVE  PATH-TBL-SRYACCT-KEY5  TO  MCP-PATH
      *           CALL  "MCPSUB"  USING
                   MOVE    "tbl_sryacct"        TO  MCP-TABLE
                   MOVE    "key5"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                                MCPAREA
                                                MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                                SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *=== 初診、再診、その他振分け追加修正 ADD By Yoshikawa(2007/07/04) end
      *
      *--- 2010/04/27 ADD START ----------------------------------------
                   IF  IDX10  <  15
                     COMPUTE   IDX11   =   IDX10  +   1
                     IF   JYURRK-ZAINUM(IDX11)  =  ZERO
                       MOVE    16               TO   IDX10
                     END-IF
                   END-IF
                   IF   ACCT-SRYKBN   =  "12"
                     MOVE    16               TO   IDX10
                   END-IF
                 END-PERFORM
                 MOVE    ACCT-HOSPNUM     TO   SRY-HOSPNUM
                 MOVE    ACCT-NYUGAIKBN   TO   SRY-NYUGAIKBN
                 MOVE    ACCT-PTID        TO   SRY-PTID
                 MOVE    ACCT-SRYKA       TO   SRY-SRYKA
                 MOVE    ACCT-SRYYM       TO   SRY-SRYYM
                 MOVE    ACCT-ZAINUM      TO   SRY-ZAINUM
                 PERFORM  900-SRYACT-SELECT-SEC
                 PERFORM  900-SRYACT-FETCH-SEC
                 PERFORM  900-SRYACT-CLOSE-SEC
      *--- 2010/04/27 ADD END ------------------------------------------
                 IF     FLG-SRYACCT    =  ZERO
                     AND   ACCT-DAY(1 IDX7)  >  ZERO
      *--- 2010/04/27 CHANGE START -------------------------------------
      *=== 初診、再診、その他振分け修正 by Yoshikawa(2008/01/31) start
      *               AND   JYURRK-SRYKBN(1)  >  "00"
                     AND   SRY-SRYKBN   =   "12"
      *=== 初診、再診、その他振分け修正 by Yoshikawa(2008/01/31) end
      *--- 2010/04/27 CHANGE END ---------------------------------------
      *=== 在宅等の誤振分け対応 ADD By Yoshikawa(2007/12/18) start
                    AND   ACCT-SRYKBN       =  "12"
      *=== 在宅等 の誤振分け対応 ADD By Yoshikawa(2007/12/18) end
      *
      ***  20050914  MOTO
      *
      *  再診の件数としてカウントする。
      ***  保険点数件数カウント 
                   IF  FLG-HKN = 1
                     ADD   1  TO   SUM-RHKNKEN
      *--- 2010/04/27 ADD START ----------------------------------------
                     MOVE    JYURRK-DENPNUM    TO   WRK-DENPNUM
      *--- 2010/04/27 ADD END ------------------------------------------
                   END-IF 
      *
      ***  20050914  MOTO
                 ELSE
      *  その他の件数としてカウントする。
      ***  保険点数件数カウント 
      *--- 2010/04/27 ADD START ----------------------------------------
                   IF   FLG-JYURRK2  =  1
      *--- 2010/04/27 ADD END ------------------------------------------
                       IF  FLG-HKN = 1
                           ADD   1  TO   SUM-OHKNKEN
                       END-IF 
      *--- 2010/04/27 ADD START ----------------------------------------
                   END-IF
      *--- 2010/04/27 ADD END ------------------------------------------
                 END-IF
      ***  20050914  MOTO
      *
               END-IF
      *--- 2010/07/06 ADD START ----------------------------------------
             ELSE
                 MOVE   2                  TO  JYURRK-RENNUM
                 MOVE   "DBSELECT"         TO  MCP-FUNC
                 MOVE   "tbl_jyurrk"       TO  MCP-TABLE
                 MOVE   "key5"             TO  MCP-PATHNAME
                 CALL   "ORCDBMAIN"        USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
                 IF  MCP-RC  = ZERO
                   MOVE  ZERO              TO  FLG-JYURRK
                   MOVE  JYURRK-ZAINUM1    TO  ACCT-ZAINUM
                 ELSE
                   MOVE  1                 TO  FLG-JYURRK
                 END-IF
      *
                 MOVE   "DBCLOSECURSOR"    TO  MCP-FUNC
                 MOVE   "tbl_jyurrk"       TO  MCP-TABLE
                 MOVE   "key5"             TO  MCP-PATHNAME
                 CALL   "ORCDBMAIN"        USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
                 MOVE  JYURRK-HOSPNUM      TO  ACCT-HOSPNUM  
                 MOVE  JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
                 MOVE  JYURRK-PTID         TO  ACCT-PTID
                 MOVE  JYURRK-SRYKA        TO  ACCT-SRYKA
                 MOVE  JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
                 MOVE  "11"                TO  ACCT-SRYKBN
                 MOVE  SRYACCT-REC         TO  MCPDATA-REC
                 MOVE  "DBSELECT"          TO  MCP-FUNC
                 MOVE  "tbl_sryacct"       TO  MCP-TABLE
                 MOVE  "key5"              TO  MCP-PATHNAME
                 CALL  "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
                 IF     MCP-RC             =  ZERO
                    MOVE   "DBFETCH"       TO  MCP-FUNC
                    MOVE    "tbl_sryacct"  TO  MCP-TABLE
                    MOVE    "key5"         TO  MCP-PATHNAME
                    CALL    "ORCDBMAIN"    USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
                    IF     MCP-RC           =  ZERO
                       MOVE MCPDATA-REC    TO  SRYACCT-REC
                       MOVE ZERO           TO  FLG-SRYACCT
                    ELSE
                       MOVE   1            TO  FLG-SRYACCT
                       INITIALIZE              SRYACCT-REC
                    END-IF
                 ELSE
                    MOVE    1              TO  FLG-SRYACCT
                    INITIALIZE             SRYACCT-REC
                 END-IF
      *
                 MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
                 MOVE    "tbl_sryacct"    TO  MCP-TABLE
                 MOVE    "key5"           TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"      USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
               IF     FLG-SRYACCT    =   ZERO
                 AND  ACCT-DAY(1 IDX7)   >  ZERO
                 AND  JYURRK-SRYKBN (1)  >  "00"
                 AND  ACCT-SRYKBN        =  "11"
      *  初診の件数としてカウントする。 
      ***  保険点数件数カウント 
                 IF  FLG-HKN = 1
                   ADD   1  TO   SUM-SHKNKEN
      *--- 2010/07/23 ADD START ----------------------------------------
                   MOVE    JYURRK-DENPNUM    TO   WRK-DENPNUM
      *--- 2010/07/23 ADD END   ----------------------------------------
                 END-IF 
               ELSE
                 MOVE  2                   TO JYURRK-RENNUM
                 MOVE "DBSELECT"           TO MCP-FUNC
                 MOVE    "tbl_jyurrk"      TO  MCP-TABLE
                 MOVE    "key5"            TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"       USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
                 IF  MCP-RC  = ZERO
                   MOVE  ZERO           TO  FLG-JYURRK
                   MOVE  JYURRK-ZAINUM1 TO  ACCT-ZAINUM
                 ELSE
                   MOVE  1              TO  FLG-JYURRK
                 END-IF
      *
                 MOVE   "DBCLOSECURSOR"    TO  MCP-FUNC
                 MOVE   "tbl_jyurrk"       TO  MCP-TABLE
                 MOVE   "key5"             TO  MCP-PATHNAME
                 CALL   "ORCDBMAIN"        USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
                 MOVE  JYURRK-HOSPNUM      TO  ACCT-HOSPNUM  
                 MOVE  JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
                 MOVE  JYURRK-PTID         TO  ACCT-PTID
                 MOVE  JYURRK-SRYKA        TO  ACCT-SRYKA
                 MOVE  JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
                 MOVE  "12"                TO  ACCT-SRYKBN
                 MOVE  SRYACCT-REC         TO  MCPDATA-REC
                 MOVE  "DBSELECT"          TO  MCP-FUNC
                 MOVE  "tbl_sryacct"       TO  MCP-TABLE
                 MOVE  "key5"              TO  MCP-PATHNAME
                 CALL  "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
                 IF     MCP-RC             =  ZERO
                    MOVE   "DBFETCH"       TO  MCP-FUNC
                    MOVE    "tbl_sryacct"  TO  MCP-TABLE
                    MOVE    "key5"         TO  MCP-PATHNAME
                    CALL    "ORCDBMAIN"    USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
                    IF     MCP-RC           =  ZERO
                       MOVE MCPDATA-REC    TO  SRYACCT-REC
                       MOVE ZERO           TO  FLG-SRYACCT
                    ELSE
                       MOVE   1            TO  FLG-SRYACCT
                       INITIALIZE              SRYACCT-REC
                    END-IF
                 ELSE
                    MOVE    1              TO  FLG-SRYACCT
                    INITIALIZE             SRYACCT-REC
                 END-IF
      *
                 MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
                 MOVE    "tbl_sryacct"    TO  MCP-TABLE
                 MOVE    "key5"           TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"      USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
                 IF     FLG-SRYACCT    =  ZERO
                   AND   ACCT-DAY(1 IDX7)  >  ZERO
                   AND   JYURRK-SRYKBN (1) >  "00"
                   AND   ACCT-SRYKBN       =  "12"
      *
      *  再診の件数としてカウントする。
      ***  保険点数件数カウント 
                   IF  FLG-HKN = 1
                       ADD   1  TO   SUM-RHKNKEN
      *--- 2010/07/23 ADD START ----------------------------------------
                       MOVE    JYURRK-DENPNUM    TO   WRK-DENPNUM
      *--- 2010/07/23 ADD END   ----------------------------------------
                   END-IF 
      *
                 ELSE
      *  その他の件数としてカウントする。
      ***  保険点数件数カウント 
                   IF  FLG-HKN = 1
                       ADD   1  TO   SUM-OHKNKEN
                   END-IF 
                 END-IF
      *
               END-IF
             END-IF
      *--- 2010/07/06 ADD END   ----------------------------------------
      *
      *    現在処理中のレコードが同一患者のレコードでない場合は、
      *    受診者数のカウントを行う。(同日診療の場合は実患者数をカウントする。)
      *    但し、現状は科別集計においては受診者数の印字は行わない。
             MOVE     1        TO      IDX3 
             PERFORM  UNTIL    IDX3    >      50000
                  IF  WRK-SMEI-PTID(IDX-S)    =  WRK-JYUSHIN-PTID(IDX3)
      * 既に同日の診療が受診件数として計上されている場合は
      * 全科合計の受診者数に計上しない.
      ***  2006/04/01 T.T BEGIN 
      *               MOVE 99999   TO      IDX3	
                      MOVE 50000   TO      IDX3
      ***  2006/04/01 T.T END 
                  END-IF
                  IF  WRK-JYUSHIN-PTID(IDX3)  =  ZERO
      *=== 受診者数の集計誤りの修正 ADD By Yoshikawa(2007/06/22) start
      ****  2006/04/01 T.T BEGIN 
      **               AND  (IDX3 NOT = 50000)
      ****  2006/04/01 T.T END 
                      AND  (IDX3 NOT = 50000)
      *=== 受診者数の集計誤りの修正 ADD By Yoshikawa(2007/06/22) end
      *
      * 同日の診療が発生していない場合は
      * 全科合計の受診者数に計上する。
                      MOVE WRK-SMEI-PTID(IDX-S)
                           TO  WRK-JYUSHIN-PTID(IDX3)
                      ADD  1       TO      SUM-JYUSHINKEN-GK
      ***  2006/04/01 T.T BEGIN 
      *               MOVE 99999   TO      IDX3
                      MOVE 50000   TO      IDX3
      ***  2006/04/01 T.T END 
                  END-IF
                  ADD 1            TO      IDX3
             END-PERFORM
      *
           .           
       320-GAIRAI-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3201-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *   保険組合せ読み出し
           INITIALIZE                  HKNCOMBI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    SYU-HOSPID        TO  COMB-HOSPID
           MOVE    SYU-HOSPNUM        TO  COMB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    SYU-PTID          TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM   TO  COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   ZERO
      *    患者保険情報読み込み       
               INITIALIZE                  PTHKNINF-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    SYU-HOSPID        TO  PTHKN-HOSPID
               MOVE    SYU-HOSPNUM        TO  PTHKN-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
               MOVE    SYU-PTID          TO  PTHKN-PTID
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
               PERFORM 900-PTHKNINF-READ-SEC
           END-IF    
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *          帳票<診療科合計>印刷処理
      *****************************************************************
      *    
       330-TERM-KA-PRINT-SEC         SECTION.
      *
           INITIALIZE                   D601      
      *
           MOVE    WRK-PARA-DENPPRTYMDWH
                                        TO  D601-SYSYMD
      *
           EVALUATE WRK-NYUGAIKBN
              WHEN "1"
                   MOVE "入院"          TO  D601-NYUGAIKBN
                   MOVE  "請求件数"     TO  D601-KENSU2
      *
      ***  2004/05/06 @@@@-MOTOHASI 追加
      *--- 2010/09/09 DELETE START -------------------------------------
      *             MOVE "入院料"       TO  D601-NYUINRYO
      *--- 2010/09/09 DELETE END ---------------------------------------
      ***  ここまで
      *
              WHEN "2"
                   MOVE "外来"          TO  D601-NYUGAIKBN
      ***  20050914  MOTO
                   MOVE  "その他件数"   TO  D601-KENSU3
      ***  20050914  MOTO
                   MOVE  "再診件数"     TO  D601-KENSU2
                   MOVE  "初診件数"     TO  D601-KENSU1
      *
      ***  2004/05/06 @@@@-MOTOHASI 追加
      *--- 2010/09/09 DELETE START -------------------------------------
      *             MOVE   SPACE         TO  D601-NYUINRYO
      *--- 2010/09/09 DELETE END ---------------------------------------
      ***  ここまで
      *
           END-EVALUATE
      *--- 2010/09/09 ADD START ----------------------------------------
           MOVE    "入院料"       TO   D601-NYUINRYO
      *--- 2010/09/09 ADD END ------------------------------------------
      *
      *    診療科名称取得
           INITIALIZE                       SYS-1005-REC
           MOVE    "1005"               TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA            TO  SYS-1005-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO  SYS-1005-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO  WRK-SYMD
      ***  20050914  MOTO
           MOVE  WRK-PARA-DENPPRTYMD    TO  SYS-1005-EDYUKYMD
                                            SYS-1005-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1005-REC         TO  MCPDATA-REC
           PERFORM 820-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC        TO  SYS-1005-REC
               MOVE  SYS-1005-SRYKANAME TO  D601-SRYKA
           ELSE
               MOVE  SPACE              TO  D601-SRYKA
           END-IF                                     
      *
          IF  FLG-GET = ZERO
      *  対象データが１件も存在しなかった場合
             MOVE  "＊＊＊  対象データが１件も存在しません。＊＊＊"
                     TO  D601-JOBMSG
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  D601-PAGE
          ELSE
      *  対象データが１件以上存在した場合
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  D601-PAGE
      *  保険点数、件数合計
             MOVE    SUM-HKNTEN   TO  WRK-Z9
             MOVE    WRK-Z9       TO  D601-GHKNTEN
             IF WRK-NYUGAIKBN =   "2" 
                MOVE    SUM-SHKNKEN   TO  WRK-Z9-5
                MOVE    WRK-Z9-5  TO  D601-SHKNKEN
      ***  20050914  MOTO
                MOVE    SUM-OHKNKEN   TO  WRK-Z9-5
                MOVE    WRK-Z9-5  TO  D601-OHKNKEN
      ***  20050914  MOTO
             END-IF
             MOVE    SUM-RHKNKEN  TO  WRK-Z9-5
             MOVE    WRK-Z9-5     TO  D601-RHKNKEN
      *
      *  診療区分別点数
            PERFORM VARYING IDX FROM    1   BY  1
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) start
      *             UNTIL    IDX   >    11
      * 20080326  H20年4月改正対応START tanaka
      *             UNTIL    IDX   >    15
                   UNTIL    IDX   >    16
      * 20080326  H20年4月改正対応END
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) end
                       MOVE  SUM-TEN (IDX)  TO  WRK-Z9 
                       MOVE  WRK-Z9     TO  D601-TEN (IDX) 
            END-PERFORM
      *
      ***  2004/05/06 @@@@-MOTOHASI 追加
      *
      *--- 2010/09/09 DELETE START -------------------------------------
      *     IF WRK-NYUGAIKBN =   "2" 
      **=== 基本診療項目追加対応 By Takashima(2006/10/30) start
      **           MOVE  SPACE     TO  D601-TEN (11) 
      ** 20080326  H20年4月改正対応START tanaka
      **           MOVE  SPACE     TO  D601-TEN (14) 
      *           MOVE  SPACE     TO  D601-TEN (15) 
      ** 20080326  H20年4月改正対応END
      **=== 基本診療項目追加対応 By Takashima(2006/10/30) end
      *     END-IF
      ***  ここまで
      *--- 2010/09/09 DELETE END ---------------------------------------
      *
      *
      *  金額合計
             MOVE  SUM-HKNFTN     TO   WRK-S9
             MOVE  WRK-S9         TO   D601-HKNFTN
             MOVE  SUM-JIHIFTN    TO   WRK-S9
             MOVE  WRK-S9         TO   D601-JIHIFTN
             MOVE  SUM-SEIKYU     TO   WRK-S9
             MOVE  WRK-S9         TO   D601-GSEIKYU
             MOVE  SUM-RYOSYU     TO   WRK-S9
             MOVE  WRK-S9         TO   D601-GRYOSYU
      *
      *  受診者数
      ***    IF WRK-NYUGAIKBN =   "2" 
      ***       MOVE  SUM-JYUSHINKEN       TO   WRK-Z9-5
      ***       MOVE  WRK-Z9-5             TO   D601-JYUSHINCNT
      ***    END-IF 
      *
          END-IF
      * 
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
      ***  20040916 MOTOHASI MONPEに変更
      ***     MOVE   "A00000D601"         TO  SPRT-PRTID
      * 20080326  H20年4月改正対応START tanaka
           IF  WRK-PARA-DENPPRTYMD  <  "20080401"
               MOVE   "A00000D601O.red"         TO  SPRT-PRTID
           ELSE
               MOVE   "A00000D601N.red"         TO  SPRT-PRTID
           END-IF
      * 20080326  H20年4月改正対応END
      ***  20040916 MOTOHASI MONPEに変更
           MOVE    "日計表合計（診療費請求明細）"
                                       TO  SPRT-TITLE
           MOVE    D601                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      ***  site-libを実行環境にする為コメントアウト
      ***     MOVE    "1"          TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC                           
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
           INITIALIZE                  D601      
      *
           MOVE    WRK-PARA-DENPPRTYMDWH
                                       TO  D601-SYSYMD
      *
           EVALUATE WRK-NYUGAIKBN
              WHEN "1"
                   MOVE "入院"         TO  D601-NYUGAIKBN
                   MOVE  "請求件数"    TO  D601-KENSU2
      *
      ***  2004/05/06 @@@@-MOTOHASI 追加
      *--- 2010/09/09 DELETE START -------------------------------------
      *             MOVE "入院料"       TO  D601-NYUINRYO
      *--- 2010/09/09 DELETE END ---------------------------------------
      ***  ここまで
      *
              WHEN "2"
                   MOVE "外来"         TO  D601-NYUGAIKBN
      ***  20050914  MOTO
                   MOVE  "その他件数"  TO  D601-KENSU3
      ***  20050914  MOTO
                   MOVE  "再診件数"    TO  D601-KENSU2
                   MOVE  "初診件数"    TO  D601-KENSU1
                   MOVE  "受診者数"    TO  D601-TJYUSHIN
      *
      ***  2004/05/06 @@@@-MOTOHASI 追加
                   MOVE   SPACE         TO  D601-NYUINRYO
      ***  ここまで
      *
           END-EVALUATE
      *--- 2010/09/09 ADD START ----------------------------------------
           MOVE    "入院料"       TO   D601-NYUINRYO
      *--- 2010/09/09 ADD END ------------------------------------------
      *
      * 診療科名称 
           EVALUATE WRK-NYUGAIKBN
               WHEN "1"
                    IF  WRK-PRNSRKA-CNT  >   1
      *    診療科が２つ以上の場合は全科合計を設定する。
                        MOVE  "全科合計"    TO  D601-SRYKA
                    ELSE
      *    診療科名称取得
      *    診療科が１つの場合は診療科名称を設定する。
                       INITIALIZE                    SYS-1005-REC
                       MOVE  "1005"              TO  SYS-1005-KANRICD
                       MOVE  WRK-SRYKA           TO  SYS-1005-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
                       MOVE  SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
                       MOVE  WRK-PARA-DENPPRTYMD TO  WRK-SYMD
      ***  20050914  MOTO
                       MOVE  WRK-PARA-DENPPRTYMD TO  SYS-1005-EDYUKYMD
                                                     SYS-1005-STYUKYMD
      ***  20050914  MOTO
                       MOVE  SYS-1005-REC        TO  MCPDATA-REC
                       PERFORM 820-SYSKANRI-READ-SEC
                       IF    FLG-SYSKANRI2        =   ZERO
                        MOVE  MCPDATA-REC        TO  SYS-1005-REC
                        MOVE  SYS-1005-SRYKANAME TO  D601-SRYKA
                       ELSE
                        MOVE  SPACE              TO  D601-SRYKA
                       END-IF                                     
                    END-IF
               WHEN "2"
                    IF  WRK-PRGSRKA-CNT  >   1
      *    診療科が２つ以上の場合は全科合計を設定する。
                        MOVE  "全科合計"         TO  D601-SRYKA
                    ELSE
      *    診療科名称取得
      *    診療科が１つの場合は診療科名称を設定する。
                       INITIALIZE                    SYS-1005-REC
                       MOVE  "1005"              TO  SYS-1005-KANRICD
                       MOVE  WRK-SRYKA           TO  SYS-1005-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
                       MOVE  SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
                       MOVE  WRK-PARA-DENPPRTYMD TO  WRK-SYMD
      ***  20050914  MOTO
                       MOVE  WRK-PARA-DENPPRTYMD TO  SYS-1005-EDYUKYMD
                                                     SYS-1005-STYUKYMD
      ***  20050914  MOTO
                       MOVE  SYS-1005-REC        TO  MCPDATA-REC
                       PERFORM 820-SYSKANRI-READ-SEC
                       IF    FLG-SYSKANRI2        =   ZERO
                        MOVE  MCPDATA-REC        TO  SYS-1005-REC
                        MOVE  SYS-1005-SRYKANAME TO  D601-SRYKA
                       ELSE
                        MOVE  SPACE              TO  D601-SRYKA
                       END-IF                                     
                    END-IF
           END-EVALUATE
      * 20030620 変更
      *
          IF  FLG-GET = ZERO
      *  対象データが１件も存在しなかった場合
             MOVE  "＊＊＊  対象データが１件も存在しません。＊＊＊"
                     TO  D601-JOBMSG
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  D601-PAGE
          ELSE
      *
      *  対象データが１件以上存在した場合
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  D601-PAGE
      *  保険点数、件数合計
             MOVE    SUM-HKNTEN-GK    TO  WRK-Z9
             MOVE    WRK-Z9           TO  D601-GHKNTEN
             IF WRK-NYUGAIKBN =   "2" 
                MOVE    SUM-SHKNKEN-GK   TO  WRK-Z9-5
                MOVE    WRK-Z9-5         TO  D601-SHKNKEN
      ***  20050914  MOTO
                MOVE    SUM-OHKNKEN-GK   TO  WRK-Z9-5
                MOVE    WRK-Z9-5         TO  D601-OHKNKEN
      ***  20050914  MOTO
             END-IF
             MOVE    SUM-RHKNKEN-GK   TO  WRK-Z9-5
             MOVE    WRK-Z9-5         TO  D601-RHKNKEN
      *
      *  診療区分別点数
            PERFORM VARYING IDX FROM    1   BY  1
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) start
      *             UNTIL    IDX   >    11
      * 20080326  H20年4月改正対応START tanaka
      *             UNTIL    IDX   >    15
                   UNTIL    IDX   >    16
      * 20080326  H20年4月改正対応END
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) end
                       MOVE  SUM-TEN-GK (IDX)  TO  WRK-Z9 
                       MOVE  WRK-Z9     TO  D601-TEN (IDX) 
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) start
                       IF  SUM-TEN-GK (IDX)   >   0
      * 20080326  H20年4月改正対応START tanaka
      *                     AND IDX                =  15
                           AND IDX                =  16
      * 20080326  H20年4月改正対応END
                           MOVE    "療養担当手当"    TO    D601-TRYO
                       END-IF
      *=== 基本診療項目追加対応 By Takashima(2006/10/30) end
            END-PERFORM
      *
      ***  2004/05/06 @@@@-MOTOHASI 追加
      *
      *--- 2010/09/09 DELETE START -------------------------------------
      *     IF WRK-NYUGAIKBN =   "2" 
      **=== 基本診療項目追加対応 By Takashima(2006/10/30) start
      **           MOVE  SPACE     TO  D601-TEN (11) 
      ** 20080326  H20年4月改正対応START tanaka
      **           MOVE  SPACE     TO  D601-TEN (14) 
      *           MOVE  SPACE     TO  D601-TEN (15) 
      ** 20080326  H20年4月改正対応END
      **=== 基本診療項目追加対応 By Takashima(2006/10/30) end
      *     END-IF
      ****  ここまで
      *--- 2010/09/09 DELETE END ---------------------------------------
      *
      *
      *  金額合計
             MOVE  SUM-HKNFTN-GK  TO   WRK-S9
             MOVE  WRK-S9         TO   D601-HKNFTN
             MOVE  SUM-JIHIFTN-GK TO   WRK-S9
             MOVE  WRK-S9         TO   D601-JIHIFTN
             MOVE  SUM-SEIKYU-GK  TO   WRK-S9
             MOVE  WRK-S9         TO   D601-GSEIKYU
             MOVE  SUM-RYOSYU-GK  TO   WRK-S9
             MOVE  WRK-S9         TO   D601-GRYOSYU
      *
      *  受診者数
             IF WRK-NYUGAIKBN =   "2" 
                   MOVE  SUM-JYUSHINKEN-GK TO   WRK-Z9-5
                   MOVE  WRK-Z9-5          TO   D601-JYUSHINCNT
             END-IF
          END-IF
      * 
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
      ***  20040916 MOTOHASI MONPEに変更
      ***     MOVE   "A00000D601"         TO  SPRT-PRTID
      * 20080326  H20年4月改正対応START tanaka
           IF  WRK-PARA-DENPPRTYMD  <  "20080401"
               MOVE   "A00000D601O.red"         TO  SPRT-PRTID
           ELSE
               MOVE   "A00000D601N.red"         TO  SPRT-PRTID
           END-IF
      * 20080326  H20年4月改正対応END
      ***  20040916 MOTOHASI MONPEに変更
           MOVE    "日計表合計（診療費請求明細）"
                                       TO  SPRT-TITLE
           MOVE    D601                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      ***  site-libを実行環境にする為コメントアウト
      ***     MOVE    "1"          TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC                           
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
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           END-IF
      *
           MOVE    1                   TO  FLG-END     
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
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT                        
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                   SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           PERFORM 900-DBDISCONNECT-SEC
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
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    WRK-PARA-HOSPID     TO  SMEI-HOSPID
           MOVE    WRK-PARA-HOSPNUM     TO  SMEI-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    "1"                 TO  SMEI-NYUGAIKBN
           MOVE    WRK-PARA-DENPPRTYMD TO  SMEI-NYUHEN-YMD
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
          IF      MCP-RC              =   ZERO
                PERFORM 910-DBFETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
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
           MOVE    "DBFETCH"         TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    ZERO          TO  FLG-SYUMEI
               MOVE    MCPDATA-REC   TO  SYUMEI-REC
           ELSE
               MOVE    1             TO  FLG-SYUMEI
           END-IF  
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"        TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
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
      *    収納明細情報　入返金発行日で検索(外来患者)
      *****************************************************************
       920-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  SYUMEI-REC
      *
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    WRK-PARA-HOSPID     TO  SMEI-HOSPID
           MOVE    WRK-PARA-HOSPNUM     TO  SMEI-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    "2"                 TO  SMEI-NYUGAIKBN
           MOVE    WRK-PARA-DENPPRTYMD TO  SMEI-NYUHEN-YMD
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
          IF      MCP-RC              =   ZERO
                PERFORM 920-DBFETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
           END-IF        
           .
       920-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納明細情報を検索処理(外来患者)
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"         TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC            =  ZERO
               MOVE    ZERO          TO  FLG-SYUMEI
               MOVE    MCPDATA-REC   TO  SYUMEI-REC
           ELSE
               MOVE    1             TO  FLG-SYUMEI
           END-IF  
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       920-DBCLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"        TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           .
       920-DBCLOSE-EXT.
           EXIT.
      *
      *--- 2010/04/27 CHANGE START -------------------------------------
      *****************************************************************
      *    収納情報検索
      *****************************************************************
      * 900-JYURRK-READ-SEC                 SECTION.
      **
      *     INITIALIZE                  JYURRK-REC
      **
      **=== 4.0対応 By Takashima(2007/09/10) start
      **     MOVE    WRK-SMEI-HOSPID(IDX-S)
      **                   TO   JYURRK-HOSPID
      *     MOVE    WRK-SMEI-HOSPNUM(IDX-S)
      *                   TO   JYURRK-HOSPNUM
      **=== 4.0対応 By Takashima(2007/09/10) end
      *     MOVE    WRK-SMEI-PTID(IDX-S)
      *                   TO   JYURRK-PTID
      *     MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)
      *                   TO   JYURRK-NYUGAIKBN
      *     MOVE    WRK-SMEI-DENPNUM(IDX-S)
      *                   TO   JYURRK-DENPNUM
      ****  20041005  Ver 2.4.0対応による削除  MOTO
      ****     MOVE    "1"              TO   JYURRK-LASTKBN
      **
      *     MOVE    JYURRK-REC       TO  MCPDATA-REC
      *     MOVE    "DBSELECT"          TO  MCP-FUNC
      **=== 4.0対応 By Takashima(2007/09/10) start
      **     MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      **     CALL    "MCPSUB"             USING
      *     MOVE    "tbl_jyurrk"            TO  MCP-TABLE
      *     MOVE    "key6"                  TO  MCP-PATHNAME
      *     CALL    "ORCDBMAIN"          USING
      **=== 4.0対応 By Takashima(2007/09/10) end
      *                                  MCPAREA
      *                                  MCPDATA-REC
      **=== 4.0対応 By Takashima(2007/09/10) start
      *                                  SPA-AREA
      **=== 4.0対応 By Takashima(2007/09/10) end
      *     IF      MCP-RC              =   ZERO
      *         MOVE    "DBFETCH"           TO  MCP-FUNC
      **=== 4.0対応 By Takashima(2007/09/10) start
      **         MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      **         CALL    "MCPSUB"             USING
      *         MOVE    "tbl_jyurrk"            TO  MCP-TABLE
      *         MOVE    "key6"                  TO  MCP-PATHNAME
      *         CALL    "ORCDBMAIN"          USING
      **=== 4.0対応 By Takashima(2007/09/10) end
      *                                      MCPAREA
      *                                      MCPDATA-REC
      **=== 4.0対応 By Takashima(2007/09/10) start
      *                                      SPA-AREA
      **=== 4.0対応 By Takashima(2007/09/10) end
      *         IF      MCP-RC              =   ZERO
      *             MOVE    MCPDATA-REC   TO  JYURRK-REC
      *             MOVE    ZERO                    TO  FLG-JYURRK
      *         ELSE
      *             MOVE    1               TO  FLG-JYURRK
      *         END-IF
      *     ELSE
      *         MOVE    1               TO  FLG-JYURRK
      *     END-IF
      **
      **=== sarge対応 UP By Takashima(2006/08/01) start
      **     MOVE    "DBCLOSE"             TO  MCP-FUNC
      *     MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
      **=== sarge対応 UP By Takashima(2006/08/01) end
      **=== 4.0対応 By Takashima(2007/09/10) start
      **     MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      **     CALL    "MCPSUB"             USING
      *     MOVE    "tbl_jyurrk"            TO  MCP-TABLE
      *     MOVE    "key6"                  TO  MCP-PATHNAME
      *     CALL    "ORCDBMAIN"          USING
      **=== 4.0対応 By Takashima(2007/09/10) end
      *                                  MCPAREA
      *                                  MCPDATA-REC
      **=== 4.0対応 By Takashima(2007/09/10) start
      *                                  SPA-AREA
      **=== 4.0対応 By Takashima(2007/09/10) end
      **
      *     .
      * 900-JYURRK-READ-EXT.
      *     EXIT.
      *
      ******************************************************************
      *    受診履歴SELECT
      ******************************************************************
       900-JYURRK-SELECT-SEC                 SECTION.
      
           INITIALIZE                         JYURRK-REC
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)    TO   JYURRK-HOSPNUM
           MOVE    WRK-SMEI-PTID(IDX-S)       TO   JYURRK-PTID
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)  TO   JYURRK-NYUGAIKBN
           MOVE    WRK-SMEI-DENPNUM(IDX-S)    TO   JYURRK-DENPNUM
           MOVE    JYURRK-REC             TO   MCPDATA-REC
           MOVE    "DBSELECT"             TO   MCP-FUNC
           MOVE    "tbl_jyurrk"           TO   MCP-TABLE
           MOVE    "key6 "                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF   MCP-RC  =  ZERO
             PERFORM  900-JYURRK-FETCH-SEC
           ELSE
             MOVE    1                    TO   FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      
      ******************************************************************
      *    受診履歴FETCH
      ******************************************************************
       900-JYURRK-FETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO   MCP-FUNC
           MOVE    "tbl_jyurrk"        TO   MCP-TABLE
           MOVE    "key6"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC       TO   JYURRK-REC
             MOVE    ZERO              TO   FLG-JYURRK
           ELSE
             MOVE    1                 TO   FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-FETCH-EXT.
           EXIT.
      
      ******************************************************************
      *    受診履歴CLOSE
      ******************************************************************
       900-JYURRK-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"    TO   MCP-FUNC
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key6"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       900-JYURRK-CLOSE-EXT.
           EXIT.
      *--- 2010/04/27 CHANGE END ---------------------------------------
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       900-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE                 SYUNOU-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    SMEI-HOSPID    TO  SYU-HOSPID  
           MOVE    SMEI-HOSPNUM    TO  SYU-HOSPNUM  
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    SMEI-NYUGAIKBN TO  SYU-NYUGAIKBN  
           MOVE    SMEI-PTID      TO  SYU-PTID  
           MOVE    SMEI-DENPNUM   TO  SYU-DENPNUM  
      *
           MOVE    SYUNOU-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"           TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_syunou"           TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC   TO  SYUNOU-REC
                   MOVE    ZERO                    TO  FLG-SYUNOU
               ELSE
                   MOVE    1               TO  FLG-SYUNOU
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"              TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"        TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL     "MCPSUB"            USING
           MOVE    "tbl_syunou"           TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
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
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)        
                         TO  SYU-HOSPNUM  
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)
                         TO  SYU-NYUGAIKBN  
           MOVE    WRK-SMEI-PTID(IDX-S)
                         TO  SYU-PTID  
           MOVE    WRK-SMEI-DENPNUM(IDX-S)
                         TO  SYU-DENPNUM  
      *
           MOVE    SYUNOU-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"           TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_syunou"           TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC   TO  SYUNOU-REC
                   MOVE    ZERO                    TO  FLG-SYUNOU
               ELSE
                   MOVE    1               TO  FLG-SYUNOU
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"             TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY   TO  MCP-PATH
      *     CALL     "MCPSUB"            USING
           MOVE    "tbl_syunou"           TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
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
           MOVE    SYU-HOSPNUM          TO  HKN-HOSPNUM  
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    SYU-SYUHKNNUM       TO  HKN-HKNNUM  
           MOVE    WRK-PARA-DENPPRTYMD TO  HKN-TEKSTYMD
           MOVE    ALL "9"             TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *     CALL    "MCPSUB"           USING
           MOVE    "tbl_hknnum"            TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                      MCPAREA
                                      MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                      SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF  MCP-RC             =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *         CALL    "MCPSUB"           USING
               MOVE    "tbl_hknnum"            TO  MCP-TABLE
               MOVE    "key5"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                          MCPAREA
                                          MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                          SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF      MCP-RC          =  ZERO
                   MOVE    MCPDATA-REC TO  HKNNUM-REC
                   MOVE    ZERO        TO  FLG-HKNNUM
               ELSE
                   MOVE    1           TO  FLG-HKNNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *     CALL     "MCPSUB"               USING
           MOVE    "tbl_hknnum"            TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_syskanri"             TO  MCP-TABLE
               MOVE    "key10"                    TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_syskanri"             TO  MCP-TABLE
               MOVE    "key10"                    TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI2
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI2
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI2
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
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       820-SYSKANRI-READ-SEC           SECTION.
      *
      ***     MOVE    WRK-SYSYMD           TO  ORC-DBYMD
           MOVE    "DBSELECT"           TO  MCP-FUNC
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
           IF      MCP-RC               =   ZERO
               MOVE    "DBFETCH"        TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_syskanri"             TO  MCP-TABLE
               MOVE    "key10"                    TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF      MCP-RC           =   ZERO
                   MOVE    ZERO         TO  FLG-SYSKANRI2
               ELSE
                   MOVE    1            TO  FLG-SYSKANRI2
               END-IF
           ELSE
               MOVE    1                TO  FLG-SYSKANRI2
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"            TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"                 USING
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key10"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
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
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_ptinf"           TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
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
           MOVE    "tbl_pthkninf"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-PTHKNINF-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_pthkninf"           TO  MCP-TABLE
               MOVE    "key"                    TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   MOVE    ZERO                TO  FLG-PTHKNINF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
                   INITIALIZE                  PTHKNINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTHKNINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_pthkninf"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
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
      *    患者労災マスター読込処理
      *****************************************************************
       900-PTRSIINF-READ-SEC         SECTION.
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTRSIINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptrsiinf"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-PTRSIINF-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"            USING
               MOVE    "tbl_ptrsiinf"           TO  MCP-TABLE
               MOVE    "key"                    TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                           MCPAREA
                                           MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                           SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTRSIINF-REC
                   MOVE    ZERO        TO  FLG-PTRSIINF
               ELSE
                   MOVE    1           TO  FLG-PTRSIINF
                   INITIALIZE              PTRSIINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
               INITIALIZE                  PTRSIINF-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTRSIINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptrsiinf"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           .
       900-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_hkncombi"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_hkncombi"           TO  MCP-TABLE
               MOVE    "key"                    TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   MOVE    ZERO                TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_hkncombi"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
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
      *    診療会計情報読込処理
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    SRYACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryacct"            TO  MCP-TABLE
           MOVE    "key3"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"             TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_sryacct"            TO  MCP-TABLE
               MOVE    "key3"                   TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC   TO  SRYACCT-REC
                   MOVE    ZERO                    TO  FLG-SRYACCT
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
                   INITIALIZE                  SRYACCT-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryacct"            TO  MCP-TABLE
           MOVE    "key3"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
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
      *--- 2010/04/27 ADD START ----------------------------------------
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       900-SRYACT-SELECT-SEC               SECTION.
      *
           MOVE    SRYACT-REC        TO   MCPDATA-REC
           MOVE    "DBSELECT"        TO   MCP-FUNC
           MOVE    "tbl_sryact"      TO   MCP-TABLE
           MOVE    "key2"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    ZERO            TO   FLG-SRYACT
           ELSE
             MOVE    1               TO   FLG-SRYACT
           END-IF        
           .
       900-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報FETCH
      *****************************************************************
       900-SRYACT-FETCH-SEC                SECTION.
      *
           MOVE    SRYACT-REC         TO   MCPDATA-REC
           MOVE    "DBFETCH"          TO   MCP-FUNC
           MOVE    "tbl_sryact"       TO   MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    ZERO             TO   FLG-SRYACT
             MOVE    MCPDATA-REC      TO   SRYACT-REC
           ELSE
             MOVE    1                TO   FLG-SRYACT
             INITIALIZE               SRYACT-REC
           END-IF  
           .
       900-SRYACT-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報 ＤＢ　クローズ処理
      *****************************************************************
       900-SRYACT-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"    TO   MCP-FUNC
           MOVE    "tbl_sryact"       TO   MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *
      *--- 2010/04/27 ADD END ------------------------------------------
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
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
           MOVE    "DBSTART"           TO  MCP-FUNC.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
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
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
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
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
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
