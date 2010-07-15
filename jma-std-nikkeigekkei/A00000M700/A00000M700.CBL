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
       PROGRAM-ID.             A00000M700.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 月計表（診療費請求明細）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/03/01    @@@@-半田     新規作成
      *  04/05/24    @@@@-有村     老人保健所持時の対応修正
      *  04/09/15    @@@@-本橋     MONPE化
      *  04/10/06    @@@@-本橋     日医標準レセプトソフトver2.4.0対応による
      *                            受診履歴テーブル−最終区分(lastkbn)の削除
      *  05/02/28    @@@@-鈴木     open-cobol 0.23への対応による変更
      *  05/04/19    @@@@-本橋     日レセVer2.5.0 収納テーブル取得KEY追加
      *                            への対応
      *  05/05/11    @@@@-本橋     受診者数が正常にカウントされていない不具合修正
      *  05/09/14    @@@@-本橋     その他件数欄を追加
      *                            ORCMCPSUB → MCPSUBに変更
      *  06/04/01    @@@@-T.T      同日再診時の集計誤りの修正
      *  06/08/01    @@@@-高嶋     sarge対応
      *  06/11/06    @@@@-高嶋     基本診療項目追加対応
      *  06/12/20    @@@@-高嶋     自賠責計算式変更対応
      *  06/12/22    @@@@-内田     特別療養費（068,069）を国保で扱う
      *  07/06/22    @@@@-吉川     初診、再診、その他の振り分け及び
      *                            受診者数の集計誤りの修正
      *  07/07/04    @@@@-吉川     初診、再診、その他振分け追加修正
      *  07/09/10    @@@@-高嶋     4.0対応
      *  07/12/18    @@@@-吉川     在宅等が再診で集計される不具合を修正
      *  08/03/25    @@@@-高嶋     H20年4月改正対応
      *  08/06/12    @@@@-田中     健保準拠時の自賠責の計算を修正
      *  09/05/26    @@@@-門間     生活療養費・生活負担額が保険分金額・患者負担金額・
      *                            医療収益に加算されていない不具合を修正
      *  10/02/09    @@@@-田中     治験対応
      *  10/03/09    @@@@-田中     初診判定条件追加
      *  10/04/27    @@@@-田中     再診の判定条件を修正
      *  10/07/06    @@@@-門間     労災・自賠責保険時の初診の判定条件追加
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
           COPY    "A00000M700.INC".
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
           COPY    "A00000M7001.INC".
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
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
      *--- 2010/04/27 ADD START ----------------------------------------
           03  FLG-SRYACT           PIC 9(01).
           03  FLG-JYURRK2         PIC 9(01).
      *--- 2010/04/27 ADD END ------------------------------------------
      *
      *  件数用フラグ領域
      **** 20050228 open-cobol 0.23への対応 SUZUKI
      *     03  FLG-KENSUU          PIC 9(01).
           03  FLG-KENSUU.
      **** 20050228
             05  FLG-HKN           PIC 9(01).
             05  FLG-SYUEKI        PIC 9(01).
             05  FLG-SEIKYU        PIC 9(01).
             05  FLG-RYOSYU        PIC 9(01).
             05  FLG-JIHI-G.
               07  FLG-JIHI        PIC 9(01)  OCCURS  5.
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
           03  IDX1G.
              05  IDX1             PIC 9(02).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(05).
           03  JIHIIDX             PIC 9(04).
      *--- 2010/04/27 ADD START ----------------------------------------
           03  IDX10               PIC 9(02).
           03  IDX11               PIC 9(02).
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
           03  WRK-PARA-PRTYM      PIC X(06).
           03  WRK-PARA-PRTSYMD    PIC X(08).
           03  WRK-PARA-PRTLYMD    PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH              PIC X(64).
      *
           03  WRK-RECEERR           PIC X(200).
      *
           03  WRK-PARA-PRTYMWH      PIC X(16).
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
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
           03  WRK-JIHI4             PIC 9(08).
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
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
           03  WRK-Z9-9              PIC ZZZZZZZZ9.
           03  WRK-Z9-10             PIC ZZZZZZZZZ9.
           03  WRK-S9-10             PIC ---------9.
           03  WRK-SZ-10             PIC ---------Z.
      *
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
           03  WRK-FORMID            PIC X(30).
           03  WRK-FORMID1           PIC X(30).
      *--- 2010/04/27 ADD START ----------------------------------------
           03  WRK-DENPNUM           PIC 9(07).
      *--- 2010/04/27 ADD END ------------------------------------------
      *
       01  CONST-AREA.
           03  CONST-H200401           PIC X(08)   VALUE "20080401".
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      *
      * 診療科別加算エリア
       01  SUM-AREA.
           05  SUM-JYUSHINKEN       PIC 9(05)  OCCURS  32.
           05  SUM-SHKNKEN          PIC 9(05)  OCCURS  32.
           05  SUM-RHKNKEN          PIC 9(05)  OCCURS  32.
           03  SUM-OHKNKEN          PIC 9(05)  OCCURS  32.
           05  SUM-HKNTEN           PIC 9(09)  OCCURS  32.
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *     05  SUM-TEN-TBL          OCCURS  11.
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      *     05  SUM-TEN-TBL          OCCURS  15.
           05  SUM-TEN-TBL          OCCURS  16.
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
               07  SUM-TEN          PIC 9(09)  OCCURS  32.
           05  SUM-HKNFTN           PIC S9(10) OCCURS  32.
           05  SUM-JIHIFTN          PIC S9(10) OCCURS  32.
           05  SUM-SEIKYU           PIC S9(10) OCCURS  32.
           05  SUM-RYOSYU           PIC S9(10) OCCURS  32.
      *
      * 入外区分別全診療科加算エリア
       01  SUM-AREA-GK.
           05  SUM-JYUSHINKEN-GK    PIC 9(05)  OCCURS  32.
           05  SUM-SHKNKEN-GK       PIC 9(05)  OCCURS  32.
           05  SUM-RHKNKEN-GK       PIC 9(05)  OCCURS  32.
           03  SUM-OHKNKEN-GK       PIC 9(05)  OCCURS  32.
           05  SUM-HKNTEN-GK        PIC 9(09)  OCCURS  32.
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *     05  SUM-TEN-TBL-GK       OCCURS  11.
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      *     05  SUM-TEN-TBL-GK       OCCURS  15.
           05  SUM-TEN-TBL-GK       OCCURS  16.
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
               07  SUM-TEN-GK       PIC 9(09)  OCCURS  32.
           05  SUM-HKNFTN-GK        PIC S9(10) OCCURS  32.
           05  SUM-JIHIFTN-GK       PIC S9(10) OCCURS  32.
           05  SUM-SEIKYU-GK        PIC S9(10) OCCURS  32.
           05  SUM-RYOSYU-GK        PIC S9(10) OCCURS  32.
      *
       01  WRK-SMEI-REC.
           03  WRK-SMEI-TBL      OCCURS  15000.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         05  WRK-SMEI-HOSPID        PIC X(24).
               05  WRK-SMEI-HOSPNUM       PIC 9(02).
      *=== 4.0対応 By Takashima(2007/09/10) end
               05  WRK-SMEI-SORT-KEY. 
                 07  WRK-SMEI-NYUGAIKBN   PIC X(01).
                 07  WRK-SMEI-SRYKA       PIC X(02).
                 07  WRK-SMEI-NYUHEN-YMD  PIC X(08).
                 07  WRK-SMEI-PTID        PIC 9(10).
                 07  WRK-SMEI-DENPNUM     PIC 9(07).
                 07  WRK-SMEI-DENPEDANUM  PIC 9(02).
               05  WRK-SMEI-SKYMONEY      PIC S9(07).
               05  WRK-SMEI-NYUHEN-MONEY  PIC S9(07).
      *
      *配列ソート時の退避エリア
           03  WRK-SV-SMEI-TBL            PIC X(68).
           03  WRK-SV-SMEI-DENPNUM        PIC 9(07).
      *
       01   WRK-JYUSHIN-AREA.
         03   WRK-JYUSHIN-PTID-TBL        OCCURS 50000. 
           05   WRK-JYUSHIN-PTID          PIC  9(10).
      ***  2005/05/11  MOTO  入返金日（診療日）もチェック
           05   WRK-NYUHEN-YMD            PIC  X(02).
      ***  2005/05/11  MOTO  入返金日（診療日）もチェック
      *
       01  WRK-CONS-AREA.
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973". 
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      *    公害保険番号 
           03  WRK-CONS-KOGAI-HKNNUM   PIC X(03)   VALUE   "975". 
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      *
           COPY    "CPSHELLTBL.INC".
      *     
           COPY    "ORCA-DBPATH".         
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
                                               WRK-PARA-PRTYM
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
           MOVE    "A00000M700"    TO  JOB-PGID
           MOVE    "月計表（診療費請求明細）"
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
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
      *    対象年月最初日設定
           MOVE    WRK-PARA-PRTYM      TO  WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"                TO  WRK-PARA-PRTSYMD(7:2)
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-PRTYM      TO  WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD            TO  WRK-PARA-PRTLYMD
      *
      *    対象年月日編集
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-PARA-PRTYMWH 
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
           MOVE  WRK-PARA-PRTSYMD      TO  SYS-1001-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD      TO  SYS-1001-STYUKYMD
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
           MOVE    WRK-PARA-PRTLYMD        TO  WRK-SYMD
      ***  20050914  MOTO
           MOVE  WRK-PARA-PRTSYMD          TO  SYS-4001-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD          TO  SYS-4001-STYUKYMD
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
           PERFORM  UNTIL   FLG-END     =   1
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
           MOVE 0                       TO  IDX-S
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
      ***     MOVE WRK-PARA-PRTLYMD(7:2)   TO  IDX7G
      ****
      ***     PERFORM  VARYING  IDX1       FROM  1  BY  1
      ***              UNTIL    IDX1       >     IDX7   
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
      *
            INITIALIZE                      SYUMEI-REC 
                                            FLG-SYUMEI
      *=== 4.0対応 By Takashima(2007/09/10) start
      *      MOVE    WRK-PARA-HOSPID     TO  SMEI-HOSPID
            MOVE    WRK-PARA-HOSPNUM     TO  SMEI-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
      ***      MOVE    "1"                 TO  SMEI-NYUGAIKBN
      ***      MOVE    WRK-PARA-PRTYM      TO  SMEI-NYUHEN-YMD(1:6)
      ***      MOVE    IDX1G               TO  SMEI-NYUHEN-YMD(7:2)
           MOVE    WRK-PARA-PRTSYMD TO   SMEI-NYUHEN-YMD
           MOVE    WRK-PARA-PRTLYMD TO   SMEI-UPNYUHEN-YMD
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
      *
            PERFORM 910-DBSELECT-SEC
      *
      *    収納明細テーブルより変数配列に対象データを抽出する。
      *
            PERFORM  UNTIL   FLG-SYUMEI     =   1
      *    入力された日付と入返金日が一致しているかをチェック
      * 
            PERFORM 900-SYUNOU-READ-SEC
            INITIALIZE   PTINF-REC 
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
      ***      MOVE SMEI-HOSPID TO PTINF-HOSPID
      ***      MOVE SMEI-PTID   TO PTINF-PTID
      ***      PERFORM 900-PTINF-READ-SEC
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
      *
      ***  収納情報にレコードが存在しないデータは抽出しない。
      ***  伝票状態区分 = 3(請求取消)は対象外データとして取り扱う
      ***  患者情報にレコードが存在しないデータは抽出しない。
      ***  テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う
             IF  ( WRK-PARA-PRTYM       =      SMEI-NYUHEN-YMD(1:6)
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         AND WRK-PARA-HOSPID      =      SMEI-HOSPID )
               AND WRK-PARA-HOSPNUM     =      SMEI-HOSPNUM )
      *=== 4.0対応 By Takashima(2007/09/10) end
               AND  FLG-SYUNOU          =      ZERO
               AND  SYU-DENPJTIKBN      NOT =  "3"   
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
      ***         AND  FLG-PTINF           =      ZERO
      ***         AND  PTINF-TSTPTNUMKBN   NOT =  "1"   
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
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
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
      ***      INITIALIZE                      SYUMEI-REC 
      ***                                      FLG-SYUMEI
      ***      MOVE    WRK-PARA-HOSPID     TO  SMEI-HOSPID
      ***      MOVE    "2"                 TO  SMEI-NYUGAIKBN
      ***      MOVE    WRK-PARA-PRTYM      TO  SMEI-NYUHEN-YMD(1:6)
      ***      MOVE    IDX1G               TO  SMEI-NYUHEN-YMD(7:2)
      ***      PERFORM 910-DBSELECT-SEC
      ****
      ****    収納明細テーブルより変数配列に対象データを抽出する。
      ****
      ***      PERFORM  UNTIL   FLG-SYUMEI     =   1
      ****    入力された日付と入返金日が一致しているかをチェック
      **** 
      ***      PERFORM 900-SYUNOU-READ-SEC
      ***      INITIALIZE   PTINF-REC 
      ***      MOVE SMEI-HOSPID TO PTINF-HOSPID
      ***      MOVE SMEI-PTID   TO PTINF-PTID
      ***      PERFORM 900-PTINF-READ-SEC
      ****
      ******  収納情報にレコードが存在しないデータは抽出しない。
      ******  伝票状態区分 = 3(請求取消)は対象外データとして取り扱う
      ******  患者情報にレコードが存在しないデータは抽出しない。
      ******  テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う
      ***       IF  ( WRK-PARA-PRTYM       =      SMEI-NYUHEN-YMD(1:6)
      ***         AND WRK-PARA-HOSPID      =      SMEI-HOSPID )
      ***         AND  FLG-SYUNOU          =      ZERO
      ***         AND  SYU-DENPJTIKBN      NOT =  "3"   
      ***         AND  FLG-PTINF           =      ZERO
      ***         AND  PTINF-TSTPTNUMKBN   NOT =  "1"   
      **** 
      ***              ADD 1  TO IDX-S
      ***              MOVE   SMEI-HOSPID 
      ***                   TO    WRK-SMEI-HOSPID(IDX-S)
      ***              MOVE   SMEI-NYUGAIKBN 
      ***                   TO    WRK-SMEI-NYUGAIKBN(IDX-S)
      ***              MOVE   SMEI-SRYKA 
      ***                   TO    WRK-SMEI-SRYKA(IDX-S)
      ***              MOVE   SMEI-PTID 
      ***                   TO    WRK-SMEI-PTID(IDX-S)
      ***              MOVE   SMEI-DENPNUM 
      ***                   TO    WRK-SMEI-DENPNUM(IDX-S)
      ***              MOVE    SMEI-DENPEDANUM 
      ***                   TO    WRK-SMEI-DENPEDANUM(IDX-S)
      ***              MOVE   SMEI-SKYMONEY 
      ***                   TO    WRK-SMEI-SKYMONEY(IDX-S)
      ***              MOVE   SMEI-NYUHEN-MONEY 
      ***                   TO    WRK-SMEI-NYUHEN-MONEY(IDX-S)
      ***              MOVE   SMEI-NYUHEN-YMD 
      ***                   TO    WRK-SMEI-NYUHEN-YMD(IDX-S)
      ***       END-IF
      ***       PERFORM 910-DBFETCH-SEC
      ***      END-PERFORM 
      ***      PERFORM 910-DBCLOSE-SEC
      ***    END-PERFORM
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
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
      *    帳票集計処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           MOVE    ZERO                        TO  CNT-LINE
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
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
            IF    ( SYU-SRYYMD              >=  CONST-H200401 )
                MOVE    "A00000M700N.red"       TO  WRK-FORMID
                MOVE    "A00000M7001N.red"      TO  WRK-FORMID1
            ELSE
                MOVE    "A00000M700O.red"       TO  WRK-FORMID
                MOVE    "A00000M7001O.red"      TO  WRK-FORMID1
            END-IF
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      *
      *     入外区分ブレイクチェック処理
            IF  WRK-SMEI-NYUGAIKBN(IDX-S)  NOT = WRK-NYUGAIKBN
      *      合計カウンターに加算
                PERFORM  VARYING     IDX1   FROM 1   BY 1
                         UNTIL       IDX1   >    32    
                         ADD  SUM-SHKNKEN(IDX1)
                                     TO     SUM-SHKNKEN-GK(IDX1)
                         ADD  SUM-RHKNKEN(IDX1)
                                     TO     SUM-RHKNKEN-GK(IDX1)
      ***  20050914  MOTO
                         ADD  SUM-OHKNKEN(IDX1)
                                     TO     SUM-OHKNKEN-GK(IDX1)
      ***  20050914  MOTO
                         ADD  SUM-HKNTEN(IDX1)
                                     TO     SUM-HKNTEN-GK(IDX1)
                         PERFORM VARYING    IDX2   FROM  1  BY 1
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *                           UNTIL      IDX2   >    11 
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      *                           UNTIL      IDX2   >    15 
                                 UNTIL      IDX2   >    16 
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                         ADD SUM-TEN(IDX2 IDX1)
                                     TO     SUM-TEN-GK(IDX2 IDX1)
                         END-PERFORM
                         ADD  SUM-HKNFTN(IDX1)
                                     TO     SUM-HKNFTN-GK(IDX1)
                         ADD  SUM-JIHIFTN(IDX1)
                                     TO     SUM-JIHIFTN-GK(IDX1)
                         ADD  SUM-SEIKYU(IDX1)
                                     TO     SUM-SEIKYU-GK(IDX1)
                         ADD  SUM-RYOSYU(IDX1)
                                     TO     SUM-RYOSYU-GK(IDX1)
                END-PERFORM
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
                PERFORM  VARYING     IDX1   FROM 1   BY 1
                         UNTIL       IDX1   >    32    
                         ADD  SUM-SHKNKEN(IDX1)
                                     TO     SUM-SHKNKEN-GK(IDX1)
                         ADD  SUM-RHKNKEN(IDX1)
                                     TO     SUM-RHKNKEN-GK(IDX1)
      ***  20050914  MOTO
                         ADD  SUM-OHKNKEN(IDX1)     
                                     TO     SUM-OHKNKEN-GK(IDX1)
      ***  20050914  MOTO
                         ADD  SUM-HKNTEN(IDX1)
                                     TO     SUM-HKNTEN-GK(IDX1)
                         PERFORM VARYING    IDX2   FROM  1  BY 1
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *                           UNTIL      IDX2   >    11 
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      *                           UNTIL      IDX2   >    15 
                                 UNTIL      IDX2   >    16 
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                         ADD SUM-TEN(IDX2 IDX1)
                                     TO     SUM-TEN-GK(IDX2 IDX1)
                         END-PERFORM
                         ADD  SUM-HKNFTN(IDX1)
                                     TO     SUM-HKNFTN-GK(IDX1)
                         ADD  SUM-JIHIFTN(IDX1)
                                     TO     SUM-JIHIFTN-GK(IDX1)
                         ADD  SUM-SEIKYU(IDX1)
                                     TO     SUM-SEIKYU-GK(IDX1)
                        ADD  SUM-RYOSYU(IDX1)
                                     TO     SUM-RYOSYU-GK(IDX1)
                END-PERFORM
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
      *     診療日別インデックス値の設定
            MOVE  WRK-SMEI-NYUHEN-YMD(IDX-S)(7:2) TO  IDX1G 
      *
      ***   伝票番号枝番が１の場合は保険点数、保険別請求額の明細を印字(開始)
             IF   WRK-SMEI-DENPEDANUM(IDX-S)   =     1
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
      *            PERFORM 900-JYURRK-READ-SEC
      *            PERFORM 320-GAIRAI-CNT-SEC
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
             IF   WRK-SMEI-SKYMONEY(IDX-S)  NOT =  0
                   ADD WRK-SMEI-SKYMONEY(IDX-S)
                           TO   SUM-SEIKYU(IDX1)
                                SUM-SEIKYU(32)
                   MOVE    1    TO   FLG-SEIKYU
             END-IF
      *    領収金額
             IF   WRK-SMEI-NYUHEN-MONEY(IDX-S)  NOT =  0
                   ADD WRK-SMEI-NYUHEN-MONEY(IDX-S)
                           TO  SUM-RYOSYU(IDX1)
                               SUM-RYOSYU(32)
                   MOVE    1    TO   FLG-RYOSYU
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
           PERFORM  VARYING     IDX1   FROM 1   BY 1
                    UNTIL       IDX1   >    32    
                         ADD  SUM-SHKNKEN(IDX1)
                                     TO     SUM-SHKNKEN-GK(IDX1)
                         ADD  SUM-RHKNKEN(IDX1)
                                     TO     SUM-RHKNKEN-GK(IDX1)
      ***  20050914  MOTO
                         ADD  SUM-OHKNKEN(IDX1)     
                                     TO     SUM-OHKNKEN-GK(IDX1)
      ***  20050914  MOTO
                         ADD  SUM-HKNTEN(IDX1)
                                     TO     SUM-HKNTEN-GK(IDX1)
                         PERFORM VARYING    IDX2   FROM  1  BY 1
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *                           UNTIL      IDX2   >    11 
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      *                           UNTIL      IDX2   >    15 
                                 UNTIL      IDX2   >    16 
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                         ADD SUM-TEN(IDX2 IDX1)
                                     TO     SUM-TEN-GK(IDX2 IDX1)
                         END-PERFORM
                         ADD  SUM-HKNFTN(IDX1)
                                     TO     SUM-HKNFTN-GK(IDX1)
                         ADD  SUM-JIHIFTN(IDX1)
                                     TO     SUM-JIHIFTN-GK(IDX1)
                         ADD  SUM-SEIKYU(IDX1)
                                     TO     SUM-SEIKYU-GK(IDX1)
                        ADD  SUM-RYOSYU(IDX1)
                                     TO     SUM-RYOSYU-GK(IDX1)
           END-PERFORM
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
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *              UNTIL   IDZ >       11
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      *              UNTIL   IDZ >       15
                    UNTIL   IDZ >       16
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                    ADD     SYU-HKNTEN (IDZ)
                            TO  SYU-TOTAL-HKNTEN
                    ADD     SYU-HKNTEN (IDZ)
                            TO  SUM-TEN (IDZ IDX1)
                                SUM-TEN (IDZ 32)
            END-PERFORM
            ADD     SYU-TOTAL-HKNTEN  TO  SUM-HKNTEN(IDX1)
                                          SUM-HKNTEN(32)
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
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      ***               "公害"
                WHEN    WRK-CONS-KOGAI-HKNNUM
                        MOVE  7          TO  WRK-SYUHKN-KBN
      ***               "後期高齢者"
                WHEN    "039"
                WHEN    "040"
                        MOVE  8          TO  WRK-SYUHKN-KBN
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
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
      *    保険負担額
               IF   WRK-SYUHKN-KBN   =  1
                OR  WRK-SYUHKN-KBN   =  2
                OR  WRK-SYUHKN-KBN   =  5
                OR  WRK-SYUHKN-KBN   =  6
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
                OR  WRK-SYUHKN-KBN   =  7
                OR  WRK-SYUHKN-KBN   =  8
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
                    EVALUATE  WRK-SYUHKN-KBN
                    WHEN  1 THRU 2
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
                    WHEN  8
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      ***             国保、社保の場合
      ***** 04/05/24 DEL 
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
      * 20080612 健保準拠時の自賠責の計算を修正START tanaka
                        IF  SYU-PTFTNRATE NOT =  ZERO
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
      * 20080612 健保準拠時の自賠責の計算を修正END
      *=== 自賠責計算式変更対応 By Takashima(2006/12/20) start
      *                  COMPUTE  WRK-RSI-MONEY1 = 
      *                  SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA
      *                  COMPUTE  WRK-RSI-MONEY2 =
      *                  WRK-RSI-MONEY1 * (SYS-4001-KSNRATE / 100)
      *                  COMPUTE  WRK-MONEY  = 
      *                     WRK-RSI-MONEY1
      *                  +  WRK-RSI-MONEY2
      * 20080612 健保準拠時の自賠責の計算を修正START tanaka
      *               COMPUTE  WRK-MONEY ROUNDED = 
      *              (SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA)
      *               COMPUTE  WRK-MONEY ROUNDED =
      *                      ( WRK-MONEY * (SYS-4001-KSNRATE1 / 100) ) 
      *                  COMPUTE  WRK-MONEY =
      *                     WRK-MONEY
      *=== 自賠責計算式変更対応 By Takashima(2006/12/20) end
      *                  +  SYU-RSISHOSHIN-MONEY 
      *                  +  SYU-RSISAISHIN-MONEY 
      *                  +  SYU-RSISDO-MONEY 
      *                  +  SYU-RSIETC-MONEY
      *                  +  SYU-RYOYOHI-SKJ  
      * 20080612 健保準拠時の自賠責の計算を修正END
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
                    WHEN   7
      ***             公害の場合
                        COMPUTE WRK-JIHI4  =  SYU-TOTAL-TGMONEY
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
                                            -   WRK-JIHI4
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
                    END-EVALUATE
      ***  2009/05/26 ADD START ----------------------------------------
                    COMPUTE WRK-MONEY  =  WRK-MONEY
                                       -  SYU-SKYMONEY-SKJ-KEI
                                       -  SYU-SKYMONEY-LIFE-KEI
      ***  2009/05/26 ADD END   ----------------------------------------
                    ADD    WRK-MONEY  TO  SUM-HKNFTN(IDX1)
                                          SUM-HKNFTN(32)
               END-IF  
      *
      *    自費負担学
      *
               IF   WRK-SYUHKN-KBN   =  1
                OR  WRK-SYUHKN-KBN   =  2
                OR  WRK-SYUHKN-KBN   =  3
                OR  WRK-SYUHKN-KBN   =  4
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
                OR  WRK-SYUHKN-KBN   =  8
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
                    EVALUATE WRK-SYUHKN-KBN
                    WHEN 1 THRU 2
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
                    WHEN 8
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
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
      ***** 04/05/20 INS END
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
                 ADD   WRK-MONEY  TO  SUM-JIHIFTN(IDX1)
                                      SUM-JIHIFTN(32)
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
               ADD   1  TO   SUM-RHKNKEN(IDX1)
                             SUM-RHKNKEN(32)
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
           MOVE  JYURRK-SRYKA       TO  ACCT-SRYKA
           MOVE  JYURRK-SRYYMD(1:6) TO  ACCT-SRYYM
           MOVE "11"                TO  ACCT-SRYKBN
      *=== 初診、再診、その他振分け追加修正 ADD By Yoshikawa(2007/07/04) start
      *    PERFORM 900-SRYACCT-READ-SEC
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
               ELSE
                   MOVE  1            TO  FLG-SRYACCT
                   INITIALIZE             SRYACCT-REC
               END-IF
           ELSE
               MOVE  1            TO  FLG-SRYACCT
               INITIALIZE             SRYACCT-REC
           END-IF
      *  初診データ探索処理
           MOVE    ZERO    TO    FLG-INPUT
           PERFORM   UNTIL   FLG-SRYACCT  =  1
              IF  ACCT-DAY(1 IDX7)   >   ZERO
      *  初診データが見つかったら入力フラグをオン
                 MOVE  1   TO   FLG-INPUT
              END-IF
      *  診療会計情報FETCH
              MOVE  SRYACCT-REC     TO  MCPDATA-REC
              MOVE  "DBFETCH"       TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *        MOVE  PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *        CALL  "MCPSUB"        USING
              MOVE    "tbl_sryacct"          TO  MCP-TABLE
              MOVE    "key3"                 TO  MCP-PATHNAME
              CALL    "ORCDBMAIN"   USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                    MCPAREA
                                    MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                    SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
              IF     MCP-RC     =   ZERO
                 MOVE   MCPDATA-REC   TO  SRYACCT-REC
                 MOVE   ZERO          TO  FLG-SRYACCT
              ELSE
                 MOVE   1             TO  FLG-SRYACCT
                 INITIALIZE               SRYACCT-REC
              END-IF
           END-PERFORM
      *  診療会計情報CLOSE
           MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE  PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *     CALL  "MCPSUB"         USING
           MOVE    "tbl_sryacct"          TO  MCP-TABLE
           MOVE    "key3"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"    USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                  MCPAREA
                                  MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                  SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
               MOVE    ZERO           TO   FLG-JYURRK2
             ELSE
               MOVE    1              TO   FLG-JYURRK2
             END-IF
           ELSE
             MOVE    1                TO   FLG-JYURRK2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"    TO   MCP-FUNC
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key22"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *--- 2010/04/27 ADD END ------------------------------------------
      *  入力フラグがオンの場合、診療会計終了フラグを切る
           IF   FLG-INPUT       =   1
              MOVE   ZERO      TO  FLG-SRYACCT
           END-IF
      *=== 初診、再診、その他振分け追加修正 ADD By Yoshikawa(2007/07/04) end
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
                  AND   (JYURRK-DOUJI-RENNUM  =  1  
                  OR   JYURRK-DOUJI-RENNUM  =  ZERO)  
      *--- 2010/03/09 ADD END ------------------------------------------
      *  初診の件数としてカウントする。 
      ***  保険点数件数カウント 
                   IF  FLG-HKN = 1
                     ADD    1       TO   SUM-SHKNKEN(IDX1)
                                         SUM-SHKNKEN(32)
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
      *           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
      *           MOVE    "key5"               TO  MCP-PATHNAME
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
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                               MCPAREA
                                               MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                               SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *=== 初診、再診、その他振分け追加修正 ADD By Yoshikawa(2007/07/04) end
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
      *
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
      *=== 在宅等の誤振分け対応 ADD By Yoshikawa(2007/12/18) end
      *
      ***  20050914  MOTO
      *
      *  再診の件数としてカウントする。
      ***  保険点数件数カウント 
                   IF  FLG-HKN  =  1
                     ADD    1       TO   SUM-RHKNKEN(IDX1)
                                         SUM-RHKNKEN(32)
      *--- 2010/04/27 ADD START ----------------------------------------
                     MOVE    JYURRK-DENPNUM   TO   WRK-DENPNUM
      *--- 2010/04/27 ADD END ------------------------------------------
                   END-IF 
      *
      ***  20050914  MOTO
                 ELSE
      *--- 2010/04/27 ADD START ----------------------------------------
                   IF   FLG-JYURRK2  =  1
      *--- 2010/04/27 ADD END ------------------------------------------
      *  その他の件数としてカウントする。
      ***  保険点数件数カウント 
                     IF  FLG-HKN = 1
                         ADD    1     TO   SUM-OHKNKEN(IDX1)
                                           SUM-OHKNKEN(32)
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
               PERFORM  VARYING  IDX10 FROM  1  BY  1
                        UNTIL  IDX10  >  15
                 MOVE  JYURRK-ZAINUM(IDX10)    TO  ACCT-ZAINUM
                 MOVE  JYURRK-HOSPNUM          TO  ACCT-HOSPNUM
                 MOVE  JYURRK-NYUGAIKBN        TO  ACCT-NYUGAIKBN
                 MOVE  JYURRK-PTID             TO  ACCT-PTID
                 MOVE  JYURRK-SRYKA            TO  ACCT-SRYKA
                 MOVE  JYURRK-SRYYMD(1:6)      TO  ACCT-SRYYM
                 MOVE  SRYACCT-REC             TO  MCPDATA-REC
                 MOVE  "DBSELECT"              TO  MCP-FUNC
                 MOVE  "tbl_sryacct"           TO  MCP-TABLE
                 MOVE  "key5"                  TO  MCP-PATHNAME
                 CALL  "ORCDBMAIN"             USING
                                                   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
                 IF     MCP-RC             =  ZERO
                   MOVE  "DBFETCH"             TO  MCP-FUNC
                   MOVE  "tbl_sryacct"         TO  MCP-TABLE
                   MOVE  "key5"                TO  MCP-PATHNAME
                   CALL  "ORCDBMAIN"           USING
                                                   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
                   IF     MCP-RC           =  ZERO
                     MOVE MCPDATA-REC          TO  SRYACCT-REC
                     MOVE ZERO                 TO  FLG-SRYACCT
                   ELSE
                     MOVE   1                  TO  FLG-SRYACCT
                     INITIALIZE                    SRYACCT-REC
                   END-IF
                 ELSE
                   MOVE    1                   TO  FLG-SRYACCT
                   INITIALIZE                      SRYACCT-REC
                 END-IF
      *
                 MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
                 MOVE  "tbl_sryacct"           TO  MCP-TABLE
                 MOVE  "key5"                  TO  MCP-PATHNAME
                 CALL  "ORCDBMAIN"             USING
                                                   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
                 IF  IDX10  <  15
                   COMPUTE   IDX11   =   IDX10  +   1
                   IF   JYURRK-ZAINUM(IDX11)  =  ZERO
                     MOVE    16                TO   IDX10
                   END-IF
                 END-IF
                 IF   ACCT-SRYKBN   =  "11"  AND  "12"
                   MOVE    16                  TO   IDX10
                 END-IF
               END-PERFORM
               MOVE    ACCT-HOSPNUM            TO   SRY-HOSPNUM
               MOVE    ACCT-NYUGAIKBN          TO   SRY-NYUGAIKBN
               MOVE    ACCT-PTID               TO   SRY-PTID
               MOVE    ACCT-SRYKA              TO   SRY-SRYKA
               MOVE    ACCT-SRYYM              TO   SRY-SRYYM
               MOVE    ACCT-ZAINUM             TO   SRY-ZAINUM
               PERFORM  900-SRYACT-SELECT-SEC
               PERFORM  900-SRYACT-FETCH-SEC
               PERFORM  900-SRYACT-CLOSE-SEC
      *
               IF     FLG-SRYACCT    =  ZERO
                 AND   JYURRK-SRYKBN(1)  >  "00"
                 AND   (JYURRK-DOUJI-RENNUM  =  1  
                 OR   JYURRK-DOUJI-RENNUM  =  ZERO)  
      *  初診の件数としてカウントする。
      ***  保険点数件数カウント 
                 IF  FLG-HKN = 1
                   ADD    1                    TO   SUM-SHKNKEN(IDX1)
                                                    SUM-SHKNKEN(32)
                   MOVE    JYURRK-DENPNUM      TO   WRK-DENPNUM
                 END-IF 
               ELSE
      *
                 IF     FLG-SRYACCT    =  ZERO
                   AND   ACCT-DAY(1 IDX7)  >  ZERO
                   AND   SRY-SRYKBN   =   "12"
                   AND   ACCT-SRYKBN       =  "12"
      *
      *  再診の件数としてカウントする。
      ***  保険点数件数カウント 
                   IF  FLG-HKN  =  1
                     ADD    1                  TO   SUM-RHKNKEN(IDX1)
                                                    SUM-RHKNKEN(32)
                     MOVE    JYURRK-DENPNUM    TO   WRK-DENPNUM
                   END-IF 
      *
                 ELSE
                   IF   FLG-JYURRK2  =  1
      *  その他の件数としてカウントする。
      ***  保険点数件数カウント 
                     IF  FLG-HKN = 1
                         ADD    1              TO   SUM-OHKNKEN(IDX1)
                                                    SUM-OHKNKEN(32)
                     END-IF
                   END-IF
                 END-IF
      *
               END-IF
             END-IF
      *--- 2010/07/06 ADD START ----------------------------------------
      *
      *    現在処理中のレコードが同一患者のレコードでない場合は、
      *    受診者数のカウントを行う。(同日診療の場合は実患者数をカウントする。)
      *    但し、現状は科別集計においては受診者数の印字は行わない。
             MOVE     1        TO      IDX3 
             PERFORM  UNTIL    IDX3    >      50000
                  IF  (WRK-SMEI-PTID(IDX-S) =  WRK-JYUSHIN-PTID(IDX3))
      ***  2005/05/11  MOTO  入返金日（診療日）もチェック
                  AND (WRK-SMEI-NYUHEN-YMD(IDX-S)(7:2)
                              =  WRK-NYUHEN-YMD(IDX3))
      ***  2005/05/11  MOTO  入返金日（診療日）もチェック
      *
      * 既に同日の診療が受診件数として計上されている場合は
      * 全科合計の受診者数に計上しない.
      ***  2006/04/01 T.T BEGIN 
      *               MOVE 99999   TO      IDX3
                      MOVE 50000   TO      IDX3
      ***  2006/04/01 T.T END 
                  END-IF
                  IF  WRK-JYUSHIN-PTID(IDX3)  =  ZERO
      ***  2005/05/11  MOTO  入返金日（診療日）もチェック
                      AND  WRK-NYUHEN-YMD(IDX3) = SPACE
      ***  2005/05/11  MOTO  入返金日（診療日）もチェック
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
      ***  2005/05/11  MOTO  入返金日（診療日）もチェック
                      MOVE WRK-SMEI-NYUHEN-YMD(IDX-S)(7:2)
                           TO  WRK-NYUHEN-YMD(IDX3)
      ***  2005/05/11  MOTO  入返金日（診療日）もチェック
                      ADD  1       TO      SUM-JYUSHINKEN-GK(IDX1)
                                           SUM-JYUSHINKEN-GK(32)
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
           INITIALIZE                       M700      
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
           INITIALIZE                       M7001      
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
      *
           MOVE    WRK-SYSYMDWH         TO  M700-SYSYMD
           MOVE    WRK-PARA-PRTYMWH     TO  M700-PRTYM
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
           MOVE    WRK-SYSYMDWH         TO  M7001-SYSYMD
           MOVE    WRK-PARA-PRTYMWH     TO  M7001-PRTYM
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
      *
           EVALUATE WRK-NYUGAIKBN
              WHEN "1"
                   MOVE "入院"          TO  M700-NYUGAIKBN
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
                   MOVE "入院"          TO  M7001-NYUGAIKBN
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                   MOVE  "請求件数"     TO  M700-KENSU2
              WHEN "2"
                   MOVE "外来"          TO  M700-NYUGAIKBN
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
                   MOVE "外来"          TO  M7001-NYUGAIKBN
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
      ***  20050914  MOTO
                   MOVE  "その他件数"   TO  M700-KENSU3
      ***  20050914  MOTO
                   MOVE  "再診件数"     TO  M700-KENSU2
                   MOVE  "初診件数"     TO  M700-KENSU1
           END-EVALUATE
      *
      *    診療科名称取得
           INITIALIZE                       SYS-1005-REC
           MOVE    "1005"               TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA            TO  SYS-1005-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO  SYS-1005-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD     TO  WRK-SYMD
      ***  20050914  MOTO
           MOVE  WRK-PARA-PRTSYMD       TO  SYS-1005-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD       TO  SYS-1005-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1005-REC         TO  MCPDATA-REC
           PERFORM 820-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC        TO  SYS-1005-REC
               MOVE  SYS-1005-SRYKANAME TO  M700-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
               MOVE  SYS-1005-SRYKANAME TO  M7001-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
           ELSE
               MOVE  SPACE              TO  M700-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
               MOVE  SPACE              TO  M7001-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
           END-IF                                     
      *
          IF  FLG-GET = ZERO
      *  対象データが１件も存在しなかった場合
             MOVE  "＊＊＊  対象データが１件も存在しません。＊＊＊"
                     TO  M700-JOBMSG
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
             MOVE  "＊＊＊  対象データが１件も存在しません。＊＊＊"
                     TO  M7001-JOBMSG
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M700-PAGE
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M7001-PAGE
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
          ELSE
      *  対象データが１件以上存在した場合
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M700-PAGE
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M7001-PAGE
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
      *
      *  月末最終日の設定
             MOVE WRK-PARA-PRTLYMD(7:2)      TO  IDX7G
             PERFORM VARYING   IDX1    FROM  1   BY  1
                     UNTIL     IDX1    >     IDX7                 
      *
      *日別
      *  保険点数、件数合計
              MOVE    SUM-HKNTEN(IDX1)   TO  WRK-Z9-9 
              MOVE    WRK-Z9-9           TO  M700-GHKNTEN(IDX1)
              IF WRK-NYUGAIKBN =   "2" 
                 MOVE    SUM-SHKNKEN(IDX1)   TO  WRK-Z9-5
                 MOVE    WRK-Z9-5            TO  M700-SHKNKEN(IDX1)
      ***  20050914  MOTO
                 MOVE    SUM-OHKNKEN(IDX1)   TO  WRK-Z9-5
                 MOVE    WRK-Z9-5            TO  M700-OHKNKEN(IDX1)
      ***  20050914  MOTO
              END-IF
              MOVE    SUM-RHKNKEN(IDX1)  TO  WRK-Z9-5
              MOVE    WRK-Z9-5           TO  M700-RHKNKEN(IDX1)
      *
      *  診療区分別点数
              PERFORM VARYING IDX FROM    1   BY  1
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *                UNTIL   IDX   >    11
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      *                UNTIL   IDX   >    15
                      UNTIL   IDX   >    16
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                      MOVE    SUM-TEN (IDX IDX1)
                                         TO   WRK-Z9-9 
                      MOVE    WRK-Z9-9   TO   M700-TEN (IDX IDX1) 
              END-PERFORM
      *
      *  金額合計
              MOVE  SUM-HKNFTN(IDX1)     TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10            TO   M700-HKNFTN(IDX1)
              MOVE  WRK-S9-10            TO   M7001-HKNFTN(IDX1)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
              MOVE  SUM-JIHIFTN(IDX1)    TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10            TO   M700-JIHIFTN(IDX1)
              MOVE  WRK-S9-10            TO   M7001-JIHIFTN(IDX1)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
              MOVE  SUM-SEIKYU(IDX1)     TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10            TO   M700-GSEIKYU(IDX1)
              MOVE  WRK-S9-10            TO   M7001-GSEIKYU(IDX1)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
              MOVE  SUM-RYOSYU(IDX1)     TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10            TO   M700-GRYOSYU(IDX1)
              MOVE  WRK-S9-10            TO   M7001-GRYOSYU(IDX1)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
      *
      * 受診者数
      ***     IF WRK-NYUGAIKBN =   "2" 
      ***       MOVE  SUM-JYUSHINKEN(IDX1) TO   WRK-Z9-5
      ***       MOVE  WRK-Z9-5             TO   M700-JYUSHINCNT(IDX1)
      ***     END-IF 
      *
             END-PERFORM
      *
      *月計
      *  保険点数、件数
              MOVE    SUM-HKNTEN(32)     TO  WRK-Z9-9 
              MOVE    WRK-Z9-9           TO  M700-GHKNTEN(32)
              IF WRK-NYUGAIKBN =   "2" 
                 MOVE    SUM-SHKNKEN(32) TO  WRK-Z9-5
                 MOVE    WRK-Z9-5        TO  M700-SHKNKEN(32)
      ***  20050914  MOTO
                 MOVE    SUM-OHKNKEN(32) TO  WRK-Z9-5
                 MOVE    WRK-Z9-5        TO  M700-OHKNKEN(32)
      ***  20050914  MOTO
              END-IF
              MOVE    SUM-RHKNKEN(32)    TO  WRK-Z9-5
              MOVE    WRK-Z9-5           TO  M700-RHKNKEN(32)
      *
      *  診療区分別点数
              PERFORM VARYING IDX FROM    1   BY  1
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *                UNTIL   IDX   >    11
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      *                UNTIL   IDX   >    15
                      UNTIL   IDX   >    16
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                      MOVE    SUM-TEN (IDX 32)
                                         TO   WRK-Z9-9 
                      MOVE    WRK-Z9-9   TO   M700-TEN (IDX 32) 
              END-PERFORM
      *
      *  金額
              MOVE  SUM-HKNFTN(32)       TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10            TO   M700-HKNFTN(32)
              MOVE  WRK-S9-10            TO   M7001-HKNFTN(32)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
              MOVE  SUM-JIHIFTN(32)      TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10            TO   M700-JIHIFTN(32)
              MOVE  WRK-S9-10            TO   M7001-JIHIFTN(32)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
              MOVE  SUM-SEIKYU(32)       TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10            TO   M700-GSEIKYU(32)
              MOVE  WRK-S9-10            TO   M7001-GSEIKYU(32)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
              MOVE  SUM-RYOSYU(32)       TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10            TO   M700-GRYOSYU(32)
              MOVE  WRK-S9-10            TO   M7001-GRYOSYU(32)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
      *
      *  受診者数
      ***     IF WRK-NYUGAIKBN =   "2" 
      ***       MOVE  SUM-JYUSHINKEN(32)   TO   WRK-Z9-5
      ***       MOVE  WRK-Z9-5             TO   M700-JYUSHINCNT(32)
      ***     END-IF 
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
      ***  20040914 MOTOHASI MONPEに変更
      ***     MOVE   "A00000M700"         TO  SPRT-PRTID
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      *     MOVE   "A00000M700.red"         TO  SPRT-PRTID
           MOVE    WRK-FORMID          TO  SPRT-PRTID
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      ***  20040914 MOTOHASI MONPEに変更
           MOVE    "月計表（診療費請求明細）"
                                       TO  SPRT-TITLE
           MOVE    M700                TO  SPRT-PRTDATA
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
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      *         MOVE   "A00000M7001.red"    TO  SPRT-PRTID
               MOVE    WRK-FORMID1         TO  SPRT-PRTID
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
               MOVE    "月計表（診療費請求明細）"
                                           TO  SPRT-TITLE
               MOVE    M7001               TO  SPRT-PRTDATA
               MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
               MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
               MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
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
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
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
           INITIALIZE                      M700      
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
           INITIALIZE                      M7001      
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
      *
           MOVE    WRK-SYSYMDWH        TO  M700-SYSYMD
           MOVE    WRK-PARA-PRTYMWH    TO  M700-PRTYM
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
           MOVE    WRK-SYSYMDWH        TO  M7001-SYSYMD
           MOVE    WRK-PARA-PRTYMWH    TO  M7001-PRTYM
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
      *
           EVALUATE WRK-NYUGAIKBN
              WHEN "1"
                   MOVE "入院"         TO  M700-NYUGAIKBN
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
                   MOVE "入院"         TO  M7001-NYUGAIKBN
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                   MOVE  "請求件数"    TO  M700-KENSU2
              WHEN "2"
                   MOVE "外来"         TO  M700-NYUGAIKBN
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
                   MOVE "外来"         TO  M7001-NYUGAIKBN
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
      ***  20050914  MOTO
                   MOVE  "その他件数"  TO  M700-KENSU3
      ***  20050914  MOTO
                   MOVE  "再診件数"    TO  M700-KENSU2
                   MOVE  "初診件数"    TO  M700-KENSU1
                   MOVE  "受診者数"    TO  M700-TJYUSHIN
           END-EVALUATE
      *
      * 診療科名称 
           EVALUATE WRK-NYUGAIKBN
               WHEN "1"
                    IF  WRK-PRNSRKA-CNT  >   1
      *    診療科が２つ以上の場合は全科合計を設定する。
                        MOVE  "全科合計"    TO  M700-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
                        MOVE  "全科合計"    TO  M7001-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                    ELSE
      *    診療科名称取得
      *    診療科が１つの場合は診療科名称を設定する。
                       INITIALIZE                    SYS-1005-REC
                       MOVE  "1005"              TO  SYS-1005-KANRICD
                       MOVE  WRK-SRYKA           TO  SYS-1005-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
                       MOVE  SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
                       MOVE  WRK-PARA-PRTLYMD    TO  WRK-SYMD
      ***  20050914  MOTO
                       MOVE  WRK-PARA-PRTSYMD    TO  SYS-1005-EDYUKYMD
                       MOVE  WRK-PARA-PRTLYMD    TO  SYS-1005-STYUKYMD
      ***  20050914  MOTO
                       MOVE  SYS-1005-REC        TO  MCPDATA-REC
                       PERFORM 820-SYSKANRI-READ-SEC
                       IF    FLG-SYSKANRI2        =   ZERO
                        MOVE  MCPDATA-REC        TO  SYS-1005-REC
                        MOVE  SYS-1005-SRYKANAME TO  M700-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
                        MOVE  SYS-1005-SRYKANAME TO  M7001-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                       ELSE
                        MOVE  SPACE              TO  M700-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
                        MOVE  SPACE              TO  M7001-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                       END-IF                                     
                    END-IF
               WHEN "2"
                    IF  WRK-PRGSRKA-CNT  >   1
      *    診療科が２つ以上の場合は全科合計を設定する。
                        MOVE  "全科合計"         TO  M700-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
                        MOVE  "全科合計"         TO  M7001-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                    ELSE
      *    診療科名称取得
      *    診療科が１つの場合は診療科名称を設定する。
                       INITIALIZE                    SYS-1005-REC
                       MOVE  "1005"              TO  SYS-1005-KANRICD
                       MOVE  WRK-SRYKA           TO  SYS-1005-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
                       MOVE  SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
                       MOVE  WRK-PARA-PRTLYMD    TO  WRK-SYMD
      ***  20050914  MOTO
                       MOVE  WRK-PARA-PRTSYMD    TO  SYS-1005-EDYUKYMD
                       MOVE  WRK-PARA-PRTLYMD    TO  SYS-1005-STYUKYMD
      ***  20050914  MOTO
                       MOVE  SYS-1005-REC        TO  MCPDATA-REC
                       PERFORM 820-SYSKANRI-READ-SEC
                       IF    FLG-SYSKANRI2        =   ZERO
                        MOVE  MCPDATA-REC        TO  SYS-1005-REC
                        MOVE  SYS-1005-SRYKANAME TO  M700-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
                        MOVE  SYS-1005-SRYKANAME TO  M7001-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                       ELSE
                        MOVE  SPACE              TO  M700-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
                        MOVE  SPACE              TO  M7001-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                       END-IF                                     
                    END-IF
           END-EVALUATE
      * 20030620 変更
      *
          IF  FLG-GET = ZERO
      *  対象データが１件も存在しなかった場合
             MOVE  "＊＊＊  対象データが１件も存在しません。＊＊＊"
                     TO  M700-JOBMSG
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
             MOVE  "＊＊＊  対象データが１件も存在しません。＊＊＊"
                     TO  M7001-JOBMSG
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M700-PAGE
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M7001-PAGE
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
          ELSE
      *
      *  対象データが１件以上存在した場合
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M700-PAGE
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
             ADD     1         TO  CNT-PAGE
             MOVE    CNT-PAGE  TO  WRK-PAGE
             MOVE    WRK-PAGE  TO  M7001-PAGE
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
      *
      *  月末最終日の設定
             MOVE WRK-PARA-PRTLYMD(7:2)      TO  IDX7G
             PERFORM VARYING   IDX1    FROM  1   BY  1
                     UNTIL     IDX1    >     IDX7                 
      *
      *日別
      *  保険点数、件数
              MOVE    SUM-HKNTEN-GK(IDX1)    TO  WRK-Z9-9
              MOVE    WRK-Z9-9               TO  M700-GHKNTEN(IDX1)
              IF WRK-NYUGAIKBN =   "2" 
                MOVE    SUM-SHKNKEN-GK(IDX1) TO  WRK-Z9-5
                MOVE    WRK-Z9-5             TO  M700-SHKNKEN(IDX1)
      ***  20050914  MOTO
                MOVE    SUM-OHKNKEN-GK(IDX1) TO  WRK-Z9-5
                MOVE    WRK-Z9-5             TO  M700-OHKNKEN(IDX1)
      ***  20050914  MOTO
              END-IF
              MOVE    SUM-RHKNKEN-GK(IDX1)   TO  WRK-Z9-5
              MOVE    WRK-Z9-5               TO  M700-RHKNKEN(IDX1)
      *
      *  診療区分別点数
              PERFORM VARYING IDX FROM    1   BY  1
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *                UNTIL   IDX   >    11
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      *                UNTIL   IDX   >    15
                      UNTIL   IDX   >    16
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                      MOVE    SUM-TEN-GK(IDX IDX1)
                                         TO   WRK-Z9-9 
                      MOVE    WRK-Z9-9   TO   M700-TEN (IDX IDX1) 
              END-PERFORM
      *
      *  金額
              MOVE  SUM-HKNFTN-GK(IDX1)   TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10             TO   M700-HKNFTN(IDX1)
              MOVE  WRK-S9-10             TO   M7001-HKNFTN(IDX1)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
              MOVE  SUM-JIHIFTN-GK(IDX1)  TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10             TO   M700-JIHIFTN(IDX1)
              MOVE  WRK-S9-10             TO   M7001-JIHIFTN(IDX1)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
              MOVE  SUM-SEIKYU-GK(IDX1)   TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10             TO   M700-GSEIKYU(IDX1)
              MOVE  WRK-S9-10             TO   M7001-GSEIKYU(IDX1)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
              MOVE  SUM-RYOSYU-GK(IDX1)   TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10             TO   M700-GRYOSYU(IDX1)
              MOVE  WRK-S9-10             TO   M7001-GRYOSYU(IDX1)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
      *
      *  受診者数
              IF WRK-NYUGAIKBN =   "2" 
                   MOVE  SUM-JYUSHINKEN-GK(IDX1) 
                         TO   WRK-Z9-5
                   MOVE  WRK-Z9-5
                         TO   M700-JYUSHINCNT(IDX1)
              END-IF
             END-PERFORM
      *
      *月計
      *  保険点数、件数
              MOVE    SUM-HKNTEN-GK(32)      TO  WRK-Z9-9
              MOVE    WRK-Z9-9               TO  M700-GHKNTEN(32)
              IF WRK-NYUGAIKBN =   "2" 
                MOVE    SUM-SHKNKEN-GK(32)   TO  WRK-Z9-5
                MOVE    WRK-Z9-5             TO  M700-SHKNKEN(32)
      ***  20050914  MOTO
                MOVE    SUM-OHKNKEN-GK(32)   TO  WRK-Z9-5
                MOVE    WRK-Z9-5             TO  M700-OHKNKEN(32)
      ***  20050914  MOTO
              END-IF
              MOVE    SUM-RHKNKEN-GK(32)     TO  WRK-Z9-5
              MOVE    WRK-Z9-5               TO  M700-RHKNKEN(32)
      *
      *  診療区分別点数
              PERFORM VARYING IDX FROM    1   BY  1
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *                UNTIL   IDX   >    11
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      *                UNTIL   IDX   >    15
                      UNTIL   IDX   >    16
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
                      MOVE    SUM-TEN-GK(IDX 32)
                                         TO   WRK-Z9-9 
                      MOVE    WRK-Z9-9   TO   M700-TEN (IDX 32) 
              END-PERFORM
      *
      *  金額
              MOVE  SUM-HKNFTN-GK(32)     TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10             TO   M700-HKNFTN(32)
              MOVE  WRK-S9-10             TO   M7001-HKNFTN(32)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
              MOVE  SUM-JIHIFTN-GK(32)    TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10             TO   M700-JIHIFTN(32)
              MOVE  WRK-S9-10             TO   M7001-JIHIFTN(32)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
              MOVE  SUM-SEIKYU-GK(32)     TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10             TO   M700-GSEIKYU(32)
              MOVE  WRK-S9-10             TO   M7001-GSEIKYU(32)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
              MOVE  SUM-RYOSYU-GK(32)     TO   WRK-S9-10
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) start
      *        MOVE  WRK-S9-10             TO   M700-GRYOSYU(32)
              MOVE  WRK-S9-10             TO   M7001-GRYOSYU(32)
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
      *
      *  受診者数
              IF WRK-NYUGAIKBN =   "2" 
                   MOVE  SUM-JYUSHINKEN-GK(32) 
                         TO   WRK-Z9-5
                   MOVE  WRK-Z9-5
                         TO   M700-JYUSHINCNT(32)
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
      ***  20040914 MOTOHASI MONPEに変更
      ***     MOVE   "A00000M700"         TO  SPRT-PRTID
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      *     MOVE   "A00000M700.red"         TO  SPRT-PRTID
           MOVE    WRK-FORMID          TO  SPRT-PRTID
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
      ***  20040914 MOTOHASI MONPEに変更
           MOVE    "月計表（診療費請求明細）"
                                       TO  SPRT-TITLE
           MOVE    M700                TO  SPRT-PRTDATA
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
      *=== 基本診療項目追加対応 By Takahima(2006/11/06) start
      *=== H20年4月改正対応 by k.takashima (2008/03/25) start
      *         MOVE   "A00000M7001.red"    TO  SPRT-PRTID
               MOVE    WRK-FORMID1         TO  SPRT-PRTID
      *=== H20年4月改正対応 by k.takashima (2008/03/25) end
               MOVE    "月計表（診療費請求明細）"
                                           TO  SPRT-TITLE
               MOVE    M7001               TO  SPRT-PRTDATA
               MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
               MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
               MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
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
      *=== 基本診療項目追加対応 By Takashima(2006/11/06) end
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
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                STS-AREA-DAY
           INITIALIZE                LNK-DAY7-AREA
           MOVE    "71"          TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                             LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO  WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細情報　入返金発行日で検索
      *****************************************************************
       910-DBSELECT-SEC                 SECTION.
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *
      ***  2005/04/15  MOTO  日レセ Ver2.5.0 KEY変更
      ***     MOVE    "SYUMEI-KEY4"       TO  ORC-DBPATH
      ***     CALL    "ORCMCPSUB"          USING
      ***                                  MCPAREA
      ***                                  ORCMCPAREA
      ***                                  MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-ST-KEY3    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE   "tbl_syumei"                TO  MCP-TABLE
           MOVE   "st_key3"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      ***  2005/04/15  MOTO  日レセ Ver2.5.0 KEY変更
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
      *     収納明細情報を検索処理
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"         TO  MCP-FUNC
      ***  2005/04/15  MOTO  日レセ Ver2.5.0 KEY変更
      ***     MOVE    "SYUMEI-KEY4"     TO  ORC-DBPATH
      ***     CALL    "ORCMCPSUB"          USING
      ***                                  MCPAREA
      ***                                  ORCMCPAREA
      ***                                  MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-ST-KEY3    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE   "tbl_syumei"                TO  MCP-TABLE
           MOVE   "st_key3"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      ***  2005/04/15  MOTO  日レセ Ver2.5.0 KEY変更
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
      ***  2005/04/15  MOTO  日レセ Ver2.5.0 KEY変更
      ***     MOVE    "SYUMEI-KEY4"    TO  ORC-DBPATH
      ***     CALL    "ORCMCPSUB"          USING
      ***                                  MCPAREA
      ***                                  ORCMCPAREA
      ***                                  MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-ST-KEY3    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE   "tbl_syumei"                TO  MCP-TABLE
           MOVE   "st_key3"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      ***  2005/04/15  MOTO  日レセ Ver2.5.0 KEY変更
           .
       910-DBCLOSE-EXT.
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
      ****  20041006  Ver 2.4.0対応による削除  MOTO
      ****     MOVE    "1"              TO   JYURRK-LASTKBN
      **
      *     MOVE    JYURRK-REC       TO  MCPDATA-REC
      *     MOVE    "DBSELECT"          TO  MCP-FUNC
      **=== 4.0対応 By Takashima(2007/09/10) start
      **     MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      **     CALL    "MCPSUB"             USING
      *     MOVE   "tbl_jyurrk"             TO  MCP-TABLE
      *     MOVE   "key6"                   TO  MCP-PATHNAME
      *     CALL   "ORCDBMAIN"           USING
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
      *         MOVE   "tbl_jyurrk"             TO  MCP-TABLE
      *         MOVE   "key6"                   TO  MCP-PATHNAME
      *         CALL   "ORCDBMAIN"           USING
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
      *     MOVE   "tbl_jyurrk"             TO  MCP-TABLE
      *     MOVE   "key6"                   TO  MCP-PATHNAME
      *     CALL   "ORCDBMAIN"           USING
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
      **
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
           MOVE    "key6"              TO   MCP-PATHNAME
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
           MOVE    "key6"             TO   MCP-PATHNAME
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
           INITIALIZE                   SYUNOU-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    SMEI-HOSPID      TO  SYU-HOSPID  
           MOVE    SMEI-HOSPNUM      TO  SYU-HOSPNUM  
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    SMEI-NYUGAIKBN   TO  SYU-NYUGAIKBN  
           MOVE    SMEI-PTID        TO  SYU-PTID  
           MOVE    SMEI-DENPNUM     TO  SYU-DENPNUM  
      *
           MOVE    SYUNOU-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"         TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE   "tbl_syunou"            TO  MCP-TABLE
           MOVE   "key"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
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
               MOVE   "tbl_syunou"            TO  MCP-TABLE
               MOVE   "key"                   TO  MCP-PATHNAME
               CALL   "ORCDBMAIN"           USING
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
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE   "tbl_syunou"            TO  MCP-TABLE
           MOVE   "key"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
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
           MOVE    "DBSELECT"         TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE   "tbl_syunou"            TO  MCP-TABLE
           MOVE   "key"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
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
               MOVE   "tbl_syunou"            TO  MCP-TABLE
               MOVE   "key"                   TO  MCP-PATHNAME
               CALL   "ORCDBMAIN"           USING
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
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE   "tbl_syunou"            TO  MCP-TABLE
           MOVE   "key"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
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
           MOVE    WRK-PARA-PRTLYMD    TO  HKN-TEKSTYMD
           MOVE    ALL "9"             TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *     CALL    "MCPSUB"           USING
           MOVE   "tbl_hknnum"             TO  MCP-TABLE
           MOVE   "key5"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"         USING
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
               MOVE   "tbl_hknnum"             TO  MCP-TABLE
               MOVE   "key5"                   TO  MCP-PATHNAME
               CALL   "ORCDBMAIN"         USING
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
           MOVE   "tbl_hknnum"             TO  MCP-TABLE
           MOVE   "key5"                   TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"              USING
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
      *     CALL    "MCPSUB"            USING
           MOVE   "tbl_syskanri"              TO  MCP-TABLE
           MOVE   "key10"                     TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"          USING
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
               MOVE   "tbl_syskanri"              TO  MCP-TABLE
               MOVE   "key10"                     TO  MCP-PATHNAME
               CALL   "ORCDBMAIN"           USING
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
           MOVE   "tbl_syskanri"              TO  MCP-TABLE
           MOVE   "key10"                     TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
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
           MOVE   "tbl_syskanri"              TO  MCP-TABLE
           MOVE   "key10"                     TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
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
               MOVE   "tbl_syskanri"              TO  MCP-TABLE
               MOVE   "key10"                     TO  MCP-PATHNAME
               CALL   "ORCDBMAIN"           USING
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
           MOVE   "tbl_syskanri"              TO  MCP-TABLE
           MOVE   "key10"                     TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
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
           MOVE   "tbl_syskanri"              TO  MCP-TABLE
           MOVE   "key10"                     TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF  MCP-RC                   =   ZERO
               MOVE    "DBFETCH"        TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_syskanri"              TO  MCP-TABLE
               MOVE    "key10"                     TO  MCP-PATHNAME
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
           MOVE    "tbl_syskanri"              TO  MCP-TABLE
           MOVE    "key10"                     TO  MCP-PATHNAME
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
               CALL    "ORCDBMAIN"         USING
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
