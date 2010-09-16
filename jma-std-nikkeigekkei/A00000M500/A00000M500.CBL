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
       PROGRAM-ID.             A00000M500.
      *****************************************************************
      *    システム名        : ＯＲＣＡ
      *    サブシステム名    : 月次帳票
      *    コンポーネント名  : 月計表
      *    管理者            : 
      *    作成日付    作業者        記述
      *    03/09/29    @@@@-有村     新規作成(ORCBM500のID変更Ver)
      *    03/10/21    @@@@-半田     自費合計行の追加
      *    03/10/27    @@@@-半田     保険適用外金額行の追加
      *    03/12/11    @@@@-半田     帳票タイトルの変更対応
      *    04/02/02    @@@@-半田     医療収益の算出方法の変更対応
      *    04/09/15    @@@@-本橋     MONPE化
      *    05/02/15    @@@@-鈴木     帳票パラメタに入外区分を追加
      *                              帳票パラメタに診療科を追加
      *    05/04/07    @@@@-本橋     日レセVer2.5.0 収納テーブル取得KEY追加
      *                              への対応
      *    05/07/14    @@@@-本橋     入退院件数が正しく保険ごとに集計されて
      *                              いない不具合修正
      *    05/09/14    @@@@-本橋     有効期限が切られた診療科が集計されてこない
      *                              不具合を修正
      *                              その他件数の欄を追加
      *                              ORCMCPSUB → MCPSUBに変更
      *    06/08/01    @@@@-高嶋     sarge対応
      *    06/10/30    @@@@-高嶋     自費、基本診療項目追加対応
      *    06/12/20    @@@@-高嶋     自賠責計算式変更対応
      *    06/12/22    @@@@-内田     特別療養費（068,069）を国保で扱う
      *    07/03/20    @@@@-内田     消費税内税の影響対応
      *    07/08/21    @@@@-田中     生活療養費、生活負担額の追加
      *    07/08/22    @@@@-大竹     自費項目修正
      *    07/09/10    @@@@-高嶋     4.0対応
      *    07/12/18    @@@@-吉川     在宅等が再診で集計される不具合を修正
      *    08/01/31    @@@@-吉川     初診、再診、その他振分け修正
      *    08/03/26    @@@@-田中     H20年4月改正対応
      *    08/04/18    @@@@-田中     医療収益に生活療養費及び負担額が加算されない不具合を修正
      *    08/06/03    @@@@-田中     医療収益の値が正しく印字されない不具合を修正
      *    08/06/12    @@@@-田中     健保準拠時の自賠責の計算を修正
      *    08/08/13    @@@@-田中     自費名称が未登録の時も値を印字する修正
      *    08/12/17    @@@@-田中     医療収益に減免金額を集計するように修正
      *    09/02/06    @@@@-田中     減免金額の計算式を修正
      *    09/02/17    @@@@-田中     減免計算式の条件式を修正
      *    09/04/08    @@@@-門間     医療収益の計算式を修正
      *    09/05/26    @@@@-門間     患者負担分が2重に加算されていた不具合の修正
      *                              生活療養費・生活負担額が保険分金額・患者負担金額・
      *                              医療収益に加算されていない不具合を修正
      *    09/12/08    @@@@-田中     医療収益の端数処理を修正
      *    10/02/09    @@@@-田中     治験対応
      *    10/03/09    @@@@-田中     初診判定条件追加
      *    10/04/27    @@@@-田中     再診の判定条件を修正
      *    10/07/06    @@@@-門間     労災・自賠責保険時の初診の判定条件追加
      *    10/07/23    @@@@-門間     医療保険の初診判定条件を追加
      *    10/09/09    @@@@-田中     外来時に入院料を集計するよう修正
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
      ***** 03/09/29 INS
      *     COPY    "ORCBM500.INC".
           COPY    "A00000M500.INC".
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
           COPY    "A00000M5001.INC".
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
      *****
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-GET                PIC 9(01).
           03  FLG-END                PIC 9(01).
           03  FLG-SYSKANRI           PIC 9(01).
           03  FLG-SYSKANRI2          PIC 9(01).
           03  FLG-SYSKANRI3          PIC 9(01).
           03  FLG-INPUT              PIC 9(01).
           03  FLG-HKNCOMBI           PIC 9(01).
           03  FLG-PTINF              PIC 9(01).
           03  FLG-PTHKNINF           PIC 9(01).
           03  FLG-SYUMEI             PIC 9(01).
           03  FLG-SYUNOU             PIC 9(01).
           03  FLG-SRYACCT            PIC 9(01).
           03  FLG-JYURRK             PIC 9(01).
           03  FLG-HKNNUM             PIC 9(01).
           03  FLG-PTNYUINRRK         PIC 9(01).
      ** 2005/02/15 追加 SUZUKI
           03  FLG-SRYKA-ALL          PIC 9(01).
           03  FLG-SRYKAGK            PIC 9(01).
      *--- 2010/04/27 ADD START ----------------------------------------
           03  FLG-SRYACT             PIC 9(01).
      *--- 2010/04/27 ADD END ------------------------------------------
      *--- 2010/04/27 ADD START ----------------------------------------
           03  FLG-JYURRK2            PIC 9(01).
      *--- 2010/04/27 ADD END ------------------------------------------
      ** 2005/02/15 ここまで
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE               PIC 9(02).
           03  CNT-PAGE               PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY             PIC 9(02).
               05  SYS-MM             PIC 9(02).
               05  SYS-DD             PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                   PIC 9(04).
           03  IDX2                   PIC 9(04).
           03  IDX3                   PIC 9(04).
           03  IDX4                   PIC 9(04).
           03  IDX5                   PIC 9(04).
           03  IDX6                   PIC 9(04).
           03  IDX7G.
              05  IDX7                PIC 9(02).
           03  IDX8                   PIC 9(04).
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
           03  IDX9                   PIC 9(04).
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
           03  IDX                    PIC 9(04).
           03  IDY                    PIC 9(04).
           03  IDZ                    PIC 9(04).
           03  IDX-S                  PIC 9(07).
           03  IDX-N                  PIC 9(07).
           03  IDX-MAX                PIC 9(07).
      *--- 2010/04/27 ADD START ----------------------------------------
           03  IDX10                  PIC 9(02).
           03  IDX11                  PIC 9(02).
      *--- 2010/04/27 ADD END ------------------------------------------
      *
      **** 20050228 open-cobol 0.23への対応 SUZUKI
      *     03  IDX2              PIC 9(04).
      **** 20050228
           03  JIHIIDX                PIC 9(04).
           03  NG                     PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID         PIC 9(07).
           03  WRK-PARA-SHELLID       PIC X(08).
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     03  WRK-PARA-HOSPID   PIC X(24).
           03  WRK-PARA-HOSPNUM       PIC 9(02).
      *=== 4.0対応 By Takashima(2007/09/10) end
           03  WRK-PARA-PRTYM         PIC X(06).
           03  WRK-PARA-PRTSYMD       PIC X(08).
           03  WRK-PARA-PRTLYMD       PIC X(08).
      ** 2005/02/15 追加 SUZUKI
           03  WRK-PARA-NYUGAIKBN     PIC X(01).
           03  WRK-PARA-SRYKA         PIC X(02).
      *  2005/02/15 ここまで SUZUKI
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH               PIC X(64).
      *
           03  WRK-RECEERR            PIC X(200).
      *
           03  WRK-PARA-DENPPRTYMDWH  PIC X(22).
           03  WRK-SYSYMDWH           PIC X(22).
           03  WRK-RENNUM             PIC 9(03).
           03  WRK-PTNUM              PIC X(20).
           03  WRK-PTNUM6             PIC 9(06). 
           03  WRK-SYUHKNNUM          PIC X(03). 
      *  
           03  WRK-SYSYMD.
               05  WRK-SYSYY          PIC 9(04).
               05  WRK-SYSMM          PIC 9(02).
               05  WRK-SYSDD          PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY            PIC 9(04).
               05  WRK-SMM            PIC 9(02).
               05  WRK-SDD            PIC 9(02).
           03  WRK-HENYMDG            PIC X(22).
      *
      *****03  WRK-IN                 PIC X(01).
      *
           03  WRK-MONEY              PIC 9(08).
           03  WRK-JIHI               PIC 9(08).
      *--- 2008/12/17 ADD START ----------------------------------------
           03  WRK-SYUEKI             PIC S9(08).
      *--- 2008/12/17 ADD END ------------------------------------------
      *
           03  WRK-JIHI2              PIC 9(08).
      * 20080326 H20年4月改正対応START tanaka
           03  WRK-JIHI4              PIC 9(08).
      * 20080326 H20年4月改正対応END
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *     03  WRK-TJIHI-G       OCCURS  5.
           03  WRK-TJIHI-G       OCCURS  10.
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
               05  WRK-TJIHI          PIC X(08).
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         05  WRK-TJIHI-FLG    PIC 9(01).
      * 20080813 自費名称が未登録の時も値を印字する修正END
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *     03  WRK-TJIHIN-G       OCCURS  5.
           03  WRK-TJIHIN-G       OCCURS  10.
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
               05  WRK-TJIHIN         PIC X(08).
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         05  WRK-TJIHIN-FLG    PIC 9(01).
      * 20080813 自費名称が未登録の時も値を印字する修正END
      *
           03  WRK-4001-TENTANKA      PIC 9(02)V9(01).
      *
           03  WRK-RENNUM-X           PIC ZZ9.
           03  WRK-PAGE               PIC ZZ9.
           03  WRK-Z9                 PIC ZZZZZZZZZZZ9.
           03  WRK-ZZ                 PIC ZZZZZZZZZZZZ.
           03  WRK-S9                 PIC -----------9.
           03  WRK-SZ                 PIC -----------Z.
           03  WRK-NYUGAIKBN          PIC X(01).
           03  WRK-SRYKA              PIC X(02).
           03  DEBUG-CNT              PIC 9(03).
      *
      * 2003/06/24  追加
           03  WRK-SYS-BEDSU          PIC 9(04).
           03  WRK-SYS-BEDSUIPN       PIC 9(04).
      ** 2005/02/15 追加 SUZUKI
           03  WRK-SYS-NYUGAIKBN      PIC 9(04).
      *--- 2009/02/06 ADD START ----------------------------------------
           03  WRK-COMPFTN            PIC S9(10).
      *--- 2009/02/06 ADD END ------------------------------------------
      *--- 2010/04/27 ADD START ----------------------------------------
           03  WRK-DENPNUM            PIC 9(07).
      *--- 2010/04/27 ADD END ------------------------------------------
      ** 2005/02/15 ここまで 
      * 2003/06/24  追加
      *
      *** 20030410 変更 入外別集計の為に変更
      *
      *** 01  SUM-AREA                VALUE   ZERO.
      ***     03  SRYKAMAX                    PIC 9(03).
      ***     03  SRYKA-TBL              OCCURS  100.    
      ***         05  SRYKA                   PIC   X(02).
      ***         05  SRYKANAME               PIC   X(20).
      ***         05  HKNTYPE-TBL     OCCURS  10.
      ***            07  ITEM-TBL          OCCURS  30.
      ***               09  YOSO              PIC S9(11).
      *
       01  SUM-AREA                VALUE   ZERO.
           03  SRYKAMAX               PIC   9(03).
           03  NYUGAI-TBL          OCCURS  2.    
             05  NYUGAIKBN            PIC   X(01).
             05  NYUGAINAME           PIC   X(04).
             05  SRYKA-TBL            OCCURS  100.    
               07  SRYKA              PIC   X(02).
               07  SRYKANAME          PIC   X(20).
      * 20080326 H20年4月改正対応START tanaka
      *         07  HKNTYPE-TBL            OCCURS  10.
               07  HKNTYPE-TBL     OCCURS  11.
      * 20080326 H20年4月改正対応END
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *           09  ITEM-TBL               OCCURS  30.
      * 20080326 H20年4月改正対応START tanaka
      *           09  ITEM-TBL               OCCURS  39.
                 09  ITEM-TBL      OCCURS  40.
      * 20080326 H20年4月改正対応END
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
                   11  YOSO           PIC S9(11).
      *
      *** 20030410 変更 入外別集計の為に変更
      *
       01   WRK-JYURRK-AREA.
           03  WRK-JR-DENPNUM-SV      PIC  9(07).
           03  WRK-JR-TBL           OCCURS  600000. 
             05  WRK-JR-SORT-KEY.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *          07  WRK-JR-HOSPID         PIC  X(24).
                07  WRK-JR-HOSPNUM    PIC  9(02).
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
       01  WRK-CONS-AREA.
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973". 
      * 20080326 H20年4月改正対応START tanaka
      *    公害保険番号 
           03  WRK-CONS-KOGAI-HKNNUM   PIC X(03)   VALUE   "975". 
      * 20080326 H20年4月改正対応END
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
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *
      *    自費管理情報
           COPY    "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    労災情報取得
           COPY    "CPSK4001.INC".
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
      *
      *--- 2010/04/27 ADD START ----------------------------------------
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *--- 2010/04/27 ADD END ------------------------------------------
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
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
           PERFORM   100-INIT-SEC
      *
           PERFORM   200-MAIN-SEC
                     UNTIL   FLG-END  =  1
      *
           PERFORM   300-END-SEC
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
           INITIALIZE                  WRK-JYURRK-AREA
      *=== 4.0対応 By Takashima(2007/09/10) start
           INITIALIZE                  SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      
      *****************************************************
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
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-SRYKA
      *=== 4.0対応 By Takashima(2007/09/10) start
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO   SPA-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *    ステップ管理開始処理
           MOVE    "STS"              TO   SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM        TO   JOB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-JOBID     TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID   TO   JOB-SHELLID
      *     MOVE    "ORCBD001"      TO  JOB-PGID
      ***** 03/09/29
      *     MOVE    "ORCBM500"      TO  JOB-PGID
           MOVE    "A00000M500"       TO   JOB-PGID
      *****
      * 20031211 帳票タイトル変更 
      ***     MOVE    "月計表"        TO  JOB-SHELLMSG
           MOVE    "保険別診療点数 月計表"  TO  JOB-SHELLMSG
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
           PERFORM   31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO   WRK-SYSYMDWH
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-PRTYM        TO   WRK-SYMD(1:6)
           PERFORM   31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD              TO   WRK-PARA-PRTLYMD
      *
      ***  20050914  MOTO
      *    対象年月初日
           MOVE    WRK-PARA-PRTYM        TO   WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"                  TO   WRK-PARA-PRTSYMD(7:2)
      ***  20050914  MOTO
      *
      *    対象年月日編集
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-PRTLYMD      TO   WRK-SYMD
           PERFORM   31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO   WRK-PARA-DENPPRTYMDWH
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE                 TO   SYS-1001-REC
           INITIALIZE                    SYS-1001-REC
           MOVE    "1001"                TO   SYS-1001-KANRICD
           MOVE    "*"                   TO   SYS-1001-KBNCD
      ***  20050914  MOTO
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM           TO   SYS-1001-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTSYMD      TO   SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD      TO   SYS-1001-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1001-REC          TO   MCPDATA-REC
           PERFORM   800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI   =   ZERO
             MOVE    MCPDATA-REC         TO   SYS-1001-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    SYS-1001-HOSPID     TO  WRK-PARA-HOSPID
             MOVE    SYS-1001-HOSPNUM    TO   WRK-PARA-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
      * 2003/06/24  追加
             MOVE    SYS-1001-BEDSU      TO   WRK-SYS-BEDSU
             MOVE    SYS-1001-BEDSUIPN   TO   WRK-SYS-BEDSUIPN
      * 2003/06/24  追加
           ELSE
             MOVE    "医療機関ＩＤが取得できませんでした。"
                                         TO   WRK-RECEERR
             PERFORM   500-ERR-HENSYU-SEC
           END-IF
      *
      ** 2005/02/15 追加 SUZUKI
      *    診療科情報
           IF   WRK-PARA-SRYKA  =  "00"
               OR    WRK-PARA-SRYKA  =  SPACE
             IF   WRK-PARA-SRYKA  =  "00"
               MOVE    1     TO   FLG-SRYKAGK
             END-IF
             IF   WRK-PARA-SRYKA  =  SPACE
               MOVE    1     TO   FLG-SRYKA-ALL
             END-IF
           ELSE
	         IF   WRK-PARA-SRYKA(2:1) =   SPACE
               MOVE    "診療科コードが不正です。"   TO   WRK-RECEERR
               PERFORM   500-ERR-HENSYU-SEC
             ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    "1005"              TO   SYS-1005-KANRICD
               MOVE    WRK-PARA-SRYKA      TO   SYS-1005-KBNCD
      ***  20050914  MOTO
      *=== 4.0対応 By Takashima(2007/09/10) start
               MOVE    SPA-HOSPNUM         TO   SYS-1005-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
               MOVE    WRK-PARA-PRTSYMD    TO   SYS-1005-EDYUKYMD
               MOVE    WRK-PARA-PRTLYMD    TO   SYS-1005-STYUKYMD
      ***  20050914  MOTO
               MOVE    SYS-1005-REC        TO   MCPDATA-REC
               PERFORM   800-SYSKANRI-READ-SEC
               IF   FLG-SYSKANRI   =   ZERO
                 MOVE    MCPDATA-REC       TO   SYS-1005-REC
               ELSE
      ***       診療科コード入力値エラーの場合
                 MOVE    "指定された診療科コードの値はありません。"
                                                     TO   WRK-RECEERR
                 PERFORM   500-ERR-HENSYU-SEC
               END-IF
             END-IF
           END-IF
      ** 2005/02/15 ここまで
      *
      *    入院自費情報取得
      *    区分11
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "11"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =  ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(1)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHIN-FLG(1)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE  SPACE               TO   WRK-TJIHIN(1)
           END-IF
      *  
      *    区分12
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "12"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(2)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHIN-FLG(2)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(2)
           END-IF
      *  
      *    区分13
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "13"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(3)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHIN-FLG(3)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(3)
           END-IF
      *  
      *    区分14
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "14"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(4)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHIN-FLG(4)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(4)
           END-IF
      *
      *    区分15
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "15"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(5)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHIN-FLG(5)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(5)
           END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *
      *    区分16
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "16"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(6)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHIN-FLG(6)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(6)
           END-IF
      *
      *    区分17
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
      *===自費項目修正 Otake 2007/08/22 Start
      *     MOVE    "16"                TO  SYS-1013-KBNCD
           MOVE    "17"                TO   SYS-1013-KBNCD
      *===自費項目修正 Otake 2007/08/22 End
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF    FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(7)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHIN-FLG(7)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(7)
           END-IF
      *
      *    区分18
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "18"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE  MCPDATA-REC         TO   SYS-1013-REC
             MOVE  SYS-1013-JIHINAME   TO   WRK-TJIHIN(8)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHIN-FLG(8)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE  SPACE               TO   WRK-TJIHIN(8)
           END-IF
      *
      *    区分19
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "19"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE  MCPDATA-REC         TO   SYS-1013-REC
             MOVE  SYS-1013-JIHINAME   TO   WRK-TJIHIN(9)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHIN-FLG(9)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE  SPACE               TO   WRK-TJIHIN(9)
           END-IF
      *
      *    区分10
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "10"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(10)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHIN-FLG(10)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(10)
           END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
      *
      *    外来自費情報取得
      *    区分21
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "21"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(1)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHI-FLG(1)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE  SPACE               TO   WRK-TJIHI(1)
           END-IF
      *  
      *    区分22
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "22"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(2)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHI-FLG(2)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(2)
           END-IF
      *  
      *    区分23
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "23"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(3)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHI-FLG(3)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(3)
           END-IF
      *  
      *    区分24
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "24"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(4)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHI-FLG(4)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(4)
           END-IF
      *
      *    区分25
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "25"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE  MCPDATA-REC         TO   SYS-1013-REC
             MOVE  SYS-1013-JIHINAME   TO   WRK-TJIHI(5)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHI-FLG(5)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE  SPACE               TO   WRK-TJIHI(5)
           END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *
      *    区分26
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "26"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE  WRK-PARA-PRTSYMD      TO   SYS-1013-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD      TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE  MCPDATA-REC       TO   SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO   WRK-TJIHI(6)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHI-FLG(6)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
               MOVE  SPACE             TO   WRK-TJIHI(6)
           END-IF
      *
      *    区分27
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "27"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(7)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHI-FLG(7)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(7)
           END-IF
      *
      *    区分28
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "28"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(8)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHI-FLG(8)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(8)
           END-IF
      *
      *    区分29
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "29"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(9)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHI-FLG(9)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(9)
           END-IF
      *
      *    区分20
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "20"                TO   SYS-1013-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(10)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *         MOVE  1                 TO  WRK-TJIHI-FLG(10)
      * 20080813 自費名称が未登録の時も値を印字する修正END
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(10)
           END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
      *
      *    労災医療機関情報
           MOVE    SPACE                TO   SYS-4001-REC
           INITIALIZE                   SYS-4001-REC
           MOVE    "4001"               TO   SYS-4001-KANRICD
           MOVE    "*"                  TO   SYS-4001-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-4001-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD     TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-PRTSYMD     TO   SYS-4001-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD     TO   SYS-4001-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-4001-REC         TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE    MCPDATA-REC        TO   SYS-4001-REC
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
           MOVE    "1005"               TO   SYS-1005-KANRICD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO   SYS-1005-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD     TO   WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-PRTSYMD     TO   SYS-1005-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD     TO   SYS-1005-STYUKYMD
      ***  20050914  MOTO
      ***     MOVE    WRK-PARA-PRTLYMD  TO   SYS-1005-STYUKYMD
      ***     MOVE    WRK-PARA-PRTLYMD  TO   SYS-1005-EDYUKYMD
           PERFORM 820-SYSKANRI-DBSELECT-SEC
           MOVE    1                    TO   IDX1
           PERFORM  UNTIL  FLG-SYSKANRI3   =  1
             IF   FLG-SYSKANRI3 = ZERO
      ***  20030410 変更
      ***                MOVE SYS-1005-KBNCD      TO SRYKA(IDX1)  
      ***                MOVE SYS-1005-SRYKANAME1 TO SRYKANAME(IDX1)
               MOVE   SYS-1005-KBNCD      TO   SRYKA(1 IDX1)
               MOVE   SYS-1005-SRYKANAME1 TO   SRYKANAME(1 IDX1)
               MOVE   SYS-1005-KBNCD      TO   SRYKA(2 IDX1)
               MOVE   SYS-1005-SRYKANAME1 TO   SRYKANAME(2 IDX1)
      ***  20030410 変更
               ADD     1                  TO   IDX1
             END-IF
             PERFORM 820-SYSKANRI-DBFETCH-SEC
           END-PERFORM
      *    20030410 変更
      *      MOVE  "全科合計"   TO   SRYKANAME(IDX1)
           MOVE    "全科合計"             TO   SRYKANAME(1 IDX1)
           MOVE    "全科合計"             TO   SRYKANAME(2 IDX1)
      ***  20030410 変更
           MOVE    IDX1                   TO   SRYKAMAX
      *
      ***  20030410 追加
           MOVE    "1"                    TO   NYUGAIKBN(1)
           MOVE    "入院"                 TO   NYUGAINAME(1)
           MOVE    "2"                    TO   NYUGAIKBN(2)
           MOVE    "外来"                 TO   NYUGAINAME(2)
      ***  20030410 追加
      *
      *  2005/02/15 追加 SUZUKI
           IF   WRK-PARA-NYUGAIKBN  NOT =  "1"  AND  "2"
             MOVE    SPACE                TO   WRK-PARA-NYUGAIKBN
           END-IF
           MOVE   WRK-PARA-NYUGAIKBN      TO   WRK-SYS-NYUGAIKBN
      *  2005/02/15 追加 SUZUKI
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
      *    収納情報集計処理
           PERFORM   910-DBSELECT-SEC
           PERFORM   320-SYUNOU-CNT-SEC
      *    入院履歴情報集計処理
           PERFORM   910-DBSELECT2-SEC
           PERFORM   320-PTNYUINRRK-CNT-SEC
      *    帳票編集印刷処理
           PERFORM   330-TERM-HEN-SEC
           MOVE   1        TO   FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報集計処理
      *****************************************************************
       320-SYUNOU-CNT-SEC      SECTION.
      *
           PERFORM  UNTIL   FLG-SYUNOU  = 1
      *
      ***  2005/04/07  MOTO  日レセ Ver2.5.0 KEY変更による削除
      * 20030704 追加
      ***      INITIALIZE         PTINF-REC
      ***      MOVE SYU-HOSPID TO PTINF-HOSPID
      ***      MOVE SYU-PTID   TO PTINF-PTID
      ***      PERFORM 900-PTINF-READ-SEC
      * 20030704 追加
      ***  2005/04/07  MOTO  日レセ Ver2.5.0 KEY変更による削除
      *
      ***  20030704 収納情報抽出条件変更
      *   集計対象年月のデータであるかをチェックする。
             IF   SYU-SRYYMD(1:6)  =  WRK-PARA-PRTYM
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         AND  SYU-HOSPID              =  WRK-PARA-HOSPID
                 AND  SYU-HOSPNUM   =  WRK-PARA-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
                 AND  SYU-CREATEKBN   NOT =  "3"
                 AND  SYU-DENPJTIKBN  NOT =  "3"
      ***  2005/04/07  MOTO  日レセ Ver2.5.0 KEY変更による削除
      ***         AND  FLG-PTINF           =      ZERO
      ***         AND  PTINF-TSTPTNUMKBN   NOT =  "1"
      ***  2005/04/07  MOTO  日レセ Ver2.5.0 KEY変更による削除
      * 伝票作成区分 = 3(入院レセプト用ダミー)は対象外データとして取り扱う
      * 伝票状態区分 = 3(請求取消)は対象外データとして取り扱う
      * 患者情報にレコードが存在しないデータは抽出しない。
      * テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う
      ***  20030704 収納情報抽出条件変更
      *
      *** 20030410 追加 入外区分用インデックス
               EVALUATE SYU-NYUGAIKBN 
                 WHEN "1"
                   MOVE     1           TO   NG
                 WHEN "2"
                   MOVE     2           TO   NG
                 WHEN OTHER
                   MOVE     2           TO   NG
               END-EVALUATE
      *** 20030410 追加
      *
               MOVE   SYU-NYUGAIKBN     TO   WRK-NYUGAIKBN
               MOVE   SYU-SRYKA         TO   WRK-SRYKA
  	           PERFORM   320-SRYKA-GROUPING-SEC
      * 20080326 H20年4月改正対応START tanaka
	           IF  WRK-PARA-PRTYM  <  "200804"
                 PERFORM   320-HOKEN-GROUPING-SEC
               ELSE
                 PERFORM   330-HOKEN-GROUPING-SEC
               END-IF
      * 20080326 H20年4月改正対応END
               IF   IDX  >  ZERO
                 IF  FLG-GET  =  ZERO
                   MOVE   1   TO   FLG-GET
                 END-IF
                 PERFORM   320-SYUNOU-CNT-SUB-SEC
               END-IF
             END-IF
             PERFORM   910-DBFETCH-SEC
           END-PERFORM
      *
           .
       320-SYUNOU-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報集計サブ処理
      *****************************************************************
       320-SYUNOU-CNT-SUB-SEC      SECTION.
      *
      *  保険番号情報読み込み
           PERFORM   900-HKNNUM-READ-SEC
           IF   FLG-HKNNUM  =  1
      *    該当レコードが存在しない場合は単価を10円に設定する。
             MOVE 10     TO HKN-TENTNK
           END-IF
      *
      *    自費以外のとき自費分だけ集計
           IF   IDX   =   7
             MOVE    ZERO            TO    WRK-JIHI
           ELSE
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) start
             IF  SYU-SRYYMD  <=  "20070331"
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) end
               COMPUTE  WRK-JIHI   =   SYU-TOTAL-TGMONEY  
                                   +   SYU-TOTAL-TGMONEY-TAX
                                   +   SYU-JIHI-TOTAL
                                   +   SYU-JIHI-TOTAL-TAX
                                   +   SYU-JIHI-TAX
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) start
             ELSE
               COMPUTE  WRK-JIHI   =   SYU-TOTAL-TGMONEY
                                   +   SYU-TOTAL-TGMONEY-TAX
                                   +   SYU-JIHI-TOTAL
                                   +   SYU-JIHI-TOTAL-TAX
             END-IF
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) end
           END-IF
           COMPUTE  WRK-MONEY   =   SYU-SKYMONEY
                                -   WRK-JIHI
      *
           MOVE    SRYKAMAX          TO   IDX3
      *
           MOVE    ZERO              TO   SYU-TOTAL-HKNTEN
      *    診療科保険点数
           ADD    SYU-SSU-HKNTEN     TO   YOSO (NG IDX1 IDX 5)
           ADD    SYU-SSU-HKNTEN     TO   YOSO (NG IDX1 10 5)
           ADD    SYU-SSU-HKNTEN     TO   YOSO (NG IDX3 IDX 5)
           ADD    SYU-SSU-HKNTEN     TO   YOSO (NG IDX3 10 5)
      ** 20030228 合計点数に加算
           ADD    SYU-SSU-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    指導料保険点数
           ADD    SYU-SDO-HKNTEN     TO   YOSO (NG IDX1 IDX 6)
           ADD    SYU-SDO-HKNTEN     TO   YOSO (NG IDX1 10 6)
           ADD    SYU-SDO-HKNTEN     TO   YOSO (NG IDX3 IDX 6)
           ADD    SYU-SDO-HKNTEN     TO   YOSO (NG IDX3 10 6)
      ** 20030228 合計点数に加算
           ADD    SYU-SDO-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    在宅料保険点数
           ADD    SYU-ZTK-HKNTEN     TO   YOSO (NG IDX1 IDX 7)
           ADD    SYU-ZTK-HKNTEN     TO   YOSO (NG IDX1 10 7)
           ADD    SYU-ZTK-HKNTEN     TO   YOSO (NG IDX3 IDX 7)
           ADD    SYU-ZTK-HKNTEN     TO   YOSO (NG IDX3 10 7)
      ** 20030228 合計点数に加算
           ADD    SYU-ZTK-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    投薬料保険点数
           ADD    SYU-TYK-HKNTEN     TO   YOSO (NG IDX1 IDX 8)
           ADD    SYU-TYK-HKNTEN     TO   YOSO (NG IDX1 10 8)
           ADD    SYU-TYK-HKNTEN     TO   YOSO (NG IDX3 IDX 8)
           ADD    SYU-TYK-HKNTEN     TO   YOSO (NG IDX3 10 8)
      ** 20030228 合計点数に加算
           ADD    SYU-TYK-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    注射料保険点数
           ADD    SYU-CSY-HKNTEN     TO   YOSO (NG IDX1 IDX 9)
           ADD    SYU-CSY-HKNTEN     TO   YOSO (NG IDX1 10 9)
           ADD    SYU-CSY-HKNTEN     TO   YOSO (NG IDX3 IDX 9)
           ADD    SYU-CSY-HKNTEN     TO   YOSO (NG IDX3 10 9)
      ** 20030228 合計点数に加算
           ADD    SYU-CSY-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    処置料保険点数
           ADD    SYU-SYC-HKNTEN     TO   YOSO (NG IDX1 IDX 10)
           ADD    SYU-SYC-HKNTEN     TO   YOSO (NG IDX1 10 10)
           ADD    SYU-SYC-HKNTEN     TO   YOSO (NG IDX3 IDX 10)
           ADD    SYU-SYC-HKNTEN     TO   YOSO (NG IDX3 10 10)
      ** 20030228 合計点数に加算
           ADD    SYU-SYC-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    手術料保険点数
           ADD    SYU-SJT-HKNTEN     TO   YOSO (NG IDX1 IDX 11)
           ADD    SYU-SJT-HKNTEN     TO   YOSO (NG IDX1 10 11)
           ADD    SYU-SJT-HKNTEN     TO   YOSO (NG IDX3 IDX 11)
           ADD    SYU-SJT-HKNTEN     TO   YOSO (NG IDX3 10 11)
      ** 20030228 合計点数に加算
           ADD    SYU-SJT-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      **
      **    検査料保険点数
      *             ADD    SYU-KNS-HKNTEN   TO  YOSO (NG IDX1 IDX 12)
      *             ADD    SYU-KNS-HKNTEN   TO  YOSO (NG IDX1 10 12)
      *             ADD    SYU-KNS-HKNTEN   TO  YOSO (NG IDX3 IDX 12)
      *             ADD    SYU-KNS-HKNTEN   TO  YOSO (NG IDX3 10 12)
      *** 20030228 合計点数に加算
      *             ADD    SYU-KNS-HKNTEN   TO  SYU-TOTAL-HKNTEN
      **
      **  X線料保険点数            
      *             ADD    SYU-GZU-HKNTEN   TO  YOSO (NG IDX1 IDX 13)
      *             ADD    SYU-GZU-HKNTEN   TO  YOSO (NG IDX1 10 13)
      *             ADD    SYU-GZU-HKNTEN   TO  YOSO (NG IDX3 IDX 13)
      *             ADD    SYU-GZU-HKNTEN   TO  YOSO (NG IDX3 10 13)
      *** 20030228 合計点数に加算
      *             ADD    SYU-GZU-HKNTEN   TO  SYU-TOTAL-HKNTEN
      **
      **  その他保険点数            
      *             ADD    SYU-ETC-HKNTEN   TO  YOSO (NG IDX1 IDX 14)
      *             ADD    SYU-ETC-HKNTEN   TO  YOSO (NG IDX1 10 14)
      *             ADD    SYU-ETC-HKNTEN   TO  YOSO (NG IDX3 IDX 14)
      *             ADD    SYU-ETC-HKNTEN   TO  YOSO (NG IDX3 10 14)
      *** 20030228 合計点数に加算
      *             ADD    SYU-ETC-HKNTEN   TO  SYU-TOTAL-HKNTEN
      **
      **** 20030410 追加 入院の場合は入院料を集計する。
      *      IF NG   =  1
      **  入院料保険点数            
      *             ADD    SYU-NYN-HKNTEN   TO  YOSO (NG IDX1 IDX 15)
      *             ADD    SYU-NYN-HKNTEN   TO  YOSO (NG IDX1 10 15)
      *             ADD    SYU-NYN-HKNTEN   TO  YOSO (NG IDX3 IDX 15)
      *             ADD    SYU-NYN-HKNTEN   TO  YOSO (NG IDX3 10 15)
      **           合計点数に加算
      *             ADD    SYU-NYN-HKNTEN   TO  SYU-TOTAL-HKNTEN
      *      END-IF  
      **
      **** 20030410 追加 入院の場合は入院料を集計する。
      **
      **  合計保険点数
      *             ADD    SYU-TOTAL-HKNTEN TO  YOSO (NG IDX1 IDX 16)
      *             ADD    SYU-TOTAL-HKNTEN TO  YOSO (NG IDX1 10 16)
      *             ADD    SYU-TOTAL-HKNTEN TO  YOSO (NG IDX3 IDX 16)
      *             ADD    SYU-TOTAL-HKNTEN TO  YOSO (NG IDX3 10 16)
      **
      ** 20031027 追加
      **  保険適用外金額
      *             ADD    SYU-TOTAL-TGMONEY     
      *                                     TO  YOSO (NG IDX1 IDX 17)
      *             ADD    SYU-TOTAL-TGMONEY
      *                                     TO  YOSO (NG IDX1 10  17)
      *             ADD    SYU-TOTAL-TGMONEY-TAX     
      *                                     TO  YOSO (NG IDX1 IDX 17)
      *             ADD    SYU-TOTAL-TGMONEY-TAX
      *                                     TO  YOSO (NG IDX1 10  17)
      *             ADD    SYU-TOTAL-TGMONEY     
      *                                     TO  YOSO (NG IDX3 IDX 17)
      *             ADD    SYU-TOTAL-TGMONEY
      *                                     TO  YOSO (NG IDX3 10  17)
      *             ADD    SYU-TOTAL-TGMONEY-TAX     
      *                                     TO  YOSO (NG IDX3 IDX 17)
      *             ADD    SYU-TOTAL-TGMONEY-TAX
      *                                     TO  YOSO (NG IDX3 10  17)
      **
      ** 20031027 追加
      **
      **  負担金の集計            
      *             IF NG   =   1
      ****        入院の場合
      ****          食事療養費
      *               COMPUTE YOSO (NG IDX1 IDX 18) =
      *               YOSO (NG IDX1 IDX 18) +
      *               SYU-RYOYOHI-SKJ + SYU-RYOYOHI-SKJ-JIHI
      *               COMPUTE YOSO (NG IDX1 10 18) =
      *               YOSO (NG IDX1 10 18) +
      *               SYU-RYOYOHI-SKJ + SYU-RYOYOHI-SKJ-JIHI
      *               COMPUTE YOSO (NG IDX3 IDX 18) =
      *               YOSO (NG IDX3 IDX 18) +
      *               SYU-RYOYOHI-SKJ + SYU-RYOYOHI-SKJ-JIHI
      *               COMPUTE YOSO (NG IDX3 10 18) =
      *               YOSO (NG IDX3 10 18) +
      *               SYU-RYOYOHI-SKJ + SYU-RYOYOHI-SKJ-JIHI
      ****          食事負担額
      *               COMPUTE YOSO (NG IDX1 IDX 19) =
      *               YOSO (NG IDX1 IDX 19) +
      *               SYU-SKYMONEY-SKJ-KEI + SYU-SKYMONEY-SKJ-JIHI-KEI
      *               COMPUTE YOSO (NG IDX1 10 19) =
      *               YOSO (NG IDX1 10 19) +
      *               SYU-SKYMONEY-SKJ-KEI + SYU-SKYMONEY-SKJ-JIHI-KEI
      *               COMPUTE YOSO (NG IDX3 IDX 19) =
      *               YOSO (NG IDX3 IDX 19) +
      *               SYU-SKYMONEY-SKJ-KEI + SYU-SKYMONEY-SKJ-JIHI-KEI
      *               COMPUTE YOSO (NG IDX3 10 19) =
      *               YOSO (NG IDX3 10 19) +
      *               SYU-SKYMONEY-SKJ-KEI + SYU-SKYMONEY-SKJ-JIHI-KEI
      ****          室料差額
      *               ADD SYU-RMSAGAKU     TO  YOSO (NG IDX1 IDX 20)
      *               ADD SYU-RMSAGAKU     TO  YOSO (NG IDX1 10 20)
      *               ADD SYU-RMSAGAKU     TO  YOSO (NG IDX3 IDX 20)
      *               ADD SYU-RMSAGAKU     TO  YOSO (NG IDX3 10 20)
      *             ELSE             
      ****        外来の場合
      ****          薬剤一部負担金
      *               ADD    SYU-YKZ-FTN   TO  YOSO (NG IDX1 IDX 20)
      *               ADD    SYU-YKZ-FTN   TO  YOSO (NG IDX1 10 20)
      *               ADD    SYU-YKZ-FTN   TO  YOSO (NG IDX3 IDX 20)
      *               ADD    SYU-YKZ-FTN   TO  YOSO (NG IDX3 10 20)
      *             END-IF
      **  自費1            
      *             ADD    SYU-JIHI-1       TO  YOSO (NG IDX1 IDX 21)
      *             ADD    SYU-JIHI-1       TO  YOSO (NG IDX1 10 21)
      *             ADD    SYU-JIHI-1-TAX   TO  YOSO (NG IDX1 IDX 21)
      *             ADD    SYU-JIHI-1-TAX   TO  YOSO (NG IDX1 10 21)
      *             ADD    SYU-JIHI-1       TO  YOSO (NG IDX3 IDX 21)
      *             ADD    SYU-JIHI-1       TO  YOSO (NG IDX3 10 21)
      *             ADD    SYU-JIHI-1-TAX   TO  YOSO (NG IDX3 IDX 21)
      *             ADD    SYU-JIHI-1-TAX   TO  YOSO (NG IDX3 10 21)
      **
      **  自費2            
      *             ADD    SYU-JIHI-2       TO  YOSO (NG IDX1 IDX 22)
      *             ADD    SYU-JIHI-2       TO  YOSO (NG IDX1 10 22)
      *             ADD    SYU-JIHI-2-TAX   TO  YOSO (NG IDX1 IDX 22)
      *             ADD    SYU-JIHI-2-TAX   TO  YOSO (NG IDX1 10 22)
      *             ADD    SYU-JIHI-2       TO  YOSO (NG IDX3 IDX 22)
      *             ADD    SYU-JIHI-2       TO  YOSO (NG IDX3 10 22)
      *             ADD    SYU-JIHI-2-TAX   TO  YOSO (NG IDX3 IDX 22)
      *             ADD    SYU-JIHI-2-TAX   TO  YOSO (NG IDX3 10 22)
      **
      **  自費3            
      *             ADD    SYU-JIHI-3       TO  YOSO (NG IDX1 IDX 23)
      *             ADD    SYU-JIHI-3       TO  YOSO (NG IDX1 10 23)
      *             ADD    SYU-JIHI-3-TAX   TO  YOSO (NG IDX1 IDX 23)
      *             ADD    SYU-JIHI-3-TAX   TO  YOSO (NG IDX1 10 23)
      *             ADD    SYU-JIHI-3       TO  YOSO (NG IDX3 IDX 23)
      *             ADD    SYU-JIHI-3       TO  YOSO (NG IDX3 10 23)
      *             ADD    SYU-JIHI-3-TAX   TO  YOSO (NG IDX3 IDX 23)
      *             ADD    SYU-JIHI-3-TAX   TO  YOSO (NG IDX3 10 23)
      **
      **  自費4            
      *             ADD    SYU-JIHI-4       TO  YOSO (NG IDX1 IDX 24)
      *             ADD    SYU-JIHI-4       TO  YOSO (NG IDX1 10 24)
      *             ADD    SYU-JIHI-4-TAX   TO  YOSO (NG IDX1 IDX 24)
      *             ADD    SYU-JIHI-4-TAX   TO  YOSO (NG IDX1 10 24)
      *             ADD    SYU-JIHI-4       TO  YOSO (NG IDX3 IDX 24)
      *             ADD    SYU-JIHI-4       TO  YOSO (NG IDX3 10 24)
      *             ADD    SYU-JIHI-4-TAX   TO  YOSO (NG IDX3 IDX 24)
      *             ADD    SYU-JIHI-4-TAX   TO  YOSO (NG IDX3 10 24)
      **
      **  自費5            
      *             ADD    SYU-JIHI-5       TO  YOSO (NG IDX1 IDX 25)
      *             ADD    SYU-JIHI-5       TO  YOSO (NG IDX1 10 25)
      *             ADD    SYU-JIHI-5-TAX   TO  YOSO (NG IDX1 IDX 25)
      *             ADD    SYU-JIHI-5-TAX   TO  YOSO (NG IDX1 10 25)
      *             ADD    SYU-JIHI-5       TO  YOSO (NG IDX3 IDX 25)
      *             ADD    SYU-JIHI-5       TO  YOSO (NG IDX3 10 25)
      *             ADD    SYU-JIHI-5-TAX   TO  YOSO (NG IDX3 IDX 25)
      *             ADD    SYU-JIHI-5-TAX   TO  YOSO (NG IDX3 10 25)
      **
      **  自費消費税            
      *             ADD    SYU-JIHI-TAX     TO  YOSO (NG IDX1 IDX 26)
      *             ADD    SYU-JIHI-TAX     TO  YOSO (NG IDX1 10 26)
      *             ADD    SYU-JIHI-TAX     TO  YOSO (NG IDX3 IDX 26)
      *             ADD    SYU-JIHI-TAX     TO  YOSO (NG IDX3 10 26)
      **
      **** 20031021 追加   新規項目追加(自費合計)
      **
      **  自費合計        
      *             COMPUTE  YOSO (NG IDX1 IDX 27) =
      *                      YOSO (NG IDX1 IDX 21) +
      *                      YOSO (NG IDX1 IDX 22) +
      *                      YOSO (NG IDX1 IDX 23) +
      *                      YOSO (NG IDX1 IDX 24) +
      *                      YOSO (NG IDX1 IDX 25) +
      *                      YOSO (NG IDX1 IDX 26)
      *             COMPUTE  YOSO (NG IDX1 10  27) =
      *                      YOSO (NG IDX1 10  21) +
      *                      YOSO (NG IDX1 10  22) +
      *                      YOSO (NG IDX1 10  23) +
      *                      YOSO (NG IDX1 10  24) +
      *                      YOSO (NG IDX1 10  25) +
      *                      YOSO (NG IDX1 10  26)
      *             COMPUTE  YOSO (NG IDX3 IDX 27) =
      *                      YOSO (NG IDX3 IDX 21) +
      *                      YOSO (NG IDX3 IDX 22) +
      *                      YOSO (NG IDX3 IDX 23) +
      *                      YOSO (NG IDX3 IDX 24) +
      *                      YOSO (NG IDX3 IDX 25) +
      *                      YOSO (NG IDX3 IDX 26)
      *             COMPUTE  YOSO (NG IDX3 10  27) =
      *                      YOSO (NG IDX3 10  21) +
      *                      YOSO (NG IDX3 10  22) +
      *                      YOSO (NG IDX3 10  23) +
      *                      YOSO (NG IDX3 10  24) +
      *                      YOSO (NG IDX3 10  25) +
      *                      YOSO (NG IDX3 10  26)
      *-------------------------------------------------------------------
      *
      *    麻酔点数
           ADD    SYU-MSI-HKNTEN     TO   YOSO (NG IDX1 IDX 12)
           ADD    SYU-MSI-HKNTEN     TO   YOSO (NG IDX1 10 12)
           ADD    SYU-MSI-HKNTEN     TO   YOSO (NG IDX3 IDX 12)
           ADD    SYU-MSI-HKNTEN     TO   YOSO (NG IDX3 10 12)
           ADD    SYU-MSI-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    検査料保険点数
           ADD    SYU-KNS-HKNTEN     TO   YOSO (NG IDX1 IDX 13)
           ADD    SYU-KNS-HKNTEN     TO   YOSO (NG IDX1 10 13)
           ADD    SYU-KNS-HKNTEN     TO   YOSO (NG IDX3 IDX 13)
           ADD    SYU-KNS-HKNTEN     TO   YOSO (NG IDX3 10 13)
           ADD    SYU-KNS-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    X線料保険点数
           ADD    SYU-GZU-HKNTEN     TO   YOSO (NG IDX1 IDX 14)
           ADD    SYU-GZU-HKNTEN     TO   YOSO (NG IDX1 10 14)
           ADD    SYU-GZU-HKNTEN     TO   YOSO (NG IDX3 IDX 14)
           ADD    SYU-GZU-HKNTEN     TO   YOSO (NG IDX3 10 14)
           ADD    SYU-GZU-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    リハ点数
           ADD    SYU-ETC-HKNTEN     TO   YOSO (NG IDX1 IDX 15)
           ADD    SYU-ETC-HKNTEN     TO   YOSO (NG IDX1 10 15)
           ADD    SYU-ETC-HKNTEN     TO   YOSO (NG IDX3 IDX 15)
           ADD    SYU-ETC-HKNTEN     TO   YOSO (NG IDX3 10 15)
           ADD    SYU-ETC-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    精神科専門点数
           ADD    SYU-SSN-HKNTEN     TO   YOSO (NG IDX1 IDX 16)
           ADD    SYU-SSN-HKNTEN     TO   YOSO (NG IDX1 10 16)
           ADD    SYU-SSN-HKNTEN     TO   YOSO (NG IDX3 IDX 16)
           ADD    SYU-SSN-HKNTEN     TO   YOSO (NG IDX3 10 16)
           ADD    SYU-SSN-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    放射線治療点数
           ADD    SYU-HOU-HKNTEN     TO   YOSO (NG IDX1 IDX 17)
           ADD    SYU-HOU-HKNTEN     TO   YOSO (NG IDX1 10 17)
           ADD    SYU-HOU-HKNTEN     TO   YOSO (NG IDX3 IDX 17)
           ADD    SYU-HOU-HKNTEN     TO   YOSO (NG IDX3 10 17)
           ADD    SYU-HOU-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      * 20080326 H20年4月改正対応START tanaka
      *    病理診断点数
           ADD    SYU-BYR-HKNTEN     TO   YOSO (NG IDX1 IDX 40)
           ADD    SYU-BYR-HKNTEN     TO   YOSO (NG IDX1 10 40)
           ADD    SYU-BYR-HKNTEN     TO   YOSO (NG IDX3 IDX 40)
           ADD    SYU-BYR-HKNTEN     TO   YOSO (NG IDX3 10 40)
           ADD    SYU-BYR-HKNTEN     TO   SYU-TOTAL-HKNTEN
      * 20080326 H20年4月改正対応END
      *
      *** 20030410 追加 入院の場合は入院料を集計する。
      *--- 2010/09/09 DELETE START -------------------------------------
      *     IF   NG  =  1
      *--- 2010/09/09 DELETE END ---------------------------------------
      *    入院料保険点数
           ADD    SYU-NYN-HKNTEN   TO   YOSO (NG IDX1 IDX 18)
           ADD    SYU-NYN-HKNTEN   TO   YOSO (NG IDX1 10 18)
           ADD    SYU-NYN-HKNTEN   TO   YOSO (NG IDX3 IDX 18)
           ADD    SYU-NYN-HKNTEN   TO   YOSO (NG IDX3 10 18)
           ADD    SYU-NYN-HKNTEN   TO   SYU-TOTAL-HKNTEN
      *--- 2010/09/09 DELETE START -------------------------------------
      *     END-IF  
      *--- 2010/09/09 DELETE END ---------------------------------------
      *
      *    療養担当手当点数
           ADD    SYU-RYO-HKNTEN     TO   YOSO (NG IDX1 IDX 19)
           ADD    SYU-RYO-HKNTEN     TO   YOSO (NG IDX1 10 19)
           ADD    SYU-RYO-HKNTEN     TO   YOSO (NG IDX3 IDX 19)
           ADD    SYU-RYO-HKNTEN     TO   YOSO (NG IDX3 10 19)
           ADD    SYU-RYO-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    合計保険点数
           ADD    SYU-TOTAL-HKNTEN   TO   YOSO (NG IDX1 IDX 20)
           ADD    SYU-TOTAL-HKNTEN   TO   YOSO (NG IDX1 10 20)
           ADD    SYU-TOTAL-HKNTEN   TO   YOSO (NG IDX3 IDX 20)
           ADD    SYU-TOTAL-HKNTEN   TO   YOSO (NG IDX3 10 20)
      *
      *    保険適用外金額
           ADD    SYU-TOTAL-TGMONEY       TO   YOSO (NG IDX1 IDX 21)
           ADD    SYU-TOTAL-TGMONEY       TO   YOSO (NG IDX1 10  21)
           ADD    SYU-TOTAL-TGMONEY-TAX   TO   YOSO (NG IDX1 IDX 21)
           ADD    SYU-TOTAL-TGMONEY-TAX   TO   YOSO (NG IDX1 10  21)
           ADD    SYU-TOTAL-TGMONEY       TO   YOSO (NG IDX3 IDX 21)
           ADD    SYU-TOTAL-TGMONEY       TO   YOSO (NG IDX3 10  21)
           ADD    SYU-TOTAL-TGMONEY-TAX   TO   YOSO (NG IDX3 IDX 21)
           ADD    SYU-TOTAL-TGMONEY-TAX   TO   YOSO (NG IDX3 10  21)
      *
      *    負担金の集計
           IF   NG   =   1
      *    入院の場合
      *    食事療養費
             COMPUTE   YOSO (NG IDX1 IDX 22)  =  YOSO (NG IDX1 IDX 22)
                                              +  SYU-RYOYOHI-SKJ
                                              +  SYU-RYOYOHI-SKJ-JIHI
      *** 070821生活療養費追加START　T.Tanaka
                                              +  SYU-RYOYOHI-LIFE
      *** 追加END
             COMPUTE   YOSO (NG IDX1 10 22)  =  YOSO (NG IDX1 10 22)
                                             +  SYU-RYOYOHI-SKJ
                                             +  SYU-RYOYOHI-SKJ-JIHI
      *** 070821生活療養費追加START　T.Tanaka
                                             +  SYU-RYOYOHI-LIFE
      *** 追加END
             COMPUTE   YOSO (NG IDX3 IDX 22)  =  YOSO (NG IDX3 IDX 22)
                                              +  SYU-RYOYOHI-SKJ
                                              +  SYU-RYOYOHI-SKJ-JIHI
      *** 070821生活療養費追加START　T.Tanaka
                                              +  SYU-RYOYOHI-LIFE
      *** 追加END
             COMPUTE   YOSO (NG IDX3 10 22)  =  YOSO (NG IDX3 10 22)
                                             +  SYU-RYOYOHI-SKJ
                                             +  SYU-RYOYOHI-SKJ-JIHI
      *** 070821生活療養費追加START　T.Tanaka
                                             +  SYU-RYOYOHI-LIFE
      *** 追加END
      *    食事負担額
             COMPUTE   YOSO (NG IDX1 IDX 23)  =  YOSO (NG IDX1 IDX 23)
                                            +  SYU-SKYMONEY-SKJ-KEI 
                                            +  SYU-SKYMONEY-SKJ-JIHI-KEI
      *** 070821生活負担額追加START　T.Tanaka
                                            +  SYU-SKYMONEY-LIFE-KEI
      *** 追加END
             COMPUTE   YOSO (NG IDX1 10 23)  =  YOSO (NG IDX1 10 23)
                                            +  SYU-SKYMONEY-SKJ-KEI
                                            +  SYU-SKYMONEY-SKJ-JIHI-KEI
      *** 070821生活負担額追加START　T.Tanaka
                                            +  SYU-SKYMONEY-LIFE-KEI
      *** 追加END
             COMPUTE   YOSO (NG IDX3 IDX 23)  =  YOSO (NG IDX3 IDX 23)
                                            +  SYU-SKYMONEY-SKJ-KEI
                                            +  SYU-SKYMONEY-SKJ-JIHI-KEI
      *** 070821生活負担額追加START　T.Tanaka
                                            +  SYU-SKYMONEY-LIFE-KEI
      *** 追加END
             COMPUTE   YOSO (NG IDX3 10 23)  =  YOSO (NG IDX3 10 23)
                                            +  SYU-SKYMONEY-SKJ-KEI
                                            +  SYU-SKYMONEY-SKJ-JIHI-KEI
      *** 070821生活負担額追加START　T.Tanaka
                                            +  SYU-SKYMONEY-LIFE-KEI
      *** 追加END
      *    室料差額
             ADD SYU-RMSAGAKU        TO   YOSO (NG IDX1 IDX 24)
             ADD SYU-RMSAGAKU        TO   YOSO (NG IDX1 10 24)
             ADD SYU-RMSAGAKU        TO   YOSO (NG IDX3 IDX 24)
             ADD SYU-RMSAGAKU        TO   YOSO (NG IDX3 10 24)
           ELSE
      *    外来の場合
      *    薬剤一部負担金
             ADD    SYU-YKZ-FTN      TO   YOSO (NG IDX1 IDX 24)
             ADD    SYU-YKZ-FTN      TO   YOSO (NG IDX1 10 24)
             ADD    SYU-YKZ-FTN      TO   YOSO (NG IDX3 IDX 24)
             ADD    SYU-YKZ-FTN      TO   YOSO (NG IDX3 10 24)
           END-IF
      *    自費1
           ADD    SYU-JIHI-1         TO   YOSO (NG IDX1 IDX 25)
           ADD    SYU-JIHI-1         TO   YOSO (NG IDX1 10 25)
           ADD    SYU-JIHI-1-TAX     TO   YOSO (NG IDX1 IDX 25)
           ADD    SYU-JIHI-1-TAX     TO   YOSO (NG IDX1 10 25)
           ADD    SYU-JIHI-1         TO   YOSO (NG IDX3 IDX 25)
           ADD    SYU-JIHI-1         TO   YOSO (NG IDX3 10 25)
           ADD    SYU-JIHI-1-TAX     TO   YOSO (NG IDX3 IDX 25)
           ADD    SYU-JIHI-1-TAX     TO   YOSO (NG IDX3 10 25)
      *
      *    自費2
           ADD    SYU-JIHI-2         TO   YOSO (NG IDX1 IDX 26)
           ADD    SYU-JIHI-2         TO   YOSO (NG IDX1 10 26)
           ADD    SYU-JIHI-2-TAX     TO   YOSO (NG IDX1 IDX 26)
           ADD    SYU-JIHI-2-TAX     TO   YOSO (NG IDX1 10 26)
           ADD    SYU-JIHI-2         TO   YOSO (NG IDX3 IDX 26)
           ADD    SYU-JIHI-2         TO   YOSO (NG IDX3 10 26)
           ADD    SYU-JIHI-2-TAX     TO   YOSO (NG IDX3 IDX 26)
           ADD    SYU-JIHI-2-TAX     TO   YOSO (NG IDX3 10 26)
      *
      *    自費3
           ADD    SYU-JIHI-3         TO   YOSO (NG IDX1 IDX 27)
           ADD    SYU-JIHI-3         TO   YOSO (NG IDX1 10 27)
           ADD    SYU-JIHI-3-TAX     TO   YOSO (NG IDX1 IDX 27)
           ADD    SYU-JIHI-3-TAX     TO   YOSO (NG IDX1 10 27)
           ADD    SYU-JIHI-3         TO   YOSO (NG IDX3 IDX 27)
           ADD    SYU-JIHI-3         TO   YOSO (NG IDX3 10 27)
           ADD    SYU-JIHI-3-TAX     TO   YOSO (NG IDX3 IDX 27)
           ADD    SYU-JIHI-3-TAX     TO   YOSO (NG IDX3 10 27)
      *
      *    自費4
           ADD    SYU-JIHI-4         TO   YOSO (NG IDX1 IDX 28)
           ADD    SYU-JIHI-4         TO   YOSO (NG IDX1 10 28)
           ADD    SYU-JIHI-4-TAX     TO   YOSO (NG IDX1 IDX 28)
           ADD    SYU-JIHI-4-TAX     TO   YOSO (NG IDX1 10 28)
           ADD    SYU-JIHI-4         TO   YOSO (NG IDX3 IDX 28)
           ADD    SYU-JIHI-4         TO   YOSO (NG IDX3 10 28)
           ADD    SYU-JIHI-4-TAX     TO   YOSO (NG IDX3 IDX 28)
           ADD    SYU-JIHI-4-TAX     TO   YOSO (NG IDX3 10 28)
      *
      *    自費5
           ADD    SYU-JIHI-5         TO   YOSO (NG IDX1 IDX 29)
           ADD    SYU-JIHI-5         TO   YOSO (NG IDX1 10 29)
           ADD    SYU-JIHI-5-TAX     TO   YOSO (NG IDX1 IDX 29)
           ADD    SYU-JIHI-5-TAX     TO   YOSO (NG IDX1 10 29)
           ADD    SYU-JIHI-5         TO   YOSO (NG IDX3 IDX 29)
           ADD    SYU-JIHI-5         TO   YOSO (NG IDX3 10 29)
           ADD    SYU-JIHI-5-TAX     TO   YOSO (NG IDX3 IDX 29)
           ADD    SYU-JIHI-5-TAX     TO   YOSO (NG IDX3 10 29)
      *
      *    自費6
           ADD    SYU-JIHI-6         TO   YOSO (NG IDX1 IDX 30)
           ADD    SYU-JIHI-6         TO   YOSO (NG IDX1 10 30)
           ADD    SYU-JIHI-6-TAX     TO   YOSO (NG IDX1 IDX 30)
           ADD    SYU-JIHI-6-TAX     TO   YOSO (NG IDX1 10 30)
           ADD    SYU-JIHI-6         TO   YOSO (NG IDX3 IDX 30)
           ADD    SYU-JIHI-6         TO   YOSO (NG IDX3 10 30)
           ADD    SYU-JIHI-6-TAX     TO   YOSO (NG IDX3 IDX 30)
           ADD    SYU-JIHI-6-TAX     TO   YOSO (NG IDX3 10 30)
      *
      *    自費7
           ADD    SYU-JIHI-7         TO   YOSO (NG IDX1 IDX 31)
           ADD    SYU-JIHI-7         TO   YOSO (NG IDX1 10 31)
           ADD    SYU-JIHI-7-TAX     TO   YOSO (NG IDX1 IDX 31)
           ADD    SYU-JIHI-7-TAX     TO   YOSO (NG IDX1 10 31)
           ADD    SYU-JIHI-7         TO   YOSO (NG IDX3 IDX 31)
           ADD    SYU-JIHI-7         TO   YOSO (NG IDX3 10 31)
           ADD    SYU-JIHI-7-TAX     TO   YOSO (NG IDX3 IDX 31)
           ADD    SYU-JIHI-7-TAX     TO   YOSO (NG IDX3 10 31)
      *
      *    自費8
           ADD    SYU-JIHI-8         TO   YOSO (NG IDX1 IDX 32)
           ADD    SYU-JIHI-8         TO   YOSO (NG IDX1 10 32)
           ADD    SYU-JIHI-8-TAX     TO   YOSO (NG IDX1 IDX 32)
           ADD    SYU-JIHI-8-TAX     TO   YOSO (NG IDX1 10 32)
           ADD    SYU-JIHI-8         TO   YOSO (NG IDX3 IDX 32)
           ADD    SYU-JIHI-8         TO   YOSO (NG IDX3 10 32)
           ADD    SYU-JIHI-8-TAX     TO   YOSO (NG IDX3 IDX 32)
           ADD    SYU-JIHI-8-TAX     TO   YOSO (NG IDX3 10 32)
      *
      *    自費9
           ADD    SYU-JIHI-9         TO   YOSO (NG IDX1 IDX 33)
           ADD    SYU-JIHI-9         TO   YOSO (NG IDX1 10 33)
           ADD    SYU-JIHI-9-TAX     TO   YOSO (NG IDX1 IDX 33)
           ADD    SYU-JIHI-9-TAX     TO   YOSO (NG IDX1 10 33)
           ADD    SYU-JIHI-9         TO   YOSO (NG IDX3 IDX 33)
           ADD    SYU-JIHI-9         TO   YOSO (NG IDX3 10 33)
           ADD    SYU-JIHI-9-TAX     TO   YOSO (NG IDX3 IDX 33)
           ADD    SYU-JIHI-9-TAX     TO   YOSO (NG IDX3 10 33)
      *
      *    自費10
           ADD    SYU-JIHI-10        TO   YOSO (NG IDX1 IDX 34)
           ADD    SYU-JIHI-10        TO   YOSO (NG IDX1 10 34)
           ADD    SYU-JIHI-10-TAX    TO   YOSO (NG IDX1 IDX 34)
           ADD    SYU-JIHI-10-TAX    TO   YOSO (NG IDX1 10 34)
           ADD    SYU-JIHI-10        TO   YOSO (NG IDX3 IDX 34)
           ADD    SYU-JIHI-10        TO   YOSO (NG IDX3 10 34)
           ADD    SYU-JIHI-10-TAX    TO   YOSO (NG IDX3 IDX 34)
           ADD    SYU-JIHI-10-TAX    TO   YOSO (NG IDX3 10 34)
      *
      *    自費消費税
           ADD    SYU-JIHI-TAX       TO   YOSO (NG IDX1 IDX 35)
           ADD    SYU-JIHI-TAX       TO   YOSO (NG IDX1 10 35)
           ADD    SYU-JIHI-TAX       TO   YOSO (NG IDX3 IDX 35)
           ADD    SYU-JIHI-TAX       TO   YOSO (NG IDX3 10 35)
      *
      *    自費合計
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) start
           IF   SYU-SRYYMD  <=  "20070331"
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) end
             COMPUTE  YOSO (NG IDX1 IDX 36)  =  YOSO (NG IDX1 IDX 25)
                                             +  YOSO (NG IDX1 IDX 26)
                                             +  YOSO (NG IDX1 IDX 27)
                                             +  YOSO (NG IDX1 IDX 28)
                                             +  YOSO (NG IDX1 IDX 29)
                                             +  YOSO (NG IDX1 IDX 30)
                                             +  YOSO (NG IDX1 IDX 31)
                                             +  YOSO (NG IDX1 IDX 32)
                                             +  YOSO (NG IDX1 IDX 33)
                                             +  YOSO (NG IDX1 IDX 34)
                                             +  YOSO (NG IDX1 IDX 35)
             COMPUTE  YOSO (NG IDX1 10  36)  =  YOSO (NG IDX1 10  25)
                                             +  YOSO (NG IDX1 10  26)
                                             +  YOSO (NG IDX1 10  27)
                                             +  YOSO (NG IDX1 10  28)
                                             +  YOSO (NG IDX1 10  29)
                                             +  YOSO (NG IDX1 10  30)
                                             +  YOSO (NG IDX1 10  31)
                                             +  YOSO (NG IDX1 10  32)
                                             +  YOSO (NG IDX1 10  33)
                                             +  YOSO (NG IDX1 10  34)
                                             +  YOSO (NG IDX1 10  35)
             COMPUTE  YOSO (NG IDX3 IDX 36)  =  YOSO (NG IDX3 IDX 25)
                                             +  YOSO (NG IDX3 IDX 26)
                                             +  YOSO (NG IDX3 IDX 27)
                                             +  YOSO (NG IDX3 IDX 28)
                                             +  YOSO (NG IDX3 IDX 29)
                                             +  YOSO (NG IDX3 IDX 30)
                                             +  YOSO (NG IDX3 IDX 31)
                                             +  YOSO (NG IDX3 IDX 32)
                                             +  YOSO (NG IDX3 IDX 33)
                                             +  YOSO (NG IDX3 IDX 34)
                                             +  YOSO (NG IDX3 IDX 35)
             COMPUTE  YOSO (NG IDX3 10  36)  =  YOSO (NG IDX3 10  25)
                                             +  YOSO (NG IDX3 10  26)
                                             +  YOSO (NG IDX3 10  27)
                                             +  YOSO (NG IDX3 10  28)
                                             +  YOSO (NG IDX3 10  29)
                                             +  YOSO (NG IDX3 10  30)
                                             +  YOSO (NG IDX3 10  31)
                                             +  YOSO (NG IDX3 10  32)
                                             +  YOSO (NG IDX3 10  33)
                                             +  YOSO (NG IDX3 10  34)
                                             +  YOSO (NG IDX3 10  35)
           ELSE
             COMPUTE  YOSO (NG IDX1 IDX 36)  =  YOSO (NG IDX1 IDX 25)
                                             +  YOSO (NG IDX1 IDX 26)
                                             +  YOSO (NG IDX1 IDX 27)
                                             +  YOSO (NG IDX1 IDX 28)
                                             +  YOSO (NG IDX1 IDX 29)
                                             +  YOSO (NG IDX1 IDX 30)
                                             +  YOSO (NG IDX1 IDX 31)
                                             +  YOSO (NG IDX1 IDX 32)
                                             +  YOSO (NG IDX1 IDX 33)
                                             +  YOSO (NG IDX1 IDX 34)
             COMPUTE  YOSO (NG IDX1 10  36)  =  YOSO (NG IDX1 10  25)
                                             +  YOSO (NG IDX1 10  26)
                                             +  YOSO (NG IDX1 10  27)
                                             +  YOSO (NG IDX1 10  28)
                                             +  YOSO (NG IDX1 10  29)
                                             +  YOSO (NG IDX1 10  30)
                                             +  YOSO (NG IDX1 10  31)
                                             +  YOSO (NG IDX1 10  32)
                                             +  YOSO (NG IDX1 10  33)
                                             +  YOSO (NG IDX1 10  34)
             COMPUTE  YOSO (NG IDX3 IDX 36)  =  YOSO (NG IDX3 IDX 25)
                                             +  YOSO (NG IDX3 IDX 26)
                                             +  YOSO (NG IDX3 IDX 27)
                                             +  YOSO (NG IDX3 IDX 28)
                                             +  YOSO (NG IDX3 IDX 29)
                                             +  YOSO (NG IDX3 IDX 30)
                                             +  YOSO (NG IDX3 IDX 31)
                                             +  YOSO (NG IDX3 IDX 32)
                                             +  YOSO (NG IDX3 IDX 33)
                                             +  YOSO (NG IDX3 IDX 34)
             COMPUTE  YOSO (NG IDX3 10  36)  =  YOSO (NG IDX3 10  25)
                                             +  YOSO (NG IDX3 10  26)
                                             +  YOSO (NG IDX3 10  27)
                                             +  YOSO (NG IDX3 10  28)
                                             +  YOSO (NG IDX3 10  29)
                                             +  YOSO (NG IDX3 10  30)
                                             +  YOSO (NG IDX3 10  31)
                                             +  YOSO (NG IDX3 10  32)
                                             +  YOSO (NG IDX3 10  33)
                                             +  YOSO (NG IDX3 10  34)
           END-IF
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) start
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
      *
      *
      *** 20031021 追加   新規項目追加(自費合計)
      *
      *    保険分金額
      *
      ***        IF   IDX NOT =  7
      ***             COMPUTE   YOSO (IDX1 IDX 26)  =
      ***                       SYU-TOTAL-HKNTEN  *  10  - WRK-MONEY +
      ***                       YOSO (IDX1 IDX 26)
      ***             COMPUTE   YOSO (IDX1 10 26)  =
      ***                       SYU-TOTAL-HKNTEN  *  10  - WRK-MONEY +
      ***                       YOSO (IDX1 10 26)
      ***             COMPUTE   YOSO (IDX3 IDX 26)  =
      ***                       SYU-TOTAL-HKNTEN  *  10  - WRK-MONEY +
      ***                       YOSO (IDX3 IDX 26)
      ***             COMPUTE   YOSO (IDX3 10 26)  =
      ***                       SYU-TOTAL-HKNTEN  *  10  - WRK-MONEY +
      ***                       YOSO (IDX3 10 26)
      ***        END-IF
      *
      ***           IF   IDX NOT =  7
      ***             COMPUTE   YOSO (IDX1 IDX 26)  =
      ***                       SYU-TOTAL-HKNTEN  *  10  - SYU-SKYMONEY +
      ***                       YOSO (IDX1 IDX 26)
      ***             COMPUTE   YOSO (IDX1 10 26)  =
      ***                       SYU-TOTAL-HKNTEN  *  10  - SYU-SKYMONEY +
      ***                       YOSO (IDX1 10 26)
      ***             COMPUTE   YOSO (IDX3 IDX 26)  =
      ***                       SYU-TOTAL-HKNTEN  *  10  - SYU-SKYMONEY +
      ***                       YOSO (IDX3 IDX 26)
      ***             COMPUTE   YOSO (IDX3 10 26)  =
      ***                       SYU-TOTAL-HKNTEN  *  10  - SYU-SKYMONEY +
      ***                       YOSO (IDX3 10 26)
      ***           END-IF
      *
      ***             COMPUTE   YOSO (IDX1 IDX 26)  =
      ***                       YOSO (IDX1 IDX 26) +
      ***                       (SYU-TOTAL-HKNTEN  *  10) -
      ***                       (SYU-SYUCOMPTOTAL  +  SYU-KOH1COMPTOTAL +
      ***                        SYU-KOH2COMPTOTAL +  SYU-KOH3COMPTOTAL +
      ***                        SYU-KOH4COMPTOTAL) 
      ***             COMPUTE   YOSO (IDX1 10 26)  =
      ***                       YOSO (IDX1 10 26) +
      ***                       (SYU-TOTAL-HKNTEN  *  10) -
      ***                       (SYU-SYUCOMPTOTAL  +  SYU-KOH1COMPTOTAL +
      ***                        SYU-KOH2COMPTOTAL +  SYU-KOH3COMPTOTAL +
      ***                        SYU-KOH4COMPTOTAL) 
      ***             COMPUTE   YOSO (IDX3 IDX 26)  =
      ***                       YOSO (IDX3 IDX 26) +
      ***                       (SYU-TOTAL-HKNTEN  *  10) -
      ***                       (SYU-SYUCOMPTOTAL  +  SYU-KOH1COMPTOTAL +
      ***                        SYU-KOH2COMPTOTAL +  SYU-KOH3COMPTOTAL +
      ***                        SYU-KOH4COMPTOTAL) 
      ***             COMPUTE   YOSO (IDX3 10 26)  =
      ***                       YOSO (IDX3 10 26) +
      ***                       (SYU-TOTAL-HKNTEN  *  10) -
      ***                       (SYU-SYUCOMPTOTAL  +  SYU-KOH1COMPTOTAL +
      ***                        SYU-KOH2COMPTOTAL +  SYU-KOH3COMPTOTAL +
      ***                        SYU-KOH4COMPTOTAL) 
      *
      ***             COMPUTE   YOSO (IDX1 IDX 26) =
      ***                       YOSO (IDX1 IDX 26) +
      ***                       (SYU-TOTAL-HKNTEN  *  10)  *
      ***                       ((100 - SYU-PTFTNRATE)  /  100) 
      ***             COMPUTE   YOSO (IDX1 10 26) =
      ***                      YOSO (IDX1 10 26) +
      ***                       (SYU-TOTAL-HKNTEN  *  10)  *
      ***                       ((100 - SYU-PTFTNRATE)  /  100)
      ***             COMPUTE   YOSO (IDX3 IDX 26) =
      ***                       YOSO (IDX3 IDX 26) +

      ***                       (SYU-TOTAL-HKNTEN  *  10)  *
      ***                       ((100 - SYU-PTFTNRATE)  /  100)
      ***             COMPUTE   YOSO (IDX3 10 26) =
      ***                      YOSO (IDX3 10 26) +
      ***                       (SYU-TOTAL-HKNTEN  *  10)  *
      ***                       ((100 - SYU-PTFTNRATE)  /  100)
      * 
      ** 20030228 保険分金額の仕樣を変更  
      ***         EVALUATE IDX 
      ***             WHEN 8 
      ***  労災の場合
      * 
      ***                  COMPUTE YOSO (IDX1 IDX 26) =
      ***                   YOSO (IDX1 IDX 26) +
      ***                   (SYU-TOTAL-HKNTEN  *  WRK-4001-TENTANKA)  *
      ***                   ((100 - SYU-PTFTNRATE)  /  100) 
      ***                  COMPUTE YOSO (IDX1 10 26) =
      ***                   YOSO (IDX1 10 26) +
      ***                   (SYU-TOTAL-HKNTEN  *  WRK-4001-TENTANKA)  *
      ***                   ((100 - SYU-PTFTNRATE)  /  100)
      ***                  COMPUTE YOSO (IDX3 IDX 26) =
      ***                   YOSO (IDX3 IDX 26) +
      ***                   (SYU-TOTAL-HKNTEN  *  WRK-4001-TENTANKA)  *
      ***                   ((100 - SYU-PTFTNRATE)  /  100)
      ***                  COMPUTE YOSO (IDX3 10 26) =
      ***                   YOSO (IDX3 10 26) +
      ***                   (SYU-TOTAL-HKNTEN  *  WRK-4001-TENTANKA)  *
      ***                   ((100 - SYU-PTFTNRATE)  /  100)
      ***            WHEN OTHER
      ***  労災以外の場合(自賠責を含む)
      ***                   COMPUTE   YOSO (IDX1 IDX 26) =
      ***                    YOSO (IDX1 IDX 26) +
      ***                    (SYU-TOTAL-HKNTEN  *  10)  *
      ***                    ((100 - SYU-PTFTNRATE)  /  100) 
      ***                   COMPUTE   YOSO (IDX1 10 26) =
      ***                    YOSO (IDX1 10 26) +
      ***                    (SYU-TOTAL-HKNTEN  *  10)  *
      ***                    ((100 - SYU-PTFTNRATE)  /  100)
      ***                   COMPUTE   YOSO (IDX3 IDX 26) =
      ***                    YOSO (IDX3 IDX 26) +
      ***                    (SYU-TOTAL-HKNTEN  *  10)  *
      ***                    ((100 - SYU-PTFTNRATE)  /  100)
      ***                   COMPUTE   YOSO (IDX3 10 26) =
      ***                    YOSO (IDX3 10 26) +
      ***                    (SYU-TOTAL-HKNTEN  *  10)  *
      ***                    ((100 - SYU-PTFTNRATE)  /  100)
      ***           END-EVALUATE   
      ** 20030228 保険分金額の仕樣を変更  
      *
      *  患者負担金額            
      ***             ADD    WRK-MONEY        TO  YOSO (NG IDX1 IDX 27)
      ***             ADD    WRK-MONEY        TO  YOSO (NG IDX1 10 27)
      ***             ADD    WRK-MONEY        TO  YOSO (NG IDX3 IDX 27)
      ***             ADD    WRK-MONEY        TO  YOSO (NG IDX3 10 27)
      *
      *** 20031021 変更 配列のインデックス
      ***             ADD    SYU-SKYMONEY        TO  YOSO (NG IDX1 IDX 27)
      ***             ADD    SYU-SKYMONEY        TO  YOSO (NG IDX1 10 27)
      ***             ADD    SYU-SKYMONEY        TO  YOSO (NG IDX3 IDX 27)
      ***             ADD    SYU-SKYMONEY        TO  YOSO (NG IDX3 10 27)
      *
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *             ADD    SYU-SKYMONEY        TO  YOSO (NG IDX1 IDX 29)
      *             ADD    SYU-SKYMONEY        TO  YOSO (NG IDX1 10 29)
      *             ADD    SYU-SKYMONEY        TO  YOSO (NG IDX3 IDX 29)
      *             ADD    SYU-SKYMONEY        TO  YOSO (NG IDX3 10 29)
           ADD    SYU-SKYMONEY        TO   YOSO (NG IDX1 IDX 38)
           ADD    SYU-SKYMONEY        TO   YOSO (NG IDX1 10 38)
           ADD    SYU-SKYMONEY        TO   YOSO (NG IDX3 IDX 38)
           ADD    SYU-SKYMONEY        TO   YOSO (NG IDX3 10 38)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
      *** 20031021 変更 配列のインデックス
      *
      *    医療収益計
      *  20030228 医療収益計の仕樣を変更  
      ***             COMPUTE   YOSO (NG IDX1 IDX 28)  =
      ***                       YOSO (NG IDX1 IDX 26)  +  YOSO (NG IDX1 IDX 27)
      ***             COMPUTE   YOSO (NG IDX1 10 28)  =
      ***                       YOSO (NG IDX1 10 26)  +  YOSO (NG IDX1 10 27)
      ***             COMPUTE   YOSO (NG IDX3 IDX 28)  =
      ***                       YOSO (NG IDX3 IDX 26)  +  YOSO (NG IDX3 IDX 27)
      ***             COMPUTE   YOSO (NG IDX3 10 28)  =
      ***                       YOSO (NG IDX3 10 26)  +  YOSO (NG IDX3 10 27)
      *
      *--- 2009/12/08 DELETE START -------------------------------------
      *--- 2009/02/06 ADD START ----------------------------------------
      *--- 2009/02/17 CHANGE START -------------------------------------
      *--- 医療収益の計算式を修正 2009/04/08 CHANGE START --------------
      ***  2009/05/26 CHANGE START -------------------------------------
      *     IF  SYU-KOH4HKNNUM  NOT =  SPACE
      *       MOVE   SYU-KOH4COMPFTN    TO   WRK-COMPFTN
      **       MOVE   SYU-KOH4HKNFTNMONEY    TO   WRK-COMPFTN
      *     ELSE
      *       IF  SYU-KOH3HKNNUM  NOT =  SPACE
      *         MOVE   SYU-KOH3COMPFTN    TO   WRK-COMPFTN
      **         MOVE   SYU-KOH3HKNFTNMONEY    TO   WRK-COMPFTN
      *       ELSE
      *         IF  SYU-KOH2HKNNUM  NOT =  SPACE
      *           MOVE   SYU-KOH2COMPFTN    TO   WRK-COMPFTN
      **           MOVE   SYU-KOH2HKNFTNMONEY    TO   WRK-COMPFTN
      *         ELSE
      *           IF  SYU-KOH1HKNNUM  NOT =  SPACE
      *             MOVE   SYU-KOH1COMPFTN    TO   WRK-COMPFTN
      **             MOVE   SYU-KOH1HKNFTNMONEY    TO   WRK-COMPFTN
      *           ELSE
      *             IF  SYU-SYUHKNNUM  NOT =  SPACE
      *               MOVE   SYU-SYUCOMPFTN     TO   WRK-COMPFTN
      **               MOVE   SYU-SYUHKNFTNMONEY    TO   WRK-COMPFTN
      ****  2009/05/26 CHANGE END   -------------------------------------
      **--- 医療収益の計算式を修正 2009/04/08 CHANGE END   --------------
      *             END-IF
      *           END-IF
      *         END-IF
      *       END-IF
      *     END-IF
      *--- 2009/02/17 CHANGE END ---------------------------------------
      *--- 2009/02/06 ADD END ------------------------------------------
      *--- 2009/12/08 DELETE END ---------------------------------------
           EVALUATE IDX 
             WHEN 7 
      *    自費の場合
               MOVE    SYU-SKYMONEY      TO   WRK-SYUEKI
             WHEN 8 
      ***  労災の場合
               COMPUTE  WRK-SYUEKI ROUNDED  = 
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
      * 薬剤一部負担金は発生しない為削除
      ***                  +  SYU-RJN-FTN
      ***                  +  SYU-KOH-FTN
      * 20030814 削除
                                  +  SYU-YKZ-FTN
                                  +  SYU-RYOYOHI-SKJ
                                  +  SYU-RYOYOHI-SKJ-JIHI
      * 20080418 医療収益に生活療養費及び負担額が加算されない不具合を修正START
                                  +  SYU-RYOYOHI-LIFE
      ***  2009/05/26 ADD START ----------------------------------------
                                  +  SYU-RYOYOHI-LIFE-JIHI
      ***  2009/05/26 ADD END   ----------------------------------------
      * 20080603 医療収益の値が正しく印字されない不具合を修正START tanaka
      *                  +  SYU-SKYMONEY-LIFE-KEI
      * 20080603 医療収益の値が正しく印字されない不具合を修正END
      * 20080418 医療収益に生活療養費及び負担額が加算されない不具合を修正END
      * 20040202 削除 食事自己負担額は食事療養費に含まれる為、削除
      ***                  +  SYU-SKYMONEY-SKJ-KEI
      ***                  +  SYU-SKYMONEY-SKJ-JIHI-KEI
      * 20040202 削除 食事自己負担額は食事療養費に含まれる為、削除
                        +  SYU-RMSAGAKU
      *
      * 20040202 変更
             WHEN 9
      *    自賠責の場合
      * 20080612 健保準拠時の自賠責の計算を修正START tanaka
               MOVE   SYU-RSIJIBAI-SKYMONEY     TO   WRK-SYUEKI
      *               COMPUTE  WRK-SYUEKI ROUNDED = 
      *               (SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA)
      * 20080612 健保準拠時の自賠責の計算を修正END
      *=== 自賠責計算式変更対応 By Takashima(2006/12/20) start
      *               COMPUTE  WRK-SYUEKI =
      *                      ( WRK-SYUEKI * (SYS-4001-KSNRATE / 100) ) 
      *                     +  WRK-SYUEKI
      *               COMPUTE  WRK-SYUEKI ROUNDED =
      *                      ( WRK-SYUEKI * (SYS-4001-KSNRATE1 / 100) ) 
      * 20080612 健保準拠時の自賠責の計算を修正START tanaka
      *               COMPUTE  WRK-SYUEKI  =
      *                        WRK-SYUEKI
      **=== 自賠責計算式変更対応 By Takashima(2006/12/20) end
      *                     +  SYU-RSISHOSHIN-MONEY 
      *                     +  SYU-RSISAISHIN-MONEY 
      *                     +  SYU-RSISDO-MONEY 
      *                     +  SYU-RSIETC-MONEY  
      *                     +  WRK-JIHI 
      *                     +  SYU-CHOSEI 
      * 20080612 健保準拠時の自賠責の計算を修正END
      * 20030411 追加
      * 20030814 削除
      * 老人一部負担金、公費一部負担金は保険点数に含まれている為削除
      ***                  +  SYU-RJN-FTN
      ***                  +  SYU-KOH-FTN
      * 20030814 削除
      * 20080612 健保準拠時の自賠責の計算を修正START tanaka
      *                     +  SYU-YKZ-FTN
      *                     +  SYU-RYOYOHI-SKJ
      *                     +  SYU-RYOYOHI-SKJ-JIHI
      * 20080612 健保準拠時の自賠責の計算を修正END
      * 20080418 医療収益に生活療養費及び負担額が加算されない不具合を修正START
      *                     +  SYU-RYOYOHI-LIFE
      * 20080603 医療収益の値が正しく印字されない不具合を修正START tanaka
      *                     +  SYU-SKYMONEY-LIFE-KEI
      * 20080603 医療収益の値が正しく印字されない不具合を修正END
      * 20080418 医療収益に生活療養費及び負担額が加算されない不具合を修正END
      * 20040202 削除 自己負担額は食事療養費に含まれている為、削除する。
      ***                  +  SYU-SKYMONEY-SKJ-KEI
      ***                  +  SYU-SKYMONEY-SKJ-JIHI-KEI
      * 20040202 削除 自己負担額は食事療養費に含まれている為、削除する。
      * 20080612 健保準拠時の自賠責の計算を修正START tanaka
      *                     +  SYU-RMSAGAKU
      * 20080612 健保準拠時の自賠責の計算を修正END
      * 20040202 変更
      * 20080326 H20年4月改正対応START tanaka
             WHEN 11
      *    公害の場合
               COMPUTE WRK-JIHI4  =  SYU-TOTAL-TGMONEY
                                  +  SYU-TOTAL-TGMONEY-TAX
                                  +  SYU-JIHI-TOTAL
                                  +  SYU-JIHI-TOTAL-TAX
                                  +  SYU-RMSAGAKU
                                  +  SYU-SKYMONEY-SKJ-JIHI-KEI
      ***  2009/05/26 ADD START ----------------------------------------
                                  +  SYU-SKYMONEY-LIFE-JIHI-KEI
      ***  2009/05/26 ADD END   ----------------------------------------
               IF   SYU-SRYYMD  <  "20070401"
                 COMPUTE   WRK-JIHI4    =   WRK-JIHI4 
                                        +   SYU-JIHI-TAX
               END-IF
                 COMPUTE   WRK-SYUEKI   =   SYU-RSIJIBAI-SKYMONEY
      * 20080326 H20年4月改正対応END
             WHEN OTHER
      *    自費、労災、自賠責以外の場合
               COMPUTE  WRK-SYUEKI   =  (SYU-TOTAL-HKNTEN * HKN-TENTNK)
                                     +  WRK-JIHI 
                                     +  SYU-CHOSEI
      * 20030411 追加
      * 20030814 削除
      * 老人一部負担金、公費一部負担金は保険点数に含まれている為削除
      ***               +  SYU-RJN-FTN
      ***               +  SYU-KOH-FTN
      * 20030814 削除
                                     +  SYU-YKZ-FTN
                                     +  SYU-RYOYOHI-SKJ
                                     +  SYU-RYOYOHI-SKJ-JIHI
      * 20080418 医療収益に生活療養費及び負担額が加算されない不具合を修正START
                                     +  SYU-RYOYOHI-LIFE
      ***  2009/05/26 ADD START ----------------------------------------
                                     +  SYU-RYOYOHI-LIFE-JIHI
      ***  2009/05/26 ADD END   ----------------------------------------
      * 20080603 医療収益の値が正しく印字されない不具合を修正START tanaka
      *                  +  SYU-SKYMONEY-LIFE-KEI
      * 20080603 医療収益の値が正しく印字されない不具合を修正END
      * 20080418 医療収益に生活療養費及び負担額が加算されない不具合を修正END
      * 20040202 削除 食事自己負担額は食事療養費に含まれる為、削除
      ***               +  SYU-SKYMONEY-SKJ-KEI
      ***               +  SYU-SKYMONEY-SKJ-JIHI-KEI
      * 20040202 削除 食事自己負担額は食事療養費に含まれる為、削除
                        +  SYU-RMSAGAKU
      *--- 2008/12/17 ADD START ----------------------------------------
      *--- 2009/02/06 CHANGE START ----------------------------------------
      *         COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  -  SYU-DISCOUNT-MONEY
      ***  2009/05/26 ADD START ----------------------------------------
               IF  SYU-DISCOUNT-MONEY  >  0
      ***  2009/05/26 ADD END   ----------------------------------------
      *--- 2009/12/08 ADD + CHANGE START -------------------------------
      *         COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI
      *                              -  (WRK-COMPFTN
      *                              +  SYU-SKYMONEY-SKJ-KEI
      *                              +  SYU-SKYMONEY-SKJ-JIHI-KEI
      *                              +  SYU-SKYMONEY-LIFE-KEI
      *                              +  SYU-SKYMONEY-LIFE-JIHI-KEI
      *                              +  SYU-RMSAGAKU
      *                              +  SYU-CHOSEI 
      *                              +  WRK-JIHI
      *                              -  SYU-SKYMONEY)
                 COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI
                                      -  SYU-DISCOUNT-MONEY
                 EVALUATE  SYS-1001-DISCOUNT-HASU-KBN
                   WHEN  1
                     COMPUTE  WRK-SYUEKI  ROUNDED  =  WRK-SYUEKI  /  10
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10
                   WHEN  2
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  /  10
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10 
                   WHEN  3
                     IF   WRK-SYUEKI(8:1)  NOT =  0
                       COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  /  10
                       COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10  +  10
                     END-IF
                 END-EVALUATE
      *--- 2009/12/08 ADD + CHANGE END ---------------------------------
      ***  2009/05/26 ADD START ----------------------------------------
               END-IF
      ***  2009/05/26 ADD END   ----------------------------------------
      *--- 2009/02/06 CHANGE END ---------------------------------------
      *--- 2008/12/17 ADD END ------------------------------------------
      *
      * 20030411 追加
           END-EVALUATE
      *** 20031021 変更 配列のインデックス
      ***           COMPUTE YOSO (NG IDX1 IDX 28) =
      ***                   YOSO (NG IDX1 IDX 28) + WRK-SYUEKI
      ***           COMPUTE YOSO (NG IDX1 10 28) =
      ***                   YOSO (NG IDX1 10 28)  + WRK-SYUEKI
      ***           COMPUTE YOSO (NG IDX3 IDX 28) =
      ***                   YOSO (NG IDX3 IDX 28) + WRK-SYUEKI
      ***           COMPUTE YOSO (NG IDX3 10 28) =
      ***                   YOSO (NG IDX3 10 28)  + WRK-SYUEKI
      *
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *           COMPUTE YOSO (NG IDX1 IDX 30) =
      *                   YOSO (NG IDX1 IDX 30) + WRK-SYUEKI
      *           COMPUTE YOSO (NG IDX1 10 30) =
      *                   YOSO (NG IDX1 10 30)  + WRK-SYUEKI
      *           COMPUTE YOSO (NG IDX3 IDX 30) =
      *                   YOSO (NG IDX3 IDX 30) + WRK-SYUEKI
      *           COMPUTE YOSO (NG IDX3 10 30) =
      *                   YOSO (NG IDX3 10 30)  + WRK-SYUEKI
           COMPUTE   YOSO (NG IDX1 IDX 39)  =  YOSO (NG IDX1 IDX 39)
                                            +  WRK-SYUEKI
           COMPUTE   YOSO (NG IDX1 10 39)   =  YOSO (NG IDX1 10 39)
                                            +  WRK-SYUEKI
           COMPUTE   YOSO (NG IDX3 IDX 39)  =  YOSO (NG IDX3 IDX 39)
                                            +  WRK-SYUEKI
           COMPUTE   YOSO (NG IDX3 10 39)   =  YOSO (NG IDX3 10 39)
                                            +  WRK-SYUEKI
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
      *** 20031021 変更 配列のインデックス
      *  20030228 医療収益計の仕樣を変更
      *
      *  20030228 保険分金額の仕樣を変更
      *
      *    保険分金額
      *** 20031021 変更 配列のインデックス
      ***             COMPUTE   YOSO (NG IDX1 IDX 26)  =
      ***                YOSO (NG IDX1 IDX 28)  -  YOSO (NG IDX1 IDX 27)
      ***             COMPUTE   YOSO (NG IDX1 10 26)   =
      ***                YOSO (NG IDX1 10 28)   -  YOSO (NG IDX1 10 27)
      ***             COMPUTE   YOSO (NG IDX3 IDX 26)  =
      ***                YOSO (NG IDX3 IDX 28)  -  YOSO (NG IDX3 IDX 27)
      ***             COMPUTE   YOSO (NG IDX3 10 26)   =
      ***                YOSO (NG IDX3 10 28)   -  YOSO (NG IDX3 10 27)
      *
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *             COMPUTE   YOSO (NG IDX1 IDX 28)  =
      *                YOSO (NG IDX1 IDX 30)  -  YOSO (NG IDX1 IDX 29)
      *             COMPUTE   YOSO (NG IDX1 10 28)   =
      *                YOSO (NG IDX1 10 30)   -  YOSO (NG IDX1 10 29)
      *             COMPUTE   YOSO (NG IDX3 IDX 28)  =
      *                YOSO (NG IDX3 IDX 30)  -  YOSO (NG IDX3 IDX 29)
      *             COMPUTE   YOSO (NG IDX3 10 28)   =
      *                YOSO (NG IDX3 10 30)   -  YOSO (NG IDX3 10 29)
           COMPUTE   YOSO (NG IDX1 IDX 37)  =  YOSO (NG IDX1 IDX 39)
                                            -  YOSO (NG IDX1 IDX 38)
           COMPUTE   YOSO (NG IDX1 10 37)   =  YOSO (NG IDX1 10 39)
                                            -  YOSO (NG IDX1 10 38)
           COMPUTE   YOSO (NG IDX3 IDX 37)  =  YOSO (NG IDX3 IDX 39)
                                            -  YOSO (NG IDX3 IDX 38)
           COMPUTE   YOSO (NG IDX3 10 37)   =  YOSO (NG IDX3 10 39)
                                            -  YOSO (NG IDX3 10 38)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
      *** 20031021 変更 配列のインデックス
      *
      *  20030228 保険分金額の仕樣を変更
      *
      *** 20030410 追加 受診履歴情報により初診、再診の件数を取得する。
           IF   NG  =  2
      ***  外来の場合は初診、再診件数を取得する。
      *--- 2010/04/27 CHANGE START -------------------------------------
      *       PERFORM   900-JYURRK-READ-SEC
      *       PERFORM   320-JYURRK-CNT-SUB-SEC
             PERFORM  900-JYURRK-SELECT-SEC
             PERFORM  UNTIL  FLG-JYURRK  =  1
               IF   JYURRK-DENPNUM  NOT =  WRK-DENPNUM
                 PERFORM   320-JYURRK-CNT-SUB-SEC
               END-IF
               PERFORM  900-JYURRK-FETCH-SEC
             END-PERFORM
             PERFORM  900-JYURRK-CLOSE-SEC 
      *--- 2010/04/27 CHANGE END ---------------------------------------
           END-IF 
      *** 20030410 追加 受診履歴情報により初診、再診の件数を取得する。
      *
           .
       320-SYUNOU-CNT-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報集計サブ処理
      *****************************************************************
       320-JYURRK-CNT-SUB-SEC      SECTION.
      *
           MOVE   JYURRK-SRYYMD(7:2)     TO   IDX7G
           PERFORM  VARYING  IDX6  FROM 1  BY  1
                    UNTIL    IDX6  >  SRYKAMAX
      *** 20030410 変更
      ***             IF  SRYKA(IDX6) = JYURRK-SRYKA
             IF  (JYURRK-NYUGAIKBN  =  "1"
                 AND  SRYKA(1 IDX6)  =  JYURRK-SRYKA)
                 OR  (JYURRK-NYUGAIKBN  =  "2"
                 AND  SRYKA(2 IDX6)  =  JYURRK-SRYKA)
      *** 20030410 変更
      ***
               MOVE   JYURRK-NYUGAIKBN    TO   WRK-NYUGAIKBN
               MOVE   JYURRK-SRYKA        TO   WRK-SRYKA
               PERFORM   320-SRYKA-GROUPING-SEC
      * 20080326 H20年4月改正対応START tanaka
	           IF  WRK-PARA-PRTYM  <  "200804"
                 PERFORM  320-HOKEN-GROUPING-SEC
               ELSE
                 PERFORM  330-HOKEN-GROUPING-SEC
               END-IF
      * 20080326 H20年4月改正対応END
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                   MOVE JYURRK-HOSPID TO  ACCT-HOSPID
               MOVE   JYURRK-HOSPNUM      TO   ACCT-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
               MOVE   JYURRK-NYUGAIKBN    TO   ACCT-NYUGAIKBN
               MOVE   JYURRK-PTID         TO   ACCT-PTID
               MOVE   JYURRK-SRYKA        TO   ACCT-SRYKA
               MOVE   JYURRK-SRYYMD(1:6)  TO   ACCT-SRYYM
               MOVE   "11"                TO   ACCT-SRYKBN
      *=== 初診、再診、その他振分け修正 by Yoshikawa(2008/01/31) start
      *                   PERFORM 900-SRYACCT-READ-SEC
      *    診療会計情報SELECT(KEY=3)
               MOVE   SRYACCT-REC         TO   MCPDATA-REC
               MOVE   "DBSELECT"          TO   MCP-FUNC
               MOVE   "tbl_sryacct"       TO   MCP-TABLE
               MOVE   "key3"              TO   MCP-PATHNAME
               CALL   "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF   MCP-RC   =  ZERO
                 MOVE   "DBFETCH"         TO   MCP-FUNC
                 MOVE   "tbl_sryacct"     TO   MCP-TABLE
                 MOVE   "key3"            TO   MCP-PATHNAME
                 CALL   "ORCDBMAIN"       USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
                 IF   MCP-RC   =  ZERO
                   MOVE MCPDATA-REC       TO   SRYACCT-REC
                   MOVE ZERO              TO   FLG-SRYACCT
                 ELSE
                   MOVE    1              TO   FLG-SRYACCT
                   INITIALIZE             SRYACCT-REC
                 END-IF
               ELSE
                 MOVE    1                TO   FLG-SRYACCT
                 INITIALIZE               SRYACCT-REC
               END-IF
      *
      *    初診データ探索処理
               MOVE     ZERO              TO   FLG-INPUT
               PERFORM  UNTIL  FLG-SRYACCT  =  1
                 IF  ACCT-DAY(1 IDX7)  >  ZERO
      *    初診データが見つかったら入力フラグをオン
                   MOVE  1              TO  FLG-INPUT
                 END-IF
      *    診療会計情報FETCH
                 MOVE SRYACCT-REC       TO  MCPDATA-REC
                 MOVE "DBFETCH"         TO  MCP-FUNC
                 MOVE "tbl_sryacct"     TO  MCP-TABLE
                 MOVE "key3"            TO  MCP-PATHNAME
                 CALL "ORCDBMAIN"       USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                 IF   MCP-RC   =  ZERO
                   MOVE   MCPDATA-REC   TO   SRYACCT-REC
                   MOVE   ZERO          TO   FLG-SRYACCT
                 ELSE
                   MOVE   1             TO   FLG-SRYACCT
                   INITIALIZE           SRYACCT-REC
                 END-IF
               END-PERFORM
      *  診療会計情報CLOSE
               MOVE "DBCLOSECURSOR"     TO   MCP-FUNC
               MOVE "tbl_sryacct"       TO   MCP-TABLE
               MOVE "key3"              TO   MCP-PATHNAME
               CALL "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *  入力フラグがオンの場合、診療会計終了フラグを切る
               IF   FLG-INPUT   =   1
                 MOVE    ZERO           TO   FLG-SRYACCT
               END-IF
      *=== 初診、再診、その他振分け修正 by Yoshikawa(2008/01/31) end
      *--- 2010/04/27 ADD START ----------------------------------------
           COMPUTE  JYURRK-EDANUM  =  JYURRK-EDANUM  +  1
           MOVE    JYURRK-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_jyurrk"        TO   MCP-TABLE
           MOVE    "key22"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    ZERO              TO   FLG-JYURRK2
             MOVE    "DBFETCH"         TO   MCP-FUNC
             MOVE    "tbl_jyurrk"      TO   MCP-TABLE
             MOVE    "key22"           TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
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
      *--- 2010/07/23 ADD END   ----------------------------------------
      *--- 2010/07/06 ADD START ----------------------------------------
               IF  JYURRK-HKNKBN  =  "0"
      *--- 2010/07/06 ADD END   ----------------------------------------
      *
               IF   FLG-SRYACCT  =  ZERO
      *=== 初診、再診、その他振分け修正 by Yoshikawa(2008/01/31) start
      *                     AND   ACCT-DAY(1 IDX7)      >  ZERO
      *=== 初診、再診、その他振分け修正 by Yoshikawa(2008/01/31) end
                   AND   JYURRK-RENNUM   =  1
      *--- 2010/03/09 ADD START ----------------------------------------
                   AND   JYURRK-SRYKBN(1)  >  "00"
      *--- 2010/07/23 ADD START ----------------------------------------
                   AND   ACCT-SRYKBN  =  "11"
      *--- 2010/07/23 ADD END   ----------------------------------------
                   AND   (JYURRK-DOUJI-RENNUM  =  1  
                   OR   JYURRK-DOUJI-RENNUM  =  ZERO)  
      *--- 2010/03/09 ADD END ------------------------------------------
      *  初診の件数としてカウントする。
      *  20030410 変更
      ***                     ADD     1  TO  YOSO (IDX6 IDX 1)
      ***                     ADD     1  TO  YOSO (IDX6 10 1)
      ***                     ADD     1  TO  YOSO (IDX3 IDX 1)
      ***                     ADD     1  TO  YOSO (IDX3 10 1)
                 IF   JYURRK-NYUGAIKBN = 1
                   ADD     1            TO   YOSO (1 IDX6 IDX 1)
                   ADD     1            TO   YOSO (1 IDX6 10 1)
                   ADD     1            TO   YOSO (1 IDX3 IDX 1)
                   ADD     1            TO   YOSO (1 IDX3 10 1)
                 ELSE
                   ADD     1            TO   YOSO (2 IDX6 IDX 1)
                   ADD     1            TO   YOSO (2 IDX6 10 1)
                   ADD     1            TO   YOSO (2 IDX3 IDX 1)
                   ADD     1            TO   YOSO (2 IDX3 10 1)
                 END-IF
      *--- 2010/04/27 ADD START ----------------------------------------
                 MOVE    JYURRK-DENPNUM   TO   WRK-DENPNUM
      *--- 2010/04/27 ADD END ------------------------------------------
      *  20030410 変更
               ELSE
      *
      *--- 2010/04/27 ADD + DELETE START -------------------------------
      *=== 初診、再診、その他振分け修正 by Yoshikawa(2008/01/31) start
      *  受診履歴から再診探索用の剤番号を取得
      *           MOVE     2             TO   JYURRK-RENNUM
      *           MOVE    "DBSELECT"     TO   MCP-FUNC
      *           MOVE    "tbl_jyurrk"   TO   MCP-TABLE
      *           MOVE    "key5"         TO   MCP-PATHNAME
      *           CALL    "ORCDBMAIN"    USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
      *                                  SPA-AREA
      *           IF   MCP-RC  =   ZERO
      *             MOVE   ZERO           TO   FLG-JYURRK
      *             MOVE   JYURRK-ZAINUM1 TO   ACCT-ZAINUM
      *           ELSE
      *             MOVE   1              TO   FLG-JYURRK
      *           END-IF
      **  受診履歴情報CLOSE
      *           MOVE "DBCLOSECURSOR"   TO   MCP-FUNC
      *           MOVE "tbl_jyurrk"      TO   MCP-TABLE
      *           MOVE "key5"            TO   MCP-PATHNAME
      *           CALL "ORCDBMAIN"       USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
      *                                  SPA-AREA
      *=== 初診、再診、その他振分け修正 by Yoshikawa(2008/01/31) end
      *
      ***  20050914  MOTO
                 PERFORM  VARYING  IDX10 FROM  1  BY  1
                          UNTIL  IDX10  >  15
                   MOVE    JYURRK-ZAINUM(IDX10)  TO   ACCT-ZAINUM
      *--- 2010/04/27 ADD + DELETE END ---------------------------------
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                     MOVE JYURRK-HOSPID       TO  ACCT-HOSPID
                 MOVE    JYURRK-HOSPNUM      TO   ACCT-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
                 MOVE    JYURRK-NYUGAIKBN    TO   ACCT-NYUGAIKBN
                 MOVE    JYURRK-PTID         TO   ACCT-PTID
                 MOVE    JYURRK-SRYKA        TO   ACCT-SRYKA
                 MOVE    JYURRK-SRYYMD(1:6)  TO   ACCT-SRYYM
      *--- 2010/04/27 DELETE START -------------------------------------
      *           MOVE "12"  TO  ACCT-SRYKBN
      *--- 2010/04/27 DELETE END ---------------------------------------
      *=== 初診、再診、その他振分け修正 by Yoshikawa(2008/01/31) end
      *                     PERFORM 900-SRYACCT-READ-SEC
      *  診療会計情報SELECT(KEY=5)
                 MOVE    SRYACCT-REC         TO   MCPDATA-REC
                 MOVE    "DBSELECT"          TO   MCP-FUNC
                 MOVE    "tbl_sryacct"       TO   MCP-TABLE
                 MOVE    "key5"              TO   MCP-PATHNAME
                 CALL    "ORCDBMAIN"         USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
                 IF   MCP-RC   =  ZERO
                   MOVE    "DBFETCH"         TO   MCP-FUNC
                   MOVE    "tbl_sryacct"     TO   MCP-TABLE
                   MOVE    "key5"            TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"       USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
                   IF   MCP-RC   =  ZERO
                     MOVE    MCPDATA-REC     TO   SRYACCT-REC
                     MOVE    ZERO            TO   FLG-SRYACCT
                   ELSE
                     MOVE    1               TO   FLG-SRYACCT
                     INITIALIZE              SRYACCT-REC
                   END-IF
                 ELSE
                   MOVE    1                 TO   FLG-SRYACCT
                   INITIALIZE                SRYACCT-REC
                 END-IF
      *  診療会計情報CLOSE
                 MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
                 MOVE    "tbl_sryacct"       TO   MCP-TABLE
                 MOVE    "key5"              TO   MCP-PATHNAME
                 CALL    "ORCDBMAIN"         USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *=== 初診、再診、その他振分け修正 by Yoshikawa(2008/01/31) end
      *
      *--- 2010/04/27 ADD START ----------------------------------------
                  IF   IDX10  <  15
                    COMPUTE   IDX11  =  IDX10  +  1
                    IF   JYURRK-ZAINUM(IDX11)  =  ZERO
                      MOVE    16         TO   IDX10
                    END-IF
                  END-IF
                  IF   ACCT-SRYKBN   =  "12"
                    MOVE    16           TO   IDX10
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
                 IF   FLG-SRYACCT  =  ZERO
                     AND   ACCT-DAY(1 IDX7)  >  ZERO
                     AND   SRY-SRYKBN   =   "12"
                     AND   ACCT-SRYKBN   =  "12"
      *
                   IF   JYURRK-NYUGAIKBN  =  1
                     ADD    1            TO   YOSO(1 IDX6 IDX 2)
                     ADD    1            TO   YOSO(1 IDX6 10 2)
                     ADD    1            TO   YOSO(1 IDX3 IDX 2)
                     ADD    1            TO   YOSO(1 IDX3 10 2)
                   ELSE
                     ADD    1            TO   YOSO(2 IDX6 IDX 2)
                     ADD    1            TO   YOSO(2 IDX6 10 2)
                     ADD    1            TO   YOSO(2 IDX3 IDX 2)
                     ADD    1            TO   YOSO(2 IDX3 10 2)
                   END-IF
      *--- 2010/04/27 ADD START ----------------------------------------
                   MOVE    JYURRK-DENPNUM   TO   WRK-DENPNUM
      *--- 2010/04/27 ADD END ------------------------------------------
      *
                 ELSE
                   IF   FLG-JYURRK2  =  1
      *  その他の件数としてカウントする。
                     IF   JYURRK-NYUGAIKBN  =  1
                       ADD    1           TO   YOSO(1 IDX6 IDX 3)
                       ADD    1           TO   YOSO(1 IDX6 10 3)
                       ADD    1           TO   YOSO(1 IDX3 IDX 3)
                       ADD    1           TO   YOSO(1 IDX3 10 3)
                     ELSE
                       ADD    1           TO   YOSO(2 IDX6 IDX 3)
                       ADD    1           TO   YOSO(2 IDX6 10 3)
                       ADD    1           TO   YOSO(2 IDX3 IDX 3)
                       ADD    1           TO   YOSO(2 IDX3 10 3)
                     END-IF
                   END-IF
                 END-IF
      ***  20050914  MOTO
      *
               END-IF
      *--- 2010/07/06 ADD START ----------------------------------------
               ELSE
                   MOVE     2             TO   JYURRK-RENNUM
                   MOVE    "DBSELECT"     TO   MCP-FUNC
                   MOVE    "tbl_jyurrk"   TO   MCP-TABLE
                   MOVE    "key5"         TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"    USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
                   IF   MCP-RC  =   ZERO
                     MOVE   ZERO           TO   FLG-JYURRK
                     MOVE   JYURRK-ZAINUM1 TO   ACCT-ZAINUM
                   ELSE
                     MOVE   1              TO   FLG-JYURRK
                   END-IF
      *  受診履歴情報CLOSE
                   MOVE "DBCLOSECURSOR"   TO   MCP-FUNC
                   MOVE "tbl_jyurrk"      TO   MCP-TABLE
                   MOVE "key5"            TO   MCP-PATHNAME
                   CALL "ORCDBMAIN"       USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
                   MOVE JYURRK-HOSPNUM      TO   ACCT-HOSPNUM
                   MOVE JYURRK-NYUGAIKBN    TO   ACCT-NYUGAIKBN
                   MOVE JYURRK-PTID         TO   ACCT-PTID
                   MOVE JYURRK-SRYKA        TO   ACCT-SRYKA
                   MOVE JYURRK-SRYYMD(1:6)  TO   ACCT-SRYYM
                   MOVE "11"                TO   ACCT-SRYKBN
                   MOVE SRYACCT-REC         TO   MCPDATA-REC
                   MOVE "DBSELECT"          TO   MCP-FUNC
                   MOVE "tbl_sryacct"       TO   MCP-TABLE
                   MOVE "key5"              TO   MCP-PATHNAME
                   CALL "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
                   IF   MCP-RC   =  ZERO
                     MOVE "DBFETCH"         TO   MCP-FUNC
                     MOVE "tbl_sryacct"     TO   MCP-TABLE
                     MOVE "key5"            TO   MCP-PATHNAME
                     CALL "ORCDBMAIN"       USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
                     IF   MCP-RC   =  ZERO
                       MOVE MCPDATA-REC     TO   SRYACCT-REC
                       MOVE ZERO            TO   FLG-SRYACCT
                     ELSE
                       MOVE   1             TO   FLG-SRYACCT
                       INITIALIZE                SRYACCT-REC
                     END-IF
                   ELSE
                     MOVE    1              TO   FLG-SRYACCT
                     INITIALIZE                  SRYACCT-REC
                   END-IF
      *  診療会計情報CLOSE
                   MOVE "DBCLOSECURSOR"     TO   MCP-FUNC
                   MOVE "tbl_sryacct"       TO   MCP-TABLE
                   MOVE "key5"              TO   MCP-PATHNAME
                   CALL "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *
                 IF   FLG-SRYACCT  =  ZERO
                    AND   ACCT-DAY(1 IDX7)   >   ZERO
                    AND   JYURRK-SRYKBN (1)  >   "00"
                    AND   ACCT-SRYKBN        =   "11"
      *  初診の件数としてカウントする。
                   IF   JYURRK-NYUGAIKBN = 1
                     ADD     1            TO   YOSO (1 IDX6 IDX 1)
                     ADD     1            TO   YOSO (1 IDX6 10 1)
                     ADD     1            TO   YOSO (1 IDX3 IDX 1)
                     ADD     1            TO   YOSO (1 IDX3 10 1)
                   ELSE
                     ADD     1            TO   YOSO (2 IDX6 IDX 1)
                     ADD     1            TO   YOSO (2 IDX6 10 1)
                     ADD     1            TO   YOSO (2 IDX3 IDX 1)
                     ADD     1            TO   YOSO (2 IDX3 10 1)
                   END-IF
      *--- 2010/07/23 ADD START ----------------------------------------
                   MOVE    JYURRK-DENPNUM   TO   WRK-DENPNUM
      *--- 2010/07/23 ADD END   ----------------------------------------
                 ELSE
      *
      *  受診履歴から再診探索用の剤番号を取得
                   MOVE     2             TO   JYURRK-RENNUM
                   MOVE    "DBSELECT"     TO   MCP-FUNC
                   MOVE    "tbl_jyurrk"   TO   MCP-TABLE
                   MOVE    "key5"         TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"    USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
                   IF   MCP-RC  =   ZERO
                     MOVE   ZERO           TO   FLG-JYURRK
                     MOVE   JYURRK-ZAINUM1 TO   ACCT-ZAINUM
                   ELSE
                     MOVE   1              TO   FLG-JYURRK
                   END-IF
      *  受診履歴情報CLOSE
                   MOVE "DBCLOSECURSOR"   TO   MCP-FUNC
                   MOVE "tbl_jyurrk"      TO   MCP-TABLE
                   MOVE "key5"            TO   MCP-PATHNAME
                   CALL "ORCDBMAIN"       USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
                   MOVE JYURRK-HOSPNUM      TO   ACCT-HOSPNUM
                   MOVE JYURRK-NYUGAIKBN    TO   ACCT-NYUGAIKBN
                   MOVE JYURRK-PTID         TO   ACCT-PTID
                   MOVE JYURRK-SRYKA        TO   ACCT-SRYKA
                   MOVE JYURRK-SRYYMD(1:6)  TO   ACCT-SRYYM
                   MOVE "12"                TO   ACCT-SRYKBN
      *  診療会計情報SELECT(KEY=5)
                   MOVE SRYACCT-REC         TO   MCPDATA-REC
                   MOVE "DBSELECT"          TO   MCP-FUNC
                   MOVE "tbl_sryacct"       TO   MCP-TABLE
                   MOVE "key5"              TO   MCP-PATHNAME
                   CALL "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
                   IF   MCP-RC   =  ZERO
                     MOVE "DBFETCH"         TO   MCP-FUNC
                     MOVE "tbl_sryacct"     TO   MCP-TABLE
                     MOVE "key5"            TO   MCP-PATHNAME
                     CALL "ORCDBMAIN"       USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
                     IF   MCP-RC   =  ZERO
                       MOVE MCPDATA-REC     TO   SRYACCT-REC
                       MOVE ZERO            TO   FLG-SRYACCT
                     ELSE
                       MOVE   1             TO   FLG-SRYACCT
                       INITIALIZE                SRYACCT-REC
                     END-IF
                   ELSE
                     MOVE    1              TO   FLG-SRYACCT
                     INITIALIZE                  SRYACCT-REC
                   END-IF
      *  診療会計情報CLOSE
                   MOVE "DBCLOSECURSOR"     TO   MCP-FUNC
                   MOVE "tbl_sryacct"       TO   MCP-TABLE
                   MOVE "key5"              TO   MCP-PATHNAME
                   CALL "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *
                   IF   FLG-SRYACCT   =   ZERO
                       AND   ACCT-DAY(1 IDX7)  >  ZERO
                       AND   JYURRK-SRYKBN(1)  >  "00"
                       AND   ACCT-SRYKBN   =  "12"
      *
      *  再診の件数としてカウントする。
                     IF   JYURRK-NYUGAIKBN = 1
                       ADD     1            TO   YOSO (1 IDX6 IDX 2)
                       ADD     1            TO   YOSO (1 IDX6 10 2)
                       ADD     1            TO   YOSO (1 IDX3 IDX 2)
                       ADD     1            TO   YOSO (1 IDX3 10 2)
                     ELSE
                       ADD     1            TO   YOSO (2 IDX6 IDX 2)
                       ADD     1            TO   YOSO (2 IDX6 10 2)
                       ADD     1            TO   YOSO (2 IDX3 IDX 2)
                       ADD     1            TO   YOSO (2 IDX3 10 2)
                     END-IF
      *--- 2010/07/23 ADD START ----------------------------------------
                     MOVE    JYURRK-DENPNUM   TO   WRK-DENPNUM
      *--- 2010/07/23 ADD END   ----------------------------------------
                   ELSE
      *  その他の件数としてカウントする。
                     IF   JYURRK-NYUGAIKBN = 1
                       ADD     1            TO   YOSO (1 IDX6 IDX 3)
                       ADD     1            TO   YOSO (1 IDX6 10 3)
                       ADD     1            TO   YOSO (1 IDX3 IDX 3)
                       ADD     1            TO   YOSO (1 IDX3 10 3)
                     ELSE
                       ADD     1            TO   YOSO (2 IDX6 IDX 3)
                       ADD     1            TO   YOSO (2 IDX6 10 3)
                       ADD     1            TO   YOSO (2 IDX3 IDX 3)
                       ADD     1            TO   YOSO (2 IDX3 10 3)
                     END-IF
                   END-IF
      *
                 END-IF
               END-IF
      *--- 2010/07/06 ADD START ----------------------------------------
               MOVE     SRYKAMAX          TO   IDX6
             END-IF
           END-PERFORM
           .           
       320-JYURRK-CNT-SUB-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴情報集計処理
      *****************************************************************
       320-PTNYUINRRK-CNT-SEC      SECTION.
      *
           PERFORM  UNTIL      FLG-PTNYUINRRK  = 1
      * 20030704 追加 
             INITIALIZE                 PTINF-REC 
      *=== 4.0対応 By Takashima(2007/09/10) start
      *      MOVE PTNYUINRRK-HOSPID  TO PTINF-HOSPID
             MOVE   PTNYUINRRK-HOSPNUM  TO   PTINF-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
             MOVE   PTNYUINRRK-PTID     TO   PTINF-PTID
             PERFORM   900-PTINF-READ-SEC
      * 20030704 追加
      *
      *    集計対象年月のデータであるかをチェックする。 
             IF (( PTNYUINRRK-NYUINYMD(1:6)  =  WRK-PARA-PRTYM
                 AND  PTNYUINRRK-RRKEDANUM   =  1)
                 OR  ( PTNYUINRRK-TAIINYMD(1:6) =   WRK-PARA-PRTYM
                 AND  PTNYUINRRK-RRKEDANUM   =  1))
      ***  20030704 収納情報抽出条件変更
                 AND  FLG-PTINF   =   ZERO
                 AND  PTINF-TSTPTNUMKBN   NOT =  "1"
      * 患者情報にレコードが存在しないデータは抽出しない。
      * テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う
      ***  20030704 収納情報抽出条件変更
               MOVE    "1"                  TO   WRK-NYUGAIKBN
               MOVE    PTNYUINRRK-NYUINKA   TO   WRK-SRYKA
               PERFORM  320-SRYKA-GROUPING-SEC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *           MOVE     PTNYUINRRK-HOSPID  TO  COMB-HOSPID
               MOVE    PTNYUINRRK-HOSPNUM   TO   COMB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
               MOVE    PTNYUINRRK-PTID      TO   COMB-PTID
      ***  2005/07/14  MOTO
      ***           MOVE     0001               TO  COMB-HKNCOMBINUM
               MOVE    PTNYUINRRK-HKNCOMBINUM   TO   COMB-HKNCOMBINUM
      ***  2005/07/14  MOTO
               PERFORM   900-HKNCOMBI-READ-SEC
      * 20080326 H20年4月改正対応START tanaka
	           IF  WRK-PARA-PRTYM  <  "200804"
                 PERFORM  321-HOKEN-GROUPING-SEC
               ELSE
                 PERFORM  331-HOKEN-GROUPING-SEC
               END-IF
      * 20080326 H20年4月改正対応END
               IF  PTNYUINRRK-NYUINYMD(1:6)  =   WRK-PARA-PRTYM
                 ADD     1        TO   YOSO (1 IDX1 IDX 1)
                 ADD     1        TO   YOSO (1 IDX1 10 1)
                 ADD     1        TO   YOSO (1 IDX3 IDX 1)
                 ADD     1        TO   YOSO (1 IDX3 10 1)
               END-IF
               IF  PTNYUINRRK-TAIINYMD(1:6) =   WRK-PARA-PRTYM
                 ADD     1        TO   YOSO (1 IDX1 IDX 2)
                 ADD     1        TO   YOSO (1 IDX1 10 2)
                 ADD     1        TO   YOSO (1 IDX3 IDX 2)
                 ADD     1        TO   YOSO (1 IDX3 10 2)
               END-IF
             END-IF
             PERFORM   910-DBFETCH2-SEC
           END-PERFORM
      *
           .
       320-PTNYUINRRK-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科グループ判別処理
      *****************************************************************
       320-SRYKA-GROUPING-SEC      SECTION.
      *
           MOVE    1            TO   IDX1
           MOVE    SRYKAMAX     TO   IDX2 
           PERFORM  UNTIL  IDX1   >   IDX2
      *** 20030410 変更
      ***       IF   SRYKA(IDX1)  = WRK-SRYKA
             IF  (WRK-NYUGAIKBN   =  "1"
                 AND  SRYKA(1 IDX1)  =  WRK-SRYKA)
                 OR   (WRK-NYUGAIKBN   =  "2"
                 AND  SRYKA(2 IDX1)  =  WRK-SRYKA)
      *** 20030410 変更
               MOVE    ZERO     TO   IDX2
             ELSE
               ADD     1        TO   IDX1
             END-IF
           END-PERFORM
      *
           .
       320-SRYKA-GROUPING-EXT.
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
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    9               TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM    
               MOVE    8               TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
      *--- 2010/02/09 ADD START ----------------------------------------
             WHEN    "900"  THRU  "919"
      *--- 2010/02/09 ADD END ------------------------------------------
               MOVE    7               TO   IDX
      * 20080326 H20年4月改正対応START tanaka
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    11              TO   IDX
      * 20080326 H20年4月改正対応END
             WHEN    "060"
             WHEN    "067"
      *=== 特別療養費対応 By Uchida(2006/12/22) start
             WHEN    "068"
             WHEN    "069"
      *=== 特別療養費対応 By Uchida(2006/12/22) end
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    5             TO   IDX
               ELSE
                 MOVE    4             TO   IDX
               END-IF
             WHEN    OTHER
               IF   SYU-KOH1HKNNUM   =   "027"
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
      * 20030620追加
      *    保険別区分例外条件(自費条件拡張)
           IF   SYU-SYUHKNNUM(1:2)  =  "98"
             MOVE      7               TO   IDX
           END-IF
      * 20030620追加
      *
           .
       320-HOKEN-GROUPING-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険グループ判別処理2
      *****************************************************************
       321-HOKEN-GROUPING-SEC      SECTION.
      *
      *    保険別金額（社保本人〜自賠責）
           MOVE     ZERO               TO   IDX
           EVALUATE   COMB-HKNNUM
             WHEN    SPACE
               MOVE    6               TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    9               TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM    
               MOVE    8               TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
      *--- 2010/02/09 ADD START ----------------------------------------
             WHEN    "900"  THRU  "919"
      *--- 2010/02/09 ADD END ------------------------------------------
               MOVE    7               TO   IDX
      * 20080326 H20年4月改正対応START tanaka
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    11      TO   IDX
      * 20080326 H20年4月改正対応END
             WHEN    "060"
             WHEN    "067"
      *=== 特別療養費対応 By Uchida(2006/12/22) start
             WHEN    "068"
             WHEN    "069"
      *=== 特別療養費対応 By Uchida(2006/12/22) end
               IF   COMB-KOH1HKNNUM   =   "027"
                 MOVE    5             TO   IDX
               ELSE
                 MOVE    4             TO   IDX
               END-IF
             WHEN    OTHER
               IF   COMB-KOH1HKNNUM   =   "027"
                 MOVE    3             TO   IDX
               ELSE
                 PERFORM 3202-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1         TO   IDX
                   WHEN    "2"
                     MOVE    2         TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
      * 20030620追加
      * 保険別区分例外条件(自費条件拡張)
           IF   COMB-HKNNUM(1:2)  =  "98"
             MOVE     7        TO    IDX
           END-IF
      * 20030620追加
      *
           .
       321-HOKEN-GROUPING-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険グループ判別処理(H20年4月以降)
      *****************************************************************
       330-HOKEN-GROUPING-SEC      SECTION.
      *
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                TO   IDX 
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6               TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    9               TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    8               TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
      *--- 2010/02/09 ADD START ----------------------------------------
             WHEN    "900"  THRU  "919"
      *--- 2010/02/09 ADD END ------------------------------------------
               MOVE    7               TO   IDX
      * 20080326 H20年4月改正対応START tanaka
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    11              TO   IDX
             WHEN    "039"
             WHEN    "040"
               MOVE    5               TO   IDX
      * 20080326 H20年4月改正対応END
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF   SYU-KOH1HKNNUM   =   "027" 
                   MOVE    5           TO   IDX
               ELSE
                   MOVE    4           TO   IDX
               END-IF
             WHEN    OTHER
               IF   SYU-KOH1HKNNUM   =   "027" 
                 MOVE    5             TO   IDX
               ELSE
                 PERFORM   3201-PTHKNINF-HENSYU-SEC
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
             MOVE     7        TO   IDX
           END-IF
      *
           .
       320-HOKEN-GROUPING-EXT.
           EXIT.
      * 20080326 H20年4月改正対応START tanaka
      *****************************************************************
      *    保険グループ判別処理2(H20年4月以降)
      *****************************************************************
       331-HOKEN-GROUPING-SEC      SECTION.
      *
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                TO  IDX 
           EVALUATE    COMB-HKNNUM
             WHEN    SPACE
               MOVE    6                   TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
              MOVE     9                   TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    8                   TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
      *--- 2010/02/09 ADD START ----------------------------------------
             WHEN    "900"  THRU  "919"
      *--- 2010/02/09 ADD END ------------------------------------------
               MOVE    7                   TO   IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    11                  TO   IDX
             WHEN    "039"
             WHEN    "040"
               MOVE    5                   TO   IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF   COMB-KOH1HKNNUM   =   "027" 
                 MOVE    5                 TO   IDX
               ELSE
                 MOVE    4                 TO   IDX
               END-IF
             WHEN    OTHER
               IF   COMB-KOH1HKNNUM   =   "027" 
                 MOVE    5                 TO   IDX
               ELSE
                 PERFORM 3202-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1             TO   IDX
                   WHEN    "2"
                     MOVE    2             TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
           IF   COMB-HKNNUM(1:2) = "98"
             MOVE    7    TO      IDX
           END-IF
      *
           .
       321-HOKEN-GROUPING-EXT.
           EXIT.
      * 20080326 H20年4月改正対応END
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
           PERFORM   900-HKNCOMBI-READ-SEC
           IF   FLG-HKNCOMBI   =   ZERO
      *    患者保険情報読み込み
             INITIALIZE                    PTHKNINF-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    SYU-HOSPID        TO  PTHKN-HOSPID
             MOVE    SYU-HOSPNUM      TO   PTHKN-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
             MOVE    SYU-PTID         TO   PTHKN-PTID
             MOVE    COMB-HKNID       TO   PTHKN-HKNID
             PERFORM   900-PTHKNINF-READ-SEC
           END-IF    
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3202-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *    患者保険情報読み込み
           INITIALIZE                    PTHKNINF-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *      MOVE    PTNYUINRRK-HOSPID TO  PTHKN-HOSPID
           MOVE    PTNYUINRRK-HOSPNUM  TO   PTHKN-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    PTNYUINRRK-PTID     TO   PTHKN-PTID
           MOVE    COMB-HKNID          TO   PTHKN-HKNID
           PERFORM   900-PTHKNINF-READ-SEC
           .
       3202-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
      *    
       330-TERM-HEN-SEC         SECTION.
      *
      *  2003/06/24 追加
           IF   WRK-SYS-BEDSU     >  ZERO
               OR  WRK-SYS-BEDSUIPN  >  ZERO
      **** 病床数が１つ以上ある場合は入院のデータも印字する。
             MOVE    1      TO    NG
           ELSE
      **** 病床数がゼロの場合は外来のデータのみ印字する。
             MOVE    2      TO    NG
           END-IF
      *  2003/06/24 追加
      *
           PERFORM    UNTIL   NG   >   2
             PERFORM    VARYING  IDX1    FROM  1   BY  1
                        UNTIL   IDX1  >  SRYKAMAX
      ** 2005/02/15 SUZUKI
      * 帳票パラメタ診療科,入外区分の追加
               IF   SRYKA(NG IDX1)  =  WRK-PARA-SRYKA
                   OR   WRK-PARA-SRYKA  =  SPACE
                   OR   FLG-SRYKAGK   =  1
      *** パラメタ診療科に”00”が入力された場合は合計項のみ印刷する
                 IF   FLG-SRYKAGK   =  1
                   MOVE    SRYKAMAX        TO   IDX1
                 END-IF
      *** パラメタ入外区分の追加。
                 IF   WRK-SYS-NYUGAIKBN    =   NG
                     OR   WRK-PARA-NYUGAIKBN   =   SPACE
      ** 2005/02/15 ここまで
                   INITIALIZE       HC90
      *
                   MOVE    "対象年月"        TO   HC90-MIDASI
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
                   MOVE    "対象年月"        TO   HC901-MIDASI
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
                   INSPECT    WRK-PARA-DENPPRTYMDWH
                              REPLACING  CHARACTERS  BY " "
                              AFTER  "月"
                   MOVE    WRK-PARA-DENPPRTYMDWH  TO   HC90-SRYYMD
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
                   MOVE    WRK-PARA-DENPPRTYMDWH  TO   HC901-SRYYMD
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
                   MOVE    WRK-SYSYMDWH           TO   HC90-SYSYMD
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
                   MOVE    WRK-SYSYMDWH           TO   HC901-SYSYMD
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
      *
      ***   20030410  追加
                   MOVE    NYUGAINAME(NG)         TO   HC90-NYUGAIKBN
      ***   20030410  追加
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
                   MOVE   NYUGAINAME(NG)          TO   HC901-NYUGAIKBN
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
      *
      ***   20030410  変更
      ***         MOVE    SRYKANAME(2 IDX1)   TO  HC90-TSRYKA
                   IF   NYUGAIKBN(NG) = "1"
                     MOVE   SRYKANAME(1 IDX1)     TO   HC90-TSRYKA
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
                     MOVE   SRYKANAME(1 IDX1)     TO   HC901-TSRYKA
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
                   ELSE
                     MOVE   SRYKANAME(2 IDX1)     TO   HC90-TSRYKA
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
                     MOVE   SRYKANAME(2 IDX1)     TO   HC901-TSRYKA
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
                   END-IF
      ***   20030410  変更
      *
      ***   20030410  変更
      ***         PERFORM  VARYING  IDX2   FROM 1  BY  1
      ***                  UNTIL    IDX2   >         5 
      ***            MOVE   WRK-TJIHI(IDX2) TO  HC90-TJIHI(IDX2)
      ***         END-PERFORM
                   IF   NYUGAIKBN(NG)  =  "1"
      ***   20031021  追加(注意書メッセージ設定)
      ***   注意書メッセージの設定
                     MOVE  SPACE            TO   HC90-CMSG1
      ***   20031021  追加(注意書メッセージ設定)
      ***   入院の件数タイトル設定
                     MOVE  "入院件数"       TO   HC90-TKENSU1
                     MOVE  "退院件数"       TO   HC90-TKENSU2
      ***   入院の入院料保険点数タイトル設定
      *--- 2010/09/09 DELETE START -------------------------------------
      *               MOVE  "入院料(点数)"   TO   HC90-THKN1
      *--- 2010/09/09 DELETE END ---------------------------------------
      ***   入院の負担金タイトル設定
      *           MOVE  "食事療養費"     TO  HC90-TFUTAN1
      *           MOVE  "食事負担額"     TO  HC90-TFUTAN2
                     MOVE  "食事療養費・生活療養費"   TO   HC90-TFUTAN1
                     MOVE  "食事負担額・生活負担額"   TO   HC90-TFUTAN2

                     MOVE  "室料差額"       TO   HC90-TFUTAN3
      ***   入院の自費タイトル設定
                     PERFORM  VARYING  IDX2   FROM 1  BY  1
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *                    UNTIL    IDX2   >    5 
                              UNTIL    IDX2   >    10 
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
                       MOVE   WRK-TJIHIN(IDX2)   TO   HC90-TJIHI(IDX2)
                     END-PERFORM
                   ELSE
      ***   20031021  追加(注意書メッセージ設定)
      ***   注意書メッセージの設定
                     MOVE  "※(注１) 同日再診も１件毎にカウントする。"
                                                 TO   HC90-CMSG1
      ***   20031021  追加(注意書メッセージ設定)
      ***   外来の件数タイトル設定
                     MOVE  "初診件数"           TO   HC90-TKENSU1
                     MOVE  "再診件数 (注１)"    TO   HC90-TKENSU2
      * 20050914 MOTO
                     MOVE  "その他件数 (注１)"  TO   HC90-TKENSU3
      * 20050914 MOTO
      ***   外来の負担金タイトル設定
      ***   20031021  削除(薬剤一部負担金廃止に伴い未印字扱いにする。)
      ***           MOVE  "薬剤一部負担金" TO  HC90-TFUTAN3
      ***   20031021  削除(薬剤一部負担金廃止に伴い未印字扱いにする。)
      ***   外来の自費タイトル設定
                     PERFORM  VARYING  IDX2   FROM 1  BY  1
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *                    UNTIL    IDX2   >    5 
                              UNTIL    IDX2   >   10 
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
                       MOVE  WRK-TJIHI(IDX2)   TO   HC90-TJIHI(IDX2)
                     END-PERFORM
                   END-IF
      *--- 2010/09/09 ADD START ----------------------------------------
                   MOVE    "入院料(点数)"   TO   HC90-THKN1
      *--- 2010/09/09 ADD END ------------------------------------------
      ***   20030410  変更
      *
                   ADD     1            TO   CNT-PAGE
                   MOVE    CNT-PAGE     TO   WRK-PAGE
                   MOVE    WRK-PAGE     TO   HC90-PAGE
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
                   ADD     1            TO   CNT-PAGE
                   MOVE    CNT-PAGE     TO   WRK-PAGE
                   MOVE    WRK-PAGE     TO   HC901-PAGE
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
      *
                   PERFORM  VARYING  IDX2   FROM  1 BY 1
      * 20080326 H20年4月改正対応START tanaka
      *             UNTIL    IDX2  >        10   
                            UNTIL    IDX2  >  11
      * 20080326 H20年4月改正対応END
                     PERFORM   VARYING  IDX3   FROM  1 BY 1
      ***   20031021 変更(印刷行数の増加に伴い変更)
      ***               UNTIL    IDX3  >        28
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *               UNTIL    IDX3  >        30
      * 20080326 H20年4月改正対応START tanaka
      *               UNTIL    IDX3  >        39
                               UNTIL    IDX3   >  40
      * 20080326 H20年4月改正対応END
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
                       IF  YOSO(NG IDX1 IDX2 IDX3)  >  0 
                           AND  IDX3  =  19
                         MOVE     "療養担当手当"    TO   HC90-TRYO
                       END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
      ***   20031021 変更(印刷行数の増加に伴い変更)
      *   予備(空き)行以外は数値を転送する。
                       EVALUATE  NYUGAIKBN(NG)
                         WHEN "1"
      *** 入院集計情報のデータ転送
                           IF   IDX3   NOT =   3
                               AND  IDX3   NOT =   4
      *   自費行は有効である場合に数値を転送する。
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *                 IF  IDX3 = 21
      *                  OR IDX3 = 22
      *                  OR IDX3 = 23
      *                  OR IDX3 = 24
      *                  OR IDX3 = 25
      *                     COMPUTE IDX8 = IDX3 - 20 
                             IF  IDX3 = 25
                                 OR   IDX3  =  26
                                 OR   IDX3  =  27
                                 OR   IDX3  =  28
                                 OR   IDX3  =  29
                                 OR   IDX3  =  30
                                 OR   IDX3  =  31
                                 OR   IDX3  =  32
                                 OR   IDX3  =  33
                                 OR   IDX3  =  34
                               COMPUTE   IDX8   =   IDX3   -   24 
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *                     IF WRK-TJIHIN-FLG(IDX8) = 1
      * 20080813 自費名称が未登録の時も値を印字する修正END
                               MOVE YOSO(NG IDX1 IDX2 IDX3)  TO   WRK-S9
                               MOVE WRK-S9   TO   HC90-YOSO(IDX2 IDX3)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *                     END-IF
      * 2 0080813 自費名称が未登録の時も値を印字する修正END
                             ELSE
      *   自費行以外の場合は数値を転送する。
                               MOVE YOSO(NG IDX1 IDX2 IDX3)  TO  WRK-S9
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *                       MOVE WRK-S9
      *                       TO   HC90-YOSO(IDX2 IDX3)
                               IF  IDX3  >  34
      * 20080326 H20年4月改正対応START tanaka
                                 IF   IDX3  =  40
                                   COMPUTE   IDX9   =   IDX3   -   5 
                                   MOVE  WRK-S9 TO HC90-YOSO(IDX2 IDX9)
                                 ELSE
                                   COMPUTE IDX9 = IDX3 - 34 
                                   MOVE  WRK-S9 TO HC901-YOSO(IDX2 IDX9)
                                 END-IF
      * 20080326 H20年4月改正対応END
                               ELSE
                                 MOVE WRK-S9  TO   HC90-YOSO(IDX2 IDX3)
                               END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
                             END-IF
                           END-IF 
                         WHEN "2"
      *** 外来集計情報のデータ転送
      * 20050914 MOTO
      ***               IF  IDX3 NOT = 3   AND
      * 20050914 MOTO
                           IF  IDX3   NOT = 4
                               AND  IDX3   NOT =   22
                               AND  IDX3   NOT =   23
                               AND  IDX3   NOT =   24
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *                   IDX3 NOT = 15  AND
      *                   IDX3 NOT = 18  AND  
      *                   IDX3 NOT = 19  AND  
      ****   20031021  追加(薬剤一部負担金廃止に伴い未印字扱いにする。)
      *                   IDX3 NOT = 20  
      ****   20031021  追加(薬剤一部負担金廃止に伴い未印字扱いにする。)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
      *   自費行は有効である場合に数値を転送する。
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *                 IF  IDX3 = 21
      *                  OR IDX3 = 22
      *                  OR IDX3 = 23
      *                  OR IDX3 = 24
      *                  OR IDX3 = 25
      *                     COMPUTE IDX8 = IDX3 - 20 
                             IF  IDX3  =  25
                                 OR  IDX3  =  26
                                 OR  IDX3  =  27
                                 OR  IDX3  =  28
                                 OR  IDX3  =  29
                                 OR  IDX3  =  30
                                 OR  IDX3  =  31
                                 OR  IDX3  =  32
                                 OR  IDX3  =  33
                                 OR  IDX3  =  34
                               COMPUTE  IDX8  =  IDX3  -  24 
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *                     IF WRK-TJIHI-FLG(IDX8) = 1
      * 20080813 自費名称が未登録の時も値を印字する修正END
                               MOVE   YOSO(NG IDX1 IDX2 IDX3) TO  WRK-S9
                               MOVE   WRK-S9   TO   HC90-YOSO(IDX2 IDX3)
      * 20080813 自費名称が未登録の時も値を印字する修正START
      *                     END-IF
      * 20080813 自費名称が未登録の時も値を印字する修正END
                             ELSE
      *   自費行以外の場合は数値を転送する。
                               MOVE   YOSO(NG IDX1 IDX2 IDX3) TO  WRK-S9
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      *                       MOVE WRK-S9
      *                       TO   HC90-YOSO(IDX2 IDX3)
                               IF  IDX3  >  34
      * 20080326 H20年4月改正対応START tanaka
                                 IF  IDX3  =  40
                                   COMPUTE   IDX9   =   IDX3   -   5
                                   MOVE  WRK-S9 TO  HC90-YOSO(IDX2 IDX9)
                                 ELSE
                                   COMPUTE   IDX9   =   IDX3   -   34
                                   MOVE  WRK-S9 TO HC901-YOSO(IDX2 IDX9)
                                 END-IF
      * 20080326 H20年4月改正対応END
                               ELSE
                                 MOVE  WRK-S9  TO  HC90-YOSO(IDX2 IDX3)
                               END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
                             END-IF
                           END-IF
      **** 20050228
                       END-EVALUATE
      **** 20050228
                     END-PERFORM
                   END-PERFORM
      *
                   INITIALIZE                      ORCSPRTAREA
                   MOVE    "INS"                   TO   SPRT-MODE
                   MOVE    LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
                   MOVE    LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
                   MOVE    LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
                   MOVE    LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
                   MOVE    LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
                   MOVE    LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
                   MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                                  TO   SPRT-SHORI-RENNUM
                   MOVE    LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
      ***** 03/09/29
      *         MOVE   "ORCBM500"   TO  SPRT-PRTID
      ***  20040914 MOTOHASI MONPEに変更
      ***         MOVE   "A00000M500" TO  SPRT-PRTID
      * 20080326 H20年4月改正対応START tanaka
      *         MOVE   "A00000M500N.red" TO  SPRT-PRTID
	               IF    WRK-PARA-PRTYM  <  "200804"
                     MOVE    "A00000M500O.red"     TO   SPRT-PRTID
                   ELSE
                     MOVE    "A00000M500N.red"     TO   SPRT-PRTID
                   END-IF
      * 20080326 H20年4月改正対応END
      ***  20040914 MOTOHASI MONPEに変更
      *****
      * 20031211 帳票タイトル変更 
      ***         MOVE    "月計表"          TO  SPRT-TITLE
                   MOVE    "保険別診療点数 月計表"   TO   SPRT-TITLE
      * 20031211 帳票タイトル変更 
                   MOVE    HC90                  TO   SPRT-PRTDATA
                   MOVE    LNK-PRTKANRI-TERMID   TO   SPRT-TERMID
                   MOVE    LNK-PRTKANRI-OPID     TO   SPRT-OPID
                   MOVE    LNK-PRTKANRI-PRTNM    TO   SPRT-PRTNM
      ***  20030814 削除
      ***  site-libを実行環境にする為コメントアウト
      ***         MOVE    "1"          TO  SPRT-SITEKBN
      ***  20030814 削除
                   CALL    "ORCSPRT"             USING
                                                 ORCSPRTAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
                                                 SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
                   IF   SPRT-RETURN   =   ZERO
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) start
      * 20080326 H20年4月改正対応START tanaka
      *             MOVE   "A00000M5001.red" TO  SPRT-PRTID
	                 IF  WRK-PARA-PRTYM  <  "200804"
                       MOVE   "A00000M5001O.red"   TO   SPRT-PRTID
	                 ELSE
                       MOVE   "A00000M5001N.red"   TO   SPRT-PRTID
	                 END-IF
      * 20080326 H20年4月改正対応END
                     MOVE    "保険別診療点数 月計表"
                                                   TO   SPRT-TITLE
                     MOVE    HC901                 TO   SPRT-PRTDATA
                     MOVE    LNK-PRTKANRI-TERMID   TO   SPRT-TERMID
                     MOVE    LNK-PRTKANRI-OPID     TO   SPRT-OPID
                     MOVE    LNK-PRTKANRI-PRTNM    TO   SPRT-PRTNM
                     CALL    "ORCSPRT"             USING
                                                   ORCSPRTAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
                                                    SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
                     IF   SPRT-RETURN   =   ZERO
                       CONTINUE
                     ELSE
                       MOVE    "印刷ＤＢに更新できませんでした"
                                           TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                     END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/10/30) end
                     CONTINUE
                   ELSE
                     MOVE    "印刷ＤＢに更新できませんでした"
                                        TO  WRK-RECEERR
                     PERFORM 500-ERR-HENSYU-SEC
                   END-IF
      * 20030620 追加
      * 診療科が単科の場合は合計頁のみ印刷する.
                   IF   SRYKAMAX   =   2
                     MOVE    2       TO      IDX1
                   END-IF
      * 20030620 追加
      * 
      * DEBUG時使用 作業の為最初の診療科のみ印刷
      ***       MOVE 999  TO IDX1
      * DEBUG時使用 作業の為最初の診療科のみ印刷
      * 診療科、入外区分判別終わり
                 END-IF
               END-IF
             END-PERFORM
      *
      ***    入外用インデックスカウントアップ 
             ADD    1      TO    NG
      	   END-PERFORM
      *
           .
       330-TERM-HEN-EXT.
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
             OPEN    OUTPUT           RECEERR-FILE
      *
             MOVE    WRK-RECEERR      TO   RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ管理開始処理
             MOVE    "JBE"            TO   SJOBKANRI-MODE
             INITIALIZE                    JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
             MOVE    SPA-HOSPNUM      TO   JOB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
             MOVE    WRK-PARA-JOBID   TO   JOB-JOBID
             MOVE    WRK-PARA-SHELLID TO   JOB-SHELLID
             MOVE    WRK-RECEERR      TO   JOB-YOBI
             MOVE    "9999"           TO   JOB-ERRCD
             CALL    "ORCSJOB"        USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                      SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           END-IF
      *
           MOVE      1                TO   FLG-END
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
           MOVE    "STE"           TO   SJOBKANRI-MODE
           INITIALIZE                   JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM     TO   JOB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-JOBID  TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO   JOB-SHELLID
           MOVE    CNT-PAGE        TO   JOB-UPDCNT
           CALL    "ORCSJOB"       USING
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
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD            TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO   WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       910-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  SYUNOU-REC
      *
      ***  2005/04/07  MOTO  日レセ Ver2.5.0 KEY変更
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    WRK-PARA-HOSPID  TO   SYU-HOSPID
           MOVE    WRK-PARA-HOSPNUM     TO   SYU-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           EVALUATE WRK-PARA-NYUGAIKBN
	         WHEN  "1"
               MOVE    "1"              TO   SYU-NYUGAIKBN
             WHEN  "2"
               MOVE    "2"              TO   SYU-NYUGAIKBN
	         WHEN  SPACE
               MOVE    "%"              TO   SYU-NYUGAIKBN
           END-EVALUATE
           MOVE    WRK-PARA-PRTYM       TO   SYU-SRYYMD
           MOVE    "01"                 TO   SYU-SRYYMD(7:2)
           MOVE    WRK-PARA-PRTLYMD     TO   SYU-UPSRYYMD
      ***  2005/04/07  MOTO  日レセ Ver2.5.0 KEY変更
      *
           MOVE    SYUNOU-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
      ***  2005/04/07  MOTO  日レセ Ver2.5.0 KEY変更
      ***     MOVE    "SYUNOU-ALL"    TO  ORC-DBPATH
      ***     CALL    "ORCMCPSUB"          USING
      ***                                  MCPAREA
      ***                                  ORCMCPAREA
      ***                                  MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-ST-KEY1    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "st_key1"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      ***  2005/04/07  MOTO  日レセ Ver2.5.0 KEY変更
           IF   MCP-RC   =   ZERO
             PERFORM   910-DBFETCH-SEC
           ELSE
             MOVE      1           TO   FLG-SYUNOU
      ***       MOVE    1            TO   FLG-END
             PERFORM   910-DBCLOSE-SEC
           END-IF
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納情報検索
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
      ***  2005/04/07  MOTO  日レセ Ver2.5.0 KEY変更
      ***     MOVE    "SYUNOU-ALL"     TO  ORC-DBPATH
      ***     CALL    "ORCMCPSUB"          USING
      ***                                  MCPAREA
      ***                                  ORCMCPAREA
      ***                                  MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-ST-KEY1    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "st_key1"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      ***  2005/04/07  MOTO  日レセ Ver2.5.0 KEY変更
           IF   MCP-RC   =   ZERO
             MOVE    ZERO               TO   FLG-SYUNOU
             MOVE    MCPDATA-REC        TO   SYUNOU-REC
           ELSE
             MOVE    1                  TO   FLG-SYUNOU
      ***       MOVE    1                            TO  FLG-END
             PERFORM   910-DBCLOSE-SEC
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
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      ***  2005/04/07  MOTO  日レセ Ver2.5.0 KEY変更
      ***     MOVE    "SYUNOU-ALL"     TO  ORC-DBPATH
      ***     CALL    "ORCMCPSUB"          USING
      ***                                  MCPAREA
      ***                                  ORCMCPAREA
      ***                                  MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-ST-KEY1    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "st_key1"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      ***  2005/04/07  MOTO  日レセ Ver2.5.0 KEY変更
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴情報検索
      *****************************************************************
       910-DBSELECT2-SEC                 SECTION.
      *
           INITIALIZE                  PTNYUINRRK-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    WRK-PARA-HOSPID     TO  PTNYUINRRK-HOSPID
           MOVE    WRK-PARA-HOSPNUM    TO   PTNYUINRRK-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-PRTLYMD    TO   PTNYUINRRK-NYUINYMD
           MOVE    WRK-PARA-PRTYM      TO   PTNYUINRRK-TAIINYMD(1:6)
           MOVE    "00"                TO   PTNYUINRRK-TAIINYMD(7:2)
      *
           MOVE    PTNYUINRRK-REC      TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTNYUINRRK-KEY14    TO  MCP-PATH
      *     CALL     "MCPSUB"               USING
           MOVE    "tbl_ptnyuinrrk"    TO   MCP-TABLE
           MOVE    "key14"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF    MCP-RC   =   ZERO
             PERFORM   910-DBFETCH2-SEC
           ELSE
             MOVE     1           TO    FLG-PTNYUINRRK
             MOVE     1           TO    FLG-END
             PERFORM   910-DBCLOSE2-SEC
           END-IF        
           .
       910-DBSELECT2-EXT.
           EXIT.
      *
      *****************************************************************
      *     入院履歴情報検索
      *****************************************************************
       910-DBFETCH2-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTNYUINRRK-KEY14    TO  MCP-PATH
      *     CALL    "MCPSUB"            USING
           MOVE    "tbl_ptnyuinrrk"     TO   MCP-TABLE
           MOVE    "key14"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF   MCP-RC   =   ZERO
             MOVE    ZERO               TO   FLG-PTNYUINRRK
             MOVE    MCPDATA-REC        TO   PTNYUINRRK-REC
           ELSE
             MOVE    1                  TO   FLG-PTNYUINRRK
             MOVE    1                  TO   FLG-END
             PERFORM   910-DBCLOSE2-SEC
           END-IF
           .
       910-DBFETCH2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       910-DBCLOSE2-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE  "DBCLOSE"          TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTNYUINRRK-KEY14    TO  MCP-PATH
      *     CALL  "MCPSUB"               USING
           MOVE    "tbl_ptnyuinrrk"   TO   MCP-TABLE
           MOVE    "key14"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                      MCPAREA
                                      MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                      SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           .
       910-DBCLOSE2-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
      ***     MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"            USING
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
             MOVE    "DBFETCH"         TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *       CALL    "MCPSUB"             USING
             MOVE    "tbl_syskanri"    TO   MCP-TABLE
             MOVE    "key10"           TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =   ZERO
               MOVE      ZERO          TO   FLG-SYSKANRI
             ELSE
               MOVE      1             TO   FLG-SYSKANRI
             END-IF
           ELSE
             MOVE      1               TO   FLG-SYSKANRI
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"      TO   MCP-TABLE
           MOVE    "key10"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
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
      *    管理マスタ読み込み(自費情報)
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
      *    管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
      ***     MOVE    WRK-SYSYMD            TO  ORC-DBYMD
           MOVE    SYS-1005-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY2    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF  MCP-RC   =   ZERO
             PERFORM   820-SYSKANRI-DBFETCH-SEC
           ELSE
             MOVE      1                TO    FLG-SYSKANRI3
             PERFORM   820-SYSKANRI-DBCLOSE-SEC
           END-IF
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY2    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF  MCP-RC   =   ZERO
             MOVE    ZERO               TO   FLG-SYSKANRI3
             MOVE    MCPDATA-REC        TO   SYS-1005-REC
           ELSE
             MOVE    1                  TO   FLG-SYSKANRI3
             PERFORM   820-SYSKANRI-DBCLOSE-SEC
           END-IF
           .
       820-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       820-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"                 TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY2    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC            TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptinf"          TO   MCP-TABLE
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
      *         MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_ptinf"        TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO   PTINF-REC
               MOVE    ZERO             TO   FLG-PTINF
             ELSE
               MOVE    1                TO   FLG-PTINF
               INITIALIZE               PTINF-REC
             END-IF
           ELSE
             MOVE    1                  TO   FLG-PTINF
             INITIALIZE                 PTINF-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptinf"          TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスター読込処理
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
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
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-PTHKNINF-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_pthkninf"    TO   MCP-TABLE
             MOVE    "key"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC     TO   PTHKNINF-REC
               MOVE    ZERO            TO   FLG-PTHKNINF
             ELSE
               MOVE    1               TO   FLG-PTHKNINF
               INITIALIZE              PTHKNINF-REC
             END-IF
           ELSE
             MOVE    1                 TO   FLG-PTHKNINF
             INITIALIZE                PTHKNINF-REC
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
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *--- 2010/04/27 CHANGE START -------------------------------------
      *****************************************************************
      *    受診履歴情報読込処理
      *****************************************************************
      * 900-JYURRK-READ-SEC         SECTION.
      **
      *     INITIALIZE               JYURRK-REC
      **=== 4.0対応 By Takashima(2007/09/10) start
      **     MOVE    SYU-HOSPID     TO  JYURRK-HOSPID
      *     MOVE    SYU-HOSPNUM      TO   JYURRK-HOSPNUM
      **=== 4.0対応 By Takashima(2007/09/10) end
      *     MOVE    SYU-PTID         TO   JYURRK-PTID
      *     MOVE    SYU-NYUGAIKBN    TO   JYURRK-NYUGAIKBN
      *     MOVE    SYU-DENPNUM      TO   JYURRK-DENPNUM
      **
      *     MOVE    JYURRK-REC       TO   MCPDATA-REC
      *     MOVE    "DBSELECT"       TO   MCP-FUNC
      **=== 4.0対応 By Takashima(2007/09/10) start
      **     MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      **     CALL    "MCPSUB"           USING
      *     MOVE    "tbl_jyurrk"     TO   MCP-TABLE
      *     MOVE    "key6"           TO   MCP-PATHNAME
      *     CALL    "ORCDBMAIN"      USING
      **=== 4.0対応 By Takashima(2007/09/10) end
      *                              MCPAREA
      *                              MCPDATA-REC
      **=== 4.0対応 By Takashima(2007/09/10) start
      *                              SPA-AREA
      **=== 4.0対応 By Takashima(2007/09/10) end
      *     IF   MCP-RC   =   ZERO
      *       MOVE    "DBFETCH"      TO   MCP-FUNC
      **=== 4.0対応 By Takashima(2007/09/10) start
      **         MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      **         CALL    "MCPSUB"           USING
      *       MOVE    "tbl_jyurrk"   TO   MCP-TABLE
      *       MOVE    "key6"         TO   MCP-PATHNAME
      *       CALL    "ORCDBMAIN"    USING
      **=== 4.0対応 By Takashima(2007/09/10) end
      *                              MCPAREA
      *                              MCPDATA-REC
      **=== 4.0対応 By Takashima(2007/09/10) start
      *                              SPA-AREA
      **=== 4.0対応 By Takashima(2007/09/10) end
      *       IF   MCP-RC   =   ZERO
      *         MOVE    MCPDATA-REC  TO   JYURRK-REC
      *         MOVE    ZERO         TO   FLG-JYURRK
      *       ELSE
      *         MOVE    1            TO   FLG-JYURRK
      *         INITIALIZE           JYURRK-REC
      *       END-IF
      *     ELSE
      *       MOVE    1              TO   FLG-JYURRK
      *       INITIALIZE             JYURRK-REC
      *     END-IF
      **
      **=== sarge対応 UP By Takashima(2006/08/01) start
      **     MOVE    "DBCLOSE"           TO  MCP-FUNC
      *     MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
      **=== sarge対応 UP By Takashima(2006/08/01) end
      **=== 4.0対応 By Takashima(2007/09/10) start
      **     MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      **     CALL    "MCPSUB"             USING
      *     MOVE    "tbl_jyurrk"         TO   MCP-TABLE
      *     MOVE    "key6"               TO   MCP-PATHNAME
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
      **      
      ******************************************************************
      *    受診履歴SELECT
      ******************************************************************
       900-JYURRK-SELECT-SEC                 SECTION.
      
           INITIALIZE                  JYURRK-REC
           MOVE    SYU-HOSPNUM         TO   JYURRK-HOSPNUM
           MOVE    SYU-PTID            TO   JYURRK-PTID
           MOVE    SYU-NYUGAIKBN       TO   JYURRK-NYUGAIKBN
           MOVE    SYU-DENPNUM         TO   JYURRK-DENPNUM
           MOVE    JYURRK-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_jyurrk"        TO   MCP-TABLE
           MOVE    "key6 "             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             PERFORM  900-JYURRK-FETCH-SEC
           ELSE
             MOVE    1                 TO   FLG-JYURRK
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
           IF    MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_hkncombi"    TO   MCP-TABLE
             MOVE    "key"             TO   MCP-PATHNAME
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
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号情報検索
      *****************************************************************
       900-HKNNUM-READ-SEC                 SECTION.
      *
           INITIALIZE                  HKNNUM-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    SYU-HOSPID          TO  HKN-HOSPID
           MOVE    SYU-HOSPNUM         TO   HKN-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    SYU-SYUHKNNUM       TO   HKN-HKNNUM
           MOVE    WRK-PARA-PRTLYMD    TO   HKN-TEKSTYMD
           MOVE    ALL "9"             TO   HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *     CALL    "MCPSUB"           USING
           MOVE    "tbl_hknnum"        TO   MCP-TABLE
           MOVE    "key5"              TO   MCP-PATHNAME
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
      *         MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *         CALL    "MCPSUB"           USING
             MOVE    "tbl_hknnum"      TO   MCP-TABLE
             MOVE    "key5"            TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =  ZERO
               MOVE    MCPDATA-REC     TO   HKNNUM-REC
               MOVE    ZERO            TO   FLG-HKNNUM
             ELSE
               MOVE    1               TO   FLG-HKNNUM
             END-IF
           ELSE
             MOVE    1                 TO   FLG-HKNNUM
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *     CALL     "MCPSUB"           USING
           MOVE    "tbl_hknnum"        TO   MCP-TABLE
           MOVE    "key5"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
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
      *    収納情報読込処理
      *****************************************************************
       900-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    SYUNOU-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"         TO   MCP-TABLE
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
      *         MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
             MOVE    "tbl_syunou"       TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO   SYUNOU-REC
               MOVE    ZERO             TO   FLG-SYUNOU
             ELSE
               MOVE    1                TO   FLG-SYUNOU
               INITIALIZE               SYUNOU-REC
             END-IF
           ELSE
             MOVE    1                  TO   FLG-SYUNOU
             INITIALIZE                 SYUNOU-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"         TO   MCP-TABLE
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
       900-SYUNOU-READ-EXT.
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
           IF   MCP-RC   =   ZERO
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
             IF      MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   SRYACCT-REC
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
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
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
