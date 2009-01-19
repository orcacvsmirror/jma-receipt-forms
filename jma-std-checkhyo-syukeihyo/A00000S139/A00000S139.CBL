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
       PROGRAM-ID.             A00000S139.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 保険別総括表 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/12/10    @@@@-有村     新規作成
      *  04/07/12    @@@@-本橋     公費と公費を併用の公費単独患者についての
      *                            請求点数を請求管理テーブルに登録されている、
      *                            MAX値を印字するように修正。
      *  04/08/20    @@@@-本橋     食事療養(日数、請求額、標準負担額)を追加する
      *                            ※別フォームでの印字
      *  04/09/17    @@@@-本橋     MONPE化
      *  05/03/08    @@@@-鈴木     open-cobol 0.23への対応
      *  05/04/12    @@@@-本橋     窓口領収額の集計における不具合修正
      *  05/04/19    @@@@-本橋     船員保険（職務上）が正しく集計されない不具合修正
      *  05/06/09    @@@@-本橋     自費・労災・自賠が正しく集計されない不具合修正
      *  05/09/07    @@@@-本橋     公費助成額の算出を修正
      *  06/08/09    @@@@-高嶋     sarge対応
      *  07/05/08    @@@@-内田     テスト患者の判定修正
      *  07/08/24    @@@@-大竹     食事療養欄の日数を回数に変更
      *                            食事回数５桁対応
      *  07/09/20    @@@@-長屋     4.0対応
      *  07/11/14    @@@@-田中     公費単独時に医療収益・保険請求額・公費請求額が
      *                            正しく集計されない不具合を修正
      *  08/04/23    @@@@-田中     H20年4月改正対応(後期高齢者、義務教育就学前、公害保険)
      *                            旧総合病院の時に診療科が表示されない不具合を修正
      *  08/05/12    @@@@-田中     返戻が正しく印字されない不具合を修正
      *  08/05/14    @@@@-田中     H20年4月以前のﾃﾞｰﾀが正しく表示されない不具合を修正
      *  08/06/16    @@@@-田中     健保準拠時の自賠責の計算を修正
      *  08/09/10    @@@@-田中     公費単独欄の食事療養の不具合を修正
      *  08/09/22    @@@@-田中     労災自賠医療機関情報が読み込めない不具合を修正
      *  08/10/14    @@@@-田中     主科設定時に診療科を印字する
      *  08/10/30    @@@@-田中     窓口請求額の不具合を修正
      *  08/11/06    @@@@-田中     自費の値が常に診療科に分かれてしまう不具合を修正
      *  08/12/18    @@@@-門間     自費(095XXXXX、096XXXXXで入力)の集計欄を追加
      *                            食事療養の収益を医療収益に加算
      *  08/12/26    @@@@-門間     医療収益の計算式を変更
      *  09/01/08    @@@@-田中     公費請求額の計算式を変更
      *********************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    全保険用ファイル
           SELECT  S139TMP-FILE    ASSIGN    S139TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  S139TMP-KEY
                                   FILE    STATUS  IS  STS-S139TMP.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    全保険一時ファイル
       FD  S139TMP-FILE.
           COPY    "A00000S139TMP.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    エラーファイル 名称領域 
      ***07/09/20 nagaya 4.0対応 start
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //RECEERR//.
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *
      *    全保険一時ファイル 名称領域
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //S139TP//.
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //S139TP//.
      ***07/09/20 nagaya 4.0対応 end
      *
           COPY    "A00000S139.INC".
           COPY    "A00000S139N.INC".
      *    
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA                 PIC X(02).
           03  STS-RECE                 PIC X(02).
           03  STS-RECEERR              PIC X(02).
           03  STS-RECEUPD              PIC X(02).
           03  STS-S139TMP              PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI             PIC 9(01).
           03  FLG-SYSKANRI3            PIC 9(01).
           03  FLG-JYURRK               PIC 9(01).
           03  FLG-PASS                 PIC 9(01).
           03  FLG-TOKUTEI              PIC 9(01).
           03  FLG-TYPE                 PIC 9(01).
           03  FLG-PTINF                PIC 9(01).
           03  FLG-HKNJAINF             PIC 9(01).
           03  FLG-SYUNOU               PIC 9(01).
           03  FLG-TENSU                PIC 9(01).
           03  FLG-S139TMP              PIC 9(01).
           03  FLG-PRINT                PIC 9(01).
           03  INP-STS                  PIC 9(04).
           03  FLG-KUMIAI               PIC 9(01).
           03  FLG-HKNNUM               PIC 9(01).
           03  FLG-PTHKNINF             PIC 9(01).
           03  FLG-HEIYOU               PIC 9(01).
           03  FLG-RJNHKN               PIC 9(01).
           03  FLG-END                  PIC 9(01).
           03  FLG-HKNCOMBI             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                 PIC 9(02).
           03  CNT-PAGE                 PIC 9(06).
           03  CNT-KOHI                 PIC 9(01).
           03  CNT-NINZU                PIC 9(06).
           03  CNT-JYURRK               PIC 9(06).
           03  CNT-DATA                 PIC 9(06).
           03  CNT-READ                 PIC 9(06).
           03  CNT-KOH                  PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY               PIC 9(02).
               05  SYS-MM               PIC 9(02).
               05  SYS-DD               PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                      PIC 9(01).
           03  IDX1                     PIC 9(04).
           03  IDX2                     PIC 9(04).
           03  IDX3                     PIC 9(04).
           03  IDX4                     PIC 9(04).
           03  IDX5                     PIC 9(04).
           03  IDX6                     PIC 9(03).
           03  IDX7                     PIC 9(01).
           03  IDX8                     PIC 9(01).
           03  IDA-G.
               05  IDA                  PIC 9(02).
           03  IDB-G.
               05  IDB                  PIC 9(02).
           03  IDC-G.
               05  IDC                  PIC 9(04).
           03  IDD-G.
               05  IDD                  PIC 9(04).
           03  IDY                      PIC 9(04).
           03  IDZ                      PIC 9(04).
           03  IDM                      PIC 9(04).
           03  IDX-MAX                  PIC 9(04).
           
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID           PIC 9(07).
           03  WRK-PARA-SHELLID         PIC X(08).
      ***07/09/20 nagaya 4.0対応 start
      *     03  WRK-PARA-HOSPID       PIC X(24).
           03  WRK-PARA-HOSPNUM         PIC 9(02).
      ***07/09/20 nagaya 4.0対応 end
      *    対象診療年月
           03  WRK-PARA-SRYYM           PIC X(06).
      *    対象年月最終日・初日
           03  WRK-PARA-PRTLYMD         PIC X(08).
           03  WRK-PARA-PRTSYMD         PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY            PIC 9(04).
               05  WRK-SYSMM            PIC 9(02).
               05  WRK-SYSDD            PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY              PIC 9(04).
               05  WRK-SMM              PIC 9(02).
               05  WRK-SDD              PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYY            PIC 9(04).
               05  WRK-SRYMM            PIC 9(02).
               05  WRK-SRYDD            PIC 9(02).
           03  WRK-HENYMDG              PIC X(22).
           03  WRK-SRYYM                PIC X(22).
           03  WRK-SEIYMD               PIC X(22).
           03  WRK-RECEERR              PIC X(200).
           03  WRK-SRYKA                PIC X(02).
           03  WRK-NYUGAIKBN            PIC X(01).
           03  WRK-PARA-NYUGAIKBN       PIC X(01).
           03  WRK-FLG-NYUGAIKBN        PIC X(01).
           03  WRK-PTID                 PIC 9(10).
           03  WRK-HKNNUM               PIC X(03).
           03  WRK-KOHNUM               PIC X(03).
           03  WRK-TENTNK               PIC 9(04).
           03  WRK-JIBAITNK             PIC 99.9.
           03  WRK-HKNJANUM             PIC X(06).
           03  WRK-HKNJANAME            PIC X(40).
           03  WRK-KYURATE              PIC 9(02).
           03  WRK-KASAN-TBL.
               05  WRK-KENSU            PIC 9(04).
               05  WRK-NISSU            PIC 9(04).
               05  WRK-TENSU            PIC 9(09).
      *--- (2008/12/26) line change start monma ------------------------
      *         05  WRK-MADOGK            PIC 9(09).
               05  WRK-MADOGK           PIC S9(09).
      *--- (2008/12/26) line change end   monma ------------------------
      *--- (2008/12/18) line add start monma ---------------------------
               05  WRK-JIHIGK           PIC 9(09).
      *--- (2008/12/18) line add end   monma ---------------------------
               05  WRK-HKNGK            PIC 9(09).
               05  WRK-KOHGK            PIC 9(09).
               05  WRK-SYUEKI           PIC 9(09).
      *===食事回数５桁対応 Otake 2007/08/24 Strart
      *         05  WRK-SNISSU            PIC 9(04).
               05  WRK-SNISSU           PIC 9(05).
      *===食事回数５桁対応 Otake 2007/08/24 End
               05  WRK-SSEIKYU          PIC 9(09).
               05  WRK-SFTNGK           PIC 9(09).
               05  WRK-KKENSU           PIC 9(04).
               05  WRK-KNISSU           PIC 9(04).
               05  WRK-KTENSU           PIC 9(09).
           03  WRK-KOHI-GOKEI-TBL.
               05  WRK-GKKENSU          PIC 9(04).
               05  WRK-GKNISSU          PIC 9(04).
               05  WRK-GKTENSU          PIC 9(09).
      *--- (2008/12/26) line change start monma ------------------------
      *     03  WRK-MADOSEIKYUGK-G        PIC 9(09).
           03  WRK-MADOSEIKYUGK-G       PIC S9(09).
      *--- (2008/12/26) line change end   monma ------------------------
      *--- (2008/12/18) line add start monma ---------------------------
           03  WRK-JIHIGK-G             PIC 9(09).
      *--- (2008/12/18) line add end   monma ---------------------------
           03  WRK-KOHSEIKYUGK          PIC 9(09).
           03  WRK-KOHSEIKYUGK-G        PIC 9(09).
           03  WRK-ZENCOMPFTN           PIC 9(09).
           03  WRK-KOHSKJSEIKYUGK       PIC 9(09).
           03  WRK-PATH                 PIC X(64).
           03  WRK-KOHTMP-KEY.
               05  WRK-KOHTMP-RJNKBN    PIC  X(03).
               05  WRK-KOHTMP-KOHNUM    PIC  X(03).
           03  WRK-CHK-TBL.
               05  WRK-CHK-PTID2        PIC 9(10).
               05  WRK-CHK-SRYKA-TBL OCCURS 20.
                   07  WRK-CHK-PTID     PIC 9(10).
                   07  WRK-CHK-DAY-TBL.
                       09  WRK-CHK-DAY  PIC 9(01) OCCURS 31.
           03  WRK-CNT1                 PIC 9(02).
           03  WRK-CNT2                 PIC 9(02).
           03  WRK-CNT3                 PIC 9(02).
           03  WRK-CNT4                 PIC 9(02).
           03  WRK-KOHNAME-G            PIC X(24).
           03  WRK-KOHNAME-G2           PIC X(24).
      *
           03  WRK-PAGE                 PIC ZZ9.
           03  WRK-Z3                   PIC ZZ9.
           03  WRK-Z4                   PIC ZZZZ.
           03  WRK-Z5                   PIC Z,ZZZ.
           03  WRK-Z6                   PIC ZZ,ZZ9.
           03  WRK-Z7                   PIC ZZZ,ZZZ.
           03  WRK-Z9                   PIC Z,ZZZ,ZZZ.
           03  WRK-Z92                  PIC ZZZZZZZZZ.
           03  WRK-Z10                  PIC ZZ,ZZZ,ZZ9.
           03  WRK-Z11                  PIC ZZZ,ZZZ,ZZZ.
           03  WRK-Z13                  PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-Z14                  PIC ZZ,ZZZ,ZZZ,ZZZ.
      *--- (2008/12/26) line add start monma ---------------------------
           03  WRK-Z93                  PIC --------Z.
      *--- (2008/12/26) line add end   monma ---------------------------
      *
      * 2004/03/18  追加
           03  WRK-SYS-BEDSU            PIC 9(04).
           03  WRK-SYS-BEDSUIPN         PIC 9(04).
      * 2004/03/18  追加
      *** 20071114 公費単独の不具合を修正START t.tanaka
           03  WRK-FUKUKOHTEN           PIC 9(09).
           03  WRK-FUKUKOHNISSU         PIC 9(09).
           03  WRK-FUKUNISSU            PIC 9(09).
      *** 20071114 公費単独の不具合を修正END
      * 20080910 公費単独欄の食事療養の不具合を修正STRAT
           03  WRK-FUKU-S-KAISU         PIC 9(09).
           03  WRK-FUKU-S-SKYMONEY      PIC 9(09).
           03  WRK-FUKU-S-FTNMONEY      PIC 9(09).
      * 20080910 公費単独欄の食事療養の不具合を修正END
      * 20080423 H20年4月改正対応START tanaka
           03  WRK-JIHI4                PIC 9(09).
           03  WRK-99                   PIC 9(02).
      * 20080423 H20年4月改正対応END
      *--- 2009/01/08 ADD START ----------------------------------------
           03  WRK-FTNFLG               PIC 9(01).
           03  WRK-HKNFTN               PIC 9(09).
           03  WRK-KOHFTN               PIC 9(09).
      *--- 2009/01/08 ADD END ------------------------------------------
      *
      *    診療科名退避エリア
       01  SUM-AREA.
           03  SUM-SRYKAMAX             PIC 9(03).
           03  SUM-SRYDR-TBL       OCCURS 100.
               05  SUM-SRYKA            PIC X(02).
      *--- 2008/11/06  ADD  START --------------------------------------
               05  SUM-SRYKA2           PIC X(02).
      *--- 2008/11/06  ADD  END ----------------------------------------
               05  SUM-SRYKANAME        PIC X(24).
      *    入院履歴情報退避エリア
       01  WRK-NYUIN-AREA.
           03  WRK-NYUIN-TBL.
               05  WRK-NYUIN-DAY        PIC 9(01) OCCURS 31.
      *
      *    室料差額退避エリア
       01  WRK-ROOM-SAGAKU-TBL.
           03  WRK-ROOM-MAX             PIC 9(02).
           03  WRK-ROOM-DATA     OCCURS 50.
               05  WRK-ROOM-CD          PIC 9(03).
               05  WRK-ROOM-SGK         PIC 9(05).
      *
           COPY    "CPSHELLTBL.INC".
      *
      ***07/09/20 nagaya 4.0対応 start
      *     COPY    "ORCA-DBPATH".
      ***07/09/20 nagaya 4.0対応 end
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    職員情報情報
           COPY    "CPSK1010.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
           
      *--- 2008/11/06  ADD  START --------------------------------------
      *    主科設定情報 
           COPY    "CPSK2009.INC".
      *--- 2008/11/06  ADD  END ----------------------------------------
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    室料差額情報
           COPY  "CPSK5113.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    請求管理マスタ
           COPY    "CPRCF010.INC".
      *
      *    患者保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
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
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
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
      *
	       COPY    "A00000S100LNK.INC".
      *
      ****************************************************************
       PROCEDURE           DIVISION
               USING       A00000S100-LNK.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM   100-INIT-SEC
      *
           PERFORM   200-MAIN-SEC
      *
           PERFORM   300-END-SEC
      *
      **** 20050308 open-cobol 0.23への対応 SUZUKI
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      **** 20050308 ここまで
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-PARA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SUM-AREA
           INITIALIZE                  WRK-ROOM-SAGAKU-TBL
           INITIALIZE                  WRK-NYUIN-AREA
      ***07/09/20 nagaya 4.0対応 start
           INITIALIZE                  SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *     INITIALIZE                  S139TP
      *     INITIALIZE                  S139
      ***07/09/20 nagaya 4.0対応 start
           MOVE    S100LNK-HOSPNUM     TO   WRK-PARA-HOSPNUM
           MOVE    WRK-PARA-HOSPNUM    TO   SPA-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO   SJOBKANRI-MODE
           INITIALIZE                       JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO   JOB-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    S100LNK-JOBID       TO   JOB-JOBID
           MOVE    S100LNK-SHELLID     TO   JOB-SHELLID
           MOVE    "A00000S100"        TO   JOB-PGID
           MOVE    "保険別請求チェック表(総括)"
                                       TO   JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      *     MOVE    "RECEERR"       TO  RECEERR-FILE-ID
      *     MOVE    S100LNK-PRTKANRI-TERMID
      *                             TO  RECEERR-TERMID
      ***07/09/20 nagaya 4.0対応 end
      *     PERFORM 100-DBOPEN-SEC
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
      *    システム日付
           MOVE  S100LNK-PRTKANRI-SKYYMD   TO   WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG               TO   WRK-SEIYMD
      * 20080423 旧総合病院の時に診療科が表示されない不具合を修正START tanaka
           MOVE  S100LNK-PRTKANRI-SKYYMD   TO   SPA-SYSYMD
      * 20080423 旧総合病院の時に診療科が表示されない不具合を修正END
      *
      ***  20040820  @@@@-MOTOHASI  追加
           MOVE   S100LNK-NYUGAIKBN        TO   WRK-PARA-NYUGAIKBN
      ***  20040820
      *
      *    対象日付処理
           MOVE    S100LNK-YM              TO   WRK-SRYYMD (1:6)
           MOVE    "01"                    TO   WRK-SRYYMD (7:2)
           MOVE    S100LNK-YM              TO   WRK-SYMD (1:6)
           MOVE    "01"                    TO   WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)       TO   WRK-SRYYM 
           MOVE    S100LNK-YM              TO   WRK-PARA-SRYYM
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    S100LNK-YM              TO   WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD                TO   WRK-PARA-PRTLYMD
      *
      *    対象年月初日取得
           INITIALIZE  WRK-SYMD
           MOVE    S100LNK-YM              TO   WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"                    TO   WRK-PARA-PRTSYMD(7:2)
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE                   TO   SYS-1001-REC
           INITIALIZE                           SYS-1001-REC
      ***07/09/20 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM             TO   SYS-1001-HOSPNUM
           MOVE    SPA-SYSYMD              TO   SYS-1001-STYUKYMD
                                                SYS-1001-EDYUKYMD
      ***07/09/20 nagaya 4.0対応 end
           MOVE    "1001"                  TO   SYS-1001-KANRICD
           MOVE    "*"                     TO   SYS-1001-KBNCD
           MOVE    SYS-1001-REC            TO   MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI   =   ZERO
             MOVE    MCPDATA-REC           TO   SYS-1001-REC
      ***07/09/20 nagaya 4.0対応 start
      *          MOVE    SYS-1001-HOSPID     TO  WRK-PARA-HOSPID
             MOVE    SYS-1001-HOSPNUM      TO   WRK-PARA-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
             MOVE    SYS-1001-BEDSU        TO   WRK-SYS-BEDSU
             MOVE    SYS-1001-BEDSUIPN     TO   WRK-SYS-BEDSUIPN
           ELSE    
                MOVE    "医療機関ＩＤが取得できませんでした。"
                                           TO   WRK-RECEERR
                PERFORM   500-ERR-HENSYU-SEC
           END-IF
      *
      **    医療機関ＩＤ編集
      *     INITIALIZE                     SYS-1001-REC
      *     MOVE    "1001"         TO      SYS-1001-KANRICD
      *     MOVE    "*"            TO      SYS-1001-KBNCD
      *     MOVE    SYS-1001-REC   TO      MCPDATA-REC
      *     PERFORM 900-SYSKANRI-INV-SEC
      *     IF     FLG-SYSKANRI        =   ZERO
      *            MOVE    MCPDATA-REC     TO    SYS-1001-REC
      *            MOVE    SYS-1001-HOSPID TO    WRK-PARA-HOSPID
      *            IF      SYS-1001-HOSPNAME  =  LOW-VALUE
      *                    MOVE    SPACE   TO    SYS-1001-HOSPNAME
      *            END-IF
      *            IF      SYS-1001-KAISETUNAME  =   LOW-VALUE
      *                    MOVE    SPACE   TO    SYS-1001-KAISETUNAME
      *            END-IF    
      *     ELSE
      *            MOVE    "医療機関情報が取得できませんでした"
      *                                    TO    WRK-RECEERR
      *            PERFORM 500-ERR-HENSYU-SEC
      *     END-IF
      **
      *    自賠責点数単価取得
           MOVE     SPACE                  TO   SYS-4001-REC
           INITIALIZE                      SYS-4001-REC
      ***07/09/20 nagaya 4.0対応 start
           MOVE     SPA-HOSPNUM            TO   SYS-4001-HOSPNUM
           MOVE     SPA-SYSYMD             TO   SYS-4001-STYUKYMD
                                                SYS-4001-EDYUKYMD
      ***07/09/20 nagaya 4.0対応 end
           MOVE     "4001"                 TO   SYS-4001-KANRICD
           MOVE     "*"                    TO   SYS-4001-KBNCD
           MOVE     SYS-4001-REC           TO   MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF       FLG-SYSKANRI        =   ZERO
             MOVE    MCPDATA-REC           TO   SYS-4001-REC
           ELSE
                    INITIALIZE              SYS-4001-REC
           END-IF
           IF   SYS-4001-TENKBN   =   "1"
             MOVE    11.5                  TO   WRK-JIBAITNK
           ELSE
             MOVE    12                    TO   WRK-JIBAITNK
           END-IF
      *--- 2008/11/06  ADD  START --------------------------------------
      *    主科設定情報 
           MOVE     SPACE                  TO   SYS-2009-REC
           INITIALIZE                           SYS-2009-REC
           MOVE    SPA-HOSPNUM             TO   SYS-2009-HOSPNUM
           MOVE    SPA-SYSYMD              TO   SYS-2009-STYUKYMD
                                                SYS-2009-EDYUKYMD
           MOVE    "2009"                  TO   SYS-2009-KANRICD
           MOVE    "*"                     TO   SYS-2009-KBNCD
           MOVE    SYS-2009-REC            TO   MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF   FLG-SYSKANRI   =   ZERO
             MOVE    MCPDATA-REC           TO   SYS-2009-REC
           ELSE
             INITIALIZE           SYS-2009-REC
           END-IF
      *--- 2008/11/06  ADD  END ----------------------------------------
      *
      *    診療科情報取得
      ***07/09/20 nagaya 4.0対応 start
           MOVE    SPACE                  TO   SYS-1005-REC
           INITIALIZE                          SYS-1005-REC
      ***07/09/20 nagaya 4.0対応 end
           MOVE    "1005"                 TO   SYS-1005-KANRICD
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    SPA-SRYYMD             TO  SYS-1005-STYUKYMD
      *                                        SYS-1005-EDYUKYMD
           MOVE    SPA-SYSYMD             TO   SYS-1005-STYUKYMD
                                               SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM            TO   SYS-1005-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    WRK-PARA-PRTLYMD       TO   WRK-SYMD
           PERFORM 820-SYSKANRI-DBSELECT-SEC
           MOVE   1        TO     IDX1 
           PERFORM  UNTIL  FLG-SYSKANRI3 = 1
             IF   FLG-SYSKANRI3  =  ZERO
               MOVE   SYS-1005-KBNCD      TO   SUM-SRYKA(IDX1)
      *--- 2008/11/06  ADD  START --------------------------------------
               MOVE   SYS-1005-RECESRYKA  TO   SUM-SRYKA2(IDX1)
      *--- 2008/11/06  ADD  END ----------------------------------------
               MOVE   SYS-1005-SRYKANAME1 TO   SUM-SRYKANAME(IDX1)
               ADD     1                  TO   IDX1
             END-IF
             PERFORM 820-SYSKANRI-DBFETCH-SEC
           END-PERFORM
           SUBTRACT     1        FROM   IDX1
           MOVE    IDX1         TO   SUM-SRYKAMAX
      *=== sarge対応 ADD By Takashima(2006/09/17) start
           PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/09/17) end
      *
           MOVE    "S139TP"               TO   S139TP-FILE-ID
           MOVE    S100LNK-PRTKANRI-TERMID TO   S139TP-TERMID
      ***07/09/20 nagaya 4.0対応 start
           MOVE    WRK-PARA-HOSPNUM       TO   S139TP-HOSPNUM
      *
      *     MOVE    "RECEERR"           TO  RECEERR-FILE-ID
      *     MOVE    S100LNK-PRTKANRI-TERMID     TO  RECEERR-TERMID
      ***07/09/20 nagaya 4.0対応 end
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    データ集計
           PERFORM  2001-SYUKEI-SEC
      *
      *    印刷処理
           IF    CNT-DATA   >   ZERO
             PERFORM  2002-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ抽出処理
      *****************************************************************
       2001-SYUKEI-SEC                SECTION.
      *
      *
           OPEN     OUTPUT  S139TMP-FILE
           CLOSE            S139TMP-FILE
           OPEN     I-O     S139TMP-FILE
      *    社保データ集計
           PERFORM  2001-SYAHO-SYUKEI-SEC
      *
      *    国保データ集計
           PERFORM  2001-KOKUHO-SYUKEI-SEC
      *
      * 20080423 H20年4月改正対応START tanaka
      *    後期高齢者データ集計
           PERFORM  2001-KOUKI-SYUKEI-SEC
      * 20080423 H20年4月改正対応END
      *    
      *    自費・自賠・労災・公害データ集計
           PERFORM  2001-ETC-SYUKEI-SEC
      *
      *
           CLOSE   S139TMP-FILE
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保データ抽出処理
      *****************************************************************
       2001-SYAHO-SYUKEI-SEC                SECTION.
      *
           INITIALIZE        WRK-PATH
           MOVE    "SEIKYU-KEY3"  TO    WRK-PATH
           PERFORM  900-SEIKYU-SELECT-SEC
      *
           PERFORM  UNTIL  FLG-END   =   1
      *
      *    データ集計
             PERFORM  2001-KASAN-SEC
      *
             PERFORM  2001-DATA-SET3-SEC
      *
             PERFORM 900-SEIKYU-READ-SEC
           END-PERFORM
      *
           PERFORM  900-SEIKYU-CLOSE-SEC
      *
           .
       2001-SYAHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保データ抽出処理
      *****************************************************************
       2001-KOKUHO-SYUKEI-SEC                SECTION.
      *
           INITIALIZE        WRK-PATH
           MOVE    "SEIKYU-KEY4"    TO    WRK-PATH
           PERFORM  900-SEIKYU-SELECT-SEC
      *
           PERFORM  UNTIL  FLG-END   =   1
      *
      *    データ集計
             PERFORM  2001-KASAN-SEC
      * 
             PERFORM  2001-DATA-SET3-SEC
      *
             PERFORM  900-SEIKYU-READ-SEC
           END-PERFORM
      *
           PERFORM  900-SEIKYU-CLOSE-SEC
      *
           .
       2001-KOKUHO-SYUKEI-EXT.
           EXIT.
      * 20080423 H20年4月改正対応START tanaka
      *****************************************************************
      *    後期高齢者データ抽出処理
      *****************************************************************
       2001-KOUKI-SYUKEI-SEC                SECTION.
      *
           INITIALIZE        WRK-PATH
           MOVE    "SEIKYU-ALL"  TO    WRK-PATH
           PERFORM  900-SEIKYU-SELECT-SEC
      *
             PERFORM  UNTIL  FLG-END   =   1
      *
               IF  (RECE10-HKNNUM  =  "039"
                   OR  RECE10-HKNNUM  =  "040")
                   AND ((( RECE10-SRYYM   =   WRK-PARA-SRYYM)
                   AND (  RECE10-SKYKBN  NOT =  "0")
                   AND (( RECE10-SKYYM   =   "000000" )
                   OR  (  RECE10-SKYYM   =   WRK-PARA-SRYYM )))
                   OR  (( RECE10-SKYYM   =   WRK-PARA-SRYYM)
                   AND (  RECE10-SKYKBN  NOT =  "0")))
      *    データ集計
                 PERFORM  2001-KASAN-SEC
      * 
                 PERFORM  2001-DATA-SET3-SEC
      *
               END-IF
               PERFORM 900-SEIKYU-READ-SEC
             END-PERFORM
      *
           PERFORM  900-SEIKYU-CLOSE-SEC
      *
           .
       2001-KOKUHO-SYUKEI-EXT.
           EXIT.
      *
      * 20080423 H20年4月改正対応END
      *****************************************************************
      *    加算データ設定処理
      *****************************************************************
       2001-KASAN-SEC                SECTION.
      *
      *
           INITIALIZE    WRK-KASAN-TBL
      *
           MOVE    RECE10-JNISSU(1)    TO   WRK-NISSU
           MOVE    RECE10-TOTALTEN(1)  TO   WRK-TENSU
      *
      *    限度額が有る場合は(点数×１０) − 患者負担 で算出する
           IF    RECE10-FTNMONEY(1)  =  ZERO
      ***  20050419  @@@@-MOTOHASI  
                   AND  RECE10-KYURATE  NOT =  ZERO
      ***  20050419  @@@@-MOTOHASI  
             COMPUTE  WRK-HKNGK  =  RECE10-TOTALTEN(1)
                                 *  RECE10-KYURATE
           ELSE
             COMPUTE  WRK-HKNGK  =  (RECE10-TOTALTEN(1) * 10)
                                 -  RECE10-FTNMONEY(1)
           END-IF
      *
      *    収納情報から窓口請求額、公費請求額を集計
           MOVE    ZERO                    TO   WRK-MADOSEIKYUGK-G
                                                WRK-KOHSEIKYUGK-G
      *--- (2008/12/18) line add start monma ---------------------------
                                                WRK-JIHIGK-G
      *--- (2008/12/18) line add end   monma ---------------------------
      *
           PERFORM  920-SYUNOU-SET-SEC
      *--- (2008/12/18) line add&del start monma -----------------------
      *    その他自費を別集計
           MOVE    WRK-JIHIGK-G             TO   WRK-JIHIGK
           COMPUTE  WRK-MADOGK  =  WRK-MADOSEIKYUGK-G  -  WRK-JIHIGK-G
      *     MOVE    WRK-MADOSEIKYUGK-G       TO    WRK-MADOGK
      *--- (2008/12/18) line add&del end   monma -----------------------
      *
           MOVE    WRK-KOHSEIKYUGK-G        TO   WRK-KOHGK
      *
      ***  20040820  @@@@-MOTOHASI  食事療養追加
      *    日数
           MOVE  RECE10-SHOKUJINISSU(1)     TO   WRK-SNISSU
      *    請求額
           MOVE  RECE10-SHOKUJIRYOYOHI(1)   TO   WRK-SSEIKYU
      *    標準負担額
           MOVE  RECE10-SHOKUJIFTN(1)       TO   WRK-SFTNGK
      ***  20040820  @@@@-MOTOHASI
      *
      *    公費単独の場合の編集
           IF    RECE10-HKNNUM  =  SPACE
           DISPLAY " 公費単独処理開始 "
      *
      ***  20040712  @@@@-MOTOHASI  公費１〜４でMAX値を印字するように変更
      ***       MOVE    RECE10-JNISSU(2)    TO    WRK-NISSU
      ***       MOVE    RECE10-TOTALTEN(2)  TO    WRK-TENSU
      ***       COMPUTE  WRK-HKNGK  = (RECE10-TOTALTEN(2) * 10)
      ***                           + RECE10-SHOKUJIRYOYOHI(2)
             MOVE    ZERO      TO   WRK-TENSU
             MOVE    2         TO   IDX8
      * 20071114 公費単独の不具合を修正START t.tanaka
             MOVE    ZERO      TO   WRK-FUKUKOHTEN
             MOVE    ZERO      TO   WRK-FUKUKOHNISSU
             MOVE    ZERO      TO   WRK-FUKUNISSU
      * 20071114 公費単独の不具合を修正END
      * 20080910 公費単独欄の食事療養の不具合を修正START
             MOVE    ZERO      TO   WRK-FUKU-S-KAISU
             MOVE    ZERO      TO   WRK-FUKU-S-SKYMONEY
             MOVE    ZERO      TO   WRK-FUKU-S-FTNMONEY
      * 20080910 公費単独欄の食事療養の不具合を修正END
             PERFORM  VARYING  IDX7  FROM  2  BY  1
                      UNTIL    IDX7  >  5
               IF    RECE10-TOTALTEN(IDX7)  >  WRK-TENSU
                 MOVE    IDX7                 TO   IDX8
      * 20071114 公費単独の不具合を修正START t.tanaka
                 ADD  WRK-TENSU               TO   WRK-FUKUKOHTEN
                 ADD  WRK-FUKUNISSU           TO   WRK-FUKUKOHNISSU
      * 20071114 公費単独の不具合を修正END
                 MOVE  RECE10-TOTALTEN(IDX7)  TO   WRK-TENSU
      * 20071114 公費単独の不具合を修正START t.tanaka
                 MOVE  RECE10-JNISSU(IDX7)    TO   WRK-FUKUNISSU
               ELSE
                 ADD  RECE10-TOTALTEN(IDX7)   TO   WRK-FUKUKOHTEN
                 ADD  RECE10-JNISSU(IDX7)     TO   WRK-FUKUKOHNISSU
      * 20080910 公費単独欄の食事療養の不具合を修正STRAT
                 ADD  RECE10-SHOKUJINISSU(IDX7)  TO   WRK-FUKU-S-KAISU
                 ADD  RECE10-SHOKUJIRYOYOHI(IDX7)
                                              TO   WRK-FUKU-S-SKYMONEY
                 ADD  RECE10-SHOKUJIFTN(IDX7) TO   WRK-FUKU-S-FTNMONEY
      * 20080910 公費単独欄の食事療養の不具合を修正END
      * 20071114 公費単独の不具合を修正END
               END-IF
             END-PERFORM
             MOVE    RECE10-JNISSU(IDX8)    TO    WRK-NISSU
             MOVE    RECE10-TOTALTEN(IDX8)  TO    WRK-TENSU
      * 20071114 公費単独の不具合を修正START t.tanaka
             ADD    WRK-FUKUKOHNISSU       TO    WRK-NISSU
             ADD    WRK-FUKUKOHTEN         TO    WRK-TENSU
      *    
      *       COMPUTE  WRK-HKNGK  = (RECE10-TOTALTEN(IDX8) * 10)
      *                + RECE10-SHOKUJIRYOYOHI(IDX8)
      *    公費請求額
      *--- 2009/01/08 ADD START ----------------------------------------
             IF  RECE10-SHOKUJIFTN(IDX8)  NOT =  ZERO
      *    患者食事負担有り
               COMPUTE   WRK-HKNGK  =  ((WRK-FUKUKOHTEN
                                  +  RECE10-TOTALTEN(IDX8))  *  10)
                                  -  (WRK-MADOGK
                                  -  RECE10-SHOKUJIFTN(IDX8))
             ELSE
      *    患者食事負担無し
      *--- 2009/01/08 ADD END ------------------------------------------
               COMPUTE   WRK-HKNGK  =  ((WRK-FUKUKOHTEN
                                  +  RECE10-TOTALTEN(IDX8))  *  10)
                                  +  RECE10-SHOKUJIRYOYOHI(IDX8)
                                  -  RECE10-FTNMONEY(IDX8)
      *--- 2009/01/08 ADD START ----------------------------------------
             END-IF
      *--- 2009/01/08 ADD END ------------------------------------------
      *
      * 20071114 公費単独の不具合を修正END
      ***  20040712  ここまで
      *
             ADD     WRK-HKNGK           TO   WRK-KOHGK
             MOVE    ZERO                TO   WRK-HKNGK
      *
      ***  20040820  @@@@-MOTOHASI  食事療養追加
      *    日数
             MOVE  RECE10-SHOKUJINISSU(IDX8)     TO   WRK-SNISSU
      * 20080910 公費単独欄の食事療養の不具合を修正START
             ADD   WRK-FUKU-S-KAISU              TO   WRK-SNISSU
      * 20080910 公費単独欄の食事療養の不具合を修正END
      *    請求額
             MOVE  RECE10-SHOKUJIRYOYOHI(IDX8)   TO   WRK-SSEIKYU
      * 20080910 公費単独欄の食事療養の不具合を修正START
             ADD   WRK-FUKU-S-SKYMONEY           TO   WRK-SSEIKYU
      * 20080910 公費単独欄の食事療養の不具合を修正END
      *    標準負担額
             MOVE  RECE10-SHOKUJIFTN(IDX8)       TO   WRK-SFTNGK
      * 20080910 公費単独欄の食事療養の不具合を修正START
             ADD   WRK-FUKU-S-FTNMONEY           TO   WRK-SFTNGK
      * 20080910 公費単独欄の食事療養の不具合を修正END
      ***  20040820  @@@@-MOTOHASI
           END-IF
      *
      *    医療収益
           IF    RECE10-HKNNUM  =  SPACE
      *    公費単独
      *--- 2009/01/08 ADD START ----------------------------------------
             IF  RECE10-SHOKUJIFTN(IDX8)  NOT =  ZERO
      *    患者食事負担無し
      *--- 2009/01/08 ADD END ------------------------------------------
               COMPUTE  WRK-SYUEKI  =  WRK-HKNGK  +  WRK-MADOGK
                                    +  WRK-KOHGK
      *--- (2008/12/18) line add start monma ---------------------------
      *    食事収益加算
                                    +  WRK-JIHIGK
                                    +  (WRK-SSEIKYU - WRK-SFTNGK)
      *--- (2008/12/18) line add end   monma ---------------------------
      *--- 2009/01/08 ADD START ----------------------------------------
             ELSE
      *    患者食事負担有り
               COMPUTE  WRK-SYUEKI  =  WRK-HKNGK  +  WRK-MADOGK
                                    +  WRK-KOHGK
                                    +  WRK-JIHIGK
             END-IF
           ELSE
      *    公費単独以外
             COMPUTE  WRK-SYUEKI  =  WRK-HKNGK  +  WRK-MADOGK
                                  +  WRK-KOHGK
                                  +  WRK-JIHIGK
                                  +  (WRK-SSEIKYU - WRK-SFTNGK)
           END-IF
      *--- 2009/01/08 ADD END ------------------------------------------
      *
      *    公費集計   (単独は集計しない)
           PERFORM  2001-KASAN-KOUHI-SEC
      *
           .
       2001-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保公費集計処理
      *****************************************************************
        2001-KASAN-KOUHI-SEC                SECTION.
      *
           IF    RECE10-KOHNUM(1)  NOT =  SPACE
               AND  RECE10-HKNNUM   NOT =  SPACE
             PERFORM VARYING IDX5  FROM  1   BY  1
                     UNTIL   IDX5  >  4
                     OR  RECE10-KOHNUM (IDX5)   =   SPACE
      *
             ADD     1                          TO   WRK-KKENSU
             ADD     RECE10-JNISSU(IDX5 + 1)    TO   WRK-KNISSU
             ADD     RECE10-TOTALTEN(IDX5 + 1)  TO   WRK-KTENSU
      *
             END-PERFORM
           END-IF
           .
       2001-KASAN-KOUHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険合計加算
      *****************************************************************
       2001-DATA-SET3-SEC                SECTION.
      *
      *    キー設定
           INITIALIZE    S139TMP-REC
      *
           MOVE    RECE10-RECEKA       TO   S139TMP-SRYKA
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
           IF  RECE10-SRYYM < "200804"
             MOVE    "0"       TO   S139TMP-SRYYMKBN
           ELSE
             MOVE    "9"       TO   S139TMP-SRYYMKBN
           END-IF
      * 20080512 返戻が正しく印字されない不具合を修正END
      *    TMPファイル読み込み
           MOVE    3                   TO   FLG-TYPE
           PERFORM  800-SKTTMP-READ-SEC
      *
      *    保険合計集計場所設定
           IF   S100LNK-YM < "200804"
             PERFORM  2001-IDX-SET3-SEC
           ELSE
             PERFORM  2002-IDX-SET3-SEC
      * 20080514 H20年4月以前のﾃﾞｰﾀが正しく表示されない不具合を修正START
           END-IF
      *
      * 20080514 H20年4月以前のﾃﾞｰﾀが正しく表示されない不具合を修正END
      *    データ設定
      *    件数加算
           ADD    1                 TO   S139TMP-KENSU(IDX1 IDX2)
                                         S139TMP-S-KENSU(IDX1 IDX3)
                                         S139TMP-S-KENSU(IDX1 4)
                                         S139TMP-GKENSU(1)
      * 20080423 H20年4月改正対応START tanaka
      *                                    S139TMP-GKENSU(5)
                                         S139TMP-GKENSU(6)
      * 20080423 H20年4月改正対応END
      *
      *    日数加算
           ADD    WRK-NISSU         TO   S139TMP-NISSU(IDX1 IDX2)
                                         S139TMP-S-NISSU(IDX1 IDX3)
                                         S139TMP-S-NISSU(IDX1 4)
                                         S139TMP-GNISSU(1)
      * 20080423 H20年4月改正対応START tanaka
      *                                    S139TMP-GNISSU(5)
                                         S139TMP-GNISSU(6)
      * 20080423 H20年4月改正対応END
      *
      *    点数加算
           ADD    WRK-TENSU         TO   S139TMP-TENSU(IDX1 IDX2)
                                         S139TMP-S-TENSU(IDX1 IDX3)
                                         S139TMP-S-TENSU(IDX1 4)
                                         S139TMP-GTENSU(1)
      * 20080423 H20年4月改正対応START tanaka
      *                                    S139TMP-GTENSU(5)
                                         S139TMP-GTENSU(6)
      * 20080423 H20年4月改正対応END
      *
      *    窓口請求額加算
           ADD    WRK-MADOGK        TO   S139TMP-MADOGK(IDX1 IDX2)
                                         S139TMP-S-MADOGK(IDX1 IDX3)
                                         S139TMP-S-MADOGK(IDX1 4)
                                         S139TMP-GMADOGK(1)
      * 20080423 H20年4月改正対応START tanaka
      *                                    S139TMP-GMADOGK(5)
                                         S139TMP-GMADOGK(6)
      * 20080423 H20年4月改正対応END
      *
      *--- (2008/12/18) line add start monma ---------------------------
      *    自費加算
           ADD    WRK-JIHIGK        TO   S139TMP-JIHIGK(IDX1 IDX2)
                                         S139TMP-S-JIHIGK(IDX1 IDX3)
                                         S139TMP-S-JIHIGK(IDX1 4)
                                         S139TMP-GJIHIGK(1)
                                         S139TMP-GJIHIGK(6)
      *--- (2008/12/18) line add end   monma ---------------------------
      *
      *    保険請求額加算
           ADD    WRK-HKNGK         TO   S139TMP-HKNGK(IDX1 IDX2)
                                         S139TMP-S-HKNGK(IDX1 IDX3)
                                         S139TMP-S-HKNGK(IDX1 4)
                                         S139TMP-GHKNGK(1)
      * 20080423 H20年4月改正対応START tanaka
      *                                    S139TMP-GHKNGK(5)
                                         S139TMP-GHKNGK(6)
      * 20080423 H20年4月改正対応END
      *
      *    公費請求額
           ADD    WRK-KOHGK         TO   S139TMP-KOHGK(IDX1 IDX2)
                                         S139TMP-S-KOHGK(IDX1 IDX3)
                                         S139TMP-S-KOHGK(IDX1 4)
                                         S139TMP-GKOHGK(1)
      * 20080423 H20年4月改正対応START tanaka
      *                                    S139TMP-GKOHGK(5)
                                         S139TMP-GKOHGK(6)
      * 20080423 H20年4月改正対応END
      *
      *    医療収益
           ADD    WRK-SYUEKI        TO   S139TMP-SYUEKI(IDX1 IDX2)
                                         S139TMP-S-SYUEKI(IDX1 IDX3)
                                         S139TMP-S-SYUEKI(IDX1 4)
                                         S139TMP-GSYUEKI(1)
      * 20080423 H20年4月改正対応START tanaka
      *                                    S139TMP-GSYUEKI(5)
                                         S139TMP-GSYUEKI(6)
      * 20080423 H20年4月改正対応END
      *
      ***  20040820  @@@@-MOTOHASI  食事療養追加
      *
      *    日数加算
           ADD    WRK-SNISSU        TO   S139TMP-SNISSU(IDX1 IDX2)
                                         S139TMP-S-SNISSU(IDX1 IDX3)
                                         S139TMP-S-SNISSU(IDX1 4)
                                         S139TMP-SGNISSU(1)
      * 20080423 H20年4月改正対応START tanaka
      *                                     S139TMP-SGNISSU(5)
                                          S139TMP-SGNISSU(6)
      * 20080423 H20年4月改正対応END
      *    請求額加算
           ADD    WRK-SSEIKYU       TO   S139TMP-SSEIKYU(IDX1 IDX2)
                                         S139TMP-S-SSEIKYU(IDX1 IDX3)
                                         S139TMP-S-SSEIKYU(IDX1 4)
                                         S139TMP-SGSEIKYU(1)
      * 20080423 H20年4月改正対応START tanaka
      *                                     S139TMP-SGSEIKYU(5)
                                          S139TMP-SGSEIKYU(6)
      * 20080423 H20年4月改正対応END
      *    標準負担額加算
           ADD    WRK-SFTNGK        TO   S139TMP-SFTNGK(IDX1 IDX2)
                                         S139TMP-S-SFTNGK(IDX1 IDX3)
                                         S139TMP-S-SFTNGK(IDX1 4)
                                         S139TMP-SGFTNGK(1)
      * 20080423 H20年4月改正対応START tanaka
      *                                     S139TMP-SGFTNGK(5)
                                          S139TMP-SGFTNGK(6)
      * 20080423 H20年4月改正対応END
      ***  20040820  @@@@-MOTOHASI
      *
      *
      *    公費分再掲
           ADD    WRK-KKENSU        TO   S139TMP-KENSU(IDX1 16)
           ADD    WRK-KNISSU        TO   S139TMP-NISSU(IDX1 16)
           ADD    WRK-KTENSU        TO   S139TMP-TENSU(IDX1 16)
      *
      *    書き出し
           IF  FLG-S139TMP   =  1
             ADD      1             TO   CNT-DATA
             WRITE    S139TMP-REC
           ELSE
             REWRITE  S139TMP-REC
           END-IF
      *
           .
       2001-DATA-SET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費・自賠・労災・公害データ抽出処理
      *****************************************************************
       2001-ETC-SYUKEI-SEC                SECTION.
      *
      *    自費・自賠・労災を受診履歴情報から抽出
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL   IDX   >   2
             INITIALIZE           WRK-CHK-TBL
             MOVE    IDX       TO   WRK-NYUGAIKBN
             PERFORM  920-DBSELECT3-SEC
      *
             PERFORM  UNTIL   FLG-JYURRK   =   1
      *
      ***  20050609  @@@@-MOTOHASI  
               IF  JYURRK-EDANUM  =  1
      ***  20050609  @@@@-MOTOHASI  
      *
      *    旧総合病院の場合、診療科コードで判定を分ける
                 MOVE    JYURRK-SRYYMD(7:2)   TO   IDA-G
      * 20081014 主科設定時に診療科を印字するSTART
                 IF    SYS-1001-HOSPSBT1    =    1
      *--- 2008/11/06  ADD  START --------------------------------------
                     OR  SYS-2009-SYUKA-KBN = 1
      *             MOVE    JYURRK-SRYKA   TO  IDB-G
                   MOVE    1              TO  IDB
      *--- 2008/11/06  ADD  END ----------------------------------------
                 ELSE
                   MOVE    1              TO  IDB
                 END-IF
      * 20081014 主科設定時に診療科を印字するEND
      *    保険組み合わせ情報を参照し、自費・自賠・労災の判定を行う
                 PERFORM  930-HKNCOMBI-SET-SEC
                 IF    COMB-HKNNUM(1:2)  =  "98" OR "97"
                   IF    JYURRK-NYUGAIKBN    =    "2"
                     PERFORM  2001-DATA-SET-ETC-G-SEC
                   ELSE
      *    入院データ時の集計
                     IF    JYURRK-PTID  NOT  =  WRK-CHK-PTID2
                       MOVE  JYURRK-PTID    TO  WRK-CHK-PTID2
                       PERFORM  940-SYUNOU-SELECT-SEC
                       PERFORM  UNTIL  FLG-SYUNOU  =  1
                         PERFORM  2001-DATA-SET-ETC-N-SEC
                         PERFORM  940-SYUNOU-FETCH-SEC
                       END-PERFORM
                       PERFORM  940-SYUNOU-CLOSE-SEC
                     END-IF
                   END-IF
                 END-IF
      ***  20050609  @@@@-MOTOHASI
               END-IF
      ***  20050609  @@@@-MOTOHASI
      *
               PERFORM  920-DBFETCH3-SEC
             END-PERFORM
      *
             PERFORM  920-DBCLOSE3-SEC
           END-PERFORM
      *
           .
       2001-ETC-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費・自賠・労災・公害 加算データ集計(外来)
      *****************************************************************
       2001-DATA-SET-ETC-G-SEC                SECTION.
      *
      *    キー設定
           INITIALIZE    S139TMP-REC
      *
      * 20081014 主科設定時に診療科を印字するSTART
      **--- 2008/11/06  ADD  START -------------------------------------
           IF    SYS-1001-HOSPSBT1  =  1 
               OR  SYS-2009-SYUKA-KBN = 1
      *       MOVE    JYURRK-SRYKA      TO    S139TMP-SRYKA
             MOVE    "00"              TO    S139TMP-SRYKA
           ELSE
      **--- 2008/11/06  ADD  END ---------------------------------------
             MOVE    "00"              TO    S139TMP-SRYKA
      **--- 2008/11/06  ADD  START -------------------------------------
           END-IF
      **--- 2008/11/06  ADD  END ---------------------------------------
      * 20081014 主科設定時に診療科を印字するEND
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
      *     MOVE    RECE10-SRYYM       TO    S139TMP-SRYYM
           IF  JYURRK-SRYYMD  <  "20080401"
             MOVE    "0"       TO    S139TMP-SRYYMKBN
           ELSE
             MOVE    "9"       TO    S139TMP-SRYYMKBN
           END-IF
      * 20080512 返戻が正しく印字されない不具合を修正END
      *    TMPファイル読み込み
           MOVE    3                   TO    FLG-TYPE
           PERFORM 800-SKTTMP-READ-SEC
      *
      *    自費・自賠・労災・公害 集計場所設定
           EVALUATE  COMB-HKNNUM
           WHEN "971"
             MOVE    2    TO    IDX1
             MOVE    3    TO    IDX2
           WHEN "973"
             MOVE    3    TO    IDX1
             MOVE    4    TO    IDX2
      * 20080423 H20年4月改正対応START tanaka
           WHEN "975"
             MOVE    4    TO    IDX1
             MOVE    5    TO    IDX2
      * 20080423 H20年4月改正対応END
           WHEN OTHER
             MOVE    1    TO    IDX1
             MOVE    2    TO    IDX2
           END-EVALUATE
      *
      *
      *    加算テーブル初期化
           INITIALIZE    WRK-KASAN-TBL
      *
      *    患者判定
           MOVE    JYURRK-SRYYMD(7:2)  TO  IDA-G
           IF   JYURRK-PTID  NOT =   WRK-CHK-PTID(IDB)
             INITIALIZE             WRK-CHK-DAY-TBL(IDB)
             MOVE    JYURRK-PTID    TO   WRK-CHK-PTID(IDB)
             MOVE    1              TO   WRK-KENSU
           END-IF
      *
      *    日数判定
           IF    WRK-CHK-DAY(IDB IDA)  =  ZERO
             MOVE    1              TO   WRK-CHK-DAY(IDB IDA)
             MOVE    1              TO   WRK-NISSU
           END-IF
      *
      *    収納情報よりデータ抽出
           PERFORM  940-SYUNOU-READ-SEC
      *    自費・時賠・労災用加算データ編集
           PERFORM  2001-KASAN-ETC-SEC
      *
      *
      * 20080423 H20年4月改正対応START tanaka
      *    データ設定
      *    件数加算
      *     ADD    WRK-KENSU       TO    S139TMP-ETC-KENSU(IDX IDX1)
      *                                  S139TMP-ETC-KENSU(IDX 4)
      *                                  S139TMP-GKENSU(IDX2)
      *                                  S139TMP-GKENSU(5)
      **
      **    日数加算
      *     ADD    WRK-NISSU       TO    S139TMP-ETC-NISSU(IDX IDX1)
      *                                  S139TMP-ETC-NISSU(IDX 4)
      *                                  S139TMP-GNISSU(IDX2)
      *                                  S139TMP-GNISSU(5)
      **
      **    点数加算
      *     ADD    WRK-TENSU       TO    S139TMP-ETC-TENSU(IDX IDX1)
      *                                  S139TMP-ETC-TENSU(IDX 4)
      *                                  S139TMP-GTENSU(IDX2)
      *                                  S139TMP-GTENSU(5)
      **
      **    窓口請求額加算
      *     ADD    WRK-MADOGK      TO    S139TMP-ETC-MADOGK(IDX IDX1)
      *                                  S139TMP-ETC-MADOGK(IDX 4)
      *                                  S139TMP-GMADOGK(IDX2)
      *                                  S139TMP-GMADOGK(5)
      **
      **    保険請求額加算
      *     ADD    WRK-HKNGK       TO    S139TMP-ETC-HKNGK(IDX IDX1)
      *                                  S139TMP-ETC-HKNGK(IDX 4)
      *                                  S139TMP-GHKNGK(IDX2)
      *                                  S139TMP-GHKNGK(5)
      **
      **    医療収益
      *     ADD    WRK-SYUEKI      TO    S139TMP-ETC-SYUEKI(IDX IDX1)
      *                                  S139TMP-ETC-SYUEKI(IDX 4)
      *                                  S139TMP-GSYUEKI(IDX2)
      *                                  S139TMP-GSYUEKI(5)
      **
      ****  20040820  @@@@-MOTOHASI  食事療養追加
      **
      **    日数加算
      *     ADD    WRK-SNISSU      TO    S139TMP-ETC-SNISSU(IDX IDX1)
      *                                  S139TMP-ETC-SNISSU(IDX 4)
      *                                  S139TMP-SGNISSU(IDX2)
      *                                  S139TMP-SGNISSU(5)
      **    請求額加算
      *     ADD    WRK-SSEIKYU     TO    S139TMP-ETC-SSEIKYU(IDX IDX1)
      *                                  S139TMP-ETC-SSEIKYU(IDX 4)
      *                                  S139TMP-SGSEIKYU(IDX2)
      *                                  S139TMP-SGSEIKYU(5)
      **    標準負担額加算
      *     ADD    WRK-SFTNGK      TO    S139TMP-ETC-SFTNGK(IDX IDX1)
      *                                  S139TMP-ETC-SFTNGK(IDX 4)
      *                                  S139TMP-SGFTNGK(IDX2)
      *                                  S139TMP-SGFTNGK(5)
      ***  20040820  @@@@-MOTOHASI  
      *
      *
      *    データ設定
      *    件数加算
           ADD    WRK-KENSU       TO    S139TMP-ETC-KENSU(IDX IDX1)
                                        S139TMP-ETC-KENSU(IDX 5)
                                        S139TMP-GKENSU(IDX2)
                                        S139TMP-GKENSU(6)
      *
      *    日数加算
           ADD    WRK-NISSU       TO    S139TMP-ETC-NISSU(IDX IDX1)
                                        S139TMP-ETC-NISSU(IDX 5)
                                        S139TMP-GNISSU(IDX2)
                                        S139TMP-GNISSU(6)
      *
      *    点数加算
           ADD    WRK-TENSU       TO    S139TMP-ETC-TENSU(IDX IDX1)
                                        S139TMP-ETC-TENSU(IDX 5)
                                        S139TMP-GTENSU(IDX2)
                                        S139TMP-GTENSU(6)
      *
      *    窓口請求額加算
           ADD    WRK-MADOGK      TO    S139TMP-ETC-MADOGK(IDX IDX1)
                                        S139TMP-ETC-MADOGK(IDX 5)
                                        S139TMP-GMADOGK(IDX2)
                                        S139TMP-GMADOGK(6)
      *
      *--- (2008/12/18) line add start monma ---------------------------
      *    自費加算
           ADD    WRK-JIHIGK      TO    S139TMP-ETC-JIHIGK(IDX IDX1)
                                        S139TMP-ETC-JIHIGK(IDX 5)
                                        S139TMP-GJIHIGK(IDX2)
                                        S139TMP-GJIHIGK(6)
      *--- (2008/12/18) line add end   monma ---------------------------
      *
      *    保険請求額加算
           ADD    WRK-HKNGK       TO    S139TMP-ETC-HKNGK(IDX IDX1)
                                        S139TMP-ETC-HKNGK(IDX 5)
                                        S139TMP-GHKNGK(IDX2)
                                        S139TMP-GHKNGK(6)
      *
      *    医療収益
           ADD    WRK-SYUEKI      TO    S139TMP-ETC-SYUEKI(IDX IDX1)
                                        S139TMP-ETC-SYUEKI(IDX 5)
                                        S139TMP-GSYUEKI(IDX2)
                                        S139TMP-GSYUEKI(6)
      *
      *    日数加算
           ADD    WRK-SNISSU      TO    S139TMP-ETC-SNISSU(IDX IDX1)
                                        S139TMP-ETC-SNISSU(IDX 5)
                                        S139TMP-SGNISSU(IDX2)
                                        S139TMP-SGNISSU(6)
      *    請求額加算
           ADD    WRK-SSEIKYU     TO    S139TMP-ETC-SSEIKYU(IDX IDX1)
                                        S139TMP-ETC-SSEIKYU(IDX 5)
                                        S139TMP-SGSEIKYU(IDX2)
                                        S139TMP-SGSEIKYU(6)
      *    標準負担額加算
           ADD    WRK-SFTNGK      TO    S139TMP-ETC-SFTNGK(IDX IDX1)
                                        S139TMP-ETC-SFTNGK(IDX 5)
                                        S139TMP-SGFTNGK(IDX2)
                                        S139TMP-SGFTNGK(6)
      * 20080423 H20年4月改正対応END
      *
      *
      *    書き出し
           IF  FLG-S139TMP   =  1
             ADD      1           TO    CNT-DATA
             WRITE    S139TMP-REC
           ELSE
             REWRITE  S139TMP-REC
           END-IF
      *
           .
       2001-DATA-SET-ETC-G-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費・自賠・労災・公害 加算データ集計(入院)
      *****************************************************************
       2001-DATA-SET-ETC-N-SEC                SECTION.
      *
      *    キー設定
           INITIALIZE    S139TMP-REC
      *
      *    入院はレセプトが診療科毎に別れない
      * 20081014 主科設定時に診療科を印字するSTART
           IF    SYS-1001-HOSPSBT1  =  1 
      *--- 2008/11/06  ADD  START --------------------------------------
                   OR  SYS-2009-SYUKA-KBN = 1
      *--- 2008/11/06  ADD  END ----------------------------------------
      *       MOVE    SYU-SRYKA         TO    S139TMP-SRYKA
             MOVE    "00"        TO    S139TMP-SRYKA
      *                                 IDB-G
             MOVE    1           TO    IDB
           ELSE
             MOVE    "00"        TO    S139TMP-SRYKA
             MOVE    1           TO    IDB
           END-IF
      * 20081014 主科設定時に診療科を印字するEND
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
      *     MOVE    RECE10-SRYYM       TO    S139TMP-SRYYM
           IF  RECE10-SRYYM < "200804"
             MOVE    "0"         TO    S139TMP-SRYYMKBN
           ELSE
             MOVE    "9"         TO    S139TMP-SRYYMKBN
           END-IF
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka

      *    TMPファイル読み込み
           MOVE    3                   TO    FLG-TYPE
           PERFORM 800-SKTTMP-READ-SEC
      *
      *    自費・自賠・労災・公害 集計場所設定
           EVALUATE  SYU-SYUHKNNUM
           WHEN "971"
             MOVE    2           TO    IDX1
             MOVE    3           TO    IDX2
           WHEN "973"
             MOVE    3           TO    IDX1
             MOVE    4           TO    IDX2
      * 20080423 H20年4月改正対応START tanaka
           WHEN "975"
             MOVE    4           TO    IDX1
             MOVE    5           TO    IDX2
      * 20080423 H20年4月改正対応END
           WHEN OTHER
             MOVE    1           TO    IDX1
             MOVE    2           TO    IDX2
           END-EVALUATE
      *
      *    加算テーブル初期化
           INITIALIZE    WRK-KASAN-TBL
      *
      *    患者判定
           IF    SYU-PTID    NOT  =  WRK-CHK-PTID(IDB)
             MOVE    SYU-PTID       TO  WRK-CHK-PTID(IDB)
             MOVE    1              TO  WRK-KENSU
           END-IF
      *
      *    日数判定
      *    請求開始年月日より、入院日数を集計
           IF    SYU-SKYEDYMD    >    WRK-PARA-PRTLYMD
             MOVE    WRK-PARA-PRTLYMD  TO   SYU-SKYEDYMD
           END-IF
      * 20080423 H20年4月改正対応START tanaka
      *     MOVE    SYU-SKYSTYMD(7:2)   TO  IDC-G
      *     MOVE    SYU-SKYEDYMD(7:2)   TO  IDD-G
           MOVE    SYU-SKYSTYMD(7:2)   TO   WRK-99
           MOVE    WRK-99              TO   IDC
           MOVE    SYU-SKYEDYMD(7:2)   TO   WRK-99
           MOVE    WRK-99              TO   IDD
      * 20080423 H20年4月改正対応END
           COMPUTE  WRK-NISSU    =    IDD - IDC + 1
      *    自費・時賠・労災用加算データ編集
           PERFORM  2001-KASAN-ETC-SEC
      *
      * 20080423 H20年4月改正対応START tanaka
      *    データ設定
      *    件数加算
      *     ADD    WRK-KENSU       TO    S139TMP-ETC-KENSU(IDX IDX1)
      *                                  S139TMP-ETC-KENSU(IDX 4)
      *                                  S139TMP-GKENSU(IDX2)
      *                                  S139TMP-GKENSU(5)
      **
      **    日数加算
      *     ADD    WRK-NISSU       TO    S139TMP-ETC-NISSU(IDX IDX1)
      *                                  S139TMP-ETC-NISSU(IDX 4)
      *                                  S139TMP-GNISSU(IDX2)
      *                                  S139TMP-GNISSU(5)
      **
      **    点数加算
      *     ADD    WRK-TENSU       TO    S139TMP-ETC-TENSU(IDX IDX1)
      *                                  S139TMP-ETC-TENSU(IDX 4)
      *                                  S139TMP-GTENSU(IDX2)
      *                                  S139TMP-GTENSU(5)
      **
      **    窓口請求額加算
      *     ADD    WRK-MADOGK      TO    S139TMP-ETC-MADOGK(IDX IDX1)
      *                                  S139TMP-ETC-MADOGK(IDX 4)
      *                                  S139TMP-GMADOGK(IDX2)
      *                                  S139TMP-GMADOGK(5)
      **
      **    保険請求額加算
      *     ADD    WRK-HKNGK       TO    S139TMP-ETC-HKNGK(IDX IDX1)
      *                                  S139TMP-ETC-HKNGK(IDX 4)
      *                                  S139TMP-GHKNGK(IDX2)
      *                                  S139TMP-GHKNGK(5)
      **
      **    医療収益
      *     ADD    WRK-SYUEKI      TO    S139TMP-ETC-SYUEKI(IDX IDX1)
      *                                  S139TMP-ETC-SYUEKI(IDX 4)
      *                                  S139TMP-GSYUEKI(IDX2)
      *                                  S139TMP-GSYUEKI(5)
      **
      ****  20040820  @@@@-MOTOHASI  食事療養追加
      **
      **    日数加算
      *     ADD    WRK-SNISSU      TO    S139TMP-ETC-SNISSU(IDX IDX1)
      *                                  S139TMP-ETC-SNISSU(IDX 4)
      *                                  S139TMP-SGNISSU(IDX2)
      *                                  S139TMP-SGNISSU(5)
      **    請求額加算
      *     ADD    WRK-SSEIKYU     TO    S139TMP-ETC-SSEIKYU(IDX IDX1)
      *                                  S139TMP-ETC-SSEIKYU(IDX 4)
      *                                  S139TMP-SGSEIKYU(IDX2)
      *                                  S139TMP-SGSEIKYU(5)
      **    標準負担額加算
      *     ADD    WRK-SFTNGK      TO    S139TMP-ETC-SFTNGK(IDX IDX1)
      *                                  S139TMP-ETC-SFTNGK(IDX 4)
      *                                  S139TMP-SGFTNGK(IDX2)
      *                                  S139TMP-SGFTNGK(5)
      ***  20040820  @@@@-MOTOHASI  
      *
      *    データ設定
      *    件数加算
           ADD    WRK-KENSU       TO    S139TMP-ETC-KENSU(IDX IDX1)
                                        S139TMP-ETC-KENSU(IDX 5)
                                        S139TMP-GKENSU(IDX2)
                                        S139TMP-GKENSU(6)
      *
      *    日数加算
           ADD    WRK-NISSU       TO    S139TMP-ETC-NISSU(IDX IDX1)
                                        S139TMP-ETC-NISSU(IDX 5)
                                        S139TMP-GNISSU(IDX2)
                                        S139TMP-GNISSU(6)
      *
      *    点数加算
           ADD    WRK-TENSU       TO    S139TMP-ETC-TENSU(IDX IDX1)
                                        S139TMP-ETC-TENSU(IDX 5)
                                        S139TMP-GTENSU(IDX2)
                                        S139TMP-GTENSU(6)
      *
      *    窓口請求額加算
           ADD    WRK-MADOGK      TO    S139TMP-ETC-MADOGK(IDX IDX1)
                                        S139TMP-ETC-MADOGK(IDX 5)
                                        S139TMP-GMADOGK(IDX2)
                                        S139TMP-GMADOGK(6)
      *
      *--- (2008/12/18) line add start monma ---------------------------
      *    自費加算
           ADD    WRK-JIHIGK      TO    S139TMP-ETC-JIHIGK(IDX IDX1)
                                        S139TMP-ETC-JIHIGK(IDX 5)
                                        S139TMP-GJIHIGK(IDX2)
                                        S139TMP-GJIHIGK(6)
      *--- (2008/12/18) line add end   monma ---------------------------
      *
      *    保険請求額加算
           ADD    WRK-HKNGK       TO    S139TMP-ETC-HKNGK(IDX IDX1)
                                        S139TMP-ETC-HKNGK(IDX 5)
                                        S139TMP-GHKNGK(IDX2)
                                        S139TMP-GHKNGK(6)
      *
      *    医療収益
           ADD    WRK-SYUEKI      TO    S139TMP-ETC-SYUEKI(IDX IDX1)
                                        S139TMP-ETC-SYUEKI(IDX 5)
                                        S139TMP-GSYUEKI(IDX2)
                                        S139TMP-GSYUEKI(6)
      *
      ***  20040820  @@@@-MOTOHASI  食事療養追加
      *
      *    日数加算
           ADD    WRK-SNISSU      TO    S139TMP-ETC-SNISSU(IDX IDX1)
                                        S139TMP-ETC-SNISSU(IDX 5)
                                        S139TMP-SGNISSU(IDX2)
                                        S139TMP-SGNISSU(6)
      *    請求額加算
           ADD    WRK-SSEIKYU     TO    S139TMP-ETC-SSEIKYU(IDX IDX1)
                                        S139TMP-ETC-SSEIKYU(IDX 5)
                                        S139TMP-SGSEIKYU(IDX2)
                                        S139TMP-SGSEIKYU(6)
      *    標準負担額加算
           ADD    WRK-SFTNGK      TO    S139TMP-ETC-SFTNGK(IDX IDX1)
                                        S139TMP-ETC-SFTNGK(IDX 5)
                                        S139TMP-SGFTNGK(IDX2)
                                        S139TMP-SGFTNGK(6)
      * 20080423 H20年4月改正対応END
      *
      *
      *    書き出し
           IF  FLG-S139TMP   =  1
             ADD      1           TO    CNT-DATA
             WRITE    S139TMP-REC
           ELSE
             REWRITE  S139TMP-REC
           END-IF
      *
           .
       2001-DATA-SET-ETC-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険合計集計場所設定(H20年4月以前)
      *****************************************************************
       2001-IDX-SET3-SEC                SECTION.
      *
      *    入外区分設定
           IF    RECE10-NYUGAIKBN  =  "1"
             MOVE    1    TO    IDX1
             MOVE   "1"    TO    WRK-FLG-NYUGAIKBN
           ELSE
             MOVE    2    TO    IDX1
           END-IF
      *
      *    明細場所設定
           IF    RECE10-HKNNUM  NOT = "060" AND "067"
               IF    RECE10-RJNHKNNUM NOT = SPACE
                 MOVE        5    TO    IDX2
               ELSE
                 IF    RECE10-AGE  <  3
                   MOVE      6    TO    IDX2
                 ELSE
                   IF  RECE10-KOHNUM(1) = SPACE
                     MOVE    1    TO    IDX2
                   ELSE
                     MOVE    3    TO    IDX2
                   END-IF
      *    家族の場合１加算
                   IF    RECE10-HONKZKKBN NOT = "1"
                     ADD     1    TO    IDX2
                   END-IF
                 END-IF
               END-IF
      *    小計加算場所設定
               MOVE          1    TO    IDX3
           ELSE
             EVALUATE  TRUE
             WHEN RECE10-RJNHKNNUM NOT = SPACE
               MOVE        13   TO    IDX2
      
             WHEN RECE10-AGE    <    3
               MOVE        14   TO    IDX2
             WHEN RECE10-HKNNUM = "067"
               IF    RECE10-HONKZKKBN  =  "1"
                 MOVE      11   TO    IDX2
               ELSE
                 MOVE      12   TO    IDX2
               END-IF
             WHEN OTHER
               IF    RECE10-KOHNUM(1)  =  SPACE
                 MOVE      7    TO    IDX2
               ELSE
                 MOVE      9    TO    IDX2
               END-IF
      *    家族の場合１加算
               IF    RECE10-HONKZKKBN NOT = "1"
                 ADD       1    TO    IDX2
               END-IF    
             END-EVALUATE
      *    小計加算場所設定
             MOVE          2    TO    IDX3
           END-IF
      *
      *    公費単独時の設定
           IF    RECE10-HKNNUM = SPACE
             MOVE         15    TO    IDX2
             MOVE         3     TO    IDX3
           END-IF
      *
      *
           .
       2001-IDX-SET3-EXT.
           EXIT.
      * 20080423 H20年4月改正対応START tanaka
      *****************************************************************
      *    保険合計集計場所設定(H20年4月以降)
      *****************************************************************
       2002-IDX-SET3-SEC                SECTION.
      *
      *    入外区分設定
           IF    RECE10-NYUGAIKBN  =  "1"
             MOVE    1    TO    IDX1
             MOVE   "1"    TO    WRK-FLG-NYUGAIKBN
           ELSE
             MOVE    2    TO    IDX1
           END-IF
      *
      *    明細場所設定
           IF    RECE10-HKNNUM  NOT = "060" AND "067"
             IF    RECE10-HKNNUM = "039" OR 
                   RECE10-HKNNUM = "040"
               IF  RECE10-KOHNUM(1) = SPACE
                 MOVE    17    TO    IDX2
               ELSE
                 MOVE    18    TO    IDX2
               END-IF
      *    小計加算場所設定
               MOVE      5     TO    IDX3
             ELSE
               IF    RECE10-RJNHKNNUM NOT = SPACE
                 MOVE        5    TO    IDX2
               ELSE
                 IF    RECE10-FLG(1)  =  1
                   MOVE      6    TO    IDX2
                 ELSE
                   IF  RECE10-KOHNUM(1) = SPACE
                     MOVE    1    TO    IDX2
                   ELSE
                     MOVE    3    TO    IDX2
                   END-IF
      *    家族の場合１加算
                   IF    RECE10-HONKZKKBN NOT = "1"
                     ADD     1    TO    IDX2
                   END-IF
                 END-IF
               END-IF
      *    小計加算場所設定
               MOVE          1    TO    IDX3
             END-IF
           ELSE
             EVALUATE  TRUE
             WHEN RECE10-RJNHKNNUM NOT = SPACE
               MOVE        13   TO    IDX2
             WHEN RECE10-FLG(1)  =  1
               MOVE        14   TO    IDX2
             WHEN RECE10-HKNNUM = "067"
               IF    RECE10-HONKZKKBN  =  "1"
                 MOVE      11   TO    IDX2
               ELSE
                 MOVE      12   TO    IDX2
               END-IF
             WHEN OTHER
               IF    RECE10-KOHNUM(1)  =  SPACE
                 MOVE      7    TO    IDX2
               ELSE
                 MOVE      9    TO    IDX2
               END-IF
      *    家族の場合１加算
               IF    RECE10-HONKZKKBN NOT = "1"
                 ADD       1    TO    IDX2
               END-IF    
             END-EVALUATE
      *    小計加算場所設定
             MOVE          2    TO    IDX3
           END-IF
      *
      *    公費単独時の設定
           IF    RECE10-HKNNUM = SPACE
             MOVE         15    TO    IDX2
             MOVE         3     TO    IDX3
           END-IF
      *
      *
           .
       2001-IDX-SET3-EXT.
           EXIT.
      *
      * 20080423 H20年4月改正対応END
      *****************************************************************
      *    自費・時賠・労災用加算データ設定処理
      *****************************************************************
       2001-KASAN-ETC-SEC                SECTION.
      *
      *
      *    点数
           ADD      SYU-TOTAL-HKNTEN      TO    WRK-TENSU
      *
      *--- (2008/12/18) line add&del start monma -----------------------
      *    窓口請求額
      *     ADD      SYU-SKYMONEY          TO    WRK-MADOGK
           MOVE     ZERO     TO     WRK-JIHIGK
           COMPUTE  WRK-JIHIGK  =  SYU-JIHI-TOTAL
                                +  SYU-JIHI-TOTAL-TAX
           COMPUTE  WRK-MADOGK  =  SYU-SKYMONEY - WRK-JIHIGK
      *--- (2008/12/18) line add&del end   monma -----------------------
      *
      *    保険請求額
           IF     SYU-PTFTNRATE           =     ZERO
             IF     SYU-SYUHKNNUM         =     "971" OR "973"
      *           COMPUTE  WRK-HKNGK  =
      *                                  SYU-TOTAL-HKNTEN 
      *                                  * WRK-JIBAITNK
               IF    SYS-4001-TENKBN    =    "1"
                 COMPUTE  WRK-HKNGK  =
                                       SYU-TOTAL-HKNTEN * 11.5
               ELSE
                 COMPUTE  WRK-HKNGK ROUNDED  =
                                       SYU-TOTAL-HKNTEN * 12
               END-IF
                 COMPUTE  WRK-HKNGK  =  WRK-HKNGK
                                     +  SYU-RSISHOSHIN-MONEY
                                     +  SYU-RSISAISHIN-MONEY
                                     +  SYU-RSISDO-MONEY
                                     +  SYU-RSIETC-MONEY
                                     +  SYU-RYOYOHI-SKJ
      * 20080616 健保準拠時の自賠責の計算を修正START tanaka
               IF   SYU-SYUHKNNUM    =   "973"
                 IF  SYU-PTFTNRATE NOT =  ZERO
                   MOVE ZERO  TO   WRK-HKNGK
                 ELSE
                   COMPUTE WRK-HKNGK  =  SYU-RSIJIBAI-SKYMONEY
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
               END-IF
      * 20080616 健保準拠時の自賠責の計算を修正END
             END-IF
      * 20080423 H20年4月改正対応START tanaka
      *    公害保険の場合
             IF     SYU-SYUHKNNUM  =  "975"
                COMPUTE WRK-JIHI4  =  SYU-TOTAL-TGMONEY
                                   +  SYU-TOTAL-TGMONEY-TAX
                                   +  SYU-JIHI-TOTAL
                                   +  SYU-JIHI-TOTAL-TAX
                                   +  SYU-RMSAGAKU
                                   +  SYU-SKYMONEY-SKJ-JIHI-KEI
               IF      SYU-SRYYMD  <  "20070401"
                 COMPUTE WRK-JIHI4   =   WRK-JIHI4  +  SYU-JIHI-TAX
               END-IF
               COMPUTE   WRK-HKNGK  =  SYU-RSIJIBAI-SKYMONEY
                                    -  WRK-JIHI4
             END-IF
      * 20080423 H20年4月改正対応END
           END-IF
      *
      *--- (2008/12/18) line del start monma ---------------------------
      *    医療収益
      *     COMPUTE  WRK-SYUEKI  =  WRK-HKNGK  +  WRK-MADOGK
      *                          +  WRK-KOHGK
      *--- (2008/12/18) line del end   monma ---------------------------
      *
      ***  20040820  @@@@-MOTOHASI  食事療養追加
      *    日数
           MOVE  SYU-SYUSKJ-NISSU     TO   WRK-SNISSU
      *    請求額
           MOVE  SYU-SYUSKJ-RYOYOHI   TO   WRK-SSEIKYU
      *    標準負担額
           MOVE  SYU-SYUSKJ-FTN       TO   WRK-SFTNGK
      ***  20040820  @@@@-MOTOHASI  
      *
      *--- (2008/12/18) line add start monma ---------------------------
      *    医療収益 食事収益加算
           COMPUTE  WRK-SYUEKI  =  WRK-HKNGK  +  WRK-MADOGK
                                +  WRK-KOHGK  +  WRK-JIHIGK
                                +  (WRK-SSEIKYU - WRK-SFTNGK)
      *--- (2008/12/18) line add end   monma ---------------------------
      *
           .
       2001-KASAN-ETC-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
      *    全保険合計印刷処理
           PERFORM 2002-PRINT-TYPE3-SEC
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    全保険合計印刷処理
      *****************************************************************
       2002-PRINT-TYPE3-SEC                SECTION.
      *
           OPEN     INPUT        S139TMP-FILE
           MOVE     ZERO        TO   FLG-PRINT
           INITIALIZE                S139TMP-REC
      *
           MOVE    3            TO   FLG-TYPE
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT   =    1
      *
      *
      ***  20040820  @@@@-MOTOHASI  追加
      *    入院フォーム
      *    見出し編集
             IF    WRK-FLG-NYUGAIKBN     =  "1"
               MOVE     1       TO   WRK-CNT1
               PERFORM  3104-HEAD-HEN-SEC
      *
      *    明細編集
               PERFORM  3204-BODY-HEN-SEC
      *
      *    印刷処理
               PERFORM  340-PRINT-OUT-SEC
             END-IF
      *
      ***  20040820  @@@@-MOTOHASI
      *
      *    外来フォーム
      *
      ***  20040820  @@@@-MOTOHASI  追加
      *    見出し編集
      ***       IF    WRK-PARA-NYUGAIKBN     =  "2"  OR  SPACE
      ***  20040820  @@@@-MOTOHASI  
      *
             MOVE     1       TO   WRK-CNT1
             PERFORM  3103-HEAD-HEN-SEC
      *
      *    明細編集
             PERFORM  3203-BODY-HEN-SEC
      *
      *    印刷処理
             PERFORM  330-PRINT-OUT-SEC
      **    END-IF
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    S139TMP-FILE
      *
           .
       2002-PRINT-TYPE3-EXT.
           EXIT.
      *
      *****************************************************************
      *    全保険帳票編集<ヘッダー部>処理
      *****************************************************************
       3103-HEAD-HEN-SEC                 SECTION.
      **
           INITIALIZE                      S139
      *
      *    ページ数
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    S139-PAGE
      *
      *    診療科名(旧総合病院時のみ)
      * 20081014 主科設定時に診療科を印字するSTART
           IF    SYS-1001-HOSPSBT1  =  1
      * 20081014 主科設定時に診療科を印字するEND
             MOVE     1               TO    IDX4
             MOVE     SUM-SRYKAMAX    TO    IDX-MAX
             PERFORM  UNTIL    IDX4   >   IDX-MAX
      *--- 2008/11/06  CHANGE  START --------------------------------------
      *         IF  S139TMP-SRYKA       =     SUM-SRYKA(IDX4)
               IF   S139TMP-SRYKA   =   SUM-SRYKA2(IDX4)
      *--- 2008/11/06  CHANGE  END ----------------------------------------
                 MOVE    ZERO                   TO    IDX-MAX
                 MOVE    SUM-SRYKANAME(IDX4)    TO    S139-SRYKANAME
               ELSE
                 ADD     1                      TO    IDX4
               END-IF
             END-PERFORM
           END-IF
      *--- 2008/11/06  ADD  START --------------------------------------
           IF   SYS-2009-SYUKA-KBN = 1
             MOVE     1               TO    IDX4
             MOVE     SUM-SRYKAMAX    TO    IDX-MAX
             PERFORM  UNTIL    IDX4   >     IDX-MAX
               IF   S139TMP-SRYKA   =   SUM-SRYKA(IDX4)
                 MOVE    ZERO                   TO    IDX-MAX
                 MOVE    SUM-SRYKANAME(IDX4)    TO    S139-SRYKANAME
               ELSE
                 ADD     1                      TO    IDX4
               END-IF
             END-PERFORM
           END-IF
      *--- 2008/11/06  ADD  END ----------------------------------------
      *
           MOVE    WRK-SRYYM        TO   S139-SRYYM
           MOVE    WRK-SEIYMD       TO   S139-SYSYMD
      *
           .
       3103-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全保険帳票編集<ヘッダー部>処理
      *****************************************************************
       3104-HEAD-HEN-SEC                 SECTION.
      **
           INITIALIZE                      S139N
      *
      *    ページ数
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    S139N-PAGE
      *
      *    診療科名(旧総合病院時のみ)
      * 20081014 主科設定時に診療科を印字するSTART
           IF    SYS-1001-HOSPSBT1  =  1
      *--- 2008/11/06  ADD  START --------------------------------------
                   OR  SYS-2009-SYUKA-KBN = 1
      *--- 2008/11/06  ADD  END ----------------------------------------
      * 20081014 主科設定時に診療科を印字するEND
             MOVE     1               TO    IDX4
             MOVE     SUM-SRYKAMAX    TO    IDX-MAX
             PERFORM  UNTIL   IDX4  >  IDX-MAX
      *--- 2008/11/06  CHANGE  START --------------------------------------
      *         IF  S139TMP-SRYKA       =     SUM-SRYKA(IDX4)
               IF  S139TMP-SRYKA       =     SUM-SRYKA2(IDX4)
      *--- 2008/11/06  CHANGE  END ----------------------------------------
                 MOVE    ZERO                   TO   IDX-MAX
                 MOVE    SUM-SRYKANAME(IDX4)    TO   S139N-SRYKANAME
               ELSE
                 ADD     1                      TO   IDX4
               END-IF
             END-PERFORM
           END-IF
      *
      *--- 2008/11/06  ADD  START --------------------------------------
           IF   SYS-2009-SYUKA-KBN = 1
             MOVE    1               TO    IDX4
             MOVE    SUM-SRYKAMAX    TO    IDX-MAX
             PERFORM  UNTIL   IDX4   >   IDX-MAX
               IF  S139TMP-SRYKA   =   SUM-SRYKA(IDX4)
                 MOVE    ZERO                   TO    IDX-MAX
                 MOVE    SUM-SRYKANAME(IDX4)    TO    S139-SRYKANAME
               ELSE
                 ADD     1                      TO    IDX4
               END-IF
             END-PERFORM
           END-IF
      *--- 2008/11/06  ADD  END ----------------------------------------
      *
           MOVE    WRK-SRYYM        TO   S139N-SRYYM
           MOVE    WRK-SEIYMD       TO   S139N-SYSYMD
      *
           .
       3104-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全保険帳票編集<明細>処理(外来)
      *****************************************************************
       3203-BODY-HEN-SEC               SECTION.
      *
           PERFORM    VARYING  IDX1  FROM  2  BY  1
                      UNTIL    IDX1  >     2
      * 20080423 H20年4月改正対応START tanaka
      *       PERFORM  VARYING  IDX2  FROM  1  BY  1
      *                UNTIL    IDX2  >     16
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     18
      * 20080423 H20年4月改正対応END
      *
      *    件数
               MOVE     S139TMP-KENSU(IDX1 IDX2)    TO    WRK-Z4
               MOVE     WRK-Z4           TO    S139-KENSU(1 IDX2)
      *
      *    日数
               MOVE     S139TMP-NISSU(IDX1 IDX2)    TO    WRK-Z4
               MOVE     WRK-Z4           TO    S139-NISSU(1 IDX2)
      *
      *    点数
               MOVE     S139TMP-TENSU(IDX1 IDX2)    TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139-TENSU(1 IDX2)
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *         MOVE     S139TMP-MADOGK(IDX1 IDX2)   TO    WRK-Z92
      *         MOVE     WRK-Z92          TO    S139-MADOGK(1 IDX2)
               MOVE     S139TMP-MADOGK(IDX1 IDX2)   TO    WRK-Z93
               MOVE     WRK-Z93          TO    S139-MADOGK(1 IDX2)
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/18) line add start monma ---------------------------
      *    自費
               MOVE     S139TMP-JIHIGK(IDX1 IDX2)   TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139-JIHIGK(1 IDX2)
      *--- (2008/12/18) line add end   monma ---------------------------
      *
      *    保険請求額
               MOVE     S139TMP-HKNGK(IDX1 IDX2)    TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139-HKNGK(1 IDX2)
      *
      *    公費請求額
               MOVE     S139TMP-KOHGK(IDX1 IDX2)   TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139-KOHGK(1 IDX2)
      *
      *    医療収益
               MOVE     S139TMP-SYUEKI(IDX1 IDX2)   TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139-SYUEKI(1 IDX2)
             END-PERFORM
      *
      *    小計
      * 20080423 H20年4月改正対応START tanaka
      *       PERFORM  VARYING  IDX2  FROM  1  BY  1
      *                UNTIL    IDX2  >     4
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     5
      * 20080423 H20年4月改正対応END
      *
      *    件数
               MOVE     S139TMP-S-KENSU(IDX1 IDX2)    TO    WRK-Z4
               MOVE     WRK-Z4       TO    S139-S-KENSU(1 IDX2)
      *
      *    日数
               MOVE     S139TMP-S-NISSU(IDX1 IDX2)    TO    WRK-Z4
               MOVE     WRK-Z4       TO    S139-S-NISSU(1 IDX2)
      *
      *    点数
               MOVE     S139TMP-S-TENSU(IDX1 IDX2)    TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139-S-TENSU(1 IDX2)
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *         MOVE     S139TMP-S-MADOGK(IDX1 IDX2)   TO    WRK-Z92
      *         MOVE     WRK-Z92      TO    S139-S-MADOGK(1 IDX2)
               MOVE     S139TMP-S-MADOGK(IDX1 IDX2)   TO    WRK-Z93
               MOVE     WRK-Z93      TO    S139-S-MADOGK(1 IDX2)
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/18) line add start monma ---------------------------
      *    自費
               MOVE     S139TMP-S-JIHIGK(IDX1 IDX2)   TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139-S-JIHIGK(1 IDX2)
      *--- (2008/12/18) line add end   monma ---------------------------
      *
      *    保険請求額
               MOVE     S139TMP-S-HKNGK(IDX1 IDX2)    TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139-S-HKNGK(1 IDX2)
      *
      *    公費請求額
               MOVE     S139TMP-S-KOHGK(IDX1 IDX2)   TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139-S-KOHGK(1 IDX2)
      *
      *    医療収益
               MOVE     S139TMP-S-SYUEKI(IDX1 IDX2)   TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139-S-SYUEKI(1 IDX2)
      *
             END-PERFORM
      * 20080423 H20年4月改正対応START tanaka
      *       PERFORM  VARYING  IDX2  FROM  1  BY  1
      *                UNTIL    IDX2  >     4
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     5
      *    自費・労災・自賠・公害
      * 20080423 H20年4月改正対応END
      *    件数
               MOVE     S139TMP-ETC-KENSU(IDX1 IDX2)    TO    WRK-Z4
               MOVE     WRK-Z4       TO    S139-ETC-KENSU(1 IDX2)
      *
      *    日数
               MOVE     S139TMP-ETC-NISSU(IDX1 IDX2)    TO    WRK-Z4
               MOVE     WRK-Z4       TO    S139-ETC-NISSU(1 IDX2)
      *
      *    点数
               MOVE     S139TMP-ETC-TENSU(IDX1 IDX2)    TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139-ETC-TENSU(1 IDX2)
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *         MOVE   S139TMP-ETC-MADOGK(IDX1 IDX2)   TO  WRK-Z92
      *         MOVE   WRK-Z92    TO    S139-ETC-MADOGK(1 IDX2)
               MOVE   S139TMP-ETC-MADOGK(IDX1 IDX2)   TO  WRK-Z93
               MOVE   WRK-Z93    TO    S139-ETC-MADOGK(1 IDX2)
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/18) line add start monma ---------------------------
      *    自費
               MOVE   S139TMP-ETC-JIHIGK(IDX1 IDX2)   TO  WRK-Z92
               MOVE   WRK-Z92    TO    S139-ETC-JIHIGK(1 IDX2)
      *--- (2008/12/18) line add end   monma ---------------------------
      *
      *    保険請求額
               MOVE   S139TMP-ETC-HKNGK(IDX1 IDX2)    TO  WRK-Z92
               MOVE   WRK-Z92    TO    S139-ETC-HKNGK(1 IDX2)
      *
      *    医療収益
               MOVE   S139TMP-ETC-SYUEKI(IDX1 IDX2)   TO  WRK-Z92
               MOVE   WRK-Z92    TO    S139-ETC-SYUEKI(1 IDX2)
             END-PERFORM
           END-PERFORM
      *
      *    総合計
      * 20080423 H20年4月改正対応START tanaka
      *     PERFORM  VARYING  IDX3   FROM  1  BY  1
      *              UNTIL    IDX3   >     5
           PERFORM  VARYING  IDX3   FROM  1  BY  1
                    UNTIL    IDX3   >     6
      * 20080423 H20年4月改正対応END
      *
      *    件数
               MOVE     S139TMP-GKENSU(IDX3)    TO    WRK-Z4
               MOVE     WRK-Z4           TO    S139-GKENSU(IDX3)
      *
      *    日数
               MOVE     S139TMP-GNISSU(IDX3)    TO    WRK-Z4
               MOVE     WRK-Z4           TO    S139-GNISSU(IDX3)
      *
      *    点数
               MOVE     S139TMP-GTENSU(IDX3)    TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139-GTENSU(IDX3)
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *         MOVE     S139TMP-GMADOGK(IDX3)   TO    WRK-Z92
      *         MOVE     WRK-Z92          TO    S139-GMADOGK(IDX3)
               MOVE     S139TMP-GMADOGK(IDX3)   TO    WRK-Z93
               MOVE     WRK-Z93          TO    S139-GMADOGK(IDX3)
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/18) line add start monma ---------------------------
      *    自費
               MOVE     S139TMP-GJIHIGK(IDX3)   TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139-GJIHIGK(IDX3)
      *--- (2008/12/18) line add end   monma ---------------------------
      *
      *    保険請求額
               MOVE     S139TMP-GHKNGK(IDX3)    TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139-GHKNGK(IDX3)
      *
      *    公費請求額
               MOVE     S139TMP-GKOHGK(IDX3)    TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139-GKOHGK(IDX3)
      *
      *    医療収益
               MOVE     S139TMP-GSYUEKI(IDX3)   TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139-GSYUEKI(IDX3)
      ***  食事療養
      *    日数
               MOVE     S139TMP-SGNISSU(IDX3)   TO    WRK-Z4
               MOVE     WRK-Z4       TO    S139-SGNISSU(IDX3)
      *
      *    請求額
               MOVE     S139TMP-SGSEIKYU(IDX3)  TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139-SGSEIKYU(IDX3)
      *
      *    標準負担額
               MOVE     S139TMP-SGFTNGK(IDX3)   TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139-SGFTNGK(IDX3)
           END-PERFORM
      *     
           .
       3203-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全保険帳票編集<明細>処理(入院)
      *****************************************************************
       3204-BODY-HEN-SEC               SECTION.
      *
           PERFORM    VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     1
      * 20080423 H20年4月改正対応START tanaka
      *       PERFORM  VARYING  IDX2  FROM  1  BY  1
      *                UNTIL    IDX2  >     16
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     18
      * 20080423 H20年4月改正対応END
      *
      *    件数
               MOVE     S139TMP-KENSU(IDX1 IDX2)    TO    WRK-Z4
               MOVE     WRK-Z4           TO    S139N-KENSU(IDX1 IDX2)
      *
      *    日数
               MOVE     S139TMP-NISSU(IDX1 IDX2)    TO    WRK-Z4
               MOVE     WRK-Z4           TO    S139N-NISSU(IDX1 IDX2)
      *
      *    点数
               MOVE     S139TMP-TENSU(IDX1 IDX2)    TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139N-TENSU(IDX1 IDX2)
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *         MOVE     S139TMP-MADOGK(IDX1 IDX2)   TO    WRK-Z92
      *         MOVE     WRK-Z92          TO    S139N-MADOGK(IDX1 IDX2)
               MOVE     S139TMP-MADOGK(IDX1 IDX2)   TO    WRK-Z93
               MOVE     WRK-Z93          TO    S139N-MADOGK(IDX1 IDX2)
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/18) line add start monma ---------------------------
      *    自費
               MOVE     S139TMP-JIHIGK(IDX1 IDX2)   TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139N-JIHIGK(IDX1 IDX2)
      *--- (2008/12/18) line add end   monma ---------------------------
      *
      *    保険請求額
               MOVE     S139TMP-HKNGK(IDX1 IDX2)    TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139N-HKNGK(IDX1 IDX2)
      *
      *    公費請求額
               MOVE     S139TMP-KOHGK(IDX1 IDX2)   TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139N-KOHGK(IDX1 IDX2)
      *
      *    医療収益
               MOVE     S139TMP-SYUEKI(IDX1 IDX2)   TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139N-SYUEKI(IDX1 IDX2)
      ***  食事療養
      *    日数
               MOVE     S139TMP-SNISSU(IDX1 IDX2)    TO    WRK-Z4
               MOVE     WRK-Z4           TO    S139N-SNISSU(IDX1 IDX2)
      *
      *    請求額
               MOVE     S139TMP-SSEIKYU(IDX1 IDX2)   TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139N-SSEIKYU(IDX1 IDX2)
      *
      *    標準負担額
               MOVE     S139TMP-SFTNGK(IDX1 IDX2)    TO    WRK-Z92
               MOVE     WRK-Z92          TO    S139N-SFTNGK(IDX1 IDX2)
             END-PERFORM
      *
      *    小計
      * 20080423 H20年4月改正対応START tanaka
      *       PERFORM  VARYING  IDX2  FROM  1  BY  1
      *                UNTIL    IDX2  >     4
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     5
      * 20080423 H20年4月改正対応END
      *
      *    件数
               MOVE     S139TMP-S-KENSU(IDX1 IDX2)    TO    WRK-Z4
               MOVE     WRK-Z4       TO    S139N-S-KENSU(IDX1 IDX2)
      *
      *    日数
               MOVE     S139TMP-S-NISSU(IDX1 IDX2)    TO    WRK-Z4
               MOVE     WRK-Z4       TO    S139N-S-NISSU(IDX1 IDX2)
      *
      *    点数
               MOVE     S139TMP-S-TENSU(IDX1 IDX2)    TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139N-S-TENSU(IDX1 IDX2)
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *         MOVE     S139TMP-S-MADOGK(IDX1 IDX2)   TO    WRK-Z92
      *         MOVE     WRK-Z92      TO    S139N-S-MADOGK(IDX1 IDX2)
               MOVE     S139TMP-S-MADOGK(IDX1 IDX2)   TO    WRK-Z93
               MOVE     WRK-Z93      TO    S139N-S-MADOGK(IDX1 IDX2)
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/18) line add start monma ---------------------------
      *    自費
               MOVE     S139TMP-S-JIHIGK(IDX1 IDX2)   TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139N-S-JIHIGK(IDX1 IDX2)
      *--- (2008/12/18) line add end   monma ---------------------------
      *
      *    保険請求額
               MOVE     S139TMP-S-HKNGK(IDX1 IDX2)    TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139N-S-HKNGK(IDX1 IDX2)
      *
      *    公費請求額
               MOVE     S139TMP-S-KOHGK(IDX1 IDX2)   TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139N-S-KOHGK(IDX1 IDX2)
      *
      *    医療収益
               MOVE     S139TMP-S-SYUEKI(IDX1 IDX2)   TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139N-S-SYUEKI(IDX1 IDX2)
      ***  食事療養
      *    日数
               MOVE     S139TMP-S-SNISSU(IDX1 IDX2)    TO    WRK-Z4
               MOVE     WRK-Z4       TO    S139N-S-SNISSU(IDX1 IDX2)
      *
      *    請求額
               MOVE     S139TMP-S-SSEIKYU(IDX1 IDX2)   TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139N-S-SSEIKYU(IDX1 IDX2)
      *
      *    標準負担額
               MOVE     S139TMP-S-SFTNGK(IDX1 IDX2)    TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139N-S-SFTNGK(IDX1 IDX2)
      *
      *         
      * 20080423 H20年4月改正対応START tanaka
             END-PERFORM
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     5
      *    自費・労災・自賠・公害
      * 20080310 公害保険対応END
      *    件数
               MOVE     S139TMP-ETC-KENSU(IDX1 IDX2)    TO    WRK-Z4
               MOVE     WRK-Z4       TO    S139N-ETC-KENSU(IDX1 IDX2)
      *
      *    日数
               MOVE     S139TMP-ETC-NISSU(IDX1 IDX2)    TO    WRK-Z4
               MOVE     WRK-Z4       TO    S139N-ETC-NISSU(IDX1 IDX2)
      *
      *    点数
               MOVE     S139TMP-ETC-TENSU(IDX1 IDX2)    TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139N-ETC-TENSU(IDX1 IDX2)
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *         MOVE   S139TMP-ETC-MADOGK(IDX1 IDX2)   TO  WRK-Z92
      *         MOVE   WRK-Z92    TO    S139N-ETC-MADOGK(IDX1 IDX2)
               MOVE   S139TMP-ETC-MADOGK(IDX1 IDX2)   TO  WRK-Z93
               MOVE   WRK-Z93    TO    S139N-ETC-MADOGK(IDX1 IDX2)
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/18) line add start monma ---------------------------
      *    自費
               MOVE   S139TMP-ETC-JIHIGK(IDX1 IDX2)   TO  WRK-Z92
               MOVE   WRK-Z92    TO    S139N-ETC-JIHIGK(IDX1 IDX2)
      *--- (2008/12/18) line add end   monma ---------------------------
      *
      *    保険請求額
               MOVE   S139TMP-ETC-HKNGK(IDX1 IDX2)    TO  WRK-Z92
               MOVE   WRK-Z92    TO    S139N-ETC-HKNGK(IDX1 IDX2)
      *
      *    医療収益
               MOVE   S139TMP-ETC-SYUEKI(IDX1 IDX2)   TO  WRK-Z92
               MOVE   WRK-Z92    TO    S139N-ETC-SYUEKI(IDX1 IDX2)
      ***  食事療養
      *    日数
               MOVE     S139TMP-ETC-SNISSU(IDX1 IDX2)    TO    WRK-Z4
               MOVE     WRK-Z4       TO    S139N-ETC-SNISSU(IDX1 IDX2)
      *
      *    請求額
               MOVE     S139TMP-ETC-SSEIKYU(IDX1 IDX2)   TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139N-ETC-SSEIKYU(IDX1 IDX2)
      *
      *    標準負担額
               MOVE     S139TMP-ETC-SFTNGK(IDX1 IDX2)    TO    WRK-Z92
               MOVE     WRK-Z92      TO    S139N-ETC-SFTNGK(IDX1 IDX2)
             END-PERFORM
           END-PERFORM
      *
      ****    総合計
      ***     PERFORM  VARYING  IDX3   FROM  1  BY  1
      ***              UNTIL    IDX3   >     5
      *
      *    件数
      ***         MOVE     S139TMP-GKENSU(IDX3)    TO    WRK-Z4
      ***         MOVE     WRK-Z4           TO    S139N-GKENSU(IDX3)
      *
      *    日数
      ***         MOVE     S139TMP-GNISSU(IDX3)    TO    WRK-Z4
      ***         MOVE     WRK-Z4           TO    S139N-GNISSU(IDX3)
      *
      *    点数
      ***         MOVE     S139TMP-GTENSU(IDX3)    TO    WRK-Z92
      ***         MOVE     WRK-Z92          TO    S139N-GTENSU(IDX3)
      *
      *    窓口請求額
      ***         MOVE     S139TMP-GMADOGK(IDX3)   TO    WRK-Z92
      ***         MOVE     WRK-Z92          TO    S139N-GMADOGK(IDX3)
      *
      *    保険請求額
      ***         MOVE     S139TMP-GHKNGK(IDX3)    TO    WRK-Z92
      ***         MOVE     WRK-Z92          TO    S139N-GHKNGK(IDX3)
      *
      *    公費請求額
      ***         MOVE     S139TMP-GKOHGK(IDX3)   TO    WRK-Z92
      ***         MOVE     WRK-Z92          TO    S139N-GKOHGK(IDX3)
      *
      *    医療収益
      ***         MOVE     S139TMP-GSYUEKI(IDX3)   TO    WRK-Z92
      ***         MOVE     WRK-Z92          TO    S139N-GSYUEKI(IDX3)
      ***     END-PERFORM
      *     
           .
       3204-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       330-PRINT-OUT-SEC         SECTION.
      *
      * 
           INITIALIZE                          ORCSPRTAREA
           MOVE    "INS"                       TO   SPRT-MODE
           MOVE    S100LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
           MOVE    S100LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
           MOVE    S100LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
           MOVE    S100LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
           MOVE    S100LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
           MOVE    S100LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM  TO   SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY      TO   SPRT-PRIORITY
      ***  20040917 MOTOHASI MONPEに変更
      ***     MOVE    "A00000S139"        TO  SPRT-PRTID
      * 20080423 H20年4月改正対応START tanaka
      *     MOVE    "A00000S139.red"        TO  SPRT-PRTID
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
      *     IF S100LNK-YM < "200804"
           IF    S139TMP-SRYYMKBN   =  "0"
             MOVE    "A00000S139O.red"        TO   SPRT-PRTID
           ELSE
             MOVE    "A00000S139.red"         TO   SPRT-PRTID
           END-IF
      * 20080512 返戻が正しく印字されない不具合を修正END
      * 20080423 H20年4月改正対応END
      ***  20040917 MOTOHASI MONPEに変更
           MOVE    S100LNK-SPRT-TITLE         TO   SPRT-TITLE
           MOVE    S139                       TO   SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID    TO   SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID      TO   SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM     TO   SPRT-PRTNM
      ***** site-libで動かすためにコメントアウト
      ***     MOVE    "1"          TO  SPRT-SITEKBN
      *****
           CALL    "ORCSPRT"                  USING
                                              ORCSPRTAREA
      ***07/09/20 nagaya 4.0対応 start
                                              SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF  SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
             MOVE    "印刷ＤＢに更新できませんでした"   TO  WRK-RECEERR
	         PERFORM 500-ERR-HENSYU-SEC
	       END-IF
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       340-PRINT-OUT-SEC         SECTION.
      *
      * 
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                       TO   SPRT-MODE
           MOVE    S100LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
           MOVE    S100LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
           MOVE    S100LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
           MOVE    S100LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
           MOVE    S100LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
           MOVE    S100LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
      ***  20040917 MOTOHASI MONPEに変更
      ***     MOVE    "A00000S139N"        TO  SPRT-PRTID
      * 20080423 H20年4月改正対応START tanaka
      *     MOVE    "A00000S139N.red"        TO  SPRT-PRTID
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
      *     IF S100LNK-YM < "200804"
           IF S139TMP-SRYYMKBN  =  "0"
             MOVE    "A00000S139NO.red"        TO   SPRT-PRTID
           ELSE
             MOVE    "A00000S139N.red"         TO   SPRT-PRTID
           END-IF
      * 20080512 返戻が正しく印字されない不具合を修正END
      * 20080423 H20年4月改正対応END
      ***  20040917 MOTOHASI MONPEに変更
           MOVE    S100LNK-SPRT-TITLE          TO   SPRT-TITLE
           MOVE    S139N                       TO   SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID     TO   SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID       TO   SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM      TO   SPRT-PRTNM
      ***** site-libで動かすためにコメントアウト
      ***     MOVE    "1"          TO  SPRT-SITEKBN
      *****
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF  SPRT-RETURN   =   ZERO
             CONTINUE             
           ELSE
             MOVE    "印刷ＤＢに更新できませんでした"
                                     TO  WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
	       END-IF
           .
       340-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    S100LNK-PRTKANRI-SKYYMD     TO  ORC-DBYMD
      ***07/09/20 nagaya 4.0対応 end
           MOVE    "DBSELECT"          TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      * 20080922 労災自賠医療機関情報が読み込めない不具合を修正START
      *     MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "key10"               TO  MCP-PATHNAME
      * 20080922 労災自賠医療機関情報が読み込めない不具合を修正END
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *         MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
      * 20080922 労災自賠医療機関情報が読み込めない不具合を修正START
      *         MOVE    "key"               TO  MCP-PATHNAME
             MOVE    "key10"             TO  MCP-PATHNAME
      * 20080922 労災自賠医療機関情報が読み込めない不具合を修正END
             CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                         MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                      ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                         MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                         SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
             IF   MCP-RC    =   ZERO
               MOVE    ZERO       TO   FLG-SYSKANRI
             ELSE
               MOVE    1          TO   FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1            TO   FLG-SYSKANRI
           END-IF
      *
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO   MCP-TABLE
      * 20080922 労災自賠医療機関情報が読み込めない不具合を修正START
      *     MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "key10"             TO   MCP-PATHNAME
      * 20080922 労災自賠医療機関情報が読み込めない不具合を修正END
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-SYMD              TO  ORC-DBYMD
      ***07/09/20 nagaya 4.0対応 end
           MOVE    SYS-1005-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"        TO   MCP-TABLE
           MOVE    "key2"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
      ***07/09/20 nagaya 4.0対応 end
                                         MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                         MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                         SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF  MCP-RC   =   ZERO
             PERFORM 820-SYSKANRI-DBFETCH-SEC
           ELSE
             MOVE    1           TO    FLG-SYSKANRI3
      *=== sarge対応 DEL By Takashima(2006/09/17) start
      *         PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/17) end
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
           MOVE    "DBFETCH"            TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
             MOVE    ZERO               TO   FLG-SYSKANRI3
             MOVE    MCPDATA-REC        TO   SYS-1005-REC
           ELSE
             MOVE    1                  TO   FLG-SYSKANRI3
      *=== sarge対応 DEL By Takashima(2006/09/17) start
      *         PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/17) end
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
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE  "DBCLOSE"                TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"        TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY2"  TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE  "tbl_syskanri"         TO   MCP-TABLE
           MOVE  "key2"                 TO   MCP-PATHNAME
           CALL  "ORCDBMAIN"            USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           .
       820-SYSKANRI-DBCLOSE-EXT.
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
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "  "
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
           MOVE    LNK-DAY7-KOYOMI    TO   WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
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
             OPEN    OUTPUT              RECEERR-FILE
      *
             MOVE    WRK-RECEERR       TO   RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ終了処理
             MOVE    "JBE"             TO   SJOBKANRI-MODE
             INITIALIZE                  JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
             MOVE    SPA-HOSPNUM       TO   JOB-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
             MOVE    S100LNK-JOBID     TO   JOB-JOBID
             MOVE    S100LNK-SHELLID   TO   JOB-SHELLID
             MOVE    WRK-RECEERR       TO   JOB-YOBI
             MOVE    "9999"            TO   JOB-ERRCD
             CALL    "ORCSJOB"         USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           END-IF
      *
           MOVE    1        TO    FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ    S139TMP-FILE
             INVALID    KEY
               MOVE    1           TO   FLG-S139TMP
             NOT INVALID  KEY
               MOVE    ZERO        TO   FLG-S139TMP
           END-READ
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************^M
       800-SYSKANRI-READ-SEC           SECTION.
      *
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-SYSYMD          TO  ORC-DBYMD
      ***07/09/20 nagaya 4.0対応 end
           MOVE    "DBSELECT"          TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY10"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"      TO   MCP-TABLE
           MOVE    "key10"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                 ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *          MOVE    "SYSKANRI-KEY10"      TO  ORC-DBPATH
      *          CALL    "ORCMCPSUB"          USING
             MOVE    "tbl_syskanri"    TO   MCP-TABLE
             MOVE    "key10"           TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                 ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
             IF   MCP-RC   =   ZERO
               MOVE    ZERO            TO   FLG-SYSKANRI
             ELSE
               MOVE    1               TO   FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                 TO   FLG-SYSKANRI
           END-IF
      *
      *=== sarge対応 Takashima(2006/08/09)
      *      MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *      MOVE    "SYSKANRI-KEY10"      TO  ORC-DBPATH
      *      CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO   MCP-TABLE
           MOVE    "key10"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                   ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *
            .
       800-SYSKANRI-READ-EXT.
            EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  S139TMP-FILE   NEXT
             AT  END
               MOVE    1    TO   FLG-PRINT
             NOT AT END
               ADD     1    TO   CNT-READ
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE   WRK-PARA-HOSPID      TO    RECE10-HOSPID.
           MOVE   WRK-PARA-HOSPNUM     TO   RECE10-HOSPNUM.
           MOVE   "0"                  TO   RECE10-XXSTHKNJANUM
           MOVE   "9999999"            TO   RECE10-XXEDHKNJANUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE   WRK-PARA-SRYYM       TO   RECE10-SKYYM.
           MOVE   WRK-PARA-NYUGAIKBN   TO   RECE10-NYUGAIKBN.
           MOVE    RECE10-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
      *     MOVE    "SEIKYU-KEY4"       TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-PATH            TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_seikyu"        TO   MCP-TABLE
      * 20080423 H20年4月改正対応START tanaka
      *     IF WRK-PATH = "SEIKYU-KEY4"
      *           MOVE    "key4"        TO  MCP-PATHNAME
      *     ELSE
      *           MOVE    "key3"        TO  MCP-PATHNAME
      *     END-IF
           EVALUATE  WRK-PATH
             WHEN  "SEIKYU-KEY4"
               MOVE    "key4"          TO   MCP-PATHNAME
             WHEN  "SEIKYU-KEY3"
               MOVE    "key3"          TO   MCP-PATHNAME
             WHEN  "SEIKYU-ALL"
               MOVE    "all"           TO   MCP-PATHNAME
           END-EVALUATE
      * 20080423 H20年4月改正対応END
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
             PERFORM 900-SEIKYU-READ-SEC
           ELSE
             MOVE    1                 TO   FLG-END
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      
      *     MOVE    "SEIKYU-KEY4"       TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-PATH            TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 end
           MOVE    "DBFETCH"            TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_seikyu"         TO   MCP-TABLE
      * 20080423 H20年4月改正対応START tanaka
      *     IF WRK-PATH = "SEIKYU-KEY4"
      *           MOVE    "key4"        TO  MCP-PATHNAME
      *     ELSE
      *           MOVE    "key3"        TO  MCP-PATHNAME
      *     END-IF
           EVALUATE  WRK-PATH
             WHEN  "SEIKYU-KEY4"
               MOVE    "key4"         TO   MCP-PATHNAME
             WHEN  "SEIKYU-KEY3"
               MOVE    "key3"         TO   MCP-PATHNAME
             WHEN  "SEIKYU-ALL"
               MOVE    "all"          TO   MCP-PATHNAME
           END-EVALUATE
      * 20080423 H20年4月改正対応END
           CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                      MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                      MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                      SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC    =   ZERO
             MOVE    ZERO             TO   FLG-END
             MOVE    MCPDATA-REC      TO   RECE10-REC
      
      *    テスト患者のときは読み飛ばす
             MOVE    SPACE            TO   PTINF-REC
             INITIALIZE               PTINF-REC
      *=== テスト患者の判定修正 Uchida(2007/05/08)
      *         MOVE    RECE10-HOSPID       TO  PTINF-HOSPID
      *         MOVE    RECE10-PTID         TO  PTINF-PTID
      *         MOVE    PTINF-REC           TO  MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
      *         MOVE    RECE10-HOSPID       TO  WRK-PARA-HOSPID
             MOVE    RECE10-HOSPNUM      TO  WRK-PARA-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
             MOVE    RECE10-PTID         TO  WRK-PTID
      *=== テスト患者の判定修正 Uchida(2007/05/08)
             PERFORM 900-PTINF-READ-SEC
             IF      FLG-PTINF           =   ZERO
                 AND     PTINF-TSTPTNUMKBN   =   "1"
               GO  TO  900-SEIKYU-READ-SEC
             END-IF
      **    ０９１と０５１を同じ公費として扱う
             PERFORM  VARYING  IDX5  FROM  1  BY  1
                      UNTIL    IDX5  >  4  
                      OR   RECE10-KOHNUM(IDX5)  =  SPACE
               IF    RECE10-KOHNUM(IDX5)  =  "091"
                 MOVE    "051"      TO   RECE10-KOHNUM(IDX5)
               END-IF
             END-PERFORM
           ELSE
             MOVE    1              TO   FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *****************************************************************
      *    請求管理情報クローズ処理
      *****************************************************************
       900-SEIKYU-CLOSE-SEC                 SECTION.
      *
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE    "SEIKYU-KEY4"       TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-PATH            TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_seikyu"        TO   MCP-TABLE
      * 20080423 H20年4月改正対応START tanaka
      *     IF WRK-PATH = "SEIKYU-KEY4"
      *           MOVE    "key4"        TO  MCP-PATHNAME
      *     ELSE
      *           MOVE    "key3"        TO  MCP-PATHNAME
      *     END-IF
           EVALUATE  WRK-PATH
             WHEN  "SEIKYU-KEY4"
               MOVE    "key4"          TO   MCP-PATHNAME
             WHEN  "SEIKYU-KEY3"
               MOVE    "key3"          TO   MCP-PATHNAME
             WHEN  "SEIKYU-ALL"
               MOVE    "all"           TO   MCP-PATHNAME
           END-EVALUATE
      * 20080423 H20年4月改正対応END
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-PARA-HOSPID     TO  PTINF-HOSPID
           MOVE    WRK-PARA-HOSPNUM    TO   PTINF-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    WRK-PTID            TO   PTINF-PTID
           MOVE    PTINF-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptinf"         TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *         MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"         USING
             MOVE    "tbl_ptinf"       TO   MCP-TABLE
             MOVE    "key"             TO   MCP-PATHNAME
            CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                 ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
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
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptinf"         TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報検索
      *****************************************************************
       920-DBSELECT3-SEC                 SECTION.
      *
           INITIALIZE                    JYURRK-REC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-PARA-HOSPID   TO  JYURRK-HOSPID
           MOVE    WRK-PARA-HOSPNUM     TO   JYURRK-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    WRK-NYUGAIKBN        TO   JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-PRTSYMD     TO   JYURRK-SRYYMD
           MOVE    WRK-PARA-PRTLYMD     TO   JYURRK-UPSRYYMD
           MOVE    JYURRK-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "JYURRK-KEY21"    TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"          USING
           MOVE    "tbl_jyurrk"         TO   MCP-TABLE
           MOVE    "key21"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
             PERFORM 920-DBFETCH3-SEC
           ELSE
             MOVE    1           TO    FLG-JYURRK
           END-IF
           .
       920-DBSELECT3-EXT.
           EXIT.
      *
      *****************************************************************
      *     受診履歴情報フェッチ
      *****************************************************************
       920-DBFETCH3-SEC                SECTION.
      *
           MOVE    "DBFETCH"          TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "JYURRK-KEY21"    TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"          USING
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key21"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                      MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                      MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                      SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF    MCP-RC   =   ZERO
             MOVE    ZERO            TO   FLG-JYURRK
             MOVE    MCPDATA-REC  TO  JYURRK-REC
      *
      *    テスト患者は対象外
             MOVE    JYURRK-PTID     TO   WRK-PTID
             PERFORM  900-PTINF-READ-SEC
             IF   FLG-PTINF   =   ZERO
                 AND     PTINF-TSTPTNUMKBN   =   "1"
               GO  TO  920-DBFETCH3-SEC
             END-IF
      *
           ELSE
             MOVE    1            TO  FLG-JYURRK
           END-IF  
           .
       920-DBFETCH3-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報クローズ処理
      *****************************************************************
       920-DBCLOSE3-SEC                 SECTION.
      *
      *=== sarge対応 Takashima(2006/08/09)
      *      MOVE  "DBCLOSE"                TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "JYURRK-KEY21"  TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key21"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                      MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                      MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                      SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           .
       920-DBCLOSE3-EXT.
           EXIT.
      *
      *****************************************************************
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  WRK-PARA-HOSPID      TO  HKNJA-HOSPID
           MOVE    WRK-PARA-HOSPNUM     TO   HKNJA-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    WRK-HKNJANUM         TO   HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNJAINF-KEY"       TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"          USING
           MOVE    "tbl_hknjainf"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC  =  ZERO
             MOVE  "HKNJAINF-KEY"       TO   ORC-DBPATH
             PERFORM  925-HKNJAINF-READ-SEC
      *     ELSE
           END-IF
      *
           INITIALIZE  MCPAREA
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE  "DBCLOSE"        TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"        TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNJAINF-KEY"   TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"      USING
           MOVE    "tbl_hknjainf"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                            ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *
           .
       920-HKNJAINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者情報読込
      *****************************************************************
       925-HKNJAINF-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     CALL  "ORCMCPSUB"  USING
           MOVE  "tbl_hknjainf"     TO   MCP-TABLE
           MOVE  "key"              TO   MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                    MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                          ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                    MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                    SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC      TO   HKNJAINF-REC
             MOVE  ZERO             TO   FLG-HKNJAINF
             MOVE  HKNJA-HKNJANAME  TO   WRK-HKNJANAME
           ELSE
             INITIALIZE  HKNJAINF-REC
             MOVE  1                TO   FLG-HKNJAINF
             MOVE  WRK-HKNJANUM     TO   WRK-HKNJANAME
           END-IF
      *
           .
       925-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           MOVE    ZERO    TO    WRK-KOHSEIKYUGK
                                 WRK-KOHSKJSEIKYUGK
                                 WRK-ZENCOMPFTN
      *--- 2009/01/08 ADD START ----------------------------------------
                                 WRK-FTNFLG
      *--- 2009/01/08 ADD END ------------------------------------------
      *
      ***  20050907 MOTOHASI 公費助成額の算出を修正
           IF   RECE10-RJNHKNNUM  NOT =  SPACE
             MOVE   SYU-KOH-COMPFTN(1)    TO   WRK-ZENCOMPFTN
           ELSE
             MOVE   SYU-SYUCOMPFTN        TO   WRK-ZENCOMPFTN
           END-IF
      ***  20050907 MOTOHASI 公費助成額の算出を修正
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >   4
                    OR   SYU-KOH-HKNNUM(IDX)  =  SPACE
             IF   SYU-KOH-HKNNUM(IDX)  NOT =  "027"
      *    差額から助成額(公費請求額)を算出
               IF   RECE10-RJNHKNNUM  NOT =  SPACE
      *
      ***  20050907 MOTOHASI 公費助成額の算出を修正
                 IF  SYU-KOH-COMPFTN(IDX) < WRK-ZENCOMPFTN
      ***  20050907 MOTOHASI 公費助成額の算出を修正
      *
                   COMPUTE  WRK-KOHSEIKYUGK  =  SYU-KOH-COMPFTN(1)
                                             -  SYU-KOH-COMPFTN(IDX)
                   COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SKJ-FTN(1)
                                                -  SYU-SKJ-FTN(IDX)
      ***  20050907 MOTOHASI 公費助成額の算出を修正
                   MOVE   SYU-KOH-COMPFTN(IDX)  TO  WRK-ZENCOMPFTN
                 END-IF
      ***  20050907 MOTOHASI 公費助成額の算出を修正
               ELSE
      *
      ***  20050907 MOTOHASI 公費助成額の算出を修正
                 IF  SYU-KOH-COMPFTN(IDX) < WRK-ZENCOMPFTN
      ***  20050907 MOTOHASI 公費助成額の算出を修正
      *
      *--- 2009/01/08 ADD+DELETE START ---------------------------------
      *             COMPUTE  WRK-KOHSEIKYUGK  =  SYU-SYUCOMPFTN
      *                                       -  SYU-KOH-COMPFTN(IDX)
      *             COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SYUSKJ-FTN
      *                                          -  SYU-SKJ-FTN(IDX)
                   IF    RECE10-FTNMONEY(1)  =  ZERO
                       AND  RECE10-KYURATE  NOT = ZERO
           DISPLAY "限度なし" RECE10-PTNUM
                     MOVE  SYU-KOH-HKNNUM(IDX)   TO  WRK-KOHNUM
                     PERFORM   950-HKNNUM-SET-SEC
                     IF   HKN-RECEFTNDSPKBN  =  1
           DISPLAY "四捨五入する" 
                       COMPUTE  WRK-KOHSEIKYUGK  =  SYU-SYUCOMPFTN
                                                 -  SYU-KOH-COMPFTN(IDX)
                       COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SYUSKJ-FTN
                                                    -  SYU-SKJ-FTN(IDX)
                     ELSE
           DISPLAY "四捨五入しない" 
                       COMPUTE  WRK-KOHSEIKYUGK = SYU-SYUCOMPFTN-ENTANI
                                          -  SYU-KOH-COMPFTN-ENTANI(IDX)
                       COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SYUSKJ-FTN
                                                    -  SYU-SKJ-FTN(IDX)
                     END-IF
                   ELSE
           DISPLAY "限度あり" RECE10-PTNUM
                     ADD    SYU-KOH-COMPFTN(IDX)  TO   WRK-KOHFTN
                     MOVE   1   TO    WRK-FTNFLG
                     COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SYUSKJ-FTN
                                                 -  SYU-SKJ-FTN(IDX)
                   END-IF
      *--- 2009/01/08 ADD+DELETE END -----------------------------------
      ***  20050907 MOTOHASI 公費助成額の算出を修正
                   MOVE   SYU-KOH-COMPFTN(IDX)  TO  WRK-ZENCOMPFTN
                 END-IF
      ***  20050907 MOTOHASI 公費助成額の算出を修正
               END-IF
      *
             END-IF
           END-PERFORM
      *
      *--- 2009/01/08 ADD START ----------------------------------------
           IF  WRK-FTNFLG  =  1
             COMPUTE  WRK-HKNFTN       =   WRK-HKNFTN
                                       -   WRK-KOHFTN
             INITIALIZE    WRK-KOHFTN
           ELSE
             ADD    WRK-KOHSEIKYUGK     TO   WRK-KOHSEIKYUGK-G
           END-IF
      *--- 2009/01/08 ADD END ------------------------------------------
      *--- (2008/12/18) line add start monma ---------------------------
           COMPUTE  WRK-JIHIGK-G  =  WRK-JIHIGK-G
                                  +  SYU-JIHI-TOTAL
                                  +  SYU-JIHI-TOTAL-TAX
      *--- (2008/12/18) line add end   monma ---------------------------
      *
      *--- 2009/01/08 DELETE START -------------------------------------
      *     ADD    WRK-KOHSEIKYUGK     TO   WRK-KOHSEIKYUGK-G
      *--- 2009/01/08 DELETE END ---------------------------------------
           ADD    WRK-KOHSKJSEIKYUGK  TO   WRK-KOHSEIKYUGK-G
           ADD    SYU-SKYMONEY        TO   WRK-MADOSEIKYUGK-G
      *
           .
       800-SEIKYU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報セット
      *****************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
             INITIALIZE  SYUNOU-REC
      *--- 2009/01/08 ADD START ----------------------------------------
                         WRK-HKNFTN
             MOVE   RECE10-FTNMONEY(1)    TO   WRK-HKNFTN
      *--- 2009/01/08 ADD END ------------------------------------------
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE  RECE10-HOSPID      TO  SYU-HOSPID
             MOVE   RECE10-HOSPNUM     TO   SYU-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
             MOVE   RECE10-NYUGAIKBN   TO   SYU-NYUGAIKBN
             MOVE   RECE10-PTID        TO   SYU-PTID
             MOVE   SYUNOU-REC         TO   MCPDATA-REC
             MOVE   "DBSELECT"         TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE  "SYUNOU-KEY2"      TO  ORC-DBPATH
      *       CALL  "ORCMCPSUB"        USING
             MOVE   "tbl_syunou"       TO   MCP-TABLE
             MOVE   "key2"             TO   MCP-PATHNAME
             CALL   "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
             IF   MCP-RC   =   ZERO
               MOVE  "SYUNOU-KEY2"  TO  ORC-DBPATH
               PERFORM  925-SYUNOU-READ-SEC
      *
               PERFORM  UNTIL  FLG-SYUNOU  =  1
                 IF  RECE10-SRYYM  =  SYU-SRYYMD(1:6)
                     AND  SYU-DENPJTIKBN   NOT = "3"
                     AND  RECE10-TEKSTYMD  <=  SYU-SRYYMD
      *
      ***  20050412  @@@@-MOTOHASI  
                     AND  SYU-SYUHKNNUM  =  RECE10-HKNNUM
      ***  20050412  @@@@-MOTOHASI  
      *
      *
      *    旧総合病院の時は該当する診療科のみ対象にする
      *--- 2008/10/30  CHANGE  START ----------------------------------
      * 20081014 主科設定時に診療科を印字するSTART
                   IF    SYS-1001-HOSPSBT1 NOT =  1
                       OR   RECE10-NYUGAIKBN      = "1"
      *              IF    RECE10-NYUGAIKBN      = "1" OR
      * 20081014 主科設定時に診療科を印字するEND
      *--- 2008/10/30  CHANGE  END ----------------------------------
                       OR   RECE10-RECEKA         = SYU-SRYKA
                     PERFORM  800-SEIKYU-SEC
                   END-IF
                 END-IF
      *
                 MOVE  "SYUNOU-KEY2"     TO   ORC-DBPATH
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
      *       ELSE
             END-IF
      *
             INITIALIZE  MCPAREA
      *=== sarge対応 Takashima(2006/08/09)
      *       MOVE  "DBCLOSE"      TO  MCP-FUNC
             MOVE  "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE  "SYUNOU-KEY2"  TO  ORC-DBPATH
      *       CALL  "ORCMCPSUB"  USING
             MOVE    "tbl_syunou"       TO   MCP-TABLE
             MOVE    "key2"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                             ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *
      *--- 2009/01/08 ADD START ----------------------------------------
           IF  WRK-FTNFLG  =  1
             ADD    WRK-HKNFTN     TO   WRK-KOHSEIKYUGK-G
           END-IF
      *--- 2009/01/08 ADD END ------------------------------------------
           .
       920-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       925-SYUNOU-READ-SEC  SECTION.
      *
           MOVE    "DBFETCH"        TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     CALL  "ORCMCPSUB"  USING
           MOVE    "tbl_syunou"     TO   MCP-TABLE
           MOVE    "key2"           TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
      ***07/09/20 nagaya 4.0対応 end
                                    MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                              ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                    MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                    SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF  MCP-RC  =  ZERO
             MOVE   MCPDATA-REC     TO   SYUNOU-REC
             MOVE   ZERO            TO   FLG-SYUNOU
           ELSE
             INITIALIZE  SYUNOU-REC
             MOVE    1              TO   FLG-SYUNOU
           END-IF
      *
           .
       925-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報READ自費・自賠・労災用(外来)
      *****************************************************************
       940-SYUNOU-READ-SEC         SECTION.
      *
           INITIALIZE                   SYUNOU-REC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  JYURRK-HOSPID        TO  SYU-HOSPID
           MOVE    JYURRK-HOSPNUM       TO   SYU-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    JYURRK-NYUGAIKBN     TO   SYU-NYUGAIKBN
           MOVE    JYURRK-PTID          TO   SYU-PTID
           MOVE    JYURRK-DENPNUM       TO   SYU-DENPNUM
           MOVE    SYUNOU-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "SYUNOU-KEY"      TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"          USING
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF  MCP-RC  =  ZERO
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE  "SYUNOU-KEY"  TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 end
             MOVE   "DBFETCH"          TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *       CALL  "ORCMCPSUB"  USING
             MOVE   "tbl_syunou"       TO   MCP-TABLE
             MOVE   "key"              TO   MCP-PATHNAME
             CALL   "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                 ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
             IF  MCP-RC  =  ZERO
               MOVE  MCPDATA-REC       TO   SYUNOU-REC
               MOVE  ZERO              TO   FLG-SYUNOU
             ELSE
               INITIALIZE  SYUNOU-REC
               MOVE  1                 TO   FLG-SYUNOU
             END-IF
      *
           ELSE
             MOVE    1                 TO   FLG-SYUNOU
           END-IF
      *
           INITIALIZE  MCPAREA
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE  "DBCLOSE"        TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"       TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "SYUNOU-KEY"  TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"      USING
           MOVE  "tbl_syunou"          TO   MCP-TABLE
           MOVE  "key"                 TO   MCP-PATHNAME
           CALL  "ORCDBMAIN"           USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                            ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           .
       940-SYUNOU-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納情報SELECT(入院データ用)
      *****************************************************************
       940-SYUNOU-SELECT-SEC         SECTION.
      *
           INITIALIZE                  SYUNOU-REC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    JYURRK-HOSPID        TO  SYU-HOSPID
           MOVE    JYURRK-HOSPNUM       TO   SYU-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    JYURRK-PTID          TO   SYU-PTID
           MOVE    JYURRK-NYUGAIKBN     TO   SYU-NYUGAIKBN
           MOVE    WRK-PARA-PRTSYMD     TO   SYU-SKYSTYMD
           MOVE    WRK-PARA-PRTLYMD     TO   SYU-SKYEDYMD
           MOVE    SYUNOU-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYUNOU-KEY5"        TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "key5"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
             PERFORM   940-SYUNOU-FETCH-SEC
           ELSE
             MOVE   1           TO   FLG-SYUNOU
           END-IF
           .
       940-SYUNOU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    収納情報１件読み込み
      *****************************************************************
       940-SYUNOU-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"            TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYUNOU-KEY5"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "key5"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC    =   ZERO
             MOVE    MCPDATA-REC     TO   SYUNOU-REC
             MOVE    ZERO            TO   FLG-SYUNOU
             IF    SYU-DENPJTIKBN    =   "3"
               GO    TO    940-SYUNOU-FETCH-SEC
             END-IF
      *
             IF    SYU-SYUHKNNUM(1:2)  NOT  =  "97" AND "98"
               GO    TO    940-SYUNOU-FETCH-SEC
             END-IF
      *
           ELSE
             MOVE    1               TO   FLG-SYUNOU
           END-IF
           .
       940-SYUNOU-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    収納情報クローズ
      *****************************************************************
       940-SYUNOU-CLOSE-SEC         SECTION.
      *
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE    "DBCLOSE"            TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYUNOU-KEY5"        TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "key5"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           .
       940-SYUNOU-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *   保険組合せ情報セット
      *****************************************************************
       930-HKNCOMBI-SET-SEC  SECTION.
      *
           INITIALIZE  HKNCOMBI-REC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  JYURRK-HOSPID          TO  COMB-HOSPID
           MOVE  JYURRK-HOSPNUM         TO   COMB-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE  JYURRK-PTID            TO   COMB-PTID
           MOVE  JYURRK-HKNCOMBINUM     TO   COMB-HKNCOMBINUM
           MOVE  HKNCOMBI-REC           TO   MCPDATA-REC
           MOVE  "DBSELECT"             TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNCOMBI-KEY"      TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"          USING
           MOVE    "tbl_hkncombi"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC  =  ZERO
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE  "HKNCOMBI-KEY"  TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 end
             MOVE    "DBFETCH"            TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *       CALL  "ORCMCPSUB"  USING
             MOVE    "tbl_hkncombi"     TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                             ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO   HKNCOMBI-REC
               MOVE    ZERO             TO   FLG-HKNCOMBI
             ELSE
               INITIALIZE  HKNCOMBI-REC
               MOVE    1                TO   FLG-HKNCOMBI
             END-IF
           ELSE
             MOVE    1                  TO   FLG-HKNCOMBI
           END-IF
      *
           INITIALIZE             MCPAREA
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE  "DBCLOSE"        TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNCOMBI-KEY"  TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"      USING
           MOVE    "tbl_hkncombi"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                            ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *
           .
       930-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   保険番号情報セット
      *****************************************************************
       950-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  WRK-PARA-HOSPID           TO  HKN-HOSPID
           MOVE  WRK-PARA-HOSPNUM     TO   HKN-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE  WRK-KOHNUM           TO   HKN-HKNNUM
           MOVE  HKNNUM-REC           TO   MCPDATA-REC
           MOVE  "DBSELECT"           TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNNUM-KEY2"        TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"          USING
           MOVE    "tbl_hknnum"       TO   MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                      MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                      MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                      SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =  ZERO
             MOVE  "HKNNUM-KEY2"      TO   ORC-DBPATH
             PERFORM  955-HKNNUM-READ-SEC
      *     ELSE
           END-IF
      *
           INITIALIZE  MCPAREA
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE  "DBCLOSE"        TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNNUM-KEY2"    TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"      USING
           MOVE    "tbl_hknnum"       TO   MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                      MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                            ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                      MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                      SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *
           .
       950-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号情報読込
      *****************************************************************
       955-HKNNUM-READ-SEC  SECTION.
      *
           MOVE    "DBFETCH"        TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     CALL  "ORCMCPSUB"  USING
           MOVE    "tbl_hknnum"     TO   MCP-TABLE
           MOVE    "key2"           TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
      ***07/09/20 nagaya 4.0対応 end
                                    MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                        ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                    MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                    SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC  =  ZERO
             MOVE  MCPDATA-REC      TO   HKNNUM-REC
             MOVE  ZERO             TO   FLG-HKNNUM
           ELSE
             INITIALIZE             HKNNUM-REC
             MOVE      1            TO   FLG-HKNNUM
           END-IF
      *
           .
       955-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
      *    ステップ管理終了処理
           MOVE    "STE"             TO   SJOBKANRI-MODE
           INITIALIZE                     JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM       TO   JOB-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    S100LNK-JOBID     TO   JOB-JOBID
           MOVE    S100LNK-SHELLID   TO   JOB-SHELLID
           MOVE    CNT-PAGE          TO   JOB-UPDCNT
           CALL    "ORCSJOB"         USING
                                     ORCSJOBKANRIAREA
                                     JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
                                     SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *
      *     PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"             TO   MCP-FUNC.
      ***07/09/20 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      ***07/09/20 nagaya 4.0対応 end
      *
           MOVE    "DBSTART"            TO   MCP-FUNC.
      ***07/09/20 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      ***07/09/20 nagaya 4.0対応 end
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
      ***07/09/20 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      ***07/09/20 nagaya 4.0対応 end
      *
           MOVE    "DBDISCONNECT"       TO   MCP-FUNC.
      ***07/09/20 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      ***07/09/20 nagaya 4.0対応 end
           .
       900-DBCLOSE-EXT.
           EXIT.
